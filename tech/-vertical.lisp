;;; Filename: -vertical.lisp

;;; Vertical substrate: one model of where an object sits in the vertical dimension,
;;; replacing the role-branching elevation rules scattered across -elevation,
;;; -support-elevation, and beam-relay.  Three terms, of which only the first is stored:
;;;
;;;   height  --  an object's extent along its own axis, base to top.  A property of the
;;;               object, not of its orientation: a wall-repeater is a one-unit rod
;;;               however it is mounted, so its height is 1 like any other repeater's.
;;;   base    --  the absolute level of the object's lowest point.
;;;   top     --  base plus height when the object's axis is vertical, base alone when it
;;;               is not.  Computed, never stored.
;;;
;;; Base is structural rather than declared.  An object that rests on something derives
;;; its base from that support's top, following ON, then HOLDING, then HAS-LOCATION, then
;;; HAS-POSITION, bottoming out at a location's own level.  An object fixed in space has
;;; its base authored -- a location as LOCATION-COORDS>'s optional third coordinate,
;;; anything else as HAS-ELEVATION -- or defaulted to zero.  No declaration distinguishes
;;; the resting case from the fixed one; the object's structure does.  "Anchor" is not a
;;; separate concept here: a beam anchor is always the anchoring object's TOP.
;;;
;;; The three achievable connector anchor heights fall out of this with no special case,
;;; measured from the floor the agent stands on: on the ground, base = floor and
;;; top = floor + 1; on a box, the box's top is floor + 1 so the connector's top is
;;; floor + 2; on a tray held by a standing agent, the agent's top is floor + 3/2, the
;;; zero-thickness tray's top is the same, and the connector's top is floor + 5/2.
;;;
;;; REQUIRES:
;;;   nested   : -height (has-height), -elevation (has-elevation), -location-coordinates
;;;              (location-coords>), -apparatus-coordinates (apparatus-coords>),
;;;              -support-occupancy (on), -location (has-location), -position
;;;              (has-position), -holding (holding)
;;; PROVIDES:
;;;   types     : vertical-object  --  everything with a place in the vertical model
;;;   parameter : *vertical-type-constants*  --  per-type height default, axis, and
;;;               base default
;;;   queries   : base, top, fixed-base, object-height
;;;
;;; WALL-GEARS and WALL-BLOWER are deliberately absent from the table.  Their
;;; HAS-ELEVATION is a stream elevation read by -gears-fan's BLOWER-ELEVATION, not the
;;; base of a solid, and folding them in would give one relation two meanings again.

(include-tech -height)
(include-tech -elevation)
(include-tech -location-coordinates)
(include-tech -apparatus-coordinates)
(include-tech -support-occupancy)
(include-tech -location)
(include-tech -position)
(include-tech -holding)

(in-package :ww)


(define-optional-types
  agent box connector jammer tray fan
  gate screen wall edge floor-repeater wall-repeater
  transmitter receiver gun
  pressure-plate toggle-plate floor-blower angled-blower)


(define-types
  vertical-object
    (either location agent box connector jammer tray fan
            gate screen wall edge floor-repeater wall-repeater
            transmitter receiver gun
            pressure-plate toggle-plate floor-blower angled-blower))


(defparameter *vertical-type-constants*
  '((location          0   :none       0)   ;a location is a point; its level is its own
    (agent             3/2 :vertical   0)
    (box               1   :vertical   0)
    (connector         1   :vertical   0)
    (jammer            1   :vertical   0)
    (tray              0   :vertical   0)   ;zero-thickness: a top flush with its base
    (fan               0   :vertical   0)
    (pressure-plate    0   :vertical   0)   ;flush with the floor it is positioned on
    (toggle-plate      0   :vertical   0)
    (floor-blower      0   :vertical   0)
    (angled-blower     0   :vertical   0)
    (gate              4   :vertical   0)
    (screen            4   :vertical   0)
    (wall              4   :vertical   0)
    (edge              3/2 :vertical   0)
    (floor-repeater    1   :vertical   0)   ;stands on the floor
    (wall-repeater     1   :horizontal 1)   ;height is projection from the wall, not lift
    (transmitter       0   :none       1)
    (receiver          0   :none       1)
    (gun               0   :none       1))
  "Per-type constants, in (TYPE HEIGHT-DEFAULT AXIS BASE-DEFAULT) form.  The axis says
   whether an object's height raises its top above its base: :VERTICAL does, :HORIZONTAL
   and :NONE do not.  The base default is where an object of that type sits when nothing
   says otherwise: 0 for anything standing on a floor, and 1 for wall-mounted apparatus,
   which hangs at about chest height.  That 1 is the same number APPARATUS-COORDS>
   registers as its third coordinate's default, for the same reason -- the relation lets a
   problem write the level inline, and this table supplies it when the problem carries no
   coordinates at all.  The types are disjoint leaves, so an object matches at most one
   entry and the order is presentational only.  A problem overrides any height for an
   individual object with HAS-HEIGHT, and any base with the coordinates or HAS-ELEVATION.")


(defparameter *vertical-type-cache* (make-hash-table :test #'eq)
  "Memoizes VERTICAL-TYPE-ENTRY by object.  Type membership is fixed for the whole
   problem instance, so each object is resolved once.  DEFPARAMETER (not DEFVAR) so the
   cache resets every time this file is respliced and loaded for a different problem.")


(define-query base (?object vertical-object)
  ;; The absolute level of the object's lowest point, derived from whatever the object
  ;; rests on.  An object resting on a support takes that support's top; a held object
  ;; takes its holder's top; an object merely at a location or positioned at one takes
  ;; that location's own level.  ON precedes HAS-LOCATION because a movable occupant
  ;; keeps its location fact while resting on a support, and HOLDING precedes it for the
  ;; same reason.  An object resting on nothing is fixed in space; see FIXED-BASE.
  (if (bind (on ?object $support))
    (top $support)
    (if (bind (holding $holder ?object))
      (top $holder)
      (if (bind (has-location ?object $location))
        (base $location)
        (if (bind (has-position ?object $site))
          (base $site)
          (fixed-base ?object))))))


(define-query fixed-base (?object vertical-object)
  ;; The authored level of an object that rests on nothing.  A location carries its level
  ;; as LOCATION-COORDS>'s third coordinate and a wall-mounted fixture as
  ;; APPARATUS-COORDS>'s, each defaulting per relation.  Everything else -- a segment
  ;; fixture, a floor repeater, or anything at all in a problem carrying no coordinates --
  ;; uses HAS-ELEVATION, and an object with none of the three falls back to its type's
  ;; base default.  A floor repeater is excluded from the mounting coordinate on purpose:
  ;; it stands on the floor, so its base defaults to 0 rather than to the wall-mounting
  ;; default of 1.  The two coordinate substrates cross-check anything that declares its
  ;; level twice.
  (if (bind (location-coords> ?object $x $y $z))
    $z
    (if (and (not (floor-repeater ?object))
             (bind (apparatus-coords> ?object $ax $ay $az)))
      $az
      (if (bind (has-elevation ?object $level))
        $level
        (fourth (vertical-type-entry ?object))))))


(define-query top (?object vertical-object)
  ;; Base plus height along a vertical axis, base alone otherwise.  A wall-repeater's
  ;; height projects horizontally from the wall it hangs on and so cannot raise its tip.
  (if (vertical-axis-p ?object)
    (+ (base ?object) (object-height ?object))
    (base ?object)))


(define-query object-height (?object vertical-object)
  ;; The object's own extent along its axis: authored, or this type's default.
  (if (bind (has-height ?object $height))
    $height
    (second (vertical-type-entry ?object))))


(defun vertical-axis-p (object)
  "Return true when OBJECT's height raises its top above its base.  Kept a Lisp function
   rather than inlined into TOP so the axis keyword never reaches the query translator."
  (eq (third (vertical-type-entry object)) :vertical))


(defun vertical-type-entry (object)
  "Return OBJECT's (TYPE HEIGHT-DEFAULT AXIS BASE-DEFAULT) entry from *VERTICAL-TYPE-CONSTANTS*.
   The table's types are disjoint leaves, so the first match is the only match.  An
   object outside the table has no place in the vertical model, which is an authoring
   error rather than a zero height -- saying so here is what keeps a mistargeted caller
   from silently reading NIL.  Memoized; see *VERTICAL-TYPE-CACHE*."
  (multiple-value-bind (cached present) (gethash object *vertical-type-cache*)
    (if present
      cached
      (setf (gethash object *vertical-type-cache*)
            (or (find-if (lambda (entry)
                           (member object (gethash (first entry) *types*)))
                         *vertical-type-constants*)
                (error "~%No vertical type constants are defined for ~S.~%~
                        Every object reaching BASE, TOP, or OBJECT-HEIGHT must belong to ~
                        one of the leaf types in *VERTICAL-TYPE-CONSTANTS*."
                       object))))))

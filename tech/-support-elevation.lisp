;;; Filename: -support-elevation.lisp

;;; Support-elevation substrate: the vertical level of a support's top and of an
;;; occupant standing either on a support or directly at a location.  These queries
;;; are shared by cargo manipulation and barrier vaulting.
;;;
;;; REQUIRES:
;;;   nested  : -support-occupancy, -location, -position, -height, -elevation, -holding
;;;             (cargo, holding -- needed to find a tray's holder)
;;; PROVIDES:
;;;   parameter : *vertical-reach-limit*, default 1 -- the maximum elevation gap an agent
;;;               can act across vertically: lifting cargo above or below its own elevation,
;;;               raising cargo onto a higher resting place, or jumping up onto a higher
;;;               support or clearing a barrier (jump.lisp reuses this parameter rather than
;;;               defining its own).  Independent of the agent's own declared height.  A
;;;               problem overrides it with its own DEFPARAMETER.
;;;   queries   : support-top-elevation, tray-top-elevation, occupant-elevation,
;;;               within-agent-vertical-reach (symmetric, for lifting),
;;;               within-agent-placement-reach (one-sided, for setting down)

(include-tech -support-occupancy)
(include-tech -location)
(include-tech -position)
(include-tech -height)
(include-tech -elevation)
(include-tech -holding)

(in-package :ww)


(define-optional-types box fan tray)


(defvar *vertical-reach-limit* 1
  "Maximum elevation gap an agent can act across vertically -- lifting cargo above or below
   its own elevation, raising cargo onto a higher resting place, or jumping up onto a higher
   support or clearing a barrier -- independent of the agent's own declared height.  Lifting
   is symmetric: an object resting more than this far below the agent's elevation is out of
   reach exactly as one more than this far above it is.  Setting an object down is not: only
   the upward direction is bounded, since a drop needs no reach at all.  Problem files can
   override this.")


(define-query support-top-elevation (?support support)
  ;; A box top is its resting level plus its declared-or-default height.  A fan is a
  ;; movable, zero-thickness support: its top is its own resting level, so a fan mounted
  ;; on gears stays flush with the floor.  A tray's top depends on whether it is currently
  ;; held (see tray-top-elevation).  A plate or fixed floor/angled blower top is the floor
  ;; elevation of the location where the fixture is positioned.
  (if (box ?support)
    (+ (occupant-elevation ?support) (declared-height ?support))
    (if (fan ?support)
      (occupant-elevation ?support)
      (if (tray ?support)
        (tray-top-elevation ?support)
        (do (bind (has-position ?support $location))
            (location-elevation $location))))))


(define-query tray-top-elevation (?tray tray)
  ;; A held tray's top is its holder's own top level -- occupant-elevation plus
  ;; declared-height, zero added for the zero-thickness tray itself.  A grounded tray is
  ;; inert, like a resting fan, contributing nothing beyond its own resting level.
  (if (bind (holding $holder ?tray))
    (+ (occupant-elevation $holder) (declared-height $holder))
    (occupant-elevation ?tray)))


(define-query occupant-elevation (?occupant support-occupant)
  ;; An occupant on a support stands at that support's top (for a plate this is the floor
  ;; elevation, so flush fixtures cost nothing; a box or fan support chains recursively).
  ;; An occupant on the ground -- including a fan mounted on gears, whose attachment is
  ;; not an (on ...) fact -- stands at the floor elevation of its own location.
  (if (bind (on ?occupant $support))
    (support-top-elevation $support)
    (do (bind (has-location ?occupant $location))
        (location-elevation $location))))


(define-query within-agent-vertical-reach (?agent agent ?target-elevation)
  ;; The lifting convention, used by cargo pickup and by any fixture an agent must reach to
  ;; manipulate: measure from the agent's standing elevation, capped by
  ;; *vertical-reach-limit* in either direction, independent of the agent's own declared
  ;; height.  Setting cargo down uses WITHIN-AGENT-PLACEMENT-REACH instead.
  (<= (abs (- ?target-elevation (occupant-elevation ?agent)))
      *vertical-reach-limit*))


(define-query within-agent-placement-reach (?agent agent ?target-elevation)
  ;; The setting-down convention, one-sided where lifting is symmetric: an agent can lower or
  ;; drop cargo any distance below its own standing elevation, since gravity does the work,
  ;; but can only raise a resting place *vertical-reach-limit* above itself.  Recovering what
  ;; it dropped is the symmetric WITHIN-AGENT-VERTICAL-REACH test, so a drop down a ledge is
  ;; deliberately not reversible from where the agent stands.
  (<= (- ?target-elevation (occupant-elevation ?agent))
      *vertical-reach-limit*))

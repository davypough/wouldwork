;;; Filename: -terrain-consistency.lisp

;;; Terrain-consistency substrate: cross-checks authored vertical facts against the walking
;;; arrangement -walkability-coordinates already derives.  It separates one geometric
;;; invariant from two topology-review policies.  The invariant runs automatically during
;;; walking initialization; TEST-TOPO applies the policies after staging each long-running
;;; topology problem.  All three ABSTAIN wherever the arrangement cannot settle the
;;; question -- an abstention is not a pass, it is the honest answer when the geometry names
;;; no level on one side of the thing being checked.
;;;
;;; A level STEP is the shared notion behind all three: an edge's grid interval flanks two
;;; cells, and when each flanking zone carries locations that agree on one level, and the
;;; two levels differ, that interval names a determinate step.  An edge whose intervals
;;; name no step, or more than one, settles nothing -- an edge running the length of a
;;; staircase separates no single pair of levels -- and every check below then leaves it
;;; alone.
;;;
;;;   Edge span.  An EDGE is the vertical surface between the two regions it separates, so
;;;   its BASE is the lower level of its step and its TOP the higher.
;;;
;;;   Edge traversability.  A step is only terrain if something can cross it.  An edge
;;;   naming a determinate step must have some level change joining a location in one of
;;;   its flanking zones to a location in the other -- otherwise the map has a wall the
;;;   author drew as a step.  Which pairs may cross is the author's choice, not the
;;;   geometry's: measured across the topology problems, one edge is crossed at exactly one
;;;   of the several location pairs whose line of centres passes through it.  So this asks
;;;   only that a crossing exist, never where.
;;;
;;;   Zone levels.  Locations in one zone are joined by derived WALKING edges, and
;;;   ONE-STEP-WALKABLE rejects every one of those that crosses a level change.  A zone
;;;   holding more than one level is therefore walking-disconnected across that
;;;   difference unless a level change spans it.  Grouping a zone's locations by level, the
;;;   groups must be joined; a location whose level has drifted leaves its own group
;;;   unjoined and is reachable by nothing.
;;;
;;; A LEVEL CHANGE, for the last two checks, is an authored STAIRWAY, JUMPING or CLIMBING
;;; traversal edge, or a floor drive's lift -- a floor-mounted fan or fixed floor blower
;;; launches its occupants from its own location to its AIMED-AT destination, which is how
;;; phobia-topo's agent reaches a loft ten units up with no traversal relation authored
;;; anywhere.  A WALKING edge is not one: the derivation that emits it is elevation-blind,
;;; and it is precisely the dead edge these checks detect.  Neither is REACH-VIA, which
;;; -traversal deliberately leaves outside the mode set: reaching across a step moves
;;; nobody over it.
;;;
;;; Same-zone locations at different levels are deliberate, not exceptional, so the zone
;;; check is a connectivity condition over level groups rather than an equality:
;;; claustro-topo's slab zone holds location11 at 0 and location13 at 3/2, joined by
;;; a STAIRWAY edge because the slab's west side carries no edge segment on purpose, and
;;; rumin-topo's zone 1 holds three ground locations and two at 3/2, joined by the
;;; STAIRWAY and JUMPING pair between location2 and location4.  An equality rule would
;;; reject both.
;;;
;;; Public WALKABILITY nests this file, so every walking problem receives the edge-span
;;; invariant automatically.  The two connectivity rules are deliberately not universal:
;;; a focused model may name locations at different levels specifically to characterize
;;; that coordinate-derived WALKING is elevation-blind, without claiming a complete route
;;; between them.  TEST-TOPO applies those stronger rules to the five full topology specs.
;;; The lower-level -WALKABILITY-COORDINATES substrate remains usable on its own with no
;;; vertical model and therefore retains its empty TERRAIN-COMPLAINTS seam.
;;;
;;; REQUIRES:
;;;   types     : location; edge, declared optional by nested -segment-geometry
;;;   nested    : -walkability-coordinates (the arrangement, the TERRAIN-COMPLAINTS seam,
;;;               and -segment-geometry's EDGE-SEGMENT>); -vertical (BASE, TOP)
;;; PROVIDES:
;;;   query     : terrain-complaints  --  overrides -walkability-coordinates' empty
;;;               default with the edge-span invariant above
;;;   functions : terrain-policy-complaints-for-state  --  the two stronger topology
;;;               review policies for TEST-TOPO
;;;   parameter : *terrain-level-change-modes*

(include-tech -walkability-coordinates)
(include-tech -vertical)

(in-package :ww)


(defparameter *terrain-level-change-modes*
  '(stairway jumping climbing)
  "The traversal modes that carry a mover across a level change.  WALKING is excluded
   because the derivation emitting it is elevation-blind -- a walking edge across a step is
   precisely the dead edge these checks detect.  A mode whose technology the problem did not
   include simply has no facts and contributes nothing, which is why this file needs no
   dependency on stairs, jump, or ladder.  Floor drives lift their occupants too, and are
   gathered separately from AIMED-AT rather than from this list.")


(define-query terrain-complaints (?arrangement)
  ;; Overrides -walkability-coordinates' empty seam.  The walking initializer enforces the
  ;; geometric invariant only; TEST-TOPO separately calls the connectivity-policy entry
  ;; point once the complete topology problem has staged.
  (do (assign $levels (terrain-location-levels))
      (assign $spans (terrain-edge-spans))
      (assign $edges (edge-segment-records))
      (terrain-arrangement-invariant-complaints ?arrangement $edges $spans $levels)))


(define-query terrain-location-levels ()
  ;; Each location paired with its own level.  BASE rather than the raw coordinate, so an
  ;; override such as -floor-blowing's hovering destination is honored here too.  The
  ;; level goes through ASSIGN first because PUSH is a macro: the translator leaves a
  ;; macro form untouched, so a query call buried inside one never receives its state.
  (do (assign $levels nil)
      (doall (?location location)
        (do (assign $level (base ?location))
            (push (cons ?location $level) $levels)))
      $levels))


(define-query terrain-edge-spans ()
  ;; Each edge paired with the vertical interval it occupies.  An edge's axis is vertical
  ;; in *VERTICAL-TYPE-CONSTANTS*, so its top is its base plus its height.
  (do (assign $spans nil)
      (doall (?edge edge)
        (do (assign $edge-base (base ?edge))
            (assign $edge-top (top ?edge))
            (push (list ?edge $edge-base $edge-top) $spans)))
      $spans))


(defun terrain-arrangement-complaints (arrangement edges spans levels)
  "Every complaint the three checks raise against ARRANGEMENT, as ready-to-print strings.
   EDGES are (name x1 y1 x2 y2) records, SPANS (edge base top), and LEVELS a
   (location . level) alist.  Returns NIL when everything agrees or nothing is
   determinable.  This combined entry point supports direct characterization; walking
   initialization calls the invariant subset, while TEST-TOPO calls the policy subset."
  (append (terrain-arrangement-invariant-complaints arrangement edges spans levels)
          (terrain-arrangement-policy-complaints arrangement edges levels)))


(defun terrain-arrangement-invariant-complaints (arrangement edges spans levels)
  "Complaints where an edge's authored vertical span contradicts its determinate step.
   This is safe for every walking model and therefore runs during coordinate initialization."
  (terrain-edge-complaints arrangement edges spans
                           (terrain-zone-levels arrangement levels)))


(defun terrain-arrangement-policy-complaints (arrangement edges levels)
  "Topology-review complaints requiring every determinate step and every level group to be
   connected by an authored level change.  These rules assume the locations describe a
   complete traversable topology, which is true of the *-TOPO specs but not of every focused
   technology model."
  (let ((zone-levels (terrain-zone-levels arrangement levels))
        (zone-of (terrain-location-zones arrangement))
        (changes (terrain-level-changes)))
    (append (terrain-uncrossed-edge-complaints arrangement edges zone-levels
                                               zone-of levels changes)
            (terrain-zone-complaints zone-levels zone-of levels changes))))


(defun terrain-policy-complaints-for-state (state)
  "The stronger terrain-policy complaints for a fully staged topology problem."
  (let ((arrangement (terrain-arrangement-for-state state))
        (edges (funcall (symbol-function 'edge-segment-records) state))
        (levels (funcall (symbol-function 'terrain-location-levels) state)))
    (terrain-arrangement-policy-complaints arrangement edges levels)))


(defun terrain-arrangement-for-state (state)
  "Rebuild the walking arrangement represented by STATE for post-staging validation."
  (walkability-coordinates-build-arrangement
    (funcall (symbol-function 'walkability-coordinates-location-coords) state)
    (append (funcall (symbol-function 'wall-segment-records) state)
            (funcall (symbol-function 'edge-segment-records) state))
    (funcall (symbol-function 'gate-segment-records) state)
    (funcall (symbol-function 'window-segment-records) state)
    (funcall (symbol-function 'screen-segment-records) state)
    (funcall (symbol-function 'walkability-coordinates-stream-specs) state)
    (car (gethash '(boundary-wall) *static-db*))))


(defun terrain-edge-complaints (arrangement edges spans zone-levels)
  "One complaint per edge whose authored span disagrees with the level step its intervals
   determine."
  (loop for record in edges
        for complaint = (terrain-edge-complaint arrangement record spans zone-levels)
        when complaint
          collect complaint))


(defun terrain-edge-complaint (arrangement record spans zone-levels)
  "The complaint RECORD's edge raises, or NIL.  An edge whose intervals determine no step,
   or more than one, raises nothing: the arrangement does not say what that edge
   separates -- one running the length of a staircase is the ordinary case -- and guessing
   there would be worse than silence."
  (let ((span (assoc (first record) spans))
        (steps (terrain-edge-steps arrangement record zone-levels)))
    (when (and span steps (null (rest steps))
               (not (and (= (second span) (car (first steps)))
                         (= (third span) (cdr (first steps))))))
      (format nil
              "EDGE ~A runs from base ~A to top ~A, but the zones it separates sit at ~
               levels ~A and ~A.~%~
               An edge is the vertical surface between two elevations, so its base must ~
               equal the lower level and its top the higher.  Set its HAS-ELEVATION to ~A ~
               and its HAS-HEIGHT to ~A, or correct the location levels on either side."
              (first record) (second span) (third span)
              (car (first steps)) (cdr (first steps))
              (car (first steps))
              (- (cdr (first steps)) (car (first steps)))))))


(defun terrain-uncrossed-edge-complaints (arrangement edges zone-levels zone-of levels changes)
  "One complaint per edge naming a determinate level step that nothing crosses."
  (loop for record in edges
        for complaint = (terrain-uncrossed-edge-complaint
                          arrangement record zone-levels zone-of levels changes)
        when complaint
          collect complaint))


(defun terrain-uncrossed-edge-complaint
    (arrangement record zone-levels zone-of levels changes)
  "The complaint RECORD's edge raises when its step has no crossing, or NIL.  Abstains on
   the same determinacy rule the span check uses, so an edge the arrangement cannot pin to
   one step is left alone here too."
  (let ((steps (terrain-edge-steps arrangement record zone-levels))
        (pairs (terrain-edge-zone-pairs arrangement record)))
    (when (and steps (null (rest steps))
               (not (terrain-zone-pairs-crossed-p pairs zone-of changes)))
      (format nil
              "EDGE ~A separates level ~A from level ~A, but nothing crosses it.~%~
               No STAIRWAY, JUMPING or CLIMBING edge joins a location on one side to a ~
               location on the other, and no floor drive lifts anything across, so the ~
               step is there and impassable.~%~
               Locations at level ~A: ~{~A~^, ~}~%~
               Locations at level ~A: ~{~A~^, ~}~%~
               Author a crossing between one of each, or make the segment a WALL if the ~
               step is meant to be a dead end."
              (first record) (car (first steps)) (cdr (first steps))
              (car (first steps))
              (terrain-locations-in-zones (mapcar #'first pairs) levels zone-of)
              (cdr (first steps))
              (terrain-locations-in-zones (mapcar #'second pairs) levels zone-of)))))


(defun terrain-zone-pairs-crossed-p (pairs zone-of changes)
  "True when some level change joins a location in one zone of some PAIRS entry to a
   location in the other, either way round."
  (some (lambda (change)
          (some (lambda (pair)
                  (terrain-change-spans-pair-p change pair zone-of))
                pairs))
        changes))


(defun terrain-change-spans-pair-p (change pair zone-of)
  "True when CHANGE's two endpoints sit in PAIR's two zones, in either order."
  (let ((from (gethash (first change) zone-of))
        (to (gethash (second change) zone-of)))
    (or (and (member (first pair) from) (member (second pair) to))
        (and (member (second pair) from) (member (first pair) to)))))


(defun terrain-locations-in-zones (zones levels zone-of)
  "The locations belonging to any of ZONES, naming a complaint's candidates rather than
   leaving the author to work out which side is which."
  (loop for entry in levels
        when (intersection zones (gethash (first entry) zone-of))
          collect (first entry)))


(defun terrain-edge-steps (arrangement record zone-levels)
  "The distinct level steps RECORD's grid intervals determine, each a (lower . higher)
   pair, in the order the intervals run."
  (let ((steps nil))
    (dolist (pair (terrain-edge-zone-pairs arrangement record))
      (pushnew (terrain-zone-pair-step pair zone-levels) steps :test #'equal))
    (remove nil (nreverse steps))))


(defun terrain-edge-zone-pairs (arrangement record)
  "The distinct (near far) zone pairs RECORD's grid intervals flank, indexed the way
   WALKABILITY-COORDINATES-DOOR-EDGES indexes a door's two sides."
  (let ((zones (getf arrangement :zones))
        (pairs nil))
    (dolist (key (walkability-coordinates-segment-interval-keys
                   record (getf arrangement :xs) (getf arrangement :ys)))
      (destructuring-bind (axis line cross) key
        (pushnew (list (if (eql axis :v)
                         (aref zones line cross)
                         (aref zones cross line))
                       (if (eql axis :v)
                         (aref zones (1+ line) cross)
                         (aref zones cross (1+ line))))
                 pairs
                 :test #'equal)))
    (nreverse pairs)))


(defun terrain-zone-pair-step (pair zone-levels)
  "The (lower . higher) level pair PAIR's two zones determine, or NIL when either holds no
   location or holds locations at more than one level, or when the two agree -- flanks at
   equal levels separate nothing vertical."
  (let ((near (gethash (first pair) zone-levels))
        (far (gethash (second pair) zone-levels)))
    (when (and near far (null (rest near)) (null (rest far))
               (/= (first near) (first far)))
      (cons (min (first near) (first far))
            (max (first near) (first far))))))


(defun terrain-zone-complaints (zone-levels zone-of levels changes)
  "One complaint per walking zone whose locations span several levels that CHANGES does
   not connect.  A zone at a single level, and one whose level groups are all reachable
   from its lowest through authored level changes, raise nothing."
  (let ((complaints nil))
    (loop for zone being the hash-keys of zone-levels using (hash-value present)
          for unjoined = (when (rest present)
                           (terrain-unjoined-levels zone present levels zone-of changes))
          when unjoined
            do (push (format nil
                             "Walking zone ~D holds locations at levels ~{~A~^, ~}, but ~
                              no authored level change joins level~P ~{~A~^, ~} to the ~
                              rest of the zone.~%~
                              Locations there: ~{~A~^, ~}~%~
                              Every derived WALKING edge across a level change is dead -- ~
                              ONE-STEP-WALKABLE rejects a step between levels -- so ~
                              either the level is wrong, or the crossing needs an ~
                              authored STAIRWAY, JUMPING or CLIMBING edge, or a floor ~
                              drive aimed at it."
                             zone present (length unjoined) unjoined
                             (terrain-zone-locations-at zone unjoined levels zone-of))
                     complaints))
    (nreverse complaints)))


(defun terrain-unjoined-levels (zone present levels zone-of changes)
  "The levels of ZONE that no chain of authored level changes reaches from its lowest.
   PRESENT is ascending, so its first element is that lowest level."
  (let ((pairs (terrain-zone-level-pairs zone levels zone-of changes))
        (reached (list (first present)))
        (growing t))
    (loop while growing
          do (setf growing nil)
             (dolist (pair pairs)
               (when (and (member (car pair) reached :test #'=)
                          (not (member (cdr pair) reached :test #'=)))
                 (push (cdr pair) reached)
                 (setf growing t))))
    (remove-if (lambda (level)
                 (member level reached :test #'=))
               present)))


(defun terrain-zone-level-pairs (zone levels zone-of changes)
  "The level pairs the authored changes join inside ZONE, each listed in both directions
   so the walk above tests only one.  A change with an endpoint outside ZONE contributes
   nothing: it crosses a barrier the derivation has already accounted for and says nothing
   about walking within this zone.  One between two locations at the same level likewise
   joins no levels."
  (let ((pairs nil))
    (dolist (change changes)
      (let ((from (terrain-change-level (first change) zone levels zone-of))
            (to (terrain-change-level (second change) zone levels zone-of)))
        (when (and from to (/= from to))
          (pushnew (cons from to) pairs :test #'equal)
          (pushnew (cons to from) pairs :test #'equal))))
    pairs))


(defun terrain-change-level (location zone levels zone-of)
  "LOCATION's level when it belongs to ZONE, and NIL when it does not, so a traversal
   with one endpoint elsewhere drops out of the walk above."
  (when (member zone (gethash location zone-of))
    (cdr (assoc location levels))))


(defun terrain-zone-locations-at (zone wanted levels zone-of)
  "The locations of ZONE sitting at one of the WANTED levels, naming the complaint's
   culprits rather than leaving the author to search the zone for them."
  (loop for (location . level) in levels
        when (and (member zone (gethash location zone-of))
                  (member level wanted :test #'=))
          collect location))


(defun terrain-zone-levels (arrangement levels)
  "A table from zone id to that zone's distinct location levels, ascending.  A zone with
   no location is absent, which is what makes both checks abstain around it."
  (let ((table (make-hash-table :test #'eql)))
    (dolist (entry (getf arrangement :memberships))
      (dolist (zone (second entry))
        (pushnew (cdr (assoc (first entry) levels))
                 (gethash zone table)
                 :test #'=)))
    (loop for zone in (loop for key being the hash-keys of table collect key)
          do (setf (gethash zone table) (sort (gethash zone table) #'<)))
    table))


(defun terrain-location-zones (arrangement)
  "A table from location to its zone list, for the membership tests above."
  (let ((table (make-hash-table :test #'eq)))
    (dolist (entry (getf arrangement :memberships) table)
      (setf (gethash (first entry) table) (second entry)))))


(defun terrain-level-changes ()
  "Every static pair of locations something can cross a level change between: the authored
   traversals, plus the floor drives that lift whatever rests on them."
  (append (terrain-authored-level-changes)
          (terrain-floor-drive-rides)))


(defun terrain-authored-level-changes ()
  "Every authored traversal that could cross a level change, as (source destination).  Read
   straight from the static database rather than through relation binds, so a mode the
   problem's technologies never registered costs nothing here.  A traversal key is
   (RELATION MODE SOURCE DESTINATION), the fluent payload having been stripped."
  (let ((changes nil))
    (loop for key being the hash-keys of *static-db*
          when (and (consp key)
                    (member (first key) '(traversal-via traversal-via>))
                    (member (second key) *terrain-level-change-modes*))
            do (push (list (third key) (fourth key)) changes))
    changes))


(defun terrain-floor-drive-rides ()
  "Every floor drive's (swept-location destination) pair.  A floor-mounted fan or a fixed
   floor blower launches the occupants resting on it to the drive's AIMED-AT destination
   and sustains them there, so that pair carries a mover across a level change exactly as
   an authored traversal does -- see -floor-blowing.  Read from the static database by name
   like the traversals above, so a problem with no gears technology contributes nothing."
  (let ((swept (make-hash-table :test #'eq))
        (rides nil))
    (loop for key being the hash-keys of *static-db* using (hash-value value)
          when (and (consp key) (eql (first key) 'has-position))
            do (setf (gethash (second key) swept) (first value)))
    (loop for key being the hash-keys of *static-db* using (hash-value value)
          when (and (consp key)
                    (eql (first key) 'aimed-at)
                    (terrain-lifting-drive-p (second key))
                    (gethash (second key) swept))
            do (push (list (gethash (second key) swept) (first value)) rides))
    rides))


(defun terrain-lifting-drive-p (drive)
  "True when DRIVE raises its occupants rather than pushing them along their own level.  A
   wall or angled drive blows horizontally and crosses no step."
  (or (member drive (gethash 'floor-gears *types*))
      (member drive (gethash 'floor-blower *types*))))

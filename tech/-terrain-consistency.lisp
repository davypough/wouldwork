;;; Filename: -terrain-consistency.lisp

;;; Terrain-consistency substrate: cross-checks the authored vertical facts against the
;;; walking arrangement -walkability-coordinates already derives, so a level that has
;;; drifted out of agreement with the geometry around it fails at initialization instead
;;; of quietly producing an unreachable location or a mismeasured barrier.  Two checks,
;;; both of which ABSTAIN wherever the arrangement cannot settle the question -- an
;;; abstention is not a pass, it is the honest answer when the geometry names no level on
;;; one side of the thing being checked.
;;;
;;;   Edge span.  An EDGE is the vertical surface between two regions of different
;;;   elevation, so its BASE is the lower of the two levels it separates and its TOP the
;;;   higher.  Every grid interval an edge covers flanks two cells; an interval whose two
;;;   flanking zones each carry locations, and whose locations within each zone agree on
;;;   one level, names a determinate step.  An edge with no determinate interval is left
;;;   alone, and so is one whose determinate intervals name different steps -- an edge
;;;   running along a staircase separates no single pair of levels.
;;;
;;;   Zone levels.  Locations in one zone are joined by derived WALK-VIA facts, and
;;;   ONE-STEP-WALKABLE rejects every one of those that crosses a level change.  A zone
;;;   holding more than one level is therefore walking-disconnected across that
;;;   difference unless an authored level change spans it.  Grouping a zone's locations
;;;   by level, the groups must be joined by authored STAIRS-VIA, JUMP-VIA, or
;;;   CLIMB-VIA> edges; a location whose level has drifted leaves its own group unjoined
;;;   and is reachable by nothing.
;;;
;;; Same-zone locations at different levels are deliberate, not exceptional, so the zone
;;; check is a connectivity condition over level groups rather than an equality.  Both
;;; coordinate-authored problems rely on that: claustro-topo's slab zone holds location11
;;; at 0 and location13 at 3/2, joined by STAIRS-VIA because the slab's west side carries
;;; no edge segment on purpose; rumin-topo's zone 1 holds three ground locations and two
;;; at 3/2, joined by the STAIRS-VIA and JUMP-VIA pair between location2 and location4.
;;; An equality rule would reject both.
;;;
;;; Only a problem that includes this file is checked, so walkability's own coordinate
;;; derivation stays usable with no vertical model at all.
;;;
;;; REQUIRES:
;;;   types     : location; edge, declared optional by nested -segment-geometry
;;;   nested    : -walkability-coordinates (the arrangement, the TERRAIN-COMPLAINTS seam,
;;;               and -segment-geometry's EDGE-SEGMENT>); -vertical (BASE, TOP)
;;; PROVIDES:
;;;   query     : terrain-complaints  --  overrides -walkability-coordinates' empty
;;;               default with the two cross-checks above
;;;   parameter : *terrain-level-change-relations*

(include-tech -walkability-coordinates)
(include-tech -vertical)

(in-package :ww)


(defparameter *terrain-level-change-relations*
  '(stairs-via stairs-via> jump-via jump-via> climb-via>)
  "The authored traversal relations whose endpoints may sit at different levels.
   WALK-VIA is excluded because it is derived and elevation-blind -- it is precisely the
   relation whose dead edges this file detects -- and REACH-VIA because reaching over a
   barrier moves nobody.  A relation belonging to a technology the problem did not
   include is simply absent from *STATIC-RELATIONS* and contributes nothing, which is why
   this file needs no dependency on stairs, jump, or ladder.  Phase 4's single traversal
   relation would replace this list with a test on the segment's mode.")


(define-query terrain-complaints (?arrangement)
  ;; Overrides -walkability-coordinates' empty seam.  Gathers the vertical facts the two
  ;; checks read -- every location's level and every edge's span, both straight out of
  ;; -vertical, so this file states no elevation rule of its own -- and hands them to the
  ;; plain-Lisp analysis below with the arrangement they are checked against.
  (do (assign $levels (terrain-location-levels))
      (assign $spans (terrain-edge-spans))
      (assign $edges (edge-segment-records))
      (terrain-arrangement-complaints ?arrangement $edges $spans $levels)))


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
  "Every complaint the two checks raise against ARRANGEMENT, as ready-to-print strings.
   EDGES are (name x1 y1 x2 y2) records, SPANS (edge base top), and LEVELS a
   (location . level) alist.  Returns NIL when everything agrees or nothing is
   determinable; -walkability-coordinates signals whatever comes back."
  (let ((zone-levels (terrain-zone-levels arrangement levels))
        (zone-of (terrain-location-zones arrangement)))
    (append (terrain-edge-complaints arrangement edges spans zone-levels)
            (terrain-zone-complaints zone-levels zone-of levels
                                     (terrain-authored-level-changes)))))


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


(defun terrain-edge-steps (arrangement record zone-levels)
  "The distinct level steps RECORD's grid intervals determine, each a (lower . higher)
   pair, in the order the intervals run."
  (let ((steps nil))
    (dolist (key (walkability-coordinates-segment-interval-keys
                   record (getf arrangement :xs) (getf arrangement :ys)))
      (pushnew (terrain-interval-step key (getf arrangement :zones) zone-levels)
               steps
               :test #'equal))
    (remove nil (nreverse steps))))


(defun terrain-interval-step (key zones zone-levels)
  "The (lower . higher) level pair grid interval KEY determines, or NIL when either
   flanking zone holds no location or holds locations at more than one level, or when the
   two flanks agree -- an interval between equal levels separates nothing vertical."
  (destructuring-bind (axis line cross) key
    (let ((near (gethash (if (eql axis :v)
                           (aref zones line cross)
                           (aref zones cross line))
                         zone-levels))
          (far (gethash (if (eql axis :v)
                          (aref zones (1+ line) cross)
                          (aref zones cross (1+ line)))
                        zone-levels)))
      (when (and near far (null (rest near)) (null (rest far))
                 (/= (first near) (first far)))
        (cons (min (first near) (first far))
              (max (first near) (first far)))))))


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
                              Every derived WALK-VIA across a level change is dead -- ~
                              ONE-STEP-WALKABLE rejects a step between levels -- so ~
                              either the level is wrong, or the crossing needs an ~
                              authored STAIRS-VIA, JUMP-VIA, or CLIMB-VIA>."
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


(defun terrain-authored-level-changes ()
  "Every authored traversal that could cross a level change, as (source destination).
   Read straight from the static database rather than through relation binds, so a
   relation the problem's technologies never declared costs nothing here."
  (let ((changes nil))
    (loop for key being the hash-keys of *static-db*
          when (and (consp key)
                    (member (first key) *terrain-level-change-relations*))
            do (push (list (second key) (third key)) changes))
    changes))

;;; Filename: problem-terrain-consistency-test.lisp

;;; Dedicated regression for the -terrain-consistency substrate: the two cross-checks
;;; that hold authored levels against the walking arrangement -walkability-coordinates
;;; derives.  The map has three compartments, each exercising one outcome:
;;;
;;;   1. GROUND1/GROUND2 at level 0, west of EDGE1, and SLAB1/SLAB2 at level 3/2 east of
;;;      it.  Every interval EDGE1 covers flanks one single-level zone on each side, so
;;;      the edge check is determinate there: EDGE1's base must be 0 and its top 3/2,
;;;      which the type table's default edge height of 3/2 supplies.
;;;   2. STAIR-LOW at 0 and STAIR-HIGH at 2 share the third compartment's zone.  Nothing
;;;      separates them geometrically, so the derived WALK-VIA between them is dead --
;;;      ONE-STEP-WALKABLE rejects a step between levels -- and the authored STAIRS-VIA is
;;;      what makes the pair legitimate.  The zone check passes exactly because of it.
;;;   3. WALL1 divides compartments 2 and 3.  It is a wall, not an edge, so the edge check
;;;      ignores it however its flanking levels read.
;;;
;;; The claims then drive the analysis directly with doctored inputs, since neither check
;;; is an init-literal check and neither can be provoked by a literal alone: an edge whose
;;; span misses its step is refused, a location whose level drifts away from its zone with
;;; no authored crossing is refused, and -- the property that keeps the checks honest -- an
;;; edge whose flank stops naming a single level is ABSTAINED on rather than guessed at,
;;; even when its span would otherwise be refused.
;;;
;;; Initial and final dynamic states are empty of anything the goal moves.  Expected
;;; minimum path length: zero.

(in-package :ww)


(ww-set *problem-name* terrain-consistency-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 0)


;;;; TYPES ;;;;


(define-types
  agent    (walker)
  location (ground1 ground2 slab1 slab2 stair-low stair-high)
  edge     (edge1)
  wall     (wall1))


;;;; TECHNOLOGY INCLUDE ;;;;


(include-tech -terrain-consistency)
(include-tech walkability)
(include-tech stairs)


;;;; INITIALIZATION ;;;;


(define-init
  (has-location walker ground1)

  ;; One rectangle, cut into three compartments.
  (boundary-wall
    ((0 0) (30 0) (30 10) (0 10) (0 0)))

  ;; EDGE1 is the ground-level footprint of the raised middle compartment: the vertical
  ;; surface between level 0 to its west and level 3/2 to its east.  Its base and height
  ;; are both defaulted, so the check reads the type table's 0 and 3/2.
  (edge-segment> edge1 10 0 10 10)

  ;; WALL1 is a plain partition, present to show that the edge check leaves walls alone
  ;; even though the levels flanking it disagree.
  (wall-segment> wall1 20 0 20 10)

  ;; Compartment 1, at ground level.  The third coordinate defaults to 0.
  (location-coords> ground1 3 3)
  (location-coords> ground2 3 7)

  ;; Compartment 2, the raised slab EDGE1 retains.
  (location-coords> slab1 15 3 3/2)
  (location-coords> slab2 15 7 3/2)

  ;; Compartment 3 holds two levels in one zone, with no segment between them.
  (location-coords> stair-low 25 3 0)
  (location-coords> stair-high 25 7 2)

  ;; The authored crossing that makes compartment 3's level difference legitimate.
  ;; Removing it is what the zone check exists to catch.
  (stairs-via stair-low () stair-high))


;;;; CHARACTERIZATION FIXTURES ;;;;


(define-problem-helper terrain-test-arrangement ()
  "The staged problem's own walking arrangement, rebuilt from its start state so the
   claims below can drive the analysis with substituted levels and spans."
  (walkability-coordinates-build-arrangement
    (funcall (symbol-function 'walkability-coordinates-location-coords) *start-state*)
    (append (funcall (symbol-function 'wall-segment-records) *start-state*)
            (funcall (symbol-function 'edge-segment-records) *start-state*))
    (funcall (symbol-function 'gate-segment-records) *start-state*)
    (funcall (symbol-function 'window-segment-records) *start-state*)
    (funcall (symbol-function 'screen-segment-records) *start-state*)
    nil
    (car (gethash '(boundary-wall) *static-db*))))


(define-problem-helper terrain-test-edges ()
  "The problem's authored edge records, as the derivation passes them."
  (funcall (symbol-function 'edge-segment-records) *start-state*))


(define-problem-helper terrain-test-levels ()
  "Every location's own level, as -vertical reports it."
  (funcall (symbol-function 'terrain-location-levels) *start-state*))


(define-problem-helper terrain-test-drifted-levels ()
  "The same levels with GROUND2 lifted to 1: a location whose level has drifted away
   from the rest of its zone with nothing authored to cross the difference."
  (cons (cons 'ground2 1)
        (remove 'ground2 (terrain-test-levels) :key #'car)))


;;;; VALIDATION CHARACTERIZATION ;;;;


(define-test-claim terrain-consistency-contract
  ;; -walkability-coordinates hands the arrangement to the seam this file overrides, and
  ;; the arrangement carries the raw grid the edge check needs to find flanking zones.
  (member 'terrain-complaints *query-names*)
  (getf (terrain-test-arrangement) :zones)
  (getf (terrain-test-arrangement) :xs)

  ;; The map as authored raises nothing, through the live seam rather than a stand-in.
  (null (funcall (symbol-function 'terrain-complaints)
                 *start-state* (terrain-test-arrangement)))

  ;; EDGE1's intervals name exactly one step, 0 up to 3/2, and its authored span matches.
  (equal (terrain-edge-steps (terrain-test-arrangement)
                             (first (terrain-test-edges))
                             (terrain-zone-levels (terrain-test-arrangement)
                                                  (terrain-test-levels)))
         '((0 . 3/2)))

  ;; An edge whose top no longer reaches the level it retains is refused, and the
  ;; complaint names the edge and the span it should have had.
  (search "EDGE1"
          (first (terrain-edge-complaints
                   (terrain-test-arrangement)
                   (terrain-test-edges)
                   '((edge1 0 2))
                   (terrain-zone-levels (terrain-test-arrangement)
                                        (terrain-test-levels)))))

  ;; The same doctored span raises nothing once one flank stops naming a single level:
  ;; the arrangement no longer determines what EDGE1 separates, so the check abstains
  ;; instead of guessing.  Abstention is the property that keeps it usable.
  (null (terrain-edge-complaints
          (terrain-test-arrangement)
          (terrain-test-edges)
          '((edge1 0 2))
          (terrain-zone-levels (terrain-test-arrangement)
                               (terrain-test-drifted-levels))))

  ;; A location whose level drifts away from its zone, with no authored crossing, is
  ;; refused and named.
  (search "GROUND2"
          (first (terrain-arrangement-complaints
                   (terrain-test-arrangement) nil nil
                   (terrain-test-drifted-levels))))

  ;; Compartment 3 holds two levels in one zone and raises nothing, because STAIRS-VIA
  ;; joins them.  That authored fact is the whole difference between it and GROUND2.
  (member 'stairs-via *terrain-level-change-relations*)
  (member '(stair-low stair-high) (terrain-authored-level-changes) :test #'equal)
  (null (terrain-arrangement-complaints
          (terrain-test-arrangement) nil nil (terrain-test-levels)))

  ;; The derivation reports every complaint at once, as a named failure.
  (expect-condition
    (lambda ()
      (report-terrain-complaints '("first complaint" "second complaint")))
    'error
    :containing "Terrain consistency check failed"))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query terrain-consistency-scenarios-valid ()
  (and
    ;; The three compartments' levels read back exactly as authored.
    (= (base ground1) 0)
    (= (base ground2) 0)
    (= (base slab1) 3/2)
    (= (base slab2) 3/2)
    (= (base stair-low) 0)
    (= (base stair-high) 2)

    ;; EDGE1 spans the ground to the slab it retains; its height is the type default.
    (= (base edge1) 0)
    (= (object-height edge1) 3/2)
    (= (top edge1) 3/2)

    ;; Walking is derived within a compartment and blocked across EDGE1 and WALL1.
    (bind (walk-via ground1 $ground-doors ground2))
    (null $ground-doors)
    (bind (walk-via stair-low $stair-doors stair-high))
    (null $stair-doors)
    (not (bind (walk-via ground1 $crossing-doors slab1)))
    (not (bind (walk-via slab2 $partition-doors stair-low)))

    ;; The derived edge across compartment 3's level change is dead, which is why the
    ;; authored STAIRS-VIA has to be there.
    (not (one-step-walkable walker stair-low stair-high))
    (bind (stairs-via stair-low $stair-means stair-high))
    (null $stair-means)))


(define-goal
  (terrain-consistency-scenarios-valid))

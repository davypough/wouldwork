;;; Filename: problem-terrain-consistency-test.lisp

;;; Dedicated regression for WALKABILITY's automatic terrain edge-span invariant and the
;;; two stronger connectivity policies TEST-TOPO applies to complete topology specs.  All
;;; three hold authored levels against the walking arrangement -walkability-coordinates
;;; derives.  The map has three compartments, each exercising one outcome:
;;;
;;;   1. GROUND1/GROUND2 at level 0, west of EDGE1, and SLAB1/SLAB2 at level 3/2 east of
;;;      it.  Every interval EDGE1 covers flanks one single-level zone on each side, so
;;;      both edge checks are determinate there: EDGE1's base must be 0 and its top 3/2,
;;;      which the type table's default edge height of 3/2 supplies, and its step must be
;;;      crossed -- by the one authored STAIRWAY edge between GROUND1 and SLAB1, since the
;;;      check asks that a crossing exist and never where.
;;;   2. STAIR-LOW at 0 and STAIR-HIGH at 2 share the third compartment's zone.  Nothing
;;;      separates them geometrically, so the derived WALKING edge between them is dead --
;;;      ONE-STEP-WALKABLE rejects a step between levels -- and the authored STAIRWAY edge is
;;;      what makes the pair legitimate.  The zone check passes exactly because of it.
;;;   3. WALL1 divides compartments 2 and 3.  It is a wall, not an edge, so the edge check
;;;      ignores it however its flanking levels read.
;;;   4. Four drive fixtures pin the static level-change classification: floor gears and a
;;;      fixed floor blower contribute vertical rides from their own locations to their
;;;      destinations, while otherwise identical wall and angled drives do not.
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
  location (ground1 ground2 slab1 slab2 stair-low stair-high
            lift-low1 lift-high1 lift-low2 lift-high2)
  edge     (edge1)
  wall     (wall1)
  floor-gears  (lift-gears)
  floor-blower (fixed-lift)
  wall-gears   (wall-drive)
  angled-gears (angled-drive)
  fan           (lift-fan))


;;;; TECHNOLOGY INCLUDE ;;;;


(include-tech floor-gears)
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

  ;; The two floor-drive fixtures occupy vertical pairs in that same mixed-level zone.
  ;; They add no new zone level and do not affect EDGE1's single-level flanks.
  (location-coords> lift-low1 25 4 0)
  (location-coords> lift-high1 25 4 2)
  (location-coords> lift-low2 25 6 0)
  (location-coords> lift-high2 25 6 2)

  ;; Terrain consistency treats floor gears with an available removable fan as a potential
  ;; static level change, independently of whether that fan is currently mounted.  Wall
  ;; and angled drives share the same HAS-POSITION/AIMED-AT representation but move
  ;; horizontally or along an arc, so they must not be mistaken for terrain lifts.
  (has-location lift-fan ground2)
  (has-position lift-gears lift-low1)
  (aimed-at lift-gears lift-high1)
  (has-position fixed-lift lift-low2)
  (aimed-at fixed-lift lift-high2)
  (has-position wall-drive ground2)
  (aimed-at wall-drive slab2)
  (has-position angled-drive slab2)
  (aimed-at angled-drive ground2)

  ;; The authored crossing that makes compartment 3's level difference legitimate.
  ;; Removing it is what the zone check exists to catch.
  (traversal-via stairway stair-low () stair-high)

  ;; The crossing over EDGE1's step.  The traversability check asks only that one exist,
  ;; not that every flanking pair carry one, so this single edge between GROUND1 and SLAB1
  ;; covers the whole edge; removing it is what that check exists to catch.
  (traversal-via stairway ground1 () slab1))


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

  ;; The live seam is intentionally narrower than the complete topology policy.  A focused
  ;; walking model may contain disconnected level groups, but a *-TOPO review may not.
  (null (terrain-arrangement-invariant-complaints
          (terrain-test-arrangement) nil nil (terrain-test-drifted-levels)))
  (search "GROUND2"
          (first (terrain-arrangement-policy-complaints
                   (terrain-test-arrangement) nil (terrain-test-drifted-levels))))

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

  ;; EDGE1's step is crossed, so the traversability check is satisfied -- by the single
  ;; authored stairway TRAVERSAL-VIA between GROUND1 and SLAB1, not by every flanking pair
  ;; carrying one.
  (null (terrain-uncrossed-edge-complaints
          (terrain-test-arrangement) (terrain-test-edges)
          (terrain-zone-levels (terrain-test-arrangement) (terrain-test-levels))
          (terrain-location-zones (terrain-test-arrangement))
          (terrain-test-levels)
          (terrain-level-changes)))

  ;; With nothing crossing it, the same edge is refused, and the complaint names both
  ;; sides so the author knows which pair to join.
  (search "EDGE1"
          (first (terrain-uncrossed-edge-complaints
                   (terrain-test-arrangement) (terrain-test-edges)
                   (terrain-zone-levels (terrain-test-arrangement) (terrain-test-levels))
                   (terrain-location-zones (terrain-test-arrangement))
                   (terrain-test-levels)
                   nil)))
  (search "SLAB1"
          (first (terrain-uncrossed-edge-complaints
                   (terrain-test-arrangement) (terrain-test-edges)
                   (terrain-zone-levels (terrain-test-arrangement) (terrain-test-levels))
                   (terrain-location-zones (terrain-test-arrangement))
                   (terrain-test-levels)
                   nil)))

  ;; The traversability check abstains exactly where the span check does: with a flank no
  ;; longer naming one level, an uncrossed EDGE1 raises nothing.
  (null (terrain-uncrossed-edge-complaints
          (terrain-test-arrangement) (terrain-test-edges)
          (terrain-zone-levels (terrain-test-arrangement)
                               (terrain-test-drifted-levels))
          (terrain-location-zones (terrain-test-arrangement))
          (terrain-test-drifted-levels)
          nil))

  ;; Only the two floor drives contribute rides.  Their common representation with the
  ;; wall and angled fixtures must not broaden the classification.
  (= (length (terrain-floor-drive-rides)) 2)
  (member '(lift-low1 lift-high1) (terrain-floor-drive-rides) :test #'equal)
  (member '(lift-low2 lift-high2) (terrain-floor-drive-rides) :test #'equal)
  (not (member '(ground2 slab2) (terrain-floor-drive-rides) :test #'equal))
  (not (member '(slab2 ground2) (terrain-floor-drive-rides) :test #'equal))
  (= (length (terrain-level-changes))
     (+ 2 (length (terrain-authored-level-changes))))

  ;; A location whose level drifts away from its zone, with no authored crossing, is
  ;; refused and named.
  (search "GROUND2"
          (first (terrain-arrangement-complaints
                   (terrain-test-arrangement) nil nil
                   (terrain-test-drifted-levels))))

  ;; Compartment 3 holds two levels in one zone and raises nothing, because the stairway
  ;; TRAVERSAL-VIA joins them.  That authored fact is the whole difference between it and
  ;; GROUND2.
  (member 'stairway *terrain-level-change-modes*)
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
    (bind (traversal-via walking ground1 $ground-doors ground2))
    (null $ground-doors)
    (bind (traversal-via walking stair-low $stair-doors stair-high))
    (null $stair-doors)
    (not (bind (traversal-via walking ground1 $crossing-doors slab1)))
    (not (bind (traversal-via walking slab2 $partition-doors stair-low)))

    ;; The derived edge across compartment 3's level change is dead, which is why the
    ;; authored stairway TRAVERSAL-VIA has to be there.
    (not (one-step-walkable walker stair-low stair-high))
    (bind (traversal-via stairway stair-low $stair-means stair-high))
    (null $stair-means)))


(define-goal
  (terrain-consistency-scenarios-valid))

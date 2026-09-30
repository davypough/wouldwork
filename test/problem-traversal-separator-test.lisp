;;; Filename: problem-traversal-separator-test.lisp

;;; Zero-action characterization of separator-based traversal on coordinate geometry, the
;;; part the substrate test's probe kinds cannot reach: real stairs, jump and walk builders,
;;; the coordinate walking derivation, and the geometric init checks.
;;;
;;; One rectangle, split at x=5 by EDGE1: a west ground zone at level 0 (LOWA, LOWB, LOWC)
;;; and an east slab at level 1 (HIGHA, HIGHB, HIGHC).  EDGE1's height 1 matches the step,
;;; so the edge-span invariant holds.  LEDGE is a short curb inside the west zone; it
;;; partitions nothing, so it flanks the west zone on both sides.
;;;
;;;   1. A staircase-or-edge pair, LOWA/HIGHA ((STAIR1) (EDGE1)).  The grounded crossing
;;;      prefers the stairs (decision D5), while a supported agent's remote ground landing
;;;      reads only the jump clause, so it crosses by the edge.
;;;   2. The crelay pattern, a gate standing on an edge.  LOWB/HIGHB ((EDGE1 GATE1)) with
;;;      GATE1 open is a plain JUMP: the open gate needs no clearance and the edge is never a
;;;      feature.  LOWC/HIGHB ((EDGE1 GATE2)) with GATE2 shut would have to vault its top at
;;;      5 from level 0, so it offers nothing.
;;;   3. A directed jump, HIGHC -> LOWC ((EDGE1)), crossed down and never up.
;;;   4. The kind-aware merge (decision D4).  LOWA/LOWB are at one level in one zone, so the
;;;      derivation grants them a direct walk; the problem also authors ((LEDGE)).  The stored
;;;      family is their union, (NIL (LEDGE)): the derived () walks, and it must not erase
;;;      the authored jump clause, which is what a supported agent lands by.
;;;   5. Init rejections: a walk clause across a coordinate level difference (decision D2),
;;;      and an edge named between two locations it does not stand between (init check 4,
;;;      driven directly since the staged spec must stay valid).
;;;
;;; Initial and final dynamic states are identical.  Expected minimum path length: zero.

(in-package :ww)


(ww-set *problem-name* traversal-separator-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 0)


;;;; TYPES ;;;;


(define-types
  agent (walker supported-agent)
  location (lowa lowb lowc higha highb highc)
  edge (edge1 ledge)
  staircase (stair1)
  gate (gate1 gate2)
  box (box1))


;;;; TECHNOLOGY INCLUDES ;;;;


(include-tech walkability)
(include-tech stairs)
(include-tech jump)


;;;; INITIALIZATION ;;;;


(define-init
  (has-location walker lowa)
  (has-location supported-agent lowa)
  (has-location box1 lowa)
  (on supported-agent box1)
  (open gate1)

  (boundary-wall
    ((0 0) (10 0) (10 10) (0 10) (0 0)))

  ;; The step between the zones, exactly as tall as the step.
  (edge-segment> edge1 5 0 5 10)
  (has-height edge1 1)

  ;; Both gates stand on EDGE1's top, the crelay geometry: supported doorways, so the
  ;; intervals under them stay solid for walking.  GATE2's top is 1 + 4 = 5.
  (gate-segment> gate1 5 8 5 9 1)
  (gate-segment> gate2 5 1 5 2 1)

  ;; A curb inside the west zone, between LOWA and LOWB but not partitioning them.
  (edge-segment> ledge 2 6 4 6)

  (location-coords> lowa 3 5)
  (location-coords> lowb 3 7)
  (location-coords> lowc 3 3)
  (location-coords> higha 7 5 1)
  (location-coords> highb 7 7 1)
  (location-coords> highc 7 3 1)

  (traverse-via lowa ((stair1) (edge1)) higha)
  (traverse-via lowb ((edge1 gate1)) highb)
  (traverse-via lowc ((edge1 gate2)) highb)
  (traverse-via> highc ((edge1)) lowc)
  (traverse-via lowa ((ledge)) lowb))


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!)))


;;;; CHARACTERIZATION FIXTURES ;;;;


(define-query separator-segment-is (?agent agent ?from location ?expected)
  ;; Membership rather than list equality, so the claims do not depend on the provider's
  ;; accumulation order.
  (ww-loop for $segment in (traversal-segments ?agent ?from)
           thereis (equal $segment ?expected)))


(define-query separator-no-segment-to (?agent agent ?from location ?to location)
  (ww-loop for $segment in (traversal-segments ?agent ?from)
           never (eql (fourth $segment) ?to)))


(define-problem-helper separator-test-edge-fit-complaint (edge key)
  "The edge-fit complaint EDGE raises as a clause member of the traversal fact KEY, a
   (relation source destination) list, or NIL when EDGE stands between the two.  The
   arrangement is rebuilt from the start state with its supported doorways, so the check
   can be driven with a fact the staged spec could not carry."
  (let ((arrangement (terrain-arrangement-for-state *start-state*)))
    (terrain-edge-fit-complaint
      arrangement
      key
      (assoc edge (funcall (symbol-function 'edge-segment-records) *start-state*))
      (terrain-location-zones arrangement))))


;;;; VALIDATION CHARACTERIZATION ;;;;


(define-test-claim traversal-separator-init-checks
  ;; D2: with coordinates on both ends, a walk clause across a level difference is refused
  ;; and must name what separates the levels; an edge clause across it is accepted.
  (expect-condition
    (lambda ()
      (validate-init-literals
        '((location-coords> lowa 3 5 0)
          (location-coords> higha 7 5 1)
          (traverse-via lowa () higha))
        :checks '(traversal-init-check)))
    'init-check-failure
    :containing "Name the staircase, edge or ladder that separates them"
    :check 'traversal-init-check)
  (null
    (validate-init-literals
      '((location-coords> lowa 3 5 0)
        (location-coords> higha 7 5 1)
        (traverse-via lowa ((edge1)) higha))
      :checks '(traversal-init-check)))
  ;; Edge fit: EDGE1 stands between LOWA and HIGHA, but not between LOWA and LOWB, which
  ;; share the west zone it only borders.  LEDGE partitions nothing, so it fits any pair
  ;; inside the zone it sits in.
  (null (separator-test-edge-fit-complaint 'edge1 '(traverse-via lowa higha)))
  (search "does not stand between"
          (separator-test-edge-fit-complaint 'edge1 '(traverse-via lowa lowb)))
  (null (separator-test-edge-fit-complaint 'ledge '(traverse-via lowa lowb))))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query traversal-separator-scenarios-valid ()
  (and
    ;; The zones sit at their authored levels, and EDGE1 spans the step between them.
    (= (location-elevation lowa) 0)
    (= (location-elevation higha) 1)
    (= (top edge1) 1)

    ;; 1. Stairs are preferred for the grounded crossing; the edge is the supported agent's
    ;;    way down onto the slab's ground, and the staircase offers no support transition.
    (separator-segment-is walker lowa '(stairs lowa (stair1) higha))
    (separator-segment-is walker higha '(stairs higha (stair1) lowa))
    (member '(jump (lowa box1) (edge1) (higha ground))
            (configuration-transition-results supported-agent) :test #'equal)
    (not (member '(jump (lowa box1) (stair1) (higha ground))
                 (configuration-transition-results supported-agent) :test #'equal))

    ;; 2. An open gate on an edge is a jump, not a vault; a shut one blocks the crossing.
    (= (top gate2) 5)
    (separator-segment-is walker lowb '(jump lowb (edge1 gate1) highb))
    (not (open gate2))
    (separator-no-segment-to walker lowc highb)

    ;; 3. The directed jump goes down only.  No walk is derived across the level change.
    (separator-segment-is walker highc '(jump highc (edge1) lowc))
    (separator-no-segment-to walker lowc highc)
    (not (bind (traverse-via lowc $cross-level-family highc)))

    ;; 4. The derived walk and the authored curb jump share one fact.  The grounded crossing
    ;;    walks; the supported agent still lands by the jump clause the walk did not erase.
    (bind (traverse-via lowa $merged-family lowb))
    (equal $merged-family '(nil (ledge)))
    (separator-segment-is walker lowa '(walk lowa nil lowb))
    (member '(jump (lowa box1) (ledge) (lowb ground))
            (configuration-transition-results supported-agent) :test #'equal)

    ;; The zero-action state is unchanged.
    (has-location walker lowa)
    (on supported-agent box1)))


(define-goal
  (traversal-separator-scenarios-valid))

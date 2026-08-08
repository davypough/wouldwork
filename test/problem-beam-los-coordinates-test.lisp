;;; Filename: problem-beam-los-coordinates-test.lisp

;;; Coordinate-derived line-of-sight regression for visibility's nested
;;; -beam-los-coordinates role.  Independent horizontal bands characterize:
;;;
;;;   1. Empty-corridor LOS to a transmitter, receiver, and fixed repeater.
;;;   2. Complete LOS removal at a wall interior and exactly at a wall endpoint.
;;;   3. Exact gate occluder lists for open and closed gates, plus the strict
;;;      gate-endpoint case that must not add an occluder.
;;;   4. Location occlusion exactly at the inclusive 1/2-unit tolerance, while
;;;      excluding a farther location and locations projected at an endpoint.
;;;   5. A concave BOUNDARY-WALL blocking a sightline that leaves and re-enters
;;;      the polygon even though both endpoints are inside it.
;;;   6. Jammer-only gate-target and gun derivation, including the deliberate
;;;      absence of intervening locations from those two occluder lists.
;;;   7. Complete LOS removal by an edge, identical to a wall -- confirming
;;;      EDGE-SEGMENT> feeds the same occluder test as WALL-SEGMENT>.
;;;
;;; The goal is the characterization query itself.  No action or propagation is
;;; needed: DERIVE-LOS-FROM-SEGMENTS establishes the static LOS tables during
;;; initialization.  Initial and final states are identical, and the expected
;;; minimum path length is 0.

(in-package :ww)


(ww-set *problem-name* beam-los-coordinates-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 0)


;;;; TYPES ;;;;


(define-types
  agent (idle-agent)
  location (idle
            clear-site
            wall-interior-site
            wall-corner-site
            closed-gate-site
            open-gate-site
            gate-corner-site
            tolerance-left
            tolerance-right
            tolerance-edge
            tolerance-outside
            tolerance-endpoint
            target-site
            target-intervening
            gun-site
            gun-intervening
            edge-interior-site
            boundary-left
            boundary-right)
  transmitter (clear-transmitter)
  receiver (clear-receiver
            wall-interior-receiver
            wall-corner-receiver
            closed-gate-receiver
            open-gate-receiver
            gate-corner-receiver
            edge-interior-receiver)
  wall-repeater (clear-repeater)
  gun (test-gun)
  jammer (derivation-enabler)
  gate (closed-gate open-gate corner-gate target-gate)
  wall (interior-wall corner-wall)
  edge (interior-edge))


;;;; TECHNOLOGY INCLUDE ;;;;


(include-tech visibility)


;;;; INITIALIZATION ;;;;


(define-init
  (has-location idle-agent idle)
  (open open-gate)

  ;; All ordinary bands lie below y=72, inside the solid lower part of this
  ;; boundary.  Above y=72, the notch between x=2 and x=8 separates two arms.
  (boundary-wall
    ((-2 -2) (12 -2) (12 82) (8 82)
     (8 72) (2 72) (2 82) (-2 82) (-2 -2)))

  ;; The first segment crosses its lane in the interior.  The second begins
  ;; exactly on its lane, exercising the inclusive wall-endpoint convention.
  (wall-segment> interior-wall 5 9 5 11)
  (wall-segment> corner-wall 5 20 5 22)

  ;; An edge blocks LOS in its interior exactly like a wall does -- EDGE-SEGMENT>
  ;; feeds the same $ALL-WALLS occluder list DERIVE-LOS-FROM-SEGMENTS builds.
  (edge-segment> interior-edge 5 69 5 71)

  ;; CLOSED-GATE and OPEN-GATE properly cross their lanes.  CORNER-GATE begins
  ;; exactly on its lane and therefore remains strict/non-occluding.  TARGET-GATE
  ;; supplies its own midpoint as the gate-target endpoint.
  (gate-segment> closed-gate 5 29 5 31)
  (gate-segment> open-gate 5 39 5 41)
  (gate-segment> corner-gate 5 50 5 52)
  (gate-segment> target-gate 10 64 10 66)

  ;; Location endpoints and location-occlusion candidates.
  (location-coords> idle -1 -1)
  (location-coords> clear-site 0 0)
  (location-coords> wall-interior-site 0 10)
  (location-coords> wall-corner-site 0 20)
  (location-coords> closed-gate-site 0 30)
  (location-coords> open-gate-site 0 40)
  (location-coords> gate-corner-site 0 50)
  (location-coords> tolerance-left 0 60)
  (location-coords> tolerance-right 10 60)
  (location-coords> tolerance-edge 5 121/2)
  (location-coords> tolerance-outside 5 303/5)
  (location-coords> tolerance-endpoint 0 60)
  (location-coords> target-site 0 65)
  (location-coords> target-intervening 5 65)
  (location-coords> gun-site 0 68)
  (location-coords> gun-intervening 5 68)
  (location-coords> edge-interior-site 0 70)
  (location-coords> boundary-left 0 80)
  (location-coords> boundary-right 10 80)

  ;; Apparatus functional points.  The three clear apparatus intentionally
  ;; coincide so one unobstructed band exercises all standard apparatus loops.
  (apparatus-coords> clear-transmitter 10 0)
  (apparatus-coords> clear-receiver 10 0)
  (apparatus-coords> clear-repeater 10 0)
  (apparatus-coords> wall-interior-receiver 10 10)
  (apparatus-coords> wall-corner-receiver 10 20)
  (apparatus-coords> closed-gate-receiver 10 30)
  (apparatus-coords> open-gate-receiver 10 40)
  (apparatus-coords> gate-corner-receiver 10 50)
  (apparatus-coords> test-gun 10 68)
  (apparatus-coords> edge-interior-receiver 10 70))


;;;; BOUNDARY VALIDATION CHARACTERIZATION ;;;;


(define-test-claim beam-boundary-validation
  (null
    (validate-init-literals
      '((boundary-wall ((0 0) (2 0) (2 1) (0 1) (0 0))))
      :checks '(segment-geometry-init-check)))
  (expect-condition
    (lambda ()
      (validate-init-literals
        '((boundary-wall ((0 0) (2 0) (2 1) (0 1) (-1 1))))
        :checks '(segment-geometry-init-check)))
    'init-check-failure
    :containing "must repeat its first point"
    :check 'segment-geometry-init-check)
  (expect-condition
    (lambda ()
      (validate-init-literals
        '((boundary-wall ((0 0) (2 0) (2 1) (1 2) (0 0))))
        :checks '(segment-geometry-init-check)))
    'init-check-failure
    :containing "not axis-aligned"
    :check 'segment-geometry-init-check))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query beam-los-coordinates-scenarios-valid ()
  (and
    ;; Direct derivation for each ordinary apparatus family.
    (bind (los-to-apparatus clear-site $transmitter-occluders clear-transmitter))
    (null $transmitter-occluders)
    (bind (los-to-apparatus clear-site $receiver-occluders clear-receiver))
    (null $receiver-occluders)
    (bind (los-to-apparatus clear-site $repeater-occluders clear-repeater))
    (null $repeater-occluders)
    (visible clear-site clear-transmitter)
    (visible clear-site clear-receiver)
    (visible clear-site clear-repeater)

    ;; Walls remove the LOS fact entirely, including at the wall's own endpoint.
    (not (potentially-visible wall-interior-site wall-interior-receiver))
    (not (visible wall-interior-site wall-interior-receiver))
    (not (potentially-visible wall-corner-site wall-corner-receiver))
    (not (visible wall-corner-site wall-corner-receiver))

    ;; An edge removes the LOS fact entirely too, exactly like a wall.
    (not (potentially-visible edge-interior-site edge-interior-receiver))
    (not (visible edge-interior-site edge-interior-receiver))

    ;; Proper gate crossings retain structural LOS with exact conditional occluders.
    (bind (los-to-apparatus
            closed-gate-site $closed-occluders closed-gate-receiver))
    (equal $closed-occluders '(closed-gate))
    (potentially-visible closed-gate-site closed-gate-receiver)
    (not (visible closed-gate-site closed-gate-receiver))
    (not (open closed-gate))

    (bind (los-to-apparatus
            open-gate-site $open-occluders open-gate-receiver))
    (equal $open-occluders '(open-gate))
    (potentially-visible open-gate-site open-gate-receiver)
    (visible open-gate-site open-gate-receiver)
    (open open-gate)

    ;; A gate intersection at the gate segment's endpoint is deliberately strict.
    (bind (los-to-apparatus
            gate-corner-site $corner-occluders gate-corner-receiver))
    (null $corner-occluders)
    (visible gate-corner-site gate-corner-receiver)

    ;; The exactly-half-unit candidate is included.  The farther and endpoint-
    ;; projected candidates must remain absent from this exact singleton list.
    (bind (los-to-location tolerance-left $tolerance-occluders tolerance-right))
    (equal $tolerance-occluders '(tolerance-edge))
    (bind (los-to-location tolerance-right $reverse-occluders tolerance-left))
    (equal $reverse-occluders '(tolerance-edge))
    (potentially-visible tolerance-left tolerance-right)
    (visible tolerance-left tolerance-right)

    ;; Coincident endpoints have no horizontal interior in which anything can occlude.
    (bind (los-to-location
            tolerance-left $coincident-occluders tolerance-endpoint))
    (null $coincident-occluders)

    ;; Both endpoints are inside the boundary, but their beam crosses the open notch.
    (not (potentially-visible boundary-left boundary-right))
    (not (potentially-visible boundary-right boundary-left))
    (not (visible boundary-left boundary-right))
    (not (visible boundary-right boundary-left))

    ;; Gate midpoint/self-segment handling and the jammer-specific exclusions:
    ;; intervening locations do not enter target-gate or gun occluder lists.
    (bind (los-to-target target-site $target-occluders target-gate))
    (null $target-occluders)
    (visible target-site target-gate)
    (not (open target-gate))

    (bind (los-to-apparatus gun-site $gun-occluders test-gun))
    (null $gun-occluders)
    (visible gun-site test-gun)

    ;; The state itself is unchanged by this zero-action characterization.
    (has-location idle-agent idle)
    (not (has-location derivation-enabler idle))))


(define-goal
  (beam-los-coordinates-scenarios-valid))

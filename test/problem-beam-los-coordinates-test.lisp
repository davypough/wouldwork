;;; Filename: problem-beam-los-coordinates-test.lisp

;;; Coordinate-derived line-of-sight regression for visibility's nested
;;; -beam-los-coordinates role.  Independent horizontal bands characterize:
;;;
;;;   1. Empty-corridor LOS to a transmitter, receiver, and fixed repeater.
;;;   2. Structural LOS retained across finite walls, with ordinary sight opaque and beam
;;;      clearance decided from crossing height (including exact-top blocking).
;;;   3. Exact gate occluder lists for open and closed gates, plus the strict
;;;      gate-endpoint case that must not add an occluder.
;;;   4. Location occlusion exactly at the inclusive 1/2-unit tolerance, while
;;;      excluding a farther location and locations projected at an endpoint.
;;;   5. A concave BOUNDARY-WALL retaining both crossings of a sightline that leaves and
;;;      re-enters the polygon, with its own default height 6.
;;;   6. Jammer-only gate-target and gun derivation, including the deliberate
;;;      absence of intervening locations from those two occluder lists.
;;;   7. Finite-height edge clearance, while ordinary sight remains opaque.
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
  (has-height interior-wall 2)

  ;; An edge has the same finite-height LOS model as a wall.  It remains excluded from
  ;; jumping because it is not a vaultable feature.
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
    (bind (los-via clear-site $transmitter-occluders clear-transmitter))
    (null $transmitter-occluders)
    (bind (los-via clear-site $receiver-occluders clear-receiver))
    (null $receiver-occluders)
    (bind (los-via clear-site $repeater-occluders clear-repeater))
    (null $repeater-occluders)
    (visible clear-site clear-transmitter)
    (visible clear-site clear-receiver)
    (visible clear-site clear-repeater)

    ;; Walls retain structural pairing but remain opaque to ordinary sight.  INTERIOR-WALL
    ;; explicitly overrides the default with height 2; equality blocks and 3 clears.
    (potentially-visible wall-interior-site wall-interior-receiver)
    (not (visible wall-interior-site wall-interior-receiver))
    (bind (los-barrier-crossings>
            wall-interior-site $wall-crossings wall-interior-receiver))
    (equal $wall-crossings '((:wall interior-wall 1/2 5 9 5 11)))
    (beam-visible wall-interior-site -1 wall-interior-receiver -1)
    (not (beam-visible wall-interior-site 2 wall-interior-receiver 2))
    (beam-visible wall-interior-site 3 wall-interior-receiver 3)

    ;; CORNER-WALL has no declared height and therefore defaults to 4.  Its own endpoint
    ;; remains inclusive in 2D, just as before.
    (potentially-visible wall-corner-site wall-corner-receiver)
    (not (visible wall-corner-site wall-corner-receiver))
    (= (object-height corner-wall) 4)
    (not (beam-visible wall-corner-site 4 wall-corner-receiver 4))
    (beam-visible wall-corner-site 5 wall-corner-receiver 5)

    ;; An undeclared edge defaults to height 3/2.
    (potentially-visible edge-interior-site edge-interior-receiver)
    (not (visible edge-interior-site edge-interior-receiver))
    (= (object-height interior-edge) 3/2)
    (not (beam-visible edge-interior-site 3/2 edge-interior-receiver 3/2))
    (beam-visible edge-interior-site 5/2 edge-interior-receiver 5/2)

    ;; Proper gate crossings retain structural LOS with exact conditional occluders.
    (bind (los-via
            closed-gate-site $closed-occluders closed-gate-receiver))
    (equal $closed-occluders '(closed-gate))
    (potentially-visible closed-gate-site closed-gate-receiver)
    (not (visible closed-gate-site closed-gate-receiver))
    (not (open closed-gate))
    (not (beam-visible closed-gate-site 4 closed-gate-receiver 4))
    (beam-visible closed-gate-site 5 closed-gate-receiver 5)

    (bind (los-via
            open-gate-site $open-occluders open-gate-receiver))
    (equal $open-occluders '(open-gate))
    (potentially-visible open-gate-site open-gate-receiver)
    (visible open-gate-site open-gate-receiver)
    (beam-visible open-gate-site 1 open-gate-receiver 1)
    (open open-gate)

    ;; A gate intersection at the gate segment's endpoint is deliberately strict.
    (bind (los-via
            gate-corner-site $corner-occluders gate-corner-receiver))
    (null $corner-occluders)
    (visible gate-corner-site gate-corner-receiver)

    ;; The exactly-half-unit candidate is included.  The farther and endpoint-
    ;; projected candidates must remain absent from this exact singleton list.
    (bind (los-via tolerance-left $tolerance-occluders tolerance-right))
    (equal $tolerance-occluders '(tolerance-edge))
    (bind (los-via tolerance-right $reverse-occluders tolerance-left))
    (equal $reverse-occluders '(tolerance-edge))
    (potentially-visible tolerance-left tolerance-right)
    (visible tolerance-left tolerance-right)

    ;; Coincident endpoints have no horizontal interior in which anything can occlude.
    (bind (los-via
            tolerance-left $coincident-occluders tolerance-endpoint))
    (null $coincident-occluders)

    ;; Both endpoints are inside the boundary, but their line leaves and re-enters through
    ;; the notch.  Both oriented crossing records are retained.  Ordinary sight is opaque;
    ;; a beam at the default top 6 blocks and one strictly above it clears.
    (potentially-visible boundary-left boundary-right)
    (potentially-visible boundary-right boundary-left)
    (not (visible boundary-left boundary-right))
    (not (visible boundary-right boundary-left))
    (bind (los-barrier-crossings>
            boundary-left $boundary-crossings boundary-right))
    (= (length $boundary-crossings) 2)
    (not (beam-visible boundary-left 6 boundary-right 6))
    (beam-visible boundary-left 7 boundary-right 7)
    (not (beam-visible boundary-right 6 boundary-left 6))
    (beam-visible boundary-right 7 boundary-left 7)

    ;; Gate midpoint/self-segment handling and the jammer-specific exclusions:
    ;; intervening locations do not enter target-gate or gun occluder lists.
    (bind (los-via target-site $target-occluders target-gate))
    (null $target-occluders)
    (visible target-site target-gate)
    (not (open target-gate))

    (bind (los-via gun-site $gun-occluders test-gun))
    (null $gun-occluders)
    (visible gun-site test-gun)

    ;; The state itself is unchanged by this zero-action characterization.
    (has-location idle-agent idle)
    (not (has-location derivation-enabler idle))))


(define-goal
  (beam-los-coordinates-scenarios-valid))

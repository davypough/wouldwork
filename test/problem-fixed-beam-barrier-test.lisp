;;; Filename: problem-fixed-beam-barrier-test.lisp

;;; Focused zero-action regression for coordinate-derived finite barriers on authored fixed
;;; COUPLED beams.  Parallel transmitter -> receiver lanes cross an undeclared wall, an
;;; explicitly shorter wall, an undeclared edge, the boundary polygon, and a closed gate.
;;; Equality with each barrier's own default top blocks; a strictly higher elevation clears
;;; -- wall and gate default to 4, edge to 3/2, and the anonymous boundary polygon to 6.
;;; The explicit wall height 1 is also honored.  Including beam-crossing exercises the
;;; former rejection path while keeping the parallel fixed beams from cutting one another.
;;; Expected minimum length: 0.

(in-package :ww)


(ww-set *problem-name* fixed-beam-barrier-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 0)


(define-types
  agent (idle-agent)
  location (idle-site)
  transmitter (wall-top-source
               wall-clear-source
               wall-override-source
               edge-clear-source
               boundary-top-source
               boundary-clear-source
               gate-top-source
               gate-clear-source)
  receiver (wall-top-receiver
            wall-clear-receiver
            wall-override-receiver
            edge-clear-receiver
            boundary-top-receiver
            boundary-clear-receiver
            gate-top-receiver
            gate-clear-receiver)
  wall (default-wall short-wall)
  edge (finite-edge)
  gate (height-gate)
  hue (red))


(include-tech beam-direct)
(include-tech beam-crossing)


(define-init
  (has-location idle-agent idle-site)
  (location-coords> idle-site 0 100)

  ;; The default wall crosses the first two lanes.  SHORT-WALL overrides height 4 with 1.
  (wall-segment> default-wall 5 -1 5 11)
  (wall-segment> short-wall 5 19 5 21)
  (has-height short-wall 1)
  (edge-segment> finite-edge 5 29 5 31)

  ;; Only the two y=40/42 lanes leave this closed polygon, crossing its right boundary.
  (boundary-wall ((-1 35) (11 35) (11 45) (-1 45) (-1 35)))

  ;; Both gate lanes name the same closed gate in BEAM-VIA.  Its exact segment crossing
  ;; replaces the legacy open-only test with finite-height clearance.
  (gate-segment> height-gate 5 49 5 61)

  (has-chroma wall-top-source red)
  (has-chroma wall-top-receiver red)
  (has-elevation wall-top-source 4)
  (has-elevation wall-top-receiver 4)
  (coupled wall-top-source wall-top-receiver)
  (beam-via wall-top-source () wall-top-receiver)
  (apparatus-coords> wall-top-source 0 0)
  (apparatus-coords> wall-top-receiver 20 0)

  (has-chroma wall-clear-source red)
  (has-chroma wall-clear-receiver red)
  (has-elevation wall-clear-source 5)
  (has-elevation wall-clear-receiver 5)
  (coupled wall-clear-source wall-clear-receiver)
  (beam-via wall-clear-source () wall-clear-receiver)
  (apparatus-coords> wall-clear-source 0 10)
  (apparatus-coords> wall-clear-receiver 20 10)

  (has-chroma wall-override-source red)
  (has-chroma wall-override-receiver red)
  (has-elevation wall-override-source 2)
  (has-elevation wall-override-receiver 2)
  (coupled wall-override-source wall-override-receiver)
  (beam-via wall-override-source () wall-override-receiver)
  (apparatus-coords> wall-override-source 0 20)
  (apparatus-coords> wall-override-receiver 20 20)

  (has-chroma edge-clear-source red)
  (has-chroma edge-clear-receiver red)
  (has-elevation edge-clear-source 5)
  (has-elevation edge-clear-receiver 5)
  (coupled edge-clear-source edge-clear-receiver)
  (beam-via edge-clear-source () edge-clear-receiver)
  (apparatus-coords> edge-clear-source 0 30)
  (apparatus-coords> edge-clear-receiver 20 30)

  (has-chroma boundary-top-source red)
  (has-chroma boundary-top-receiver red)
  (has-elevation boundary-top-source 6)
  (has-elevation boundary-top-receiver 6)
  (coupled boundary-top-source boundary-top-receiver)
  (beam-via boundary-top-source () boundary-top-receiver)
  (apparatus-coords> boundary-top-source 0 40)
  (apparatus-coords> boundary-top-receiver 20 40)

  (has-chroma boundary-clear-source red)
  (has-chroma boundary-clear-receiver red)
  (has-elevation boundary-clear-source 7)
  (has-elevation boundary-clear-receiver 7)
  (coupled boundary-clear-source boundary-clear-receiver)
  (beam-via boundary-clear-source () boundary-clear-receiver)
  (apparatus-coords> boundary-clear-source 0 42)
  (apparatus-coords> boundary-clear-receiver 20 42)

  (has-chroma gate-top-source red)
  (has-chroma gate-top-receiver red)
  (has-elevation gate-top-source 4)
  (has-elevation gate-top-receiver 4)
  (coupled gate-top-source gate-top-receiver)
  (beam-via gate-top-source (height-gate) gate-top-receiver)
  (apparatus-coords> gate-top-source 0 50)
  (apparatus-coords> gate-top-receiver 20 50)

  (has-chroma gate-clear-source red)
  (has-chroma gate-clear-receiver red)
  (has-elevation gate-clear-source 5)
  (has-elevation gate-clear-receiver 5)
  (coupled gate-clear-source gate-clear-receiver)
  (beam-via gate-clear-source (height-gate) gate-clear-receiver)
  (apparatus-coords> gate-clear-source 0 60)
  (apparatus-coords> gate-clear-receiver 20 60))


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!)))


(define-query fixed-beam-barrier-scenarios-valid ()
  (and
    (= (object-height default-wall) 4)
    (= (object-height short-wall) 1)
    (= (object-height finite-edge) 3/2)
    (= (object-height height-gate) 4)

    ;; Equality with the default wall top blocks; strictly above it clears.
    (not (fixed-beam-corridor-clear wall-top-source wall-top-receiver))
    (not (active wall-top-receiver))
    (fixed-beam-corridor-clear wall-clear-source wall-clear-receiver)
    (active wall-clear-receiver)

    ;; The explicit height override and the new finite-height edge rule are both live.
    (fixed-beam-corridor-clear wall-override-source wall-override-receiver)
    (active wall-override-receiver)
    (fixed-beam-corridor-clear edge-clear-source edge-clear-receiver)
    (active edge-clear-receiver)

    ;; Anonymous boundary segments use base 0, height 6, and inclusive top blocking.
    (not (fixed-beam-corridor-clear boundary-top-source boundary-top-receiver))
    (not (active boundary-top-receiver))
    (fixed-beam-corridor-clear boundary-clear-source boundary-clear-receiver)
    (active boundary-clear-receiver)

    ;; A closed gate follows the same finite-height rule when crossing geometry is known.
    (not (open height-gate))
    (not (fixed-beam-corridor-clear gate-top-source gate-top-receiver))
    (not (active gate-top-receiver))
    (fixed-beam-corridor-clear gate-clear-source gate-clear-receiver)
    (active gate-clear-receiver)

    ;; The initialization records prove clearance is using static geometry, not BEAM-VIA.
    (bind (los-barrier-crossings>
            wall-top-source $wall-crossings wall-top-receiver))
    (equal $wall-crossings '((:wall default-wall 1/4 5 -1 5 11)))
    (bind (los-barrier-crossings>
            edge-clear-source $edge-crossings edge-clear-receiver))
    (equal $edge-crossings '((:edge finite-edge 1/4 5 29 5 31)))
    (bind (los-barrier-crossings>
            boundary-top-source $boundary-crossings boundary-top-receiver))
    (equal $boundary-crossings '((:boundary 2 11/20 11 35 11 45)))
    (bind (los-barrier-crossings>
            gate-clear-source $gate-crossings gate-clear-receiver))
    (equal $gate-crossings '((:gate height-gate 1/4 5 49 5 61)))))


(define-goal
  (fixed-beam-barrier-scenarios-valid))

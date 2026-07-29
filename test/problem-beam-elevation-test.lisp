;;; Filename: problem-beam-elevation-test.lisp

;;; Targeted exercise for the relay-beam elevation interpolation added to beam-relay/
;;; visibility (-beam-occlusion, -beam-los-coordinates' BEAM-COORDINATES-ELEVATION-AT,
;;; visibility.lisp's BEAM-VISIBLE): the scenario that motivated the whole design --
;;; connector1 on the ground beaming to connector2 resting on a stack of boxes, with a low
;;; obstacle sitting between them.
;;;
;;; Sightlines here are hand-authored, not WALL-SEGMENTS-derived: connect-connector's initial
;;; PAIRED facts are checked at load time against literal LOS-TO-APPARATUS/LOS-TO-LOCATION
;;; entries in DEFINE-INIT (CHECK-INIT-PAIRED-SIGHTLINES), which run before any coordinate
;;; derivation would; a WALL-SEGMENTS-driven problem instead builds its PAIRED facts at solve
;;; time via connect-connector, never hand-authoring them in DEFINE-INIT.  low and high both
;;; get a direct, unoccluded LOS-TO-APPARATUS to their own transmitter/receiver; the
;;; interesting hop is connector1 -> connector2's own LOS-TO-LOCATION, which names mid as its
;;; one occluder candidate.
;;;
;;; The geometry.  low(0,0), high(10,0); connector1 sits at low with no support (elevation
;;; 0); connector2 sits at high on box1 (declared height 3, so connector2's own
;;; occupant-elevation is 3).  mid is at (5, 2/5) -- off the exact low-high line by 2/5, still
;;; within *beam-occlusion-tolerance*'s default 1/2.  Since the occluder entry itself is hand-
;;; authored here rather than derived, this doesn't exercise the tolerance-based derivation
;;; test (Stage 3's own corner-topo/claustro-topo regression covers that); it does confirm
;;; that beam-visible's live interpolation reads the same off-axis coordinates a derivation
;;; would have, not just exact endpoint values.  mid's projection parameter onto the low-high
;;; line is exactly 1/2 (the line is horizontal, so mid's y-offset doesn't move its x-axis
;;; projection).
;;;
;;; The beam's own interpolated elevation at mid is therefore 0 + 1/2*(3-0) = 3/2.  box2,
;;; sitting at mid with declared height 1, spans elevations [0,1] -- short of 3/2, so the
;;; sloped beam clears it.  This is the case that distinguishes real interpolation from a
;;; broken flat-elevation model: pin the beam to either endpoint's own elevation without
;;; interpolating (0, or 3) and box2 either wrongly blocks (pinned at 0) or the test stops
;;; discriminating; only computing 3/2 at mid, and only there, gets this right, and it is what
;;; makes receiver1 -- and therefore gate1, and therefore the goal -- reachable.
;;;
;;; Expected minimum solution (2 steps): walk agent1 low->mid, walk agent1 mid->goal.  gate1
;;; is open from t=0, since compute-connector-lighting/relay-beam-reaches-receiver light
;;; connector2 and activate receiver1 during INITIALIZE-DERIVED-STATE's own PROPAGATE-CHANGES!,
;;; before any action runs -- so unsolvability (or a "no route" search failure) is the signal
;;; that the interpolation regressed, not something requiring inspection of an intermediate
;;; state.


(in-package :ww)


(ww-set *problem-name* beam-elevation-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 4)


(defparameter *max-pairings* 2)  ;connector2 pairs to both connector1 and receiver1


;;;; TYPES ;;;;


(define-types
  agent (agent1)
  location (low mid high goal)
  transmitter (transmitter1)
  receiver (receiver1)
  connector (connector1 connector2)
  box (box1 box2)
  gate (gate1)
  hue (blue)
  mode (normal)
)


;;;; TECHNOLOGY INCLUDES ;;;;


(include-tech gate)          ;controls; energized; update-gate-status!
(include-tech beam-relay)    ;paired; color; compute-connector-lighting; relay-beam-reaches-receiver
(include-tech visibility)    ;los-to-apparatus; los-to-location; visible; beam-visible
(include-tech walkability)   ;walk-via; walkable-locations; walkable; walk


;;;; INITIALIZATION ;;;;


(define-init
  ;; Movable objects.  connector2 rests on box1; box1 and box2 sit directly on the ground at
  ;; their own locations (no ON fact for either).
  (has-location agent1 low)
  (has-location connector1 low)
  (has-location box1 high)
  (has-height box1 3)
  (has-location connector2 high)
  (on connector2 box1)
  (has-location box2 mid)
  (has-height box2 1)

  ;; Beam wiring: transmitter1 -> connector1 -> connector2 -> receiver1, pre-paired rather
  ;; than built by connect-connector, so the puzzle turns on the sightline geometry and
  ;; elevation, not on assembling the relay chain.  low/high each get a direct, unoccluded
  ;; sightline to their own apparatus; mid is the one occluder candidate, on the
  ;; connector1<->connector2 hop only.
  (has-chroma transmitter1 blue)
  (has-chroma receiver1 blue)
  (paired connector1 transmitter1)
  (paired connector2 connector1)
  (paired connector2 receiver1)
  (los-to-apparatus low () transmitter1)
  (los-to-apparatus high () receiver1)
  (los-to-location low (mid) high)

  ;; Coordinates for the low<->high beam and its one occluder candidate, mid -- all
  ;; beam-visible's live elevation interpolation needs (low/high's own sightlines to
  ;; transmitter1/receiver1 carry no occluder, so beam-coordinates-elevation-at is never
  ;; called for those two, and neither needs coordinates here).
  (location-coords> low 0 0)
  (location-coords> mid 5 2/5)
  (location-coords> high 10 0)

  ;; Walking topology: low -> mid direct, mid -> goal only once gate1 opens.
  (walk-via low () mid)
  (walk-via mid ((gate1)) goal)

  ;; gate1 opens once receiver1 activates.
  (controls ((receiver1)) gate1 normal)
)


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!))
)


;;;; GOAL ;;;;


(define-goal
  (has-location agent1 goal)
)

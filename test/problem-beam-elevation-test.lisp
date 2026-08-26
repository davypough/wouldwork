;;; Filename: problem-beam-elevation-test.lisp

;;; Targeted exercise for the relay-beam elevation interpolation added to beam-relay/
;;; visibility (-beam-occlusion, -beam-los-coordinates' BEAM-COORDINATES-ELEVATION-AT,
;;; visibility.lisp's BEAM-VISIBLE): the scenario that motivated the whole design --
;;; connector1 on the ground beaming to connector2 resting on a stack of boxes, with a low
;;; obstacle sitting between them.
;;;
;;; Sightlines here are hand-authored, not WALL-SEGMENT>-derived: connect-connector's initial
;;; PAIRED facts are checked at load time against literal LOS-VIA
;;; entries in DEFINE-INIT (CHECK-INIT-PAIRED-SIGHTLINES), which run before any coordinate
;;; derivation would; a WALL-SEGMENT>-driven problem instead builds its PAIRED facts at solve
;;; time via connect-connector, never hand-authoring them in DEFINE-INIT.  low and high both
;;; get a direct, unoccluded LOS-VIA to their own transmitter/receiver; the interesting hop
;;; is connector1 -> connector2's location-to-location LOS-VIA, which names mid as its
;;; one occluder candidate.
;;;
;;; The geometry.  low(0,0), high(10,0); connector1 sits at low with no support (elevation
;;; 0); connector2 sits at high on box1 (declared height 3, so connector2's own standing
;;; elevation is 3).  Both connectors explicitly declare height 1, putting their beam
;;; anchors at elevations 1 and 4 respectively.  mid is at (5, 2/5) -- off the exact low-high
;;; line by 2/5, still within *beam-occlusion-tolerance*'s default 1/2.  Since the occluder
;;; entry itself is hand-authored here rather than derived, this doesn't exercise the
;;; tolerance-based derivation test (Stage 3's own corner-topo/claustro-topo regression
;;; covers that); it does confirm that beam-visible's live interpolation reads the same
;;; off-axis coordinates a derivation would have, not just exact endpoint values.  mid's
;;; projection parameter onto the low-high line is exactly 1/2 (the line is horizontal, so
;;; mid's y-offset doesn't move its x-axis projection).
;;;
;;; The beam's own interpolated elevation at mid is therefore 1 + 1/2*(4-1) = 5/2.  box2,
;;; sitting at mid with declared height 2, spans elevations [0,2] -- short of 5/2, so the
;;; sloped beam clears it.  Using the connectors' standing elevations instead would put the
;;; beam at 3/2 and box2 would wrongly block it.  Thus the test specifically distinguishes
;;; connector-top anchors from the old connector-base anchors while continuing to exercise
;;; live elevation interpolation.  The correct result makes receiver1 -- and therefore
;;; gate1, and therefore the goal -- reachable.
;;;
;;; The isolated blocked lane uses the same midpoint projection with explicit height-1
;;; connectors, but connector4 stands on a height-2 box.  Its anchors are therefore 1 and 3,
;;; putting the beam exactly at elevation 2 over mid2.  box4 spans [0,2], so the inclusive
;;; upper boundary must block the beam: connector3 lights, but connector4 and receiver2 do
;;; not, gate2 stays closed, and blocked-goal stays unreachable.
;;;
;;; Expected minimum solution (1 step): MOBILITY-RESULTS closes over low->mid->goal, so MOVE
;;; takes agent1 directly from low to goal once gate1 is open.  gate1 is open from t=0, since
;;; compute-relay-lighting/relay-beam-reaches-receiver light connector2 and activate
;;; receiver1 during INITIALIZE-DERIVED-STATE's own PROPAGATE-CHANGES!, before any action runs
;;; -- while the goal's characterization query checks both lanes' intermediate derived state
;;; directly.


(in-package :ww)


(ww-set *problem-name* beam-elevation-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 1)


(ww-set *max-connector-pairings* 2)  ;connector2/4 each pair to a connector and receiver


;;;; TYPES ;;;;


(define-types
  agent (agent1 agent2)
  location (low mid high goal low2 mid2 high2 blocked-goal)
  transmitter (transmitter1 transmitter2)
  receiver (receiver1 receiver2)
  connector (connector1 connector2 connector3 connector4)
  box (box1 box2 box3 box4)
  gate (gate1 gate2)
  hue (blue red)
)


;;;; TECHNOLOGY INCLUDES ;;;;


(include-tech gate)          ;controls; energized; update-gate-status!
(include-tech beam-relay)    ;paired; color; compute-relay-lighting; relay-beam-reaches-receiver
(include-tech visibility)    ;los-via; visible; beam-visible
(include-tech walkability)   ;walking mode; mobility-results; traversable; move


(define-test-claim beam-occlusion-tolerance-default-contract
  (= *beam-occlusion-tolerance* 1/2)
  (= *boundary-wall-height* 6)
  (not (assoc '*beam-occlusion-tolerance* *problem-parameter-defaults*))
  (not (assoc '*boundary-wall-height* *problem-parameter-defaults*))
  (not (member '*beam-occlusion-tolerance* *persisted-problem-parameters*))
  (not (member '*boundary-wall-height* *persisted-problem-parameters*))
  (expect-condition
    (lambda ()
      (check-problem-parameter '*beam-occlusion-tolerance* 1/2))
    'error
    :containing "not a valid parameter name")
  (expect-condition
    (lambda ()
      (check-problem-parameter '*boundary-wall-height* 6))
    'error
    :containing "not a valid parameter name")
  (let ((display (with-output-to-string (*standard-output*)
                   (display-current-parameters))))
    (and (null (search "*BEAM-OCCLUSION-TOLERANCE* =>" display))
         (null (search "*BOUNDARY-WALL-HEIGHT* =>" display)))))


;;;; INITIALIZATION ;;;;


(define-init
  ;; Positive clearance lane.  connector2 rests on box1; both boxes sit on the ground.
  (has-location agent1 low)
  (has-location connector1 low)
  (has-height connector1 1)
  (has-location box1 high)
  (has-height box1 3)
  (has-location connector2 high)
  (has-height connector2 1)
  (on connector2 box1)
  (has-location box2 mid)
  (has-height box2 2)

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
  (los-via low () transmitter1)
  (los-via high () receiver1)
  (los-via low (mid) high)

  ;; Coordinates for the low<->high beam and its one occluder candidate, mid -- all
  ;; beam-visible's live elevation interpolation needs (low/high's own sightlines to
  ;; transmitter1/receiver1 carry no occluder, so beam-coordinates-elevation-at is never
  ;; called for those two, and neither needs coordinates here).
  (location-coords> low 0 0)
  (location-coords> mid 5 2/5)
  (location-coords> high 10 0)

  ;; Walking topology: low -> mid direct, mid -> goal only once gate1 opens.
  (traverse-via walking low () mid)
  (traverse-via walking mid ((gate1)) goal)

  ;; gate1 opens once receiver1 activates.
  (controls ((receiver1)) gate1 normal)

  ;; Boundary-blocked lane.  connector4's standing elevation is 2, so its explicit
  ;; height-1 anchor is 3; the beam is exactly elevation 2 at midpoint box4's height-2 top.
  (has-location agent2 low2)
  (has-location connector3 low2)
  (has-height connector3 1)
  (has-location box3 high2)
  (has-height box3 2)
  (has-location connector4 high2)
  (has-height connector4 1)
  (on connector4 box3)
  (has-location box4 mid2)
  (has-height box4 2)

  (has-chroma transmitter2 red)
  (has-chroma receiver2 red)
  (paired connector3 transmitter2)
  (paired connector4 connector3)
  (paired connector4 receiver2)
  (los-via low2 () transmitter2)
  (los-via high2 () receiver2)
  (los-via low2 (mid2) high2)

  (location-coords> low2 0 10)
  (location-coords> mid2 5 52/5)
  (location-coords> high2 10 10)

  (traverse-via walking low2 () mid2)
  (traverse-via walking mid2 ((gate2)) blocked-goal)
  (controls ((receiver2)) gate2 normal)
)


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!))
)


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query beam-elevation-scenarios-valid ()
  (and
    ;; Positive lane: connector-top anchors put the beam at 5/2, above box2.
    (has-location agent1 goal)
    (= (top connector1) 1)
    (= (top connector2) 4)
    (= (beam-elevation-at-location
         mid low (top connector1)
         high (top connector2))
       5/2)
    (not (beam-blocker-occludes-location mid 5/2))
    (beam-visible
      low (top connector1)
      high (top connector2))
    (color connector1 blue)
    (color connector2 blue)
    (active receiver1)
    (open gate1)
    (traversable agent1 low goal)

    ;; Blocked lane: the beam meets box4 exactly at its inclusive height-2 boundary.
    (has-location agent2 low2)
    (= (top connector3) 1)
    (= (top connector4) 3)
    (= (beam-elevation-at-location
         mid2 low2 (top connector3)
         high2 (top connector4))
       2)
    (beam-blocker-occludes-location mid2 2)
    (not (beam-visible
           low2 (top connector3)
           high2 (top connector4)))
    (color connector3 red)
    (not (exists (?h hue)
           (color connector4 ?h)))
    (not (active receiver2))
    (not (open gate2))
    (not (traversable agent2 low2 blocked-goal))))


(define-goal
  (beam-elevation-scenarios-valid))

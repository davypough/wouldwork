;;; Filename: problem-intersect.lisp

;;; Coordinate/topology-driven talos puzzle 'Intersections'.  Single floor; four gates
;;; passed in the forced order G1 -> G2 -> G3 -> G4; crossing beams cut each other.
;;; Analysis: doc/problems/intersect/problem-analysis.txt (rev 4).
;;; Spec record: doc/problems/intersect/spec-analysis.txt.


(in-package :ww)


(ww-set *problem-name* intersect)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *progress-reporting-interval* 1000000)
(ww-set *max-connector-pairings* 3)
(ww-set *symmetry-pruning* t)  ;connectors are interchangeable
(ww-set *depth-cutoff* 7)


;;; Diagram names -> spec names:  Ln -> locationn;  wn -> walln;  WINn -> windown;
;;; Gn -> gaten;  T1b/T2r -> transmitter1/2;  R1b/R2r/R3b/R4r -> receiver1..4;
;;; P1 -> plate1;  LD1 -> ladder1;  C1..C3 -> connector1..3.
(define-types
  agent (agent1)
  gate (gate1 gate2 gate3 gate4)
  wall (wall1 wall2 wall3 wall4 wall5 wall6 wall7 wall8 wall9 wall10)
  window (window1 window2 window3 window4)
  location (location1 location2 location3 location4 location5 location6 location7 location8 location9)
  pressure-plate (plate1)
  connector (connector1 connector2 connector3)
  transmitter (transmitter1 transmitter2)
  receiver (receiver1 receiver2 receiver3 receiver4)
  ladder (ladder1)
  hue (blue red)
)


(include-tech gate)
(include-tech plate)
(include-tech step)  ;agent can step onto plate1
(include-tech ladder)
(include-tech beam-relay)
(include-tech beam-crossing)
(include-tech walkability)
(include-tech visibility)


;;;; INITIALIZATION ;;;;


(define-init
  ;; Movable objects: everything starts unlinked
  (has-location agent1 location1)
  (has-location connector1 location1)
  (has-location connector2 location2)
  (has-location connector3 location3)

  ;; Fixed-position objects
  (has-position plate1 location6)
  (has-position ladder1 location3)

  ;; Representative location coordinates
  (location-coords> location1 15 5)
  (location-coords> location2 21 6)
  (location-coords> location3 24 8)
  (location-coords> location4 15 16)
  (location-coords> location5 4 8)
  (location-coords> location6 1 7)
  (location-coords> location7 3 4)
  (location-coords> location8 15 8)
  (location-coords> location9 10 8)

  ;; Exact beam-fixture coordinates.  Each fixture faces one way, so the 1/10 offsets
  ;; place it on its facing side of the adjacent wall.
  (apparatus-coords> transmitter1 15 1/10)     ;faces north
  (apparatus-coords> transmitter2 7 149/10)    ;faces south
  (apparatus-coords> receiver1 13 59/10)       ;faces south
  (apparatus-coords> receiver2 229/10 5)       ;faces west
  (apparatus-coords> receiver3 15 199/10)      ;faces south
  (apparatus-coords> receiver4 1/10 8)         ;faces east

  ;; Apparatus properties
  (has-chroma transmitter1 blue)
  (has-chroma transmitter2 red)
  (has-chroma receiver1 blue)
  (has-chroma receiver2 red)
  (has-chroma receiver3 blue)
  (has-chroma receiver4 red)

  ;; Gate controllers.  Each gate is open only while its condition holds; gate4 needs
  ;; receiver4 active AND plate1 depressed.
  (controls ((receiver1)) gate1 normal)
  (controls ((receiver2)) gate2 normal)
  (controls ((receiver3)) gate3 normal)
  (controls ((receiver4 plate1)) gate4 normal)

  ;; Boundary wall.  The repeated final point explicitly closes the polygon.
  (boundary-wall ((0 0) (29 0) (29 20) (0 20) (0 0)))

  ;; Opaque internal walls
  (wall-segment> wall1 5 0 5 6)
  (wall-segment> wall2 8 3 8 6)
  (wall-segment> wall3 8 6 14 6)
  (wall-segment> wall4 17 0 17 6)
  (wall-segment> wall5 23 4 23 6)
  (wall-segment> wall6 23 10 23 15)
  (wall-segment> wall7 17 15 23 15)
  (wall-segment> wall8 5 15 13 15)
  (wall-segment> wall9 5 10 5 15)
  (wall-segment> wall10 5 10 8 10)

  ;; Windows: beams pass, agents and reach do not
  (window-segment> window1 23 6 23 10)
  (window-segment> window2 13 15 17 15)
  (window-segment> window3 8 6 8 10)
  (window-segment> window4 5 6 5 10)

  ;; Gate geometry
  (gate-segment> gate1 14 6 17 6)
  (gate-segment> gate2 23 0 23 4)
  (gate-segment> gate3 18 15 18 20)
  (gate-segment> gate4 0 6 5 6)

  ;; Ladder LD1: one-way climb from location3 up to location2, empty-handed only
  (traverse-via> location3 ((ladder1)) location2)
)


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!))
)


;;;; SEARCH HOOKS ;;;;


;;; Placeholder estimate of steps still needed, never too high: at least one move
;;; while the agent is not yet at location7.  A stronger bound is a search-advisor question.
(define-query min-steps-remaining? ()
  (if (has-location agent1 location7)
    0
    1))


;;;; GOAL ;;;;


(define-goal
  (has-location agent1 location7)
)

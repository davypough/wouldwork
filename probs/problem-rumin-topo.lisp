;;; Filename: problem-rumin-topo.lisp

;;; Coordinate/topology-driven version of Purgatory 'Rumination'.


(in-package :ww)


(ww-set *problem-name* rumin-topo)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *progress-reporting-interval* 1000000)

(ww-set *symmetry-pruning* t)

(ww-set *depth-cutoff* 34)


(defparameter *max-pairings* 2)


;;;; TYPES ;;;;


(define-types
  agent (agent1 agent1*)
  recorder (recorder1)
  gate  (gate1 gate2 gate3 gate4 gate5)
  wall (wall1 wall2 wall3 wall4 wall5 wall6 wall7 wall8 wall9 wall10 wall11 wall12 wall13)
  edge (edge1 edge2 edge3 edge4)
  location (location1 location2 location3 location4 location5 location6 location7
            location8 location9 location10 location11 location12 location13)
  pressure-plate (plate1 plate2 plate3)
  box (box1 box1*)
  connector (connector1 connector2 connector1* connector2*)
  tray (tray1 tray1*)
  transmitter (transmitter1 transmitter2)
  receiver (receiver1 receiver2)
  ladder (ladder1)
  hue (blue red)
)


;;;; TECHNOLOGY INCLUDES ;;;;


(include-tech gate)
(include-tech plate)
(include-tech elevation)
(include-tech tray)
(include-tech box)
(include-tech recorder)
(include-tech ladder)
(include-tech stairs)
(include-tech step)
(include-tech jump)
(include-tech beam-relay)
(include-tech walkability)
(include-tech visibility)
(include-tech reachability)

;;;; INITIALIZATION ;;;;


(define-init
  ;; Movable objects
  (has-location agent1 location1)
  (has-location agent1* location1)
  (has-location connector1 location1)
  (has-location connector1* location1)
  (has-location connector2 location10)
  (has-location connector2* location10)
  (has-location box1 location7)
  (has-location box1* location7)
  (has-location tray1 location4)
  (has-location tray1* location4)

  ;; Ghost definitions
  (recording-copy> agent1 agent1*)
  (recording-copy> connector1 connector1*)
  (recording-copy> connector2 connector2*)
  (recording-copy> box1 box1*)
  (recording-copy> tray1 tray1*)

  ;; Fixed-position objects and initial support occupancy
  (has-position plate1 location6)
  (has-position plate2 location9)
  (has-position plate3 location12)
  (has-position ladder1 location5)
  (has-position recorder1 location3)

  ;; Representative location coordinates
  (location-coords> location1 8 14)
  (location-coords> location2 6 9)
  (location-coords> location3 6 5)
  (location-coords> location4 8 9)
  (location-coords> location5 17 151/10)
  (location-coords> location6 22 16)
  (location-coords> location7 25 16)
  (location-coords> location8 241/10 6)
  (location-coords> location9 23 6)
  (location-coords> location10 20 5)
  (location-coords> location11 31 10)
  (location-coords> location12 32 5)
  (location-coords> location13 16 10)

  ;; Exact beam-fixture coordinates.  The 1/10 offsets place each fixture
  ;; unambiguously on the intended side of its adjacent boundary.
  (apparatus-coords> transmitter1 69/10 17)
  (apparatus-coords> transmitter2 69/10 2)
  (apparatus-coords> receiver1 239/10 9)
  (apparatus-coords> receiver2 31 41/10)

  ;; Nondefault elevations.  Other locations, gates, and screens default to 0.
  ;; Transmitters and receivers default to elevation 1.
  (has-elevation location4 3/2)
  (has-elevation location9 3/2)
  (has-elevation location10 3/2)
  (has-elevation gate4 3/2)
  (has-elevation receiver1 2)

  ;; Nondefault heights.
  (has-height wall10 3/2)
  (has-height wall11 3/2)
  (has-height wall12 3/2)
  (has-height wall13 3/2)

  ;; Gate controllers
  (controls ((receiver1)) gate1 normal)
  (controls ((receiver1)) gate2 inverted)
  (controls ((plate1)) gate3 normal)
  (controls ((plate2)) gate4 normal)
  (controls ((receiver2 plate3)) gate5 normal)

  ;; Apparatus properties
  (has-chroma transmitter1 blue)
  (has-chroma transmitter2 red)
  (has-chroma receiver1 blue)
  (has-chroma receiver2 red)

  ;; Boundary wall.  The repeated final point explicitly closes the polygon.
  (boundary-wall
    ((0 19) (7 19) (7 15) (16 15) (16 18) (26 18) (26 15) (33 15)
     (33 11) (35 11) (35 8) (33 8) (33 4)
     (30 4) (30 6) (27 6) (27 4) (7 4) (7 0) (0 0)
     (0 19)))

  ;; Opaque internal wall/edge
  (wall-segment> wall1 7 11 7 13)
  (wall-segment> wall2 7 4 7 8)
  (wall-segment> wall3 9 12 9 15)
  (wall-segment> wall4 16 15 19 15)
  (wall-segment> wall5 19 12 19 15)
  (wall-segment> wall6 19 4 19 7)
  (wall-segment> wall7 19 7 24 7)
  (wall-segment> wall8 23 12 23 15)
  (wall-segment> wall9 23 15 26 15)
  (wall-segment> wall10 6 16 7 16)
  (wall-segment> wall11 6 16 6 18)
  (wall-segment> wall12 6 3 7 3)
  (wall-segment> wall13 6 0 6 3)
  (edge-segment> edge1 7 8 7 11)
  (edge-segment> edge2 9 11 11 11)
  (edge-segment> edge3 7 8 11 8)
  (edge-segment> edge4 24 4 24 7)

  ;; Gate geometry
  (gate-segment> gate1 19 7 19 12)
  (gate-segment> gate2 19 12 23 12)
  (gate-segment> gate3 23 15 23 18)
  (gate-segment> gate4 22 4 22 7)
  (gate-segment> gate5 33 8 33 11)

  ;; One-way ladder traversal
  (climb-via> location5 (ladder1) location4)

  ;; Authorized elevation changes
  (jump-via location8 () location9)
  (stairs-via location1 () location4)
  (stairs-via location4 () location13)

  ;; Nearby manipulation across boundaries
  (reach-via location4 () location2)
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
  (open gate5)
)

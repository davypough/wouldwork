;;; Filename: problem-rumin-topo.lisp

;;; Coordinate/topology-driven version of Purgatory 'Rumination'.


(in-package :ww)


(ww-set *problem-name* rumin-topo)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *progress-reporting-interval* 1000000)
(ww-set *max-recorder-cycles* 5)  ;at most 5 total START-RECORDER actions, one per subgoal
(ww-set *max-connector-pairings* 2)
(ww-set *depth-cutoff* 15)


(define-types
  agent (agent1 agent1*)
  recorder (recorder1)
  gate  (gate1 gate2 gate3 gate4 gate5 gate6)
  wall (wall1 wall2 wall3 wall4 wall5 wall7 wall8 wall9 wall10 wall11 wall12 wall13 wall14 wall15 wall16)
  window (window1)
  edge (edge1 edge2 edge3 edge4 edge5)
  location (location1 location2 location3 location4 location5
            location6 location7 location8 location9 location10
            location11 location12 location13 location14 location15 location16 location17)
  pressure-plate (plate1 plate2 plate3 plate4)
  box (box1 box1*)
  connector (connector1 connector2 connector1* connector2*)
  tray (tray1 tray1*)
  transmitter (transmitter1 transmitter2)
  receiver (receiver1 receiver2)
  ladder (;ladder1
          ladder2)
  hue (blue red)
)


(include-tech gate)
(include-tech plate)
(include-tech elevation)
(include-tech tray)
(include-tech box)
(include-tech recorder)
(include-tech stairs)
(include-tech ladder)
(include-tech step)
(include-tech jump)
(include-tech beam-relay)
(include-tech walkability)
(include-tech visibility)
(include-tech reachability)
(include-tech topo-lower-bound)  ;admissible finite-resource bound; prunes on the cutoff before any solution exists


;;;; INITIALIZATION ;;;;


(define-init
  ;; Movable objects.  The asterisk names in DEFINE-TYPES above are the recording copies:
  ;; the recorder derives each RECORDING-COPY> pair from them, so no mapping is declared
  ;; here.  Ghosts have no initial location either -- START-RECORDER forks each one from its
  ;; live counterpart's current state when the search finds it, per rule 5, and a ghost does
  ;; not exist beforehand.
  (has-location agent1 location3)
  (has-location connector1 location13)
  (has-location connector2 location10)
  (has-location box1 location7)
  (has-location tray1 location3)
  (paired connector1 receiver1)

  ;; Fixed-position objects and initial support occupancy
  (has-position plate1 location6)
  (has-position plate2 location9)
  (has-position plate3 location12)
  (has-position plate4 location15)
  ;(has-position ladder1 location5)
  (has-position ladder2 location14)
  (has-position recorder1 location3)

  ;; Representative location coordinates.  The optional third coordinate is the
  ;; location's own level, default 0.
  (location-coords> location1 21 10)
  (location-coords> location2 6 9)
  (location-coords> location3 6 5)
  (location-coords> location4 8 10 3/2)
  (location-coords> location5 36 9)
  (location-coords> location6 22 16)
  (location-coords> location7 25 16)
  (location-coords> location8 241/10 6)
  (location-coords> location9 23 6 3/2)
  (location-coords> location10 20 5)
  (location-coords> location11 31 10)
  (location-coords> location12 32 5)
  (location-coords> location13 11 9 3/2)
  (location-coords> location14 349/10 9)
  (location-coords> location15 34 2 2)
  (location-coords> location16 31 2 2)
  (location-coords> location17 18 5)

  ;; Exact beam-fixture coordinates.  The 1/10 offsets place each fixture
  ;; unambiguously on the intended side of its adjacent boundary.
  (apparatus-coords> transmitter1 69/10 17 3/2)
  (apparatus-coords> transmitter2 69/10 2 3/2)
  (apparatus-coords> receiver1 13 149/10 3/2)
  (apparatus-coords> receiver2 31 41/10)

  ;; Nondefault heights.
  (has-height wall10 3/2)
  (has-height wall11 3/2)
  (has-height wall12 3/2)
  (has-height wall13 3/2)
  (has-height edge5 2)

  ;; Gate controllers
  (controls ((receiver1)) gate1 normal)
  (controls ((receiver1)) gate2 inverted)
  (controls ((plate1)) gate3 normal)
  (controls ((plate2)) gate4 normal)
  (controls ((receiver2 plate3)) gate5 normal)
  (controls ((plate4)) gate6 normal)

  ;; Apparatus properties
  (has-chroma transmitter1 blue)
  (has-chroma transmitter2 red)
  (has-chroma receiver1 blue)
  (has-chroma receiver2 red)

  ;; Boundary wall.  The repeated final point explicitly closes the polygon.
  (boundary-wall
    ((0 19) (7 19) (7 15) (16 15) (16 18) (26 18) (26 15) (33 15)
     (33 11) (38 11) (38 1) (30 1) (30 6) (27 6) (27 4) (7 4) (7 0) (0 0) (0 19)))

  ;; Opaque internal wall/edge
  (wall-segment> wall1 7 11 7 13)
  (wall-segment> wall2 7 4 7 8)
  (wall-segment> wall3 10 11 10 15)
  (wall-segment> wall4 16 15 19 15)
  (wall-segment> wall5 19 12 19 15)
  ;(wall-segment> wall6 19 4 19 7)
  (wall-segment> wall7 19 7 24 7)
  (wall-segment> wall8 23 12 23 15)
  (wall-segment> wall9 23 15 26 15)
  (wall-segment> wall10 6 16 7 16)
  (wall-segment> wall11 6 16 6 18)
  (wall-segment> wall12 6 3 7 3)
  (wall-segment> wall13 6 0 6 3)
  (wall-segment> wall14 33 8 35 8)
  (wall-segment> wall15 33 4 33 8)
  (wall-segment> wall16 30 4 33 4)
  (edge-segment> edge1 7 8 7 11)
  (edge-segment> edge2 10 11 12 11)
  (edge-segment> edge3 7 8 12 8)
  (edge-segment> edge4 24 4 24 7)
  (edge-segment> edge5 35 8 35 11)

  ;; Gate geometry
  (gate-segment> gate1 19 7 19 12)
  (gate-segment> gate2 19 12 23 12)
  (gate-segment> gate3 23 15 23 18)
  (gate-segment> gate4 22 4 22 7 3/2)
  (gate-segment> gate5 33 8 33 11)
  (gate-segment> gate6 33 1 33 4)

  (window-segment> window1 19 4 19 7)

  ;; Authorized elevation changes
  (traverse-via jumping location8 () location9)
  ;(traverse-via jumping location2 () location4)
  (traverse-via stairway location2 () location4)
  (traverse-via stairway location9 () location10)
  ;(traverse-via> climbing location5 ((ladder1)) location13)
  (traverse-via> climbing location14 ((ladder2)) location5)

  ;; Nearby manipulation across boundaries
  (reach-via location4 () location2)
  (reach-via location5 () location14)  ;across the loc15 ledge; the vertical tests bound each direction
)


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!))
)


;;;; GOAL ;;;;

;; Leave GHOST-STOPS-RECORDER disabled to test immediate satisfaction, where
;; the final recorder cycle may remain open.  Enable it to test explicit ghost
;; completion, where the final cycle must be closed.
(define-goal
  (and (has-location agent1 location16)
       (ghost-stops-recorder)
  )
)


;; Initial state
;;  (and (has-location agent1 location3) (has-location tray1 location3) (has-location connector1 location13) (paired connector1 receiver1) (has-location connector2 location10) (has-location box1 location7)

;; First subgoal -- use recorder to open gate1, get box1 to loc8, return with tray1 to loc3
;; (and (has-location agent1 location3) (has-location tray1 location3) (has-location connector1 location3) (has-location connector2 location10) (has-location box1 location8))

;; Second subgoal -- setup for reopening gate1
;; (and (has-location agent1 location3) (has-location tray1 location3) (has-location connector1 location13) (paired connector1 receiver1) (has-location connector2 location10) (has-location box1 location8))

;; Third subgoal -- use recorder to reopen gate1, relocate conn2 and box1, return tray1 to loc3
;; (and (has-location agent1 location17) (has-location tray1 location3) (has-location connector1 location13) (has-location connector2 location17) (has-location box1 location17))

;; Third subgoal -- setup for red path
;; (and (has-location connector2 location17) (paired connector2 receiver1) (has-location box1 location2) (has-location agent1 location3) (has-location tray1 location3) (has-location connector1 location13)

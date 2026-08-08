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


;;;; TYPES ;;;;


(define-types
  agent (agent1)
  recorder (recorder1)
  gate  (gate1 gate2 gate3 gate4 gate5)
  wall (wall1 wall2 wall3 wall4 wall5 wall6 wall7 wall8 wall9)
  edge (edge1 edge2 edge3 edge4 edge5 edge6 edge7 edge8)
  location (location1 location2 location3 location4 location5 location6 location7
            location8 location9 location10 location11 location12)
  pressure-plate (plate1 plate2 plate3)
  box (box1)
  connector (connector1 connector2)
  tray (tray1)
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
(include-tech jump)
(include-tech walkability)
(include-tech visibility)
(include-tech ladder)


;;;; INITIALIZATION ;;;;


(define-init
  ;; Movable objects
  (has-location agent1 location1)
  (has-location connector1 location1)
  (has-location connector2 location10)
  (has-location box1 location7)

  ;; Fixed-position objects and initial support occupancy
  (has-position plate1 location6)
  (has-position plate2 location9)
  (has-position plate3 location12)
  (has-position ladder1 location5)

  ;; Representative location coordinates
  (location-coords> location1 8 14)
  (location-coords> location2 6 9)
  (location-coords> location3 6 5)
  (location-coords> location4 10 10)
  (location-coords> location5 17 151/10)
  (location-coords> location6 22 16)
  (location-coords> location7 25 16)
  (location-coords> location8 241/10 6)
  (location-coords> location9 23 6)
  (location-coords> location10 20 5)
  (location-coords> location11 31 10)
  (location-coords> location12 32 5)

  ;; Nondefault elevations.  Other locations, gates, and screens default to 0.
  ;; Transmitters and receivers default to elevation 1.
  (has-elevation location4 1)
  (has-elevation location9 2)
  (has-elevation location10 2)
  (has-elevation gate4 2)
  (has-elevation edge1 1)
  (has-elevation edge2 1)
  (has-elevation edge3 1)
  (has-elevation edge4 1)
  (has-elevation edge5 1)
  (has-elevation edge6 1)
  (has-elevation edge7 1)
  (has-elevation edge8 2)
  (has-elevation receiver1 2)

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
  (edge-segment> edge1 6 16 7 16)
  (edge-segment> edge2 6 16 6 18)
  (edge-segment> edge3 6 3 7 3)
  (edge-segment> edge4 6 0 6 3)
  (edge-segment> edge5 7 8 7 11)
  (edge-segment> edge6 9 11 11 11)
  (edge-segment> edge7 7 8 11 8)
  (edge-segment> edge8 24 4 24 7)

  ;; Gate geometry
  (gate-segment> gate1 16 5 16 10)
  (gate-segment> gate2 16 99/10 24 99/10)
  (gate-segment> gate3 16 101/10 24 101/10)
  (gate-segment> gate4 24 101/10 33 101/10)
  (gate-segment> gate5 16 10 16 17)
  (gate-segment> gate6 101/10 10 101/10 17)
  (gate-segment> gate7 99/10 10 99/10 17)
  (gate-segment> gate8 51/10 10 51/10 17)
  (gate-segment> gate9 49/10 10 49/10 17)

  ;; Screen1 lies south of gate4, preserving the area2 -> area3
  ;; traversal order gate4, then screen1.
  (screen-segment> screen1 24 99/10 33 99/10)

  ;; Exact beam-fixture coordinates.  The 1/10 offsets place each fixture
  ;; unambiguously on the intended side of its adjacent boundary.
  (apparatus-coords> transmitter1 111/10 9)
  (apparatus-coords> receiver1 239/10 9)

  ;; Directional jamming exclusions
  (jam-disallowed> location1 location7 gate1)
  (jam-disallowed> location7 location1 gate4)

  ;; One-way ladder traversal contributed to the mobility closure.
  (climb-via> location7 (ladder1) location1)

  ;; Authored elevation changes
  (jump-via location10 () location12)
  (jump-via location13 () location11)

  ;; Nearby manipulation across boundaries
  (reach-via location1 () location7)
  (reach-via location2 (gate2 gate3) location3)
  ;(reach-via location4 () location5)  ;removing these 2 reaches increases solution by one step,
  ;(reach-via location5 () location6)  ;but reduces search time by 12%

  ;; Beam corridor.  Hand-authored, not coordinate-derived: -beam-los-coordinates.lisp only
  ;; derives LOS-TO-APPARATUS/LOS-TO-LOCATION (per-location sightline occluders), not
  ;; BEAM-VIA's own gate+location corridor list, so beam-direct still needs this listed
  ;; explicitly.  transmitter1 (111/10 9) -> receiver1 (239/10 9) runs along y=9; gate1
  ;; (x=16, y 5-10) crosses it, and location2 (20 9) sits exactly on it, so a beam-blocker
  ;; placed at location2 can occlude the beam.
  (beam-via transmitter1 (gate1 location2) receiver1)
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
  (has-location agent1 location11)
)

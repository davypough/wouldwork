;;; Filename: problem-crelay-topo-1.lisp

;;; Coordinate/topology-driven version of Purgatory 'Connector Relay' problem.

;;; Design notes:
;;;   - Solved as a chain of milestones, each a short min-length search with SOLVE-SUBGOAL;
;;;     a single search over the whole plan (some 80 moves) is far out of reach.
;;;   - A milestone names a resting state that later steps depend on (an object on a plate,
;;;     a gate held open), plus (recorder-cycle-ended) when the next leg needs the recorder.
;;;   - Each SOLVE-SUBGOAL may use exactly one new recorder cycle; the recorder cap stays
;;;     unlimited so the chain as a whole can use as many as it needs.
;;;   - The depth cutoff is a per-leg default of about one searchable leg; raise it at the
;;;     REPL for a longer leg.
;;;   - The generic lower bound reads the active subgoal, so it prunes every leg.


(in-package :ww)


(ww-set *problem-name* crelay-topo-1)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *max-recorder-cycles* nil)
(ww-set *max-connector-pairings* 3)
(ww-set *depth-cutoff* 12)


(define-types
  agent (agent1 agent1*)
  recorder (recorder1)
  gate  (gate1 gate2 gate3 gate4 gate5 gate6 gate7 gate8 gate9)
  wall (wall1 wall2 wall3 wall4 wall5 wall6 wall7 wall8 wall9 wall10 wall11 wall12 wall13)
  edge (edge1)
  location (location1 location2 location3 location4 location5
            location6 location7 location8 location9 location10
            location11 location12 location13 location14 location15 location16 location17
            location18 location19 location20 location21)
  pressure-plate (plate1 plate2 plate3 plate4 plate5 plate6 plate7 plate8)
  box (box1 box1*)
  connector (connector1 connector1*)
  tray (tray1 tray1*)
  transmitter (transmitter1)
  receiver (receiver1)
  floor-repeater (repeater1)
  floor-blower (blower1)
  switch (switch1 switch2)
  ladder (ladder1 ladder2 ladder3)
  screen (screen1)
  hue (red))


(include-tech gate)
(include-tech plate)
(include-tech elevation)
(include-tech tray)
(include-tech box)
(include-tech switch)
(include-tech floor-blower)
(include-tech ladder)
(include-tech step)
(include-tech jump)
(include-tech beam-relay)
(include-tech walkability)
(include-tech visibility)
(include-tech reachability)
(include-tech topo-lower-bound)  ;admissible bound on the active (sub)goal; prunes on the cutoff
(include-tech recorder)


;;;; INITIALIZATION ;;;;


(define-init
  ;; Movable objects.  The asterisk names in DEFINE-TYPES above are the recording copies:
  ;; the recorder derives each RECORDING-COPY> pair from them, so no mapping is declared
  ;; here.  Ghosts have no initial location either -- START-RECORDER forks each one from its
  ;; live counterpart's current state when the search finds it, per rule 5, and a ghost does
  ;; not exist beforehand.
  (has-location agent1 location1)
  (has-location connector1 location9)
  (has-location box1 location6)
  (has-location tray1 location7)

  ;; Fixed-position objects and initial support occupancy
  (has-position plate1 location2)
  (has-position plate2 location7)
  (has-position plate3 location10)
  (has-position plate4 location13)
  (has-position plate5 location12)
  (has-position plate6 location16)
  (has-position plate7 location17)
  (has-position plate8 location18)
  (has-position ladder1 location3)
  (has-position ladder2 location8)
  (has-position ladder3 location11)
  (has-position recorder1 location1)
  (has-position blower1 location4)
  (on tray1 plate2)
  (aimed-at blower1 location20)

  ;; Representative location coordinates.  The optional third coordinate is the
  ;; location's own level, default 0.
  (location-coords> location1 2 10)
  (location-coords> location2 2 65/10)
  (location-coords> location3 31/10 3)
  (location-coords> location4 75/10 1)
  (location-coords> location5 65/10 1)
  (location-coords> location6 75/10 -19/10 3/2)  ;deep enough in the alcove to be out of reach from location4/5/20
  (location-coords> location7 14 65/10)
  (location-coords> location8 151/10 3)
  (location-coords> location9 19 115/10)
  (location-coords> location10 27 9)
  (location-coords> location11 26 131/10)
  (location-coords> location12 27 17)
  (location-coords> location13 27 18)
  (location-coords> location14 295/10 23)
  (location-coords> location15 19 26)
  (location-coords> location16 9 25)
  (location-coords> location17 10 25)
  (location-coords> location18 11 25)
  (location-coords> location19 10 20)
  (location-coords> location20 75/10 1 3/2)
  (location-coords> location21 165/10 27)

  ;; Exact fixture coordinates.  The 1/10 offsets place each fixture
  ;; unambiguously on the intended side of its adjacent boundary.
  (apparatus-coords> transmitter1 299/10 115/10)
  (apparatus-coords> receiver1 19 279/10)
  (apparatus-coords> repeater1 19 18)
  (apparatus-coords> switch1 75/10 1/10)
  (apparatus-coords> switch2 295/10 239/10)

  ;; Nondefault heights.
  (has-height wall9 1/2)
  (has-height wall10 1)
  (has-height edge1 3/2)

  ;; Controllers
  (controls ((receiver1)) gate8 normal)
  (controls ((plate1)) gate1 normal)
  (controls ((plate2)) gate3 normal)
  (controls ((plate3)) gate4 normal)
  (controls ((plate4 plate5)) gate6 normal)
  (controls ((plate6 plate7 plate8)) gate9 normal)
  (controls ((switch1)) blower1 normal)
  (controls ((switch1)) gate2 inverted)
  (controls ((switch2)) gate5 inverted)
  (controls ((switch2)) gate7 normal)

  ;; Apparatus properties
  (has-chroma transmitter1 red)
  (has-chroma receiver1 red)

  ;; Boundary wall.  The repeated final point explicitly closes the polygon.
  (boundary-wall
    ((0 24) (0 0) (6 0) (6 -2) (9 -2) (9 0) (15 0) (15 2)
     (28 2) (28 10) (31 10) (31 24) (25 24) (25 28) (8 28) (8 24) (0 24)))

  ;; Opaque internal wall/edge
  (wall-segment> wall1 3 8 3 24)
  (wall-segment> wall2 3 0 3 5)
  (wall-segment> wall3 3 13 17 13)
  (wall-segment> wall4 15 8 15 13)
  (wall-segment> wall5 15 2 15 5)
  (wall-segment> wall6 25 13 31 13)
  (wall-segment> wall7 17 13 17 24)
  (wall-segment> wall8 21 13 21 24)
  (wall-segment> wall9 17 16 21 16)
  (wall-segment> wall10 17 20 21 20)
  (wall-segment> wall11 28 13 28 16)
  (wall-segment> wall12 28 19 28 24)
  (wall-segment> wall13 12 24 17 24)
  (edge-segment> edge1 6 0 9 0)  ;its 3/2-high top supports gate2

  ;; Gate geometry
  (gate-segment> gate1 3 5 3 8)
  (gate-segment> gate2 6 0 9 0 3/2)  ;elevated directly above edge1
  (gate-segment> gate3 15 5 15 8)
  (gate-segment> gate4 28 10 28 13)
  (gate-segment> gate5 21 13 25 13)
  (gate-segment> gate6 28 16 28 19)  ;co-located with screen1: both must pass
  (gate-segment> gate7 21 24 25 24)
  (gate-segment> gate8 17 24 17 28)
  (gate-segment> gate9 8 24 12 24)
  (screen-segment> screen1 28 16 28 19)

  ;; Authorized elevation changes.  location6's alcove lies behind edge1, which carries
  ;; gate2 on its top.  location20 is the top of blower1's stream, so the drop to
  ;; location5 beside it names the stream's drive.
  (traverse-via location4 ((edge1 gate2)) location6)
  (traverse-via location5 ((edge1 gate2)) location6)
  (traverse-via location20 ((edge1 gate2)) location6)
  (traverse-via location20 ((blower1)) location5)
  (traverse-via> location3 ((ladder1)) location1)
  (traverse-via> location8 ((ladder2)) location5)
  (traverse-via> location11 ((ladder3)) location10)
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
  (has-location agent1 location19))

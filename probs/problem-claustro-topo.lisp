;;; Filename: problem-claustro-topo.lisp

;;; Coordinate-driven version of Talos 'Claustrophobia'.
;;; The fixed BEAM-VIA corridor remains authored, while raw planar geometry derives its
;;; finite-barrier crossings, the visibility tables, and walking topology.  Jumping, climbing,
;;; reachability, and directional jamming exclusions remain explicitly authored
;;; because they are not determined by planar geometry alone.


(in-package :ww)


(ww-set *problem-name* claustro-topo)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *progress-reporting-interval* 1000000)

(ww-set *symmetry-pruning* t)

(ww-set *depth-cutoff* 34)


;;;; TYPES ;;;;


(define-types
  agent (agent1)
  gate  (gate1 gate2 gate3 gate4 gate5 gate6 gate7 gate8 gate9)
  screen (screen1)
  wall (wall1 wall2 wall3)
  edge (edge1)
  window (window1)
  location (location1 location2 location3 location4 location5 location6 location7 location8
            location9 location10 location11 location12 location13)
  pressure-plate (plate1 plate2 plate3)
  box (box1 box2)
  jammer (jammer1 jammer2)
  transmitter (transmitter1)
  receiver (receiver1)
  ladder (ladder1)
  hue (blue)
)


;;;; TECHNOLOGY INCLUDES ;;;;


(include-tech gate)
(include-tech plate)
(include-tech elevation)
(include-tech beam-direct)
(include-tech jammer)
(include-tech box)
(include-tech jump)
(include-tech stairs)
(include-tech walkability)
(include-tech visibility)
(include-tech reachability)
(include-tech ladder)


;;;; INITIALIZATION ;;;;


(define-init
  ;; Movable objects
  (has-location agent1 location1)
  (has-location jammer1 location1)
  (has-location jammer2 location9)
  (has-location box1 location4)
  (has-location box2 location10)

  ;; Fixed-position objects and initial support occupancy
  (has-position plate1 location4)
  (has-position plate2 location5)
  (has-position plate3 location6)
  (has-position ladder1 location7)
  (on box1 plate1)

  ;; Nondefault base elevations of segment fixtures.  Other gates and screens default
  ;; to 0.  Location levels are the third LOCATION-COORDS> coordinate, defaulting to 0;
  ;; transmitters and receivers default to elevation 1.
  (has-elevation gate8 3/2)
  (has-elevation gate9 3/2)

  ;; Gate controllers
  (controls ((receiver1)) gate2 normal)
  (controls ((receiver1)) gate3 normal)
  (controls ((receiver1)) gate4 inverted)
  (controls ((receiver1)) gate6 normal)
  (controls ((receiver1)) gate7 normal)
  (controls ((plate1 plate2 plate3)) gate8 normal)
  (controls ((plate1 plate2 plate3)) gate9 normal)

  ;; Direct beam properties
  (has-chroma transmitter1 blue)
  (has-chroma receiver1 blue)
  (coupled transmitter1 receiver1)

  ;; Boundary wall.  The repeated final point explicitly closes the polygon.  tech/-beam-los-coordinates.lisp's
  ;; DERIVE-LOS-FROM-SEGMENTS retains each polygon crossing, so ordinary low sight outside
  ;; this silhouette is blocked exactly like an internal wall segment.
  ;; tech/-walkability-coordinates.lisp's own walking derivation folds it in the same way, so
  ;; walking across this silhouette is blocked too.
  (boundary-wall
    ((0 10) (11 10) (11 5) (16 5)
     (16 0) (33 0) (33 17) (0 17) (0 10)))

  ;; Opaque internal wall, interrupted by a visibility-transparent,
  ;; non-walkable window.  wall3 caps the notch shared with problem-corner-topo.lisp
  ;; (same segment declared there) so LOS/walkability derivations here see it too.
  ;; edge1 is the ground-level footprint of the raised slab that location12, location13,
  ;; gate8, and gate9 sit on (elevation 3/2): its east edge, between location12 and
  ;; location10, sealing against the boundary at y 10 and 17.  Walking across the slab
  ;; at ground level is thereby blocked on that side; the elevation-3/2 crossing
  ;; location12 <-> location13 lies entirely inside the footprint (gated by
  ;; gate8/gate9), and the level change onto the slab is the authored JUMPING edge
  ;; below.  This is EDGE, not WALL: it marks the vertical boundary between two
  ;; different-elevation regions rather than a freestanding linear partition.  Its
  ;; default height 3/2 spans that step for terrain and LOS checks, but EDGE is absent
  ;; from jump's vaultable-object type; the JUMPING edge below therefore crosses with an
  ;; empty feature list rather than treating edge1 as a vault barrier.
  ;;
  ;; The slab's west side, location13 <-> location11, carries no edge at all: it is
  ;; instead the authored STAIRWAY crossing below, unconditional in both directions
  ;; and blind to the elevation difference by design (tech/stairs.lisp).  With no edge
  ;; there, -walkability-coordinates.lisp's zone flood-fill merges location11 into the
  ;; slab's own 2D zone and derives a WALKING edge between them; that edge is inert --
  ;; walking-segment-for-clause's elevation-equality check always rejects it as a usable
  ;; WALK, since the two locations sit at different elevations -- so the STAIRWAY edge remains
  ;; the only way to actually cross.
  (wall-segment> wall1 24 0 24 2)
  (wall-segment> wall2 24 4 24 101/10)  ;extended 1/10 to intercept gate3
  (wall-segment> wall3 11 10 16 10)
  (edge-segment> edge1 7 10 7 17)

  (window-segment> window1 24 2 24 4)

  ;; Gate geometry.  Paired barriers are separated by 1/5 unit:
  ;; each lies 1/10 unit from its integral center line.
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

  ;; Representative location coordinates
  (location-coords> location1 23 2)
  (location-coords> location2 20 9)
  (location-coords> location3 20 11)
  (location-coords> location4 21 14)
  (location-coords> location5 24 14)
  (location-coords> location6 27 14)
  (location-coords> location7 25 3)
  (location-coords> location8 31 9)
  (location-coords> location9 13 13)
  (location-coords> location10 8 13)
  (location-coords> location11 1 13)
  (location-coords> location12 6 13 3/2)   ;third coordinate is the location's own level
  (location-coords> location13 4 13 3/2)

  ;; Exact beam-fixture coordinates.  The 1/10 offsets place each fixture
  ;; unambiguously on the intended side of its adjacent boundary.
  (apparatus-coords> transmitter1 111/10 9)
  (apparatus-coords> receiver1 239/10 9)

  ;; Directional jamming exclusions
  (jam-disallowed> location1 location7 gate1)
  (jam-disallowed> location7 location1 gate4)

  ;; Specifically authorized acts and activities between locations
  (traversal-via> climbing location7 ((ladder1)) location1)
  (traversal-via jumping location10 () location12)  ;authorized elevation change
  (traversal-via stairway location13 () location11)  ;authorized elevation change, unrestricted both ways
  (reach-via location1 () location7)  ;authorizes manipulation
  (reach-via location2 (gate2 gate3) location3)
  (beam-via transmitter1 (gate1 location2) receiver1)  ;authorizes direct beam
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

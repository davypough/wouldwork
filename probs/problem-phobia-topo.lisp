;;; Filename: problem-phobia-topo.lisp

;;; Claustrophobia problem from Subscription -- Oti's Trials

(in-package :ww)


(ww-set *problem-name* phobia-topo)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *progress-reporting-interval* 2000000)

(ww-set *symmetry-pruning* t)

(ww-set *depth-cutoff* 20)


(defparameter *max-pairings* 2)  ;rename to max-connector-pairings for clarity


;;;; TYPES ;;;;


(define-types
  agent (agent1)
  jammer (jammer1)
  connector (connector1 connector2)
  gate (gate1 gate2)
  transmitter (transmitter1)
  receiver (receiver1 receiver2)
  location (compute (loop for i from 1 to 13
                          collect (intern (format nil "LOCATION~D" i))))
  wall-gears (wgears1)
  floor-gears (fgears1)
  wall-blower (wblower2 wblower3 wblower4)
  fan (fan1)
  wall (wall1 wall2 wall3 wall4 wall5 wall6)
  hue (red)
)


;;;; TECHNOLOGY INCLUDES ;;;;


(include-tech floor-gears)
(include-tech step)  ;boarding the floor fan is how the agent rides fgears1's stream to the loft
(include-tech wall-blower)
(include-tech jammer)
(include-tech gate)
(include-tech beam-relay)
(include-tech visibility)
(include-tech walkability)
(include-tech -terrain-consistency)  ;holds the authored levels against the derived zones


;;;; HEURISTIC ;;;;


(define-query heuristic? ()
  ;Manhattan distance from agent1's current location to the loft (location11).
  ;Lower is better; biases DFS/B&B to try the location10->location11 direction first.
  (do (bind (has-location agent1 $agent-loc))
      (bind (location-coords> $agent-loc $x $y))
      (bind (location-coords> location11 $goal-x $goal-y))
      (+ (abs (- $x $goal-x))
         (abs (- $y $goal-y)))))


;;;; INITIALIZATION ;;;;


(define-init
  ;; Movable objects
  (has-location agent1 location4)
  (has-location jammer1 location1)
  (has-location connector1 location6)
  (has-location connector2 location3)
  (has-location fan1 location2)

  ;; Fixed-position objects
  (has-position fgears1 location10)
  (has-position wgears1 location2)
  (has-position wblower2 location5)
  (has-position wblower3 location7)
  (has-position wblower4 location9)

  ;; Object coordinates
  (location-coords> location1 12 125/10)
  (location-coords> location2 61/10 125/10)
  (location-coords> location3 4 16)
  (location-coords> location4 18 115/10)
  (location-coords> location5 239/10 115/10)
  (location-coords> location6 28 15)
  (location-coords> location7 349/10 15)
  (location-coords> location8 30 115/10)
  (location-coords> location9 241/10 115/10)
  (location-coords> location10 241/10 6)
  ;; The loft, directly above location10: same x,y, ten units up.  The level has to be
  ;; written here rather than left to -floor-blowing's hover default, because that default
  ;; only covers a drive destination carrying no coordinates at all -- and this one carries
  ;; coordinates, so BASE reads their third argument and would otherwise put the loft on
  ;; the ground.
  (location-coords> location11 241/10 6 10)
  (location-coords> location12 7 8)  ;station: sees transmitter1 (up x=7 through the curtain gaps), receiver1, gate1, and gate2 through open gate1
  (location-coords> location13 239/10 19)  ;station in the north alcove: sees wblower2 (down x=23.9 past wall3's west end) and receiver2, so wblower2 can be jammed from the east side of its band
  (apparatus-coords> transmitter1 7 169/10)
  (apparatus-coords> receiver1 11/10 9)
  (apparatus-coords> receiver2 23 209/10)

  ;; Object properties
  (has-chroma transmitter1 red)
  (has-chroma receiver1 red)
  (has-chroma receiver2 red)
  (mounted-on fan1 wgears1)
  ;; The other three wall blowers are fixed combined units; only fan1 is removable and
  ;; can be ferried to fgears1.

  ;; Air stream blowing destination
  (aimed-at wgears1 location1)
  (aimed-at wblower2 location4)
  (aimed-at wblower3 location6)
  (aimed-at wblower4 location8)
  (aimed-at fgears1 location11)

  ;; The east corridor (wall3 to the boundary, height 4) is fully sealed by wblower3's
  ;; stream: default width 3 would leave half-unit walkable slips along wall3 and the
  ;; boundary, deriving a direct location8<->location6 route the room does not have.
  (stream-width wblower3 4)

  ;; Controllers
  (controls ((receiver1)) gate1 normal)
  (controls ((receiver2)) wblower2 inverted)
  (controls ((receiver2)) wblower3 normal)

  ;; Boundary wall
  (boundary-wall
    ((1 17) (8 17) (8 14) (11 14) (11 21) (18 21) (18 17) (22 17) (22 21) (28 21) (28 17) (35 17)
     (35 11) (33 11) (33 1)
     (22 1) (22 10) (18 10) (18 4) (12 4) (12 11) (9 11) (9 7) (1 7) (1 17)
    ))

  ;; Segments
  (wall-segment> wall1 6 11 6 17)
  (wall-segment> wall2 22 13 22 17)
  (wall-segment> wall3 24 13 30 13)
  (wall-segment> wall4 24 10 24 13)
  (wall-segment> wall5 26 11 33 11)
  (wall-segment> wall6 22 10 24 10)  ;seals the lower room's west slot: its only exit is L9's slot under wblower4

  (gate-segment> gate1 1 12 6 12)
  (gate-segment> gate2 1 14 6 14)

  ;; Air-stream barriers are DERIVED, not authored: each wall drive's band runs from the
  ;; solid backstop behind its blower through its has-position swept location to its
  ;; aimed-at destination, 3 units wide by default (override with a (stream-width
  ;; gears w) fact) -- see -stream-passability and -walkability-coordinates.  The
  ;; swept location is standable only while its stream is off (every edge to it is
  ;; gears-gated); each zone flanking a band's side curtains instead gets a free
  ;; one-way walk-via> ride edge to the stream's destination -- step in laterally and
  ;; be carried there while blowing, or walk the same trip across the dead band.
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
  ;; Ride fgears1's air stream to the loft: mount a fan on fgears1 at location10, step
  ;; onto it, and the ensuing propagation launches the agent to location11 (elevation 10).
  ;; Hovering there is sustained only while the stream blows, so no blowing conjunct is
  ;; needed: if the stream stopped, drop-occupants! would return the agent to location10
  ;; before any goal check.
  (holding agent1 connector2)  ;first subgoal
  ;(has-location agent1 location11)  ;final goal
)


  ;(and (active receiver2) (has-location agent1 location13) (has-location jammer1 location13) (has-location fan1 location13) (jamming jammer1 wblower2))  ;second subgoal

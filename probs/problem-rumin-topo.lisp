;;; Filename: problem-rumin-topo.lisp

;;; Coordinate/topology-driven version of Purgatory 'Rumination'.


(in-package :ww)


(ww-set *problem-name* rumin-topo)

(ww-set *problem-type* planning)

(ww-set *solution-type* first)

(ww-set *tree-or-graph* graph)

(ww-set *progress-reporting-interval* 1000000)  ;~2000 states/sec

(ww-set *max-recorder-cycles* 5)  ;allows 1 per subgoal, 5 total

;; Serial search is required here, not merely preferred: goal chaining, novelty pruning and
;; the recorder's live/ghost interleaving canonicalization all disable themselves when
;; *THREADS* is non-zero.  It cannot be set from this file -- unlike its peers, the *THREADS*
;; branch of WW-SET carries no *WW-LOADING* guard, so it calls DISPLAY-CURRENT-PARAMETERS
;; while the spliced problem is still loading and dies on a not-yet-defined tech function.
;; Set it at the REPL after staging:  (ww-set *threads* 0)

;; UNSOUND on this problem, not merely unhelpful.  DETECT-SYMMETRY-GROUPS puts CONNECTOR1,
;; CONNECTOR1*, CONNECTOR2 and CONNECTOR2* in one interchangeable family, so under graph
;; search the closed list keys live and ghost states to the same canonical form and can
;; discard the solution.
(ww-set *symmetry-pruning* nil)

;; Measured at the action-30 boundary of the 90-step solution, at cutoffs 2 and 3: identical
;; state counts with it on and off, zero states pruned, ~25% more wall clock.  Enable it per
;; chunk, for a chunk with many ghost moves and long live runs between them.  Still
;; unmeasured at depth 30.
(ww-set *recorder-prefix-pruning* nil)

;; Novelty pruning is deliberately NOT set here.  It can discard the only path to a
;; solution, so a standing setting would make every "no solution found" uninformative.
;; Enable it per run -- see doc/search-strategies/novelty.md -- with
;;   (ww-set *novelty-pruning* 2) (ww-set *novelty-partition* depth)
;; and retro-validate against a known plan before believing a negative result.

(ww-set *max-connector-pairings* 2)

;; Bound automatic searches over individual recorder-cycle subgoals.  The known
;; direct solution is 90 actions and is validated independently of this cutoff.
(ww-set *depth-cutoff* 30)


;;;; TYPES ;;;;


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
  ladder (ladder1 ladder2)
  hue (blue red)
)


;;;; TECHNOLOGY INCLUDES ;;;;


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
  (has-location tray1 location13)

  ;; Fixed-position objects and initial support occupancy
  (has-position plate1 location6)
  (has-position plate2 location9)
  (has-position plate3 location12)
  (has-position plate4 location15)
  (has-position ladder1 location5)
  (has-position ladder2 location14)
  (has-position recorder1 location3)

  ;; Representative location coordinates.  The optional third coordinate is the
  ;; location's own level, default 0.
  (location-coords> location1 21 10)
  (location-coords> location2 6 9)
  (location-coords> location3 6 5)
  (location-coords> location4 8 10 3/2)
  (location-coords> location5 17 151/10)
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
  (traverse-via jumping location2 () location4)
  (traverse-via stairway location2 () location4)
  (traverse-via stairway location9 () location10)
  (traverse-via> climbing location5 ((ladder1)) location13)
  (traverse-via> climbing location14 ((ladder2)) location15)

  ;; Nearby manipulation across boundaries
  (reach-via location4 () location2)
  (reach-via location15 () location14)  ;across the loc15 ledge; the vertical tests bound each direction
)


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!))
)


;;;; LOWER BOUND ;;;;

;; Admissible lower bound on remaining actions.  Dispatches on RECORDER-CYCLE-COUNT, which
;; identifies the chunk of the subgoal chain being searched, and returns 0 elsewhere so an
;; unrelated search is never affected.  Each component counts actions of a disjoint kind
;; (manipulation of one object / session / agent movement), so the components sum validly.

(define-query rt-some-agent-holds-connector ()
  (exists (?a agent)
    (exists (?c connector)
      (holding ?a ?c))))

(define-query rt-some-agent-holds-weight ()
  (exists (?a agent)
    (or (exists (?t tray) (holding ?a ?t))
        (exists (?b box) (holding ?a ?b)))))

(define-query rt4-blue-cost ()
  ;; RECEIVER1 must end lit.  From dark that needs a CONNECT at minimum, plus a PICKUP
  ;; unless some agent already holds a connector.  No plate-driven gate occludes the
  ;; loc2 -> loc17 -> receiver1 chain, so there is no cheaper indirect route to lighting it.
  (if (active receiver1)
    0
    (if (rt-some-agent-holds-connector) 1 2)))

(define-query rt4-plate-cost ()
  ;; PLATE3 must end depressed.  That needs a PUT at minimum, plus a PICKUP unless some
  ;; agent already holds something that can weigh a plate down.
  (if (depressed plate3)
    0
    (if (rt-some-agent-holds-weight) 1 2)))

(define-query rt4-session-cost ()
  ;; Cycle 4 must be opened and closed: one START-RECORDER and one STOP-RECORDER.
  (do (assign $cycles (recorder-cycle-count))
      (if (< $cycles 4)
        2
        (if (recording-in-progress) 1 0))))

(define-query rt4-move-cost ()
  ;; The agent must finish at LOCATION3.
  (do (bind (has-location agent1 $agent-location))
      (if (eql $agent-location 'location3) 0 1)))


(define-query rt3-box-cost ()
  ;; BOX1 must end at LOCATION2.  Only a live agent can move the live box: a PUT at
  ;; minimum, plus a PICKUP unless it is already held.
  (if (has-location box1 location2)
    0
    (if (exists (?a agent) (holding ?a box1)) 1 2)))

(define-query rt3-session-cost ()
  (do (assign $cycles (recorder-cycle-count))
      (if (< $cycles 3)
        2
        (if (recording-in-progress) 1 0))))

(define-query rt3-move-cost ()
  ;; While BOX1 is not at LOCATION2 the live agent must stand where the box is and then
  ;; carry it to LOCATION2 -- one move if it is already there, two otherwise.
  (if (has-location box1 location2)
    0
    (do (bind (has-location agent1 $agent-location))
        (bind (has-location box1 $box-location))
        (if (eql $agent-location $box-location) 1 2))))

;; USING THIS EFFECTIVELY
;;
;; The bound prunes when DEPTH + LB exceeds *DEPTH-CUTOFF*, so what matters is not the
;; bound's size but where pruning starts, at CUTOFF - LB.  Measured on chunk 4: cutoff 10,
;; bound 7 at the boundary, pruning from depth 3, 6,472 states in 3 s -- against 2,200,000+
;; states in 32 min with no bound.  Chunk 3: cutoff 15, bound 6, pruning only from depth 9,
;; 500,000 states and still running.  A bound that leaves the frontier deep buys little.
;;
;; It is CHUNK-SCOPED.  The terms below were written against the chunk-3 and chunk-4
;; boundaries and carry no admissibility argument anywhere else.  At the chunk-1 boundary
;; RECORDER-CYCLE-COUNT is 0 or 1, both tests fall through to 0, and the bound is inert --
;; it costs one query per node and prunes nothing.  Applied at a boundary it was not written
;; for it is worse than inert: replaying from action 80 of the 90-step solution, the rt3
;; terms fired at $cycles = 2 and pruned the goal path, so a solvable search reported none.
;; When searching a chunk this does not cover, disable it for the run:
;;     (fmakunbound 'min-steps-remaining?)
;; Staging the problem again restores it.
;;
;; Note the branch tests overlap: $cycles = 3 always takes the first arm, so the rt3 arm is
;; reachable only at $cycles = 2.  Intended or not, it is what the code does.
;;
;; Open work, in order of value: chunk 3's bound counts only box1's 6 of a true 15 and omits
;; the forced bootstrap, which would take it to about 10-11 and move the frontier from depth
;; 9 to 4 or 5 -- the regime that made chunk 4 collapse.  Chunks 1, 2 and 5 have no bound at
;; all.  Keep added terms on disjoint action kinds so the sum stays admissible.

(define-query min-steps-remaining? ()
  (do (assign $cycles (recorder-cycle-count))
      (if (or (= $cycles 3) (= $cycles 4))
        (+ (rt4-blue-cost) (rt4-plate-cost) (rt4-session-cost) (rt4-move-cost))
        (if (or (= $cycles 2) (= $cycles 3))
          (+ (rt3-box-cost) (rt3-session-cost) (rt3-move-cost))
          0))))


;;;; GOAL ;;;;

;; Leave GHOST-STOPS-RECORDER disabled to test immediate satisfaction, where
;; the final recorder cycle may remain open.  Enable it to test explicit ghost
;; completion, where the final cycle must be closed.
(define-goal
  (and (has-location agent1 location16)
       (ghost-stops-recorder)
  )
)

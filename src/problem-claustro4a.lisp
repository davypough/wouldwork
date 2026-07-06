;;; Filename: problem-claustro4a.lisp

;;; Talos 'Claustrophobia', rebuilt from self-contained technology files.
;;; Same objects, initializations, and goal as claustro4, with an extra elevated platform.  Behavior is supplied by
;;; (include-tech ...) directives that the stage-time splicer (exchange-problem-file)
;;; expands in place; this file holds only the glue: types, the master propagation driver,
;;; and the init/goal.  Optics is supplied
;;; by beam-direct-tech, the transmitter/receiver-only beam bundle.  The baseline has
;;; one direct transmitter -> receiver line with authored gate/location occluders.


(in-package :ww)


(ww-set *problem-name* claustro4a)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *progress-reporting-interval* 1000000)

(ww-set *depth-cutoff* 36)

;;;; TYPES ;;;;
;;;; Leaf object types the problem instantiates live here, ahead of the technology includes.
;;;; Every composite type (mobile-object, cargo, support-occupant, support, target,
;;;; beam-blocker, fixed-location) is now declared identically inside its consuming tech
;;;; file(s) instead, so no tech file depends on a type declaration living in the problem.


(define-types
  agent (agent1)
  gate  (gate1 gate2 gate3 gate4 gate5 gate6 gate7 gate8 gate9)
  screen (screen1)
  location (location1 location2 location3 location4 location5 location6 location7 location8
            location9 location10 location11 location12)
  plate (plate1 plate2 plate3)
  box (box1 box2)
  jammer (jammer1 jammer2)
  transmitter (transmitter1)
  receiver (receiver1)
  ladder (ladder1)
  hue (blue)
  mode (normal inverted toggle)  ;controller mode
)


;;;; TECHNOLOGY INCLUDES ;;;;
;;;; Self-contained capability files, spliced at stage time.  Include order is chosen for
;;;; readability: the loader pre-scans the fully-spliced file for type names, relation
;;;; signatures, and query/update names before translation, so techs may refer to relations
;;;; supplied by later includes.  The causal update order still lives explicitly in this
;;;; problem's propagate-consequences! definition.


(include-tech gate)                  ;controls; energized; update-gate-status!
(include-tech plate)                 ;depressed; update-plate-status!
(include-tech location-elevation)    ;elevation; location-elevation
(include-tech beam-direct)           ;direct transmitter -> receiver beams with gate/location occluders
(include-tech jammer)                ;jamming jam-disallowed>; pickup-jammer jam-target; nests -support-occupancy
(include-tech agent)                 ;agent-height
(include-tech box)                   ;pickup-box put-box jump-onto; jump-via
(include-tech accessibility)         ;walk-via; accessible one-step-accessible one-way-clear accessible-clear; move
(include-tech visibility)            ;los-to-fixture los-to-location; visible visible-clear
(include-tech reachability)          ;reachable-via; reachable reachable-clear
(include-tech ladder)                ;traversable>; one-way-clear; use-ladder


;;;; MASTER PROPAGATION DRIVER ;;;;


(define-update propagate-changes! ()
  ;; Binds the change-detection gate so add-prop/del-prop flag *propagated-state-changed*
  ;; on real derived-fact mutations during the fixpoint.  Each pass runs to convergence (no
  ;; change) or, failing that, the cap declares the state inconsistent.
  (let ((*detect-propagated-changes* t))
    (ww-loop for $iteration from 1 to 5
             do (if (not (propagate-consequences!))
                  (return t))
             finally (inconsistent-state)
                     (return nil))))


(define-update propagate-consequences! ()
  ;; One propagation pass.  Assembled here from exactly the loaded technologies' update
  ;; functions, in dependency order: receivers light, then plates depress, then gates
  ;; combine.  Returns t iff some derivation changed stored state, telling
  ;; propagate-changes! to run another pass.
  (let ((*propagated-state-changed* nil))
    (update-receiver-status!)
    (update-plate-status!)
    (update-gate-status!)
    *propagated-state-changed*))


;;;; INITIALIZATION ;;;;


(define-init
  ;; Dynamic state (agent-manipulable or derived)
  (located agent1 location1)
  (located jammer1 location1)
  (located jammer2 location9)
  (located box1 location4)
  (located box2 location10)

  ;; Plates (fixed positions); box1 starts on plate1, so plate1 begins depressed
  (position plate1 location4)
  (position plate2 location5)
  (position plate3 location6)
  (on box1 plate1)

  ;; Ladder (fixed fixture, positioned like a plate); boards at location7, descends to location1
  (position ladder1 location7)

  ;; Elevation (fixed floor levels above ground; absent means ground); location12 is a platform
  (elevation location12 2)

  ;; Static environment follows
  ;; Gate controllers in DNF: ((c11 c12...) (c21...)) means (c11 AND c12...) OR (c21...)
  (controls ((receiver1)) gate2 normal)
  (controls ((receiver1)) gate3 normal)
  (controls ((receiver1)) gate4 inverted)
  (controls ((receiver1)) gate6 normal)
  (controls ((receiver1)) gate7 normal)
  (controls ((plate1 plate2 plate3)) gate8 normal)  ;all three plates depressed -> open
  (controls ((plate1 plate2 plate3)) gate9 normal)
  (chroma transmitter1 blue)
  (chroma receiver1 blue)
  (coupled transmitter1 receiver1)  ;fixed beam source -> target

  ;; Beam geometry
  (beam-via transmitter1 (gate1 location2) receiver1)  ;corridor: gate1 open, location2 unoccupied

  ;; Per-location line-of-sight to a fixture; $list = occluder gates that must be open
  (los-to-fixture location1 () gate1)
  (los-to-fixture location1 () gate2)
  (los-to-fixture location1 (gate2) gate3)
  (los-to-fixture location1 () gate4)
  (los-to-fixture location1 (gate2 gate3) gate5)
  (los-to-fixture location1 (gate1) transmitter1)
  (los-to-fixture location1 () receiver1)

  (los-to-fixture location2 () gate1)
  (los-to-fixture location2 () gate2)
  (los-to-fixture location2 (gate2) gate3)
  (los-to-fixture location2 (gate2 gate3) gate5)
  (los-to-fixture location2 () receiver1)
  (los-to-fixture location2 (gate1) transmitter1)

  (los-to-fixture location3 (gate3 gate2) gate1)
  (los-to-fixture location3 (gate3) gate2)
  (los-to-fixture location3 () gate3)
  (los-to-fixture location3 () gate4)
  (los-to-fixture location3 () gate5)
  (los-to-fixture location3 (gate5) gate6)
  (los-to-fixture location3 (gate5 gate6) gate7)
  (los-to-fixture location3 (gate5 gate6 gate7) gate8)
  (los-to-fixture location3 (gate5 gate6 gate7 gate8) gate9)
  (los-to-fixture location3 (gate2 gate3) receiver1)

  (los-to-fixture location4 (gate3 gate2) gate1)
  (los-to-fixture location4 (gate3) gate2)
  (los-to-fixture location4 () gate3)
  (los-to-fixture location4 () gate4)
  (los-to-fixture location4 () gate5)
  (los-to-fixture location4 (gate5) gate6)
  (los-to-fixture location4 (gate5 gate6) gate7)
  (los-to-fixture location4 (gate5 gate6 gate7) gate8)
  (los-to-fixture location4 (gate5 gate6 gate7 gate8) gate9)
  (los-to-fixture location4 (gate2 gate3) receiver1)

  (los-to-fixture location5 (gate3 gate2) gate1)
  (los-to-fixture location5 (gate3) gate2)
  (los-to-fixture location5 () gate3)
  (los-to-fixture location5 () gate4)
  (los-to-fixture location5 () gate5)
  (los-to-fixture location5 (gate5) gate6)
  (los-to-fixture location5 (gate5 gate6) gate7)
  (los-to-fixture location5 (gate5 gate6 gate7) gate8)
  (los-to-fixture location5 (gate5 gate6 gate7 gate8) gate9)

  (los-to-fixture location6 (gate3 gate2) gate1)
  (los-to-fixture location6 (gate3) gate2)
  (los-to-fixture location6 () gate3)
  (los-to-fixture location6 () gate4)
  (los-to-fixture location6 () gate5)
  (los-to-fixture location6 (gate5) gate6)
  (los-to-fixture location6 (gate5 gate6) gate7)
  (los-to-fixture location6 (gate5 gate6 gate7) gate8)
  (los-to-fixture location6 (gate5 gate6 gate7 gate8) gate9)

  (los-to-fixture location7 () gate1)
  (los-to-fixture location7 () gate2)
  (los-to-fixture location7 (gate2) gate3)
  (los-to-fixture location7 () gate4)
  (los-to-fixture location7 (gate2 gate3) gate5)
  (los-to-fixture location7 (gate1) transmitter1)

  (los-to-fixture location8 () gate4)
  (los-to-fixture location8 (gate4) gate5)

  (los-to-fixture location9 (gate5 gate3) gate2)
  (los-to-fixture location9 (gate5) gate3)
  (los-to-fixture location9 (gate5) gate4)
  (los-to-fixture location9 () gate5)
  (los-to-fixture location9 () gate6)
  (los-to-fixture location9 (gate6) gate7)
  (los-to-fixture location9 (gate6 gate7) gate8)
  (los-to-fixture location9 (gate6 gate7 gate8) gate9)

  (los-to-fixture location10 (gate7 gate6 gate5 gate3) gate2)
  (los-to-fixture location10 (gate7 gate6 gate5) gate3)
  (los-to-fixture location10 (gate7 gate6 gate5) gate4)
  (los-to-fixture location10 (gate7 gate6) gate5)
  (los-to-fixture location10 (gate7) gate6)
  (los-to-fixture location10 () gate7)
  (los-to-fixture location10 () gate8)
  (los-to-fixture location10 (gate8) gate9)

  (los-to-fixture location11 (gate9 gate8 gate7 gate6 gate5 gate3) gate2)
  (los-to-fixture location11 (gate9 gate8 gate7 gate6 gate5) gate3)
  (los-to-fixture location11 (gate9 gate8 gate7 gate6 gate5) gate4)
  (los-to-fixture location11 (gate9 gate8 gate7 gate6) gate5)
  (los-to-fixture location11 (gate9 gate8 gate7) gate6)
  (los-to-fixture location11 (gate9 gate8) gate7)
  (los-to-fixture location11 (gate9) gate8)
  (los-to-fixture location11 () gate9)

  ;; location12 (platform) sits directly above location10 via an ungated vertical jump, so it
  ;; shares location10's interior sightlines; its view toward gate8/gate9 is inherited from
  ;; claustro4's now-split (walk-via location10 (gate8 gate9) location11) sightline, now
  ;; (jump-via location12 (gate8 gate9) location11).
  (los-to-fixture location12 (gate7 gate6 gate5 gate3) gate2)
  (los-to-fixture location12 (gate7 gate6 gate5) gate3)
  (los-to-fixture location12 (gate7 gate6 gate5) gate4)
  (los-to-fixture location12 (gate7 gate6) gate5)
  (los-to-fixture location12 (gate7) gate6)
  (los-to-fixture location12 () gate7)
  (los-to-fixture location12 () gate8)
  (los-to-fixture location12 (gate8) gate9)

  ;; Per-location line-of-sight to a different location; $list = occluder gates that must be open
  (los-to-location location1 () location2)
  (los-to-location location1 (gate2 gate3) location3)
  (los-to-location location1 (gate2 gate3) location4)
  (los-to-location location1 (gate2 gate3) location5)
  (los-to-location location1 () location7)
  (los-to-location location1 () location8)

  (los-to-location location2 (gate2 gate3) location3)
  (los-to-location location2 (gate2 gate3) location4)
  (los-to-location location2 (gate2 gate3) location5)
  (los-to-location location2 (gate2 gate3) location6)
  (los-to-location location2 (gate2 gate3 gate5) location9)

  (los-to-location location3 () location4)
  (los-to-location location3 () location5)
  (los-to-location location3 () location6)
  (los-to-location location3 (gate5) location9)

  (los-to-location location4 () location5)
  (los-to-location location4 () location6)
  (los-to-location location4 (gate4) location8)
  (los-to-location location4 (gate5) location9)
  (los-to-location location4 (gate5 gate6 gate7) location10)
  (los-to-location location4 (gate5 gate6 gate7 gate8 gate9) location11)

  (los-to-location location5 () location6)
  (los-to-location location5 (gate4) location8)
  (los-to-location location5 (gate5) location9)
  (los-to-location location5 (gate5 gate6 gate7) location10)
  (los-to-location location5 (gate5 gate6 gate7 gate8 gate9) location11)

  (los-to-location location6 (gate4) location7)
  (los-to-location location6 (gate4) location8)
  (los-to-location location6 (gate5) location9)
  (los-to-location location6 (gate5 gate6 gate7) location10)
  (los-to-location location6 (gate5 gate6 gate7 gate8 gate9) location11)

  (los-to-location location7 () location8)

  (los-to-location location8 (gate4 gate5) location9)

  (los-to-location location9 (gate6 gate7) location10)
  (los-to-location location9 (gate6 gate7 gate8 gate9) location11)

  (los-to-location location10 () location12)  ;ungated upward view onto the platform, mirroring the jump edge
  (los-to-location location12 (gate8 gate9) location11)  ;platform's view toward location11, inherited from claustro4's split edge

  ;; Directional jamming exclusions: agent location, jammer placement, target gate
  (jam-disallowed> location1 location7 gate1)
  (jam-disallowed> location7 location1 gate4)

  ;; Accessibility (move location to location).  Unobstructed edges form sparse
  ;; connected areas; guarded edges join those areas.  accessible computes the
  ;; complete reachable set, so these edges do not add walking actions to a plan.
  (walk-via location1 () location2)  ;area1
  (walk-via location3 () location4)  ;area2
  (walk-via location3 () location5)  ;area2
  (walk-via location3 () location6)  ;area2
  (walk-via location7 () location8)  ;area3

  (walk-via location2 (gate2 gate3) location3)  ;area1 -> area2

  (walk-via location3 (gate4 screen1) location7)  ;area2 -> area3
  (walk-via location3 (gate5) location9)  ;area2 -> area4

  (walk-via location9 (gate6 gate7) location10)  ;area4 -> area5

  ;; Alternate accessibility
  (traversable> location7 (ladder1) location1)   ;one-way via ladder

  ;; Elevation crossings (jump only; excluded from move by one-step-accessible's equality check
  ;; on location-elevation, but declared here as their own relation for clarity).
  (jump-via location10 () location12)  ;area5 -> platform
  (jump-via location12 (gate8 gate9) location11)  ;platform -> area6; sole route to location11

  ;; Reachability (put/pickup at a nearby location within reach); symmetric like accessible.
  ;; $list = barrier that must be open to reach across.
  (reachable-via location1 () location7)  ;wall opening between L1 and L7; reach only, not movement
  (reachable-via location2 (gate2 gate3) location3)
  (reachable-via location4 () location5)
  (reachable-via location5 () location6)
)


(define-init-action initialize-derived-state
  ;; Runs once during initialization (via do-init-action-updates), after define-init has
  ;; established the base and static facts.  Derives the entire derived layer (open, active,
  ;; depressed) via propagate-changes!, so the start state is consistent by construction.
  ;; propagate-changes! is called directly, not as a (finally ...) followup: init-action
  ;; application commits assert changes but does not run deferred followups.
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!)))


(define-goal
  ;; Claustrophobia planning goal: agent1 reaches location11.
  (located agent1 location11))

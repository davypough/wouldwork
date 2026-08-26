;;; RUMIN-TOPO -- subgoal chain.  115 actions in FIVE closed recorder cycles.
;;; Every action below was replayed with VALIDATE-ACTION-SEQUENCE from the staged start
;;; state; all 115 succeed and the final state satisfies (has-location agent1 location16).
;;; Each of the five subgoals was checked to hold at its own cycle boundary and NOT at the
;;; preceding one.
;;;
;;; This is a re-cut of "rumin-topo solution (90 steps).lisp", not a different plan.  The
;;; 90-step version uses only two cycles, so it admits exactly one legal subgoal boundary
;;; (after action 52), splitting 52 | 38.  Five cycles split 30 | 25 | 15 | 10 | 35.
;;;
;;; The single change that makes the extra cycles possible: cycle 1 carries TRAY1 BACK WEST
;;; to loc2 before it closes.  The tray is the bootstrap token -- see the notes at the foot
;;; of this file -- and the 90-step plan spends it in cycle 1 and never recovers it, which
;;; is why its cycle 1 has to do everything at once.


;;; ======================================================================================
;;; PREREQUISITE -- ONE-LINE-ISH FIX TO tech/-mobility-action.lisp, ALREADY VERIFIED
;;; ======================================================================================
;;;
;;; Before the patch, (solve) and (solve-subgoal ...) both died on the FIRST node expansion
;;; of rumin-topo, from the pristine staged start state, at depth cutoff 2:
;;;     No vertical type constants are defined for NIL.
;;;     4: (VERTICAL-TYPE-ENTRY NIL)   5: (FIXED-BASE ... NIL)
;;;     6: (JUMP-CONFIGURATION-TRANSITIONS ... AGENT1* (NIL GROUND))
;;;
;;; Cause: AGENT-CONFIGURATION uses DO, which evaluates every form and returns the last, so
;;; (bind (has-location ?agent $location)) failing for a not-yet-forked ghost is discarded
;;; and the query returns (NIL GROUND).  MOVE's precondition could not screen that out
;;; because TRANSLATE-ASSIGN compiles every (assign ...) to "always return T as a conjunct",
;;; making the precondition unconditionally true.  JUMP-CONFIGURATION-TRANSITIONS then asks
;;; for the elevation of the NIL location.
;;;
;;; See the companion file "fix-mobility-ghost-agent.lisp" for the two changed forms.
;;; Verified: (solve) at cutoff 2 now completes (33 states); the 115-action chain below
;;; still replays clean to location16; (test-talos) reports Overall PASSED, 0 failures,
;;; 25 mutation cases, 0 surviving mutants.


;;; ======================================================================================
;;; WHAT TO EXPECT FROM THE SEARCH -- MEASURED, AND NOT ENCOURAGING
;;; ======================================================================================
;;;
;;; With the patch in place I ran chunk 4 -- the SMALLEST chunk, 10 actions -- for real
;;; through SOLVE-SUBGOAL, from the genuine chunk-3 boundary state, graph search,
;;; *depth-cutoff* 10, *symmetry-pruning* nil, recorder prefix diagnostic enabled:
;;;
;;;     *solution-type* = first : 2,200,000+ states in 32 min, no solution yet
;;;     net average branching factor  = 2.3
;;;     effective branching factor b* = 3.3 rising to 4.0
;;;
;;; (That box is roughly 3x slower than yours -- your subgoal1 run reported 3,782 states/sec
;;; against ~1,300-1,850 here -- so scale accordingly.  min-length runs at the same cutoff
;;; had produced nothing after 50 min and were stopped.)
;;;
;;; Compare your own subgoal1 figures: b* = 2.59, first solution at 1.88M states, depth 14.
;;; The later boundary states are RICHER -- box1, both connectors and the tray are all
;;; placed rather than sitting in their start positions -- so every node offers more pickup,
;;; placement and pairing instantiations.  Shallower does not mean easier here: chunk 4 is
;;; 4 levels shallower than your subgoal1 and branches harder.
;;;
;;; Consequence, stated plainly: chunk 4 (10) is the only chunk in reach, and even it is a
;;; long run.  Chunk 3 (15) is hours at b* ~ 3.5.  Chunks 2, 1 and 5 (25, 30, 35) are out of
;;; reach of blind search by many orders of magnitude, and no combination of the settings
;;; below closes that gap.  The re-cut is worth having because it is the finest legal
;;; division of this problem, but the two structural chunks are not going to fall to search.
;;; If the goal is a searchable chain rather than a validated one, the lever is a heuristic
;;; or MIN-STEPS-REMAINING? for this domain, not parameter tuning.


;;; ======================================================================================
;;; PARAMETER SETTINGS
;;; ======================================================================================
;;;
;;; Set once for the whole chain:
;;;
;;;   (ww-set *threads* 0)                ; goal chaining errors otherwise
;;;   (ww-set *tree-or-graph* graph)      ; as you intended
;;;   (ww-set *max-connector-pairings* 2) ; as you intended -- required, see below
;;;   (ww-set *max-recorder-cycles* 5)    ; NOT 1 -- see below
;;;   (ww-set *symmetry-pruning* nil)     ; unsound on this problem -- see below
;;;
;;; *MAX-RECORDER-CYCLES* -- must be 5, not 1.
;;;   RECORDER-CYCLES-USED accumulates across the chain; the boundary states below carry
;;;   1, 2, 3, 4, 5.  VALIDATE-RECORDER-CYCLE-ORCHESTRATION computes
;;;   next-cycle = 1 + (recorder-cycle-count *start-state*) and refuses if it exceeds the
;;;   setting.  Measured from the real chunk-1 boundary state:
;;;       *max-recorder-cycles* = 1  ->  "Guided recorder cycle 2 exceeds
;;;                                       *MAX-RECORDER-CYCLES* = 1."
;;;       *max-recorder-cycles* = 2  ->  ok, next cycle = 2
;;;       *max-recorder-cycles* = 5  ->  ok, next cycle = 2
;;;   RUN-RECORDER-CYCLE-SEARCH then rebinds it to exactly the cycle number for the duration
;;;   of that one search, so a value of 5 does not let any single chunk consume a later
;;;   chunk's slot.  Set it to 5 once and leave it alone.
;;;
;;; *SYMMETRY-PRUNING* -- NIL for every chunk.  This one is not just a speed question.
;;;   DETECT-SYMMETRY-GROUPS finds exactly one family here, at every chunk boundary:
;;;       ((CONNECTOR1 CONNECTOR1* CONNECTOR2 CONNECTOR2*))
;;;   It has put the two ghost copies in the same interchangeable family as the two live
;;;   connectors.  With *tree-or-graph* = graph, USE-CANONICAL-SYMMETRY-P is then true, so
;;;   the closed list keys states on BUILD-EXACT-CANONICAL-IDB-FORM -- the lexicographically
;;;   least form under every permutation of that family.  Measured on hand-built state
;;;   pairs:
;;;       swap connector1 <-> connector2   (live <-> live)   canonical forms equal:  T
;;;       swap connector1 <-> connector1*  (live <-> ghost)  canonical forms equal:  T
;;;       same, with (recording-in-progress) present                                 T
;;;   The first is legitimate.  The second and third are not: CONNECTOR1 and CONNECTOR1*
;;;   are bound by RECORDING-COPY>, and START-RECORDER's fork, LIVE-RECORDING-OBJECT,
;;;   GHOST-RECORDING-OBJECT, SUPPORT-USE-ALLOWED and CONNECTOR-PAIRING-ALLOWED all
;;;   distinguish them.  Two states related by a live/ghost swap are not equivalent, but the
;;;   closed list will keep one and discard the other -- which can discard the solution.
;;;   Root cause is benign enough: the problem file deliberately does not declare
;;;   RECORDING-COPY> in DEFINE-INIT (the recorder derives it), so the symmetry detector
;;;   never sees the live/ghost binding and merges the layers.
;;;
;;; *MAX-CONNECTOR-PAIRINGS* -- keep 2.  The chain needs two-terminus pairings
;;;   ((CONNECTOR1* TRANSMITTER1), (CONNECTOR1 RECEIVER2), (CONNECTOR2 TRANSMITTER1) ...),
;;;   so 1 makes it infeasible.  Raising it above 2 only widens branching.
;;;
;;; *SOLUTION-TYPE* -- min-length is branch-and-bound, not first-solution-wins: after a
;;;   solution is found the search continues with that depth as the bound (see F-VALUE-BETTER
;;;   and the min-length cases in ww-searcher).  It is close to exhaustive at the cutoff.
;;;   Your earlier 14-step subgoal took 450 s with *solution-type* = first.  Chunks 4 and 3
;;;   (10 and 15 deep) are plausible under min-length; chunks 1, 2 and 5 (30, 25, 35) are
;;;   not.  Per-chunk recommendation below.  If you switch a chunk to first, the depth
;;;   cutoff below is still the right value.
;;;
;;; *DEPTH-CUTOFF* -- set to the chunk length given below.  Each is a validated upper bound,
;;;   so a solution provably exists at that depth; min-length may still find shorter.
;;;
;;; *RECORDER-PREFIX-PRUNING* -- per chunk, below.  Reasoned from the code and the chunk
;;;   shapes, NOT measured, because the search crashes (see BLOCKER).  T registers
;;;   VALIDATE-RECORDER-RECORDING-PREFIX, which fires only when the newest move is
;;;   START-RECORDER, STOP-RECORDER, or a ghost move, and replays the isolated recording so
;;;   far.  Its value is proportional to how many live actions sit between the last ghost
;;;   move and the STOP that would otherwise expose a dead ghost prefix.  Per chunk:
;;;       chunk   ghost moves   longest live run   recommend
;;;         1         10               10             T
;;;         2          9               14             T
;;;         3          5                8             T
;;;         4          1                9             NIL   (the one ghost move IS the stop)
;;;         5          5               18             T
;;;   The cycle-boundary validator at STOP-RECORDER runs regardless of this setting.

(validate-solution
;;; ==================================================================
;;; CHUNK 1 -- cycle 1 (30 actions)
;;;   Bootstrap the blue beam with the ghost tray, take TRAY1 east, open gate3 via plate1,
;;;   lift BOX1 out of loc7 to loc8, and carry TRAY1 back west to loc2.
;;;
;;;   SETTINGS:
;;;     (ww-set *depth-cutoff* 30)
;;;     (ww-set *solution-type* first)             ; min-length not viable at depth 30
;;;     (defparameter *recorder-prefix-pruning* t) ; after (include-tech recorder)
;;;     (ww-set *symmetry-pruning* nil)
;;;
;;;   SUBGOAL: (and (has-location box1 location8) (has-location tray1 location2))
;;; ==================================================================
  (START-RECORDER AGENT1)
  (MOVE AGENT1 ((WALK LOCATION3 NIL LOCATION2) (STAIRS LOCATION2 NIL LOCATION4) (WALK LOCATION4 NIL LOCATION13)))
  (PICKUP-CONNECTOR AGENT1 CONNECTOR1 LOCATION13)
  (MOVE AGENT1 ((WALK LOCATION13 NIL LOCATION4)))
  (MOVE AGENT1* ((WALK LOCATION3 NIL LOCATION2) (STAIRS LOCATION2 NIL LOCATION4) (WALK LOCATION4 NIL LOCATION13)))
  (PICKUP-CONNECTOR AGENT1* CONNECTOR1* LOCATION13)
  (CONNECT-CONNECTOR AGENT1* CONNECTOR1* LOCATION13 GROUND (RECEIVER1))
  (PICKUP-TRAY AGENT1* TRAY1* LOCATION13 LOCATION13)
  (MOVE AGENT1* ((WALK LOCATION13 NIL LOCATION4) (JUMP LOCATION4 NIL LOCATION2)))
  (CONNECT-CONNECTOR AGENT1 CONNECTOR1 LOCATION2 TRAY1* (CONNECTOR1* TRANSMITTER1))
  (MOVE AGENT1 ((WALK LOCATION4 NIL LOCATION13)))
  (PICKUP-TRAY AGENT1 TRAY1 LOCATION13 LOCATION13)
  (MOVE AGENT1 ((WALK LOCATION13 NIL LOCATION4) (JUMP LOCATION4 NIL LOCATION2) (WALK LOCATION2 (GATE1) LOCATION1)))
  (MOVE AGENT1* ((WALK LOCATION2 NIL LOCATION3)))
  (MOVE AGENT1 ((WALK LOCATION1 (GATE2) LOCATION6)))
  (PUT-TRAY AGENT1 TRAY1 PLATE1 LOCATION6)
  (MOVE AGENT1 ((WALK LOCATION6 (GATE3) LOCATION7)))
  (PICKUP-BOX AGENT1 BOX1 LOCATION7 LOCATION7)
  (MOVE AGENT1 ((WALK LOCATION7 (GATE3) LOCATION6)))
  (MOVE AGENT1 ((WALK LOCATION6 (GATE2) LOCATION8)))
  (PUT-BOX AGENT1 BOX1 GROUND LOCATION8)
  (MOVE AGENT1 ((WALK LOCATION8 (GATE2) LOCATION6)))
  (PICKUP-TRAY AGENT1 TRAY1 LOCATION6 LOCATION6)
  (MOVE AGENT1 ((WALK LOCATION6 (GATE2) LOCATION1)))
  (MOVE AGENT1* ((WALK LOCATION3 NIL LOCATION2)))
  (MOVE AGENT1 ((WALK LOCATION1 (GATE1) LOCATION2)))
  (PUT-TRAY AGENT1 TRAY1 GROUND LOCATION2)
  (PUT-TRAY AGENT1* TRAY1* GROUND LOCATION2)
  (MOVE AGENT1* ((WALK LOCATION2 NIL LOCATION3)))
  (STOP-RECORDER AGENT1*)

;;; ==================================================================
;;; CHUNK 2 -- cycle 2 (25 actions)
;;;   Second bootstrap (TRAY1 is west again).  With BOX1 at loc8 the agent can jump to loc9,
;;;   take the stairs to loc10, collect CONNECTOR2, and park it at loc17 paired to RECEIVER1.
;;;   loc17 is reachable from loc2 with no barrier at all, so this is the permanent relay.
;;;
;;;   SETTINGS:
;;;     (ww-set *depth-cutoff* 25)
;;;     (ww-set *solution-type* first)             ; min-length not viable at depth 25
;;;     (defparameter *recorder-prefix-pruning* t) ; best case: 14 live actions after the
;;;     (ww-set *symmetry-pruning* nil)            ;   last ghost move
;;;
;;;   SUBGOAL: (and (has-location connector2 location17) (paired connector2 receiver1)
;;;              (has-location box1 location8) (has-location tray1 location2))
;;; ==================================================================
  (MOVE AGENT1 ((WALK LOCATION2 NIL LOCATION3)))
  (START-RECORDER AGENT1)
  (MOVE AGENT1* ((WALK LOCATION3 NIL LOCATION2)))
  (PICKUP-CONNECTOR AGENT1* CONNECTOR1* LOCATION2)
  (MOVE AGENT1* ((STAIRS LOCATION2 NIL LOCATION4) (WALK LOCATION4 NIL LOCATION13)))
  (CONNECT-CONNECTOR AGENT1* CONNECTOR1* LOCATION13 GROUND (RECEIVER1))
  (MOVE AGENT1* ((WALK LOCATION13 NIL LOCATION4) (JUMP LOCATION4 NIL LOCATION2)))
  (PICKUP-TRAY AGENT1* TRAY1* LOCATION2 LOCATION2)
  (MOVE AGENT1 ((WALK LOCATION3 NIL LOCATION2)))
  (PICKUP-CONNECTOR AGENT1 CONNECTOR1 LOCATION2)
  (MOVE AGENT1 ((STAIRS LOCATION2 NIL LOCATION4)))
  (CONNECT-CONNECTOR AGENT1 CONNECTOR1 LOCATION2 TRAY1* (CONNECTOR1* TRANSMITTER1))
  (MOVE AGENT1 ((JUMP LOCATION4 NIL LOCATION2) (WALK LOCATION2 (GATE1) LOCATION8)))
  (MOVE AGENT1 ((JUMP (LOCATION8 GROUND) NIL (LOCATION8 BOX1))))
  (MOVE AGENT1 ((JUMP (LOCATION8 BOX1) NIL (LOCATION9 GROUND))))
  (MOVE AGENT1 ((STAIRS LOCATION9 NIL LOCATION10)))
  (PICKUP-CONNECTOR AGENT1 CONNECTOR2 LOCATION10)
  (MOVE AGENT1 ((STAIRS LOCATION10 NIL LOCATION9)))
  (MOVE AGENT1 ((JUMP LOCATION9 NIL LOCATION8)))
  (MOVE AGENT1 ((WALK LOCATION8 (GATE1) LOCATION17)))
  (CONNECT-CONNECTOR AGENT1 CONNECTOR2 LOCATION17 GROUND (RECEIVER1))
  (MOVE AGENT1 ((WALK LOCATION17 NIL LOCATION2)))
  (PUT-TRAY AGENT1* TRAY1* GROUND LOCATION2)
  (MOVE AGENT1* ((WALK LOCATION2 NIL LOCATION3)))
  (STOP-RECORDER AGENT1*)

;;; ==================================================================
;;; CHUNK 3 -- cycle 3 (15 actions)
;;;   Cheap bootstrap: CONNECTOR2* now forks at loc17 already paired to RECEIVER1, so the ghost
;;;   only has to hold TRAY1* -- no trip to loc13.  Agent ferries BOX1 from loc8 to loc2.
;;;
;;;   SETTINGS:
;;;     (ww-set *depth-cutoff* 15)
;;;     (ww-set *solution-type* min-length)        ; borderline; fall back to first if slow
;;;     (defparameter *recorder-prefix-pruning* t)
;;;     (ww-set *symmetry-pruning* nil)
;;;
;;;   SUBGOAL: (and (has-location box1 location2) (has-location connector2 location17)
;;;              (paired connector2 receiver1))
;;; ==================================================================
  (MOVE AGENT1 ((WALK LOCATION2 NIL LOCATION3)))
  (START-RECORDER AGENT1)
  (MOVE AGENT1* ((WALK LOCATION3 NIL LOCATION2)))
  (PICKUP-TRAY AGENT1* TRAY1* LOCATION2 LOCATION2)
  (MOVE AGENT1 ((WALK LOCATION3 NIL LOCATION2)))
  (PICKUP-CONNECTOR AGENT1 CONNECTOR1 LOCATION2)
  (MOVE AGENT1 ((STAIRS LOCATION2 NIL LOCATION4)))
  (CONNECT-CONNECTOR AGENT1 CONNECTOR1 LOCATION2 TRAY1* (CONNECTOR2* TRANSMITTER1))
  (MOVE AGENT1 ((JUMP LOCATION4 NIL LOCATION2) (WALK LOCATION2 (GATE1) LOCATION8)))
  (PICKUP-BOX AGENT1 BOX1 LOCATION8 LOCATION8)
  (MOVE AGENT1 ((WALK LOCATION8 (GATE1) LOCATION2)))
  (PUT-BOX AGENT1 BOX1 GROUND LOCATION2)
  (PUT-TRAY AGENT1* TRAY1* GROUND LOCATION2)
  (MOVE AGENT1* ((WALK LOCATION2 NIL LOCATION3)))
  (STOP-RECORDER AGENT1*)

;;; ==================================================================
;;; CHUNK 4 -- cycle 4 (10 actions)
;;;   The watershed.  CONNECTOR1 goes onto BOX1 at loc2 paired to TRANSMITTER1 and CONNECTOR2,
;;;   so blue is now held by live objects alone and gate1 stays open with no recorder.
;;;   TRAY1 then goes onto plate3.
;;;
;;;   NOTE the ordering: TRAY1 is put on plate3 AFTER start-recorder, not before.  A plate is
;;;   a fixed, shared support, so a ghost ON fact forked onto it displaces the live one; if
;;;   the tray were already on plate3 at the fork, TRAY1* would take the plate and the live
;;;   tray would be left loose when the cycle closed.
;;;
;;;   SETTINGS:
;;;     (ww-set *depth-cutoff* 10)
;;;     (ww-set *solution-type* min-length)        ; the one chunk where this is comfortable
;;;     ;; Leave *recorder-prefix-pruning* at NIL. ; no ghost moves to prune
;;;     (ww-set *symmetry-pruning* nil)
;;;
;;;   SUBGOAL: (and (active receiver1) (depressed plate3) (has-location agent1 location3))
;;; ==================================================================
  (PICKUP-CONNECTOR AGENT1 CONNECTOR1 LOCATION2)
  (CONNECT-CONNECTOR AGENT1 CONNECTOR1 LOCATION2 BOX1 (CONNECTOR2 TRANSMITTER1))
  (MOVE AGENT1 ((WALK LOCATION2 NIL LOCATION3)))
  (START-RECORDER AGENT1)
  (MOVE AGENT1 ((WALK LOCATION3 NIL LOCATION2)))
  (PICKUP-TRAY AGENT1 TRAY1 LOCATION2 LOCATION2)
  (MOVE AGENT1 ((WALK LOCATION2 (GATE1) LOCATION12)))
  (PUT-TRAY AGENT1 TRAY1 PLATE3 LOCATION12)
  (MOVE AGENT1 ((WALK LOCATION12 (GATE1) LOCATION3)))
  (STOP-RECORDER AGENT1*)

;;; ==================================================================
;;; CHUNK 5 -- cycle 5 (35 actions)
;;;   Unchanged from the 90-step solution's cycle 2.  Blue is handed to the ghosts, both live
;;;   connectors move east, the ghost repoints CONNECTOR1* to TRANSMITTER2, red lights
;;;   RECEIVER2, gate5 opens, and the agent reaches loc16 via the loc15 ledge and gate6.
;;;
;;;   SETTINGS:
;;;     (ww-set *depth-cutoff* 35)
;;;     (ww-set *solution-type* first)             ; min-length not viable at depth 35
;;;     (defparameter *recorder-prefix-pruning* t) ; 18 live actions after the red switch
;;;     (ww-set *symmetry-pruning* nil)
;;;
;;;   SUBGOAL: the problem goal -- use (solve), not (solve-subgoal)
;;; ==================================================================
  (START-RECORDER AGENT1)
  (MOVE AGENT1 ((WALK LOCATION3 NIL LOCATION2)))
  (PICKUP-CONNECTOR AGENT1 CONNECTOR1 LOCATION2)
  (MOVE AGENT1 ((WALK LOCATION2 (GATE1) LOCATION8)))
  (PUT-CONNECTOR AGENT1 CONNECTOR1 LOCATION8 GROUND)
  (MOVE AGENT1 ((WALK LOCATION8 (GATE1) LOCATION2)))
  (PICKUP-BOX AGENT1 BOX1 LOCATION2 LOCATION2)
  (MOVE AGENT1 ((WALK LOCATION2 (GATE1) LOCATION8)))
  (PUT-BOX AGENT1 BOX1 GROUND LOCATION8)
  (PICKUP-CONNECTOR AGENT1 CONNECTOR1 LOCATION8)
  (MOVE AGENT1 ((JUMP (LOCATION8 GROUND) NIL (LOCATION8 BOX1))))
  (MOVE AGENT1 ((JUMP (LOCATION8 BOX1) NIL (LOCATION9 GROUND))))
  (CONNECT-CONNECTOR AGENT1 CONNECTOR1 LOCATION9 PLATE2 (CONNECTOR2*))
  (MOVE AGENT1 ((JUMP LOCATION9 NIL LOCATION8)))
  (MOVE AGENT1 ((WALK LOCATION8 (GATE1) LOCATION17)))
  (PICKUP-CONNECTOR AGENT1 CONNECTOR2 LOCATION17)
  (MOVE AGENT1 ((WALK LOCATION17 (GATE1) LOCATION11)))
  (CONNECT-CONNECTOR AGENT1 CONNECTOR2 LOCATION11 GROUND (CONNECTOR1 RECEIVER2))
  (MOVE AGENT1* ((WALK LOCATION3 NIL LOCATION2)))
  (PICKUP-CONNECTOR AGENT1* CONNECTOR1* LOCATION2)
  (CONNECT-CONNECTOR AGENT1* CONNECTOR1* LOCATION2 BOX1* (CONNECTOR2* TRANSMITTER2))
  (MOVE AGENT1 ((WALK LOCATION11 NIL LOCATION8)))
  (PICKUP-BOX AGENT1 BOX1 LOCATION8 LOCATION8)
  (MOVE AGENT1 ((WALK LOCATION8 (GATE5) LOCATION14)))
  (PUT-BOX AGENT1 BOX1 GROUND LOCATION14)
  (MOVE AGENT1 ((WALK LOCATION14 (GATE5) LOCATION12)))
  (PICKUP-TRAY AGENT1 TRAY1 LOCATION12 LOCATION12)
  (MOVE AGENT1 ((WALK LOCATION12 (GATE5) LOCATION14)))
  (PUT-TRAY AGENT1 TRAY1 BOX1 LOCATION14)
  (MOVE AGENT1 ((LADDER LOCATION14 (LADDER2) LOCATION15)))
  (PICKUP-TRAY AGENT1 TRAY1 LOCATION14 LOCATION15)
  (PUT-TRAY AGENT1 TRAY1 PLATE4 LOCATION15)
  (MOVE AGENT1 ((WALK LOCATION15 (GATE6) LOCATION16)))
  (MOVE AGENT1* ((WALK LOCATION2 NIL LOCATION3)))
  (STOP-RECORDER AGENT1*)

)

;;; ======================================================================================
;;; WHY CHUNKS 1 AND 5 CANNOT BE CUT FURTHER
;;; ======================================================================================
;;;
;;; Measured, not assumed (all figures from BEAM-VISIBLE and TRAVERSAL-SEGMENTS called
;;; directly against staged states):
;;;
;;; 1. Sweeping all 17 locations at anchors 1, 3/2, 2, 5/2, 3, 7/2, 4, 9/2 and 5:
;;;    TRANSMITTER1 and TRANSMITTER2 are beam-visible from loc2 and loc3 ONLY, and only at
;;;    anchor >= 2.  loc4 does not see them -- the sightline is stopped by the boundary wall
;;;    at x = 7.  So every beam in the problem must source from a connector standing at loc2
;;;    on something two units tall.
;;;
;;; 2. At loc2 the achievable anchors are: ground 1, on a box 2, on a ghost-held tray 5/2.
;;;    SUPPORT-USE-ALLOWED permits the tray perch only for a LIVE connector on a tray a GHOST
;;;    is actively holding.  So until BOX1 physically reaches loc2, gate1 can only be opened
;;;    by a ghost holding TRAY1* -- the bootstrap.
;;;
;;; 3. The bootstrap needs TRAY1 on the west side.  TRAY1 is also the only weight that opens
;;;    plate1, hence gate3, hence BOX1's release from loc7.  A cycle that takes TRAY1 east
;;;    and leaves it there spends the last bootstrap: no later cycle can reopen gate1, and
;;;    the ladder1 route (loc11 -> gate2 -> loc5 -> loc13) is westbound only.  Chunk 1 must
;;;    therefore do the bootstrap, the tray delivery, the box release AND the tray's return
;;;    in one cycle.  30 actions.
;;;
;;; 4. The red chain needs four relay nodes:
;;;       TRANSMITTER2 -> CONNECTOR1* @loc2 on BOX1* -> CONNECTOR2* @loc17
;;;                    -> CONNECTOR1  @loc9 on plate2 -> CONNECTOR2 @loc11 -> RECEIVER2
;;;    That is both live connectors and both ghost copies at once.  Ghosts exist only inside
;;;    a cycle and fork at their live counterparts' positions, so the live connectors have to
;;;    be sitting at loc2 and loc17 when that cycle opens -- and everything downstream of red
;;;    (gate5, BOX1 to loc14, TRAY1 to loc14, the ladder, plate4, gate6, loc16) has to finish
;;;    before it closes, because red dies with the ghosts.  35 actions, one cycle.
;;;
;;; Useful side findings:
;;;
;;;  * loc17 <-> loc2 and loc17 <-> loc3 are (WALK ... NIL ...) -- no barrier.  For walking,
;;;    loc17 is on the WEST side of gate1; it only reads as a gate1 crossing when approached
;;;    from loc1/loc8/loc11/loc12.  This is what makes loc17 a free permanent relay.
;;;  * loc2@2 -> loc13@5/2 and loc2@2 -> loc10@1 are also clear beam hops, and loc13@>=2 and
;;;    loc10@any both see RECEIVER1.  loc13 is the ghost's relay in chunk 2.
;;;  * gate4 does NOT block loc9 <-> loc10; the stairs there are unguarded.  It blocks the
;;;    loc9 -> loc17 beam, which is why CONNECTOR1 on plate2 has to hold gate4 open for its
;;;    own hop in chunk 5.
;;;  * Placing a connector onto a ghost-held tray at loc2 requires the agent to stand at loc4
;;;    (elevation 3/2) and reach across: the tray top is at 5/2, which from loc2's own floor
;;;    exceeds *VERTICAL-REACH-LIMIT*.  PLACEMENT-OPTIONS returns (GROUND) from loc2 and
;;;    (GROUND TRAY1*) from loc4.

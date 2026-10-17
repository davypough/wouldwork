# crelay-topo — Briefing

Reopened 2026-10-16 for an independent re-derivation (see Handoff, Objective).
Maximum search depth: 12, set by D 2026-10-16.
Profile: Constraint-Static-Profile.txt, SHA-256 063200E5…2BE2448F; regenerated 2026-10-16 from
probs/problem-crelay-topo.lisp with current tech (SBCL 2.2.9 container) and unchanged.

## Spec-diagram check

2026-10-16, A, against probs/problem-crelay-topo-1.lisp (same rules as problem-crelay-topo.lisp);
diagram viewed whole and at full resolution.  Title "CRELAY"; drawing covers the whole boundary.
- MATCH: boundary polygon; walls wall1-wall13 and edge1 (endpoints); gates gate1-gate9 and screen1
  co-located with gate6; locations 1-21 (location6 drawn near x 7, spec 7.5: within drawing
  precision; location20 drawn above location4 as the raised stream top); plates 1-8 at their
  locations; ladders at locations 3, 8, 11; recorder at location1; blower1 at location4 and
  switch1 at (7.5, 0.1); switch2, transmitter1, receiver1 and repeater1 positions; start places
  of agent1 (location1), connector1 (location9), box1 (location6), tray1 (on plate2, location7).
- UNSETTLED (the diagram does not show them): heights of wall9 (1/2), wall10 (1) and edge1 (3/2);
  CONTROLS wiring and modes; red hues; ladder directions (traverse-via>) and the edge/blower
  traversals.  A stray pencil dot near (24, 28) on the top wall carries no label.
- Start state (display-validation-state, 2026-10-16): plate2 depressed by tray1, so gate3 open;
  gate2 open (switch1 off, inverted); gate5 open (switch2 off, inverted); all else closed.
  Consistent with the init facts and controllers.

## Summary

Written 2026-10-16 from the spec, the static profile, the tech files and the cycle-2 endpoint
only.  Sources: [spec], [profile <section>], [tech <file>], [measured].

Goal: agent1 at location19 [spec].  The recorder cycle may be open at that moment: the
problem is solved whenever the live agent reaches the goal [tech Recorder-ghost operations, 23].

The only route to location19 crosses, in order [profile S3, SD]:
  location1 -gate1- location4/5/7 -gate3- location8/9/10 -gate5- location11/12/13
  -gate7- location15 -gate8- location16/17/18/21 -gate9- location19.
Gates on that route and what holds each open [spec CONTROLS]:
- gate1: a body on plate1 (location2).  Now held by connector1.
- gate3: a body on plate2 (location7).  Now held by box1.
- gate5: switch2 OFF (the start setting).
- gate7: switch2 ON.
- gate8: receiver1 lit by the red beam.
- gate9: bodies on plate6, plate7 and plate8 together (location16, 17, 18).
Off the route [spec, profile S3]:
- switch2 is at location14, reached from location11/12/13 through gate6 and screen1.  gate6
  needs bodies on plate4 (location13) and plate5 (location12); screen1 lets only an
  empty-handed agent through [tech -passability].
- gate4 needs a body on plate3 (location10).  It opens no route but lets transmitter1's beam
  in: every beam chain to receiver1 found by the profile needs gate4 open [profile H3.1].
- One-way ladders, empty-handed only: location11 down to location10, location8 to location5,
  location3 to location1 [spec, tech -passability].
Beam: transmitter1 at the east wall shines through gate4 onto connector stations at
location7, 8 and 9 [profile MC].  The shortest candidate chains [profile H3.3, geometric only,
not validated]: connector1 raised on box1 at location9 paired to receiver1 (3 bodies
counting the plate3 keeper), or connector1 at location9 on the ground relaying through a
second connector raised at location15 [H3.2].  The repeater sits in a walled cell between
location9 and location15 and can relay but cannot be reached.
Bodies: live agent1, box1, connector1, tray1; inside a recorder cycle also their ghosts
agent1*, box1*, connector1*, tray1* [profile MC recorder].  Ghosts weigh down plates, relay
beams, and a ghost's switch toggle changes the real switch [tech -recorder-switch-shadow];
the live agent may connect to a ghost connector and stand on or place onto a ghost-held tray,
but not use other ghost objects; a ghost never uses a live object.

## Difficulties

Evidence carried from the search-led run of 2026-10-06 (no search-analysis.txt exists; summary
recorded from that session's continuation notes and confirmed by D 2026-10-16):
- One search over the whole plan is far out of reach; the problem was solved as a chain of
  milestones with SOLVE-SUBGOAL (serial goal chaining, the pre-2026-10-15 form).
- Measured growth about 2.6x per move; legs of about 10 moves for FIRST and 6 to 9 for
  MIN-LENGTH were searchable.
- "box1 at location7" straight after SG1 (FIRST, cutoff 14) found nothing in 10 minutes;
  splitting at "agent1 at location6" (inside the alcove) succeeded.
- Leg 2a with MIN-LENGTH at cutoff 10 did not finish in 15 minutes (serial).

Difficulties from the cycle-2 endpoint onward (2026-10-16; each read from the spec, profile or
tech, not measured unless marked):
1. **switch2 serves two gates in opposite settings.**  gate5 (into location11-13) needs it off;
   gate7 (on to location15) needs it on.  Once it is on, location11-13 can be left downward by
   ladder3 but not re-entered from location8-10.  So every body that must end up beyond gate7
   has to be in location11-13 (or beyond) before switch2 turns on, or switch2 must be turned
   off and on again, each time through gate6.  [profile H4.1, S3]
2. **Reaching switch2 needs two keepers.**  Bodies on plate4 and plate5 while the agent goes
   through gate6 to location14 and back, and the agent must be empty-handed for screen1.
   [profile H2.1, tech -passability]
3. **gate8 needs the beam lit while the agent crosses.**  Every candidate chain uses gate4
   (a body on plate3 at location10) plus connector1 (or connector1*) at location9, raised on a
   box for the direct chain.  Those bodies sit in location8-10, which the agent cannot re-enter
   once switch2 is on, and taking any of them away darkens the beam.  So the beam bodies do
   not follow the agent; the beam is needed only while crossing gate8.  [profile H3, S1]
4. **gate9 needs three keepers beyond gate8 at the final crossing.**  Those bodies must have
   crossed gate7 and gate8 earlier, while the beam was lit by other bodies.  [profile H2.2, H1.3]
5. **Bodies are short.**  Besides agent1 there are three live bodies; plate demand along the
   way is 8 (5 devices), never all at once.  The decisive crossings need about 3 bodies for the
   beam and 3 on plates 6-8, plus keepers for gate1 and gate3 (and 2 for gate6) at the times
   those are crossed.  Ghosts are needed, so the final part runs inside an open recorder
   cycle.  [profile T6, H1]
6. **The cycle-2 results are also the beam's parts.**  connector1 (holding gate1) is the only
   live connector and box1 (holding gate3) the only live box.  Using them for the beam closes
   gate1 or gate3 unless another body or a ghost takes over.  Without gate1, location1 (the
   recorder) is reached only by ladder1 from location3, and left only through gate1.
   [measured endpoint, spec]
7. **Carrying limits.**  One object at a time; ladders and screen1 empty-handed only.  A held
   tray carries what rests on it, but only another agent can place onto a held tray (live
   onto a ghost-held tray, never the reverse), and a held tray's top (3/2) is out of reach of a
   grounded placer.  [tech tray.lisp, -passability; profile S5, H7]
8. **Search size.**  Legs of about 10 moves take under a minute serial; growth about 2.6x per
   move [measured]; maximum depth 12 (D, 2026-10-16).
Open checks (NEEDS): which beam chains are lit in the physical view by ghost connectors (the
live agent reads gate8 in the physical view); whether connector1 raised at location9 sees
receiver1 over the repeater cell's low walls in a settled state.

## Backward analysis (from the goal)

Notation: C_k is a condition (only facts the remaining steps need); E_k the gate crossing that
last made a needed fact true; C_k = Pre(E_k) + (C_(k-1) - Eff(E_k)).  In-room moves are left
to search.  (D and A, 2026-10-16.)

- C0: agent1 at location19.
- E1: agent1 crosses gate9.  C1: agent1 in the plate room (location16, 17, 18 or 21) and
  plates 6, 7, 8 each held by a body other than agent1 (D: stepping off a plate closes gate9
  at once, so agent1 cannot be a keeper).
- E2: the last crossing of gate8 into the plate room.  Options considered: A agent1 carries
  one live object; B agent1* carries one ghost object; C agent1* carries tray1* with live
  riders; D agent1* crosses empty-handed.  (A live-held tray can take no riders: only another
  agent may load a held tray, and a ghost may not use a live support [tech -recorder-core
  support-use-allowed].  D noted loading a ghost-held tray needs a raised live placer, i.e. at
  location5 from location20.)

**Ghost crossing of gate8: ruled out (A, 2026-10-16; source-read, not measured).**
The ghost's moves are made during recording, when only the recorded objects (the ghost
versions) exist; playback repeats them.  So any gate the ghost passes must have been opened by
recorded objects alone: plates pressed by them, the beam relayed by connector1*, switch2 set by
the recording agent.  Help from live objects during playback does not count for the ghost
[tech -gate, beam-relay, -recorder-core, -recorder-switch-shadow].  gate7 alone is passable for it: two ghost objects on
plate4 and plate5 let it flip switch2.  gate8 is where it fails: its own beam needs three ghost
bodies (one on plate3, connector1* raised on box1* at location9) [profile H3.1, H3.3], placed
before the flip, since afterwards nothing can be carried back past gate5.  That is 2 + 3 = 5
ghost objects; there are 3.  So options B, C, D are impossible.
Consequences:
1. Every crossing of gate8 is by agent1, in the physical view, carrying at most one object.
2. The three keepers on plates 6-8 are live objects: box1, connector1 and tray1 (ghost objects
   cannot cross).  agent1 crosses gate8 into the plate room at least three times, and out at
   least twice, each with receiver1 lit.
3. The physical-view beam counts every body and every connector, ghosts included [tech
   beam-relay, view nil], so ghost bodies (agent1* included) can keep the beam lit while the
   live objects are ferried.

**Ghost-only beam versus gates 1 and 3 (D and A, 2026-10-16; by reading, not tested).**
D: there are not enough ghost objects to hold gate1 and gate3 open and also light receiver1.
Confirmed: the agent must pass gate1 and gate3 after starting the recorder at location1, and a
beam with connector1* raised on box1* at location9 plus a body on plate3 leaves the ghost side
one body short (the ghost agent can hold only one plate).
Way round: real objects that are not crossing gate8 in that playback can help, and a real
object left on a plate when recording starts has its ghost copy on the same plate, so the real
one can be taken away while the copy keeps the plate pressed.  One-cycle candidate
(CONSISTENT by reading; NEEDS a test):
- At recording start: tray1 on plate2; connector1 on box1 at location9, paired to receiver1;
  switch2 off; agent1 at location1.  The copies start in the same places, pairing included.
- Ghost: agent1* walks to location2 and stays on plate1 (gate1).  tray1* stays on plate2
  (gate3).  connector1* on box1* at location9 stays (the beam's connector and riser).
- Real: agent1 takes connector1 off box1 and puts box1 on plate3 (gate4): receiver1 lit.  It
  takes tray1 off plate2 (tray1* keeps gate3 open), carries connector1 and tray1 into
  location11-13, uses them on plate4 and plate5, turns switch2 on at location14, then carries
  them one at a time through gate7 and gate8 into the plate room.
- Result: connector1 and tray1 in the plate room; box1 left on plate3 in location8-10, which
  is cut off by gate5 once switch2 is on.  Getting box1 in is the open question: when it
  crosses gate8, the beam needs a connector somewhere else.

**Beam test (A, 2026-10-16, measured; hand-built states in crelay-topo-1, SBCL 2.2.9).**
- Real objects: connector1 on box1 at location9, tray1 on plate3, switch2 on.  receiver1 lit
  and gate8 open; agent1 walked location11 -> location16 through gate7 and gate8 in one move.
- Ghost objects: connector1* on box1* at location9, real box1 on plate3, recording open.
  receiver1 lit and gate8 open for agent1: ghost connectors light the beam for the real agent.
- connector1 must be connected to transmitter1 as well as receiver1 (2 of its 3 connections);
  with only receiver1 it stays dark.
- Anything standing at location15 blocks the beam: location15 lies on the line from location9
  to receiver1.  With agent1 at location15 receiver1 went dark; with agent1 at location13 it
  was lit.  So agent1 crosses gate8 from location11-13 in one walk (through gate7 and gate8),
  and nothing may be left at location15 while the beam is needed.

**Leftover object in a later cycle (D and A, 2026-10-16; by reading).**
- Every beam to receiver1 starts at a connector at location7, 8 or 9 (the only spots
  transmitter1 reaches) [profile RC].  Once connector1 is in the plate room, its copy starts
  there too in every later recording, so no later cycle can light receiver1.  Hence the
  one-cycle candidate above (connector1 and tray1 delivered, box1 left on plate3) is a dead
  end, and connector1 must cross gate8 in the last delivery cycle.
- With box1 or tray1 as the leftover the same holds once connector1 is delivered.  With
  connector1 as the leftover, the next cycle lacks box1* at location9 to raise connector1*.
- Two tricks for the next round of analysis: (1) a real object resting on plate1 or plate2
  when recording starts leaves a copy there that holds gate1 or gate3 for the whole playback
  and, during recording, lets the ghost agent pass that gate; (2) the same holds on plates
  6-8: a copy keeps its plate pressed for gate9 while the real object is moved again.
- The profile also lists "latch" beams: a connector raised at location21, just inside gate8,
  fed from a connector at location8 or 9 through the repeater; it works only while gate8 is
  already open.

**Probe FC1 (A, 2026-10-16, measured): lighting receiver1 from a final-cycle start.**
Hand-built pre-recording state in crelay-topo-1: agent1 at location1; tray1 on plate1;
connector1 on box1 on plate2 (location7); switch2 off; cycle 2 closed.  Goal
(active receiver1), MIN-LENGTH, cutoff 12, serial, *time-limit* 840 s (SBCL 2.2.9 container,
2 cores).  Result: stopped at the time limit, not exhaustive, no solution; 963,442 program
cycles, 6,467,463 states, effective branching about 3.3.  A bound relative to this start,
settings and time, not a refutation.  Forms:
  (stage crelay-topo-1) (ww-set *solution-type* min-length) (ww-set *depth-cutoff* 12)
  (solve-subgoal ((has-location agent1 location1) (has-location tray1 location2)
    (on tray1 plate1) (has-location box1 location7) (on box1 plate2)
    (has-location connector1 location7) (on connector1 box1) (depressed plate1)
    (depressed plate2) (open gate1) (open gate2) (open gate3) (open gate5)
    (recorder-cycle-closed) (recorder-cycles-used 2))
   (active receiver1))
Next: split it at a hard precondition (e.g. the ghost's part first: connector1* at location9
and a ghost body on plate3), or run it at 16 threads on lumpy.

**Body count for the last entry into the plate room (A, 2026-10-17; by reading, plus one
measurement).**  Premises, each read from the tech or the profile:
- At agent1's last crossing of gate8 receiver1 must be lit, and every chain to it needs a raised
  connector at location9 (connector plus riser, both on the same side, real or ghost) and a body on
  plate3; chains through location15 need even more.  Measured 2026-10-17 on a spec copy with 72
  extra test points across location8-10: no spot on the ground sees receiver1.
- Those three beam bodies stay outside afterwards.  The final presses on plates 6-8 come only from
  real objects inside at the end and from copies of objects that were inside when that cycle's
  recording started.
- Every object has one start place per cycle, so its copy serves one role: a beam part at
  location9/10, a keeper of plate1 or plate2, or a press inside.
- The real agent starts each cycle at location1 and must cross gate1 and gate3: plate1 and plate2
  pressed at those moments.  The ghost agent can press plate1, or plate2 if a copy holds plate1;
  it can reach plate3 only if copies hold both plate1 and plate2.  A real object left on plate2
  stays west of gate3 unless something else presses plate2 when it is carried east.
Result: every allocation is one body short.  With the ghost on plate3, two copies keep plates 1
and 2 and at most one copy is left for the beam, so a real beam part stays out and needs a copy
inside: 4 start places for 3 objects.  With the ghost on plate1 or plate2, one copy keeps the
other plate and the beam needs 3 bodies, of which any real one stays out and needs a copy inside:
again 4 for 3.  So, under these premises, no final cycle exists.  One premise must be wrong, or a
mechanic is missing from this reading.  Put to D 2026-10-17.

**Probe CARRY: can agent1 move more than one real object at once? (A, 2026-10-17, measured.)**
D asked to test by search whether option A could bring several real objects through gate8 at
once.  Hand-built state in crelay-topo-1, no recording: agent1, box1, connector1 and tray1 all
at location11 (ground); gate2 and gate5 open; cycle 2 closed.  MIN-LENGTH, serial, lower-bound
pruning off, SBCL 2.2.9 container.  Both searches completed normally (exhaustive to the cutoff).
- Goal box1 and connector1 at location13, cutoff 9: shortest 7 actions, one object per trip
  (46,274 cycles, 15 s).  A combined carry would take at most 5.
- Goal box1, connector1 and tray1 at location13, cutoff 12: shortest 10 actions, one object per
  trip, the last trip carrying tray1 (215,019 cycles, 66 s).  A combined carry would take at
  most 8.
So in the real world agent1 carries one object per trip: a box is lifted only when nothing is on
it, and nothing can be set on a tray agent1 holds itself.  The test used location11 to
location13 (no gate between); what agent1 can carry does not depend on the gate.  Not covered:
real objects riding a tray held by the ghost, which is how the ghost could move several, but the
ghost cannot pass gate8 (options B-D).
Forms:
  (stage crelay-topo-1) (ww-set *solution-type* min-length) (ww-set *depth-cutoff* 9)
  (setf *min-steps-pruning-enabled* nil)
  (solve-subgoal ((has-location agent1 location11) (has-location box1 location11)
    (has-location connector1 location11) (has-location tray1 location11) (open gate2)
    (open gate5) (recorder-cycle-closed) (recorder-cycles-used 2))
   (and (has-location box1 location13) (has-location connector1 location13)))
  ;; then cutoff 12 and the goal with (has-location tray1 location13) added

**Second way to load a tray agent1 holds: hand-over from the ghost's tray (D's question; A,
2026-10-17, by reading, then measured).**  D questioned "nothing can be placed on a tray agent1
holds".  Source: when a held tray is set down at its holder's own location, whatever rides on it
settles onto the highest eligible surface at or below its previous height
[tech -support-settling, -placement place-held-object!].  A real rider may use a real held tray.
So if the ghost holds tray1* with real objects on it and agent1 holds tray1 at the same place,
the ghost putting tray1* down drops the real riders onto tray1.  A stack moves as one: only its
bottom object rides the tray.
Measured (hand-built recording-open state, crelay-topo-1, SBCL 2.2.9): agent1 holding tray1 and
agent1* holding tray1* at location5; box1 on tray1*, connector1 on box1.  Action 1, agent1* puts
tray1* on the ground at location5: result (on box1 tray1), (on connector1 box1).  Action 2,
agent1 walks to location7: box1 and connector1 come along.  So agent1 can carry all three real
objects in one trip, which breaks the premise "one object per crossing" in the body count above.
Not yet checked: loading tray1* in play (a grounded agent cannot reach a held tray's top; the
profile names location20 or location6 as raised spots), and the crossing of gate8 with the loaded
tray (screen1 and the ladders need empty hands; gate7 and gate8 do not).

**How the gap in option A could have been caught (D and A, 2026-10-17).**  The options for the
last crossing of gate8 were listed from what we believed the actions allow; "agent1 carries one
object" was never derived from the rules, and probe CARRY tested it without a recording running,
so the ghost's tray was absent.  A backward enumeration builds earlier states from the real
actions and would have produced, about 7 layers back from the goal, agent1 at location13 holding
tray1 loaded with box1 and connector1.  Cheaper check for next time: at each crossing an
argument calls decisive, list every one-step predecessor of a concrete state just after it and
confirm each fits a listed option.  To be formalized in the solving advisor (D, 2026-10-17).

## Backward analysis, second pass (D and A, from 2026-10-17)

Redone one step at a time, each step with the checks of the solving advisor's *Checking
reasoning*.  C_k is the condition the remaining steps need; E_k the last action that made it
true.

**Step 1.**  C0: agent1 at location19.
- E1 options (R2, writers of agent1's location: tech -mobility-action MOVE; -support-motion
  relocate-stack! for an agent riding a support; -floor-blowing for blower1, aimed at
  location20 only): (a) agent1 moves into location19 itself; (b) agent1 rides a tray held by
  the ghost.  (b) needs the ghost in the plate room, excluded with options B-D (by reading,
  the ghost's own beam needs 5 ghost bodies; R6 probe not run).
- location19's only entrance is gate9, from location16, 17 or 18 [profile S3, R6 <-> R7].
  agent1 may leave from the ground or from a support there; it may carry something.
- gate9 needs plates 6, 7 and 8 pressed at that moment [spec CONTROLS].  agent1 cannot be one
  of the bodies: leaving a plate is a separate action, after which the gate closes [tech
  -mobility-action, read; D].
- C1: agent1 in the plate room (location16, 17, 18 or 21) and plates 6, 7, 8 each pressed by a
  body other than agent1.  Bodies may be real objects, or copies of objects that stood on those
  plates when the current recording started (only while that recording is open).  A stack
  presses only the plate under its bottom object.

- R1 (A, 2026-10-17, measured; crelay-topo-2, SBCL 2.2.9 container).  S1: agent1 just walked
  location16 -> location19; box1, connector1, tray1 on plates 6, 7, 8; no recording.  4
  predecessors, all option (a): agent1 walks in through gate9 from location16, 17, 18 or 21
  (correction: location21 too, not only 16-18).  S1r: the same with a recording open, the plates
  pressed by copies and the real objects loose at location21: 17 predecessors, the same 4 plus 13
  ghost moves far away that change nothing needed.  No unlisted way in.  (b) cannot show here (no
  ghost in the plate room); its exclusion still rests on reading.  VERDICT: CONFIRMED for (a).

**Step 2.**  C1: agent1 in the plate room; plates 6, 7, 8 each pressed by a body other than
agent1.
- E2 is the last action that made either part true.  R2, writers of a pressed plate: putting or
  connecting an object onto it (tech box, tray, beam-relay), a tray set down at its holder's
  place dropping its rider onto it (-support-settling), the ghost's own actions (excluded: no
  ghost in the plate room).  Starting a recording adds copies only where real objects already
  press, so it presses nothing new.  Writers of agent1 in the plate room: agent1 crossing gate8
  (from location15, or through gate7 and gate8 from location11-13 in one walk).
- Reach from location16, 17 or 18 covers all three plates; from location21 none [measured
  2026-10-16, reach pairs 16-17, 16-18, 17-18].
- R4, measured 2026-10-17: agent1 at location16 holding tray1 with box1 on it and connector1 on
  box1, plates empty; agent1 puts tray1 on the ground: box1 (with connector1) lands on plate6.
- E2 options and C2:
  (i) agent1 enters through gate8 with all three plates already pressed.  C2: agent1 at
      location15 or location11-13; plates 6-8 pressed by bodies inside; receiver1 lit (and
      switch2 on, from location11-13).
  (ii) agent1, inside, puts or connects a held object onto the last free plate.  C2: agent1 at
      location16, 17 or 18 holding it; the other two plates pressed.
  (iii) agent1, inside, sets down a held tray so its rider lands on the last free plate.  C2:
      agent1 at the free plate's location holding the tray with a rider; the other two
      pressed.
- In (ii) and (iii) the plate room must already hold, besides agent1 and its load, two
  pressing bodies; in (i) three.  Every option needs earlier crossings of gate8 or copies.

- R1 (A, 2026-10-17, measured).  S2: agent1 at location16 empty-handed, having just put box1 on
  plate6; connector1 on plate7, tray1 on plate8; recording open; receiver1 lit by copies outside
  (connector1* raised on box1* at location9, paired to transmitter1 and receiver1; tray1* on
  plate3); switch2 on.  29 predecessors.  Those that make C1 true: agent1 walks in through gate7
  and gate8 from location11, 12 or 13 (option i); agent1 puts box1 on plate6, connector1 on
  plate7 or tray1 on plate8 (option ii); agent1 walks back from location19 (the goal was already
  reached, so not a new case).  The rest keep C1 true (agent1 moving inside, stepping off box1)
  or are ghost moves.  No entry from location15 is listed: agent1 standing there blocks this
  beam, as measured on 2026-10-16.  Option (iii) needs a tray left on the ground and is not in
  this state; R4 of 2026-10-17 confirms it.  VERDICT: CONFIRMED, no unlisted option.

**Step 3.**  From here a step is a crossing of gate8, not a single action: moves, puts and
pickups inside the plate room rearrange what is there but bring nothing in.
- R2, writers of an object's place inside the plate room: carried in by agent1 (held, or riding a
  tray agent1 holds), placed by reach across gate8 (no location pair across gate8 is within the
  reach limit 5/2: location15 to location21 is 2.69 apart; read from coordinates), carried in by
  the ghost (excluded).  Copies inside exist only while a recording is open that started with
  their real objects already inside.  So every body inside entered with agent1 through gate8.
- E3: agent1's last entry through gate8 (it is E2 itself in option (i)).
- What agent1 can bring in one entry (R2 on loads; probe CARRY; the tray hand-over): one object
  in hand; or tray1 with one rider, which may carry a stack (box1 with connector1 on it).  A box
  with something on it cannot be lifted, and agent1 cannot load its own held tray: a rider gets
  on tray1 only by dropping from a tray the ghost sets down beside it.  So the load is 1, 2 or 3
  objects, and 2 or 3 only after a hand-over from the ghost.
- C3: agent1 at location15 or location11-13 (switch2 on), about to cross, carrying load L;
  already inside a set S of bodies; real objects in S and L, plus copies on plates 6-8, give at
  least three pressing bodies after in-room rearranging; receiver1 lit at the crossing by bodies
  outside, not in L: a connector raised on a box or tray at location9 (same side) and a body on
  plate3 [profile RC, 2026-10-17 test-point sweep], all still outside afterwards.
- CORRECTION (D, 2026-10-17): the beam need not be outside the load.  A connector riding a tray
  agent1 holds keeps its location and its beam [tech beam-relay, pickup-connector-retaining-
  pairings comment], so agent1 at location15 holding tray1, with box1 on it and connector1 on
  box1 paired to repeater1 and receiver1, is itself the last relay (chains via location15 in
  profile RC).  A beam ending at location15 is not blocked by agent1 standing there: a location
  never occludes a beam it terminates [tech -beam-los-coordinates].  The step-3 option list had
  missed this because R2 was applied to what can be carried, not to what can light receiver1.
- R4, measured 2026-10-17 (hand-built recording-open state, crelay-topo-1): agent1 at location15
  holding tray1 / box1 / connector1 (connector1 paired to receiver1 and repeater1); connector1*
  on the ground at location9 paired to transmitter1 and repeater1; box1* on plate3; switch2 on.
  After one propagating ghost step: connector1*, repeater1 and connector1 red, receiver1 active,
  gate8 open.  agent1 then walked location15 -> location16 through gate8 carrying all three
  real objects.  So the last entry can bring in every real object while only two copies stay
  outside: a connector copy on the ground at location8 or 9 and a body copy on plate3.
- C3 (revised): agent1 at location15 (or location11-13 with switch2 on), about to cross, carrying
  load L; inside S; S and L plus copies on plates 6-8 give three pressing bodies; receiver1 lit
  at the crossing, possibly through a connector in L.

- R1 (A, 2026-10-17, measured).  Four states:
  - S3a (the tool's own test): agent1 has just walked location15 -> location16 with tray1, box1
    and connector1.  19 predecessors; the only one with objects outside before is that crossing.
  - S3b: box1 on plate6, agent1 has just walked back out to location15, empty-handed.  25
    predecessors, none with box1 outside before: nothing is placed across gate8 by reach.
  - S5: just before S3a (agent1 at location15 with the loaded tray, receiver1 lit).  17
    predecessors; receiver1 goes from dark to lit only by agent1 walking into location15 through
    gate7 from location11-13 carrying the loaded tray: connector1 rides in as the last relay.  So
    the revised C3 ("possibly through a connector in L") is the case the tool finds.
  - S4 (the hand-over): agent1 holding tray1 and agent1* at location5; agent1* has just put
    tray1* down, so box1 with connector1 settled onto tray1.  First run: 27 predecessors, the
    true one (box1 on tray1* before) MISSING, since the tool copied each rider's support from the
    state; and two impossible entries (the ghost standing on the live box1 or the held tray1).
    Re-run after the fix (Schema-Gaps, "Predecessors where a rider changes support by settling"):
    27 predecessors, the hand-over listed, the two impossible entries gone, and one new entry
    the rules allow (agent1 standing on tray1* and stepping down as it is set down).  The
    hand-over is the only listed way box1 comes onto tray1; every other entry has it there
    already.  Direct placements onto tray1: none listed (agent1 cannot hold two things; a ghost
    may not use a live object).
  VERDICT: "every body inside entered with agent1 through gate8" and the C3 beam option are
  CONFIRMED.  "A rider gets on tray1 only by a ghost hand-over" is CONFIRMED for one step from
  S4 (placement and settling); a recording stop is not covered by the tool.

## Contracts

No hand contracts recorded.

## Hints

None.  Hints from the earlier solution are excluded by the objective.

## Subgoal log

| subgoal | whose idea | check | route | result |
|---|---|---|---|---|
| SG1: connector1 on plate1, recorder cycle ended | D and A | search | one search | ACCEPTED 2026-10-16, 9 actions |
| SG2: box1 on plate2, recorder cycle ended | D and A | search | two searches (2a, 2b) | ACCEPTED 2026-10-16, 10 + 9 = 19 actions (28) |

**Regenerated 2026-10-16** with the checkpoint form (SOLVE-SUBGOAL <checkpoint> <goal>), serial,
SBCL 2.2.9 container, from (stage crelay-topo-1):
- SG1 MIN-LENGTH cutoff 12: 9 actions, 1,369 cycles, 0.3 s (same as 2026-10-06).
- SG2 leg 2a FIRST cutoff 10, (has-location agent1 location6): 10 actions, 98,812 cycles, 42 s
  (2026-10-06: 259,904 cycles, 239 s; the lower bound's elevation term was added since).
- SG2 leg 2b MIN-LENGTH cutoff 9, (and (on box1 plate2) (recorder-cycle-ended)): 9 actions,
  55,794 cycles, 28 s.  This joins the 2026-10-06 legs 2b and 2c (3 + 6) into one leg ending at
  the final resting state; the total is the same.
Replay of Actions.lisp from the start, 2026-10-16: SUCCESS-P T, 28 actions, goal not yet
satisfied (expected).  Checkpoint.txt holds the endpoint.

The 2026-10-06 measurements (serial goal chaining) are kept below for comparison.

**SG1.** From (stage crelay-topo-1), serial:
`(solve-subgoal (and (on connector1 plate1) (recorder-cycle-ended)))`, MIN-LENGTH, cutoff 12:
9 moves, 1,369 cycles, 0.6 s.

**SG2.** Chained from SG1, serial, in three legs:
- 2a `(ww-set *solution-type* first) (ww-set *depth-cutoff* 10)`
  `(solve-subgoal (has-location agent1 location6))`: 10 moves, 259,904 cycles, 239 s.
  The search found the means of entering the alcove itself.
- 2b `(ww-set *solution-type* min-length) (ww-set *depth-cutoff* 8)`
  `(solve-subgoal (has-location box1 location7))`: 3 moves, 394 cycles.
- 2c `(ww-set *depth-cutoff* 9)`
  `(solve-subgoal (and (on box1 plate2) (recorder-cycle-ended)))`: 6 moves, 18,917 cycles.

Endpoint review (2026-10-16): agent1 at location1, empty-handed, no ghost, recorder cycle 2
closed.  connector1 on plate1 (location2) holds gate1 open; box1 on plate2 (location7) holds
gate3 open; tray1 on the ground at location7, free.  gate2 and gate5 open from the start
(switches off).  Closed: gate4 (plate3), gate6 (plates 4 and 5), gate7 (switch2 on),
gate8 (receiver1 lit), gate9 (plates 6, 7, 8).  blower1 off; receiver1 unlit.  Lessons from these legs are in
doc/solving-advisor.md, *Choosing a subgoal*.

## Result

Open.

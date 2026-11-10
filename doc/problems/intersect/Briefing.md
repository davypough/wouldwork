# intersect — Briefing (solving advisor)

Started 2026-11-07.  Spec: `probs/problem-intersect.lisp` (revision 3, location9 added
2026-11-08; revision 2 added location8).
Diagram: `doc/problems/intersect/intersect-diagram.png`.
Static profile: generated 2026-11-07 for revision 1 (8 of 8 technologies covered, 0
UNCOVERED); stale from revision 3 and removed at wrap-up.  Regenerate if needed.

Wrap-up (2026-11-10, D's request): only the final problem and solution are kept.  Removed:
the probe copy `probs/problem-intersect-1.lisp`, the pre-spec analysis snapshot, the Python
beam-dynamics sketch and the stale static profile (all in git history).  Their findings and
lessons remain below.

## Spec-diagram check

Done by the spec advisor (spec-analysis.txt checks 11-13, against the current diagram);
not repeated.  Start state after propagation (2026-11-07): agent1, connector1 at
location1; connector2 at location2; connector3 at location3; nothing lit, all gates
closed — MATCH with the spec's init facts.

## Summary

agent1 must reach location7 (SW room) through gate1, gate2, gate3, gate4 in that forced
order (profile SD: the only transit door set).  Each gate is open only while its receiver
is lit (gate4 also needs plate1 pressed at location6).  Blue comes only from transmitter1,
seen only from location1; red only from transmitter2.  Three connectors, at most 3
pairings/selections each.  Read from the spec and profile.

## Difficulties

- Crossing beams cut each other.  Every red route from transmitter2 to receiver2 crosses
  the x = 15 line that carries blue from location1 up to receiver3 (transmitter2->location2
  crosses it at (15, 9.81)), so gate2 and gate3 compete.  A sacrificial beam that cuts one
  of them nearer its source decides which survives.  (Profile crossing pool; measured.)
- After gate2 the agent can act in the central room only by climbing the ladder back to
  location2 empty-handed, and must pass gate2 again to return east.
- Carried from the search hand-off: one search to ~35 steps was projected at weeks and
  ~10^9 states.  Those runs predate the engine corrections below, which shrink the space
  (the opening search fell from 12,714 to 3,521 states processed), so the growth figures
  and "no plan within 19 steps" are no longer evidence.  Not re-measured.
- D's hand milestones: agent1 at location2 by about step 5, location3 by 9, location5 by 23;
  full plan near 35.  location8 (15, 8) was added for the location3 -> location5 leg; it
  lies on the x = 15 blue line, so a body there blocks that beam (now modelled, below).

## Engine corrections made during intake (2026-11-07)

Found while checking the first search result; each approved by D, tested with
`(test-topo)` and `(test-talos)` (all passing, 0 surviving mutants), docs updated.
1. A beam stopped by a wall, edge or boundary segment it does not clear no longer cuts
   crossings beyond it (`beam-reaches-crossing`, `beam-barrier-before-crossing`).  The
   first search's 8-step opening relied on connector2's beam to receiver1 cutting through
   wall4.
2. A beam stopped by a body (agent or connector within the occlusion tolerance) no longer
   cuts crossings beyond it (`occupied-location-before`).  Replay: agent1 at location8
   blocks connector1's blue beam, so transmitter2's red beam survives and gate2 opens.
3. Terminus selection offers only termini structurally visible from the agent's present
   location: walls, edges, boundary block by height; gates and bodies do not
   (`potentially-visible`).  Selections still accumulate across viewpoints (D's rule).
Terms (D, 2026-11-07): say *selection* (a selected terminus), *pairing* (stored after
put-down) and *beam*; not "link".

## Contracts

None needed (0 UNCOVERED).

## Hints

- Body blocking at location8 is a likely mechanism for the location3 -> location5 leg
  (D added location8 for it).  D's guess, unverified.

## Subgoal log

| subgoal | whose idea | check | route | result |
|---|---|---|---|---|
| SG1 agent1 at location3 | D's milestone (L3 by ~9) | replay from start | one plain search | 8 steps, accepted 2026-11-07 |
| SG2 agent1 at location5 | D's milestone (L5 by ~23) | replay from start | one plain search | 11 steps, accepted 2026-11-08 |
| SG3 agent1 at location7 (goal) | A's reanalysis: blue via location10 and location11 | replay from start | hand plan, replayed | 21 steps, goal reached 2026-11-10 |
| (old) SG3 exit to location3 with relay at location9 | A's backward reading, agreed by D | search from SG2 endpoint | one plain search | none within 14 (lumpy, 2026-11-08; again on probe copy intersect-1, 2026-11-09) |

**SG1.**  Search: state start, `(solve-subgoal *start-state* (has-location agent1 location3))`,
cutoff 10, min-length, threads 16, lower-bound pruning off; 0.8 s, 3,521 states, one
solution of 8 (cloud SBCL 2.2.9, corrected engine).  Expected: connector1 relays blue to
receiver1 (gate1), connector2 relays red to receiver2 (gate2).  Result: as expected, plus
connector2 paired with receiver4 — a sacrificial beam (location2 sees receiver4 through
window3 and window4) that cuts connector1's blue beam to receiver3 at crossing169
(15, 6.57), before that beam reaches transmitter2's red beam at crossing33 (15, 9.81).
Replayed from the start by `Actions.lisp`: success T, goal not yet satisfied.
Endpoint review: agent1 at location3 empty-handed; connector3 at location3; gates 1 and 2
open, gate3 closed (receiver3 dark); connector1 holds 3 pairings (full), connector2 holds
3 (full), connector3 none.  The ladder returns agent1 to location2 only empty-handed.

**SG2.**  SG1 first re-replayed on the current engine (after the symmetry and pairing
commits of 2026-11-07/08): same endpoint.  Search: state start,
`(solve-subgoal <SG1 endpoint> (has-location agent1 location5))`, cutoff 14 (A's choice:
D's count of about 14 for the leg), min-length, threads 16, lower-bound pruning off;
completed normally in 260 s, 955,782 states, 3 solutions, shortest 11 (cloud SBCL 2.2.9,
2 cores).  Expected (reading): gate2 and gate3 compete, so a ladder trip back to the
central room, with agent1's body at location8 blocking blue while gate2 is crossed.
Result: as expected, with connector3 as the red feed:
- connector3 at location3 is paired with transmitter2 (location3 sees it through window1).
- After the ladder climb, connector2 is picked up (clearing all three of its pairings,
  including the sacrifice to receiver4) and re-paired with connector3 and receiver2:
  red runs transmitter2 -> connector3 -> connector2 -> receiver2.
- transmitter2's beam to connector3 crosses blue on the x = 15 line (about (15, 11.6)).
  With agent1 at location8 blue stops at its body below that crossing, so red survives
  and gate2 opens; when agent1 walks location8 -> gate2 -> location3 the crossing cuts
  both (crossing60), so gate2 closes behind it.
- Picking up connector3 at location3 removes the red beam, blue reaches receiver3, gate3
  opens, and agent1 walks location3 -> gate3 -> location5 carrying connector3.
Replayed from the start by `Actions.lisp` (19 actions): success T, goal not yet satisfied.
Endpoint review: agent1 at location5 holding connector3; connector1 at location1 paired
with transmitter1, receiver1, receiver3 (blue lit, gates 1 and 3 open); connector2 at
location2 paired with receiver2 only (unlit; gate2 closed); receiver4 dark.  gate3 stays
open while blue reaches receiver3, so agent1 can still return east.
Look-ahead for gate4 (open question): it needs receiver4 red and plate1 pressed; red
comes only from transmitter2 in the central room, and red beams toward receiver4 cross
the blue x = 15 line, so blue must be stopped or cut first.

**SG3 dialogue (2026-11-08).**  Target: gate4 open with agent1 west of it (receiver4 red,
plate1 pressed; D: a connector on plate1 can do both, from location6).
Finding (A, from coordinates; reading, not replayed): with locations 1-8, red reaches the
west room only from a connector at location2, location3 or location8 paired with
transmitter2; no west location sees transmitter2 (location5's line hits wall9 at (5, 10.3)).
transmitter2's beams to location2 and location3 cross blue's x = 15 line at (15, 9.81)
and (15, 11.65), and a connector at location8 blocks blue with its body; receiver3 is lit
only by blue up that line.  So gate3 is closed whenever a red feed exists, yet agent1 must
lay the feed in the central room and then recross gate3 west: CONTRADICTED.  Relay at
location4 (D's idea): its lines to location6, location5 and receiver4 hit wall10 or wall9:
CONTRADICTED (reading).
Resolution (D, 2026-11-08): new location9 at (10, 8), spec revision 3.  From location9 a
connector sees transmitter2 (line clears wall10 at x = 9.1) and location6 through window3
and window4, all west of x = 15, so the feed need not cut blue.  Likely chain:
transmitter2 -> relay at location9 -> connector on plate1 at location6 -> receiver4.
SG1 + SG2 re-replayed on revision 3 (D, 2026-11-08): success T, same endpoint.
Backward reading (A): the last exit from the central room through gate2, with the relay
already at location9, seems to need four connectors at once (connector1 for blue, the
relay, one at location2 lighting receiver2, one at location3 holding the removable
transmitter2 pairing); the relay's beams to receiver2 and location2 cross blue at
(15, 6.84) and (15, 7.09), below agent1's body at location8.  Unchecked; SG3 tests it.
**SG3 (agreed with D 2026-11-08).**  Goal: agent1 at location3, receiver3 active, and a
connector at location9 paired with transmitter2.  State start (SG2 endpoint), cutoff 14
(A's hand count about 12), min-length, threads 16, lower-bound pruning off; cloud SBCL
2.2.9, 2 cores.  Stopped by A at its 15-minute limit: TRUNCATED, not a bound.  3,012,105
states at 529 s (last report; about 3,600 states/s), max depth 14 reached, b* 2.81,
75.7% repeats, 0 solutions.  (Cutoff 4 check: 1,040 states, none.)  For comparison SG2's
whole cutoff-14 search was 955,782 states.
Lumpy run (D, 2026-11-08, same forms, 16 threads): completed normally, cutoff 14, max
depth 14, 4,542,235 states, 76.2% repeats, 77,054 inconsistent abandoned, 220 s, no
solution.  Bound: no plan of 14 or fewer steps reaches SG3 from the SG2 endpoint.
Consistent with the four-connector reading (A's hand count for an exit was about 12);
not a refutation.
R4 replay (A, 2026-11-09, cloud SBCL 2.2.9; D's question: can the location9 connector
light receiver2?).  From the SG2 endpoint: connector3 put at location5, location5 ->
gate3 -> location3, ladder to location2, connector2 picked up and given receiver2 and
transmitter2, carried to location9 and put down; then agent1 to location8.  Result:
connector2 red, crossing227 active (its beam to receiver2 x blue at (15, 6.84)),
receiver2 and receiver3 both dark, gates 2 and 3 closed, before and after agent1 stands
at location8.  CONTRADICTED (measured).

Probe copy `probs/problem-intersect-1.lisp` (2026-11-09, D's suggestion; file since removed): location8 moved
from (15, 8) to (15, 25/4), on blue's line just north of gate1 (y = 13/2 would put the body
within the 1/2 occlusion tolerance of the location9 -> receiver2 beam).  R4 replay (A,
cloud): SG1 + SG2 replay success T on the copy; then connector3 left at location5,
ladder, connector2 paired with receiver4, receiver2, transmitter2 and put at location9:
crossing234 active, receiver2 dark; agent1 to location8: receiver2 lit, gate2 open; walk
location8 -> gate2 -> location3 succeeds; then crossing234 active again, receiver2 and
receiver3 dark, gates 2 and 3 closed (as read).  receiver4 stayed dark: connector3 at
location5 blocks the location9 -> receiver4 beam with its body.
SG3 on the copy (D, lumpy, 2026-11-09): completed normally, cutoff 14, 4,448,758 states,
239 s, no solution.  Bound relative to the copy, start and cutoff.
Game facts (D, 2026-11-09): 3 connectors; receiver2 faces west; crossing beams always
cut; a carried connector keeps pairings but sends no beam until put down; closed gates
block beams (not pairings); a gate pushes objects out of its gateway and closes; no low
walls or raised places; windows barred; a connector must be put down before climbing.
All match the spec.  Locations are D's estimates, so placement is open.
Premise ledger (A, R5).  P1 the receiver2 lighter at agent1's last gate2 exit must stay
placed afterwards; connector1 must be at location1 and connector3 must end on plate1, so
it is the final red relay, lit for good by its transmitter2 pairing (read).  P2 receiver3
is lit only by blue up x = 15 from (15, 5) (read).  P3 sweep (A, Python, 0.1 grid, walls,
gate1 open, other gates closed, facing): 15,191 points see both transmitter2 and receiver2;
every one has a beam to them crossing blue's line or lies within 1/2 of it (measured,
geometry only).  So after the last gate2 exit blue stays cut and gate3 cannot reopen,
wherever locations are placed.  A solution exists, so a premise or a game rule differs.
Oscillation (D, 2026-11-09): in the game, connector1 at location1 (transmitter1, receiver1,
receiver3), connector2 at location2 (receiver2, transmitter2) and connector3 at location3
(transmitter2, receiver4) make a triangle of crossings, (15, 8), (15, 9.81), (17.85, 8),
whose sides go dark in rotation about once a second; no receiver flickers.  Reading (A):
each beam's first crossing is another's second, so each is always cut at one of its two
crossings and never reaches past the triangle; a pure loop cannot flicker a receiver, only
one fed back through a relay it switches.  R4 replay (A, cloud, 2026-11-09): from the SG2
endpoint, connector3 paired and put at location3 (crossing224 active), then connector2
paired with receiver2 and transmitter2 and put at location2: state dropped as
inconsistent.  The engine settles a two-way flip-flop (union or arbitration) when the
result holds, and drops anything else, so plans through oscillating states are invisible
to search.
Feedback-oscillation search (A, 2026-11-09, Python beam-dynamics sketch in the cloud
container, not the engine: each tick a beam stops at its first crossing with a beam that
reached that point the tick before; relays light from incoming beams; bodies block within
1/2; it reproduces D's rotating triangle).  Searched relay connector2 over points seeing
transmitter2 and receiver2, connector3 over the west room with any pairings (connector1
excluded: full), agent1 at location3.  Found: connector2 at location9 paired with
transmitter2, receiver2, connector3; connector3 at about (0.25-1.25, 10.25-11.25) (west wall,
north of location6) paired with connector2 and receiver2.  connector3's beam to receiver2
(through window4 and window3) crosses connector2's feed from transmitter2 at about
(9.8, 8.4), just short of location9: connector2 lit -> connector3 lit -> feed cut ->
connector2 dark -> connector3 dark -> feed restored.  While connector2 is dark its beam to
receiver2 no longer cuts blue at (15, 6.84), and connector3's beam is cut before blue, so
receiver3 lights: in the sketch receiver3 is lit about 40% of the time with agent1 at
location3, and receiver2 about 45% with agent1's body on blue at (15, 6.25) (0% at
(15, 6.5), where the body blocks connector2's beam to receiver2).  Candidate ending: sprint
through gate2 and then gate3 in open phases; pick up connector3, pair it with connector2
and receiver4 on plate1; gate4.  Unverified in game; the engine drops these states.
In-game test (D, 2026-11-09): no oscillation.  connector3's beam to receiver2 is limited to
the band between window4's top corner and wall4's top corner; at its north end connector2
cannot sit south of it and still see receiver2 past wall4, so the feed is never crossed.
Sketch re-run with a 1/2 clearance from wall corners: the only family left needs connector3
within 1/2 of window4's corner, i.e. none in practice.  The sketch found no other family
(connector2 anywhere seeing transmitter2 and receiver2; connector3 anywhere west, north
strip included, with any pairings).

Gate passage while body-blocking (D, 2026-11-09): the engine lets agent1 walk through a
gate held open only by its own body in a beam (SG2's location8 -> gate2 move); plates are
already strict.  D keeps the engine as is for intersect; check other problems later
(Schema-Gaps.txt, NEED of 2026-11-09).

**Reanalysis (2026-11-10).**  Fresh premise review found P2 false: receiver3 is not lit
only up x = 15.  The lower room includes the strip west of wall2 and the cell between
window3 and window4 (no wall along y = 6, x 5-8).  A connector in the lower room south of
wall2 sees transmitter1 and receiver1 and, through window4, a west-room spot that sees
receiver3; blue then never enters the central room, so red there no longer competes with
it.  Confirmed by D in the game (2026-11-10).  Why missed: "transmitter1 is seen only from
location1" (true of locations 1-9) was promoted to "blue reaches receiver3 only up x = 15"
(read, never measured); the P3 sweep covered only the red side in the central room; the
cell had no location, so search and profile could not see it.  Lesson: an "only from / only
via" premise needs a sweep of every walkable area before an impossibility rests on it.
Spec revision 4 (D approved): location10 (8, 1) and location11 (2, 15), best integer pair
by a geometry script (tightest wall-corner clearance 0.79, location10 -> location11 past
window4's edge).  With 11 locations staging fails ("Design Limit Error: Total number of
planning objects exceeds 999", crossing records), so location4 and location5 were removed
(A's change, to be confirmed by D); SG2 now ends at location11.

**SG3 (2026-11-10).**  A's candidate ending, replayed from the start (cloud SBCL 2.2.9,
`validate-action-sequence`, revision 4): 40 actions, success T, goal satisfied.
- connector3 put at location11 paired with receiver3; agent1 back east through gate3 and
  up the ladder.
- connector2 re-paired with transmitter2 and receiver2 at location9 (gate1 stays open).
- connector1 moved to location10, paired with transmitter1, receiver1, connector3: blue
  runs location10 -> location11 -> receiver3; gates 1, 2 and 3 all open, no crossings.
- agent1 walks lower room -> central -> east -> west, picks up connector3 and puts it on
  plate1 paired with connector2 and receiver4: red location9 -> location6, gate4 opens.
Not shown shortest (no search run).  The put on a plate is `on plate1`, not `on ground`.

## Result

Reached 2026-11-10: 40 actions (SG1 8 + SG2 11 + SG3 21) in `Actions.lisp`, replay success
T, goal satisfied (cloud; confirmed by D's local validation, 2026-11-10).  A valid plan;
minimal length not established.  Not yet played in the game by D.

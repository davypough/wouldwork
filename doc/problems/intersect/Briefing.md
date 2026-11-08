# intersect — Briefing (solving advisor)

Started 2026-11-07.  Spec: `probs/problem-intersect.lisp` (revision 2, location8 added).
Diagram: `doc/problems/intersect/intersect-diagram.png`.
Static profile: `doc/problems/intersect/Constraint-Static-Profile.txt`, generated
2026-11-07 on the corrected engine (see *Engine corrections*), SHA-256
`edb0fbaea1d96429415ecaefe967a886366a7c28fa2b48ab69c4a8c06530e8f5`.  8 of 8 technologies
covered, 0 UNCOVERED.

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

Next (A's choice of depth, rule revised 2026-11-07): SG2 toward location5, cutoff 14
(D's count for the leg is about 14).

## Result

Not reached.

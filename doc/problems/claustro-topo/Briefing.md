# claustro-topo — Briefing
Profile: Constraint-Static-Profile.txt, SHA-256 88303a097d3069ed6ff5a70096c96ea6f2ddd710db9c9a01e0acb89cffae0e78.  Max search depth: 8.
Written 2026-09-27; simplified the same day at D's request.  Spec: probs/problem-claustro-topo.lisp
(authoritative over the diagram).  Approached as a new problem.

## Summary

agent1 starts at location1 and must reach location11, west of a raised slab.

Start: agent1 and jammer1 at location1; jammer2 at location9; box1 on plate1 at location4;
box2 at location10.  Plates: plate1-3 at location4, location5, location6.  Ladder at location7.

Gates and what opens them:
- gate1, gate5: nothing but a jammer.
- gate2, gate3, gate6, gate7: the beam (receiver1 lit).  gate4: the beam OFF.
- gate8, gate9 (on the slab): all three plates weighted at once.

The beam runs transmitter1 -> receiver1 along y = 9.  It is lit only while gate1 is jammed
open and nothing stands at location2.

Way to the goal: location1 -> (gate2+gate3, or gate1) -> the plate room (location3-6) ->
gate5 -> location9 -> gate6+gate7 -> location10 -> jump onto the slab at location12 (needs a
box to stand on) -> gate8+gate9 -> location13 -> stairs -> location11.

## Anticipated difficulties

1. Too few bodies.  Crossing the slab needs three plate weights plus the jump box at
   location10; getting there needs gate1 and gate5 jammed.  Only four movable things exist
   (two boxes, two jammers).  A jammer on a plate both jams and weighs -- if it can see
   its gate from there.
2. Bootstrapping the beam.  gate1 must be jammed to light the beam; jamming it from the
   plate room needs gate2/gate3 open, which needs the beam already lit.
3. gate4 flips.  Lighting the beam closes gate4, cutting off location7/location8 (ladder).
4. jammer2 sits behind gate5, which only a jammer opens.
5. One-way moves: the ladder goes only location7 -> location1; a box can be lowered from
   the slab but not lifted back.
6. Depth 8 per search, so the plan needs several stages with good stopping points.

## Hand contracts (historical: these three were UNCOVERED during solving)

- **beam-direct**: receiver1 is lit iff transmitter1 and receiver1 match in hue, gate1 is
  open, and no agent, box or jammer at location2 spans the beam height (1).  receiver1
  controls gate2/3/6/7 (open when lit) and gate4 (open when unlit).
- **jammer**: carried.  JAM-TARGET sets it down (ground, plate, or box top) and forces the
  target gate open, provided the jammer's top (1 on the ground, 2 on a box) sees the gate's
  midpoint (2; 7/2 for gate8/9) and the placement is within reach.  PICKUP-JAMMER ends the
  jam.  A jammer on a plate also weighs it.  Forbidden: from location1 placing at location7
  to jam gate1; from location7 placing at location1 to jam gate4.
- **stairs**: location13 <-> location11, both ways, unconditional.
- Also: a screen and a ladder pass only an empty-handed agent.

## Couplings

- One beam opens four gates and closes a fifth.
- Anything at location2 cuts the beam.
- A jammer can jam and weigh a plate at once.
- box2 is the only jump support at location10 and also a candidate plate weight.

## Necessity hints

- **HH1 NECESSARY.**  The beam gates (2, 3, 6, 7) open only while gate1 is jammed and
  location2 is empty, or while they are jammed themselves.
- **HH2 NECESSARY.**  Reaching location10 means passing gate5 (jam only) and gate6+gate7.
  With the beam lit (one jammer on gate1), gate5 takes the other jammer.
- **HH3 NECESSARY, if the slab reach is one-way.**  The jump box stays at location10, so
  the three plates are held by the other box and both jammers.  NEEDS a check.
- **HH4 CANDIDATE.**  End arrangement: box1 on plate1, jammers on plate2/plate3 jamming
  gate1 and gate5, box2 at location10.  NEEDS: jammers on plate2/plate3 can see gate1 and
  gate5.
- **HH5 CANDIDATE.**  Anything done at location7/location8 must happen while the beam is
  off, empty-handed through screen1.

At solving time, the profile's beam, budget and keeper sections were blind on
this problem (G20-G22). T37 update, 2026-09-27: G22 is resolved; gate8/gate9
share one demand of 3. H1 evaluates the pooled budget but gives no necessity
hint: four bodies remain after the goal actor leaves the pool. This does not
account for reserving box2 as the jump support, so the earlier domain reasoning
still matters. T36 update, 2026-09-27: G21 is resolved. S4 uses all 12 quotient
rows, gives directional verdicts for gate8/gate9, and evaluates H2/H4. H2 has
two keeper hints; H4 has no applicable hints. S1 override qualifications and
the limits of relaxed graph reachability still apply. T35 update, 2026-09-27:
G20 is resolved. MC has zero UNCOVERED mechanics; automated beam-direct,
jammer and stairs contracts supplement the historical hand contracts above.
RC/H3 identify gate1 and location2 for the fixed transmitter1 -> receiver1
corridor. Jammer sightlines are geometric candidates under explicit gate
premises, not proof of applicable placements. All G20-G22 gaps are resolved
at their documented scope; the validated 36-action solution is unchanged.

## D's tricks

Interview opened backward (D, 2026-09-27).  Agreed penultimate state: agent1 at location12,
plate1-3 all weighted.

| trick | hypothesis | CONSISTENT / CONTRADICTED / NEEDS | evidence |
|---|---|---|---|
| End arrangement (A, agreed backward step) | Before the agent leaves the plate room for good: box1 and both jammers on plate1-3, one jammer jamming gate1, the other gate5; box2 at location10; location2 empty | CONSISTENT; NEEDS the beam already lit when the gate1 jam is placed | Sightline check 2026-09-27 (JAMMER-TARGET-VISIBLE-FROM-PLACEMENT, jammer on each plate): gate5 visible from location4/5/6 with all gates closed; gate1 visible only with gate2 and gate3 open (D) |
| Recover jammer2 (D) | jammer1 jams gate1 from location1; agent reaches location7 and picks jammer1 up through the window (beam off); jams gate5 from location8; walks empty-handed through gate4, screen1, gate5 to location9 for jammer2 | CONSISTENT on steps 1, 3, 4; step 2 answered by D: box1 put at location2 from location3 cuts the beam and reopens gate4.  Steps 1-3 FOUND by search: 8 actions, min-length at cutoff 8; steps 4-5 VALIDATED by hand from that endpoint (4 actions: move location8, jam gate5 with jammer1 at location8, move location9 via gate4 gate5 screen1, pickup jammer2) | Check 2026-09-27, gate4 open, others closed: jammer1 on the ground at location1 sees gate1 T; at location8 sees gate5 T; REACHABLE location1 from location7 T.  Search 2026-09-27 from start, goal agent1 at location7 holding jammer1: pickup-jammer, jam gate1 at location1, move to location4 (gate1 gate3), pickup box1, move to location3, put box1 at location2, move to location7 (gate4 screen1), pickup jammer1 |

## Probe map

Not run: D asked for no probe battery.

## Result (2026-09-27)

SOLVED.  constraint-evidence/full-solution-candidate.lisp, 36 actions from the start,
VALIDATE-ACTION-SEQUENCE from the staged start: SUCCESS-P T, goal agent1 at location11
satisfied.  Actions 1-8 found by search (min-length, cutoff 8); 9-36 hand-derived in the
interview.  Final state is the end arrangement: box1 on plate1, jammer1 on plate2 jamming
gate5, jammer2 on plate3 jamming gate1, box2 at location10.  Not claimed shortest.

The tricks, in order: (1) jam gate1 from location1 and cut the beam with box1 at
location2 to reopen gate4; (2) take jammer1 back through the window from location7;
(3) jam gate5 from location8 to reach jammer2; (4) park jammer2 on a plate holding gate5;
(5) jam gate1 by placing jammer1 at location1 from location7, through the window;
(6) climb the ladder and clear location2, lighting the beam; (7) hand gate1 over to a
plate jammer, then move jammer1 onto the last plate holding gate5.

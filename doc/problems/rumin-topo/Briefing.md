# rumin-topo — Briefing

Profile: doc/problems/rumin-topo/Constraint-Static-Profile.txt (generated 2026-09-29; SHA-256 pending).
Maximum search depth: 10.
Diagram: doc/problems/rumin-topo/diagram.png (read through the device link).

## Spec-diagram check

Diagram: diagram.png, updated 2026-09-29 (3300x2550, 300 dpi grayscale, titled RUMINATION).
Spec: probs/problem-rumin-topo.lisp as updated by D 2026-09-29. Viewed as overview plus six
full-resolution tiles.

MATCH
- Boundary polygon (18 vertices, east section to x=38, y=1, notch x 27-30, y 4-6).
- wall1-wall5, wall7-wall16 (wall10 now labelled on the horizontal y=16 segment);
  edge1-edge5; window1; gate1-gate6.
- Transmitters and receivers with hues; location1-location17 coordinates; recorder1,
  agent1, tray1 (now at location2), connector1, connector2, box1; plate1-plate4; ladder2.
- Stairs: west stairs (x 7-10, y 11-12, reached from location2's floor through the opening
  at x=7, y 13-15) = stairway location2-location4; east stairs (x 12-13, y 8-11) = stairway
  location13-location17; stairs x 21-22, y 4-7 with gate4 at their head = stairway
  location9-location10, clause ((gate4)) (applied by A on D's instruction).
- Jump arcs: location8-location9 across edge4.

MISMATCH / to fix
1. Jumping location13-location17 is authored as two facts with families (edge2) and (edge3).
   Invalid: each clause must be a list, jumping clauses accept only gate/screen/wall, and
   traverse-via is keyed on (mode source destination), so the two facts collide. Proposed:
   one fact (traverse-via jumping location13 () location17).
2. Jumping location2-location4 across edge1 is drawn but commented out in the spec.
   Proposed: restore (traverse-via jumping location2 () location4).
3. Minor: the vertical x=6, y 16-18 label is hard to read ("w1D"); spec calls it wall11.

UNSETTLED (not on drawing): heights and levels (consistent with edges: 3/2 platforms, edge5
2), CONTROLS wiring, reach-disallowed location10-location17, pairing capacity, recorder cycles.

Status: items 1-2 await D. Profile must be regenerated after the spec settles.

## Summary

Goal: agent1 at location16 (level 2), and the final recorder cycle closed by a ghost STOP
(ghost agent back at the recorder, location3, empty-handed, no live/ghost HOLDING or ON).

Route (S3, SD): location3 (R2) -> R1 through gate1 -> location14 (R3) through gate5 ->
location5 (R4) by ladder2 (one way) -> location16 (R5) through gate6.
Necessary doors: gate1, gate5, gate6, ladder2.

Controllers (S1): gate1 = receiver1 (blue); gate2 = not receiver1 (open at start);
gate3 = plate1; gate4 = plate2; gate5 = receiver2 (red) AND plate3; gate6 = plate4.
gate1 and gate2 exclude each other.

Beams (S6, RC): both transmitters are visible only from location2 and location3, and only
at connector height 2 (on a box) or 5/2 (on a held tray); both hues are visible there, so a
connector there must pair to exactly one transmitter. receiver1 is seen from location10,
location13, location17 (and location1, location12 through gate1). receiver2 is seen only from
location11 and location12. connector1 starts at location13 already paired to receiver1.

Bodies: live agent1, box1, tray1, connector1, connector2; each doubled by a ghost during a
cycle. At the start, the only riser in R2 is tray1; box1 is at location7 (behind gate2 and
gate3), connector2 at location10 (behind gate1). Recorder1 is at location3. Max 5 cycles.

## Difficulties

1. R4 is one way: nothing that climbs ladder2 can return. A ghost cannot hold plate4 and
   still reach the recorder. NEEDS check: ladder climbing appears to require empty hands
   (OBSTACLE-CLEAR's ladder arm reads HOLDING), so plate4's weight would go from location14
   to location5 by reach, not carried up.
2. The recorder is in R2, and gate1 closes whenever receiver1 goes dark. Every cycle begins
   with both agents at location3, so the final cycle must carry agent1 from location3 all the
   way to location16 while the ghost returns to location3.
3. First lighting of receiver1: the only R2 riser is tray1, and a held tray (top 3/2) cannot
   be loaded from the ground. A second body is needed: a ghost holding the tray at location2,
   loaded by a live agent standing at location4 (level 3/2). A live connector on a ghost tray
   is a cross-layer ON that blocks STOP until removed.
4. Competing hues at location2/location3 for any feeder connector.
5. Depth 10 per search against a long plan: many subgoals.

## Contracts

No UNCOVERED mechanics (MC: 15 of 15 covered).

## Hints (qualified)

- H2.2/H2.3 (graph candidates): a body other than the crosser on plate3 while crossing
  gate5, and on plate4 while crossing gate6.
- H6.1: gate2 open at the start.
- H7.1: load a held tray from a raised location (location4, location13, location9, ...).

## Subgoal log

| subgoal | whose idea | check | result |
|---|---|---|---|
| SG1: agent1 into R1 through gate1, cycle 1 open (ghost holds tray1* at location2; live agent1 carries connector1 from location13 to location4, places it on the ghost-held tray, pairs it to transmitter1 and to connector1* at location13) | A | CONSISTENT: location2@5/2->transmitter1 ALWAYS (S6); location2@5/2->location13@5/2 ALWAYS (RC); location13@5/2->receiver1 ALWAYS (S6). NEEDS: live placement onto a ghost-held tray from location4; live pairing to a ghost connector (rule 20) | proposed |

## Result

None yet.

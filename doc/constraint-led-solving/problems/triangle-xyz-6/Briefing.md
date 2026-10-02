# triangle-xyz-6 — Briefing (pilot)

Pilot of the constraint-led method on a problem with no tech/ mechanics.  There is no generated
profile: tech/constraint-profile.lisp reads talos mechanics only.  Its place is taken by a hand
static analysis from the spec's action definitions (Static facts below); the computation is in
static-analysis.py in this folder (A, 2026-10-01; numerical, run outside Wouldwork).
Maximum search depth: 4, set by D 2026-10-01.  The cutoff bounds searches only; a hand-derived
sequence of any length can be validated (D, 2026-10-01).

## Spec-diagram check

- MATCH: header board diagram (positions labelled xy) and the init loop: 21 positions, x+y+z = N+2,
  hole at (1,1,6) = "11", 20 pegs.
- MATCH: the six jump guards (`<= ... (- *N* 2)`, `>= ... 3`) admit exactly the jumps whose landing
  position is on the board: 60 directed jumps on 30 lines, 0 mismatches (static-analysis.py).
- Note: each jump's effect variables are `($x $y)` (src/ww-installer.lisp, ww-planner.lisp), so an
  action prints and replays as `(jump-<dir> x y)`, the jumping peg's position; peg names never
  appear in action forms.
- MATCH: D's `(display-validation-state *start-state*)` (2026-10-01): 20 pegs on every position
  except (1 1 6), PEG-COUNT 20, BOARD-PEGS PEG1-PEG20.  No derived facts, as expected.

## Summary

21-position triangular board, side 6, all filled except the top corner 11.  A move jumps a peg over an
adjacent peg into an empty position along a line, removing the jumped peg.  Goal: one peg left.

## Difficulties

1. Moves are irreversible and every move removes a peg: dead ends are everywhere, and a bad early
   move is only found out much later.
2. No bottlenecks or controllers: every move interacts with every other, so the talos-style
   decomposition (regions, services, setup) has nothing to grip.
3. Searches cost: 19 moves in all, and peg names make boards reached by different routes distinct
   states (see S6).

## Contracts

- **jump** (jump-LD/RU/RD/LU/RH/LH, one contract): requires a peg at `from`, a peg at `over` and an
  empty `to`, all on one line; moves the peg from `from` to `to`, removes the peg at `over`;
  decrements peg-count by 1.  Nothing else changes.  No static or derived (propagated) facts.

## Static facts (hand profile)

- **S1 counter [definitional].** Every action lowers peg-count by exactly 1, so every solution is
  exactly 19 moves; MIN-LENGTH adds nothing, and a subgoal at peg-count k is exactly 20-k moves
  from the start.
- **S2 conserved quantities [inductive].**  Colour each position by (y - z) mod 3.  The three
  positions of any line have different colours, so every jump changes each colour count by 1.
  The parities of the count differences are therefore invariant.  The GF(2) invariant space of
  the jump vectors has dimension 2 and is exactly these colour-pair parities, so the colouring
  was computed, not recalled.
  Class 0: 12 15 23 31 34 42 61.  Class 1 (the hole's): 11 14 22 25 33 41 52.  Class 2: 13 16 21 24 32 43 51.
- **S3 final position [inductive, necessary only].**  The last peg must be on class 1:
  11, 14, 22, 25, 33, 41 or 52.
- **S4 pagoda bound [inductive].**  A linear-programming search for a non-increasing weighting
  (w(to) <= w(from)+w(over), weights in [-1,1]) excludes none of the seven.  Not a proof that any
  of them is reachable.
- **S5 roles [definitional].**  Corners 11, 16, 61 are never jumped over.  A corner peg leaves only
  by its own jump: 16 by RU over 15 to 14 or RH over 25 to 34; 61 by LU over 51 to 41 or LH over
  52 to 43.  So the pegs at 16 and 61 must each jump out at some point unless one of them is the
  last peg (neither corner is in class 1, so both must).  Corner 11 is filled only from 13 over 12
  or from 31 over 21.
- **S6 symmetry [definitional].**  Swapping x and y preserves the jumps and the start, so the two
  first moves (13 over 12 to 11, 31 over 21 to 11) are mirror images: fixing the first move halves
  the search.  Peg names are interchangeable for the goal, but the state records them, so boards
  reached by different routes are different states (a spec-level search cost; not changed here).

## Hints

- **H1 NECESSARY (S5).**  Both corner pegs 16 and 61 jump out of their corners before the end.
- **S7 pagoda dead-board test [inductive, per board].**  A board is dead for a finish at 11 if some
  weighting obeying w(to) <= w(from)+w(over) gives it less than position 11 (one LP per board).
  After the fixed first move, none of 4 / 23 / 125 / 573 / 2,184 / 6,539 / 15,507 boards at moves
  2-8 is dead.  The static facts give no guidance in the opening; they may bite late.
- **H2 CANDIDATE (S3, S5).**  Finishing on corner 11 forces the last move (13 over 12, or 31 over 21),
  which gives a definite penultimate state for working backward.

## Subgoal log

Opening (A, 2026-10-01): refine the goal to "last peg at 11" (premise, retractable); penultimate
state = pegs only at 12 and 13, or the mirror 21 and 31.  Searches reach 4 moves past a checkpoint;
hand-derived segments may be longer.  First move fixed as 13 over 12 to 11 (S6).

| subgoal | whose idea | check | result |
|---|---|---|---|
| SG1: corner 16 peg out (16 pegs; empty 13 15 16 22 31) | A, from H1 | CONSISTENT: H1; not pagoda-dead (S7) | ACCEPTED, hand-derived, 4 actions |
| SG2: corner 61 peg out (13 pegs; empty 13 15 16 22 33 42 51 61) | A, from H1 | CONSISTENT: H1; not pagoda-dead (S7) | ACCEPTED, hand-derived, 3 actions (7) |
| SG3: bottom edge empty except 43 (9 pegs) | A, from the SG2 look-ahead | CONSISTENT: unique board within 4 moves; not pagoda-dead (S7) | REJECTED by D 2026-10-01: finish at 11 lost (S8) |
| SG3 (redo): bottom edge down to 25 34, finish at 11 kept (9 pegs) | A, S8 | CONSISTENT: S8 live for 11; no corner peg | ACCEPTED, hand-derived, 4 actions (11) |
| SG4: meeting board 11 12 21 23 32 (5 pegs) | A, S8 | CONSISTENT: in both the forward 4 and backward 4 sets | ACCEPTED, hand-derived, 4 actions (15) |
| SG5: goal, one peg | original goal | none needed | ACCEPTED by closure, 4 actions (19); finishes at 41 |

**SG1.** 13 over 12 to 11; 31 over 22 to 13; 14 over 13 to 12; 16 over 15 to 14.  Checked move by
move against the spec guards (A, script).  Look-ahead: the mirror corner 61 (SG2) leaves by 51 over
41 or 52 over 43; both lines are still full.  D's replay 2026-10-01: success T, goal not satisfied, PEG-COUNT 16,
empty positions as stated.  ACCEPTED.

**SG2.** 51 over 41 to 31; 33 over 42 to 51 (refills 51); 61 over 51 to 41.  Checked move by move
(A, script).  After it H1 is discharged: neither bottom corner holds a peg.  Look-ahead: the peg at
11 must leave (by 12 to 13 or 21 to 31) and a peg return there last; the bottom-edge pegs 25 34 43 52
can be removed only along the bottom edge or by jumping upward.  D's replay 2026-10-01: success T,
PEG-COUNT 13, empty positions as stated.  ACCEPTED.

**SG3.** A's bounded check from the SG2 board (script, no Wouldwork search): within 4 moves the
bottom edge can be reduced to one peg on exactly two boards, at 43 or in corner 16, both with the
same upper part (11 12 14 21 23 24 32 41).  43 is chosen: a peg in corner 16 would have to leave
again.  Each move lowers the bottom-edge count by at most 1, so 4 moves are needed.  S7 begins to
bite here: 38 of the 431 boards 4 moves from SG2 are pagoda-dead for a finish at 11.
Realized by one MIN-LENGTH search at cutoff 4 from the replayed SG2 endpoint, to exercise the search
step.  Method gap (pilot): no checkpoint can be built from a hand-derived prefix, so the search uses
SOLVE-SUBGOAL's raw-state form and the found actions are appended to Actions.lisp by hand.
Expected: 4 actions; end board 11 12 14 21 23 24 32 41 43.
D's search 2026-10-01: MIN-LENGTH, cutoff 4, threads 16, found 4 actions: 31 over 32 to 33; 34 over
33 to 32; 52 over 43 to 34; 25 over 34 to 43.  End board as expected.  Endpoint review: see S8.

**S8 liveness [exhaustive within the fixed remaining length] (A, live.py).**  Since S1 fixes the
number of remaining moves (pegs - 1), meeting a forward enumeration from a board with a backward
(un-jump) enumeration from a one-peg finish, over exactly that many moves, decides whether that
finish is reachable.  Here an exhaustion is a refutation, not a cost bound: the Guide's invariant
assumes open-ended length.  Finishes still reachable:
  SG1 board: 11 14 22 25 33 41 52 (all seven allowed by S3)
  SG2 board: 11 14 25 41 52
  SG3 board: 14 25 41 52 -- the finish at 11 was lost at SG3; S7 did not detect it.
The SG3 proposal checked only S7; the liveness check is the look-ahead it lacked.
Decision for D: retract the finish-at-11 premise and continue from SG3 (the actual goal is any one
peg), or reject SG3 and choose another SG3 from the SG2 board.


**SG3 redo.** D rejected the first SG3 (2026-10-01).  A's selection (liveness.py functions): of the 431
boards 4 moves from SG2, 28 keep the finish at 11.  Among them the fewest bottom-edge pegs is 2; of
those, two have no corner peg (bottom 25 34, or 43 52); A chose 25 34.  The board is reached by one
sequence only: 31 over 32 to 33; 14 over 23 to 32; 34 over 24 to 14; 52 over 43 to 34.
End board 11 12 14 21 25 32 33 34 41.  Pilot note: from the midgame the subgoal is selected by the S8
liveness check, an exhaustive computation, rather than by the static constraints.
D's replay 2026-10-01: 11 actions, success T, PEG-COUNT 9, board as stated.  SG3 ACCEPTED.

**SG4.** With 8 moves left, the boards 4 moves forward from SG3 that are also 4 un-jumps back from a
single peg at 11 number 6 (liveness.py functions).  A chose 11 12 21 23 32: compact at the top and the
only one with two finishing sequences.  Moves: 25 over 34 to 43; 41 over 32 to 23; 14 over 23 to 32;
43 over 33 to 23.  **SG5** is the final leg, one MIN-LENGTH search at cutoff 4 for the original goal
from the replayed SG4 endpoint, then full closure by loading Actions.lisp.
D's search 2026-10-01 (MIN-LENGTH, cutoff 4, threads 16, from the replayed SG4 endpoint; the replay
confirmed SG4): 11 over 12 to 13; 13 over 23 to 33; 33 over 32 to 31; 21 over 31 to 41.  One peg at 41.
The search goal was the original goal (PEG-COUNT 1), not A's finish-at-11 premise, so the search took
another class-1 finish; the two finishes at 11 from SG4 remain unused.

## Result

CLOSED 2026-10-01.  19 actions (4 + 3 + 4 + 4 + 4) validated from the original start against the
original goal by loading Actions.lisp: SUCCESS-P, GOAL-CHECKED-P and GOAL-SATISFIED-P all T, solution
validators accepted; Validation.txt written.  Last peg at 41.  Hand-derived: SG1-SG4 (15 actions);
search-found: SG3's rejected first version and SG5.  A's finish-at-11 premise steered SG3-SG4 and was
not used by the final search.  Not claimed shortest (every solution is 19 moves anyway, S1).

## Pilot findings

1. **The process transferred unchanged.**  Intake, spec check, Briefing, subgoal dialogue with
   CONSISTENT checks, hand or search realization, Actions.lisp closure.  Only the record location
   differed (doc/constraint-led-solving/constraint-pilot/, so no talos record was touched).
2. **The static layer had to be rebuilt from the actions**, without tech/: counter (S1), GF(2)
   invariants (S2-S3, which recovered the colour classes by computation), roles (S5), symmetry (S6),
   pagoda (S7).  Useful: fixed length, possible finishes, corners must exit (drove SG1-SG2), the
   symmetric first move.  Not useful: pagoda in the opening (no dead board in 8 moves), and nothing
   static detected that the first SG3 lost the finish at 11.
3. **The decisive check was S8 liveness**, an exhaustive forward/backward meet over the remaining
   moves.  Because S1 fixes the remaining length, it refutes rather than bounds: an exception to the
   Guide's invariant that an exhaustion is a cost bound.  From the midgame on it chose the subgoals
   (SG3 redo, SG4); constraints became structural preferences (no corner pegs, few bottom pegs)
   applied to its live set.
4. **Method gaps.**  No search checkpoint can be built from a hand-derived prefix (raw-state
   SOLVE-SUBGOAL used, results appended by hand).  The Guide's MIN-LENGTH rule is moot when length is
   fixed.  A search goal must carry the working premises (here the finish at 11) or the search takes
   another finish.  Look-ahead must include a liveness check, not only static consistency.
5. **Representation.**  Peg names did not affect replay forms ((jump-<dir> x y)); they cost about 10%
   extra states per depth-4 search, growing with depth.

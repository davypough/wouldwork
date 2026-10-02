# Problem Classification Guide

Classify a Wouldwork problem by the shape of its state space, then choose strategies from the
class.  Written 2026-10-01 from the triangle-xyz-6 pilot (`doc/constraint-pilot/triangle-xyz-6/`),
the talos constraint-method work (`doc/constraint-method/`), a state-space count of
tiles1e-heuristic, and the *Wouldwork User Manual (26.8)*, Part 3.

**Relation to the Manual.**  Part 3's *Decision Outline* chooses settings by **objective** (one
path, every path, an optimal path; planning vs csp).  This guide adds the **structural** questions
the outline does not ask: can moves be undone, is the length fixed, how large is the space, where
does the difficulty lie.  The two are orthogonal: classify structure here, then pick the objective
settings from the Manual.  Section 6 lists Manual passages that apply and some that need correcting.

---

## 1. How to use it

1. Answer the diagnostics in section 2.  Each is settled from the **spec** (S) or by a cheap
   **probe** (P): a few shallow searches, never a full solve.
2. Walk the hierarchy in section 3 to a leaf.  A problem may need two leaves (section 3.4).
3. Take the leaf's strategy bundle; details and conflicts are in section 4.
4. Re-classify when the problem changes phase (opening vs endgame, setup vs execution).

The first strategy is always brute force on a small version, to validate the spec (Manual,
*Brute-Force Search*).  Classification decides what to try when that does not scale.

---

## 2. Diagnostics

| Id | Question | How decided | Why it matters |
|---|---|---|---|
| D1 | Is the answer a sequence of actions, or an assignment of values? | S: does the goal test only final values, and is each variable set once? | Assignment problems belong to CSP strategies even when written as planning. |
| D2 | Is a score optimized? | S: `*solution-type*` min-length, min-time, min-value, max-value | Enables bound-based pruning; changes what "exhausted" means. |
| D3 | Are there exogenous events or time? | S: `define-happening`, `define-patroller`, action durations | Graph search is unsound with happenings (Manual, *Exogenous Events*): tree only. |
| D4 | Can every move be undone? | P: from sampled states, check each successor can return in one move; S: look for one-way moves, consumption, irreversible commitments | Reversible: no dead ends.  Irreversible: dead ends, and look-ahead matters. |
| D5 | Does some quantity change by a fixed amount on every move? | S: action effects (e.g. peg-count - 1) | Fixes the solution length.  Then MIN-LENGTH is moot, and an exhaustive search over the remaining length is a **refutation**, not a cost bound. |
| D6 | How large is the reachable space? | P: states per level for the first few depths and the duplicate ratio; estimate b^d (Manual) | Small spaces need no cleverness.  High duplicate ratio favours graph search; low favours tree. |
| D7 | Are there derived relations computed by propagation? | S: `define-update`s called from `propagate-changes!`, tech/ mechanics | Propagation cost per state can dominate; relaxation targets it. |
| D8 | Is the difficulty concentrated in a few controllers or chokepoints? | S: gates, switches, plates, receivers; regions joined only through controlled barriers | Bottlenecks give natural subgoals (constraint-led method). |
| D9 | Are objects or positions interchangeable? | S/P: same type and static facts, not named in the goal; board automorphisms | Symmetry pruning; fixing one symmetric first move. |
| D10 | Is the problem one member of a family with a size parameter n? | S: a `*N*`-style parameter, regular board or layout | Large n needs induction over reusable patterns, not search. |
| D11 | Can goal states be listed, and moves reversed? | S: is the goal a small set of explicit states (one peg at a given position) or a partial description? | Decides whether backward search, bidirectional search or the enumerator is practical. |
| D12 | Is there a measurable distance to the goal? | S: can a cheap function rank states (Manhattan distance, unmet goal conditions)? | Heuristic beam search needs a gradient; dense dead-end problems often lack one. |
| D13 | Do the constraints derived from the actions bite? | P: invariants (parity of weighted sums of state facts) and lower bounds computed from the action effects | Turns hard facts into pruning (`prune-state?`, `min-steps-remaining?`) and goal refinement. |

---

## 3. The problem type hierarchy

```
A  Assignment (D1)
   A1  Pure satisfaction
   A2  Optimized assignment (D2)
   A3  Assignment written as planning
B  Sequence (D1)
   B1  Timed or exogenous (D3)
   B2  Untimed
       B2.1  Reversible (D4)
             a  small (D6)
             b  large, measurable distance (D12)
             c  large, no useful distance
       B2.2  Irreversible, fixed length (D4, D5)
             a  small enough for exhaustive search to the end
             b  large
             c  scalable family, large n (D10)
       B2.3  Mixed: undoable movement plus irreversible commitments or device state (D4)
             a  little derived state
             b  derived state and bottlenecks (D7, D8)
Objective modifier (D2) applies at any leaf: first, n, every, min-length, min-time, min/max-value.
```

### 3.1 A — Assignment problems

**A1 Pure satisfaction.**  N-queens, logic puzzles (queensN-csp, captjohn, tiles0a-csp).
`*problem-type*` csp, backtracking + tree, `*depth-cutoff*` 0 (search to the number of variables),
symmetry pruning when values or objects are interchangeable, `first` or `every`.  Order variables
so the most constraining come first.

**A2 Optimized assignment.**  Knapsack-style (knap4b, knap19).  min-value or max-value with a
`bounding-function?` that computes an optimistic value cheaply (greedy, fractional relaxation).
Note: `bounding-function?` is ignored by the backtracking algorithm (src/ww-initialize.lisp).

**A3 Assignment written as planning.**  Each item placed once, order irrelevant (crossword5-11 is
planning + tree + max-value; to be confirmed when probs/ is reviewed).  Either convert to csp, or
impose a fixed order (place item k only after item k-1) so each assignment is generated once.
Tree search, since states rarely repeat.

### 3.2 B — Sequence problems

**B1 Timed or exogenous** (sentry, mine1).  Tree search is mandatory with happenings; min-time
when durations matter; `define-constraint` for global safety conditions (e.g. never share an area
with the sentry); wait actions.  Subgoals must carry a time or phase, because the same position at
a different time is a different state.

**B2.1 Reversible.**  No dead ends: every reached state is safe, an exhausted search is only a cost
bound, and subgoals cannot ruin the puzzle.  The question at each step is distance, not liveness.
- **a Small** (tiles1e: 866 reachable states, shortest solution 40).  Plain graph search,
  min-length, no depth cleverness.  Constraint-led subgoals only add overhead.
- **b Large, measurable distance.**  Heuristic beam search (serial only) or, for optimal paths,
  a `min-steps-remaining?` lower bound with min-length.  Stronger bounds come from exact solutions
  of a simplified problem (keep the target object and its main blockers, drop the rest; a pattern
  table).  Bottleneck states make natural subgoals for goal chaining.
- **c Large, no useful distance.**  Bidirectional search when goal states can be listed (D11);
  macro operators found with `freq`; parallel tree search.

**B2.2 Irreversible, fixed length** (triangle peg solitaire).  Dead ends everywhere, no gradient
early on, but the length is known and goal states are explicit.
- Use `first` (MIN-LENGTH adds nothing) with the depth cutoff at the exact length.
- Prune dead states with `prune-state?`: invariants (triangle: colour-class parities) and pagoda
  bounds.  These are weak in the opening and stronger late.
- Symmetry: fix one of the symmetric first moves; consider removing object names that the goal
  ignores (triangle: peg names cost ~10% more states at depth 4, 2.5x at depth 7).
- Bidirectional search (`problem-triangle-backward.lisp`, `encode-state`, `get-state-codes`) is the
  natural engine form; with fixed length, a failed meet over the full remaining length proves the
  board dead.
- **a Small enough:** exhaustive search or bidirectional meet to the end.
- **b Large:** subgoals chosen by liveness (does any state at the meeting depth connect?) with
  constraints as preferences; macro operators.
- **c Scalable family, large n:** induction.  Find a short local pattern that clears a block given
  a nearby hole and helper, check it once by small search, and apply it repeatedly to reduce n to a
  solved base case.  No global search.  Needs translation regularity (D10).

**B2.3 Mixed** (talos problems, corner).  Movement is reversible, but placements, jams, recordings
and one-way passages are commitments, and device state is derived.
- **a Little derived state:** standard graph search with min-length; goal chaining for depth.
- **b Derived state and bottlenecks:** the constraint-led method
  (`doc/constraint-method/Problem-Solving-Guide.md`): static profile, subgoals at controllers and
  barriers, look-ahead before each subgoal, checkpoint searches at a fixed maximum depth.  Add
  relaxation when propagation dominates (Manual, *Relaxation*; corner-relaxed), the enumerator's
  meet-in-the-middle when goal states can be generated from base relations (Manual,
  *Meet-In-The-Middle Search*; corner), and goal-counting heuristics.

### 3.3 Objective modifier

Applied at any leaf (Manual, *Decision Outline*): `first` / n to establish solvability;
`every` / `all-paths` for enumeration (symmetry pruning removes variants); min-length, min-time,
min-value, max-value for optimization, with `min-steps-remaining?` or `bounding-function?` to prune.
With a fixed length (B2.2) min-length is pointless.

### 3.4 Problems in more than one class

Classify by phase when the structure changes: a talos problem can be B2.3b during setup and B2.1
on its final walk; peg solitaire is constraint-friendly in the opening (forced corner exits) and
search-friendly in the endgame (exhaustive liveness).  Re-run D4, D6 and D12 at each checkpoint.

---

## 4. Strategy catalogue

| Strategy | Setting or form | Pays off when | Conflicts and cautions |
|---|---|---|---|
| Brute force, iterative deepening | `first`, rising `*depth-cutoff*` | always first, on a small version | exponential in depth |
| Graph vs tree | `*tree-or-graph*` | graph: many repeated states (D6); tree: few, or happenings | happenings require tree; parallel speedups are better with tree (Manual) |
| Backtracking | `*algorithm*` backtracking (REPL only) | CSP, trees without repeats | ignores `bounding-function?` and `min-steps-remaining?` |
| Parallel | `*threads*` | large searches | heuristic beam is serial only; changing threads restages |
| Depth bound | `*depth-cutoff*` | known or suspected solution depth | an exhaustion is a cost bound unless the length is fixed (D5) |
| Symmetry pruning | `*symmetry-pruning*` t | interchangeable objects not named in the goal (D9) | overhead; removes variants under `every` |
| Dead-state pruning | `prune-state?` | invariants or bounds prove a state dead (D13) | must be sound: never prune a live state |
| Lower-bound pruning | `min-steps-remaining?` | min-length, or a depth cutoff, with an admissible bound | must never overestimate |
| Value bound | `bounding-function?` | min/max-value with a cheap optimistic estimate | not used by backtracking |
| Global constraints | `define-constraint` | safety conditions that hold in every state (B1) | — |
| Heuristic beam | `heuristic?` | a gradient exists (D12) | serial only; not optimal (`doc/search/heuristics.md`) |
| Macro operators | extra actions; `freq` | recurring multi-move patterns; B2.2c patterns | each macro adds work per state |
| Goal chaining, checkpoints | `solve-subgoal`, checkpoint export/import | bottleneck subgoals; depth beyond one search | search goals must carry working premises; no checkpoint can be built from a hand-derived prefix (pilot) |
| Relaxation | relaxed preconditions, goal post-validation | propagation dominates (D7) | relaxed test must be implied by the true test |
| Bidirectional | backward spec, `encode-state`, `get-state-codes`, `backward-state-exists` | explicit goal states, reversible actions (D11) | memory for the backward layer |
| Enumerator meet-in-the-middle | `define-base-relation`, `find-goal-states`, `find-predecessors`, `solve-meeting-point` | goal described by base relations, derived state inferable | backward layers can explode |
| Constraint-led method | `doc/constraint-method/` | B2.3b; bottlenecks and couplings | heavy process for B2.1a or B2.2 endgames |
| Induction over patterns | method, outside the engine | B2.2c, scalable regular families | needs regularity away from edges |

---

## 5. Worked classifications

| Problem | Path | Evidence | Strategy that worked or is indicated |
|---|---|---|---|
| triangle-xyz-6 | B2.2b, endgame B2.2a | 19 moves fixed (peg-count); irreversible; 2 GF(2) invariants; dead-board test weak early | opening by constraints (corner exits, symmetric first move), midgame and endgame by liveness meet; pilot closed in 19 actions |
| triangle, large n | B2.2c | regular board | induction over clearing patterns |
| tiles1e-heuristic | B2.1a | 866 reachable states, shortest 40 (script, 2026-10-01) | one graph search, min-length |
| claustro-topo and other *-topo | B2.3b | derived gate and beam state; controllers and barriers | constraint-led method; closed in 36 actions |
| corner | B2.3b | heavy propagation | relaxation (corner-relaxed, ~400 s); enumerator meet-in-the-middle |
| knap19, knap4b | A2 (as planning) | max-value, `bounding-function?` | branch and bound |
| queensN-csp, captjohn | A1 | csp | backtracking + tree |
| sentry, mine1 | B1 | happenings, patroller | tree search, constraints |

Rows other than the first four are from spec settings only; they will be checked when probs/ is
reviewed.

---

## 6. Notes on the Manual (26.8)

**Applies directly.**  Part 3's *Decision Outline* and *Quick Reference* (objective settings);
*Brute-Force Search* (validate small first; b^d size estimate); *Problem Types* and *Algorithm
Types* (A vs B; backtracking for trees); *Exogenous Events* (tree only); *Symmetry Pruning*;
*Relaxation*; *Searching with Macro Operators* (`freq`); *Bi-Directional Search* and
*Meet-In-The-Middle Search*; *Goal Chaining*.

**Missing or inconsistent (for a future Manual revision; not changed here).**
- The optimization section names the bounding query `get-best-relaxed-value?`; the knapsack
  appendix and the source use `bounding-function?` (src/ww-initialize.lisp).
- `prune-state?` and `min-steps-remaining?` are supported by the source but not described.
- *Bi-Directional Search* cites `problem-triangle-forward6.lisp` and `problem-triangle-backward6.lisp`;
  probs/ holds `problem-triangle-backward.lisp` and no forward6 file.
- No structural classification: nothing on reversibility (D4), fixed length (D5) or what an
  exhausted search means in each case.

**Updated 2026-10-02:** The Manual’s Goal Chaining section now includes “Manual continuation from saved checkpoints”, covering explicit serial/parallel continuation, export/import, and complete-path validation.

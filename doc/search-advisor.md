# Wouldwork Search Advisor

> **Usage:** Use this specialist procedure through [consultant.md](consultant.md)
> for a search decision on a characterized problem. Select only the relevant profiling,
> probing, improvement, or estimation procedures. If invoked directly, establish the
> consultant's objective, approval scope, and shared record first. Source-only advice
> needs no run; proposed experiments and edits require their own authorization.

> **Status:** Source-checked 2026-10-03 against `src/ww-settings.lisp`, `ww-initialize.lisp`,
> `ww-searcher.lisp`, `ww-planner.lisp`, `ww-parallel.lisp`, `ww-parallel-infrastructure.lisp`,
> `ww-validator.lisp`, `ww-support.lisp` and the enumerator,
> and against the *Wouldwork User Manual (26.8)*, Part 3.  Where they disagree the source wins;
> the disagreements are listed in section 7. Process and evidence guidance integrated
> with the consultant on 2026-10-07; this was not a fresh audit of every technical claim.

**Scope.** Choose an efficient search approach for the user's objective, with explicit
assumptions about the model. Fidelity review belongs to the [specification advisor](spec-advisor.md).
Read the current model and fidelity evidence; a spec that stages is not thereby faithful.
Conditional analysis is useful, but resolve consequential model uncertainty before relying
on its results. A probe exposing a discrepancy returns it to the consultant as a specification
topic; pause dependent work and preserve unaffected evidence. This procedure does not
authorize a full solve.

**Entry and return.** Read the selected question, objective, relevant record entries,
current artifacts, and approval limits. Reuse current evidence instead of repeating
completed phases. Write findings to `doc/problems/<problem-name>/Analysis.txt` for short
and sustained work alike; link a separate text evidence artifact only when detail warrants
it. Return the conclusion, evidence and scope, assumptions, unresolved issues, affected
dependencies, and recommended next action. Stop when the question is answered, needs
approval, or needs another procedure. Use the consultant's invalidation rules after changes.

**When one search appears impractical.** If evidence suggests one search is unlikely to
finish within the available budget, return a milestone-strategy topic to the consultant
with its supporting estimates and uncertainty. A short probe is not proof that no single
search can finish. The [solving advisor](solving-advisor.md) supplies a constraint-led
dialogue: the user and assistant agree one
subgoal at a time, check it against the problem's static constraints, realize it with a
bounded search, and finally validate the whole chained path.  It was built for Talos problems,
but its subgoal dialogue applies to any problem with milestones (Q16).

---

## 1. Process

The four phases, numbered 0 to 3, are selectable procedures, not a required pipeline.
The consultant chooses the next consequential question and its dependencies. Phase 0
reads and reasons without staging or running; it can be the whole requested consultation.
Use probes only for unresolved decisions, upgrades only when justified, and estimates
only when they affect the objective. Skip irrelevant work and defer nonblocking unknowns.
Record results before returning for approval, redirection, or completion.

Measurements belong to a named model revision. If an approved phase-2 change affects a
phase-3 estimate, remeasure the affected evidence before using it. Measurements can also
reopen strategy or modeling questions; do not force a return to a predetermined phase.

Give every finding and every recommendation its own item with a short descriptive name
("Wait action cost", "Unused box"), never a code (F1, R1, H1), and let each recommendation
name the findings it rests on; separate items can be approved, deferred or argued one at a
time, where a finding mentioned in passing is easily lost.  In discussion with the user, name
topics in words ("symmetric objects", "dead-state pruning"), not by the question and strategy
ids of sections 2 and 3, which are this advisor's own index.  Keep each item to its result
and recommendation; the reasoning behind it is given when the user asks.

### Phase 0: profile and questions

No build, no staging and no runs: read, reason, and ask.

1. **Read the spec** in full: types, actions, any happenings, the goal, the current `ww-set`
   values, and any search hooks already defined (`heuristic?`, `prune-state?`,
   `min-steps-remaining?`, `bounding-function?`, `encode-state`, enumerator declarations).
   Note any construction that is unusual or more complex than it needs to be (for example,
   read-time `#.` evaluation), and also the minor clutter that makes the spec harder to read:
   commented-out debug prints, a query that only calls another, intermediate variables or
   lists a simpler test makes unnecessary.  Check comments against the code (donald's header
   claimed forward checking that the actions did not do).  Check data that records a relation
   from both sides for entries with no mirror: crossword15-18 listed one crossing from one
   slot only, and crossword13 two, so placing a word wrote a letter its crossing never saw.
   A few lines that look up each entry's mirror find them.  A type that no parameter names may
   still be in use: `$row` in a relation's signature means a value of type `row`
   (tiles7a's `(loc tile $row $col)`), and staging fails if the type is removed.
2. **Select the relevant questions in section 2**, reusing established answers. Mark their
   source *spec*, *user* or *probe*, separately from whether the claim is confirmed,
   provisional, or unknown. A user hypothesis is not a requirement. Questions needing
   experiments become proposed topics rather than an automatic probe schedule.
3. **Summarize the problem** in its own terms, with no setting names: what it asks, and the
   size readable from the spec (objects, actions, moves available at the start, solution
   length if fixed).
4. **Ask the user** only for missing information that affects the selected decision:
   objective, available time, known depth or length, or a relevant scaling target.
   Reuse the shared objective and local execution preferences; do not ask them again.
   Group related questions when helpful. This is the phase-0 report (section 8).

### Phase 1: first probes and main findings

5. **Propose and run authorized probes** (section 6.1) to answer specific uncertainties.
   State the model revision, start, settings, limits, stop condition, and interpretation
   of possible results. Use short bounded searches, not an unapproved full solve
   (section 1.1). A fast `first` result may justify a solution candidate; it does not
   establish typical performance. Propose a randomized comparison only if sensitivity
   to ordering matters to the current decision (answer-ordered data, section 6.1).
6. **Report the main findings** as named items, each with the number behind it, and the
   recommendation in a sentence.  This is the phase-1 report (section 8).

### Phase 2: spec upgrades for simplicity and efficiency

7. **Propose the upgrades** as one numbered list of specific changes, each of which the user
   can accept or reject on its own.  Each item has a short descriptive name, then a few plain
   sentences in the puzzle's terms: what changes, why (the finding and the number behind it),
   and a recommendation (accept, or accept only if ...).  Do not group items under category
   headings or mix in settings that stay unchanged (crossword13: a report grouped as upgrades,
   settings, hooks and cleanup left the user unsure which lines were proposals).  Where a
   recommendation touches the problem type or a setting the user may ask about (csp, tree
   search), say in a sentence before the list why it is or is not changed.  The items cover:
   - choices that live in the spec: the strategy where it is a spec setting (`*problem-type*`
     csp, `*tree-or-graph*`, `*solution-type*`, symmetry), and action order (section 3);
   - representation changes (Q18) and refactorings of unusual constructions (section 1.1);
   - search hooks (section 4.1), each with its soundness argument;
   - with happenings, how time passes: the spec's `wait` action or `*auto-wait*`, never both
     (section 5).  Time the same probe with each and keep the cheaper (sentry: the `wait`
     action, about 6 times cheaper);
   - minor clutter, one item per clean-up.
   End with the changes considered and not recommended, each with its reason.  Check the
   conflicts in section 5 first. Draft-copy experiments are edits and tests too: perform
   them only within explicit approval, and distinguish predicted from measured gains.
8. **Apply what the user agrees to** in a copy of the spec with its design notes (section
   1.1), one change at a time, and check each: the copy stages, and a probe gives the same
   answers appropriate to the change (goal states, best value), and report observed counts.
   Equal counts alone do not establish equivalent behavior or fidelity. An independent
   counting script (section 6.3) can provide additional scoped evidence when justified;
   reuse it if later measurements need it. For a clean-load check, use a fresh SBCL
   process following AGENTS.md, load Wouldwork, and stage the tested copy.
   Functions left by the previously staged problem can hide a missing definition
   (crossword5-11-1 lost `full-word`, which only its post-processing uses, and passed every
   run until loaded fresh).  Then deliver exactly the file tested, and compare checksums: a
   delivery made just after an edit can carry the previous version (seen twice with
   crossword5-11-1); deliver again until they match.
9. **Report** the phase-2 changes (section 8): what changed, the before and after counts,
   and the REPL forms to stage and test the copy.

### Phase 3: results and estimates

10. **Measure only what the decision needs**, on the current approved spec revision,
   using bounded authorized runs (section 1.1). Possible measurements include relevant
   regime costs (section 3.2), density (section 6.1), target-size projections (section 6.3),
   and family growth (section 6.4). Omit those unrelated to the objective. Give estimates
   as ranges with assumptions; do not promise a precision the evidence cannot support.
11. **Report the results and estimates** using relevant parts of section 8. If evidence
    changes the strategy's justification, return the affected topic to the consultant.
    List larger interventions the measurements point to (a
    re-encoding, a generator for the family, a pruning invariant, an engine change) under
    FURTHER, as options for the user to take up later, not as steps of this run.  Deep runs,
    and any run that would only sharpen an estimate, happen when the user asks for a
    follow-up.

### 1.1 Who does what

**Editing the spec.**  The advisor never changes the original spec.  The first change the user
agrees to creates a copy beside it, named by extending the original's name
(`problem-knap19.lisp` to `problem-knap19-1.lisp`), with `*problem-name*` changed to match
(`knap19-1`) and the first line set to `;;; Filename: problem-knap19-1.lisp`, with exactly
three semicolons.  Staging reloads the file that line names, so a copy that keeps the
original's line silently stages the original (seen with a donald variant).  Later agreed
changes in the same run go into that copy.  A change that needs a separate variant of its own
(relaxed, backward, re-encoded) takes the next number.  Never edit the generated
`src/problem.lisp`.

- State the change and its reason first; change only what was agreed.
- Open the copy with a short *Design notes* comment block below its header, written for the
  user: the high-level design concepts and trade-offs the spec relies on (one decision per
  step in a fixed order, most constrained first, an optimistic bound, a lean state), each in
  a line or two of general terms rather than problem-specific details or counts
  (`problem-crossword5-11-1.lisp`).
- A `ww-set` goes in the spec's `ww-set` block, replacing any existing value for that setting.
- REPL-only settings (`*algorithm*`, `*debug*`, `*probe*`) are never written
  to the spec; give the REPL form instead. `*threads*` may be declared in the
  spec when the user wants a machine-specific default; otherwise set it at the REPL.
  Never declare it in a spec meant for chained `(solve-subgoal <goal>)`, which errors
  unless `*threads*` is 0.
- A hook query or macro action is shown in full and written once agreed.
- **Representation changes** (Q18, or a probe showing wasted states). Propose an edit when
  the new spec is a re-encoding of the same rules and goal: the same moves, only stored
  differently.  Check it by running the same probe on both specs: the reachable boards and goal
  states must correspond within the tested scope. Explain the correspondence beyond
  sampled cases before making a general equivalence claim. Implement only after approval.
  When meaning changes (new rules, a changed goal, a different level of detail), return a
  specification topic to the consultant with the finding, proposed encoding, and affected
  assumptions. Resume dependent analysis when the relevant fidelity questions are resolved;
  successful staging alone is insufficient.
- **Refactorings** (phase 0, step 1: an unusual or needlessly complex construction).  When a plainer
  form gives the same rules and goal, write it into the copy once the user agrees, after
  checking that it stages and that a probe gives the same counts and best result as before.
  Example: knap19 read its data with `#.` forms so that the values existed when the file was
  read, but `define-types` evaluates `(compute ...)` and `define-init` evaluates backquoted
  literals (`` `(capacity ,(first *knapsack*)) ``) when the file loads, so ordinary
  `defun` and `defparameter` forms do the same job.  Minor clean-ups follow the same rule:
  knap30's `compute-bounds?` built a list of the "missing" items to skip, and its caller
  `bounding-function?` only passed it the contents; one query testing each item directly
  (packed, or numbered above the largest packed item) gave identical counts on knap19 and
  knap30.
- Write files containing `$` whole, or with literal quoting, never through a
  regex-replacement edit.
- After each edit give the REPL forms to apply it, e.g. `(stage <problem>)`.  Staging applies
  the spec's own settings.  `(refresh)` does not, and neither does a plain
  `(ql:quickload :wouldwork)`: it re-copies the edited spec into `src/problem.lisp`, but the
  `vals.lisp` saved at the last staging then overrides every setting in it (measured with
  `*depth-cutoff*` and `*threads*`).  Restage after every spec edit.

**Running searches.**

- **REPL examples.** Assume the user has already loaded Wouldwork and entered the
  `ww` package. Omit `(asdf:load-system :wouldwork)` and `(in-package :ww)` from
  the supplied forms. Use `(solve)`, not `(time (solve))`: `solve` already times
  the search. If an engine change requires a reload, mention that in prose.
- **Short runs** (bounded probes or validation): follow AGENTS.md and the consultant's
  approved test scope. The assistant may run authorized small checks locally; identify
  who runs each test and preserve the user's existing REPL. Report measured evidence,
  not expectations. Review and planning phases do not implicitly authorize runs.
- **Local defaults.** Use `*threads*` = 16 for compatible local searches, including probes.
  Thread initialization and initial root-task generation are serial; include startup
  cost in timing interpretations. Use 0 where required (backtracking, auto-wait, declared
  dynamic-object registration, serial goal chaining) or for explicitly justified serial
  measurements. Record the exception. This preference does not change engine defaults.
- **Deep runs** (substantial full solves, backward searches to the memory limit,
  enumerator layers, `every` searches feeding `freq`): only when the user asks for one (step
  11) or an explicitly approved run plan, normally at the user's REPL. The assistant supplies the exact
  forms and what to paste back, and never raises the depth or thread count silently.  A spec
  copy written for a deep run leaves `*progress-reporting-interval*` unset (section 4).
- **Multi-step strategies** (S7 macros, S8 subgoaling, S9 relaxation, S10 bidirectional, S11
  enumerator): guide one step at a time.  Give the forms for the step, wait for its result,
  check it, then give the next.  Keep the forms of each step so the run can be repeated.

---

## 2. Questions that decide something

Only questions whose answers change a strategy, a setting, a spec addition, or the meaning
of a result belong here.  The notes below the table give the details and measured examples.

| Id | Question | How settled | What it decides |
|---|---|---|---|
| Q1 | Is the answer a sequence of moves, or an assignment (each variable set once, order irrelevant)?  If an assignment, is there one action per variable, or one action over all of them? (note 1) | spec | `*problem-type*`; the assignment cases of section 3.1 |
| Q2 | What is wanted: any one solution, N, every, every path, or a best one (fewest steps, least time, min/max value)? | user | `*solution-type*`; optimization (S4) |
| Q3 | Are there happenings or patrollers? | spec: `define-happening`, `define-patroller` | tree search; `*auto-wait*`; no backtracking |
| Q4 | Do states repeat (moves can be undone, or different orders reach the same state)? (note 2) | spec, then probe: repeated-state percentage | `*tree-or-graph*`; whether backtracking fits |
| Q5 | Does some quantity change by a fixed amount on every move, **and** does the goal fix its final value? (note 3) | spec: action effects and goal | fixed solution length: `first` rather than `min-length`; exhaustion becomes proof (section 6.2) |
| Q6 | How large is the space, and how much work does each expansion cost? (note 4) | spec, probe | whether brute force can finish (roughly under a billion program cycles); finer actions, or `bind` for parameters the state fixes |
| Q7 | Are there interchangeable objects: one type, identical static facts, not named by the goal, and taken by the actions as typed parameters (`?peg peg`)? (note 5) | spec; staging lists the families | `*symmetry-pruning*`, kept only if a timed probe shows it faster (S5); Q18 instead if the names serve no purpose |
| Q8 | Is there a cheap measure of how close a state is to the goal, and does it ever overestimate the moves still needed? | spec, user, probe | `heuristic?` (S6), kept only if a timed probe shows it faster; if it never overestimates, also try it as `min-steps-remaining?` (Q9) |
| Q9 | Is there a cheap lower bound on the moves still needed, one that never overestimates? | spec, user | `min-steps-remaining?` (S5) |
| Q10 | Can some states be proved dead (an invariant broken, a resource gone, a bound exceeded)? | spec, user | `prune-state?` (S5) |
| Q11 | For min/max-value, can an optimistic value of a partial state be computed cheaply? | spec, user | `bounding-function?` (S4) |
| Q12 | Can goal states be listed explicitly, and can every action be reversed? | spec | bidirectional search (S10) |
| Q13 | Is the goal described by base facts (positions, pairings) from which the derived facts follow? | spec: `propagate-changes!`, derived relations | enumerator meet-in-the-middle (S11) |
| Q14 | Is the work per state dominated by working out derived facts? | probe: states per second, compared with a problem without propagation | relaxation (S9) |
| Q15 | Do the same few moves recur together in solutions of small versions? | probe: `freq` on every solution of a small version | macro actions (S7) |
| Q16 | Are there natural milestones every solution must pass (a gate opened, an object placed)? | spec, user | subgoaling (S8) |
| Q17 | Does the problem use the Talos recorder or connectors? | spec: `include-tech` | recorder and connector limits (section 4) |
| Q18 | Do objects carry names the puzzle never uses, is the same fact stored more than once, or are some objects ruled out of every action by static facts? (note 6) | spec | re-encoding (section 1.1): record only what matters |

1. **Assignments.**  csp fixes the order by depth: at depth n, while n is below the number of
   actions, only the nth defined action is tried (`generate-children` in `ww-planner.lisp`),
   whether or not the spec intends an order.  So csp orders a spec with one action per
   variable (or group of variables), but gives no order to one action over all the variables
   (knap19's `put`, crossword13's `fill`).
2. **Undoable moves.**  Routes of different lengths reach the same state, and graph search
   expands a state again each time a shorter route reaches it (section 6.1).
3. **Fixed length.**  Examples: one peg left, all N items placed.  Both conditions are needed:
   a knapsack places one item per move, but its goal does not fix how many.  Set an exact
   `*depth-cutoff*` only if moves can continue past that length, or for
   `min-steps-remaining?`.
4. **Size and work per expansion.**  b^d counts paths; when moves commute (any order of the
   same choices reaches the same state) graph search visits only the distinct states, which
   can be far fewer (knap19: 17^11 paths, 36,326 states).  An action's `product` enumerates
   every combination of its parameters before the precondition rejects any, so k free
   parameters over a domain of size m cost up to m^k tests per state (a 41-addend cryptarithm
   whose units column holds 9 letters: 31 s for 197,005 program cycles, against 7 ms for
   DONALD's 424).  The remedy is finer actions, each fixing one or two variables.  A parameter
   that the state already fixes (the support a disk sits on, an agent's location) should be
   read with `bind`, not enumerated and tested: hanoi's `move` took both supports as
   parameters, 880 combinations per state at 8 disks; binding the origin made it 5.4 times
   faster, and a peg-per-disk encoding 13 times.
5. **Interchangeable objects.**  Objects supplied by a query (`?peg (get-remaining-pegs?)`)
   are not recognized, so no family is found.  Identical means the same rules (the same static
   facts: shape, moves, capacities), not the same appearance: tiles7a's 16 blue tiles share a
   colour, but only its five single squares share a shape.
6. **Re-encoding.**  Examples: identical pegs or tokens; `loc>` beside `contents>`, or a list
   beside a count; an item heavier than the capacity, tried and rejected at every state.
   Re-encoding removes duplicate states at the source, which is cheaper and more complete than
   `*symmetry-pruning*`.  Merge only objects with the same rules (Q7): storing only colours is a
   different puzzle when same-coloured pieces differ in shape.  tiles7a-heuristic-1 stores its
   five identical squares as one sorted list of cells, moved by their own action whose
   parameter comes from a query: shortest solution 3,440 program cycles in 0.1 s, against 3,649
   in 0.4 s with symmetry pruning (both with a lower bound).

---

## 3. Strategies

Listed roughly from cheapest to most effort.  Most combine: parallel with pruning, macros with
heuristics, subgoals with any of them.  The notes below the table give the details.

| Id | Strategy | Use when | How | Cost and cautions |
|---|---|---|---|---|
| S1 | **Brute force, iterative deepening** | A useful baseline when its cost fits the objective and budget | `first` at approved cutoffs; propose further depths only when needed; test shortest length only if requested | Exponential in depth |
| S2 | **Parallel search** | The space is large (depth-first or backtracking search) | `(ww-set *threads* N)` at the REPL | Best with tree search; graph search shares a locked closed table.  Not with PATH-mode backtracking (`*bt-cycle-check*`), auto-wait, or objects created during search (section 5) |
| S3 | **CSP (fixed-order assignment)** | Q1: one action per variable | `*problem-type*` csp; `*depth-cutoff*` 0; actions defined in the order they should run (note 1) | Backtracking is serial only and supports `prune-state?` and move lower bounds (see section 5 for unsupported hooks); the order matters; with forward checking the goal must test the constraints (note 1) |
| S4 | **Optimization** | Q2 asks for a best solution | `min-length`, `min-time` (action durations), `min-value`/`max-value` (assign `$objective-value` in each assert); `bounding-function?` for value problems | Must search until the bound is proved, so far more work than `first`; pointless at fixed length (Q5); set a cutoff when moves can be undone (note 2) |
| S5 | **Pruning hooks** | Q7, Q9 or Q10 answered yes | `*symmetry-pruning*` t; `min-steps-remaining?` or `prune-state?` as queries | Must be **sound**; time symmetry as well as counting what it saves (note 3) |
| S6 | **Heuristic ordering** | Q8 yes and the first solution is wanted fast | define `heuristic?`; lower values are explored first | Orders successors only; time it against no ordering (note 4) |
| S7 | **Macro actions** | Q15 shows recurring multi-move patterns | combined actions before the base actions; candidates from `(freq 2 3)` after an `every` search of a small version | Each added action costs work at every state; keep the base actions; a saving can reverse with size (note 5) |
| S8 | **Subgoaling (goal chaining)** | One search cannot reach the goal, and Q16 gives milestones | `(solve-subgoal <goal>)` (chained, serial only: it errors unless `*threads*` is 0), or the two-argument checkpoint form `(solve-subgoal <start> <goal>)` (serial or parallel), with `ww-undo`, checkpoint export and import; `solve-via-strategy` for a registered multi-phase strategy | A milestone reached the wrong way can block the rest.  The solving-advisor is the worked-out interactive form (note 7) |
| S9 | **Relaxation** | Q14: propagation dominates and base facts approximate the derived ones | a separate spec whose preconditions ask a weaker, cheaper question; the goal calls `propagate-changes!` and tests the true conditions last | The cheap test must hold wherever the true one does, never the reverse.  No help when the difficulty is the number of choices |
| S10 | **Bidirectional search** | Q12 yes, and depth is the obstacle | a backward spec searched with `every`, then a forward search whose goal calls `(backward-path-exists state)` (note 6) | A second spec to write and keep consistent; memory for the backward layer (note 6) |
| S11 | **Enumerator meet-in-the-middle** | Q13 yes: goal states can be generated from base facts | `define-base-relation` (plus optional `define-goal-filter`, `state-feasible?`); `(find-goal-states)`, `(find-predecessors)`, `(solve-meeting-point :depth-cutoff N :solution-type first)` (see the end of `problem-corner.lisp`) | Backward layers can explode; constrain base relations early |
| S12 | **Randomized and branch-restricted runs** | Exploring a huge space for any solution, or splitting work by hand | `*randomize-search*` t (repeat runs); `*branch*` n explores only the nth first move | A failed run proves nothing.  Ignored when `heuristic?` is defined |

1. **CSP.**  Define the actions so that each one's inputs are fixed by earlier actions (a
   carry chain runs from the units column), with the most constraining variables first.
   `*algorithm*` backtracking (REPL) is optional; depth-first also respects the order.  Narrow
   the remaining domains as values are assigned (forward checking; `define-update` as in
   `problem-captjohn.lisp`).  Order matters: donald-1 right to left took 424 program cycles to
   prove, left to right 1,440.  With forward checking a variable can be fixed by elimination,
   so the goal must test the constraints themselves, not only that every variable has one
   value: donald-1's first draft, whose goal tested only that, reported 30 false solutions one
   column early.
2. **Optimization.**  When moves can be undone, `min-length` without a `*depth-cutoff*` first
   dives along a very long path and then shortens it a move or two at a time (hanoi, 7 disks: a
   first solution of 732 moves against an optimum of 127, then 239 improvements, 31.9 s; with
   the cutoff at 127, 4.5 s).  Set the cutoff to the known optimum, or to the length of a
   solution already found.
3. **Pruning.**  A bound that overestimates, or a dead test that rejects a live state,
   silently discards solutions.  Symmetry pruning removes variants under `every` and has
   overhead: in hanoi's peg encoding it cut program cycles by 23% and made the run 4 times
   slower (8 disks, 3.2 s to 12.7 s).
4. **Heuristic.**  Still complete depth-first search, not beam or A*; the first solution need
   not be shortest.  Serial depth-first, parallel depth-first, and backtracking use it; it overrides
   `*randomize-search*`.  Time it for `first` and the optimizing search alike: tiles7a's
   distance of the yellow tile to its goal made `first` slower (40,412 program cycles against
   31,842) and `min-length` no faster, because the distance says little about clearing the
   way; the same distance, which never overestimates, cut `min-length` from 49,767 to 12,869
   as a lower bound (Q9).  The same ordering can help on one board and hurt on another:
   `problem-triangle-xyz-heuristic.lisp` at N = 7 took 2,766 program cycles against 4,107
   without it on one starting board, and 3,024 against 1,117 on another.
5. **Macros.**  In `problem-triangle-xyz-macros.lisp` double jumps took 11 program cycles
   against 108 at N = 5, but 1,138 against 350 at N = 6, although the plan was shorter both
   times.
6. **Bidirectional.**  Search the backward spec to depth d2 with `every`, with
   `encode-state` defined; collect its boards with `(get-state-codes)`; then search forward to
   d1 = d - d2.  `problem-triangle-xyz-backward.lisp` gives the procedure in its header, with
   its partner `problem-triangle-xyz-forward.lisp`.  Both specs need an identical
   `encode-state`.  Staging the forward spec keeps the backward boards; staging the backward
   spec clears them.  The reported plan is the forward path followed by the reversed backward
   path, renumbered to follow on; replay it with `validate-action-sequence`.  Triangle at
   N = 6, split 12 + 7: the backward search collected 16,253 boards in 8,865 program cycles,
   and the forward search met one in about 20.
7. **Subgoaling.**  Choose milestones as resting states the next step needs, and measure each
   with `solve-subgoal`.  crelay-topo (2026-10-06, two recorder cycles in 28 moves against a
   hand plan's 31): (a) Name the precondition the next step needs, not its end product:
   "agent1 at location6" (inside the alcove) took 10 moves and the search found the means
   itself, while "box1 at location7" (the box out of the alcove) straight after leg 1 found
   nothing in 10 minutes.  (b) Size each leg to about 10 moves for `first`, 6 to 9 for
   `min-length`; cost grew about 2.6 times per move.  (c) Use the final resting state, not a
   waypoint: a leg that set the box down was followed by one that picked it up again, 2
   wasted moves.  (d) Add `(recorder-cycle-ended)` only where a cycle should close; legs may
   run inside one cycle.  (e) When a leg stalls, split it at its hard precondition rather
   than raising the cutoff.

### 3.1 Choosing

Start at the first row that applies; the fallback column is the escalation order.

| Situation | Primary | Then |
|---|---|---|
| Assignment, one action per variable (Q1) | S3, with symmetry if Q7 | S4 for value optimization (depth-first provides automatic objective bounds; backtracking supports user bounds) |
| Assignment, one action, every variable set (a crossword's slots) | the action takes its variable from an ordered list of the open ones held in the state, most constrained first; tree search (note 1) | S5 dead-state pruning (an open variable with no value left); a completion check when some variables are left for a later fill (note 3) |
| Assignment, one action choosing a subset (knap19's `put`) | S1, or S4 if a best one is wanted, in graph search: other orders of the same choices close as repeated states (note 2) | S5 bound; S2 |
| Happenings (Q3) | S1 in tree mode, `*auto-wait*` if waiting matters | S8 with time-tagged milestones |
| b^d modest (Q6) | S1, then S4 if a best solution is wanted | S2 |
| Fixed length (Q5) | S1 at the exact length with S5 dead-state pruning and symmetry | S10 (exhausting the remaining length proves a position dead); S7 |
| Large, reversible, with a distance measure (Q8) | S5 lower bound if the distance never overestimates and the shortest path is wanted; S6 only if a timed probe shows the ordering faster; S2 | S8 at bottlenecks |
| Large, reversible, no distance | S2 | S10 if Q12; S7 |
| Expensive derived state (Q14) | S9 | S11; S8 |
| Too deep for one search, with milestones (Q16) | S8 | S10 or S11 for the last stretch |

1. **Ordered open list.**  Each fill removes its variable from the list, and the goal is the
   empty list.  Each state is then reached by one path, so graph search's table would only
   cost memory.  crossword13: 2.8 million paths by depth 3 when any open slot could be filled,
   22 to 25 program cycles with the slots taken along the crossings; filling all the acrosses
   first (which never cross each other) checks nothing for 11 steps, and did not finish in 5
   minutes.
2. **Not S3.**  csp fixes the action only while the depth is below the number of actions, so a
   single `put` is fixed at depth 0 alone and every later depth tries all orders, in tree
   search.  If the spec has one action per variable group but no order (the original donald),
   use S3 rather than graph search: graph search closes the reordered states, but csp never
   generates them (donald, every solution: tree 165,978 program cycles, graph 17,239, csp
   1,441).
3. **Variables left for a later fill.**  When the search sets only some variables and leaves
   the rest to be filled afterwards (crossword15-18: listed words placed, other slots left for
   the dictionary), checking each crossing on its own does not ensure the open variables can
   be filled together.  crossword15-18's best grids had no completion; five early placements
   had already ruled one out, and a check that also tested each open slot against its
   crossings' remaining letters still found none (and cost 15 times the program cycles).
   Whether the rest can be filled is a search of its own: a completion solver (bit-vector
   domains, propagation to a fixpoint, fewest values first, regions sharing no open variable
   solved apart) decides it, here in about 0.1 s.  Run it on every placement and every
   recorded state is completable, at far fewer states (5 minutes at 16 threads: 48,000 to
   54,000 program cycles against 22 million, and 18, 19, 19 listed words); or search as
   before and afterwards drop words from the best state until it completes (24 placed, 17
   kept).  Checking every placement did better and leaves no repair step.

### 3.2 Search regimes and their trade-offs

What the user wants decides what the search must do, and the costs can differ by orders of
magnitude.  Report the regimes the user might want with their costs, measured where cheap
(RESULTS in section 8), rather than choosing one silently.

| Regime | Setting | Must cover | Cost and what decides it | The result means |
|---|---|---|---|---|
| Find one | `first` | until the first goal | depends on density and on ordering (S6, action order); falls steeply with many solutions | a valid answer; nothing about others |
| Prove (no solution, or uniqueness) | `first` exhausted without a goal for absence; exhaustive enumeration for uniqueness | the relevant model space, less what justified pruning removes | the size of the space; ordering does not reduce exhaustive coverage | a scoped model claim, subject to coverage and the chosen equivalence of solutions (section 6.2) |
| Every solution | `every`, `all-paths` | the whole space | as prove, plus memory for the solutions recorded | every goal state (every path with `all-paths`) |
| Best | `min-length`, `min-value`, ... | until the bound is proved | as prove, less what the bound prunes (S4) | the optimum, if run to completion |

The ratio of prove to find is itself a measurement: near 1 means solutions are scarce or
found late, so a heuristic will not help; a large ratio means a `first` search is cheap but a
proof is not.  Cryptarithms, measured with the donald-1 scheme: DONALD 225 to find and 424 to
prove (1.9 times); a 41-addend puzzle 5,161 and 111,116 (21 times); random base-16 puzzles of
two 8-letter addends 6,253 and 1,302,741 (about 200 times). When the gap is small and the
objective benefits from exhaustive evidence, propose that additional work within its budget.
Exhaustion does not confirm that the model has no unintended solutions: that requires
comparison with intended rules, independently of search coverage.

---

## 4. Settings

Set in the spec with `ww-set`, except where marked REPL.  `(stage <problem>)` applies the
spec's own values; a saved `vals.lisp` otherwise overrides them on an ordinary load.
The table lists engine defaults. The consultation's local search preference is 16 threads
where compatible, applied after staging; see section 1.1 for exceptions and serial startup.

| Setting | Values (default) | Choose |
|---|---|---|
| `*problem-type*` | planning, csp (planning) | csp only for assignments (Q1).  csp forces tree search |
| `*algorithm*` **REPL** | depth-first, backtracking (depth-first) | backtracking for CSP, or a tree with no repeats where memory is tight; otherwise depth-first.  An error if set in the spec |
| `*solution-type*` | first, N, every, all-paths, min-length, min-time, min-value, max-value (first) | from Q2.  `first` at fixed length.  `every` gives one path per goal state; `all-paths` every distinct path to every goal, but only serial depth-first graph search with a depth cutoff (otherwise it falls back to `every`) |
| `*tree-or-graph*` | tree, graph (graph) | graph when states repeat (Q4: repeated-state percentage high); tree when they rarely do, with happenings, or for better parallel speedup |
| `*depth-cutoff*` | integer; 0 = none (0) | known or fixed length (Q5); otherwise iterative deepening.  0 for CSP.  Needed for `min-steps-remaining?` to prune before a first solution.  Set it for `min-length` whenever moves can be undone (S4) |
| `*symmetry-pruning*` | t, nil (nil) | t when Q7; staging reports the groups found, and suggests turning it off if none |
| `*threads*` | 0 = serial, N (0) | May be declared in the spec or set at the REPL. Crossing between serial and parallel requires a rebuild; changing a positive count to another positive count does not. 0 for PATH-mode backtracking (`*bt-cycle-check*`), auto-wait, and problems that create objects during search |
| `*randomize-search*` | t, nil (nil) | S12 only |
| `*branch*` | n (0 = all) | S12 only |
| `*auto-wait*` | t, nil (nil) | happenings where waiting may be needed and the spec has no `wait` action (section 5); try without first, since it enlarges the search.  Tree, serial, depth-first only |
| `*auto-wait-max-time*` | integer (100) | with `*auto-wait*` |
| `*progress-reporting-interval*` | nil or integer (nil) | nil reports on a time schedule whose gaps double, starting at `*progress-first-report*` seconds (10): about 12 reports in 12 hours, serial or parallel; an integer N reports every N states |
| `*max-recorder-cycles*` | integer, nil (1) | Talos recorder: recordings allowed in one path |
| `*recorder-prefix-pruning*` | t, nil (nil) | Talos recorder: also reject open recordings that can no longer replay |
| `*max-connector-pairings*` | integer, nil (nil: beam-relay's default) | Talos connectors |

Leave the parallel tuning settings (`*split-depth-max*`, `*tasks-per-thread*`, `*min-tasks*`,
`*num-closed-shards*`, work donation) at their defaults unless a measurement says otherwise.
One that does: when each expansion is slow, the serial split into at least `*min-tasks*`
(256) tasks can use the whole run (crossword15-18-2 with a 0.1 s completion check per
placement: the split took all of a 40 s run at 2 threads, and 3.4 s with `*min-tasks*` 8).
Set the two after `(ww-set *threads* N)`, e.g. `(setf *min-tasks* 32 *tasks-per-thread* 2)`.
`*debug*` and `*probe*` are diagnostic, REPL-only, and not search choices.

### 4.1 Search hooks

Defined in the spec as `define-query` with these reserved names; the state is supplied.

| Hook | Returns | Used by | Notes |
|---|---|---|---|
| `heuristic?` | a number, lower = more promising | depth-first, serial and parallel; backtracking | ordering only (S6). Backtracking scores all sibling choices on a temporary state copy, then explores lowest first; ties retain generation order. Without a heuristic it retains streaming generation |
| `prune-state?` | true to stop expanding the state | depth-first, serial and parallel; backtracking | must be sound (S5); stops descendants, not acceptance of an already reached goal |
| `min-steps-remaining?` | a lower bound on moves to the goal | depth-first, serial and parallel; backtracking | consulted only with a depth cutoff, or after a solution under `min-length` or `first`; must never overestimate. Backtracking uses the same registered contributors, aggregate fallback, serial adaptive sampling, and pruning counters. In parallel it runs at task splitting and in every worker, without the serial adaptive sampling |
| `bounding-function?` | `(values cost upper)` for value optimization, both in minimizing terms (a max-value problem returns both negated) | depth-first, serial and parallel; backtracking | `cost` is an optimistic bound: never worse than the best value any completion of the state can reach.  `upper` is the value of one completion that can actually be reached; the smallest `upper` seen becomes the incumbent, and a node is pruned when its `cost` is worse than it.  An `upper` that cannot actually be reached prunes the true optimum.  States the search records as best never tighten the incumbent; only `upper` does.  See `problem-knap19.lisp` (S4).  In parallel it runs at task splitting and in every worker; the shared bound is updated without a lock, so a race can only loosen it (sound, less pruning).  A hook that keeps its own state in globals (for example, a bound memoized across calls) is shared by all threads and is not thread-safe |

Global invariants (`define-invariant`) are also checked during parallel root-task
generation, before a successor can become a task or an accepted goal, as well as
in workers. A failed invariant signals the existing diagnostic; continuing from
that diagnostic discards the successor in both paths.

**Ordered completions.**  A bound may ignore completions that the state can reach but that
another order of the same moves also reaches, provided it depends only on the state and
every solution can be built in some order in which no state's bound excludes it.  knap19's
bound counts only items numbered above the largest item already packed: any packing built in
ascending item order never has one of its own items excluded, so the optimum survives.  Test
such a bound against an unpruned run (section 6.1).

**Exact bounds.**  For a family with a known closed-form solution, `min-steps-remaining?` can
return the exact distance to the goal (hanoi with 3 pegs, largest disk first: a disk off its
target adds 2^k, k the number of smaller disks, and the third peg becomes their target).  It is
sound, and it reduces the search to walking the solution (8 disks: 289,145 program cycles to 382;
16 disks in 4.8 s).  Report such a bound as a benchmark beside the search's own reach, not as
its result: it measures how fast Wouldwork replays a known answer, not how well it searches,
and it rarely survives a change to the family (there is no such formula for 4 pegs).

**Distance plus obligations.**  With a depth cutoff at or near the optimum, a lower bound cuts
detours: a move that does not reduce the bound uses up slack and is soon pruned.  It applies to
tree and graph search alike.  When tree search is forced (happenings, csp), repeated states
cannot be closed, and the bound is the main remedy; under graph search it also prunes the
detours by which a closed state is reached again and reopened.  tiles7a (graph search, cutoff
22 = optimum): the yellow tile's distance to its goal alone cut `min-length` from 49,767
program cycles in 2.1 s to 12,869 in 0.3 s, and the search cut off at 21 proved 22 optimal in
9,480.  Add the
agent's distance to the goal along the static adjacency to one for each action every remaining
solution must still take, counting an action only when it is provably required.  sentry-1:
distance to area8, plus a jam of the sentry unless it is jammed or passed, plus a pickup of the
jammer if it is not held; every solution at cutoff 16 fell from 733k program cycles to 4,698, and
at 18 from 4.65 million to 33,954, with the same paths and goal states.  Check such a bound
with every-solution runs with and without it at two cutoffs, which must agree.  Its gain falls
as the cutoff rises above the optimum.  When the family will grow, compute the distances from
the adjacency relation rather than writing them by hand.

**Generic topology bound.**  A problem that includes `topo-lower-bound` gets an automatic
finite-resource contributor, with no `min-steps-remaining?` of its own and consulted under the
same conditions.  To run without it, stage, then `(setf *min-steps-remaining-contributors*
nil)`; `min-length` must give the same lengths with and without it.  Fixes from crelay-topo
(2026-10-06): a fixed floor blower no longer disables it (its stream is a gate-free route, and
a leg ending at a lift location costs no move); an ON goal on a support with a fixed position
routes as a location goal; a plate press costs 1 when a second agent (a recording ghost counts)
exists.  Gears, wall and angled blowers still disable it.  Its route relaxation ignores height,
so it read 1 for a leg needing a raised launch point that took 10 moves.

Under `*threads*` > 0 the hooks must be pure functions of the state: any global a hook reads and writes is shared by every worker.

---

## 5. Conflicts and automatic adjustments (from `ww-initialize.lisp`)

- **Backtracking** forces tree search; is an error with `*threads*` > 0 or with happenings; and
  supports `prune-state?`, move lower bounds, and `bounding-function?` before generating
  choices, including at the initial state. `heuristic?` orders choices across actions and parameter combinations.
  Move lower bounds can prune against a depth cutoff or an existing `min-length`
  solution. User bounds share the depth-first incumbent and counters; automatic objective-bound pruning
  remains unsupported. With planning and no depth cutoff it
  may dive without limit.
  Reached goals must pass solution validators and the active goal-chain candidate
  rejector; a rejected goal remains searchable for an acceptable descendant.
- **csp** forces tree search, whatever `*tree-or-graph*` says.
- **Happenings with graph search** are reported as an error: states cannot be closed when time
  matters.
- **`heuristic?` with `*randomize-search*`**: randomization is ignored.
- **`*auto-wait*` or the spec's `wait` action: use one, not both.**  Both let time pass.  A
  `wait` action is an ordinary successor at every state, lasting until the next event, and is
  checked like any action.  `*auto-wait*` inserts a wait only when a state has no applicable
  action, or after all its successors are exhausted, simulating the happenings until an
  action applies; it checks kill conditions and `define-constraint` (the constraint check was
  added 2026-10-04: before that, sentry's auto-wait returned a 12-move "plan" that waited into
  the sentry's area, against a true optimum of 16).  With both, each wait is found twice
  (sentry-1, every solution: 66 paths for 33, and 22 times the program cycles).  Which is cheaper
  depends on the problem; in sentry the spec's `wait` was about 6 times cheaper (every
  solution at cutoff 16: 733k program cycles against 4.59 million).  `*auto-wait*` needs tree,
  serial, depth-first.
- **Symmetry pruning with `every`** drops solutions that differ only by symmetric objects.
- **Fixed length with `min-length`**: every solution has the same length, so the optimizing
  search only does extra work.
- **Value objectives with a goal**: once a solution exists, `min-value` prunes any node whose
  own value is already no better than it.  That is sound only if value never decreases along a
  path (costs accumulate, as in `problem-tsp.lisp`); a min-value problem whose value can fall
  needs a different solution type or a `bounding-function?`.  `max-value` prunes only goals
  that fail to beat the best solution; its other bounds come from `bounding-function?`, since
  rewards normally grow along a path (`test/search/problem-max-value-goal.lisp`). During parallel
  task generation, all four optimization modes register only goals that improve the
  incumbent; enumeration modes still retain their requested goals
  (`test/search/problem-task-goal-incumbent.lisp`).
- **A value search with no goal must not define one.**  Best states are recorded only when no
  goal is defined (`process-min-max-value` in `ww-searcher.lisp`), so `(define-goal nil)`
  records nothing: crossword15-18 reported "No solutions found" after 2.5 million states in
  60 s, and 35 words placed with the line removed.  Leave `define-goal` out.
- **The `bounding-function?` incumbent starts at 1,000,000.**  A min-value problem whose
  `cost` at the start state exceeds that has the start state pruned and searches nothing.
- **Parallel (`*threads*` > 0)**: `*auto-wait*` is an error; `all-paths` falls back to
  `every`; a problem that creates objects during search (the corner family's beams) declares
  `(ww-set *search-registers-dynamic-objects* (beam))` and runs serially (see
  `problem-corner.lisp`); `*debug*` above 1 is reset to 1; symmetry statistics are
  approximate.  All search hooks, `*randomize-search*` and `*branch*` apply.

---

## 6. Probes and reading results

### 6.1 Probes

A probe is a bounded experiment addressing a named uncertainty, such as Q4, Q6, or Q14.
Use shallow exhaustive searches when coverage statistics are needed. All examples below
are candidate procedures, not a mandatory battery or authorization to run. Select cutoffs
and other limits beforehand; record the actual termination and restore temporary settings
or hooks afterward. For an approved coverage probe, use a goal not reached at that depth
or temporarily select `every`. For a compatible local search, the basic form is:

```lisp
(stage <problem>)
(ww-set *threads* 16)
(ww-set *depth-cutoff* 6)
(solve)
```

**Program cycles** counts the states expanded, and is the work count this advisor uses
throughout (older notes say "expansions").  **Total states processed** counts the states
generated, so it grows with the branching factor.

Read from the summary: **Total states processed** and how it grows between depths (effective
branching, so b^d for the needed depth); under graph search, **Program cycles** equals the
distinct states only when every route to a state has the same length (moves that commute, as in knap19).
When moves can be undone, a closed state reached again by a shorter route is reopened and
expanded again (`better-than-closed` in `ww-searcher.lisp`, under `first`, `every` and
`min-length` alike), so Program cycles can far exceed the distinct states (hanoi, `every` to
exhaustion: 68 for 27 boards at 3 disks, 358,827 for 2,187 at 7); take the distinct count from
an independent script (section 6.3); **Repeated states pruned … percent** (high favours
graph search); **Average branching factor**; and elapsed time, giving states per second.  A low
rate on a problem that calls `propagate-changes!` motivates investigation of propagation
cost (Q14). Propose symmetry or thread-count comparisons only when they could change
the decision. Include serial startup in elapsed-time comparisons.

*Tree search (happenings, csp).*  Program cycles counts paths, not states, and there is no
repeated-state percentage.  Probe by exhausting successive cutoffs: the ratio of Program
cycles per level is the effective branching, the first cutoff that yields a solution bounds
the optimum, and density is the goal states and paths found per cutoff.  Undone moves are
re-expanded at every level, so a lower bound (section 4.1) or a dead-state prune is the main
remedy.

**Time-limited probes.** If depth alone may be expensive, agree a time bound before running.
One possible mechanism is a temporary `prune-state?` that stops further expansion after
a deadline; the search then unwinds and reports any candidates found. This is cooperative
stopping, not a hard wall-clock limit: initialization, an in-progress expansion, and
unwinding can take additional time. A deadline-pruned run is truncated, even if its summary
looks exhausted; it establishes no exhaustive bound or optimum.

```lisp
(defvar *deadline* (+ (get-internal-real-time) (* 60 internal-time-units-per-second)))
(defun prune-state? (state) (declare (ignore state)) (> (get-internal-real-time) *deadline*))
```

Reset `*deadline*` before each run and restore the original hook afterwards (use
`fmakunbound` only if there was no original hook); the deadline test only
reads a global, so it is safe under threads.  A spec that defines its own `prune-state?`
needs the deadline test added to it instead.  Before each repeated run with
`*randomize-search*`, seed the random state, `(setf *random-state* (make-random-state t))`:
SBCL starts every session with the same one, so runs in fresh sessions would repeat each
other.

For macro candidates (Q15): solve a small version with `every`, then `(freq 2 3)`.

**Answer-ordered data.**  A `first` search that finishes almost at once may only be following
the order of the spec's data.  crossword13 listed each word in the position of its slot, so
the first word tried always fitted: 22 program cycles, while a run with `*randomize-search*` t
had passed 2.7 million states unsolved after 150 s. Report the fast find with its actual
ordering. If a claim about typical performance matters, propose a bounded randomized
or shuffled comparison. A solution-only objective need not investigate typical speed.

**Solution density.**  Run `every` to exhaustion on the full problem if it finishes in
seconds, otherwise on a small version.  Compare the number of distinct goal states with
the distinct states (**Program cycles** when moves commute, otherwise the independent count).  Under graph search the recorded path count is only a
lower bound, since paths through repeated states are cut.

| Density | Means | Favours |
|---|---|---|
| Many goal states, found by many workers | Solutions are plentiful | `first`; S12 randomized runs; no heuristic needed |
| Few goal states, deep in the space | A needle in a haystack | S5 dead-state pruning; S10 bidirectional; S6 if a distance measure exists |
| None after complete bounded coverage | No accepted solution within that model and cutoff, subject to sound pruning | Reconsider the strategy or propose a justified model check or changed bound |

A value problem with no goal (every state is a candidate) has no goal states to count.  Its
density is the number of states that reach the optimum, and what decides the cost is how
much `bounding-function?` prunes.  Measure that with the same run twice, the second with the
hook removed:

```lisp
(stage <problem>)
(ww-set *threads* 16) ; compatible local searches; approve limits for both runs
(solve)
(fmakunbound 'bounding-function?)
(solve)
```

The ratio of **Program cycles** is the bound's pruning factor (knap19: 36,326 to 432,
with the optimum reached by 1 state).  The unpruned run also tests the bound's soundness: its
best value must equal the pruned run's when both establish the optimum in the same scope.
Agreement on tested instances is scoped evidence, not a general proof of bound soundness.
Restore the hook after the comparison.

With `*threads*` > 0, **Program cycles** leaves out the states expanded while the search is
split into tasks (the shallowest levels), and is 0 when the whole search finishes during the
split.  knap19 unpruned at 16 threads: 36,172, plus the 154 states with 0 to 2 items, gives the
serial 36,326.  For exact counts, compare serial runs (`(ww-set *threads* 0)`).

### 6.2 What a result means

| Result | Means |
|---|---|
| Solution found | A candidate under the searched model; validate the full path, actual goal, and applicable validators before a final solution claim |
| Exhausted with no accepted goal, fixed remaining solution length covered | No solution from that start under that model, if all relevant choices were covered and pruning was sound |
| Exhausted with no accepted goal, depth bounded otherwise | No solution in the covered model scope; a longer one may exist |
| Exhausted entire reachable model space with no accepted goal | No model solution, provided coverage and pruning justify that conclusion |
| Exhausted with pruning, symmetry, or other restrictions | Scope the claim to the justified coverage and equivalence; unsupported pruning cannot establish absence or optimality |
| Exhausted with `*branch*` or a relaxed spec | A result about that restricted or altered search; transfer to the original problem requires a separate argument |
| Out of memory, interrupted, or deadline-pruned | No new exhaustive bound; preserve candidates already found and any independently established evidence |

Record the starting state, model revision, settings, cutoff, termination, and validation
scope with the result. Optimality requires a completed search or other valid argument
covering all better possibilities. Search exhaustion and replay do not themselves establish
fidelity to intended rules. A failed bounded search is not an omission diagnosis.

### 6.3 Projecting to a larger problem

Projections are rough estimates from short runs, given as a range; an estimate good to a
factor of 2 is enough, and a longer run to sharpen it waits for the user's request.  When the
user will scale the problem up (a larger board, more items, more steps), run the
same probe at two or three sizes a step apart and report the growth per step: the ratio of
distinct states, and of elapsed time.  Project those ratios to the target size, and compare
the projected states with memory and the projected time with what the user has.  Memory is
the closed table's entries times the bytes per state, and the bytes per state must be
**measured**, not guessed: the peak memory of a run that fits (Task Manager, or `(room)` after
`(sb-ext:gc :full t)`) divided by its **Program cycles**, compared with the heap limit
`(sb-ext:dynamic-space-size)`, not the machine's RAM.  (triangle-xyz at N = 7: 40.6 million
states overran a 25 GB heap; measured at N = 6, the closed table holds 672 bytes per state,
since each entry keeps the state's whole proposition hash table.)  To measure after a graph
search: `(sb-ext:gc :full t)`, `(sb-kernel:dynamic-usage)`, `(setf *closed-shards* nil)`
(parallel) or `(setf *closed* (make-hash-table))` (serial), then the first two again; the
difference divided by **Program cycles** is the bytes per state.  Then name the mitigation for the
regime it lands in:

| Projection | Mitigation |
|---|---|
| Within time and memory | the current settings |
| Within memory, too slow | S2 parallel; S6 if solutions are plentiful; S5 pruning |
| Exceeds memory | tree search with S5 pruning, or S10/S11 to split the depth; a leaner encoding (Q18) |
| Exceeds both by orders of magnitude | S8 milestones through the solving-advisor's dialogue, or a different formulation |

A heuristic (S6) only reorders the search.  It shortens a `first` search when solutions are
plentiful, but when they are scarce or absent the search must cover most or all of the space
anyway, so only fewer states (S5 pruning, a leaner encoding, S10) or less memory per state
helps.

Say whether the density is likely to hold at the larger size; a solution count that falls
relative to the space means `first` will slow down faster than the space grows.  A size step
can also remove every solution (triangle-xyz: 4 goal states at N = 5, 7 at N = 6, none at
N = 7); `first` then searches the whole space, and exhausting it is the answer.

For a value problem with a bound, project from instances whose data resembles the target's,
not from the item count alone: how the values relate to the weights decides how much the
bound prunes.  Instances shaped like knap19 stay in the tens of program cycles from 19 to 28
items while their distinct states grow about 1.8 times per item; `data-knap30.lisp`, whose
values nearly equal their weights, keeps the bound to a factor of 2 (13,053 program cycles
unpruned, 6,577 pruned).

**Threads.**  When the deep run will use threads, time the threaded run at two sizes as well,
and project with its growth per step, not the serial one: the speedup can shrink as the problem
grows (hanoi, 16 threads: 8.4 times at 10 disks, 6.8 at 11; threaded time grew 8.3 times per
disk against 6.7 serially).  A serial projection divided by one measured speedup is therefore
optimistic; give the projection as a range between the two growth rates.

When moves are simple enough, a short independent script that counts the reachable states
(outside Wouldwork) checks both the projection and the encoding: its counts should equal
**Program cycles** at the sizes already run when moves commute.  When moves can be undone, it
gives the distinct states and the optimum instead, and Program cycles exceeds it by the
reopenings (hanoi at 8 disks, cutoff 255: 254,368 program cycles for 6,561 boards).

### 6.4 Expanding the problem family

Analyse family growth when scaling is part of the objective or could change a consequential
decision. Otherwise skip it or record a deferred topic with a trigger. Settings that suit
one instance can fail on larger members; section 6.3 projects to a target size, while this
procedure asks which relevant growth dimension is likely to become limiting first.

1. **Name the dimensions** along which the family grows: for a cryptarithm, the base, the
   number of distinct letters, the word length and the number of addends; for a board, its
   side and the number of pieces.
2. **Probe one dimension at a time**, the others fixed, at three or more steps (short runs
   only; stop a dimension once a step takes minutes, and project the rest), reporting
   the median and the maximum over several instances where instances vary.
3. **When the spec is written by hand for one instance**, generate the larger instances
   with the independent counting script (section 6.3), after checking that it reproduces
   **Program cycles** on the instance in hand.  Generated instances should be solvable and
   resemble the target's data (random digits, then letters assigned, for cryptarithms).
4. **Report the growth per step along each dimension**, the first dimension to break, and
   its mitigation (the table in section 6.3); note any dimension along which the cost falls.

Two dimensions are easily overlooked.  **Goal distance:** when the board or layout is fixed,
moving the goal grows the solution depth without a new instance; the counting script lists the
distance to every candidate goal in one run.  tiles7a-heuristic-1 (goal cells 22, 40, 50 and 55
moves away): 0.1 s, 4.1 s, 10 s, 14 s, while the reopenings per distinct state rose from about
4 to 8.  **Mobility, not free space:** pieces that can move without ever affecting the goal
multiply the space by their own arrangements.  In tiles7a, one added column of identical
squares holding a single empty cell multiplied the distinct states within 22 moves 205 times
(2,978 to 611,170; 0.1 s to 17 s), while turning squares into empty cells inside the original
board shortened the solution and made the search cheaper.  Count what can move, not how much
room there is.

Cryptarithms with the donald-1 scheme (csp, columns right to left, forward checking), median
program cycles to prove: word length 6, 10, 14 (base 10, two addends): 1,556, 1,304, 750; more
columns add constraints, so length is harmless.  Addends 2, 3, 4, 6 (length 6): 1,556,
2,070, 6,757, 16,044; per-expansion work grows too (Q6).  Base 10, 12, 14, 16 (length 8):
1,036, 3,987, 39,485, 1,302,741, about 3.5 times per base step, while finding stays near
6,000 to 12,000 from base 13 up.  The base breaks first; and since each new puzzle needs a
hand-written column action per column, a family of any size calls for a spec that builds its
actions from the words.

Hanoi (peg-per-disk encoding, cutoff 2^n - 1, 16 threads): 10 disks 6.6 s, 11 disks 54.9 s,
growing 8.3 times per disk while the boards grow only 3 times, because a board is expanded
again each time a shorter route reaches it (section 6.1).  Projected: 14 disks overnight
(4.6 to 8.7 hours), 15 out of reach.  A breadth-first search would expand each board once,
but Wouldwork has none; the number of pegs was not measured.

---

## 7. Corrections to the Manual (26.8)

Recorded here, not yet made in the Manual:

- *Heuristic Search* says the heuristic works only in serial and produces a beam search.  The
  source orders successors in both serial and parallel search, with no beam width
  (`doc/search/heuristics.md`).
- *Optimization Problems* names the bounding query `get-best-relaxed-value?`; the source uses
  `bounding-function?`.
- *Bi-Directional Search* names `backward-state-exists`; the source function is
  `backward-path-exists`.  It cites `problem-triangle-forward6.lisp` and
  `problem-triangle-backward6.lisp`; probs/ holds the pair `problem-triangle-xyz-backward.lisp` and
  `problem-triangle-xyz-forward.lisp`.
- *Decision Outline* says CSP may use graph search; staging forces csp to tree.
- `prune-state?` and `min-steps-remaining?` are supported but not described.

---

## 8. Report to the user

Use only the report fields needed for the selected procedure and objective. They summarize
findings in Analysis.txt, not four mandatory reports or a second record. Include supporting
evidence, scope, uncertainties, affected dependencies, and the recommended next action.
State what was not run when relevant. Pause at the applicable approval boundary; omit
irrelevant measurements rather than performing work to fill a report field.

**Phase 0: profile** (in the puzzle's own terms, no setting names; nothing run)

```
PROBLEM:      <name>, <spec path>
SUMMARY:      what the problem asks and its size as read from the spec, in a few sentences
PROFILE:      the answered questions that mattered, each named by what it profiles rather
              than its Q number, then the answer, marked spec / user
              ("Repeated states: yes, moves can be undone (spec)"); unknowns listed, with
              a proposed clarification or check if resolving them matters
QUESTIONS:    what only the user can say (objective, time, machine, known lengths, larger
              members of the family), as one numbered list
```

**Phase 1: overview** (first probes; the puzzle's own terms, no setting names)

```
FINDINGS:     one named line each, what was read or measured with the number behind it,
              marked spec / probe
DIRECTION:    the recommendation in a sentence
```

**Phase 2: recommendations** (proposed, accepted or rejected one by one, then reported once
applied)

```
RECOMMENDATIONS:  numbered items, each one specific change: a short descriptive name, then a
                  few plain sentences saying what changes, why (the finding and the number
                  behind it), and a recommendation (accept, or accept only if ...).  Settings,
                  action order, re-encodings, hooks (with their soundness argument),
                  refactorings and each minor clean-up are all items of this one list
NOT RECOMMENDED:  changes considered and declined, each with its reason
DRAFT RESULTS:    results of authorized draft experiments, with scope; otherwise not run
CHECKS:           once applied: before and after counts, the agreement of any independent
                  script, and the REPL forms to stage and test the copy
```

**Phase 3: results and estimates** (short runs only; rough estimates).  Present the results
briefly, without the background: the user asks for the justification when needed.

```
RESULTS:      the shortest or best answer found, and the cost of each regime the user might
              want (find, prove, every, best); how many distinct solutions exist (density,
              section 6.1); whether threads are needed
SCALING:      a small table: each way the problem grows, the cost growth per step, and an
              estimate at larger sizes as a range; which way limits it
MEANING:      one line: what a solution, or a search that runs out, proves
FURTHER:      options beyond this run, each with a descriptive topic name, a plain-language
              description of what it is and what it would gain, and a recommendation (do it,
              do it only if ..., or leave it); none is started without the user's go-ahead
```

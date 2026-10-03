# Wouldwork Search Advisor

> **Usage:** An optional aide, run on its own.  Attach this file with a problem spec that
> already stages and whose rules you trust, and say what you want from the search.  The
> assistant works in three phases (section 1): an overview with the main findings; spec
> upgrades for simplicity or efficiency, each written into a copy of the spec once you agree
> (the original is never changed); and the global analysis (scaling, trade-offs, search
> options) ending in the final report.  It guides or (with approval) runs the searches, and
> says what each result means.

> **Status:** Source-checked 2026-10-03 against `src/ww-settings.lisp`, `ww-initialize.lisp`,
> `ww-searcher.lisp`, `ww-planner.lisp`, `ww-parallel.lisp`, `ww-parallel-infrastructure.lisp`,
> `ww-validator.lisp`, `ww-support.lisp` and the enumerator,
> and against the *Wouldwork User Manual (26.8)*, Part 3.  Where they disagree the source wins;
> the disagreements are listed in section 7.

**Scope.**  This advisor does not check whether the spec is a faithful model of the puzzle
(that is the spec-advisor's job) and does not run a solve.  It assumes the rules are right and
asks only: what is the most efficient way to search them?  If a probe exposes a modelling
error, stop and fix the spec first.

**When one search is not enough.**  If the probes show that no single search can finish (a
large N, or a problem whose difficulty lies in reaching its milestones in the right order),
offer the solving-advisor (`doc/constraint-led-solving/solving-advisor.md`) as the
alternative.  It is an interactive, constraint-led dialogue: the user and assistant agree one
subgoal at a time, check it against the problem's static constraints, realize it with a
bounded search, and finally validate the whole chained path.  It was built for Talos problems,
but its subgoal dialogue applies to any problem with milestones (Q16).

---

## 1. Process

The work runs in three phases, each ending at a point where the user approves, defers or
redirects before the next begins.  Phase 2 comes before phase 3 because the global measurements
(expansion, trade-offs, density) depend on the spec they are taken on: they are taken on the
upgraded spec, not the original (donald: the original's 720 reorderings of one answer would
have swamped every scaling figure).  Phase 3 may point back to phase 2 (a spec that generates
its actions, finer actions); such a change is proposed, applied and re-measured as in phase 2,
then phase 3 resumes.

Number every finding (F1, F2, ...) and every recommendation (R1, R2, ...) throughout, and let
each recommendation cite the findings it rests on; numbered items can be approved, deferred
or argued one at a time, where a finding mentioned in passing is easily lost.  Numbering
continues across phases.

### Phase 1: overview and main findings

1. **Read the spec** in full: types, actions, any happenings, the goal, the current `ww-set`
   values, and any search hooks already defined (`heuristic?`, `prune-state?`,
   `min-steps-remaining?`, `bounding-function?`, `encode-state`, enumerator declarations).
   Note any construction that is unusual or more complex than it needs to be (for example,
   read-time `#.` evaluation), and also the minor clutter that makes the spec harder to read:
   commented-out debug prints, a query that only calls another, intermediate variables or
   lists a simpler test makes unnecessary.  Check comments against the code (donald's header
   claimed forward checking that the actions did not do).
2. **Answer the questions in section 2** from the spec, marking each answer *spec*, *user* or
   *probe*.  Leave an answer *unknown* rather than guess.
3. **Run the first probes** (section 6.1): short bounded searches, never a full solve
   (section 1.1 says who runs them), enough to measure the size of the search and the waste
   the phase-2 changes would remove (donald: 165,978 expansions as written, 17,239 in graph
   search, 1,441 in a fixed order).
4. **Orient the user.**  A short summary in the puzzle's own terms, with no setting names:
   what the problem asks, how large its search is, the main findings as numbered items, and
   the recommendation in a sentence.  This is the phase-1 report (section 8).
5. **Ask the user** only what the spec cannot tell: the objective (any solution, several,
   every, or a best one, and best by what), the time available, the thread count of the
   machine, whether a solution length or depth is already known, and whether larger members
   of the family matter.  One question at a time.

### Phase 2: spec upgrades for simplicity and efficiency

6. **Propose the upgrades**, numbered, each with its reason and the findings behind it:
   - choices that live in the spec: the strategy where it is a spec setting (`*problem-type*`
     csp, `*tree-or-graph*`, `*solution-type*`, symmetry), and action order (section 3);
   - representation changes (Q18) and refactorings of unusual constructions (section 1.1);
   - search hooks (section 4.1), each with its soundness argument;
   - minor clutter, together under CLEANUP.
   Check the conflicts in section 5 first.
7. **Apply what the user agrees to** in a copy of the spec (section 1.1), one change at a
   time, and check each: the copy stages, and a probe gives the same answers (goal states,
   best value) with the counts the change predicts.  An independent counting script
   (section 6.3) that reproduces **Program cycles** on the upgraded spec is the strongest
   check, and is reused in phase 3.
8. **Report** the phase-2 changes (section 8): what changed, the before and after counts,
   and the REPL forms to stage and test the copy.

### Phase 3: scaling, trade-offs, search options and the final report

9. **Measure on the upgraded spec**: the cost of each regime the user might want (section
   3.2), solution density (section 6.1), the projection to a target size if one is named
   (section 6.3), and always the expansion of the problem family (section 6.4).
10. **Choose the run-time strategy and settings** (sections 3 and 4): threads and the other
    REPL-only settings, the escalation order if the search does not finish, and the
    multi-step strategies (S7 to S11) where the measurements call for them.
11. **Final report** in the form of section 8.  Give one recommendation, not a menu, plus the
    order in which to escalate.  List larger interventions the measurements point to (a
    re-encoding, a generator for the family, a pruning invariant, an engine change) under
    FURTHER, as options for the user to take up later, not as steps of this run.
12. **Run the strategy**: short runs here with approval, deep runs on the user's machine,
    multi-step strategies one step at a time (section 1.1).  Interpret each result with
    section 6.2 before the next step, and escalate only with the user's agreement.

### 1.1 Who does what

**Editing the spec.**  The advisor never changes the original spec.  The first change the user
agrees to creates a copy beside it, named by extending the original's name
(`problem-knap19.lisp` to `problem-knap19-1.lisp`), with `*problem-name*` and the `;;; Filename:` header line changed to match
(`knap19-1`).  Staging reloads the file that the header names, so a copy that still carries
the original's header silently stages the original (seen with a donald variant: every
`(stage ...)` of the copy ran the original's actions, with no error); later agreed changes in the same run go into that copy.  A change that needs a
separate variant of its own (relaxed, backward, re-encoded) takes the next number.  Never edit
the generated `src/problem.lisp`.

- State the change and its reason first; change only what was agreed.
- A `ww-set` goes in the spec's `ww-set` block, replacing any existing value for that setting.
- REPL-only settings (`*algorithm*`, `*threads*`, `*debug*`, `*probe*`) are never written
  to the spec; give the REPL form instead.
- A hook query or macro action is shown in full and written once agreed.
- **Representation changes** (Q18, or a probe showing wasted states).  Implement directly when
  the new spec is a re-encoding of the same rules and goal: the same moves, only stored
  differently.  Check it by running the same probe on both specs: the reachable boards and goal
  states must correspond.  When the change alters what the model means (new rules, a changed
  goal, a different level of detail), write a short prompt for the spec-advisor instead,
  naming the finding and the proposed encoding, and resume here once that spec stages.
- **Refactorings** (phase 1, step 1: an unusual or needlessly complex construction).  When a plainer
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
  the spec's own settings; `(refresh)` does not, and a saved `vals.lisp` overrides them on an
  ordinary load.

**Running searches.**

- **Short runs** (probes, `validate-solution`, shallow searches taking seconds): the
  assistant may run these itself with approval, in its own environment or on the user's
  machine.  Report the measured numbers, not expectations.  Staging on the user's machine
  overwrites their `src/problem.lisp` and `vals.lisp`: say so first.
- **Deep runs** (full solves, parallel searches, backward searches to the memory limit,
  enumerator layers, `every` searches feeding `freq`): run on the user's own hardware, at
  their REPL, where the threads and memory are.  The assistant supplies the exact forms and
  what to paste back, and runs one itself only with explicit approval for that run.  Never
  start a long search unasked, and never raise the depth or thread count silently.
- **Multi-step strategies** (S7 macros, S8 subgoaling, S9 relaxation, S10 bidirectional, S11
  enumerator): guide one step at a time.  Give the forms for the step, wait for its result,
  check it, then give the next.  Keep the forms of each step so the run can be repeated.

Always validate on a small version of the problem first with plain brute force (Manual,
*Brute-Force Search*).  Every other strategy rests on the same depth-first search.

---

## 2. Questions that decide something

Only questions whose answers change a strategy, a setting, a spec addition, or the meaning
of a result belong here.

| Id | Question | How settled | What it decides |
|---|---|---|---|
| Q1 | Is the answer a sequence of moves, or an assignment of values (each variable set once, order irrelevant)?  If an assignment, does every solution use each action exactly once (one action per variable or group of variables), or does one action range over all the variables (knap19's `put`)? | spec | `*problem-type*`; CSP strategy (S3) when each action is used exactly once: csp itself imposes the fixed order (at depth n, while n is below the number of actions, only the nth defined action is tried; `generate-children` in `ww-planner.lisp`), whether or not the spec intends one |
| Q2 | What is wanted: any one solution, N, every, every path, or a best one (fewest steps, least time, min/max value)? | user | `*solution-type*`; optimization (S4) |
| Q3 | Are there happenings or patrollers? | spec: `define-happening`, `define-patroller` | tree search; `*auto-wait*`; no backtracking |
| Q4 | Do states repeat (moves can be undone, or different orders reach the same state)? | spec, then probe: repeated-state percentage | `*tree-or-graph*`; whether backtracking fits |
| Q5 | Does some quantity change by a fixed amount on every move (peg count, items placed), **and** does the goal fix its final value (one peg left, all N items placed)?  Both are needed: a knapsack places one item per move, but its goal does not fix how many | spec: action effects and goal | fixed solution length: `first` rather than `min-length`; exhaustion becomes proof (section 6.2); an exact `*depth-cutoff*` only if moves can continue past that length, or for `min-steps-remaining?` |
| Q6 | How large is the space?  Branching factor b and solution depth d; under graph search, the number of distinct states | spec, probe | whether brute force can finish (roughly under a billion).  b^d counts paths; when moves commute (any order of the same choices reaches the same state) graph search visits only the distinct states, which can be far fewer (knap19: 17^11 paths, 36,326 states).  Count the work per expansion too: an action's `product` enumerates every combination of its parameters before the precondition rejects any, so an action with k free parameters over a domain of size m costs up to m^k tests per state (a 41-addend cryptarithm whose units column holds 9 letters: 31 s for 197,005 expansions, against 7 ms for DONALD's 424).  The remedy is finer actions, each fixing one or two variables |
| Q7 | Are there objects of one type with identical static facts that the goal does not name, **and** do the actions take them as typed parameters (`?peg peg`)?  Objects supplied by a query (`?peg (get-remaining-pegs?)`) are not recognized, so no family is found | spec; staging lists the families | `*symmetry-pruning*`; if the names serve no purpose, Q18 instead |
| Q8 | Is there a cheap measure of how close a state is to the goal? | spec, user | `heuristic?` (S6) |
| Q9 | Is there a cheap lower bound on the moves still needed, one that never overestimates? | spec, user | `min-steps-remaining?` (S5) |
| Q10 | Can some states be proved dead (an invariant broken, a resource gone, a bound exceeded)? | spec, user | `prune-state?` (S5) |
| Q11 | For min/max-value, can an optimistic value of a partial state be computed cheaply? | spec, user | `bounding-function?` (S4) |
| Q12 | Can goal states be listed explicitly, and can every action be reversed? | spec | bidirectional search (S10) |
| Q13 | Is the goal described by base facts (positions, pairings) from which the derived facts follow? | spec: `propagate-changes!`, derived relations | enumerator meet-in-the-middle (S11) |
| Q14 | Is the work per state dominated by working out derived facts? | probe: states per second, compared with a problem without propagation | relaxation (S9) |
| Q15 | Do the same few moves recur together in solutions of small versions? | probe: `freq` on every solution of a small version | macro actions (S7) |
| Q16 | Are there natural milestones every solution must pass (a gate opened, an object placed)? | spec, user | subgoaling (S8) |
| Q17 | Does the problem use the Talos recorder or connectors? | spec: `include-tech` | recorder and connector limits (section 4) |
| Q18 | Do objects carry names the puzzle never uses (identical pegs, tokens), is the same fact stored more than once (`loc>` and `contents>`, a list beside a count), or are there objects that static facts rule out of every action (an item heavier than the capacity, tried and rejected at every state)? | spec | re-encoding (section 1.1): record only what matters (which positions are occupied).  Removes duplicate states at the source, which is cheaper and more complete than `*symmetry-pruning*` |

---

## 3. Strategies

Listed roughly from cheapest to most effort.  Most combine: parallel with pruning, macros with
heuristics, subgoals with any of them.

| Id | Strategy | Use when | How | Cost and cautions |
|---|---|---|---|---|
| S1 | **Brute force, iterative deepening** | Always first; and as the whole answer when b^d is modest | `first`, small `*depth-cutoff*`, raise it until a solution appears, then lower it to find the shortest | Exponential in depth |
| S2 | **Parallel search** | The space is large and the search is depth-first | `(ww-set *threads* N)` at the REPL | Best with tree search; graph search shares a locked closed table.  Not with backtracking or auto-wait (errors), nor with problems that create objects during search (section 5); `all-paths` falls back to `every` |
| S3 | **CSP (fixed-order assignment)** | Q1 is an assignment with each action used exactly once | `*problem-type*` csp; `*depth-cutoff*` 0; define the actions in the order they should run: each action's inputs fixed by earlier actions (a carry chain runs from the units column), and the most constraining variables first.  `*algorithm*` backtracking (REPL) optionally; depth-first also respects the order.  Narrow the remaining domains as values are assigned (forward checking; `define-update` as in `problem-captjohn.lisp`) | Backtracking is serial only and ignores every search hook (section 5).  Action order matters: donald-1 right to left 424 expansions to prove, left to right 1,440.  With forward checking a variable can be fixed by elimination, so the goal must test the constraints themselves, not only that every variable has one value: donald-1's first draft, whose goal tested only that, reported 30 false solutions one column early |
| S4 | **Optimization** | Q2 asks for a best solution | `min-length`, `min-time` (action durations), `min-value`/`max-value` (assign `$objective-value` in each assert); add `bounding-function?` for value problems | Must search until the bound is proved, so far more work than `first`.  Pointless at fixed length (Q5) |
| S5 | **Pruning hooks** | Q7, Q9 or Q10 answered yes | `*symmetry-pruning*` t; define `min-steps-remaining?` or `prune-state?` as queries | Must be **sound**: a bound that overestimates, or a dead test that rejects a live state, silently discards solutions.  Symmetry checking has overhead and removes variants under `every` |
| S6 | **Heuristic ordering** | Q8 yes and the first solution is wanted fast | define `heuristic?`; lower values are explored first | Orders successors only: still complete depth-first search, not beam or A*; first solution need not be shortest.  Serial and parallel both use it; backtracking does not; overrides `*randomize-search*` |
| S7 | **Macro actions** | Q15 shows recurring multi-move patterns | add combined actions before the base actions; find candidates with `(freq 2 3)` after an `every` search of a small version | Each added action costs work at every state.  Keep the base actions |
| S8 | **Subgoaling (goal chaining)** | One search cannot reach the goal, and Q16 gives milestones | `(solve-subgoal <goal>)` serially, or the two-argument checkpoint form (serial or parallel), with `ww-undo`, checkpoint export and import; `solve-via-strategy` for a registered multi-phase strategy | A milestone reached the wrong way can block the rest.  The solving-advisor (`doc/constraint-led-solving/solving-advisor.md`) is the worked-out interactive form, built for Talos problems with gates and bottlenecks |
| S9 | **Relaxation** | Q14: propagation dominates and base facts approximate the derived ones | a separate spec whose preconditions ask a weaker, cheaper question; the goal calls `propagate-changes!` and tests the true conditions last | The cheap test must hold wherever the true one does, never the reverse.  No help when the difficulty is the number of choices |
| S10 | **Bidirectional search** | Q12 yes, and depth is the obstacle | a backward spec searched to depth d2 with `every`; `encode-state` in it; `(get-state-codes)`; a forward search to d1 = d - d2 whose goal calls `(backward-path-exists state)` (see `problem-triangle-backward.lisp`) | A second spec to write and keep consistent; memory for the backward layer |
| S11 | **Enumerator meet-in-the-middle** | Q13 yes: goal states can be generated from base facts | `define-base-relation` (plus optional `define-goal-filter`, `state-feasible?`); `(find-goal-states)`, `(find-predecessors)`, `(solve-meeting-point :depth-cutoff N :solution-type first)` (see the end of `problem-corner.lisp`) | Backward layers can explode; constrain base relations early |
| S12 | **Randomized and branch-restricted runs** | Exploring a huge space for any solution, or splitting work by hand | `*randomize-search*` t (repeat runs); `*branch*` n explores only the nth first move | A failed run proves nothing.  Ignored when `heuristic?` is defined |

### 3.1 Choosing

Start at the first row that applies; the fallback column is the escalation order.

| Situation | Primary | Then |
|---|---|---|
| Assignment, each action used once (Q1) | S3, with symmetry if Q7 | S4 for value optimization (depth-first, since backtracking ignores bounds) |
| Assignment by order-free actions (Q1; e.g. knap19's `put`) | S1, or S4 if a best one is wanted, in graph search: other orders of the same choices close as repeated states | S5 bound; S2.  Not S3: csp fixes the action only while the depth is below the number of actions, so a single `put` is fixed at depth 0 alone and every later depth tries all orders, in tree search.  If the spec has one action per variable group but no order (the original donald), S3 rather than graph search: graph search closes the reordered states, but csp never generates them (donald, every solution: tree 165,978 expansions, graph 17,239, csp 1,441) |
| Happenings (Q3) | S1 in tree mode, `*auto-wait*` if waiting matters | S8 with time-tagged milestones |
| b^d modest (Q6) | S1, then S4 if a best solution is wanted | S2 |
| Fixed length (Q5) | S1 at the exact length with S5 dead-state pruning and symmetry | S10 (exhausting the remaining length proves a position dead); S7 |
| Large, reversible, with a distance measure (Q8) | S6, S2 | S5 lower bound for an optimal path; S8 at bottlenecks |
| Large, reversible, no distance | S2 | S10 if Q12; S7 |
| Expensive derived state (Q14) | S9 | S11; S8 |
| Too deep for one search, with milestones (Q16) | S8 | S10 or S11 for the last stretch |

### 3.2 Search regimes and their trade-offs

What the user wants decides what the search must do, and the costs can differ by orders of
magnitude.  Report the regimes the user might want with their costs, measured where cheap
(REGIMES in section 8), rather than choosing one silently.

| Regime | Setting | Must cover | Cost and what decides it | The result means |
|---|---|---|---|---|
| Find one | `first` | until the first goal | depends on density and on ordering (S6, action order); falls steeply with many solutions | a valid answer; nothing about others |
| Prove (no solution, or uniqueness) | `every`, or `first` run to exhaustion | the whole space, less what sound pruning removes | the size of the space; ordering does not help | with sound hooks, a proof (section 6.2) |
| Every solution | `every`, `all-paths` | the whole space | as prove, plus memory for the solutions recorded | every goal state (every path with `all-paths`) |
| Best | `min-length`, `min-value`, ... | until the bound is proved | as prove, less what the bound prunes (S4) | the optimum, if run to completion |

The ratio of prove to find is itself a measurement: near 1 means solutions are scarce or
found late, so a heuristic will not help; a large ratio means a `first` search is cheap but a
proof is not.  Cryptarithms, measured with the donald-1 scheme: DONALD 225 to find and 424 to
prove (1.9 times); a 41-addend puzzle 5,161 and 111,116 (21 times); random base-16 puzzles of
two 8-letter addends 6,253 and 1,302,741 (about 200 times).  When the gap is small, prefer the
proof, which also confirms the model has no unintended solutions.

---

## 4. Settings

Set in the spec with `ww-set`, except where marked REPL.  `(stage <problem>)` applies the
spec's own values; a saved `vals.lisp` otherwise overrides them on an ordinary load.

| Setting | Values (default) | Choose |
|---|---|---|
| `*problem-type*` | planning, csp (planning) | csp only for assignments (Q1).  csp forces tree search |
| `*algorithm*` **REPL** | depth-first, backtracking (depth-first) | backtracking for CSP, or a tree with no repeats where memory is tight; otherwise depth-first.  An error if set in the spec |
| `*solution-type*` | first, N, every, all-paths, min-length, min-time, min-value, max-value (first) | from Q2.  `first` at fixed length.  `every` gives one path per goal state; `all-paths` every distinct path to every goal, but only serial depth-first graph search with a depth cutoff (otherwise it falls back to `every`) |
| `*tree-or-graph*` | tree, graph (graph) | graph when states repeat (Q4: repeated-state percentage high); tree when they rarely do, with happenings, or for better parallel speedup |
| `*depth-cutoff*` | integer; 0 = none (0) | known or fixed length (Q5); otherwise iterative deepening.  0 for CSP.  Needed for `min-steps-remaining?` to prune before a first solution |
| `*symmetry-pruning*` | t, nil (nil) | t when Q7; staging reports the groups found, and suggests turning it off if none |
| `*threads*` **REPL** | 0 = serial, N (0) | any depth-first search (S2).  Changing it restages.  0 for backtracking, auto-wait, and problems that create objects during search |
| `*randomize-search*` | t, nil (nil) | S12 only |
| `*branch*` | n (0 = all) | S12 only |
| `*auto-wait*` | t, nil (nil) | happenings where waiting may be needed; try without first, since it enlarges the search.  Tree, serial, depth-first only |
| `*auto-wait-max-time*` | integer (100) | with `*auto-wait*` |
| `*progress-reporting-interval*` | integer (100000) | raise for long runs to cut output |
| `*max-recorder-cycles*` | integer, nil (1) | Talos recorder: recordings allowed in one path |
| `*recorder-prefix-pruning*` | t, nil (nil) | Talos recorder: also reject open recordings that can no longer replay |
| `*max-connector-pairings*` | integer, nil (nil: beam-relay's default) | Talos connectors |

Leave the parallel tuning settings (`*split-depth-max*`, `*tasks-per-thread*`, `*min-tasks*`,
`*num-closed-shards*`, work donation) at their defaults unless a measurement says otherwise.
`*debug*` and `*probe*` are diagnostic, REPL-only, and not search choices.

### 4.1 Search hooks

Defined in the spec as `define-query` with these reserved names; the state is supplied.

| Hook | Returns | Used by | Notes |
|---|---|---|---|
| `heuristic?` | a number, lower = more promising | depth-first, serial and parallel | ordering only (S6) |
| `prune-state?` | true to stop expanding the state | depth-first, serial and parallel | must be sound (S5) |
| `min-steps-remaining?` | a lower bound on moves to the goal | depth-first, serial and parallel | consulted only with a depth cutoff, or after a solution under `min-length` or `first`; must never overestimate.  In parallel it runs at task splitting and in every worker, without the serial adaptive sampling |
| `bounding-function?` | `(values cost upper)` for value optimization, both in minimizing terms (a max-value problem returns both negated) | depth-first, serial and parallel | `cost` is an optimistic bound: never worse than the best value any completion of the state can reach.  `upper` is the value of one completion that can actually be reached; the smallest `upper` seen becomes the incumbent, and a node is pruned when its `cost` is worse than it.  An `upper` that cannot actually be reached prunes the true optimum.  States the search records as best never tighten the incumbent; only `upper` does.  See `problem-knap19.lisp` (S4).  In parallel it runs at task splitting and in every worker; the shared bound is updated without a lock, so a race can only loosen it (sound, less pruning).  A hook that keeps its own state in globals (for example, a bound memoized across calls) is shared by all threads and is not thread-safe |

**Ordered completions.**  A bound may ignore completions that the state can reach but that
another order of the same moves also reaches, provided it depends only on the state and
every solution can be built in some order in which no state's bound excludes it.  knap19's
bound counts only items numbered above the largest item already packed: any packing built in
ascending item order never has one of its own items excluded, so the optimum survives.  Test
such a bound against an unpruned run (section 6.1).

Under `*threads*` > 0 the hooks must be pure functions of the state: any global a hook reads and writes is shared by every worker.

---

## 5. Conflicts and automatic adjustments (from `ww-initialize.lisp`)

- **Backtracking** forces tree search; is an error with `*threads*` > 0 or with happenings; and
  ignores `heuristic?`, `prune-state?`, `min-steps-remaining?` and `bounding-function?`.  With an
  optimizing solution type it enumerates without pruning.  With planning and no depth cutoff it
  may dive without limit.
- **csp** forces tree search, whatever `*tree-or-graph*` says.
- **Happenings with graph search** are reported as an error: states cannot be closed when time
  matters.
- **`heuristic?` with `*randomize-search*`**: randomization is ignored.
- **`*auto-wait*`** needs tree, serial, depth-first.
- **Symmetry pruning with `every`** drops solutions that differ only by symmetric objects.
- **Fixed length with `min-length`**: every solution has the same length, so the optimizing
  search only does extra work.
- **Value objectives with a goal**: once a solution exists, `min-value` prunes any node whose
  own value is already no better than it.  That is sound only if value never decreases along a
  path (costs accumulate, as in `problem-tsp.lisp`); a min-value problem whose value can fall
  needs a different solution type or a `bounding-function?`.  `max-value` prunes only goals
  that fail to beat the best solution; its other bounds come from `bounding-function?`, since
  rewards normally grow along a path (`test/problem-max-value-goal.lisp`). During parallel
  task generation, all four optimization modes register only goals that improve the
  incumbent; enumeration modes still retain their requested goals
  (`test/problem-task-goal-incumbent.lisp`).
- **The `bounding-function?` incumbent starts at 1,000,000.**  A min-value problem whose
  `cost` at the start state exceeds that has the start state pruned and searches nothing.
- **Parallel (`*threads*` > 0)**: `*auto-wait*` is an error (`ww-validator.lisp`); `all-paths`
  falls back to `every` with a note; problems that register objects during search must declare
  `(ww-set *search-registers-dynamic-objects* (beam))` (for example, the corner family's beam
  objects). With worker snapshots enabled, this combination is rejected at search entry,
  before task generation; use `(ww-set *threads* 0)` for serial execution. Registration
  during initialization, before snapshots are published, is allowed and does not require
  the declaration. Disabling snapshots is not a validated parallel workaround for these
  problems. `*debug*` above 1 is reset to 1; symmetry statistics are approximate.  `heuristic?`, `prune-state?`,
  `min-steps-remaining?`, `bounding-function?`, `*randomize-search*` and `*branch*` all apply.

---

## 6. Probes and reading results

### 6.1 Probes

A probe is a bounded exhaustive search at a shallow depth, whose statistics answer Q4, Q6
and Q14.  Run it on the full problem with a goal it will not reach that early (or temporarily
`(ww-set *solution-type* every)`), at two or three depths a step or two apart:

```lisp
(stage <problem>)
(ww-set *depth-cutoff* 6)
(solve)
```

Read from the summary: **Total states processed** and how it grows between depths (effective
branching, so b^d for the needed depth); under graph search, **Program cycles** counts the
distinct states expanded, which is the better size measure when moves commute; **Repeated states pruned … percent** (high favours
graph search); **Average branching factor**; and elapsed time, giving states per second.  A low
rate on a problem that calls `propagate-changes!` points to relaxation (Q14).  Run the same
probe with `*symmetry-pruning*` t to see whether symmetry pays for its overhead, and with
`*threads*` N to see whether parallelism does.

For macro candidates (Q15): solve a small version with `every`, then `(freq 2 3)`.

**Solution density.**  Run `every` to exhaustion on the full problem if it finishes in
seconds, otherwise on a small version.  Compare the number of distinct goal states with
**Program cycles** (distinct states).  Under graph search the recorded path count is only a
lower bound, since paths through repeated states are cut.

| Density | Means | Favours |
|---|---|---|
| Many goal states, found by many workers | Solutions are plentiful | `first`; S12 randomized runs; no heuristic needed |
| Few goal states, deep in the space | A needle in a haystack | S5 dead-state pruning; S10 bidirectional; S6 if a distance measure exists |
| None | No solution within the cutoff | raise the cutoff, or check the model |

A value problem with no goal (every state is a candidate) has no goal states to count.  Its
density is the number of states that reach the optimum, and what decides the cost is how
much `bounding-function?` prunes.  Measure that with the same run twice, the second with the
hook removed:

```lisp
(stage <problem>)
(solve)
(fmakunbound 'bounding-function?)
(solve)
```

The ratio of **Program cycles** is the bound's pruning factor (knap19: 36,326 to 432,
with the optimum reached by 1 state).  The unpruned run also tests the bound's soundness: its
best value must equal the pruned run's.

With `*threads*` > 0, **Program cycles** leaves out the states expanded while the search is
split into tasks (the shallowest levels), and is 0 when the whole search finishes during the
split.  knap19 unpruned at 16 threads: 36,172, plus the 154 states with 0 to 2 items, gives the
serial 36,326.  For exact counts, compare serial runs (`(ww-set *threads* 0)`).

### 6.2 What a result means

| Result | Means |
|---|---|
| Solution found | A valid path under the model; shortest only with `min-length` run to completion |
| Exhausted, fixed-length problem, cutoff = remaining length | **Proof**: no solution from that state |
| Exhausted, otherwise | No solution within the cutoff; a longer one may exist |
| Exhausted with a hook pruning | Only as reliable as the hook is sound |
| Exhausted with `*branch*` or a relaxed spec | Says nothing about the full problem |
| Out of memory or interrupted | Neither a bound nor a result |

### 6.3 Projecting to a larger problem

When the user will scale the problem up (a larger board, more items, more steps), run the
same probe at two or three sizes a step apart and report the growth per step: the ratio of
distinct states, and of elapsed time.  Project those ratios to the target size, and compare
the projected states with memory and the projected time with what the user has.  Memory is
the closed table's entries times the bytes per state, and the bytes per state must be
**measured**, not guessed: the peak memory of a run that fits (Task Manager, or `(room)` after
`(sb-ext:gc :full t)`) divided by its **Program cycles**, compared with the heap limit
`(sb-ext:dynamic-space-size)`, not the machine's RAM.  (triangle-xyz-1 at N = 7: 40.6 million
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
can also remove every solution (triangle-xyz-1: 4 goal states at N = 5, 7 at N = 6, none at
N = 7); `first` then searches the whole space, and exhausting it is the answer.

For a value problem with a bound, project from instances whose data resembles the target's,
not from the item count alone: how the values relate to the weights decides how much the
bound prunes.  Instances shaped like knap19 stay in the tens of expansions from 19 to 28
items while their distinct states grow about 1.8 times per item; `data-knap30.lisp`, whose
values nearly equal their weights, keeps the bound to a factor of 2 (13,053 program cycles
unpruned, 6,577 pruned).

When moves are simple enough, a short independent script that counts the reachable states
(outside Wouldwork) checks both the projection and the encoding: its counts should equal
**Program cycles** at the sizes already run.

### 6.4 Expanding the problem family

Always analyse how the problem's family grows, even when the user names no target size.  A
spec is usually one instance of a family (a board size, a puzzle of a given shape), and the
settings that suit the instance can fail on its larger members.  Section 6.3 projects to one
target size; this section asks which way of growing hurts first.

1. **Name the dimensions** along which the family grows: for a cryptarithm, the base, the
   number of distinct letters, the word length and the number of addends; for a board, its
   side and the number of pieces.
2. **Probe one dimension at a time**, the others fixed, at three or more steps, reporting
   the median and the maximum over several instances where instances vary.
3. **When the spec is written by hand for one instance**, generate the larger instances
   with the independent counting script (section 6.3), after checking that it reproduces
   **Program cycles** on the instance in hand.  Generated instances should be solvable and
   resemble the target's data (random digits, then letters assigned, for cryptarithms).
4. **Report the growth per step along each dimension**, the first dimension to break, and
   its mitigation (the table in section 6.3); note any dimension along which the cost falls.

Cryptarithms with the donald-1 scheme (csp, columns right to left, forward checking), median
expansions to prove: word length 6, 10, 14 (base 10, two addends): 1,556, 1,304, 750; more
columns add constraints, so length is harmless.  Addends 2, 3, 4, 6 (length 6): 1,556,
2,070, 6,757, 16,044; per-expansion work grows too (Q6).  Base 10, 12, 14, 16 (length 8):
1,036, 3,987, 39,485, 1,302,741, about 3.5 times per base step, while finding stays near
6,000 to 12,000 from base 13 up.  The base breaks first; and since each new puzzle needs a
hand-written column action per column, a family of any size calls for a spec that builds its
actions from the words.

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
  `problem-triangle-backward6.lisp`; probs/ holds `problem-triangle-backward.lisp` only.
- *Decision Outline* says CSP may use graph search; staging forces csp to tree.
- `prune-state?` and `min-steps-remaining?` are supported but not described.

---

## 8. Report to the user

The report comes in three pieces, one at the end of each phase, so that each can be approved
before the next phase builds on it.  Numbering of findings and recommendations continues
across the pieces.

**Phase 1: overview** (the orientation, in the puzzle's own terms, no setting names)

```
PROBLEM:      <name>, <spec path>
SUMMARY:      what the problem asks and how large its search is, in a few sentences
FINDINGS:     F1, F2, ...: one numbered line each, what was read or measured with the number
              behind it, marked spec / user / probe
PROFILE:      the answered questions that mattered, each marked spec / user / probe;
              unknowns listed
DIRECTION:    the recommendation in a sentence, and the question for the user
```

**Phase 2: spec upgrades** (proposed, then reported once applied)

```
UPGRADES:     U1, U2, ...: each change with its reason and the findings it rests on
              (strategy settings that live in the spec, action order, re-encodings,
              refactorings)
SETTINGS:     the ww-set block of the copy, one reason per line
SPEC HOOKS:   H1, H2, ...: proposed queries or actions, each with its soundness argument
CLEANUP:      C1, C2, ...: minor rewrites that make the spec easier to read without changing
              the search, each with what it removes; written together into the copy once
              agreed, and checked by a probe giving the same counts
CHECKS:       before and after counts for each applied change, the agreement of any
              independent script, and the REPL forms to stage and test the copy
```

**Phase 3: final report**

```
STRATEGY:     <primary>, with the reason in a sentence, citing findings (F2, F4)
ESCALATION:   E1, E2, ...: in order, with what would trigger each
REPL:         REPL-only settings (*threads*, *algorithm*, ...), one reason each
REGIMES:      the cost of each regime the user might want (find, prove, every, best),
              measured where cheap, and the ratio between them (section 3.2)
DENSITY:      goal states found versus distinct states, and what it favours (section 6.1)
SCALING:      growth per size step and the projected cost at the user's target size, with
              the mitigation for that regime (section 6.3); "none named" if no target
EXPANSION:    the family's size dimensions, the growth per step along each, and the first
              to break, with its mitigation (section 6.4)
PROBES:       P1, P2, ...: commands still worth running, and what each would change
MEANING:      what a solution, an exhaustion, or a crash will prove under these settings
FURTHER:      X1, X2, ...: interventions beyond settings and hooks, for the user to pursue
              or not: a re-encoding (Q18), a generator for the family, a sound pruning
              invariant worth prototyping, an engine change the measurements point to.
              Each with its expected payoff, what it would cost, and the measurement behind
              it; none is started without the user's go-ahead.  An item that is a spec
              change can return to phase 2
RECOMMEND:    R1, R2, ...: the actions proposed, in order, each citing the findings it
              rests on
```

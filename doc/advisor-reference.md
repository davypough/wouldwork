# Wouldwork advisor capability reference

Shared Wouldwork capability reference for the [problem](problem-advisor.md),
[specification](spec-advisor.md), [search](search-advisor.md), and
[solving](solving-advisor.md) advisors. It records what the Wouldwork engine (`src/`) can do, under which conditions, with source evidence. Talos technology behavior (`tech/`) is outside its scope: see `tech/README.html` and the solving advisor's engine facts. Authoring syntax is in [spec-advisor.md](spec-advisor.md) Part 4, with a pre-staging checklist in Section 4.15.

## How to use this reference

Consult only the entries that a current issue raises. They are an index for reasoning, not an opening questionnaire. Each entry gives a trigger and question, consequences and support, source evidence (file and symbol), a proposed check, and its verification date. Source observations establish source behavior only: unless an entry says otherwise, no runtime checks, searches, or benchmarks were performed for it. Proposed checks require the appropriate later authorization. Recheck an entry when the cited source changes or a proposed use goes beyond its stated contract. Keep one authoritative contract here and refer to its entry ID when another entry or document depends on it.

Support labels, used independently of evidence status:

- **B — built in:** an engine mechanism supplies the capability under the stated conditions.
- **S — spec logic:** problem-specific representation, rules, queries, formulas, or proof obligations are required.
- **A — possible augmentation:** support has not been established; investigate the gap before proposing engine work.

Problem-specific recommendations drawn from these entries remain deductions conditional on their stated assumptions. Possible augmentations are hypotheses, not claims that a facility is absent.

## Index

| ID | Topic | Engine features covered |
|---|---|---|
| F01 | What counts as a different answer | COUNT, EVERY, ALL-PATHS, `*unique-solution-states*` |
| F02 | Which result is required | FIRST, N, EVERY, MIN-LENGTH, MIN-TIME, MIN-VALUE, MAX-VALUE |
| F03 | What grows, and what interacts | branching statistics, parallel timing |
| F04 | What the situation must remember | state databases, duplicate handling |
| F05 | Which choice is generated next | action domains, CSP action scheduling |
| F06 | Can situations recur | tree/graph, `*depth-cutoff*`, `*bt-cycle-check*` |
| F07 | Does the order of independent choices matter | duplicate handling |
| F08 | Which identities are interchangeable | `*symmetry-pruning*` |
| F09 | What can already rule out a choice | preconditions, inconsistent states, followups, `prune-state?` |
| F10 | Which resources must remain available | state updates, pruning and bounds |
| F11 | Can parts be solved independently | goal chaining, `solve-subgoal` |
| F12 | Is an estimate safe for elimination | `heuristic?`, `min-steps-remaining?`, `bounding-function?` |
| F13 | Would working from endings help | enumerator, meeting-point search |
| F14 | What can be maintained instead of recomputed | backtracking undo, worker read snapshots |
| F15 | Can time change the available choices | happenings, waiting, `*auto-wait*` |
| F16 | What evidence would change this design | validation, search reporting |
| F17 | Search hooks: what each does and when it runs | the four hooks; check order in each driver |
| F18 | Passing a result from one search to the next | two-argument `solve-subgoal`, `list-database`, checkpoints |
| F19 | Settings: where each can be set and what reloads | `ww-set`, `stage`, `refresh`, `vals.lisp` |
| F20 | Symmetric and bijective relations as stored facts | `>` suffix, `:bijective`, index facts |
| F21 | Replaying and validating a path | `validate-solution`, `validate-action-sequence`, validators, checkpoints |
| F22 | Ways Wouldwork can approach a problem | CSP, backward search, enumerator, macros, relaxation, milestones, strategies |

## Entries

### F01 — What counts as a different answer?

**Trigger and question:** Counting, enumeration, or interchangeable-looking answers. Must an answer identify an assignment, final situation, action sequence, or equivalence class?

**Consequences and support:** B: solution registration, COUNT, and restricted ALL-PATHS. S: unique construction and canonical answer rules. If only final situations matter, merging suitable states may be sound; if routes or labels matter, it may remove requested answers. COUNT increments accepted-goal encounters in `*solution-count*` and retains `*count-example*`, without populating the ordinary solution lists. It is not a general distinct-answer counter. Special ALL-PATHS requires serial depth-first graph search and a positive cutoff; otherwise it falls back to EVERY semantics. Reject that fallback as a design mismatch when path multiplicity is required. Its bounded engine path treatment does not promise all unrestricted walks with repeated cycles. A parallel run, depth-first or backtracking, rebuilds `*unique-solution-states*` when the search ends, keeping one solution per goal database, so a count of distinct goal states remains valid with threads; COUNT skips that step.

**Evidence and assumptions:** [ww-searcher.lisp](../src/ww-searcher.lisp): `count-accepted-goal`, `register-solution`, `initialize-hybrid-mode`; [ww-backtracker.lisp](../src/ww-backtracker.lisp): `register-solution-bt`, `record-solution-bt`; [ww-parallel.lisp](../src/ww-parallel.lisp): `finalize-parallel-search-results`. Enumeration correctness also depends on F06-F08 and candidate acceptance. **Proposed check:** compare two legal construction orders reaching the same answer and state explicitly whether the requested count is one or two.

*Verified 2026-10-12.*

### F02 — Which result is required?

**Trigger and question:** “Solve,” “best,” “all,” or “impossible” is underspecified. Is one acceptable answer enough, are ties required, or must the covered space be exhausted? What cost and units define best?

**Consequences and support:** B: FIRST, numeric solution limits, EVERY, COUNT, MIN-LENGTH, MIN-TIME, MIN-VALUE, and MAX-VALUE registration and stopping paths. S: objective, acceptance, tie handling, and coverage argument. Feasibility can stop early; optimality or absence needs the appropriate exhaustive scope and sound reductions. Do not assume an optimization mode retains all tied optima. A cutoff, interruption, or restricted generator limits an absence claim.

**Evidence and assumptions:** [ww-searcher.lisp](../src/ww-searcher.lisp): `solution-count-reached-p`, `register-solution`, `node-descendants-cannot-improve-p`; F01 defines counting and F12 defines bound assumptions. **Proposed check:** describe two equally good answers and agree whether returning only one meets the requirement.

*Verified 2026-10-12.*

### F03 — What grows, and what interacts?

**Trigger and question:** The user wants larger instances. Which object counts, domain sizes, dependencies, sequence lengths, or state fields grow?

**Consequences and support:** S: instance families, compact sufficient representation, and avoidance of unnecessary permutations. B: search statistics and parallel timing. A: investigate a different method (F22) or engine extension if a justified representation cannot meet the target. Track both number of choices and cost per choice; size alone does not predict difficulty.

**Evidence and assumptions:** [ww-searcher.lisp](../src/ww-searcher.lisp): `compute-average-branching-factor`, `compute-effective-branching-factor`; [ww-parallel-backtracker.lisp](../src/ww-parallel-backtracker.lisp): `process-partitioned-parallel-bt-body`. These counters and timings do not supply a scaling law. **Proposed check:** specify a bounded family varying one growth dimension, with time, allocation/memory, generated work, and accepted outcomes; execute only after approval.

*Verified 2026-10-12.*

### F04 — What must the situation remember?

**Trigger and question:** Two apparently identical arrangements arise by different routes or at different times. Can their legal futures, goals, or costs differ?

**Consequences and support:** B: state databases, duplicate handling, and history validation hooks. S: sufficient encoding of resources, history, phase, and obligations. If futures differ, retain the distinguishing information or avoid the unsound merge. If they do not, merging may save repeated work. A validator that inspects a path does not automatically make database-based merging safe.

**Evidence and assumptions:** [ww-searcher.lisp](../src/ww-searcher.lisp): `process-successors`, `on-current-path`, `search-prefix-pruned-p`; [ww-planner.lisp](../src/ww-planner.lisp): `get-new-states`. F06 covers cycle restrictions and F15 time. **Proposed check:** give two histories with the same proposed state and compare all relevant next choices and goal/cost consequences.

*Verified 2026-10-12.*

### F05 — Which choice should be generated next?

**Trigger and question:** Many candidate combinations fail immediately. Are choices assignments to remaining variables, or transitions with changing available actions?

**Consequences and support:** B: typed and query-dependent action domains, preconditions, and CSP action scheduling. S: variable selection, domain narrowing, and unique construction. Both inspected drivers select the action indexed by depth in CSP mode while depth is below the number of actions, then fall back to the action collection. CSP is not an unconditional general variable-ordering or propagation solver. A single repeated assignment action can maintain its own next-variable progression.

**Evidence and assumptions:** [ww-planner.lisp](../src/ww-planner.lisp): `generate-children`; [ww-backtracker.lisp](../src/ww-backtracker.lisp): `backtracking-actions`, `visit-generated-choices-bt`. A restricted generator must preserve requested answers. **Proposed check:** enumerate the intended choices at two consecutive partial assignments and compare them with the proposed action/domain schedule.

*Verified 2026-10-12.*

### F06 — Can situations recur?

**Trigger and question:** Actions can undo earlier work. Is progress permanent, and what proves finite exploration?

**Consequences and support:** B: depth-first graph/tree search, backtracking tree search, depth cutoff, and optional BT PATH checking. S: monotone progress or canonical construction. Backtracking and CSP are forced to tree mode at initialization. Ordinary backtracking inverse detection compares only the immediate previous choice, and is skipped when either choice has `finally` or `next` followups, so back-and-forth moves in a problem that propagates are not caught by it; depth-first `on-current-path` scans ancestors. Optional BT PATH uses ancestor fingerprints to select exact database comparisons, but requires backtracking/planning/tree and excludes solution and prefix validators, goal-chain state/policies, happenings, auto-wait, recorder pruning/history, and static database writes. The current validator has no serial-only guard, and parallel workers initialize private path stacks; do not repeat the older plan's serial-only claim. This is source evidence, not a parallel PATH regression result.

**Evidence and assumptions:** [ww-initialize.lisp](../src/ww-initialize.lisp): `init`; [ww-backtracker.lisp](../src/ww-backtracker.lisp): `detect-path-cycle`, `validate-bt-path-mode`, `bt-on-current-path-p`; [ww-searcher.lisp](../src/ww-searcher.lisp): `on-current-path`; [ww-support.lisp](../src/ww-support.lisp): `reject-bt-path-static-write`; [ww-parallel-backtracker.lisp](../src/ww-parallel-backtracker.lisp): `run-bt-task`. Database-only recurrence must be semantically sound (F04). **Proposed check:** a three-move cycle distinguishes ancestor checking from immediate-inverse detection; state the cutoff's coverage separately.

*Verified 2026-10-12.*

### F07 — Does the order of independent choices matter?

**Trigger and question:** Several orderings appear to produce the same result. Does swapping adjacent choices preserve intermediate legality, cost, futures, and answer identity?

**Consequences and support:** B: graph duplicate handling. S: fixed construction order or ordering constraints can remove redundant permutations when the swap is sound. If order is part of an answer or changes a prerequisite, preserve it. A: investigate general partial-order reduction only if the problem needs more than spec logic.

**Evidence and assumptions:** [ww-searcher.lisp](../src/ww-searcher.lisp): `process-successors`; [ww-backtracker.lisp](../src/ww-backtracker.lisp): `backtracking-actions`. These mechanisms do not prove commutativity. **Proposed check:** compare both orders of two choices, including the intermediate state and any consumed resource; apply F01 before discarding either route.

*Verified 2026-10-12.*

### F08 — Which identities are interchangeable?

**Trigger and question:** Objects or spatial arrangements look equivalent. Does exchanging them preserve every rule, goal, cost, and requested distinction?

**Consequences and support:** B: object symmetry detection, instantiation filtering, and graph canonical symmetry hashing. S: problem-specific spatial canonicalization and counting conventions. Current detection considers static row symmetries and explicit transition, goal, and happening object references. This does not establish that arbitrary helper logic or user answer identity is invariant. Spatial rotations/reflections are separate from object interchangeability.

**Evidence and assumptions:** [ww-symmetry.lisp](../src/ww-symmetry.lisp): `detect-symmetry-groups`, `filter-symmetric-instantiations`, `use-canonical-symmetry-p`; [problem-queensN.lisp](../probs/problem-queensN.lisp): `queens-canonical-board-p`, `*queens-count-classes*`. The queens example has its own optional board-class rule. **Proposed check:** swap a candidate pair in a rule, goal, and answer; for rotations, separately test the intended class convention. Reopen F01 if labels become significant.

*Verified 2026-10-12.*

### F09 — What can already rule out a choice?

**Trigger and question:** A partial choice appears impossible, or seems to force a consequence. Is that conclusion necessary under every legal completion?

**Consequences and support:** B: preconditions, inconsistent-update/state handling, `finally`/`next` followups, and `prune-state?`. S: contradiction tests and justified forced updates; A: investigate stronger propagation if necessary. Reject a branch only from a sound impossibility argument. Followups run in every driver: under backtracking they run after each choice's forward update, inside its undo trail, and inconsistency is tested on the resulting state, so a contradiction test written as a followup (for example `(finally (check-clues!))`) prunes there as it does in depth-first search. In the inspected depth-first and backtracking expansion paths, `prune-state?` suppresses descendants; it does not reject a goal already accepted by the caller. Put goal acceptance requirements in the appropriate goal/validation logic. Hook calling order in each driver: F17.

**Evidence and assumptions:** [ww-planner.lisp](../src/ww-planner.lisp): `expand`, `update-is-inconsistent`, `state-is-inconsistent`; [ww-backtracker.lisp](../src/ww-backtracker.lisp): `backtrack`, `accept-goal-bt`, `apply-choice-followups-bt`, `explore-choice-bt`. Selecting CSP alone promises no general propagation strength. **Proposed check:** construct a partial state just inside and just outside the proposed contradiction condition, including a state that already satisfies the goal.

*Verified 2026-10-12.*

### F10 — Which resources must remain available?

**Trigger and question:** A scarce item, capacity, or commitment constrains later work. Is its use temporary, recoverable, or permanent, and can one resource satisfy several demands together?

**Consequences and support:** B: state updates and the pruning/bound hooks of F09/F12. S: balances, reservations, obligations, and admissible resource bounds. Permanent loss can provide monotone progress or impossibility tests; reusable equipment needs continuation reasoning. A failed proposed allocation is not proof that every allocation fails. Do not double-count demands that can share a resource.

**Evidence and assumptions:** [ww-planner.lisp](../src/ww-planner.lisp): `get-new-states`; [problem-knap19.lisp](../probs/problem-knap19.lisp): `bounding-function?` gives a problem-specific capacity example, not a universal resource model. **Proposed check:** compare two remaining obligations that can share one item with two that require it simultaneously.

*Verified 2026-10-12.*

### F11 — Can parts be solved independently?

**Trigger and question:** A proposed milestone or decomposition simplifies the task. What resources and continuation options must pass across the boundary?

**Consequences and support:** B: goal-chain continuation, recovery, and one-off subgoal search. S: interface contracts and milestone selection. Chained continuation requires serial execution. The explicit start/goal form supports either thread mode, but discards an active chain and saves undo state; it is not interchangeable with chained continuation. Mechanics of both forms and of carrying a result across a reload: F18. A: investigate specialized decomposition when the available interfaces do not suffice. Sequential milestone success does not establish global optimality.

**Evidence and assumptions:** [ww-goal-chaining.lisp](../src/ww-goal-chaining.lisp): `validate-continuation-preconditions`, `run-goal-chain-request`, `solve-subgoal-from-form`. **Proposed check:** compare two states satisfying the same milestone where only one preserves the resource needed next; refine the contract before treating the milestone as sufficient.

*Verified 2026-10-12.*

### F12 — Is an estimate safe for elimination?

**Trigger and question:** A score suggests promising choices or an unreachable target. Is it an ordering estimate, a proved optimistic bound, or the value of a feasible completion?

**Consequences and support:** B: `heuristic?`, `min-steps-remaining?`, and `bounding-function?`; S: formulas, admissibility, and evaluation-cost decisions. Ordering estimates need not be proofs; elimination bounds do. `bound-search-state` kills descendants when the first returned minimizing-form cost exceeds `*upper-bound*`, and tightens that bound with the second value when smaller. For maximization, the inspected knapsack example negates values into that convention. The second value needs a valid feasible-completion argument. Backtracking calls user and move bounds but lacks automatic objective-bound pruning. Depth-first automatic MIN-VALUE pruning assumes nondecreasing cost; a low-value MAX-VALUE non-goal does not bound its descendants. When and in which order each hook is called: F17.

**Evidence and assumptions:** [ww-searcher.lisp](../src/ww-searcher.lisp): `bound-search-state`, `node-descendants-cannot-improve-p`, `min-steps-remaining-bound-prunes-p`; [ww-backtracker.lisp](../src/ww-backtracker.lisp): `backtrack`, `ordered-choices-bt`; [problem-knap19.lisp](../probs/problem-knap19.lisp): `bounding-function?`. Move bounds are nonnegative lower bounds, compared with depth cutoff and applicable incumbent depth; driver and objective contracts must fit the use. **Proposed check:** on independently understood tiny cases, compare each proposed bound with actual completions, including a negative-cost step or growing reward that would invalidate a monotonicity assumption.

*Verified 2026-10-12.*

### F13 — Would working from endings help?

**Trigger and question:** Goal arrangements may be few or easier to describe than routes. Is generating endings cheaper, and can predecessor relationships be justified?

**Consequences and support:** B: base-relation schemas, goal enumeration, predecessor entry points, and meeting-point search. S: schemas, filters, feasibility logic, state keys, and witnesses. A: investigate unsupported inverse/domain requirements only after a focused audit. A satisfying ending is not necessarily reachable; enumerating all endings may itself be too large. The feasibility helper accepts a consistent state when no custom hook exists, which supplies no reachability proof. Other approaches, including backward search with a forward meeting search: F22.

**Evidence and assumptions:** [ww-enumerator-build.lisp](../src/ww-enumerator-build.lisp): `define-base-relation`; [ww-enumerator-run.lisp](../src/ww-enumerator-run.lisp): `find-goal-states-fn`, `find-predecessors-fn`, `fps-state-feasible-p`, `solve-meeting-point-fn`. Inspection confirms these entry points and the stated feasibility behavior, not universal inverse support or exhaustive reachability. **Proposed check:** audit a selected domain's schema, predecessor generation, key equivalence, and forward witness before recommending the method concretely.

*Verified 2026-10-12.*

### F14 — What can be maintained instead of recomputed?

**Trigger and question:** An expensive query repeats. Which inputs actually change, and would the proposed cache key distinguish every relevant context?

**Consequences and support:** B: static/dynamic databases, physical-write backtracking undo, and worker read snapshots. S: incremental summaries, cache keys, invalidation, and parallel-safe helpers. Undo records database writes, including those made by `finally`/`next` followups, and restores them with recording disabled; this does not automatically restore arbitrary mutable helper objects. Active worker snapshots reject declared search-time dynamic-object registration. Assess memory and synchronization cost before assuming memoization helps.

**Evidence and assumptions:** [ww-support.lisp](../src/ww-support.lisp): `record-bt-undo`, `restore-bt-undo`, `fold-store`, `fold-remove`; [ww-backtracker.lisp](../src/ww-backtracker.lisp): `apply-choice-database-bt`; [ww-worker-read-snapshots.lisp](../src/ww-worker-read-snapshots.lisp): `validate-worker-read-snapshot-mode`; [ww-parallel-backtracker.lisp](../src/ww-parallel-backtracker.lisp): `run-bt-task`. **Proposed check:** change each future-relevant input in turn, then undo a choice and compare the summary with direct recomputation; audit shared mutable state before parallel use.

*Verified 2026-10-12.*

### F15 — Can time change the available choices?

**Trigger and question:** Events occur without a chosen action, or waiting changes access. Do identical arrangements at different times have different futures?

**Consequences and support:** B: happenings, action-state timing, explicit wait handling, and restricted auto-wait. S: relevant time/phase encoding and the intended event model. Backtracking errors when happenings are present. Graph+happenings emits an incompatibility diagnostic; the inspected initializer does not enforce it with an error. Auto-wait validation requires serial depth-first planning/tree mode. A: investigate a different temporal mechanism if these contracts do not represent the agreed rules; do not remove timing requirements to fit a mode.

**Evidence and assumptions:** [ww-initialize.lisp](../src/ww-initialize.lisp): `init`; [ww-validator.lisp](../src/ww-validator.lisp): `check-problem-parameter` auto-wait branch; [ww-planner.lisp](../src/ww-planner.lisp): `create-action-state`, `get-wait-happenings`. F04 remains the state-sufficiency obligation. **Proposed check:** compare the same delivery arrangement immediately before and after an access window; record the required behavior before choosing happenings or another representation.

*Verified 2026-10-12.*

### F16 — What evidence would change this design?

**Trigger and question:** The design is plausible but unmeasured, or a larger instance fails. Which claim is in doubt: fidelity, reduction correctness, implementation, or performance?

**Consequences and support:** B: candidate/prefix validation and search reporting. S: allowed/forbidden cases, independent small oracles, matched comparisons, and bounded experiments. Separate these evidence types; equal work counts do not establish equal answers, and a successful path does not prove every intended rule was modeled. A cutoff or interruption is evidence about the covered attempt only. Replay and validation tools: F21.

**Evidence and assumptions:** [ww-searcher.lisp](../src/ww-searcher.lisp): `search-prefix-pruned-p`, `count-accepted-goal`, `compute-effective-branching-factor`; F01 identifies the COUNT result to inspect. **Proposed check:** specify the question, instance family, exact settings, limits, metrics, acceptance criteria, and authorized scope before execution. Preserve useful checkpoints when an approved search stops; do not silently deepen or rerun it.

*Verified 2026-10-12.*

### F17 — Search hooks: what each does and when it runs

**Trigger and question:** A design relies on a search hook to order, prune, or bound. Under the chosen driver and settings, does the hook actually run where its soundness argument assumes?

**Consequences and support:** B: four reserved `define-query` names, each called with the state. S: the formula and its soundness or admissibility argument.

- `heuristic?` returns a real number; lower values are explored first. It orders successors in serial and parallel depth-first search. In backtracking it scores all sibling choices on a scratch copy of the state and explores the lowest first; ties keep generation order. It only orders, and it overrides `*randomize-search*`.
- `prune-state?` returns true to stop expanding that state. It removes descendants only.
- `min-steps-remaining?` returns a nonnegative lower bound on the moves still needed. A node is pruned when depth plus bound exceeds `*depth-cutoff*`, or when a MIN-LENGTH or FIRST solution exists and depth plus bound is at least its length. Without a cutoff or an incumbent solution it prunes nothing. Registered contributors (`*min-steps-remaining-contributors*`, for example from `topo-lower-bound` or a spec's `declare-relaxed-model`) are tried first. Serial search samples the aggregate query adaptively after a run of non-pruning evaluations; parallel search evaluates it at every node.
- `bounding-function?` returns `(values cost upper)` in minimizing terms. A node is killed when `cost` exceeds `*upper-bound*`; a smaller `upper` tightens the bound, by compare-and-swap under threads. The bound starts at 1,000,000 in every search.

Order of checks before a node is expanded:

| Driver | Order |
|---|---|
| Serial depth-first (`df-bnb1`) | depth cutoff → incumbent → lower bound → bounding → `prune-state?` (in `expand`) |
| Parallel depth-first, task splitting | depth cutoff → lower bound → bounding → `prune-state?` |
| Parallel depth-first, workers | depth cutoff → lower bound → bounding → cached incumbent → `prune-state?` |
| Backtracking, serial and parallel (`backtrack`; task generation runs the same function) | depth cutoff → `prune-state?` → lower bound → bounding |

Goal states are accepted when generated, before any of these checks, so no hook removes an accepted goal; the start state is checked too. A goal that a solution validator rejects remains an ordinary successor and may be pruned. Under threads every worker calls the hooks, so a hook must be a pure function of the state; any global it reads and writes is shared by all workers.

**Evidence and assumptions:** [ww-planner.lisp](../src/ww-planner.lisp): `expand`; [ww-searcher.lisp](../src/ww-searcher.lisp): `df-bnb1`, `min-steps-pruning-relevant-p`, `min-steps-remaining-bound-prunes-p`, `min-steps-remaining-prunes-node-p`, `bound-search-state`, `dfs`; [ww-parallel.lisp](../src/ww-parallel.lisp): `generate-root-tasks`, `worker-local-dfs`; [ww-backtracker.lisp](../src/ww-backtracker.lisp): `backtrack`, `ordered-choices-bt`; [ww-preliminaries.lisp](../src/ww-preliminaries.lisp): `minimize-global`. The hook contract is only the calling convention; a formula's soundness remains the spec's obligation (F09, F12). **Proposed check:** run every solution, or the optimum, with and without the hook at two cutoffs; the answers must agree, and the pruning counters show whether the hook ran.

*Verified 2026-10-12.*

### F18 — Passing a result from one search to the next

**Trigger and question:** A design runs two or more searches in sequence (placement then route, or milestone after milestone). What exactly carries over to the next search, and in which thread mode does each step run?

**Consequences and support:** B: two forms of `solve-subgoal`, `list-database`, and checkpoints. S: which facts make up the hand-over and where they are saved.

- `(solve-subgoal <goal>)` continues the active chain from the last solution; it requires `*threads*` 0.
- `(solve-subgoal <start> <goal>)` is one search, serial or parallel. The start is an unquoted literal fact list, or a form evaluating to a fact list, a state, or a checkpoint. It discards any active chain and saves one `ww-undo` point. A checkpoint start returns a new checkpoint on success and the unchanged checkpoint when the search finds nothing.
- The goal is never quoted; a quoted goal is an error.
- A fact list may contain dynamic facts only. It is laid onto the staged problem's static facts, and each fact is checked (`make-start-state-from-facts`).
- To carry a result across a reload, such as an `*algorithm*` change (F19), save it as symbolic facts with `(list-database (problem-state.idb <state>))` in a `defvar`, which keeps its value through a reload. Do not carry the state itself: its database keys are integer codes rebuilt at load. Bijective facts appear in their stored index form (F20) and are accepted back as listed.

**Evidence and assumptions:** [ww-goal-chaining.lisp](../src/ww-goal-chaining.lisp): `solve-subgoal`, `solve-subgoal-from-form`, `make-start-state-from-facts`, `validate-continuation-preconditions`, `ww-undo`; [ww-structures.lisp](../src/ww-structures.lisp): `list-database`; [ww-search-checkpoint.lisp](../src/ww-search-checkpoint.lisp): `capture-search-checkpoint`, `export-search-checkpoint`, `import-search-checkpoint`. The fact-list pattern was exercised in the captjohn-advisor consultation (2026-10-11/12). Whether solution lists survive a reload is not verified at runtime; do not rely on it. Shortest segments do not make a globally shortest path (F11). **Proposed check:** save the first search's endpoint as facts, reload, start the second search from them, and confirm that `list-database` on the rebuilt start matches the saved facts.

*Verified 2026-10-12.*

### F19 — Settings: where each can be set and what reloads the system

**Trigger and question:** A plan changes settings between searches or writes them into a spec. Does the change apply immediately, reload Wouldwork (recompiling the staged problem), or get refused?

**Consequences and support:** B: `ww-set` behavior falls into groups.

| Group | Settings |
|---|---|
| Applied immediately | `*depth-cutoff*`, `*solution-type*`, `*tree-or-graph*` (not GRAPH under backtracking), `*randomize-search*`, `*branch*`, `*auto-wait*`, `*progress-reporting-interval*`, the recorder settings, `*bt-cycle-check*`, the parallel tuning settings |
| Reload Wouldwork | `*algorithm*` (switching to backtracking also sets TREE), `*problem-type*`, `*symmetry-pruning*`, `*debug*`, `*probe*`, and `*threads*` when it crosses 0 (a change between positive counts does not reload) |
| Spec file only | `*problem-name*` |
| Error in a spec file | `*debug*`, `*probe*` |

An `*algorithm*` written in a spec applies on a fresh `stage` (as in `problem-queensN.lisp`). `stage` deletes `vals.lisp` and applies the spec's own values; `refresh` keeps the values set at the REPL; a plain reload restores the values saved at the last staging. Some settings are checked when set: for example `*auto-wait*` requires tree search, planning, serial execution, and depth-first.

**Evidence and assumptions:** [ww-set.lisp](../src/ww-set.lisp): `ww-set`; [ww-validator.lisp](../src/ww-validator.lisp): `check-problem-parameter`; [ww-interface.lisp](../src/ww-interface.lisp): `stage`, `refresh`; [ww-preliminaries.lisp](../src/ww-preliminaries.lisp): `read-init-vals`. Source reading only; the groups were not exercised one by one. A reload recompiles the problem, so values held in `defparameter` are reset while `defvar` values survive (F18). **Proposed check:** for a planned change between two searches, name its group and whether anything the second search needs must first be saved (F18).

*Verified 2026-10-12.*

### F20 — Symmetric and bijective relations as stored facts

**Trigger and question:** A relation pairs objects of the same type, or a one-to-one pairing must be readable from either side. What does the database actually store, and which facts appear when a state is listed or rebuilt from facts?

**Consequences and support:** B: storage and checks. S: choosing a directional, symmetric, or bijective declaration.

- A relation with two or more arguments of the same type is symmetric; fluent arguments count by their type. Unless its name ends in `>`, asserting one order also asserts every swapped order. A one-way relation (precedes, next in order) must end in `>`, or the swapped fact also holds and silently changes the rules.
- A `:bijective` relation is stored as two index relations named after it with `1` and `2` appended, each keyed by one side with the other side as its value. Asserting a new pairing retracts the stale reverse mapping. `list-database` and `display-validation-state` print the index facts (for example `LOCATED1` and `LOCATED2`), and a fact list built from that output is accepted back as listed (F18). Initialization rejects pairing one object with two partners, checking each index separately.
- Declaration syntax: [spec-advisor.md](spec-advisor.md) Section 4.4.

**Evidence and assumptions:** [ww-installer.lisp](../src/ww-installer.lisp): `create-bijective-indices`, `symmetric-type-indexes`, symmetric-relation installation (`final-charp #\>`); [ww-support.lisp](../src/ww-support.lisp): `add-proposition`; [ww-init-validator.lisp](../src/ww-init-validator.lisp): `check-init-duplicate-fluent-keys`. Both behaviors arose in the captjohn-advisor consultation (2026-10-11 and 2026-10-12). **Proposed check:** after staging, list the start state and confirm that each same-type relation shows only the intended orders and each bijective pairing appears once in each index.

*Verified 2026-10-12.*

### F21 — Replaying and validating a path

**Trigger and question:** A path exists: hand-written, found by search, or assembled from milestones. What does replaying it establish, and what does it leave unproven?

**Consequences and support:** B: replay and validators. S: problem-specific validators, where needed.

- `(validate-solution [:verbose] <action> ...)` replays from `*start-state*` and reports the first failing action with its reason. It returns the final state, the intermediate state if the goal is not reached, or NIL on a failed action.
- `validate-action-sequence` is the function form: a start state, an action list, and an optional `:goal-test`; it reports executability and goal satisfaction separately.
- Solution validators (`*solution-validators*`, for example the Talos recorder's) judge a complete candidate. Search applies them to every goal it finds (`candidate-solution-valid-p`); `report-solution-validator-verdicts` applies them to a replayed path. Search-prefix validators prune partial paths during search (`search-prefix-pruned-p`).
- `capture-search-checkpoint`, `export-search-checkpoint`, and `import-search-checkpoint` save and restore a milestone chain by replay; import runs no search, but the thread mode must be set first.
- A successful replay with the goal checked shows that the path is legal and reaches the goal under the model. It does not show that the model matches the intended problem, that the path is shortest, or that no other solution exists.

**Evidence and assumptions:** [ww-solution-validation.lisp](../src/ww-solution-validation.lisp): `validate-solution`, `validate-action-sequence`, `candidate-solution-valid-p`, `report-solution-validator-verdicts`; [ww-searcher.lisp](../src/ww-searcher.lisp): `search-prefix-pruned-p`; [ww-search-checkpoint.lisp](../src/ww-search-checkpoint.lisp): capture, export, import. The action-phrase format for hand-written Talos actions is in [solving-advisor.md](solving-advisor.md), *Constructing hand-written action sequences*. **Proposed check:** replay the path from a fresh staging against the real goal; require executability, goal satisfaction, and every registered validator's acceptance before calling it a solution.

*Verified 2026-10-12.*

### F22 — Ways Wouldwork can approach a problem

**Trigger and question:** Forward search from the start state looks too deep, too wide, or too costly. Which other approach fits the problem's structure, and what does each require and prove?

**Consequences and support:** B: entry points; S: extra spec logic or a second spec for each. Choosing an approach is a design decision (problem advisor), implementing it is spec work (spec advisor), measuring it belongs to the search advisor, and the solving advisor uses milestones.

| Approach | Fits when | Supply and entry points | What a result means | Example |
|---|---|---|---|---|
| Forward search (default) | A solution lies within the depth cutoff's reach | `solve`, depth-first or backtracking (F17, F19) | A path, or bounded exhaustion | most problems |
| Fixed-order assignment | One action per variable or group, in dependency order | `*problem-type*` CSP: action *n* runs at depth *n* (`generate-children`, `backtracking-actions`); F05 | Complete assignments | problem-captjohn.lisp |
| Backward search, then forward to meet | Goal states are explicit and every move reverses | a backward spec searched with EVERY, identical `encode-state` in both specs, `get-state-codes`, then a forward goal calling `backward-path-exists` | Forward path plus reversed backward path; replay it (F21) | problem-triangle-xyz-backward / -forward |
| Backward enumeration and meet-in-the-middle | Goals are described by base facts from which derived facts follow | `define-base-relation`, `define-goal-filter`, optional `state-feasible?`, `find-goal-states`, `find-predecessors`, `solve-meeting-point`; F13 | A meeting state with both halves; feasibility without a hook proves no reachability | end of problem-corner.lisp |
| Macro actions | The same few moves recur in solutions of small versions | combined actions placed before the base actions; candidates from `(freq 2 3)` after an EVERY search | Same answers if the base actions are kept; work per state rises | problem-triangle-xyz-macros, problem-corner-macro |
| Relaxation | Working out derived facts dominates the cost | a separate, weaker spec whose goal checks the true conditions last | A candidate to check against the original spec | problem-corner-relaxed |
| Goal chaining and milestones | One search cannot reach the goal, but natural resting states exist | `solve-subgoal` in both forms, checkpoints; F11, F18 | Each segment at best optimal; the whole path is not shown shortest | crelay-topo, captjohn-advisor |
| Named multi-phase strategies | A reusable milestone sequence applies under stated conditions | `define-strategy`, `solve-via-strategy`, `list-strategies`; the heuristic is disabled during its phases | As for goal chaining | none in probs/ |
| Randomized or single-branch runs | Probing a huge space for any solution, or splitting work by hand | `*randomize-search*` (ignored when `heuristic?` exists), `*branch*` | A failed run proves nothing | — |

Experimental and not offered: `ww-backward.lisp` (one-step regression, not integrated with search) and `ww-novelty.lisp` (retired, not loaded).

**Evidence and assumptions:** the symbols named in the table, in particular [ww-planner.lisp](../src/ww-planner.lisp): `generate-children`; [ww-support.lisp](../src/ww-support.lisp): `get-state-codes`, `backward-path-exists`; [ww-enumerator-run.lisp](../src/ww-enumerator-run.lisp): `find-goal-states`, `find-predecessors`, `solve-meeting-point`; [ww-enumerator-build.lisp](../src/ww-enumerator-build.lisp): `define-base-relation`, `define-goal-filter`; [ww-frequencies.lisp](../src/ww-frequencies.lisp): `freq`; [ww-advisor.lisp](../src/ww-advisor.lisp): `define-strategy`, `solve-via-strategy`. Entry points confirmed in source; none was run for this entry. Measured costs and lessons are in [search-advisor.md](search-advisor.md) Section 3. **Proposed check:** before committing to an approach, state the problem property it relies on (reversible moves, explicit goals, recurring patterns, milestones) and confirm it on a small instance.

*Verified 2026-10-12.*

## Compatibility review across the selected features

Current source includes parallel backtracking: [ww-searcher.lisp](../src/ww-searcher.lisp), `dfs`, dispatches to `process-partitioned-parallel-bt`; [ww-parallel-backtracker.lisp](../src/ww-parallel-backtracker.lisp), `process-partitioned-parallel-bt-body` and `generate-bt-tasks`, generate tasks serially before workers explore subtrees. These observations were source-checked on 2026-10-12; they are not runtime validation or a speedup claim. Do not assume backtracking requires serial execution.

Before recommending settings, reconcile answer identity (F01), state/history (F04), cycle treatment (F06), symmetry (F08), bounds (F12, F17), worker state (F14), and time (F15). Apply the local 16-thread baseline only after this review. ALL-PATHS special behavior, chained continuation, and auto-wait require serial operation under their contracts. BT PATH has its own exclusions, not a current serial-only guard. Declared dynamic-object registration is incompatible with active worker snapshots. Check deterministic choice generation and private helper state for parallel backtracking's ordinal-prefix replay.

Explain a required exception in terms of the user's answer or rule. If support remains uncertain, record a focused source-audit question or a separately proposed runtime check. Hook calling conditions are in F17, and which settings reload or are refused is in F19.

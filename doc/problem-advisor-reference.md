# Wouldwork problem advisor capability reference

Companion to the [operational advisor](problem-advisor.md). Includes source-derived examples; disclose relevant prior exposure when the user selects an evaluation problem. Consult only entries relevant to the agreed description.

## Selective feature-to-capability reference

Use entries only when the agreed description raises the corresponding issue. They are an index for reasoning, not an opening questionnaire. Each entry gives a trigger, a consequential question, design alternatives, support, assumptions, source evidence, and a proposed check. All source observations below were checked on **2026-10-08**. They establish source behavior only: no runtime checks, searches, or benchmarks were performed for this reference. Proposed checks require the appropriate later authorization.

The B/S/A labels above describe support, independently of evidence. The engine mappings below are source-checked to the stated extent; problem-specific recommendations remain deductions conditional on the stated assumptions. Possible augmentations are hypotheses, not claims that a facility is absent. Keep one authoritative contract here and refer to its feature ID when another entry depends on it.

### F01 — What counts as a different answer?

**Trigger and question:** Counting, enumeration, or interchangeable-looking answers. Must an answer identify an assignment, final situation, action sequence, or equivalence class?

**Consequences and support:** B: solution registration, COUNT, and restricted ALL-PATHS. S: unique construction and canonical answer rules. If only final situations matter, merging suitable states may be sound; if routes or labels matter, it may remove requested answers. COUNT increments accepted-goal encounters in `*solution-count*` and retains `*count-example*`, without populating the ordinary solution lists. It is not a general distinct-answer counter. Special ALL-PATHS requires serial depth-first graph search and a positive cutoff; otherwise it falls back to EVERY semantics. Reject that fallback as a design mismatch when path multiplicity is required. Its bounded engine path treatment does not promise all unrestricted walks with repeated cycles.

**Evidence and assumptions:** [ww-searcher.lisp](../src/ww-searcher.lisp): `count-accepted-goal`, `register-solution`, `initialize-hybrid-mode`; [ww-backtracker.lisp](../src/ww-backtracker.lisp): `register-solution-bt`. Enumeration correctness also depends on F06-F08 and candidate acceptance. **Proposed check:** compare two legal construction orders reaching the same answer and state explicitly whether the requested count is one or two.

### F02 — Which result is required?

**Trigger and question:** “Solve,” “best,” “all,” or “impossible” is underspecified. Is one acceptable answer enough, are ties required, or must the covered space be exhausted? What cost and units define best?

**Consequences and support:** B: FIRST, numeric solution limits, EVERY, COUNT, MIN-LENGTH, MIN-TIME, MIN-VALUE, and MAX-VALUE registration and stopping paths. S: objective, acceptance, tie handling, and coverage argument. Feasibility can stop early; optimality or absence needs the appropriate exhaustive scope and sound reductions. Do not assume an optimization mode retains all tied optima. A cutoff, interruption, or restricted generator limits an absence claim.

**Evidence and assumptions:** [ww-searcher.lisp](../src/ww-searcher.lisp): `solution-count-reached-p`, `register-solution`, `node-descendants-cannot-improve-p`; F01 defines counting and F12 defines bound assumptions. **Proposed check:** describe two equally good answers and agree whether returning only one meets the requirement.

### F03 — What grows, and what interacts?

**Trigger and question:** The user wants larger instances. Which object counts, domain sizes, dependencies, sequence lengths, or state fields grow?

**Consequences and support:** S: instance families, compact sufficient representation, and avoidance of unnecessary permutations. B: search statistics and parallel timing. A: investigate a different method or engine extension if a justified representation cannot meet the target. Track both number of choices and cost per choice; size alone does not predict difficulty.

**Evidence and assumptions:** [ww-searcher.lisp](../src/ww-searcher.lisp): `compute-average-branching-factor`, `compute-effective-branching-factor`; [ww-parallel-backtracker.lisp](../src/ww-parallel-backtracker.lisp): `process-partitioned-parallel-bt-body`. These counters and timings do not supply a scaling law. **Proposed check:** specify a bounded family varying one growth dimension, with time, allocation/memory, generated work, and accepted outcomes; execute only after approval.

### F04 — What must the situation remember?

**Trigger and question:** Two apparently identical arrangements arise by different routes or at different times. Can their legal futures, goals, or costs differ?

**Consequences and support:** B: state databases, duplicate handling, and history validation hooks. S: sufficient encoding of resources, history, phase, and obligations. If futures differ, retain the distinguishing information or avoid the unsound merge. If they do not, merging may save repeated work. A validator that inspects a path does not automatically make database-based merging safe.

**Evidence and assumptions:** [ww-searcher.lisp](../src/ww-searcher.lisp): `process-successors`, `on-current-path`, `search-prefix-pruned-p`; [ww-planner.lisp](../src/ww-planner.lisp): `get-new-states`. F06 covers cycle restrictions and F15 time. **Proposed check:** give two histories with the same proposed state and compare all relevant next choices and goal/cost consequences.

### F05 — Which choice should be generated next?

**Trigger and question:** Many candidate combinations fail immediately. Are choices assignments to remaining variables, or transitions with changing available actions?

**Consequences and support:** B: typed and query-dependent action domains, preconditions, and CSP action scheduling. S: variable selection, domain narrowing, and unique construction. Both inspected drivers select the action indexed by depth in CSP mode while depth is below the number of actions, then fall back to the action collection. CSP is not an unconditional general variable-ordering or propagation solver. A single repeated assignment action can maintain its own next-variable progression.

**Evidence and assumptions:** [ww-planner.lisp](../src/ww-planner.lisp): `generate-children`; [ww-backtracker.lisp](../src/ww-backtracker.lisp): `backtracking-actions`, `visit-generated-choices-bt`. A restricted generator must preserve requested answers. **Proposed check:** enumerate the intended choices at two consecutive partial assignments and compare them with the proposed action/domain schedule.

### F06 — Can situations recur?

**Trigger and question:** Actions can undo earlier work. Is progress permanent, and what proves finite exploration?

**Consequences and support:** B: depth-first graph/tree search, backtracking tree search, depth cutoff, and optional BT PATH checking. S: monotone progress or canonical construction. Backtracking and CSP are forced to tree mode at initialization. Ordinary backtracking inverse detection compares only the immediate previous choice; depth-first `on-current-path` scans ancestors. Optional BT PATH uses ancestor fingerprints to select exact database comparisons, but requires backtracking/planning/tree and excludes solution and prefix validators, goal-chain state/policies, happenings, auto-wait, recorder pruning/history, and static database writes. The current validator has no serial-only guard, and parallel workers initialize private path stacks; do not repeat the older plan's serial-only claim. This is source evidence, not a parallel PATH regression result.

**Evidence and assumptions:** [ww-initialize.lisp](../src/ww-initialize.lisp): `init`; [ww-backtracker.lisp](../src/ww-backtracker.lisp): `detect-path-cycle`, `validate-bt-path-mode`, `bt-on-current-path-p`; [ww-searcher.lisp](../src/ww-searcher.lisp): `on-current-path`; [ww-support.lisp](../src/ww-support.lisp): `reject-bt-path-static-write`; [ww-parallel-backtracker.lisp](../src/ww-parallel-backtracker.lisp): `run-bt-task`. Database-only recurrence must be semantically sound (F04). **Proposed check:** a three-move cycle distinguishes ancestor checking from immediate-inverse detection; state the cutoff's coverage separately.

### F07 — Does the order of independent choices matter?

**Trigger and question:** Several orderings appear to produce the same result. Does swapping adjacent choices preserve intermediate legality, cost, futures, and answer identity?

**Consequences and support:** B: graph duplicate handling. S: fixed construction order or ordering constraints can remove redundant permutations when the swap is sound. If order is part of an answer or changes a prerequisite, preserve it. A: investigate general partial-order reduction only if the problem needs more than spec logic.

**Evidence and assumptions:** [ww-searcher.lisp](../src/ww-searcher.lisp): `process-successors`; [ww-backtracker.lisp](../src/ww-backtracker.lisp): `backtracking-actions`. These mechanisms do not prove commutativity. **Proposed check:** compare both orders of two choices, including the intermediate state and any consumed resource; apply F01 before discarding either route.

### F08 — Which identities are interchangeable?

**Trigger and question:** Objects or spatial arrangements look equivalent. Does exchanging them preserve every rule, goal, cost, and requested distinction?

**Consequences and support:** B: object symmetry detection, instantiation filtering, and graph canonical symmetry hashing. S: problem-specific spatial canonicalization and counting conventions. Current detection considers static row symmetries and explicit transition, goal, and happening object references. This does not establish that arbitrary helper logic or user answer identity is invariant. Spatial rotations/reflections are separate from object interchangeability.

**Evidence and assumptions:** [ww-symmetry.lisp](../src/ww-symmetry.lisp): `detect-symmetry-groups`, `filter-symmetric-instantiations`, `use-canonical-symmetry-p`; [problem-queensN.lisp](../probs/problem-queensN.lisp): `queens-canonical-board-p`, `*queens-count-classes*`. The queens example has its own optional board-class rule. **Proposed check:** swap a candidate pair in a rule, goal, and answer; for rotations, separately test the intended class convention. Reopen F01 if labels become significant.

### F09 — What can already rule out a choice?

**Trigger and question:** A partial choice appears impossible, or seems to force a consequence. Is that conclusion necessary under every legal completion?

**Consequences and support:** B: preconditions, inconsistent-update/state handling, and `prune-state?`. S: contradiction tests and justified forced updates; A: investigate stronger propagation if necessary. Reject a branch only from a sound impossibility argument. In the inspected depth-first and backtracking expansion paths, `prune-state?` suppresses descendants; it does not reject a goal already accepted by the caller. Put goal acceptance requirements in the appropriate goal/validation logic.

**Evidence and assumptions:** [ww-planner.lisp](../src/ww-planner.lisp): `expand`, `update-is-inconsistent`, `state-is-inconsistent`; [ww-backtracker.lisp](../src/ww-backtracker.lisp): `backtrack`, `accept-goal-bt`. Selecting CSP alone promises no general propagation strength. **Proposed check:** construct a partial state just inside and just outside the proposed contradiction condition, including a state that already satisfies the goal.

### F10 — Which resources must remain available?

**Trigger and question:** A scarce item, capacity, or commitment constrains later work. Is its use temporary, recoverable, or permanent, and can one resource satisfy several demands together?

**Consequences and support:** B: state updates and the pruning/bound hooks of F09/F12. S: balances, reservations, obligations, and admissible resource bounds. Permanent loss can provide monotone progress or impossibility tests; reusable equipment needs continuation reasoning. A failed proposed allocation is not proof that every allocation fails. Do not double-count demands that can share a resource.

**Evidence and assumptions:** [ww-planner.lisp](../src/ww-planner.lisp): `get-new-states`; [problem-knap19.lisp](../probs/problem-knap19.lisp): `bounding-function?` gives a problem-specific capacity example, not a universal resource model. **Proposed check:** compare two remaining obligations that can share one item with two that require it simultaneously.

### F11 — Can parts be solved independently?

**Trigger and question:** A proposed milestone or decomposition simplifies the task. What resources and continuation options must pass across the boundary?

**Consequences and support:** B: goal-chain continuation, recovery, and one-off subgoal search. S: interface contracts and milestone selection. Chained continuation requires serial execution. The explicit start/goal form supports either thread mode, but discards an active chain and saves undo state; it is not interchangeable with chained continuation. A: investigate specialized decomposition when the available interfaces do not suffice. Sequential milestone success does not establish global optimality.

**Evidence and assumptions:** [ww-goal-chaining.lisp](../src/ww-goal-chaining.lisp): `validate-continuation-preconditions`, `run-goal-chain-request`, `solve-subgoal-from-form`. **Proposed check:** compare two states satisfying the same milestone where only one preserves the resource needed next; refine the contract before treating the milestone as sufficient.

### F12 — Is an estimate safe for elimination?

**Trigger and question:** A score suggests promising choices or an unreachable target. Is it an ordering estimate, a proved optimistic bound, or the value of a feasible completion?

**Consequences and support:** B: `heuristic?`, `min-steps-remaining?`, and `bounding-function?`; S: formulas, admissibility, and evaluation-cost decisions. Ordering estimates need not be proofs; elimination bounds do. `bound-search-state` kills descendants when the first returned minimizing-form cost exceeds `*upper-bound*`, and tightens that bound with the second value when smaller. For maximization, the inspected knapsack example negates values into that convention. The second value needs a valid feasible-completion argument. Backtracking calls user and move bounds but lacks automatic objective-bound pruning. Depth-first automatic MIN-VALUE pruning assumes nondecreasing cost; a low-value MAX-VALUE non-goal does not bound its descendants.

**Evidence and assumptions:** [ww-searcher.lisp](../src/ww-searcher.lisp): `bound-search-state`, `node-descendants-cannot-improve-p`, `min-steps-remaining-bound-prunes-p`; [ww-backtracker.lisp](../src/ww-backtracker.lisp): `backtrack`, `ordered-choices-bt`; [problem-knap19.lisp](../probs/problem-knap19.lisp): `bounding-function?`. Move bounds are nonnegative lower bounds, compared with depth cutoff and applicable incumbent depth; driver and objective contracts must fit the use. **Proposed check:** on independently understood tiny cases, compare each proposed bound with actual completions, including a negative-cost step or growing reward that would invalidate a monotonicity assumption.

### F13 — Would working from endings help?

**Trigger and question:** Goal arrangements may be few or easier to describe than routes. Is generating endings cheaper, and can predecessor relationships be justified?

**Consequences and support:** B: base-relation schemas, goal enumeration, predecessor entry points, and meeting-point search. S: schemas, filters, feasibility logic, state keys, and witnesses. A: investigate unsupported inverse/domain requirements only after a focused audit. A satisfying ending is not necessarily reachable; enumerating all endings may itself be too large. The feasibility helper accepts a consistent state when no custom hook exists, which supplies no reachability proof.

**Evidence and assumptions:** [ww-enumerator-build.lisp](../src/ww-enumerator-build.lisp): `define-base-relation`; [ww-enumerator-run.lisp](../src/ww-enumerator-run.lisp): `find-goal-states-fn`, `find-predecessors-fn`, `fps-state-feasible-p`, `solve-meeting-point-fn`. Inspection confirms these entry points and the stated feasibility behavior, not universal inverse support or exhaustive reachability. **Proposed check:** audit a selected domain's schema, predecessor generation, key equivalence, and forward witness before recommending the method concretely.

### F14 — What can be maintained instead of recomputed?

**Trigger and question:** An expensive query repeats. Which inputs actually change, and would the proposed cache key distinguish every relevant context?

**Consequences and support:** B: static/dynamic databases, physical-write backtracking undo, and worker read snapshots. S: incremental summaries, cache keys, invalidation, and parallel-safe helpers. Undo records database writes and restores them with recording disabled; this does not automatically restore arbitrary mutable helper objects. Active worker snapshots reject declared search-time dynamic-object registration. Assess memory and synchronization cost before assuming memoization helps.

**Evidence and assumptions:** [ww-support.lisp](../src/ww-support.lisp): `record-bt-undo`, `restore-bt-undo`, `fold-store`, `fold-remove`; [ww-worker-read-snapshots.lisp](../src/ww-worker-read-snapshots.lisp): `validate-worker-read-snapshot-mode`; [ww-parallel-backtracker.lisp](../src/ww-parallel-backtracker.lisp): `run-bt-task`. **Proposed check:** change each future-relevant input in turn, then undo a choice and compare the summary with direct recomputation; audit shared mutable state before parallel use.

### F15 — Can time change the available choices?

**Trigger and question:** Events occur without a chosen action, or waiting changes access. Do identical arrangements at different times have different futures?

**Consequences and support:** B: happenings, action-state timing, explicit wait handling, and restricted auto-wait. S: relevant time/phase encoding and the intended event model. Backtracking errors when happenings are present. Graph+happenings emits an incompatibility diagnostic; the inspected initializer does not enforce it with an error. Auto-wait validation requires serial depth-first planning/tree mode. A: investigate a different temporal mechanism if these contracts do not represent the agreed rules; do not remove timing requirements to fit a mode.

**Evidence and assumptions:** [ww-initialize.lisp](../src/ww-initialize.lisp): `init`; [ww-validator.lisp](../src/ww-validator.lisp): `check-problem-parameter` auto-wait branch; [ww-planner.lisp](../src/ww-planner.lisp): `create-action-state`, `get-wait-happenings`. F04 remains the state-sufficiency obligation. **Proposed check:** compare the same delivery arrangement immediately before and after an access window; record the required behavior before choosing happenings or another representation.

### F16 — What evidence would change this design?

**Trigger and question:** The design is plausible but unmeasured, or a larger instance fails. Which claim is in doubt: fidelity, reduction correctness, implementation, or performance?

**Consequences and support:** B: candidate/prefix validation and search reporting. S: allowed/forbidden cases, independent small oracles, matched comparisons, and bounded experiments. Separate these evidence types; equal work counts do not establish equal answers, and a successful path does not prove every intended rule was modeled. A cutoff or interruption is evidence about the covered attempt only.

**Evidence and assumptions:** [ww-searcher.lisp](../src/ww-searcher.lisp): `search-prefix-pruned-p`, `count-accepted-goal`, `compute-effective-branching-factor`; F01 identifies the COUNT result to inspect. **Proposed check:** specify the question, instance family, exact settings, limits, metrics, acceptance criteria, and authorized scope before execution. Preserve useful checkpoints when an approved search stops; do not silently deepen or rerun it.

### Compatibility review across the selected features

Current source includes parallel backtracking: [ww-searcher.lisp](../src/ww-searcher.lisp), `dfs`, dispatches to `process-partitioned-parallel-bt`; [ww-parallel-backtracker.lisp](../src/ww-parallel-backtracker.lisp), `process-partitioned-parallel-bt-body` and `generate-bt-tasks`, generate tasks serially before workers explore subtrees. These observations were source-checked on 2026-10-08; they are not runtime validation or a speedup claim. Recheck them when used in a later consultation. Do not assume backtracking requires serial execution.

Before recommending settings, reconcile answer identity (F01), state/history (F04), cycle treatment (F06), symmetry (F08), bounds (F12), worker state (F14), and time (F15). Apply the local 16-thread baseline only after this review. ALL-PATHS special behavior, chained continuation, and auto-wait require serial operation under their contracts. BT PATH has its own exclusions, not a current serial-only guard. Declared dynamic-object registration is incompatible with active worker snapshots. Check deterministic choice generation and private helper state for parallel backtracking's ordinal-prefix replay.

Explain a required exception in terms of the user's answer or rule. If support remains uncertain, record a focused source-audit question or a separately proposed runtime check. Existing [search-advisor.md](search-advisor.md) may contain older compatibility claims; use the verified contracts here and current source instead of copying them. Actual dialogue validation and integration changes remain deferred.

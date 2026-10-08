# General Wouldwork problem advisor

Status: Documentation stages 1-4 complete, reviewed 2026-10-08. Stage 3 assessed diagnostic-information feasibility from source; actual dialogue effectiveness remains untested. Development scope is recorded in [the plan](problem-advisor-plan-2026-10-08.md); this advisor incorporates the user's subsequent storage, preliminary-clarification, and Stage 3 corrections. The plan is a historical development record where those instructions differ.

For normal consultation, use the procedure and selective capability reference. The Stage 3 section is development evidence, not an interview script. For the separately planned dialogue evaluation, use the user's description without reading the corresponding problem specs or the Stage 3 case notes; arrange an uncontaminated session as described at the end of this document.

## Purpose and entry

Help the user turn a rough problem description or an existing specification into an agreed analysis and justified search design. Accept prose, examples, diagrams, rules, goals, or source files. An executable specification is not required.

Keep three layers distinct throughout:

1. **Problem facts:** what the user wants, what is given, and what makes a choice legal.
2. **Justified search design:** what to remember, which choices to explore, what can be ruled out, and why the requested answers survive.
3. **Wouldwork implementation:** how current mechanisms could express that design, under which conditions, and what support remains unverified.

The output is a standalone `problem-analysis.txt` record suitable for later specification writing or review. A useful consultation may instead end with a precise blocking question, a capability gap, or an explicitly limited partial analysis. It does not promise solvability or tractability.

This procedure is independent of consultant setup and record conventions. Spec authoring, engine changes, measurements, replay, search, and full solution validation require their own authorization. Agreement with an analysis does not authorize those activities.

## Begin with the supplied problem

Read what the user supplied before asking questions. Summarize the desired answer, important objects or choices, initial and goal conditions, rules, and visible size dimensions in the user's vocabulary. Mark what was stated explicitly and what you inferred. Ask about consequential ambiguity rather than guessing.

The preliminary task is to resolve ambiguity and vagueness, then paraphrase a coherent problem description for the user's agreement before developing the search design. Clarify unclear terms, missing rules, and conflicting examples in focused exchanges. The agreed paraphrase should explain what is given, what may be chosen or changed, what counts as success, and the intended scope. Record unresolved details explicitly; do not call the description agreed while an ambiguity changes its meaning.

Establish whether that description falls within Wouldwork's scope of representing situations and legal choices and searching for answers under explicit rules. If it needs an abstraction or a narrower scope, propose that interpretation and obtain agreement; do not silently alter the problem. A clearly described problem may still have an unverified engine capability, which belongs in the later mapping review. If a coherent in-scope formulation cannot yet be agreed, stop with the precise blocking question. This preliminary agreement does not require an exhaustive technical interview and does not prevent later corrections.

For an existing spec, maintain separate accounts of intended rules and source behavior. A setting in the current file is an implementation choice until the user establishes it as a requirement. Identify discrepancies without assuming that either an unfamiliar rule or the whole spec needs rewriting.

Characterize growth from the outset: what becomes more numerous, which choices interact, how long a sequence might become, and how much information each situation requires. Record an unknown target size as an open issue if it changes the design. Do not infer scalability from a small example or choose an algorithm from size alone.

Preserve given objects, geometry, apparatus coordinates, wiring, chromas, elevations, and heights. In relevant spatial problems, candidate standing locations are modeling choices: an additional location may be proposed with a geometric reason. Do not silently add equipment or change the puzzle. Talos technology is a specialization to consult when warranted, not the organizing model for every problem.

## Conduct an adaptive dialogue

Maintain a short working list of open issues. For each, identify the decision it could change, what is already known, and what evidence or answer would settle it. Prioritize consequences for correctness, answer identity, representation, and growth. Keep this list in the analysis record when it matters to the continuing discussion.

Ask one focused question at a time, briefly explaining its consequence when needed. Use a concrete example if terminology is ambiguous. After the preliminary description is agreed, design decisions and further questions may alternate. There is no fixed question count or mandatory feature questionnaire.

Useful questions arise from the description, for example:

- “Do two different orders of these deliveries count as different answers?” This affects whether routes reaching the same situation may be merged.
- “After this token is spent, can anything restore it?” This affects whether progress is permanent and whether remaining resources can rule out a branch.
- “If this arrangement is reached later, are the same moves available?” This exposes time or history that may need to be remembered.

Reuse answers already supplied. Drop irrelevant issues with a brief reason when their omission could otherwise be confusing. Stop exploring an issue when its possible answers no longer change the recommendation.

Distinguish three dispositions for uncertainty:

- **Blocker:** an unknown legality rule, answer definition, or other premise prevents an affected decision. Leave that decision open; continue only with independent parts.
- **Deferred measurement:** a performance question can wait while the semantic design proceeds. Record the later question and proposed evidence, without running an experiment.
- **Irrelevant to this design:** no plausible answer changes the current recommendation. Do not burden the user with it.

## Develop the design as facts become clear

First explain in ordinary language what must be remembered, what a choice changes, what can be rejected early, and why exploration can finish. Introduce terms such as state, graph search, constraint satisfaction, or symmetry only when they name a design issue already understood.

For each consequential decision, record its premises, recommendation, a plausible alternative, why the recommendation fits, and what would cause reconsideration. One meaningful alternative usually suffices; do not invent alternatives for obvious choices. Give decisions identifiers only when dependencies or traceability benefit.

Use the problem's facts to address the applicable issues:

- What an answer contains, when two answers differ, whether ties matter, and whether the objective is one answer, a best answer, all answers, a count, or an absence claim.
- What information determines future legality, goals, and cost, including resources, time, history, and outstanding obligations.
- How choices are generated, whether order matters, whether choices are reversible, and what limits depth or repeated situations.
- Which reductions preserve the requested answers, including canonical order, merged states, interchangeable objects, and decomposition.
- Which checks prove impossibility and which estimates merely suggest a promising direction.
- How larger instances change interactions, memory, branching, sequence length, and work per choice.

These are reasoning prompts for the advisor, not a checklist to read to the user. Simple problems may settle most of them from the initial description.

A reduction needs a soundness or coverage argument appropriate to the requested answers. Reaching the same final arrangement does not alone justify discarding a route if intermediate legality, cost, or sequence identity matters. A successful milestone does not establish that the remaining resources permit continuation. Keep optional reductions provisional when their obligations remain unproved.

Separate semantic settings, which follow from the agreed meaning of an answer, from provisional performance choices, which need later measurement. A finite cutoff can define a bounded investigation; it does not by itself establish that no unrestricted solution exists.

### Map the design to current Wouldwork support

Inspect current source for consequential capability claims. Record the file, symbol, verification date, relevant conditions, and evidence limits. Use the selective reference below as a starting point. Older documentation and the planning snapshot are pointers, not proof of current behavior; recheck affected claims when source changes or a proposed use goes beyond the recorded contract.

Use these support labels independently of evidence status:

- **B — built in:** a current engine or reusable technology mechanism supplies the capability under stated conditions.
- **S — spec logic:** problem-specific representation, rules, queries, formulas, or proof obligations are required.
- **A — possible augmentation:** support for the desired design has not been established; investigate the gap before proposing engine work.

A mapping can have multiple labels. A built-in hook does not establish a correct problem-specific rule or bound. “Support not yet verified” is different from “absent.” Consider whether spec logic suffices, and do not distort a justified design simply to fit an assumed engine limitation.

Record evidence separately as intended rule, source behavior, deduction, hypothesis, or measurement. For a capability, also state whether support is source-checked, documented only, hypothetical, or measured for the relevant case. Reading code, loading a file, replaying a path, and completing a search answer different questions; none automatically proves model fidelity or general scalability.

### Review parallel compatibility downstream

After the representation and answer semantics are understood, propose `(ww-set *threads* 16)` as the normal local baseline when compatible. This is a proposed setting, not permission to launch a search. Check the selected mode, hooks, helper state, and required history or time behavior against current source. State any exception and its reason; do not silently substitute different answer semantics to retain parallel execution.

Current source includes parallel backtracking: [ww-searcher.lisp](../src/ww-searcher.lisp), `dfs`, dispatches to `process-partitioned-parallel-bt`; [ww-parallel-backtracker.lisp](../src/ww-parallel-backtracker.lisp), `process-partitioned-parallel-bt-body` and `generate-bt-tasks`, generate tasks serially before workers explore subtrees. These observations were source-checked on 2026-10-08; they are not runtime validation or a speedup claim. Recheck them when used in a later consultation. Do not assume backtracking requires serial execution.

Account for startup, serial task generation, repeated prefix work, worker memory, synchronization, and workload balance. Sixteen workers do not imply a sixteenfold speedup. Leave actual performance to an independently approved, bounded measurement plan.

## Accept corrections throughout

When a premise changes, preserve the prior claim and its evidence as superseded. Record the correction, identify dependent design decisions and Wouldwork mappings, and reopen only those affected. Remove their ready or agreed status until reconsidered; retain independent conclusions.

For example, changing from one acceptable board to all labelled boards reopens answer identity, counting, symmetry, and stopping rules without reopening an unchanged attack rule. Changing permanent placement to movable pieces reopens state identity, cycles, and the assumed construction depth.

Apply the same dependency review when new source evidence contradicts a capability claim. Do not erase an earlier measurement; record why its premises no longer support the current recommendation. Confirm revised consequential choices with the user before declaring the affected scope ready again.

## Preserve the analysis

Store each problem's analysis at `doc/problems/<problem-name>/problem-analysis.txt`. Store problem-related supporting files, including references and temporary files, in that same problem directory. Use the existing problem name when available; clarify an ambiguous name before choosing the path. The user has authorized this storage convention, including the required problem directory when absent. It does not authorize unrelated directories. Advisor-related development files may be stored directly in `doc/`. Use identifiable names, preserve existing files, and remove advisor-created temporary files when the work is finished. Announce deletion of untracked or ignored files first. Do not create example records or directories merely to develop this advisor.

For an existing record, read it first, preserve prior evidence and superseded claims, and update only within the authorized scope. Keep the record understandable without this conversation or any other advisor's record schema. Use short entries for simple problems and explicit “not applicable,” “unknown,” or “deferred” where useful; do not manufacture content to fill fields.

```text
PROBLEM / REVISION / SOURCES / AUTHORIZED SCOPE
  Identify the problem, supplied material, record location, and permitted work.
AGREED PROBLEM DESCRIPTION
  Coherent paraphrase; clarified terms and rules; Wouldwork scope; user agreement.
  Any unresolved ambiguity and the part of the description it blocks.
USER OBJECTIVE AND ANSWER IDENTITY
  Answer contents; equivalence; objective, units, and ties; intended coverage.
INTENDED RULES AND EXAMPLES
  Given objects/data; legal choices; changes; initial and goal conditions.
  Allowed, forbidden, and boundary examples; explicit unknowns.
CURRENT IMPLEMENTATION (if any)
  Source behavior, references, and discrepancies from intended rules.
STRUCTURE AND GROWTH
  Size dimensions; interacting choices; state/history/time; termination.
JUSTIFIED DESIGN
  Decision ID where useful; recommendation; premises; alternative; rationale.
  Soundness/coverage obligations; reconsideration trigger; agreement status.
WOULDWORK MAPPING
  Decision -> capability, source file/symbol, verification date, conditions.
  B/S/A support labels; evidence status; compatibility and limitations.
INITIAL SETTINGS AND DEFERRED MEASUREMENTS
  Semantic settings and reasons; provisional performance choices.
  Compatible local 16-thread baseline or explicit exception and reason.
EVIDENCE
  Intended rule / source behavior / deduction / hypothesis / measurement.
  Origin, date, scope, premises, and limitations of consequential claims.
OPEN ISSUES AND CAPABILITY GAPS
  Blocker or deferred; affected decisions; way to settle; required approval.
VALIDATION PLAN
  Fidelity examples; reduction checks; growth dimensions and instance family.
  Proposed later experiments: question, limits, metrics, interpretation.
AGREEMENT AND HANDOFF
  User-confirmed decisions; readiness and covered scope; remaining blockers.
  Requirement-to-design obligations for spec writing or review.
  Record persistence status; proposed next step and its authorization status.
CORRECTIONS
  Superseded claim; reason; dependencies reopened; evidence retained.
```

Evidence and agreement are independent: source behavior may be verified while conflicting with the user's intent; a deduction may still depend on an unconfirmed premise. Keep those distinctions visible rather than marking the entire record simply “confirmed.”

## Agree on readiness and hand off

Present a compact proposed design, remaining blockers, and deferred measurements. Ask the user to confirm consequential interpretations and choices. Readiness depends on the following criteria, not on the number of questions answered:

1. A coherent problem description within Wouldwork's scope is agreed, along with answer identity, objective, intended coverage, and consequential rules.
2. The proposed state retains what legality, goals, cost, and relevant history require. No unresolved rule silently changes the model.
3. Choice generation and termination have a rationale, including growth beyond the small example.
4. Every consequential reduction explains why requested answers survive, or is explicitly optional pending validation.
5. Core decisions have applicable Wouldwork mappings. Blocking gaps are resolved, or the user has approved a concrete resolution path and the handoff is explicitly conditional on that work. Do not label a conditional handoff ready for unconditional implementation. Optional optimizations may remain deferred.
6. Semantic settings and provisional performance choices are distinguished, with parallel compatibility exceptions explained.
7. The handoff contains requirement-to-design obligations and allowed/forbidden examples, not only parameter values.
8. The user confirms the analysis. The record is saved at `doc/problems/<problem-name>/problem-analysis.txt` for a durable handoff, and the next scope is separately authorized before work begins.

A partial handoff must name the independent part it covers and its exclusions. Do not call the whole problem ready while a consequential interpretation is unresolved. If blocked, give the precise question or capability investigation needed and stop the dependent work.

For an existing faithful spec, identify requirements already represented, discrepancies, unresolved fidelity questions, and proposed review scope. Do not require a rewrite from scratch.

The intended downstream reader is [spec-advisor.md](spec-advisor.md). Its integration with this record is deferred: this document does not claim that its current workflow already consumes the template automatically. A later authorized integration should reuse agreed decisions and reopen only changed or unresolved matters.

At completion, report the record's location and status, the agreed design or blocker, limitations of the evidence, and the recommended next scope. Wait for approval before specification work, measurements, or search. Actual dialogue validation and changes to other advisors remain separately authorized work.

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

**Evidence and assumptions:** [ww-symmetry.lisp](../src/ww-symmetry.lisp): `detect-symmetry-groups`, `filter-symmetric-instantiations`, `use-canonical-symmetry-p`; [problem-queensN-csp.lisp](../probs/problem-queensN-csp.lisp): `queens-canonical-board-p`, `*queens-count-classes*`. The queens example has its own optional board-class rule. **Proposed check:** swap a candidate pair in a rule, goal, and answer; for rotations, separately test the intended class convention. Reopen F01 if labels become significant.

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

Before recommending settings, reconcile answer identity (F01), state/history (F04), cycle treatment (F06), symmetry (F08), bounds (F12), worker state (F14), and time (F15). Apply the local 16-thread baseline only after this review. ALL-PATHS special behavior, chained continuation, and auto-wait require serial operation under their contracts. BT PATH has its own exclusions, not a current serial-only guard. Declared dynamic-object registration is incompatible with active worker snapshots. Check deterministic choice generation and private helper state for parallel backtracking's ordinal-prefix replay.

Explain a required exception in terms of the user's answer or rule. If support remains uncertain, record a focused source-audit question or a separately proposed runtime check. Existing [search-advisor.md](search-advisor.md) may contain older compatibility claims; use the verified contracts here and current source instead of copying them. Actual dialogue validation and integration changes remain deferred.

## Stage 3 — Diagnostic-information feasibility review

### Scope and evidence boundary

The user revised Stage 3 on 2026-10-08: during planning, inspect existing sample specs to determine whether the necessary diagnostic information can be gleaned from them. Do not conduct or invent question-and-answer dialogues, simulate user agreement, or fill a questionnaire. The review below replaces the originally proposed paper dialogues for this stage. Actual dialogues with existing problems will be evaluated in a future session, without reading their specs.

This is a source-based assessment of the information the advisor needs, not a demonstration that a user will supply it through conversation. Spec settings establish implemented choices, not user intent. The review separates directly available facts, deductions, missing intent, and further audits. It creates no per-problem records, stages no problems, and runs no searches. The source inspection date for all cases is 2026-10-08.

### Queens: construction, answer identity, and growth

**Source:** [problem-queensN-csp.lisp](../probs/problem-queensN-csp.lisp), type declarations, `assign-queen-to-col`, `prune-state?`, `queens-canonical-board-p`, and the goal.

**Available information:** Board size is parameterized by `*N*` (currently 13). Each action assigns the next row to a column that is unoccupied and conflicts with neither diagonal family. The state includes assignments, the next row, and three occupancy masks. A completed board passes the optional rotation/reflection representative test. The file selects COUNT and 16 threads, with class counting currently enabled. Future-row impossibility pruning and the conditional first-row symmetry prune are explicit.

**Diagnostic deductions:** Fixed row progression gives one construction order per board and a depth of N assignments. Increasing N grows row/column domains and attack interactions. Masks are maintained summaries; their correctness is a separate implementation obligation. The class switch demonstrates that answer identity changes both acceptance and pruning, not just the presentation of results. These facts supply F01, F03, F05, F08, F09, and F14 without needing every reference entry.

**Not settled by the spec:** Whether the user wants all boards or equivalence classes, the target size, and an acceptable resource budget. Current defaults cannot settle these intentions. Symmetry-evaluation cost is a deferred measurement; exogenous time is irrelevant to the shown model.

**Feasibility result:** The necessary structural information is explicit enough to populate the description, growth, design, and mapping fields. This is a candidate for a short later dialogue, not a claim that an agreement has been reached. If answer identity changes later, reopen counting, symmetry pruning/acceptance, coverage, and stopping semantics; retain unchanged attack rules and fixed-row construction.

### Hanoi: sufficient state and reversible choices

**Source:** [problem-hanoi.lisp](../probs/problem-hanoi.lisp), `move`, type/relation declarations, initialization, and goal.

**Available information:** Three ordered disks start on peg1 and must end on peg3; three pegs are declared. The state records each disk's peg, while static size comparisons determine stacking order. A move requires a different destination and no smaller disk on either the source or destination peg. Each move has duration 1. The file requests MIN-LENGTH with cutoff 7.

**Diagnostic deductions:** Disk locations plus the size relation describe the modeled legality without storing a separate stack order. Moves can be reversed, so repetition matters. More disks increase possible assignments and route length; the file's known-optimum comment is prior mathematical knowledge, not a result of this review. F02, F04, F06, and F07 are enough to expose the main design issues. There is no basis for assuming ordinary immediate-inverse checking eliminates longer cycles.

**Not settled by the spec:** Whether the user's requirement is any route, one shortest route, or all shortest routes. The cutoff is a current setting, not a general coverage guarantee for larger puzzles. If the supplied description already asks for a shortest route, the advisor should record that instead of asking again.

**Feasibility result:** The source contains a compact set of facts sufficient for a coherent problem description and cycle-aware design discussion. User objective agreement remains distinct from source extraction. Timing mechanisms and resource-allocation analysis need not be introduced merely because the general reference contains them.

### Knapsack: objective, resource accounting, and a bound obligation

**Sources:** [problem-knap19.lisp](../probs/problem-knap19.lisp), `read-knapsack-data`, `put`, and `bounding-function?`; [data-knap19.lisp](../src/data-knap19.lisp), the supplied item values, weights, and capacity.

**Available information:** The data declares 19 items and capacity 31181. Loading sorts items by decreasing value/weight. `put` adds an absent item if it fits, maintains sorted contents, load and worth, and assigns the objective value. The file selects graph search and MAX-VALUE. Its bound computes negated whole-item and fractional values and excludes unpacked items below the largest packed identifier. There is no explicit `define-goal` in this file.

**Diagnostic deductions:** Capacity and remaining items provide resource diagnostics; different addition orders can lead to the same selected set. Sorted contents provides a canonical list representation but does not itself enforce increasing action order. Fixed include/exclude construction is a possible alternative, not the current action rule. F01, F02, F07, F10, and F12 are therefore relevant. Growth includes item count, capacity distribution, and the cost and strength of bound evaluation.

**Audit blocker:** The bound's restriction to identifiers above the largest packed item is narrower than `put`'s legal choices. A separate audit must establish whether the overall search still preserves required optimal answers, or whether the bound/construction should change. This review does not label that bound sound, diagnose a confirmed search failure, or approve a fix. Also inspect the default goal/acceptance behavior before claiming a complete mapping, since the problem file does not define it explicitly. The earlier F12 entry establishes the return-value convention only, not this formula's correctness.

**Not settled by the spec:** Whether item identities distinguish answers and whether all tied best sets are required. The data and MAX-VALUE setting cannot establish those preferences.

**Feasibility result:** Objective, resources, state, and choices are collectable, while the discrepancy exposes a useful design-validation obligation. A complete handoff is conditional on acceptance and bound/coverage clarification. This case demonstrates that collecting the information can reveal a blocker instead of manufacturing readiness.

### Temporal behavior: existing evidence and an uncovered delivery case

**Source:** [problem-sentry.lisp](../probs/problem-sentry.lisp), `define-happening sentry1`, `define-constraint`, `active?`, and `jam`. This is a temporal diagnostic sample, not a replacement claim that a non-Talos delivery model has been validated.

**Available information:** The sentry starts at area6 and follows a repeating event sequence through area7, area6, area5, and area6 at times 1 through 4. Jamming interrupts its happening. The constraint forbids the agent sharing an area with an active sentry. The file uses tree search, MIN-LENGTH, and a bounded depth.

**Diagnostic deductions:** Current position alone cannot generally capture future motion: the sentry revisits area6 at different phases with different next positions. Event phase and interruption state therefore matter. The source provides concrete examples of what happens without a chosen move, how an action affects events, and how time-dependent legality arises. F04 and F15 can be populated from explicit declarations; compatibility must then be checked under their engine contracts.

**Uncovered information:** The plan's invented periodic-access delivery problem has no supplied sample spec. Sentry establishes that temporal diagnostic categories have concrete source examples, but it does not establish delivery deadlines, waiting rules, boundary timing, or their support. Those remain future intake and mapping questions. No delivery requirements or answers are invented here.

**Feasibility result:** Temporal diagnostic collection is supported by an existing example, with a clearly stated coverage gap for non-Talos delivery. Full generality and the effectiveness of a spec-blind temporal dialogue remain untested.

### Corner-topo: given geometry and continuing resource obligations

**Sources:** [problem-corner-topo.lisp](../probs/problem-corner-topo.lisp), object declarations, technology includes, initialization, and goal; [gate.lisp](../tech/gate.lisp), `update-gate-status!`; [beam-relay.lisp](../tech/beam-relay.lisp), connector pickup, placement, and connection actions.

**Available information:** The problem declares one agent, three connectors, two transmitters, three receivers, one gate, two walls, one window, and four candidate locations. The agent and connector1 begin at location1; the other connectors begin at locations2 and 3. Receiver1 controls gate1. Red and blue chromas, apparatus coordinates, boundary polygon, and wall/gate/window segments are explicit. The goal requires the agent at location4 and receivers2 and 3 active. The file requests MIN-LENGTH, graph search, symmetry pruning, cutoff 15, and at most three termini in a connector pairing action.

**Diagnostic deductions:** Gate control and final receiver demands can compete for connector commitments, so a gate-open milestone alone does not express the full continuation requirement. This is a dependency to examine, not proof that a particular relay allocation or route works. The included technology is needed to interpret action consequences: ordinary pickup removes pairings, whereas the retaining-pairings action preserves them while a held connector lacks its placed location. Geometry-derived visibility and walkability cannot be replaced by guesses based on object counts. F04, F08, F10, F11, and F14 are relevant after characterization.

**Not settled by the spec alone:** User confirmation of the intended puzzle, whether current search restrictions preserve the intended solution space, and any proposed location's usefulness. Existing coordinates and apparatus must remain fixed; additional standing locations may be proposed with justification. Detailed reachable-continuation and derived-geometry claims require further evidence. No working relay arrangement or route is claimed.

**Feasibility result:** The spec exposes the given apparatus, geometry, wiring, initial arrangement, and goal. Included technology supplies some action semantics, so a self-contained top-level file is not a prerequisite for collecting diagnostics. Source evidence can identify the resource and geometry questions without solving the puzzle.

### Coverage and conclusion

| Required analysis information | Evidence available in the samples | Remaining boundary |
|---|---|---|
| Coherent description, given data, choices, initial and goal conditions | Explicit in queens, Hanoi, and corner; knapsack has data and actions | User agreement remains necessary; knapsack acceptance needs a source audit |
| Answer identity, objective, and ties | Queens class switch; Hanoi MIN-LENGTH; knapsack MAX-VALUE | Settings are not user intent; tie and sequence requirements may be absent |
| Future-relevant state, history, and time | Hanoi locations; queens masks; sentry event phase | State sufficiency is a reasoning obligation, not established by declarations alone |
| Growth and choice generation | Board dimension, disk count, item domains, spatial locations/pairings | Target scale and performance need user input and later measurements |
| Reductions, bounds, resources, decomposition | Queens construction/symmetry; knapsack bound; corner gate/relay dependencies | Proof obligations remain; knapsack exposes a concrete bound/choice mismatch to audit |
| Mapping, compatibility, and validation plan | Stage 2 contracts plus named hooks, settings, and included technology | Source review is not runtime validation or measured performance |
| Agreement, corrections, and durable handoff | The template can record each fact and its status | No actual user dialogue, correction exercise, or handoff was performed |

The samples support feasibility of collecting the principal structural diagnostics: the information categories correspond to concrete facts and identifiable omissions. They do not establish complete engine support for every proposed design, faithfulness to unstated user intentions, or that the dialogue will elicit the information efficiently. In particular, non-Talos temporal delivery remains uncovered and knapsack retains explicit audit blockers.

### Later dialogue validation, separately authorized

In a future session, conduct actual dialogues from the user's rough descriptions without reading the corresponding specs or using these extracted facts to prefill answers. Keep this source review out of the interview context where practical. If prior exposure would influence the exercise, disclose it and use a fresh session supplied with the general procedure and capability reference but not these case notes or sample source contents.

Evaluate whether the preliminary paraphrase becomes coherent and agreed, questions change decisions, already supplied facts are reused, irrelevant features are skipped, performance unknowns can wait, and corrections reopen only dependent conclusions. At least one simple case should reach readiness without traversing the entire reference, and a genuine blocker should be preserved when encountered. Any later comparison against the specs must be separately authorized after the dialogue record is fixed; it must not silently change the user's intended rules. The planning review above makes no claim that these dialogue criteria have passed.

## Documentation release review and deferred work

Reviewed on 2026-10-08 against the development plan as amended by the user. The procedure covers entry, preliminary agreement, adaptive questions, design reasoning, corrections, persistence, and scoped handoff. All 16 seed features have reference entries. Local links and cited source-symbol presence were checked, along with code fences, whitespace, dollar escaping, storage rules, and authorization boundaries. These are documentation checks; source-symbol presence alone does not validate a behavioral claim. The preceding source reviews supply the stated behavioral evidence and its limits.

The artifact is ready for the next authorized phase of advisor use or development. It is not dialogue-validated, benchmarked, or proof that every proposed mapping is correct for every problem. The source-derived sample review retains the knapsack audit obligations and the uncovered non-Talos delivery case. No engine or problem-spec changes, other-advisor changes, runtime searches, problem records, temporary files, or new directories were made for this documentation deliverable.

Deferred work, requiring its own scope and approval:

- Evaluate actual dialogues in a future session without sample-spec or case-note access; record agreement and information gaps before any later source comparison.
- Adapt spec-advisor to consume the saved analysis, preserving agreed decisions and reopening only changed or unresolved items.
- Correct stale search-advisor capability statements against current source, including parallel backtracking and BT PATH restrictions.
- Resolve any broader coordination with consultant or solving-advisor separately; this advisor does not depend on that integration.
- Investigate problem-specific gaps only when relevant and authorized, including the knapsack bound/acceptance audit and any selected temporal-delivery model.

Storage is settled by this advisor's convention and is not a deferred integration decision. The historical plan's unresolved-storage language and original paper-dialogue acceptance criteria have been superseded by the user's corrections recorded here.

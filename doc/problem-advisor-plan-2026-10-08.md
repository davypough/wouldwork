# General Wouldwork problem advisor: development plan

Date: 2026-10-08. Status: proposed for user review; implementation not authorized by this document.

## 1. Purpose and scope

Build `doc/problem-advisor.md` as a standalone advisory procedure that takes an informal problem description, or an existing problem specification, to an agreed problem analysis and justified search design. Its central distinction is:

**Problem facts → justified search design → Wouldwork implementation.**

The advisor should help the user understand which aspects of the problem make it difficult, how those difficulties change with size, what design follows from those facts, and where Wouldwork supports or constrains that design. It should explain relevant alternatives when they change a decision, not teach a catalogue of algorithms before understanding the problem.

This planning session reads documentation and current source only. It creates this one file; it does not create the advisor, alter any existing advisor or engine, stage a problem, execute a search, or create directories. `consultant.md` is set aside: the proposed advisor neither invokes it nor depends on its record schema or workflow. Existing advisor references to it are an integration issue for later work.

The eventual advisor ends with an analysis ready for specification writing or review, not a promise that the problem is solvable or tractable. It can also end with a precise blocking question or capability gap. Spec authoring, engine augmentation, measurements, search, and full solution validation remain separately authorized work.

## 2. Recommended advisor structure

Keep the first implementation in one top-level file, with these sections:

1. **Purpose and entry:** accept a rough description, examples, diagrams, rules, goals, or an existing spec. Explain the intended output in plain language.
2. **Adaptive dialogue:** extract established facts, identify consequential uncertainty, ask a focused question, revise the understanding, and stop when further answers would not change the current design.
3. **Design reasoning:** connect state, choices, interactions, answer identity, and growth to representation and search decisions. Introduce technical labels only here, after characterization.
4. **Feature-to-capability reference:** compact indexed entries used selectively by the advisor, not a questionnaire shown wholesale to the user.
5. **Analysis record and handoff readiness:** an embedded plain-text template and explicit criteria.
6. **Limits, evidence, and corrections:** explain proof obligations, unknowns, source checks, and what changes when a premise changes.
7. **Short walkthroughs:** illustrative dialogues and resulting design excerpts, including non-Talos cases first.

Do not copy the full DSL reference, search settings reference, or Talos diagnostic catalogue. Link to relevant material after explaining why it applies. No extra directories, scripts, or engine features are required for this initial documentation deliverable.

## 3. How the dialogue should work

### Start from what the user gives

Read the supplied description first. Summarize the intended answer, important objects or choices, rules, and any obvious growth dimensions in the user's vocabulary. Separate explicit statements from deductions. A missing executable spec is normal, not a blocker.

For an existing spec, maintain two accounts: the user's intended rules and what the source implements. Readable code is evidence of implementation, not authority to settle ambiguous intent. Do not silently turn a current setting into a requirement.

### Select questions by consequence

Maintain a short internal list of open design issues. For each issue, record the decision it could change, what is already known, and what would settle it. Prefer the issue with the largest effect on correctness, answer identity, representation, or growth. Ask one focused question at a time; there is no fixed count and no requirement to visit every reference entry.

Examples:

- “Do two different orders of these deliveries count as different answers?” determines whether merging identical end situations is acceptable.
- “After spending this token, can anything restore it?” determines whether apparent progress is permanent and whether the remaining supply can rule out a branch.
- “If the same arrangement is reached later, are the available moves still the same?” exposes time or history that must be retained before discussing repeated states.

Explain the reason briefly when it is not obvious. Offer a concrete contrasting example when terminology is ambiguous. If the description already settles the answer, record it without asking again.

Unknowns have different consequences. An unknown rule that changes legality blocks the affected representation decision. An unknown heuristic cost can remain a future measurement. An irrelevant question is dropped with a short reason. Stop exploring an issue when its possible answers no longer change the current recommendation.

### Develop and revise the design

Once enough facts are established for a decision, propose the design in ordinary terms: what must be remembered, what a choice does, what gets ruled out early, and what makes progress finite. Then map it to Wouldwork terminology and supported mechanisms. Decisions may be made incrementally; no exhaustive interview precedes all reasoning.

For each consequential design decision, record its premises, a plausible alternative, why the recommendation fits, and what would cause reconsideration. Usually one alternative is enough; do not invent alternatives for obvious choices.

Corrections can arrive at any point. Preserve the prior claim and its evidence as superseded, identify dependent decisions and capability mappings, and reopen only those affected. For example, changing “one board” to “all labelled boards” reopens symmetry, counting, and termination decisions without reopening the basic attack rule. Changing “placement is permanent” to “pieces can move again” reopens state identity, cycles, and the assumed construction depth.

### Stop at a useful agreement

Present a compact proposed design with remaining blockers and deferred measurements. Ask the user to confirm the consequential interpretation and design choices. Readiness is based on the criteria in section 5, not the number of questions answered. An early partial handoff may cover an explicitly named independent part, but must not be labelled ready for the whole problem.

## 4. Feature-to-capability mapping

### Organization and maintenance

Use stable feature IDs, plain-language titles, and three support labels:

- **B — built in:** a current engine or reusable technology mechanism exists; list its conditions.
- **S — spec logic:** the mechanism needs problem-specific rules, representation, queries, or proof obligations.
- **A — possible augmentation:** the desired design is not established as supported; inspect the gap before proposing engine work. This is not an approved implementation project.

A row may have more than one label: a built-in hook does not supply a correct problem-specific bound. Also maintain a separate evidence status: source-checked, documented only, hypothesis, or measured. “Built in” and “tested for this problem” are different claims.

The table below is the seed inventory for implementation. Each expanded advisor entry should contain: trigger; question or observation; alternative answers and design consequences; mechanism and support label; assumptions; source file plus symbol; verification date; and an example or proposed check. Keep source details in the reference, not every interview question. Review affected entries when their source changes or a walkthrough exposes a contradiction. Prefer one authoritative entry per mechanism with links from related features.

| ID / plain-language question or observation | How answers affect the design | Wouldwork mapping and support | Assumptions or unresolved evidence |
|---|---|---|---|
| F01 Answer identity: “What must an answer contain, and when are two answers different?” | Distinguish assignments, final states, sequences, and equivalence classes. Merging states or symmetric choices may destroy requested answers. | B: solution modes and retained paths; `count`; restricted `all-paths`. S: unique construction, canonical answer rules, validators. C01/C02 below. | COUNT is accepted-goal encounters, not a general distinct-answer counter. Check path multiplicity and symmetry before choosing a mode. |
| F02 Objective: “Is one acceptable answer enough, or must we find the best, all, a count, or establish none exists?” | Feasibility can stop early; optimality and complete enumeration need coverage and sound reductions. Absence needs finite exhaustive scope or a separate proof. Clarify cost, ties, and units. | B: `first`, numeric limits, `every`, `count`, `min-length`, `min-time`, `min-value`, `max-value`. S: objective and acceptance definitions. C01/C08. | A cutoff, interruption, branch restriction, or model mismatch limits the conclusion. “All optima” needs explicit handling, not assumed optimization-mode semantics. |
| F03 Growth: “What becomes larger, and which choices start interacting?” | Track object/domain counts, interaction density, path length, state size, and cost per expansion. Choose compact sufficient information and avoid gratuitous permutations. | B: search counters and parallel timing. S: parameterized instance families, representation, proposed scaling checks. A: another method if the representation cannot support the target scale. | Establish structural growth before measurements. No speed or tractability guarantee; do not extrapolate from size alone. |
| F04 Future-relevant information: “Can two apparently identical situations have different possible futures?” | If yes, retain the time, resource, history, or obligations that distinguish them. If no, equivalent-state merging may be sound. | B: dynamic/static relations, state comparison, graph search, history validation hooks. S: sufficient state encoding and matching semantics. C03/C05. | The engine's database equality is not a proof that the model retains everything relevant. History validators do not automatically make merging histories sound. |
| F05 Choices: “What remains undecided, and can we generate only relevant options?” | Finite domains suggest assignments; transitions suggest action choices. Prefer economical legal-choice generation over large products filtered late. | B: typed actions, query-dependent action domains, preconditions; CSP action scheduling. S: selection of the next variable, domain narrowing, and construction order. C04. | CSP does not imply general automatic variable ordering or propagation. Prove that a restricted generator preserves the requested answers. |
| F06 Progress and repetition: “Can a choice be undone, or can the same situation recur?” | Permanent assignments can give finite depth and a unique construction. Reversible moves need cycle treatment and termination reasoning; merged states trade memory for repeated work. | B: depth-first graph/tree, backtracking tree, depth cutoff, restricted BT PATH checking. S: monotone measures and canonical construction. C03. | Ordinary backtracking inverse detection is weaker than ancestor-cycle checking. A bounded absence claim is only about the covered depth. |
| F07 Equivalent orders: “Do independent choices commute, and is their order part of an answer?” | If order is irrelevant, canonical construction can remove factorial duplication. If order changes legality or answer identity, preserve it. | B: graph duplicate handling. S: fixed order, ordering constraints, or action design. A: a general partial-order reduction only if needed and separately investigated. C03/C04. | A swap must preserve intermediate legality, objective, and continuation possibilities, not merely the final arrangement. |
| F08 Interchangeable objects: “Would exchanging these objects change any rule, goal, cost, or requested answer?” | True interchangeability permits representatives; visible identities or unequal roles prevent that reduction. Spatial rotations are a separate equivalence question. | B: detected object symmetry, instantiation filtering, graph canonical symmetry. S: problem-specific spatial canonicalization, as in queens. C06. | Verify rules, static distinctions, goals, and counting semantics. Do not equate object symmetry with every geometric symmetry. |
| F09 Early impossibility and consequences: “What can already rule out this partial choice, and what becomes forced?” | Reject proven dead branches early; propagate necessary consequences; order difficult choices early when helpful. | B: preconditions, constraint/invariant mechanisms, update/query machinery, `prune-state?`. S: domain checks, forced updates, contradiction tests. C04/C07. | A prune must be sound; forced consequences must follow from the rules. No general propagation strength is promised merely by selecting CSP. |
| F10 Scarce resources: “What is consumed, what must be kept available, and what commitments cannot be reversed?” | Track capacity and future demand; distinguish temporary use from loss. Necessary obligations can produce prunes or bounds. | B: relations and search hooks. S: resource balances, reservation/obligation logic, admissible resource bounds. Talos technology only when relevant. | Avoid double-counting demands that one action or object can satisfy together. A proposed allocation's failure does not prove all allocations fail. |
| F11 Decomposition: “Which parts can be solved independently, and what must pass between them?” | Truly independent parts can combine; coupled parts need explicit interfaces. Milestones must retain resources and continuation options. | B: subgoal/checkpoint facilities and goal-chain recovery. S: milestone contracts, interfaces, candidate checks. A: specialized decomposition if warranted. C09. | Goal chaining is not automatic discovery of independent parts. Sequential milestone success does not establish global optimality. Chained and one-off subgoal forms have different parallel support. |
| F12 Estimates and bounds: “Does this number merely look promising, or can it safely rule out a completion?” | Use promising estimates for ordering; use proved optimistic bounds for elimination. Account for evaluation cost. | B: `heuristic?`, `min-steps-remaining?`, `bounding-function?`. S: formulas and soundness arguments. C07/C08. | Do not promote a successful heuristic to a bound. Max-value sign conventions and feasible completion values matter; backtracking lacks automatic objective-bound pruning. |
| F13 Work from endings: “Are acceptable endings few or easier to describe than complete routes?” | Goal arrangements, predecessor information, or a meeting strategy may help; generating all endings may itself be too large. | B: base-relation enumeration, `find-goal-states`, predecessor and meeting-point entry points. S: schema, filters, applicability and witness checks. A: unsupported inverse/domain behavior if established. C10. | A satisfying ending is not necessarily reachable. Existing APIs are not evidence of universal reverse-action support; audit the selected path before recommending it concretely. |
| F14 Repeated calculation: “What is recalculated, and what actually changes after a choice?” | Separate static facts from changing facts, maintain cheap summaries, and consider memoization only with valid keys and invalidation. | B: static/dynamic relations, backtracking undo, worker read snapshots. S: incremental queries, caches, maintained summaries. C04/C11. | Cache keys must include all relevant context. Updates must undo correctly; shared mutable helper state requires a parallel-safety check. Memory cost can outweigh saved computation. |
| F15 Time and external changes: “Can something happen while no chosen action occurs?” | Event phase and waiting can affect the future. A current arrangement alone may be insufficient. | B: happenings/patrollers, action durations, explicit waiting; restricted auto-wait. S: timing model. A: unsupported temporal requirements after checking. C05. | Backtracking rejects happenings; graph+happenings is diagnosed as incompatible. Auto-wait requires serial depth-first planning tree mode. |
| F16 Validation as size grows: “What evidence would make us revise this design?” | Separate fidelity examples, reduction arguments, and performance experiments. Expand one or more growth axes deliberately. | B: validation/search reporting tools. S: allowed/forbidden cases, independent small oracles, matched comparisons, bounded measurement plan. | Specify questions, instances, limits, metrics, interpretations, and approval before executing. Equal counts alone do not establish equal answers or rule fidelity. |

Parallel execution is a downstream compatibility review across these features, not another principal interview branch. Default the proposed local baseline to `(ww-set *threads* 16)` when compatible. Explain required exceptions and consider startup, serial task generation, repeated prefix work, worker memory, synchronization, and workload balance. Do not imply that 16 workers divide runtime by 16 or that serial execution is always the backtracking baseline.

## 5. Proposed analysis artifact and handoff criteria

Use a standalone plain-text `problem-analysis.txt` record, as confirmed by the user during planning, to preserve the agreed analysis for the spec-advisor. Its contents must make sense without the consultant or this conversation. The location is an open integration decision: use a user-approved existing location; never create a problem directory automatically. This session creates no example analysis files.

Proposed fields, kept brief when the problem is simple:

```text
PROBLEM / REVISION / SOURCES / AUTHORIZED SCOPE
USER OBJECTIVE AND ANSWER IDENTITY
  What an answer contains; equivalence; objective and ties; intended coverage.
INTENDED RULES AND EXAMPLES
  Given objects/data; legal choices; changes; initial and goal conditions.
  Allowed, forbidden, and boundary examples; explicit unknowns.
CURRENT IMPLEMENTATION (if any)
  Source behavior and discrepancies; do not substitute this for intended rules.
STRUCTURE AND GROWTH
  Size dimensions; interacting choices; state/history/time; termination.
JUSTIFIED DESIGN
  Decision ID; recommendation; premises; alternatives; consequences;
  soundness/completeness obligations; reconsideration trigger; agreement status.
WOULDWORK MAPPING
  Decision -> capability/source; B/S/A support; compatibility; limitations.
INITIAL SETTINGS AND DEFERRED MEASUREMENTS
  Semantically determined settings versus provisional performance choices.
  Compatible local parallel baseline; reasons for exceptions.
EVIDENCE
  Intended rule / source behavior / deduction / hypothesis / measurement.
  Origin, date, scope, premises, and limitations for each consequential claim.
OPEN ISSUES AND CAPABILITY GAPS
  Blocker or deferred; affected decisions; way to settle; required approval.
VALIDATION PLAN
  Fidelity cases; reduction checks; growth family; bounded later experiments.
AGREEMENT AND HANDOFF
  User-confirmed decisions; readiness; spec obligations; next authorized step.
CORRECTIONS
  Superseded claim; reason; dependent decisions reopened; retained evidence.
```

Evidence type and agreement status are separate. A source fact can be confirmed as source behavior while still conflicting with intended behavior. A deduction can be conditional on an unconfirmed premise. Use IDs only where dependencies or traceability justify them; avoid bureaucratic entries for every sentence.

Ready for whole-problem spec writing when:

1. Answer identity, objective, intended scope, and consequential rules are agreed.
2. The proposed state retains everything needed for legality, goals, costs, and relevant history; unresolved rules do not silently change the model.
3. Choice generation and termination have a rationale, including growth beyond the small example.
4. Every consequential reduction states why the required answers survive, or remains explicitly optional pending validation.
5. Core decisions have applicable Wouldwork mappings; blocking gaps have been resolved or the user has approved a concrete path to resolve them. Optional optimizations may remain deferred.
6. Initial semantic settings are distinguished from performance settings needing measurements. Parallel compatibility exceptions are explicit.
7. The spec author receives requirement-to-design obligations and allowed/forbidden examples, not just parameter values.
8. The user confirms the analysis and authorizes the next scope separately. Handoff readiness does not authorize writing or searching.

For an existing spec, the handoff instead identifies the requirements already represented, discrepancies, proposed review scope, and unresolved fidelity questions. No demand to rewrite a faithful spec from scratch.

## 6. Current capability evidence and limitations

Source inspection date: 2026-10-08, working tree based on Git HEAD `be251cb`, including pre-existing local modifications and the untracked parallel-backtracker file. These are current source observations, not claims that runtime checks passed. A later implementation session must recheck changed claims; HEAD alone does not identify this working tree.

| Ref | Evidence inspected | Consequential conclusion |
|---|---|---|
| C01 | [ww-searcher.lisp](../src/ww-searcher.lisp): `count-accepted-goal`, `register-solution`; [ww-backtracker.lisp](../src/ww-backtracker.lisp): `register-solution-bt`; [counting.md](search/counting.md) | COUNT increments accepted encounters and retains one example, without filling the ordinary solution lists. It does not itself establish distinct boards, states, paths, or symmetry classes. |
| C02 | [ww-searcher.lisp](../src/ww-searcher.lisp): `initialize-hybrid-mode` | Special ALL-PATHS mode requires serial depth-first graph search with positive cutoff. Parallel requests fall back to EVERY semantics; the advisor must not silently accept this when all paths are required. |
| C03 | [ww-initialize.lisp](../src/ww-initialize.lisp); [ww-searcher.lisp](../src/ww-searcher.lisp): `process-successors`, `on-current-path`; [ww-backtracker.lisp](../src/ww-backtracker.lisp): `detect-path-cycle`, `validate-bt-path-mode` | Backtracking forces tree mode. Ordinary inverse checking is not full ancestor checking. Optional BT PATH mode requires serial planning/tree/backtracking and excludes listed history-sensitive facilities. CSP also forces tree mode; that adjustment does not prove a particular spec has no duplicate constructions. |
| C04 | [ww-planner.lisp](../src/ww-planner.lisp): `generate-children`, `get-new-states`; [ww-backtracker.lisp](../src/ww-backtracker.lisp): `backtracking-actions`, `visit-generated-choices-bt` | Typed/query-dependent domains and preconditions generate choices. CSP selects the action indexed by depth while depth is below the number of actions; beyond that the code falls back to the action collection. Do not describe CSP as an unconditional general assignment solver. A repeated assignment action can enforce its own row/variable progression, as queens does. |
| C05 | [ww-initialize.lisp](../src/ww-initialize.lisp); [ww-validator.lisp](../src/ww-validator.lisp): auto-wait parameter validation | Backtracking errors with happenings. Graph+happenings prints an incompatibility diagnostic. Auto-wait checks serial depth-first planning/tree requirements. Distinguish a printed diagnostic from an enforced error. |
| C06 | [ww-symmetry.lisp](../src/ww-symmetry.lisp): `detect-symmetry-groups`, `filter-symmetric-instantiations`, `use-canonical-symmetry-p`; [queensN](../probs/problem-queensN.lisp) | Built-in object symmetry differs from spec-specific square rotations/reflections. The current queens spec has optional class counting and incremental occupied-column/diagonal masks. Old all-board timings do not describe all current settings. |
| C07 | [ww-planner.lisp](../src/ww-planner.lisp): `expand`; [ww-backtracker.lisp](../src/ww-backtracker.lisp): `backtrack`, `ordered-choices-bt`; [ww-searcher.lisp](../src/ww-searcher.lisp): lower-bound functions | Prune, move-lower-bound, and ordering hooks exist in current search paths. Pruning descendants differs from rejecting an already accepted goal. The advisor must state the purpose of each hook and recheck driver-specific contracts for a proposed spec. |
| C08 | [ww-searcher.lisp](../src/ww-searcher.lisp): `bound-search-state`, `node-descendants-cannot-improve-p`; [ww-initialize.lisp](../src/ww-initialize.lisp); [knap19](../probs/problem-knap19.lisp): `bounding-function?` | User bounds compare minimizing-form optimistic cost with an incumbent and can tighten it using a feasible completion value. Backtracking has user bounds and move bounds but no automatic objective-bound pruning. Depth-first MIN-VALUE automatic pruning assumes nondecreasing cost. These are design conditions, not merely speed settings. |
| C09 | [ww-goal-chaining.lisp](../src/ww-goal-chaining.lisp): `solve-subgoal`, `validate-continuation-preconditions`, `run-goal-chain-request`, `solve-subgoal-from-form` | Chained continuation requires serial execution. The explicit start/goal form supports either thread mode; it is a one-off search and has different session effects. Milestone recovery exists, but does not discover sound milestone abstractions for the user. |
| C10 | [ww-enumerator-build.lisp](../src/ww-enumerator-build.lisp); [ww-enumerator-run.lisp](../src/ww-enumerator-run.lisp): `find-goal-states`, `fps-state-feasible-p`, predecessor functions, `solve-meeting-point-fn` | Goal enumeration, predecessor, and meeting facilities exist. This inspection establishes entry points and some contracts, not general applicability or exhaustive reachability. Domain-specific use needs a focused schema, predecessor, key, and witness audit. A missing feasibility hook is not a reachability proof. |
| C11 | [ww-parallel-backtracker.lisp](../src/ww-parallel-backtracker.lisp): `process-partitioned-parallel-bt-body`, `generate-bt-tasks`, `run-bt-task`; [ww-searcher.lisp](../src/ww-searcher.lisp): `dfs`; [wouldwork.asd](../wouldwork.asd); [ww-worker-read-snapshots.lisp](../src/ww-worker-read-snapshots.lisp): `validate-worker-read-snapshot-mode` | Current loading and dispatch include parallel backtracking. Tasks are generated serially and workers replay ordinal prefixes on private state. Check deterministic choice generation and shared helper state. Active worker snapshots reject declared search-time dynamic-object registration. Source presence is not a measured speedup or a completed regression claim. |

The mapping deliberately leaves possible gaps visible. Dynamic variable selection, stronger constraint propagation, partial-order reduction, a different temporal model, or a more general reverse search may be useful designs. Determine whether spec logic suffices before proposing an engine extension; if uncertain, record “support not yet verified” rather than “absent.” Do not discard a justified design solely to fit the current engine.

## 7. Material to reuse and integration to defer

- [spec-advisor.md](spec-advisor.md): reuse intent-fidelity checks, characterization before classification, the DSL reference, and technology selection guidance. Later integration should accept the agreed analysis, check changes and unresolved items, and avoid restarting the interview. Its current consultant dependency and template need a separately approved compatibility change.
- [search-advisor.md](search-advisor.md): reuse focused strategy explanations, hook contracts, growth-measurement questions, and interpretations of incomplete searches. Do not import its spec-first probe workflow into the opening dialogue. Sections 3 and 5 still include serial-only backtracking claims inconsistent with the current implementation; defer correction, but do not copy them into the new advisor.
- [solving-advisor.md](solving-advisor.md): reuse the general reasoning about resource obligations, milestone continuation, and evidence scope. Its actual procedure is Talos-specific, with its own records and search constraints. Do not make those defaults apply to all problems or attempt its generalization here.
- [tech/README.html](../tech/README.html): consult for Talos roles, capabilities, dependencies, and geometry-derived behavior only after the problem warrants that specialization. Preserve given apparatus, geometry, wiring, chromas, and heights; candidate standing locations can be proposed with justification.
- Existing worked problem sources provide contrasting modeling examples, not mandatory designs or evidence that every related problem shares their performance.
- The working-reference and missing-relations companions linked by spec-advisor are downstream fidelity aids. Adapt their Talos-specific detail only when relevant.

Do not change consultant coordination, move existing records, remove old templates, or broaden this plan into an advisor-suite redesign. Future integration should decide which document owns each explanation and link to it to prevent duplicate maintenance.

## 8. Planned walkthroughs — no searches

Use five short paper walkthroughs. Begin each with an informal description, record facts extracted without questions, show the consequential questions, and finish with a small analysis/handoff excerpt. Examples are illustrative, not user-confirmed problem requirements. Read existing examples for modeling evidence; do not present their solutions as advisor discoveries.

| Case | Proposed exercise | Expected design lesson and acceptance criteria |
|---|---|---|
| Small queens placement, then larger boards | Use [queensN](../probs/problem-queensN.lisp) as a source reference, with a hypothetical four-row intake. Ask whether rotated/reflected boards count separately. Later change the answer. | Unique fixed-row construction, incremental conflict checks, future-row impossibility checks, and correct board/class identity. Correction reopens counting/symmetry only. Explain growing domains and interactions without claiming small-board success proves scalability. |
| Small disk-transfer puzzle | Use [hanoi](../probs/problem-hanoi.lisp) for three disks. Clarify whether any sequence or a shortest sequence is wanted; compare arrangements reached by different routes. | Sufficient state, reversible moves, repeated states, path versus goal identity, and growth of sequence length. Do not recommend ordinary backtracking on the assumption it removes all cycles. A known distance formula is separate knowledge, not a search discovery. |
| Small packing optimization | Describe four or five items; use [knap19](../probs/problem-knap19.lisp) to inspect existing bound conventions rather than execute the 19-item instance. Clarify whether item order matters and whether tied best sets are all required. | Compare fixed include/exclude decisions with unordered additions; distinguish value-to-weight ordering from a sound optimistic bound and a feasible completion. Identify resource growth, bound cost, tie semantics, and data-dependent difficulty. |
| Time-sensitive non-Talos delivery | Invent a small narrative with two deliveries and a periodic access window; no file or implementation. Give two identical arrangements at different times and ask whether their futures differ. | Retain relevant time/phase, expose an actual temporal compatibility question, and avoid mapping it blindly to ordinary graph or backtracking search. Separate required timing semantics from whether current happenings fit. Mark unresolved support honestly; no compulsory engine project. |
| Talos gate/relay resource case | Use a limited intake slice of [corner-topo](../probs/problem-corner-topo.lisp), preserving its actual given apparatus and geometry. Discuss the goal, possible equipment commitments, and whether an apparent gate-opening milestone preserves onward options. | Reuse technology only after characterization. Distinguish geometric feasibility, a stable arrangement, and a reachable continuation. Additional standing locations can be proposed; extra equipment cannot be invented. Explain how locations/interactions and relay choices grow. Do not run or claim a route. |

Across the walkthroughs also inject: an already answered question, an irrelevant feature, an unknown that can safely wait, and a correction to a premise. At least one concise case must reach readiness without visiting most reference entries; one must stop with a legitimate blocker. The review checks usefulness and logical consistency, not runtime search performance.

## 9. Staged implementation proposal

Each stage is independently reviewable. After the user approves the plan, the later session should perform only the explicitly approved stage and stop for review. These are documentation stages, so REPL runs are not their acceptance test.

| Stage | Deliverables | Acceptance criteria |
|---|---|---|
| 1 — Standalone procedure and artifact contract | Create `doc/problem-advisor.md` with scope, adaptive dialogue, correction handling, embedded analysis template, and readiness rules. | Works from a description without a spec; no consultant dependency; three reasoning layers remain distinct; no technical opening questionnaire; no fixed question count; growth and compatible 16-thread baseline present; semantic unknowns block affected decisions rather than being guessed. |
| 2 — Capability reference | Add the feature entries, support/evidence labels, source anchors, compatibility review, and limits. Recheck current source before consequential claims. | Every seed feature is accounted for without requiring every interview to visit it. Parallel backtracking is represented accurately. COUNT, ALL-PATHS fallback, CSP scheduling, time/history, symmetry, bounds, and goal-chain restrictions are explicit. Built-in hooks are separated from spec obligations; uncertain support is not called absent. |
| 3 — Walkthrough validation and revision | Add concise paper walkthroughs and refine the procedure against their failure cases. Keep the material in the same advisor file unless the user approves another deliverable. | Five contrasting cases satisfy section 8. Questions change decisions; facts already given are reused; corrections invalidate the right conclusions; at least one gap is handled without distorting the problem. No searches, claimed benchmark results, or full problem implementations. |
| 4 — Documentation review and release of this advisor | Check links, source references, consistency, dollar escaping, and permission boundaries; present the finished advisor with remaining limitations and a deferred integration list. | A reader can start and finish a consultation using this file alone. Every handoff field has a purpose. No contradictory serial-only backtracking claim. No unapproved edits to other advisors or engine files. No temporary files or new directories remain. |

Future work, separately approved: adapt spec-advisor to consume the analysis; correct stale search-advisor capability claims; resolve storage/coordination policy; consider any broader solving-advisor changes. No engine augmentation is part of these four stages.

## 10. Decisions for the user

1. **Artifact storage:** the user confirmed that the problem-advisor records its analysis for the spec-advisor and named the record `problem-analysis.txt`. The remaining decision is its storage location. Whether to reuse a particular existing directory convention is undecided; the advisor must not depend on consultant setup or automatically create folders.
2. **Mapping placement:** recommend keeping the first feature reference inside `problem-advisor.md`. Approve that scope or request a separate reference later if its size warrants it.
3. **Walkthrough breadth:** recommend the five cases above, including the invented temporal example to test honest handling of a potentially unsupported requirement. These are paper reviews only.
4. **First implementation scope:** recommend approving Stage 1 first and reviewing its dialogue and output contract before filling in the complete reference. Approval of the overall direction should state whether it also authorizes any later stages.

No other user decision is needed to finish this plan. Specific engine support questions in C10 and future problem-specific mappings remain source-audit work, not questions the user must answer from memory.

## 11. Self-contained prompt for a later implementation session

```text
Work in D:\quicklisp\local-projects\wouldwork. Read and follow AGENTS.md;
ignore CLAUDE.md. Read doc/problem-advisor-plan-2026-10-08.md and any
user corrections or approval supplied with this prompt.

Implement only Stage 1 of that plan, after confirming that the accompanying
approval authorizes Stage 1. If no implementation approval accompanies this
prompt, present the proposed scope and wait. Do not infer approval of later stages.

Create doc/problem-advisor.md as a standalone general problem advisor. It must
start from a rough description or an existing spec, extract established facts,
ask focused consequential questions without a fixed question count, accept
corrections throughout, and distinguish problem facts, justified search design,
and Wouldwork implementation. Characterize before introducing engine labels.
Address growth from the outset and use compatible local parallel execution,
normally 16 threads, as the downstream baseline with explicit exceptions.

Include the advisor's scope, adaptive dialogue, correction/dependency handling,
standalone problem-analysis.txt template, and handoff-readiness criteria. The user
confirmed this filename and its purpose as the spec-advisor's input. Keep intended
rules, source behavior, deductions, hypotheses, and measurements distinct.
Treat artifact location as unresolved unless the user's review settled it;
do not create problem records or directories as part of this documentation task.

Set doc/consultant.md aside. Do not modify spec-advisor, search-advisor,
solving-advisor, engine code, or problem specs. Do not stage problems, run
searches, or implement possible capability gaps. Talos is one specialization,
not the organizing model for general dialogue. Later spec-advisor integration
is deferred. The complete capability reference and walkthroughs are later stages.

Inspect current files and preserve existing uncommitted work. Recheck any
capability claim needed for Stage 1: source changed recently, particularly
parallel backtracking, and some advisor documentation is stale. The planning
snapshot was based on be251cb plus local changes, not a clean commit or a runtime
validation. Do not treat that snapshot as confirmed-current.

Verify the Stage 1 documentation against the plan's acceptance criteria and
check links and formatting. Report the completed file, limitations, and the
proposed next stage. Stop for review; do not proceed to Stage 2 automatically.
```

## 12. Planning-session verification

The plan is based on read-only inspection of AGENTS.md, the three requested advisors, relevant problem examples, technology overview material, and the source anchors listed above. No searches or runtime checks were performed. Only this planning document was added by this session; pre-existing work is preserved. Review of the document should precede any implementation approval.

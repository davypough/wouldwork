# Wouldwork Specification Advisor

> **Purpose:** Build a Wouldwork problem specification, with the user's interactive
> assistance, that can attempt to solve the problem previously defined by the
> [problem advisor](problem-advisor.md). Begin with its agreed analysis files.
> The specification should include justified enhancements for larger problems of
> the same general type identified by that analysis.
>
> **Entry:** Invoke this standalone procedure directly. Reuse the established objective and authorization
> scope. Receiving an analysis or discussing its handoff does not authorize drafting,
> reading an existing specification, staging, or running a problem.

> **Status:** Six-step interactive authoring workflow revised 2026-10-09. Document
> consistency and links were reviewed; the revised workflow has not yet been exercised
> in a problem consultation. This file tracks the current Talos `tech/` vocabulary. If it disagrees with
> the *Wouldwork User Manual* on a technology-based specification, verify the detail against
> `tech/README.html` and the current technology source.

**Companion documents.** [working-reference-builder.md](problem-analysis/spec-building/working-reference-builder.md) makes the model's behavior
readable so the user can compare it with their intent.
[inferring-missing-relations.md](problem-analysis/spec-building/inferring-missing-relations.md) helps investigate a discrepancy,
including a suspected omission; it does not assume every difficult problem needs another relation.
The advisor introduces each companion when useful; the user need not invoke them separately.
Both contain Talos-specific detail: adapt their review questions to other problems without
importing Talos mechanics. For the technology library,
`tech/README.html`; for relation signatures, `tech/Talos Technology  Summary.txt`; for
what the engine does at load time, [loading and initialization](load-ordering/loading-and-initialization.md).


---

## Section 0: Process Guide

### 1. Read and confirm the incoming analysis

Read AGENTS.md and ignore CLAUDE.md. When the analysis paths have not already been
supplied, begin with exactly:

> Welcome to the Wouldwork Specification Advisor. Please share the path to your problem analysis files (as prepared by the Wouldwork Problem Advisor).

When the paths are already supplied, acknowledge and use them without asking the
user to repeat them. Read the current problem analysis and its linked, preserved
pre-specification snapshot. If a required file is missing or ambiguous, ask for
that file's path; do not reconstruct agreed requirements from an existing spec.
An informal description first needs the problem advisor's agreed analysis.

Review the agreed problem, objective and answer identity, given data, justified
reductions and their premises, growth dimensions, capability evidence, corrections,
and implementation acceptance criteria. Distinguish requirements from provisional
settings and optional implementation choices. Summarize only what is needed to
confirm the implementation starting point; do not repeat the problem interview.
Ask only about gaps or conflicts that could change implementation or fidelity.

Preserve the original snapshot. Current agreed corrections govern subsequent work;
the snapshot records the earlier agreement rather than overriding those corrections.
Keep intended behavior, source observations, deductions, and executed evidence
distinct. A withdrawn drafting step or historical next-action entry is not current
authorization. Do not read an existing problem specification or its problem-specific
dependencies while comparison remains deferred.

Finish this step by stating what is ready for implementation planning, any blockers,
and the requested next scope. Readiness does not itself authorize specification
creation or execution. For resumed work, reuse the previously approved scope and
review only changed premises or unresolved matters. Consult the DSL reference
(Section 1) when mapping the agreed analysis to an implementation; use Section 2
only for relevant information not already established in the incoming analysis.

### 2. Agree the specification-building scope

Before writing a specification, present a concrete scope based on the incoming
analysis and obtain approval for work not already authorized. Identify:

- The analysis revision, objective, given instance, and larger problem family to
  preserve, including agreed reductions and their premises.
- The intended specification path and whether the work creates a new specification
  or revises an explicitly selected existing one. State any proposed source or
  dependency reading; preserve deferred comparison boundaries.
- The section-by-section work: establish the basic model first, then implement and
  check justified enhancements for the identified growth dimensions. Select the
  particular enhancements interactively as their prerequisites become clear.
- The active `spec-analysis-` record and expected deliverables: specification,
  explanation and requirement mapping, check results and limitations, and a handoff
  for the optional search-advisor phase.
- Which incremental checks are included, who runs them, and their limits. Distinguish
  static review, staging, executed fidelity checks, enhancement checks, and a bounded
  final run. A solution is not required for that final run.

Keep the activities below distinct when stating or interpreting approval:

| Activity | Scope of authorization |
|---|---|
| Receive and review analysis | Reuse and assess the supplied requirements and readiness; does not authorize specification creation or comparison. |
| Propose implementation | Explain representation, rules, affected files, alternatives where consequential, and acceptance checks; does not write the specification. |
| Write or revise | Make the agreed specification changes, incorporating the user's section reviews; does not by itself authorize execution. |
| Stage | Load and initialize the named specification revision; does not authorize solving it. |
| Check fidelity or enhancements | Execute the specified allowed, forbidden, boundary, invariant, or hook checks within the approved scope; record what they establish. |
| Replay | Execute an identified action sequence against the model under an approved replay scope; keep its conclusions distinct from fidelity. |
| Run a bounded smoke check | Attempt search on the named instance with agreed settings and stopping limits to check operation; need not reach a solution and is not a performance benchmark. |
| Search substantially, benchmark, or tune | Requires its own approved plan; performance investigation normally belongs to the search advisor. |

An approved scope may explicitly bundle drafting, incremental staging and checks,
and the final bounded run. Reuse that approval rather than asking again for each
covered action. Still present each section for the user's comments before proceeding
to the next; approval of the overall work does not bypass the interactive review.
If execution was not included, propose the relevant checks and wait for approval.
Preserve the user's existing REPL and identify whether checks are assistant-run or
provided for the user to run.

Each executed check needs a question, model revision, relevant starting state and
settings, expected observations, operator, and applicable stopping limits. Choose
small checks that answer the correctness question; test an incomplete section only
when its prerequisites make the result meaningful. Record untested sections explicitly.
Do not silently deepen, repeat, or expand a run beyond its approved scope.

Return consequential changes in rules, objective, answer identity, or reduction
premises to the problem analysis for agreement before relying on them. Explain any
proposed tradeoff and the guarantee it loses. Routine implementation choices within
the approved scope need explanation where useful, not a separate approval each time.
Record the approval and its limits in the active spec analysis. If a blocker remains,
name the affected work and continue only with authorized independent sections.

#### Specification analysis records

Store the spec advisor's analysis in the same `doc/problems/<problem-name>/`
directory as the problem advisor's records. Use the `spec-analysis-` prefix for
spec-advisor records, for example `spec-analysis-current.txt` for the active record
and `spec-analysis-YYYY-MM-DD-before-revision.txt` for a preserved snapshot.
Use the same prefix for supporting analysis documents, with a descriptive suffix.
Reuse the existing active record; do not create a new record for each exchange.
Identify the active record explicitly when linking it from another advisor's documents.

Record implementation decisions, section review status, user corrections, checks
and their results, unresolved issues, and the next authorized step. Link the incoming
`problem-analysis.txt` and its preserved snapshots rather than duplicating their
agreed requirements. Return consequential changes to those requirements for agreement;
do not silently establish different rules in the specification analysis. Preserve
the original problem-analysis snapshot unchanged.

Do not use the generic `Analysis.txt` for new spec-advisor analysis. Existing records
retain their provenance and status; this naming convention does not authorize renaming
them or resuming withdrawn work. This advisor owns its implementation agenda and
evidence; no consultant or shared coordination record is required. Link upstream
requirements and downstream findings without copying their authority. Keep detailed evidence in a linked
`spec-analysis-` artifact only when needed. Return when the selected question is
answered, needs approval, or requires another procedure, without requiring all the
phases below.

The return includes clarified requirements, relevant characterization, implementation
mapping, discrepancies, evidence and its scope, unresolved issues, affected dependencies,
and the recommended next action. A correction reopens affected conclusions in the
linked records with the user's agreement; unaffected evidence
and replayable progress remain available.

### 3. Build the basics interactively, section by section

Use the agreed analysis to propose a short section agenda. Build the basic model
before adding advanced search enhancements, while preserving reductions already
accepted in the analysis. A reduced model need not first implement the discarded
actions. Keep the identified larger problem family in view: avoid hard-coding an
instance-specific cutoff, object count, or solution into general rules.

The discussion order follows dependencies and the user's comments; the finished
file follows Section 1.1's organization. A useful default agenda is:

| Section to discuss | Decisions to explain and review |
|---|---|
| Representation and settings | What the state remembers; objective and action-cost units; authoring approach; semantic settings versus provisional execution settings. |
| Objects and relations | Given instances and static data; changing and derived facts; capacities, conservation, identity, and other invariants. |
| Queries, updates, and applicable technologies | How rules are expressed, consequences derived, and shared behavior reused without changing the agreed meaning. |
| Actions | Generated choices, locality, legality, effects, counted steps, and understandable action descriptions. |
| Initial state and initialization | How supplied data becomes the starting state; which facts are authored and which are derived. |
| Goal and basic model review | How the intended answer is recognized; whether all required behavior and accepted reductions are accounted for. |

Combine small dependent sections or split a complex section when that helps review.
Discuss the goal and initial state early whenever they constrain representation;
the agenda is not a requirement to postpone those decisions.

For each section:

1. Link the relevant agreed requirements and explain the proposed representation
   or rules in the user's terms. Show consequential alternatives only when they
   affect fidelity, clarity, or the identified growth requirements.
2. Resolve implementation questions with the user, then write the section within
   the approved scope. Present what was written and how it satisfies the requirements.
3. Perform covered incremental checks when the prerequisites exist, or provide the
   checks for the user to run. Record expected and observed behavior separately.
   Do not add fictitious rules or a placeholder goal merely to make an incomplete
   specification stage; defer the check until a meaningful executable unit exists.
4. Incorporate the user's comments, revisit affected earlier sections, and update
   the active spec analysis with decisions, evidence, unresolved issues, and checks
   still pending. Present corrections before applying them as required by the scope.
5. Pause for the user's review before proceeding to the next section. Reuse already
   granted implementation and test approval; a section review is an opportunity to
   adjust the model, not a repeated request for the same authorization.

Finish the basics with a coherent requirement-to-implementation account and the
status of the applicable checks. Name any blockers before moving to enhancements;
do not mistake completion of the basic model for completion of the specification.

#### Review against the user's intent

The objective is a faithful model, not merely a specification that loads or finds a solution.
Keep the user's intended rules and the implemented rules distinct until they have been compared.
Use these questions throughout the interview and review:

| Check | Plain-language question | Useful evidence |
|---|---|---|
| Soundness | Does the model allow any action or outcome the intended problem forbids? | An example that should be rejected, with its conditions and modeled result |
| Completeness | Does it preserve the requested answers and relevant behavior, accounting for every agreed reduction? | Requirement-to-spec mapping, allowed examples, and reduction premises and coverage arguments |
| Consistency | Do the declarations, initial state, derived rules, and requirements agree? | Initialization checks, invariant checks, and examination of conflicting cases |
| Clarity | Do the user and advisor mean the same thing by each rule? | Concrete examples, boundary cases, and explicit unresolved questions |

These are model-fidelity checks, not claims about search completeness or optimality. A few
passing examples provide scoped evidence, not a proof over every possible state. Successful
loading establishes only that the implemented checks passed; replay establishes behavior under
the model, not independently that the model matches the intended puzzle.

During section review, use the reference builder's categories as an internal checklist.
For each important rule, explain what is allowed, what is forbidden, and what changes afterward.
Ask one focused question at a time where intent is unclear. Reuse established technology semantics,
but explain consequential behavior so the user can confirm that the chosen technology fits.
Record requirements as confirmed, provisional, contradicted, or unresolved, with their source.

Keep intended abilities distinct from the transitions retained in a reduced search
model. An agreed proof may justify omitting an ability without changing the original
problem. Preserve that proof's premises and objective scope; do not ask about details
that affect only omitted behavior unless a changed premise makes them relevant.
Conceptual interchangeability does not by itself verify engine symmetry or a compact
encoding; retain any obligations for consistent object names in returned instructions.

Preserve given objects, apparatus, wiring, geometry, colors, and heights. Locations are candidate
standing spots, so proposing an additional location is legitimate when supported by the geometry.
Do not add equipment or change barriers to manufacture a solution. A user-confirmed correction
to an incorrectly transcribed given must be distinguished from changing the given itself.

#### Choose the authoring approach from the agreed analysis

Reuse the problem advisor's characterization of objects, initial conditions, changes,
derived effects, restrictions, and success criteria. Clarify only consequential gaps
before committing to a representation. Keep intended rules, source behavior, deductions,
and hypotheses separate. Use the approaches below when supported by those features,
record why the choice fits, and reopen it only when relevant premises change.

**Path A — a new Talos Principle problem.**
Build it on the `tech/` technology library. These files implement a *topological*
representation of Talos mechanics — beams, connectors, gates, walkability,
visibility, elevation — as reusable roles that splice into the spec via
`include-tech` (Section 1.5). A new Talos problem should be assembled from the
existing technologies rather than hand-authoring the mechanics, so that behavior
stays consistent across problems and fixes propagate.

Read `tech/README.html` before drafting. It is authoritative for the technology
library: the role system, the tier picture, the file inventory, hard vs. soft
dependencies, and the integration checklist.

**Path B — everything else.**
Hand-authored specifications with no `include-tech`. This includes all non-Talos
problems *and* the legacy Talos specs — `problem-corner.lisp` is the reference
example — which predate the technology library and are not written against it. Do
not retrofit `tech/` onto a legacy spec as part of a new problem session; treat the
two approaches as separate.

On Path B, choose the engine specialization from the established behavior:

**Planning or CSP?** Set via `(ww-set *problem-type* planning)` or `csp`.

- *Planning* — actions represent changes to the problem state; the solution is a
  sequence of steps. Usually paired with `depth-first` + `graph`.
- *CSP* — actions represent sequential assignments of values to variables; the
  solution is a complete assignment satisfying the constraints.
  `problem-captjohn.lisp` is the reference example. Usually paired with
  `backtracking` + `tree`, and leave `*depth-cutoff*` at 0, since search must reach
  a depth equal to the number of rules.

A CSP can be expressed as a planning problem. Assignment structure and ordering determine
whether Wouldwork's CSP specialization fits; the desired output alone is not enough.
Explain the choice from the characterization rather than asking the user to choose an
engine label. Use the [search advisor](search-advisor.md) for consequential strategy
questions, retaining provisional choices until their prerequisites are established.

#### Adapt the dialogue and investigate discrepancies

Ask one focused question at a time, explaining which implementation or fidelity
decision it affects. There is no fixed question count or repeated intake questionnaire.
Use supplied answers and clearly label deductions; do not guess consequential intent.
Skip irrelevant features and stop questioning an issue once its possible answers no
longer change the design. Accept corrections at any point and reopen affected sections
without discarding independent evidence.

On Path A, explain consequential technology semantics and confirm that they fit the
agreed rules. Finalize technology selection against `tech/README.html`'s integration
checklist within the authorized reading scope. Library behavior is not authority to
replace intended behavior.

Use the reference builder when useful to explain the modeled rules, objects, initial
conditions, and goal. After authorized staging, incorporate inspected derived facts;
label unstaged predictions as predictions. Map important requirements to their
implementation and identify omissions, extra behavior, and uncertainty.

Use focused allowed, forbidden, boundary, and invariant checks rather than requiring
a full solve to review fidelity. When a discrepancy appears, use the missing-relations
companion where appropriate. Consider extra permissions, wrong conditions, initialization
or loading faults, misunderstood rules, and intentional reductions as well as missing
relations. A failed search alone does not diagnose an omission. Present intended and
observed behavior, evidence, and the proposed correction before implementing it.
Refresh affected explanations and repeat relevant approved checks after correction.
Keep separately authorized complete-path replay distinct from fidelity and solvability
claims.

### 4. Implement and exercise enhancements for larger instances

After the basic model is coherent, work through enhancements for the larger problem
family identified by the problem advisor. Implement suitable, justified enhancements
even when the given instance can be solved without them. Do not leave them merely
as recommendations for the search advisor or disable them solely because the given
instance is small. Preserve reductions already integrated into the basic model;
this step does not require implementing them twice.

Use the incoming growth analysis to propose a short enhancement agenda. For each
candidate, explain the growth problem it addresses, its prerequisites, the intended
benefit, its cost or complexity, and the correctness obligation. Candidates can
include ordering heuristics, sound pruning or bounds, interchangeable-object handling,
compact representations, efficient choice generation, or maintained summaries.
Select what fits the analyzed problem; do not add a hook simply to fill a checklist.
If no additional mechanism is justified, discuss that conclusion with the user and
record the agreed disposition rather than silently omitting this step.

#### Preserve the requested answers

Distinguish mechanisms that only order exploration from mechanisms that remove
choices or merge states. Explain why each restriction preserves the requested
answers, costs, and coverage under the agreed premises. In particular:

- A pruning rule needs an impossibility or dominance argument; a plausible ranking
  score is not enough. A bound used to eliminate candidates needs the appropriate
  optimistic guarantee and must fit the selected engine hook's actual contract.
- Interchangeable objects must preserve legality, goals, costs, and answer identity.
  Check the concrete encoding or symmetry mechanism and any named-path reconstruction;
  the conceptual interchangeability argument alone does not verify implementation.
- Duplicate sharing must preserve relevant futures and the required cost comparison,
  including cheaper later arrivals where applicable. Do not assume a chosen search
  mode supplies that behavior without checking its current contract.
- Navigation and resource decomposition must retain relevant entry/exit conditions,
  resource locations and commitments, recovery options, and cost. Independently short
  segments do not establish a globally shortest solution. Combining bounds requires
  justification that costs are not counted twice.
- Cached or maintained information needs complete dependencies, correct update and
  restoration behavior, and compatibility with the intended execution mode.

Preserve the scope of each argument: a proof for the supplied instance is not
automatically valid for the growing family. Do not use a known solution as a general
rule or silently change the objective to make an enhancement applicable. Return
consequential semantic changes to the analysis for agreement.

Consult the problem advisor's [capability reference](problem-advisor-reference.md) selectively and verify consequential
hook and compatibility assumptions against current source within the authorized scope.
Documented availability is not proof that a particular formula or encoding is correct.
Investigate an uncertain capability before relying on it; engine changes require their
own proposal and approval.

#### Implement, check, and review one enhancement at a time

Follow the section-review cycle from step 3. Explain the proposed mechanism and its
correctness argument, incorporate the user's comments, implement it within the approved
scope, and perform the covered checks before proceeding. Recheck affected basic-model
behavior. Keep the general rule separate from instance data so larger instances of the
agreed type do not require changing its meaning.

Exercise each selected enhancement, rather than treating successful staging as evidence
that it ran. Use the supplied instance where it reaches the relevant behavior. Otherwise
propose focused legal states, boundary cases, or a small instance of the same family
under an approved check scope. Do not invent new mechanics or silently create a benchmark
problem. If the needed check is outside the current scope, request its authorization
and leave the enhancement's readiness explicitly unresolved until checked.

Useful evidence depends on the mechanism: an ordering hook returns usable priorities;
a pruning rule retains an allowed case and rejects a case justified by its argument;
a bound agrees with independently understood tiny completions; a symmetry mechanism
merges the intended equivalents while preserving meaningful distinctions; a maintained
summary agrees with direct calculation after relevant updates. Verify integration with
the intended execution path as well as any direct helper checks. A direct function call
does not show that search invokes the hook.

Use bounded checks to establish correctness and operation, not to require a solution or
demonstrate a speedup. Passing sampled cases supplies scoped evidence alongside the
correctness argument, not a general proof. Where enhancements interact, check the
combination for conflicting assumptions and unintended behavior before declaring it ready.

#### Record readiness and remaining performance questions

For each enhancement, record in the active `spec-analysis-` document:

- The growth dimension addressed, implementation location, premises, and correctness
  argument, linked to the incoming analysis where already established.
- Whether it is implemented and enabled in the intended configuration.
- What checks actually exercised it, their settings and results, and what remains
  untested. Distinguish helper checks from execution through the engine.
- Any unresolved correctness or integration issue, its consequence, and the next action.
- Performance questions for the search advisor, including possible overhead on small
  instances, tuning choices, and behavior as the identified dimensions grow.

The spec advisor owns implementing and checking these enhancements. The optional search
advisor phase measures their usefulness, compares strategies, tunes settings, and studies
larger instances. An exercised enhancement may be operationally ready while its scaling
benefit remains unmeasured. An unexercised or failing enhancement is not ready merely
because the basic specification runs. Do not silently drop an agreed enhancement or
claim full readiness; resolve it or agree an explicit change of scope with the user.

### 5. Consolidate and check the complete specification

Review the whole specification after the section dialogue and enhancement checks.
Consolidate the active `spec-analysis-` record into one current account of the model,
implementation decisions, evidence, and unresolved matters. Preserve superseded claims
and useful evidence in clearly marked history or linked records; leave the incoming
problem-analysis snapshot unchanged. Do not leave obsolete questions marked pending
when subsequent agreement has resolved them.

#### Review fidelity and consistency

Trace each consequential requirement to its implementation or to an agreed reduction
with its premises and coverage argument. Check the initial state, given objects and
data, legal actions and effects, invariants, goal, answer identity, and cost units
together. Confirm that sections agree with one another and that enhancements preserve
the objective and identified problem family. Explain the complete model in plain
language, including intentional omissions and limitations.

Review the allowed, forbidden, boundary, and enhancement checks already performed.
Repeat affected checks after relevant changes; do not repeat independent checks just
to fill a checklist. Distinguish reviewed source, deductions, actual staging, executed
checks, replay, and search results. No one of these establishes all the others.

Classify remaining issues as correctness or execution blockers, irrelevant under the
accepted design, or deferred performance questions with an owner. An unresolved rule,
failed invariant, or unverified required enhancement cannot be relabeled as performance
tuning. Resolve blockers or explicitly report a partial result; continue independent
work only within the approved scope.

#### Stage and run the final revision

Use the exact final specification revision for acceptance checks. Within the approved
scope, stage it and require completion without errors or warnings. Inspect the output;
the absence of a debugger interruption alone is insufficient. Investigate diagnostics
rather than suppressing them to obtain a clean result. If a diagnostic originates in
the environment, a dependency, or the engine, record that distinction and propose any
needed work separately; do not claim warning-free acceptance while it remains unresolved.

Ensure the check does not rely on definitions left by a previously staged problem.
Use a fresh process when necessary to establish that, without disturbing the user's
existing REPL. Record the environment and relevant settings. Restage after changes
affecting the tested specification; do not present results from an earlier revision
as evidence for the final one.

Run a bounded operational check on the supplied instance with the agreed enhancements
enabled. State the objective, settings, stopping condition, applicable resource limits,
and operator before execution, reusing approval when already covered. Use supported
limits appropriate to the selected mode; do not assume a depth cutoff also bounds
elapsed time or memory. Review the observed stop reason and retain enough output to
show that search entered its intended execution path and ran without errors.

Reaching a solution is not required. Normal completion within the chosen scope or a
planned controlled stop may provide the operational evidence, with the covered work
and limitations stated. A crash, resource failure, or interruption before meaningful
execution is not a passing check. Never silently deepen or continue a run to obtain
a solution. This check establishes observed operation within its scope, not absence
of all runtime defects, global optimality, or larger-instance performance. Required
enhancement exercise evidence from step 4 remains necessary even if this run does
not trigger every relevant case.

#### Acceptance and completion status

The specification is ready for the optional search-advisor phase when:

1. The complete model and its requirement mapping are reviewed with the user, with
   no unresolved consequential interpretation or known fidelity defect.
2. Agreed reductions and enhancements are implemented with their premises recorded;
   applicable correctness and integration checks pass within their stated scope.
3. The final revision stages without errors or warnings.
4. The approved bounded run executes without errors, with its stop reason and evidence
   recorded. It may finish without a solution.
5. The current spec analysis names the delivered revision, checks, limitations, and
   deferred questions to carry into the handoff. No pending blocker is hidden by a
   general statement that the specification works.

If checks are awaiting authorization or user execution, report that status and the
specific missing evidence; do not mark completion on the strength of proposed tests.
Report an unresolved blocker or agreed partial scope explicitly. Finding a solution,
proving optimality, benchmarking, and demonstrating scalability are not acceptance
requirements for this phase.

After the handoff is prepared, use a concise completion message such as:

> Specification complete for [agreed scope]. [Specification path and revision] stages
> without errors or warnings and passed [bounded run and stop condition]. The agreed
> enhancements passed [scoped checks]. [Solution found / no solution established].
> Remaining unverified: [larger-instance performance and other limitations]. The
> spec analysis and optional search-advisor handoff are at [paths].

Use this wording only when supported by the actual evidence; otherwise state
"Specification incomplete" or "Specification complete only for [agreed partial scope]"
and identify what remains. Propose the next scope and wait for authorization.

### 6. Prepare the optional search-advisor handoff

Prepare a concise handoff for the [search advisor](search-advisor.md) after the final
review. Store it alongside the other problem records using the `spec-analysis-`
prefix, for example `spec-analysis-search-handoff.txt`. Link it from the active spec
analysis. The handoff summarizes readiness and points to detailed evidence; it does
not become a second authoritative requirements record or a competing work agenda.

Include the following, using links rather than copying detailed analysis:

| Handoff content | What the next advisor needs |
|---|---|
| Scope and status | Complete, partial, or blocked; the covered problem family; proposed next question; what work is and is not authorized. |
| Requirements and provenance | Current problem analysis, preserved pre-specification snapshot, current spec analysis, and any agreed corrections. |
| Delivered model | Specification path and identifiable tested revision; relevant dependencies and environment; staging instructions and actual test settings. |
| Objective and representation | Answer identity, cost units, goal, sufficient state, action granularity, and the mapping to the agreed requirements. |
| Reductions and enhancements | Implemented mechanisms, enabled configuration, correctness arguments and premises, interaction constraints, and exercise evidence. |
| Validation | Final staging outcome, fidelity and integration checks, bounded run result and stop reason, and any separately authorized replay or solution evidence. |
| Growth and open performance questions | The identified size dimensions and interactions; predicted costs or benefits distinguished from measurements; candidate tuning or comparison questions. |
| Remaining issues | Blockers or limitations, evidence still missing, deferred work with an owner, and conditions that would reopen earlier conclusions. |

Distinguish settings required by the objective from provisional search choices. Give
the actual tested configuration rather than assuming that defaults or saved settings
match it. State compatibility conditions and any unverified capability assumptions.
Do not infer a general size limit or a speedup from the bounded operational check.

The next optional phase investigates search behavior: measuring enhancement overhead
and benefit, comparing strategies, tuning settings, and examining larger instances of
the agreed family. Propose a concrete first question with its evidence needs; leave
execution limits and any new experiments for the authorized search plan. Preparing
the handoff does not invoke that phase, authorize benchmarks, or authorize a full solve.

Preserve the user's objective, reductions, corrections, and useful evidence across the
handoff. No repeated problem interview is needed. If downstream work exposes a fidelity
gap or invalidates a premise, return the affected issue for agreement and correction,
then refresh dependent evidence and the handoff. Historical evidence remains available
with its model revision and limits; it must not be represented as current after changes.

The search advisor may subsequently recommend the optional solving advisor when a
constraint-led milestone approach is justified. Carry the original objective and
resource/continuation obligations into that discussion. Shortest milestone segments
do not establish a globally shortest path; any loss of guarantee needs explicit
agreement. Reserve a later `Handoff.md` for solving progress, accepted actions, and
checkpoints; it should link these analysis records rather than replace or delete them.

Report the specification, active spec analysis, and handoff paths with the completion
status from step 5. A partial or blocked handoff must name the independent scope it
supports and the missing evidence; it is not a declaration of full readiness. Stop
for the user's review and authorization of the optional next phase.


---

## Section 1: Wouldwork DSL Reference

### 1.1 File Structure

Every problem spec is a `.lisp` file in the **`probs/`** directory. Test problems
exercising a single technology go in **`test/talos/`**. Wouldwork resolves a problem name
by searching `probs/` first, then `test/` and each category folder directly below it.

**Do not write a spec in `src/`.** `src/problem.lisp` is a *generated* file — staging
any problem overwrites it. It is the spliced snapshot the engine compiles, not a
source file.

Required package declaration: `(in-package :ww)`

Sections appear in this canonical order:

1. `ww-set` declarations (problem parameters)
2. `defparameter` / `defun` helpers, if any
3. `define-types` (object types and instances) — and `define-optional-types`
4. `include-tech` directives — **Path A only; must come after `define-types`**
5. `define-dynamic-relations` (state relations that change)
6. `define-derived-relations` (dynamic relations computed during initialization), if any
7. `define-static-relations` (state relations that don't change)
8. Query functions (`define-query`)
9. Update functions (`define-update`)
10. Actions (`define-action`)
11. `define-happening` / `define-patroller` (exogenous events), if any
12. `define-init` (initial state assertions)
13. `define-init-action` (derivations run once at initialization), if any
14. `define-goal` (goal condition)

Only one of these orderings is a hard requirement rather than a convention:
**`define-types` must precede every `include-tech` directive.** Section 1.5 explains
why, and why violating it fails silently. The rest is house style — forward references
among queries and updates are resolved by a pre-scan pass, so definition order among
them does not matter.


### 1.2 Problem Parameters (`ww-set`)

```lisp
(ww-set *problem-name* <symbol>)          ; e.g., match3a
(ww-set *problem-type* planning)          ; or csp (constraint satisfaction)
(ww-set *solution-type* <type>)           ; first, every, all-paths, min-length,
                                          ; min-time, min-value, max-value, or integer N
(ww-set *tree-or-graph* <mode>)           ; tree or graph
(ww-set *depth-cutoff* <integer>)         ; max search depth (0 = no limit)
(ww-set *symmetry-pruning* <bool>)        ; t or nil
(ww-set *progress-reporting-interval* <integer>)  ; omit for adaptive reports; N = every N states
```

**Three parameters must NOT appear in a problem file. Each signals an error if it
does:**

| Parameter | Why |
|---|---|
| `*debug*` | gates conditional compilation |
| `*algorithm*` | selects which translations are generated |
| `*probe*` | gates conditional compilation, and validates against loaded actions |

Each requires recompilation to take effect, and the problem file is read during the
very compile it would need to influence. Set all three at the REPL after staging; each
triggers an automatic reload. `*problem-name*` is the mirror-image case — it must be
set in the problem file and is refused at the REPL.

Note also that on an ordinary load, a saved `vals.lisp` overrides the problem file's
`ww-set` values. Use `(stage <problem>)` to get the problem's own intended settings.
See `doc/load-ordering/parameter-precedence.md`.


### 1.3 Type System (`define-types`)

Defines object types and their instances (ground atoms).

```lisp
(define-types
  block  (A B C)
  table  (T)
  support (either block table))   ; union type
```

- `either` creates a union of previously defined types.
- Type names become unary predicates: `(block A)` is automatically true.
- Numeric instances are permitted: `row (0 1 2 3)`.
- `(compute <form>)` can generate instances programmatically.
- A type declared with an empty instance list — `beam ()` — is legal, and is the
  pattern for populations that only exist at runtime. See Section 1.9.


### 1.4 Relations

#### Dynamic Relations (`define-dynamic-relations`)

Relations that change during search. Stored in the mutable state database (IDB).

```lisp
(define-dynamic-relations
  (on block support)              ; non-fluent: all args are typed
  (loc agent $area)               ; fluent: $area is a value looked up via bind
  (cell row col $fixnum)          ; fluent: $fixnum is the stored value
  (holds agent $cargo)            ; fluent
  (color relay $hue))             ; fluent
```

- **Non-fluent args** (no `$` prefix): Part of the database key.
  The relation either exists or doesn't.
- **Fluent args** (`$` prefix): Stored as the value associated with the key.
  Use `bind` to retrieve. A relation can have multiple fluent args.
- `(inconsistent-state)` is a built-in nullary dynamic relation. Asserting it
  causes Wouldwork to prune the current search branch.

#### Derived Relations (`define-derived-relations`)

Dynamic relations whose initial values are computed by initialization and propagation:

```lisp
(define-derived-relations
  active
  open
  color)
```

Each name must already be declared as a dynamic relation somewhere in the assembled
problem. A derived relation may be asserted and retracted by initialization actions and
updates, but must not appear positively or negatively in `define-init`.

#### Static Relations (`define-static-relations`)

Relations that never change. Asserted in `define-init` and stored separately.

```lisp
(define-static-relations
  (coords area $rational $rational $rational)
  (controls receiver gate)
  (max-row $fixnum))
```


### 1.5 Technology Includes (`include-tech`) — Path A

New Talos Principle problems assemble their mechanics from the `tech/` library rather
than hand-authoring them:

```lisp
(include-tech gate)                  ;controls; energized; update-gate-status!
(include-tech beam-relay)            ;paired; color; pickup/put/connect actions
(include-tech beam-crossing)         ;crossing-active; crossings-along-beam>
(include-tech walkability)          ;walking traversal-via; mobility-results; traversable; move
(include-tech visibility)            ;los-via; visible; visible-clear
```

**What it does.** `include-tech` is not a runtime macro. Before compilation, each
directive's target file is *textually spliced* into the generated `src/problem.lisp`,
recursively — a technology may include others. Each technology is spliced at most once;
repeats leave a `;; ... already included -- skipped` marker. By the time anything is
evaluated, the tech bodies are ordinary top-level forms.

**The one hard ordering rule: `define-types` must appear above the includes.**

A `define-query` body is translated the instant its form is evaluated, and a `doall`
over a bare type name is compiled into a *literal* domain list at that moment. A type
declared below the includes is therefore already known by name — the pre-scan pass
registers it — but with an empty instance list. Every tech query iterating over it
collapses into a silent no-op. **This is not an error.** Nothing warns you; the problem
just fails to do anything. `problem-corner-topo.lisp` carries a comment block warning
about exactly this.

**Division of labor.** Composite types a technology needs (`mobile-object`, `cargo`,
`support`, `target`, and so on) are declared inside the technology file itself, so no
tech file depends on a declaration living in the problem. The problem declares only the
leaf types it instantiates. Where both declare the same type, consistency is enforced —
they must resolve to the same instance list.

**Further reading.** `tech/README.html` is authoritative for the library: the role
system, tiers, file inventory, hard vs. soft dependencies, and the integration
checklist. `doc/load-ordering/loading-and-initialization.md` covers splicing and its
failure modes in engine terms (Stage 2, and Traps 1–3).


### 1.6 Variable Conventions

| Prefix | Meaning | Scope |
|--------|---------|-------|
| `?var` | Parameter variable | Bound by action/quantifier domains or passed to a query/update |
| `$var` | Local/scratch variable | Bound by `setq`, `bind`, `let`, `mvsetq` |

- Action and quantifier `?variables` have domains and iterate over their instances.
- Query/update `?variables` may independently declare a Wouldwork object type, but
  they are passed by the caller rather than instantiated by the function.
- `$variables` hold local or computed Lisp values. They are not permitted as
  query/update formal parameters.


### 1.7 Query Functions (`define-query`)

Read-only functions that examine state. Cannot modify the database.

```lisp
(define-query <name>
    (?object <object-type>
     ?value
     ?other-object (either <object-type-1> <object-type-2>))
  <body>)    ; MUST be a single expression — use (do ...) to group multiple statements
```

- Every formal parameter is a `?variable`; `$variables` are not allowed here.
- Each parameter may independently be followed by a Wouldwork object type or an
  inline `(either ...)` object type. Typed and untyped parameters may appear in
  any order.
- Type object parameters when doing so states a useful requirement. Leave
  computed Lisp values—numbers, strings, lists, hash tables, booleans, and
  `nil`—untyped. Wouldwork does not currently accept Lisp type declarations in
  query/update signatures.
- These annotations validate compatible uses and calls. They do not enumerate,
  convert, or otherwise change the value supplied by the caller.
- A literal planning object supplied by a caller must belong to the declared
  type. A computed expression remains permissible when its result type cannot
  be established while the problem is translated.
- Action parameter headers (`standard`, `combination`, etc.) and query-valued
  action domains do not belong in a query/update signature.
- An empty optional object type is still a valid annotation. The function is
  installed normally; an action or quantifier that enumerates that type simply
  produces no calls. This permits a typed null-default technology hook to remain
  present and return its neutral result when the corresponding capability has no
  objects in the current problem.
- **Single-expression body:** The body must be exactly one expression. To
  execute multiple statements, wrap them in `(do ...)`.
- Body is translated in `pre` (precondition/read) context.
- Return value is the value of the last expression in the body.
- Can use all DSL operators: `bind`, `exists`, `forall`, `doall`, `ww-loop`,
  `setq`, `do`, `if`, `cond`, `let`, `mvsetq`, etc.
- Query functions call other queries freely, including forward references to queries
  defined later in the file.
- Use `(return-from <name> <value>)` for early return.

Example:
```lisp
(define-query cleartop? (?block block)
  (not (exists (?b block)
         (on ?b ?block))))
```

Mixed object/value example:
```lisp
(define-query beam-visible
    (?location location
     ?near-elevation
     ?object (either transmitter receiver floor-repeater wall-repeater gun location)
     ?far-elevation)
  ...)
```

Here `?location` and `?object` are planning objects. The two elevations are
computed numeric values, so they deliberately have no Wouldwork object type.


### 1.8 Update Functions (`define-update`)

Functions that modify the database (assert/retract relations).

```lisp
(define-update <name> (<parameters>)
  <body>)    ; MUST be a single expression — use (do ...) to group multiple statements
```

- Parameter lists use exactly the same `?variable` and optional Wouldwork
  object-type syntax as queries. Computed Lisp-value parameters remain untyped.
- **Single-expression body:** The body must be exactly one expression. To
  execute multiple statements, wrap them in `(do ...)`.
- Body is translated in `eff` (effect/write) context.
- **Naming convention:** Must end with `!` (e.g., `propagate-changes!`,
  `apply-gravity!`). The translator uses this suffix to identify update functions.
- Can assert relations by writing them as bare forms: `(cell 0 0 3)`
- Can retract relations with `(not (cell 0 0 3))`
- Can call other update functions and query functions.
- `(inconsistent-state)` — asserts the nullary inconsistent-state relation,
  causing branch pruning.

**The `propagate-changes!` pattern** — convergence loop for derived effects:
```lisp
(define-update propagate-changes! ()
  (ww-loop for $iteration from 1 to <max>
           do (if (not (<single-pass-fn>))
                (return t))       ; converged, no changes
           finally (inconsistent-state) (return nil)))  ; failed to converge
```
Returns T on convergence, NIL on failure. On Path A this driver is usually derived
automatically from the splice order of the included technologies; author one only if
the problem needs to override that.


### 1.9 Actions (`define-action`)

The primary search operators. Each action has 6 components:

```lisp
(define-action <name>
    <cost>                    ; numeric cost (usually 1)
  <parameter-header>          ; typed iteration variables
  <precondition>              ; boolean test (pre context)
  <description-variables>     ; variables captured for solution trace
  <effect>)                   ; state modifications (starts pre, shifts to eff in assert)
```

#### Parameter Headers

Control how action parameters are instantiated:

- **`standard`** (default if omitted): All distinct; cartesian product with
  deduplication. `(standard ?a type1 ?b type2)` means `?a ≠ ?b` when same type.
- **`combination`**: Ordered combinations (no duplicates, no permutations).
  `(combination (?a ?b) type)` generates `?a < ?b` pairs.
- **`dot-product`**: Element-wise pairing of parallel type lists.
  `(dot-product ?dr delta-row ?dc delta-col)` pairs corresponding elements.
- **Multiple groups**: `(?agent agent (combination (?t1 ?t2) terminus))`
- **Dynamic queries in headers**: A type position can be a query call like
  `(get-current-beams)` that returns a list at runtime.

#### Preconditions

Boolean expressions in read-only (`pre`) context. Key operators:

- `(bind (relation key... $fluent...))` — Look up fluent values. Returns T/NIL.
- `(exists (?v type) <body>)` — Existential: true if any instance satisfies body.
- `(forall (?v type) <body>)` — Universal: true if all instances satisfy body.
- `(different ?a ?b)` — Symbol inequality.
- Standard CL: `and`, `or`, `not`, `if`, `cond`, `=`, `<`, `>`, `eql`, etc.
- `setq` — Capture intermediate values for use in effects.

#### Effects

State modifications. The effect body starts in `pre` context and shifts to
`eff` (write) context inside `assert`:

```lisp
(assert (cell ?row ?col $new-sym)           ; assert new relation
        (not (cell ?row ?col $old-sym))     ; retract old relation
        (finally (propagate-changes!)))     ; trigger derived effects after assert
```

- **`assert`**: Groups writes into a single state transition. Copies state first
  (for depth-first search).
- **`finally`**: Schedules an update function to run after the assert completes.
  Typically used for `(finally (propagate-changes!))`.
- **Multiple `assert` blocks**: An action can contain multiple `assert` blocks
  (e.g., inside `doall`), each generating a separate successor state.
- **`setq` in effects**: `(setq $place 'ground)` captures values for the
  description-variables trace.

#### Description Variables

A list of variables whose values are captured for the solution trace:
```lisp
(?agent ?connector $area)    ; printed as action description in solution
```


### 1.10 Iteration and Quantification

| Form | Context | Meaning |
|------|---------|---------|
| `(exists (?v type) body)` | pre | True if any instance satisfies body |
| `(forall (?v type) body)` | pre | True if all instances satisfy body |
| `(doall (?v type) body)` | eff | Execute body for each instance (side effects) |
| `(ww-loop ...)` | any | Translates to CL `loop` — use for counted/conditional iteration |

- **Single-expression body constraint:** `exists`, `forall`, and `doall` each
  take exactly one body expression. To execute multiple statements, wrap them
  in `(do ...)`. This is the most common source of translation errors.
- Multi-variable quantifiers: `(exists ((?a ?b) type) ...)` or
  `(doall ((?x ?y) type) ...)`.
- **Static vs. dynamic domain.** A bare type name compiles to a literal list at
  translation time. A query call — `(doall (?b (get-current-beams)) ...)` — is
  evaluated at runtime against state instead. The query form is the only way to
  iterate a population that isn't known when the file loads; declare the type empty
  (`beam ()`) and publish the pool through a relation.
- **Performance note:** For dynamic query types, nested `doall`s with a guard
  are more efficient than `(doall (combination (?a ?b) (dynamic-query)) ...)`
  because `combination`/`standard` perform runtime product/dedup on every call.


### 1.11 Initialization (`define-init`)

Asserts the initial state — both dynamic and static relations:

```lisp
(define-init
  ;; Dynamic
  (loc agent1 area1)
  (cell 0 0 3)
  ;; Static
  (max-row 3)
  (coords area1 9 1 0))
```

- If a relation name is in `define-dynamic-relations`, it goes to the dynamic DB.
- If a relation name is in `define-static-relations`, it goes to the static DB.
- If a dynamic relation is named by `define-derived-relations`, omit it from
  `define-init`; initialization actions and propagation establish its value.
- `(not (...))` retracts a relation (rarely needed in init).

`install-init` validates the complete raw literal set before asserting any of it. The engine
rejects storage-level contradictions such as duplicate fluent keys and authored derived facts;
each included technology registers its own semantic checks for the relations it owns.
These checks can diagnose specification failures during staging; they do not detect
every semantic error or establish fidelity to the intended problem. A standalone
problem that includes no technologies does not inherit Talos-specific rules.

`define-init-action` runs a derivation once during initialization — computing static
facts from raw geometry, for instance. Two cautions: init-actions fire in file/splice
order, *not* by the numeric duration argument; and an init-action is **silently
skipped** if any of its parameter types has no instances. See
`doc/load-ordering/loading-and-initialization.md`, Traps 4–6.


### 1.12 Goal (`define-goal`)

A boolean expression evaluated in `pre` context:

```lisp
(define-goal
  (and (loc agent1 area4)
       (active receiver2)))
```

- Can use `exists`, `forall`, `bind`, `not`, `and`, `or`, and query calls.
- **Update functions in goals:** `propagate-changes!` can be called in
  `define-goal` for post-validation (the translator permits update calls
  in goal/`pre` context). On goal states this modifies in-place since
  goal states are leaf nodes — no copy needed.
- Goal is evaluated against each candidate state during search.


### 1.13 Key Patterns

#### Functional Relations (single value per key)
When a relation like `(cell row col $fixnum)` has typed keys and a fluent value,
asserting with the same key replaces the previous value (hash table semantics).

#### Database Key Validity
Wouldwork encodes relation keys as integers using the type system. All
non-fluent arguments in a `bind` or relation lookup **must be known type
instances**. Computed values (e.g., `(1+ ?col)`) used as key arguments will
crash the integer encoder if they fall outside the declared type range.

**Always validate computed indices before using them in `bind` or relation
assertions.** For example, a `swap-right` action computing `$next-col = (1+ ?col)`
must check `(< ?col $max-col)` *before* `(bind (cell ?row $next-col $sym))`.
This applies to any arithmetic on grid coordinates, array indices, or similar
computed key values.

#### State Copying
`(copy-problem-state state)` creates a deep copy of the current state.
Used when you need to test modifications without corrupting the search state
(e.g., gated walk preconditions that propagate on a copy).

#### `register-dynamic-object`
`(register-dynamic-object <symbol> <type>)` registers a new object at runtime
(e.g., creating new beam entities during propagation). It assigns an integer code and
asserts the type proposition, but it does **not** add the object to the type's
extension — so the object is reachable only through a predicate test or a query-domain
`doall`, never through a static-domain one. Total planning objects are capped at 999.

#### Common Lisp Integration
Arbitrary CL code is allowed in query/update bodies: `push`, `incf`, `cons`,
`member`, `make-hash-table`, `gethash`, `setf`, `format`, `maphash`,
`lambda`, `funcall`, `mapcar`, `some`, `every`, `sqrt`, `abs`, `min`, `max`,
`values`, `multiple-value-setq` (via `mvsetq`), etc.

Functions defined with `defun`/`defparameter` at top level are available
globally (they are standard CL, not translated by the DSL).


### 1.14 Running

These are reference commands, not an instruction to execute every command or an
authorization to stage or solve. Use only the operations covered by the agreed scope.

```lisp
(progn (ql:quickload :wouldwork) (in-package :ww))
(stage match3a)                  ; stage a problem without solving it
(solve)                          ; solve the currently staged problem
(run match3a)                    ; stage and solve in one step
(refresh)                        ; reload after editing the current problem file
(params)                         ; display current parameters
(list-problem-names)             ; list available problems
(help)                           ; list all commands
(ww-reset)                       ; discard generated problem and saved settings
```

`run` and `stage` are macros; each requires a problem name, given either as a bare
symbol or as a string — `(run match3a)` and `(run "match3a")` are equivalent. There is
no zero-argument `(run)`; use `(solve)` to solve what is already staged.

`(stage X)` applies the problem file's own `ww-set` settings. `(refresh)` deliberately
skips them, preserving whatever you set at the REPL.


---

## Section 2: Specification Analysis Template

Use the relevant fields within the active `spec-analysis-` record, linking established
requirements in the problem advisor's analysis rather than repeating intake or creating
a second requirements account. Keep sources and certainty alongside claims. Omit
irrelevant fields; keep consequential unknowns explicit. This is an implementation
record, not another problem interview. Update it throughout the six-step workflow.

```
PROBLEM / REVISION / AUTHORIZED SCOPE:
   - Active spec-analysis path; specification path and revision.
   - Approved edits, source reading, checks, operator, and limits.

INPUT ANALYSIS AND OBJECTIVE:
   - Current problem-analysis path and preserved snapshot path.
   - Links to agreed rules, objective, answer identity, reductions, and growth analysis.
   - Consequential corrections and their agreement status.

REPRESENTATION AND AUTHORING APPROACH:
   - Sufficient state; objects; static, dynamic, and derived relations.
   - Path A or B and applicable technologies, with rationale.
   - Semantic settings versus provisional execution choices.

SECTION REVIEW AND REQUIREMENT MAPPING:
   - Requirement or reduction -> implementation location and explanation.
   - Section proposed / written / reviewed; user comments and their disposition.
   - Allowed, forbidden, boundary, and invariant checks; pending prerequisites.

ENHANCEMENTS FOR THE LARGER FAMILY:
   - Growth issue -> mechanism, implementation location, premises, correctness argument.
   - Enabled configuration; helper and engine-integration exercise evidence.
   - Interactions, unresolved issues, and deferred performance questions.

CHECKS AND EVIDENCE:
   - Question, tested revision, starting state, settings, operator, and limits.
   - Expected versus observed result; stop reason; evidence location and limitations.
   - Distinguish source review, deduction, staging, fidelity checks, replay, and search.

READINESS AND OPEN ISSUES:
   - Final consolidation and user review status.
   - Warning-free staging and bounded error-free run: passed / failed / not run.
   - Required enhancement checks; blockers versus deferred work, with owners.
   - What is ready and what remains unverified; no solution required for completion.

HANDOFF AND NEXT STEP:
   - spec-analysis-search-handoff.txt or the selected spec-analysis- handoff path.
   - Proposed search-advisor question and authorization status.

CORRECTIONS AND HISTORY:
   - Superseded conclusion, reason, affected decisions, retained evidence.
   - Links to preserved records; leave the original problem-analysis snapshot intact.
```

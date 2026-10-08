# Wouldwork Adaptive Consultant

Use this document to guide a problem from the user's objective to an agreed result.
One assistant maintains the consultation and uses the three advisors as specialist
procedures. Separate expert agents are unnecessary. Read repository AGENTS.md first;
ignore CLAUDE.md. Propose changes before editing and respect the user's approval boundaries.

## Responsibilities and limits

The consultant owns the objective, shared problem record, priorities, approval scope,
overall progress, and completion criteria. The user determines intended requirements,
strategic commitments, and changes to the objective. Choose routine technical details
within authorized work, explaining consequential choices.

Characterize the problem before committing to a classification or representation:
objects and properties, initial conditions, possible changes, allowed and forbidden
behavior, derived effects, constraints, and the meaning of success. Reuse supplied
answers; keep unknowns explicit. Gather enough understanding to justify the next
decision without requiring an exhaustive account of irrelevant features.

Keep intended rules, implemented behavior, hypotheses, and measured evidence distinct.
Source establishes what is implemented, not what the user intended. Preserve given
objects, geometry, apparatus, wiring, colors, elevations, and heights. An additional
representative standing location may be proposed when supported by existing geometry;
adding equipment or changing barriers to manufacture a solution is not a model repair.

The consultant selects and coordinates procedures; it does not duplicate DSL syntax,
search settings tables, technology semantics, or diagnostic contracts. Read their
current references when needed. A proposal, recorded development need, or successful
analysis does not itself authorize implementation, replay, or substantial search.

## Adaptive loop

1. Review the objective, current evidence, open topics, and any user guidance.
2. Select the most consequential next topic, considering its dependencies.
3. Choose the appropriate specialist procedure or a direct clarification.
4. Propose the clarification, analysis, edit, or test and its expected contribution.
5. Perform authorized work and interpret what its result does and does not establish.
6. Update the record, revisit affected conclusions, and continue, redirect, or finish.

The agenda is dependency-aware, not first-in-first-out. Address a changed objective,
stop instruction, or contradiction affecting current work first. Then resolve
prerequisites that block consequential decisions. Prefer the smallest investigation
likely to change correctness, feasibility, or the chosen strategy. Explain why the
selected topic matters; no numerical priority score is required.

Reuse established conclusions when their source revision and premises still hold.
Skip irrelevant topics. Defer nonblocking uncertainty with a reconsideration trigger.
Deepen analysis when the answer could change a consequential decision. If several
explanations remain, seek a distinguishing example, source check, or bounded test.
Do not complete unused advisor phases merely because they appear in a checklist.

## Shared problem record

Every problem consultation, short or sustained, produces a plain-text artifact recording
analysis results in `doc/problems/<problem-name>/`. Start with `Analysis.txt` and update
it before presenting results, pausing, or handing work back to the user. A short task
may need only a few paragraphs; it still gets a file. Record read-only analysis too,
explicitly stating when no tests or searches were run.

Use an established problem name. If the name or artifact destination is ambiguous,
clarify it. The approved artifact layout permits creating the named problem directory
for this purpose; do not create unrelated directories or scratch folders. Do not
create an empty problem record for method-development work with no problem instance.

Keep one authoritative current record. Add a focused `.txt` artifact, such as
`Search-Evidence.txt`, only when detailed results would obscure Analysis.txt; link it
from the record rather than copying the same findings into both. Keep executable
actions and machine-readable checkpoints in their required formats. Generated output
is never hand-edited. Link existing Talos Briefing, Handoff, actions, and checkpoints
at their actual paths; do not silently relocate them or maintain competing agendas.
For resumed work, record which file owns current consultation state and use any
existing Handoff as a pointer to it. Preserve evidence and replayable progress.

Use this minimal template, omitting empty detail rather than inventing answers:

```text
<problem-name> -- Analysis
Updated: <date>
Sources: <spec and relevant source revisions; user requirements; evidence paths>

OBJECTIVE AND AUTHORITY
Requested result and completion criteria:
Limits and preferences:
Authorized work; work still awaiting approval:

PROBLEM UNDERSTANDING
Relevant features and intended rules, with sources:
Implemented behavior and comparison with intent:
Hypotheses and consequential unknowns:
Classification/representation decisions and their justification, if ready:

TOPICS
<Descriptive question or name>
Significance: <which decision or outcome it affects>
Evidence: <claim kind, source, scope, and certainty; links to detail>
Status: <open / active / established / deferred / not applicable>
Depends on: <named premises or topics; blocking dependency if any>
Next action or resolution criterion: <including approval needed or revisit trigger>

OUTCOMES AND CONTINUATION
Understanding; fidelity; strategy; realized progress; final validation:
  <each sufficient / incomplete / not required, with evidence and scope>
Accepted progress and restore instructions: <links, or none>
Consequential changes: <former conclusion, reason, affected dependencies>
Next proposed step, or completion result and remaining limits:
```

For consequential assertions, distinguish claim kind from provenance and certainty.
A user suggestion can be a tentative hypothesis; a confirmed user rule is intent.
A source reading describes implementation; a deduction names its premises; a measured
result identifies the model revision, starting state, settings, and test scope.
User-reported measurements retain that attribution. Keep `unknown` and
`not applicable` distinct. Record only dependencies whose change could affect a claim.

Topics begin open, become active when selected, and become established when their
resolution criterion is met within a stated scope. A rejected hypothesis can have an
established negative resolution. Open topics may be blocked or awaiting approval;
name that condition without adding more statuses. Deferred topics carry a trigger;
not-applicable topics carry a reason when omission would otherwise be surprising.
Contradicted or stale conclusions return to open, preserving the former conclusion
and explaining why it no longer follows. Do not count closed topics as progress.

## User guidance and invalidation

Accept corrections and guidance at any point. Interpret their role before changing
the record; clarify an ambiguous instruction rather than treating a guess as a rule.

| Input | Response |
|---|---|
| Corrected requirement | Update intent and reopen affected fidelity conclusions and dependent decisions. |
| Tentative hypothesis | Record a conditional explanation or candidate; identify how to distinguish it. |
| Strategic preference | Reprioritize compatible approaches while preserving factual evidence. |
| Changed objective | Revise completion criteria and reassess which topics and results remain relevant. |
| Stop or defer instruction | Stop the affected work and preserve its state and continuation instructions. |

Explain what changed, which prior conclusions no longer follow, what remains valid,
and the next consequence. Follow dependencies transitively: mark dependent conclusions
for review without declaring them false merely because a premise changed. Revalidate
only what is affected, broadening the review when the dependency scope is uncertain.

Old measurements remain evidence about the old model, even when no longer applicable
to the current one. Preserve accepted prefixes and checkpoints; mark their current
validity pending when relevant semantics change. Never silently overwrite a useful
checkpoint with a failed candidate or discard unaffected evidence.

## Specialist procedures

All procedures receive the selected question, objective, relevant record entries,
current artifacts, and authorization limits. They return the conclusion, supporting
evidence and scope, unresolved issues, affected dependencies, and recommended next
action. The same assistant merges these findings into Analysis.txt and selects again.
A return is an update to the consultation, not a new session or an agent handoff.

| Procedure | Entry question | Expected return |
|---|---|---|
| [Specification advisor](spec-advisor.md) | What is intended, how should it be represented, or does the model match it? | Characterization, clarified requirements, implementation mapping, discrepancies, scoped checks, and proposed corrections. |
| [Search advisor](search-advisor.md) | Which search strategy or change is justified for this understood model and objective? | Relevant search characteristics, alternatives, bounded measurements, assumptions, and recommended settings or changes. |
| [Solving advisor](solving-advisor.md) | How can the next useful state or complete result be realized without losing necessary future options? | Candidate or realized progress, endpoint obligations, action/checkpoint evidence, failed assumptions, and continuation options. |

Return as soon as the question is answered, requires approval, or needs another
procedure. Search analysis may proceed conditionally on explicit model assumptions,
but a consequential fidelity uncertainty must be settled before relying on its
results. A discrepancy discovered during search reopens a specification topic.

Use the specification companions for a readable account of implemented behavior and
for investigating a demonstrated discrepancy. Their Talos sections apply only where
relevant. Source details remain in those companions and current technology references.

The general solving procedure is: identify a useful intermediate condition, inspect
its prerequisites and consequences for later options, agree the next step, realize it
within approved limits, inspect the actual endpoint, and preserve accepted progress.
A proposed milestone is a strategic choice unless its necessity is established.
Reaching it does not prove the continuation possible; check known later dependencies
without requiring a complete plan before every useful step.

Talos problems may additionally require diagram comparison, static constraint profiles,
body and service allocations, and beam, support, or recorder diagnostics. These remain
specialist procedures in the solving advisor and its references. Non-Talos problems
do not require Talos profiles, mechanics, filenames, or resource terminology.

## Authorization and testing

State the concrete work, purpose, scope, and relevant limits before requesting
approval. Keep analysis, editing, replay, and expensive search as distinct boundaries.
An explicitly approved batch covers its stated actions and limits; do not ask again
for actions it already authorizes. Do not extend it silently. After an incremental
implementation, present checks and the next recommendation, then wait for review.

Each proposed test specifies:

- The uncertainty and the decision its answer could change.
- The model revision, starting state, procedure, and settings.
- Depth, time, memory, or other applicable limits, who runs it, and how it stops.
- Expected observations and what each would establish or leave unresolved.
- Evidence to retain and how to restore accepted progress.

For local searches, default to `(ww-set *threads* 16)`: this is the user's preferred
efficient configuration for large problems. Thread initialization is serial; initial
root-task generation also runs serially before worker search. Include this startup
cost when interpreting timing, especially for small tests. Sixteen threads does not
mean the entire run executes in parallel or gains a sixteenfold speedup.

Use serial execution when required by the chosen procedure or problem (for example,
backtracking, auto-wait, or declared dynamic-object registration), or when a specific
measurement needs serial counts. Explain and record the exception. Select the
solution objective and bounds for the actual question; MIN-LENGTH is not universal.
Use current [search guidance](search-advisor.md) for compatibility and staging details.
This convention does not change the engine's defaults or authorize a search.

Keep depth limits fixed unless a change is approved. A depth limit alone may not
bound runtime acceptably. Do not silently deepen, retry an expensive failed run, or
replace the approved objective with an easier one. Record termination, truncation,
and accepted results separately. A cutoff or interruption is not an impossibility
proof; a valid candidate found before interruption can still be retained and checked.

## Evidence, progress, and completion

Loading establishes only that implemented load checks passed. Static consistency
does not establish reachability. A hypothetical state is not a witnessed endpoint.
Replay establishes behavior under the tested model, not independent fidelity to intent.
Fidelity requires comparison with confirmed requirements, including relevant allowed
and forbidden cases. Passing examples provide scoped evidence, not a universal proof.

Exhaustion supports only the conclusion justified by the explored space, bounds,
settings, and soundness of pruning. It cannot alone establish fidelity or absence of
unintended behavior. Keep a search-found segment distinct from one separately replayed.
For a solution claim, validate the complete path from the original start, the actual
goal, and all applicable validators. Shortest milestone segments do not establish a
globally shortest path. Optimality, uniqueness, counts, and impossibility require
evidence appropriate to those claims and their scope.

Measure overall progress through established outcomes: sufficient problem understanding,
model fidelity, justified strategy, realized progress, and validated final results.
These can be incomplete or unnecessary for a particular objective, and can reopen.
A strategy review need not solve; a model review need not optimize. Finish when the
agreed criteria are met and no unresolved issue undermines them. Report nonblocking
uncertainty and why it does not affect completion. Stop without additional tuning or
family scaling when it would not advance the requested result.

A resource limit, user stop, or unresolved blocker yields a paused, deferred, or
incomplete result, not successful completion. Save Analysis.txt and useful progress
with a specific continuation or clarification needed.

## Paper walkthroughs

These illustrate decisions and conditional results; they report no executed searches.

**Simple problem: three-disk Hanoi, any legal solution.** Establish the movement
rules, start, and goal before choosing planning. Reuse current model evidence and
propose a short direct sequence and validation. Record the characterization, proposed
sequence, and any later authorized validation in `doc/problems/hanoi/Analysis.txt`
(using the actual problem name in practice). If full replay and goal checks succeed
under the accepted model, finish. No scaling study, thread benchmark, or milestone
apparatus is needed. Even this short consultation leaves a text analysis artifact.

**Difficult non-Talos problem: constrained scheduling.** Characterize resources,
precedence, deadlines, and the requested objective. Uncertain preemption rules block
representation: clarify them first. Route to model review, then propose a bounded
comparison of two justified assignment orders. For a compatible local search, use
16 threads and account for serial startup; if PATH-mode backtracking is selected, record
its serial requirement. If the user corrects capacity, reopen dependent feasibility and
pruning conclusions, retain unaffected precedence evidence, and qualify old timings.
Record each result in the problem's Analysis.txt, separating detailed measurements
only if useful. A feasible schedule at budget expiry is progress, not an optimality
proof; preserve it and the unresolved completion criterion.

**Talos problem: temporary gate access and device recovery.** Compare intended rules
and geometry, then use applicable Talos diagnostics. A stable hypothetical arrangement
remains a candidate until reached. Agree a milestone that opens access while preserving
the device needed later. After authorized bounded realization, inspect its actual
endpoint. If recovery is blocked, reopen the continuation assumption, preserve the
accepted prefix, and propose a revised handover or earlier branch. Record the dependency
change and link preserved actions/checkpoints from Analysis.txt. A requirement correction
reopens affected model conclusions as well. Close only after complete replay, goal checks,
and applicable validators; do not infer global optimality from the milestone searches.

## Incremental integration

The three advisors have moved to `doc/`. The specification and search procedures now
use this consultant's characterization, shared record, entry/return expectations,
conditional phases, and evidence standards. The solving procedure still awaits its
separately approved integration step. During consultant-led work, use this document's
agenda, shared record, and authorization
rules when older advisor process instructions differ. Do not treat an older mandatory
phase, environment choice, or fresh-start rule as authority to discard reusable evidence.

Next, review the specification/search integration, then approve the general/Talos
solving split and record integration. Finally update the manual's entry point and check
its touched technical claims against current source. Each step ends with document/link
checks and user review; the manual edit also requires visual verification. These later
steps are proposals, not authorization to implement them now.

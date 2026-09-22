# Constraint-Led Method — Implementation Plan

Opened 2026-09-20. Hand-maintained. Authoritative for task state, and the
**single source for this development**: nothing about the constraint-led method
lives in `CLAUDE.md` or `AGENTS.md`, which cover ordinary coding in this
repository and say nothing about this work. `doc/README.md` lists
`constraint-method/` as a directory and points here; that is a map entry, not a
second source.

## On reading this file — act, do not wait

A session that opens this file is being asked to continue the work, not to
report on it. Unless the user's message says otherwise:

1. **Take the CURRENT TASK below.** It is the next waiting task, always.
2. **Read its entry in full**, plus the sources that entry names, plus
   `doc/problems/<problem>/Constraint-Continuation-Prompt.txt` for the problem it
   touches — that prompt is authoritative for reading boundaries, permitted line
   ranges, approvals already granted, and verification hashes. Read nothing else;
   the prohibitions below are not optional.
3. **Check the task's Approval field.**
   - *Granted* — begin implementation now. Do not re-ask for an approval this
     file or the continuation prompt records as given; re-asking wastes a turn
     and the approval history exists precisely so it need not be repeated.
   - *Not requested* — present the task, the approach you intend, and its
     acceptance criteria, then ask before writing code.
4. **Work one task at a time**, to its acceptance criteria. If the work turns out
   to need something outside the task's stated scope, stop and ask rather than
   widening it.
5. **Before the session ends**, make the state files current, update this file's
   board and CURRENT TASK header, and regenerate the affected continuation
   prompt last (M3), including its CURRENT TASK block.

## Current Task

**T10 — gate6/source resource probe exhausted; no automatic next search.**
The approved audit's single cutoff-12 return-access probe completed from the
depth-32 keeper candidate.  It found no solution and directly observed cutoff
truncation, so it is a bound for only that exact combined source/gate6/location14
goal—not a stranding proof or a refutation of another resource assignment.  The
approved narrower source/gate6 conjunction also exhausted at the same ceiling
with direct truncation.  Neither result authorizes an automatic next search.
The two Talos test failures were fixed first at D's request. D now reports that
`(test-talos)` runs successfully. Focused assistant checks also passed both
repaired fixtures and all 55 cutoff assertions. No engine behavior changed. Evidence:
`evidence/talos-two-test-fixes-2026-09-21.txt` and its two retained logs.

Read `doc/problems/crelay-topo/Constraint-Continuation-Prompt.txt` for boundaries,
then `Constraint-Restart-Checkpoint.txt` for exact inventories, archive hashes,
restart commands and ledger reconstruction. Do not use memory or old conversations
for puzzle answers. This session's accidental memory exposure was disclosed;
D chose a new session. No further puzzle derivation occurred after that exposure.

**Starting point for the analysis:** the saved keeper candidate at cumulative
**depth 32, nine checkpoints**, not the old depth-19 seed. Agent1 is empty-handed
on the ground at location10. Tray1 holds plate3, keeping gate4 open; connector1
at location9 is paired to transmitter1 and repeater1, both relays red. Plates4/5
are empty, gate6 is closed, switch2 is off and gate7 is closed. Gate8 is still
closed. Ghost agent/tray hold plate1/plate2; ghost connector is at location9,
both boxes at location6. One recorder cycle remains open.

**Audit/probe conclusion:** switch2 access requires physical gate6 to have both
plate4/plate5 witnesses while the agent is empty-handed; source-power tray1 and
connector1 cannot also be assumed to fill those supports.  The two approved
cutoff-12 experiments found neither the full source/gate6/location14 state nor
the narrower source/gate6 state.  They do not identify a missing resource or
prove the branch impossible, irreversibly stranded, or fit for crossing.
**No new search, validation/replay, extractor, code change or ledger restructuring
is authorized.** Substantial searches remain D's to run.

**State and evidence:**
- Main spine: seven of nine found; LK1/LK2/LK3/LK7/LK8 CLOSED,
  LK9/LK4 REALIZED, LK5/LK6 OPEN. Sixteen premises and three bounds in the ledger.
- Eighteen reported searches include three historical restoration searches. The
  two newest are separate cutoff-12 experiments, not ledger links or bounds.
- BD2: crossing cutoff8, coverage UNKNOWN. BD3: crossing cutoff10, truncation T,
  130,956 hits, threads16, GRAPH, symmetry NIL, minimum-steps pruning T.
- Separate source-power phase: 8 actions, 7.164 s, depth27, REALIZED/validated NIL.
- Separate keeper phase: 5 actions, 0.209 s, depth32, REALIZED/validated NIL.
  Power was temporarily interrupted then restored, as its endpoint goal allowed.
- Separate return-access experiment: cutoff12, threads16, no solution for
  `(and (color repeater1 red) (open gate6) (has-location agent1 location14))`;
  truncation T, 779,648 hits, GRAPH, symmetry NIL, minimum-steps pruning T.
- Separate gate6/source experiment: cutoff12, threads16, no solution for
  `(and (color repeater1 red) (open gate6))`; truncation T, 779,445 hits,
  GRAPH, symmetry NIL, minimum-steps pruning T.
- Both experimental phases are preserved separately, not adopted as mandatory
  spine links or main-ledger bounds; neither closes LK5. Their results are in
  the corresponding
  `repeater-source-result-2026-09-21.txt` and
  `keeper-replacement-result-2026-09-21.txt`, plus
  `switch2-return-access-result-2026-09-21.txt` and
  `gate6-source-resource-result-2026-09-21.txt`, under
  `constraint-evidence/`.

**All three archives now exist and were hashed/read as files, not replayed:**
`t10-location15-checkpoint.txt` (7 checkpoints/19 actions),
`t10-repeater-source-checkpoint.txt` (8/27), and
`t10-keeper-checkpoint.txt` (9/32). Keep all three; branch variables remain
`*t10-checkpoint*`, `*t10-source-checkpoint*`, and `*t10-keeper-checkpoint*`.
No speculative endpoint replaces an earlier checkpoint automatically.

D's maximum reasonable search cutoff is **12**. This ceiling is not a new run
or automatic deepening authorization. All prior recommended probes are complete;
no search command is pending. Normally skip independent replay of search-found
phases; retain REALIZED/validated NIL and the final composed-path acceptance.

**Outstanding separate work:** the generic ledger reporter still emits stale
checkpoint commands and overstrong exhaustion wording; use explicit dated
recommendations until a separate fix is approved. See
`evidence/t10-consistency-review-2026-09-21.txt`. The gate8-opening alternative
is unapproved and its old arithmetic inference was corrected. The lk4 schema-gap
candidate awaits D's hand filing. T11 remains approved but deferred. No other
extractor or implementation task is selected.

## Prohibitions that bind every session

Stated here rather than by reference, because a session that misses them
contaminates the experiment silently.

- `doc/problems/<problem>/subgoal-solution-*.txt` is **SEALED ALWAYS**. Never
  open it, for any reason, at any stage. The method is being validated on a
  problem whose solution is already known; opening it destroys the evidence that
  the method derived rather than recalled.
- While specifying or implementing any extractor (C1): do not open
  `Backward-*.txt`, `Forward-*.txt`, or `Initial-Conditions.txt`. They contain
  prior answers, not implementation inputs.
- In the prediction register: sections 4 and 5 are sealed against
  implementation, section 7.3 quotes sealed content, and G9 in the schema-gap
  file is TAG ONLY. At scoring, read only the completed extractor's own value
  paragraph and stop before the next one (M8).
- An assistant that has opened a sealed file must disclose it before writing any
  code, and let D decide whether to continue.
- Use explicit line ranges from the continuation prompt. Do not scan a whole
  register for headings; a boundary scan is a disclosed violation, not a
  shortcut.

**Rules of construction and method (C1–C4, M1–M9)** are stated in the problem's
`Constraint-Prediction-Register.txt` preamble and its continuation prompt. They
are not duplicated here; read them there and treat them as binding.

## Loading the diagnostic

```lisp
(progn (ql:quickload :wouldwork) (in-package :ww))
(stage <problem>)
(load (merge-pathnames "tech/constraint-profile.lisp"
                       (asdf:system-source-directory :wouldwork)))
(report-static-constraint-profile)   ; all generated extractors, S0-S4
(report-role-obligations <scenario>) ; RO, explicit scenario required
```

The continuation prompt carries the generated-profile writer call and the
staging cautions. Staging resets problem settings and state, and crossing the
serial/parallel boundary restages, so set threads before rebuilding any replay
state.

## Working conventions for this method

These are specific to this work. General codebase conventions are in `CLAUDE.md`
and are not restated here.

- D already has Wouldwork running locally. Omit QUICKLOAD/IN-PACKAGE from
  recommended command sequences. A restart sequence begins with `(stage ...)`;
  a live checkpoint continuation does not restage.

- **`tech/constraint-profile.lisp` is a loadable diagnostic**: never named in an
  `include-tech` directive, never an ASDF component, plain Common Lisp in `:WW`.
  No `define-query`, `define-types`, `define-dynamic-relations` or any other DSL
  defining form — a merely LOADed file gets no tech splice.
- **Definition order is callees-first in that file**, which is the reverse of
  this project's usual high-level-first order. It is reloaded by hand after every
  edit, so a forward reference costs a style warning on each load and a
  screenful of them hides the one warning that matters. Extractors still sit in
  contiguous blocks, each ending in its own reporter, with the entry points last.
  Avoid `LABELS` and `FLET`; prefer named top-level helpers.
- **No problem object names anywhere in the file (C3).** Named substrate
  interfaces are allowed when declared and justified. A component needing
  problem-specific terms takes them from a caller-supplied scenario, which is
  data, not code — RO set that precedent.
- **Generated output is never hand-edited (M2).** Fix the code and regenerate.
- **Editing hazard.** Wouldwork symbols are dollar-prefixed. Any edit tool that
  applies regex-substitution rules to its replacement text will mangle them.
  Prefer a whole-file write or a shell heredoc with a quoted delimiter. The
  Filesystem extension has also failed outright here with a JSON-schema dialect
  error on every call; if it does, use the shell rather than spending turns
  retrying it.
- **Where work runs.** Focused diagnostic staging is cheap. Substantial search is
  the user's to run locally and needs explicit approval; the interactive phase is
  designed around that split, with results reported back into the ledger.
- **Technical choices belong to the assistant, not to D.** Placement, naming, data
  shapes, algorithm selection, file organization, output formatting and the like are
  decided and acted on without asking. The decision and its one-line reason go in the
  task entry so a later session can see why. Asking D to arbitrate a technical detail
  spends a turn on something D has already delegated.
- **Read the profile before recommending a cutoff.** A spine arc says which device must be
  open; S4's controller row for that device says what opening it costs, including whether
  its controller is reachable only from somewhere the spine never goes. T10 spent a
  23-second exhaustion learning that, having built a decomposition from the spine alone
  while S4's rows sat in the same file. A recommendation whose cutoff was chosen without
  reading the controller row is a guess wearing a number.
- **Consequential strategic choices are D's**, and are put to D in plain language: a
  short list of options, what each costs, and a recommendation with its reason. Then
  wait. A choice is strategic when it changes what the method claims, what is sealed or
  scored, the contamination boundary, the build order, or what a later session is
  committed to. When in doubt about which kind a choice is, make the technical call and
  say in one line that it was made and can be reversed.

## File map

| File | Authoritative for |
|---|---|
| `doc/constraint-method/Constraint-Implementation-Plan.md` | task state and build order (this file) |
| `doc/constraint-method/Status-Algebra-and-Record-Schema.md` | the ledger's record schema, status algebra, retraction and exhaustion rules (T1) |
| `tech/constraint-ledger.lisp` | the whole interactive phase: the ledger (T2), the recommender (T3), the ingester (T4) and the question generator (T5) |
| `doc/constraint-method/evidence/` | method-level run, load and check evidence, the counterpart of a problem's `constraint-evidence/` |
| `doc/problems/<p>/Constraint-Continuation-Prompt.txt` | reading boundaries, approvals, hashes, per-problem status |
| `doc/problems/<p>/Constraint-Prediction-Register.txt` | sealed extractor specifications, committed predictions, scores |
| `doc/problems/<p>/Constraint-Static-Profile.txt` | generated extractor output; never hand-edited |
| `doc/problems/<p>/Constraint-Schema-Gaps.txt` | questions the schema failed to ask, stated domain-generally |
| `doc/problems/<p>/Constraint-Abstract-Model.txt` | bodies x roles x segments, the budget, and what is deliberately not claimed |
| `doc/problems/<p>/Constraint-Role-Obligations.txt` | the RO audit, its specification, and the G14 fix design |
| `tech/constraint-profile.lisp` | the extractors themselves |

The register is append-only in section 7 and sealed in sections 3–6; this plan
never edits it.

## What this file is

The coordination document for building the constraint-led method to the point
where it produces a validated plan. It is method-level, not per-problem: the
components below serve every topo problem, and crelay-topo is the instance they
are being built and exercised against.

Per-problem session handoffs — `Constraint-Continuation-Prompt.txt` in each
problem directory — carry a short pointer to the current task and nothing more.
They keep their existing job: reading boundaries, sealed-file prohibitions,
approvals granted, hashes, and per-problem status. Task state lives here so it
survives past any one problem and so the handoff stays readable in full at the
start of every session.

## What this file is not

- **Not the schema-gap record.** A gap is a question about problem structure the
  schema failed to ask, recorded domain-generally in each problem's
  `Constraint-Schema-Gaps.txt`. A gap may *spawn* a task — G14 did — but the two
  lists stay separate, or the portable learning record fills with engineering
  chores and stops travelling to the next problem.
- **Not a prediction register.** Sealed extractor specifications, committed
  predictions and scores stay in each problem's
  `Constraint-Prediction-Register.txt`, under its own append-only rules.
- **Not a substitute for approval.** Listing a task here does not authorize it.

## Objective

A static analysis phase followed by an interactive phase:

1. **Static.** A pure function of the staged spec. One call, no user input,
   producing the constraint profile.
2. **Interactive.** A loop combining constraint output with the user's problem
   intuition: the analysis reports what it cannot determine, the user supplies a
   milestone guess or answers an enumerated question, the analysis recommends a
   bounded search, the user runs it locally, and the result comes back into the
   ledger at its correct grade.
3. **Closure.** Concrete links composed and validated under
   `VALIDATE-ACTION-SEQUENCE`. Per M4, a forced abstract skeleton alone is an
   intermediate result, not a plan witness.

Two invariants constrain everything below:

- **A user guess is a premise, not a fact.** Everything derived under it is
  conditional on it and must be retractable without discarding results that did
  not depend on it.
- **An exhausted search is a cost bound, not a refutation.** Grade 3 licenses no
  impossibility claim. Constraint-derived impossibility and search-derived
  exhaustion stay visibly different kinds of record. Confusing them is the error
  that produced T22 and the pre-test-12 alcove verdict.

## Conventions

- **Ids are stable and append-only.** Never renumber. A task that dies is CLOSED
  with a reason, not deleted. Same discipline as G1–G14 and register §7.x, so a
  score or a gap can cite a task id years later and still mean the same thing.
- **Acceptance criteria are written before the work starts**, not settled after
  seeing the output.
- **Status** is one of PROPOSED, SPECIFIED, APPROVED, IN PROGRESS, COMPLETE,
  BLOCKED, CLOSED.
- **Approval is recorded separately from status**, because this project advances
  by explicit per-step approval and a specified task is not an approved one.
- **Contamination scope is stated per task.** Build work on the ledger and the
  interaction loop needs no sealed-file access at all; extractor work is bound by
  C1. Saying so per task is cheaper than re-deriving it from the global
  prohibitions each session.
- **Evidence is named by file**, not described.

## Board

| Id | Task | Status | Approval | Depends on |
|---|---|---|---|---|
| T0 | Wire the coordination scheme | COMPLETE | approved | — |
| T1 | Status algebra and record schema | COMPLETE | approved, design only | T0 |
| T2 | Realization ledger | COMPLETE | approved in session, 2026-09-20 | T1 |
| T3 | Search recommender | COMPLETE | approved in session, 2026-09-20 | T2 |
| T4 | Result ingester | COMPLETE | approved in session, 2026-09-20 | T2, T3 |
| T5 | Question generator | COMPLETE | approved in session, 2026-09-20 | T1, T2 |
| T6 | Mechanized budget arithmetic | PROPOSED | not requested | — |
| T7 | S5 height and reach lattice | PROPOSED | not requested | — |
| T8 | S6 beam sightline table | PROPOSED | not requested | T7 |
| T9 | S7 landmark graph and orderings | PROPOSED | not requested | — |
| T10 | End-to-end closure on crelay-topo (7/9 spine links found; return-access probe exhausted at cutoff12) | IN PROGRESS | access audit/probe completed; no further search approved; tests fixed and full suite passed per D; cutoff ceiling12 | T2–T5 |
| T11 | Implement and score the G14 fix | APPROVED, DEFERRED | approved, four parts | — |

**T11 is listed last because it runs last, not because it is unapproved.** D
deferred it on 2026-09-20 so the architecture tasks settle first. It is
per-problem cleanup and blocks nothing here.

**Build order rationale.** T1–T5 before T6–T9, against the instinct to finish the
extractors first. A thin end-to-end loop over the *existing* S0–S4 and RO output
tests whether the conception holds — whether constraint output plus user
intuition plus short searches converges — while the analysis is still weak enough
that user guesses carry real weight. That is the intended operating regime, not a
degraded one. Finishing S5–S7 first buys a better profile with still no path to a
validated plan, and teaches nothing about the loop.

**The risk in that order** is building the ledger before knowing everything the
later extractors will need to put in it. Mitigated by keeping each link record's
premise list open-ended from the start (T1).

## T0 — Wire the coordination scheme

**Goal.** This file exists and the per-problem handoff points at it.

**Acceptance.** `Constraint-Continuation-Prompt.txt` carries a CURRENT TASK block
near the top naming the current task id, its status, the single next action, and
this file as the authoritative record. The prompt's existing sections are
otherwise unchanged, and it is regenerated last per M3.

**Contamination scope.** None. Documentation only; nothing executed.

**Status.** COMPLETE, 2026-09-20. **Evidence.** This file, and the CURRENT TASK
block at the top of `doc/problems/crelay-topo/Constraint-Continuation-Prompt.txt`.

## T11 — Implement and score the G14 fix

**Goal.** The per-problem work already specified, approved and committed before
this plan existed: RO currently prints S1's device-state premises unfiltered by
the stated view, and the approved fix labels them without changing what RO
allocates.

**Why it carries a late id.** Ids are identifiers, not an order. This task was
in flight when the plan opened; it is listed first on the board because it runs
first, and it keeps its own id forever.

**Specification.** `Constraint-Role-Obligations.txt` section 7, and commitments
P1–P8 in register 7.28. Both are approved; neither is to be re-specified, and
7.28's predictions must not be revised once coding starts — they were committed
before any of the fix's code existed, which is the point of the experiment.

**Acceptance.** The four approved parts, in the order the handoff states:
implement `REPORT-ROLE-AXIOMS` and its helpers inside the RO block and switch
`REPORT-ROLE-DEVICE` to call it; implement section 7.8's fifteen acceptance
cases and run them in a clean image *before* any staged run; one staged run with
7.26's scenario unchanged; regenerate the profile and check P7's two hashes.
Then append the score as 7.29, stating plainly that the fix has no sealed value
paragraph and is therefore a weaker experiment per M9, and recording the actual
order of the acceptance run and the staged run.

**Scope trap.** If the implementation finds it must change an allocation to make
the index work, it has exceeded its scope: stop and ask.

**Contamination scope.** C1 applies. No sealed-file reading, no solve and no
action search is authorized by the existing approval.

**Status.** APPROVED, DEFERRED 2026-09-20 by D until the architecture tasks are
settled. **Approval.** Granted, all four parts, in one turn; the deferral does
not withdraw it and it must not be re-asked when this resumes.

## T1 — Status algebra and record schema

**Goal.** Define, in writing and before any code, how a link, a premise, a bound
and a question are each tagged, and what happens to dependents when a user
assumption is retracted.

**Why first.** It shapes every record in T2–T5. Retrofitting dependency tracking
after the ledger exists means rewriting the ledger.

**Must settle.**
- The provenance vocabulary: derived (with grade 1–4), user-asserted, search-measured.
- How a record names the premises it rests on, with the premise list open-ended.
- Retraction semantics: what is invalidated when a user premise is withdrawn, and
  what survives.
- How an exhaustion result is stored so it can never be read as impossibility.
- How a conditional obligation is distinguished from an established one — RO
  already draws this line; the schema must preserve it rather than flatten it.

**Acceptance.** A written specification sufficient for T2 to be implemented from
it without further design decisions, plus worked examples of a retraction and of
an exhaustion result.

**Contamination scope.** None required. Design work against RO's existing output
shape and the abstract model's grade vocabulary.

**Status.** COMPLETE, 2026-09-20. **Evidence.**
`doc/constraint-method/Status-Algebra-and-Record-Schema.md`: the provenance
vocabulary and its grade rules (section 4), the clause-shaped premise list
(section 5), the status algebra separated from computed standing (sections 7
and 8), retraction with its empty-clause cascade (section 9), the four
independent guards on an exhaustion result (section 10), eighteen
well-formedness rules (section 11), the file format (section 12), the API T2
implements (section 13), and the two worked examples acceptance required
(sections 14 and 15).

**Approval.** Granted 2026-09-20 for the written specification only.
Implementing it is T2 and needs its own approval. The specification is D's to
accept or amend; an amendment is recorded in that file, not here.

**Open question left for D**, stated in that file's section 8 and worth D's eye
because it changes what every later report says: RO prints `status CONDITIONAL`,
and the schema splits that one word into a stored lifecycle `:status` and a
computed `standing`. The ledger's reporter therefore prints
`standing CONDITIONAL` beside RO's `status CONDITIONAL`. If D prefers one word
across both, the change belongs in the specification before T2 starts.

## T2 — Realization ledger

**Goal.** The interactive phase's state: links, their status, the premises each
depends on, and the evidence closing each. Program-written and user-amendable.

**Acceptance.** Reader, writer and reporter exist; a ledger round-trips through
the file without loss; retraction of a user premise correctly invalidates exactly
its dependents; the reporter prints open links, their blocking premises, and what
would close each. Domain-general per C3.

**Placement.** DECIDED: `tech/constraint-ledger.lisp`, its own loadable
diagnostic on the same terms as `constraint-profile.lisp` — never
`include-tech`'d, not an ASDF component, plain Common Lisp in `:WW`. The reason
is that every entry point in `constraint-profile.lisp` reads what staging built,
while the ledger reads no staged data at all: it is loadable and usable with no
problem staged, it persists across sessions, and it is hand-amended between them.
Keeping it separate means a ledger session never stages anything and the
hash-locked S0–S4 generator is never touched by ledger work. Neither file calls
the other, so they load in either order.

**Contamination scope.** None required. Nothing sealed was opened; nothing was
staged; no extractor ran.

**Status.** COMPLETE, 2026-09-20, pending D's own run of the acceptance checks.

**Approval.** Granted in session by D on 2026-09-20 ("let's continue with the next
task here and now"), together with the standing instruction that technical choices
are the assistant's to make. Placement was therefore decided rather than put to D.

**Evidence.** `tech/constraint-ledger.lisp`, the T2 block;
`doc/constraint-method/evidence/ledger-checks-2026-09-20.lisp`, 24 cases and 90
assertions; `ledger-checks-run-2026-09-20.txt`, the run record;
`ledger-sample-report-2026-09-20.lisp` and its `.txt` output.

**Acceptance, against the criteria above.** Reader, writer and reporter exist
(`READ-REALIZATION-LEDGER`, `WRITE-REALIZATION-LEDGER`,
`REPORT-REALIZATION-LEDGER`). A ledger round-trips through the file `EQUAL`,
including a key the reader does not recognise, and a second write is idempotent
(case 21). Retracting a user premise invalidates exactly its dependents and
leaves everything else untouched (cases 3–8). The reporter prints open links,
their blocking premises and what would close each (case 22). C3 holds: no problem
object name appears in the file.

**Disclosure on where the checks ran.** In an isolated cloud sandbox with a stock
SBCL, not on lumpy, from copies of the two files at the same relative paths.
Wouldwork was not loaded, nothing was staged, no extractor ran and no generated
output was written. `COMPILE-FILE` reported zero warnings and zero style
warnings, which is what verifies the callees-first order — the acceptance script
evaluates forms as source and would not catch a forward reference. D's run on
lumpy Powershell is the authoritative one:

    cd d:/quicklisp/local-projects/wouldwork
    sbcl --noinform --no-userinit --no-sysinit --script doc/constraint-method/evidence/ledger-checks-2026-09-20.lisp

and, in the usual image,

    (progn (ql:quickload :wouldwork) (in-package :ww))
    (load (merge-pathnames "tech/constraint-ledger.lisp" (asdf:system-source-directory :wouldwork)))

**Not an extractor, and therefore not scored in the register.** T2 reads no staged
data and makes no prediction about a problem, so it has no sealed value paragraph
and nothing to commit to register section 7. M9 exists for a component that has
commitments and a staged run; this one has neither. Its acceptance checks were run
first and in a clean image regardless, because that is the order M9 makes a habit
of.

**Five underspecifications found and settled** during implementation are listed in
section 16 of the schema document, which was corrected in the same turn.

## T3 — Search recommender

**Goal.** For each open link, emit a concrete runnable recommendation: subgoal
expression, start point, depth cap, thread setting.

**Acceptance.** Recommendations are runnable as printed, without the user editing
them. Each carries the premises it rests on and states what a success and what an
exhaustion would each establish — written before the run, so the interpretation
is not chosen after seeing the outcome.

**Notes.** Staging resets problem settings and state; crossing the
serial/parallel boundary restages. Concrete realization uses short
`SOLVE-SUBGOAL` chains, unquoted goals and `*threads*` 0. Recommendations must
not silently deepen searches.

**Threads, amended by D on 2026-09-20 to 16 after staging, and what the engine
permits.** The default is now 16 — but only for a link that states its own start.
`VALIDATE-CONTINUATION-PRECONDITIONS` in `src/ww-goal-chaining.lisp` signals "Goal
chaining requires single-threaded mode" unless `*threads*` is 0, and the
one-argument `SOLVE-SUBGOAL` is goal chaining, so a parallel chain search is not a
slow command, it is an error. `SOLVE-SUBGOAL-FROM-FORM`, the two-argument form,
states in its own docstring that it runs in any thread mode. The recommender
therefore defaults to 16 for a stated start and 0 for a chain link, and signals if
asked for a parallel chain search. Nothing is coerced silently, and the printed
cautions name which form is in play.

**Placement.** DECIDED: a contiguous callees-first block at the end of
`tech/constraint-ledger.lisp`, not a third file. The recommender reads ledger
records and writes one back; it needs no staged data and shares the ledger's
accessors. Keeping the interactive phase in one loadable diagnostic means one
hand-load for T2–T5.

**Contamination scope.** None required to build, and none used. Running a
recommendation is search and still needs its own approval; nothing was run.

**Status.** COMPLETE, 2026-09-20. **Approval.** Granted in session by D.

**How the acceptance criteria are met.**
- *Runnable as printed.* The command block is the five lines a session actually
  enters, in order: the load preamble, `(stage <problem>)`, `(ww-set *threads* n)`,
  `(ww-set *depth-cutoff* c)`, then the `SOLVE-SUBGOAL` form — one-argument when
  the link searches the active chain, two-argument when it states a start. The
  goal prints unquoted. Case 25 asserts the five strings exactly; case 26 asserts
  the two-argument form.
- *Not editable, or not printed.* A link missing its goal, its start or a positive
  cutoff prints what is missing and no command at all. A half-command the user must
  edit is not the recommendation that was committed to (case 31).
- *Carries its premises.* The stored recommendation records the dependency closure
  and the live guesses as they stood when it was written, so a later retraction
  shows as drift rather than being absorbed.
- *Both readings, before the run.* `LEDGER-SUCCESS-READING` and
  `LEDGER-EXHAUSTION-READING` are templates, not prose authored per link, and both
  are stored on the record for T4 to file against (case 29).
- *No silent deepening.* A cutoff greater than the deepest existing attempt's
  signals unless `:deepen t` is passed; the recommendation then names the bound it
  goes past and the report says so (case 27).
- *No uncapped search by default.* `*depth-cutoff*` 0 or negative means no cutoff
  at all in this engine, so a missing or zero cutoff signals rather than defaulting
  (case 28).

**Cautions printed with every recommendation**, from reading the engine rather than
from memory: settings go after `(stage ...)` because staging resets them; crossing
the serial/parallel boundary with `(ww-set *threads* ...)` forces a system rebuild;
the goal is unquoted because a quoted goal installs `(quote ...)`, which the
translator reads as trivially true; `*depth-cutoff*` 0 means no cutoff; and the
two-argument form discards any active goal chain, which one `(ww-undo)` restores.

**Evidence.** `tech/constraint-ledger.lisp`, the T3 block;
`doc/constraint-method/evidence/ledger-checks-2026-09-20.lisp`, now 31 cases and
130 assertions; `ledger-checks-run-2026-09-20.txt`; and
`ledger-sample-report-2026-09-20.txt`, which shows a recommendation including a
deliberate deepening past an existing bound. Ran in the assistant's sandbox with a
stock SBCL, zero warnings from `COMPILE-FILE`; D's run on lumpy is authoritative.

## T4 — Result ingester

**Goal.** Take back what the user's local run produced and file it at the correct
grade, propagating consequences.

**Acceptance.** A found segment closes its link and records its action sequence
as evidence; an exhaustion is stored as a cost bound relative to its start state
and is structurally incapable of closing a link as impossible; a surprise is
routed to the problem's schema-gap file with the question that should have been
asked, per M5. Ingestion never hand-edits generated output; M2 holds.

**Placement.** DECIDED: the same file, `tech/constraint-ledger.lisp`, after T3's
block. The ingester writes ledger records and reads a recommendation; it needs
nothing else.

**Contamination scope.** None required, and none used. Nothing was staged, no
search was run, and no search is authorized.

**Status.** COMPLETE, 2026-09-20. **Approval.** Granted in session by D.

**One departure from the acceptance wording, stated rather than quietly resolved.**
The criterion says a found segment "closes its link". T1's schema and M4 both say a
find that has not composed under `VALIDATE-ACTION-SEQUENCE` is an intermediate
result, not a plan witness. The ingester therefore makes a find `:realized` and
`:closed` only when it is handed `:validated t`, and the link's standing stays
CONDITIONAL until then however established its premises are. If D wants the looser
reading, say so and it is a one-line change; the tighter one is what M4 asks for.

**How the rest is met.**
- *An exhaustion closes nothing.* It becomes a `:bound` record, which WF8 forbids in
  `:closed-by` and `:refuted-by`, and its statement is generated from a template so
  it cannot say in prose what its type forbids it to mean. It attaches to the link
  as an attempt, and the link stays open (case 34).
- *Filed against the committed reading.* The ingester signals for a link carrying no
  recommendation, and copies the recommendation's exhaustion text onto the bound
  verbatim. An outcome filed without a prior reading would be a reading chosen after
  the fact, which is the error the whole schema exists to prevent (case 33).
- *A find files its actions or is refused* (case 36).
- *M5.* A surprise is filed alongside the outcome as a question marked
  `:gap-candidate`, and `REPORT-LEDGER-GAP-CANDIDATES` prints it for hand-appending
  to the problem's `Constraint-Schema-Gaps.txt`. Nothing writes that file: it is
  hand-maintained, and M2's regeneration rule does not cover it (case 37).
- *Two bounds against one link stand side by side*, neither replacing the other
  (case 39).

**The substantive finding, and it came from the engine rather than from the plan.**
`*DEPTH-CUTOFF-TRUNCATED*` records whether the cutoff cut off a node that still had
successors. When it is NIL the reachable space from that start was actually
exhausted within the cutoff, which is a stronger reading than a truncated one —
still a cost bound relative to that start, still not an impossibility. An
exhaustion under symmetry or repeated-state pruning is weaker again, since it
excludes solutions in pruned branches. Both are recorded on the bound and printed
beside it rather than inferred later from the cutoff alone. Neither changes the
grade.

**Evidence.** `tech/constraint-ledger.lisp`, the T4 block;
`doc/constraint-method/evidence/ledger-checks-2026-09-20.lisp`, now 39 cases and
174 assertions; the run record and the sample report, which now ingests an
exhausted run with a surprise and prints the gap candidate.

## T5 — Question generator

**Goal.** Convert underdetermination into an enumerated question the user can
answer from intuition, rather than prose the user must interpret.

**Acceptance.** Every unresolved premise RO currently narrates is emitted instead
as a question with a candidate answer set and a default of "unknown"; an answer
is recorded as user-asserted provenance per T1; answering never silently
upgrades a conditional obligation to an established one.

**Placement.** DECIDED: the same file, after T4's block.

**Contamination scope.** None required, and none used. `tech/constraint-profile.lisp`
is READ AS TEXT by one acceptance case; RO is not loaded, staged or run anywhere.

**Status.** COMPLETE, 2026-09-20. **Approval.** Granted in session by D.

**Coverage, and how it is kept honest.** Eleven templates, one per unresolved
premise RO narrates: the five on its `unresolved premises:` line (segment
necessity, ghost absence, agent occupancy, replacement witnesses, recorder
transitions), the three restrictions its per-support line leaves open (reach,
elevation, occupancy history), the availability it prints as UNKNOWN, the segment
it prints as NONE STATED, and the device it prints as having no declared control
aggregate. Each template records in `:narrated-as` the fragment of RO's printed
text it stands for, and case 42 asserts that fragment is still present in
`tech/constraint-profile.lisp`. A change to RO's narration that the table has not
followed fails the checks rather than going unnoticed — which is the only way a
claim of the form "every premise RO narrates" stays true a month from now.

**Three answer kinds, because not all underdetermination is a choice.** `:one-of`
for the eight that are; `:subset-of` for availability, whose answer is which of a
stated pool are actually free; `:stated` for the segment description, which is
prose nobody has written down. Squeezing the last two into a choice, or leaving
them out, would have made the coverage claim false in the place it is easiest not
to notice. `:unknown` is accepted in every kind, writes no premise and leaves the
question open.

**Nothing is upgraded.** An answer goes through `ANSWER-LEDGER-QUESTION`, which
writes a `:user-asserted` premise carrying `:asked-as`, and makes the question rest
on it. Everything above recomputes to CONDITIONAL by section 8 of the schema, and
no status is touched (case 43). Generation is idempotent, since ids are
append-only and a duplicate could never be cleaned up afterwards (case 40).

**C3 holds.** Every template is domain-general. The segment description, the
support names, the witness pool and the device names arrive as data in the caller's
scenario — the same plist RO takes — because this component is never handed a
staged problem to compute them from.

**Evidence.** `tech/constraint-ledger.lisp`, the T5 block;
`doc/constraint-method/evidence/ledger-checks-2026-09-20.lisp`, now 47 cases and
307 assertions; the run record and the sample report, which now prints the
questionnaire.

## T6 — Mechanized budget arithmetic

**Goal.** An extractor consuming S1 and S2 that emits the impossibility
constraints currently derived by hand as AM1–AM3: total role cost over disjoint
support sets against the occupant pool, and the resulting "not all of these
devices" statements, with their grades.

**Working name.** Not S5, S6 or S7. Those specifications are sealed in each
problem's register and must not be enlarged or amended; RO set the precedent for
adding a component under its own name.

**Acceptance.** Regenerates AM1–AM3 on crelay-topo from the profile alone,
including the per-segment budget readings, with each claim carrying its grade and
its premises. No problem object names, per C3.

**Contamination scope.** C1 applies: written from the instance and tech semantics
only. No `Backward-*`, `Forward-*`, `Initial-Conditions.txt`, or
`subgoal-solution-*` access.

**Status.** PROPOSED. **Approval.** Not requested.

## T7 — S5 height and reach lattice

**Goal.** Implement the sealed S5 specification.

**Acceptance.** As specified in the problem's register, section 3.

**Contamination scope.** C1 applies. **S5 has a sealed value paragraph**, so this
is a stronger experiment than RO's or G14's: commit predictions before coding,
score the first completed run, and at scoring read only S5's own paragraph in
section 4, stopping before the next extractor, per M8.

**Status.** PROPOSED. **Approval.** Not requested.

## T8 — S6 beam sightline table

**Goal.** Implement the sealed S6 specification. Depends on S5's achievable tops.

**Acceptance.** As specified in the problem's register, section 3. Resolves AM8
and the conditional beam competition of AM4 only if the sightline semantics
actually establish them; a run that does not establish them says so.

**Contamination scope.** C1, and the sealed-paragraph discipline of T7. G9 is TAG
ONLY and must not be opened to obtain this information.

**Status.** PROPOSED. **Approval.** Not requested.

## T9 — S7 landmark graph and orderings

**Goal.** Implement the sealed S7 specification.

**Acceptance.** As specified in the problem's register, section 3, with the
relaxation in force stated explicitly in the output. S7 cannot silently supply a
simultaneous role requirement or a segment premise.

**Contamination scope.** C1, and the sealed-paragraph discipline of T7.

**Status.** PROPOSED. **Approval.** Not requested.

## T10 — End-to-end closure on crelay-topo

**Goal.** Run the whole loop on crelay-topo and compose the resulting concrete
links into a validated sequence.

**Acceptance.** `VALIDATE-ACTION-SEQUENCE` accepts the composed links from the
initial state to the goal. Every premise the chain rests on is recorded with its
provenance, and every user-asserted premise is listed separately from the derived
ones, so the result's dependence on intuition is visible rather than absorbed.

**Notes.** crelay-topo is contaminated: a solution already exists. A validated
plan here demonstrates that the loop closes; it does not demonstrate that the
loop discovered anything. The honest test of discovery is the first uncontaminated
problem, and that belongs in its own task when this one completes.

**Contamination scope.** C1 throughout. `subgoal-solution-*.txt` remains SEALED
ALWAYS. Searches require their own approval.

**Status.** IN PROGRESS, 2026-09-20. **Approval.** Granted in session by D, including
the searches, after being told plainly that T10 needs them.

**What exists after the first half.**
`doc/problems/crelay-topo/Constraint-Realization-Ledger.txt`, built by
`constraint-evidence/build-realization-ledger-2026-09-20.lisp`: ten derived premises,
six links and nine open questions. Every link is OPEN. Nothing has been searched.

**The decomposition, and where each part of it came from.** The agent's initial
location and the goal are the instance's own `DEFINE-INIT` and `DEFINE-GOAL`, which
C1 names as a permitted source. S3's regions place the start and the goal in the
first and last regions of the adjacency spine, and the spine joins them through five
intermediate regions across six controlled devices in one fixed order. One link per
crossing, in that order. Nothing was taken from a hand analysis, and no sealed file
was opened.

**One design decision inside the milestones.** Each milestone goal is a DISJUNCTION
over the target region's endpoints, not a chosen representative. S3 warns that two
endpoints of one region need not be mutually reachable, since each mode carries a
predicate the extractor cannot evaluate. Picking one endpoint would smuggle in a
premise the quotient does not carry.

**Ghost presence is `:unknown` in every record**, deliberately. Whether a recorder
fork is needed to hold a support is one of the things this loop exists to settle;
stating `:absent` would settle it by assumption, which is the failure RO's own
scenario handling is built to refuse.

**Threads.** The chain runs at `*threads*` 0 and cannot run otherwise — chained
milestones are goal chaining, and `VALIDATE-CONTINUATION-PRECONDITIONS` signals
unless `*threads*` is 0. D's 16 applies to a standalone probe from a stated start.

**Disclosure.** The builder ran in the assistant's sandbox, not on lumpy. It is
deterministic and its dates are fixed strings, so re-running it on lumpy must
reproduce the ledger byte for byte; the SHA-256 and byte count are recorded in
`constraint-evidence/ledger-build-2026-09-20.txt`. Nothing was staged and no search
was run there or anywhere.

**Progress, 2026-09-20.** Four searches run, all on lumpy. lk1 found at depth 4 by
recorder fork — a ghost holds the required support while the live agent crosses — and
validated at four actions. lk2 found in one action and validated. lk3 found in one action,
not yet validated. Cumulative chain depth 6. Evidence:
`constraint-evidence/lk1-ingest-2026-09-20.txt` and `runs-ingest-2026-09-20.txt`.

**Three findings the runs produced, recorded because two of them are criticisms.**

1. *The replayable path is shorter than the transcript.* Four actions against a seven-line
   display: the pause, the ghost's return walk, the recorder stop and the resume are
   display structure and the closure is synthesized. A session reading the transcript as
   the action sequence would build a wrong composition. Filed as pr13.
2. *Two of the six devices were already open in the initial state*, one from an occupant
   the instance places on its support at the start and one from the switch pair's initial
   setting — which is why two crossings cost one walk each. **This is grade 1 and was
   derivable from the profile before any search ran.** The ledger did not carry it, and two
   measured runs are what made it visible. Filed as pr14 with that omission stated in its
   premise gap. A cheap crossing is not evidence the method found something; it is evidence
   the decomposition was built without a fact the analysis already had.
3. *A goal chain is session state and does not survive a REPL restart.* The per-link
   recommendation prints only the next line, which is right in a live session and useless
   after a restart. `REPORT-CHAIN-REPLAY` now prints the whole ordered sequence from a cold
   image, marking what is settled.

**Two refusals and a rejected command, all the assistant's, none forced through.** WF9
refused a dependency that would have made a grade-2 link rest on a search result. The
recommender printed a staging preamble for chained milestones, which would have discarded
the chain being continued. And a wrong retrieval command was rejected by the REPL, which
turned up the engine fact below.

**The engine composes the chain itself.** `COMMIT-GENERIC-GOAL-CHAIN` sets
`*SOLUTION-PATHS*` to NIL on a mid-chain commit by design; a phase's own path lives in the
session's phase record. Only the final commit publishes a cumulative path, built by
`MAKE-GOAL-CHAIN-CUMULATIVE-SOLUTION` over every phase, and it restores `*START-STATE*` to
the chain's origin. **T10's composition check is therefore one
`VALIDATE-ACTION-SEQUENCE` call after the last milestone, not a hand-built
concatenation.**

**Still to do.** Run the three inserted milestones, then lk4–lk6, finish with
`(solve)`, and validate the cumulative path from the restored origin. Until that call
succeeds, T10 is not complete.

**The first exhaustion, 2026-09-20.** lk4 at cutoff 8: no solutions, 23.1 seconds, 9.25 GB,
and the engine reported the cutoff TRUNCATED the space. Filed as a grade-3 bound, truncated,
carrying the reading committed before the run. The link stays open; WF8 makes that
structural.

**What the profile already said.** S4's controller row for that arc's device: its controller
is a switch with exactly one manipulation reach candidate, in a region off the spine; the arc
into that region is a graph cut and the region is a cul-de-sac; and the device guarding it
demands two supports, both APPROACH-ONLY and KEEPER-OBLIGATED, neither of which can be the
agent. **The milestone was never one crossing**, and eight actions was never going to cover
it. Filed as pr15 and pr16, both grade 1, both derivable before any search ran.

**The honest reading of that.** A constraint-led method that burns 23 seconds to be told to
read its own static profile is not yet leading with constraints. The bound is real and
correctly filed, but the finding is that the decomposition was built from the spine alone
while S4's controller rows sat in the same file — the second time in T10 that a grade-1 fact
already in hand surfaced only after a measurement (pr14 was the first). Both are recorded as
criticisms in their own premise gaps, and the working convention above now says to read the
controller row first.

**The refinement.** lk4 split into three preceding milestones — hold the two supports, reach
the switch, throw the switch — with the walk through the device left as lk4 at cutoff 4. Ids
are append-only, so the new links append to the file; `:chain-order` puts them where they
belong in the chain and the replay follows chain order, not file order.

**Acceptance suite.** 51 cases, 333 assertions, `COMPILE-FILE` clean. D's runs on lumpy
passed at 90, 130, 174 and 307 as components landed.

**T10 validation update, 2026-09-20.** D restored the first three milestones after
a cold restart (three replay searches, cumulative depths 4, 5, 6), then validated lk3
from its phase source: SUCCESS-P T, ACTION-COUNT 1, goal checked and satisfied.
LK1-LK3 are now CLOSED; T10 remains IN PROGRESS. Next: lk7 at the already
recommended cutoff 8. Evidence: constraint-evidence/lk3-validation-2026-09-20.txt.
Append ingest-lk3-validation-2026-09-20.lisp after ingest-lk4-bound-2026-09-20.lisp
to reproduce the ledger. The ingestion ran standalone without user initialization,
loading no problem and running no search; ledger checks and readback passed.
The two-plate goal alone does not exclude the agent as a witness; any find still
needs its endpoint inspected before claiming readiness for the excursion.


T10 UPDATE, 2026-09-20 -- LK7 VALIDATED; LK8 NEXT
D found and validated lk7 in five additional actions, cumulative depth 11.
SUCCESS-P T, ACTION-COUNT 5, goal checked and satisfied. Five of nine links
are CLOSED: lk1, lk2, lk3, lk7. T10 remains approved and IN PROGRESS.
The stated goal is met by tray1 on plate4 and agent1 on plate5; the stronger
non-agent witness intent was NOT established. Do not infer excursion readiness.
Next approved probe: lk8, agent1 at location14, cutoff 6, threads 0, from
the current live chain. No restaging. S4 controller rows were read again.
There have now been nine reported searches including three cold-restoration
replays; the original five-search count above describes the earlier session.
Evidence: constraint-evidence/lk7-validation-2026-09-20.txt. Append
ingest-lk7-validation-2026-09-20.lisp after ingest-lk3-validation-2026-09-20.lisp
when reproducing the ledger. Standalone ingestion checked and read back the
ledger successfully; no problem was loaded or searched by the assistant.
No sealed material was opened. This update supersedes older pending-lk7 text.


T10 UPDATE, 2026-09-20 -- LK8 VALIDATED; LK9 NEXT
D found and validated lk8 in six additional actions, cumulative depth 17.
SUCCESS-P T, ACTION-COUNT 6, goal checked and satisfied. Five of nine links
are CLOSED: lk1, lk2, lk3, lk7, lk8. T10 remains approved and IN PROGRESS.
Connector1 replaced agent1 on plate5; tray1 holds plate4; agent1 reached
location14. The recorder cycle remains open. This is a measured endpoint,
not a general claim that these witnesses are always available.
Next approved probe: lk9, (switched-on switch2), cutoff 4, threads 0,
from the current live chain. Do not restage. S4's controller row was reread.
Ten reported searches include three cold-restoration replays.
Evidence: constraint-evidence/lk8-validation-2026-09-20.txt. Reproduce by
appending ingest-lk8-validation-2026-09-20.lisp after the lk7 ingestion.
Standalone ingestion passed ledger checks and readback; no problem loaded
or search run by the assistant. No sealed material opened. This update
supersedes older pending-lk8 text. Full-chain validation remains outstanding.


T10 UPDATE, 2026-09-20 -- LK9 FOUND; ROUTINE PHASE VALIDATION SKIPPED
D clarified: normally skip validation of Wouldwork search-found solutions;
validation is useful for logically developed action sequences. Do not request
routine per-phase replay for subsequent search finds. File them REALIZED with
validated NIL; retain the final composed-path check in T10's acceptance.
LK9 found in one action, toggling switch2; cumulative depth 18. Agent1 remains
at location14. Gate7 is open and physical gate5 is closed; recording gate5
remains open. Five links CLOSED, lk9 REALIZED, three links still OPEN.
Next: lk4, (has-location agent1 location15), cutoff 4, threads 0, current chain.
No restaging. Eleven reported searches include three restoration replays.
Evidence: constraint-evidence/lk9-found-2026-09-20.txt. For ledger reproduction
append ingest-lk9-found-2026-09-20.lisp after the lk8 ingestion. Standalone
ingestion passed checks/readback; no problem loaded or search run by assistant.
No sealed material read. This update supersedes older requests for routine
phase validation and pending-lk9 instructions. T10 remains IN PROGRESS.


**Restart handoff, 2026-09-20.** lk4 found in one action from location14 to
location15 after switching, cumulative depth 19. Filed REALIZED, no separate
replay, as D requested. Current instructions are in CURRENT TASK and the restart
checkpoint; dated progress paragraphs above are history, not next-step commands.
No assistant search run. Ledger ingestion checks/readback passed.

**Standalone checkpoint workflow, 2026-09-21.** D approved replacing repeated
serial restoration searches with persisted endpoints and two-argument searches at
threads 16. Implemented in `src/ww-search-checkpoint.lisp`, sharing the existing
subgoal-progress archive and phase replay implementation. `solve-subgoal` accepts
a checkpoint object and returns a new one only on success; exhaustion retains its
input. Historical phases are retained for final composed validation, with no
automatic predecessor retries. No silent deepening is introduced.

Technical decision: reuse symbolic action-replay archives, rather than persisting
stage-local hash tables. This preserves exact endpoints across fresh staging and
thread-mode recompilation. Search-found phases are saved without routine replay;
import reconstructs them by replay, and `validate-search-checkpoint` performs the
final original-goal check with `VALIDATE-ACTION-SEQUENCE` and exact endpoint equality.
The earlier instruction to finish this workflow with ordinary `(solve)` is superseded:
search the final goal explicitly from the checkpoint, then validate the checkpoint.

Evidence: `doc/constraint-method/evidence/search-checkpoint-checks-2026-09-21.log`
(34 assertions plus three legacy persistence claims); synthetic recorder tests
continue an open cycle at threads 16 and validate all four actions from the origin.
`doc/problems/crelay-topo/constraint-evidence/checkpoint-replay-2026-09-21.log`
records replay-only reconstruction and fresh-stage round trip of the 19-action
prefix, checking the documented location15 inventory. The generated archive is
`constraint-evidence/t10-location15-checkpoint.txt`; its builder reads only the
permitted realization ledger. Archive settings describe the migration replay
environment; historical search settings remain in the ledger.

T10 remains IN PROGRESS. Twelve reported crelay-topo searches, seven found links,
and their ledger statuses are unchanged; replay for restart migration does not
silently re-ingest or upgrade them. Existing ledger recommendation command fields
describe the former serial chain; they are not current execution instructions.
Before the next LK5 run, record its new exact checkpoint start and threads 16
alongside the bounded recommendation.

**Local restart confirmed, 2026-09-21.** D reported seven checkpoints and 19
actions restored with no search. Beam/controller semantics and connector1's
plate5 commitment were inspected before issuing the unchanged cutoff-8 LK5
experiment from that exact archive at threads 16. The dated recommendation
records sources, unresolved sightline feasibility, and outcome interpretations.
No new search or milestone result is recorded yet. Await the local result.

**Parallel coverage correction completed, 2026-09-21.** LK5's subsequent
cutoff-8 exhaustion exposed missing worker-side truncation measurement. D
approved the fix: worker-local successor witnesses now aggregate into the
existing flag without shared writes in workers. The ledger accepts T/NIL/UNKNOWN,
defaults missing measurements to UNKNOWN, and no longer equates reliable NIL
with unpruned full-space exhaustion. This conservative wording also accounts
for other pruning. Existing serial cutoff behavior is unchanged.

55 focused engine assertions and 346 ledger assertions passed. LK5 is OPEN
with BD2, unknown historical coverage, all reported metrics, and raw COMPLETE/NIL
preserved. The recommendation was transcribed from its pre-run evidence file,
not regenerated after the outcome. Ledger checks and readback passed.
Evidence: `doc/constraint-method/evidence/cutoff-reporting-fix-2026-09-21.txt`
and `doc/problems/crelay-topo/constraint-evidence/lk5-bound-ingestion-2026-09-21.log`.
The next cutoff increase is a proposal only; implementation approval did not
authorize deepening. T10's final composed-path validation is still outstanding.

**Subsequent cutoff-10 approval and consistency review, 2026-09-21.** D approved
one LK5 crossing probe at cutoff 10, threads 16, and requested a repository check.
Source wiring, checkpoint behavior, BD2 provenance, retained test evidence and
archive/profile/register-prefix hashes were checked. No Lisp or search ran.
The restart note's unapproved gate8-opening alternative is not a prerequisite;
its claimed opening-cost inference is unsupported by BD2 and the endpoint facts.
The generic ledger command/wording reporters still need a separate implementation
step. Use the explicit dated cutoff-10 recommendation, starting with STAGE to
reload the corrected engine, then threads 16 and archive import. Transcribe that
pre-run record before next ingestion; preserve the last measured cutoff-8 record
and BD2 in the meantime. Review: `evidence/t10-consistency-review-2026-09-21.txt`.

**Fresh-session handoff, 2026-09-21.** Subsequent source and keeper probes are
complete; all three checkpoint archives are saved and their hashes verified.
The latest keeper candidate has nine checkpoints and 32 actions. D reports
`(test-talos)` now succeeds after the two fixture repairs. The already approved
next task is read-only switch2 return-access/resource analysis from that candidate,
in a fresh session using only permitted evidence. No search is pending or newly
authorized; the reasonable cutoff ceiling is 12. Current Task above and the
refreshed continuation/restart documents supersede historical next-step commands.

**Switch2 return-access audit, 2026-09-21.** The approved read-only audit is
complete.  From the keeper endpoint, agent1 can only manipulate switch2 from
location14; the R3-to-R4 passage requires physical gate6 (both plate4 and plate5)
and screen1 admits only an empty-handed agent.  Switch2 off keeps physical gate5
open and gate7 closed; turning it on reverses those states, while the recording
view remains separate.  Tray1 on plate3 and connector1 at location9 are measured
source commitments, not free gate6 keepers.  Ghost cargo is a route candidate,
not an established replacement supply.  This neither proves stranding nor
restores access.  The single proposed next experiment, requiring new approval,
is a threads-16 cutoff-12 standalone search from `*t10-keeper-checkpoint*` for
`(and (color repeater1 red) (open gate6) (has-location agent1 location14))`.
It preserves the candidate and tests the direct access condition; success still
requires endpoint resource/view inspection.  Evidence:
`constraint-evidence/switch2-return-access-audit-2026-09-21.txt`.

**Switch2 return-access probe, 2026-09-21.** D approved the proposed standalone
threads-16 cutoff-12 probe.  From the nine-checkpoint keeper archive it found
no solution for simultaneous red repeater power, physical gate6 open, and agent1
at location14.  The engine reported direct cutoff truncation with 779,648 cutoff
hits, GRAPH pruning, symmetry NIL, and minimum-steps pruning T; the unchanged
checkpoint was returned.  This is a grade-3 bound for that exact experimental
goal, not an LK5 bound, stranding proof, or resource-assignment refutation.
It is retained separately, so the main ledger remains at three bounds and the
seven-of-nine main-link count is unchanged.  No further search is authorized.
Evidence: `constraint-evidence/switch2-return-access-result-2026-09-21.txt`.

**Gate6/source resource proposal, 2026-09-21.** To separate the exhausted
location14 component from the resource question, the next candidate experiment
is one threads-16 cutoff-12 standalone search from the keeper checkpoint for
`(and (color repeater1 red) (open gate6))`.  It would test coexistence of
source power and gate6 support only; it is neither a necessary condition claim
nor a crossing/final-plan claim.  Evidence:
`constraint-evidence/gate6-source-resource-proposal-2026-09-21.txt`.

**Gate6/source resource probe, 2026-09-21.** D approved that exact probe.  It
found no solution at threads 16 and cutoff 12 from the unchanged keeper archive;
direct cutoff truncation was observed with 779,445 cutoff hits, GRAPH pruning,
symmetry NIL, and minimum-steps pruning T.  It is a grade-3 bound for the
exact source/gate6 conjunction, not a claim about deeper states, universal
necessity, stranding, or a particular failed resource.  It remains separate
from LK5, leaving the main ledger at three bounds and seven found main links.
No further search is authorized.  Evidence:
`constraint-evidence/gate6-source-resource-result-2026-09-21.txt`.

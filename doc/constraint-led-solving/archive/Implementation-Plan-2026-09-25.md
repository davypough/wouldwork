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

1. **Take the CURRENT TASK below.** It is the next waiting task, always. If it
   says no task is approved, present its candidates to D and wait.
2. **Read its entry in full**, plus the sources that entry names, plus
   `doc/problems/<problem>/Constraint-Continuation-Prompt.txt` for the problem it
   touches — that prompt is authoritative for approvals already granted,
   verification hashes and per-problem status. Its reading boundaries, permitted
   line ranges and sealed-file prohibitions are lifted by the Reading policy
   below; where the two disagree, the Reading policy governs.
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

**READING POLICY CHANGED (D, 2026-09-25).** Nothing in the wouldwork directory
is sealed any longer unless D specifically requests it. See "Reading policy"
below; it supersedes every sealed-file prohibition, line-range boundary and
grant requirement in this file and in the per-problem continuation prompts.

**T17 COMPLETE (2026-09-25). Next: D approves T18 (document restructure), the
first task in the agreed build order T18 → T19 → T20 → T21 → T22 → T23 →
T24, with T25 alongside T18 and T26 any time.** The post-mortem,
`doc/constraint-method/Post-Mortem-2026.md`, is the design basis for all of
them. T18–T26 are PROPOSED: each needs acceptance criteria written before work
and D's approval. The T17 block below is history.

**T17 APPROVED (D, 2026-09-25): post-mortem of the constraint-led method's
development, in a fresh session, before any new problem is opened.** Read the
T17 entry below, then open the discussion with D. It is a discussion task:
no code, instance, ledger or generated-file changes, and no staging or search.
This supersedes the T10-complete block below, kept as history.

**T10 COMPLETE (2026-09-25). No task is approved to start; D chooses the next
one.** Both acceptance clauses are met: `validate-search-checkpoint` accepted the
composed 87-action path (80 hand-derived, D-directed + 7 search-found) from the
initial state to the goal, and the chain's user-asserted premises are recorded
as pr18-pr22 (`ingest-t10-user-premises-2026-09-25.lisp`; the ingest listed
premises by provenance). lk5/lk6 stay OPEN as spine links: the final route was
never searched as those crossings. Grade: exposed analysis -- the loop closes;
discovery is untested. Remaining candidates: open the first uncontaminated
problem (the honest test of discovery, reserved by T10's notes). The D1-D3
files were tidied on 2026-09-25 at D's request: headers and the evidence file
mark them SUPERSEDED; D then validated D3 (73 actions, all T; stack at
location15 unlit, gate8 closed). Action lists unchanged. Archives and cold restore: the restart
checkpoint's 2026-09-25 block. Earlier 2026-09-25 steps follow as history.

**D chose option 1 (2026-09-25): close T10 by its written acceptance.** One
approved search, threads 16, cutoff 12, from the location15 checkpoint for the
original goal, then `validate-search-checkpoint`. Files (D runs, in order):
`constraint-evidence/build-c3-location15-checkpoint-2026-09-25.lisp` (replay of
the 80 validated actions, export `t10-c3-location15-checkpoint.txt`, no search)
and `constraint-evidence/run-t10-final-search-2026-09-25.lisp` (import, one
search, export `t10-final-checkpoint.txt` on success, validate). No deepening
or retry is approved.
**Result (2026-09-25):** the build exported 80 actions; the cutoff-12 search ran
out of memory (no solution, no bound; 21st reported search). Next: D chooses the
rerun configuration (options in the evidence file's OPTION 1 section and the
session reply). A 10-action solution is known from this checkpoint. D chose option 1: one rerun, same
settings, cutoff 10 (run file edited in place; the header records the change).
**Rerun result (2026-09-25):** solution found, a 7-action final leg (87 total),
exported to `t10-final-checkpoint.txt`; `validate-search-checkpoint` returned
SUCCESS-P NIL because phase 1 (replay-built, plain forms) and phase 2 (search,
timestamped forms) are mixed and `normalize-validation-actions` checks only the
first entry. D to choose the fix; details in the evidence file.
D approved the engine fix: `normalize-validation-actions`
(`src/ww-solution-validation.lisp`) now normalizes each entry on its own.
`validate-t10-final-checkpoint-2026-09-25.lisp` (import + validate, no search)
then passed: SUCCESS-P T, 87 actions, goal T. This block supersedes the
2026-09-24 late block below, which is kept as history.

- Question 8 answered by D with a new cycle 3, replacing parts A-C and the
  D1-D3 files (those exist on disk; their validation is not recorded).
  `validate-c3-alt-2026-09-25.lisp`, 80 actions, validated by D: SUCCESS-P T,
  goal T. Endpoint: agent1 at location15 holding tray1 with box1 and a paired
  connector1; receiver1 ACTIVE; gates 1, 2, 3, 4, 7, 8 open; ghost on plate3,
  connector1* at location9 paired with (repeater1 transmitter1); cycle 3 open.
- The problem goal is `(has-location agent1 location19)`, behind gate9
  (plates 6, 7, 8). The final-leg file adds 10 hand-derived actions: through
  gate8, tray1 on plate6, connector1 on plate7, box1 on plate8, through gate9.
- Evidence: `constraint-evidence/b2-ghost-tray-loc5-check-2026-09-24.txt`
  (section ALTERNATIVE CYCLE 3). Ledger unchanged; pr18 and D's cycle-3 plans
  remain un-ingested.
- Open strategic point for D after the final leg: T10's written acceptance is
  an approved final-goal search from a checkpoint plus
  `validate-search-checkpoint`; a hand-validated complete route is a
  different kind of evidence. D decides how T10 closes.

**Previous block (2026-09-24 late, history).**

**T10 is ACTIVE again (resumed by D, 2026-09-24, late session). Next step:
D answers Question 8 in a fresh session -- how the stack gets from location12
to location15 and the beam is lit.** This block supersedes the 2026-09-24
candidate list below, which is kept as history.

**Where T10 stands.**
- B2 reopened: D named the ghost-held tray at location5 as the elevation
  resource. Every stage since has been hand-derived and then checked with
  `VALIDATE-ACTION-SEQUENCE` from a fresh stage; no search was run.
- Validated chain, each file a superset of the one before (all under
  `doc/problems/crelay-topo/constraint-evidence/`):
  `validate-b2-ghost-tray-2026-09-24.lisp` (23: box1 out of the alcove),
  `validate-b2-box-plate2-rev-2026-09-24.lisp` (31: cycle 2 closed; box1 on
  plate2, connector1 on plate1, tray1 on ground at location7),
  `validate-c3-part-a/-b/-c-2026-09-24.lisp` (41, 53, 60).
- **Current endpoint (60 actions, cycle 3 open):** agent1 at location12
  holding tray1; box1 on tray1; connector1 on box1, paired with repeater1 and
  receiver1; agent1* at location12, tray1* on the ground there; box1* on
  plate2, connector1* on plate1; gates 1, 3, 5 open (physical and recording);
  switch1 on, blower1 turning. Reproduce by loading
  `validate-c3-part-c-2026-09-24.lisp` (replay only).
- `validate-b2-box-plate2-2026-09-24.lisp` (39) also validated but is NOT the
  chosen endpoint (D corrected the arrangement).
- Evidence and every D decision: `constraint-evidence/b2-ghost-tray-loc5-check-2026-09-24.txt`.
- User-asserted premises from this session, NOT yet ingested into the ledger:
  pr18 (connector1 on plate1, tray1 at location7 when the ghost-tray cycle
  starts) and D's cycle-3 handoff plan. Main ledger unchanged: seventeen
  premises, nine links, three bounds; lk5/lk6 OPEN.
- Engine facts learned (record, do not rediscover): a grounded agent cannot
  place onto a held tray (top 3/2, placement reach 1; S5 matrix NO) -- place
  from location20 via blower1; CONNECT-CONNECTOR always sets the connector down
  (no pair-while-holding action); pairing uses structural LOS from any vantage
  the agent can move to; generated termini lists run in reverse terminus order,
  e.g. `(repeater1 receiver1)`; HOLDING is bijective and prints as
  HOLDING1/HOLDING2 in `DATABASE`.

**Optional state probe (2026-09-24, D approved; usefulness not yet judged).**
`tech/constraint-state-probe.lisp`, loadable diagnostic like the profile file.
`(report-applicable-actions <action-list> &key action object)` replays a validated
prefix from the staged initial state and lists every next action GENERATE-CHILDREN
accepts, as replay-readable phrases (symmetry pruning off). One step ahead only; no
search. Separate file because the profile is static and this reads a live state.
Candidate for a wider diagnostic procedure if it proves useful.

**D's working rule (2026-09-24).** Write out and validate any path that can be
derived by hand; reserve searches for hypotheses that cannot be written out
and validated. Work in small stages, one question to D at a time.

**Question 8 (open, for D).** From location12: reaching location14 (switch2)
needs gate6 (plates 4 and 5) and an empty-handed agent through screen1; switch2
on opens gate7 and closes gate5. How does the stack reach location15 and the
beam get lit?

**Next-session entry.** Read this block, the continuation prompt's CURRENT
HANDOFF, and the evidence file above. Then ask D for Question 8's answer and
continue in validated stages. The Filesystem extension fails here; use the
shell on lumpy.

**Superseded candidate list, 2026-09-24 (history).**
No task was approved to start. The next step was D's choice among the
remaining candidates below. T14 and T15 completed on 2026-09-24. This section,
refreshed that day, replaces all earlier next-step text, which is preserved
verbatim under "T10 handoff history" at the end of the T10 entry.

**Where the work stands.**
- T0–T9 and T11–T16 are COMPLETE. T16 added RC, the relay chain table
  (G17's fix); the profile was regenerated 2026-09-24 (1344 lines,
  SHA-256 `294DC749…`). G17 stays OPEN for the S4/T6 joins. T15 (2026-09-24, candidate 7) re-derived
  AM4/AM8: gate8 open now requires plate3 (S6-derived); the in-cycle beam
  cost is settled only by hand. T13 was closed on 2026-09-22 when D chose
  full T6 implementation; T6–T9 followed the same day. T14 (2026-09-24) made
  the ledger's generic recommendations match the checkpoint workflow, so
  future recommendations can be printed by the reporter rather than written
  by hand.
- T10 is IN PROGRESS but paused. No run is pending and none is approved. Its
  B2 construction branch is closed: the nominal six-action route was rejected
  by the construction audit, and the follow-up found no elevation resource
  reachable from B1. Reopening B2 needs D to name a new, separately justified
  elevation resource and approve its read-only check. B3/B4 are not approved.
- Main ledger: seventeen premises, nine links, three bounds; qn10 answered
  (pr17, user-asserted, 2026-09-24). lk1/lk2/lk3/lk7/lk8
  CLOSED, lk9/lk4 REALIZED, lk5/lk6 OPEN — seven of nine links found.
- Twenty reported puzzle searches: eighteen through the gate6/source probe
  (including three historical restoration searches), plus the separate B1 and
  B2 experiments. Archive imports and failed setup launches are not searches.
- Four checkpoint archives, kept distinct, under
  `doc/problems/crelay-topo/constraint-evidence/`: `t10-location15-checkpoint.txt`
  (7/19), `t10-repeater-source-checkpoint.txt` (8/27), `t10-keeper-checkpoint.txt`
  (9/32) and `t10-b1-checkpoint.txt` (1/9, separate fresh-origin experiment).
  All four SHA-256 hashes were rechecked on 2026-09-24 and match the restart
  checkpoint.
- D's maximum reasonable search cutoff is 12. It authorizes no run and no
  automatic deepening.
- Exposure: the 2026-09-21 event led to a fresh session; the 2026-09-22
  supplied-memory exposure was disclosed and D authorized continued analysis.
  T10 analysis from 2026-09-22 on is therefore exposed, not a clean prediction
  experiment. The 2026-09-24 session was supplied no puzzle memory and opened
  no sealed material.

**Candidates for D's choice — items 5–6 open, none approved or given a task id:**
1. *Documentation consistency pass* — DONE 2026-09-24, all 21 findings
   dispositioned. Record: `evidence/doc-consistency-review-2026-09-24.txt`. One
   hand-analysis follow-up it deliberately did not do: re-derive AM4/AM8 in
   `Constraint-Abstract-Model.txt` from the S5/S6 output.
2. *Regenerate the static profile* — DONE 2026-09-24 by D. New SHA-256
   `83772F5D…`, 938 lines; S0–S4 byte-identical to before. Details in the
   continuation prompt under PROFILE REGENERATED.
3. *Correct the generic ledger reporter* — DONE as **T14**, 2026-09-24.
4. *File the lk4 schema-gap candidate* — DONE 2026-09-24: filed as **G16** at
   the end of `Constraint-Schema-Gaps.txt` with D's approval. Two follow-ups
   were D's: qn10 is now answered (D ran
   `constraint-evidence/ingest-qn10-answer-2026-09-24.lisp`; pr17 added,
   checks and readback passed). The T15 relay-sightline candidate was checked
   against G9, register section 3 (S6's spec asks for no station-to-station
   rows) and section 6 (read under D's one-session grant; no match) and filed
   as **G17**. Disclosures are recorded in the continuation prompt.
5. *Resume T10* — requires a newly justified elevation resource for B2, or a
   different baseline, and its own approval.
6. *Open the first uncontaminated problem* — the honest test of discovery that
   T10's notes reserve for its own task.
7. *Re-derive AM4/AM8* — DONE as **T15**, 2026-09-24. It raised one question
   for item 4's kind of filing: S6 has no connector-to-connector sightline
   table, which is what leaves AM4's in-cycle reading at HAND grade.

**Standing items unchanged by this refresh.** T10's final acceptance remains an
approved final-goal search from a chosen checkpoint followed by
`validate-search-checkpoint` with SUCCESS-P, GOAL-CHECKED-P and GOAL-SATISFIED-P
all T. Search-found phases stay REALIZED with validated NIL unless D asks for a
replay. The gate8-opening alternative stays unapproved and its old arithmetic
inference withdrawn. G15 is OPEN; its checklist exists but has not been applied
to any traversal.

**Next-session entry.** Read this section, the continuation prompt's CURRENT
HANDOFF and the restart checkpoint's current block. Then present the
candidates above to D and wait. Read no new source material, and stage,
search, replay or validate nothing, until D chooses and approves one.

## Reading policy (D, 2026-09-25)

**Nothing in the wouldwork directory is sealed.** Every file may be read in
full, at any stage, without a grant from D and without a disclosure. This
includes `doc/problems/<problem>/subgoal-solution-*.txt`, `Backward-*.txt`,
`Forward-*.txt`, `Initial-Conditions.txt`, every section of the prediction
register (including sections 4, 5 and 7.3), and G9 in the schema-gap file.

**Exception.** D may seal a specific file, section or range for a specific
purpose. Such a seal is recorded here, with its date, scope and reason, and
binds until D lifts it. There are currently none.

**What this changes, and what it does not.**
- Line-range boundaries, "DO NOT READ" ranges, TAG ONLY rules and grant
  requirements in the per-problem continuation prompts are no longer binding.
- The register's edit rules stand: sections 3–6 are not edited, and section 7
  is append-only. Generated output is still never hand-edited (M2).
- Consequence for scoring: a prediction committed, or a score made, after its
  value has been read is a regeneration check, not an independent prediction.
  The score says so, as T8/T9 already did. This is a statement of what the
  evidence shows, not a restriction on reading.
- The prohibition text that stood here before 2026-09-25, and the per-task
  contamination scopes recorded in completed task entries below, are history:
  they describe the rules those tasks were done under.

**Rules of construction and method (C1–C4, M1–M9)** are stated in the problem's
`Constraint-Prediction-Register.txt` preamble and its continuation prompt. They
are not duplicated here; read them there and treat them as binding, except
where they restrict reading (C1's file prohibitions, M7's tag-only rule, M8's
bounded reads), which the Reading policy above lifts.

## Loading the diagnostic

```lisp
(progn (ql:quickload :wouldwork) (in-package :ww))
(stage <problem>)
(load (merge-pathnames "tech/constraint-profile.lisp"
                       (asdf:system-source-directory :wouldwork)))
(report-static-constraint-profile)   ; all generated extractors: S0-S7 and T6
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
- **What to ask D.** D answers questions about the problem domain and about
  how Wouldwork searches it. D does not answer questions about the contents
  of the constraint-method documents; checks against those documents are the
  assistant's. (Stated by D, 2026-09-24; the grant requirement for closed
  ranges was removed by the Reading policy, 2026-09-25.)
- **Session timing.** The assistant suggests starting a new session at an
  opportune point, judged by the next task's difficulty and the context
  remaining, after making the state files current. (D, 2026-09-25.)
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
| `doc/problems/<p>/Constraint-Prediction-Register.txt` | extractor specifications, committed predictions, scores |
| `doc/problems/<p>/Constraint-Static-Profile.txt` | generated extractor output; never hand-edited |
| `doc/problems/<p>/Constraint-Schema-Gaps.txt` | questions the schema failed to ask, stated domain-generally |
| `doc/problems/<p>/Constraint-Abstract-Model.txt` | bodies x roles x segments, the budget, and what is deliberately not claimed |
| `doc/problems/<p>/Constraint-Role-Obligations.txt` | the RO audit, its specification, and the G14 fix design |
| `tech/constraint-profile.lisp` | the extractors themselves |
| `doc/constraint-method/Launch-Configuration-Checklist.md` | the G15 launch-configuration check: required evidence and result labels (T12) |
| `src/ww-search-checkpoint.lisp` | standalone search checkpoints: `export-search-checkpoint`, `import-search-checkpoint` (by replay), `validate-search-checkpoint` |
| `doc/search-strategies/standalone-checkpoints.md` | user-level description of the standalone checkpoint workflow |
| `doc/problems/<p>/Constraint-Realization-Ledger.txt` | the problem's interactive-phase ledger; program-written, reproduced by the ingest scripts in order |
| `doc/problems/<p>/Constraint-Restart-Checkpoint.txt` | archive inventory and hashes, cold-restore commands, ledger reconstruction order |

The register is append-only in section 7 and not edited in sections 3–6 (they
are readable; see the Reading policy); this plan never edits it.

## What this file is

The coordination document for building the constraint-led method to the point
where it produces a validated plan. It is method-level, not per-problem: the
components below serve every topo problem, and crelay-topo is the instance they
are being built and exercised against.

Per-problem session handoffs — `Constraint-Continuation-Prompt.txt` in each
problem directory — carry a short pointer to the current task and nothing more.
They keep their existing job: approvals granted, hashes, and per-problem
status. (Their reading boundaries and sealed-file prohibitions were lifted by
the Reading policy, 2026-09-25.) Task state lives here so it
survives past any one problem and so the handoff stays readable in full at the
start of every session.

## What this file is not

- **Not the schema-gap record.** A gap is a question about problem structure the
  schema failed to ask, recorded domain-generally in each problem's
  `Constraint-Schema-Gaps.txt`. A gap may *spawn* a task — G14 did — but the two
  lists stay separate, or the portable learning record fills with engineering
  chores and stops travelling to the next problem.
- **Not a prediction register.** Extractor specifications, committed
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
- **Reading scope is unrestricted by default** (Reading policy, 2026-09-25). A
  task states a contamination scope only when D has sealed something for it.
  Earlier per-task contamination scopes are kept as history.
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
| T6 | Mechanized budget arithmetic | COMPLETE | approved 2026-09-22 via T13's selection; eight acceptance assertions passed | T13 (selection only) |
| T7 | S5 height and reach lattice | COMPLETE | approved 2026-09-22; first miss and correction score recorded | — |
| T8 | S6 beam sightline table | COMPLETE | approved 2026-09-22; disclosed regeneration score recorded | T7 |
| T9 | S7 landmark graph and orderings | COMPLETE | approved 2026-09-22; disclosed miss recorded | — |
| T10 | End-to-end closure on crelay-topo (87-action composed path validated from the initial state to the goal: 80 hand-derived + 7 search-found) | COMPLETE | 2026-09-25; premises pr18-pr22 user-asserted; exposed, not discovery | T2–T5 |
| T11 | Implement and score the G14 fix | COMPLETE | approved, four parts | — |
| T12 | Specify the G15 launch-configuration check | COMPLETE | approved in session, 2026-09-22 | — |
| T13 | Select and approve a concrete application of G15 | COMPLETE | selected and approved T6 implementation, 2026-09-22 | T12 |
| T14 | Bring the ledger's search recommendations up to date | COMPLETE | approved in session, 2026-09-24; D's runs passed 346 + 54 | T3, T4 |
| T15 | Re-derive AM4/AM8 from the S5/S6 output | COMPLETE | approved in session, 2026-09-24 (candidate 7) | T7, T8 |
| T16 | RC relay chain table (G17's proposed fix) | COMPLETE | approved in session, 2026-09-24; D's run passed 4 | T7, T8, G17 |
| T17 | Post-mortem of the constraint-led method's development (architecture, strategy, effectiveness, weak points, improvements) | COMPLETE | 2026-09-25; all five sections agreed by D | T10 |
| T18 | Document restructure to the phased strategy (post-mortem 4.1) | PROPOSED | not requested; build order 1 | T17 |
| T19 | I5 Mechanic coverage: static contract per tech mechanic, UNCOVERED report | PROPOSED | not requested; build order 2 | T18 |
| T20 | I3 Coupling census | PROPOSED | not requested; build order 3 | T19 |
| T21 | I6 Necessity hints | PROPOSED | not requested; build order 4 | T19, T20 |
| T22 | I1 State-based static queries ("from here" report) | PROPOSED | not requested; build order 5 | T19 |
| T23 | I7 Probe battery | PROPOSED | not requested; build order 6 | T22 |
| T24 | I2 Cycle-plan check (T6, RO, RC budgets) | PROPOSED | not requested; build order 7 | T22 |
| T25 | I4 Domain interview (procedure; replaces the question generator) | PROPOSED | not requested; with T18 | T18 |
| T26 | I8 Memory estimate before search | PROPOSED | not requested; any time | — |

**T11 carries a late id but was already in flight when this plan opened.** D
deferred it on 2026-09-20 so the architecture tasks could settle first; it ran
and completed on 2026-09-22. It was per-problem cleanup and blocked nothing
here. (Corrected 2026-09-24: this note previously said T11 is "listed last
because it runs last".)

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

## T18 — Document restructure to the phased strategy

**Goal.** Reorganize the constraint-method documents to support the phased
strategy of `Post-Mortem-2026.md` section 2, per the verdicts and target file
set in its section 4.1, with T25's domain-interview procedure written into the
Method Guide.

**Must include.** Two fixed session entry points (D, 2026-09-25): this plan
for method development, and the new Method Guide for solving a problem. Each
states in its opening lines what a session does on reading it, and points to
the per-problem Handoff. A new session should need only "Find your
instructions at <entry file>".

**Acceptance.** Not yet written. The session that takes T18 writes it from
section 4.1 and presents it to D with the intended approach before changing
any file.

**Status.** PROPOSED. **Approval.** Not requested.

## T17 — Post-mortem of the method's development

**Goal.** Before the first uncontaminated problem is opened, review how the
constraint-led method was built and how it performed on crelay-topo, with D, so
that the next problem starts from a revised design rather than an inherited one.

**Scope of discussion (D's list, 2026-09-25):** overall architecture, strategy,
effectiveness, weak points and improvements, plus anything D adds. Starting
points the session should bring, each backed by named evidence rather than
recollection:
- *Architecture.* The static profile (S0-S7, RC, RO, T6 budget arithmetic) and
  the interactive phase (ledger, recommender, ingester, question generator,
  standalone checkpoints). Which parts carried weight in T10 and which were
  never consulted when they mattered (e.g. the S5 placement matrix before cycle
  3 part A; S4 controller rows before the 23-second exhaustion).
- *Strategy.* Spine-link decomposition vs D's later working rule (hand-derive
  and validate; search only what cannot be written out). Where each paid off.
- *Effectiveness.* What the method derived vs what D supplied: the final chain
  rests on user-asserted pr17-pr22; lk5/lk6 were never closed as spine links;
  the final 7-action leg was search-found at cutoff 10 after cutoff 12 ran out
  of memory. Register scores (hits, partials, misses) and schema gaps G1-G17.
- *Weak points.* Contamination exposure from 2026-09-22 on; process and
  documentation overhead (handoff drift, e.g. the unrecorded D1-D3 files);
  tooling faults (Filesystem extension; the mixed-format validation bug fixed
  2026-09-25); search memory limits.
- *Improvements.* Candidates only. Each agreed improvement becomes a PROPOSED
  task with its own acceptance criteria; none is implemented under T17.

**Reading.** This plan (board, T-entries, conventions), the crelay-topo
continuation prompt and restart checkpoint, the ledger report
(REPORT-REALIZATION-LEDGER, data-only), Constraint-Schema-Gaps.txt,
Constraint-Abstract-Model.txt, Constraint-Role-Obligations.txt,
Status-Algebra-and-Record-Schema.md, Launch-Configuration-Checklist.md, the
static profile, and the evidence files under `doc/constraint-method/evidence/`
and `doc/problems/crelay-topo/constraint-evidence/`. Register: any section, in
full (Reading policy, 2026-09-25).

**Contamination scope.** None, under the Reading policy (D, 2026-09-25): all
files, including subgoal-solution files, register sections 3-6, G9 and the
Backward-*/Forward-*/Initial-Conditions files, may be read.
(Superseded text: subgoal-solution files SEALED ALWAYS; register sections 3-6
and G9 closed without a grant from D; Backward-*/Forward-*/Initial-Conditions
files not opened unless D granted it for a stated comparison.)

**Acceptance.** A written post-mortem, `doc/constraint-method/Post-Mortem-2026.md`,
agreed with D section by section: findings (each citing evidence), a verdict
on each architectural component (keep, revise, drop), and a list of improvements
filed as PROPOSED tasks on this board. The plan's Current Task then names the
next step D chooses (by default, opening the first uncontaminated problem).

**Status.** COMPLETE, 2026-09-25. **Approval.** Requested by D for a fresh
session; discussion and writing only.

**Result.** `doc/constraint-method/Post-Mortem-2026.md`, all five sections
agreed by D. Improvements filed as T18–T26 (PROPOSED) in D's build order.
Decisions made during T17: Reading policy (nothing sealed); prediction
registers dropped and crelay-topo's register frozen; per-problem maximum
search depth from D, all searches at `*threads*` 16 in the checkpoint form;
approving a stage plan pre-approves its searches within that depth.

## T16 — RC relay chain table (G17's proposed fix)

**Goal.** Implement the component G17 proposes: relay-to-relay sightlines,
source-to-receiver relay chains, and each chain's gate set and body cost, so
AM4's in-cycle reading no longer rests on hand geometry and S4's receiver row
has an extractor behind it.

**Working name.** RC. Not S6: S6's specification is sealed and is not amended.
Same precedent as T6 and RO.

**Scope.** A new block in `tech/constraint-profile.lisp`, printed by
`REPORT-STATIC-CONSTRAINT-PROFILE` after S6. (DECIDED during coding: the
block is defined after S4, not S6, because it reuses S4's pressure-clause
helpers and the file is callees-first.) It computes:
1. stations: every location and every connector top achievable there
   (location level plus each S5 placement support's top plus connector
   height), each with its riser cost (ground 0; box 1; held tray 2, the tray
   and its holder) and whether a pressure plate is positioned there;
2. links: station–fixed endpoint and station–station visibility through
   `BEAM-VISIBLE` on copied start states. Required gates are found by testing
   with all gates open and then with each gate closed in turn. This assumes a
   link is blocked exactly when some gate on it is closed (monotone,
   conjunctive), which is checked against S6's full-subset rows;
3. chains: simple paths transmitter → relays → receiver, where the first and
   last relays are connectors (fixed couplings are counted and reported but
   not enumerated), at most one connector per location, at most as many
   connector stations as the connector pool, each repeater at most once;
4. per chain: gate set (rejected if it contains an S1 exclusion pair), LATCH
   when it needs a device its own receiver controls, pressure plates its gates
   need, and bodies: connectors plus risers, those standing on plates, and the
   off-plate count;
5. per receiver: gates common to every bootstrap chain, and the minimum
   off-plate body count overall and per number of connectors used.

**Not in scope.** Recording-view (ghost-only) sightlines; hue conflicts
between several transmitters; fixed-coupling corridors; reachability of any
placement; segment assignment (G8).

**Acceptance.** Written before code, 2026-09-24:
- Loads with no warnings; `COMPILE-FILE` of the profile reports none; no
  problem object name in the new block (C3).
- For every location and top that S6 also tests, RC's station-to-endpoint
  status and required gates equal S6's (this also checks the monotone
  assumption against S6's 512-subset evaluation).
- The report states its premises and exclusions, prints non-NEVER
  station-to-station links, every chain, and the per-receiver summary.
- Predictions committed in register 7.37 before code are scored in 7.38
  after the first run; a miss is recorded, not adjusted.
- The profile is regenerated by D (M2), never hand-edited.

**Contamination scope.** C1 applies. No `Backward-*`, `Forward-*`,
`Initial-Conditions.txt` or `subgoal-solution-*` access.

**Approval.** Granted in session by D, 2026-09-24 ("proposed fix approved",
for G17).

**Status.** COMPLETE, 2026-09-24. D regenerated the profile on lumpy and ran
the check file (4 passed). The new profile — 1344 lines, SHA-256
`294DC749…` — is byte-identical to the sandbox-generated one (register 7.39).

**Results against acceptance (sandbox).** Checked in a sandbox copy of the
repository (src/, tech/, instance) loaded under SBCL; that copy reproduced the
profile's S6 section byte-for-byte and every non-RC section identically.
- `COMPILE-FILE` of the profile: 0 warnings, 0 style warnings. Only additions
  to the file; no existing definition changed.
- `evidence/rc-relay-chain-checks-2026-09-24.lisp`: 4 assertions passed — 177
  station-to-endpoint rows agree with S6 in status, gates and subset count;
  C3 holds; 96 chains classified consistently; the report prints.
- Profile grows from 938 to 1344 lines.
- Register 7.38: R1–R4 and R6 HIT; R5 PARTIAL. The least off-plate count
  is 2, as predicted, but it is also reached by a two-connector chain whose
  first connector stands on plate2 at location7.
- Findings: 96 chains to the receiver — 40 bootstrap, 12 latch (all at
  location21), 44 excluded on {gate5, gate7}. Gate4 is common to every
  bootstrap chain. At least two bodies are off every plate in every bootstrap
  chain. AM4b is now settled by extractor output, not hand geometry.

**Correction found while specifying.** S5's placement supports include a
tray HELD by an agent (top 3/2), and a grounded tray is inert. T15's AM4a
wrongly excluded connector top 5/2 at location9 and allowed a tray between box
and connector. Corrected in the abstract model and the T15 evidence file; the
crossing-time conclusion and the two-body bound are unchanged.


## T15 — Re-derive AM4/AM8 from the S5/S6 output

**Goal.** Candidate 7 of the 2026-09-24 Current Task, left open by the
documentation consistency pass (finding 18). Decide, from the generated S5 and
S6 sections of crelay-topo's `Constraint-Static-Profile.txt`, whether they
settle AM4 (beam competition for the plate budget) and AM8 (gate4 necessity),
and rewrite those two entries of `Constraint-Abstract-Model.txt` accordingly.

**Scope.** Hand analysis only. Inputs: the profile's S1, S2, S5 and S6 sections
(hash-checked first), the instance file, and the beam semantics in
`tech/beam-relay.lisp` and `tech/-beam-substrate.lisp`. Output: a dated evidence
file under crelay-topo's `constraint-evidence/`, dated AM4/AM8 re-derivations in
the abstract model (the original entries kept verbatim above them), then this
plan's board and Current Task, the restart checkpoint's current block, and the
continuation prompt last (M3).

**Not in scope.** Any code, extractor, staging, search, replay or validation;
the register; the ledger; AM1–AM3 and AM5–AM9 except where AM4/AM8 cite them.

**Acceptance.** Written before the abstract model was edited, 2026-09-24:
- Each claim is labelled either S5/S6-DERIVED (following from generated rows
  plus named source premises) or HAND (geometry or reasoning the profile does
  not contain), with its grade and premises; no HAND claim is presented as
  extractor output.
- Where S6 does not settle a question, the entry says so and names what is
  missing, per T8's acceptance.
- AM8's result states separately whether gate4 is necessary for gate8 and
  whether it is necessary for the goal.
- The profile hash matches `83772F5D…` before the analysis; no sealed file,
  register range or G9 content is opened.

**Contamination scope.** None beyond the standing prohibitions. The instance and
tech sources are permitted under C1; nothing sealed is needed.

**Approval.** Granted in session by D, 2026-09-24 ("I choose candidate 7,
re-derive AM4/AM8").

**Status.** COMPLETE, 2026-09-24.

**Results against acceptance.** Profile hash matched `83772F5D…` before the
analysis; nothing sealed was opened. Evidence:
`doc/problems/crelay-topo/constraint-evidence/am4-am8-rederivation-2026-09-24.txt`.
- **AM8, S5/S6-DERIVED, grade 2.** Every S6 row that sees transmitter1 needs
  gate4, and the instance's beam semantics admit no other first link, so gate8
  open ⇒ gate4 open ⇒ plate3 depressed, continuously. Gate4's necessity for the
  *goal* is exactly gate8's, which stays AM5's conditional graph candidate.
- **AM4a, outside a cycle, S5/S6-DERIVED, grade 2.** The only single-connector
  chain is connector1 on box1 at location9; with agent1 at location15 about to
  cross gate8, tray1 must hold plate3 and gates 1, 3, 6, 9 are closed.
- **AM4b, inside a cycle, NOT SETTLED BY S6.** S6 has no connector-to-connector
  sightlines. **AM4c, HAND:** a 2-D segment test finds every such link blocked
  or caught by the gate5/gate7 exclusion, giving at least two bodies off every
  plate while gate8 is open.

**Decided.** The original AM4/AM8 text was kept verbatim with dated RE-DERIVED
blocks under it, following the file's supersede-by-dated-note pattern, so the
earlier conditional claims stay citable.

**Left for D.** Whether the missing connector-to-connector sightline table is a
schema gap to file (that filing is D's), and whether AM4c should be checked by a
staged run instead of by hand.


## T14 — Bring the ledger's search recommendations up to date

**Goal.** Make T3's generic recommendation reporter match the standalone
checkpoint workflow and the corrected exhaustion semantics, so its printout can
be used as written instead of being replaced by hand-written dated
recommendations. Findings 2 and 3 of
`evidence/t10-consistency-review-2026-09-21.txt`.

**Scope.**
1. A link whose `:search-start` is a string names a checkpoint variable. A new
   optional link key, `:search-archive`, gives the saved archive's path relative
   to the repository root. The printed commands are: stage, threads, import the
   archive into the variable, cutoff, settings, `(setf <var> (solve-subgoal
   <var> <goal>))`, then the run-metadata form. A `:continue` link omits stage,
   threads and import but keeps the cutoff. Without `:continue` and without an
   archive, the reporter prints what is missing and no commands.
2. Checkpoint cautions and on-find advice: set threads before import; an
   exhaustion returns the checkpoint unchanged and a find returns a new one;
   export a find to a new archive and keep the old; no routine replay (REALIZED,
   validated NIL); `validate-search-checkpoint` only for the final milestone.
3. Every future exhaustion reading says what the search FOUND under its
   settings and pruning, qualified by measured truncation (T / NIL / UNKNOWN),
   and no longer says a realization does not EXIST within the cutoff.
4. The cold-restart chain printout names the checkpoint-start links and says
   they are restored by archive import, not by re-running the chain.

**Not in scope.** Stored recommendations and bounds in any existing ledger
(history, left as committed); the chain and stated-start command blocks except
for item 3's wording; the load-preamble line those blocks print.

**Acceptance.** Written before the work, 2026-09-24:
- For a checkpoint link the printed commands, read as Lisp forms, equal the
  forms of the dated LK5 cutoff-10 recommendation
  (`doc/problems/crelay-topo/constraint-evidence/lk5-cutoff10-recommendation-2026-09-21.txt`),
  so they are runnable as printed; the `:continue` variant prints only the
  cutoff, search and metadata forms.
- Checkpoint cautions and on-find advice contain no goal-chain cautions and no
  routine-replay instruction.
- The new exhaustion reading contains neither "exists" nor any of the bound
  lint words.
- Every existing ledger acceptance assertion still passes, except any that pin
  the old wording; those are listed before they are changed.
- New checks cover items 1–4; `COMPILE-FILE` reports no warnings; C3 holds.
- `doc/problems/crelay-topo/Constraint-Realization-Ledger.txt` still reads and
  writes back byte-identical.

**Placement.** DECIDED: inside T3's block of `tech/constraint-ledger.lisp`, and
new acceptance cases in a separate dated check file under `evidence/`, so the
2026-09-20 suite remains the record of what T2–T5 were accepted against.

**Contamination scope.** None required. No sealed file, no staging, no search.

**Status.** COMPLETE, 2026-09-24. Implemented and first checked in the
assistant's sandbox (stock SBCL, no Wouldwork loaded); D's authoritative run on
lumpy then passed both suites (346 and 54 assertions), loaded the file in the
live image without warnings, and printed crelay-topo's own LK5 recommendation
in full with the checkpoint commands and the BD2 deepening line.
**Approval.** Granted in session by D. Findings 2 and 3 of
`evidence/t10-consistency-review-2026-09-21.txt` are resolved; its advice to
use hand-written dated recommendations instead of the reporter is superseded
for recommendations written after 2026-09-24.

**Left as is, noted for a later touch.** The report's heading over the on-find
advice still reads "on a find, the action sequence is here:", which fits the
older start kinds better than a checkpoint start; older acceptance cases may
pin it, so it was not changed inside T14.

**Results against acceptance.**
- `COMPILE-FILE` of `tech/constraint-ledger.lisp`: zero warnings, zero style
  warnings. No problem object name appears in it (C3).
- New suite `evidence/ledger-reporter-checks-2026-09-24.lisp`: 54 assertions
  passed. It reads the LK5 cutoff-10 recommendation's hand-written forms and
  checks the generated commands equal them; covers the `:continue` variant, the
  missing-archive refusal, cautions, on-find advice, the new exhaustion wording,
  the chain-replay note, and the crelay-topo ledger's byte-identical round trip.
- 2026-09-20 suite: 346 assertions passed after exactly two were replaced, both
  pinning the old wording ("not that lk4 is impossible", "cannot close or refute
  a link"); their replacements check the same intent in the new wording and are
  marked in place.

**Two findings made during the work, both fixed inside this scope.**
1. The reporter crashed on crelay-topo's own LK5 recommendation: its
   hand-transcribed `:deepens` holds the bound id (`bd2`), not the `(bound
   cutoff)` pair the recommender stores. The reporter now accepts either and
   reads the cutoff from the bound. The ledger file is unchanged.
2. A `:continue` link may have no archive, so the on-find export path falls
   back to the problem's `constraint-evidence/` directory rather than the
   repository root.

**Technical decisions.** A string start means a checkpoint variable, because
the only string start ever used (LK5) is one; the stored recommendation now
also records `:archive`; the schema document's link keys and section 16 were
updated to match.

## T0 — Wire the coordination scheme

**Goal.** This file exists and the per-problem handoff points at it.

**Acceptance.** `Constraint-Continuation-Prompt.txt` carries a CURRENT TASK block
near the top naming the current task id, its status, the single next action, and
this file as the authoritative record. The prompt's existing sections are
otherwise unchanged, and it is regenerated last per M3.

**Contamination scope.** None. Documentation only; nothing executed.

**Status.** COMPLETE, 2026-09-20. **Evidence.** This file, and the CURRENT TASK
block at the top of `doc/problems/crelay-topo/Constraint-Continuation-Prompt.txt`.

## T13 — Select and approve a concrete application of G15

**Goal.** Choose the next bounded application of G15 without silently turning
its portable specification into a B2 reopening, a construction audit, or an
extractor project.

**Acceptance.** D chooses one named traversal or resource proposal, the
applicable G15 report form is copied into the approved task's scope, and the
task records whether it is documentation-only, source analysis, or
implementation. No source inspection or runtime operation occurs before that
approval.

**Scope.** The selection is strategic. Present the three choices named in
CURRENT TASK; do not infer a preferred resource or construction from the
closed B2 evidence.

**Contamination scope.** None for the selection. The chosen task defines its
own reading boundary and contamination scope.

**Status.** COMPLETE, 2026-09-22. **Approval.** D selected and approved full
T6 implementation. This is an implementation selection, so G15's traversal
report form does not apply. No source material was opened before the selection.

**Departure from acceptance, stated rather than resolved (recorded 2026-09-24).**
The criterion above asks for "one named traversal or resource proposal". D
instead chose an implementation task, so no traversal was named, no G15 report
form was copied into a scope, and the checklist remains unapplied. The
criterion is left as written; the task is COMPLETE because D made the strategic
selection it existed to obtain. T6's approval came through this selection.

## T12 — Specify the G15 launch-configuration check

**Goal.** Turn G15's portable missing question into a read-only procedure that
must be applied before a quotient traversal receives a concrete realization or
action budget.

**Acceptance.** A method-level specification requires a concrete actor/view,
launch and landing points, all movement-predicate configuration requirements,
the complete propagated successor of every intervening controller transition,
and separate reach/cargo checks. It distinguishes concrete, conditional,
rejected-construction, and open-question outcomes without claiming
impossibility. It supplies a reusable report form.

**Scope.** Documentation only. It does not add an extractor, revise a quotient,
authorize a construction analysis, reopen B2, or permit staging, replay,
validation, search, testing, or an instance change.

**Contamination scope.** G15 and its permitted construction audit only. No
sealed-file access is needed.

**Status.** COMPLETE, 2026-09-22. **Approval.** Granted in session when D
selected the portable read-only G15 specification. **Evidence.**
`doc/constraint-method/Launch-Configuration-Checklist.md`.

## T11 — Implement and score the G14 fix

**Goal.** The per-problem work already specified, approved and committed before
this plan existed: RO currently prints S1's device-state premises unfiltered by
the stated view, and the approved fix labels them without changing what RO
allocates.

**Why it carries a late id.** Ids are identifiers, not an order. This task was
in flight when the plan opened, and it keeps its own id forever. D deferred it
behind the architecture tasks on 2026-09-20; it completed on 2026-09-22.
(Corrected 2026-09-24: this paragraph previously said it is listed first
because it runs first, which contradicted both the board and the deferral.)

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

**Status.** COMPLETE, 2026-09-22. The approved clean-image checks ran before
the one staged RO report. All P1-P8 commitments regenerated; no allocation
changed. Evidence:
`doc/problems/crelay-topo/constraint-evidence/g14-helper-checks-2026-09-22.log`,
`g14-first-run-2026-09-22.txt`, and register 7.29.

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

*Resolution, recorded 2026-09-24.* No amendment was made before T2 started.
Section 8 of the schema adopted the two-word split, and the ledger reporter
prints a NOTE saying `standing` is not RO's `status`. D may still ask for one
word; that would now be a specification amendment plus a reporter change.

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

**Status.** COMPLETE, 2026-09-20. D's acceptance runs on lumpy subsequently
passed (see "Acceptance suite" in T10); the earlier "pending D's own run"
qualifier was removed on 2026-09-24.

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

**Five underspecifications found and settled** during implementation are the first
five items of section 16 of the schema document, which was corrected in the same
turn. Later tasks appended further items to that section.

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

**Status.** COMPLETE, 2026-09-22. **Approval.** Granted when D selected full
T6 implementation through T13. The extractor reports AM1–AM3 only when its
pressure-plate supports are disjoint; it derives the goal actor and destination
from the goal form, reports the full S2 pool for AM1, and reports both live and
full segment budgets for AM3. Each emitted claim names its S1/S2 premises and
grade. `doc/constraint-method/evidence/t6-budget-arithmetic-checks-2026-09-22.lisp`
ran eight assertions successfully in a clean staged diagnostic, including the
overlapping-support guard. No sealed material was opened.

## T7 — S5 height and reach lattice

**Goal.** Implement the sealed S5 specification.

**Acceptance.** As specified in the problem's register, section 3.

**Contamination scope.** C1 applies. **S5 has a sealed value paragraph**, so this
is a stronger experiment than RO's or G14's: commit predictions before coding,
score the first completed run, and at scoring read only S5's own paragraph in
section 4, stopping before the next extractor, per M8.

**Status.** COMPLETE, 2026-09-22. **Approval.** Granted when D instructed the
session to continue with T7. The first staged run is a retained MISS because
it reported current support tops instead of achievable carried-object tops.
The corrected second run regenerates connector tops 1, 2, and 5/2 and the
held-tray unreachable-from-ground row. Register 7.31 preserves the first score;
7.32 records the correction score. The five-assertion clean staged check is
`doc/constraint-method/evidence/s5-height-reach-checks-2026-09-22.lisp`.

## T8 — S6 beam sightline table

**Goal.** Implement the sealed S6 specification. Depends on S5's achievable tops.

**Acceptance.** As specified in the problem's register, section 3. Resolves AM8
and the conditional beam competition of AM4 only if the sightline semantics
actually establish them; a run that does not establish them says so.

**Contamination scope.** C1, and the sealed-paragraph discipline of T7. G9 is TAG
ONLY and must not be opened to obtain this information.

**Status.** COMPLETE, 2026-09-22. **Approval.** Granted when D instructed the
session to continue with T8. The first score is a disclosed regeneration check
because the prior authorized sealed scan displayed S6. The report evaluates all
512 direct gate subsets in copied start states with no propagation or search,
and its six-assertion check is
`doc/constraint-method/evidence/s6-sightline-checks-2026-09-22.lisp`.
Register 7.34 records the score, including the empty location-occluder kill
list.

## T9 — S7 landmark graph and orderings

**Goal.** Implement the sealed S7 specification.

**Acceptance.** As specified in the problem's register, section 3, with the
relaxation in force stated explicitly in the output. S7 cannot silently supply a
simultaneous role requirement or a segment premise.

**Contamination scope.** C1, and the sealed-paragraph discipline of T7.

**Status.** COMPLETE, 2026-09-22. **Approval.** Granted when D instructed the
session to continue with T9. The first score is a disclosed regeneration check
because the prior authorized sealed scan displayed S7. The explicit location
goal has no S1 device-state conjunct, so the delete-relaxed extractor emits no
controller expansion or route ordering; it is a measured MISS rather than an
invented K1–K9 chain. The four-assertion check is
`doc/constraint-method/evidence/s7-landmark-checks-2026-09-22.lisp`.

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

**Status update, 2026-09-25.** Validation clause MET: `validate-search-checkpoint`
accepted the composed 87-action path (80 hand-derived, D-directed actions replayed
into a checkpoint + a 7-action search-found final leg at cutoff 10) from the initial
state to the goal. Premise-provenance clause met the same day (pr18-pr22). **T10 COMPLETE.** Record:
`doc/problems/crelay-topo/constraint-evidence/b2-ghost-tray-loc5-check-2026-09-24.txt`.

**Baseline update, 2026-09-22.** With D's explicit read-only approval, the
alternative recorder baseline was developed from permitted source and evidence.
It is a fresh two-cycle arrangement, not a transformation of the keeper
checkpoint: first move tray1 to plate1 under agent1*'s open-cycle gate1
witness, return agent1 empty-handed and cancel; then move box1 to plate2; then
start a second cycle. The second fork puts tray1* on plate1 and box1* on
plate2, keeping recording gates1 and3 open while the new ghost exits R1 and
reaches R2. It removes the local ghost-release obstruction but leaves source
restoration, gate6 resources, and every later traversal unresolved. No runtime
operation, checkpoint import, replay, search, validation, extractor, code
change, or ledger restructuring occurred. Evidence:
`constraint-evidence/recorder-baseline-analysis-2026-09-22.txt`. The next
strategic choice is only whether to authorize further read-only analysis of
source restoration and resource commitments from that baseline.

**Baseline source/resource update, 2026-09-22.** D approved that further
read-only analysis. With tray1/box1 held at plates1/2, the new ghost can reach
R2 and hold plate3; because physical pressure is layer-blind, that opens gate4
for the low transmitter1-to-connector1 source beam while agent1 pairs the live
connector at location9. The second fork leaves tray1* and box1* at plates1/2;
the live tray and box can then move independently to plates4/5, retaining the
ghost gate witnesses and opening physical gate6 for an empty-handed agent1.
This gives a source-derived candidate for red repeater, gate6, and location14.
It is not a measured path, and the truncated cutoff-12 bounds from the different
keeper checkpoint neither explain nor refute it. No runtime operation,
checkpoint import, replay, search, validation, extractor, code change, or
ledger restructuring occurred. Evidence:
`constraint-evidence/recorder-baseline-source-resource-analysis-2026-09-22.txt`.
The next choice is whether to derive a staged bounded construction for this
candidate before any specific replay or search is proposed.

**Baseline staged-construction update, 2026-09-22.** D approved that read-only
derivation. B1 transfers tray1 to plate1 during a first recorder cycle and
cancels at a closed boundary; B2 moves box1 to plate2 and starts the second
cycle; B3 uses agent1* at plate3 and connector1 at location9 to restore the
source; B4 uses the live tray and box at plates4/5 for gate6, then crosses
screen1 empty-handed to location14. The nominal skeleton counts are 9, 6, 5,
and 9 actions. Each future bounded stage is proposed at cutoff12; the 29-action
combined skeleton is not a legitimate single fresh-start probe under that
ceiling. No runtime operation, checkpoint import, replay, search, validation,
extractor, code change, or ledger restructuring occurred. Evidence:
`constraint-evidence/recorder-baseline-staged-construction-2026-09-22.txt`.
The next choice is whether to approve B1 as an explicitly recorded bounded
experiment.

**B1 result update, 2026-09-22.** D approved the B1 fresh-start experiment.
At threads16/cutoff12 it found the requested closed boundary at minimum depth9:
agent1* opens gate1, agent1 transfers tray1 from plate2 to plate1, returns to
recorder1, and cancels. The result has tray1 on plate1, agent1 at location1,
and no ghosts; physical and recording gate1 are open. Three candidate paths
were accepted. The returned checkpoint could not be exported because the
post-search PowerShell pathname form caused a READER-EOF-ERROR after the search
was complete.

**B1 recovery update, 2026-09-22.** D separately approved action-sequence
reconstruction. The canonical nine actions were validated from a fresh staged
origin and exported as `constraint-evidence/t10-b1-checkpoint.txt`; no planner
operation ran. The archive contains one checkpoint at depth9 with nine actions
and SHA-256
`D004ADBDB1F055E907F95C6A603CE3D8B48C4696AD24C3BD6AD3CBF83322DBA7`.
It is a separate B1 experimental checkpoint, not a main-spine link, validation,
or B2 authorization. The next choice is whether to approve B2 from this
recovered checkpoint. See `constraint-evidence/b1-baseline-result-2026-09-22.txt`.

**What exists after the first half.**
Current audit completion (2026-09-22): D approved read-only review and handoff.
The B2 six-action route omitted jump-rise and propagated lift-loss conditions;
that estimate is withdrawn, with G15 filed as a portable obligation. B1 and
the three older archives are hash-verified and unchanged. Next strategic
decision: approve read-only revised box retrieval from B1; no execution is
authorized. Evidence: `constraint-evidence/b2-construction-audit-2026-09-22.txt`.
Technical decision: preserve original pre-run text under explicit correction
notices, so the measured failure remains tied to the proposal actually tested.

**Revised B2 construction update, 2026-09-22.** D approved read-only
derivation of a replacement for the withdrawn six-action route. The source
trace shows that current authored geometry has no complete replacement:
locations4/5 cannot jump to level-3/2 location6, and location20 is reachable
only through a blower whose switch simultaneously closes gate2 and drops an
unsupported actor when turned off. A valid future B2 would first need an
independent elevation/support transition that survives with gate2 open, then
perform a carried-box return, plate2 placement, empty-handed recorder return
and second fork. This is conditional analysis, not an impossibility claim or
runtime guidance. No location or transition was proposed. Evidence:
`constraint-evidence/b2-revised-construction-analysis-2026-09-22.txt`.
The next strategic choice is whether to investigate such a separately
justified elevation resource or close the branch.

Latest B2 result (2026-09-22): the approved single search exhausted with direct
truncation at cutoff12. B1 remains unchanged; no B2 archive. See
`constraint-evidence/b2-baseline-result-2026-09-22.txt`. Read-only construction
audit is proposed, not yet approved; no further search or deepening authorized.

Latest B2 preparation (2026-09-22): the separately filed proposal retains the
staged-construction goal and threads16/cutoff12. Technical decision: preserve
the goal rather than add an unmeasured cycle-count restriction; inspect the
actual cycle count and holding states at any successful endpoint. D authorized
preparation in this exposed session, not execution. See
`constraint-evidence/b2-baseline-proposal-2026-09-22.txt`.

Historical first-half state follows:
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
are CLOSED: lk1, lk2, lk3, lk7. [Corrected 2026-09-24: four of nine, as the
list shows.] T10 remains approved and IN PROGRESS.
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


**B2 reopened and box retrieval validated, 2026-09-24.** D named a new
elevation resource: the ghost holds its tray at location5, so the live agent
lifted to location20 can jump onto it, turn switch1 off, and jump through gate2
into location6. D approved a read-only check; all seven steps passed on hand
reading, and D then validated the whole 23-action route from a fresh stage with
`VALIDATE-ACTION-SEQUENCE` (no search): SUCCESS-P T, goal satisfied, agent1 at
location5 holding box1, connector1 on plate1, tray1 on plate2, cycle 2 open.
Premise pr18 (user-asserted, proposed, not yet ingested): connector1 on plate1
and tray1 at location7 when the second recording starts. D's working rule,
adopted: a hand-derivable path is validated first; searches are reserved for
hypotheses that cannot be written out and validated. Evidence:
`doc/problems/crelay-topo/constraint-evidence/b2-ghost-tray-loc5-check-2026-09-24.txt`,
`validate-b2-ghost-tray-2026-09-24.lisp`.

**B2 endpoint validated, 2026-09-24.** Extending the route, D validated 39
actions from a fresh stage (`validate-b2-box-plate2-2026-09-24.lisp`, no
search): tray1 on plate1, box1 on plate2, connector1 on the ground at
location9, agent1 at the recorder, cycle 2 closed. D's direction for the
beam: a ghost connector on the ground at location9 paired with transmitter1
and repeater1. Next: the third cycle (beam and gate8).
*Corrected by D the same day:* the chosen endpoint keeps connector1 on
plate1 and leaves tray1 on the ground at location7; revised 31-action route in
`validate-b2-box-plate2-rev-2026-09-24.lisp`, awaiting D's run. The 39-action
result stays on record but is not the chosen endpoint.
D ran it: 31 actions, SUCCESS-P T, goal satisfied -- cycle 2 is complete.
Cycle 3 (D's plan): a ghost-to-live handoff of box1 and a paired connector1 on
tray1*, carried past gate5 to location12 and dropped onto the live agent's held
tray1. Part A in `validate-c3-part-a-2026-09-24.lisp`.
Parts A-C validated by D (41, 53, 60 actions; files `validate-c3-part-a/-b/-c-2026-09-24.lisp`):
box1 placed on the ghost-held tray from location20 (a grounded agent cannot
reach a held tray's 3/2 top -- S5 already said NO); connector1 paired at
location9 with repeater1 and receiver1 and placed on box1 from location20; the
ghost carried the stack to location12 and put its tray down, and the riders
settled onto agent1's held tray1. Endpoint: agent1 at location12 holding tray1
with box1 and paired connector1 on it; cycle 3 open.

### T10 handoff history (superseded, kept verbatim)

Moved here unchanged from the Current Task section on 2026-09-24. It records
the 2026-09-21/22 handoffs in the order they were written, which is not
chronological. Where it conflicts with Current Task, Current Task governs;
nothing below is a next-step instruction.

**T10 — End-to-end closure on crelay-topo.**

T9 completed on 2026-09-22 as a disclosed regeneration check. Its delete-
relaxed S7 report correctly leaves the explicit location-only goal unexpanded:
there is no S1 device-state conjunct to backward-chain. It therefore does not
regenerate the exposed K1–K9 outline and is recorded as a MISS, rather than
inventing an unsound route or simultaneous role requirement. The four-assertion
check passed without profile-load warnings. Evidence:
`doc/constraint-method/evidence/s7-landmark-checks-2026-09-22.lisp`.

T10 remains in progress, but no run is pending. The B2 branch stays closed:
the nominal six-action construction is rejected and the recorder split-lift
resource is unavailable from B1. No construction analysis, source audit,
staging, replay, validation, search, instance change, or further extractor
work is authorized by T9’s completion. T11 and T12 are complete. All older
current-task text below is historical where it conflicts with this header.

Follow-up, 2026-09-22: D approved a read-only investigation of a possible
recorder-based elevation resource. The mechanism is conditionally sound only
if a ghost already holds tray1* at location20: a ghost switch-on can sustain
the ghost tray there, then a live switch-off opens physical gate2 while a live
rider remains supported. It is unavailable from B1. Carrying tray1* away from
plate1 closes recording gate1 before the ghost can use its required passage
toward blower1; live tray1 cannot restore the recording view and cannot be
manipulated by the ghost. No other authored support reaches the elevated
approach. The B2 branch remains closed. Evidence:
`doc/problems/crelay-topo/constraint-evidence/b2-recorder-elevation-resource-followup-2026-09-22.txt`.
No runtime operation, new baseline, B3/B4, or extractor is authorized. T11 is
complete; T12's read-only G15 specification is complete.

Handoff, 2026-09-22: D approved the read-only revised-construction derivation.
The proposed B2 route omitted a launch-elevation prerequisite: location6 is
at3/2, the nearby ground launches at0, and the jump-rise limit is1. The
blower fallback also needs a support/transition that survives opening gate2:
switching off its lift drops an unsupported actor before the next move.
This rejects the nominal six-action construction, not B2's reachability.
The six-action B2 and combined29-action estimates are withdrawn as execution
guidance; the second-fork and later B3/B4 endpoints remain conditional.
Read `doc/problems/crelay-topo/constraint-evidence/b2-construction-audit-2026-09-22.txt`.
Portable question G15 was appended without reading sealed gap content.
No Lisp/search/replay/tests ran during the audit. Four archive hashes, the
profile hash and three register-prefix hashes match; register content was
not displayed. The main ledger remains unchanged. Historical18 searches plus
the separate B1/B2 runs gives20; setup failures/imports add none.

The revised derivation and follow-up elevation-resource audit found no
complete route or current elevation resource in the authored geometry. Any
valid B2 needs an independently retained level-3/2 approach while switch1 is
off and gate2 is open; the existing blower cannot provide that because
stopping it drops the actor. The cargo return and second fork are therefore
conditional only. No location or transition was proposed.

NEXT SESSION: read the continuation prompt and restart checkpoint, then
present T13's three scopes for D's choice. Do not read new source material or
begin analysis until that choice is approved. The B2 construction branch stays
closed unless D identifies a new elevation resource and approves the first
scope. All older next-step text below is historical where it conflicts.

Latest result, 2026-09-22: D approved B2. One threads16/cutoff12 search from
restored B1 found no solution; direct truncation T, 381717 cutoff hits,
18.166 seconds. No B2 checkpoint was created; B1's hash remains unchanged.
This separate experimental bound does not explain the failure or refute the
baseline. Evidence: `doc/problems/crelay-topo/constraint-evidence/b2-baseline-result-2026-09-22.txt`.
Recommend read-only audit of the nominal construction's movement and recorder
assumptions, subject to new approval. No further runtime operation is approved.
Earlier pending-B2 text below is historical and superseded by this result.

Latest preparation, 2026-09-22: D approved B2 preparation after this session
disclosed supplied older puzzle memory and a related registry lookup. This
remains an exposed session. The B1 archive hash and stored endpoint were
checked without import. The concrete one-run proposal, restoration scope,
endpoint acceptance and bounded outcome readings are in
`doc/problems/crelay-topo/constraint-evidence/b2-baseline-proposal-2026-09-22.txt`.
No B2 operation is approved. No Lisp or tests ran; no sealed material was read.
On 2026-09-22 D approved read-only development of the alternative baseline after
the disclosed supplied-memory exposure; this remains an exposed, not clean,
prediction session. The analysis produced a fresh two-cycle baseline candidate:
the first cycle moves tray1 to plate1 while agent1* holds gate1, then cancels
cleanly; box1 is moved to plate2; the second start forks cargo witnesses onto
both plates. The new ghost can therefore leave R1 through recording gate1 and
reach R2 through recording gate3 without depending on agent1* as a gate1
witness. The baseline is not a checkpoint replacement, replayable sequence, or
source/gate6 solution.

D then approved read-only source-restoration analysis from that baseline. A
new ghost can hold plate3, opening physical gate4 for the low source beam while
the live agent pairs connector1 at location9. The baseline fork leaves tray1*
and box1* on plates1/2, so their live counterparts can move independently to
plates4/5 while the ghosts preserve gates1/3. With agent1 empty-handed, this
is a source-derived candidate for red repeater power, physical gate6, and
location14. It is neither a measured path nor a refutation of the earlier
cutoff-12 bounds, which began from the different keeper checkpoint. Evidence:
`doc/problems/crelay-topo/constraint-evidence/recorder-baseline-analysis-2026-09-22.txt`.
`doc/problems/crelay-topo/constraint-evidence/recorder-baseline-source-resource-analysis-2026-09-22.txt`.

D then approved read-only staged-construction analysis. The candidate separates
into B1 first-cycle tray transfer/cancellation (nominal 9 actions), B2 box
transfer and second fork (6), B3 ghost plate3 plus source pairing (5), and B4
live gate6 witnesses plus location14 crossing (9). Each is proposed at the
standing cutoff ceiling 12; the 29-action combined skeleton cannot be tested as
one fresh-start probe.

D approved B1. Its fresh-start threads16/cutoff12 search found the exact closed
tray1-on-plate1 boundary at minimum depth9. The post-search export form then
failed to parse because of a PowerShell pathname escape. D separately approved
action-sequence recovery; the canonical nine actions were replayed and exported
without invoking a solver. The recovered one-phase B1 archive has depth9,
nine actions, and SHA-256
`D004ADBDB1F055E907F95C6A603CE3D8B48C4696AD24C3BD6AD3CBF83322DBA7`.
Evidence: `doc/problems/crelay-topo/constraint-evidence/b1-baseline-result-2026-09-22.txt`.

**Next-session entry:** read the continuation prompt, restart checkpoint, and
the four 2026-09-22 analyses plus the B1 recommendation/result. The pending
choice is whether to approve B2 from the recovered B1 checkpoint. Do not rerun
B1, import archives, start B2, begin a new baseline, or resume deferred T11
automatically without that approval.
Handoff verification on 2026-09-22 rechecked all three archive hashes, the full
generated-profile hash and all three recorded register-prefix hashes: all match.
Register bytes were hashed only; no sealed content was displayed or interpreted.

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
for puzzle answers. On 2026-09-21 an exposed session was stopped for a fresh
session. On 2026-09-22 supplied-memory exposure was disclosed again and D
explicitly authorized continued analysis. Keep these two events distinct;
the latter analysis is not an independent clean prediction experiment.

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

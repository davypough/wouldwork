# Constraint-Led Method — Problem-Solving Guide

**Begin with Intake.** For a resumed problem, read its Handoff and continue
from its Next step, completing only missing intake fields. Method development uses `doc/constraint-method/Constraint-Implementation-Plan.md`.

## Roles and standing decisions

D (the user) supplies domain knowledge and strategic choices; A (the assistant) reads semantics, checks ideas, records evidence and chooses technical details.
D runs substantial searches in the existing Wouldwork REPL on lumpy, package WW. A checks these documents rather than asking D what they contain.

- Treat each new problem as new; do not use a solution found in its files.
- Use plain language and the problem's location names, without invented area
  labels. Ask one question at a time, skipping information already supplied.
- Nothing is sealed unless D records a specific seal in the implementation plan.
  New problems use no prediction registers; crelay-topo's register is frozen.
- D sets the maximum search depth from experiments and available time. Every
  search uses `*threads*` 16 and `*solution-type*` MIN-LENGTH at that maximum;
  use a lower cutoff only for a result shown reachable within it. Depth is the only search bound. Run the probe battery only on D's request. Never deepen silently or
  recommend above the maximum; ask D if more depth is needed.
- Agree each subgoal with D before its realization. An approved stage plan, if
  supplied, authorizes its searches within the maximum without per-search approval.
  Otherwise agree subgoals in dialogue; no advance whole-plan approval is needed.
- Technical choices (files, names, formats, realization) are A's; record the
  reason briefly. Strategic choices (claims, stage goals, build order, seals,
  future commitments) are D's. If uncertain, make a reversible technical choice
  and identify it. Suggest a fresh session when useful after updating records.

## Intake — Record the inputs

Gather the problem-spec path, optional corresponding diagram path or explicit
"none", and D's non-negative maximum search depth. Check supplied files are
accessible; ask about missing inputs rather than guessing. Record them in
`doc/problems/<problem>/Handoff.md`. Preserve a resumed problem's current state.
D may choose to proceed without a missing diagram. Raise spec/diagram mismatches with D; do not silently change the spec.

## Static profile — Read the problem and its constraints

Read the spec and diagram. Supply the staging and profile commands below; D runs them. Read the generated profile before discussing subgoals. Explain the goal,
controllers and crossings, couplings, beam needs and body budget in domain terms. Every UNCOVERED mechanic needs a source-grounded hand contract in the Briefing
before proceeding (what it controls, lifts or moves, and its prerequisites), or a separately approved method component. Static hints remain qualified.

```lisp
(stage <problem>)
(ww-set *threads* 16)
(load (merge-pathnames "tech/constraint-profile.lisp"
                       (asdf:system-source-directory :wouldwork)))
(report-static-constraint-profile)
(write-static-constraint-profile
  (merge-pathnames "doc/problems/<problem>/Constraint-Static-Profile.txt"
                   (asdf:system-source-directory :wouldwork)))
```

Wouldwork is already loaded; omit QUICKLOAD and IN-PACKAGE. Restart with STAGE;
a live checkpoint continuation does not restage. Staging resets problem settings;
crossing the serial/parallel boundary restages, so set threads before replay/import.

## Subgoal dialogue — Check, realize, review

1. Write the Briefing and present a short summary and anticipated difficulties.
   Open with either a penultimate state working backward from the goal, or a
   first subgoal working forward. D's tricks emerge through this discussion.
2. Restate each idea with its bodies, supports, devices, locations and view.
   Check it immediately against static tables or a focused query. Record
   **CONSISTENT** (allowed by named rows), **CONTRADICTED** (forbidden by a named
   fact), or **NEEDS** (a named prerequisite). CONSISTENT is not a realizable plan.
   Keep whose idea it was; user guesses remain conditional, retractable premises.
3. Agree the next subgoal. A hand-derives and validates a sequence when practical;
   otherwise supply one min-length search at D's maximum depth, from the accepted
   checkpoint. Keep the expected interpretation and commands in its evidence.
   Review the endpoint before choosing another subgoal or search.
4. Record the result in the Briefing's subgoal log, save accepted actions and
   checkpoint evidence, and update the Handoff. An exhaustion is a bound relative
   to its start, settings and cutoff, never a refutation. Record termination and
   truncation separately; an out-of-memory crash is neither a bound nor a result.
   Discuss a lower cutoff with D after such a crash; do not silently retry.
5. On a surprise, read the semantics and file the missing general question in
   `doc/constraint-method/Schema-Gaps.txt`. Queue complex development in the
   implementation plan with origin, need, scope, dependencies and acceptance;
   queuing is not approval. Record any blocker and next step in the Handoff.

Search form (replace placeholders; retain the prior checkpoint until review):
```lisp
(ww-set *depth-cutoff* <D's maximum>)
(ww-set *solution-type* min-length)
(defparameter *checkpoint* (capture-search-checkpoint)) ; at the stated start
(defparameter *candidate* (solve-subgoal *checkpoint* (<subgoal>)))
(export-search-checkpoint *candidate* <archive-path>)   ; after endpoint review
```
Use the two-argument checkpoint form, not serial goal chaining. Exhaustion returns its input unchanged; keep results in separate variables. Restore by staging,
setting threads 16, then `(import-search-checkpoint <archive-path>)`, which replays
without searching. Record archive SHA-256 hashes in the Handoff. Search-found segments remain REALIZED, validated NIL, until separate replay; do not routinely
replay them unless D requests it. Hand-derived sequences must be validated. Checkpoint details: `doc/search-strategies/standalone-checkpoints.md`.

## Closure — Validate the complete path from the start

Use `(validate-search-checkpoint <final-checkpoint>)`, or after fresh staging and setting threads 16, replay all accumulated actions against the actual goal:
```lisp
(validate-action-sequence *start-state* <complete-action-list>
                          :goal-test (symbol-function 'goal-fn) :verbose t)
```
Require SUCCESS-P, GOAL-CHECKED-P and GOAL-SATISFIED-P all T; mere executability is insufficient. Keep the result and complete path as evidence, record premises
and gaps, and mark the Handoff CLOSED. A final search is needed only if the accepted path has not yet reached the goal. Do not infer global shortest length
from minimum-length searches for individual subgoals.

## Quick static checks

After staging and loading the profile, test on a private hypothetical gate state:
```lisp
(defparameter *check-state*
  (sightline-state-with-open-gates (census-type-instances 'gate) '(gate2 gate3)))
(jammer-target-visible-from-placement *check-state* 'jammer1 'location5 'plate2 'jammer1 'gate1)
(reachable *check-state* 'location1 'location7)
```
These are claustro-topo syntax examples; substitute the current problem's objects. Only the listed gates are forced open; other start-state facts remain. This
bypasses propagation: gate bits are premises, not controller-consistent states. REACHABLE takes target then reacher and checks arm reach, not a walking route.
Jammer visibility alone proves neither legal placement nor availability; also
check `jam-disallowed>` and the action's placement/reach conditions.

MOVE takes a list of route transitions. Formats illustrated by claustro-topo:
```lisp
(move agent1 ((walk location1 (gate1 gate3) location4)))
(move agent1 ((ladder location7 (ladder1) location1)))
(move agent1 ((jump (location10 ground) nil (location10 box2))))
(move agent1 ((jump (location10 box2) nil (location12 ground))))
(move agent1 ((stairs location13 nil location11)))
```
Use the actual route barriers; NIL means none. Plain MOVE is accepted. Actions
with multiple outcomes need their printed phrase to select the outcome, e.g.
`(jam-target > agent1 jams gate5 with jammer1 at location8 on ground)`.
Validation accepts mixed plain and timestamped action lists.
Hand-written CONNECT-CONNECTOR termini must follow the engine's enumeration order, since the validator
compares them strictly: reverse declaration order (connectors, receivers, transmitters, each descending).

## Optional tools, on D's request

Load diagnostics with `(load (merge-pathnames <file> (asdf:system-source-directory :wouldwork)))`.
Their contracts are in `doc/constraint-method/Extractor-Specifications.md`;
the ledger schema is `doc/constraint-method/Status-Algebra-and-Record-Schema.md`.

- **Probe battery:** load `tech/constraint-probe-battery.lisp` after the profile.
  `(report-probe-battery-list)` lists without searching. At threads 16,
  `(run-probe-battery <max-depth> <results-path> [probes])`
  runs with depth as its only bound. Optional probes are
  `(d-probes '((<subject> <goal> "<provenance>") ...))`, alone or appended to
  `(probe-battery-probes)`. `(report-probe-battery <results-path>)` reports results.
- **From here (FH):** `(report-from-here <checkpoint>)` reports the state and
  one-step opportunities. For all applicable actions load
  `tech/constraint-state-probe.lisp` and use `(report-applicable-actions <action-prefix>)` after fresh staging.
- **Role obligations (RO) / cycle-plan check:** `(report-role-obligations <scenario>)`
  and `(report-cycle-plan-check <plan>)` use explicit data (specifications 4/13).
  PASS is not a plan witness; CONDITIONAL names a premise to settle with D.
- **Ledger:** load `tech/constraint-ledger.lisp` (no staging needed). Create once
  with `(write-realization-ledger (make-stage-ledger "<problem>") <ledger-path>)`.
  Later changes use `(ledger-file-apply <ledger-path> #'<operation> <arguments>)`;
  report with `(report-realization-ledger (read-realization-ledger <ledger-path>))`.
  Use schema stage operations for checks, endpoints, bounds and supersession.
- **Supplied relay views:** `(report-relay-view-scenario <scenario>)` tests explicit
  complete-state scenarios (specification 6.1). Optional scenarios also go to
  REPORT-RELAY-CHAIN-TABLE, REPORT-NECESSITY-HINTS and profile report/write calls.
  Never combine hops from different scenarios; CLEAR proves no stable/reachable setup.
- **Supplied stability:** load `tech/constraint-arrangement.lisp`; call
  `(report-relay-arrangement <scenario>)` or `(check-relay-arrangement <scenario>)`.
  Specification 6.2 requires a complete configuration, phase, provenance, view,
  receiver and chain, with primitive controls and no forced gate premises.
  Private-copy propagation and a second-pass fixed point distinguish working,
  failed, invalid/inconsistent and unresolved arrangements. Preserve plate-edge
  and recorder memory. Stability proves neither reachability nor survival through
  later moves, tray release or recorder boundaries; those require separate checks.

## Rules, grades and engine facts

C1: derive extractors from instance/tech semantics, not hand-analysis answers.
C3: no problem object names in diagnostic code; use caller data and declared,
justified substrate interfaces. M2: never hand-edit generated output. M3: when
dialogue outweighs records, update files and the Handoff last before a fresh
session. M4: completion needs concrete links that compose under validation.
M5: investigate surprises and record general schema gaps. Prediction/scoring
rules C2, C4, M1, M6–M9 survive only as history in the frozen experiment.

Grades: **definitional** (definitions/static facts), **inductive** (initial case
and preservation by every applicable action), **cost bound** (cutoff and start),
**trace/ordering** (separate proof obligation). No inductive proof rests on a
cutoff. Label graph candidates, missing analysis and conditional bounds explicitly.

A grounded agent cannot place onto a held tray (top 3/2, placement reach 1);
use a raised location. CONNECT-CONNECTOR sets the connector down, never pairs
while held; pairing uses structural sightlines. Generated termini are reversed
(e.g. repeater then receiver). HOLDING is bijective, printed HOLDING1/HOLDING2.
The Filesystem extension worked on 2026-09-27; write dollar-bearing files whole
or with literal quoting, avoiding replacement-string substitution. Hash with
`certutil -hashfile <file> SHA256`. Escape leading dollars in prose.

## Records and templates

Under `doc/problems/<problem>/`, keep the generated `Constraint-Static-Profile.txt`,
Briefing, Handoff, and `constraint-evidence/` (an INDEX.md and a subfolder per
subgoal when useful). Existing Stage-Plan.md files remain historical records;
new stages go in the Briefing's log. A ledger is optional. State lives in the
Handoff; evidence and the log retain provenance and results.

Briefing.md: profile path/hash and maximum depth, then **Summary**, **Difficulties**,
**Contracts** (including UNCOVERED mechanics), **Hints** (qualified, with sources),
**Subgoal log** (`subgoal | whose idea | check | result`, with evidence links),
and **Result** (full-path validation and any limits).

Handoff.md template (unchanged):
```text
# <problem> — Handoff
Updated <date>.  Status: <phase, stage>.
Problem spec: <file path>.
Corresponding diagram: <file path or "none">.
Maximum search depth: <D's cutoff>, set <date>.
## Next step       one line: who does what
## State           ledger file SHA-256 and stage counts; current endpoint in domain terms
## Checkpoints     file | phases/actions | SHA-256 | what it is
## Restore         the exact REPL forms
## Open items      at most a few lines
```
When there is no ledger or checkpoint, state that explicitly.

# Constraint-Led Method — Implementation Plan

**On reading this file, continue the method's development.** Take the Current
Task below. If its approval is granted, begin work; if not, present the task,
your intended approach and its acceptance criteria to D, and ask before
changing any file. To solve a problem instead, the entry point is
`doc/constraint-method/Problem-Solving-Guide.md`, which leads to the
problem's `Handoff.md`.

Opened 2026-09-20; restructured by T18, 2026-09-25. This file holds the
board, conventions, policy and current task. It does not hold task history:
completed entries T0–T17, verbatim as of 2026-09-25, are in
`archive/Implementation-Plan-2026-09-25.md`. Nothing about this work lives in
`CLAUDE.md` or `AGENTS.md`. `doc/README.md` only lists this directory.

## Session rules

1. **Take the Current Task.** It is always the next waiting task. If none is
   approved, present its candidates to D and wait.
2. **Read its entry in full**, and the sources the entry names.
3. **Check the Approval field.** If it is *Granted*, begin: do not re-ask for
   a recorded approval. If it is *Not requested*, present the task, your
   approach and its acceptance criteria, then ask.
4. **Work one task at a time**, to its acceptance criteria. If the work needs
   something outside the task's scope, stop and ask.
5. **Before the session ends**, update the task entry, the board and the
   Current Task, and update any Handoff the work touched.

## Current Task

**T30 — cleanup before a new problem.** PROPOSED; approval requested from D
2026-09-26. T28 and T29 are COMPLETE. After T30 the next work is a new
problem, through `Problem-Solving-Guide.md` (D, 2026-09-26).

## Reading policy (D, 2026-09-25)

**Nothing in the wouldwork directory is sealed.** Every file may be read in
full, at any stage, without a grant from D and without a disclosure.

**Exception.** D may seal a specific file, section or range for a specific
purpose. The seal is recorded here with its date, scope and reason, and binds
until D lifts it. There are currently none.

**Unchanged.** Generated output is never hand-edited (M2). The crelay-topo
prediction register is frozen. Sealed-reading rules in archived and frozen
files are history; they describe the rules those tasks were done under.

## Conventions

- **Ids are stable and append-only.** Never renumber. A task that dies is
  CLOSED with a reason, not deleted.
- **Acceptance criteria are written before the work starts.**
- **Status** is one of PROPOSED, SPECIFIED, APPROVED, IN PROGRESS, COMPLETE,
  BLOCKED, CLOSED. **Approval is recorded separately from status.**
- **Evidence is named by file**, not described. Method-level evidence goes
  in `evidence/`.
- **A completed task's entry stays here until the next restructure**, then
  moves to `archive/` verbatim.

## Working conventions for method development

The general rules (roles, grades, C1/C3/M2–M5, technical vs strategic
choices, tooling) are in the Problem-Solving Guide and apply here too.
These are specific to building the components:

- **`tech/constraint-profile.lisp` is a loadable diagnostic.** It is never
  named in an `include-tech` directive and is never an ASDF component. It is
  plain Common Lisp in `:WW`, with no `define-*` DSL forms (a LOADed file gets
  no tech splice). The same holds for `tech/constraint-ledger.lisp` and
  `tech/constraint-state-probe.lisp`.
- **Definition order is callees-first in those files.** This reverses the
  project's usual high-level-first order. They are reloaded by hand after
  every edit, so forward references would bury real warnings. Each extractor
  sits in a contiguous block that ends in its own reporter, with the entry
  points last. Avoid `LABELS` and `FLET`.
- **No problem object names in them (C3).** Problem-specific terms come
  from caller-supplied data.
- **A new component writes its specification into
  `Extractor-Specifications.md` before code**, with its acceptance checks.
  The checks run before the component's first staged run, and their evidence
  goes in `evidence/`.
- **Where work runs.** Focused diagnostic staging is cheap. Substantial
  search is D's to run on lumpy.
- **What to ask D.** D answers questions about the problem domain and about
  how Wouldwork searches it. Checks against these documents are A's.

## File map

| File | Authoritative for |
|---|---|
| `doc/constraint-method/Constraint-Implementation-Plan.md` | method development: board, current task (this file) |
| `doc/constraint-method/Problem-Solving-Guide.md` | solving a problem: procedure, rules, grades, templates |
| `doc/constraint-method/Extractor-Specifications.md` | every static component's specification |
| `doc/constraint-method/Schema-Gaps.txt` | schema gaps from all problems (G1–G17 from crelay-topo) |
| `doc/constraint-method/Status-Algebra-and-Record-Schema.md` | the ledger's record schema and status algebra (T1, revised by T27) |
| `doc/constraint-method/Post-Mortem-2026.md` | T17 findings; design basis for T18–T27 |
| `doc/constraint-method/evidence/` | method-level check, run and load evidence |
| `doc/constraint-method/archive/` | the pre-T18 plan with every task entry; the G15 checklist |
| `tech/constraint-profile.lisp` | the static extractors (S0–S7, T6, RC, RO) |
| `tech/constraint-ledger.lisp` | ledger (stages, file of record), recommender, ingester, question generator (T2–T5, T27) |
| `tech/constraint-state-probe.lisp` | one-step applicable-action probe from a replayed prefix |
| `src/ww-search-checkpoint.lisp` | standalone search checkpoints: export, import (by replay), validate |
| `doc/search-strategies/standalone-checkpoints.md` | user-level description of the checkpoint workflow |
| `doc/problems/<p>/Handoff.md` | a problem's current state (the only per-problem state file) |
| `doc/problems/<p>/Briefing.md`, `Stage-Plan.md` | Phase 1 and 2 records (new problems) |
| `doc/problems/<p>/Constraint-Static-Profile.txt` | generated extractor output |
| `doc/problems/<p>/Constraint-Realization-Ledger.txt` | the problem's ledger |
| `doc/problems/crelay-topo/Constraint-Prediction-Register.txt` | FROZEN record of the crelay-topo validation experiment |

## Objective

The method is organized as phases (Problem-Solving Guide): Phase 0 static
profile; Phase 1 shared understanding (briefing, hints, probes, domain
interview); Phase 2 D's stage plan, checked; Phase 3 stage loop; Phase 4
closure under `VALIDATE-SEARCH-CHECKPOINT`. The aim for new problems
(post-mortem 3.3) is that constraints and small searches suggest promising
paths that inform D's intuition, not only check D's proposals afterwards.

There are two invariants. **A user guess is a premise, not a fact.** **An
exhausted search is a cost bound, not a refutation.**

## Board

| Id | Task | Status | Approval | Depends on |
|---|---|---|---|---|
| T0–T16 | Coordination, T1–T5 interactive phase, T6–T9 and T16 extractors, T10 closure on crelay-topo, T11–T15 | COMPLETE | see archive | — |
| T17 | Post-mortem of the method's development | COMPLETE | 2026-09-25 | T10 |
| T18 | Document restructure to the phased strategy (post-mortem 4.1) | COMPLETE | approved by D, 2026-09-25 | T17 |
| T19 | I5 Mechanic coverage: static contract per tech mechanic, UNCOVERED report | COMPLETE | approved by D, 2026-09-25 | T18 |
| T20 | I3 Coupling census | COMPLETE | approved by D, 2026-09-25, scope (a) | T19 |
| T21 | I6 Necessity hints | COMPLETE | approved by D, 2026-09-25, scope (a) | T19, T20 |
| T22 | I1 State-based static queries ("from here" report) | COMPLETE | approved by D, 2026-09-26, scope (a) | T19 |
| T23 | I7 Probe battery | COMPLETE | approved by D, 2026-09-26, scope (a) | T22 |
| T24 | I2 Cycle-plan check (T6, RO, RC budgets) | COMPLETE | approved by D, 2026-09-26, scope (a) | T22 |
| T25 | I4 Domain interview (procedure; replaces the question generator) | COMPLETE | delivered with T18 | T18 |
| T26 | I8 Memory estimate before search | CLOSED | closed by D, 2026-09-26: D sets the maximum depth | — |
| T27 | Stage-organized ledger and Record Schema revision (post-mortem 4.1) | COMPLETE | approved by D, 2026-09-26, scope (a) | T24 |
| T28 | Mechanic contracts: jump and recorder contracts, step entry (T19's UNCOVERED) | COMPLETE | approved by D, 2026-09-26 | T19 |
| T29 | D's own probe goals in the probe battery (T23 scope (b)) | COMPLETE | approved by D, 2026-09-26 | T23 |
| T30 | Cleanup before a new problem | PROPOSED | not requested | T28, T29 |

## T19 — I5 Mechanic coverage

**Goal.** Give each tech/ mechanic a declared static contract: what it
controls, lifts or moves, and what it requires. The profile prints UNCOVERED
for any included mechanic that has none. The first contracts are
floor-blower and ladder (post-mortem 1.5).

**Approval.** D approved it on 2026-09-25, with scope (a): write the
floor-blower and ladder contracts; give every other public tech a registry
entry only where an existing extractor already covers it or it is plainly
infrastructure; report the rest UNCOVERED as later work. (Scope (b),
contracts for all included techs now, was declined.)

**Approach (A's technical choices).** Coverage is judged over the public
(non-dash) names in `*spliced-tech-names*`; dash files are covered through
the public tech that nests them (precedent: `report-inert-techs`). A contract
registry (a DEFPARAMETER in `tech/constraint-profile.lisp`) is keyed by tech
name; tech names are substrate interface names, not problem objects (C3).
A new section MC prints first in `REPORT-STATIC-CONSTRAINT-PROFILE`, since
it is the Phase 0 coverage gate. The stale paths in that file's header are
fixed (T18 decision).

**Acceptance (written before work, approved by D).**
- A1 Specification first: §8 of `Extractor-Specifications.md` holds the
  contract schema, the coverage rule, the two contracts and these checks,
  before code.
- A2 Expected readings first: `evidence/` records the expected MC output
  for crelay-topo, hand-derived from its DEFINE-INIT (blower1 and
  ladder1–3 rows; the exact COVERED/UNCOVERED list), before the first run.
- A3 The generated floor-blower and ladder rows match A2 exactly.
- A4 Every public tech crelay-topo splices appears exactly once, COVERED or
  UNCOVERED (checked by script against the include list).
- A5 Negative test: removing floor-blower from the registry inside a LET
  prints it UNCOVERED.
- A6 No problem object names in the code (grep); callees-first; no LABELS
  or FLET; the file loads by hand without warnings.
- A7 After D regenerates the profile, every existing section is
  byte-identical; only MC is new.
- A8 The T19 entry, board, Current Task and the guide's Build state line
  are updated; evidence in `evidence/t19-mechanic-coverage-<date>.txt`.

**Result.** MC prints first in the profile. On crelay-topo: 16 public
techs, 13 COVERED (2 contracts, 6 by extractors, 5 infrastructure),
3 UNCOVERED (jump, recorder, step: later work). The floor-blower row states
the lift (location4 → location20 under switch1) and the exits from the hover
point; the ladder rows state the three one-way climbs.
- A1 §8 of `Extractor-Specifications.md`, written before code.
- A2 expected readings in the evidence file, part 1, before the run.
- A3, A4, A5, A6 (C3) passed: 20 checks, run by D on lumpy.
- A6 load: clean, no warnings (D, 2026-09-25).
- A7 the regenerated profile minus MC hashes to the pre-T19 baseline.
- A8 this entry, the board, the Current Task and the guide's Build state.

**Status.** COMPLETE, 2026-09-25. **Evidence.**
`evidence/t19-mechanic-coverage-2026-09-25.txt`,
`evidence/t19-mechanic-coverage-checks-2026-09-25.lisp`.

## T20 — I3 Coupling census

**Goal.** Flag every action or control that changes two subsystems at once
(e.g. a switch that stops a blower and opens a gate). Latent constraints
sit there, and this makes G15-type findings routine. The G15 checklist
(Extractor Specifications §7) is an input.

**Approval.** D approved it on 2026-09-25, with scope (a): control-level and
device-role couplings only. Scope (b), walking every action's effects for
action-level couplings, was declined; the plate-as-support role covers most
of those.

**Approach (A's technical choices).** A new section CC, printed last
(after S4), since it reads S1, S3 and S6. Each controlled device and each primitive
controller gets roles read from static interfaces, with no problem object
names: *barrier* (named in a traversal arc family), *occluder* (a gate in
some S6 CONDITIONAL row's required-open set), *lift* (has AIMED-AT),
*support controller* (a primitive of type support), *beam-driven
controller* (S1 tier device-mediated). Amended 2026-09-25 after the first run
(A's choice, D deferred to A): the occluder role reads RC's hops, station
to endpoint and station to station, instead of S6's rows (specification
§9.5). Rows: K1 fan-out (one primitive, two
or more devices, with polarity and roles); K2 multi-role (a device or
primitive spanning two or more subsystems); K3 beam feedback (gates that
can cut a beam-driven controller's last hop, with their controllers; SELF
when the controller drives that gate itself). A K1 row holding a lift and a
barrier that guards an exit arc from the lift's destination carries the G15
flag and checklist §2.2's question; the checklist is not applied.

**Acceptance (written before work, approved by D).**
- A1 Specification first: §9 of `Extractor-Specifications.md` holds CC's
  roles, rows, G15 flag rule and these checks, before code.
- A2 Expected readings first: `evidence/` records the expected CC output
  for crelay-topo, hand-derived, before the first run. It includes
  switch1 → {blower1 lift normal, gate2 barrier inverted} with the G15 flag.
- A3 The generated CC rows match A2 exactly.
- A4 Every CONTROLS primitive and controlled device appears in CC's role
  table (checked by script against the control facts).
- A5 Negative test: rebinding the control facts inside a LET without the
  lift (or without gate2) removes switch1's G15 flag.
- A6 No problem object names in the code (grep); callees-first; no LABELS
  or FLET; the file loads by hand without warnings.
- A7 After D regenerates the profile, every existing section is
  byte-identical; only CC is new.
- A8 The T20 entry, board, Current Task and the guide's Build state line
  are updated; evidence in `evidence/t20-coupling-census-<date>.txt`.

**Result.** CC prints last in the profile. On crelay-topo: 21 objects;
K1 fan-out switch1 (route, beam, lift) and switch2 (route, beam), both
EXCLUSION pairs; K2 eight gates in route and beam (gate4 is beam only);
K3 receiver1's last hop needs gate7 (switch2, K1) or gate8 (SELF); one
G15 FLAG: switch1 opens gate2, the exit from blower1's destination
location20, only while blower1 is stopped.
- A1 §9 of `Extractor-Specifications.md`, written before code; §9.5 amends
  the occluder role after the first run.
- A2 expected readings in the evidence file, part 1, before the first run.
  The first run failed A3 on the occluder role (part 3): A2's cross-read
  had spanned RC, while the code read S6 only. D deferred the choice to A;
  A extended the rule to RC's hops (§9.5, part 4), under which part 1's
  expected block holds on a corrected basis.
- A3, A4, A5, A6 (C3, LABELS/FLET) passed: 12 checks, run by D on lumpy.
- A6 load: clean, no warnings (D, 2026-09-25); callees-first checked by
  script.
- A7 the regenerated profile minus CC hashes to the pre-T20 baseline
  (part 5).
- A8 this entry, the board, the Current Task, the guide's Build state and
  the crelay-topo Handoff's profile line.

**Status.** COMPLETE, 2026-09-25. **Evidence.**
`evidence/t20-coupling-census-2026-09-25.txt`,
`evidence/t20-coupling-census-checks-2026-09-25.lisp`.

## T21 — I6 Necessity hints

**Goal.** Turn each static limit into a candidate plan element for the
Briefing (post-mortem 3.4).

**Approval.** D approved it on 2026-09-25, with scope (a): all seven hint
families below, each reading data an existing extractor already computes.
(Scope (b), the four post-mortem examples only, was declined.)

**Approach (A's technical choices).** A new section NH, printed last (after
CC), since it reads T6, S1, S3, S4, S5, RC, MC and CC. Each hint prints its
limit, the candidate plan element, its grade and sources, and a label:
NECESSARY (every plan meets it, under the grade shown) or CANDIDATE (one way
to meet a limit). Families: H1 body budget (T6, S2); H2 keepers left behind
when crossing (S4 approach-only rows); H3 beam-held devices and candidate
beams (S1 depth, RC); H4 controllers off the goal route (S4, S3); H5 lift
landings (CC G15 rows, MC); H6 devices already active at the start (S1,
start state); H7 placement limits (S5). Recorder cycle counts are left out:
the recorder is UNCOVERED in MC. Correction to the proposal's H1 example
(A, before the specification): T6 shows at most 7 of 8 plates can be held
even with ghosts, so H1 says "more than 3 held plates needs ghosts; the
demands are met in sequence", not "ghosts hold at least 5".

**Acceptance (written before work, approved by D).**
- A1 Specification first: §10 of `Extractor-Specifications.md` holds the
  families, labels, grades and these checks, before code.
- A2 Expected readings first: `evidence/` records the expected NH output
  for crelay-topo, hand-derived, before the first run. It includes the four
  post-mortem 3.4 examples (H1 body budget; H3 plate3 held while gate8 is
  open; H3 candidate beam with a ground connector at location9 and a raised
  connector at location15; H5 landing, pointing to location5).
- A3 The generated NH rows match A2 exactly.
- A4 Completeness, checked by script: each source row yields exactly one
  hint (S4 approach-only keeper rows, G15 rows, S1 device-mediated devices,
  off-route controllers, T6 shortages, S5 unreachable tops, active-at-start
  devices).
- A5 Negative tests: an occupancy with no ghost layer turns H1 into
  "sequence the plate demands"; control facts without the lift give no H5
  hint.
- A6 No problem object names in the code (grep); callees-first; no LABELS
  or FLET; the file loads by hand without warnings.
- A7 After D regenerates the profile, every existing section is
  byte-identical; only NH is new.
- A8 The T21 entry, board, Current Task, the guide's Build state line and
  the Briefing template's hint line are updated; evidence in
  `evidence/t21-necessity-hints-<date>.txt`.

**Result.** NH prints last in the profile. On crelay-topo: 14 hints,
7 NECESSARY and 7 CANDIDATE. H1: more than 3 held plates needs ghosts; at
least 1 plate free at every moment; gate9 takes every live body but agent1.
H2: keepers stay behind at gate6 (plate4, plate5) and gate9 (plate6-8). H3:
gate8 needs plate3 held and 2 off-plate bodies; candidate beams by last
connector at location15 (incl. location9 ground + location15 raised, pr21)
and location9. H4: switch2 needs an excursion to location14 through gate6;
{gate5, gate7} never together. H5: blower1's landing, jumping to location5
(pr19). H6: gate2, gate3, gate5 free at the start (pr14). H7: place on the
held tray from location6 or location20.
- A1 §10 of `Extractor-Specifications.md`, written before code.
- A2 expected readings in the evidence file, part 1, before the first run.
- A3, A4, A5 passed on the first run; A6's name scan failed on the
  declared substrate constants normal and inverted (part 3). A declared
  GROUND and made the scan list its matches and allow only declared
  constants; second run 18 checks passed (part 4).
- A6 callees-first and balance checked by script.
- A7 the regenerated profile minus NH hashes to the pre-T21 baseline.
- A8 this entry, the board, the Current Task, the guide's Build state and
  Briefing template line, and the crelay-topo Handoff's profile line.

**Status.** COMPLETE, 2026-09-25. **Evidence.**
`evidence/t21-necessity-hints-2026-09-25.txt`,
`evidence/t21-necessity-hints-checks-2026-09-25.lisp`.

## T22 — I1 State-based static queries

**Goal.** Make the static tables queryable from any checkpoint state, with a
short "from here" report: reachable placements, live beam chains, and
accessible controllers (post-mortem 1.1, 1.7).

**Approval.** D approved it on 2026-09-26, with scope (a): rows F0–F3 only.
(Scope (b), rerunning the necessity hints against the checkpoint state, was
declined.)

**Approach (A's technical choices).** Rebinding `*start-state*` and rerunning
the profile does not give "from here" readings: most extractors read the
static database, enumerate supports by type, or force gate bits. FH reads the
checkpoint state itself, through the engine's own queries and its one-step
successor generator (`GENERATE-CHILDREN`), with no search. A new section FH in
`tech/constraint-profile.lisp`, entry point `REPORT-FROM-HERE` (a checkpoint,
an action prefix, or the staged start). FH is not called by
`REPORT-STATIC-CONSTRAINT-PROFILE`. Rows: F0 state; F1 reachable locations
(MOVE successors) and accessible controllers; F2 placements of held cargo;
F3 beams (the live relay state, then RC's usable chains labelled against the
current gates).

**Acceptance (written before work, approved by D).**
- A1 Specification first: §11 of `Extractor-Specifications.md` holds the
  rows, labels, grades and these checks, before code.
- A2 Expected readings first, hand-derived into `evidence/` before the first
  run, for the fresh staging and `t10-c3-location15-checkpoint` (80 actions).
- A3 The generated FH output matches A2.
- A4 Checked by script: every agent, primitive controller and relay appears
  exactly once; F1's grounded destinations agree with `MOBILITY-LOCATIONS`;
  F2's current-configuration row agrees with the applicable placing actions;
  at the fresh staging F3's groups cover RC's usable chains.
- A5 Negative tests on state copies: closing one gate removes the locations
  behind it from F1; opening one gate moves the chains it alone blocked into
  F3's "open now" group.
- A6 No problem object names in the code; callees-first; no LABELS or FLET;
  the file loads by hand without warnings.
- A7 After D regenerates the profile, it is byte-identical.
- A8 The T22 entry, board, Current Task, the guide's Build state and Phase 3
  step 7 are updated; evidence in `evidence/t22-from-here-<date>.txt`.

A4 and A5 were refined by A while writing §11, before code (reasons in
§11.6): F1 is built from the MOVE successors themselves, so checking it
against the applicable actions would be circular, and F3 labels chains by
gates rather than as LIT, because RC's stations cannot represent a stacked
riser. Confirmed by D, 2026-09-26.

**Result.** `REPORT-FROM-HERE` prints FH (§11); the profile is unchanged.
On crelay-topo's fresh staging: agent1 has one move (location2); plate1
changes after that move; every other controller is not within one move,
with the gates in the way (e.g. switch2 at location14 needs gate1 and
gate6, closed); receiver1's 52 usable chains fall in two groups, both
needing gate4 (plate3). At the t10-c3-location15 endpoint: agent1 has 7
walks, agent1* one step off plate3; plate3 and receiver1 change now;
plate4–8 after one of agent1's moves; the live beam is read from PAIRED
and COLOR, including connector1 at the stacked top 7/2.
- A1 §11 of `Extractor-Specifications.md`, written before code.
- A2 expected readings in the evidence file, part 1, before the first run;
  receiver1's checkpoint rows marked provisional (top 7/2 not in RC).
- A3, A4, A5, A6 (C3, LABELS/FLET), A7: 25 checks passed, 0 failed, first
  run, by D on lumpy (part 2). The provisional rows matched.
- A6 callees-first, no blank lines inside functions, and balance checked by
  script; the block compiled with stubbed engine functions without warnings
  other than those functions being undefined. Load on lumpy: no warnings
  reported by D.
- A7 checked inside the run: the regenerated profile equals the file.
- A8 this entry, the board, the Current Task, the guide's Build state and
  Phase 3 step 7.

**Status.** COMPLETE, 2026-09-26. **Evidence.**
`evidence/t22-from-here-2026-09-26.txt`,
`evidence/t22-from-here-checks-2026-09-26.lisp`.

## T23 — I7 Probe battery

**Goal.** Run small searches from the start state, one per resource or
landmark, within D's maximum depth at threads 16. The output is a map of
cheap / not found / blocked subgoals (post-mortem 3.4).

**Approval.** D approved it on 2026-09-26, with scope (a): probe families
P1–P4 generated from the profile's data. (Scope (b), adding a list of D's
own probe goals, was declined.) D set crelay-topo's maximum search depth at
11, the same day.

**Approach (A's technical choices).** A new loadable file
`tech/constraint-probe-battery.lisp`, under the same rules as the other
diagnostics; kept apart from `constraint-profile.lisp` because it runs
searches, so the profile stays static. A generator builds probe goals from
profile data (P1 landmark, P2 resource, P3 controller, P4 relay); a runner
searches each from the fresh staging's checkpoint by two-argument
`SOLVE-SUBGOAL` at `*threads*` 16, deepening one step at a time to the
maximum depth, with a caller-supplied states limit as a stopgap for T26;
results go to a data file read by the reporter (section PB). Found in
reading, before the specification: held cargo has no HAS-LOCATION
(`pickup-box`, tech/box.lisp), so P2 asks for cargo set down outside its
start region, not merely away from its start location.

**Acceptance (written before work, approved by D).**
- A1 Specification first: §12 of `Extractor-Specifications.md` holds the
  families, labels, grades and these checks, before code.
- A2 Expected readings first, hand-derived into `evidence/` before the
  first run: crelay-topo's exact probe list and START rows, and expected
  labels where the T10 evidence settles them; the rest provisional.
- A3 The generated probe list matches A2 exactly; the settled labels match.
- A4 Checked by script: each family source row yields exactly one probe;
  each CHEAP plan replays by `VALIDATE-ACTION-SEQUENCE` from the start and
  satisfies its probe goal; each row not found records depth and
  truncation.
- A5 Negative tests: a goal true at the start prints START with no search;
  a cutoff below a CHEAP length gives NOT FOUND; a tiny states limit gives
  STOPPED.
- A6 No problem object names in the code; callees-first; no LABELS or FLET;
  the file loads by hand without warnings.
- A7 After D regenerates the profile, it is byte-identical.
- A8 The T23 entry, board, Current Task, the guide's Build state and
  Phase 1 step 3 are updated; evidence in
  `evidence/t23-probe-battery-<date>.txt`.

**Result.** `tech/constraint-probe-battery.lisp`: `REPORT-PROBE-BATTERY-LIST`
(no search), `RUN-PROBE-BATTERY`, `REPORT-PROBE-BATTERY` (section PB, §12).
On crelay-topo at maximum depth 11, threads 16: 21 probes, 12 CHEAP and
9 NOT FOUND, none STOPPED; every NOT FOUND run explored about 467,000
states at cutoff 11. Cheap: agent1 into R8 (4), location20 by the lift (6),
plate1 (2), plate3-5 and switch1 (5), connector1 out of R2 (7), tray1 out of
R8 (6), plate2 released (8), connector1 paired with repeater1 (6). Not
found within 11: location14, location19, location6, box1 out of the alcove,
plate6-8, receiver1, switch2.
- A1 §12, written before code; §12.7 amends P2's held-cargo premise after
  the run (a held tray keeps HAS-LOCATION, tech/tray.lisp).
- A2 expected readings in the evidence file, part 1, before the first run.
- A3 the probe list matched row for row; the four settled rows matched.
  P3.2's provisional bound (5) was wrong: 8, explained by D's answers
  (the ghost tray keeps plate2 pressed while the cycle is open; part 2,
  2.2 b).
- A4, A5, A6 (C3, LABELS/FLET, callees-first, no blank lines inside a
  definition), A7: 72 checks passed, 0 failed, first run, by D on lumpy.
  Load: no warnings reported by D.
- A progress-line FORMAT bug (display only) was fixed after the run.
- A8 this entry, the board, the Current Task, the guide's Build state and
  Phase 1 step 3.

**Status.** COMPLETE, 2026-09-26. **Evidence.**
`evidence/t23-probe-battery-2026-09-26.txt`,
`evidence/t23-probe-battery-checks-2026-09-26.lisp`,
`evidence/t23-probe-battery-results-2026-09-26.lisp`.

## T24 — I2 Cycle-plan check

**Goal.** Check D's stage plan against plate, body and view budgets (T6, RO,
RC) before any action is written. This absorbs RO's scenario input and the
G15 checklist (post-mortem 1.6, 1.7).

**Approval.** D approved it on 2026-09-26, with scope (a): budget checks
only (view, plate budget, exclusion, beam, lift landing). Scope (b), also
discharging the G15 checklist per traversal on concrete states, was
declined.

**Approach (A's technical choices).** D's plan is data: stages of segments,
each segment an extension of RO's scenario plist (view, cycle, bodies,
required devices, pinned plate holders, off-plate bodies, landing). A new
section CP in `tech/constraint-profile.lisp`, entry point
`REPORT-CYCLE-PLAN-CHECK`, not called by `REPORT-STATIC-CONSTRAINT-PROFILE`
(like RO and FH). Families B0 view, B1 plate budget (RO's matching), B2
control conflict (S1), B3 beam (RC), B4 lift landing (CC G15). Labels PASS,
CONFLICT, CONDITIONAL. Test subject: D's crelay-topo plan (cycles 1-3 and
the final leg) as data.

**Acceptance (written before work, approved by D).**
- A1 Specification first: §13 of `Extractor-Specifications.md` holds the
  plan data shape, the families, labels, grades and these checks, before
  code.
- A2 Expected readings first: `evidence/` holds D's crelay-topo plan as data
  and the expected CP output, hand-derived, before the first run.
- A3 The generated output matches A2.
- A4 Checked by script: every segment yields one B1 row per required plate;
  RO's 2026-09-20 scenario gives the same allocation as its first run
  (PERFECT, 3 forced members, 0 pairings).
- A5 Negative tests on plan copies: removing the plate3 holder from the lit
  segment gives CONFLICT citing gate4 and plate3; requiring gate5 and gate7
  together gives a B2 CONFLICT; a lift segment followed by a gate2 segment
  with no landing support gives CONDITIONAL (G15); more required plates than
  bodies gives a shortage.
- A6 No problem object names in the code; callees-first; no LABELS or FLET;
  the file loads by hand without warnings.
- A7 After D regenerates the profile, it is byte-identical.
- A8 The T24 entry, board, Current Task, the guide's Build state and Phase 2
  step 6, and the Stage-Plan template's Check column are updated; evidence
  in `evidence/t24-cycle-plan-check-<date>.txt`.

**Result.** `REPORT-CYCLE-PLAN-CHECK` prints CP (§13); the profile is
unchanged. On D's crelay-topo plan (4 stages, 18 segments): 18 PASS, plan
PASS. The one B4 row, c2.lift -> c2.alcove, passes on the premise that the
landing tray1* is a support; B3 rows in c3.lit and f.gate8 add gate4
(plate3), held by agent1*, with 5 off-plate bodies against RC's least 2.
- A1 §13 of `Extractor-Specifications.md`, written before code.
- A2 plan data, segment breakdown (confirmed by D) and expected output in
  the evidence file, part 1, before the first run.
- A3, A4, A5, A6 (C3, LABELS/FLET, callees-first), A7: 19 checks passed,
  0 failed, first run, by D on lumpy (part 2). Load: no warnings reported
  by D.
- Choices made in code where §13 is silent are recorded in part 2.2
  (empty availability is known-empty; B3 inactive and no-bootstrap cases;
  G15 CHECK wording).
- A8 this entry, the board, the Current Task, the guide's Build state and
  Phase 2 step 6, and the Stage-Plan template's Check column.

**Status.** COMPLETE, 2026-09-26. **Evidence.**
`evidence/t24-cycle-plan-check-2026-09-26.txt`,
`evidence/t24-cycle-plan-check-checks-2026-09-26.lisp`.

## T25 — I4 Domain interview

**Result.** The procedure is written into the Problem-Solving Guide
("Domain interview"), delivered with T18 as the build order specified. It
replaces the question generator in the procedure. The generator's code in
`tech/constraint-ledger.lisp` is left in place, unused by the procedure.

## T26 — I8 Memory estimate before search

**Goal.** Use earlier searches' growth on lumpy to warn when a search at the
requested depth is likely to exhaust memory, so the cutoff is lowered before
the run rather than after a crash (post-mortem 4.3). The case to catch:
crelay-topo's final leg at cutoff 12 from the 80-action checkpoint ran out
of memory; cutoff 10 found the plan.

**Approval.** D approved it on 2026-09-26, with scope (a): pilot, estimate
and guard as standalone entry points, run by hand before a stage's
searches. (Scope (b), building them into the probe battery runner and the
standalone checkpoint searches, was declined.) While reading, A found that
no in-search stop exists: a loadable file cannot stop a parallel search
safely, since `*shutdown-requested*` alone can leave workers waiting on the
task queue, which is local to `PROCESS-PARTITIONED-PARALLEL-BODY`. D chose
the engine setting (same day): one small change to `src/`, outside the
loadable-diagnostic rule, for the guard only.

**Approach (A's technical choices).**
- A new loadable file `tech/constraint-memory-estimate.lisp` under the
  diagnostic rules; it searches, so it is not part of the profile.
- **Pilot.** From the search's own start and goal, with its own settings
  except the cutoff, deepen d = 1 ... k. After each run: `(gc :full t)`, then
  `SB-KERNEL:DYNAMIC-USAGE`. The visited-state tables survive a search until
  the next one resets them (`ww-searcher.lisp`), so this measures what the
  run retained. The cutoff-1 run's reading is the base heap; bytes per state
  at d is (usage − base) / states.
- **Estimate.** States at cutoff c: states at k times r^(c − k), r the larger
  of the last two growth ratios. Memory: base plus states times the largest
  bytes per state among the substantial pilot runs. Compared with
  `SB-EXT:DYNAMIC-SPACE-SIZE`: SAFE, AT RISK or LIKELY TO EXHAUST, and the
  deepest cutoff that is SAFE.
- **Guard.** A new engine setting `*max-states-processed*` (default NIL, no
  effect). Parallel workers check it at their bound refresh, beside the
  existing shutdown checks, and stop through
  `REQUEST-PARALLEL-WORKER-SHUTDOWN`, which wakes waiting workers. The guard
  entry point sets it from the memory budget, runs the search, restores it,
  and reports STOPPED when the limit ended the run.
- Found in reading, for A2: T23's 21 probes share one search space (every
  NOT FOUND probe explores the same states within 0.2%), so its results are
  one growth curve, not 21 samples.

**Acceptance (written before work, approved by D).**
- A1 Specification first: §14 of `Extractor-Specifications.md` holds the
  pilot, estimate, labels, guard and these checks, before code.
- A2 Back-test with no new search: from T23's saved results, predict each
  next-cutoff state count from the cutoffs below it, and record the error
  table in `evidence/`. Refined by A before code, confirmed by D 2026-09-26
  (reason: T23's data is one curve, and under-prediction is the unsafe
  direction): along that curve,
  one-step predictions are never below half the actual count and never above
  three times it.
- A3 Retrospective test on lumpy: pilot runs from the 80-action checkpoint
  (`t10-c3-location15-checkpoint.txt`, goal and settings of the T10 final
  search) up to cutoff 9 label cutoff 12 LIKELY TO EXHAUST and cutoff 10
  SAFE. Expected readings go into `evidence/` before the run. Refined by A
  before code, confirmed by D 2026-09-26: at the T10 heap (16,000 MiB),
  cutoff 12 LIKELY TO EXHAUST and cutoff 10 *not* LIKELY TO EXHAUST, since
  the cutoff-10 run's retained memory was never measured (§14.6).
- A4 Negative tests: a pretended smaller ceiling gives LIKELY TO EXHAUST; a
  tiny budget makes the guard stop the run with STOPPED instead of running
  out of memory; with `*max-states-processed*` NIL a search's outcome and
  state count are unchanged.
- A5 No problem object names in the new file; callees-first; no LABELS or
  FLET; the file and the engine load without warnings.
- A6 After D regenerates the profile, it is byte-identical.
- A7 The T26 entry, board, Current Task, the guide's Build state and Phase 3
  step 8 are updated; evidence in `evidence/t26-memory-estimate-<date>.txt`.

**Progress.** A1 §14 written before code. A2 MET: factors 0.60 to 2.55
(evidence part 1.1). A3 and A4 expected readings in part 1.2–1.3, before
code. The engine setting, `tech/constraint-memory-estimate.lisp`, a pilot
script and a checks file were written; D's first run on lumpy (part 2): 19
passed, 2 failed, both A3. The guard stopped a real 16-thread search at
20,004,564 states against a limit of 20,000,000. A3 failed on A's
expectations and on §14.3's bytes-per-state rule (part 2.2).

**Closure.** D, 2026-09-26: the maximum search depth is D's own
determination, made by independent experiments before the constraint
analysis (typically deepening an exhaustive search for the end goal until
it takes longer than D will spend), and every search stays within it. D
chose to remove the memory analysis and the guard altogether. Removed: the
engine setting and its check (`src/ww-settings.lisp` and
`src/ww-parallel.lisp` are as before T26) and
`tech/constraint-memory-estimate.lisp`. Kept as record: §14 of
`Extractor-Specifications.md` (with a closing note) and the evidence files,
whose pilot script and checks load the removed file and no longer run. The
guide's Search settings, Build state, Phase 3 rules and Handoff template
were updated (part 3).

**Status.** CLOSED, 2026-09-26. **Evidence.**
`evidence/t26-memory-estimate-2026-09-26.txt`,
`evidence/t26-pilot-results-2026-09-26.lisp`,
`evidence/t26-run-pilot-2026-09-26.lisp`,
`evidence/t26-memory-estimate-checks-2026-09-26.lisp`.

## T27 — Stage-organized ledger and Record Schema revision

**Goal.** Reorganize the ledger by stage instead of by spine link, and make
the ledger file itself the record rather than a replay of ingest scripts.
Revise `Status-Algebra-and-Record-Schema.md` to match. Split out of T18 by
D's scope decision (2026-09-25) so that T18 stayed documents-only; it waited
until T24 settled what a stage record needs.

**Approval.** D approved it on 2026-09-26, with scope (a): the stage record,
file-level editing and a stage-ordered reporter, for new problems.
Scope (b), migrating crelay-topo's version-1 ledger to stages, was declined;
that ledger is left as is. Scope (c), a per-stage search recommender, was
declined; the T3 recommender and T5 question generator stay in
`tech/constraint-ledger.lisp`, unused by the stage procedure.

**Approach (A's technical choices).**
- A new record kind `:stage` (prefix ST): the plan stage's id and intent,
  the plan-data file it comes from (segments are referenced, not copied),
  its CP label with date and evidence, its realization (hand, search or
  mixed), its endpoint (checkpoint file, action count, SHA-256) and its
  attempts (BD ids). D's tricks enter as user-asserted premises in
  `:depends-on`, so standing (§8) and retraction (§9) apply unchanged.
- Lifecycle: the link statuses plus `:superseded`, with `:superseded-by`,
  for a stage D revises.
- A bound gains `:for-stage` beside `:for-link`. X1–X4 are unchanged; bounds
  keep their own report section.
- File of record: entry points take the ledger path, read it, apply one
  change, check well-formedness and write it back; a failed check leaves the
  file unchanged. Header version 2; version-1 ledgers read and report as
  before. The Handoff records the ledger's SHA-256, not a script order.
- Reporter: stages in plan order (standing, live guesses, endpoint,
  attempts), then the existing sections.
- The Stage-Plan template's Status and Endpoint columns become the stage's
  ST id, so status is kept in one place.

**Acceptance (written before work, approved by D).**
- A1 Specification first: `Status-Algebra-and-Record-Schema.md` holds the
  `:stage` kind, its lifecycle, the new well-formedness rules, version 2 and
  the file-level entry points, before code.
- A2 Expected output first: a test fixture in `evidence/` before the first
  run: D's crelay-topo plan (T24's plan data) as a stage ledger with its 80-
  and 87-action endpoints, and its report derived by hand.
- A3 The generated report matches A2.
- A4 Compatibility: `evidence/ledger-checks-2026-09-20.lisp` and
  `evidence/ledger-reporter-checks-2026-09-24.lisp` still pass; crelay-topo's
  version-1 ledger reads, passes `CHECK-LEDGER-WELL-FORMED`, and reports
  byte-identical to its current output.
- A5 File of record: a sequence of file-level edits reads back equal to the
  in-memory ledger; an edit that fails well-formedness leaves the file
  byte-identical; unknown keys survive the round trip.
- A6 Negative tests: a `:closed` stage without a validated endpoint is
  rejected; a `:superseded` stage must name its successor; retracting a
  trick premise invalidates a stage resting on it alone, but not one with a
  surviving alternative.
- A7 No problem object names in the code; callees-first; no LABELS or FLET;
  the file loads by hand without warnings.
- A8 The T27 entry, board, Current Task, the guide (Phase 3 step 9, the
  Stage-Plan template, the per-problem file table) and the Build state line
  are updated; evidence in `evidence/t27-stage-ledger-<date>.txt`.

**Result.** A version-2 ledger records D's approved plan stage by stage, and
the file is its own record: every change is one `LEDGER-FILE-APPLY`, which
reads, checks, applies and writes, leaving the file byte-identical on any
signal. New in `tech/constraint-ledger.lisp`: `MAKE-STAGE-LEDGER`,
`MAKE-LEDGER-STAGE`, `SET-LEDGER-STAGE-CHECK`, `REALIZE-LEDGER-STAGE`,
`CLOSE-LEDGER-STAGE`, `FILE-LEDGER-STAGE-BOUND`, `SUPERSEDE-LEDGER-STAGE`,
`LEDGER-FILE-APPLY` and the STAGES report section; WF19–WF25. On the
fixture (D's crelay-topo plan: c1, a superseded first c2, c2, c3, f) the
report lists five stages in plan order, all CONDITIONAL on D's approval
premise, four closed with endpoints at 11, 31, 80 and 87 actions.
crelay-topo's version-1 ledger is untouched and reports as before.
- A1 the schema's T27 revision, before code (§16 item 12 records three
  points settled while writing A2 and reading T2's checks: endpoints inside
  a longer archive, a superseded stage's own standing, and
  `MAKE-STAGE-LEDGER` beside a version-1 `MAKE-REALIZATION-LEDGER`).
- A2 the evidence file, part 1, before code; D captured the A4 baseline
  (135 lines) with the pre-T27 code.
- A3, A4, A5, A6, A7: 67 checks passed, 0 failed, second run, by D on lumpy
  (part 2.5). The first run's one FAIL was the checker's A7 blank-line test
  reading a section comment into `LEDGER-STAGE-RECORD`; the checker was
  corrected and the code left unchanged (part 2.4).
- A4 T2 and T14 checks unchanged: 346 and 54 passed. A7 load: T, no
  warnings (D).
- A8 this entry, the board, the Current Task, and the guide: the
  per-problem file table, the Build state, Phase 2 step 6, Phase 3 step 9,
  the Loading section and the Stage-Plan and Handoff templates.

**Status.** COMPLETE, 2026-09-26. **Evidence.**
`evidence/t27-stage-ledger-2026-09-26.txt`,
`evidence/t27-stage-ledger-checks-2026-09-26.lisp`,
`evidence/t27-baseline-capture-2026-09-26.lisp`,
`evidence/t27-crelay-report-baseline-2026-09-26.txt`.

## T28 — Mechanic contracts for jump and recorder, entry for step

**Goal.** Close T19's UNCOVERED list so a new problem splicing jump, recorder
or step needs no hand contract at the coverage gate: contracts for jump and
recorder, and a registry entry for step.

**Selection (A, 2026-09-26, at D's request to judge option 2).** Kept: this
task, T29 (D's own probe goals, T23 (b)) and T30 (cleanup). Omitted:
T22 (b), since FH already gives the live readings and NH's hints read the
static database; T24 (b), since a hand stage is validated by
`VALIDATE-ACTION-SEQUENCE` and FH shows the propagated successor; T27 (b),
since crelay-topo is closed and its version-1 ledger reads as is; T27 (c),
since choosing a stage's realization is A's judgment (Phase 3 step 8).

**Approval.** D approved it on 2026-09-26, as proposed.

**Approach (A's technical choices).**
- jump, a contract: the reach limit on landing and on clearing a closed gate,
  screen or wall. Rows: one per direction of each jumping arc, with an open
  and a closed reading of its gates, each as "from the floor" or the raise
  above the floor the launch needs.
- recorder, a contract written from `-recorder-session`, `-recorder-core`,
  `-recorder-cycle-boundary` and `-recorder-solution`. Rows: cycles allowed,
  live → ghost pairs, each recorder's position. No cycle count (analysis,
  not a contract).
- step, an extractors entry: S2 T6 for plates; blower mounting is in the
  floor-blower contract; gears-mounted fans have no component.

**Acceptance (written before work, approved by D).**
- A1 Specification first: §8.7 of `Extractor-Specifications.md`.
- A2 Expected crelay-topo MC readings in `evidence/`, hand-derived, before
  the first run.
- A3 The generated rows match A2.
- A4 Every public tech crelay-topo splices appears once; none UNCOVERED.
- A5 Negative tests: registry without recorder prints it UNCOVERED; with the
  reach limit at 5 the location4 → location6 open reading needs no raise.
- A6 No problem object names; callees-first; no LABELS or FLET; loads
  without warnings.
- A7 In the regenerated profile every section but MC is byte-identical.
- A8 This entry, the board, the Current Task and the guide's Build state.

**Progress.**
- A1 §8.7 written before code. A2 in the evidence file, part 1, before code.
- Code: registry entries and five functions in the MC block of
  `tech/constraint-profile.lisp`, callees-first before
  `REPORT-MECHANIC-CONTRACT`.
- Pre-check by A (not the acceptance run): the MC block compiled in SBCL
  against stubbed engine functions with no warnings, and the checks file's
  A3–A6 checks passed on stubbed crelay-topo data (19 of 19). A7 needs the
  staged engine.

**Result.** MC now reads 16 of 16 COVERED on crelay-topo. The jump rows
show the alcove (location6) is entered from location4 or location5 only
through an open gate2 and from a base at least 1/2 up, and is level from
location20; a closed gate2 needs a 9/2 launch, above any riser the problem
has. The recorder rows give unlimited cycles, four live → ghost pairs and
recorder1 at location1.
- A1 §8.7, before code. A2 evidence part 1, before code.
- A3–A7: 21 checks passed, 0 failed, second run, by D on lumpy (part 2.3).
  The first run's A7 error was the checker's temporary file name colliding
  with the profile writer's own temporary; the checker was corrected, the
  code unchanged (part 2.2).
- A7: regenerated 1562 lines against 1526; every section but MC identical.
- A8 this entry, the board, the Current Task and the guide's Build state.

**Status.** COMPLETE, 2026-09-26. **Evidence.**
`evidence/t28-mechanic-contracts-2026-09-26.txt`,
`evidence/t28-mechanic-contracts-checks-2026-09-26.lisp`.

## T29 — D's own probe goals in the probe battery

**Goal.** Let D state subgoals (typically from the domain interview) and run
them through the probe battery's deepening, labels and report, as family P5,
instead of as loose searches. T23's declined scope (b), taken up by A's
selection (T28 entry) at D's request.

**Approval.** D approved it on 2026-09-26, as proposed.

**Approach (A's technical choices).** Found in reading: `RUN-PROBE-BATTERY`
already takes a probe list. Added, in `tech/constraint-probe-battery.lisp`:
- `D-PROBES`, which checks each entry `(subject goal provenance)` and every
  literal head of its goal before any search, and numbers them P5.1 ...;
- a fifth family title, "D's own";
- a provenance column on P5 rows;
- an optional probe list for `REPORT-PROBE-BATTERY-LIST`.

P1–P4, the labels and the results format are unchanged.

**Acceptance (written before work, approved by D).**
- A1 Specification first: §12.8.
- A2 Expected readings before the run: two crelay-topo P5 probes settled
  by T23 (START; CHEAP 2 @ 2).
- A3 P5 rows match A2; P1–P4 equal T23's recorded probes.
- A4 Negative tests: an entry without a goal signals; an empty list gives an
  empty P5 block; an undeclared relation signals from `D-PROBES`, before
  any search.
- A5 No problem object names; callees-first; no LABELS or FLET; loads
  without warnings.
- A6 This entry, the board, the Current Task, the guide's Phase 1 step 3 and
  the domain interview.

**Result.** `(d-probes '((subject goal "provenance") ...))` gives P5 probes for
`RUN-PROBE-BATTERY`; the report prints them in a "P5 D's own" block with their
provenance. On crelay-topo: agent1 at location1 is START; agent1 on plate1 at
location2 is CHEAP 2 @ 2.
- A1 §12.8, before code. A2 evidence part 1, before code.
- A3–A5: 17 checks passed, 0 failed, first run, by D on lumpy (part 2.2).
  Load: no warnings reported by D.
- A6 this entry, the board, the Current Task, the guide's Phase 1 step 3 and
  domain interview step 4.

**Status.** COMPLETE, 2026-09-26. **Evidence.** `evidence/t29-d-probes-2026-09-26.txt`,
`evidence/t29-d-probes-checks-2026-09-26.lisp`,
`evidence/t29-d-probes-results-2026-09-26.lisp`.

## T18 — Document restructure to the phased strategy

**Goal.** Reorganize the constraint-method documents to support the phased
strategy of `Post-Mortem-2026.md` section 2, per the verdicts and target file
set in its section 4.1. T25's domain-interview procedure is written into the
guide.

**Approval.** D approved it on 2026-09-25, with these decisions:
- The guide is named `Problem-Solving-Guide.md`, not Method Guide.
- The stage-organized ledger and its Record Schema revision are left out;
  they become T27.
- Stale file paths in `tech/*.lisp` comments and printed strings are left
  alone, to be fixed by the next task that edits those files.
- Documents only: no code, instance or generated-file changes.

**Acceptance (written before work, approved by D).**
- A1 Nothing lost: every moved or archived file is byte-identical, with
  SHA-256 hashes recorded before and after.
- A2 Entry points: the plan and the guide each state in their first 10
  lines what a session does, and both lead to the problem's Handoff.
- A3 Size: the plan is at most 450 lines; the crelay-topo Handoff at most 150.
- A4 Every file path named in the plan, the guide and the Handoff exists
  (checked by script).
- A5 A single source of state: the plan's Current Task (method) and the
  Handoff (per problem).
- A6 Cold start: a session given only "Find your instructions at <entry
  file>" reaches the current task or problem state without opening
  `archive/`.

**Technical choices** (A's):
- Schema gaps are kept as `.txt`, with an index header above the original
  text, byte for byte, so the old file's layout survives.
- Extractor Specifications copies each source verbatim in fenced blocks, so
  the provenance stays checkable.
- The Briefing and Stage Plan are templates in the guide, filled in only
  for new problems; crelay-topo gets no after-the-fact versions.
- crelay-topo's `constraint-evidence/` stays flat; subfolders and an index
  apply to new problems.
- The whole pre-T18 plan is archived rather than cut into pieces, so it
  hashes identical to the original.

**Status.** COMPLETE, 2026-09-25. **Evidence.**
`evidence/t18-restructure-2026-09-25.txt` holds the hashes, the moves, and
the A1–A6 checks.

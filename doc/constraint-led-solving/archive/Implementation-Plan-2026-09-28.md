# Constraint-Led Method — Implementation Plan

**On reading this file, continue the method's development.** Take the Current
Task below. If its approval is granted, begin work; if not, present the task,
your intended approach and its acceptance criteria to D, and ask before
changing any file. To solve a problem instead, the entry point is
`doc/constraint-method/Problem-Solving-Guide.md`, which leads to the
problem's `Handoff.md`.

Opened 2026-09-20; restructured by T18, 2026-09-25, and by T30, 2026-09-26.
This file holds the board, conventions, policy and current task. It does not
hold task history: completed entries T0–T17, verbatim as of 2026-09-25, are
in `archive/Implementation-Plan-2026-09-25.md`; T18–T29, in the whole plan
as of 2026-09-26, are in `archive/Implementation-Plan-2026-09-26.md`. Nothing about this work lives in
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

**Next: T46 — Cleanup after T31-T45.** Approval granted by D, 2026-09-28;
D starts it in a new session. Read its entry below, then do it within the
approved scope without asking again. T31-T45 are COMPLETE; G20-G22 remain
resolved at their documented scope. On completion, set this pointer to "none
waiting" and stop for D. No puzzle solve or probe battery is authorized.

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
- **Queue complex development needs found during problem-solving here.**
  Give each a new task id, originating problem and Handoff, the need,
  proposed scope, dependencies and acceptance criteria; add its board row.
  Initially use status PROPOSED and Approval "Not requested" unless D has
  already approved that scope. Queuing is not implementation approval.
  Record any blocking dependency in the problem's Handoff so solving can
  resume from a clear next step. Update Current Task to the next waiting task.

## Working conventions for method development

The general rules (roles, grades, C1/C3/M2–M5, technical vs strategic
choices, tooling) are in the Problem-Solving Guide and apply here too.
These are specific to building the components:

- **`tech/constraint-profile.lisp` is a loadable diagnostic.** It is never
  named in an `include-tech` directive and is never an ASDF component. It is
  plain Common Lisp in `:WW`, with no `define-*` DSL forms (a LOADed file gets
  no tech splice). The same holds for `tech/constraint-ledger.lisp` and
  `tech/constraint-state-probe.lisp`, `tech/constraint-arrangement.lisp` and
  `tech/constraint-boundary.lisp`.
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
| `doc/constraint-method/archive/` | the plan before T18 (T0–T17 entries) and before T30 (T18–T29 entries); the G15 checklist |
| `tech/constraint-profile.lisp` | the static extractors (S0–S7, T6, RC, RO, MC incl. beam-crossing, beam-relay and floor-gears rows, SD); supplied-state views, crossings, relay lighting, equipment and service transitions (6.1, BX, RL, EQ, SW) |
| `tech/constraint-ledger.lisp` | ledger (stages, file of record), recommender, ingester, question generator (T2–T5, T27) |
| `tech/constraint-state-probe.lisp` | one-step applicable-action probe from a replayed prefix |
| `tech/constraint-arrangement.lisp` | explicit supplied-state stability check (T34); no search |
| `tech/constraint-boundary.lisp` | BT: one supplied recorder boundary or support change (T45); no search |
| `tech/constraint-probe-battery.lisp` | the probe battery (PB, P1–P5; T23, T29); the only diagnostic that searches |
| `src/ww-search-checkpoint.lisp` | standalone search checkpoints: export, import (by replay), validate |
| `doc/search-strategies/standalone-checkpoints.md` | user-level description of the checkpoint workflow |
| `doc/problems/<p>/Handoff.md` | a problem's current state (the only per-problem state file) |
| `doc/problems/<p>/Briefing.md`, `Stage-Plan.md` | Phase 1 and 2 records (new problems) |
| `doc/problems/<p>/Constraint-Static-Profile.txt` | generated extractor output |
| `doc/problems/<p>/Constraint-Realization-Ledger.txt` | the problem's ledger |
| `doc/problems/crelay-topo/Constraint-Prediction-Register.txt` | FROZEN record of the crelay-topo validation experiment |

## Objective

The method begins with recorded intake: problem spec, optional corresponding
diagram or "none", and D's maximum search depth. Then come the static profile
(with hand contracts for uncovered mechanics), a subgoal dialogue beginning
with a summary and anticipated difficulties, and full-path validation from
the start using VALIDATE-ACTION-SEQUENCE or VALIDATE-SEARCH-CHECKPOINT.
Optional probes and other tools inform D's intuition when requested; an
advance stage plan and ledger are not required. The Problem-Solving Guide
is authoritative for this procedure.

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
| T30 | Cleanup before a new problem (plan archive, path check, stray file) | COMPLETE | approved by D, 2026-09-26 | T28, T29 |
| T31 | Preserve quotient reachability and report unavailable hints | COMPLETE | approved by D, 2026-09-27 | T30 |
| T32 | Wall-blower coverage and stable relay-station qualifications | COMPLETE | approved by D, 2026-09-27 | T30 |
| T33 | Physical and recording-view sightlines | COMPLETE | Implementation and focused checks approved 2026-09-27 | T32 |
| T34 | Stability of a supplied relay arrangement | COMPLETE | Approved 2026-09-27 | T33 |
| T35 | Fixed-beam, jammer and stairs contracts; connectorless beam analysis (G20) | COMPLETE | Approved by D, 2026-09-27 | — |
| T36 | S4 cut-keeper support for alternative door families (G21) | COMPLETE | Approved by D, 2026-09-27 | T31 |
| T37 | T6 budget: one demand per equivalent device set (G22) | COMPLETE | Approved by D, 2026-09-27 | — |
| T38 | Remove the probe battery's states limit (D: depth cutoff only) | COMPLETE | Approved by D, 2026-09-27 | — |
| T39 | Consolidate the Problem-Solving Guide around the subgoal dialogue | COMPLETE | Approved by D, 2026-09-27 | — |
| T40 | Beam crossings and cut order | COMPLETE | Granted by D, 2026-09-28; separate session | T19 |
| T41 | Competing colors and persistent connector links | COMPLETE | Granted by D, 2026-09-28; separate session | T33, T40 |
| T42 | Removable equipment and mounted-fan consequences | COMPLETE | Granted by D, 2026-09-28; separate session | T19, T32 |
| T43 | Temporary requirements, setup dependencies and service withdrawal | COMPLETE | Granted by D, 2026-09-28; separate session | T35, T41, T42 |
| T44 | Compatible object roles and explicit resource reservations | COMPLETE | Granted by D, 2026-09-28; separate session | T24, T37, T42 |
| T45 | Dependencies across recorder boundaries and support changes | COMPLETE | Granted by D, 2026-09-28; separate session | T28, T34, T43, T44 |
| T46 | Cleanup after T31-T45 (plan archive, path check, transfer files) | APPROVED | Granted by D, 2026-09-28; separate session | T45 |

## T46 — Cleanup after T31-T45

Status APPROVED. Approval granted by D, 2026-09-28; to be done in its own
session. Model: T30 (entry below; `evidence/t30-cleanup-2026-09-26.txt`).

Need: this plan has grown to about 925 lines against T30's 450-line bound,
because the completed T31-T45 entries were never moved to `archive/`; two
transfer archives wait in `_to_delete/`.

Scope (no code, generated file or Handoff content change beyond references):
1. Copy this whole plan, byte for byte, to
   `archive/Implementation-Plan-2026-09-28.md`; SHA-256 before and after.
2. Rewrite this file to the session rules, Current Task, reading policy,
   conventions, working conventions, file map, objective and board, with a
   one-paragraph pointer per archive (T0-T17, T18-T29, T30-T46 or as split),
   and T46's own entry. Keep the board rows; move task entries, the T40-T45
   shared requirements and the component build history to the archive copy.
   Add the new archive to the file map and the opening paragraph.
3. By script, check that every path named in this plan, the Guide, the
   Extractor Specifications index, and the crelay-topo, windtunnel-topo,
   claustro-topo, corner-topo and phobia-topo Handoffs exists; fix a broken
   reference in the document, never by moving a file.
4. Delete `_to_delete/t43-transfer.tar.gz` and `_to_delete/t45-transfer.tgz`
   (ask D for delete permission once if the shell refuses).
5. If D has deleted `artifacts/`, mark its two references (`doc/README.md`
   and `doc/search-strategies/parallel-search-defaults.md`) as removed on
   that date, without rewriting the documents' other content.

Acceptance (written before work):
- A1 archive copy byte-identical to the plan as it stood (hashes recorded);
- A2 this plan at most 450 lines, stating in its first 10 lines what a
  session does, with the Current Task reachable without opening `archive/`;
- A3 every path named in the checked files exists (script output kept);
- A4 no code, profile, ledger or Guide procedure change (hashes before and
  after for `tech/`, `src/`, `probs/` and generated profiles/ledgers);
- A5 the two transfer archives gone; evidence
  `evidence/t46-cleanup-2026-09-28.txt`; board and Current Task updated.

## T40-T45 — Shared delivery requirements

Approval granted by D, 2026-09-28, for all six recommendations; each task
is to be planned and implemented individually in a new session. The entries
below preserve the approved scope; interfaces and detailed algorithms remain
for that task's planning. Dependencies name components to reuse, not authority
to implement several tasks in one session.

For each task:
- Read current technology semantics and existing diagnostics before design.
  Derive general rules from source, never hard-code a solved puzzle's answer.
- Specify inputs, outputs, grades, scope limits and focused acceptance checks
  in `Extractor-Specifications.md` before coding. Reuse existing components.
- Put generally available facts/candidates in the static profile. Where an
  arrangement or transition is needed, expose a supplied-scenario diagnostic
  and say explicitly when its inputs are absent. Do not infer a complete plan.
- Keep geometric candidates, start-state observations, conditional requirements,
  supplied-state results and validated trajectories distinct. Preserve caller
  state in diagnostics; missing/unsupported analysis is not impossibility.
- Run focused checks and relevant regressions; retain named evidence under
  `doc/constraint-method/evidence/`. Existing solved traces are reference
  evidence, not permission to search for new solutions or reopen the problems.
- Update the Guide's usage/coverage and the specifications alongside code;
  reconcile relevant schema gaps at the exact scope established. Regenerate
  any updated profile through its writer, never edit generated text by hand.
- Record results and limitations here, update the board/Current Task, remove
  task-created temporary files, and stop before the next task/session.

## T40 — Beam crossings and cut order

Status COMPLETE, 2026-09-28. Approval granted by D, 2026-09-28; implemented
in its own session. Origin: corner-topo; `doc/problems/corner-topo/Handoff.md`
and `doc/problems/corner-topo/Briefing.md` (Contracts and Subgoal log).
Depends on T19 mechanic coverage.

Need: the solution used an early beam cut to prevent a later crossing from
cutting another beam. MC currently leaves beam-crossing uncovered; candidate
crossings were calculated manually during solving.

Scope: add a source-grounded beam-crossing contract and instance report of
crossing beam segments, ordered along each directed beam from its source,
with gate positions relative to crossings. Read the engine's crossing data;
do not substitute an independent approximate geometry model. Separate possible
crossings from active cuts; any evaluated cuts require an explicit arrangement.
Sources: `tech/beam-crossing.lisp`, its coordinate/init substrates, current
profile S6/RC and Guide beam-crossing rules.

Acceptance:
- Report source order and gate-relative order for corner's relevant segments.
- Focused fixtures distinguish inactive beams, an earlier cut shielding a
  later crossing, and crossings before versus beyond a closed gate.
- Agreement with engine crossing semantics is checked; geometric rows do not
  claim joint beam feasibility or reachability. Non-crossing profiles regress.

Result (2026-09-28): specification 8.10 written before code. MC adds a
beam-crossing contract; corner-topo has zero UNCOVERED. Instance rows read
the published pool, each stored beam's source order, gate splits and a
crossing index with engine-derived points (agreeing with the Briefing's hand
geometry). New optional `report-beam-crossing-scenario` evaluates one settled
state (propagation fixed point on a private copy, else UNRESOLVED): gates
read, active set, and per live beam ACTIVE / REACHED-INACTIVE / BEYOND CUT /
BEYOND CLOSED labels, each agreeing with the engine's reaching query. 462
focused checks passed: corner orders and gate positions, start and trace-
prefix states (crossing7 shielding crossing12 at prefix 14), closed-gate
fixtures on both sides of gate1, unresolved inputs, caller-state
preservation. Compiles without warnings; crelay, windtunnel, claustro,
phobia and rumin profiles byte-identical; corner changes only in MC and was
regenerated. Guide and corner Handoff updated; no schema gap applied. No
search. Temporary files removed. Evidence:
`evidence/t40-beam-crossings-2026-09-28.md`.

## T41 — Competing colors and persistent connector links

Status COMPLETE, 2026-09-28. Approval granted by D, 2026-09-28; implemented
in its own session.
Origin: `doc/problems/corner-topo/Handoff.md` and its `Briefing.md`.
Depends on T33 supplied-view checks and T40 crossing reporting.

Need: corner changes a connector's effective color by cutting/restoring its
direct feed. Independently possible color routes need not work together.

Scope: provide an accessible beam-relay contract and problem-specific candidate
source competition/link information. Explain first-arriving propagation layers,
same-layer color conflict, later colors, pairing capacity/ownership, persistent
links and pickup clearing. Reuse supplied-view lighting checks for an explicit
combined arrangement; distinguish pairing, live beam and delivered color.
Sources: `tech/beam-relay.lisp`, T33 specification/implementation, T40 output,
and the corner subgoal log. Do not confuse propagation layers with travel time.

Acceptance:
- Check earlier single-color arrival, simultaneous conflicting colors, and
  cut/restored direct feed with an alternative relayed color.
- Check link persistence through a blocked beam and removal on pickup; retain
  outgoing-pairing capacity and incoming-link distinctions.
- Report joint-arrangement outcomes using engine rules; no claim that separate
  per-color candidates compose. Preserve physical/recording-view distinctions.

Result (2026-09-28): specification 8.11 written before code. MC's beam-relay
entry is now a contract (engine layer, conflict, persistence, capacity and
receiver rules); its rows give capacity, hue pools, start links and each RC
station's directly visible transmitters by hue (COMPETING HUES: corner
location1-3). New optional `report-relay-lighting-scenario` (RL) replays one
settled state's lighting by layer through the engine's link query, with each
stored link's sightline, cut and outcome, outgoing/incoming capacity and
receiver feeders; engine agreement is checked; `:chains` reuses T33. 228
corner and 45 windtunnel checks passed: prefix 14 relayed red in layer 2 with
blue CUT, prefix 15 blue first and red IGNORED, a same-layer CONFLICT fixture,
pairing persistence at prefix 8, pickup and retaining-pickup variants,
capacity, T33 chain CLEAR/BLOCKED, windtunnel physical vs recording, and
UNRESOLVED inputs. T40's 462 checks still pass; compiles without warnings.
Profiles splicing beam-relay change only in MC and were regenerated (corner,
crelay, phobia, windtunnel; rumin has no stored profile); claustro identical.
Guide and Handoffs updated; no schema gap applied. No search. Temporary files
removed. Evidence: `evidence/t41-competing-colors-2026-09-28.md`.

## T42 — Removable equipment and mounted-fan consequences

Status COMPLETE, 2026-09-28. Approval granted by D, 2026-09-28; implemented
in its own session.
Origin: `doc/problems/phobia-topo/Handoff.md` and its `Briefing.md`
(floor-gears hand contract, SG2 and final lift). Depends on T19 and T32.

Need: removing the sole fan disables its old wall stream and frees the jammer;
mounting it on floor gears supplies the final lift. Current step coverage
explicitly lacks gears-mounted fans, and floor-gears needed a hand contract.

Scope: report removable components, compatible mounts and their prerequisites,
effects of removal/installation, and boarding, lift and landing conditions.
Distinguish turning gears from an effective stream and fixed fixtures from
removable equipment. Reuse wall/floor-blower coverage rather than duplicating it.
Sources: `tech/floor-gears.lisp`, `tech/-gears-fan.lisp`,
`tech/-floor-blowing.lisp`, `tech/step.lisp`, and existing MC contracts.

Acceptance:
- Phobia rows identify one fan shared between its original and destination
  mounts, with removal disabling the old stream and installation enabling lift.
- Check mount vacancy/reach, inert ground fan versus mounted boarding support,
  running gears without a fan, and loss of lift/landing requirements.
- Fixed blower behavior is unchanged; static compatibility is not transport
  feasibility. Close the identified MC coverage gaps at tested scope.

Result (2026-09-28): specification 8.12 written before code, with a dated
clarification. MC's floor-gears entry is a contract and step names both
boarding contracts; phobia has zero UNCOVERED. The floor-gears block lists
fan1 shared by wgears1 (start, EFFECTIVE STREAM) and fgears1 (VACANT,
TURNING, NO FAN), one stream at a time, reach sites, wgears1's removal
consequence and 32 gated arcs, and fgears1's boarding, lift, landing and
exits. New optional `report-equipment-scenario` (EQ) reads one settled
state's fans, mounts, boarding, mounting, removal and lifted occupants, with
engine agreement, and with `:before` what changed between two states. 166
phobia checks passed on validated-path prefixes 11-19, 52-54 and settled
fixtures: jammed removal, the freed jam, stream lost while turning, inert
ground fan, reach and occupancy failures, stream gained, boarding, lift,
and loss of lift by jam or unmounting; UNRESOLVED inputs; caller states
unchanged. Five problems without fans check UNRESOLVED EQ and no block. T40
(462) and T41 (228) still pass; compiles without new warnings. Profiles:
phobia changes only in MC; crelay, windtunnel and rumin only in the step
line; corner and claustro identical. Guide and Handoffs updated; no schema
gap applied. No search. Temporary files removed. Evidence:
`evidence/t42-removable-equipment-2026-09-28.md`.

## T43 — Temporary requirements, setup dependencies and service withdrawal

Status COMPLETE, 2026-09-28. Approval granted by D, 2026-09-28; implemented
in its own session.
Origins: corner-topo, phobia-topo and claustro-topo; read each problem's
`doc/problems/<problem>/Handoff.md` and `Briefing.md`.
Depends on T35, T41 and T42.

Need: a service may be required during a crossing but expendable afterward;
placing its eventual provider may itself require that service already working.

Scope: connect control/beam facts to conditional placement, retrieval and access
requirements. Report setup dependencies and consequences of withdrawing a
service (pickup, beam interruption, unmounting or relevant control change).
Use an explicit arrangement/transition for concrete consequences; expose
static prerequisite links where supported. Separate transit, return and final
requirements. Reuse RC bootstrap/latch distinctions, CC, FH and CP.
Sources: the three Briefings; current S1/MC/RC and action semantics; G15/G16
and existing supplied-arrangement limits in the specifications.

Acceptance:
- Represent claustro's gate-dependent jammer placement, corner's temporary
  receiver1 service and phobia's deliberate receiver2 shutdown.
- Name affected devices and unresolved retrieval/return conditions; distinguish
  an alternative provider/override from unconditional loss of service.
- A dependency cycle flags a setup question, not impossibility. Snapshot
  consistency does not establish a realizable ordering or safe transfer.

Result (2026-09-28): specification 8.13 written and saved before code, with a
dated clarification. New profile section SD (grade 2, last): per service (gate
open, clause-named drive clear, receiver active) its providers -- S1 CONTROL
literals, MC jam sites, no-fan gears, RC chains (BOOTSTRAP/LATCH) and fixed
corridors -- classified by an AND-OR closure as DIRECT, SUPPORTED, NEEDS
<service> FIRST (with path) or UNSUPPORTED; a standing-providers pass exposes
dependencies another jam would hide. Also transit/return door sets, FINAL and
TEMPORARY services, access, retrieval and OPPOSED CONTROLS. New optional
`report-service-transition` (SW) compares two settled states: services KEPT,
KEPT BY ALTERNATIVE (OVERRIDE), LOST or GAINED; supplies withdrawn with the
devices they drive; arcs, mobility, retrieval; transit/return/final
requirements; engine agreement with OBSTACLE-CLEAR. 126 claustro, 56 corner
and 58 phobia checks passed, plus general checks on six problems: claustro's
plate-room gate1 sites NEED gate1 FIRST via gate2/gate3 -> receiver1; 5 -> 6
withdraws receiver1, 28 -> 29 keeps gate1 by jammer2; corner gate1 TEMPORARY,
14 -> 15 loses it with return NOT MET and finals MET; phobia 43 -> 44 keeps
wblower2 by the jam and gains wblower3. T40-T42 checks still pass; compiles
without new warnings. All five stored profiles change only by the added SD
section and were regenerated. Guide, Handoffs and G16 (in part) updated.
Recorder problems: SD physical only, SW UNRESOLVED (T45). No search.
Temporary files removed. Evidence:
`evidence/t43-services-and-setup-2026-09-28.md`.

## T44 — Compatible object roles and explicit resource reservations

Status COMPLETE, 2026-09-28. Approval granted by D, 2026-09-28; implemented
in its own session.
Origin: `doc/problems/claustro-topo/Handoff.md` and its `Briefing.md`
(body budget and climbing-box reservation). Depends on T24, T37 and T42.

Need: raw body counts omit objects reserved for other work; one object can
also perform compatible jobs such as weighting a plate while jamming a gate.

Scope: extend the existing RO/CP/T6 foundation with source-grounded role
compatibility and explicit, phase-qualified reservations supplied by a proposed
arrangement. Report shared jobs, conflicting jobs and the remaining eligible
pool. Do not automatically invent mandatory destinations or reservations.
Sources: RO/CP specifications, T37 shared-demand budget, G8/G9/G12, current
holding/support/jammer semantics and T42 equipment roles.

Acceptance:
- Account for claustro's box reserved as a step while three other bodies
  weight plates, including jammers simultaneously serving their targets.
- Reject incompatible simultaneous held/stationary commitments; do not count
  a shared plate demand twice or forbid legitimate sharing of different roles.
- Keep reservations, view, phase and eligibility premises visible; release of
  a reservation in a later phase is distinct from simultaneous availability.
  A conditional shortage refutes only the supplied allocation, not the puzzle.

Result (2026-09-28): specification 13.8 written before code. CP accepts a
stage's `:reservations` (body, one role, purpose, `:from`/`:through` phase).
Roles :weight, :jam, :place, :hold, :mount, :support are read from ON,
JAMMING, HAS-LOCATION, HOLDING and MOUNTED-ON. New B5 (only with a
reservation): per-body compatibility C1-C7 with SHARED jobs, MC-survey jam
sightlines with single-closure gate premises, capacity K1-K4 (live and ghost
share a support), the eligible pool and release rows. B1 pins `:weight`
holders and matches with per-plate eligibility, so a jammer may weight a plate
it jams from; with reservations a shortage refutes only that allocation.
63 claustro and 28 phobia checks passed: the final phase matches three plates
once with both jammers doubling (gate1 from a plate needs gate2/gate3 open),
box2 reserved as the step; the pinned end arrangement PASS; C1, C2, C3, C5,
C7 and K1-K4 conflicts; a refuted sightline on a trimmed survey copy; box1
reserved elsewhere gives the conditional shortage; release and phase rows;
floor versus wall fan mounts. T24's 19 checks pass and its CP output is
byte-identical; no problem names, LABELS or FLET; no new compile warnings;
profiles unchanged (CP is not in them). Guide, claustro Handoff and G12
(status note, in part) updated. No search. Temporary files removed.
Evidence: `evidence/t44-role-reservations-2026-09-28.md`.

## T45 — Dependencies across recorder boundaries and support changes

Status COMPLETE, 2026-09-28. Approval granted by D, 2026-09-28; implemented
in its own session.
Origins: rumin-topo's `doc/problems/rumin-topo/rumin-topo solution (91 steps).lisp`;
windtunnel-topo and crelay-topo Handoffs under `doc/problems/`, with their
retained validated evidence. Rumin's older trace is historical evidence to
check against current semantics, not a newly verified run.
Depends on T28, T34, T43 and T44.

Need: static stability does not show what survives ghost removal, support
release or a change of support; a working beam may rely on a particular view.

Scope: for an explicit arrangement and specified boundary/support transition,
report affected supports, beams, devices and route conditions, prerequisites
for the event, and surviving versus lost obligations. Reuse recorder contracts,
T33/T34 view/stability checks and T43/T44 dependency/role inputs. Evaluate on
private state where supported; expose unresolved cases rather than inventing
settling rules. No change to recorder, support-loss or engine semantics.
Sources: current recorder/support technology, specifications 6.1/6.2 and
recorder contract, G15/G19, windtunnel review, and the rumin closure commentary.

Acceptance:
- Distinguish rumin's expendable ghost-dependent route from its persistent
  live plate support; check stop/cancel prerequisites separately from effects.
- Preserve windtunnel's live/ghost environmental distinction; a stable supplied
  arrangement does not imply survival of a later boundary or toggle.
- Cover a support-removal case and retained support chains using current
  engine policy; unsupported transitions report UNRESOLVED without mutation.
- Report event-relative consequences, not a reachability proof, complete-plan
  validation or newly established global necessity.

Result (2026-09-28): specification 6.3 written and saved before code. New
optional `tech/constraint-boundary.lisp` (BT): `report-boundary-transition`
takes a settled state and one event, `(:stop <ghost>)`, `(:cancel <live>)` or
`(:action <form>)` changing a support or the recorder session. Prerequisites
are itemized apart from effects and checked against the engine; STOP and
CANCEL are closed by the engine's own CLOSE-RECORDER-CYCLE-STATE! on a private
copy, ENGINE when applicable (agreeing with the successor), else HYPOTHETICAL.
Reports support chains (RETAINED, CHANGED, REMOVED), plates by occupant layer,
pairings and receivers in both views, devices and primitives, each agent's
route conditions in its own view, and obligations (fact, T44 role or reach;
EXPENDED, SURVIVES, LOST, MET, NOT MET). 246 checks passed. Rumin's historical
trace fails at action 7 only on old argument order of two connector actions;
normalized, it replays to the goal. At rumin's final STOP gate5's ghost-held
route is EXPENDED while gate6 on live tray1's plate4 SURVIVES; CANCEL there is
NOT MET, HYPOTHETICAL. A tray release lands live connector1 on box1; a
hypothetical cancel leaves it on the ground. Windtunnel's views differ before
closure; its T34-stable arrangement loses receiver1 when closed. Rumin and
crelay boundaries agree with engine and replay. T33 (61) and T34 (155) pass;
profiles unchanged; compiles without warnings. Guide, specifications, G19/G15
note and windtunnel/crelay Handoffs updated (rumin has no Handoff). No search.
Evidence: `evidence/t45-boundary-transitions-2026-09-28.md`.

## T39 — Consolidate the Problem-Solving Guide

Origin: claustro-topo, closed 2026-09-27 (`doc/problems/claustro-topo/Handoff.md`,
`Briefing.md`, `Stage-Plan.md`). D asked for the procedure to be reviewed and
consolidated before the next problem. Status COMPLETE; Approval granted by D, 2026-09-27.

What worked on claustro-topo: intake; the static profile; a plain summary
with anticipated difficulties; a subgoal dialogue (backward to a penultimate
state, then forward), each idea checked by a one-line static query, then
realized by one min-length search at D's maximum depth or by a hand sequence
validated with VALIDATE-ACTION-SEQUENCE; finally the whole path validated from
the start against the goal. Unused: the probe battery, the ledger, the
cycle-plan check, RO, FH, and stage-plan approval ahead of search.

Proposed scope (Guide only; no code):
1. Four phases: Intake; Static profile (with hand contracts for UNCOVERED
   mechanics); Subgoal dialogue (summary and difficulties first, then
   subgoals backward or forward, each checked, searched or hand-validated);
   Closure (full-path validation from the start, VALIDATE-ACTION-SEQUENCE or
   VALIDATE-SEARCH-CHECKPOINT).
2. Probe battery, ledger, cycle-plan check, RO and FH move to one short
   "Optional tools, on D's request" section; their commands kept.
3. Briefing template: Summary, Difficulties, Contracts, Hints, Subgoal log
   (subgoal | whose idea | check | result), Result. Stage-Plan.md folds into
   the subgoal log; Handoff unchanged.
4. A "Quick static checks" section with the checks claustro used (jam
   sightline from a placement under forced gate bits, REACHABLE between two
   locations) and the MOVE route formats (walk, ladder, jump transitions,
   stairs).
5. Move the "Build state" history and task-number references to this plan;
   update the stale tooling note (the Filesystem extension worked on
   2026-09-27; `certutil -hashfile <file> SHA256` for hashes).
6. Keep every standing decision of D's: threads 16, min-length at D's
   maximum depth, no states limit, plain language, place names only, each
   problem treated as new, one question at a time.

Acceptance: the Guide is at most half its current length; a new problem can
go from intake to closure using only it; every path it names exists; the
crelay-topo and windtunnel-topo Handoffs still resolve their references;
no code or generated file changes.

Result (2026-09-27): Guide reduced from 449 to 218 lines. The four-phase
procedure uses an agreed subgoal dialogue, a Briefing subgoal log, and either
full-path validator at closure. Optional commands, grades, rules, engine
facts and the unchanged Handoff template remain. Quick-check signatures and
MOVE routes were checked against source and existing evidence. The component
build history moved below. Current probe-call compatibility remains until T38.
All 19 resolved Guide path checks passed; existing crelay-topo and windtunnel-topo
Handoff references remain valid. Hash comparison found no change to 453 Lisp,
generated profile/ledger and Handoff files. No search, staging or replay ran;
no temporary files were created. Evidence: `evidence/t39-guide-consolidation-2026-09-27.md`.

## T35 — Fixed-beam, jammer and stairs contracts (G20)

Origin: claustro-topo Phase 0, `doc/problems/claustro-topo/Handoff.md`.
Status COMPLETE; Approval granted by D, 2026-09-27. The Briefing retains
historical hand contracts.

Need: MC has no contract for beam-direct, jammer or stairs, and S6/RC/NH H3
enumerate connector placements only, so a fixed coupled corridor is
invisible to the profile.

Proposed scope: registry contracts and instance rows for the three
technologies (per fixed beam: its recorded barrier crossings and authored
obstacles; per jammer target: placement locations with a sightline and the
`jam-disallowed>` exclusions); a fixed-beam row in RC/H3 naming the gates
and occupied locations that decide each receiver.

Acceptance: specification amended first; on claustro-topo MC reports zero
UNCOVERED, and the fixed-beam rows name gate1 and location2 for receiver1
without any problem name in code (C3); crelay-topo and windtunnel-topo
profiles otherwise unchanged.

Result (2026-09-27): specification 8.9 amended before code. MC adds all three
contracts and instance reporters. Shared fixed-corridor records feed MC/RC,
with recorded barriers, authored obstacles, chromas and staged physical
clearance; H3 adds qualified fixed receiver candidates. Jammer rows survey
ground/plates/staged boxes, name single-gate sightline dependencies and
preserve directional exclusions; stairs rows retain direction and clauses.
2,762 focused checks passed, including finite-height and occupancy engine
checks and state/static preservation. Diagnostic compilation has no warnings.
Full crelay and windtunnel profiles are byte-identical to their pre-change
captures. Claustro regenerated: zero UNCOVERED, gate1/location2 named for
receiver1. G20, Briefing and Handoff synchronized. No search or replay.
Temporary launcher, compiled file and cache removed. Evidence:
`evidence/t35-contracts-2026-09-27.md`.

## T36 — S4 support for alternative door families (G21)

Origin: claustro-topo Phase 0. Status COMPLETE; Approval granted by D, 2026-09-27.

Need: one row with alternative clauses (gate1 or gate2) disables S4 and NH
H2/H4 for every device.

Proposed scope: evaluate reachability with a device forbidden by testing
each clause of a row (a row is usable when some clause avoids the device),
which KEEPER-ROW-AVOIDS-P already does; extend the spine reduction and
reachability-mismatch check to multi-clause rows, or skip reduction and run
directional analysis on the unreduced quotient when reduction is unsupported.

Acceptance: claustro-topo S4 reports a spine check other than
:ALTERNATIVE-FAMILIES and a directional verdict for gate8/gate9; H2/H4 are
evaluated; T31's reachability checks still pass; windtunnel-topo unchanged.

Result (2026-09-27): specification 10.7 amended before code. Used the
approved unreduced-quotient option for multi-clause families; single-clause
reduction is unchanged. The existing clause-aware reachability and final
full-graph exclusion check now run for both. H2 excludes rows with an
alternative avoiding its device. Scope and fallback are explicit in S4.
11,490 checks passed: adapted T31 regressions, independent DNF closure
oracles, direction/empty-clause/shared-door cases, H2 and staged checks.
Windtunnel's full profile is byte-identical. Claustro retains 12 rows,
reports gate8/gate9 directions, and evaluates H2 (two hints) and H4 (none).
Diagnostic compilation succeeded without warnings. Regenerated claustro
profile, G21, Briefing and Handoff updated; no search or replay.
Temporary files removed. Evidence:
`evidence/t36-alternative-families-2026-09-27.md`.

## T37 — T6 budget: one demand per equivalent device set (G22)

Origin: claustro-topo Phase 0. Status COMPLETE; Approval granted by D, 2026-09-27.

Need: T6 declines when two devices share plates, even when they are an S1
equivalence pair (identical clause set and mode) with one demand between them.

Proposed scope: collapse S1 equivalence classes to one body-cost demand
before the disjointness test; keep declining for genuinely different
devices that share a plate.

Acceptance: claustro-topo T6 reports one demand of 3 for {gate8, gate9} and
NH H1 is evaluated; a fixture with two non-equivalent devices sharing a
plate still declines; crelay-topo T6 output unchanged.

Result (2026-09-27): specification 3.1 amended before code. S1 canonical
clause sets plus mode define classes; every member is retained, each class
costs once, and disjointness is checked between classes. T6 names shared
demands. Opposite modes, different clauses and different non-plate controls
still decline when their supports overlap. H1 uses the same grouped budget.
Forty focused checks passed, plus byte-identical crelay-topo T6 regression;
the profile diagnostic compiles without warnings. Claustro's regenerated
profile changes only T6: {gate8, gate9} costs 3; H1 is evaluated but validly
empty (four available pooled bodies). The no-shortage case no longer claims
to refute the fully-open assignment. G22, Briefing and Handoff updated.
No search or replay; temporary files removed. Evidence:
`evidence/t37-equivalent-demands-2026-09-27.md`.

## T38 — Remove the probe battery's states limit

Origin: claustro-topo Phase 1, 2026-09-27. D's standing rule: searches are
bounded by the depth cutoff only, never by a states limit. Status COMPLETE;
Approval granted by D, 2026-09-27. The earlier claustro-topo workaround is
retained only in historical evidence.

Need: RUN-PROBE-BATTERY takes a STATES-LIMIT (I8's stopgap, and I8 is
closed) and PROBE-DEEPEN stops a probe as STOPPED when it is exceeded.

Proposed scope: drop the parameter and the STOPPED label from
`tech/constraint-probe-battery.lisp`; amend Extractor-Specifications.md
§12.3-12.5 first; update the Guide's Optional tools probe command (T39 replaced the old
Phase 1 and Loading sections).

Acceptance: `(run-probe-battery <max depth> <pathname> [probes])` runs and
reports; old results files that record a states limit still report; no
remaining reference to a states limit in the Guide or §12.

Result (2026-09-27): specification amended before code. Removed the runner's
cap parameter, settings field and early-stop branch; new results use only
START, CHEAP, NOT FOUND and EXHAUSTED. Historical STOPPED rows remain readable
as LEGACY STOPPED with no bound claim; old settings keys are ignored.
The existing FIRST/deepening strategy is unchanged. Zero depth produces
START or NOT FOUND without searching, and invalid depth signals an error.
All 35 focused checks passed, including tiny finite-chain searches, independent
replay, huge-count simulation, normal/exception restoration and old-file reports.
The diagnostic compiles without warnings. Guide and specification section 12
contain no states-limit reference. Old evidence files were not rewritten.
Temporary cache/launcher removed. No substantial puzzle search ran.
Evidence: `evidence/t38-depth-only-2026-09-27.md`.

## T33 - Physical and recording-view sightlines

Origin: D's follow-up after T32; `doc/problems/windtunnel-topo/Handoff.md`
and `Static-Profile-Review.md`. Related gap G19. Depends on T32.
Status COMPLETE, 2026-09-27. Implementation and focused checks approved by D on that date.

Goal: distinguish a clear beam path in the physical view from one in the
recording view, without treating either as a stable or reachable solution.

Scope and approach:
- Specify explicit view/scenario inputs: reference state, recorder phase,
  live/ghost presence and assignments, placements/heights, pairings and
  gate conditions. Report missing inputs; do not silently manufacture ghosts
  or assume both views agree. Respect body availability and pairing limits.
- Reuse engine view-aware visibility and relay-presence queries for blockers,
  eligible relays and every required hop. A connector's motion view is not
  automatically the view in which the whole beam/receiver is evaluated;
  follow the engine's actual rules.
- Keep hypothetical gate conditions as labelled premises. Forced gate bits
  do not establish controller consistency. Do not combine independently
  evaluated hops unless their conditions share a consistent scenario.
- Extend RC/NH with separate physical/recording results and assumptions;
  retain geometric-candidate labels. Unsupported or missing analysis is
  UNRESOLVED, distinct from a blocked sightline. No stability simulation or
  search in T33; T34 handles settling.
- Write the interface, output and checks in Extractor-Specifications.md
  before code. Update existing callers to the documented interface.

Read first: Extractor-Specifications.md sections 6, 8.8 and 10;
`tech/constraint-profile.lisp` (S6, RC, NH and FH);
`tech/visibility.lisp`, `tech/beam-relay.lisp`,
`tech/-recording-shadow-policy.lisp`, `tech/-recorder-core.lisp`,
`tech/-recorder-gate-shadow.lisp`, `tech/-recorder-receiver-shadow.lisp`;
`evidence/t32-wall-blower-2026-09-27.md`.

Acceptance, written before implementation:
- Fixtures cover physical-open/recording-closed and the reverse; mapped live
  versus ghost blockers; absent versus present recorded connectors; and
  unavailable recording context. Each result names its view and premises.
- Hop results agree with direct engine queries in each view. Supplied whole
  chains agree with view-aware relay evaluation, including presence and
  pairing constraints. A scenario can pass one view and fail the other,
  with a named reason. Ordinary non-recorder/physical behavior stays correct.
- No visibility result is promoted to stable, reachable or replay-validated.
  Windtunnel is a regression example, not a special case in diagnostic code.
- Retain focused evidence under `evidence/`; update specifications, G19,
  the board and affected Handoffs. Regenerate affected outputs through their
  reporters. No puzzle solve or search.

Result: 61 focused checks passed; diagnostic compiled without warnings.
An explicit complete reference-state scenario now yields separate physical
and recording hop/chain verdicts, with gate premises, availability, pairing
limits, lighting and receiver checks. Missing context is UNRESOLVED; tested
failures name their reason. RC/NH accept the scenario without assigning
bodies to their abstract geometric candidates. Default profile regenerated.
Specification: Extractor-Specifications.md section 6.1. Evidence:
`evidence/t33-view-sightlines-2026-09-27.md`. G19 then awaited T34 stability (now complete below);
windtunnel remains solved. No search, solution replay or settling ran.
Temporary implementation files/cache removed at closeout.

## T34 - Stability of a supplied relay arrangement

Origin: the same T32 follow-up and windtunnel Handoff. Related gap G19.
Depends on T33. Status COMPLETE, 2026-09-27. Implementation and focused checks
approved by D, 2026-09-27, following T33 completion.

Goal: determine whether a supplied arrangement stays intact and delivers
its beam after immediate consequences. This is a bounded state simulation,
not purely static geometry or an action-sequence planner.

Scope and approach:
- Accept a supplied arrangement and reference state: live/ghost identities,
  locations, supports/holdings, pairings, recorder phase, primitive control
  states and the receiver/view to test. Specify which facts are premises,
  inherited or derived. Reject invalid combinations and report missing data.
- Validate structural/type/resource consistency on a private state copy;
  run the engine's existing consequence/settling machinery. Derive gate,
  fan and receiver states normally; do not force derived bits against their
  controllers. Preserve original state, static tables and caller data.
- Compare requested placements/supports/pairings and beam operation against
  the settled result. Distinguish stable-and-beam-working, settled-but-failed,
  invalid/inconsistent, and unresolved/not-established outcomes. Name what
  moved or changed. Respect existing propagation limits; hitting a limit
  does not prove every possible arrangement impossible.
- Use T33's view-specific checks. Stability applies only to the supplied
  scenario with no further action. It proves neither reachability from the
  start nor survival through later toggles, moves or recorder closure;
  those transitions require separately supplied checks.
- Do not enumerate arrangements, assign objects automatically, search for
  action sequences or change technology semantics. Specify the interface,
  verdicts and workflow before code; prefer an explicit arrangement-check
  call over hidden simulation in every static profile.

Read first: T33's final specification/evidence and the windtunnel Handoff;
`tech/constraint-state-probe.lisp`, `tech/constraint-profile.lisp` (FH),
`tech/-propagation.lisp`, `tech/-support-motion.lisp`,
`tech/-support-settling.lisp`, `tech/support-settling.md`,
`tech/wall-blower.lisp`, `tech/floor-blower.lisp`,
`tech/-recorder-blower-shadow.lisp`, `tech/-recorder-cycle-boundary.lisp`,
and `evidence/t32-wall-blower-checks-2026-09-27.lisp`.

Acceptance, written before implementation:
- Fixtures cover an intact working arrangement; a stationary but unlit one;
  live/ghost wall sweeps; support displacement/settling; controller feedback
  changing a required gate/fan; and invalid inputs.
- Check settling success and bounded failure/inconsistency, with reasons.
  A second settling pass leaves a successful result unchanged. Original
  state/static/caller data remain unchanged on success and failure,
  including exceptions.
- Windtunnel distinguishes the live-ground station conflict from a supplied
  successful mixed-view arrangement using actual settled beam evaluation.
  A known supplied arrangement is not an independently found solution or
  proof of reachability. No puzzle solve or search.
- Retain reproducible fixture/result evidence; update specifications,
  diagnostic usage guidance, G19, the board and affected Handoffs. Keep
  unsupported mechanisms and future-transition questions explicit.

Result: 155 focused assertions passed; new optional
`tech/constraint-arrangement.lisp` compiles without warnings. Structural
validation precedes bounded engine propagation; a second pass must be a
fixed point. T33 and actual receiver facts distinguish intact working,
settled-but-failed, invalid/inconsistent and unresolved outcomes. Full input,
static facts and schema tables are preserved, including error paths.
Windtunnel's mixed-view arrangement succeeds; the geometrically clear
role-swapped live-ground arrangement loses its live station under propagation.
Specification: section 6.2. Evidence:
`evidence/t34-arrangement-stability-2026-09-27.md`. G19 resolved for supplied
scenarios; future transitions and reachability remain outside scope. Existing
profile and replay evidence unchanged. No search or replay. Temporary files
removed at closeout; no waiting implementation task.

## T31 — Preserve quotient reachability and report unavailable hints

Origin: `doc/problems/windtunnel-topo/Handoff.md` and its
`Static-Profile-Review.md`; generated profile S3/S4/NH. Related gap G18.
Status COMPLETE, 2026-09-27. Approval granted by D, 2026-09-27, on the proposed scope.

Scope: replace independent removal of mutually redundant quotient rows
with a deterministic reduction that preserves the reachability checks S4
requires, including directed rows and device exclusions. Preserve explicit
unsupported-family handling. Carry S4 failure reasons into NH H2/H4 rather
than printing an unexplained absence of hints.

Acceptance: focused synthetic checks cover mutually redundant directed and
bidirectional rows; reduced and full graph reachability agree for every
region with no excluded device and with each device excluded; the
windtunnel profile no longer loses R1's outgoing reachability; forced
unavailability is labelled in NH. Update specifications before code.
No puzzle solve is needed or approved.

Result: 4,120 synthetic checks and 39 staged checks passed. The windtunnel
quotient retains five of twelve rows; R1 reaches outward and S4 verifies
full/reduced equivalence. H2/H4 distinguish unavailable from valid empty
analysis. S3 no longer describes the spine as a physical doorway count.
The related NIL-versus-empty-door-set composition defect is fixed.
Specification amendment: Extractor-Specifications.md �10.6.
Evidence: `evidence/t31-reachability-2026-09-27.md` (commands, results and
artifact index). The profile was regenerated, its prior version retained,
and the problem Handoff updated. No search or replay was run.

## T32 — Wall-blower coverage and stable relay-station qualifications

Origin: `doc/problems/windtunnel-topo/Handoff.md` and its
`Static-Profile-Review.md`; generated profile MC/RC/CC/NH. Related gap G19.
Status COMPLETE, 2026-09-27. Approval granted by D, 2026-09-27, on the proposed scope.

Scope: add a source-grounded wall-blower contract; distinguish horizontal
transport from vertical lift in coupling output; qualify RC/NH candidates
for forced transport and environmental view. Explicitly mark recording
sightlines and occupancy stability unresolved where not evaluated rather
than promoting geometric chains into full feasibility claims.

Acceptance: focused fixtures cover swept versus unswept heights, live
versus ghost fan states, pairing retention through transport, horizontal
versus vertical roles, and the windtunnel live-ground station conflict.
Geometric candidates remain distinct from stable/validated realizations;
existing floor-blower coverage remains correct. Update specifications
before code. No puzzle solve is needed or approved.

Result: all 57 focused checks passed, including real sweep updates on copied
states, both live/ghost activity splits, height boundaries, pairing retention,
removable fan presence, S1 override/polarity limits, and unchanged floor-blower
contract/instance/lift-barrier output. The regenerated windtunnel profile has
zero uncovered technologies; its live-ground conflict is labelled without
pruning its geometric chain or claiming recording-view feasibility.
Specification: Extractor-Specifications.md �8.8. Evidence:
`evidence/t32-wall-blower-2026-09-27.md`. G19's reporting gap is addressed;
general stable occupancy and recording sightline analysis remain unresolved,
explicitly marked. No solve or solution replay ran. Temporary files removed.

## T30 — Cleanup before a new problem

**Goal.** Leave the method's documents short and correct before a new
problem starts: the plan back under T18's size bound, every named path
real, and no stray files.

**Approval.** D approved it on 2026-09-26, as proposed, with delete
permission for item 3.

**Approach (A's technical choices).**
1. The whole plan as of 2026-09-26 (T18–T29 entries included) is copied byte
   for byte to `archive/Implementation-Plan-2026-09-26.md`, as T18 did; this
   file keeps the rules, board and T30's entry. The file map gains the
   probe battery row it lacked.
2. A script checks every path named in this plan, the guide, the
   Extractor Specifications index, the crelay-topo Handoff and the
   `tech/constraint-*.lisp` headers. A broken reference is fixed in the
   document, never by moving a file.
3. The stray `evidence/sedMSV4U3` is deleted.

Left alone, with reasons: the T3 recommender and T5 question generator,
which the T2 and T14 compatibility checks still call; the T26 evidence
scripts, kept as record (§14's note); crelay-topo's flat evidence folder
(T18's decision).

**Acceptance (written before work, approved by D).**
- A1 Archived text byte-identical to its source, SHA-256 before and after.
- A2 This plan at most 450 lines, stating in its first 10 lines what a
  session does.
- A3 Every path named in the checked files exists (by script).
- A4 A cold start from this file reaches the Current Task without opening
  `archive/`.
- A5 No code change; profile and ledgers untouched (hashes before and after).

**Result.** Plan 907 → about 180 lines; the archive copy hashes identical to
the old plan. The path check found 112 distinct paths; the only fix was
qualifying the crelay-topo Handoff's bare `Post-Mortem-2026.md`. The stray
file (a copy of the T28 checks file) was deleted. No code or generated file
changed. Evidence, parts 1–5.

**Status.** COMPLETE, 2026-09-26. **Evidence.** `evidence/t30-cleanup-2026-09-26.txt`.

## Component build history (moved from the Guide by T39)

**Build state (2026-09-27).** Every component the procedure uses is built.
I8, a memory estimate before search (T26), was closed by D on 2026-09-26:
the maximum search depth is D's own determination (Search settings above).
I5 coverage is built (T19, T28, T32): the profile's MC section. Its contracts
cover floor-blower, wall-blower, jump, ladder and recorder, and step has an extractors
entry; a technology outside the registry is UNCOVERED and still needs a hand
contract in the Briefing. I3 is built (T20): the profile's CC section,
which supplies the Briefing's Couplings rows. I6 is built (T21): the
profile's NH section, which supplies the Briefing's Necessity hints; hints
it cannot state (e.g. recorder cycle counts) are added by hand. T32 qualifies
RC/NH relay chains as physical geometric candidates; wall-stream conflicts
are view-specific. T33 adds optional supplied-state scenarios (specification
6.1): separate physical/recording sightlines and whole-chain lighting checks.
Without a scenario both evaluations remain UNRESOLVED. Stable occupancy is
checked explicitly by T34 for a supplied complete arrangement, using bounded
propagation and a second-pass fixed-point check; it is not inferred for
unassigned geometric candidates. I1 is
built (T22): `REPORT-FROM-HERE`, run on its own, not part of the profile.
I7 is built (T23): `tech/constraint-probe-battery.lisp`, loaded after the
profile and run on its own at `*threads*` 16 (see Phase 1 step 3).
I2 is built (T24): `REPORT-CYCLE-PLAN-CHECK`, run on its own with D's
stage plan as data (see Phase 2 step 6). The stage ledger is built (T27):
`MAKE-STAGE-LEDGER`, the stage operations and `LEDGER-FILE-APPLY` in
`tech/constraint-ledger.lisp` (Record Schema §6.5, §9.1, §12.1; see Phase 2
step 6 and Phase 3 step 9).

This is the pre-T39 build record. Its phase and step numbers refer to the former Guide; current usage is in the consolidated Guide. The former guide was written 2026-09-25 by T18 from Post-Mortem-2026.md sections 2 and 4. T25 supplied the domain interview; T27 supplied the optional stage ledger. T33/T34 supplied the explicit scenario checks (specifications 6.1/6.2).

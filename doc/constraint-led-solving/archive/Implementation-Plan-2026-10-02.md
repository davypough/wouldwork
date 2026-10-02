# Constraint-Led Method — Implementation Plan

**On reading this file, continue the method's development.** Take the Current
Task below. If its approval is granted, begin work; if not, present the task,
your intended approach and its acceptance criteria to D, and ask before
changing any file. To solve a problem instead, the entry point is
`doc/constraint-led-solving/Problem-Solving-Guide.md`, which leads to the
problem's `Handoff.md`.

Opened 2026-09-20; restructured by T18 (2026-09-25), T30 (2026-09-26) and
T46 (2026-09-28). This file holds the board, conventions, policy and current
task. It does not hold task history: completed entries are in `archive/`,
one paragraph per copy under Archives below (T0–T17 in
`archive/Implementation-Plan-2026-09-25.md`, T18–T29 in
`archive/Implementation-Plan-2026-09-26.md`, T30–T45 in
`archive/Implementation-Plan-2026-09-28.md`). Nothing about this work lives in
`CLAUDE.md` or `AGENTS.md`. The file map below describes this directory.

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

**None waiting.** T46 (cleanup after T31-T45) was completed 2026-09-28; its
entry is below. No task is PROPOSED or APPROVED. A session that finds this
pointer stops for D, who proposes the next task. No puzzle solve or probe
battery is authorized.

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

## Path migration — 2026-10-02

The current root is `doc/constraint-led-solving/`. Method documents are
at its top level; problem records are in `problems/`, and the triangle
pilot is in `constraint-pilot/`. Current document references and all
seven `Actions.lisp` validation-output paths were updated with D's approval.

Historical paths in `archive/`, `evidence/`, verbatim quotations and
dated schema-gap entries are preserved. For locating moved files, map
`doc/constraint-method/` to this root, `doc/problems/` to its `problems/`,
and `doc/constraint-pilot/` to its `constraint-pilot/`. Earlier removals
and the crelay-topo archive move still apply; this mapping does not
restore deleted files or make historical scripts runnable. The former
`doc/README.md` is absent; mentions in completed task records are historical.

## File map

| File | Authoritative for |
|---|---|
| `doc/constraint-led-solving/Constraint-Implementation-Plan.md` | method development: board, current task (this file) |
| `doc/constraint-led-solving/Problem-Solving-Guide.md` | solving a problem: procedure, rules, grades, templates |
| `doc/constraint-led-solving/Extractor-Specifications.md` | every static component's specification |
| `doc/constraint-led-solving/Schema-Gaps.txt` | schema gaps from all problems (G1–G17 from crelay-topo) |
| `doc/constraint-led-solving/Status-Algebra-and-Record-Schema.md` | the ledger's record schema and status algebra (T1, revised by T27) |
| `doc/constraint-led-solving/Post-Mortem-2026.md` | T17 findings; design basis for T18–T27 |
| `doc/constraint-led-solving/evidence/` | method-level check, run and load evidence |
| `doc/constraint-led-solving/archive/` | the plan before T18 (T0–T17 entries), before T30 (T18–T29 entries) and before T46 (T30–T45 entries, T40–T45 shared requirements, component build history); the G15 checklist; `crelay-topo-experiment/`: crelay-topo's FROZEN prediction register, its version-1 ledger and its T10 evidence, moved there 2026-09-30 |
| `tech/constraint-profile.lisp` | the static extractors (S0–S7, T6, RC, RO, MC incl. beam-crossing, beam-relay and floor-gears rows, SD); supplied-state views, crossings, relay lighting, equipment and service transitions (6.1, BX, RL, EQ, SW) |
| `tech/constraint-ledger.lisp` | ledger (stages, file of record), recommender, ingester, question generator (T2–T5, T27) |
| `tech/constraint-state-probe.lisp` | one-step applicable-action probe from a replayed prefix |
| `tech/constraint-arrangement.lisp` | explicit supplied-state stability check (T34); no search |
| `tech/constraint-boundary.lisp` | BT: one supplied recorder boundary or support change (T45); no search |
| `tech/constraint-probe-battery.lisp` | the probe battery (PB, P1–P5; T23, T29); the only diagnostic that searches |
| `src/ww-search-checkpoint.lisp` | standalone search checkpoints: export, import (by replay), validate |
| `Wouldwork User Manual(26.8).docx`, Goal Chaining / Manual continuation from saved checkpoints | user-level description of the checkpoint workflow |
| `doc/constraint-led-solving/problems/<p>/Handoff.md` | a problem's current state (the only per-problem state file) |
| `doc/constraint-led-solving/problems/<p>/Briefing.md` | the problem's analysis and subgoal log, with its evidence |
| `doc/constraint-led-solving/problems/<p>/Actions.lisp`, `Validation.txt`, `Checkpoint.txt` | accepted actions from the start; closure validation with its `validate-solution` form; the latest search checkpoint (Guide, Records and templates) |
| `doc/constraint-led-solving/problems/<p>/Constraint-Static-Profile.txt` | generated extractor output |

## Objective

The method begins with recorded intake: problem spec, optional corresponding
diagram or "none", and D's maximum search depth. Then come the static profile
(with hand contracts for uncovered mechanics), a subgoal dialogue beginning
with a summary and anticipated difficulties, and full-path validation from
the start by loading the problem's `Actions.lisp` (VALIDATE-ACTION-SEQUENCE
plus every registered solution validator).
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
| T46 | Cleanup after T31-T45 (plan archive, path check, transfer files) | COMPLETE | Granted by D, 2026-09-28; separate session | T45 |

## Archives

Each archive file is a byte-for-byte copy of this whole plan on its date;
task entries there are authoritative for the tasks' scope, acceptance and
results. Read one only for a task's history.

- **T0–T17** — `archive/Implementation-Plan-2026-09-25.md`, the plan before
  T18's restructure: coordination, the interactive phase (T1–T5), the first
  extractors (T6–T9, T16), crelay-topo closure (T10), T11–T15 and the T17
  post-mortem.
- **T18–T29** — `archive/Implementation-Plan-2026-09-26.md`, the plan before
  T30: the phased restructure (T18) and the post-mortem improvements I1–I8
  (T19–T29), including T26's closure.
- **T30–T45** — `archive/Implementation-Plan-2026-09-28.md`, the plan before
  T46 (SHA-256 f023e406…dc25d0c): T30 cleanup; windtunnel and claustro work
  (T31–T38); the Guide consolidation (T39); the T40–T45 shared delivery
  requirements and entries (beam crossings, competing colors, removable
  equipment, services, role reservations, boundary transitions); and the
  component build history moved from the Guide by T39.

## T46 — Cleanup after T31-T45

Status COMPLETE, 2026-09-28. Approval granted by D, 2026-09-28; done in its
own session. Model: T30 (entry below; `evidence/t30-cleanup-2026-09-26.txt`).

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
   and `doc/search/parallel/worker-read-snapshots.md`) as removed on
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

Result (2026-09-28): the whole plan (962 lines) was copied to
`archive/Implementation-Plan-2026-09-28.md`, SHA-256 f023e406… identical
before and after. This file is 962 → 246 lines, with an Archives
section of one paragraph per copy. The path check over the eight named
files found 115 distinct paths; the only unresolved names are templates,
this entry's own (now absent) subjects and its evidence file, so no
reference needed fixing and no file moved. `_to_delete/` was already empty:
both transfer archives were gone before the session, so nothing was deleted.
`artifacts/` was also gone (untracked, date unknown): its references in
`doc/README.md` (two, including the Conventions line) and
`doc/search/parallel/worker-read-snapshots.md` are marked "removed by D
by 2026-09-28". Hashes of 305 code, profile, ledger and Guide files are
identical before and after. No Lisp loaded, no search. Evidence:
`evidence/t46-cleanup-2026-09-28.txt`.

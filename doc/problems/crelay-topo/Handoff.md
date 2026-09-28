# crelay-topo — Handoff

Updated 2026-09-25 (T18). **Status: CLOSED.** crelay-topo was the prototype
for the constraint-led method. T10 is complete and the prediction register
is frozen. No further work on this problem is planned. Procedure and rules:
`doc/constraint-method/Problem-Solving-Guide.md`. Method tasks:
`doc/constraint-method/Constraint-Implementation-Plan.md`.

## Next step

None for this problem. If D reopens it, D says what for; start from the
Restore section below.

## Result

- The 87-action path (80 hand-derived from D's recorder-cycle design, plus a
  7-action search-found final leg) passed `VALIDATE-SEARCH-CHECKPOINT` from
  the initial state to the goal `(has-location agent1 location19)`:
  SUCCESS-P, GOAL-CHECKED-P and GOAL-SATISFIED-P all T.
- 22 searches were reported in all. The 21st (cutoff 12, from the
  80-action checkpoint) ran out of memory. The 22nd (cutoff 10) found the
  final leg.
- Assessment: the static part served as a checker, not a discoverer
  (`doc/constraint-method/Post-Mortem-2026.md`, section 3).

## Ledger state

`Constraint-Realization-Ledger.txt`, SHA-256 `7EFED062…B7A47418`. It has 22
premises (pr18–pr22 are user-asserted, from D's design), nine spine links and
three bounds. lk1, lk2, lk3, lk7 and lk8 are CLOSED; lk4 and lk9 are REALIZED;
lk5 and lk6 are OPEN (never searched as crossings).

To rebuild it, load these data-only scripts in `constraint-evidence/`, in
order. Do not re-ingest into an already updated ledger, because events append.

1. build-realization-ledger-2026-09-20.lisp
2. ingest-lk1-2026-09-20.lisp
3. ingest-runs-2026-09-20.lisp
4. ingest-lk4-bound-2026-09-20.lisp
5. ingest-lk3-validation-2026-09-20.lisp
6. ingest-lk7-validation-2026-09-20.lisp
7. ingest-lk8-validation-2026-09-20.lisp
8. ingest-lk9-found-2026-09-20.lisp
9. ingest-lk4-found-2026-09-20.lisp
10. ingest-lk5-bound-2026-09-21.lisp
11. ingest-lk5-cutoff10-bound-2026-09-21.lisp
12. ingest-qn10-answer-2026-09-24.lisp
13. ingest-t10-user-premises-2026-09-25.lisp

**Evidence-name errata.** The ledger and scripts are left unedited so the
rebuild stays byte-identical. They cite four names that were never created:
lk1-first-run → lk1-ingest-2026-09-20.txt; lk1-validation and lk2-validation
→ runs-ingest-2026-09-20.txt; lk4-bound-8 → lk4-bound-2026-09-20.txt.

## Checkpoints

All are in `constraint-evidence/`. SHA-256 values were rechecked on 2026-09-25.

| File | Phases/actions | SHA-256 | What it is |
|---|---|---|---|
| t10-final-checkpoint.txt | 2/87 | `ED143883…4D1EFD58` | the complete solution |
| t10-c3-location15-checkpoint.txt | 1/80 | `59F34FF7…23D58F93` | D's three cycles; the lit stack at location15 |
| t10-keeper-checkpoint.txt | 9/32 | `21953443…F82D578D` | spine experiment; keeper endpoint |
| t10-repeater-source-checkpoint.txt | 8/27 | `659F0EDD…CF2C25DB` | spine experiment; repeater powered |
| t10-location15-checkpoint.txt | 7/19 | `76B89383…F4259635` | spine seed |
| t10-b1-checkpoint.txt | 1/9 | `D004ADBD…3322DBA7` | B1 fresh-origin experiment |

The full hashes are in `doc/constraint-method/evidence/t18-restructure-2026-09-25.txt`.

## Restore

Replay only; no search:

```lisp
(load (merge-pathnames
        "doc/problems/crelay-topo/constraint-evidence/validate-t10-final-checkpoint-2026-09-25.lisp"
        (asdf:system-source-directory :wouldwork)))
```

For another archive: `(stage crelay-topo)`, then `(ww-set *threads* 16)`, then
`(import-search-checkpoint (merge-pathnames "doc/problems/crelay-topo/constraint-evidence/<file>" (asdf:system-source-directory :wouldwork)))`.

## Files

- **Current:** `Constraint-Static-Profile.txt` (generated, 1654 lines,
  SHA-256 `263947f3…`, regenerated 2026-09-28 by T42: only MC's step verdict line changed; earlier
  by T41: MC's beam-relay contract, which also picked up T33's four RC scenario lines absent from
  the stored 2026-09-26 file), `Constraint-Realization-Ledger.txt`,
  `constraint-evidence/` (107 files, flat, kept as is), and
  `Constraint-Prediction-Register.txt` (FROZEN; the record of the validation
  experiment).
- **The design and its decisions:** `constraint-evidence/b2-ghost-tray-loc5-check-2026-09-24.txt`.
- **Archived by T18 in `archive/`, byte-identical:**
  `Constraint-Continuation-Prompt.txt`, `Constraint-Restart-Checkpoint.txt`,
  `Constraint-Abstract-Model.txt`, `Constraint-Role-Obligations.txt` and
  `Constraint-Schema-Gaps.txt`. Gaps now live in
  `doc/constraint-method/Schema-Gaps.txt`, and the RO/G14 specifications in
  `doc/constraint-method/Extractor-Specifications.md`.
- **Belonging to earlier methods, not reviewed:** the Backward-\*,
  Forward-\*, Initial-Conditions and subgoal-solution files.

- **T43, 2026-09-28:** profile regenerated through its writer; only the new SD
  section (services and setup dependencies, physical view only) was added.
  SHA-256 038FC689E051B312FB796FFB8EA2E7316E8AB634162AB7F193A4B6A46DC56700.
  Evidence: `doc/constraint-method/evidence/t43-services-and-setup-2026-09-28.md`.

- **T45, 2026-09-28:** BT (`tech/constraint-boundary.lisp`, specification 6.3)
  checked the final path's two CANCEL-PLAYBACK boundaries (actions 11 and 31):
  itemized prerequisites, the engine's closure and the replay agree. Profile
  unchanged (regenerates byte-identical). No search. Evidence:
  `doc/constraint-method/evidence/t45-boundary-transitions-2026-09-28.md`.

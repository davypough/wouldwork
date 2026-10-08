# Compact undo records for backtracking

Status: first implementation completed, 2026-10-07. The user subsequently ran
`(test-bt)`: 13 problems, zero failures, validating this implementation.
Measurements and validation are recorded in `backtracking-performance-investigation.md`.

## Objective and boundary

Restore database entries directly instead of reconstructing old propositions and
replaying them through relation expansion. Preserve action order, goal handling,
heuristic ordering, and current immediate-inverse cycle detection. This is not a
proposal to replace the search algorithm or expand its cycle pruning.

The corrected N=11 CSP already runs faster with backtracking than DFS. Further
speedup is a hypothesis, not a requirement to justify more machinery. Added
recording costs must be measured against the saved undo work.

## Record actual writes

One authored literal can write several database entries. `add-proposition` and
`delete-proposition` expand symmetries; bijections maintain two indexes and can
remove displaced partners; complements write or remove additional entries.
Capturing only the authored literal misses these side effects.

Record the old contents at the common database mutation boundary:

- `fold-store`: before replacing an entry.
- `fold-remove`: before removing a present entry.

These are reached by `add-int-prop-key` and `del-int-prop-key`, including generated
direct-key writes and relation expansion through `add-prop`/`del-prop`.
The recorder must be inactive outside a backtracking capture scope; measure its
inactive-path cost on DFS as well as its active-path cost on backtracking.

The record contains `key`, `old-value`, `present-p`,
`previous` (the preceding record), and `secondary-db` (NIL for the working IDB,
the static table for a static write). This is a linked undo stack without a
second list cell around every record. Presence is explicit: a present NIL value
must remain distinguishable from a missing entry. Stored values are shared under
the engine's existing immutable-value contract in `copy-idb`.

For the first implementation, retain repeated writes as separate records. Do
not add a per-choice hash table to deduplicate keys. Recording every store,
including an idempotent one, is the simple exact-restoration baseline; skipping
proven no-op stores can be measured separately. Absent removals need no record.
Reuse the old-value/presence lookup already needed by the folding helpers where
possible, rather than doing a second lookup only for recording.

## Restore directly, preserving bookkeeping

Walk records newest first. For a previously present entry, restore its old value;
otherwise remove its key. Use the existing integer-key mutation helpers with
recording disabled. These avoid literal construction, fluent extraction,
relation lookup, integer-key conversion, and symmetry/bijection/complement
expansion, while still reaching hash folding and propagated-change handling.

Save the propagated-change flag at the application boundary and restore it after
rollback so undo does not report a fresh semantic propagation event. Preserve
active standard/split hash bookkeeping through the existing folding helpers;
restore the prior symmetry-touched flag where scoped. Invalidate state hash and
canonical-form caches once after the whole rollback, as current backtracking
already does. Keep the existing restoration of action/time/value/heuristic/
arguments and the choice stack.

Do not blindly restore using raw GETHASH/REMHASH and leave those mechanisms stale.
Do not re-expand a relation during rollback: its physical side effects are
already represented in the recorded entries.

## Records belong to one application

An undo stack is valid for the database contents immediately before the
application that created it. It is not a reusable inverse of the action.

1. Capture all writes while an effect runs in one `bt-undo-frame`. The implementation
   simplifies the proposal: multiple ASSERT results are restored together after
   generation, so per-ASSERT marks and an `update` slot are unnecessary.
2. Preserve the single-update fast path: if that effect is left applied, transfer
   its undo records to the choice without undoing and applying it again.
3. For multiple ASSERT results, unwind generation in reverse mutation order,
   then retain the current forward operations and successor ordering. Do not
   assume every ASSERT began at the same database contents.
4. Whenever a choice is applied again, capture a **new** undo stack for that
   application. This applies to sibling exploration and to actual exploration
   after heuristic scoring.
5. Consume/discard the application stack on rollback. Undo must not append records
   to an enclosing capture scope. Each application has its own frame, so child
   rollback cannot consume parent records.

This distinction matters because `ordered-choices-bt` scores successors on a
disposable copy. Records must not retain that copy's table pointer and later
restore it during real search. The frame retains its application target and is
consumed and removed from the choice after scoring. Reapplication creates a new
frame for the current working table.

Exception cleanup must cover partial effect application and registration, not
only recursion. Existing `explore-choice-bt` establishes UNWIND-PROTECT after
registration; its wrapper alone cannot unwind a write that fails earlier.
Use small capture/rollback helpers and a scoped cleanup form. Avoid LABELS/FLET
and a large macro that hides all search control flow.

## Keep the first change narrow

The explicit `choice.undo-frame` field holds restoration data. Literal lists
retain their existing format; `update` needs no new field.

- Keep forward literal operations for reapplication, tracing, replay, initial
  updates, and the existing inconsistency check.
- Keep inverse literal operations for planning-mode cycle signatures in this
  first step. Actual rollback uses the new records. This deliberately leaves
  some planning-mode allocation until cycle detection is separately reviewed.
- CSP has no inverse-cycle checking. Once consumers are audited, omit inverse
  literal construction in that mode; this removes its old-value reconstruction
  cost as well as literal-based rollback.
- Keep snapshot-style enumeration choices on their current copy-based apply/
  restore path. An incremental child beneath a snapshot captures the currently
  installed IDB normally.
- Keep diagnostic output readable: distinguish inverse literals used for cycle
  checks from the entry records used for restoration.

## Consumer and scope audit required during implementation

Read and adjust these consumers together:

| Area | Why it matters |
|---|---|
| `ww-translator.lisp`, `ww-installer.lisp` | ASSERT scopes and nested update functions currently bind/use forward-list and inverse-list. |
| `ww-support.lisp` | Mutation hooks, old-value lookup, key restoration, and suppression during rollback. |
| `ww-structures.lisp`, `ww-backtracker.lisp` | Generation records versus live application records; all rejection and unwind paths. |
| `ww-planner.lisp`, `ww-solution-validation.lisp` | Init/replay consume forward changes; do not require them to interpret undo entries. |
| `ww-action-trace.lisp`, backtracking narration | Preserve literal diagnostics and explain the new restoration data. |
| `ww-backward.lisp` | Two helpers bind forward-list/inverse-list around standalone propagation; they must not accidentally start search rollback scopes. |

Capture is for the current dynamic IDB, not arbitrary process state. The
translator's static-database route is also captured: non-integer stores/removals
now pass through the folding helpers, and sibling reapplication dispatches to the
same dynamic/static table as translation. Snapshot replacement remains explicitly
supported. No literal-inverse fallback is used for incremental rollback.

The consumer audit found that init and replay still consume forward changes;
backward helpers run outside capture and retain their inverse behavior; action
tracing retains readable literals. Direct IDB construction in the converter and
replay validation occurs outside live choice application. Exogenous happenings
are incompatible with backtracking already. No additional live engine mutation
route requiring a new problem restriction was found. Arbitrary user GETHASH writes
or destructive mutation inside stored values are not automatically captured;
the existing immutable-value/update API contract still applies.

## Focused validation

Extend the existing `test/search/backtracking-updates.lisp` coverage rather than
create another broad suite. Use exact parent/successor database comparisons and
saved metadata, not just the final solution count.

1. Ordinary facts and fluents: overwrite, insert, delete, present NIL, already
   present addition, missing deletion, and repeated writes to the same key.
2. Expanded relations: asymmetric starting contents for a symmetric relation;
   a bijection that displaces existing partners; complements whose starting
   contents cannot be inferred merely by negating the authored update.
3. Lifetimes: multiple ASSERT siblings with overlapping keys, incremental child
   under a snapshot, snapshot siblings, heuristic scoring then exploration,
   constraint rejection, and an error after at least one write.
4. Bookkeeping: empty choice stack, parent metadata, propagated-change flag,
   standard/split hash equivalence to recomputation, and invalidated caches.

Reuse heuristic, pruning, bounding, rejected-goal, and COUNT checks. Retain
`(test-bt)` as the user's REPL boundary. Planning-cycle behavior must remain
unchanged; specifically compare accepted paths/search work on bounded planning
cases, not only CSP results.

Then repeat the corrected N=11 CSP comparison with normal STAGE and WW-SET
overrides in an isolated test checkout. Compare elapsed time, allocation, and
equal effect/precondition/goal counts. Also check that inactive recording does
not materially slow DFS. No claim of success based solely on fewer function
calls or a passing load.

## Proposed approval sequence

1. Implement the mutation recorder and direct rollback, retaining planning
   inverse literals and snapshot handling; complete the caller audit above.
   Run focused regressions and the bounded before/after benchmark.
2. Stop for the user's `(test-bt)` result and review measured benefit.
3. Only with another approval consider removing planning inverse literals,
   changing signatures, deduplicating trail entries, or pooling records.

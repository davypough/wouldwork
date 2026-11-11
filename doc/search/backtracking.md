# Backtracking Search

Reference for wouldwork's backtracking engine: how it differs from depth-first search, how it
restores state, its two cycle-checking modes, and when to prefer it. Citations name files and
functions, not line numbers. Source-checked on 2026-11-10.

Select it at the REPL with `(ww-set *algorithm* backtracking)`. A problem file may also set it,
but that applies only on a fresh stage (see `../load-ordering/parameter-precedence.md`).
Backtracking is tree search only: `ww-set` refuses `graph`, and `init()` forces `tree`.

---

## One working state, undone in place

Depth-first search (`ww-searcher.lisp`) retains a separate state per node. Backtracking
(`ww-backtracker.lisp`) keeps one working state, `*backtrack-state*`, applies a choice to it,
recurses, and undoes the choice on return. Each live choice sits on `*choice-stack*`.

Effects come in two kinds:

- **Incremental.** The effect modifies the working state in place. Restoration uses undo records.
- **Snapshot.** The effect returns a complete database (enumeration producers such as
  `make-update-from` in `ww-enumerator-build.lisp`). The choice keeps the parent table as its
  inverse; the forward/undo helpers copy snapshots when installing them. Snapshot effects must
  leave the parent and shared static tables unchanged.

### Undo records

One authored literal can write several database entries: symmetric relations expand,
bijections maintain two indexes and displace partners, complements add or remove entries. So
undo records are captured at the common mutation boundary, not from the authored literal.

`fold-store` and `fold-remove` (`ww-support.lisp`) call `record-bt-undo` before changing an
entry. Each `bt-undo-entry` (`ww-structures.lisp`) holds `key`, `old-value`, `present-p`,
`previous`, and `secondary-db` (non-nil for a static-table write). Presence is explicit, so a
stored NIL is distinguishable from a missing entry. Repeated writes produce separate records;
absent removals produce none. Recording is inactive outside a `bt-undo-frame`.

`restore-bt-undo` walks a frame newest first, restoring old values or removing keys through the
same folding helpers with recording disabled. That keeps hash accumulators consistent without
re-expanding relations. The frame also restores the propagated-change and symmetry-touched
flags; state hash and canonical-form caches are invalidated once afterwards.

A frame belongs to one application of a choice, not to the action. Every reapplication
(sibling exploration, or real exploration after heuristic scoring) captures a new frame, and
rollback consumes it. Cleanup covers a failure partway through an effect, not just recursion.

Values stored in the database are shared under the existing immutable-value contract. Raw user
`gethash` writes, or destructive mutation inside stored values, are not captured.

### Hooks the engine honors

- **`heuristic?`**: `visit-choices-in-order-bt` scores all choices at a level on a disposable
  copy (`ordered-choices-bt`) and explores in stable ascending order. Without a heuristic,
  choices stream in generation order.
- **`prune-state?`**: checked at each `backtrack` level, including the start state.
- **`min-steps-remaining?`** and registered minimum-step bounds, `bounding-function?`, the time
  limit, and COUNT (see `counting.md`). Objective types such as `min-length` use the user bounds
  but get no automatic objective-bound pruning (`init()` prints a note).
- **Exogenous happenings are incompatible** with backtracking; `init()` signals an error.

---

## Cycle checking: `*bt-cycle-check*`

Neither mode keeps a closed table, so neither removes duplicates across separate branches.

### `nil` (default): immediate inverse checking

`detect-path-cycle` rejects a candidate whose forward operations undo the preceding choice. It
compares order-insensitive signatures (`update-set-signature`) before an exact check. It applies
to planning problems only (`cycle-check-enabled-bt`); CSP gets no cycle check and builds no inverse
literals. It skips choices with followups and snapshot transitions, and does not catch longer
cycles.

### `t`: full-path (PATH) checking

Rejects any candidate whose database equals an ancestor's, as DFS tree search does with
`on-current-path`, but without retaining ancestor states:

1. `*bt-path-fingerprints*` holds a hash and entry count (`bt-path-fingerprint`) for each
   ancestor, aligned with `*choice-stack*`.
2. Goal acceptance runs first, so an accepted goal is never pruned as a repeat.
3. On a fingerprint match, `bt-on-current-path-p` copies the candidate into scratch and walks
   back through the choices with `bt-scratch-parent`, reading undo records without consuming
   them (or copying a snapshot's saved parent). Only exact `equalp` equality rejects. A hash
   collision moves on to older matches.
4. A verified repeat increments `*repeated-states*` and returns through normal undo.

PATH builds no inverse literals or signatures. It allocates scratch space only on a fingerprint
match. Hash mismatches between `equalp`-equal but differently represented values can miss a
repeat, though never cause a false prune.

**Enabling PATH asserts that database equality means equal futures.** It may change solution
path counts. `validate-bt-path-mode` rejects known violations at solve setup:

- It requires `backtracking`, `planning` and `tree`, so CSP is excluded.
- It rejects active solution or prefix validators, goal-chain rejection, sessions and policy,
  `*final-goal*`, happenings, `*auto-wait*`, `*recorder-prefix-pruning*`, and recorder types or
  technology.
- Static-table writes during PATH search signal a named error.

Arbitrary user predicates cannot be audited, so time-, reward- or history-dependent models
remain the author's responsibility.

---

## Parallel backtracking

`ww-parallel-backtracker.lisp`: with `*threads*` > 0, a task is the list of choice ordinals from
the root to a node at the split depth. Task generation reruns the serial backtracker
(`collect-bt-tasks`), deepening until there are enough tasks. Each worker (`run-bt-task`) replays
its prefix from a private copy of the start state with an ordinary `(backtrack 0)`, so it
explores only the forced choice at each prefix level (`visit-backtracking-choices`). Because
replay starts at the root, both cycle-checking modes work unchanged in parallel. Worker read
snapshots are covered in `parallel/worker-read-snapshots.md`; scaling measurements are in
`parallel/backtracking-scaling.md`.

---

## Choosing tree DFS or backtracking

These are structural expectations, not benchmark conclusions. First confirm PATH eligibility
and that database-based cycle pruning is valid for the model. Only then weigh costs.

| Situation | Try first | Why |
|---|---|---|
| Large state, few physical writes per action | Backtracking | Small undo trails instead of retaining many large state copies |
| Small state | DFS (weak) | Copying is cheap; backtracking's bookkeeping has less to save |
| Many physical writes per action | DFS (conditional) | Long undo trails can cost more than copying |
| Deep paths, large states, memory pressure | Backtracking | Less retained data, though pending choices and trails still use space |
| Complete-database (snapshot) effects | DFS (weak) | Backtracking still pays snapshot copying, losing its main advantage |
| Frequent exact ancestor matches | DFS | DFS already holds ancestors; PATH reconstructs them in scratch |
| Monotonic progress (no cycles possible) | Neither | Ancestor checking gains nothing; compare state-management cost alone |
| Many branches converging on the same states | Neither | Both revisit across branches; consider graph DFS |
| Expensive domain reasoning | Neither alone | Domain work may dominate state-management differences |

Measure with elapsed time, allocation, and search work: effect calls, precondition calls,
accepted goals. Native state and cycle counters mean different things in the two engines, and
backtracking returns at the cutoff without DFS's truncation probe. Allocated bytes are cumulative,
not peak retained memory. Don't infer PATH results from immediate-mode or CSP measurements.

---

## Tests

- `(test-bt)` (`ww-problem-tests.lisp`): the standard problem suite under backtracking.
- `test/search/backtracking-updates.lisp`: `test-backtracking-updates` covers undo restoration,
  relation expansion, snapshots, error cleanup and PATH cycle-data omission;
  `test-backtracking-path` covers ancestor pruning, collisions and goal-first handling;
  `test-bt-path-parameters` covers parameter validation and persistence. It restages, so run it last.
- `test/search/backtracking-heuristic.lisp`, `backtracking-prune.lisp`,
  `backtracking-followups.lisp`, `count-solutions.lisp`: heuristic ordering, pruning and bounds,
  followups, COUNT.

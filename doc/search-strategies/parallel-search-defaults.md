# Parallel search: default worker-owned reads

2026-09-14: Approved integration and generalization are complete. Ordinary
staging and solving remain the user workflow.

## Ordinary use

Stage and solve normally. The worker-read optimization defaults to T after STAGE;
there is no per-problem enablement command and no problem-name whitelist.
The problem's ordinary worker-count setting still controls whether search is
serial or parallel. Serial searches use the specialized canonical read code
without copying worker views. Backtracking remains serial, as before.

```lisp
(asdf:load-system :wouldwork :force t)
(in-package :ww)
(stage corner-topo)
;; Set a worker count as usual if the problem does not supply one:
(ww-set *threads* 4)
(solve)
```

The retained serial selector optimization, worker-owned static facts/object-code
maps, and cold worker-private traversal dependency caches are integrated.
The current source also registers traversal segment caches as worker-private.
Worker memo tables are created unsynchronized because each belongs to one worker.
This supersedes the original integration's shared segment-cache policy; it does
not establish the performance effect of that later change.

For an explicit diagnostic comparison, WW-SET can disable WORKER-READ-SNAPSHOTS.
REFRESH retains the current setting; STAGE restores the enabled default. The flag
does not increase the configured worker count or change the search objective.

## Ownership follows the loaded technology

The experiment's Claustro/Corner name checks, specific objective/mode whitelist,
and blanket callback/happening exclusions are removed. The engine creates views
for parallel depth-first worker groups regardless of problem name, planning/CSP,
tree/graph or solution objective. Existing engine constraints still apply, such
as serial backtracking and serial automatic waiting; snapshots do not turn a
serial-only algorithm into a parallel one.

Each technology registers its own memo policy when its spliced code loads.
Registry contents reset on reload, so changing to an unrelated problem cannot
leave references to caches from the previous technology set.

| Owner | Worker-local data | Initialization |
| --- | --- | --- |
| -vertical | type and location-elevation caches | Empty tables with original hash settings |
| -mobility | canonical route keys | Empty table |
| -traversal | canonical families, dependency classifications, and segments | Empty worker-private unsynchronized tables |
| beam-crossing | lazily built crossing endpoint table | NIL; built inside each worker |
| topo-lower-bound | 23 lazy model/context values, including their built flags | NIL; structures and tables rebuilt inside each worker |

The traversal dependency cache holds integer keys and T/NIL classifications.
Starting cold avoids transporting coordinator cache contents across worker
lifetimes. Negative cached NIL entries preserve GETHASH's presence value.
Segment results and the fact values referenced by their keys remain
immutable; mobility copies segments when constructing routes.

The lower-bound technology needs more than a private top-level table: its lazy
operator lists, indexed-model structures, relevance records, lookup tables and
matching built flags must belong to the same worker. All 23 are registered as a
set of NIL-initialized specials. Goal/state-dependent work remains local to the
query calls. The temporary state-facts override and numeric tuning parameters
are not mistaken for memo storage.

## Static state and registration contract

Initialization and root-task generation finish before publication. The coordinator
then freezes registration/static writes, copies static-fact and object-code tables,
constructs worker memos, and starts the worker group. Donated tasks use the
receiving worker's view. No view is stored in a task or survives the group.

The existing copy/key policy, missing-code errors and static-write guards remain.
The coordinator verifies static facts, both code maps, the bijective canonical
map, object index and stage generation at teardown. Registered configuration
values are also compared. Every created worker is joined before releasing the
phase, including preparation/startup errors, cancellation and nonlocal exits.

Problem callbacks retain their ordinary engine contract: they must be safe to
call concurrently and must not modify static facts or register new apparatus
during worker execution. There is no callback-name whitelist. New technology
memo writers use the registration API below; search-wide policy state with
existing synchronization remains shared. This does not make arbitrary unsynchronized
user Lisp thread-safe.

Source audit covered all technology global cache definitions, their writers, and
problem-local definitions. Beam-coordinate object registration runs during
initialization; recorder ghosts come from declared types. Recorder boundary
dominance already locks its search-wide frontier/counter under SEARCH-LOCK, so
that frontier is not incorrectly replaced by independent worker histories.
The general relaxed-heuristic evaluators construct local working data; the lazy
technology models they consume are handled by the 23 memo bindings. Problem-local
crossword lookup tables are built during loading and read during search. The
legacy cost/upper scratch globals are used by coordinator bound initialization,
not introduced as worker-private mutable memo candidates.

## Adding a technology memo

For a lazy hash table:

```lisp
(defparameter *technology-cache* (make-hash-table :test #'equal))
(register-worker-read-memo '*technology-cache* :empty-table)
```

For a lazy model containing structures or several internal tables:

```lisp
(defparameter *technology-model* nil)
(register-worker-read-memo '*technology-model* :nil)
```

Declare configuration that must stay fixed during a worker group:

```lisp
(register-worker-read-configuration '*technology-parameters*)
```

These declarations belong to the defining technology, not to a list of problem
names in the engine. Duplicate memo registrations are errors. Memo table hash
test, size/rehash settings and synchronization flag are preserved. Registration
itself is rejected during the frozen phase. Traversal parameter registration
also registers the parameter value for configuration verification.

## Staging corrections discovered by the repository sweep

- Problem-file worker-count declarations no longer trigger a nested ASDF load.
  STAGE completes a second compilation pass when the declared count changes the
  serial/worker compilation mode. Explicit worker-count overrides and REFRESH
  retain their settings; QueensN-CSP was the regression case.
- Persisted goals now encode cons/hash-table data instead of writing unreadable
  hash-table object descriptions. The positional VALS layout and early startup
  fields remain unchanged; older ordinary goal entries still load. Tiles1b
  stages, changes worker count and refreshes with its hash-table goal intact.
- The fluent-type validator accepts atomic dotted tails in destructuring
  patterns. Tiles2a-Heuristic now stages with its existing coordinate bindings.

## Validation and retained evidence

New evidence is under artifacts/parallel-closeout-01. Runs used isolated SBCL
processes, unique instance files and fresh ASDF caches. The user's REPL was not
used. No TEST-THREADS invocation or expensive puzzle solve was run.

| Check | Result / evidence |
| --- | --- |
| All 192 repository problem files (76 problems, 116 tests) | Initial STAGE/snapshot-copy sweep passed 188. Targeted retests resolved QueensN-CSP and both tile cases. Buzzer1 stages with default snapshots enabled in its existing serial automatic-wait mode; its explicit-wait diagnostic also passes snapshot ownership and two-worker expansion. See result-sweep-01.txt, sweep-01-results.sexp and result-staging-03.txt. |
| 16 representative two-worker start expansions | Complete successor/state/HIDB/happening comparisons with standard and canonical hashes passed, including recorder, heuristics, callbacks, all four major Topo puzzles, mobility and blowers. Repeated on final source: result-behavior-final.txt. These are bounded expansions, not full solves. |
| Claustro and Corner complete bounded movement/successor comparisons | Installed memo policy passed at 1/2/4 workers, including paranoid cache verification. See result-lazy-and-full-01.txt. Its later lazy-cache return-value harness check failed; corrected cache-content checks passed in result-lazy-02.txt. |
| Cold crossing and lower-bound model construction | Two workers match coordinator results while leaving coordinator memo bindings unchanged: result-lazy-02.txt. |
| Copy ownership, write/registration guards, missing codes and lifecycle failures | Final focused suite passed, including preparation/startup/worker/coordinator failures, cancellation and nonlocal exit: result-focused-final.txt. |
| Hash-table dynamic fluents and recorder registration guards | Tiles1b two-worker expansion and goal round-trip passed using the ordinary dynamic-state copier; recorder registration is rejected during publication: result-ancillary-03.txt. |
| Serial selector and settings transitions | Generated forms, presence values, stale-mode rejection, expected failed-compilation rejection, serial behavior, REFRESH retention and STAGE reset passed: result-selector-final.txt. |
| Normal small solves | Serial Blocks3, two-worker Blocks3 and two-worker three-disk Hanoi solved and independently replayed; backtracking Blocks3 solved. Final-source repeat: result-small-solves-final.txt. Actual workers were forced with small root-task settings. |

This establishes repository-wide staging/static-payload coverage and focused
parallel behavior, not exhaustive solution verification for every problem.
Future technologies must honor the registration and immutability contract.
The static copier accepts finite cons trees, strings and atomic values; arbitrary
circular payloads, structures and general vectors are not silently shared.
Dynamic fluents keep their ordinary engine copy semantics.

All prior serial-01, selector-01, selector-timing-01 and traversal-01 evidence,
reference copies/worktrees and unrelated source changes remain preserved.

The earlier measured gains remain scoped to their reports. In particular, the
roughly 62-66% four-worker reduction in repeated traversal work is not a measured
whole-search gain from this generalized default implementation. No expensive
Claustro, Corner, Rumin or Crelay solve is included in this closeout validation.

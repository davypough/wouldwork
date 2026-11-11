# Parallel search: default worker-owned reads

Worker-owned read views for parallel depth-first and backtracking search. Source-checked
2026-11-10. Ordinary staging and solving remain the user workflow.

## Ordinary use

Stage and solve normally. The worker-read optimization defaults to T after STAGE;
there is no per-problem enablement command and no problem-name whitelist.
The problem's ordinary worker-count setting still controls whether search is
serial or parallel. Serial searches use the specialized canonical read code
without copying worker views. Parallel backtracking, including its PATH
cycle-check mode, uses the same worker views.

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
Traversal segment caches are also worker-private. Worker memo tables are created
unsynchronized because each belongs to one worker.

For an explicit diagnostic comparison, WW-SET can disable WORKER-READ-SNAPSHOTS.
REFRESH retains the current setting; STAGE restores the enabled default. The flag
does not increase the configured worker count or change the search objective.

## Ownership follows the loaded technology

There are no problem-name checks, objective/mode whitelists, or blanket
callback/happening exclusions. The engine creates views
for parallel depth-first and backtracking worker groups regardless of problem name, planning/CSP,
tree/graph or solution objective. Existing engine constraints still apply, such
as serial automatic waiting; snapshots do not turn a
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

The static copier accepts finite cons trees, strings and atomic values; arbitrary
circular payloads, structures and general vectors are not silently shared.
Dynamic fluents keep their ordinary engine copy semantics.

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

## Search-time object registration

Problems whose search actions or helpers call `register-dynamic-object` declare
`(ww-set *search-registers-dynamic-objects* (beam))` in their specification, listing
all object types registered during search. The error identifies these types. The
declaration defaults to NIL on STAGE and survives REFRESH. Search entry (`ww-solve`, via
`validate-worker-read-snapshot-mode`) rejects it when worker snapshots are active for parallel
depth-first or backtracking search, before root-task generation. Use
`(ww-set *threads* 0)` for these problems. Disabling snapshots is not a validated
parallel workaround.

Initialization-only registration (such as the coordinate-derived crossing pool) occurs
before snapshot publication and does not require this declaration. The runtime write
guard remains in place to catch undeclared registration. The integer allocation lock
does not make new objects visible in already-published worker copies.

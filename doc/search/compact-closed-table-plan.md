# Compact Closed Table — Plan

> **Status:** proposed 2026-10-03, not started.  Implement one phase per session; after a
> phase's edits are tested, mark it **closed** here so it can be committed.

## Why

Graph search keeps one closed-table entry per distinct state.  Measured on
`problem-triangle-xyz` at N = 6 (291,694 states, parallel, `every`): the closed table held
**672 bytes per state**.  At N = 7 the 40.6 million reachable states need about 27 GB and
overran the 25 GB SBCL heap, although a board is only 28 occupied-or-empty positions.

The cost is the entry, not the board.  `make-closed-entry` (`ww-searcher.lisp`) stores:

```
(idb depth time value canonical-form symmetry-idb [node])
```

and `idb` is the state's whole proposition hash table, retained after the search backtracks
past the state.  An SBCL hash table holding ~20 entries costs several hundred bytes on its
own.  In canonical symmetry mode `symmetry-idb` is a second hash table per entry.

The stored idb has one use: the exact identity check in `closed-bucket-find` when a state's
`idb-hash` bucket is non-empty (`equalp` on the two hash tables, or `fixed-idb-equal-p` on the
fixed parts in canonical mode).  A packed, immutable copy of the propositions answers that
check equally well.

## Goal

Replace the stored hash table(s) with a compact exact representation, keeping identity checks
exact.  Expected: roughly 3–4x less memory per entry for small states (about 170–250 bytes
for the triangle), about 2x for large states.  No change to search results.

**Not a goal:** storing only a hash fingerprint.  Smaller still, but a collision would
silently drop a state and invalidate exhaustion proofs ("no solution within the cutoff").

## Code involved (all `src/ww-searcher.lisp` unless noted)

| Function | Role | Reads from the entry |
|---|---|---|
| `make-closed-entry` | builds entries (serial line ~995, start state ~432; `ww-parallel.lisp` ~146, ~417, ~432) | — |
| `closed-bucket-find` | identity check within a bucket | 1st (idb), 5th (canonical form), 6th (symmetry idb) |
| `fixed-idb-equal-p` | canonical-mode fixed-part comparison | idb and symmetry slice |
| `better-than-closed` | path-quality comparison | 2nd–4th (depth, time, value) |
| `get-closed-node` | hybrid mode | 7th (node) |
| `closed-bucket-insert` / `-remove`, `closed-key` | bucket handling | entry identity only |
| stats (~1914), `closed-shards-distribution` (`ww-parallel-infrastructure.lisp`) | counting | bucket lengths only |

Check again for other readers before Phase 1 (`grep -n "closed" src/*.lisp`).

## Phases

### Phase 0 — Baseline measurement
- Add a small REPL helper (e.g. `closed-table-bytes`) that, after a graph search, reports
  the closed table's retained bytes and bytes per entry (full GC, `sb-kernel:dynamic-usage`,
  drop `*closed*` / `*closed-shards*`, GC, measure again).
- Record baselines (bytes/entry, program cycles, elapsed time; serial and 16 threads) for:
  `triangle-xyz` (N = 6), `knap19`, a blocks problem, one Talos problem with many facts, and
  one problem with `*symmetry-pruning*` t in graph mode.
- Exit: table of baselines in this file.

### Phase 1 — Packed idb in closed entries
- Define the packed form, e.g. a sorted simple-vector of fact keys whose value is `t`, plus a
  sorted vector of `(key . value)` for fluent facts (values compared with `equalp`, matching
  today's hash-table `equalp`).  Prefer a specialized integer vector for keys if all keys are
  fixnums.
- `make-closed-entry` stores the packed form instead of the idb.
- `closed-bucket-find` compares the packed closed form against the successor's **live idb**
  (count check, then lookups into the live hash table), so a successor is packed only when it
  is inserted, not on every lookup.
- Exit: identical program cycles, repeated-state counts and solutions to Phase 0 on every
  baseline problem, serial and parallel; bytes/entry measured.

### Phase 2 — Canonical symmetry mode
- Store the packed **fixed part** (entries outside the symmetry slice) plus the canonical form;
  drop the per-entry `symmetry-idb` hash table.
- Adapt `fixed-idb-equal-p` (or add a packed variant) for the closed side.
- Exit: same canonical duplicate counts and results as Phase 0 on the symmetry baseline.

### Phase 3 — Entry structure
- Replace the 6/7-element entry list with a struct (or a short simple-vector) and update the
  readers in the table above.
- Exit: results unchanged; bytes/entry measured.

### Phase 4 — Hybrid mode
- Hybrid mode stores the search node, which keeps the full state alive; the packing gains
  nothing there.  Assess whether the node can be replaced by what hybrid mode actually needs.
  Close with "no change" if it cannot.

### Phase 5 — Validation and docs
- Full test suite; every `probs/` graph-search problem that solved before still gives the same
  solutions, serial and 16 threads.
- `triangle-xyz` at N = 7: should now fit the heap and prove no single-peg finish from a
  corner hole (40,600,768 states, per an independent count).
- Update `doc/problem-analysis/search-advisor/search-advisor.md` section 6.3 with the new
  bytes per state.

# Optional rotation/reflection-class counting

The current version also prunes right-half first placements in class mode using
`prune-state?`; see [queens-reflection.md](queens-reflection.md). The leaf-only
measurements below describe the preceding version.

`probs/problem-queensN-csp-1.lisp` is the working copy of the original queens
spec. It defaults to N=13 and COUNT. The original spec is unchanged. The row
order and placement constraints remain the same; the current copy uses three
occupied-set bit masks instead of repeated remaining-column lists.

The subsequent N=14/15 measurements and overnight recommendation are in
[queens-scaling.md](queens-scaling.md). The tables below retain the earlier
N=13 implementation comparisons.

`*queens-count-classes*` selects the result:

- `t`: count one representative per rotation/reflection class.
- `nil`: count every board.

The switch defaults to `t` when the file loads. Set it after staging or changing
threads, and never change it while a search is running. COUNT now retains one
example in `*count-example*` and prints its path and final state. With checking
enabled it is a canonical representative. The final implementation may omit symmetry checking if
larger-board measurements show an unacceptable cost.

## Why the check counts exactly one per class

A completed board is a zero-based vector of column positions in row order.
Reversing its rows and complementing its columns give three other square
symmetries. Transposing the board gives its inverse permutation; applying the
same reversals and complements to that inverse gives the remaining four images.
The original board is accepted if it is lexicographically no greater than any of
the seven other images. Equality is allowed: a board invariant under some
transformations still contributes exactly one representative. All working vectors
are local to the call, with no shared cache or writes to the search state.

The check runs only at complete boards. It does not reduce the partial-board
search. Noncanonical complete boards fail the goal and are expanded as dead ends;
this explains the additional program cycles with checking enabled. Counting fewer
accepted goals also avoids some candidate-path construction and counter updates.

## Validation and measurements, 2026-10-06

The independent list-based enumerator in `test/search/queens-symmetry.lisp` generated all
1,225 valid boards for N=1 through N=10. Independently constructed coordinate
rotations and reflections verified exactly one accepted image per class. Total
and class counts matched [OEIS A000170](https://oeis.org/A000170) and
[OEIS A002562](https://oeis.org/A002562), including the small classes at N=1,
N=4, and N=5. This checks the canonicalization helpers; these small sizes were
not staged separately in Wouldwork.

The delivered N=13 spec staged successfully and ran in Wouldwork with SBCL 2.6.9,
a 4 GiB heap, and 16 threads. Three runs per mode alternated in the order
off/on, on/off, off/on, with a full GC before each run. Each search had a
five-minute timeout; none approached it. Every run left both solution-record
lists empty, and all counts matched the reference sequences.

| Mode | Count | Times (seconds) | Median | Program cycles |
|---|---:|---|---:|---:|
| All boards | 73,712 | 2.143, 2.190, 2.147 | 2.147 | 4,601,032 |
| Rotation/reflection classes | 9,233 | 2.134, 2.134, 2.158 | 2.134 | 4,665,511 |

Allocation was approximately 19.060 GB per run without the check and 19.007 GB
with it. These are cumulative allocations, not peak retained memory. The timing
difference is within the observed run-to-run variation. There is no evident
penalty at N=13, but larger-N overhead and the overnight limit remain unmeasured.
These measurements preceded the addition of one retained example to COUNT.

## User REPL comparison

```lisp
(stage queensN-csp-1)
(ww-set *threads* 16)
(setf *queens-count-classes* t)
(solve)
(list *solution-count* (length *solution-paths*)
      (length *unique-solution-states*))
;; Expected: (9233 0 0)

(setf *queens-count-classes* nil)
(solve)
(list *solution-count* (length *solution-paths*)
      (length *unique-solution-states*))
;; Expected: (73712 0 0)
```

To rerun the independent checks after staging the copy:

```lisp
(load "test/search/queens-symmetry.lisp")
(test-queens-symmetry)
```

## Occupied-set representation, 2026-10-06

The approved next change replaced the per-row remaining lists and scans of earlier
queens with `(occupied> columns sum-diagonals difference-diagonals)`. The directed
relation keeps these three integer masks from being treated as symmetric. For
one-based row r and column c, their bit positions are c-1,
r+c-2, and r-c+N-1, respectively. A move is legal exactly when all three bits are
clear; placing its queen sets them. Row assignments remain for symmetry and the
printed example. The initialization action and obsolete list/conflict helpers
were removed from the copy.

Equivalence follows by induction: the empty board has three zero masks; each move
adds exactly the occupied column and two diagonals for its queen. Two squares
share a diagonal exactly when their row sums or row differences agree. Fixed row
order prevents row conflicts. Thus every prefix admits the same next placements
as the original constraints. This changes storage and checking cost, not the
search tree. Integer masks are not restricted to a fixed machine-word width.

The independent attack test in `test-queens-symmetry.lisp` matched actual engine
successors at all 1,176 N=13 prefixes through three placements (including their
fourth-row choices). The N=1..10 canonicalization tests passed again. Six complete
N=13 searches matched both counts and their earlier program-cycle totals, and
their retained examples independently passed column/diagonal checks. Examples
from class-counting runs also passed canonicalization.

A fresh before/after comparison included the one-example feature on both sides:
SBCL 2.6.9, 4 GiB heap, 16 threads, three runs per mode, full GC before each search,
and a five-minute per-search timeout. No search approached the limit.

| Mode | Before times (s) | After times (s) | Before median | After median |
|---|---|---|---:|---:|
| All boards | 2.163, 2.212, 2.148 | 2.017, 1.994, 1.995 | 2.163 | 1.995 |
| Classes | 2.168, 2.151, 2.157 | 2.015, 1.982, 1.967 | 2.157 | 1.982 |

Median cumulative allocation increased from 19.060 to 20.984 GB for all boards,
and from 19.007 to 20.931 GB for classes: about 10% more. The logical state is
smaller, but this is not an allocation reduction in Wouldwork's execution path.
Program cycles remained 4,601,032 and 4,665,511, respectively. The current version
was about 8% faster here; peak retained memory and larger-N costs are unmeasured.
The optional symmetry check still has no evident penalty at N=13.

An initial draft that materialized three temporary bit values per candidate was
also checked and then simplified to direct LOGBITP tests; the table reports only
the delivered version. The original spec remains available for comparison.

Use the REPL comparison above to validate the delivered copy. Further changes and
larger-N timing comparisons remain separate approval steps.

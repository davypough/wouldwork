# Early reflection pruning and solution density

## Built-in pruning hook

The current `problem-queensN-1.lisp` uses Wouldwork's `prune-state?` hook:

```lisp
(define-query prune-state? ()
  (and *queens-count-classes*
       (next-row 2)
       (bind (assigned 1 $first-column))
       (> $first-column (ceiling *N* 2))))
```

`expand` invokes the hook before generating successors. Both serial depth-first
search and parallel root-task/worker expansion use that entry point. The first
queen's state is still generated; its entire descendant branch is rejected.
Backtracking also invokes this hook before generating choices, so it receives
the same early-pruning benefit. The completed-board canonical check remains necessary.

Soundness: reflecting a board left-to-right changes its first column c to N+1-c.
If c is in the right half, that reflected board is lexicographically smaller.
The canonical representative therefore cannot start there. The middle column of
an odd board is retained. All other symmetries and middle-column ties remain
handled by the completed-board check. With `*queens-count-classes*` NIL the hook
returns false, preserving all-board counting. No engine code was changed.

The first-placement hook and independent legal-successor checks passed at N=13,
14, and 15. The spec selects 16 threads on staging. The original queens spec
remains unchanged, and the working copy is restored to N=13 after measurements.

## Paired measurements, 2026-10-06

SBCL 2.6.9, 4 GiB heap, 16 threads, one run per condition, full GC before each
search, and a five-minute timeout per search. Class checking remained enabled in
both conditions. The unpruned baseline temporarily unbound `prune-state?` in the
test process and restored it afterwards; the leaf canonical test was unchanged.
The hook-enabled run came first at each size. All searches completed normally.

| N | Class count | Unpruned time | Pruned time | Unpruned cycles | Pruned cycles |
|---:|---:|---:|---:|---:|---:|
| 13 | 9,233 | 2.044 s | 1.093 s | 4,665,511 | 2,522,417 |
| 14 | 45,752 | 11.969 s | 6.136 s | 27,312,630 | 13,633,439 |
| 15 | 285,053 | 76.160 s | 41.062 s | 170,843,821 | 91,598,540 |

Pruning reduced elapsed time by 46–49%. Cumulative allocation fell from 20.930
to 11.347 GB at N=13, 125.054 to 62.558 GB at N=14, and 797.884 to 428.733 GB at
N=15. These are cumulative allocations, not peak memory. The center-column
branch remains at odd N, explaining why the reduction there is less than half.

With the hook installed but class mode switched off, N=13 still counted 73,712
boards in 2.033 seconds. Every measured example passed independent column and
diagonal checks; class examples also passed canonicalization. Independent legal
successor checks covered 1,176, 1,535, and 1,962 prefixes at N=13, 14, and 15,
respectively, with pruning disabled by the user switch for those checks.

**Keep this change.** It preserves both counting meanings, uses an existing engine
hook, and approximately halves class-counting work. An illustrative extrapolation
from 41.062 seconds at N=15, using 6.69–8-fold growth per size step, puts N=18 at
3.4–5.8 hours and N=19 at 23–47 hours. These are scenarios, not measured results
or bounds; odd/even center-column effects complicate extrapolation. N=18 remains
the recommended overnight target, with a better margin than before.

User validation at the restored N=13 default:

```lisp
(stage queensN-1) ; selects 16 threads
(solve)              ; expected 9233 classes
```

Setting `*queens-count-classes*` to NIL before another `(solve)` must still give
73,712. Further optimizations remain separate approval steps.

The subsequent [legal-column domain experiment](queens-domains.md) found no
speed benefit at N=13 or N=15 and was reverted. The subsequent
[forward-checking experiment](queens-forward.md) retained reflection pruning
and added empty-future-row pruning, improving the current implementation further.

## What solution density means

Let Q(N) be the number of complete boards, counting rotations/reflections
separately. Among the N! permutations that place one queen in every row and
column, the fraction also avoiding diagonals is Q(N)/N!.

| N | Q(N) | Fraction of permutations | Approximately one valid board in |
|---:|---:|---:|---:|
| 8 | 92 | 0.228175% | 438 |
| 10 | 724 | 0.0199515% | 5,012 |
| 13 | 73,712 | 0.00118374% | 84,478 |
| 15 | 2,279,184 | 0.000174293% | 573,747 |
| 18 | 666,090,624 | 0.0000104038% | 9,611,866 |
| 20 | 39,029,188,884 | 0.00000160422% | 62,335,449 |

These are arithmetic ratios calculated from the published counts in
[OEIS A000170](https://oeis.org/A000170), checked 2026-10-06. N=18 and N=20 counts
are reference values, not Wouldwork runs. There are many more solutions as N
grows, but they occupy a smaller fraction of this candidate space. Among all N^N
row assignments allowing repeated columns, the fraction is smaller still.

Wouldwork does not enumerate all N! complete permutations. It rejects diagonal
conflicts at partial boards. Before the early reflection change, the measured
ratio Q(N)/(total states processed) was about 1.58% at N=13, 1.34% at N=14, and
1.33% at N=15. This denominator includes partial boards and dead ends: it is not
the fraction of complete permutations, nor the probability of quickly finding
one solution. No large-N extrapolation of this visited-state ratio is justified
from those three sizes.

For exhaustive counting, even many solutions do not let the search stop early.
Pruning equivalent or impossible branches helps by reducing the tree that must
be exhausted. Ordering candidates alone cannot remove that counting work.

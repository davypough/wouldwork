# Queens forward-checking experiment

## Scope and correctness

Forward checking rejects a partial board when any unassigned row has no legal
column under the queens already placed. Adding queens can only remove legal
columns, so such a board cannot extend to a solution. A nonempty domain in every
row is necessary but not sufficient: this check does not enforce arc consistency
or detect all conflicts between future assignments.

The experiment uses the existing `prune-state?` expansion hook, after the cheap
first-row reflection test. Forward checking applies in both all-board and class
counting modes. As with reflection pruning, the backtracking algorithm does not
use this hook; the tested execution path is tree depth-first search with 16
threads. The goal test and optional canonical representative selection are
unchanged.

The helper computes unused columns once. For each future row it removes one
diagonal family's attacked columns with a shift and mask, then checks the other
diagonal family while scanning remaining bits until it finds a legal column.
It stops at the first empty domain and allocates no domain lists.

Tests compare the actual compiled hook against an independent coordinate-based
test. They visit prefixes through the action generator directly, including
prefixes that the pruning hook would prevent a normal search from reaching.
Action successor tests likewise use `generate-children`, separating action
legality from intentional dead-state pruning.

## Measurements, 2026-10-06

SBCL 2.6.9, 4 GiB heap, 16 threads, full GC before each solve, and a 300-second
timeout per solve. Baselines preceded candidate runs. N=13 times are medians
of three runs per condition. Both versions counted 9,233 classes and 73,712
boards; retained examples passed column, diagonal, and applicable canonical
checks. The forward-checking hook matched the independent test on 38,680
prefixes through five assignments. Action encoding and reflection checks passed.

| N=13 mode | Baseline time | Forward checking | Baseline cycles | Forward cycles |
|---|---:|---:|---:|---:|
| Classes | 1.087 s | 0.799 s | 2,522,417 | 1,860,195 |
| All boards | 1.994 s | 1.503 s | 4,601,032 | 3,382,998 |

Time fell about 27% and 25%, respectively. Cumulative allocation fell from
11.347 to 8.319 GB for classes and from 20.984 to 15.417 GB for all boards.
Allocation figures are totals over each search, not peak memory.

At N=15, one run per condition counted 285,053 classes. Baseline time was
40.645 seconds versus 29.002 with forward checking (29% faster). Search cycles
fell from 91,598,540 to 64,880,760; allocation fell from 428,740,372,768 to
301,602,280,960 bytes (30% less). The hook comparison passed on 105,370 prefixes;
1,962-prefix action encoding, reflection, and retained-example checks passed.
No N=15 all-board timing was taken in this experiment.

**Retain forward checking.** The spec is restored to N=13 with 16 threads and
class counting enabled by default. No engine changes were made. User validation:

```lisp
(stage queensN-1)
(solve)                          ; expected 9233
(setf *queens-count-classes* nil)
(solve)                          ; expected 73712
(setf *queens-count-classes* t)
```

These improvements do not establish a new overnight maximum N. A subsequent
approved N=16 class-counting run, capped at five minutes, would give a more
useful scaling measurement than extrapolating another three sizes from N=15.

## Possible future engine support

Action-by-depth can supply a fixed row order if the spec generates one action
per row. This could remove `next-row`, but requires action-generation machinery
and a separate completion test; the current single action is simpler to read.
Moreover, action-by-depth falls back to the full action list after its last
indexed depth. It is not by itself a completion or no-reassignment guarantee.

Reusable stronger CSP support would benefit from explicit declarations of
variables, finite domains, and constraint scopes/predicates. Arbitrary Lisp
action preconditions do not provide enough structure to infer these reliably.
Such an optional interface could support forward checking, minimum-remaining-
values selection, and then suitable arc-consistency algorithms. Domain updates
would need correct branch-local storage or undo and parallel-worker isolation.
It would also need to preserve counting semantics and complete assignments.
Whether this outperforms specialized masks should be measured across several
specs before committing to an engine design. No engine changes are part of this
experiment.

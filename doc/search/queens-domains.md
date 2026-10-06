# Legal-column domain experiment

## CSP interpretation

The queens working spec models a constraint satisfaction problem: each row is a
variable, its column is the value, and column/diagonal conflicts are constraints.
It uses fixed-variable-order depth-first assignment with immediate conflict
checks. This is a standard basic CSP strategy implemented through Wouldwork's
generic state/action machinery. It does not currently use minimum-remaining-values
variable selection, forward checking of every unassigned row, or arc consistency.

In `src/ww-planner.lisp`, `generate-children` gives the `csp` problem type a
specific action-ordering rule: select the action at the current depth while that
depth is within the action list. This adds no restriction to the queens spec's
single reusable action; `next-row` supplies its fixed variable order. The `csp`
label alone does not enable domain propagation. Problem type, search algorithm,
tree/graph selection, and solution type are separate settings.

## Trial, 2026-10-06

The candidate replaced the typed column parameter with a query-generated domain.
It scanned unused column bits, rejected attacked diagonals, and returned legal
columns in ascending order before action precondition evaluation. It retained
the same occupied masks, assignments, first-row reflection pruning, and leaf
canonicalization. It did not prune additional search branches.

Measurements used SBCL 2.6.9, a 4 GiB heap, 16 threads, full GC before each solve,
and a 300-second timeout per solve. N=13 results below are medians of three runs
per condition; baseline runs preceded candidate runs. Small timing differences
are not strong evidence of a speed regression, but there is no demonstrated gain.

| N=13 mode | Existing typed domain | Legal-column query | Change |
|---|---:|---:|---:|
| Classes | 1.091 s | 1.110 s | 1.7% slower |
| All boards | 1.998 s | 2.050 s | 2.6% slower |

Cumulative allocation increased from about 11.347 to 11.549 GB in class mode
and from 20.984 to 21.355 GB in all-board mode. These are allocated bytes over
the whole run, not peak memory. Both versions produced 9,233 classes and 73,712
boards. Search cycles were unchanged: 2,522,417 and 4,601,032 respectively.

Independent legal-successor comparisons and first-placement pruning checks
passed at N=13. At N=15, the legal-successor comparison passed for 1,962 prefixes
and the candidate counted 285,053 classes in 41.427 seconds, allocating
436,068,314,208 bytes with 91,598,540 search cycles. Retained example boards were
checked for column/diagonal validity and canonical form.

The dynamic domain uses Wouldwork's general query evaluation and argument-list
construction path. That overhead is a plausible explanation for the lack of a
gain; it was not isolated with a profiler.

The paired N=15 baseline, run after the candidate, counted the same 285,053
classes in 40.728 seconds, allocating 428,734,443,984 bytes with the same
91,598,540 cycles. Its 1,962-prefix successor check also passed. The candidate
was about 1.7% slower and allocated 1.7% more; each condition was measured once
at this size.

**Do not retain this candidate.** The working spec was restored to its exact
pre-experiment contents, including N=13, 16 threads, and optional class counting.
The earlier reflection optimization remains installed. No engine changes were
needed for this experiment.

A next separately approved experiment could test forward checking: reject a
partial board if any unassigned row has no legal column. Unlike this domain
generation trial, it can eliminate branches earlier, but checking future rows
adds work and must earn its cost in a controlled comparison.

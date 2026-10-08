# Queens counting: measured scaling and overnight estimate

This report records the version before early reflection pruning. See
[queens-reflection.md](queens-reflection.md) for the subsequent approved hook
and its measurements; the timings and projection below are the earlier baseline.

Measured 2026-10-06 on the occupied-set version of
`probs/problem-queensN-1.lisp`, with COUNT retaining one example, depth-first
tree search, 16 threads, SBCL 2.6.9, and a 4 GiB heap. The source was restored
byte-for-byte to N=13 afterwards (SHA256
`D11AC484E818B8916A5E0C4C39CF25642AC179C65639229DF2F6FDC612857756`).

## Completed searches

| N | All boards | Classes | All-board time | Class time |
|---:|---:|---:|---:|---:|
| 13 | 73,712 | 9,233 | 1.995 s | 1.982 s |
| 14 | 365,596 | 45,752 | 11.609 s | 11.589 s |
| 15 | 2,279,184 | 285,053 | 74.734 s | 74.269 s |

N=13 times are the earlier three-run medians; N=14 and N=15 have one completed
run per mode, classes first. Each process staged the chosen size, set 16 threads,
and ran the same `measure-queens-symmetry` helper, which has a five-minute timeout
per search. Full GC preceded each run. Counts and retained examples passed the
helper's assertions. Counts match [OEIS A000170](https://oeis.org/A000170) and
[OEIS A002562](https://oeis.org/A002562). No search timed out or crashed.

| N | All-board program cycles | Class program cycles | All-board allocated bytes | Class allocated bytes |
|---:|---:|---:|---:|---:|
| 14 | 26,992,786 | 27,312,630 | 125,340,681,408 | 125,054,495,520 |
| 15 | 168,849,690 | 170,843,821 | 799,803,010,432 | 797,874,019,808 |

The class check rejects noncanonical completed boards as dead ends, so it adds
program cycles while reducing accepted-goal registration. It does not reduce the
partial-board search. Class times were 0.2% and 0.6% lower at N=14 and N=15;
these small single-run differences do not establish a speedup. They show no
evident penalty through N=15, so retaining the optional check is reasonable.

Both modes processed the same total states: 27,358,553 at N=14 and 171,129,072
at N=15. Complete valid boards were about 1.34% and 1.33% of those visited states,
respectively. This is a fraction of the visited search tree, not the probability
that an arbitrary arrangement is a solution.

## Memory interpretation

Retained Lisp heap, measured with `sb-kernel:dynamic-usage` after full GC:

| N | Before searches | After class count | After all-board count |
|---:|---:|---:|---:|
| 14 | 25,280,304 bytes | 25,351,712 bytes | 25,360,736 bytes |
| 15 | 25,023,280 bytes | 25,167,936 bytes | 25,166,864 bytes |

These include the loaded system and one retained solution; they are not peak
heap or peak working-set measurements. Cumulative allocation of about 800 GB at
N=15 does not mean 800 GB of RAM is required. Both runs fit the 4 GiB heap.
The count mode avoids retaining millions of solution records. Larger-N peak
memory remains unmeasured; the observed limiting factor is elapsed time.

## Projection, not a completed result

The family grows in one linked dimension: increasing N adds a row, a column, and
a queen. Class-count time grew 5.85 times from N=13 to N=14, then 6.41 times to
N=15. Program-cycle growth from 14 to 15 was about 6.25 times, with a small
additional increase in time per cycle.

For a rough planning range, anchor on the measured N=15 class time of 74.269 s.
The lower scenario keeps the latest 6.41-fold growth per size step; the upper
scenario allows 8-fold growth per step. Eight is a conservative planning choice,
not a measured bound or a confidence interval. The latest ratio may itself
increase, so the lower scenario may be optimistic.

| N | Projected class-count time at 16 threads |
|---:|---:|
| 16 | 8–10 minutes |
| 17 | 50–80 minutes |
| 18 | 5.4–10.6 hours |
| 19 | 35–85 hours |

N=16 was not started: even the lower projection exceeds the five-minute analysis
limit. Neither N=18 nor N=19 has been run here. The user's N=13 measurements were
faster than the assistant's, but these projections are not rescaled by that one
comparison; sustained load, GC, and system activity may differ overnight.

**Recommendation:** N=18 with symmetry enabled is the largest plausible target
for an 8–12-hour overnight count using the current implementation. It is not a
guarantee, particularly for an eight-hour deadline. N=17 gives a much larger
margin; N=19 is unlikely to finish overnight. Symmetry remains user-selectable.

To prepare another size, edit the spec's `(defparameter *N* 13)` to the desired
N, then stage it and set 16 threads. Merely setting `*N*` in an already staged
problem does not rebuild the row and column domains. Any overnight run is a
separate user decision; none was started by this evaluation.

## Further options

- Early symmetry restrictions could reduce the partial-board tree; the current
  leaf check only chooses representatives. This needs a separate soundness check
  and timing comparison.
- Generating only currently legal columns could avoid trying all N columns at
  every state. That is another spec change, not part of these measurements.
- A user-run N=16 test would tighten the extrapolation, but exceeds the current
  five-minute analysis budget. It is optional, not needed to sharpen this report.

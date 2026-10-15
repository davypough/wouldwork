# Parallel backtracking: scaling measurements

Recorded 2026-10-14 at commit 1a5a77b. Re-run the procedure below after any change to the
parallel backtracker, the task queue, or allocation on the backtracking path, and compare.

## Machine

lumpy: Intel i9-14900K (8 P-cores with hyperthreading + 16 E-cores, 32 hardware threads),
Windows, SBCL 2.6.8 Win32 (sb-safepoint), `--dynamic-space-size 24000`.

## Procedure

Reload wouldwork, then define the two measurement forms (re-enter them after any full
reload). `timed-silent-solve` runs a full GC first, discards search output, and returns
wall seconds, states, and the TIME report. Within parallel mode `*threads*` is changed with
SETF, as TEST-THREADS does; only crossing zero needs `ww-set` (it reloads the system).

```lisp
(defparameter *bt-scaling-run*
  '(dolist (n '(1 4 8 16))
     (setf *threads* n)
     (multiple-value-bind (seconds states report) (timed-silent-solve)
       (format t "~&~%=== ~A ~D threads: ~,2F s, ~:D states, ~:D solutions~%~A"
               *problem-name* n seconds states *solution-count* report)
       (display-worker-stats))))

(defparameter *bt-serial-run*
  '(multiple-value-bind (seconds states report) (timed-silent-solve)
     (format t "~&~%=== ~A serial: ~,2F s, ~:D states, ~:D solutions~%~A"
             *problem-name* seconds states *solution-count* report)))
```

Problems (enter one form at a time):

```lisp
;; queensN: CSP, count, N=15 as staged (class counting)
(stage queensN)
(ww-set *threads* 0)
(eval *bt-serial-run*)
(ww-set *threads* 1)
(eval *bt-scaling-run*)

;; triangle-xyz: planning problem searched by backtracking, count (exhaustive)
(stage triangle-xyz)
(ww-set *algorithm* backtracking)
(ww-set *solution-type* count)
(eval *bt-serial-run*)
(ww-set *threads* 1)
(eval *bt-scaling-run*)

;; quern: planning, PATH cycle checking, count, depth 11
(stage quern)
(ww-set *algorithm* backtracking)
(ww-set *bt-cycle-check* t)
(ww-set *solution-type* count)
(ww-set *depth-cutoff* 11)
(eval *bt-serial-run*)
(ww-set *threads* 1)
(eval *bt-scaling-run*)
```

## Results

Speedup and CPU are relative to serial (`*threads*` 0). Imbalance is the busiest worker's
state count divided by the mean, minus 1. Every run matched serial exactly in states and
solutions.

| Problem | Run | Wall s | Speedup | Total CPU s | CPU vs serial | System s | Imbalance |
|---|---|---|---|---|---|---|---|
| queensN | serial | 41.08 | 1.00 | 41.00 | 1.00 | 2.13 | - |
| 39,925,457 states | 1 thr | 37.87 | 1.08 | 37.80 | 0.92 | 1.89 | - |
| 285,053 solutions | 4 thr | 10.39 | 3.95 | 40.97 | 1.00 | 2.27 | 0.4% |
| 54.6 GB consed | 8 thr | 5.98 | 6.87 | 46.59 | 1.14 | 3.02 | 1.5% |
| | 16 thr | 5.95 | 6.90 | 84.06 | 2.05 | 10.92 | 1.1% |
| triangle-xyz | serial | 4.21 | 1.00 | 4.20 | 1.00 | 0.55 | - |
| 1,263,420 states, 29,760 solutions, 1.9 GB | 16 thr | 0.54 | 7.84 | 8.48 | 2.02 | 1.34 | 1.1% |
| quern | serial | 16.98 | 1.00 | 16.88 | 1.00 | 1.25 | - |
| 3,430,196 states | 1 thr | 16.56 | 1.03 | 16.50 | 0.98 | 0.97 | - |
| 96,527 solutions | 4 thr | 4.75 | 3.57 | 18.84 | 1.12 | 1.61 | 0.2% |
| 26.0 GB consed | 8 thr | 2.97 | 5.72 | 23.39 | 1.39 | 2.17 | 4.7% |
| | 16 thr | 2.94 | 5.78 | 39.86 | 2.36 | 6.31 | 5.9% |

## Conclusions

- Correctness: identical states and solutions at every thread count. Bytes consed are equal
  across thread counts, so task generation and prefix replay add no measurable cost.
- Load balance: the busiest worker is at most 6% above the mean, so work donation for
  backtracking could save at most about 6% of wall time. It was not implemented.
- Ceiling: wall time is flat from 8 to 16 threads while CPU doubles and system time rises.
  Threads beyond the 8 P-cores land on hyperthread siblings or E-cores. Use 8 threads for
  parallel backtracking on lumpy.
- Allocation: quern (about 7,600 bytes consed per state) loses efficiency sooner than
  queensN (about 1,370 bytes per state): CPU 1.39x vs 1.14x serial at 8 threads. Further
  gains would come from reducing consing on the backtracking path, not from scheduling.
- One worker thread is 3-8% faster than serial; not investigated.

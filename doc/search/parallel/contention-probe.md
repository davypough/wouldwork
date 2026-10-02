# Parallel contention diagnostic

Load `src/ww-contention-probe.lisp` explicitly after staging. Loading runs no
search. With workers idle, use `(ww-set *threads* 8)` and then
`(run-contention-probe DEPTH)` with the previously measured fixed cutoff.
Run one measurement at a time and review it before choosing another worker count.
The helper requires parallel depth-first graph search, debug/probe off, and
`*ww-timing-enabled*` NIL. It temporarily uses EVERY and disables randomization.

The helper reads and compiles the current successor function with a local lock
macro. It temporarily installs that function plus worker/queue wrappers, restoring
the original definitions and search controls with UNWIND-PROTECT. It does not
change the production lock macro, disable counters, or remove synchronization.
Do not reload/stage or run other searches while the diagnostic is active.

Each worker records every 1024th shard acquisition: raw wait and hold wall-clock
ticks, sample counts, and total acquisition counts. Hold time includes scheduling
delays and diagnostic overhead; wait time includes acquisition overhead. Clock
resolution can hide short waits. Neither sample total is a whole-run total.
Fixed-period sampling can be biased by periodic work. Every acquisition also
incurs the diagnostic function/closure and counter overhead, so compare like
instrumented runs rather than treating these as baseline performance timings.

Queue time includes mutex acquisition, task retrieval, and waiting for work.
Terminal queue time is reported separately; neither is pure idle time. Per-worker
states, cycles, duplicates, and donations help identify imbalance. The existing
global symmetry-duplicate count records frequency, not time spent on its atomic
increment, and includes coordinator work. No per-event shared counter is added.

Results remain in `*contention-results*` and print between explicit markers.
The helper performs one solve only, with no replay. Its final solution state is
left available normally. Compilation/runtime verification is user-run at the
REPL; static checks alone do not establish correctness or causal attribution.

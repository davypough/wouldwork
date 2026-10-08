# Counting solutions with one retained example

After staging a problem, use `(ww-set *solution-type* count)` and `(solve)`.
The accepted-goal total is available in `*solution-count*`, reset at each search.
`*solution-paths*` and `*unique-solution-states*` stay empty. COUNT retains only
the first accepted solution in `*count-example*`, reset to NIL at each search.
The summary prints its action path and final state directly, without replay or
another search. It is an example, not a shortest or best solution. With no
accepted goals there is no example. Backtracking copies that one goal state
before undoing it; other drivers retain the existing state. Parallel callers
claim the first example under the same lock as the counter, so only one saves it.
Candidate paths can still be constructed for the existing validation pipeline.

Supported drivers are serial and parallel depth-first and serial and parallel
backtracking, including goals found at the start or during root-task generation.
Workers increment the shared integer under a lock. Candidate solution validators
run before counting, just as they do before recording an ordinary solution.

COUNT counts accepted goal encounters under the search's existing traversal,
depth limit, branch restrictions, and pruning. It does not deduplicate goal states,
enumerate all paths through a graph, or identify rotation/reflection classes.
In graph search a goal reached by multiple routes can be counted more than once;
closed-state pruning can also suppress routes. Use this mode for distinct-board
counts when the model gives each board exactly one construction path, as in
fixed-row-order queens. A cutoff or interrupted run is not a complete count.

Regression check: load `test/search/count-solutions.lisp` and call
`(ww::test-count-solutions)`. It checks serial, parallel task generation, workers,
mixed task/worker goals, backtracking, validators, zero goals, cutoff, start goals,
counter/example reset, unchanged EVERY recording, empty solution lists, and a
valid retained example (including validator acceptance and the zero-action case).
It also checks ordinary and mutation test outcomes for zero and positive counts.
`test-talos` treats a positive accepted-goal count as success in COUNT mode;
zero counted goals fails the ordinary test and detects a mutation. Other modes
continue to use retained solution paths to determine whether a solution exists.

For the unmodified N=13 queens spec, with Wouldwork already loaded and the REPL
in the `ww` package (`solve` includes timing):

```lisp
(stage queensN-csp)
(ww-set *threads* 16)
(ww-set *solution-type* count)
(solve)
(list *solution-count* (length *solution-paths*)
      (length *unique-solution-states*))
```

The user verified `(73712 0 0)` on 2026-10-06 with 16 threads, matching the
73,712-board EVERY baseline. COUNT took 2.060 seconds, including 0.147 seconds
of GC, and allocated 19,106,470,336 bytes. The supplied EVERY run took 2.264
seconds, including 0.360 seconds of GC, and allocated 19,117,376,080 bytes.
These are single-run observations, not a repeatable speedup measurement or a
peak-memory comparison. This counts all boards, including rotations and
reflections; symmetry-class counting is a separate change.

Those timings preceded the one-example addition. `(73712 0 0)` remains the
expected list result; the separately retained `*count-example*` is also printed
in the summary. N=13 performance of this addition awaits the user's REPL check.

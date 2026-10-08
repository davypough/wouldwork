# Assignment tree comparison

This synthetic workload isolates serial tree traversal with cheap actions. It is
not an empirical average of Wouldwork problems and should not be generalized to
expensive preconditions, heuristics, planning cycles, or graph search.

Eight variables each accept three values. At depth eight there are 6,561 goals,
3,280 internal states and 9,841 states including the root. Exactly 9,840
precondition calls and effect calls are expected per complete search. Each action
replaces the position fluent and one assignment fluent. Eight assignment fluents,
one position fluent and 31 unchanged dynamic payload facts give a constant 40
facts. Payload deliberately exposes copying cost; it is not useful domain work.

CSP mode avoids cycle checks in both algorithms. There is one action, reused at
every level. Search uses serial tree/COUNT mode, no heuristic, no symmetry
pruning, no randomization and no depth cutoff. All leaves are accepted; later
siblings cannot be skipped by first-solution termination.

Load the helper after Wouldwork:

```lisp
(load "test/search/assignment-tree-benchmark.lisp")
```

For initial REPL validation, use the same helper on the tiny depth-two tree:

```lisp
(benchmark-assignment-tree :depth 2)
```

Acceptance: each warm-up reports 12 preconditions, 12 effects and 9 goals; all
six measured runs report 9 goals. Assertions also check fact count, example
depth, COUNT path storage, and backtracking database restoration. Timing at this
depth is not a useful performance result.

After validation and separate approval to benchmark:

```lisp
(benchmark-assignment-tree)
```

Each invocation performs one instrumented warm-up per algorithm and exactly
three uninstrumented measured runs each, alternating which algorithm runs first
by round. Each run stages and translates the fixture outside the timer; full GC
also occurs outside the timer. Measured SOLVE includes search initialization,
COUNT goal/path processing, and reporting directed to a discard stream. GC during
SOLVE remains included. Warm-up counts are asserted against the mathematical
tree; each measured run independently verifies the accepted-goal count.

The helper returns all six rows and warm-up evidence, and prints median elapsed
time, CPU time, allocated bytes, and elapsed DFS/BT speedup. Loading the helper
alone does not run anything. Invoking it replaces the staged problem and leaves
the assignment fixture staged; it does not restore the preceding problem.

Optional, separately invoked sensitivity cases use `:facts 32` and `:facts 48`.
They change only unchanged payload size, preserving the same tree and two
changed fluents. No sensitivity cases run automatically. Depth may be 1 through
8. Custom depth/fact values are dynamically bound during the helper; restaging
afterward uses the defaults again.

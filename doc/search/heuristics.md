# Heuristic Search

A heuristic function analyses a state and returns a non-negative number indicating how promising it is — **lower is more promising**. Wouldwork explores lower-valued successors first.

This is **heuristic-ordered depth-first search**. The heuristic sorts successors; it does not impose a beam width or turn the search into A*. A first solution need not be shortest. Use `*solution-type* min-length` for length optimization, and distinguish a completed search under valid pruning rules from an interrupted or restricted run.

**Serial and parallel depth-first search both use heuristics.** `generate-children` in
`ww-planner.lisp` computes scores. `search-serial` in `ww-searcher.lisp` and the task-generation
and worker loops in `ww-parallel.lisp` order successors using those scores. Parallel execution
does not preserve the serial visitation order. Backtracking does not consult `heuristic?`.

Source-checked on 2026-10-02; no performance runs were made for this review.

See Manual Part 3, "Heuristic Search," for the mechanism. What follows is applicability guidance.

---

## Defining one

A heuristic is a query with the reserved name `heuristic?` and no arguments; the current state is supplied automatically.

```lisp
(define-query heuristic? ()
  ...
  (return-from heuristic? $value))
```

`init()` applies it to the start state during load and prints the result. A zero score alone
does not establish whether it discriminates: compare scores across states with different progress.

### Combining components

`combine-heuristics` (in `ww-support.lisp`) merges weighted components:

```lisp
(define-query heuristic? ()
  (do (combine-heuristics
        state                          ;<-- state is the FIRST argument
        '((200 . h-color-path-deficit)
          (100 . h-unpowered-placed-connectors)
          (10  . h-agent-goal-distance))
        :combiner :weighted-sum)))
```

Three things to get right:

1. **`state` is the first argument.** The signature is `(combine-heuristics state specs &key combiner admissible)`. Omitting it is the most common error.
2. **Weights only apply under `:weighted-sum`.** The `:max` and `:sum` branches destructure the weight and then never use it — under those combiners `((200 . h-a) (10 . h-b))` behaves identically to `((1 . h-a) (1 . h-b))`. If you are weighting, use `:weighted-sum`.
3. **`:admissible` does nothing.** It is accepted and immediately declared ignored. It documents intent to a reader and has no effect on search.

---

## The four kinds

### 1. Goal-counting

Count goal conjuncts, or goal-relevant objects, not yet satisfied.

```lisp
(define-query h-inactive-receivers ()
  (do (setq $count 0)
      (doall (?r receiver)
        (if (not (active ?r))
          (incf $count)))
      $count))
```

**When it helps.** The goal is a conjunction of roughly independent, individually achievable conditions, and progress is genuinely monotone — satisfying one conjunct tends not to break another.

**When it doesn't.** Coupled goals, where achieving one conjunct destroys another. The count then stays flat or oscillates while real progress is being made, and the search is pushed away from the states it needs to pass through. This is the failure mode on must-undo problems, where the correct plan *increases* the count partway through.

**Applicability criteria.** Goal is a conjunction; conjuncts are largely independent; no scarce resource is shared between them.

**Cost.** Cheap — one pass over a type.

### 2. Distance

Spatial or numeric distance from where things are to where they need to be.

```lisp
(defun manhattan-distance (x1 y1 x2 y2)
  (+ (abs (- x2 x1)) (abs (- y2 y1))))
```

**When it helps.** The problem has real geometry, the goal names positions, and movement cost is roughly proportional to distance. Grid and navigation problems.

**When it doesn't.** When the topology is gated rather than open — distance-to-goal is actively misleading when the correct route runs *away* from the target to open a gate first. Also useless when position is incidental to the real difficulty.

**Applicability criteria.** Coordinates or a metric exist; the goal is positional; few detours are forced by gating.

**Note on availability.** Wouldwork has no built-in distance library. `manhattan-distance` and friends are plain Common Lisp defuns you write in your own problem file. `get-coordinates` and `get-fixed-coordinates` exist only inside the `problem-corner-*-macro` variants, not as general utilities — coordinate access depends on how your problem represents position (`location-coords>`, `coords>`, or a fluent of your own).

### 3. Conditional / penalty

A step function: a fixed penalty when some condition fails, zero when it holds.

```lisp
(define-query h-agent-goal-distance ()
  (do (bind (loc agent1 $area))
      (if (eql $area 'area4) 0 1)))
```

**When it helps.** As a *component* in a weighted combination, to encode a known ordering constraint — "opening gate1 matters far more than being in the goal area, because reaching the goal area is impossible until gate1 opens." Weights let you state that priority directly.

**When it doesn't.** Alone. A single binary heuristic partitions states into two buckets and gives no gradient within either, so the search is unguided almost everywhere.

**Applicability criteria.** You know a hard precedence between subgoals and can express it as weights.

### 4. Aggregation

Not a peer of the other three — a combinator over them. Sum, max, or min a per-object heuristic across a collection.

```lisp
(defun h-sum-over (items heuristic-fn)
  (reduce #'+ items :key heuristic-fn :initial-value 0))
```

**Which aggregator.** Sum when every object must be fixed and the work is roughly additive.
Max emphasizes the hardest remaining requirement when all must be satisfied. Min fits alternatives
where reaching any one target suffices — "how far to the closest connector."

**Caution on sum.** Summing per-object distances can overestimate remaining work when one action
advances several objects. As an ordering score, this can affect search effort and the first plan
found. Reusing it as a pruning bound requires a separate admissibility argument: an overestimate
there can discard a needed path. `heuristic?` and `min-steps-remaining?` have different roles.

---

## Worked example — `problem-corner.lisp`

The clearest real heuristic in the repository. Seven components, weighted by how tightly each blocks the goal:

| Weight | Component | Measures |
|---|---|---|
| 200 | `h-color-path-deficit` | missing active transmitter→receiver paths |
| 100 | `h-unpowered-placed-connectors` | placed but inactive connectors |
| 80 | `h-color-mismatch` | powered connectors with the wrong colour |
| 50 | `h-goal-receivers-inactive` | goal receivers still needing activation |
| 30 | `h-gate1-blocks-goal` | gate1 must open for the agent to reach area4 |
| 20 | `h-useless-pairings` | receiver-only pairings that can never activate |
| 10 | `h-agent-goal-distance` | agent not yet in the goal area |

Two things worth copying. The weights encode a **causal ordering** — beam paths dominate, agent position is a tiebreaker — rather than being tuned by trial. And each component carries a comment saying what it measures and why its weight sits where it does, including one (`h-useless-pairings`) annotated as a candidate for removal because pairings can always become useful later.

Note what this problem does *not* do: it has no relaxation. Its sibling `problem-corner-relaxed.lisp` has no heuristic. See [`relaxation.md`](relaxation.md).

---

## Pitfalls

- **Omitting `state`** from the `combine-heuristics` call.
- **Weighting under `:max` or `:sum`**, where weights are silently discarded. Use `:weighted-sum`.
- **Expecting `:admissible t` to do something.** It is ignored.
- **Expecting identical serial and parallel traversal.** Both use scores, but scheduling and donation change the overall order. Heuristic callbacks must be safe for concurrent use.
- **Expecting ordering alone to prove optimality.** Use the appropriate optimization objective and assess the search's completion and pruning assumptions.
- **Goal-counting on a must-undo problem**, where the count must rise before it can fall.
- **Distance heuristics on gated topology**, where the route to the goal leads away from it.
- **An expensive heuristic.** It is evaluated on generated child states. Full propagation inside it can cost more than the search it saves. Measure that cost before choosing a cheaper score or a separately validated relaxation.

# Novelty Pruning

Discard a generated state unless it asserts some small conjunction of atoms that no earlier
state in the search asserted. Unlike a heuristic, it changes nothing about exploration
*order*; unlike a relaxation, it changes nothing about which states are *legal*. It simply
throws states away, and it is the one strategy here that can lose a solution.

Implemented in `src/ww-novelty.lisp`, controlled by `*novelty-pruning*` and
`*novelty-partition*`. Serial depth-first only — the tables are unsynchronized, and
`check-problem-parameter` refuses to enable it while `*threads*` is non-zero.

---

## What it is

A state is **novel at width k** when some conjunction of at most k of its atoms has not been
asserted by any earlier state. Width 1 asks for one new atom; width 2 also accepts a new
*pair*. Non-novel states are discarded.

The technique targets a specific failure mode: a problem where the state "barely improves
for twenty actions and then three actions decide everything." That is exactly where a
heuristic has no gradient to offer. Novelty needs no gradient and no domain knowledge — it
measures whether the search is seeing anything it has not seen before.

```lisp
(ww-set *novelty-pruning* 2)        ; nil (off), 1, or 2
(ww-set *novelty-partition* depth)  ; nil, depth, or query
```

Both live in `*problem-parameter-defaults*`, so staging resets them. Setting them in the
problem file works — problem-file `ww-set` forms run during staging, after the defaults are
restored — and so does setting them at the REPL afterwards.

### What counts as an atom

Entries of the state's `idb`. A fluent proposition is identified by its storage key
*together with its values*, so an object that moved is a different atom, not the same atom
with a new value. Each distinct proposition is interned to a small integer id for the
duration of one search; pairs are encoded as one integer under a canonical ordering. The
tables are cleared and re-seeded from the start state before every search.

### Where it sits

`process-successors` applies it **after** the goal test, deliberately. `search-successor-pruned-p`
runs *before* that test, so a novelty pruner registered through that hook would silently
discard a goal state that happened to be non-novel. Activity is reported as
`Novelty pruning discarded N states, X% of total states.`

---

## The soundness obligation

Relaxation has an implication to argue: real ⟹ relaxed. **Novelty has no such argument.** It
is incomplete by construction. A plan found under it is valid — the states on it were
legal — but a failure to find one says nothing about the problem.

The substitute for an argument is a measurement. **Retro-validate against a known plan
before trusting a negative result:** replay the plan one action at a time and ask whether
each state survives the filter.

```lisp
(setf *novelty-pruning* 2)
(setf *novelty-partition* 'depth)
(reset-novelty-pruning)
(let ((st (copy-problem-state *start-state*)))
  (iter (for a in *plan*)
        (for i from 1)
        (setf st (apply-action-to-state a st nil))
        (when (novelty-pruned-p st i)
          (format t "~%plan state ~D would be pruned" i))))
```

Note this test is *optimistic*: in a real search many other states are seen first, so the
plan's states have more competition than the replay gives them. A plan that fails the replay
is certainly lost; a plan that passes is not guaranteed.

---

## Partitions

A state is compared only against earlier states in its own partition. A partition that
advances with real progress lets a search revisit ground it has already covered, which is
what a **must-undo** plan requires.

| `*novelty-partition*` | Meaning |
|---|---|
| `nil` | One global partition. Strongest pruning, most likely to lose a plan. |
| `depth` | Compare only within a search level. Weakest pruning, safest. |
| `query` | Compare within the value of a problem-defined `novelty-partition?` query. |

**`novelty-partition?` must be defined in the problem file, before staging.** A
`define-query` issued at the REPL after `(stage ...)` never becomes fbound, and the search
dies with `The function NOVELTY-PARTITION? is undefined`. Return any value; partitions are
compared with `equal`.

---

## Worked example — rumin-topo

All figures measured on SBCL 2.2.9, graph search, `*solution-type*` first,
`*symmetry-pruning*` nil.

### Where it pays: a short horizon

From the action-85 boundary of the 90-step solution, cutoff 5, with `min-steps-remaining?`
unbound (its terms are chunk-specific and inadmissible elsewhere):

| | states | result |
|---|---|---|
| no novelty | 1,149 | solution found |
| width 2 | **49** | same solution |
| width 1 | 278 | **no solution** |

Width 1 is too aggressive for this domain, and the retro-validation says why: it prunes 40
of the 90 states of the known solution. Width 2 prunes 1.

Two further effort comparisons, same problem, novelty against nothing: the action-30
boundary at cutoff 3 goes 16,931 → 3,358 states, and the action-80 boundary at cutoff 10
goes 137,960 → 5,393.

### Where it does not: a deep must-undo chunk

Chunk 1 of the five-cycle subgoal chain — 30 actions, subgoal
`(and (has-location box1 location8) (has-location tray1 location2))`. The chunk carries
tray1 east to open plate1 and brings it *back* to loc2, so its own plan re-asserts a fact
that was true at the start.

| partition | plan states pruned | search at cutoff 30 |
|---|---|---|
| `nil` | 1 of 30 (state 27) | 4,926 states, 3.9 s, no solution |
| `query`, satisfied goal conjuncts | 1 of 30 (state 27) | 4,926 states, 3.9 s, no solution |
| `query`, 13 progress markers | 1 of 30 (state 27) | — |
| `depth` | **0 of 30** | 100,160 states, 95.5 s, 53.1% pruned, no solution |

**Why the designed partitions failed, and it is the same reason twice.** State 27 is
`PUT-TRAY TRAY1 GROUND LOCATION2`, and it changes only `on` and `holding` facts. By state 26
the tray is *already* at loc2 — carried by an agent standing there — gate1 is already open
and box1 is already at loc8. Both partitions returned the identical value at 26 and 27
(1021 for the progress-marker version), so state 27 was compared against its own
predecessor and lost.

The feature that separates those two states is "held versus resting", not progress. A
partition fine enough to express that class of distinction is converging on `depth`, which
gets 0 of 30 with no design work at all.

`depth` is therefore the setting to use here — and it still does not bring chunk 1 into
reach. That is not a failure of the filter. Chunk 1 is 30 levels deep at a branching factor
near 10, and no state-discarding rule closes that gap.

---

## Applicability criteria

- Serial depth-first search, `*threads*` 0.
- A **known plan to retro-validate against**, or acceptance that a negative result is
  uninformative.
- A horizon short enough that the search can plausibly finish — novelty multiplies a
  feasible search, it does not rescue an infeasible one.
- Width 2 unless retro-validation shows width 1 preserves your plan. Width 1 is usually
  too aggressive on problems with conjunctive goals.
- `depth` partitioning when the plan must undo something; `nil` when it need not.

## Pitfalls

- **Reading "no solution found" as a fact about the problem.** Under novelty it is a fact
  about the filter. Re-run with `*novelty-pruning*` nil before concluding anything.
- **Width 1 by default.** It is the cheap setting and the one that most often silently
  discards the answer.
- **Registering novelty as a search-successor pruner.** That hook runs before the goal test;
  a non-novel goal state would be thrown away.
- **Defining `novelty-partition?` at the REPL.** It must be in the problem file before
  staging.
- **Designing a partition without tracing the plan.** Two partitions were designed here from
  plausible reasoning and both failed at the same state, for a reason a two-minute trace of
  the plan's partition values would have shown first.
- **Expecting it to compose with an inadmissible bound.** `min-steps-remaining?` written for
  one chunk will prune the goal path at another boundary, and the resulting "no solution" is
  then doubly uninformative.

## See also

[`heuristics.md`](heuristics.md) — changes exploration order, needs a gradient.
[`relaxation.md`](relaxation.md) — changes which states are legal, needs goal post-validation
and a soundness argument. Novelty needs neither and offers no guarantee in exchange.

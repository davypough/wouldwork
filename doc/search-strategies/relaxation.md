# Relaxation

Replace expensive derived-relation tests in action preconditions with cheap base-relation approximations, then re-check the real conditions once at the goal.

Introduced in *Wouldwork User Manual* Part 3, "Relaxation" (added in 26.8), which gives the plain-language version. These notes add the applicability detail. The worked example is `problem-corner-relaxed.lisp`, which solves in roughly 400 seconds.

---

## What it is

A precondition like `(open gate1)` is a *derived* fact — true only after `propagate-changes!` has run the full consequence cascade over the current state. Testing it during search means propagating on every candidate, which on a problem with rich derived state dominates the entire search cost.

Relaxation replaces that test with an approximation computed from **base** relations only — the facts actions assert directly, such as `loc` and `paired` — which requires no propagation. The approximation is deliberately permissive: it admits some states that are not truly legal. Those get filtered at the goal, where full propagation runs once.

The technique has three parts, and all three are required:

1. **Base/derived separation.** Know which of your dynamic relations are base — asserted directly by actions — and which are derived by the propagation cascade. The Manual discusses this distinction, but only in the enumerator's context; it applies equally here.
2. **Relaxed preconditions.** For each expensive derived test in an action, write a query that approximates it from base relations alone.
3. **Goal post-validation.** The goal checks base conjuncts first, then propagates in place, then checks the derived conjuncts.

---

## The soundness obligation

**The relaxed test must be implied by the true test.** Wherever the real precondition holds, the relaxed one must also hold. The relaxation admits a *superset* of the legal states.

Get this backwards — write a relaxation that is stricter than reality in some case — and the search silently discards states on the only path to a solution. You get "no solution found" on a solvable problem, with nothing indicating why.

The safe direction to err is permissive. An over-permissive relaxation costs search time exploring states that will fail post-validation. An over-restrictive one costs you the answer.

Practically: enumerate what the real test depends on, and drop terms rather than adding them. `gate1-open-relaxed` drops beam occlusion, beam-beam interference, and gate occlusion of beams — three ways a beam could fail to arrive. Dropping them can only make the test more often true.

---

## When it helps

- Derived state is expensive and pervasive — `propagate-changes!` runs a multi-pass cascade and gets called on nearly every candidate state.
- The derived facts you test in preconditions are reachable, approximately, from base facts you already have.
- The approximation is *tight enough* that post-validation doesn't reject nearly everything.
- Goal states are relatively rare, so paying full propagation only there is a large net saving.

## When it doesn't

- **Propagation isn't the bottleneck.** If the cost is branching factor or depth, relaxation buys nothing. Profile before assuming.
- **No cheap approximation exists.** If the derived fact genuinely requires the cascade, a relaxed version will either be unsound or so loose it admits everything.
- **The relaxation is too loose.** If almost every relaxed-legal state fails post-validation, you have moved the cost rather than removed it, and possibly made it worse.
- **You need every solution.** Relaxation pairs naturally with finding *a* solution. With `*solution-type* every` you pay post-validation on a much larger candidate set.

## Applicability criteria

- Actions assert base relations and call `propagate-changes!`, rather than asserting derived facts directly.
- At least one derived precondition is both expensive and frequently tested.
- You can state, for each relaxation, why the real condition implies the relaxed one.
- The goal can be split into base conjuncts and derived conjuncts.

---

## Worked example — `problem-corner-relaxed.lisp`

The header states the approach directly:

> This version explores replacing derived-relation checks (open, active, color, holds, etc.) with base-relation approximations (loc, paired) to avoid expensive `propagate-changes!` calls during search. The relaxed preconditions admit a superset of the exact accessible states, so candidate solutions must be post-validated with full propagation at goal check.

### The relaxation

`(open gate1)` becomes `gate1-open-relaxed`. Gate1 opens when receiver1 is activated by a red beam from transmitter1, so the relaxed test looks for a *pairing chain* from transmitter1 to receiver1 using only `loc` and `paired` plus static line-of-sight:

```lisp
(define-query gate1-open-relaxed ()
  ;; 1-hop: one connector paired with both ends
  (or (exists (?c connector)
        (and (bind (loc ?c $area))
             (paired ?c transmitter1)
             (paired ?c receiver1)))
      ;; 2-hop: area3 connector to area2 connector
      (exists ((?c1 ?c2) connector)
        (and (loc ?c1 area3)
             (loc ?c2 area2)
             (paired ?c1 transmitter1)
             (or (paired ?c1 ?c2) (paired ?c2 ?c1))
             (paired ?c2 receiver1)))))
```

What it ignores, as documented in its own comment: beam occlusion, beam-beam interference, and gate occlusion of beams. Each omission can only make the test more often true — the soundness direction.

The 2-hop clause is asymmetric, restricted to area3 → area2, because the reverse direction crosses beams and causes interference. That is a hand-verified geometric fact about this puzzle, recorded in the file header. It is also the most fragile part of the relaxation: it is a *restriction*, and restrictions are the dangerous direction. It is safe here only because the excluded direction is genuinely never viable.

### The post-validating goal

```lisp
(define-goal
  (and ;; First check base relations
       (loc agent1 area4)
       (exists ((?c-blue ?c-red ?c-other) connector)
         (and (loc ?c-blue area2)
              (loc ?c-red area3)
              (not (bind (loc ?c-other $anywhere)))
              (paired ?c-blue transmitter2)
              (paired ?c-blue receiver3)
              (paired ?c-red transmitter1)
              (paired ?c-red receiver2)))
       ;; If base satisfied then propagate in place and check derived relations
       (propagate-changes!)
       (active receiver2)
       (active receiver3)))
```

The ordering is the whole point. Cheap base conjuncts are tested first and reject most candidates. Only survivors reach `propagate-changes!`. The derived conjuncts are then tested against the fully propagated state.

Calling an update function inside `define-goal` is legal — the translator permits update calls in goal context — and it modifies state in place, which is safe because goal states are leaves that are never expanded further.

---

## Pitfalls

- **A relaxation that is stricter than reality**, anywhere. Silently loses solutions with no diagnostic. Always argue the implication in the direction *real ⟹ relaxed*.
- **Forgetting post-validation**, or putting derived conjuncts before `propagate-changes!` in the goal. Then relaxed-but-illegal states are accepted and you get plans that don't work.
- **Ordering the goal badly.** Derived checks before base checks means propagating on candidates a cheap test would have rejected — the exact cost the technique exists to avoid.
- **Relaxing something that wasn't expensive.** Adds a second definition to keep in sync with the real one for no gain.
- **Letting the relaxed and real definitions drift.** They encode the same intent at different fidelities. When the real rule changes, the relaxation must be re-checked for soundness. Keep them adjacent in the file, and note in each which one it approximates.
- **Assuming a relaxed spec is a drop-in replacement.** `problem-corner-relaxed.lisp` is a separate file from `problem-corner.lisp` for good reason: the relaxation encodes puzzle-specific geometric facts that do not transfer.

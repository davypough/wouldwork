# Relaxation

Relaxation replaces selected expensive derived-relation tests with cheaper, permissive
approximations. A resulting candidate path must still satisfy the original action rules and goal.
Checking only the final configuration does not establish that earlier actions were legal.

See *Wouldwork User Manual* Part 3, "Relaxation," for background. These notes describe the
current `problem-corner-relaxed.lisp` example and the validation obligations. Source-checked on
2026-10-02; no solve, timing measurement, or full-path replay was performed for this review.

---

## What it is

A precondition like `(open gate1)` depends on derived state. If actions already maintain that
state, testing it is a lookup. If the search stores only base facts, obtaining the correct gate
status may require propagation. That recomputation can be expensive; measure before relaxing it.

An approximation can use **base** relations such as `loc` and `paired`, together with static
geometry, without full propagation. It may admit transitions that the original rules forbid.
Final propagation checks the endpoint's derived facts; exact path validation checks the transitions.

The technique has three parts, and all three are required:

1. **Base/derived separation.** Know which of your dynamic relations are base — asserted directly by actions — and which are derived by the propagation cascade. The Manual discusses this distinction, but only in the enumerator's context; it applies equally here.
2. **Relaxed preconditions.** For each expensive derived test in an action, write a query that approximates it from base relations alone.
3. **Exact acceptance.** Check the derived goal conditions and replay every action from the original
   start under the original rules. Replaying only the relaxed specification does not provide this
   guarantee. Alternatively, retain exact checks for every affected transition during search.

---

## The two validation obligations

**Preserve legal transitions:** wherever the real precondition holds, the relaxed test must also
hold. For unchanged effects, weakening preconditions admits a superset of transitions. Changes
to state representation or effects require their own correspondence argument.

**Accept only legal paths:** a relaxed candidate is not yet a solution to the original problem.
For example, reaching an open-gate final configuration does not prove that the gate was open
when an earlier movement crossed it. Complete exact replay is required when transition checks
were weakened. Rejecting one candidate also does not prove that no valid solution exists.

Get this backwards — write a relaxation that is stricter than reality in some case — and the search silently discards states on the only path to a solution. You get "no solution found" on a solvable problem, with nothing indicating why.

A permissive approximation can cost time exploring invalid paths; an overly restrictive one
can remove a valid solution. Neither final-state checking nor exact replay repairs lost paths.

Practically: enumerate what the real test depends on, and drop terms rather than adding them. `gate1-open-relaxed` drops beam occlusion, beam-beam interference, and gate occlusion of beams — three ways a beam could fail to arrive. Dropping them can only make the test more often true.

---

## When it helps

- Derived state is expensive and pervasive — `propagate-changes!` runs a multi-pass cascade and gets called on nearly every candidate state.
- The derived facts you test in preconditions are reachable, approximately, from base facts you already have.
- The approximation is *tight enough* that post-validation doesn't reject nearly everything.
- Candidate goals are rare enough that endpoint checks and complete exact replay cost less than the propagation avoided during exploration.

## When it doesn't

- **Propagation isn't the bottleneck.** If the cost is branching factor or depth, relaxation buys nothing. Profile before assuming.
- **No cheap approximation exists.** If the derived fact genuinely requires the cascade, a relaxed version will either be unsound or so loose it admits everything.
- **The relaxation is too loose.** If almost every relaxed-legal state fails post-validation, you have moved the cost rather than removed it, and possibly made it worse.
- **You need every solution.** Relaxation pairs naturally with finding *a* solution. With `*solution-type* every` you pay post-validation on a much larger candidate set.

## Applicability criteria

- Actions assert base relations and call `propagate-changes!`, rather than asserting derived facts directly.
- At least one derived precondition is both expensive and frequently tested.
- You can state, for each relaxation, why the real condition implies the relaxed one.
- The goal can be split into base and derived conjuncts, and the original transition rules are available for exact validation.

---

## Worked example — `problem-corner-relaxed.lisp`

The file retains an experimental relaxation, but its current behavior is mixed:

- `accessible` copies the state, calls `propagate-changes!` on the copy, and checks the actual
  gate status for gated movement. The movement action uses this query.
- `passable` uses `gate1-open-relaxed`. It is used when deciding which termini can be selected
  from an adjacent area, so a relaxed check remains in connection selection.
- The goal checks base facts, then propagates and checks receiver activation.

The older header describes the original experiment more broadly than the current movement code.
This document does not establish full-path validity or a performance result for that experiment.

### The relaxation

In `passable`, the exact gate test is replaced by `gate1-open-relaxed`. It looks for a pairing
chain from transmitter1 to receiver1 using `loc` and `paired`. The query below does not itself
test line of sight; any geometric restrictions must come from surrounding rules or an explicit argument.

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

The two-hop clause restricts the chain to area3 → area2. A source comment attributes this to
interference in the reverse direction. Treat that as a puzzle-specific claim requiring verification,
not a general proof. The limited chain lengths and area restriction must preserve every relevant
legal case before this query can be claimed to be a permissive approximation.

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

The current goal calls an update in place. Do not generalize this into a guarantee that mutation
inside a goal test is safe: propagation may run and a later derived conjunct may still fail,
leaving a modified non-goal state available for further processing. For a new implementation,
prefer testing derived conditions on a copy unless mutation is explicitly part of the state contract.
In either case, this goal checks the endpoint, not the complete action history.

---

## Pitfalls

- **A relaxation that is stricter than reality**, anywhere. Silently loses solutions with no diagnostic. Always argue the implication in the direction *real ⟹ relaxed*.
- **Treating endpoint propagation as full validation.** Check every relaxed transition under the original rules, as well as the final goal.
- **Ordering the goal badly.** Derived checks before base checks means propagating on candidates a cheap test would have rejected — the exact cost the technique exists to avoid.
- **Relaxing something that wasn't expensive.** Adds a second definition to keep in sync with the real one for no gain.
- **Letting the relaxed and real definitions drift.** They encode the same intent at different fidelities. When the real rule changes, the relaxation must be re-checked for soundness. Keep them adjacent in the file, and note in each which one it approximates.
- **Assuming a relaxed spec is a drop-in replacement.** `problem-corner-relaxed.lisp` is a separate file from `problem-corner.lisp` for good reason: the relaxation encodes puzzle-specific geometric facts that do not transfer.

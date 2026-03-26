# Continuation Prompt: Relaxed Search for problem-corner-relaxed.lisp

## Context

We are developing an optimization for Wouldwork's search on the Talos Principle "Around the Corner" puzzle. The original `problem-corner.lisp` uses expensive `propagate-changes!` calls after every action to compute all derived relations (beams, colors, active states, open gates) from base relations (`loc`, `paired`). We are exploring a **relaxed/validate approach** where the search operates primarily on base relations, avoiding `propagate-changes!` during search, and then post-validates candidate solutions with full propagation.

## Files

- `problem-corner.lisp` — Original problem with full beam physics propagation
- `problem-corner-relaxed.lisp` — Work-in-progress relaxed variant (current development target)

## What Has Been Done

1. **Created `problem-corner-relaxed.lisp`** as a copy of the original, with `*problem-name*` set to `corner-relaxed` and heuristics section removed.

2. **Relaxed the `accessible` query** — Replaced `(open ?g)` (a derived relation requiring full beam propagation) with `gate1-open-relaxed`, a new query that checks for a pairing chain from transmitter1 to receiver1 using only base + static relations:
   - **1-hop:** Any connector placed somewhere, paired with both transmitter1 and receiver1.
   - **2-hop:** A connector in area3 paired with transmitter1, linked to a connector in area2 paired with receiver1. This is the only viable 2-hop direction — the reverse (area2→area3) causes beam-beam interference. Domain knowledge constrains this; no need for 3-hop chains.
   - Color matching is guaranteed structurally (transmitter1 has chroma red, receiver1 needs red), so no `color` check is needed.

## What Needs To Be Done Next

We were about to address the **move action's effect side** and then the remaining derived-relation dependencies. Here is the roadmap:

3. **Remove `propagate-changes!` from the move action effect.** Currently:
   ```lisp
   (assert (loc ?agent ?area2)
           (finally (propagate-changes!)))
   ```
   For relaxed search, this should just be `(assert (loc ?agent ?area2))` since we only track base relations.

4. **Address `connectable` in connect actions.** The `connectable` query checks whether another connector in the same area has active beams impinging on it — this depends on `beam-segment` (a derived relation). Options:
   - Remove the check entirely (most relaxed — allows placing connectors in occupied beam areas)
   - Replace with a base-relation approximation

5. **Address `selectable` in connect actions.** `selectable` calls `accessible`, which we've already relaxed. But `selectable` also calls `observable`, which is already static-only. So this should be OK as-is.

6. **Remove `propagate-changes!` from pickup-connector and connect actions.** Same reasoning as move — in relaxed search, only base-relation updates matter. The `disconnect-connector!` update in pickup also clears `color` (derived), which may need adjustment.

7. **Address `holds` in pickup-connector precondition.** `(not (bind (holds ?agent $held)))` depends on a derived relation. Replace with base-relation equivalent: `(not (exists (?c cargo) (not (bind (loc ?c $any)))))` — i.e., no cargo is locationless (held).

8. **Relax the goal condition.** The current `define-goal` uses `(active receiver2)` and `(active receiver3)` — both derived. For relaxed search, the goal should be expressed in base relations, similar to `filter-constraints`.

9. **Post-validation.** After relaxed search finds candidate action sequences, replay them with full `propagate-changes!` to confirm the solution is physically valid. Design the validation mechanism.

10. **Lower bound function.** `min-steps-remaining?` currently references `(active ...)`, `(open gate1)`, and `(holds ...)` — all derived. These need base-relation equivalents for the relaxed variant.

## Key Design Decisions

- **Base relations:** `loc` and `paired` (as defined by `define-base-relation`)
- **Static relations:** `coords`, `los0/los1`, `visible0/visible1`, `accessible0/accessible1`, `chroma`, `controls`, geometry segments — all remain unchanged
- **Derived relations to eliminate from preconditions:** `holds`, `open`, `active`, `color`, `beam-segment`, `current-beams`, `connectable`
- **The relaxed search admits a superset of valid states** — it may allow moves through closed gates or placements in occupied beam areas, so post-validation is essential
- **Color matching is structural** — transmitter chroma determines what color flows through pairing chains, so no runtime `color` check is needed
- **Beam interference is domain-specific** — the area3→area2 direction for the receiver1 2-hop chain is the only viable one; this is hardcoded

## Reference: Known Goal State (base relations only)

```
LOC AGENT1 AREA4
LOC CONNECTOR1 AREA2
LOC CONNECTOR3 AREA3
(CONNECTOR2 has no loc — held)
PAIRED CONNECTOR1 RECEIVER3
PAIRED CONNECTOR1 RECEIVER1
PAIRED CONNECTOR1 TRANSMITTER2
PAIRED CONNECTOR3 CONNECTOR1
PAIRED CONNECTOR3 RECEIVER2
PAIRED CONNECTOR3 TRANSMITTER1
```

# Launch Configuration Checklist — T12 / G15

Written 2026-09-22 under the approval recorded for T12 in
`Constraint-Implementation-Plan.md`. This is a read-only construction
specification. It neither authorizes code nor makes a traversal, search
recommendation, or action budget valid.

It closes the specification work spawned by G15: a region-graph traversal is
not concrete until its launch configuration survives every intervening
controller transition. The plan remains authoritative for task state; this
document is authoritative for this check's required evidence and result labels.

**Next task.** T13 must select and obtain approval for a concrete application
before this checklist is used for any new source analysis. It does not reopen a
closed construction branch by itself.

**Contamination.** This specification used only G15 and its permitted
construction audit. No sealed material was opened, and no problem was staged,
searched, replayed, validated, or changed.

---

## 1. The question attached to each proposed traversal

For a proposed traversal, record:

1. **Actor and view.** Name the acting body and the environmental view that
   governs its movement, support, controller, barrier, and recorder facts.
2. **Launch and landing.** Name the concrete launch point and intended landing
   point, rather than only their quotient regions.
3. **Required configuration.** State every launch-height, support, carried
   cargo, reach, and barrier condition read by the proposed movement predicate.
4. **Intervening transition.** If an action changes a controller before the
   crossing, name that action and the barrier condition it is meant to change.

A quotient edge is only a prompt to ask this question. It is not an answer.

## 2. Discharge order

Evaluate a construction in this order. A later item must use the successor
state produced by the preceding item.

### 2.1 Launch legality

Show that the proposed movement predicate admits the actor at the concrete
launch point in the current state. Discharge its elevation and support
requirements directly. Region membership, a nearby raised point, and an open
barrier do not substitute for this evidence.

For remote pickup or manipulation, discharge horizontal and vertical reach
separately. A named location's horizontal relation to an object is not changed
merely by raising the actor at that same location.

### 2.2 Controller transition

Treat a controller change as its own state transition unless the action model
proves otherwise. Record the successor after all propagation: drops, launches,
support loss, device-state updates, and recorder effects are facts of that
successor, not optional side effects.

Never combine the pre-transition lift with the post-transition barrier state.
If propagation removes the required support, the crossing has no legal launch
until another stated transition restores one.

### 2.3 Crossing from the propagated successor

Evaluate the proposed crossing anew in the propagated successor. It must
independently satisfy:

- the landing-height and segment conditions;
- the retained launch support;
- the barrier condition in the actor's view; and
- any cargo, reach, or occupancy preconditions.

For recorder actors, each of these facts must be established in that actor's
environmental view. A physical-view controller fact cannot silently supply a
recording-view barrier or support condition.

### 2.4 Budget

Count an explicit controller transition as an action when the action model
does. Do not charge or waive a movement action from prose alone: segments may
compose only when the engine permits every segment in the same state. A budget
is conditional until the preceding launch, transition, and successor checks
are discharged.

## 3. Result labels

Record each proposed traversal with one of these labels:

| Label | Meaning |
|---|---|
| **CONCRETE** | Every item in §2 is discharged for the named actor and view. |
| **CONDITIONAL** | A named prerequisite is not yet established. State it beside the traversal and omit it from any definite budget. |
| **REJECTED CONSTRUCTION** | A stated step contradicts the movement or propagated-successor requirements. This rejects that construction only. |
| **OPEN QUESTION** | The necessary condition is not yet known or represented by the available analysis. |

Neither **CONDITIONAL**, **REJECTED CONSTRUCTION**, nor **OPEN QUESTION**
establishes reachability, unreachability, a mandatory resource count, or a
search bound. A new search, instance change, or extractor still needs its own
approval.

## 4. Portable report form

Use this form before assigning a concrete realization or action budget:

```text
Traversal:
Actor/view:
Launch -> landing:
Movement predicate and launch requirements:
Controller transition, if any:
Propagated successor:
Crossing requirements in that successor:
Reach/cargo requirements:
Action budget:
Result label and unresolved prerequisite:
```

The form deliberately asks for concrete locations and propagated states before
it asks for a cost. It therefore applies to any quotient traversal whose
regions abstract elevation, support, controller state, or environmental view.

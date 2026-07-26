# Guidelines for Inferring a Required Location to Solve a Talos Principle Planning Problem

A reusable analytical playbook for WouldWork (WW) planning specs that are otherwise complete and correct, but **unsolvable because one required location has been omitted** from the specification. The task is to infer where that location must sit and what relations it must carry so that WW can find a solution.

> **Status:** living document. Section 9 (Exemplars), Section 5 (Deadlock patterns), and Section 8 (Pitfalls) are expected to grow as more specs are analyzed.

---

## 1. Purpose & scope

Given a problem spec (e.g. `problem-claustro.lisp`) that is known to be solvable *except* for one missing location, derive:

1. **Where** the location belongs (which area, which sightline group, which reach/movement neighbors).
2. **What relations** it must carry, and **with what barriers/occluders**, so that it unblocks a solution without trivializing the puzzle.

**Initial focus is a missing *location*.** The same method generalizes to a missing *relation instance* of any kind (a reach edge, an interface obstacle list, a `los-via` entry). Treat the location as the common case; reach for the general framing when a location alone does not explain the gap.

**Assumptions.** The spec's queries, update logic, actions, types, and static environment are correct and current. Only one location (and its incident relations) is missing. Always analyze against the **current file on disk**, never a remembered version.

---

## 2. Core method: bidirectional reasoning

Work the problem from both ends and find where they fail to meet.

- **Backward from the goal.** Reduce the goal to the forced terminal action(s), then to *their* preconditions, recursively. Goal gates that no controller drives can only be opened one way (e.g. by a jammer), which often pins down the final action and the resource that must be consumed there.
- **Forward from the start.** Compute what the start state, plus its derived/coupled state, actually makes reachable — by movement, by reach, and by sight. Prune everything that merely *looks* reachable.

**The missing location advances a frontier.** It does one of:

- **(a) Bridges** the forward-reachable frontier to a backward-required precondition (the two ends meet at the new location), or
- **(b) Extends the forward frontier** — permits further forward progress from a point where the agent was stalled, or
- **(c) Enables a backward regression step** — supplies a missing precondition so a required action becomes applicable.

**Do not assume case (a).** A missing location frequently just unblocks one direction (b or c). In the claustro exemplar it reads most naturally as (b): the agent had already crossed into the target area but was stalled, and the new location is what lets forward progress resume.

---

## 3. Analytical milestones

A scaffold that has held up in practice. Each milestone is a checkpoint to confirm before proceeding.

1. **Goal reduction.** Reduce the goal to the forced terminal action(s) and the resource(s) they consume.
2. **Terminal-vantage analysis.** Enumerate every position/state from which the terminal action could fire. Extract the constraints on location, visibility, and reachability.
3. **Invariants & resource couplings.** Identify conserved or mutually-exclusive quantities (e.g. a single jammer; *holding* vs *jamming* exclusivity; a beam corridor that must stay unoccupied). These are the hard constraints that generate the deadlock.
4. **Forward-reachability pruning.** Compute true movement/reach/sight reachability from the start. Discard destinations that are plausible but unreachable. Do this **before** committing to any destination.
5. **Forced corridor.** When pruning leaves a single productive path, commit to it and trace the action sequence along it.
6. **Deadlock isolation.** Find the exact point where forward progress stalls or a backward precondition cannot be met. The omission lives *here*, not necessarily where the symptom first appears.
7. **Synthesis.** Derive the new location's relations, one per unmet need (Section 6).
8. **Validation.** Confirm the additions yield a concrete plan within the depth cutoff, keep geometry honest, and do not trivialize (Section 7).

---

## 4. WW relation reference (capability lens)

Read each relation as a *capability* and an associated *question to ask*. Note especially that **movement, reach, and sight cross barriers under different conditions** — conflating them is a common error.

| Relation | Capability | Question to ask | Barrier semantics |
|---|---|---|---|
| `in-area` | Walk-adjacency clique | Can the agent *stand here / walk freely here*? | Intra-area moves are free (no obstacles). |
| `interface` | Gated movement between areas | Can the agent *walk across* this boundary now? | Every obstacle must be passable (open gate; screen/ladder block a *carrying* agent). |
| `traversable>` | One-way movement edge | Can the agent move *in this direction only*? | Obstacle list, same passability rules; not symmetric. |
| `reachable-via` | Put/pickup across a gap *without walking* | Can cargo *cross here while the agent stays put*? | Barriers must be **open gates**; symmetric; non-gate barriers block. |
| `los-via` | Sightline with occluder list | Is the *target visible* from this group? | Every occluder must be transparent (open gate); empty list = always clear. |
| `in-los-group` | Sightline-equivalence clique | Which targets does standing here let me *see*? | — |
| `beam-via` | Beam corridor | Does the beam *reach its receiver*? | Corridor gates open **and** corridor locations unoccupied. |
| `controls` + `mode` | Derived gate state from a controller | Is this gate *driven open/closed* right now? | `normal` = open when energized; `inverted` = open when not; jamming overrides. |

**Key asymmetry to internalize:** gates block by being *closed*; screens and ladders block specifically a *cargo-carrying* agent. So a path that is walkable empty-handed may be impassable while carrying a resource — and reach may succeed where movement fails, or vice versa.

---

## 5. Deadlock patterns (catalog — grows)

Named couplings that produce "almost solvable" specs. Recognizing the pattern shortcuts the deadlock-isolation milestone.

- **"The opener is the payload."** The single scarce resource that *unlocks* the path is the same one that must be *consumed at the destination*. It cannot be in both places. *(claustro: the jammer opens gate1 to activate the receiver, yet must end on gate5.)*
- **"Carrying closes the path it opened."** Moving the resource flips a derived state that was holding the path open, re-sealing it behind the agent. Expect a **stranded-resource sub-problem**: the resource sits on one side, the agent on the other, with no legal way to reunite them. *(claustro: lifting the jammer off gate1 de-activates the receiver and shuts gate2/gate3.)*
- **"Self-defeating vantage."** Occupying the spot needed for the terminal action violates a constraint that the action depends on. *(claustro: standing/placing on location2 breaks the beam corridor that must stay unoccupied.)*

---

## 6. Synthesis checklist

For the new location, map each **unmet need** to exactly one relation, then choose barriers/occluders deliberately.

- *"The agent must stand here / walk here"* → `in-area` (which area?) — and possibly `interface` / `traversable>` to connect it.
- *"The target must be visible from here"* → `in-los-group` + the group's `los-via` entry (which occluders?).
- *"Cargo must cross a gap from here without the agent walking"* → `reachable-via` (which barrier gates?).

Then pick **barriers/occluders** to satisfy two competing constraints simultaneously:

1. The edge/sightline is **usable exactly when intended** (in the state the forced corridor reaches it).
2. The edge/sightline is **not usable when it would trivialize** the puzzle (typically: closed in the start state, so the agent is compelled to do the real work first).

The barrier choice is frequently the keystone of the whole inference, not a stylistic afterthought.

---

## 7. Validation checklist

- **Anti-trivialization.** Does the addition admit a short-circuit solve (e.g. a 2-step jam straight from the start)? If so, the missing barrier/occluder is the fix — add it and re-check.
- **Geometric honesty.** A vantage's sightline must agree with which side of a wall it sits on. A location reachable only through a gated wall should *see past* that wall only when the wall's gates are open. Reject any addition where sight, reach, and movement tell inconsistent stories about position.
- **Concrete plan.** Produce an explicit action sequence end to end, and confirm its length is within `*depth-cutoff*`.
- **Consistency on propagation.** Confirm the terminal action's `propagate-changes!` settles without `inconsistent-state` and without disturbing unrelated derived facts.

---

## 8. Pitfalls & anti-patterns (grows)

- **Assuming a bridge.** Treating the missing location as case 2(a) when it only extends one frontier (2(b)/2(c)). Check which it is.
- **Committing to a destination before pruning.** A destination can look viable on sight/reach grounds yet be movement-unreachable. Prune accessibility first. *(claustro: area3 / location8 sees gate5 via the open gate4, but area3 is unreachable by movement, so it is a false lead.)*
- **Sightline that contradicts the wall.** Granting a location a clear view of a target that, by its position, should be occluded by the intervening wall's gates.
- **Unbarred reach edge through a wall.** A `reachable-via` with no barriers can punch a hole straight through a gated wall and collapse the puzzle to two actions.
- **Conflating reach with movement (or sight).** They cross barriers under different rules; a fix valid for one is often invalid for another.
- **Fixing the symptom site.** The deadlock surfaces downstream of the omission; place the new relations at the deadlock's *cause*.

---

## 9. Worked exemplars (grows)

### Exemplar template

> *Goal reduction → Terminal vantage → Key invariant(s) → Forward-reachability prune → Forced corridor → Deadlock → Synthesized relations → Validation & plan → Pitfalls encountered.* (Fields will evolve.)

### Exemplar 1 — `problem-claustro.lisp`, missing `location3`

**Goal reduction.** Goal `(open gate5)`. No `controls … gate5` entry exists, so `update-gate-status!` can open gate5 only via jamming. There is one jammer. ⇒ the terminal action is `jam-gate … gate5 …`, and jammer1 must terminate on gate5.

**Terminal vantage.** `jam-gate` requires the agent *holding* the jammer, with a placement `?location` reachable from the agent and `(visible ?location gate5)`. So a vantage with a sightline to gate5 is required.

**Key invariants.** (i) Single jammer. (ii) *Holding* and *jamming* are mutually exclusive states of that one jammer. (iii) Receiver1 is active iff the beam corridor `(gate1 location2)` is clear — gate1 open **and** location2 unoccupied. (iv) gate1 is uncontrolled, so only jamming opens it. (v) gate2/gate3 are `normal`-controlled by receiver1; gate4 is `inverted`.

**Forward-reachability prune.** Seeing gate5 from location1 or location2 needs gate2 **and** gate3 open ⇒ receiver active ⇒ gate1 jammed ⇒ jammer **not** held — contradicting the `holding` precondition. So gate5 cannot be jammed from area1. Carrying the jammer flips the receiver off, which closes gate2/gate3 and (with screen1/ladder1 blocking a carrier) confines the agent to its current area. area3 is unreachable by movement under any single consistent gate state, so the area3 vantage (location8, which sees gate5 via the open gate4) is a **false lead**. ⇒ **area2 is the only productive destination**, where los-group2 sees gate5 directly (`los-via los-group2 gate5 ()`).

**Forced corridor.** Pick up jammer1 at location1 → jam gate1 at location1 (receiver activates; gate2/gate3 open; gate4 closes) → move into area2.

**Deadlock.** The agent is now in area2 but the jammer sits at location1 holding gate1 open. It cannot be carried across (carrying shuts gate2/gate3 behind the agent) and is not reach-reachable from area2 (no reach edge crosses the gate2/gate3 wall). The jammer is **stranded** relative to the agent — the *"opener is the payload"* + *"carrying closes the path it opened"* patterns combined.

**Synthesized relations for `location3`.** A walkable area2 spot, sight-equivalent to its neighbors, with a reach opening back to location1 that is gated by the very wall it crosses:

```lisp
(in-area area2 (location3 location4 location5 location6))
(in-los-group los-group2 (location3 location4 location5 location6))
(reachable-via location1 location3 (gate2 gate3))
```

The `(gate2 gate3)` barrier is the keystone: it makes the reach opening usable **only while those gates are open** (i.e. only after gate1 has been jammed and the receiver activated), which is what forces the agent through the real sequence instead of a two-step shortcut.

**Validation & plan (5 actions, ≤ depth 20).**

1. `pickup-cargo agent1 jammer1` (at location1)
2. `jam-gate agent1 gate1 location1` — receiver active; gate2/gate3 open
3. `move agent1 location3` — crosses the now-open interface, empty-handed
4. `pickup-cargo agent1 jammer1` — reaches back through the open gates to lift the jammer off gate1; receiver off, gate2/gate3 shut (harmless — agent already across)
5. `jam-gate agent1 gate5 location3` — los-group2 sees gate5 directly ⇒ jam succeeds ⇒ `(open gate5)`

Anti-trivialization holds: at the start, gate2/gate3 are closed, so `reachable location3 location1` is false; the agent cannot place the jammer onto location3 from location1 until it has first jammed gate1.

**Pitfalls encountered.**

- *Sightline-vs-wall:* an early attempt put location3 in los-group2 but reach-accessible *unbarred* from location1 — a view/reach punched through the gate2/gate3 wall. Resolved by barring the reach edge with `(gate2 gate3)`.
- *False destination:* area3 (location8 sees gate5 via the open gate4) looked viable on sight grounds but is movement-unreachable; rejected only after accessibility pruning.
- *Bridge assumption:* location3 is not a pure forward/backward bridge — it is a **forward-progress enabler** (case 2(b)) that lets the stalled agent in area2 acquire the stranded jammer.

---

## Appendix — quick procedure

1. Reduce the goal to its forced terminal action and consumed resource.
2. Characterize every legal terminal vantage; note the sight/reach/stand constraints.
3. List invariants and resource couplings.
4. Prune forward reachability (movement, then reach, then sight); kill false destinations.
5. Commit to the forced corridor; trace actions until progress stalls.
6. Isolate the deadlock; name its pattern.
7. Synthesize the new location's relations — one per unmet need — and choose barriers/occluders to be usable-when-intended and inert-when-trivializing.
8. Validate: concrete plan within the cutoff, honest geometry, no short-circuit, clean propagation.

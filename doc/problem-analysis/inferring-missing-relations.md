# Guidelines for Inferring a Missing Relation in a Talos Planning Problem

A reusable analytical playbook for Wouldwork specs that are otherwise complete and correct, but **unsolvable because one required relation instance has been omitted**. The task is to infer what is missing, where it belongs, and what barriers or occluders it must carry so that the planner can find a solution without the addition trivializing the puzzle.

**Where this fits.** This method assumes you have already built and verified a working reference for the spec — see `working-reference-builder.md`. Section 6 of that reference (world-mode enumeration) is what the forward-reachability pruning below reads from. For writing a spec in the first place, see `wouldwork-problem-template.md`; for relation signatures, `tech/Talos Technology  Relations.txt` is authoritative.

> **Status:** living document. Section 4 was rewritten against the current `tech/` relation vocabulary; the previous version described a representation (`in-area`, `interface`, `los-via`, `in-los-group`, `reachable-via`) that no longer exists. Section 5 (deadlock patterns) and Section 8 (pitfalls) are expected to grow. Appendix B is a historical exemplar, retained for its reasoning but written in the superseded vocabulary.

---

## 1. Purpose & scope

Given a problem spec that is known to be solvable *except* for one missing relation instance, derive:

1. **Where** it belongs — which locations, which sightline, which movement or reach edge.
2. **What barriers or occluders** it must carry, so that it unblocks a solution without trivializing the puzzle.

**A missing location is the common case**, and the most tractable to reason about: a location that should exist as a standing spot, sight vantage, or reach endpoint, together with all its incident relations. But the same method applies to any single missing relation instance — a `walk-via` edge, a `reach-via` opening, a `los-to-target` entry.

**Assumptions.** The spec's queries, update logic, actions, types, and static environment are correct and current. Only one relation instance (and, if it is a location, its incident relations) is missing. Always analyze against the **current file on disk**, never a remembered version.

**Before starting, determine whether the relevant facts are authored or derived.** See Section 4's closing note. If the problem asserts `wall-segment>` facts, its `walk-via` and `los-to-*` facts are computed from geometry at initialization, and hand-adding one is the wrong fix.

---

## 2. Core method: bidirectional reasoning

Work the problem from both ends and find where they fail to meet.

- **Backward from the goal.** Reduce the goal to the forced terminal action(s), then to *their* preconditions, recursively. Goal gates that no controller drives can only be opened one way (e.g. by a jammer), which often pins down the final action and the resource that must be consumed there.
- **Forward from the start.** Compute what the start state, plus its derived/coupled state, actually makes reachable — by movement, by reach, and by sight. Prune everything that merely *looks* reachable.

**The missing relation advances a frontier.** It does one of:

- **(a) Bridges** the forward-reachable frontier to a backward-required precondition (the two ends meet there), or
- **(b) Extends the forward frontier** — permits further forward progress from a point where the agent was stalled, or
- **(c) Enables a backward regression step** — supplies a missing precondition so a required action becomes applicable.

**Do not assume case (a).** A missing relation frequently just unblocks one direction (b or c).

---

## 3. Analytical milestones

A scaffold that has held up in practice. Each milestone is a checkpoint to confirm before proceeding.

1. **Goal reduction.** Reduce the goal to the forced terminal action(s) and the resource(s) they consume.
2. **Terminal-vantage analysis.** Enumerate every position/state from which the terminal action could fire. Extract the constraints on location, visibility, and reachability.
3. **Invariants & resource couplings.** Identify conserved or mutually-exclusive quantities (e.g. a single jammer; *holding* vs *jamming* exclusivity; a beam corridor that must stay unoccupied). These are the hard constraints that generate the deadlock.
4. **Forward-reachability pruning.** Compute true movement/reach/sight reachability from the start. Discard destinations that are plausible but unreachable. Do this **before** committing to any destination.
5. **Forced corridor.** When pruning leaves a single productive path, commit to it and trace the action sequence along it.
6. **Deadlock isolation.** Find the exact point where forward progress stalls or a backward precondition cannot be met. The omission lives *here*, not necessarily where the symptom first appears.
7. **Synthesis.** Derive the missing relation(s), one per unmet need (Section 6).
8. **Validation.** Confirm the additions yield a concrete plan within the depth cutoff, keep geometry honest, and do not trivialize (Section 7).

---

## 4. Relation reference (capability lens)

`tech/Talos Technology  Relations.txt` is the authoritative inventory of relations and is kept current; consult it rather than this table for signatures. What follows reads the movement/sight/reach relations as *capabilities*, with the question each answers and how barriers behave — which is what the inference method actually turns on.

| Relation | Capability | Question to ask | Barrier semantics |
|---|---|---|---|
| `walk-via location $list location` | Symmetric walking edge | Can the agent *walk between these two spots* now? | `$list` is a DNF clause list — see below. Agent-dependent. |
| `walk-via> location $list location` | Directional walking edge | Can the agent walk *this way only*? | Same convention. Emitted for rides into an air stream's destination. |
| `jump-via` / `jump-via>` | Jumping edge (symmetric / directional) | Can the agent *jump* this gap? | Same clause convention. |
| `climb-via> location $list location` | One-way climb (ladders) | Can the agent climb here? | Same clause convention; not part of walkability. |
| `reach-via location $list location` | Put/pickup across a gap *without walking* | Can cargo *cross while the agent stays put*? | `$list` is a **flat conjunction of barrier gates**, all of which must be open. Symmetric, agent-independent. |
| `los-to-location location $list location` | Sightline between two locations | Can I *see that spot* from here? | `$list` = occluders; `()` is a direct, always-clear line; clear iff every occluder gate is open. |
| `los-to-target location $list gate` | Sightline to a jam target | Can I *jam that gate* from here? | As above. Gate targets only — a gears target resolves through its `has-position` location's `los-to-location` entry instead. |
| `los-to-apparatus location $list apparatus` | Sightline to a beam endpoint | Can a connector here *pair* with that transmitter/receiver? | As above. |
| `beam-via transmitter $list receiver` | Beam corridor | Does the beam *reach its receiver*? | Corridor gates open **and** corridor locations unoccupied. |
| `controls $list <barrier> $mode` | Derived barrier state from controllers | Is this gate *driven open/closed* right now? | `$list` is a DNF clause list of controllers. `normal` = open when energized; `inverted` = open when not; jamming overrides. |
| `jam-disallowed> location location target` | Explicit jam prohibition | Is jamming ruled out from here? | — |

### The DNF clause convention

`walk-via`, `walk-via>`, `jump-via`, `climb-via>`, and `controls` all take a **disjunctive-normal-form clause list**: `()` means direct and unguarded; a nonempty value is **OR over clauses, AND within a clause**. So `((gate1) (gate2 gate3))` means *gate1 open, or else both gate2 and gate3 open*. A clause is a set of simultaneous conditions; multiple clauses are alternative routes.

Clause items on a traversal edge may be **gates, screens, ladders, or gears**:

- A **gate** passes when open.
- A **screen** or **ladder** passes only when the agent is **empty-handed** (`obstacle-clear` in `tech/-passability.lisp`).
- A **gears** item is an air-stream crossing, passable unless a blowing fan is mounted.

### Three asymmetries to internalize

1. **Walking is agent-dependent; reach and sight are not.** A path walkable empty-handed may be impassable while carrying a resource. `reachable` and `visible` take no agent argument at all.
2. **Reach clears only through open gates.** `reachable-clear` (`tech/reachability.lisp`) admits a barrier only if it is a gate and open — a screen or ladder on a reach edge blocks *outright*, with no empty-handed exemption. This differs from the same obstacle on a walk edge.
3. **Walk edges are disjunctive; reach edges are conjunctive.** A `walk-via` list offers alternative routes; a `reach-via` list is a flat set of gates that must *all* be open. Writing a reach barrier as if it were DNF is a silent modeling error.

### Authored or derived?

Movement and sightline facts come from one of two places, and this decides how a gap may legitimately be fixed.

- **Hand-authored.** The problem asserts `walk-via` and `los-to-*` facts directly. A missing edge is fixed by adding the fact.
- **Derived from geometry.** If the problem asserts `wall-segment>` facts (with `gate-segment>`, `window-segment>`, `screen-segment>`, `boundary-wall`), then `-walkability-coordinates` derives `walk-via`/`walk-via>` and `-beam-los-coordinates` derives the `los-to-*` tables at initialization, from raw 2D segment geometry. **Hand-adding a fact in this case is the wrong fix** — the derivation owns those relations, and an added fact either conflicts with what the derivation produces or is silently overwritten. Fix the geometry instead: a missing sightline means a segment is wrong, or a location's `location-coords>` is wrong.

Check for `wall-segment>` in the spec before synthesizing anything in Section 6.

---

## 5. Deadlock patterns (catalog — grows)

Named couplings that produce "almost solvable" specs. Recognizing the pattern shortcuts the deadlock-isolation milestone. These are structural observations, independent of the relation vocabulary in use.

- **"The opener is the payload."** The single scarce resource that *unlocks* the path is the same one that must be *consumed at the destination*. It cannot be in both places.
- **"Carrying closes the path it opened."** Moving the resource flips a derived state that was holding the path open, re-sealing it behind the agent. Expect a **stranded-resource sub-problem**: the resource sits on one side, the agent on the other, with no legal way to reunite them.
- **"Self-defeating vantage."** Occupying the spot needed for the terminal action violates a constraint that the action depends on — for instance, standing in a beam corridor that must stay unoccupied.

Appendix B works all three at once.

---

## 6. Synthesis checklist

First confirm the facts are hand-authored, not derived (Section 4). Then map each **unmet need** to exactly one relation, and choose its barriers deliberately.

- *"The agent must be able to walk here"* → `walk-via` entries joining the location to its neighbors — or `walk-via>` if the passage is one-way. Choose the DNF clause list.
- *"The agent must be able to jump or climb here"* → `jump-via` / `jump-via>` / `climb-via>`, same clause convention.
- *"The target must be visible from here"* → the appropriate `los-to-*` relation for the consuming role: `los-to-target` for a jammer's gate target, `los-to-apparatus` for beam pairing, `los-to-location` for everything else. Choose the occluder list.
- *"Cargo must cross a gap from here without the agent walking"* → `reach-via`, with a flat list of barrier gates.

Then pick **barriers/occluders** to satisfy two competing constraints simultaneously:

1. The edge or sightline is **usable exactly when intended** — in the state the forced corridor reaches it.
2. The edge or sightline is **not usable when it would trivialize** the puzzle — typically closed in the start state, so the agent is compelled to do the real work first.

The barrier choice is frequently the keystone of the whole inference, not a stylistic afterthought. Note that DNF gives a second lever beyond *which* barriers: a second clause creates an alternative route that opens under different conditions, which can be exactly right or a hole straight through the puzzle.

---

## 7. Validation checklist

- **Anti-trivialization.** Does the addition admit a short-circuit solve (e.g. a two-step jam straight from the start)? If so, the missing barrier or occluder is the fix — add it and re-check.
- **Geometric honesty.** A vantage's sightline must agree with which side of a wall it sits on. A location reachable only through a gated wall should *see past* that wall only when the wall's gates are open. Reject any addition where sight, reach, and movement tell inconsistent stories about position. If the spec is coordinate-derived, this check is automatic — which is a reason to prefer geometry over hand-authored facts.
- **Concrete plan.** Produce an explicit action sequence end to end, and confirm its length is within `*depth-cutoff*`.
- **Consistency on propagation.** Confirm the terminal action's `propagate-changes!` settles without `inconsistent-state` and without disturbing unrelated derived facts.

---

## 8. Pitfalls & anti-patterns (grows)

- **Hand-adding a derived fact.** The spec asserts `wall-segment>`, so `walk-via` and the `los-to-*` tables are computed at init. Adding one by hand fixes nothing. Fix the geometry.
- **Assuming a bridge.** Treating the missing relation as case 2(a) when it only extends one frontier (2(b)/2(c)). Check which it is.
- **Committing to a destination before pruning.** A destination can look viable on sight or reach grounds yet be movement-unreachable. Prune walkability first.
- **Sightline that contradicts the wall.** Granting a location a clear view of a target that, by its position, should be occluded by the intervening wall's gates.
- **Unbarred reach edge through a wall.** A `reach-via` with an empty barrier list punches a hole straight through a gated wall and can collapse the puzzle to two actions.
- **Treating a reach list as DNF.** `reach-via`'s list is a flat conjunction — every gate must be open. Writing it as alternative clauses does not mean what it looks like.
- **Forgetting that screens and ladders block reach absolutely.** They have an empty-handed exemption on walk edges only.
- **Conflating reach with movement (or sight).** They cross barriers under different rules; a fix valid for one is often invalid for another.
- **Fixing the symptom site.** The deadlock surfaces downstream of the omission; place the new relations at the deadlock's *cause*.
- **Wrong `los-to-*` for the role.** Sightlines are split by consuming role, not object kind. A jammer aiming at a gate needs `los-to-target`; beam pairing needs `los-to-apparatus`; a gears jam target needs `los-to-location` on its `has-position` location.

---

## Appendix A — quick procedure

1. Determine whether movement/sightline facts are authored or derived from `wall-segment>`.
2. Reduce the goal to its forced terminal action and consumed resource.
3. Characterize every legal terminal vantage; note the sight/reach/stand constraints.
4. List invariants and resource couplings.
5. Prune forward reachability (movement, then reach, then sight); kill false destinations.
6. Commit to the forced corridor; trace actions until progress stalls.
7. Isolate the deadlock; name its pattern.
8. Synthesize the missing relation(s) — one per unmet need — choosing barriers to be usable-when-intended and inert-when-trivializing.
9. Validate: concrete plan within the cutoff, honest geometry, no short-circuit, clean propagation.

---

## Appendix B — historical exemplar: `problem-claustro.lisp`, missing `location3`

> **Superseded vocabulary.** This analysis was written against a representation that no longer exists — `in-area`, `in-los-group`, `los-via`, `reachable-via` — and against `problem-claustro.lisp`, which has since been deleted (only `problem-claustro-topo.lisp` remains). It is retained because the *reasoning* is a clean worked instance of Sections 2–7, and because it demonstrates all three deadlock patterns interacting. Do not copy its relation names.

**Goal reduction.** Goal `(open gate5)`. No `controls … gate5` entry exists, so `update-gate-status!` can open gate5 only via jamming. There is one jammer. ⇒ the terminal action is `jam-gate … gate5 …`, and jammer1 must terminate on gate5.

**Terminal vantage.** `jam-gate` requires the agent *holding* the jammer, with a placement `?location` reachable from the agent and `(visible ?location gate5)`. So a vantage with a sightline to gate5 is required.

**Key invariants.** (i) Single jammer. (ii) *Holding* and *jamming* are mutually exclusive states of that one jammer. (iii) Receiver1 is active iff the beam corridor `(gate1 location2)` is clear — gate1 open **and** location2 unoccupied. (iv) gate1 is uncontrolled, so only jamming opens it. (v) gate2/gate3 are `normal`-controlled by receiver1; gate4 is `inverted`.

**Forward-reachability prune.** Seeing gate5 from location1 or location2 needs gate2 **and** gate3 open ⇒ receiver active ⇒ gate1 jammed ⇒ jammer **not** held — contradicting the `holding` precondition. So gate5 cannot be jammed from area1. Carrying the jammer flips the receiver off, which closes gate2/gate3 and (with screen1/ladder1 blocking a carrier) confines the agent to its current area. area3 is unreachable by movement under any single consistent gate state, so the area3 vantage (location8, which sees gate5 via the open gate4) is a **false lead**. ⇒ **area2 is the only productive destination**, where los-group2 sees gate5 directly.

**Forced corridor.** Pick up jammer1 at location1 → jam gate1 at location1 (receiver activates; gate2/gate3 open; gate4 closes) → move into area2.

**Deadlock.** The agent is now in area2 but the jammer sits at location1 holding gate1 open. It cannot be carried across (carrying shuts gate2/gate3 behind the agent) and is not reach-reachable from area2 (no reach edge crosses the gate2/gate3 wall). The jammer is **stranded** relative to the agent — the *"opener is the payload"* and *"carrying closes the path it opened"* patterns combined.

**Synthesized relations.** A walkable area2 spot, sight-equivalent to its neighbors, with a reach opening back to location1 barred by the very wall it crosses. The barrier `(gate2 gate3)` is the keystone: it makes the reach opening usable **only while those gates are open** — i.e. only after gate1 has been jammed and the receiver activated — which forces the agent through the real sequence instead of a two-step shortcut.

**Validation & plan (5 actions, ≤ depth 20).**

1. `pickup-cargo agent1 jammer1` (at location1)
2. `jam-gate agent1 gate1 location1` — receiver active; gate2/gate3 open
3. `move agent1 location3` — crosses the now-open interface, empty-handed
4. `pickup-cargo agent1 jammer1` — reaches back through the open gates to lift the jammer off gate1; receiver off, gate2/gate3 shut (harmless — agent already across)
5. `jam-gate agent1 gate5 location3` — sightline to gate5 is direct ⇒ jam succeeds ⇒ `(open gate5)`

Anti-trivialization holds: at the start, gate2/gate3 are closed, so location3 is not reach-reachable from location1; the agent cannot place the jammer onto location3 until it has first jammed gate1.

**Pitfalls encountered.**

- *Sightline-vs-wall:* an early attempt gave location3 the right sightline but an *unbarred* reach edge from location1 — a hole punched through the gate2/gate3 wall. Resolved by barring the reach edge.
- *False destination:* area3 looked viable on sight grounds but is movement-unreachable; rejected only after walkability pruning.
- *Bridge assumption:* location3 is not a pure forward/backward bridge — it is a **forward-progress enabler** (case 2(b)) that lets the stalled agent in area2 acquire the stranded jammer.

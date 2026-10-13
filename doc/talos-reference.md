# Talos reference for specification work

Companion to the [specification advisor](spec-advisor.md) for Talos problems built on the
`tech/` technology library. Part 1 explains how the movement, reach, and sight relations
work, and how beam relay and crossing operations behave. Part 2 builds a readable working
reference of what a specification actually models, so the user can compare it with what
they intend. Part 3 investigates a discrepancy between intent and model, including a
suspected missing relation or location.

**Where this fits.** The spec advisor introduces Part 2 during drafting and after staging,
and Part 3 when comparison with user intent reveals a discrepancy. The general rules for
comparing intent with the model (soundness, completeness, consistency, clarity; allowed and
forbidden cases; keeping intended and implemented behavior distinct) are in
[spec-advisor.md](spec-advisor.md) Section 2.6 and apply throughout; this document adds
the Talos-specific content. For other problems, use Section 2.6 with the problem's own
rules and entities, without importing Talos mechanics.

Record findings in `doc/problems/<problem-name>/spec-analysis.txt`, even for a short
review. If a detailed working reference or investigation needs its own artifact, use a
`spec-analysis-` file in that directory (for example `spec-analysis-working-reference.txt`)
and link it from `spec-analysis.txt`. Return findings, evidence, scope, unresolved issues,
and affected dependencies to the spec advisor. Maintain one agenda, reuse unaffected
current evidence, and reopen affected conclusions after a correction. Neither part
authorizes editing the spec or launching a search.

**Authoritative sources.** `tech/Talos Technology  Summary.txt` for relation signatures;
`tech/README.html` for the library; [loading and initialization](load-ordering/loading-and-initialization.md)
for what the engine does at load time.

> **Status:** Combined 2026-10-12 from the former `working-reference-builder.md` and
> `inferring-missing-relations.md`. Parts 1–3 use the current `tech/` relation vocabulary;
> the earlier area/interface/LOS-group representation (`in-area`, `interface`,
> `traversable>`, `in-los-group`, `reachable-via`) no longer exists, and the name `los-via`
> now denotes one unified symmetric endpoint relation. The deadlock patterns (3.5) and
> pitfalls (3.8) are expected to grow. The appendix is a historical exemplar in the
> superseded vocabulary.

---

## Part 1: Talos relations

### 1.1 Authored or derived?

Settle this first: it decides what a working reference can transcribe and how a gap may
legitimately be fixed.

**Does the spec assert any `wall-segment>`, `edge-segment>`, or `boundary-wall` facts?**

- **Yes — coordinate-derived.** The movement and sightline tables are computed at
  initialization from 2D segment geometry: `-walkability-coordinates` derives the `walking`
  facts in `traversal-via` / `traversal-via>`, and `-beam-los-coordinates` derives `los-via`
  plus the finite-barrier `los-barrier-crossings>` records. These facts are not in the file
  and cannot be transcribed from it. The geometry inputs are the authoritative source:
  `location-coords>`, `apparatus-coords>`, `wall-segment>`, `edge-segment>`,
  `gate-segment>`, `window-segment>`, `screen-segment>`, `boundary-wall`, and the relevant
  elevations and heights. Take the derived edges and crossing records from the load
  printout, marked as derived output. **Hand-adding a derived fact is the wrong fix**: the
  derivation owns those relations, and an added fact either conflicts with what it produces
  or is silently overwritten. The absence may be correct. Inspect the geometry inputs,
  included derivation rules, exclusions, and loading order; propose an additional
  representative location where justified, or correct a confirmed transcription error. Do
  not alter given geometry merely to create a sightline.
- **No — hand-authored.** The spec states its edges directly; a missing edge is fixed by
  adding the fact.

Legacy specs may be mixed: `problem-corner.lisp` asserts segment lists *and* hand-authored
sightline relations in its own older vocabulary (`los0`/`los1`, `visible0`/`visible1`,
`accessible0`/`accessible1`). Read whatever that file actually declares; do not translate it
into `tech/` names.

**If a derived table comes back empty or short**, suspect a load-ordering fault before the
geometry. Two failure modes are silent: an init-action skipped because one of its parameter
types has no instances, and a type declared *below* the `include-tech` block, which
collapses every `doall` over it into a no-op. See
[loading and initialization](load-ordering/loading-and-initialization.md), Traps 3 and 4.

### 1.2 Relation table

The table reads the movement, reach, and sight relations as capabilities: the question each
answers and how barriers behave. Consult the technology summary for signatures.

| Relation | Capability | Question to ask | Barrier semantics |
|---|---|---|---|
| `traversal-via walking location $list location` | Symmetric walking edge | Can the agent *walk between these two spots* now? | `$list` is DNF (1.3). The walking predicate also requires equal endpoint elevations and agent-dependent passability. |
| `traversal-via> walking location $list location` | Directional walking edge | Can the agent walk *this way only*? | Same convention. Emitted for rides into an air stream's destination. |
| `traversal-via` / `traversal-via>` (`stairway`) | Stairs edge (symmetric or directional) | Can grounded mobility use these stairs? | `$list` is DNF; elevation difference is unrestricted. |
| `traversal-via` / `traversal-via>` (`jumping`) | Jumping edge (symmetric or directional) | Can grounded mobility *jump* this gap? | `$list` is DNF; feasibility uses the source and destination floor elevations. |
| `traversal-via> climbing location $list location` | One-way climb (ladders) | Can the agent climb here? | `$list` is DNF, and each usable clause must contain a ladder positioned at the source. |
| `reach-via location $list location` | Put/pickup across a gap *without walking* | Can cargo *cross while the agent stays put*? | `$list` is a **flat conjunction of barrier gates**, all of which must be open (1.5). Symmetric, agent-independent. |
| `los-via visibility-object $list visibility-object` | Symmetric structural sightline | Is there a possible straight sightline from this location to that location, gate, apparatus, or gun? | Authored facts start from a location and carry gate/location occluders. Coordinate derivation also retains separate finite wall/edge/gate/boundary crossings (1.6). |
| `los-barrier-crossings> los-endpoint $list visibility-object` | Oriented finite-barrier geometry | Which exact segment crossings does a coordinate-derived sightline encounter? | Derived, not authored. Beam and elevated-jammer policies interpolate elevation at each crossing; equality with a barrier's base or top still blocks. |
| `beam-via fixed-beam-source $list fixed-beam-sink` | Fixed beam corridor | Does the fixed beam *reach its sink*? | Authored locations block only when an occupant spans the beam height. Coordinate-known walls, edges, boundaries, and closed gates block only across their finite vertical span. |
| `controls $list <barrier> $mode` | Derived barrier state from controllers | Is this gate *driven open/closed* right now? | `$list` is a DNF clause list of controllers. `normal` = open when energized; `inverted` = open when not; jamming overrides. |
| `jam-disallowed> location location target` | Explicit jam prohibition | Is jamming ruled out from here? | — |

### 1.3 The DNF clause convention

`traversal-via`, `traversal-via>`, and `controls` take a **disjunctive-normal-form clause
list**: `()` means direct and unguarded; a nonempty value is **OR over clauses, AND within a
clause**. `((gate1) (gate2 gate3))` means *gate1 open, or else both gate2 and gate3 open*. A
clause is a set of simultaneous conditions; multiple clauses are alternative routes.
Collapsing alternatives into one flat list changes the meaning. Every traversal mode uses
this shape; `reach-via` remains a flat conjunction.

Allowed clause items depend on the mode, and `traversal-init-check` rejects anything outside
that mode's registered vocabulary:

- `walking`: gates, screens, ladders, and gears.
- `stairway`: gates, screens, ladders, gears, and fixed blowers.
- `jumping`: gates, screens, and walls, interpreted as path features to pass or clear.
- `climbing`: gates, screens, and ladders; every usable clause must include a ladder
  positioned at the source.

### 1.4 Passability and mobility modes

Mobility is composed from the installed traversal modes (`walking`, `stairway`, `jumping`,
`climbing`) by `mobility-results`, `mobility-locations`, and `traversable`; passability comes
from `obstacle-clear` / `all-clear` in `tech/-passability.lisp`.

- A **gate** passes when open; a **screen** or **ladder** passes only when the agent is
  empty-handed; a **stream device** (gears or fixed blower) follows its current blowing
  state; a jump's gates, screens, and walls are path features to pass or clear.
- Jump feasibility uses the hypothetical source location's floor elevation, the destination
  floor elevation, and the highest non-passable feature top. Every usable climbing clause
  must contain a ladder positioned exactly at the segment source; the selected ladder
  appears first in the route witness. Support-changing jumps and steps are explicit
  one-transition boundaries rather than mobility segments, but the central action presents
  both kinds of result as `move`.
- **Air streams** are derived, not authored: each wall drive's band runs from the solid
  backstop behind its blower, through its `has-position` swept location, to its `aimed-at`
  destination, 3 units wide unless `stream-width` overrides. A drive is either mountable
  `wall-gears` with a removable fan or a fixed `wall-blower`. The swept location is
  standable exactly while the stream is off.

### 1.5 Reach

From symmetric `reach-via`, directional `reach-via>` (reacher first), and `reachable-clear`
(`tech/reachability.lisp`). Reach differs from movement at several points:

- The barrier list is a **flat conjunction**: every gate in it must be open, with no
  alternative clauses. Writing it as DNF does not mean what it looks like.
- `reach-via` is symmetric, `reach-via>` directional, and reach is **not transitive**.
- Reach is **agent-independent**, so carrying does not matter. `reachable` is trivially true
  for identical endpoints.
- `reachable-clear` admits a barrier only if it is a gate and open. A closed gate blocks, and
  **non-gate barriers are rejected during initialization**: screens and ladders cannot be
  reach-edge barriers, and mobility's empty-handed exemption does not apply.

### 1.6 Sight

From the symmetric `los-via` relation, its coordinate-derived `los-barrier-crossings>`
records, and the visibility-policy queries in `tech/visibility.lisp`. There are no sightline
groups in the current representation; each entry is one endpoint pair.

- Every authored sightline is written once from the location an actor occupies; `los-via` is
  symmetric, so the engine stores its mirror. The far endpoint identifies its use: a gate is
  a direct jammer target; a transmitter, receiver, repeater, or gun is an apparatus
  endpoint; a location supports connector-to-connector pairing and a gears target resolved
  through `has-position`.
- Coordinate derivation retains walls, edges, gates, and boundary segments in oriented
  `los-barrier-crossings>` records, separate from the `los-via` occluder list.
- Ordinary `visible` sight never clears a solid crossing by height and requires gate
  occluders to be open. Beam and elevated-jammer sight can clear finite barriers above their
  inclusive tops; only beam sight also treats a location occluder as blocked when an
  occupant there spans the interpolated beam elevation.

### 1.7 Three differences to keep in mind

1. **Walking is agent-dependent; reach and sight are not.** A path walkable empty-handed may
   be impassable while carrying a resource. `reachable` and `visible` take no agent argument.
2. **Reach clears only through open gates** (1.5), unlike the same obstacle on a walking edge.
3. **Traversal edges are disjunctive; reach edges are conjunctive.** A `traversal-via` list
   offers alternative routes; a `reach-via` list is a set of gates that must *all* be open.

Walking, reach, and sight are distinct graphs with distinct passability rules; a fact true in
one is routinely false in the others.

### 1.8 Beam relay and crossing operations

Source-checked 2026-10-12 against `tech/beam-relay.lisp`, `tech/beam-crossing.lisp`,
`tech/-pickup.lisp`, `tech/-mobility-action.lisp`, `tech/-beam-occlusion.lisp`, and
`tech/-beam-los-coordinates.lisp`. Recheck an affected claim when those files change.

**Moving.** `move` (`-mobility-action.lisp`) takes the agent in one action to any location
in `mobility-results`, so a multi-segment route through intermediate locations costs one
action.

**Connector actions** (`beam-relay.lisp`). Each costs one action.

- `pickup-connector`: `pickup-clear` requires an empty-handed agent, a reachable connector
  (identity reach unless a reach technology is installed), and vertical reach. The action
  removes every `paired` fact on the connector, in both directions.
- `pickup-connector-retaining-pairings`: the same lift, but every `paired` fact survives.
  It is offered only when the connector has at least one pairing, since otherwise it
  duplicates `pickup-connector` exactly.
  A held connector has no `has-location` and so emits no beam. Setting it down again with
  `put-connector` revives whatever its retained pairings can still light.
- `put-connector`: sets the held connector down at a reachable location without adding
  pairings.
- `connect-connector`: sets the held connector down at a reachable location and pairs it
  with 1 to `*max-connector-pairings*` termini (default 3) in a single action.
  - **Termini:** a terminus qualifies when it is structurally visible (`potentially-visible`)
    from any location the agent can currently move to (`mobility-locations`), not
    necessarily from the placement location. Whether the beam actually carries a hue is
    decided later by live beam visibility from the placement.
  - **Connector termini:** a connector terminus must sit at a different location.
  - **Placement restriction:** `connectable-location` forbids placement where another
    connector is already lit.

**Lighting** (`compute-relay-lighting`). Hues spread breadth-first from the transmitters,
one relay link per layer.

- A link carries a hue only if it is paired, beam-visible, and not cut.
- A relay reached in one layer by a single hue takes that hue. If two different hues reach
  it in the same layer, it stays unlit. Hues arriving in later layers are ignored.
- Only one lit connector counts per location.
- A receiver is active when a relay of its chroma is paired with it and has a visible,
  uncut beam to it.

**Crossings** (`beam-crossing.lisp`).

- **When a crossing activates:** when both of its beams are live and each reaches it uncut.
  - A transmitter-to-connector beam is live whenever the pairing exists.
  - A beam leaving a connector is live only while that connector is lit.
- **What stops a beam short of a crossing:** an earlier active crossing on the same beam,
  or a closed gate that lies before the crossing (`beam-crossings-before-gate>`).
- **Effect:** an active crossing cuts each whole beam through it, so the relay or receiver
  at its far end gets nothing from that beam.
- **Resolution:** the active set is iterated to a fixed point. Oscillating sets are
  arbitrated by keeping first the crossing whose farther beam source is nearer the
  transmitters. An unresolvable set marks the state inconsistent.

**Occlusion.** A beam-blocker (agent, box, jammer, or connector) whose location lies within
`*beam-occlusion-tolerance*` (default 1/2) of a beam's line, and whose base-to-top span
covers the beam's elevation there, blocks that beam.

---

## Part 2: Building a working reference

Build a readable account of what the specification actually models, then help the user
compare it with what they intend. Collect scattered facts into one reference, but lead with
plain-language behavior and concrete allowed and forbidden examples rather than requiring the
user to inspect Lisp tables; tables are supporting evidence. It supports soundness,
completeness, and consistency review, not just later solution analysis. If a mismatch
appears, investigate it with Part 3.

### 2.1 Inputs

- **Required:** the problem spec `.lisp` file. Read it **fresh from disk every time**. Never
  rely on memory of a prior version, a prior session's reference, or a remembered conclusion.
- **Optional:** a hand drawing or diagram of the environment.
- **Intent evidence:** the user's description, confirmed requirements, and allowed/forbidden
  examples. If absent, explain the model but leave fidelity to intent unconfirmed.
- **After staging:** load output and direct queries of staged databases for derived tables.
  Before staging, clearly distinguish predictions from inspected results.

### 2.2 Discipline

1. **Determine the authoring path first** (1.1); it changes what the movement, reach, and
   sight sections can contain.
2. **Transcribe, don't recall.** Every structural fact in the reference comes verbatim from
   the current file, or from inspected staged output where the fact is derived; label further
   deductions and their assumptions separately. Cite line numbers where it aids checking.
3. **Keep intent and implementation apart, and present for intent review before relying on
   the reference** ([spec-advisor.md](spec-advisor.md) Section 2.6). Correct a transcription
   error in the reference, but record a real mismatch between model and intent and propose a
   spec correction for review.
4. **Keep the capability families separate** (1.7).
5. **Geometry is spec fact in a coordinate-derived spec.** Record positions and segments like
   any other declaration, and use the intent comparison (2.3, section 8) for what cannot be
   read off the spec: an unstated adjacency, a diagram-only feature, an intended but
   unmodeled relationship. If the spec has no geometry, wall sides and sightline
   plausibility belong there as readings of the diagram flagged for confirmation.
6. **Check the start state.** Inspect staged derived facts where available; otherwise trace
   the derivation cascade (the `define-update` functions, in `propagate-consequences!`
   order) over the init facts and label the result a prediction. Record the initial derived
   vector with its evidence: open gates, active receivers, crossing states. On a tech-based
   spec the driver is usually *derived from splice order* rather than authored, so read the
   technology include order to get the sequence right.
7. **Words, not symbols, in legends.** Use tokens like `clear` / `(occluders…)` / `none`.
   Bare symbols render inconsistently and invite transcription drift.
8. **Do not invent modeled facts.** Omit irrelevant mechanics, but retain an intended
   capability that is absent from the spec as an explicit possible omission.
9. **Check currency before reuse.** Refresh affected sections after changes and preserve
   unaffected evidence. Rebuild the reference when its dependency scope cannot be
   established. Never reuse stale conclusions.

### 2.3 Output: the working reference

Open with a **header block**: source filename; authoring path (coordinate-derived or
hand-authored); the technologies included, if any; the `ww-set` configuration
(`*problem-type*`, `*solution-type*`, `*tree-or-graph*`, `*depth-cutoff*`); and any **type
members declared in `define-types` but absent from every init relation**, as candidates for
review rather than automatically missing pieces to place.

Treat the sections below as a template: drop any the problem doesn't exercise, and add
problem-specific ones where a derived layer doesn't fit the headings.

**1. Mobility network** (rules: 1.3, 1.4)
- Each walking edge as a `walking` fact with its exact clause list, marking direction:
  `traversal-via` is symmetric; `traversal-via>` is directional, and the reverse may have a
  different clause list or none.
- `stairway`, `jumping`, and `climbing` facts recorded separately from `walking`.
- The passability rule for each obstacle kind the spec actually uses.
- Air streams, if present, as derived bands.
- Flag any location with no `walking` traversal edge at all, and any location reachable only
  by a directional edge.
- For a coordinate-derived spec, record the geometry inputs and the derived edge count, and
  note that the edge table comes from the load printout.

**2. Reachability network** (rules: 1.5)
- Every reach edge with its barrier list, restating that it is a flat conjunction.
- Note which locations no reach edge touches.

**3. Visibility network** (rules: 1.6)
- **One location × visibility-object table of occluder lists**, keyed by endpoint rather than
  consuming role, using the word-token legend: `clear` for an empty list, `(occluders…)`
  where a sightline exists but carries gates or location candidates, `none` where there is no
  entry and therefore no sightline. **This table is historically error-prone: transcribe it
  exactly and call it out in the verification request.**
- Finite-barrier crossings recorded separately, not flattened into the occluder list.
- State the sight policy being applied (ordinary, beam, elevated jammer).
- Flag any location with no sight data.

**4. Object and role inventory**
Every movable (cargo) object: start location, kind, the roles it can play (mover, carrier,
jammer/placer, beam-blocker, …), and **what world state each can independently force**. Name
any scarce, load-bearing resource (a single jammer, one connector short of the pairings
needed). Overloaded resources drive the puzzle.

**5. Derived-state dependency chain**
Read the truth condition of each derived fluent off the `define-query` helpers and
`define-update` combine rules, and write the chain `base fact → derived fluent`:
- Each controller's drive condition (`energized`, beam-reaches conditions, corridor-clear
  rules).
- The per-output combine rule and its modes (`normal` = open when energized, `inverted` =
  open when not, `toggle` if present), including override precedence. Jamming overrides: a
  jammed gate is forced open, jammed gears forced stopped.
- `controls` takes a DNF clause list of controllers (1.3).
- Which outputs are uncontrolled and therefore forceable only by a direct base action such as
  jamming. These usually pin the terminal action.
- If beams are in play, the crossing bookkeeping: `beam-via`, `crossings-along-beam>`,
  `beam-crossings-before-gate>`, and the dynamic `crossing-active`.

**6. World-mode enumeration**
- Identify the **minimal set of agent-controllable base toggles** that fix all derived state
  (for example, "is the jammer on gate X?", "is corridor location L occupied?").
- Describe relevant combinations as named modes. Mark each as reachable by a witnessed path,
  ruled out by an established invariant, or unverified; do not infer reachability from a
  consistent assignment alone. For each mode give the **full derived vector** plus which
  movement, reach, and sight passages it opens or closes.
- List the **transitions**: which action flips which toggle, and hence which modes are
  adjacent.
- Watch for mutually exclusive modes: a passage needed for step A opens only in a mode that
  closes the passage needed for step B. That tension is usually the puzzle's core, and it is
  what the deadlock patterns (3.5) name.

**7. Goal reduction**
- State the goal fluent(s).
- Find the **forced terminal action**: the `define-action` whose effect can assert the goal
  fluent. If only one action or branch can, it is forced. Recurse onto its preconditions.
- Pin the **mode** the terminal action must fire in (holding and occupancy preconditions often
  force one) and read that mode's row in section 6 for the vantages and edges then live.

**8. Intent comparison and unresolved questions**
Compare important requirements with the model, including declared geometry: a coordinate
being present does not establish that it matches the intended drawing. Follow
[spec-advisor.md](spec-advisor.md) Section 2.6 for the comparison itself; record each
requirement's status here without duplicating raw tables.

| Intended rule and source | Modeled behavior and evidence | Review status | Next check or question |
|---|---|---|---|
| User-confirmed rule or explicitly provisional interpretation | Source declaration, staged query, or replayed case | Confirmed for tested cases / mismatch / unresolved / not represented | One concrete example or clarification |

Include action effects and goal meaning, not only where movement is possible. Legitimate
Talos entries: an adjacency the diagram shows but no relation encodes; a feature drawn but
not modeled; an intended relationship the spec appears to be missing. Phrase each as a
reading of the diagram, or as an open question if none was given, and ask for confirmation.
Nothing here is a spec fact.

### 2.4 Self-check before presenting

- Was the authoring path determined first, and do the network sections reflect it?
- Does every structural claim trace to a line in the current file, or to inspected staged
  output for a derived fact, with deductions labeled separately?
- Were the Part 1 rules applied: movement, reach, and sight kept strictly separate; DNF
  clause lists recorded as clauses, not flattened; reach lists recorded as flat conjunctions?
- Is the computed start state internally consistent (re-run the cascade to a fixpoint)?
- Does the intent comparison distinguish intended behavior, implemented behavior, evidence,
  and unresolved differences, with claims scoped to the checks performed?
- Are unused relation and obstacle kinds omitted rather than invented?
- Have I named the missing type members and the scarce resources?

---

## Part 3: Investigating a discrepancy or missing relation

Help the user understand a difference between the intended problem and the modeled behavior,
and assess whether a missing relation explains it. The procedure below is a specialized
omission investigation, not a general assumption that a difficult or apparently unsolvable
spec needs more relations. Explain the current model first (Part 2); its intent comparison
supplies requirements and open questions, and its world modes supply state-dependent
evidence, with reachability claims qualified by their actual verification.

### 3.1 Before diagnosing an omission

State the user's expected behavior, its source, the model's actual behavior, and one concrete
case that distinguishes them. Ask the user to clarify uncertain intent before treating it as
a requirement. Classify the discrepancy as soundness, completeness, consistency, or
uncertainty ([spec-advisor.md](spec-advisor.md) Section 2.6). Two Talos-specific cautions:

- For a **soundness** problem (the model admits something forbidden), look for an extra
  permission or missing restriction; adding connectivity would usually make it worse.
- For a **completeness** problem, a missing relation is only one possible cause, alongside a
  wrong precondition, a loading failure (1.1), or an inadequate choice of representative
  locations.

Present findings as **intended behavior → modeled behavior → evidence → proposed correction →
allowed and forbidden cases to recheck**. Staging and bounded checks can expose a mismatch
without a full solve. Search failure, a cutoff, or a static gap alone does not prove
unsolvability or a missing relation, and a solution does not prove fidelity to intent.

Preserve supplied objects, apparatus, geometry, controller wiring, colors, and heights. An
additional standing location can be proposed when justified by existing geometry. Do not
invent equipment, openings, or barriers to obtain a preferred plan. Correcting a confirmed
transcription error is different from changing the intended puzzle; explain that distinction
to the user.

### 3.2 Purpose and scope

Once evidence supports an omitted relation or representative location, investigate:

1. **Where** it belongs: which locations, which sightline, which movement or reach edge.
2. **What barriers or occluders** follow from the supplied geometry and intended rules.

**A missing location is the common case**, and the most tractable: a location that should
exist as a standing spot, sight vantage, or reach endpoint, together with all its incident
relations. The same method applies to any single missing relation instance: a `walking`
traversal edge, a `reach-via` opening, or a `los-via` sightline.

**Working hypothesis, not a conclusion.** The procedure assumes the relevant mechanics are
correct and one relation (or a location and its incident relations) explains the mismatch.
Record evidence for that hypothesis and revisit it if checks disagree. Always read the
current file, and first establish whether the facts involved are authored or derived (1.1).

### 3.3 Core method: working from both ends

Work the problem from both ends and find where they fail to meet.

- **Backward from the goal.** Reduce the goal to the forced terminal action(s), then to
  *their* preconditions, recursively. Goal gates that no controller drives can only be opened
  one way (for example by a jammer), which often pins down the final action and the resource
  that must be consumed there.
- **Forward from the start.** Compute what the start state, plus its derived and coupled
  state, actually makes reachable by movement, by reach, and by sight. Prune everything that
  merely *looks* reachable.

**The missing relation advances a frontier.** It does one of:

- **(a) Bridges** the forward-reachable frontier to a backward-required precondition (the two
  ends meet there), or
- **(b) Extends the forward frontier**: permits further forward progress from a point where
  the agent was stalled, or
- **(c) Enables a backward regression step**: supplies a missing precondition so a required
  action becomes applicable.

**Do not assume case (a).** A missing relation frequently just unblocks one direction.

### 3.4 Analytical milestones

Each milestone is a checkpoint to confirm before proceeding.

1. **Goal reduction.** Reduce the goal to the forced terminal action(s) and the resource(s)
   they consume.
2. **Terminal-vantage analysis.** Enumerate every position and state from which the terminal
   action could fire. Extract the constraints on location, visibility, and reachability.
3. **Invariants and resource couplings.** Identify conserved or mutually exclusive quantities
   (a single jammer; holding versus jamming; a beam corridor that must stay unoccupied).
   These are the hard constraints that generate the deadlock.
4. **Forward-reachability pruning.** Compute true movement, reach, and sight reachability
   from the start. Discard destinations that are plausible but unreachable. Do this **before**
   committing to any destination.
5. **Forced corridor.** When pruning leaves a single productive path, commit to it and trace
   the action sequence along it.
6. **Deadlock isolation.** Find the exact point where forward progress stalls or a backward
   precondition cannot be met. The omission lives *here*, not necessarily where the symptom
   first appears.
7. **Synthesis.** Derive the missing relation(s), one per unmet need (3.6).
8. **Validation.** Compare corrected behavior with intent using allowed and forbidden cases;
   check consistency and geometry. Validate a complete solution separately when requested
   (3.7).

### 3.5 Deadlock patterns (catalog — grows)

Named couplings that produce "almost solvable" specs. Recognizing the pattern shortcuts
deadlock isolation. These are structural observations, independent of the relation
vocabulary.

- **"The opener is the payload."** The single scarce resource that *unlocks* the path is the
  same one that must be *consumed at the destination*. It cannot be in both places.
- **"Carrying closes the path it opened."** Moving the resource flips a derived state that was
  holding the path open, re-sealing it behind the agent. Expect a **stranded-resource
  sub-problem**: the resource sits on one side, the agent on the other, with no legal way to
  reunite them.
- **"Self-defeating vantage."** Occupying the spot needed for the terminal action violates a
  constraint the action depends on, for instance standing in a beam corridor that must stay
  unoccupied.

The appendix works all three at once.

### 3.6 Synthesis checklist

First confirm the facts are hand-authored, not derived (1.1). Then map each **unmet need** to
exactly one relation, and choose its barriers deliberately.

- *"The agent must be able to walk here"* → `traversal-via` entries in `walking` mode joining
  the location to its neighbors, or `traversal-via>` if the passage is one-way. Choose the DNF
  clause list.
- *"The agent must be able to use stairs here"* → `traversal-via` / `traversal-via>` in
  `stairway` mode, with a DNF enabling-means list.
- *"The agent must be able to jump or climb here"* → `traversal-via` / `traversal-via>` in
  `jumping` or `climbing` mode, with a DNF feature/means list; every usable climbing clause
  must contain its positioned ladder.
- *"The target must be visible from here"* → one `los-via` fact from the vantage location to
  the relevant endpoint: a gate for direct gate jamming, an apparatus or gun for that fixture,
  or a location for connector pairing and a gears target resolved through `has-position`.
  Choose the flat occluder list.
- *"Cargo must cross a gap from here without the agent walking"* → `reach-via`, with a flat
  list of barrier gates.

Derive **barriers and occluders** from the intended environment, and check both directions:

1. The edge or sightline is **usable exactly when intended**, in the state the forced corridor
   reaches it.
2. The edge or sightline is **not usable when the intended rules forbid it**. An unexpectedly
   short solution is a reason to check the model, not authority to add a barrier.

The barrier choice is frequently the keystone of the whole inference. DNF gives a second
lever beyond *which* barriers: a second clause creates an alternative route that opens under
different conditions, which can be exactly right or a hole straight through the puzzle.

### 3.7 Validation checklist

- **Intent and regression checks.** Show that the previously missing legal case is now
  represented, and that nearby forbidden cases remain forbidden. Investigate unexpected
  shortcuts against confirmed rules rather than suppressing them merely because they are
  short.
- **Geometric honesty.** Check locations, directions, heights, and crossings against supplied
  geometry using each capability's actual rules. Movement, reach, and sight need not agree
  about passage. Coordinate derivation applies implemented geometry rules; it does not prove
  that inputs or those rules match the user's intent.
- **Concrete plan, when requested.** Replay the complete path under the corrected spec and
  check the original goal and applicable technology validators
  ([capability reference](advisor-reference.md), entry F21). Keep this separate from fidelity
  checks; a search cutoff is not a requirement on the intended puzzle. Obtain approval for
  expensive search.
- **Consistency on propagation.** Confirm the terminal action's `propagate-changes!` settles
  without `inconsistent-state` and without disturbing unrelated derived facts.

### 3.8 Pitfalls and anti-patterns (grows)

- **Hand-adding a derived fact.** The spec asserts wall, edge, or boundary geometry, so the
  `walking` traversal facts, `los-via`, and barrier-crossing records are computed at
  initialization (1.1). Investigate the derivation and its inputs, then correct only a
  demonstrated mismatch with intent.
- **Assuming a bridge.** Treating the missing relation as case 3.3(a) when it only extends one
  frontier (b or c). Check which it is.
- **Committing to a destination before pruning.** A destination can look viable on sight or
  reach grounds yet be movement-unreachable. Prune walkability first.
- **Sightline that contradicts the wall.** Granting a location a clear view of a target that,
  by its position, should be occluded by the intervening wall's gates.
- **Unbarred reach edge through a wall.** A `reach-via` with an empty barrier list punches a
  hole straight through a gated wall and can collapse the puzzle to two actions.
- **Misreading reach barriers.** Writing a reach list as alternative clauses, or listing
  screens or ladders in it; reach lists are flat conjunctions of gates only (1.5).
- **Conflating reach with movement (or sight).** They cross barriers under different rules; a
  fix valid for one is often invalid for another.
- **Fixing the symptom site.** The deadlock surfaces downstream of the omission; place the new
  relations at the deadlock's *cause*.
- **Wrong `los-via` endpoint.** Authored sightlines always start from a location. A jammer
  aiming at a gate names that gate; a beam pairing names its apparatus endpoint; a gears jam
  target names the location in its `has-position` fact. The relation name no longer encodes
  the role.

### 3.9 Quick procedure

1. Determine whether movement and sightline facts are authored or derived (1.1).
2. Reduce the goal to its forced terminal action and consumed resource.
3. Characterize every legal terminal vantage; note the sight, reach, and stand constraints.
4. List invariants and resource couplings.
5. Prune forward reachability (movement, then reach, then sight); kill false destinations.
6. Commit to the forced corridor; trace actions until progress stalls.
7. Isolate the deadlock; name its pattern.
8. Propose the relation(s) supported by the confirmed intended behavior and supplied geometry.
9. Review the proposal, check allowed and forbidden cases and propagation consistency, update
   the working reference, and separately replay a complete solution if requested. Report
   unresolved scope.

---

## Appendix — historical exemplar: `problem-claustro.lisp`, missing `location3`

> **Superseded vocabulary.** This analysis was written against an area/LOS-group
> representation that no longer exists (`in-area`, `in-los-group`, `reachable-via`) and
> against `problem-claustro.lisp`, which has since been deleted (only
> `problem-claustro-topo.lisp` remains). The current relation named `los-via` has the unified
> symmetric endpoint semantics of Part 1, not those of this exemplar. It is retained because
> the *reasoning* is a clean worked instance of Part 3 and demonstrates all three deadlock
> patterns interacting. Do not copy its obsolete relation names or representation.

**Goal reduction.** Goal `(open gate5)`. No `controls … gate5` entry exists, so
`update-gate-status!` can open gate5 only via jamming. There is one jammer. ⇒ the terminal
action is `jam-gate … gate5 …`, and jammer1 must terminate on gate5.

**Terminal vantage.** `jam-gate` requires the agent *holding* the jammer, with a placement
`?location` reachable from the agent and `(visible ?location gate5)`. So a vantage with a
sightline to gate5 is required.

**Key invariants.** (i) Single jammer. (ii) *Holding* and *jamming* are mutually exclusive
states of that one jammer. (iii) Receiver1 is active iff the beam corridor `(gate1 location2)`
is clear: gate1 open **and** location2 unoccupied. (iv) gate1 is uncontrolled, so only jamming
opens it. (v) gate2/gate3 are `normal`-controlled by receiver1; gate4 is `inverted`.

**Forward-reachability prune.** Seeing gate5 from location1 or location2 needs gate2 **and**
gate3 open ⇒ receiver active ⇒ gate1 jammed ⇒ jammer **not** held, contradicting the
`holding` precondition. So gate5 cannot be jammed from area1. Carrying the jammer flips the
receiver off, which closes gate2/gate3 and (with screen1/ladder1 blocking a carrier) confines
the agent to its current area. area3 is unreachable by movement under any single consistent
gate state, so the area3 vantage (location8, which sees gate5 via the open gate4) is a
**false lead**. ⇒ **area2 is the only productive destination**, where los-group2 sees gate5
directly.

**Forced corridor.** Pick up jammer1 at location1 → jam gate1 at location1 (receiver
activates; gate2/gate3 open; gate4 closes) → move into area2.

**Deadlock.** The agent is now in area2 but the jammer sits at location1 holding gate1 open.
It cannot be carried across (carrying shuts gate2/gate3 behind the agent) and is not
reach-reachable from area2 (no reach edge crosses the gate2/gate3 wall). The jammer is
**stranded** relative to the agent: the *"opener is the payload"* and *"carrying closes the
path it opened"* patterns combined.

**Synthesized relations.** A walkable area2 spot, sight-equivalent to its neighbors, with a
reach opening back to location1 barred by the very wall it crosses. The barrier
`(gate2 gate3)` is the keystone: it makes the reach opening usable **only while those gates
are open**, i.e. only after gate1 has been jammed and the receiver activated, which forces the
agent through the real sequence instead of a two-step shortcut.

**Validation and plan (5 actions, ≤ depth 20).**

1. `pickup-cargo agent1 jammer1` (at location1)
2. `jam-gate agent1 gate1 location1`: receiver active; gate2/gate3 open
3. `move agent1 location3`: crosses the now-open interface, empty-handed
4. `pickup-cargo agent1 jammer1`: reaches back through the open gates to lift the jammer off
   gate1; receiver off, gate2/gate3 shut (harmless; the agent is already across)
5. `jam-gate agent1 gate5 location3`: sightline to gate5 is direct ⇒ jam succeeds ⇒
   `(open gate5)`

Anti-trivialization holds: at the start, gate2/gate3 are closed, so location3 is not
reach-reachable from location1; the agent cannot place the jammer onto location3 until it has
first jammed gate1.

**Pitfalls encountered.**

- *Sightline versus wall:* an early attempt gave location3 the right sightline but an
  *unbarred* reach edge from location1, a hole punched through the gate2/gate3 wall. Resolved
  by barring the reach edge.
- *False destination:* area3 looked viable on sight grounds but is movement-unreachable;
  rejected only after walkability pruning.
- *Bridge assumption:* location3 is not a pure forward/backward bridge; it is a
  **forward-progress enabler** (case 3.3(b)) that lets the stalled agent in area2 acquire the
  stranded jammer.

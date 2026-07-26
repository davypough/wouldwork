# Subgoal Deduction Strategy for WouldWork

A repeatable procedure for proposing a useful intermediate subgoal for an
intractable WouldWork problem, given only the spec (and optionally the
final goal state). Designed for problems where heuristic search is
unreliable or unavailable, depth cutoffs are modest, and no example
solutions are known.

The procedure is executed in phases. Each phase produces a short,
inspectable result that is presented for user confirmation or adjustment
before the next phase begins.

---

## Workflow

Per-problem application:

1. Read the spec file (use Filesystem; never recall from prior sessions).
2. **Phase 1 — Problem-class triage.** Classify the problem along four axes
   and assign a primary class label by consulting `problem-class-library.md`.
   *Present, await confirmation.*
3. **Phase 2 — Landmark extraction.** Three-pass procedure: persistent
   landmarks from the goal state, backward-chained transients, and
   action-schema transients. *Present, await confirmation.*
4. **Phase 3 — Phase-boundary identification.** Locate the action-schema
   phase transition between landmark establishment and goal completion.
   *Present, await confirmation.*
5. **Phase 4 — Co-stability vetting.** Verify the candidate landmark set
   can hold simultaneously in some single state, against static-geometry
   mutex, dynamic-relation mutex, and depth-cost feasibility.
   *Present, await confirmation.*
6. **Phase 5 — Restrictiveness calibration.** Decide what to commit and
   what to leave free. *Present, await confirmation.*
7. **Empirical test.** User runs the candidate subgoal via `solve-subgoal`.
8. **Phase 6 — Diagnostic protocol** (only if test fails). Ordered
   relaxation rules. *Present, await confirmation.*

---

## Phase 1 — Problem-class triage

### Inputs

- The problem spec file (types, dynamic relations, static relations,
  action schemas, init, goal, problem parameters).
- Optionally the final goal state, when an enumerator or partial knowledge
  has produced one.

### Output

An axis profile and a primary class label (with secondary classes if any),
both keyed to entries in `problem-class-library.md`.

### Procedure

#### Step 1 — Inventory

From the spec, extract:

- Goal conjuncts (each as a tuple of relation + bound objects).
- Object types and instance counts.
- Action schemas: which object types each schema manipulates, which
  relations each schema asserts/retracts.
- Static accessibility: `accessible0`, `accessible1`, `controls`.
- Static visibility/los: `los0/1`, `visible0/1`, `observable`.
- Problem parameters: `*depth-cutoff*`, `*solution-type*`,
  `*tree-or-graph*`, `*symmetry-pruning*`, `*threads*`.

#### Step 2 — Decomposability axis

For each pair of goal conjuncts (gᵢ, gⱼ):

1. Do they share variables? If yes, **coupled**.
2. Do they reference disjoint object types in their relation arguments,
   *and* are their causal chains (preconditions traced backward through
   action schemas) on disjoint object pools? If yes for every pair,
   **independent**.
3. Otherwise, **coupled**.

#### Step 3 — Resource-scarcity axis

For each object type T appearing as the manipulated cargo in some action
schema (i.e., T appears in an action's parameter list and the action
asserts/retracts a relation on T):

1. Count instances `|T|` from init.
2. Count distinct *configurations* of T required by the goal, where a
   configuration is the joint assignment of T's location and any
   pairings/orientations the goal mentions. Include *transient*
   configurations forced by causal chains tracing backward from goal
   conjuncts (e.g., a gate-opening receiver needs a chain).
3. If `|configurations| > |T|`, T is **scarce-shared** (must-undo signal).
   Otherwise **abundant** for T.

The axis label is **scarce-shared** if any T is scarce-shared, else
**abundant**.

#### Step 4 — Topology axis

Examine static accessibility:

1. For the goal area(s) `A_goal` (any area appearing in a `(loc agent _)`
   goal conjunct), is there an `(accessible0 _ A_goal)` from any area
   reachable from the agent's start? If yes, **open**.
2. Otherwise, identify the gate(s) `G` such that `(accessible1 _ G A_goal)`
   is the only access path. Check whether `G` is initially open. If not,
   `G` is a **bottleneck**, and the topology is **bottleneck-gated: G**.
3. The bottleneck is *hard* if `G`'s opener (via `controls`) requires
   non-trivial setup (e.g., a beam chain) that itself consumes scarce
   resources or requires the agent to traverse to another area first.

#### Step 5 — Dynamic-interaction axis

Examine `propagate-changes!` (or equivalent), dynamic-relation
cross-references, and the geometry of `coords`:

1. Does any update function deactivate one relation as a side effect of
   activating another? Examples in WouldWork: beam-beam interference
   (`update-beams-if-interference!`), distance-priority hue conflicts
   (`deactivate-conflicted-relays!`).
2. Do goal conjuncts involve relations whose physical realizations share a
   medium (a beam path, a corridor, an occupancy slot)?
3. If yes, mark **mutex-prone** with the candidate mutex pairs noted.
   Otherwise **simple**.

#### Step 6 — Profile and class label

Combine the four axes into a profile and look up the primary class label
in `problem-class-library.md`. If multiple labels match, output a ranked
list with the dominant axis first.

#### Step 7 — Present for confirmation

Output format:

```
PROBLEM:        <n>
DECOMPOSABILITY: <independent | coupled>
RESOURCE-SCARCITY: <abundant | scarce-shared: T₁, T₂, ...>
TOPOLOGY:       <open | bottleneck-gated: G₁, ...>
DYNAMIC-INTERACTIONS: <simple | mutex-prone: (relation₁ ↔ relation₂), ...>
PRIMARY CLASS:  <label, with library reference>
SECONDARY CLASSES: <if any>
CONFIDENCE:     <high | medium | low>
RATIONALE:      <one paragraph>
```

---

## Phase 2 — Landmark extraction

### Inputs

- The problem spec.
- Output of Phase 1 (axis profile, primary class).
- Optionally a final goal state (enumerator output, or a hand-supplied
  `*known-goal-state*`).

### Output

A categorized set of *candidate landmark facts*. These are the building
blocks from which Phase 3 will assemble the subgoal. No commitment yet
to which landmarks make the cut.

### Procedure

Three passes are run independently and the results unioned. A final
annotation step records compatibility relations among the merged set.

#### Pass A — Persistent landmarks

A fact P is *persistent* if it holds in some final goal state. P is
trivially a landmark (it held at end ⇒ it held at some point).

**Pass A1 (always available).** Every conjunct of the goal expression
is a persistent landmark.

**Pass A2 (when a goal state is enumerated).** Every non-derived atom
in the enumerated goal state is a persistent landmark. Skip atoms that
are pure consequences of `propagate-changes!` (e.g., `beam-segment`,
`current-beams`, `color` of relays — these are recomputed from
`paired`, `loc`, and source activity, so they will re-derive whenever
the same `paired`/`loc` configuration is re-established).

A2 strictly enriches A1 when available.

#### Pass B — Backward-chained transient landmarks

A fact Q is a *backward-chained transient landmark* if every solution
must satisfy Q at some intermediate point, derived by backward-chaining
through static structure from a goal conjunct or a Pass-A landmark.

For each persistent landmark P:

1. Identify the action schemas (and `propagate-changes!` rules) that
   can assert P. Gather the dynamic preconditions of each.
2. Identify the static-relation constraints on P (`accessible1`,
   `controls`, `los1`, `visible1`, `chroma`).
3. If, across all enabling sources, the dynamic precondition set has a
   common required fact Q, Q is a backward-chained transient landmark.
4. Recurse on Q. Stop when Q is already a Pass-A landmark, an init
   fact, or fully grounded by static relations.

In WouldWork, two static-structure patterns dominate:

- **Gate-traversal pattern.** From `(loc agent A_target)` and an init
  containing only `(accessible1 _ G A_target)` (no `accessible0`),
  derive the landmark `(open G)`. Then from `(controls R G)` derive
  `(active R)`. Then from `(active R)` and `(chroma R hue)` derive a
  beam-chain landmark: a connector chain from a transmitter of
  matching `chroma`, ending paired with `R`, with chain members
  located in areas having LOS to their neighbors.
- **Color-source pattern.** From `(active R)` with `(chroma R hue)`,
  the connector immediately upstream of `R` must transiently have
  `(color C hue)`. This may differ from the goal-state color of `C`
  (a transient color that flips later under chain reorganization).

#### Pass C — Action-schema transient landmarks (object relocation)

For each scarce-shared object T (from Phase 1's resource-scarcity axis,
or any cargo T that is manipulated by some action schema):

1. Compare T's init configuration to its goal-state configuration
   (location and pairings).
2. **Location-change subcase.** If T's `loc` differs between init and
   goal, T was held at some point. Hence `(holds agent T)` is a
   transient landmark.
3. **Pairing-change subcase.** If T's pairings in goal differ from
   those in init (and pairings can only be asserted by
   `connect-to-N-terminus`, which requires T held at the moment of
   pairing), then T was held at some point. Hence `(holds agent T)`
   is a transient landmark even if T's `loc` is unchanged.
4. The two subcases produce the same landmark. Record once per T.

For Pass C to be exhaustive, also include cargo T whose goal-state has
T held: `(holds agent T)` is then both persistent (Pass A2) and a
landmark; record under both passes.

#### Step — Compatibility annotations

Annotate the merged landmark set with:

- **Mutex sets.** Landmarks that cannot co-hold in any single state.
  In WouldWork, the canonical mutex set is `{(holds agent T_i)}` over
  all i, since at most one cargo is held at a time.
- **Implications.** Landmarks that always co-hold by virtue of static
  structure (e.g., `(active R) ⇒ (open G)` whenever `(controls R G)`).
- **Causal precedence.** Landmark X must precede landmark Y in time.
  Sourced from Phase 1's bottleneck identification (e.g., `(open G)`
  precedes `(loc agent A_target)` for the bottleneck's near and far
  sides).

Mutex annotations are required for Phase 3, which cannot place mutex
landmarks in the same subgoal snapshot. Causal precedence drives
Phase 3's selection of which side of the bottleneck the subgoal sits
on.

#### Step — Present for confirmation

Output format:

```
PERSISTENT LANDMARKS (Pass A1):
  <list of goal conjuncts>
PERSISTENT LANDMARKS (Pass A2, if enumerated goal state available):
  <list of non-derived goal-state atoms>
BACKWARD-CHAINED TRANSIENT LANDMARKS (Pass B):
  <list, each with its derivation chain>
ACTION-SCHEMA TRANSIENT LANDMARKS (Pass C):
  <list, each with relocation/pairing rationale>
COMPATIBILITY ANNOTATIONS:
  Mutex sets:        <list>
  Implications:      <list>
  Causal precedence: <list>
```

---

## Phase 3 — Phase-boundary identification

### Inputs

- Spec.
- Output of Phase 1 (class label).
- Output of Phase 2 (landmark sets and compatibility annotations).

### Output

A candidate subgoal expression suitable as the argument to
`solve-subgoal`, plus an estimate of leg-1 cost.

### Concept

A WouldWork solution naturally factors into **manipulation phases**, one
per Pass-C transient (one per held-cargo episode). Each phase is the
sequence: pickup-T, possibly some moves, connect-T. Each phase
classifies as either:

- **Anchor phase** — its `connect` action asserts persistent landmarks
  (Pass A2 atoms about T: T's goal-state location and pairings).
- **Bridge phase** — its `connect` action asserts only transient
  pairings, which are later undone (T does not appear in Pass A2 with
  the same pairings, or T is held at goal). Bridge phases exist only in
  must-undo problems and are why those problems resist heuristic search.

The productive subgoal sits at a phase boundary chosen so that:

1. All anchor-phase landmarks established up to the boundary are
   committed (constraining leg 2 maximally).
2. No bridge-phase pairings are committed (avoiding conflict with
   leg-2's reorganization).
3. The next phase's transient `(holds agent T_next)` is at peak (the
   pickup has fired but the connect has not), which positions the agent
   meaningfully for leg 2 without committing anything that leg 2 must
   undo.

### Procedure

#### Step 1 — Identify manipulation phases

For each Pass-C transient cargo T_i, define `phase-T_i = pickup-T_i,
move(s), connect-T_i`. The connect's asserted pairings are read off:

- From Pass A2 if T_i appears placed there (anchor phase).
- From Pass-B beam-chain landmarks if T_i is the upstream connector of
  a transient receiver activation (bridge phase).

#### Step 2 — Classify phases as anchor or bridge

For each phase-T_i:

- **Anchor** if T_i's goal-state pairings are non-empty in Pass A2.
- **Bridge** if T_i's goal-state pairings are absent or differ from the
  pairings the phase asserts (i.e., the phase's pairings are transient,
  reorganized later).

#### Step 3 — Order the phases

Sequence phases so that:

1. **Pairing dependencies.** If `phase-T_i` asserts `(paired T_i T_j)`
   and T_j is a connector, T_j must already be placed (so that
   `selectable` succeeds for T_j as a terminus). This forces an
   ordering between phases whose connects reference each other.
2. **Bottleneck precedence.** Any bridge phase whose connect activates
   the bottleneck-opener must occur before the bottleneck cross.
3. **Spatial cost.** Under min-length, prefer the cheapest pickup
   continuation: the next phase's T should be co-located with (or
   reachable cheaply from) the agent's location after the previous
   phase's connect.

#### Step 4 — Apply class-specific boundary heuristic

Lookup the class in `problem-class-library.md`. The canonical placements:

- **Class A (independent).** No boundary; direct `(solve)`.
- **Class B (bottleneck-gated).** Boundary at bottleneck cross —
  opener active, agent on far-side or just crossed.
- **Class C, E, F (must-undo family).** Boundary
  `after-first-anchor-phase`: immediately after the first anchor
  phase's connect, at the peak of the next phase's pickup transient.
  This commits the first anchor's persistent landmarks while leaving
  all bridge-phase work to leg 2.
- **Class D (mutex-prone).** Boundary at a state where a mutex-free
  subset of the goal is satisfied; defer mutex-prone conjuncts.

#### Step 5 — Determine T_next and agent location

Given the chosen boundary:

1. **T_next** = the cargo of the next phase in the ordering. If the
   boundary is `after-first-anchor-phase`, T_next is the cargo of the
   second phase. The transient `(holds agent T_next)` is included in
   the subgoal.
2. **Agent location at boundary.** The agent's natural position is
   determined by the action sequence to reach the boundary: end of
   phase-T_1's connect (in T_1's connect site), then pickup-T_next,
   then optionally one strategic move toward T_next's connect site or
   the bottleneck. Include `(loc agent A_strategic)` in the subgoal
   if the move substantially constrains leg 2 (typically: agent at
   bottleneck-near-side or at T_next's intended connect site).

#### Step 6 — Compose the candidate subgoal

```
(and ;; Persistent landmarks of completed anchor phase(s) (from Pass A2)
     <(loc T_1 A_T_1)>
     <(paired T_1 t_1)>, <(paired T_1 t_2)>, ...
     ;; Pass-C transient at peak for the next phase
     (holds agent T_next)
     ;; Strategic agent location
     <(loc agent A_strategic)>)
```

Do not include:

- Goal conjuncts that depend on bridge-phase work (those go in leg 2).
- Pass-B transients (e.g. `(open G)`, `(active R_opener)`) unless they
  are entailed by committed pairings under propagation; even then,
  prefer to omit and let leg-2 search reach them.

#### Step 7 — Estimate leg-1 cost

Count actions in the natural path from init to the subgoal state:

- 1 per pickup-T.
- 1 per move between adjacent areas (count `accessible0`/`accessible1`
  hops from agent's current area to the next).
- 1 per connect-T_i for completed anchor phases.

Verify total ≤ `*depth-cutoff*`.

#### Step 8 — Present for confirmation

Output format:

```
MANIPULATION PHASES:
  Phase 1 (anchor/bridge): pickup T_1 → move(s) → connect T_1 with <pairings>
                           establishes: <persistent landmarks asserted>
                           side effects: <propagation effects>
  Phase 2 (anchor/bridge): ...
  ...

PHASE ORDERING:
  T_1 → T_2 → ...
  rationale: <pairing dependencies, bottleneck precedence, cost>

BOUNDARY:
  <after-first-anchor-phase | at-bottleneck-cross | ...>
  T_next: T_2
  agent location: A_strategic, derived as <pickup-and-move sequence>

CANDIDATE SUBGOAL:
  (and <conjuncts>)

LEG-1 ACTION COUNT: <N> (cutoff = <C>)
```

---

## Phase 4 — Co-stability vetting

### Inputs

- Spec.
- Output of Phase 2 (landmark sets, compatibility annotations).
- Output of Phase 3 (candidate subgoal, leg-1 sequence and cost).

### Output

A pass/fail verdict on the candidate subgoal, with three-fold rationale
(static-geometry mutex, dynamic-relation mutex, depth-cost feasibility)
plus a goal-side feasibility check on the work deferred to leg 2.

### Concept

A subgoal that *names* a set of co-required facts is useless if no
single state in the problem's reachable set actually satisfies them all
simultaneously. Co-stability vetting tests three failure modes before
the subgoal is run empirically:

1. Two committed facts deactivate each other under
   `propagate-changes!` (geometric or relational mutex within the
   subgoal itself).
2. Leg 2's deferred Pass-A1 landmarks are themselves mutex with each
   other (the goal is infeasible, regardless of subgoal placement).
3. Either leg's action count exceeds the cutoff.

### Procedure

#### Step 1 — Construct the candidate state

Apply the leg-1 action sequence from Phase 3's Step 7 to the init state
and read off the resulting facts. The resulting state is the candidate
state — a concrete instance the subgoal should be satisfied by.

The construction is mental or symbolic, not run in the planner. The
question is: do the propagation rules, applied to the action sequence's
asserted/retracted facts, produce a state that satisfies every conjunct
of the candidate subgoal?

#### Step 2 — Static-geometry mutex check

For the candidate state, enumerate the *active beams* (pairings
producing line segments between coordinate-bearing endpoints in the
same or visible areas):

1. For each pairing in the candidate state, identify the two endpoints
   and their coordinates (via `coords` and `loc`).
2. Compute the segment between them; restrict to segments whose
   endpoints lie in areas with mutual `los1`/`los0`.
3. For every pair of segments, test geometric intersection (the same
   logic `update-beams-if-interference!` applies).
4. If any pair intersects, the candidate state is *not* a stable
   propagation fixpoint — one beam will deactivate. If the deactivated
   beam underlies any subgoal conjunct (directly or via a derived fact
   the subgoal commits), the subgoal is infeasible. If only
   leg-2-deferred facts depend on the deactivated beam, the subgoal
   itself is fine but leg-2 reachability is at risk; flag and proceed.

#### Step 3 — Dynamic-relation mutex check (within subgoal)

For each Phase-2 candidate-mutex pair `{X, Y}`: check whether the
candidate state has both X and Y simultaneously committed by the
subgoal. If yes, the subgoal is infeasible.

For mutex pairs annotated as *candidate-only* in Phase 2 (geometry-
dependent), promote them to *confirmed* mutex if the candidate state's
geometry reveals interference; otherwise the candidate state is
unaffected by them.

#### Step 4 — Goal-side co-stability (deferred Pass-A1 landmarks)

Independently of the subgoal, verify that the *full goal* admits a
co-stable state. For each pair of Pass-A1 conjuncts deferred to leg 2:

1. Identify the pairings each conjunct requires (via Pass-B chains).
2. Test whether *any* assignment of pairings can make both conjuncts
   simultaneously active without geometric or relational interference.

If no assignment exists, the goal itself is infeasible — abort. If an
assignment exists but it differs from any committed pairings in the
subgoal, flag the leg-2 reorganization burden (which informs Phase 5
calibration).

This step catches the subtle case where the candidate subgoal is
locally fine but no continuation exists.

#### Step 5 — Depth-cost feasibility

Compute leg-2 lower bound from the work deferred to leg 2:

```
leg-2 LB = Σ over remaining anchor phases (1 pickup + 1 connect)
        +  Σ over required bridge phases (1 pickup + 1 connect)
        +  Σ required moves (agent area transitions; gate cross = 1)
        +  Σ re-pickup actions for connectors needing reorganization
           (pickup + connect for each cargo whose pairings change
            again on leg 2 relative to the boundary state)
```

Verify both:
- leg-1 cost ≤ `*depth-cutoff*` (from Phase 3 — re-confirm).
- leg-2 LB ≤ `*depth-cutoff*`.

The bound is approximate by design; the test is to flag clearly
infeasible cases, not to pin down an exact count.

#### Step 6 — Reachability sanity check

Walk the leg-1 action sequence from init and verify each action's
preconditions hold in the predecessor state:

- **pickup-T**: `(loc agent A) ∧ (loc T A) ∧ (free agent)`.
- **connect-T**: `(holds agent T)` and the chosen termini observable
  from agent's area.
- **move A→B**: `(accessible0 A B)` or `(accessible1 A G B) ∧ (open G)`.

Failures here mean Phase 3's leg-1 sequence is wrong (typically: a
move requires a closed gate, or a connect's terminus is not
observable). Return to Phase 3.

#### Step 7 — Present for confirmation

Output format:

```
CANDIDATE STATE (after Phase-3 leg-1 sequence):
  <salient committed facts; omit unchanged init facts>

STATIC-GEOMETRY MUTEX:
  Active beams: <chains>
  Intersections: <none | list of pairs>
  Verdict: <pass | fail>

DYNAMIC-RELATION MUTEX (within subgoal):
  Phase-2 candidate-mutex pairs checked: <list>
  Triggered by candidate state: <none | list>
  Verdict: <pass | fail>

GOAL-SIDE CO-STABILITY (deferred Pass-A1 landmarks):
  Pairs checked: <list>
  Co-stable assignment found: <yes (description) | no — goal infeasible>
  Verdict: <pass | fail>

DEPTH-COST FEASIBILITY:
  Leg-1 cost: <N₁>
  Leg-2 lower bound: <N₂>, breakdown: <bullet list>
  Cutoff: <C>
  Verdict: <pass | fail>

REACHABILITY:
  Leg-1 sequence verified: <yes | issues: ...>

OVERALL VERDICT: <pass — proceed to Phase 5 | fail — return to Phase 3>
```

---

## Phase 5 — Restrictiveness calibration

### Inputs

- Candidate subgoal from Phase 3 (verified by Phase 4).
- Phase 1 axis profile.
- Phase 2 compatibility annotations.

### Output

A *calibrated subgoal* — same as the candidate, with each conjunct
classified by role and any over-restrictive conjuncts relaxed or
dropped. This is the form passed to `solve-subgoal`.

### Concept

The Phase-3 candidate names *one* satisfying state. But `solve-subgoal`
searches for *any* state satisfying the conjunction; widening the
conjunction (where it can be widened without expanding leg-2 search)
gives the planner more freedom on leg 1 and exploits symmetry pruning.
Each conjunct falls into one of three roles:

- **R — Reachability-necessary.** Removing makes the cheapest leg-1
  path no longer satisfy the subgoal, or makes the subgoal
  unreachable within the cutoff.
- **N — Narrowing-necessary.** Removing admits subgoal-states from
  which leg 2 cannot complete within budget.
- **F — Free.** Removing has no measurable effect on either leg.

R and N are not mutually exclusive; a conjunct can be both. F conjuncts
can be dropped without harm, though sometimes retaining them clarifies
intent.

### Procedure

#### Step 1 — R-test (per conjunct)

For each conjunct C:

1. Construct the cheapest leg-1 sequence that satisfies the *reduced*
   subgoal (subgoal − C). If that sequence's resulting state still
   satisfies C, then C is automatically achieved — flag C as
   **R-redundant** (auto-satisfied; can drop without cost change).
2. If the cheapest reduced-subgoal sequence does *not* satisfy C, but
   a slightly longer one does, C is **R-binding** (the subgoal forces
   a specific leg-1 path). Keep C.
3. If no leg-1 sequence within cutoff satisfies the reduced subgoal,
   C is also a reachability lifeline — keep.

#### Step 2 — N-test (per conjunct)

For each conjunct C still being considered for relaxation:

1. Identify the set of states satisfying *(subgoal − C)*. (This is a
   superset of the candidate state.)
2. Ask: from any such state, can leg 2 still complete within
   `*depth-cutoff*`? If yes for every member, C is **N-free**
   (removing widens leg-1 without harming leg-2).
3. If for some member leg 2 cannot complete (or the reorganization
   cost balloons), C is **N-binding** (must keep).

This step is approximate — exhaustive enumeration of the superset is
impractical. The practical version: list the obvious members of the
superset (states differing in c1's location, in agent's area, in
which connector is held, etc.) and check each by Phase-4-style
reasoning.

#### Step 3 — Symmetry-pruning audit

For each object name committed in the subgoal:

1. Identify sibling objects of the same type.
2. Check init symmetry: do siblings have identical init-state
   attributes (loc, pairings, free/held status)?
3. If a set of siblings is symmetric, WouldWork's `*symmetry-pruning*`
   will canonicalize over them; an explicit name and an existential
   over the symmetric set are equivalent. Prefer the existential form
   only if it makes the subgoal's intent clearer; otherwise keep the
   name.
4. If siblings are *not* symmetric (different init positions or
   roles), the specific name is forced; keep it.

In practice, must-undo problems (Class C/E/F) typically have
*non-symmetric* connectors (they start in different areas), so
Step 3 yields no relaxations for that family.

#### Step 4 — Agent-location relaxation

The `(loc agent A_strategic)` conjunct is the most commonly relaxable.
Decision rule:

1. If the strategic move is on the cheapest leg-1 path *and* it saves
   ≥1 leg-2 action (e.g., positions at bottleneck near-side or at the
   next anchor's connect site), keep.
2. If the move is *not* on the cheapest leg-1 path, drop — leg 2 will
   move the agent itself.
3. If leg 1 has multiple equally-cheap endings (connect can fire from
   several areas), use a disjunction `(or (loc agent A_1) (loc agent
   A_2) ...)` over the strategic candidates; this lets symmetry-
   pruning collapse equivalent leg-1 paths.

#### Step 5 — Existential relaxation of pairings

For each `(paired T target)` conjunct where T's identity is forced by
no other conjunct *and* multiple candidates of T's type could fill the
role:

- Replace with `(exists (?t T_TYPE) (paired ?t target))`.
- Reject if T appears in any other committed conjunct that fixes its
  identity (e.g., `(loc T A_T)` and the subgoal commits a specific
  location).
- Reject for must-undo problems if the assignment is forced by
  Pass-A2 (the anchor pairings must match goal-state pairings).

#### Step 6 — Compose calibrated subgoal

Walk the candidate subgoal in original order. For each conjunct,
attach the role classification(s) from Steps 1-2 as a comment.
Apply any relaxations from Steps 3-5.

Drop conjuncts marked **F** (free) only if dropping them clearly
improves leg-1 search; otherwise retain for clarity.

#### Step 7 — Re-verify against Phase 4

Phase 5 only widens the candidate, never narrows. The candidate state
constructed in Phase 4 still satisfies the calibrated subgoal, so
Phase 4's static-geometry, dynamic-relation, and depth-cost verdicts
carry over without re-running. Spot-check only if a relaxation
introduced a disjunction (Step 4.3) — verify each disjunct branch is
co-stable.

#### Step 8 — Present for confirmation

Output format:

```
CALIBRATED SUBGOAL:
  (and <conjunct₁>     ;(R/N/F) — rationale
       <conjunct₂>     ;(R/N/F) — rationale
       ...)

RELAXATIONS APPLIED:
  - <relaxation, with reason>
  - ...

RELAXATIONS CONSIDERED AND REJECTED:
  - <relaxation, with reason>
  - ...

LEG-1 COST (calibrated):  <N₁>
LEG-2 LB (calibrated):    <N₂>
```

Suggested REPL test expression after calibration:

```lisp
(progn (ql:quickload :wouldwork) (in-package :ww))
;; Then evaluate
(solve-subgoal '<calibrated-subgoal>)
```

---

## Phase 6 — Diagnostic protocol

Invoked only when the empirical test of the Phase-5 calibrated subgoal
fails. Phase 6 modifies *only the subgoal expression* — it does not
change the spec, depth cutoff, or available actions. If subgoal
modifications cannot recover, the protocol escalates back to a prior
phase.

### Inputs

- The calibrated subgoal from Phase 5.
- The empirical test result (from running `(solve-subgoal '<calibrated
  subgoal>)` and, if that succeeded, a subsequent `(solve)`).
- All prior phase outputs.

### Output

Either a revised subgoal that succeeds empirically, or a directive to
return to Phase 3 (boundary placement) or Phase 1 (class assignment).

### Concept

The remedy ladder is ordered by cost: try the cheapest fix first
(Phase-5 relaxation), escalate through Phase-3 boundary changes, and
finally Phase-1 reclassification. The failure mode points to which
ladder to climb.

### Failure modes

Identify the mode from the empirical output:

- **Mode 1 — leg-1 unreachable.** `solve-subgoal` finds no solution
  within `*depth-cutoff*`. The subgoal is over-restrictive or the
  boundary is too far from init.
- **Mode 2 — leg-2 unreachable.** `solve-subgoal` succeeds; the
  subsequent `(solve)` finds no solution within cutoff. Leg 2 has
  too much work remaining, or the subgoal pinned a value that blocks
  the optimal leg-2 path.
- **Mode 3 — wrong satisfying state.** `solve-subgoal` succeeds with
  a state that satisfies the conjunction but is not the intended
  Phase-4 candidate state, and `(solve)` from that state cannot
  complete. The subgoal expression admits an unintended state space.
- **Mode 4 — search timeout/exhaustion.** Either call runs out of
  resources before terminating. The subgoal under-narrows or planner
  configuration is suboptimal.

### Procedure

#### Step 1 — Identify the failure mode

Examine the empirical output. If the planner reports "no solution
within cutoff," determine whether the failure occurred on the
`solve-subgoal` call (Mode 1) or a subsequent `(solve)` (Mode 2 or 3).
To distinguish 2 from 3, inspect the boundary state returned by
`solve-subgoal` (e.g. via `*current-state*` or a state-inspection
helper) and compare to the Phase-4 candidate state. If they differ,
it is Mode 3.

#### Step 2 — Apply the mode-specific remedy ladder

##### Mode 1 — leg-1 unreachable

Try, in order, until the run succeeds:

1. **Drop F conjuncts.** Any conjunct marked F in Phase 5 — drop it
   from the subgoal.
2. **Drop R-redundant conjuncts.** Any conjunct marked R-redundant.
   Even if our Phase-5 model said leg-1's cheapest path satisfies it,
   the planner may take a different path under symmetry pruning or
   tie-breaking.
3. **Loosen agent location.** Replace `(loc agent A_strategic)` with
   `(or (loc agent A_a) (loc agent A_b) ...)` over candidate
   strategic areas, or omit entirely.
4. **Existential pairings.** Replace specific-named pairings with
   existentials over the connector type, even if Phase-5 audit
   rejected this.
5. **Earlier boundary.** Move the boundary to before the strategic
   move, before the next phase's pickup, or even into the middle of
   phase-T_1's connect (e.g., commit only some of phase-T_1's
   pairings). This reduces leg-1's search depth.

If Mode 1 persists after step 5, escalate: the boundary is wrong for
the class. Return to Phase 3 with a different boundary heuristic,
or to Phase 1 (wrong class).

##### Mode 2 — leg-2 unreachable

Try, in order:

1. **Restore R-redundant conjuncts.** If any were dropped during
   calibration or earlier diagnostic iterations, re-add them.
2. **Add Pass-A2 landmarks.** From Phase 2's Pass A2, add a
   previously-omitted persistent landmark. Prefer pairings over
   locations; prefer the second anchor phase's commitments over more
   bridge-phase pins.
3. **Later boundary.** Move the boundary from
   `after-first-anchor-phase` to `after-first-bridge-phase`. This
   commits the bridge connect (with its pairings) and the bottleneck
   cross to leg 1, leaving leg 2 only the second anchor.
4. **Re-vet co-stability.** Run Phase 4 on the new subgoal. Mode 2
   sometimes signals a missed mutex in deferred Pass-A1 landmarks.

If Mode 2 persists, the goal-side co-stability check from Phase 4
was wrong. Return to Phase 4, re-examine deferred Pass-A1 conjuncts
for hidden mutex.

##### Mode 3 — wrong satisfying state

1. **Compare returned state to Phase-4 candidate.** Identify each
   fact that differs.
2. **Pin the discrepancy.** For the most consequential discrepancy
   (typically a pairing or a location), add a conjunct that forces
   the intended value.
3. **Iterate** if multiple pins are needed.

If Mode 3 persists, the subgoal expression's solution set is
fundamentally too wide — the boundary placement (Phase 3) or class
assignment (Phase 1) needs revision.

##### Mode 4 — search timeout/exhaustion

1. **Confirm planner config.** `*tree-or-graph*` should be `graph`
   (closed-list deduplication), and `*symmetry-pruning*` enabled if
   applicable to the spec.
2. **Tighten the subgoal.** Re-add R-redundant conjuncts, add a pin
   on agent location.
3. **Add a state-symmetry-breaking pin.** A specific committed value
   (a particular connector held, a particular pairing) that prevents
   the planner from exploring symmetric alternatives.
4. **Split leg 1.** Introduce an *earlier* intermediate subgoal —
   generalizing the strategy from 2 legs to 3 legs. Run the entire
   procedure on the earlier portion.

#### Step 3 — Re-vet against Phase 4 (cheap)

For each iteration, do a quick co-stability re-check on the modified
subgoal. Phase 4's machinery is light enough to run mentally; the
goal is to catch obvious mutex violations before retesting.

#### Step 4 — Empirical re-test

Run the modified subgoal. Return to Step 1 if the new outcome is also
a failure (possibly a different mode now).

#### Step 5 — Escalation

If the remedy ladder for the current mode is exhausted:

- Escalate to **Phase 3** (revise boundary placement or phase
  ordering).
- Escalate to **Phase 1** (revise class assignment) if Phase-3
  revision does not help.

A reasonable iteration budget is 3–5 attempts per mode before
escalation; further iteration without progress almost always
indicates a wrong assumption upstream.

#### Step 6 — Record outcome

Append to the library entry under the problem's worked instance:

```
DIAGNOSTIC HISTORY:
  Attempt 1: <subgoal>
  Outcome:   <failure mode>
  Remedy:    <ladder step applied>
  Attempt 2: ...
  ...
  FINAL:     <subgoal that worked>
```

The history is the empirical signal that refines the strategy and
class library over time — patterns in remedies suggest improvements
to upstream phases.

#### Step 7 — Present for confirmation

When invoking Phase 6 in a session, present:

```
EMPIRICAL OUTCOME:
  Test:    (solve-subgoal '<previous subgoal>)
  Result:  <description>
  Mode:    <1 | 2 | 3 | 4>

PROPOSED REMEDY (mode-<M>, ladder step <N>):
  <description of change>

REVISED SUBGOAL:
  (and <conjuncts>)

REPL TEST:
  (progn (ql:quickload :wouldwork) (in-package :ww))
  (solve-subgoal '<revised subgoal>)

FALLBACK if this also fails: <next ladder step or escalation target>
```

After empirical retest, return to Step 1 with the new outcome.

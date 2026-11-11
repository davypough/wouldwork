# Talos Diagnostics — Component Reference

What each constraint diagnostic reports, what it reads, and how far its claims reach. This
replaces `doc/constraint-led-solving/Extractor-Specifications.md`, whose verbatim history
(the frozen crelay-topo register, task records T6–T46) is in git history. `doc/solving-advisor.md`
says when to use these tools; this file says what they mean. Source-checked on 2026-11-10
against the files named in each section.

---

## Loading and running

The diagnostics are plain Lisp files under `tech/`. They are never named in an
`(include-tech ...)` directive and are not ASDF components, so they cannot affect a search.
Load them by hand after staging:

```lisp
(stage <problem>)
(load (merge-pathnames "tech/constraint-profile.lisp"
                       (asdf:system-source-directory :wouldwork)))
(report-static-constraint-profile)               ; print the whole profile
(write-static-constraint-profile <pathname>)     ; write it, e.g. doc/problems/<p>/Constraint-Static-Profile.txt
```

`write-static-constraint-profile` writes through a temporary file and renames it only on
success, so a failing extractor never leaves a truncated profile. A profile is generated output:
never hand-edit it, and regenerate it whenever the spec changes.

Both entry points take an optional relay-view **scenario** (see RC), passed to RC and NH. Without
one, view-dependent results print as UNRESOLVED.

Each extractor also has its own `report-...` function and can be run alone.

**Profile order** (`report-static-constraint-profile`): MC, S0, S1, S2, T6, S5, S6, RC, S7, S3,
S4, CC, NH, SD. RO, CP, FH, the state probe, PB, BX, RL, EQ, SW, RA and BT take explicit input
and are run separately (see *Components run separately*).

### Conventions

- **Domain generality (C3).** No problem object name appears in diagnostic code. Diagnostics
  read declared relations and named substrate interfaces (CONTROLS, ON, HAS-POSITION,
  HAS-LOCATION, PRESSURE-PLATE, the traversal relations). Everything problem-specific comes from
  the staged problem or from caller-supplied data.
- **Grades.** Each section header and claim carries a grade, matching the four kinds in
  `doc/solving-advisor.md`, *Rules and grades*:

  | Grade | Kind | Holds because |
  |---|---|---|
  | 1 | definitional | static facts and definitions; true in every well-formed state, reachable or not |
  | 2 | inductive | true initially and preserved by every action (e.g. no action asserts a traversal fact) |
  | 3 | cost bound | a search result relative to its start, settings and cutoff |
  | 4 | trace/ordering | needs a separate proof obligation |

  `grade 1 -> 2` means the census part is grade 1 and the derived bound is grade 2.
- **Qualified output.** Graph candidates, conditional bounds and missing analysis are labelled as
  such (`UNRESOLVED`, `CONDITIONAL`, `candidate`). A diagnostic never prints a verdict its inputs
  don't support; read the qualification before using a row.

---

## S0 — Type extent census  [grade 1]

`report-type-extent-census`. Step 0 of the profile: every later extractor needs these
emptiness facts before it may read a control aggregate as an axiom.

- **Reads:** `*types*`, `*type-components*`, the signatures in `*relations*` and
  `*static-relations*`, and the bodies of installed queries and updates.
- **Reports:**
  - every declared type with its cardinality and provenance, its EITHER components when it is an
    alias, and an EMPTY or SINGLETON flag;
  - the empty and singleton lists on their own;
  - relations made identically false by an argument type that admits no object;
  - quantifier sites over a statically empty domain, and query bodies that therefore fold to a
    constant.
- **Why grade 1:** no action creates or destroys a declared object, so a predicate quantified over
  an empty type is constant in every state. Dynamically registered objects never enter `*types*`
  (see `load-ordering/loading-and-initialization.md`, Trap 8), so they are not counted here.

## S1 — Control algebra  [grade 1]

`report-control-algebra`.

- **Reads:** every `(controls <clauses> <device> <mode>)` fact in the static database.
- **Reports:**
  - **device state axioms:** per device, how its state relates to its control aggregate:
    `state == aggregate`, `state == not aggregate`, constant TRUE/FALSE, or
    `state == override OR aggregate`. With a live override, every later claim about that device is
    conditional on it.
  - **control table:** one Boolean function per device, from its clause literals;
  - **exclusion and equivalence pairs:** devices with identical clause sets, with opposite or
    equal polarity, each pair qualified by the axioms above;
  - **primitive controllers** with their status relations, the **device dependency DAG** with
    each device's depth, and any cycles.
- **Use:** "these two gates are never open together" is an exclusion pair, valid in every state,
  but only for devices whose axiom is a plain aggregate.

## S2 — Functional-relation census  [grade 1 -> 2]

`report-functional-relation-census`. Builds on S0's extents.

- **Reads:** declared relation keying, S0's extents, and the placement relations (support
  occupant → support).
- **Reports:**
  - **functional relations:** each keyed relation with its key, value and signature (dynamic and
    static; bijective index relations excluded);
  - **occupancy pools** per placement relation: occupants and supports, split by layer (live,
    ghost, unpaired) for recorder problems;
  - **consumers:** for each relation an update asserts under a test reading it, the site and the
    witnesses it reads. Assertion side only; preconditions are not listed;
  - **cardinality bounds** (grade 2): `|occupied supports| <= bodies - |unavailable|`, by
    injectivity of the keyed relation, with the bodies counted per layer. A `*start-state*`
    reading is printed beside each bound and is labelled as not being the bound.
- **Use:** the occupant pool and per-layer body counts are the inputs to T6, S4 and RO.

## T6 — Mechanized budget arithmetic  [grade 1 -> 2]

`report-budget-arithmetic`. Joins S1's control demands with S2's occupant pool to give
impossibility constraints on which devices can be open together.

- **Body-cost classes:** control facts are grouped by S1 equivalence (mode plus canonical clause
  set, ignoring order and duplicates). Each multi-device class is one shared demand, counted once.
  Equal plate sets alone are not equivalence: different non-plate primitives, clause structure or
  polarity stay distinct.
- **Constraints**, emitted only when the classes' support sets are disjoint (otherwise no summed
  cost is sound, and the report says so):
  - **AM1:** the budget is tight; all body-cost devices can be open only if every body is on a plate.
  - **AM2:** the goal actor leaves the occupant pool; its destination has no plate, so it spends one
    body.
  - **AM3a / AM3b:** outside a recorder cycle (live pool) and inside one (full pool), how many
    bodies remain for supports, against the demand: at least the difference in support cost must
    close.
- **Limits:** these constrain simultaneous state assignments; they are not action-sequence proofs.
  With no shortage, the report says no shortage follows and claims no refutation. NH's H1 hint
  uses the same grouped budget.

## S5 — Height and reach lattice  [grade 2]

`report-height-and-reach-lattice`.

- **Reads:** the vertical type constants, authored `has-height` overrides, location levels (third
  coordinate), `*vertical-reach-limit*`, and the placement and lift predicates of the vertical
  technology.
- **Reports:** type heights (axis, base, overrides); location levels; achievable tops for a carried
  object (ground, on mobile supports, on held supports, where cleartop permits); a placement
  legality matrix (agent base → support top: YES/NO); and the support tops unreachable for
  placement from ground level.

## S6 — Beam sightline table  [grade 2]

`report-beam-sightline-table`.

- **Method:** evaluates the problem's `beam-visible` query directly for every station location,
  every achievable S5 connector top, every fixed endpoint, and every subset of directly forced gate
  states. It applies no propagation and runs no search.
- **Reports:**
  - **visibility rows:** per station, height and endpoint, visible always, only under a gate set, or
    never;
  - **location-occluder kill list:** locations whose body blocks a hop at the interpolated
    elevation.
- **Caution:** rows read start-state bodies. A body within `*beam-occlusion-tolerance*` of a sightline
  blocks it, so check the kill list before calling a NEVER structural.

## S7 — Landmark graph and orderings  [grade 4]

`report-landmark-graph-and-orderings`.

- **Method:** backward-chains the explicit goal's device conditions through S1 under the delete
  relaxation (achieved landmarks persist).
- **Reports:** explicit goal landmarks (device conditions; movement or query conjuncts are listed
  as not expandable), each device's S1 controller expansion, and greedy-necessary orderings
  ("establish controller primitives for X before Y").
- **Limits:** it states its relaxation, and emits no route, keeper-return, segment or
  simultaneous-role claim. Route and order extraction needs a separately stated movement relaxation.

## S3 — Gate-labelled region quotient  [grade 2]

`report-region-quotient`.

- **Reads:** the propagated traversal relations (`traverse-via`, `traverse-via>`) and their
  clauses.
- **Method:** a directed graph over locations, each arc labelled with its door family and movement
  kind (walk, stairs, jump, climb). An arc's doors are its clauses' means; static separators
  (staircase, edge, floor drive) are never doors. Arcs with an empty door family are contracted,
  treating an arc as bidirectional only when it is authored symmetric (`traverse-via`, not `>`).
- **Reports:**
  - **region crossings,** classified as **spine** or **composed**: the coordinate derivation supplies a
    transitive closure, so redundant composed rows are separated from the adjacency spine (the same
    reduced graph S4 uses);
  - the **adjacency spine** and the doors on arcs;
  - **controlled devices labelling no traversal arc** (a gate that blocks no movement: an authoring
    signal), and doors that are not controlled devices.
- **Why grade 2:** "passing between these regions requires one of these door families" is an
  invariant, resting on no action asserting a traversal fact.
- **Not supplied:** which side of a crossing a door's controller lies on. That is S4.

## S4 — Cut-keeper table  [conditional bounds; graph candidates]

`report-cut-keeper-table`. Joins S1, S2 and S3's reduced graph with plate positions, object
locations and the goal.

- **Scope, printed with every report:** graph reachability relaxes all other doors, elevation and
  cargo. It omits non-traversal relocation, support transitions and recorder lifecycle. Graph cuts
  and approach-only regions therefore do **not** prove concrete stranding; device conclusions need
  S1's axiom premises.
- **Reports, per controlled device:**
  - controller, position and region, with manipulation-reach candidates;
  - pressure alternatives and individually mandatory plates;
  - for each spine crossing in each direction with the device absent: source and destination
    reachability, whether it is a **graph cut**, and mandatory pressure. That gives a
    **KEEPER-OBLIGATED candidate** where the device state requires the aggregate; permanent
    stranding stays UNRESOLVED;
  - **conditional shortage:** fewer eligible ON witnesses than the minimum simultaneous plate demand
    means this aggregate cannot activate.
- **Goal destinations:** explicit destinations from the goal form, with GRAPH-REQUIRED candidate
  devices; concrete necessity stays UNRESOLVED. Objects without an explicit destination are listed
  as such.
- **Keeper supply:** the S2 pool, with available witnesses treated as a parameter per view and
  segment. Total sequential crossers are not simultaneous body demand.
- **Alternative door families:** when a crossing has clauses with different door sets, S4 uses the
  unreduced quotient, and any clause avoiding the forbidden device permits the row.
- **Never emitted:** an unconditional stranding verdict.

## RC — Relay chain table  [grade 2]

`report-relay-chain-table`, optional `scenario`. Enumerates the ways a beam could be relayed
from a source to each receiver through the connector pool.

- **Method:** stations are S5 placement supports at every location level. Hops (station to fixed
  endpoint, and station to station) are tested with `beam-visible` on start-state copies with gate
  OPEN bits forced, without propagation or search. A hop's **required gates** are those whose closing
  alone blocks it (monotone reading). Every simple source-to-receiver chain within the connector
  count is listed.
- **Reports:**
  - visible station-to-endpoint hops, ALWAYS or with required gates;
  - per receiver, its chains, each with gates and their plates, connectors, risers, bodies, and
    on-/off-plate counts;
  - a summary per receiver: counts by class, gates open in every bootstrap chain, and the least
    off-plate bodies;
  - fixed beam corridors, where the problem has fixed beams.
- **Chain classes:**
  - **EXCLUDED:** needs an S1 exclusion pair open together;
  - **INFEASIBLE:** no riser assignment from the support pool;
  - **LATCH:** needs a device this receiver itself controls;
  - **BOOTSTRAP:** otherwise.
- **Body cost:** the connector plus its riser (ground 0, held tray 2, other support 1). A station on a
  plate counts as keeping it, so off-plate counts are least values.
- **Scope, printed:** physical view only; stations are placements, not reachability. Stable
  simultaneous occupancy and support motion are UNRESOLVED. BOOTSTRAP and LATCH are geometric
  classes, not validated realizations.

**Supplied relay views** (`report-relay-view-scenario`, also passed to RC, NH and the profile
entry points). Tests explicit, complete-state scenarios in two independent views; it never infers
one from geometric candidates. The scenario is a plist:
- `:state` (a problem-state), `:complete-state t`;
- `:phase` (`:ordinary`, `:open`, `:closed` or `:shadow-only`);
- `:provenance` (a non-empty string);
- `:hops` and/or `:chains`;
- optional `:gate-premises`.

Missing data prints as unavailable analysis, never as a failed sightline. CLEAR means every supplied
link and the engine's lighting and receiver checks pass. It is conditional: not stability,
reachability or replay validation. Forced gate bits do not establish controller consistency.

## MC — Mechanic coverage  [grade 1]

`report-mechanic-coverage`. First in the profile: the coverage gate.

- **Coverage table:** every public technology the problem spliced is listed. The registry is
  `*mechanic-contracts*` in `constraint-profile.lisp`. Each is one of:
  - **COVERED, contract:** a quoted statement of what the `tech/` code controls, moves and
    requires, followed by its instance rows;
  - **COVERED, extractors:** named components carry its static consequences (e.g. gate: S1 S3 S4;
    plate: S1 S2 T6);
  - **COVERED, infrastructure:** elevation, reachability, visibility, walkability,
    topo-lower-bound;
  - **UNCOVERED:** no static contract is declared. Per `solving-advisor.md`, an UNCOVERED mechanic
    needs a source-grounded hand contract in the Briefing, or an approved component, before going on.
- **Contracts and instance rows:**

  | Technology | Instance rows show |
  |---|---|
  | floor-blower, wall-blower | source, destination, horizontal direction, stream elevation and width, control (or "default on, unless jammed") |
  | floor-gears | each fan's start mount and compatible mounts (one fan, one stream at a time), mount reach sites, gated arcs, a floor mount's boarding, lift and landing |
  | jammer | sightline sites to each target (gates forced open, no propagation), `jam-disallowed>` triples |
  | jump, ladder, stairs | arcs by clause with levels; ladder presence at source; the reach limit and least launch raise |
  | recorder | cycles allowed (`*max-recorder-cycles*`), live → ghost pairs |
  | beam-relay | pairing capacity, start-state links, per RC station the transmitters visible by hue (COMPETING HUES when two or more) |

  With beam-crossing spliced, MC also prints the engine's crossing pool: each directed beam's
  crossings in order from its source, gate splits as `|gate|`, and each crossing's two beams and
  point.
- **Limits:** a contract states what the code does; it is not a check of it. Instance rows are
  static readings: compatibility and prerequisites, not transport, reachability or which color
  wins (that needs a settled state). For a settled state's equipment, use EQ (below).

## CC — Coupling census  [grade 1; occluder role grade 2]

`report-coupling-census`. Which controls or devices change two subsystems at once. Reads S1,
S3 and RC.

- **Domain:** every controlled device and every primitive controller.
- **Roles:**

  | Role | Rule | Subsystem |
  |---|---|---|
  | barrier | named in a clause of a traversal arc's family | route |
  | occluder | in the required-open set of some RC hop (grade 2) | beam |
  | lift | has an `aimed-at` fact | lift |
  | support-controller | a primitive in the extent of type `support` | occupancy |
  | beam-driven | a device-mediated primitive (S1) | beam |
- **Rows:**
  - **role table:** each object's kind, roles and subsystems;
  - **K1 fan-out:** a primitive driving two or more devices, each pair with its S1 relation
    (EXCLUSION, EQUIVALENCE or DISTINCT CLAUSES);
  - **K2 multi-role:** objects whose roles span two or more subsystems;
  - **K3 beam feedback:** for each beam-driven primitive, the devices it drives and its last-hop
    gates (from RC station-to-endpoint hops) with their controllers, marked SELF or K1;
  - **G15 lift-barrier couplings:** for a K1 primitive driving a lift L (destination D) and a
    barrier B on an exit from D, labelled by L and B's relation:
    - EXCLUSION gives **G15 FLAG**: crossing B starts only after the lift has stopped;
    - EQUIVALENCE is compatible;
    - DISTINCT CLAUSES gives **G15 CHECK**.
    
    FLAG and CHECK rows ask whether the launch support at D survives the toggle (see G15 in
    `open-issues.txt`).
- **Limits:** CC states couplings; it does not apply the launch-configuration question to any
  particular traversal.

## NH — Necessity hints  [grade per hint]

`report-necessity-hints`, optional `scenario`. Restates limits the profile already prints as
candidate plan elements for the Briefing. It derives no new constraint.

- **Labels:**
  - **NECESSARY:** every plan meets it, under the grade and premises shown;
  - **CANDIDATE:** one way to meet a limit; others may exist.
  
  Each hint carries the weakest grade of its sources, and a graph candidate stays labelled as one.
  A hint is not a plan; check it with D.
- **Format:** `H<k>.<i> <LABEL> [<grade>; <sources>]`, then `limit`, `hint` and `note` lines. An
  empty family prints `none`. A family whose S4 graph context failed prints **UNAVAILABLE** with
  S4's reason, which is distinct from a valid empty result.
- **Families:**

  | Family | Grade; sources | Content |
  |---|---|---|
  | H1 body budget | 1 -> 2; T6 S2 | From T6's grouped demand T and the live/full ON pool (less the goal actor when AM2 holds): when held plates exceed live bodies, ghosts are needed or demands are met in sequence; per device, when its least plate demand uses every body. Empty when T6's preconditions fail |
  | H2 keepers left behind | 2, graph candidate; S4 S1 | Per spine crossing direction with approach-only mandatory plates: leave that many bodies other than the crosser on them; a note when the return needs the same plates. Not issued for a device some clause of the row avoids |
  | H3 beam-held devices | 2; S1 RC | A device held by a receiver needs the gates common to all usable chains open, with their plates, while it must hold; then CANDIDATE least-cost bootstrap chains per receiver-end location |
  | H4 controllers off the goal route | 2, graph candidate; S4 S3 S1 | A switch or latch controlling a goal-route device whose every manipulation site needs extra devices: NECESSARY when one is common to all sites; a note for route devices it drives in S1 exclusion |
  | H5 lift landings | 1; CC MC S5 | Per G15 FLAG or CHECK row: keep the lift through the toggle with a support at the destination, or leave by an exit not naming the barrier |
  | H6 active at the start | 1; S1, start state | Devices whose aggregate holds initially: free while the start holders stay on their plates, or until primitives change |
  | H7 placement limits | 2; S5 | Per support top unreachable from ground: raised locations whose level plus reach reaches it. CANDIDATE only |

## SD — Services and setup dependencies  [grade 2]

`report-service-dependencies`. Last in the profile.

- **Services:** passage services (every gate open; each drive a traversal clause names, clear)
  and receiver literals.
- **Providers:**
  - CONTROL: S1 options;
  - OVERRIDE: MC jam sites, with any `jam-disallowed>` sites;
  - EQUIPMENT: gears with no fan;
  - CHAINS: RC chains, EXCLUDED and INFEASIBLE ones noted but not counted as options;
  - CORRIDOR: fixed beam corridors to a receiver.
- **Classes,** from a monotone closure over premises:
  - **DIRECT:** no premise;
  - **SUPPORTED:** premises available without this service;
  - **NEEDS \<service\> FIRST:** installing the provider needs the service it will provide, a
    handover from a temporary provider;
  - **UNSUPPORTED IN SCOPE.**
  
  A service with only NEEDS-FIRST options is a **SETUP QUESTION**, not an impossibility.
  *Through standing providers* repeats the test using only CONTROL, chains, corridors and equipment
  (never another jam), and is printed when that exposes a setup dependency a further jammer
  would hide.
- **Also reports:**
  - service counts by verdict;
  - **opposed controls:** a primitive needed on by some services and off by others;
  - setup dependencies;
  - for the goal actor: minimal **transit** and **return** door sets, **FINAL** services (named by
    the goal) and **TEMPORARY** services (in a transit set but not final; expendable after
    crossing unless a return needs them again);
  - access per region from the start region;
  - retrieval sites (start places of jammers, connectors and fans).
- **Not claimed, printed:** an order, simultaneous availability, body or reach allocation,
  occupancy, view, or a realized setup. With a recorder spliced, SD reads the physical view only.
  For concrete consequences of a change between two settled states, use SW (below).

---

## Components run separately

None of these is called by `report-static-constraint-profile`, and none changes the profile file.
All except PB run no search. The caller supplies every segment, plan, state and event: nothing
here infers one. A missing input key prints as UNRESOLVED, never as a convenient default.

### RO — Role obligations  [conditional allocation for one stated segment]

`(report-role-obligations <scenario>)`, in `constraint-profile.lisp`.

- **Method:** for each requested active device condition, allocates witnesses to the supports its
  normal control aggregate demands, by injective matching. It separates forced membership from a
  forced body-to-plate pairing.
- **Scenario keys:**
  - `:device-conditions`, the devices requested active;
  - `:view` and `:cycle`;
  - `:ghosts`;
  - `:provenance`;
  - `:available-witnesses` (a list; absent or `:unknown` means demand only);
  - `:excluded` and `:committed`: `(object reason)` pairs, each removal printed with its reason.
- **Reports, per device:**
  - the S1 axiom and its view index (in-view, out-of-view, unindexed premises);
  - each pressure alternative with its minimum simultaneous demand and eligible witnesses;
  - **matching:** PERFECT (an injective simultaneous assignment exists, with its forced members and
    pairings) or SHORTAGE (with the Hall violator);
  - conditional occupancy destinations.
  
  A satisfied allocation is grade 1, status CONDITIONAL.
- **Not emitted:** a transport obligation, a stranding verdict, or any goal not given. Presence is
  not availability, and sequential crossers are not simultaneous demand. Per-support restrictions
  beyond availability (reach, elevation, history) are UNRESOLVED.

### CP — Cycle-plan check  [grade per row]

`(report-cycle-plan-check <plan>)`, in `constraint-profile.lisp`. Checks a stage plan stated as
data against the plate, body and view budgets, before any action is written.

- **Plan:** `(:name ... :provenance ... :stages (...))`.
  - Each stage: `:id`, `:intent`, optional `:reservations`, and `:segments`.
  - Each segment: `:id`, `:view`, `:cycle`, `:ghosts`, `:available-witnesses`, `:held`,
    `:off-plate`, `:require` (device conditions) and an optional `:landing`.
  - A reservation states one body's job over a phase range, e.g.
    `(:body <b> :role (:jam <target>) :from "<id>" :through "<id>" :purpose "<text>")`. Roles are
    `:weight`, `:jam`, `:place`, `:hold`, `:mount` and `:support`. Nothing is reserved unless stated.
- **Rows per segment:**

  | Row | Checks |
  |---|---|
  | B0 view | witnesses belong to the stated view and pool; ghosts only when present |
  | B1 plate budget | RO's matching over required plates, with per-plate eligibility from reservations |
  | B2 control conflict | a plate demanded both empty and occupied; S1 exclusion pairs required together |
  | B3 beam | receiver demands against RC's usable chains (physical view only) |
  | B4 lift landing | CC's G15 rows: whether the stated landing is a support that survives the toggle |
  | B5 reservations | role compatibility (K1–K4), shared supports, holders and gears, the eligible pool, releases; jam sightlines from MC's survey |
- **Labels:** PASS, CONDITIONAL (names a premise to settle with D), CONFLICT. A PASS is not a plan
  witness, and a CONFLICT rejects only the stated budget or allocation.

### FH — From here  [grade 1; grade 2 where marked]

`(report-from-here)` for the staged start, `(report-from-here <checkpoint>)`, or
`(report-from-here <action-list>)` for a prefix replayed from the staged start. In
`constraint-profile.lisp`. Every reading uses the engine's own queries and `generate-children`;
nothing is propagated by hand.

- **F0 state:** the state itself.
- **F1 moves and controllers:** each agent's MOVE successors, and which primitive controllers a
  one-step successor, or the agent's next action after one of its moves, changes.
- **F2 placements:** where held cargo can be set down.
- **F3 beams:** the live relay state, then RC's usable chains grouped by the gates closed now.

A line absent from the same section of the staged start's report is marked `*`.

### Applicable actions  (state probe)

`tech/constraint-state-probe.lisp`, loaded on its own after fresh staging:
`(report-applicable-actions <action-list> [:action <name or names>] [:object <symbol>])`.
It replays the prefix and lists every action `generate-children` accepts in the resulting state,
as replay-readable phrases. Symmetry pruning is disabled for the enumeration so no equivalent
instantiation is hidden. One step ahead only.

### PB — Probe battery  [grade per row]

`tech/constraint-probe-battery.lisp`, loaded after `constraint-profile.lisp`. The only diagnostic
that searches. Its results are the Briefing's probe map.

```lisp
(report-probe-battery-list)                         ; list the probes; no search
(run-probe-battery <max-depth> <results-path>)      ; requires (ww-set *threads* 16)
(report-probe-battery <results-path>)               ; print PB from the results file
(run-probe-battery <max-depth> <path> (append (probe-battery-probes)
                                              (d-probes '((<subject> <goal> "<provenance>")))))
```

- **Probe families:**
  - P1: an agent at a landmark location;
  - P2: cargo set down outside its start region;
  - P3: a primitive controller changed from its start reading;
  - P4: a fixed relay paired with a connector;
  - P5: D's own probes, stated as data with `d-probes`.
- **Search:** each probe searches from the fresh staging's checkpoint with solution type `first`,
  deepening one cutoff at a time to `<max-depth>`. Depth is the only bound. The results file is
  rewritten after every probe, so a crash keeps finished probes. The staged start, goal, cutoff and
  solution type are restored however the run ends.
- **Labels:**

  | Label | Meaning | Grade |
  |---|---|---|
  | START | the goal holds at the start | — |
  | CHEAP *n* @ *d* | a plan of *n* actions found at cutoff *d*, none below | 1 on replay |
  | NOT FOUND ≤ *D* | no plan up to the maximum depth, last run truncated | 3 |
  | EXHAUSTED ≤ *d* | no plan, and no truncation at *d* | 3 |
  | LEGACY STOPPED | historical rows only; no bound claimed | — |

  NOT FOUND and EXHAUSTED are relative to the start, settings and cutoff, never a refutation.

### Supplied settled-state checks

Each reads one caller-supplied settled state (`:state`, a problem-state, with a non-empty
`:provenance`) and reports what holds in it. None is a reachability, stability-through-actions or
composition result.

- **BX — beam crossings** (`report-beam-crossing-scenario`, `constraint-profile.lisp`). Takes
  `:state` and `:provenance`. Reports gate states, the active crossings and each live beam's cut
  order.
- **RL — relay lighting** (`report-relay-lighting-scenario`). Takes `:state`, `:provenance`,
  `:view`, `:phase` and optional `:chains`. Reports relay layers, links, pairing capacity and
  receivers in one view.
- **EQ — equipment** (`report-equipment-scenario`). Takes `:state` and `:provenance`, with
  optional `:before` and `:before-provenance`. Reports removable fans and mounts (MOUNTED, HELD,
  RESTING), whether each mounted fan's gears turn (EFFECTIVE STREAM or no stream), and, with
  `:before`, what changed. Not a transport plan.
- **SW — service transition** (`report-service-transition`). Takes `:before`, `:before-provenance`,
  `:state` and `:provenance`. Compares SD's passage services between two settled states, each
  service labelled:
  - **KEPT**;
  - **KEPT BY ALTERNATIVE** (OVERRIDE when a jam keeps it): lists providers lost and remaining;
  - **LOST**: lists providers lost;
  - **GAINED**: lists the new provider;
  - **blocked both**.
  
  This is SD's concrete counterpart.
- **RA — supplied arrangement stability** (`tech/constraint-arrangement.lisp`, load after the
  profile). `(report-relay-arrangement <scenario>)` takes the relay-view keys (`:state`,
  `:provenance`, `:phase`, `:hops`/`:chains`, `:gate-premises`) plus `:view` (`:physical` or
  `:recording`) and `:receiver`.
  - **Method:** treats the configuration as supplied premises and runs only the engine's bounded
    propagation fixpoint, twice to confirm a fixed point. It applies no gate forcing, action or
    release event. Support cycles are rejected before any height query.
  - **Status:**
    - `STABLE-AND-BEAM-WORKING`: configuration intact and the receiver active;
    - `SETTLED-BUT-FAILED`: the configuration changed, or the beam failed;
    - `INCONSISTENT`: the engine rejected the state or hit its bound, with no impossibility claimed;
    - `INVALID`;
    - `UNRESOLVED`.
- **BT — boundary and support transition** (`tech/constraint-boundary.lisp`, load after the
  profile and `constraint-arrangement.lisp`). `(report-boundary-transition <scenario>)`.
  - **Event:** one recorder boundary or one support-changing action applied to a settled state:
    `:event` is `(:stop <ghost agent>)`, `(:cancel <live agent>)` or `(:action <action form>)`.
  - **Optional keys:** `:agents`, and `:obligations` such as
    `(:fact (open <gate>) :phase :across)`, `(:body <b> :role (:weight <plate>) :phase :across)` or
    `(:reach (<agent> <location>) :phase :until-event)`.
  - **Prerequisites** are read apart from effects, with the engine's applicability beside them;
    disagreement is flagged.
  - **Unmet prerequisites:** a closure is then evaluated only as a HYPOTHETICAL, through the engine's
    own cycle-closing function, and compared with the engine successor.
  - **Reports:** removed and added facts, supports and retained chains, plates in each layer, beams,
    changed S1 primitives and their devices, ghost objects removed, each agent's route conditions
    in its own view (arcs lost and gained), and each obligation's status.
  - **Not claimed:** reachability of the state or event, a plan, or global necessity.
  - Every state is evaluated on a private copy.

### ME — Memory estimate

Closed 2026-09-26 and removed (`tech/constraint-memory-estimate.lisp` and the engine's
`*max-states-processed*` no longer exist). Search depth limits are set by D's own experiments.

# Constraint-Led Method — Extractor Specifications

Note (2026-10-02): `archive/` and `evidence/` under doc/constraint-led-solving/ were removed; files cited there are in git history.

Method level, domain-general. The specification of every static component
in `tech/constraint-profile.lisp`, gathered here by T18 (2026-09-25) from the
places each was first written. Text in fenced blocks is copied verbatim from
its source, which is named above it; it keeps its original references
(e.g. "sealed", "register 7.x", crelay-topo examples) as history. The
sources themselves are unchanged: the crelay-topo register is frozen, and
the other sources are in `archive/` or were in `doc/problems/crelay-topo/archive/`
(deleted 2026-09-30; see git history). Paths below under
`doc/problems/crelay-topo/` (the register, `constraint-evidence/`) now resolve
under `doc/constraint-led-solving/archive/crelay-topo-experiment/` (moved 2026-09-30).

New components (T19–T26) add their specification here when their task
writes it. A change to a component is written here, dated, beneath the
component's current text; the old text stays.

## Component index

| Component | Grade | Specification (this file) | Later amendments and implementation record |
|---|---|---|---|
| S0 type extent census | 1 | §2 | frozen register 7.4–7.9 (G2, G5 fixes) |
| S1 control algebra | 1 | §1 | register 7.1, 7.6–7.7 (G1 fix: device state vs control aggregate) |
| S2 functional-relation census | 1→2 | §1 | register 7.10–7.17 (G6, G7 fixes) |
| S3 gate-labelled region quotient | 2 | §1 | register 7.18–7.22 |
| S4 cut-keeper table | 2 | §1 | register 7.23 (approved interpretation), 7.24; T31 amendment �10.6 |
| S5 height and reach lattice | 2 | §1 | register 7.30–7.32; archived plan T7 |
| S6 beam sightline table | 2 | §1 | register 7.33–7.34; archived plan T8 |
| S7 landmark graph and orderings | 4 | §1 | register 7.35–7.36; archived plan T9 |
| T6 budget arithmetic | 1 | §3 | archived plan T6 |
| RO role obligations | — | §4 | register 7.25–7.27 |
| G14 fix (RO view index) | — | §5 | register 7.28–7.29 |
| RC relay chain table | 2 | §6 | register 7.37–7.39; archived plan T16; T33 section 6.1 |
| RA supplied-arrangement stability | supplied-state simulation | section 6.2 | T34 (plan and evidence) |
| BT boundary and support transition (not in the profile) | supplied state and event | section 6.3 | T45 (plan and evidence) |
| Launch configuration check (G15) | — | §7 | manual procedure, never applied; input to T20 and T24 |
| MC mechanic coverage | 1 | §8 (§8.7: T28; §8.10: T40; §8.11: T41; §8.12: T42; §8.13: T43) | T19, T28, T40, T41, T42 (plan) |
| BX beam-crossing scenario (not in the profile) | supplied state | §8.10 | T40 (plan) |
| RL relay-lighting scenario (not in the profile) | supplied state | §8.11 | T41 (plan) |
| EQ equipment scenario (not in the profile) | supplied state | §8.12 | T42 (plan) |
| SD services and setup dependencies | 2 | §8.13 | T43 (plan) |
| SW service transition (not in the profile) | supplied state pair | §8.13 | T43 (plan) |
| CC coupling census | 1 (occluder role 2) | §9 | T20 (plan) |
| NH necessity hints | per hint (1 to 2) | §10 | T21 (plan) |
| FH from here (not in the profile) | 1 (sites, candidates 2) | §11 | T22 (plan) |
| PB probe battery (own file, searches) | per row (1, 3) | §12 (§12.8: T29) | T23, T29 (plan) |
| CP cycle-plan check (not in the profile) | per row (1 to 2) | §13 (§13.8: T44) | T24, T44 (plan) |
| ME memory estimate (own file, searches; engine guard) | none (estimate) | §14 | T26 (plan) |

Register = `doc/constraint-led-solving/archive/crelay-topo-experiment/Constraint-Prediction-Register.txt`.
Archived plan = `doc/constraint-led-solving/archive/Implementation-Plan-2026-09-25.md`.
Post-mortem verdicts per component: `Post-Mortem-2026.md` section 1.6 (removed 2026-10-02; in git history).

## 1. S1–S7

Source: register section 3, lines 122–243, sealed 2026-09-19.

```text
3  EXTRACTOR SPECIFICATIONS  [SEALED 2026-09-19]
-------------------------------------------------

S1  CONTROL ALGEBRA                                          grade 1
    Input    every (controls <clause> <device> <polarity>) in the init
             database.
    Algorithm
      1 emit, per device, a Boolean function of primitives:
        normal   -> device == conjunction of clause literals
        inverted -> device == negation of that conjunction
      2 group devices by IDENTICAL clause (set equality).  Within a
        group: same polarity -> EQUIVALENCE pair; opposite polarity ->
        EXCLUSION pair (never simultaneously active, in any state).
      3 build the dependency DAG device -> primitives -> the devices
        those primitives depend on.  Report depth and any cycle.
    Output   control table; exclusion list; equivalence list; DAG with
             per-device depth.

S2  FUNCTIONAL-RELATION CENSUS                          grade 1 -> 2
    Input    define-types from the problem; the support-occupant and
             support type unions from tech/-support-occupancy.lisp; the
             arity/keying of dynamic relations.
    Algorithm
      1 a relation declared (rel KEY $value) is FUNCTIONAL in KEY.
        Report every such relation.  (on support-occupant $support) is
        the one that matters.
      2 intersect the support-occupant type union with the problem's
        declared extents -> the occupant pool.  Same for support.
      3 partition the occupant pool by recording side (plain vs starred
        RECORDING-COPY> partner).
      4 emit the cardinality bound: for any set P of supports,
        |{p in P : occupied}| <= |unheld occupants|, by injectivity of
        the witness map, which follows from (1).
      5 emit the bound separately per layer, since the recording view
        of a plate counts ghost occupants only.
    Output   functional relation list; occupant pool by layer; support
             pool; the cardinality bound with its per-layer readings.

S3  GATE-LABELLED REGION QUOTIENT                            grade 2
    Input    the propagated traversal relations (traverse-via,
             traverse-via>), each with its clause list.
    Algorithm
      1 build a directed graph over locations; label each arc with its
        door family and its kind (walk/stairs/jump/climb).  A fact splits
        into one arc per kind its clauses infer (-traversal's clause kind
        inference); an arc's doors are its clauses' means, i.e. without
        static separators (staircase, edge, floor drive), which are never
        doors.
      2 contract arcs whose door family is EMPTY, treating an arc as
        bidirectional only when authored by traverse-via (not >).
        Record the contraction rule used; a clause naming a ladder or
        screen is NOT empty; one naming only a staircase or an edge is.
      3 emit the quotient, its arcs with clauses, and per-arc
        directedness.
      4 flag any gate appearing on no arc (a gate that blocks no
        movement -- an authoring signal, per the rumin lattice run).
    Output   region blocks; quotient arcs with clause and direction;
             movement-irrelevant gate list.

S4  CUT-KEEPER TABLE                                         grade 2
    Input    S1's control table, S3's quotient, has-position of every
             plate, has-location of every movable object at init, and
             the goal.
    Algorithm
      1 for each quotient arc, read its gate's controller from S1.
      2 locate the controller: which region block holds the plate's
        has-position, or which primitive (a switch) controls it and
        where that switch is reachable from.
      3 flag KEEPER-OBLIGATED when the controller is a plate lying in
        the region on the approach side of the arc: crossing requires a
        body left behind on that plate.
      4 for each object whose init region differs from a region it must
        reach, list the keeper-obligated arcs on its route.
      5 report any arc where the count of objects that must cross
        exceeds the bodies available to hold its keeper (join with S2).
    Output   per-arc: gate, controller, controller region, obligation
             kind; per-object: crossing list; the stranding conflicts.

S5  HEIGHT AND REACH LATTICE                                 grade 2
    Input    *vertical-type-constants*, has-height overrides, location
             levels from the third coordinate, *vertical-reach-limit*,
             and the placement/lift predicates in -support-elevation.
    Algorithm
      1 tabulate each type's height and each support's top.
      2 enumerate achievable TOPS for a carried object, by composing
        support-on-support where cleartop permits: ground, on each
        mobile support, on a held support.
      3 cross the achievable tops against the placement reach predicate
        to emit, for each (agent base, target support top) pair,
        whether a placement is legal.
      4 report every support top UNREACHABLE for placement from ground
        level.
    Output   height table; achievable-top enumeration; the placement
             legality matrix; the unreachable-from-ground list.

S6  BEAM SIGHTLINE TABLE                                     grade 2
    Input    apparatus-coords>, location-coords>, wall-segment>,
             gate-segment>, boundary-wall, has-height; the achievable
             tops from S5; and the beam-visible query itself.
    Algorithm
      1 beam-visible is a define-query with no state parameter and
        reads current state implicitly, so: copy *start-state*, force
        each subset of the gate open-bits directly into the idb, run no
        propagation.
      2 for each (station location, achievable near-elevation, beam
        endpoint object, far elevation) evaluate beam-visible.
      3 collapse over gate subsets to report, per (station, height,
        endpoint): visible always / visible only under gate set G /
        never.
      4 separately report LOS-VIA location occluders that block at the
        interpolated elevation -- a body standing there kills the hop.
    Output   the full visibility table; the gate dependency per entry;
             the location-occluder kill list.

S7  LANDMARK GRAPH AND ORDERINGS                             grade 4
    Input    S1's control algebra and the goal.
    Algorithm
      1 backward-chain from the goal conjuncts through S1: a device
        condition expands to its controlling primitives.
      2 a primitive that is a plate expands to "some occupant on it";
        a switch to "a toggle from a location that reaches it"
        (join S3, reach edges).
      3 emit the landmark set and the greedy-necessary orderings.
      4 state explicitly which relaxation is in force.
    Output   landmark graph; orderings; the relaxation used.

```

## 2. S0

Source: register section 7.4, lines 534–561, written 2026-09-19 as an
addition to section 3 (G2).

```text
7.4  S0 -- TYPE EXTENT CENSUS.  Written 2026-09-19, AFTER S1's score and
BEFORE S0's first run.  Everything in this subsection is dated ahead of
the observation, per M6 and G3's fix; the score follows in 7.5.

WHY THERE IS AN S0 AT ALL.  G2 of Constraint-Schema-Gaps.txt: no
extractor in the sealed section 3 emits type extents, and G1's fix
cannot be written without them.  Section 3 is sealed and is not being
edited; S0 is an ADDITION, specified here on the date it was written,
and it is scored under C2 exactly like a section-3 extractor.  If the
split later changes enough to invalidate section 3, a new register is
written per the prompt's STATUS section rather than section 3 amended.

S0  TYPE EXTENT CENSUS                                       grade 1
    Input   *TYPES*, *TYPE-COMPONENTS*, the signatures in *RELATIONS*
            and *STATIC-RELATIONS*, and the raw bodies of every
            installed query and update.  No problem object name (C3).
    Algorithm
      1 every declared type with its cardinality, its EITHER components
        when it is an alias, and a flag for EMPTY or SINGLETON.
      2 the empty and singleton lists on their own.
      3 every declared relation with an argument position whose type
        specification admits no object -- identically false, grade 1.
      4 every quantifier in a query or update body whose domain is
        statically empty, and every query body that therefore folds to
        a constant.
    Output  type extent table; empty and singleton lists; the
            identically-false relation list; the constant quantifier
            sites and constant predicates.
```

## 3. T6 budget arithmetic

Source: archived plan, T6 entry.

```text
## T6 — Mechanized budget arithmetic

**Goal.** An extractor consuming S1 and S2 that emits the impossibility
constraints currently derived by hand as AM1–AM3: total role cost over disjoint
support sets against the occupant pool, and the resulting "not all of these
devices" statements, with their grades.

**Working name.** Not S5, S6 or S7. Those specifications are sealed in each
problem's register and must not be enlarged or amended; RO set the precedent for
adding a component under its own name.

**Acceptance.** Regenerates AM1–AM3 on crelay-topo from the profile alone,
including the per-segment budget readings, with each claim carrying its grade and
its premises. No problem object names, per C3.

**Contamination scope.** C1 applies: written from the instance and tech semantics
only. No `Backward-*`, `Forward-*`, `Initial-Conditions.txt`, or
`subgoal-solution-*` access.

**Status.** COMPLETE, 2026-09-22. **Approval.** Granted when D selected full
T6 implementation through T13. The extractor reports AM1–AM3 only when its
pressure-plate supports are disjoint; it derives the goal actor and destination
from the goal form, reports the full S2 pool for AM1, and reports both live and
full segment budgets for AM3. Each emitted claim names its S1/S2 premises and
grade. `doc/constraint-method/evidence/t6-budget-arithmetic-checks-2026-09-22.lisp`
ran eight assertions successfully in a clean staged diagnostic, including the
overlapping-support guard. No sealed material was opened.

```

### 3.1 Equivalent control demands (T37, 2026-09-27)

Specified before implementation. Group control facts by the S1 equivalence key:
`(mode canonical-clause-set)`, using CLAUSE-SET-KEY to ignore clause order,
primitive order and duplicates. Equal plate sets alone do not establish
equivalence: different non-plate primitives, clause structure or polarity
remain distinct demands. Group every member, not only disjoint pairs.

BUDGET-ARITHMETIC-BODY-COST-DEVICES returns `(devices support-form)` per
body-cost class, with DEVICES a name-sorted list retaining every member.
Classes retain first-occurrence order; support forms use the existing T6
cost extraction. GATE-COSTS maps each class to `(devices cost)`. Check support
disjointness between classes and sum each class once. Distinct overlapping
classes still decline summed-cost constraints and H1 body-budget hints.
Grouping does not change T6's existing cost model or S1's override qualifications.

T6 prints each multi-device class as one shared control demand, naming its
members and cost. No class rows are added for singleton-only budgets. AM1/H1 totals
count independent demands when classes contain multiple devices. H1 uses the
same grouped budget; per-device hints may still identify individual devices.
A successfully evaluated budget may yield no necessity hint if it is not tight.
When the pooled count has no shortage, the report must not claim to refute
the fully-open assignment.

Acceptance, before runs: canonical/reordered clauses and groups of three
collapse; non-equivalent shared-plate controls (including opposite modes,
different non-plate primitives and different clauses) still fail disjointness.
Input facts are unchanged. On claustro-topo, {gate8, gate9} costs 3 once;
H1 reaches budget evaluation. On crelay-topo, the T6 report equals the captured
pre-change report byte for byte. Compile/load without warnings, retain focused
evidence and regenerate affected output via reporters. No search is needed.

## 4. RO role obligations

Source: `Constraint-Role-Obligations.txt` sections 2–4 (crelay-topo,
2026-09-20), formerly `doc/problems/crelay-topo/archive/` (deleted 2026-09-30; see git
history).

```text
2  PROPOSED COMPONENT: ROLE-OBLIGATION ANALYSIS
----------------------------------------------
Working name RO, deliberately not S5/S6/S7. Do not change those sealed
specifications or silently enlarge S4. Start with conditional allocation
logic; transport and permanent stranding are separate future components.

Inputs:
  - S1 control alternatives, polarity and device-state premises;
  - S2 functional occupancy signature, witness types and view restrictions;
  - explicit requested device/role conditions, with their provenance;
  - an explicit segment description: physical/recording view, open/closed
    cycle, occupant presence, and proved exclusions/commitments;
  - support positions from the instance for role destinations.
Do not equate a graph-required candidate with an unconditional input goal.
Do not infer a segment merely because it would make the budget tight.

Each output obligation carries:
  condition; segment/view; support set; eligible witness set; minimum
  simultaneous demand; exclusions and their reasons; derivation sources;
  proof grade; unresolved premises; status CONDITIONAL or ESTABLISHED.
ESTABLISHED is permitted only when every prerequisite is established in
the stated scope. A conditional statement may be valid without its
condition being shown reachable or necessary. Report these separately.

Algorithm:
  1 Preserve alternative control clauses and their polarity. Obtain
    positive pressure roles only through justified occupancy semantics;
    persistent switches and latches do not require continuous weight.
  2 Build each alternative's distinct support set; repeated mentions of
    one plate cost one witness. Do not sum mutually exclusive alternatives.
  3 Determine per-support eligible witnesses in the specified view.
    Unknown availability stays symbolic; a declared type extent alone
    is not a current availability set. Do not silently choose a layer.
  4 With fully known finite eligibility, check injective assignments from
    required supports to witnesses. A shortage is lack of such a matching,
    not simply a count when different supports have different eligibility.
    With unresolved eligibility, retain the condition and report unknown.
  5 A named body is necessary only if every admissible assignment across
    every surviving control alternative uses it. Report a forced exact
    plate only if every assignment pairs that body with that plate.
    If no assignment survives, report shortage/inconsistent premises;
    do not infer forced membership vacuously from an empty assignment set.
    This is finite allocation reasoning, not action search or a plan witness.
  6 Attach the role's position as a conditional occupancy destination.
    A destination becomes a transport obligation only after its necessity,
    initial position, object-presence history and segment are established.
  7 Print missing prerequisites. Never emit permanent stranding merely
    because allocation is tight or a relaxed graph has an approach-only side.

Initial implementation boundary, when separately approved:
  Plain Common Lisp diagnostic helpers and an independent reporter.
  No problem object names in code. Do not modify the problem, search,
  recorder semantics, S0-S4 outputs, or existing first scores. Prefer
  an explicit scenario input over pretending to infer unproved segments.
  Decide the file/API placement in the implementation proposal.

3  VALIDATION CONTRACT BEFORE ANY IMPLEMENTATION RUN
----------------------------------------------------
These are proposed synthetic acceptance cases, not new scores:
  - Three roles and exactly three eligible witnesses force membership
    of all three witnesses but not a particular role pairing.
  - Add a fourth universally eligible witness: no one witness is forced.
  - Two roles both restricted to one witness produce a shortage even
    if unrelated witnesses make the total pool larger than two.
  - Two references to one support require one witness, not two.
  - Alternatives requiring different supports remain alternatives;
    a body forced in just one alternative is not globally forced.
  - Unknown segment availability yields unresolved obligations.
  - Persistent/inverted controls do not create positive pressure roles
    without a separate derivation.
  - Reaching an explicit agent goal does not invent named cargo goals.
Before a first staged RO run, append exact observable predictions and
the selected segment assumptions to register section 7. Audit results
already known must be marked known regression expectations, not blind
discoveries. Score the first completed run without revising commitments.

4  LAST-KEEPER PROOF CHECKLIST (NOT YET DISCHARGED)
--------------------------------------------------
A proposed impossibility must identify the required cargo destination,
the view and interval requiring pressure, all eligible replacements,
pickup/placement reach on both sides, loaded movement and support routes,
blower effects, fork/stop/cancel possibilities, and preservation by every
relevant action. A cutoff or missing route in S3 proves none of these.

```

## 5. G14 fix: view index for RO's inherited premises

Source: `Constraint-Role-Obligations.txt` section 7 (approved
pre-implementation specification, 2026-09-20).

```text
7  G14 FIX -- APPROVED PRE-IMPLEMENTATION SPECIFICATION
-------------------------------------------------------
2026-09-20, later the same day.  Sections 1-6 above are unrevised.  This is
the specification as it stands BEFORE any of the fix's code exists, written
so the pre-implementation record survives exactly as section 2's did for RO.
The commitments derived from it are register 7.28.  D approved implementation,
the acceptance cases, appending 7.28, and one staged run with a score, and
directed that the work be done in a fresh session.

7.1  THE DEFECT IN MECHANISM

REPORT-ROLE-DEVICE calls REPORT-KEEPER-AXIOMS, which selects axioms by
RELATION-KEYS-A-DEVICE-P alone.  That test asks only whether the state
relation admits the device, and both the playback and the recording gate-state
relations do, so both print.  The scenario's :VIEW never reaches the selection.

What :VIEW indexes and what the axioms are indexed by are two different axes.
ROLE-VIEW-CLASSES maps :PHYSICAL to the live and unpaired layer classes of
BODIES, computed from the bijective layer-pair relation S2 discovers by shape.
The axioms are not indexed by body class at all: they are two distinct STATE
RELATIONS, selected per reading object.  Both sit under one keyword, which is
exactly G14's "indexed by different things".

7.2  THE COMPUTABLE LINK

The gate substrate supplies a VIEW-DISPATCH query: an IF whose test asks
whether the reading object is a recording-shadow object, whose then-branch
reads the recording-side gate state through the shadow query, and whose
else-branch reads the ordinary one.  The shadow-object test resolves to the
GHOST class, because the tracked variable sits at position 2 of the layer-pair
relation, which is the same car/cdr rule CENSUS-LAYER-CLASS already uses.  The
else-branch is therefore restricted to the complement, live and unpaired.

This is G14's sentence read literally: the relation it keys is READ BY A
CONSUMER RESTRICTED TO THAT VIEW.  The index is computed by walking UPWARD
from the state relation to its readers, not downward toward the occupancy
relation.  A downward walk was considered and rejected: it reaches the
occupancy relation through consumers S2 classifies as layer-blind, so it
returns UNINDEXED for both relations and separates nothing.

7.3  DEFINITION, per state relation R and stated view V

  IN-VIEW(V)    some read site of R is governed by a layer-class test whose
                admitted classes are exactly V's classes.
  OUT-OF-VIEW   R has at least one class-governed read site, and none of them
                admits V's classes.
  UNINDEXED     R has no resolvable class-governed read site, or its governing
                tests resolve to no class at all.

The rule is EXISTENTIAL, NOT EXCLUSIVE, and must say so at the claim site.
The playback gate state is also read by ungoverned queries that restrict
nothing.  IN-VIEW therefore means "a consumer restricted to this view reads
it", never "only this view reads it".  Each axiom line carries the count of
view-blind read sites found, which is what keeps the label from overclaiming.

7.4  MACHINERY: WHAT IS REUSED AND WHAT IS NEW

Reused unchanged from S2: LAYER-PAIR-RELATIONS, CENSUS-QUERY-READS-P,
FORM-READS-RELATION-P, FORM-CALLS-QUERY-P, CENSUS-TERM-READS-LAYER-P, and the
interprocedural parameter mapping through :RAW-ARGS behind
CENSUS-CALL-PARAMETER.

New, and the whole substance of the fix:

  1  Resolving a layer-class test to WHICH class, by the position the tracked
     variable occupies in the layer-pair relation -- position 1 live, position
     2 ghost, both or neither unresolved.  S2 deliberately never names the
     class, only counts it, so this capability does not exist yet.
  2  Collecting a read site's GOVERNING conditions with polarity: IF tests
     positive in the then-branch and negated in the else-branch, enclosing AND
     conjuncts, NOT flipping.  S2 collects only AND/OR siblings that mention
     the KEY variable.  The gate dispatch is an IF on a DIFFERENT variable
     than the read's key, so S2's site walk as written reports NONE for both
     relations and the fix would collapse to UNINDEXED/UNINDEXED.
  3  Inheriting governing conditions across a call, so a read inside a callee
     carries the governance of the call site that reached it.  This is what
     indexes the recording-side relation, whose own query body holds no layer
     test.

Nothing here changes which witnesses RO allocates.

7.5  PLACEMENT AND OUTPUT SHAPE

REPORT-KEEPER-AXIOMS IS SHARED: REPORT-KEEPER-DEVICE calls it for every S4
row, and S4's output is generated into Constraint-Static-Profile.txt, whose
S0-S4 prefix is hash-locked.  It must not be touched, and S4 has no view to
index against in any case.  The fix adds an RO-local reporter,
REPORT-ROLE-AXIOMS, as a contiguous callees-first addition inside the existing
RO block, and REPORT-ROLE-DEVICE calls it instead.  No signature churn:
REPORT-ROLE-DEVICE already receives the scenario, so the view is read from
there rather than threaded as a new parameter.

The two existing data lines per axiom stay verbatim and everything new is
appended beneath them, so the diff against ro-first-run-2026-09-20.txt is
purely additive and mechanically checkable.  Each axiom gains an index line
naming its verdict, the governing test, the query the test was found in, and
the classes admitted; then a line giving its view-blind read-site count, with
the existential caveat on an in-view row and the RETAINED marker on an
out-of-view one.  The block ends with a summary line counting in-view,
out-of-view and unindexed rows against the stated view, and two NOTEs: that
the index is over CONSUMERS that read the relation and not over the bodies
whose occupancy derives it, those being different axes; and that the walk
descends IF, AND, OR, NOT and the queries those call, so UNINDEXED is weaker
than "no restriction", in the same voice as S2's existing switch-walk NOTE.

7.6  THREE DECISIONS INSIDE THAT SHAPE

  D1  RETAIN, DO NOT FILTER.  G14 permits either.  RO's printed output IS the
      score record, so dropping a row removes evidence; labelling it satisfies
      "never printed plain beside an in-view conclusion" without deleting
      anything.
  D2  THE EMPTY-TYPE PREMISE IS LEFT EXACTLY AS IT IS.  It is a type-extent
      fact, not a view-indexed one.  Inventing an index for it would be the
      assumed index G14 objects to, in the other direction.
  D3  AN OR DOES NOT GOVERN.  A read inside a disjunction collects nothing: a
      disjunct is an alternative, not a restriction, and treating it as one
      would manufacture an index.  This is a decision, not a limitation.

The no-axiom case is unchanged: the existing UNRESOLVED line prints verbatim
with no index line.

7.7  WHAT MUST NOT CHANGE

  - Constraint-Static-Profile.txt, 640 lines, 41,214 bytes, hash EFD35D39...,
    and its S0-S4 prefix of 41,101 bytes, hash 9E4A9B0A...
  - REPORT-KEEPER-AXIOMS and REPORT-KEEPER-DEVICE.
  - The two trailing lines of REPORT-STATIC-CONSTRAINT-PROFILE.
  - Every other RO line: the SCOPE block, the segment block, the requested
    conditions header, the device header and its requested/provenance line,
    the alternative header, the per-support eligibility lines, the UNRESOLVED
    restriction caveat, the matching verdict, forced members, forced pairings,
    destinations, the grade/status line, sources, unresolved premises, and the
    closing no-transport line.
  - Register 7.26, 7.27 and 7.28, and sections 1-6 of this document.

C3 HOLDS.  The new code names no substrate relation.  The layer-pair relation
is discovered by shape through LAYER-PAIR-RELATIONS and the dispatch query by
walking the installed query names.  Every query and relation named in the
output is data the walk found, never a symbol in the source.

7.8  ACCEPTANCE CASES, TO RUN BEFORE THE FIRST STAGED RUN

M9's order is a commitment, not an intention.  7.27 disclosed that RO's checks
ran after its staged run; repeating that would be the same deviation twice.
These run in a clean image first, with evidence retained in
constraint-evidence/.

  Class resolution by position
   1  Tracked variable at layer-pair position 1 resolves live; at position 2,
      ghost.
   2  At both positions, or at neither, resolves to nothing.  No guess and no
      default class.
   3  Interprocedural: the term calls a query whose own body reads the pair
      relation, the variable mapped through :RAW-ARGS.  Resolves correctly.
      This is G7's lesson -- the class test lives in the callee, which is
      exactly where the gate dispatch puts it.
  Governing conditions and polarity
   4  A read in an IF then-branch collects that test positive; in the else-
      branch, negated; inside NOT, flipped again; nested IFs accumulate both.
   5  A read as an AND conjunct collects its sibling conjuncts as governing.
   6  A read inside an OR collects nothing (D3).
  Class admission
   7  One positive ghost test admits (ghost); one negated ghost test admits
      the complement (live unpaired).
   8  Two governing tests naming opposite classes admit nothing, giving
      UNINDEXED-contradictory and never IN-VIEW.  This is the empty-set trap
      ROLE-FORCED-WITNESSES already guards against in the allocator,
      reappearing on the indexing side.
   9  An unresolvable governing test neither narrows the admitted set nor
      upgrades the verdict; it is counted and printed.
  Index verdict
  10  One class-governed site admitting exactly the view's classes gives
      IN-VIEW; only the other view's classes gives OUT-OF-VIEW; no class-
      governed site gives UNINDEXED with a nonzero ungoverned count; no reader
      at all gives UNINDEXED with zero sites.
  11  Both an in-view governed site and ungoverned sites gives IN-VIEW AND a
      nonzero view-blind count, asserted together so neither can drift from
      the other.
  12  The relation's own asserting update is not a consumer.  Without this
      every axiom is trivially read by its writer and every index is IN-VIEW
      by accident.
  13  A recursive query terminates the walk.
  Non-interference
  14  For 7.26's scenario the allocation helpers return the first run's values
      unchanged: three supports, the same per-support eligibility, PERFECT,
      three forced members, zero forced pairings, three destinations.  The fix
      is reporting-only, and this is what makes that claim testable rather
      than asserted.
  15  REPORT-KEEPER-AXIOMS produces identical output for a constructed axiom
      list.  The S4 path is untouched by construction; this checks the
      construction.

HYGIENE CONSTRAINT ON THE CHECKS THEMSELVES.  Cases 3, 10 and 13 need stub
definitions carrying :RAW-BODY and :RAW-ARGS.  They must use freshly generated
symbols and bind the query-name list with LET, never SETF.  A check that
permanently installs a stub would contaminate the image the staged run then
uses, and that contamination would be invisible in the run output.  The stubs
are disclosed as stubs in the evidence file.

7.9  AUTHORIZATION STATE

D approved all four items: implementation, the acceptance cases run first,
appending register 7.28, and one staged run with regeneration and a score.
D directed that the documentation be made current and the work done in a fresh
session, so no code was written in the session that produced this section.
The next session implements against 7.28's commitments, which are not to be
revised.  No solve, no action search, and no sealed-file access is authorized.
```

## 6. RC relay chain table

Source: archived plan, T16 entry.

```text
## T16 — RC relay chain table (G17's proposed fix)

**Goal.** Implement the component G17 proposes: relay-to-relay sightlines,
source-to-receiver relay chains, and each chain's gate set and body cost, so
AM4's in-cycle reading no longer rests on hand geometry and S4's receiver row
has an extractor behind it.

**Working name.** RC. Not S6: S6's specification is sealed and is not amended.
Same precedent as T6 and RO.

**Scope.** A new block in `tech/constraint-profile.lisp`, printed by
`REPORT-STATIC-CONSTRAINT-PROFILE` after S6. (DECIDED during coding: the
block is defined after S4, not S6, because it reuses S4's pressure-clause
helpers and the file is callees-first.) It computes:
1. stations: every location and every connector top achievable there
   (location level plus each S5 placement support's top plus connector
   height), each with its riser cost (ground 0; box 1; held tray 2, the tray
   and its holder) and whether a pressure plate is positioned there;
2. links: station–fixed endpoint and station–station visibility through
   `BEAM-VISIBLE` on copied start states. Required gates are found by testing
   with all gates open and then with each gate closed in turn. This assumes a
   link is blocked exactly when some gate on it is closed (monotone,
   conjunctive), which is checked against S6's full-subset rows;
3. chains: simple paths transmitter → relays → receiver, where the first and
   last relays are connectors (fixed couplings are counted and reported but
   not enumerated), at most one connector per location, at most as many
   connector stations as the connector pool, each repeater at most once;
4. per chain: gate set (rejected if it contains an S1 exclusion pair), LATCH
   when it needs a device its own receiver controls, pressure plates its gates
   need, and bodies: connectors plus risers, those standing on plates, and the
   off-plate count;
5. per receiver: gates common to every bootstrap chain, and the minimum
   off-plate body count overall and per number of connectors used.

**Not in scope.** Recording-view (ghost-only) sightlines; hue conflicts
between several transmitters; fixed-coupling corridors; reachability of any
placement; segment assignment (G8).

**Acceptance.** Written before code, 2026-09-24:
- Loads with no warnings; `COMPILE-FILE` of the profile reports none; no
  problem object name in the new block (C3).
- For every location and top that S6 also tests, RC's station-to-endpoint
  status and required gates equal S6's (this also checks the monotone
  assumption against S6's 512-subset evaluation).
- The report states its premises and exclusions, prints non-NEVER
  station-to-station links, every chain, and the per-receiver summary.
- Predictions committed in register 7.37 before code are scored in 7.38
  after the first run; a miss is recorded, not adjusted.
- The profile is regenerated by D (M2), never hand-edited.

**Contamination scope.** C1 applies. No `Backward-*`, `Forward-*`,
`Initial-Conditions.txt` or `subgoal-solution-*` access.

**Approval.** Granted in session by D, 2026-09-24 ("proposed fix approved",
for G17).

**Status.** COMPLETE, 2026-09-24. D regenerated the profile on lumpy and ran
the check file (4 passed). The new profile — 1344 lines, SHA-256
`294DC749…` — is byte-identical to the sandbox-generated one (register 7.39).

**Results against acceptance (sandbox).** Checked in a sandbox copy of the
repository (src/, tech/, instance) loaded under SBCL; that copy reproduced the
profile's S6 section byte-for-byte and every non-RC section identically.
- `COMPILE-FILE` of the profile: 0 warnings, 0 style warnings. Only additions
  to the file; no existing definition changed.
- `evidence/rc-relay-chain-checks-2026-09-24.lisp`: 4 assertions passed — 177
  station-to-endpoint rows agree with S6 in status, gates and subset count;
  C3 holds; 96 chains classified consistently; the report prints.
- Profile grows from 938 to 1344 lines.
- Register 7.38: R1–R4 and R6 HIT; R5 PARTIAL. The least off-plate count
  is 2, as predicted, but it is also reached by a two-connector chain whose
  first connector stands on plate2 at location7.
- Findings: 96 chains to the receiver — 40 bootstrap, 12 latch (all at
  location21), 44 excluded on {gate5, gate7}. Gate4 is common to every
  bootstrap chain. At least two bodies are off every plate in every bootstrap
  chain. AM4b is now settled by extractor output, not hand geometry.

**Correction found while specifying.** S5's placement supports include a
tray HELD by an agent (top 3/2), and a grounded tray is inert. T15's AM4a
wrongly excluded connector top 5/2 at location9 and allowed a tray between box
and connector. Corrected in the abstract model and the T15 evidence file; the
crossing-time conclusion and the two-body bound are unchanged.


```

### 6.1 T33: explicit physical/recording scenarios

Specified 2026-09-27 before code; implementation and focused checks approved.
RC's historical enumeration remains geometric and does not assign identities
to stations. T33 adds a supplied-scenario evaluator shared by RC and NH.

`(relay-view-results scenario)` returns physical and recording result plists.
`(report-relay-view-scenario scenario)` prints them. RC, NH and the whole
profile reporter accept an optional scenario; the writer accepts it after
pathname. Without one, both scenario evaluations are UNRESOLVED. Existing
physical geometric candidates and their qualified bounds remain available.
FH/CP keep those geometric qualifications, not scenario verdicts.

Required scenario plist keys:
- `:state`: reference problem-state for the currently staged problem.
- `:complete-state t`: caller asserts a complete engine-valid state, not a
  partial fact list. Placements, supports/holdings, directed pairings, presence
  and environmental facts are inherited explicitly from this state. Heights
  come from engine queries. Full structural validation and settling are T34.
- `:phase`: :ordinary, :open, :closed or :shadow-only. The latter explicitly
  supports legacy capability fixtures without recorder session actions.
- `:provenance`: nonempty text identifying the state and hypothetical facts.
- `:hops`: `(location top endpoint top)` lists, explicit geometric heights.
- `:chains`: lists of actual identities, transmitter -> one or more relays ->
  receiver. Pairings and placements must already exist in the supplied state.
Hops/chains may individually be empty, but at least one test is required.

Optional `:gate-premises` lists `(view gate boolean)` triples, view :physical
or :recording, without duplicate view/gate. They override gate bits on a
private state copy, with no propagation. Other gates inherit reference values.
Premises are printed and never establish controller consistency. Recording
overrides require recording gate support. No ghost, placement, pairing or
cycle is manufactured. Every hop in a chain uses the same scenario.

Physical view uses NIL and includes ghosts actually present in playback.
Recording view uses the engine's mapped ghost selector and presence rules.
A connector's motion view does not select its beam's view. Closed/unavailable
recording context, phase disagreement and missing input are UNRESOLVED.
Malformed supplied chain resources are INVALID. A tested sightline or chain
failure is BLOCKED; CLEAR means only clear now under the stated premises.

Hops use BEAM-VISIBLE-FOR-OBJECT. Chains use view-aware relay links, full-state
lighting and receiver evaluation, including physical beam cuts. Check distinct
identities, connector presence, outgoing pairing capacity and recording-side
pairing policy. Every required link and every relay's source hue must agree;
other beams remain present, so color conflicts are not silently removed.
Report view, phase, assumptions, row verdicts and reasons. Results never claim
stability, reachability, replay validation or exhaustive arrangement coverage.

Acceptance: schema/missing-input checks before staging; both asymmetric gate
cases; mapped live/ghost blockers; present/absent recorded connectors; absent
recording context; pairing capacity/policy and reused-body rejection; direct
engine agreement for hops and chains; ordinary physical regression; unchanged
reference state/static data. Windtunnel supplies a mixed-view regression, not
a new solution. Retain evidence, regenerate reports, update G19/Handoff/board.
No settling, solution replay, action enumeration or search.

Implementation record: 61 checks passed and COMPILE-FILE returned no warnings
or failure. See `evidence/t33-view-sightlines-2026-09-27.md`. Result plists carry
`:view`, `:status`, `:reason`, `:hops`, `:chains` (where evaluated), plus
provenance, gate and phase premises. Row plists carry input/status/reason;
chain rows also expose link results, computed lighting and receiver verdict.
Malformed list shapes surface errors; missing fields return UNRESOLVED.
The current staged visibility/relay capabilities are checked explicitly so
neutral or stale function definitions cannot be mistaken for a real analysis.


### 6.2 T34: stability of one supplied arrangement

Specified 2026-09-27 before code; D approved T34 after completion of T33.
Load `tech/constraint-arrangement.lisp` after `constraint-profile.lisp`.
It is an optional plain Common Lisp diagnostic, not ASDF or include-tech.
`(check-relay-arrangement scenario)` returns a result plist;
`(report-relay-arrangement scenario)` prints and returns it. It is never
implicitly called by a static profile, FH, CP or T33.

Input uses T33's complete reference-state scenario, with additional required
`:view` (:physical or :recording) and `:receiver`. Supply at least one identity
chain ending at that receiver. `:gate-premises` must be empty: this check never
forces derived gate/fan/receiver bits. All supplied dynamic configuration and
primitive controller values are premises from the reference state; static
geometry, types and wiring are inherited from the currently staged problem.
`:provenance` must identify the source and hypotheses. Dynamic gate, fan,
color and receiver facts are only starting caches and are rederived normally.
Plate depression and recording latch/switch memory are retained as historical
edge state; the check neither seeds a cycle nor invents a plate press.

The complete state is the supplied arrangement: preserve every location,
ON support, HOLDING, MOUNTED-ON and PAIRED fact, not only the relay chain.
Report all removed/added facts, separately naming configuration changes and
derived/control changes. This strict whole-configuration comparison makes
unrequested incidental motion visible rather than silently accepting it.

Before any engine height or consequence query, validate full ground relation
arity/types, functional/bijective consistency, physical placement and support
cycles, support capacity/eligibility, mount rules, same-layer holdings,
connector presence/pairing capacity/policy, closed-cycle ghost absence and
declared phase. Reuse staged physical-validation helpers where their rules
apply to runtime states, and use runtime support policy for the live-on-ghost
held-tray exception. Absent ghosts may remain absent; no automatic assignments.
Incomplete or unavailable phase/technology context is UNRESOLVED. Invalid
configuration is INVALID. Unknown action/event requests are not executed.

On a private deep state copy, call the engine's PROPAGATE-CHANGES! and check
STATE-IS-INCONSISTENT. Respect its existing ten-pass bound; no added search or
unbounded iteration. Engine rejection/nonconvergence is INCONSISTENT, without
claiming impossibility or attributing a more precise cause than the engine
reports. Exceptions become UNRESOLVED with the condition text and stage.
Validate the resulting physical configuration again. Run a second propagation
pass on a second private copy and require the same canonical facts and state
metadata. A second-pass change is UNRESOLVED, not established stability.

Use T33's view-specific chain/hop checks on the settled state and check the
chosen receiver's actual derived ACTIVE or RECORDING-ACTIVE fact as well.
STABLE-AND-BEAM-WORKING requires unchanged configuration, convergence,
idempotence, all supplied tests CLEAR, and that receiver active in the chosen
view. SETTLED-BUT-FAILED names motion or failed light even if the final state
itself converged. Return settled state, changes, view results and idempotence
evidence where available. Caller state, static databases and input lists stay
unchanged on every result, including exceptions.

Only immediate existing engine consequences run. Floor launch/drop and wall
sweeps include support motion/landings. Tray-release highest-below settling
is event-driven and is not triggered by merely checking an arrangement;
release, recorder closure, cancellation and future toggles need separate
transition checks. No reachability, survival through future actions, general
gravity, or independently discovered solution is claimed.

Acceptance fixtures: intact working and stationary unlit arrangements;
physical/recording sweep splits; support stack displacement and floor drop;
controller feedback changing a needed gate/fan; invalid types, holdings,
support cycles/capacity and pairings; actual bounded transport loop rejection;
exception preservation; second-pass fixed point and full caller/static
preservation. Windtunnel must separate live-ground conflict from the supplied
successful mixed-view configuration. Evidence, usage, G19, board and Handoff
are updated; no puzzle solve or action-sequence search.

Implementation record: 155 checks passed; compilation without warnings.
Evidence: `evidence/t34-arrangement-stability-2026-09-27.md`. Raw nullary
relation signatures use the engine's T marker; bijective indexes are checked
before canonicalization hides duplication. Mixed ON/HOLDING cycles are
checked before recursive heights, beyond the individual initialization checks.
Result plists expose status/reason, view/receiver/phase/provenance, private
settled state, configuration and all-fact changes, idempotence, actual receiver
activity and T33 view results. Missing capabilities and exceptions remain
UNRESOLVED. Fault injection is confined to the test's diagnostic boundary.


### 6.3 T45: dependencies across a recorder boundary or support change

Specified 2026-09-28 before code, under T45's approved scope (plan, T45
entry). Sources: `tech/-recorder-session.lisp` (START-RECORDER,
STOP-RECORDER, CANCEL-PLAYBACK), `tech/-recorder-cycle-boundary.lisp`
(RECORDER-CYCLE-BOUNDARY-SAFE, CLOSE-RECORDER-CYCLE-STATE!),
`tech/-recorder-core.lisp` (sides, presence, SUPPORT-USE-ALLOWED),
`tech/-recorder-solution.lisp` (RECORDING-AGENT-AT-RECORDER,
RECORDING-AGENT-EMPTY-HANDED, RECORDING-AGENT-CAN-CLOSE),
`tech/-placement.lisp` and `tech/-support-settling.lisp` (local tray release
and highest-below landing), `tech/-support-motion.lisp`,
`tech/support-settling.md`, `tech/-passability.lisp` (OBSTACLE-CLEAR is
read in the agent's own view). Reused: the recorder contract (8.7.2), T33's
mappings and view terms (6.1), T34's structural validation, snapshot and
propagation rules (6.2), T43's settled-state rules, passage objects,
mobility and primitive changes (8.13), and T44's role vocabulary (13.8).
No recorder, support-loss or engine semantics change; no search.

Load `tech/constraint-boundary.lisp` after `constraint-profile.lisp` and
`constraint-arrangement.lisp`. It is an optional plain Common Lisp
diagnostic, not ASDF or include-tech, and never runs inside the profile,
FH, CP, T33, T34 or SW. `(boundary-transition-result scenario)` returns a
result plist; `(report-boundary-transition scenario)` prints and returns it.

**Scenario plist.**
- `:state`: a settled engine state of the staged problem (a problem-state,
  not marked inconsistent, a propagation fixed point, valid under T34's
  structural validation for its phase: :open while a cycle is recording,
  :closed with the recorder spliced and no cycle open, else :ordinary).
- `:provenance`: nonempty text naming the source of the state.
- `:event`, one of:
  - `(:stop <ghost agent>)`: STOP-RECORDER;
  - `(:cancel <live agent>)`: CANCEL-PLAYBACK;
  - `(:action <action form>)`: one engine action, in replay phrase form,
    whose successor opens or closes a recording cycle or changes a support
    relation (ON, HOLDING of a tray, MOUNTED-ON).
- `:agents` (optional): agents whose route conditions are reported;
  default every agent located in the state before the event.
- `:obligations` (optional): a list of plists, each with `:purpose`
  (text), `:phase` and exactly one subject:
  - `:fact`: a ground proposition, read as a dynamic or static fact;
  - `:body` and `:role`: a T44 role, `(:weight plate)`, `(:jam target)`,
    `(:place location)`, `(:hold agent)`, `(:mount gears)` or
    `(:support occupant)`, read as the fact it asserts (13.8);
  - `:reach (agent location)`: the location is in the agent's engine
    mobility closure from its own location, in its own view.
  `:phase` is `:until-event` (needed up to the event, expendable after),
  `:across` (needed before and after) or `:after` (needed after).

**UNRESOLVED, with its reason only, when:** the state or provenance is
missing, or the state is inconsistent, unsettled or structurally invalid;
the event is missing or malformed; `:stop` or `:cancel` is asked without
the recorder spliced, for a non-agent, or when no cycle is recording;
propagation is unavailable; scheduled happenings are present; an agent or
obligation is malformed; an `:action` names no action, or its successor
neither changes the recorder session nor a support relation (outside T45:
a pure toggle or move goes to SW or T34); the successor is not a fixed
point under a second propagation pass or fails T34 validation; or an
exception occurs (its text and stage are kept). An engine rejection of the
successor (marked inconsistent) is INCONSISTENT, not an impossibility.

**Prerequisites, separately from effects.**
- STOP: the agent is a mapped ghost; a cycle is recording; every mapped
  ghost agent is at a recorder and empty-handed (RECORDING-AGENT-AT-
  RECORDER, RECORDING-AGENT-EMPTY-HANDED, one row per ghost agent); no
  HOLDING or ON fact joins a live and a ghost object (each such fact
  listed; RECORDER-CYCLE-BOUNDARY-SAFE).
- CANCEL: the agent is a live mapped agent; a cycle is recording; the agent
  is at a recorder and empty-handed. Ghost dependencies are not
  prerequisites: cancellation discards them.
- Each named agent's RECORDING-AGENT-CAN-CLOSE (empty-handed with a
  recorder in its mobility closure) is printed as information, not as a
  prerequisite: a move to a recorder is a further step, not this event.
- `:action`: the engine's applicability and failure reason.
- Engine agreement: the itemized STOP or CANCEL conjunction must equal the
  engine's applicability of the action (APPLY-ACTION-TO-STATE on a private
  copy); disagreement is printed.

**Effects.** On private copies only.
- STOP and CANCEL: the closure is computed whether or not the
  prerequisites hold: the session facts the action asserts (recording not
  in progress, cycle closed, stopped-by-ghost set for STOP and cleared for
  CANCEL), then the engine's own CLOSE-RECORDER-CYCLE-STATE! (every ghost
  reference removed, recording shadows reset and seeded, propagation).
  When the prerequisites hold, the engine's successor is also computed and
  must have identical facts: effects ENGINE. Otherwise effects are
  HYPOTHETICAL: what closure would do to this very state, not a claim that
  closure is available here.
- `:action`: the engine successor, only when the action applies; an
  inapplicable action has no hypothetical effect (effects UNRESOLVED).
- The successor is validated as above (T34 phase rules, second pass).

**Consequences, before -> after**, read from each state's facts and the
engine's queries on that state:
1. objects removed and added (located, held or mounted in one state only);
2. support relations: ON, HOLDING, MOUNTED-ON removed and added; per
   mobile object, its support chain (supports and holders down to the
   ground, or a wall mount) and engine BASE in each state, labelled
   RETAINED, CHANGED (with both chains; a new bottom support is a landing
   by the engine's policy) or REMOVED;
3. plates: occupants with their layer (live, ghost, unmapped), DEPRESSED
   and RECORDING-DEPRESSED in each state;
4. beams: PAIRED removed and added; per receiver ACTIVE and
   RECORDING-ACTIVE in each state;
5. devices: every other dynamic fact removed and added, grouped by
   relation (physical and recording relations stay separate), and the S1
   primitives whose physical status changed with the devices they drive;
6. route conditions per reported agent, in that agent's own view:
   presence and location; each passage object (T43) passable before and
   after; traversal arcs lost and gained; mobility from its own location
   lost and gained. An agent the event removes is REMOVED, with no rows;
7. obligations: held before and after, and a verdict by phase:
   `:until-event` EXPENDED (lost at the event) or KEPT; `:across` SURVIVES
   or LOST; `:after` MET or NOT MET; any phase not held before the event:
   NOT HELD BEFORE. A LOST or NOT MET row refutes only the supplied plan's
   reliance across this event.

At a recorder boundary the closure removes only ghost references and then
rederives, so every loss it reports depended on ghost state at that
boundary; rows name the ghost objects in the removed support, holding and
pairing facts. This is attribution by the removed facts, not a causal
chain through controllers.

**Not claimed.** That the state is reachable, that the event can be
reached (a move to a recorder is separate), that the successor is part of
a complete plan, that an obligation or boundary is globally necessary, or
that a HYPOTHETICAL closure is available. A T34-stable arrangement carries
no guarantee through this event. Scheduled happenings are not modelled.

**Acceptance checks (T45).**
- A1 rumin-topo's historical 91-action trace is checked against current
  semantics: record whether it replays; if phrases need normalization,
  only argument order may change (no action, object or place), and the
  normalization is recorded. The replay is reference evidence, not a new
  solution.
- A2 rumin, state after action 90, `(:stop agent1*)`: prerequisites MET,
  effects ENGINE, equal to the hypothetical closure and to the replayed
  state after action 91. Gate5's open state is lost, with the ghost
  pairings of the red chain and tray1*'s weight on plate3 removed; gate6
  stays open with plate4 held by live tray1 before and after. Obligations:
  `(open gate5)` :until-event EXPENDED; `(open gate6)` :across SURVIVES;
  tray1 `(:weight plate4)` :across SURVIVES; `(has-location agent1
  location16)` :after MET. agent1* is REMOVED; agent1 loses its arcs
  through gate5.
- A3 the same state, `(:cancel agent1)`: prerequisites NOT MET (agent1 not
  at a recorder), the engine agrees; effects HYPOTHETICAL with the same
  physical losses as A2.
- A4 support removal: rumin, state after action 49, `(:action (put-tray
  agent1* tray1* ground location2))`: live connector1, riding the
  ghost-held tray1*, is CHANGED onto live box1 by the engine's settling;
  its pairings are kept; tray1's chain on plate3 is RETAINED.
- A5 the same state: `(:stop agent1*)` NOT MET, listing agent1*'s holding
  and the cross-layer ON of connector1 on tray1*; `(:cancel agent1)` NOT
  MET, hypothetical effect: connector1 loses its support and rests on the
  ground, with no catch.
- A6 windtunnel-topo, validated final state (17 actions): STOP and CANCEL
  NOT MET; hypothetical closure removes connector1*, receiver1's physical
  ACTIVE and gate2's physical OPEN, while the before-state's views differ
  (receiver1 physically active, not recording-active). T34's stable
  mixed-view arrangement, closed the same way, loses receiver1: stability
  does not carry through the boundary. A support change that is also a
  toggle (agent1 stepping off plate1, from the validated prefix) changes
  physical plate facts without changing recording-view facts.
- A7 engine agreement at every STOP-RECORDER and CANCEL-PLAYBACK in the
  replayed rumin and crelay-topo trajectories: itemized prerequisites,
  hypothetical closure and engine successor agree with the replay's next
  state.
- A8 each UNRESOLVED reason prints only its reason; a move without support
  change is UNRESOLVED; an inapplicable action has UNRESOLVED effects;
  `:stop` on a non-recorder problem is UNRESOLVED; caller state, scenario
  and static data unchanged; no problem name, LABELS or FLET in the new
  code; the file compiles without warnings; T33 and T34 checks still pass;
  stored profiles are unchanged (BT is not part of the profile).

Implementation record, 2026-09-28: 246 checks passed (rumin 165, windtunnel
39, crelay 21, and 7 on each of claustro, corner and phobia); COMPILE-FILE
of `tech/constraint-boundary.lisp` gives no warning. A1: the historical
rumin trace fails at action 7 under current semantics only because the
CONNECT-CONNECTOR and PUT-CONNECTOR phrases use the old argument order;
with that order normalized (no action, object or place changed) all 91
actions replay to the goal. A2-A7 hold as written. At rumin's action-49
state the released rider connector1 lands on live box1; under a
hypothetical cancel it keeps no support. crelay's two CANCEL-PLAYBACK and
rumin's two STOP-RECORDER boundaries agree with the engine and the replay.
T33 (61) and T34 (155) checks still pass; the windtunnel and crelay
profiles regenerate byte-identical. Result plists carry `:status`,
`:reason`, `:event`, `:kind`, `:prerequisites` (rows, met, engine,
agreement, readiness), `:effects`, `:closure-agrees`, `:successor`,
`:supports`, `:chains`, `:plates`, `:pairings`, `:receivers`, `:devices`,
`:primitives`, `:ghosts`, `:routes` and `:obligations`. Evidence:
`evidence/t45-boundary-transitions-2026-09-28.md`.


## 7. Launch configuration check (G15)

Source: `Launch-Configuration-Checklist.md` (T12, 2026-09-22), now in
`archive/`. A manual procedure, never applied to a traversal. Its checks are
input to the coupling census (T20) and the cycle-plan check (T24).

```text
# Launch Configuration Checklist — T12 / G15

Written 2026-09-22 under the approval recorded for T12 in
`Constraint-Implementation-Plan.md`. This is a read-only construction
specification. It neither authorizes code nor makes a traversal, search
recommendation, or action budget valid.

It closes the specification work spawned by G15: a region-graph traversal is
not concrete until its launch configuration survives every intervening
controller transition. The plan remains authoritative for task state; this
document is authoritative for this check's required evidence and result labels.

**Application.** T13 completed on 2026-09-22 by selecting T6 implementation,
not an application of this checklist, so the checklist has not yet been applied
to any traversal. Using it for new source analysis needs its own approved
scope. It does not reopen a closed construction branch by itself.

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
```

## 8. MC mechanic coverage (T19, I5)

Written 2026-09-25, before code, under T19's approved acceptance criteria
(plan, T19 entry). Design basis: `Post-Mortem-2026.md` sections 1.5 and 1.7.

### 8.1 Purpose and grade

MC is the Phase 0 coverage gate (Problem-Solving Guide, step 2). It answers
one question per staged problem: *which included technologies have a declared
static contract, and which do not?* It prints first in the profile. Its
instance rows are grade 1: direct readings of static facts, interpreted by a
contract text that A wrote from the technology's source and that is quoted,
not derived. A contract is a statement of what the tech/ code does; it is
not checked against that code by the extractor.

### 8.2 Coverage rule

- **Domain.** The public technologies of the staged problem: the names in
  `*spliced-tech-names*` (`src/ww-preliminaries.lisp`) that do not begin with
  `-`, sorted by name. The engine recomputes that list on every splice and
  itself relies on it (`vertical-reach-technology-present-p`). Dash files are
  covered through the public technology that nests them (precedent:
  `report-inert-techs`). A public technology nested by another public one
  (e.g. `reachability` under `switch`) is in the domain.
- **Registry.** `*mechanic-contracts*` in `tech/constraint-profile.lisp`, one
  entry per technology name, of one of three kinds:
  - **contract**: MC prints the contract text and its instance rows;
  - **extractors**: existing extractors, named, already carry the
    technology's static consequences;
  - **infrastructure**: the technology supplies geometry, derivation or
    search machinery and imposes no problem-level constraint of its own.
- **Verdict.** A technology in the domain with a registry entry is COVERED
  (with its kind); without one it is UNCOVERED. A registry entry for a
  technology the problem does not splice is not printed.
- **Scope (D, 2026-09-25, option (a)).** Contracts for floor-blower and
  ladder only. Other entries only where an existing extractor already covers
  the technology or it is plainly infrastructure. Anything else stays
  UNCOVERED and is later work.

### 8.3 Registry entries at T19

| Technology | Kind | Basis |
|---|---|---|
| floor-blower | contract (also S1) | §8.4 |
| ladder | contract (also S3) | §8.5 |
| gate | extractors S1 S3 S4 | control algebra, door labels, cut-keeper rows |
| plate | extractors S1 S2 T6 | primitive controllers, occupancy pools, budgets |
| switch | extractors S1 S4 | primitive controllers, controller position and reach |
| tray | extractors S2 S5 | placement pools, height and reach lattice |
| box | extractors S2 S5 | placement pools, height and reach lattice |
| beam-relay | extractors S6 RC | sightlines, relay chains |
| elevation | infrastructure | authors levels; read by S5 |
| walkability | infrastructure | derives walking arcs; read by S3 |
| visibility | infrastructure | line of sight; read by S6 |
| reachability | infrastructure | reach relations; read by S4 |
| topo-lower-bound | infrastructure | search pruning bound |

Deliberately absent (UNCOVERED if spliced): recorder, step, jump, and every
technology crelay-topo does not include. The recorder is read in part by S2
(live/ghost classes) and RO (views), but no component states what recording,
playback and cycles control or require.

### 8.4 Contract: floor-blower

Source: `tech/floor-blower.lisp`, `tech/-floor-blowing.lisp`,
`tech/-gears-fan.lisp`, `tech/step.lisp`.

- **Controls.** The blower is a controlled device: its CONTROLS entry, and
  S1's device state axiom (turning == control aggregate while no jammer jams
  it). In recorder problems each object reads the blower in its own view.
- **Moves.** While turning in an occupant's view, every non-fan occupant
  resting ON the blower is launched, with its stack, to the blower's AIMED-AT
  destination. A fan resting on it is toppled at the source instead.
- **Requires.** An occupant ON the blower (an agent mounts it by step). When
  no floor drive aimed at the destination is turning in an occupant's view,
  an occupant at the destination that is not ON a support falls back to the
  blower's HAS-POSITION location. So a lift is kept only by leaving the
  destination by an exit arc, or by standing on a support there, before the
  stream stops.

Instance rows, one per blower instance (type `floor-blower`), from the static
database: source (HAS-POSITION), destination (AIMED-AT), control (the
CONTROLS entry's clauses and mode, or "no CONTROLS entry"), and the exit arcs
at the destination: every traversal arc read by S3's `traversal-arc-facts`
that leaves the destination (a symmetric arc with the destination at either
end; a directed arc with it as source), printed as kind, other endpoint,
family and relation. Couplings (other devices on the same controller) are
not listed here: S1's exclusion and equivalence pairs hold them, and T20's
census will extend them.

### 8.5 Contract: ladder

Source: `tech/ladder.lisp`.

- **Controls.** Nothing: a ladder has no CONTROLS entry and no state.
- **Moves.** An agent across a climb-kind clause (one naming a ladder) from
  its fact's source to its destination, one way (`traverse-via>`). A
  supported agent at the source lands on the ground at the destination.
- **Requires.** A ladder named in the clause positioned at the arc's source
  (enforced by `ladder-init-check`), every other means in the clause clear,
  and a safe destination.

Instance rows, one per ladder instance: its HAS-POSITION location, then every
traversal arc whose family names it, printed as kind, source, destination,
family, and whether the ladder stands at that arc's source.

### 8.6 Checks (T19 A2–A6)

- **A2.** Before the first run, `evidence/t19-mechanic-coverage-2026-09-25.txt`
  records the expected crelay-topo readings: the verdict list and the blower1
  and ladder1–3 rows, hand-derived from `probs/problem-crelay-topo.lisp`.
  Walking arcs are derived from geometry at staging and are not hand-derivable,
  so the expected exit list names the authored arcs and states that derived
  walking arcs at the destination are checked against `traversal-arc-facts`.
- **A3.** The generated rows match A2 exactly.
- **A4.** Every public technology crelay-topo splices appears once, COVERED
  or UNCOVERED, checked against the problem's include list.
- **A5.** Binding `*mechanic-contracts*` without floor-blower in a LET makes
  MC print floor-blower UNCOVERED.
- **A6.** No problem object names in the MC code; callees-first; no LABELS or
  FLET; the file loads by hand without warnings.

### 8.7 Amendment, 2026-09-26: contracts for jump and recorder, entry for step (T28)

Written before code, under T28's approved acceptance criteria (plan, T28
entry). With these, every public technology crelay-topo splices is COVERED.

**Registry additions.**

| Technology | Kind | Basis |
|---|---|---|
| jump | contract (also S3 S5) | 8.7.1 |
| recorder | contract (also S2 RO CP) | 8.7.2 |
| step | extractors S2 T6 | mounting a plate holds it by weight (S2 pools, T6 budgets); mounting a blower is in the floor-blower contract (8.4); step changes no elevation; gears-mounted fans have no component |

**8.7.1 Contract: jump.** Source: `tech/jump.lisp`.

- **Controls.** Nothing: a jump has no CONTROLS entry and no state.
- **Moves.** An agent across a jump-kind clause -- one naming an edge, a wall
  or a floor drive, or in a bare-level problem naming none across a level
  difference -- (symmetric, or one way when directed), landing on the floor or on a box or held-tray top at the
  far end; locally, onto a box or held-tray top at its own location, and
  down from one.
- **Requires.** The landing at most `*vertical-reach-limit*` above the launch
  elevation (the floor, or the top of the support the agent stands on).
  Edges and floor drives are static and always passable; every other
  clause member a gate, screen or wall. Each member not passable (a
  closed gate, a non-passable screen, every wall) has its top at most that
  limit above the launch. A safe destination. Downward and level landings
  are unrestricted. A grounded tray is no landing, nor the agent's own
  held tray.

Instance rows. A reach-limit line, then one row per direction of every
traversal arc of kind `jump` (`traversal-arc-facts`, in its order; a
symmetric arc prints its stored direction, then the reverse): source,
destination, the clause, and the two floor levels (`LOCATION-ELEVATION`,
staged start). One reading per clause state:
- a clause naming a member that is not a gate, screen or wall: one line,
  no jump across it;
- a clause with gates or screens: an *open* reading (only walls vaulted) and
  a *closed* reading (every member vaulted);
- a clause of walls only: one *walls* reading; an empty clause: one *no
  feature* reading.

For each reading: launch L = the greatest of (destination level − limit) and
(top − limit) over the vaulted members (`TOP`, staged start); raise R =
max(0, L − source level). R = 0 prints "from the floor"; otherwise "raise R
above the floor", which needs a support at the source (S5's tops). Grade 1:
static levels and tops, and the jump predicate as written; passability is a
state reading, so both states are printed.

**8.7.2 Contract: recorder.** Sources: `tech/recorder.lisp`,
`tech/-recorder-core.lisp`, `tech/-recorder-session.lisp`,
`tech/-recorder-cycle-boundary.lisp`, `tech/-recorder-solution.lisp`.

- **Controls.** The recording session, not a device: no CONTROLS entry.
  START-RECORDER opens a cycle. It needs a live agent at a recorder's position,
  empty-handed, with no ghost left from a closed cycle, within
  `*max-recorder-cycles*`. STOP-RECORDER (by a ghost agent) or
  CANCEL-PLAYBACK (by a live agent) closes it.
- **Moves.** At START-RECORDER, each live mobile object's ghost appears where
  the live one is, with its holding, ON and pairing state. While the cycle is
  open, live and ghost bodies both act, each manipulating only its own side's
  objects. Closing removes every ghost and every fact naming one, and
  rebuilds the recording view from live state.
- **Requires.** STOP: every ghost agent at a recorder's position and
  empty-handed, with no HOLDING or ON between a live and a ghost object.
  CANCEL: the live agent at a recorder's position and empty-handed; ghost
  dependencies are discarded. Devices and plates are read in each object's
  own view: the physical view counts every body present, ghosts included;
  the recording view counts ghost occupants only. A live body may stand on a
  ghost-held tray; a ghost never uses a live support. A closed cycle must
  leave persistent progress. Initialization rejects beam crossings, floor
  gears, angled blowers, threats, receiver-controlled blower drives and
  movable wall-fan copies.

Instance rows: the cycles allowed (`*max-recorder-cycles*`, or unlimited);
the live → ghost pairs (S2's `LAYER-PAIRS`, by live name); each recorder
instance with its HAS-POSITION location. The recorder's cycle count is not
stated: that would be new analysis, not a contract.

**8.7.3 Checks (T28 A2–A7).**
- **A2.** Before the first run, `evidence/t28-mechanic-contracts-2026-09-26.txt`
  records the expected crelay-topo MC output: the verdict table, and the jump
  and recorder rows, hand-derived from `probs/problem-crelay-topo.lisp` and
  `tech/-vertical.lisp`.
- **A3.** The generated rows match A2 exactly.
- **A4.** Checked by script: every public technology crelay-topo splices
  appears exactly once, and none is UNCOVERED.
- **A5.** Negative tests: the registry bound in a LET without recorder prints
  recorder UNCOVERED; with `*vertical-reach-limit*` bound to 5, the
  location4 → location6 open reading prints "from the floor".
- **A6.** No problem object names in the new code; callees-first; no LABELS or
  FLET; the file loads by hand without warnings.
- **A7.** In the regenerated profile, every section but MC is byte-identical.

### 8.8 T32 wall-blower contract and RC/CC/NH qualifications

Written 2026-09-27 before code; T32 approved by D.
Sources: `tech/wall-blower.lisp`, `tech/-gears-fan.lisp`,
`tech/-stream-passability.lisp`, `tech/-recorder-blower-shadow.lisp`,
`tech/-support-motion.lisp`, and `tech/gate.lisp`.

MC covers fixed wall blowers and mountable wall gears through wall-blower.
Report each drive's source, destination, stream elevation (default 1), width
(default 3), fixed/removable fan kind, and control clauses. State the exact
body-strike rule base < stream <= top, presence plus own-view activity,
horizontal relocation, support detachment, carried cargo/stack motion,
flush-floor landing, pairing retention and effect recomputation. Fans are
never swept; a wall-mounted fan is not a standing support. Passage arcs
labelled by the drive require it inactive in the actor's view. No claims
that every elevation is swept or that physical and recording states agree.

CC replaces AIMED-AT-implies-lift: floor-gears/floor-blower have the lift
role, wall-gears/wall-blower horizontal-transport, and other AIMED-AT drives
transport. Transport is its own subsystem. Existing floor lift/barrier
checks remain limited to lifts; a wall destination does not acquire a
floor-blower hover/drop obligation.

RC retains geometric chain classes and costs. Each chain is explicitly a
physical-view geometric candidate, never a stable or replayed realization.
For stations at a wall drive's source, report the connector base/top used
by RC, stream contact, destination and view-dependent activity condition.
Even a directly unswept connector has unresolved support motion and
occupancy stability. RC still assumes the connector height used by its
station enumeration; differing connector heights are not enumerated.

Flag a LIVE STATION CONFLICT only when the connector is struck, the drive
is a fixed wall-blower, a required physical gate has identical control
clauses and polarity, and S1 proves OPEN and TURNING equal their control
aggregates for those devices. Otherwise report a conditional sweep, without
claiming required fan activity. Shared clauses alone are insufficient if
jamming overrides survive. This same-view implication never determines a
ghost's recording fan state. Recording sightlines, body/view assignment,
support motion and stable simultaneous occupancy remain UNRESOLVED.
No conflict prunes a geometric chain or rejects a mixed-view solution.

NH H3 carries the same qualification and exposure details. Its S1
receiver-controlled-device condition remains necessary; common gates and
body bounds apply only within the enumerated physical geometric candidates,
not to all recording-view beams or full feasible solutions. RC's absence of
a bootstrap candidate is not a proof that a receiver cannot activate.

Acceptance: before staged reporting, pure fixtures check below-stream,
inclusive-top and exclusive-base boundaries and floor/wall/other roles.
On copied windtunnel states, invoke the real sweep update to check both
asymmetric live/ghost fan-state cases and pairing retention. An authored
high-stream fixture checks swept versus unswept heights through the real
update; static heights are established before staging. Check the live-ground conflict, an unswept raised station, failure
to infer activity with live overrides, and explicit RC/NH unresolved output.
Check mountable wall-gears presence/absence using the existing wall-blower
test instance. Compare the floor-blower contract/instance output against
pre-T32 code and check its lift role still feeds lift/barrier analysis.
No search, puzzle solve or solution replay. Preserve the original profile,
regenerate through the reporter and update the Handoff and gap G19.

### 8.9 T35: fixed beams, jammers and stairs

Specified before code, 2026-09-27. MC adds three source-grounded contracts.
No technology semantics or search settings change. New diagnostic data comes
from staged facts, type extents and queries, never named problem objects.

**beam-direct.** Each oriented COUPLED pair gets a fixed-corridor record:
BEAM-VIA presence and authored obstacles; LOS-BARRIER-CROSSINGS> records or
UNRECORDED; gate candidates from both sources; authored location blockers;
endpoint chromas and the staged physical corridor-clear result. Missing
BEAM-VIA is distinct from an empty obstacle list. Recorded gates block only
where their finite vertical span intersects the interpolated beam; authored
gates without recorded geometry use the open-only rule. Location occupants
block only when they span the interpolated height. Direct receiver activation
also requires matching chromas and no BEAM-CUT; repeater-origin links require
upstream lighting. Other beam mechanisms may supply a receiver independently.

MC prints these records, RC adds the same fixed rows alongside connector
chains, and H3 adds a CANDIDATE row for each fixed link ending at a receiver.
Gate names are potential occluders, not unconditional required-open claims.
No enumeration of mixed fixed/relay chains, propagation, stability, future
occupancy, recording-view evaluation or reachability is implied. Existing
profiles without beam-direct acquire no fixed rows.

**jammer.** The contract describes carry, inert placement, target placement,
pickup clearing the jam, and target-dependent override (gate open, blower
stopped, gun safe). List each jammer/target's visible placement sites under
all-gates-open geometry, with gates whose individual closure blocks the sight.
The surveyed sites are ground at every location, fixed plates at their authored
positions, and boxes at their staged positions/heights. This is not exhaustive
for moved/stacked supports, trays or fans. Use the engine placement sight query
with physical selector NIL on private state copies. Report each authored
JAM-DISALLOWED> triple in agent-location / placement-location / target order.
Those exclusions, reach, legal support use, occupancy, availability and actual
view must still pass for JAM-TARGET. Gate bits are hypothetical; controllers
are not propagated. No placement or action is asserted to be reachable.

**stairs.** List every stairs-kind traversal arc (a clause naming a staircase),
its direction and alternative door family (the doors beside the staircase;
NIL for the staircase alone). MOVE requires all means in the chosen clause passable for the
mover and a safe destination, with no elevation-difference/equality restriction.
A symmetric arc permits both directions; directed arcs permit only the authored
one. Empty hands are required only when the clause's means impose that rule.

Acceptance before completion: claustro MC zero UNCOVERED; fixed MC/RC/H3 rows
name gate1 and location2 for receiver1; engine query agreement and missing vs
empty corridor distinctions; finite-height gate and occupancy qualification;
jammer sightlines, gate assumptions and directional exclusions; stairs row
and contract. Verify input state/facts unchanged, compile without warnings,
regenerate claustro through its reporter, and compare full crelay/windtunnel
profiles byte for byte against pre-change captures. No search or replay.

### 8.10 T40: beam crossings and cut order

Specified before code, 2026-09-28. Source: `tech/beam-crossing.lisp`,
`tech/-beam-crossing-coordinates.lisp`, `tech/-beam-substrate.lisp`,
`tech/beam-relay.lisp` (liveness and lighting) and `tech/beam-direct.lisp`.
No technology semantics or search settings change. Every row is read from
staged facts or engine queries; no problem object is named in code (C3).

**Contract (MC, kind contract).**

- *Controls*: the derived CROSSING-ACTIVE of each crossing in the published
  pool CURRENT-BEAM-CROSSINGS, recomputed in every propagation before relay
  and receiver status.
- *Moves*: nothing.
- *Requires*: a crossing is active when both of its beams are live for
  cutting and each reaches it: no active crossing lies earlier on that beam
  from its live source, and no closed gate's BEAM-CROSSINGS-BEFORE-GATE> list
  for that direction omits it. Live for cutting: transmitter to a location
  whose connector is paired with it, lit or not; a lit connector to a paired
  receiver or repeater, or to a location whose connector is paired with it
  either way; a fixed coupled beam from a transmitter with a clear corridor,
  or from a lit repeater. Liveness does not test current visibility, bodies,
  walls or receiver hue; only gates split a crossing sequence, and fixed
  coupled beams have no split (a closed gate on the corridor removes the
  whole beam). An active crossing cuts every beam through it: a cut link
  neither lights its relay nor activates its receiver. The set is iterated
  to a fixed point; a two-set oscillation is resolved by validating the
  union, else by arbitration nearest-to-source first (name tie-break), else
  the state is inconsistent. Crossings are proper 2D intersections, with no
  height filter. Recorder problems reject crossings at initialization.

**Static instance rows (grade 1).** Printed under the MC contract:

1. The pool: its size, or that no pool is published. With CROSSINGS-ALONG-
   BEAM> facts but no pool the engine's crossing loops are empty, so every
   crossing is inert; that is reported, not repaired.
2. One row per stored directed beam (CROSSINGS-ALONG-BEAM>), in the stored
   order, which is from its source. A location-to-location beam appears in
   both directions. Each crossing names its other beam. Each gate with a
   BEAM-CROSSINGS-BEFORE-GATE> fact for that direction is inserted as
   `|gate|` after the crossings its list contains. A gate named by the beam's
   LOS-VIA with no split fact is listed as unsplit.
3. The count of potential beams with no crossing.
4. A crossing index: each crossing's two beams and its position on each, and
   its point where coordinates exist. The point is recomputed only with the
   engine's own BEAM-COORDINATES-CROSSING-RECORDS against the published pool
   (the check the engine's own second init pass makes), as the first beam's
   endpoint plus its parameter times the beam's extent.

These are possible crossings. None is claimed active, and no row claims
that its two beams can be live together, or any reachability.

**Supplied-state scenario (not in the profile).**
`(report-beam-crossing-scenario <scenario>)` prints, and
`(beam-crossing-scenario-result <scenario>)` returns, the evaluation of one
explicit state. The scenario is a plist `(:state <problem-state>
:provenance "<text>")`. It is UNRESOLVED, with a reason and nothing else,
when: beam-crossing is not spliced; no pool is published; the state is not
a problem-state; provenance is missing; the state is marked inconsistent;
propagation is unavailable; or the state is not a propagation fixed point
(a private copy changes under PROPAGATE-CHANGES!). A replayed state is
settled; a hand-built one must be settled first (T34 check, section 6.2).

Otherwise it reports, all on that state and with the engine's queries:

- gate states read (not premises and not re-derived), and the active set;
- whether COMPUTE-ACTIVE-BEAM-CROSSINGS returns the stored set (fixed point);
- each active crossing with its two beams;
- each directed beam live for cutting, its BEAM-CUT value, and each of its
  crossings in source order labelled ACTIVE; REACHED, INACTIVE with the
  other beam NOT LIVE or NOT REACHING; BEYOND CUT at the earlier active
  crossing; or BEYOND CLOSED gate. A label for a crossing that is not
  reached is printed only when the engine's BEAM-REACHES-CROSSING says it is
  not reached; a reason the source rules do not identify is UNEXPLAINED.

The result holds for that state only. It is not a reachability result, a
stability result beyond the fixed-point check, or a claim that separately
evaluated beams compose. The caller's state is not changed.

**Acceptance checks (T40).**

- A1 corner-topo: the rows reproduce the stored order of every directed
  beam; gate1 follows all crossings of transmitter1->location4 and
  transmitter2->location4, precedes all of location4->location1 and follows
  all of location1->location4. Each crossing lies on exactly two stored
  beams; the engine records' parameters increase along each stored order.
- A2 inactive beams: at the start state nothing is live or active; with one
  transmitter pairing its crossings are REACHED, INACTIVE, other beam NOT LIVE.
- A3 earlier cut shielding a later crossing: the validated corner trace's
  14-action prefix has crossing7 ACTIVE, transmitter1->location4 BEYOND CUT
  for its later crossings, crossing12 reached by location2->receiver1 but
  inactive, receiver1 active; the 15-action state has no active crossing.
- A4 gate: a settled fixture with gate1 closed has an active crossing on
  the source side of the gate; a settled fixture has a live beam whose
  crossings lie beyond closed gate1, labelled BEYOND CLOSED gate1, and
  inactive for that reason.
- A5 unresolved inputs (each reason above) and caller state unchanged.
- A6 profiles of problems without beam-crossing are byte-identical;
  corner-topo changes only in MC. The diagnostic compiles without warnings.

**Clarification during implementation, 2026-09-28.** The engine evaluates a
crossing's beam from its canonical endpoints, trying the name-first endpoint
as source first. A location-to-location beam live both ways is therefore
reported once, from its name-first endpoint, not once per direction. The
static rows still list both stored directions.

### 8.11 T41: competing colors and persistent connector links

Specified before code, 2026-09-28. Sources: `tech/beam-relay.lisp`
(actions, COMPUTE-RELAY-LIGHTING-FOR-OBJECT, RELAY-LINK-CLEAR-FOR-OBJECT,
RELAY-BEAM-REACHES-RECEIVER and its recording twin),
`tech/-interaction-policy.lisp` and `tech/-recorder-core.lisp` (pairing and
location policy), `tech/-recorder-receiver-shadow.lisp` (recording view),
T33's section 6.1 and T40's section 8.10. No technology semantics or search
settings change. No problem object is named in code (C3).

**Contract (MC; beam-relay moves from extractors to contract, also S6 RC).**

- *Controls*: the derived COLOR of every relay (connector or repeater),
  recomputed in each propagation after the crossing set, and each
  receiver's ACTIVE fact (RECORDING-ACTIVE in the recording view).
- *Moves*: connectors only. PICKUP-CONNECTOR lifts one and deletes every
  PAIRED fact it owns and every one naming it.
  PICKUP-CONNECTOR-RETAINING-PAIRINGS keeps them; a held connector has no
  location, so it neither receives nor sends a beam. PUT-CONNECTOR places
  without pairing. CONNECT-CONNECTOR places the held connector at a
  location the agent reaches and stores 1 to *MAX-CONNECTOR-PAIRINGS*
  outgoing PAIRED facts, to termini structurally visible from any location
  the agent can walk to, never to a connector at the placement location.
- *Requires*: lighting runs in propagation layers from every transmitter
  (layer 0). A relay settles in the first layer in which any clear link
  from a lit source reaches it: one hue lights it; two or more hues in that
  layer leave it dark, and a dark relay feeds nothing; any hue arriving in
  a later layer is ignored. A connector is also dark when a connector at its
  location is already lit. A clear link is a stored PAIRED fact in either
  direction (COUPLED for fixed apparatus), a live sightline in the reading
  view, and no cut by an active crossing (physical view only). Pairings
  persist while their beam is blocked or cut; only pickup clears them.
  Capacity counts a connector's outgoing pairings; incoming links are
  unlimited. A receiver is reached only by a relay of its hue whose own
  outgoing pairing (or coupling) names it, visible and uncut; beam-direct
  may reach it independently. CONNECT-CONNECTOR needs no lit connector of
  the same recorder layer at the placement location. Layers are propagation
  order, not travel time or action order.

**Static instance rows (grade 1, candidates grade 2).** Under the contract:

1. Capacity, and the pools: transmitters and receivers with their hues,
   connectors, repeaters with their authored couplings.
2. Start-state links: per connector its outgoing pairings (count against
   capacity) and incoming links, or none.
3. Direct-feed competition per RC station (location and top): the
   transmitters visible from it, grouped by hue, each with the gates RC's
   monotone reading requires open. A station seeing two or more hues is
   marked COMPETING HUES. The rows are geometry only (start-state copies,
   forced gates, no crossings evaluated). A note states the consequence:
   pairing one connector to two hues in one layer leaves it dark, and a
   direct transmitter link reaches a connector in layer 1, ahead of any
   relayed hue, so a relayed hue lights it only while its direct link is
   unpaired, blocked or cut. Separately possible per-hue routes are not
   claimed to compose.

**Supplied-state scenario RL (not in the profile).**
`(relay-lighting-scenario-result <scenario>)` returns and
`(report-relay-lighting-scenario <scenario>)` prints one explicit state's
lighting. Scenario plist: `:state`, `:provenance`, optional `:view`
(:physical by default, or :recording), optional `:chains` with `:phase` for
T33 reuse. UNRESOLVED, with its reason only, when: beam-relay is not
spliced; the state is not a problem-state; provenance is missing; the state
is marked inconsistent; propagation is unavailable; the state is not a
propagation fixed point (T40's check); or the recording view is asked for
without the recorder technology or an open recording cycle.

Otherwise, on that state, with the engine's own queries (view selector NIL
for physical, the engine's recording view object for recording; the active
crossing set in the physical view, none in the recording view, as the
engine does):

- *Layers*: the propagation replayed layer by layer through the engine's
  link query, each relay with its layer, its arrivals (source, hue) and a
  verdict: LIT hue, CONFLICT (hues), LOCATION ALREADY LIT, UNREACHED, or
  ABSENT (no location, or not present in the recording view).
- *Links*: per relay, every stored pairing or coupling touching it, as
  owned (outgoing) or incoming, with sightline (CLEAR or BLOCKED), cut
  (physical view) and outcome: DELIVERED hue, IGNORED (hue arrived later
  than the target settled), SOURCE DARK, or NOT CLEAR. Pairing, live beam
  and delivered color are separate columns.
- *Capacity*: per connector, outgoing count against capacity (OVER
  CAPACITY is reported, not repaired) and incoming count.
- *Receivers*: required hue, the stored status fact of the view, each relay
  whose own link names it with hue, sightline, cut and verdict (DELIVERS,
  WRONG HUE, DARK, BLOCKED, CUT), and the engine's direct-beam reading.
- *Agreement*: the replayed lit set equals the engine's
  COMPUTE-RELAY-LIGHTING-FOR-OBJECT (relay, hue, layer); in the physical
  view it also equals the stored COLOR facts; each receiver's stored status
  equals the engine's reaching query. Disagreement is printed, not hidden.
- *Chains*: when `:chains` are supplied, T33's RELAY-VIEW-RESULTS for the
  same state and phase, both views. These are the only composed verdicts;
  a chain CLEAR in one state says nothing about another.

The result holds for that state only: not reachability, not stability
beyond the fixed-point check (section 6.2 for a hand-built arrangement),
and not a claim that separately evaluated colors compose. The caller's
state is not changed.

**Acceptance checks (T41).**

- A1 corner MC: beam-relay a contract; zero UNCOVERED; start rows show
  capacity 3 and no links; COMPETING HUES stations reproduce RC's hop rows.
- A2 first arrival and restored direct feed: validated trace prefix 14 has
  connector1's direct blue link CUT, connector1 red in layer 2 from
  connector3, receiver1 delivered and active. Prefix 15 has connector1 blue
  in layer 1 and connector3's red IGNORED, receiver1 dark, receiver3 active.
- A3 same-layer conflict: a settled fixture with one connector paired to
  both transmitters reports CONFLICT, dark, and its receiver link DARK.
- A4 persistence and pickup: prefix 8 keeps connector2's pairing with its
  sightline BLOCKED by closed gate1, and connector1's pairings are gone
  (present at prefix 7). A replayed variant (prefix 13, walk to location2,
  pickup) removes connector3's pairing naming connector1; the retaining
  pickup keeps it, with connector1 ABSENT.
- A5 capacity: prefix 13 connector1 has 3 outgoing (at capacity) and 1
  incoming; a fixture with 4 outgoing is marked OVER CAPACITY.
- A6 joint outcome: T33 chain transmitter1 connector3 connector1 receiver1
  is CLEAR physically at prefix 14 and BLOCKED at prefix 15; recording is
  UNRESOLVED in an ordinary problem.
- A7 views: on windtunnel's validated final state (cycle open) physical and
  recording results are both evaluated, each agreeing with the engine;
  recording without an open cycle is UNRESOLVED.
- A8 agreement holds at every evaluated state; unresolved inputs print only
  their reason; caller states are unchanged. Compile without warnings.
  Profiles of problems splicing beam-relay change only in MC; others are
  byte-identical.

### 8.12 T42: removable equipment and mounted-fan consequences

Specified before code, 2026-09-28. Sources: `tech/floor-gears.lisp`,
`tech/-gears-fan.lisp` (types, BLOWER-PRESENT, UPDATE-BLOWER-STATUS!,
PICKUP-FAN, PUT-FAN, MOUNT-FAN), `tech/-floor-blowing.lisp`
(UPDATE-FLOOR-BLOWING-STATUS!, BLOW-OCCUPANTS-AWAY!, DROP-OCCUPANTS!,
LOCATION-LEVEL), `tech/step.lisp` (STEPPABLE-FIXTURE-AT,
STEP-CONFIGURATION-TRANSITIONS), `tech/-stream-passability.lisp`
(STREAM-OBSTACLE-CLEAR), `tech/-pickup.lisp`, `tech/-placement.lisp` and
`tech/-support-elevation.lisp`. No technology semantics or search settings
change. No problem object is named in code (C3). Wall-stream behaviour stays
in the wall-blower contract (8.8) and fixed floor blowers in 8.4; this
section reuses both and duplicates neither.

**Registry.** floor-gears becomes a contract (also S1 CC). step keeps kind
extractors; its note now says boarding a fixed floor blower is in the
floor-blower contract and boarding a gears-mounted fan in the floor-gears
contract, closing T28's "gears-mounted fans have no component". Profiles
that splice step but not floor-gears change only in that verdict line.

**Contract: floor-gears.**

- *Controls*: the gears' CONTROLS aggregate (uncontrolled default on) unless
  a jammer jams them; TURNING is that state. A stream exists only while a fan
  is mounted (BLOWER-PRESENT) and the gears turn. Turning gears with no fan
  lift nothing; a fan whose gears stop is inert.
- *Moves*: a non-fan occupant resting ON the mounted fan is detached and
  relocated with its stack to AIMED-AT. When no floor drive aimed at that
  destination has a fan and turns, an occupant there not ON a support drops
  back to the gears' location. A loose fan resting on a mounted fan is
  toppled to the ground at the source.
- *Requires*: removal by PICKUP-FAN (empty hands, REACHABLE, vertical reach
  to the fan's base; a floor-mounted fan must also be clear -- an occupied
  fan cannot be lifted). MOUNT-FAN: holding the fan, manipulation allowed,
  the gears' location REACHABLE, no fan already mounted there, vertical reach
  to the gears' working height; mounting on turning gears is legal. A floor
  mount gives the fan the gears' location, making it a flush steppable
  support; a fan lying on the ground or a box is not steppable. Boarding is a
  STEP from the ground at that location onto the clear fan, with support use
  allowed. One fan occupies one mount at a time; any gears type accepts it.
  Recorder problems reject floor gears at initialization.

**Static instance rows (grade 1), under the contract.** One block for every
removable-equipment fact in the problem, so the wall mounts appear here too
(their stream physics stays in 8.8):

1. Fans: each fan and its start state -- mounted on which gears (floor or
   wall; a wall-hung fan has no location), held, or resting (support,
   location) -- and that every gears instance is a compatible mount.
2. Mounts: each gears instance (floor, wall, angled) with kind, position,
   destination and destination level, working height, control, and start
   occupancy (fan or VACANT) with start turning. A turning vacant mount is
   printed TURNING, NO FAN: no stream.
3. Per mount, prerequisites: the locations from which the engine's REACHABLE
   holds in the start state, and whether the working height lies within
   *vertical-reach-limit* of each such location's floor.
4. Per mount, consequences:
   - removal of a mounted fan: the stream stops though the gears keep
     turning; the traversal arcs whose clauses name the drive become passable
     without it; a jam of that drive is then redundant for those arcs;
   - installation on floor gears: the fan becomes a steppable support at the
     gears' location; boarding requires an agent on the ground there and a
     clear top; the lift delivers to the destination while the gears turn;
     the landing is kept only while some floor drive aimed at it has a fan
     and turns, or by standing ON a support there, or by leaving through an
     exit arc (MC's exit list) before the stream stops.
5. A closing line: these rows state compatibility and prerequisites only; they
   do not claim the fan can be carried between mounts, the mount reached, or
   the lift used.

**Supplied-state scenario EQ (not in the profile).**
`(equipment-scenario-result <scenario>)` returns and
`(report-equipment-scenario <scenario>)` prints one settled state's
equipment. Scenario plist: `:state`, `:provenance`, optional `:before`
(another settled state) with `:before-provenance`. UNRESOLVED, with its
reason only, when: no fan type or gears type is populated; the state (or the
before state) is not a problem-state; provenance is missing; a state is
marked inconsistent; propagation is unavailable; a state is not a
propagation fixed point (T40's check); or the recorder technology is spliced
(views not evaluated). Otherwise, with the engine's own queries on that state:

- *Fans*: MOUNTED on gears (floor, at a location; or WALL-HUNG), HELD by an
  agent, RESTING on a support or the ground at a location; BLOWING; and
  STEPPABLE by STEPPABLE-FIXTURE-AT.
- *Drives*: per gears, its fan or VACANT, TURNING, the jammers jamming it,
  and a status: EFFECTIVE STREAM, TURNING NO FAN, FAN MOUNTED STOPPED, or
  VACANT STOPPED.
- *Boarding*: per floor-mounted fan and agent, BOARDABLE when the engine's
  STEP-CONFIGURATION-TRANSITIONS offers a step onto it from the agent's
  configuration; otherwise the first failing condition: AGENT ELSEWHERE,
  AGENT NOT ON GROUND, TOP OCCUPIED, SUPPORT USE NOT ALLOWED. A resting fan
  at an agent's location is reported NOT STEPPABLE (inert).
- *Mounting*: per agent holding a fan, per gears: MOUNTABLE, or the failing
  condition (MANIPULATION NOT ALLOWED, OUT OF REACH, OCCUPIED by the fan
  there, BEYOND VERTICAL REACH).
- *Removal*: per fan not held and agent: PICKUP POSSIBLE, or the failing
  condition (HANDS FULL, OUT OF REACH, BEYOND VERTICAL REACH, TOP OCCUPIED,
  MANIPULATION NOT ALLOWED).
- *Lifts*: per floor gears, its destination and each non-fan occupant there
  not ON a support, SUSTAINED by the named active floor drives, with the loss
  conditions (the drive stops or its fan is removed) and exits.
- *Agreement*: the MOUNTABLE and PICKUP POSSIBLE pairs equal the engine's
  MOUNT-FAN and PICKUP-FAN children (GENERATE-CHILDREN, symmetry pruning
  off); the BOARDABLE pairs equal the step transitions. Printed, not hidden.
- *Transition*, when `:before` is given: fans whose mount, holder or place
  changed; drives whose status changed (STREAM GAINED, STREAM LOST, other);
  mobile objects whose location changed; lifted occupants gained or lost.
  It compares two engine states; it does not say which action caused a
  change, or that the second state follows from the first.

The result holds for that state (or pair) only: not reachability, not a
transport plan, and not stability beyond the fixed-point check. The caller's
states are not changed.

**Acceptance checks (T42).**

- A1 phobia MC: floor-gears a contract, zero UNCOVERED; fan1 mounted on
  wgears1 (wall-hung) at start; both wgears1 and fgears1 listed as
  compatible mounts; fgears1 VACANT, TURNING, NO FAN; wgears1 removal names
  its gated arcs; fgears1 installation names location10 boarding, location11
  lift and its exits. Rows contain no problem name in code.
- A2 removal: validated prefix 11 -> 12 (pickup of fan1 at location2):
  wgears1 STREAM LOST while still turning; fan1 HELD; the transition lists
  it. At prefix 12 the arcs gated by wgears1 are passable without the jam
  (engine STREAM-OBSTACLE-CLEAR).
- A3 inert ground fan: prefix 14 (fan1 on the ground at location4, agent
  there) reports fan1 RESTING, NOT STEPPABLE, no step transition; a fixture
  with the fan resting on the ground at location10 is not steppable.
- A4 mount prerequisites: prefix 52 (agent holding fan1 at location10):
  fgears1 MOUNTABLE; wgears1 OUT OF REACH; a fixture with another fan
  already on fgears1 reports OCCUPIED; with *vertical-reach-limit* bound
  to -1 every mount is BEYOND VERTICAL REACH. Engine agreement at each.
- A5 installation: prefix 53 (fan1 mounted on fgears1): EFFECTIVE STREAM,
  STEPPABLE, agent1 BOARDABLE; transition 52 -> 53 shows STREAM GAINED.
  The full path (54) has agent1 at location11 SUSTAINED by fgears1.
- A6 running gears without a fan: the start state (fgears1 turning, vacant)
  is TURNING NO FAN, with no lift sustained.
- A7 loss of lift: a settled fixture from the final state with fgears1
  jammed drops agent1 to location10; the transition reports STREAM LOST and
  the lifted occupant lost. A fixture with the fan removed (held) likewise.
- A8 unresolved inputs (each reason) and caller states unchanged; engine
  agreement at every evaluated state. Compile without warnings. Profiles
  without floor-gears or step are byte-identical; those with step change
  only in its MC verdict line; phobia changes only in MC. Fixed blowers are
  unchanged (no floor-blower or wall-blower row differs).

**Clarification during implementation, 2026-09-28.** Mounting and removal
list every failing condition, not only the first. The engine records bound
locations after an action's parameters; agreement compares MOUNT-FAN's
(agent fan gears) and PICKUP-FAN's (agent fan) only. A held fan is read
through the bijective HOLDING index, as FH reads it. Three checks are
restated to what the validated trace and the problem support:
- A2: at prefix 11 jammer1 already jams wgears1, so 11 -> 12 shows fan1
  WALL-HUNG -> HELD and wgears1 FAN MOUNTED STOPPED -> VACANT STOPPED;
  prefix 18 -> 19 (the jammer picked up) shows VACANT STOPPED -> TURNING NO
  FAN, the jam no longer needed. STREAM LOST while turning is checked on a
  settled fixture from the start with fan1 moved to the ground, where the
  engine's STREAM-OBSTACLE-CLEAR for wgears1 turns from false to true.
- A4: phobia has one fan, so OCCUPIED is checked on the reason function
  alone, with a second mounted fan added to its fact list; the reach-limit
  case binds *vertical-reach-limit* to -1.
- A7 (and A3's second fixture): from the final state, jammer1 moved to
  location9 jamming fgears1 instead of wblower4; and fan1 left resting on
  the ground at location10, unmounted (not steppable). Each settles with
  agent1 back at location10, STREAM LOST and agent1 aloft lost.

### 8.13 T43: temporary requirements, setup dependencies and service withdrawal

Specified before code, 2026-09-28. Sources: `tech/gate.lisp`
(UPDATE-GATE-STATUS!), `tech/-gears-fan.lisp` (UPDATE-BLOWER-STATUS!,
BLOWER-PRESENT, BLOWER-ACTIVE-FOR-OBJECT), `tech/-passability.lisp`
(OBSTACLE-CLEAR, ALL-CLEAR), `tech/-stream-passability.lisp`,
`tech/-controls.lisp` (CONTROL-ON), `tech/jammer.lisp`, `tech/-mobility.lisp`
(MOBILITY-RESULTS-IN-STATE) and `tech/reachability.lisp` (REACHABLE).
Reused components: S1 control facts, status relations and exclusion pairs;
S3's region rows; MC's jammer survey (8.9) and fixed-beam corridors; RC's
chains with their BOOTSTRAP/LATCH classes (6); CC's driven facts (9); EQ's
settled-state rules (8.12). G15's checklist (7) supplies the separation of
launch, transition and propagated successor. FH and CP are unchanged: FH
reads one state and CP a stated plan; neither is called. No technology
semantics or search settings change. No problem object is named in code (C3).

**Services.** A service is a condition a crossing or the goal needs.

- *Passage service*: every gate, and every drive (the gears and fixed-blower
  leaf types) named in a traversal clause. It holds when OBSTACLE-CLEAR
  does: a gate when OPEN, a drive when not active. Source rules: OPEN iff
  some jammer jams it or CONTROL-ON with default NIL; TURNING iff
  CONTROL-ON with default T and no jam; active iff present and turning;
  a fixed blower is always present, gears only with a fan mounted. Screens
  and ladders are actor conditions (empty hands), not services; they stay
  in door sets as printed.
- *Beam literal*: a receiver ACTIVE, or INACTIVE, where a control option or
  a goal conjunct needs it.
- *Leaf literals*, available in the relaxation with a note: receiver
  INACTIVE (no delivered beam), a plate pressed (a body, T6) or not
  pressed, a switch or toggle state (reach sites, S4), a location clear,
  and gears with no fan (T42 removal).

**Providers.** Each service has options; each option is a conjunction of
premises.

- CONTROL: the S1 aggregate taking the value that makes the service hold
  (true for a gate, false for a drive; INVERTED negates), expanded to DNF
  over primitive literals. An uncontrolled device has no CONTROL option.
  A negation whose expansion exceeds 16 options is one opaque option,
  UNSUPPORTED IN SCOPE.
- OVERRIDE: one per MC jam site (location, support) seeing the target,
  over all jammers; premises are the gates that site needs open. JAM-
  DISALLOWED> rows naming the placement are notes, not premises. A site
  standing on an occupancy location of a fixed corridor that a dependent
  receiver option needs is flagged.
- EQUIPMENT: gears with no fan mounted (leaf).
- A receiver ACTIVE: RC chains ending at it, BOOTSTRAP or LATCH, premises
  the chain's gates, grouped by gate set with counts (EXCLUDED and
  INFEASIBLE chains counted, not options); and fixed corridors to it,
  premises the corridor's gates and its occupancy locations clear.

**Premise closure (grade 2).** An AND-OR least fixed point over services,
from the leaves: a service is available when some option has every premise
available. For each service S it is computed three times: as is, with S
forced unavailable, and with S forced available. Option classes:
DIRECT (no service premise); SUPPORTED (available with S unavailable);
NEEDS S FIRST (available only if S is already provided: installing this
provider requires the service it will provide; a shortest dependency path
is printed); UNSUPPORTED IN SCOPE. Service verdicts: SUPPORTED; SETUP
QUESTION (not available, yet some option NEEDS S FIRST: the cycle needs a
start not in scope, such as the start state or an unenumerated mechanism);
NO PROVIDER IN SCOPE. Each row also gives the start status (goal actor's
OBSTACLE-CLEAR, receiver ACTIVE facts). Control options that require
opposite literals of one primitive are listed as OPPOSED CONTROLS.
Everything is a monotone relaxation: no ordering, simultaneity, body,
reach, occupancy or view is checked. A cycle is a question, not an
impossibility; SUPPORTED is not a realization.

**Requirements by phase.** The goal actor is the object of the first
positive HAS-LOCATION goal conjunct. TRANSIT: minimal door sets from its
start region to its goal region over S3's unreduced rows (one clause per
row crossed; directed rows forward only; minimal under inclusion); a door
in every set is NECESSARY in that graph. RETURN: the same from the goal
region back to the start region. FINAL: goal conjuncts naming a receiver or
a controlled device. TEMPORARY: a passage service in some transit set and
not FINAL: needed while crossing and expendable afterward, unless a return
or a later crossing needs it again. ACCESS: minimal door sets from the goal
actor's start region to every region. RETRIEVAL: each jammer, connector and
fan with its start place and region. Access is to a site's own region;
placement or pickup from another location within reach is not modelled.
S3's kind predicates (jump reach, ladder position) are not evaluated.

**Output: profile section SD (after NH).** Scope and grade; goal actor,
regions, transit and return sets, FINAL and TEMPORARY services; access and
retrieval; one row per service with start status, route role, verdict and
options grouped by class and premises; OPPOSED CONTROLS; setup questions;
a closing statement of what is not claimed. Recorder problems print the
section with a physical-view-only note.

**Supplied transition SW (not in the profile).**
`(service-transition-result <scenario>)` returns and
`(report-service-transition <scenario>)` prints what changes between two
settled states. Scenario plist: `:before`, `:before-provenance`, `:state`,
`:provenance`; optional `:agent` (default the sole agent), `:transit` and
`:return` (location lists) and `:final` (ground propositions).
UNRESOLVED, with its reason only, when: the recorder technology is spliced
(views are T45's); propagation is unavailable; either state is missing,
lacks provenance, is marked inconsistent or is not a propagation fixed
point (8.12's rules); the agent is missing or ambiguous; or a requirement
names a non-location.

Per state, with the engine's queries for that agent: each passage service
passable (OBSTACLE-CLEAR), its state bit, and its providers in force
(CONTROL when the S1 aggregate evaluated on the state's status facts gives
the passable value; JAM by each jamming jammer; NO FAN); receivers ACTIVE;
the agent's mobility set from its own location; each unheld mobile
object's retrievability (some mobility location REACHABLE to its
location). Agreement: passability derived from the providers equals
OBSTACLE-CLEAR; disagreement is printed.

Transition: per passage service KEPT, KEPT BY ALTERNATIVE (providers
changed; OVERRIDE when only jams remain), LOST (naming the providers lost)
or GAINED; supplies withdrawn and added (primitive status changes, jams,
PAIRED facts, fan mounts), each primitive with the devices whose clauses
name it; traversal arcs whose passability for the agent changed (some
clause ALL-CLEAR); mobility locations lost and gained; retrieval lost and
gained. Requirements on the second state: TRANSIT target MET when in the
agent's mobility set; RETURN target MET when the agent's location is in
the mobility set from the target (the agent is not relocated; that premise
is printed); FINAL fact MET when present. The result compares two engine
states: it does not say which action caused a change, that the second
follows from the first, that an ordering is realizable or that a transfer
is safe. The caller's states are not changed.

**Acceptance checks (T43).**

- A1 claustro SD: gate1 has override options only; DIRECT sites include
  location1, location2 and location7 on the ground; plate-room sites NEED
  gate1 FIRST, with a path through gate2 or gate3 and receiver1's fixed
  corridor; the location2 site is flagged as receiver1's corridor
  occupancy; gate1 SUPPORTED. gate2, gate3, gate6 and gate7 have CONTROL
  via receiver1 ACTIVE; gate4 via receiver1 INACTIVE; OPPOSED CONTROLS
  names them. jammer2's retrieval region's access sets name gate5.
- A2 claustro SW on the validated path: 5 -> 6 (box1 put at location2):
  receiver1 withdrawn; gate2, gate3, gate6, gate7 LOST; gate4 GAINED.
  28 -> 29 (jammer1 picked up at location1 after the handover): gate1
  KEPT BY ALTERNATIVE, jammer2 remaining; no service LOST.
- A3 corner SD: gate1 in every transit and return set, not FINAL:
  TEMPORARY; CONTROL via receiver1 ACTIVE by RC chains; FINAL receiver2
  and receiver3 ACTIVE with their chain options; no OVERRIDE (no jammer).
- A4 corner SW 14 -> 15: gate1 LOST with receiver1 withdrawn; with
  `:return (location1)` NOT MET and `:final ((active receiver2) (active
  receiver3))` MET.
- A5 phobia SD: wblower2 CONTROL needs receiver2 ACTIVE, wblower3 receiver2
  INACTIVE, both OPPOSED, both with OVERRIDE options; wgears1 has the
  EQUIPMENT option.
- A6 phobia SW 43 -> 44 (connector1 picked up at location5): receiver2
  withdrawn, connector1's pairings removed; wblower2 KEPT BY ALTERNATIVE
  (OVERRIDE, jammer1); wblower3 GAINED; affected devices wblower2 and
  wblower3; no service LOST.
- A7 closure on synthetic tables: a service whose only option needs itself
  is a SETUP QUESTION; adding a premise-free option makes it SUPPORTED and
  the other option NEEDS S FIRST; an unmet premise gives NO PROVIDER.
- A8 unresolved inputs (each reason) print only their reason; caller
  states unchanged; engine agreement at every evaluated state.
- A9 regressions: every regenerated profile differs from its stored copy
  only by the added SD section; T40, T41 and T42 checks still pass;
  the diagnostic compiles without new warnings; no problem name, LABELS or
  FLET in T43 code.

**Clarification during implementation, 2026-09-28.** On claustro the plain
closure hides the setup dependency: any further jam (for example jammer2 on
gate2 from location1) supplies the plate-room sites' gate premises, because
the relaxation does not count jammers. Each option is therefore classified a
second time with premises supplied only by standing providers (CONTROL,
chains, corridors, equipment; never another jam). When that pass alone gives
NEEDS <service> FIRST, the row prints "through standing providers NEEDS ...
FIRST (path ...)"; a service whose standing verdict is a SETUP QUESTION says
so beside its verdict. A1 is checked on the standing class and path; the
plain class of those sites is SUPPORTED. The summary lists, per service, the
options needing their own service first, and how many do so even with further
jams. Access lists every region, "not reached in the relaxed graph" where no
door set reaches it; an empty door set prints as (). Retrieval prints a
wall-hung fan by its mount and a recorder ghost as absent. SW names each
provider KEPT, and prints a drive's bit as turning or stopped.

## 9. CC coupling census (T20, I3)

Written 2026-09-25, before code, under T20's approved acceptance criteria
(plan, T20 entry). Design basis: `Post-Mortem-2026.md` sections 1.4 and
1.7; the G15 checklist (§7) is an input.

### 9.1 Purpose and grade

CC answers, per staged problem: *which controls or devices change two
subsystems at once?* Latent constraints sit there (post-mortem 1.4: switch1
stops blower1 and opens gate2). Its rows feed the Briefing's "Couplings"
section and, later, the cycle-plan check (T24). CC prints last in the
profile, after S4, since it reads S1, S3 and S6. Grade 1, except the
occluder role, which is read from S6's evaluated rows and is grade 2. CC
states couplings; it does not apply the G15 checklist to any traversal.

### 9.2 Domain and roles

- **Domain.** Every controlled device (third element of a CONTROLS fact)
  and every primitive controller (S1's `control-primitives`), sorted by
  name. An object can be both.
- **Roles**, each read from a static interface, no problem object named:

| Role | Rule | Subsystem |
|---|---|---|
| barrier | named in some clause of a traversal arc's family (`traversal-arc-facts`) | route |
| occluder | a gate in the required-open set of some S6 CONDITIONAL row | beam |
| lift | has an AIMED-AT fact | lift |
| support-controller | a primitive in the extent of type `support` | occupancy |
| beam-driven | a primitive of S1 tier device-mediated | beam |

  Subsystems print in the order route, beam, lift, occupancy.

### 9.3 Rows

- **Role table.** One row per domain object: name, kind (device,
  primitive, or both), roles (or none), subsystems.
- **K1 fan-out.** A primitive named in the clauses of two or more devices.
  The row prints the primitive's subsystems (its own, plus every driven
  device's), then each driven device with its S1 Boolean form and roles,
  then each device pair with its S1 relation: EXCLUSION (identical clause
  set, opposite mode), EQUIVALENCE (identical clause set, same mode), or
  DISTINCT CLAUSES.
- **K2 multi-role.** Every domain object whose own roles span two or more
  subsystems.
- **K3 beam feedback.** For each beam-driven primitive: the devices it
  drives, then its last-hop gates, the union of the required-open sets of
  the S6 CONDITIONAL rows whose endpoint is that primitive. Each gate prints
  its controllers (the primitives of its CONTROLS clauses, or uncontrolled),
  marked SELF when the beam-driven primitive is among them and K1 when a
  controller is a fan-out primitive. A primitive that is no S6 endpoint
  prints "no S6 rows".
- **G15 lift-barrier couplings.** For each K1 primitive, each driven lift L
  with destination D (its AIMED-AT), and each other driven device B that is
  a barrier on an exit arc from D (the same exit rule as MC: a symmetric arc
  with D at either end, or a directed arc from D): one row naming L, D, B
  and each such exit arc. Label by the S1 relation of L and B:
  - EXCLUSION → **G15 FLAG**: B is active only while L is inactive, so
    crossing from D through B starts after the lift has stopped;
  - EQUIVALENCE → compatible: B is active exactly while L runs;
  - DISTINCT CLAUSES → **G15 CHECK**: the relation depends on the other
    literals.
  FLAG and CHECK rows print the checklist §2.2 question: in the successor
  after the toggle, is the launch support at D still present?

### 9.4 Checks (T20 A2–A6)

- **A2.** Before the first run, `evidence/t20-coupling-census-2026-09-25.txt`
  records the expected crelay-topo CC output, hand-derived from
  `probs/problem-crelay-topo.lisp`. Derived walking arcs and S6 rows are not
  hand-derivable; the expected barrier and occluder sets state their basis
  (geometry, cross-read against S3 and S6 in the current profile).
- **A3.** The generated rows match A2 exactly.
- **A4.** Every CONTROLS primitive and controlled device appears once in
  the role table, checked against `control-facts`.
- **A5.** Calling CC's row reporter with control facts bound in a LET
  without the lift (or without the barrier) removes the G15 FLAG.
- **A6.** No problem object names in the CC code; callees-first; no LABELS
  or FLET; the file loads by hand without warnings.

### 9.5 Amendment, 2026-09-25: occluder role read from RC hops

Made after the first run (T20 evidence, part 3). The occluder rule of 9.2
read only S6's rows, which end at a fixed endpoint (transmitter, receiver,
repeater). That misses gates that cut a relay hop between two connector
stations, which is how a beam is actually built (crelay-topo pr21: a ghost
connector plus a live connector). Replaced by:

- **occluder**: a gate in the required-open set of some RC hop, station to
  fixed endpoint or station to station (`relay-chain-endpoint-links`,
  `relay-chain-station-links`). A hop's required gates are those whose
  closing alone blocks it (RC's monotone reading). Grade 2, as before.
- **K3** last-hop gates: the union of the required-open sets of the RC
  station-to-endpoint hops whose endpoint is the beam-driven primitive; "no
  RC hops" when there are none.

RC's station-to-endpoint hops give the same required sets as S6's
CONDITIONAL rows (RC's acceptance check tests this against S6), so K3 is
unchanged. RC evaluates one open state plus one state per gate instead of
S6's every gate subset, so this reading is also cheaper. CC now reads S1,
S3 and RC.


## 10. NH necessity hints (T21, I6)

Written 2026-09-25, before code, under T21's approved acceptance criteria
(plan, T21 entry; scope (a), all seven families). Design basis:
`Post-Mortem-2026.md` section 3.4.

### 10.1 Purpose, labels and grades

NH turns each static limit the profile already states into a candidate plan
element, for the Briefing's "Necessity hints" section (Problem-Solving Guide,
Phase 1 step 3). It is read before the domain interview, so it can shape
D's intuition. NH derives no new constraint: every hint restates rows of
T6, S1, S2, S3, S4, S5, RC, MC or CC as a plan element. It prints last in
the profile, after CC.

- **NECESSARY**: every plan meets it, under the grade and premises shown.
- **CANDIDATE**: one way to meet a limit; other ways may exist.

Each hint carries the weakest grade of its sources, and a graph candidate
stays labelled as one. A hint is not a plan; D checks it in the interview.

### 10.2 Common terms

- **Goal route.** For each positive ground `has-location` goal conjunct
  (S4's goal destinations), the devices S4 calls GRAPH-REQUIRED from the
  object's start region to the goal region. A device on it is marked
  "(goal route)", any other device "(off the goal route)".
- **Regions** are S3's, printed as `R<n> {members}` where a region is named
  as a side of a crossing.
- **Plates of a device** are S4's individually mandatory plates.

### 10.3 Families

Rows print in family order; within a family, in the order given.

**H1 body budget** [grade 1 -> 2; T6 S2]. Inputs are T6's: the body-cost
devices and their plates, total demand T, and the ON pool, live L and all
N (ghosts N - L). When T6's own preconditions fail (no body-cost device, no
pool, or overlapping plate sets), H1 is empty. Let g be 1 when T6's AM2
holds (the goal actor leaves the pool), else 0; S_out = L - g, S_in = N - g.
- H1.a, when T > S_out, NECESSARY: more than S_out held plates needs ghost
  bodies (N - L of them); with no ghosts, at most S_out plates are held at
  once and the rest are met in sequence.
- H1.b, when N > L and T > S_in, NECESSARY: at least T - S_in plates are
  free at every moment; demands are met in sequence.
- H1.c, one per device (by name) whose least single-clause plate demand c
  satisfies 0 < c and c >= S_out, NECESSARY: when c = S_out, opening it
  outside a cycle takes every such body, and any other plate held at the
  same time needs a ghost (or, with no ghosts, none can be); when c > S_out,
  it needs c - S_out ghosts, or cannot be opened when c > S_in.

**H2 keepers left behind** [grade 2, graph candidate; S4 S1]. One hint per
S4 spine crossing direction (device, source region, destination region)
with at least one APPROACH-ONLY mandatory plate, NECESSARY: before crossing
the device into the destination region, leave that many bodies other than
the crosser on those plates. A note is added when the crossing row is
bidirectional and every one of those plates is DEPARTURE-ONLY in the
reverse direction: coming back needs the same plates held, so the bodies
stay until the crosser returns.

**H3 beam-held devices and candidate beams** [grade 2; S1 RC]. For each
controlled device whose clauses name a device-mediated primitive P (S1
tier) that is a receiver with RC chains:
- one NECESSARY hint: the device holds only while P is in its status
  relation; every usable chain (RC bootstrap or latch) needs the gates
  common to all usable chains, less the devices P drives, open, with their
  plates; so while the device must hold, those plates are held, and at
  least the least off-plate body count over bootstrap chains is off plates.
  Note: RC's scope (physical view; least values).
- then, per receiver, one CANDIDATE hint per receiver-end location (the
  location of the last connector before P), in location name order: the
  bootstrap chains ending there with the least off-plate bodies, each with
  its risers, bodies, off-plate count and self-kept plates, in path-text
  order.

**H4 controllers off the goal route** [grade 2, graph candidate; S4 S3 S1].
For each goal route and each switch or latch controller of a device on it
(sorted by name): its manipulation sites (S4 reach sites for a switch; its
position for a latch). For each site, the extra devices are S4's
GRAPH-REQUIRED devices from the route's start region to the site's region,
plus the site's required-open barriers, less the route's own devices. A
hint is emitted when every site has extra devices: NECESSARY when some
extra device is common to all sites, else CANDIDATE listing each site. A
note is added for every pair of route devices the controller drives in S1
EXCLUSION: never open together, so the route crosses them under different
settings of the controller.

**H5 lift landings** [grade 1; CC MC S5]. One CANDIDATE hint per CC G15
row that is not EQUIVALENCE (FLAG or CHECK): keep the lift through the
toggle by a support at the destination (S5's placement supports, less the
ground), then leave through the barrier's exits; or leave the destination
while the lift runs by an exit whose family does not name the barrier.
Exits are MC's, grouped by kind; a note says each kind's own rule is not
evaluated.

**H6 active at the start** [grade 1; S1, start state]. One CANDIDATE hint
per controlled device (by name) whose control aggregate holds in the start
state, each primitive read in its S1 status relation. When some active
primitive is a pressure plate, the device is free while that plate's start
holders (ON facts) stay on it; otherwise it is free until its primitives
change.

**H7 placement limits** [grade 2; S5]. One hint per support top in S5's
"unreachable from ground" list (supports sharing a top grouped), CANDIDATE:
place from a raised location, naming those (level > 0) whose level plus the
placement reach limit reaches that top, or "none" when there are none. A
note says that placement reach from there to the support, and a base raised
by standing on a support, are not evaluated. H7 has no NECESSARY form: S5
evaluates grounded bases only.

### 10.4 Output

A header `NH  NECESSITY HINTS  [grade per hint]`, a READING line, a count
line `hints (n): a NECESSARY, b CANDIDATE`, then one block per family
headed `H<k> <title> (<count>)`, `none` when empty. A hint prints as
`H<k>.<i>  <LABEL>  [<grade>; <sources>]`, then a `limit` line, one or more
`hint` lines and any `note` lines.

### 10.5 Checks (T21 A2-A6)

- **A2.** Before the first run, `evidence/t21-necessity-hints-2026-09-25.txt`
  records the expected crelay-topo NH block, derived by hand from the
  instance and cross-read from the current profile where a source row is
  generated (walking exits, RC chains); each basis is stated.
- **A3.** The generated block matches A2 line for line.
- **A4.** Each source row yields exactly one hint, checked against the
  printed source sections: S4 APPROACH-ONLY directions (H2), S1
  device-mediated devices (H3), off-route switch controllers (H4), CC G15
  rows (H5), T6 shortages (H1), S5 unreachable tops (H7), active-at-start
  devices (H6).
- **A5.** With an occupancy that has no ghost layer, H1 prints "met in
  sequence" and has no H1.b row; with control facts lacking the lift, H5 is
  empty.
- **A6.** No problem object names in the NH code; callees-first; no LABELS
  or FLET; the file loads by hand without warnings.

### 10.6 T31 amendment: shared S3/S4 reduction and NH availability

Written 2026-09-27 before code; approved by D with T31.

S3 and S4 use one deterministic, sequential reduction. Order rows by their
printed data, then consider deleting each row against the retained graph.
Delete only when a replacement uses the same or a subset of its doors
(both directions for a bidirectional row), and full versus candidate graph
reachability agrees from every region with no exclusion and with each named
device excluded. Include doors named by rows, even without control wiring.
Keep the final S4 equivalence check as an independent guard. This is a
reachability-preserving representative graph, not a unique physical doorway
decomposition or a minimum graph. S3 classifications describe this retained
graph, not independent deletion decisions. Unsupported alternative clause
families remain unreduced and S4 returns :ALTERNATIVE-FAMILIES explicitly.

NH route context retains S4's failure reason. H2/H4 print UNAVAILABLE with
that reason instead of `none`; a valid empty graph remains distinguishable
from failed analysis. Unavailability does not count as a hint.

Acceptance before staged use: synthetic directed and bidirectional mutually
redundant rows; all-region reachability with no device and every device
excluded; door-free alternatives; asymmetric reverse paths; deterministic
results under input reversal; unsupported families; explicit unavailable
H2/H4 output and valid empty output. Then stage windtunnel-topo without
search: S4 must succeed, R1 must retain its outgoing reachability, and S3
and S4 must agree on retained rows. Regenerate its profile through the
reporter, retaining before/after evidence and updating the Handoff.

### 10.7 T36 amendment: alternative door families in S4

Specified before code, 2026-09-27. This supersedes 10.6's unsupported-family
rule only. For any quotient containing a row with multiple DNF clauses,
QUOTIENT-REDUCED-ROWS keeps every row, in its existing deterministic order.
KEEPER-SPINE accepts that unreduced graph and retains the final all-region,
all-device exclusion check. Single-clause graph reduction is unchanged.
S4 reports explicitly that alternative families use the unreduced quotient.

A row is usable with a device forbidden when any clause avoids that device;
no family means a direct edge, and an empty clause supplies a door-free
alternative. Preserve directedness. Clause alternatives must never be
conjoined by unioning their doors for reachability. The union of names is
used only to enumerate relevant devices and rows, not to require all doors.

S4 still prints each device's directional graph-cut verdict and plate-side
classification with its existing relaxed-graph and S1 qualifications. H2
must not issue a necessary keeper hint for a device that some clause of the
same row avoids. H4 uses the successful graph context and required-device
queries as before. A valid empty H2/H4 result is distinct from UNAVAILABLE;
real reachability mismatches still propagate as failures. No concrete
stranding, mode applicability or lifecycle proof is added.

Checks before staged use: retain the earlier single-clause reachability,
mutual-redundancy, direction and failure-reporting tests. Update only their
obsolete expectation of alternative-family rejection. Compare mixed and
multi-clause graphs with an independent closure oracle for every region,
with no device and each named device excluded (including uncontrolled doors).
Cover empty clauses, shared doors across alternatives, reversed input,
bidirectional and asymmetric paths. Check that alternatives do not create
false H2 obligations. Then claustro S4 must be verified, give directional
verdicts for gate8/gate9, and evaluate H2/H4. The windtunnel full profile
must equal its pre-change capture byte for byte. No search or replay.

## 11. FH from here (T22, I1)

Written 2026-09-26, before code, under T22's approved acceptance criteria
(plan, T22 entry; scope (a), rows F0–F3). Design basis:
`Post-Mortem-2026.md` sections 1.1 and 1.7 (loop 2).

### 11.1 Purpose and scope

FH answers, at one state, the Phase 3 step 7 question: what can the bodies
reach from here, which controllers they can operate, where held cargo can be
set down, and what the beam is doing. It is not part of the profile:
`REPORT-STATIC-CONSTRAINT-PROFILE` does not call it, and the profile file
does not change.

Every reading is of the state given, not of the start state. Rebinding
`*START-STATE*` and rerunning the extractors would not do: S3–S6 and RC read
the static database, enumerate supports by type, or force gate bits, so
they would print nearly the same tables from any state. FH instead calls
the engine's own queries on the state and its one-step successor generator,
`GENERATE-CHILDREN` (depth-first children, symmetry pruning off, as in
`tech/constraint-state-probe.lisp`). No search runs; nothing is propagated
by hand.

**Entry point.** `(REPORT-FROM-HERE &optional source)`. SOURCE is a search
checkpoint (its endpoint, `SEARCH-CHECKPOINT-STATE`), a list of actions
(replayed by `VALIDATE-ACTION-SEQUENCE` from the staged start, as
`REPORT-APPLICABLE-ACTIONS` does), or NIL (the staged start). A failed
replay is an error.

### 11.2 Grades and labels

- **Grade 1**: a reading of the state, or of the engine's one-step
  successors of it. F0, F1 moves, F1 NOW and AFTER ONE MOVE, F2, and F3's
  live relay state.
- **Grade 2, graph candidate**: F1's site lines for controllers not within
  one move (S4's relaxed region graph) and F3's candidate chains (RC). Both
  read the physical view only; a ghost's recording view can differ.

A line that does not occur in the same section (F0 to F3) of the staged
start's report is marked `*` at its end; the staged start's own report marks
nothing.

### 11.3 Common terms

- **Agents** are the instances of type agent. An agent with no
  HAS-LOCATION is printed "not present" and takes no further row.
- **Configuration** is the engine's `AGENT-CONFIGURATION`: (location,
  ground or the support it stands on).
- **Primitive controllers** are S1's primitives (the members of every
  CONTROLS clause); each is read in its S1 status relation
  (`CONTROL-STATUS-RELATION`).
- **Controlled devices** are S1's devices; each is read in every device
  state relation S1's axioms name (e.g. open, recording-open), all that
  hold being printed.
- **Relays** are the instances of connector, floor-repeater and
  wall-repeater. The beam is read from PAIRED facts (both directions) and
  COLOR, and each receiver from its status relation.
- **Regions** are S3's; the relaxed graph and its required devices are
  S4's (`KEEPER-REQUIRED-DEVICES` over S4's spine).
- **Acting agent** of a successor is the first agent in its action's
  arguments.

### 11.4 Rows

**F0 state** [grade 1]. Agents: configuration, base, what the agent holds.
Objects: every non-agent with a HAS-LOCATION, by name: location, "on X",
"on ground" or "held by A", and top. Devices: each controlled device by
name, with the state relations that hold for it ("-" for none). Primitives:
each primitive by name, active or "-"; an active plate names its occupants
(ON facts).

**F1 reachable and controllers** [grade 1; sites grade 2].
- *Moves.* For each present agent, the configurations of its MOVE
  successors, sorted by name, each with its route's segment labels
  (walk, jump, step, ladder, ...). Transitions are single steps and are not
  closed, as in MOVE itself.
- *Controllers.* For each primitive controller: its status; then
  - **NOW** by the (agent, action) pairs whose one-step successor changes
    its status;
  - **AFTER ONE MOVE** by agent A, listing A's move destinations from which
    A's own next action changes it (two levels of `GENERATE-CHILDREN`, the
    second expanded only through A's MOVE successors and filtered to A's
    actions);
  - otherwise **NOT WITHIN ONE MOVE**, followed for a switch by its S4 reach
    sites and for a plate by its position: per site and per present agent,
    the agent's region, the site's region, and the devices the relaxed
    graph requires between them plus the site's barriers, each with its
    physical state now (open or closed); "needs no device" when there are
    none, "not joined" when the relaxed graph does not join them. A
    receiver has no site: "beam-driven: see F3".

**F2 placements** [grade 1]. For each present agent that holds cargo: from
its current configuration, and from each MOVE successor's configuration,
every location L with `REACHABLE L` from where it stands, and the places
`PLACEMENT-OPTIONS` offers there for the held object (ground first, then
supports by name). Configurations with no option are omitted. An agent
holding nothing prints "holds nothing".

**F3 beams** [grade 1; candidates grade 2].
- *Receivers*: status now.
- *Relays*: for each relay, its location, support and top (a fixed relay:
  "fixed"; a connector with no location: held by A, or "not present"), its
  links (PAIRED in either direction), and its COLOR ("unlit" when none).
- *Candidates*: for each receiver not active now, RC's usable chains
  (bootstrap and latch; exactly RC's chain set, `HINT-RELAY-CHAINS`)
  grouped by the chain's gates that are closed now (physical view). The
  group with none closed is "open now" and lists its chains in RC's path
  text; every other group is one line: the closed gates, each with its S1
  form and its primitives' states now, the chain count by class, the least
  connectors and the least off-plate bodies. Groups print by number of
  closed gates, then names. An active receiver prints "active now;
  candidates not listed".

Chains are not matched to placed connectors. RC's stations carry a single
riser, so a connector on a stacked riser (e.g. a box on a held tray) has a
top no station has; the live relay rows above report such a beam exactly.

### 11.5 Output

A header `FH  FROM HERE  [grade 1; grade 2 where marked]`, a READING line,
a source line (staged start, checkpoint, or action prefix, with its action
count), then F0 to F3 in order, each headed by its name and count line.

### 11.6 Checks (T22 A2–A6)

- **A2.** Before the first run, `evidence/t22-from-here-2026-09-26.txt`
  records the expected FH output for the fresh staging and for
  `t10-c3-location15-checkpoint` (80 actions), derived by hand from the
  instance, the tech rules, the checkpoint's recorded endpoint facts, the
  static facts dumped in `doc/problems/crelay-topo/Initial-Conditions.txt`
  (traversal and reach edges), and the current profile (RC chains); each
  basis is stated, and readings that rest on untabulated geometry are
  marked provisional.
- **A3.** The generated output matches A2 line for line.
- **A4.** By script, on both states: each agent, primitive controller and
  relay appears exactly once; for each agent standing on the ground, F1's
  grounded destinations equal `MOBILITY-LOCATIONS` less its own location
  (a query path independent of the successor generator); each held object's
  F2 current-configuration row equals the (location, place) pairs of the
  one-step successors in which that object leaves the agent's hands; at the
  fresh staging, F3's group counts sum to RC's usable chains.
- **A5.** On state copies with one gate bit forced, no propagation (as RC
  and S6 do): closing a gate an agent's F1 destinations cross removes the
  locations behind it; opening the one closed gate a group needs moves that
  group into "open now".
- **A6.** No problem object names in the FH code; callees-first; no LABELS
  or FLET; the file loads by hand without warnings.

Refined from the plan's first wording, before code (A): A4 checked F1
against `REPORT-APPLICABLE-ACTIONS`, but F1 is built from the same
successors, so that check would be circular; `MOBILITY-LOCATIONS` is an
independent path. A5 spoke of demoting a LIT chain; F3 labels chains by
their gates instead, for the stacked-riser reason in 11.4.

## 12. PB probe battery (T23, I7)

Written 2026-09-26, before code, under T23's approved acceptance criteria
(plan, T23 entry; scope (a), families P1–P4). Design basis:
`Post-Mortem-2026.md` section 3.4 (I7) and section 2.2 (Phase 1 step 3).

### 12.1 Purpose and scope

PB runs small searches from the staged start, one per landmark or resource,
and prints a map of which subgoals are cheap and which are not found within
the problem's maximum depth. It runs before the domain interview, so it can
shape D's intuition. It is the only diagnostic component that searches; it
lives in its own loadable file, `tech/constraint-probe-battery.lisp`, under
the working conventions for those files (plain CL in `:WW`, no `define-*`
forms, callees-first, no LABELS or FLET, no problem object names). It is not
part of the profile: `REPORT-STATIC-CONSTRAINT-PROFILE` does not call it.

PB reads its sources through the functions that print S1, S3, S4 and MC, and
the start state; it adds no static analysis of its own.

### 12.2 Probe families

Probes are generated in family order; within a family, by subject name, then
by location name. Only objects present at the start are subjects: a ghost
body has no HAS-LOCATION there and takes no probe.

**P1 landmark.** For each agent present at the start, and each landmark
location other than the agent's start location: goal
`(has-location <agent> <location>)`. Landmarks are the union of S4's switch
reach sites (manipulation reach candidates), MC's lift destinations
(AIMED-AT of each covered lift), and S4's explicit goal destinations. Plate
positions are not landmarks: P3 covers them.

**P2 resource.** For each cargo object with a HAS-LOCATION at the start:
goal `(or (has-location <object> <l1>) ... )` over every location outside
the S3 region of its start location. Held cargo has no HAS-LOCATION, so the
probe asks for the object set down elsewhere, not merely picked up. A cargo
object whose start region holds every location takes no probe.

**P3 controller.** For each S1 primitive controller: goal
`(<status relation> <primitive>)` when that does not hold at the start, else
`(not (<status relation> <primitive>))`. The status relation is S1's
(`CONTROL-STATUS-RELATION`), read in the physical view.

**P4 relay.** For each fixed relay (the instances of floor-repeater and
wall-repeater): goal `(exists (?c connector) (paired ?c <relay>))`.

A probe whose goal holds at the start is labelled START and not searched.

### 12.3 Search

Each probe is searched from the fresh staging's checkpoint
(`CAPTURE-SEARCH-CHECKPOINT` once, then two-argument `SOLVE-SUBGOAL` per
search; no probe continues from another's endpoint). The caller supplies
the maximum depth D; `*threads*` must already be 16
(the runner checks this and signals otherwise; it does not cross the
serial/parallel boundary itself). The solution type is FIRST for the
battery; every other setting is as staged, and all are recorded.

For each probe the cutoff runs d = 1, 2, ... D. After each run the runner
reads the planner's outcome, `*DEPTH-CUTOFF-TRUNCATED*` and
`*TOTAL-STATES-PROCESSED*`, and stops at the first of:
- a plan found: CHEAP;
- d = D reached without a plan;
- no plan and no truncation at d: the space within reach was exhausted
  before the cutoff, so a deeper run cannot differ.

**Interface (T38, 2026-09-27, specified before code).**
`(RUN-PROBE-BATTERY max-depth pathname &optional probes)` and
`(PROBE-DEEPEN checkpoint probe max-depth)`. D is a non-negative integer;
a false start goal at D = 0 yields NOT FOUND with no runs. Counts remain
measurements only. New settings contain no resource-cap field; no new run
produces STOPPED. Existing FIRST/deepening behavior is unchanged by T38.

### 12.4 Labels and grades

- **START** [grade 1]: the goal holds at the start; not searched.
- **CHEAP** n @ d [grade 1 on replay]: a plan of n actions, found at
  cutoff d. Every run below d found none. No claim that n is least.
- **NOT FOUND** ≤ D [grade 3]: no plan at cutoff D, and the cutoff
  truncated the search (or D = 0 and the start goal check was false).
- **EXHAUSTED** ≤ d [grade 3]: no plan, and no truncation observed at d.
  Pruning (lower bound, symmetry, recorder policy) also limits the explored
  states, so this is still a cost bound, not an impossibility. This is the
  post-mortem's "blocked".

### 12.5 Results and output

The runner writes one readable data file (path supplied by the caller)
holding the settings and one record per probe: id, family, subject, goal,
label, the cutoff at which it ended, plan length and actions (CHEAP),
truncation flag and states per run. `REPORT-PROBE-BATTERY` prints PB from
that file, so the report is regenerated without searching (M2).

A header `PB  PROBE BATTERY  [grade per row]`, a READING line, a settings
line (maximum depth, threads, solution type), a count line by
label, then one block per family headed `P<k> <title> (<count>)`, `none`
when empty. A row prints as `P<k>.<i>  <subject>  <goal>  <LABEL> ...`,
and a CHEAP row adds its plan on following lines. The rows are the
Briefing's probe map.

Historical result files remain readable without rewriting. Ignore obsolete
settings keys. A historical :STOPPED row prints LEGACY STOPPED with its last
completed cutoff and no grade or new bound; include its count only when present.

### 12.6 Checks (T23 A2–A6)

- **A2.** Before the first run, `evidence/t23-probe-battery-<date>.txt`
  records crelay-topo's expected probe list (every row's family, subject and
  goal) and START rows, derived by hand from the current profile and the
  instance, at maximum depth 11; and expected labels where the T10 evidence
  settles them, with the basis named; every other label provisional.
- **A3.** The generated probe list matches A2 exactly; the settled labels
  match.
- **A4.** By script: each landmark location per present agent (P1), each
  cargo object with a location (P2), each S1 primitive (P3) and each fixed
  relay (P4) yields exactly one probe; each CHEAP plan replays by
  `VALIDATE-ACTION-SEQUENCE` from the start with the probe's goal as goal
  test and succeeds; each row not found records, for every run, its cutoff,
  truncation flag and states.
- **A5.** A probe whose goal holds at the start is START and runs no search;
  a CHEAP probe rerun with D below its cutoff is NOT FOUND or EXHAUSTED;
  arbitrarily high state counts never stop deepening before D.
- **A6.** No problem object names in the PB code; callees-first; no LABELS
  or FLET; the file loads by hand without warnings.

**T38 acceptance checks.** Run the new interface on a tiny synthetic problem
for START, CHEAP, truncated NOT FOUND and untruncated EXHAUSTED; replay a
found plan independently. Check zero depth and invalid depth. Simulate huge
state counts and verify all requested cutoffs run. Verify settings restoration,
new result/report shape, and reports of existing old files plus a historical
STOPPED fixture. Load/compile without warnings. No substantial puzzle search.

### 12.7 Amendment after the first run (2026-09-26)

12.2's P2 note, "held cargo has no HAS-LOCATION", was read from
`pickup-box` and does not hold for a tray: `tech/tray.lisp` keeps a held
tray's HAS-LOCATION, synced to its holder, so its riders' consumers keep
working. The P2 goal is unchanged. For a tray it is met by carrying the tray
outside its start region as well as by setting it down there; for other
cargo, only by setting it down. crelay-topo's P2.3 plan carries tray1 to
location9 (evidence part 2, 2.2 a).

### 12.8 Amendment, 2026-09-26: P5, D's own probes (T29)

Written before code, under T29's approved acceptance criteria (plan, T29
entry; T23's declined scope (b), taken up by D's choice of 2026-09-26).

**Purpose.** In the domain interview (Problem-Solving Guide), a trick often
rests on a subgoal D expects to be cheap from the start. P5 runs such
subgoals through the battery's own deepening, labels and report, rather than
as loose searches.

**Input.** `(D-PROBES entries)`, each entry a list `(subject goal
provenance)`: SUBJECT a symbol (the object the probe is about, for the
report), GOAL a goal form, PROVENANCE a string naming where it comes from
(e.g. "interview trick 2"). D-PROBES returns probes numbered P5.1, P5.2 ...
in entry order: plists with `:id`, `:family 5`, `:subject`, `:goal` and
`:provenance`. They are run by passing them to `RUN-PROBE-BATTERY` as its
optional PROBES argument, alone or appended to `(PROBE-BATTERY-PROBES)`.

**Checks at construction, before any search.** An entry that is not a
three-element list with a non-NIL symbol, a cons and a string signals an
error. Every literal head in GOAL must be a declared relation (dynamic or
static) or a function (an installed query, or a Lisp function the goal
translator accepts); otherwise an error names it. The walk descends AND,
OR and NOT through their arguments, and EXISTS and FORALL through their
bodies (the variable list is skipped).

**Unchanged.** P1–P4, the labels and grades of 12.4, the fresh-start
checkpoint, the settings, and the results file format (P5 records carry the
extra `:provenance` key). A P5 goal that holds at the start is START, as in
12.2.

**Output.** `*PROBE-FAMILY-TITLES*` gains a fifth title, "D's own", so every
PB report, including a report of results written before T29, prints a
`P5 D's own (n)` block, `none` when empty. A row with a provenance prints it
in angle brackets after the goal; P1–P4 rows have none and print as before.

**Checks (T29 A2–A5).**
- **A2.** Before the first run, `evidence/t29-d-probes-2026-09-26.txt`
  records two crelay-topo P5 probes whose labels T23 settles, and their
  expected rows: agent1 at location1 (START; its start location) and agent1
  on plate1 at location2 (CHEAP 2 @ 2; T23's P3.1 plan).
- **A3.** The generated P5 rows match A2; the P1–P4 list equals T23's
  recorded probes.
- **A4.** An entry without a goal signals an error; an empty entry list
  gives no probes and a `P5 D's own (0)` block with `none`; a goal naming an
  undeclared relation signals an error from D-PROBES, before any search.
- **A5.** No problem object names in the new code; callees-first; no LABELS
  or FLET; the file loads by hand without warnings.

## 13. CP cycle-plan check (T24, I2)

Written 2026-09-26, before code, under T24's approved acceptance criteria
(plan, T24 entry; scope (a), budget checks only). Design basis:
`Post-Mortem-2026.md` sections 1.6, 1.7 (I2) and 2.2 (Phase 2 step 6). It
absorbs RO's scenario input (§4) and the budget part of the G15 checklist
(§7, item 2.2 as a question; items 2.1 and 2.3 are not discharged).

### 13.1 Purpose and scope

CP answers, for a stage plan D proposes: *do the plate, body and view
budgets allow each stage, before any action is written?* It reads D's plan
as data and the static tables S1, S2, RC and CC, and runs no search and no
state evaluation. It is not part of the profile:
`REPORT-STATIC-CONSTRAINT-PROFILE` does not call it, and the profile file
does not change. Entry point `(REPORT-CYCLE-PLAN-CHECK plan)`.

CP checks budgets, not plans. It does not evaluate reach, elevation,
movement, transport, object-presence history or action order within a
segment; a PASS says only that no budget forbids the stage. Every problem
term comes from the plan (C3).

### 13.2 Plan data

A plan is a plist: `:name` (string), `:provenance` (string), `:stages`
(a list). A stage is a plist: `:id` (string), `:intent` (string),
`:segments` (a list, in time order). A segment extends RO's scenario plist;
RO's keys keep RO's meaning:

| Key | Value | Meaning |
|---|---|---|
| `:id` | string | the segment's name in the report |
| `:view` | `:physical` or `:recording` (RO) | the view the required devices are read in |
| `:cycle` | `:none` or `:open` (RO) | whether a recorder cycle is open |
| `:ghosts` | `:absent` or `:present` (RO) | whether ghost bodies exist |
| `:provenance` | string (RO) | where the segment comes from (D's plan, a premise id) |
| `:available-witnesses` | list of bodies, or `:unknown` (RO) | bodies free to hold plates |
| `:require` | list of controlled devices | devices that must be active at the same time, in `:view` |
| `:held` | list of `(body plate)` | plate holders D's plan fixes (optional) |
| `:off-plate` | list of bodies | bodies the plan commits off plates: relays, risers, carriers (optional) |
| `:landing` | a body, or absent | the support a lifted agent stands on through a toggle (B4) |

A body is a member of S2's ON occupant pool. A body in `:held` or
`:off-plate` is not also listed in `:available-witnesses`.

### 13.3 Common terms

- **Layer** of a body: S2's layer class (live, ghost, unpaired).
- **Eligible in a view** [grade 1; S2 consumer sites]. Physical view: the
  plate status relation is read by a layer-blind consumer, so every body
  present counts: live and unpaired, and ghost when `:ghosts :present`.
  Recording view: the recording plate status is layer-restricted (S2 step
  5), so only ghost bodies count, and it needs `:cycle :open`.
- **Literals of a device**: its S1 form, one clause per alternative.
  A positive plate literal demands a holder; an inverted plate literal
  demands the plate empty; a switch literal demands a setting; a
  device-mediated primitive (S1 tier) demands a beam (B3). A device with
  more than one alternative clause is not combined: its B1 row is
  CONDITIONAL, "alternatives not combined".
- **Required plates** of a segment: the plates demanded by the `:require`
  devices, plus the plates B3 adds; each plate once (§4 step 2).
- **Free witnesses**: `:available-witnesses` eligible in the view, less the
  bodies in `:held` and `:off-plate`.

### 13.4 Families

Each family prints rows per segment, in family order. A row's label is
PASS, CONFLICT (naming the row and fact that forbid it) or CONDITIONAL
(naming the missing premise).

**B0 view** [grade 1; S2]. CONFLICT for each of: a named body not in the ON
pool; a ghost body with `:ghosts :absent`; `:ghosts :present` or
`:view :recording` with `:cycle :none`; in the recording view, a held or
witness body that is not ghost; a body in two of `:held`, `:off-plate` and
`:available-witnesses`. One PASS row when there is none.

**B1 plate budget** [grade 1 -> 2; S1 S2 RO]. One row per required plate:
its holder when `:held` names one (CONFLICT if that holder is not eligible
in the view), else "matched". The unpinned plates are matched injectively
to the free witnesses with RO's allocation (§4 steps 3-5): a perfect
matching gives PASS on each row; otherwise CONFLICT, shortage, naming RO's
violator (the plates and the witnesses they share). `:unknown`
availability gives CONDITIONAL. A plate demanded empty that `:held` names,
or that is also a required plate, is a CONFLICT. The RO caveat is printed:
per-plate eligibility beyond the view (reach, elevation, history) is not
evaluated.

**B2 control conflict** [grade 1; S1]. Collect every switch and plate
literal of the `:require` devices (and B3's added gates). A primitive
demanded both ways (on and off, held and empty) is a CONFLICT naming the
two devices, with S1's EXCLUSION pair when S1 lists one. One PASS row when
there is none.

**B3 beam** [grade 2; S1 RC]. For each `:require` device whose literals
name a device-mediated primitive P:
- physical view, P has RC chains: the gates common to every usable chain
  (bootstrap and latch), less the devices P drives, join the requirement
  (NH H3's set): their plates go to B1, their switch literals to B2. Then
  the off-plate count: CONFLICT when `:off-plate` has fewer bodies than
  RC's least off-plate count over bootstrap chains, else PASS. A note says
  chains are not matched to bodies, and a stacked riser (§11.4) is not in
  RC's stations;
- recording view, or no RC chains to P: CONDITIONAL, "RC is physical-view
  only" or "no RC chains".

**B4 lift landing** [grade 1; CC]. For each pair of consecutive segments
s, t in one stage, each CC G15 row (lift L, barrier B) with L in s's
`:require` and B in t's: the toggle between them drops a lifted occupant
not on a support. t's `:landing` absent gives CONDITIONAL, with checklist
§2.2's question; a landing that is not in S2's support pool gives
CONFLICT; a support gives PASS on that premise (its presence at the lift's
destination is not evaluated).

### 13.5 Labels and grades

A segment's label is the worst of its rows (CONFLICT, then CONDITIONAL,
then PASS); a stage's is the worst of its segments; the plan's is the worst
of its stages. A row carries its family's grade. PASS is not a plan
witness and CONFLICT rejects the stated budget only, not the stage's idea:
both are relative to D's stated segments.

### 13.6 Output

A header `CP  CYCLE-PLAN CHECK  [grade per row]`, a READING line, a plan
line (name, provenance, stage count), a count line by label over segments,
then per stage a line `stage <id>  <LABEL>  <intent>` and per segment a
line `segment <id>  view <v>  cycle <c>  ghosts <g>  <LABEL>`, followed by
its rows `B<k>  <LABEL>  [<grade>; <sources>]  <text>`. The report ends
with the plan label.

### 13.7 Checks (T24 A2-A6)

- **A2.** Before the first run, `evidence/t24-cycle-plan-check-<date>.txt`
  records D's crelay-topo plan as plan data (cycles 1-3 and the final leg,
  from `doc/problems/crelay-topo/constraint-evidence/b2-ghost-tray-loc5-check-2026-09-24.txt`
  and the validated 87-action path), each segment's basis named, and the
  expected CP output derived by hand from the current profile.
- **A3.** The generated output matches A2 line for line.
- **A4.** By script: each segment yields exactly one B1 row per required
  plate; RO's 2026-09-20 scenario, run as a one-segment plan requiring
  gate9, gives RO's first-run allocation (PERFECT, forced members box1,
  connector1, tray1, no forced pairing).
- **A5.** On plan copies: the lit segment without its plate3 holder, every
  other body committed, gives a B1 CONFLICT citing plate3 (added by B3 for
  gate4); a segment requiring gate5 and gate7 gives a B2 CONFLICT naming
  switch2 and S1's exclusion pair; a segment requiring the lift followed
  by one requiring gate2 with no `:landing` gives a B4 CONDITIONAL; a
  segment requiring more plates than free witnesses gives a B1 shortage.
- **A6.** No problem object names in the CP code; callees-first; no LABELS
  or FLET; the file loads by hand without warnings.

### 13.8 T44 amendment: role compatibility and explicit reservations (2026-09-28)

Written before code, under T44's approved scope (plan, T44 entry). CP gains
a stage key, a role vocabulary, family B5 and per-plate eligibility in B1.
Nothing is inferred: every reservation is supplied by the caller, and a
reservation's destination is never derived (G12). A plan without
`:reservations` prints exactly what it printed before.

**Stage key `:reservations`**, a list of plists:

| Key | Value | Meaning |
|---|---|---|
| `:body` | a body | the object reserved |
| `:role` | a role form (below) | the job it does |
| `:from`, `:through` | segment ids of this stage | the phases it covers, inclusive; default the stage's first and last segment |
| `:purpose` | string, optional | printed with the role (e.g. "jump support") |

A body's roles in a segment are the roles of every reservation covering
that segment. An unknown role keyword, or a `:from`/`:through` naming no
segment of the stage or ending before it begins, signals an error.

**Roles**, each read from the relation it asserts:

| Role | Asserts | Source of its rule |
|---|---|---|
| `(:weight plate)` | `(on body plate)`; location the plate's HAS-POSITION | ON functional in the occupant (-support-occupancy) |
| `(:jam target [location])` | `(jamming body target)`; the jammer is placed | JAMMING functional in the jammer; JAM-TARGET places, pickup clears (jammer) |
| `(:place location)` | `(has-location body location)` | HAS-LOCATION functional (-location) |
| `(:hold agent)` | `(holding agent body)` | HOLDING bijective (-holding) |
| `(:mount gears)` | `(mounted-on body gears)`; floor/angled gears give the gears' location, wall gears none | MOUNTED-ON functional; MOUNT-FAN (-gears-fan) |
| `(:support occupant)` | `(on occupant body)` | S2's support pool; CLEARTOP |

Inside B5 only, a `:held (body plate)` entry counts as `(:weight plate)`
and an `:off-plate` body as a commitment off plates.

**Same-body compatibility** in one segment [grade 1]. CONFLICT when:
- C1 `:hold` with `:weight`, `:jam` or `:mount`, or with `:place` unless
  the body is a tray (INIT-CHECK-OBJECT-NOT-HELD-AND-HAS-LOCATION and its
  tray exception; INIT-CHECK-HELD-OBJECT-NOT-ON);
- C2 two location-bearing roles give different locations (a wall mount
  has none);
- C3 two roles of one kind give different values (two plates, targets,
  holders or gears), by the relation's functional key;
- C4 `:mount` with `:weight` (a mounted fan rests on no support);
- C5 `:support` on a held body other than a tray, on a tray not held (a
  grounded tray is inert), or on a wall-mounted fan;
- C6 `:weight` on a body committed off plates;
- C7 the body's type does not admit the role, or the role's argument is
  not of the required type (plate a pressure plate, target in TARGET,
  gears in GEARS, holder an agent, occupant in the ON pool).
Otherwise the roles are compatible; roles of two or more kinds on one body
are printed as SHARED jobs.

**Jam sightline** [grade 2; MC jammer survey, JAMMER-SIGHTLINE-ROWS: ground,
fixed plates and staged box tops, physical view, all gates forced open].
For a jam role whose location is fixed (by `:weight`, its own location or
`:place`): at a plate site, visible gives PASS with the gates whose single
closure blocks it as premises; not visible gives CONFLICT, relative to the
survey. At another location, a visible surveyed site gives PASS with its
premises; none gives CONDITIONAL (moved supports are not surveyed). No fixed
location: no sightline row. Recording view: CONDITIONAL, the survey is
physical. JAM-DISALLOWED> depends on the agent's location and is not
evaluated; a note says so.

**Cross-body capacity** [grade 1]. CONFLICT when:
- K1 an occupant is on two supports, or two occupants on one support
  contend (SUPPORT-OCCUPANCY-CONFLICT-P: they contend unless one is live
  and the other its opposite, ghost, layer; S2 classes), over every
  implied ON fact;
- K2 one agent holds two bodies;
- K3 one gears carries two fans;
- K4 a reserved ghost body while `:ghosts :absent`.

**Eligible pool and B1.** A plate's holders are its `:held` entries and
its `:weight` roles. Free witnesses are as in 13.3, less every reserved
body. A reserved body is eligible for an unpinned required plate P when it
is eligible in the view (13.3) and adding `(:weight P)` passes C1-C7 and,
for a jam role without a fixed location, P's site passes the jam sightline
(the recording view admits it, premise unevaluated). A reservation states
its body's presence, so the body need not be in `:available-witnesses`.
B1 matches the unpinned plates with per-plate eligibility; a plate named by
several devices is still one plate (13.3). When the segment has an active
reservation, a shortage row adds "for the supplied reservations; refutes
this allocation only".

**B5 rows**, printed only for a segment with an active or released
reservation, in order:
1. per reserved body: its roles with purposes and phase ranges, SHARED
   when of two or more kinds, and each C and sightline result; the row's
   label is the worst of them;
2. K1-K4: one CONFLICT row per violation, else one PASS;
3. the pool: per reserved body, the unpinned required plates it is
   eligible for (with jam premises), or "reserved off plates"; then the
   free witnesses;
4. per reservation that ended at the previous segment of the stage:
   "released after <id>", and whether this segment lists the body as
   available. A release is not simultaneous availability: the body counts
   here only as this segment states it.

Not evaluated: reach, transport, placement legality, order within a
segment, a reservation's necessity or destination.

**Acceptance (T44), before runs.**
- A1 claustro-topo, final phase (require gate8 and gate9; box1 available;
  box2 `(:place location10)` "jump support"; jammer1 `(:jam gate5)`;
  jammer2 `(:jam gate1)`): three B1 rows (the shared plates once), PASS;
  B5 shows each jammer eligible for plates with its premises (gate1 from a
  plate needs gate2/gate3 open), box2 reserved off plates. The pinned end
  arrangement (box1 on plate1, jammer1 on plate2 jamming gate5, jammer2 on
  plate3 jamming gate1, box2 at location10) gives PASS with SHARED rows.
- A2 CONFLICTs: jammer1 held and jamming (C1); two jam targets (C3); box2
  at location10 weighting plate1 (C2); box and jammer on one plate (K1); a
  box asked to jam (C7). Reserving box1 elsewhere gives a B1 shortage with
  the supplied-allocation text.
- A3 phases: a reservation through segment s1 prints a release row in s2
  and its body counts in s2 only when listed available.
- A4 phobia-topo: fan1 mounted on floor gears supporting agent1 is
  compatible; on wall gears with a support role, CONFLICT (C5); two fans
  on one gears (K3) by construction when available.
- A5 A plan without reservations: T24's crelay-topo plan output unchanged;
  caller plan unchanged; no problem names, LABELS or FLET in the new code;
  the file compiles without new warnings.

## 14. ME memory estimate (T26, I8)

**Closed, 2026-09-26 (D).** T26 was closed before completion and its code
removed: the engine setting `*max-states-processed*` and
`tech/constraint-memory-estimate.lisp` no longer exist. D sets each
problem's maximum search depth by D's own experiments (Problem-Solving
Guide, Search settings). This section is kept as the record of the design;
its known defect is in the T26 evidence, part 2.2 b: the bytes-per-state
rule of 14.3 takes early runs whose fixed costs inflate b about fourfold.

Written 2026-09-26, before code, under T26's approved acceptance criteria
(plan, T26 entry; scope (a) with the engine guard setting). Design basis:
`Post-Mortem-2026.md` section 4.3 (I8).

### 14.1 Purpose and scope

ME answers, before a search: *will this search, at this cutoff, fit in
lumpy's memory?* It measures the search's own growth with small pilot runs,
projects it to the requested cutoffs, and labels each against the heap
ceiling. A guard then runs the real search with a states limit derived from
the same readings, so a wrong estimate ends in STOPPED rather than a crash.

ME lives in its own loadable file, `tech/constraint-memory-estimate.lisp`,
under the working conventions for those files (plain CL in `:WW`, no
`define-*` forms, callees-first, no LABELS or FLET, no problem object
names). It searches, so it is not part of the profile. It is run by hand
before a stage's searches (scope (a)); the probe battery and the checkpoint
searches do not call it. Its one engine dependency is the setting of 14.5.

### 14.2 Pilot

`(RUN-MEMORY-PILOT checkpoint goal pilot-depth pilot-states-limit pathname)`.
CHECKPOINT and GOAL are the target search's own start (a search checkpoint;
`CAPTURE-SEARCH-CHECKPOINT` for the fresh staging) and goal form. Every
setting is as staged or as the caller set it, except the cutoff; all are
recorded. `*threads*` must be 16 (checked, as in PB).

For d = 1, 2, ... pilot-depth, one standalone search (two-argument
`SOLVE-SEARCH-CHECKPOINT`) at cutoff d, under the guard setting (14.5) at
pilot-states-limit. After each run: `(sb-ext:gc :full t)`, then
`(sb-kernel:dynamic-usage)`. The visited-state tables of a search survive
it until the next search resets them (`ww-searcher.lisp`, DFS
initialization), so the reading is what the run retained. A run records:
cutoff, planner outcome (status, reason), truncation flag, states
(`*total-states-processed*`), retained bytes, and, when found, the plan
length (the last goal-chain phase's path).

The pilot stops at the first of:
- d = pilot-depth;
- a plan found with solution type FIRST: FOUND n @ d. The target search
  stops at its first plan too, so deeper pilot runs would measure nothing
  it does. Under any other solution type (min-length, as staged for
  crelay-topo) a found plan is recorded and the pilot goes on: the target
  search runs under the same solution bound, so the bounded growth is the
  growth to project;
- no plan and no truncation: EXHAUSTED @ d; deeper cutoffs explore the same
  states;
- the guard stopped the run: STOPPED @ d.

Results go to a readable data file at PATHNAME (like PB, rewritten after
every run), holding the settings, `(sb-ext:dynamic-space-size)` and the
runs, so the report regenerates without searching (M2).

### 14.3 Estimate

From the pilot file:
- **Base** B: the retained bytes after the cutoff-1 run (a few states; the
  heap with empty tables).
- **Bytes per state** b: max over growth runs with at least
  `*memory-estimate-substantial-states*` (10,000) states of
  (retained − B) / states; if none qualifies, the deepest growth run's
  value. Hash tables grow by doubling, so b is stepwise; taking the max is
  on the safe side.
- **Growth runs**: completed pilot runs that were truncated and not
  stopped by the guard, with or without a plan, except a FIRST run that
  found one. The FOUND (FIRST), EXHAUSTED or STOPPED run that ends a pilot
  is not a growth run; its retained bytes still count for b.
- **Growth ratio** r at the deepest growth run k: the larger of
  s(k)/s(k−1) and s(k−1)/s(k−2). At least three growth runs are needed;
  with fewer, every cutoff above k is UNKNOWN.
- **Projected states** at cutoff c > k: s(k) · r^(c−k). For c ≤ k, the
  measured states. After EXHAUSTED @ d, every c ≥ d takes d's states.
- **Projected memory** M(c) = B + b · states(c).

Projection extends the pilot's own growth, under the target's own
solution type and bound. A plan found by the pilot is reported with its
length and cutoff; it may make the deeper search unnecessary.

### 14.4 Labels and report

`(REPORT-MEMORY-ESTIMATE pathname cutoffs &optional ceiling)`: no search.
CEILING defaults to the pilot file's dynamic-space size (A4 passes a
smaller one). For each requested cutoff, with fractions in
`*memory-estimate-fractions*`:
- **SAFE**: M(c) ≤ 0.5 · ceiling.
- **AT RISK**: 0.5 · ceiling < M(c) ≤ 0.8 · ceiling.
- **LIKELY TO EXHAUST**: M(c) > 0.8 · ceiling.
- **UNKNOWN**: too few growth runs to project.
A cutoff within the pilot is marked `measured`, above it `projected`.
The margins are wide on purpose: SBCL's collector needs free space to copy
into, a doubling hash table needs its old and new vectors at once, and
single-step growth ratios on crelay-topo miss by up to a factor of 2
(T26 evidence, A2).

The section `ME  MEMORY ESTIMATE  [estimate; no grade]` prints a READING
line, a settings line (problem, goal, ceiling, threads, pilot depth,
pilot limit), the pilot table (cutoff, outcome, truncated, states,
retained, bytes per state, ratio), the end of the pilot (FOUND n @ d,
EXHAUSTED @ d, STOPPED @ d or pilot depth reached), B, b and r, one row per
requested cutoff (`cutoff c  states ~s  memory ~m GB  LABEL  measured |
projected`), the deepest requested SAFE cutoff (or `none`), and the guard's
states limit. Estimates carry no grade: they are cost forecasts, not
constraints, and never a reason to call a subgoal impossible.

### 14.5 Guard and engine setting

**Engine setting** (`src/ww-settings.lisp`): `*max-states-processed*`,
default NIL. NIL has no effect. A positive integer N is read by parallel
search only: each worker, at its bound refresh (every
`*bound-refresh-interval*` cycles), sums the states processed so far
(`*total-states-processed*` from task generation plus every worker's count,
as the progress report does), and when the sum is at least N calls
`REQUEST-PARALLEL-WORKER-SHUTDOWN` on its task queue and returns. That is
the engine's existing stop path: it sets `*shutdown-requested*` under the
queue lock and wakes waiting workers. DFS then returns `:shutdown`, and
`WW-SOLVE` records the outcome `(:unknown :shutdown)`. Overshoot is at most
about one refresh interval per worker. Serial search ignores the setting.

**Guard** `(RUN-GUARDED-SEARCH checkpoint goal cutoff pathname &optional
limit)`: requires `*threads*` 16; states limit L = LIMIT when given (A4),
else floor((0.6 · ceiling − B) / b) from the pilot file at PATHNAME; binds nothing thread-locally (workers do not see a
thread's LET), so it sets `*max-states-processed*` and `*depth-cutoff*`
globally and restores both however the search ends. It prints one line,
`GUARD cutoff c  limit L  states s  <outcome>  <label>`, and returns the
search's two values plus the label: STOPPED when the outcome reason is
`:shutdown` and s ≥ L, otherwise the planner's outcome status.

### 14.6 Checks (T26 A2–A5)

- **A2.** Before code, `evidence/t26-memory-estimate-<date>.txt` records the
  back-test on T23's saved results: along the shared growth curve
  (cutoffs 1–11), each one-step prediction from the cutoffs below it by
  14.3's ratio rule, with the error factor predicted / actual. Every factor
  lies in [0.5, 3].
- **A3.** Before the run, the same file records the expected pilot and
  estimate for the T10 final search: start `t10-c3-location15-checkpoint.txt`,
  goal `(has-location agent1 location19)`, crelay-topo as staged, graph,
  minimum-steps pruning on, solution type min-length as staged, threads
  16; pilot depth 9; ceiling 16,000 MiB, the heap of the T10 runs. Expected:
  a 7-action plan first found at cutoff 7 (the T10 leg); cutoff 12 LIKELY
  TO EXHAUST (it ran out of memory); cutoff 10 not LIKELY TO EXHAUST (it
  completed). Refined by A from the approved "cutoff 10 SAFE" before code:
  the cutoff-10 run's retained memory was never measured (it consed
  1.24 TB with 9,995,969 cutoff hits), so SAFE and AT RISK are both
  consistent with the record.
- **A4.** A ceiling a tenth of the real one turns the cutoffs SAFE at the
  real ceiling into AT RISK or LIKELY TO EXHAUST; a guarded search for
  T23's P1.1 goal from the fresh staging at cutoff 11 (466,890 states
  unguarded) with a forced limit of 10,000 prints STOPPED, with states at
  least 10,000 and under 100,000, and a following search in the same
  session completes normally; with the setting NIL, the T23
  run P1.1 at cutoff 8 from the fresh staging (solution type FIRST)
  gives 10,021 states and outcome `(:exhausted-no-solution
  :depth-cutoff-truncated)`, as T23 recorded.
- **A5.** No problem object names in the ME code; callees-first; no LABELS
  or FLET; the file and the engine load without warnings.

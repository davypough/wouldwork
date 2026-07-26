# Problem-Class Library for WouldWork Subgoal Deduction

A reference catalog of problem classes, each with diagnostic signs, the
canonical subgoal form for that class, and worked instances. Consulted by
Phase 1 (triage) of `subgoal-strategy.md` to assign a class label, and by
Phase 3 (phase-boundary identification) to retrieve the canonical
subgoal-form template.

Classes are organized by the four axes of Phase 1:
**decomposability**, **resource-scarcity**, **topology**, and
**dynamic-interactions**. Compound classes (combinations of two or more
non-trivial axes) are catalogued separately at the end.

---

## Class A — Independent

**Profile:** decomposability=independent, resource-scarcity=abundant,
topology=open, dynamic-interactions=simple.

**Diagnostic signs.** Goal conjuncts touch disjoint object pools. No
scarce-resource contention. No bottleneck gating the goal area. No mutex
between conjuncts in dynamic relations.

**Canonical subgoal form.** None needed; direct `(solve)` is expected to
work. If it does not, the triage was wrong on some axis.

**Worked instances.** *(none yet)*

---

## Class B — Bottleneck-gated

**Profile:** topology=bottleneck-gated; other axes trivial.

**Diagnostic signs.** Goal area reachable only via `(accessible1 _ G _)`
where `G` is a closed gate. Opener of `G` (via `controls`) requires setup
work that does not contribute directly to the goal conjuncts.

**Canonical subgoal form.** A snapshot at the moment the bottleneck
becomes traversable, with the gate's opener active and the agent
positioned to traverse:

```
(and <opener-active>            ;e.g. (active receiver-G)
     <agent-on-near-side-of-G>) ;e.g. (loc agent A) where (accessible1 A G A_goal)
```

**Worked instances.** *(none yet)*

---

## Class C — Must-undo (scarce-shared resource)

**Profile:** decomposability=coupled, resource-scarcity=scarce-shared;
topology and dynamic-interactions trivial.

**Diagnostic signs.** A scarce object T (or pool of T-instances) is
manipulated by some action schema as cargo, and the goal — together with
its causal-chain transients — requires more configurations of T than
there are instances. Local progress on one configuration must be undone
to free T for the next. Phases divide into **anchor phases** (which
assert persistent landmarks) and **bridge phases** (which assert only
transient pairings, later reorganized).

**Canonical subgoal form (after-first-anchor-phase boundary).** A
snapshot immediately after the first anchor phase's connect, with the
next phase's pickup transient at peak:

```
(and <anchor-phase-1's persistent landmarks>     ;Pass A2
     (holds agent T_next)                        ;Pass C transient
     <agent at strategic location>)              ;Phase-3 placement
```

**Worked instances.** See Class F (Corner) for a compound case where
must-undo coexists with bottleneck and mutex.

---

## Class D — Mutex-prone

**Profile:** dynamic-interactions=mutex-prone; other axes trivial.

**Diagnostic signs.** Two or more goal conjuncts depend on relations
whose physical realizations share a medium and which `propagate-changes!`
(or equivalent) cross-deactivates. Co-stability of the full goal is
non-obvious or has been empirically observed to fail.

**Canonical subgoal form.** A snapshot in which a mutex-free *subset* of
the goal is satisfied, deferring mutex-prone conjuncts to leg 2 — where
the leg-2 search can resolve the mutex by reorganizing the medium.

**Worked instances.** *(none yet)*

---

## Class E — Compound: must-undo + bottleneck-gated

**Profile:** decomposability=coupled, resource-scarcity=scarce-shared,
topology=bottleneck-gated; dynamic-interactions=simple.

**Diagnostic signs.** As in Class C, but the goal area is also gated by
a bottleneck whose opener is established by a bridge phase that
competes with the goal-completion configurations.

**Canonical subgoal form (same as Class C).** After-first-anchor-phase
boundary. The bottleneck cross is deferred to leg 2; leg 1 commits the
first anchor phase only. Agent is positioned at the bottleneck near-side
or at the next phase's intended connect site.

**Worked instances.** *(none yet)*

---

## Class F — Compound: must-undo + bottleneck-gated + mutex-prone

**Profile:** decomposability=coupled, resource-scarcity=scarce-shared,
topology=bottleneck-gated, dynamic-interactions=mutex-prone.

**Diagnostic signs.** As in Class E, plus mutex pairs in the dynamic
relations that prevent some goal conjuncts from co-existing with
bridge-phase configurations.

**Canonical subgoal form (same as Class C).** After-first-anchor-phase
boundary. At the boundary:

- The first anchor phase's persistent landmarks are committed
  (including pairings that will eventually contribute to the
  bottleneck-opener chain).
- The next phase's pickup transient `(holds agent T_next)` is at peak.
- Agent is at bottleneck-near-side (not yet crossed; the bridge phase
  that opens the bottleneck has not yet fired).
- Bridge-phase pairings are *omitted*. The bottleneck-opener
  `(active R_opener)` and `(open G)` are *omitted*. Mutex-prone goal
  conjuncts are *omitted*.

This factoring defers all bridge work, the bottleneck cross, and all
mutex resolution to leg 2 entirely.

### Worked instance — `problem-corner.lisp`

#### Phase 1 — Triage output

```
PROBLEM:               corner
DECOMPOSABILITY:       coupled
                       (all three goal conjuncts depend on the connector
                        pool; (loc agent1 area4) requires gate1 open
                        which requires a red beam chain that competes
                        with the (active receiver2) chain)
RESOURCE-SCARCITY:     scarce-shared: connector
                       (3 connector instances; goal + transient gate-
                        opening = 3 chains, but the red transmitter t1
                        must serve receiver1 transiently and receiver2
                        permanently — temporal contention on t1's chain)
TOPOLOGY:              bottleneck-gated: gate1
                       (no (accessible0 _ area4); only (accessible1 _
                        gate1 area4); gate1 is closed in init; opener
                        is receiver1, requiring a red chain from t1)
DYNAMIC-INTERACTIONS:  mutex-prone:
                       ((active receiver1) ↔ (active receiver3))
                       (the red chain to receiver1 in area1 and the
                        blue chain to receiver3 in area3 cross
                        geometrically; update-beams-if-interference!
                        deactivates one when both are present)
PRIMARY CLASS:         F (must-undo + bottleneck-gated + mutex-prone)
SECONDARY CLASSES:     E (drop the mutex axis), C (drop both)
CONFIDENCE:            high
RATIONALE:             All four axes are nontrivial; the configuration
                       is the canonical reason heuristic search fails on
                       this problem (local progress on receiver1 must
                       be undone, and the natural goal-state slice
                       (active receiver1) ∧ (active receiver3) is
                       infeasible).
```

#### Phase 2 — Landmark extraction

```
PERSISTENT LANDMARKS (Pass A1, goal conjuncts):
  (loc agent1 area4)
  (active receiver2)
  (active receiver3)

PERSISTENT LANDMARKS (Pass A2, non-derived atoms in *known-goal-state*):
  (loc agent1 area4)
  (loc connector1 area2)
  (loc connector3 area3)
  (holds agent1 connector2)
  (paired connector1 transmitter2)
  (paired connector1 receiver1)
  (paired connector1 receiver3)
  (paired connector3 transmitter1)
  (paired connector3 receiver2)
  (paired connector3 connector1)
  (skipped as derived: color, beam-segment, current-beams,
                       active receiver{2,3} — already in A1)

BACKWARD-CHAINED TRANSIENT LANDMARKS (Pass B):
  (open gate1)
    derivation: (loc agent1 area4) ← move ← accessible(_, area4)
                ← only via (accessible1 _ gate1 area4) ← (open gate1)
  (active receiver1)
    derivation: (open gate1) ← activate-receivers-that-gained-power!
                where (controls ?r gate1) ← (controls receiver1 gate1)
                ⇒ (active receiver1)
  (∃ connector C : (paired C receiver1) ∧ (color C red) at some state)
    derivation: (active receiver1) requires beam reaching receiver1
                with hue matching (chroma receiver1 red); only red source
                is transmitter1, so a connector chain from transmitter1
                ending paired with receiver1, with the immediate-upstream
                connector colored red.

ACTION-SCHEMA TRANSIENT LANDMARKS (Pass C):
  (holds agent1 connector1)  -- relocated area1 → area2
  (holds agent1 connector2)  -- both transient and persistent (goal)
  (holds agent1 connector3)  -- pairings change, picked up and replaced

COMPATIBILITY ANNOTATIONS:
  Mutex sets:
    { (holds agent1 c1), (holds agent1 c2), (holds agent1 c3) }
    { (active receiver1), (active receiver3) } -- candidate, geometry-
      dependent; final ruling deferred to Phase 4
  Implications:
    (active receiver1) ⇒ (open gate1)
  Causal precedence:
    (open gate1) precedes (loc agent1 area4)
```

#### Phase 3 — Phase-boundary identification

**Step 1 — Manipulation phases.**

```
phase-c1: pickup c1 → move(s) → connect c1 with t2/r1/r3 in area2
  asserts persistent landmarks:
    (loc connector1 area2)
    (paired connector1 transmitter2)
    (paired connector1 receiver1)
    (paired connector1 receiver3)
  side effects (propagation):
    (color connector1 blue)
    (active receiver3)         ; blue beam c1→r3 matches r3 chroma blue

phase-c2: pickup c2 → move(s) → connect c2 with c1/r1/t1 in area3
  asserts pairings:
    (paired connector2 connector1)
    (paired connector2 receiver1)
    (paired connector2 transmitter1)
  side effects (propagation):
    (color connector2 red)
    (active receiver1)         ; red beam c2→r1 matches r1 chroma red
    (open gate1)               ; via (controls receiver1 gate1)

phase-c3: pickup c3 → connect c3 with t1/r2/c1 in area3
  asserts persistent landmarks:
    (loc connector3 area3)     ; unchanged but re-asserted via connect
    (paired connector3 transmitter1)
    (paired connector3 receiver2)
    (paired connector3 connector1)
  side effects (propagation):
    (color connector3 red)
    (active receiver2)         ; red beam c3→r2 matches r2 chroma red
```

**Step 2 — Anchor vs bridge classification.**

```
phase-c1: ANCHOR
  c1's goal-state pairings (Pass A2) match exactly: t2, r1, r3.
phase-c2: BRIDGE
  c2's goal-state has *no pairings* (c2 is held at goal). The pairings
  asserted by phase-c2 (c1, r1, t1) are transient; they will be undone
  by a re-pickup of c2 before goal-state.
phase-c3: ANCHOR
  c3's goal-state pairings (Pass A2) match exactly: t1, r2, c1.
```

**Step 3 — Phase ordering.**

```
phase-c1 → phase-c2 → cross gate1 → phase-c3
  rationale:
  - Pairing dependence: phase-c2's connect references c1 as a terminus
    (paired c2 c1), so c1 must already be placed. c1-before-c2.
  - Pairing dependence: phase-c3's connect references c1 (paired c3 c1),
    so c1 must already be placed. c1-before-c3.
  - Bottleneck precedence: phase-c2 establishes (active receiver1)
    which opens gate1; the cross must follow phase-c2.
  - Phase-c3 needs r2 chain, which requires t1 source. After phase-c2
    fires, t1's beam goes via c2 (not c3). Phase-c3 reorganizes by
    re-picking-up c2 first (must-undo signature), then re-placing c2
    with new pairings, then placing c3 — but for boundary purposes
    phase-c3's anchor commitment can be the simple connect.
  - Spatial cost (under min-length): agent starts in area1 with c1.
    Cheapest order is c1-first.
```

**Step 4 — Class-F boundary heuristic.**

```
boundary = after-first-anchor-phase
        = immediately after phase-c1's connect, before phase-c2's connect
        = at the peak of phase-c2's pickup transient
```

**Step 5 — T_next and agent location.**

```
T_next = connector2 (the bridge phase's cargo)
agent location after phase-c1's connect = area2 (where c1 was placed)
agent location after pickup-c2 = area2 (c2 already in area2)
strategic move toward bottleneck-near-side = area3
  (area3 is the natural connect site for phase-c2 — los0 area3 t1
   makes the red chain feasible from there — and also the bottleneck
   near-side for gate1)
agent location at boundary = area3
```

**Step 6 — Compose candidate subgoal.**

```lisp
(and (loc connector1 area2)
     (paired connector1 transmitter2)
     (paired connector1 receiver1)
     (paired connector1 receiver3)
     (holds agent1 connector2)
     (loc agent1 area3))
```

**Step 7 — Leg-1 cost estimate.**

```
1. pickup-connector agent1 connector1 (in area1)        [agent already in area1]
2. move agent1 area1 → area2                            [accessible0]
3. connect-to-3-terminus agent1 connector1 r3 r1 t2     [in area2]
4. pickup-connector agent1 connector2 (in area2)
5. move agent1 area2 → area3                            [accessible0]

LEG-1 ACTION COUNT: 5  (cutoff = 10)
```

#### Phase 4 — Co-stability vetting

**Candidate state (after leg-1 sequence applied to init).**

```
Salient committed facts:
  (loc agent1 area3)
  (holds agent1 connector2)
  (loc connector1 area2)
  (paired connector1 transmitter2)
  (paired connector1 receiver1)
  (paired connector1 receiver3)
Salient propagated facts:
  (color connector1 blue)        ; c1's source is t2 (blue)
  (active receiver3)             ; r3 chroma blue, chain t2→c1→r3
Salient negative facts:
  ¬(active receiver1)            ; chain to r1 carries blue, r1 wants red
  ¬(active receiver2)            ; no chain to r2 yet
  ¬(open gate1)                  ; (controls receiver1 gate1) inactive
  c3 unchanged at init: (loc connector3 area3), no pairings
  c2 has no pairings (held)
```

**Static-geometry mutex check.**

```
Active beams (segments produced by current pairings):
  t2 ↔ c1   (within area2)
  c1 ↔ r1   (area2 → area1, blue)
  c1 ↔ r3   (area2 → area3, blue)

Pairwise intersections:
  All three segments share endpoint c1 (radial fan from c1).
  No segments belong to disjoint chains, so no
  update-beams-if-interference! triggers.
Verdict: PASS
```

**Dynamic-relation mutex check (within subgoal).**

```
Phase-2 candidate-mutex pairs:
  { (active receiver1), (active receiver3) }
Triggered by candidate state:
  No — candidate state has only (active receiver3); r1 inactive.
Verdict: PASS
```

**Goal-side co-stability (deferred Pass-A1 landmarks).**

```
Deferred Pass-A1 conjuncts:
  (loc agent1 area4)
  (active receiver2)
  (active receiver3)

Pairwise co-stability:
  (active receiver2) needs red chain reaching r2; source t1, chroma red.
  (active receiver3) needs blue chain reaching r3; source t2, chroma blue.
  Different transmitters, different colors, distinct connector chains.

Pass A2 supplies a co-stable assignment:
  Blue chain: t2 → c1 → r3   (c1 in area2, paired t2/r1/r3, color blue)
  Red chain:  t1 → c3 → r2   (c3 in area3, paired t1/r2/c1, color red)
  Structural link (paired c3 c1) joins the chains at c3-c1 but each
  connector retains its own color (blue from t2 for c1, red from t1
  for c3); no power mixing.

Geometric interference between the two final chains: depends on the
segment t1↔c3 (in area3) vs c1↔r3 (area2→area3). Pass A2 was
constructed to be a valid goal state, so no interference at goal.
Verdict: PASS
```

**Depth-cost feasibility.**

```
Leg-1 cost:                           5
Leg-2 lower bound breakdown:
  connect-c2 (bridge phase, c2 already held)         1
  move area3 → area4 (after gate1 opens)             1
  pickup-c2 (clears bridge pairings)                 1
  pickup-c3 + connect-c3 (anchor phase-c3)           2
  return moves area4 → area3 / setup moves           ~1
  ----------------------------------------------------
  Leg-2 LB                                           ~6
Cutoff:                              10
Leg-1 ≤ cutoff:                       PASS
Leg-2 LB ≤ cutoff:                    PASS
```

(Empirical leg-2 cost from prior session: 9; LB of 6 is
consistent — the LB undercounts by omitting some intermediate moves.)

**Reachability sanity check.**

```
1. pickup-c1: agent in area1 ✓, c1 in area1 ✓ (init), free ✓
2. move area1→area2: (accessible0 area1 area2) ✓
3. connect-c1 with t2/r1/r3 from area2: termini t2 in area2; r1 and r3
   reachable via los/visible relations from area2 (the same relations
   that justify the goal-state pairings — verified by Pass A2
   consistency).
4. pickup-c2: agent in area2 ✓, c2 in area2 ✓ (init), free ✓
5. move area2→area3: (accessible0 area2 area3) ✓
Verdict: PASS
```

**Overall verdict: PASS — proceed to Phase 5.**

#### Phase 5 — Restrictiveness calibration

**Per-conjunct R/N analysis.**

```
(loc connector1 area2)
  R-test:  R-redundant. The cheapest leg-1 path satisfying the other
           five conjuncts puts c1 in area2 anyway: the (paired c1 r1)
           and (paired c1 r3) pairings, combined with los relations,
           force the connect to fire from area2.
  N-test:  N-binding (mild). Without the pin, nothing in the subgoal
           explicitly forbids alternative placements admitted by other
           losses; explicit pin keeps the planner from wandering.
  Role:    (R-redundant) (N) — KEEP (clarity).

(paired connector1 transmitter2)
  R-test:  R-binding. Without t2 paired, c1 has no source and (color
           connector1 blue) won't propagate; the leg-1 sequence can't
           produce the required propagated state.
  N-test:  N-binding. Removing admits leg-1 endings where c1 is paired
           only to r1/r3 with no source — leg 2 must add t2 pairing
           (re-pickup + connect = 2 extra actions; pushes leg-2 over
           cutoff).
  Role:    (R) (N) — KEEP.

(paired connector1 receiver1)
  R-test:  R-redundant. Phase-c1's connect-to-3-terminus fires t2/r1/r3
           together; the leg-1 sequence produces this pairing as part of
           the connect, but a leg-1 sequence using connect-to-2-terminus
           (t2/r3 only) would also satisfy the other conjuncts at the
           same cost.
  N-test:  N-binding. Without this pin, leg 2 must add (paired c1 r1)
           later via re-pickup of c1 (2 extra actions). Empirical
           leg-2 budget is tight (9 of 10); 2 extra actions would
           overrun.
  Role:    (N) — KEEP.

(paired connector1 receiver3)
  R-test:  R-binding (via N coupling). The blue chain to r3 is the
           only way to make (active receiver3) for the goal; committing
           it in leg 1 is the cheapest realization.
  N-test:  N-binding. Without this pin, leg 2 must add (paired c1 r3)
           via re-pickup of c1 (2 extra actions, overrun).
  Role:    (R) (N) — KEEP.

(holds agent1 connector2)
  R-test:  R-binding. Defines T_next and forces leg-1 to include the
           pickup-c2 action.
  N-test:  N-binding. Without it, leg 2 must pickup c2 itself (1 extra
           action plus possible repositioning); pushes leg-2 to budget.
  Role:    (R) (N) — KEEP.

(loc agent1 area3)
  R-test:  R-binding. Forces the strategic move area2 → area3 in leg 1.
           Not implied by other conjuncts (the connect of c1 ends with
           agent in area2; pickup-c2 keeps agent in area2).
  N-test:  N-binding. Without it, leg 2 must move agent area2→area3 to
           perform phase-c2's connect from area3 (1 extra action).
           Combined with other costs, pushes leg-2 to or past cutoff.
  Role:    (R) (N) — KEEP.
```

**Symmetry-pruning audit.**

```
Connectors c1, c2, c3:
  init (loc c1 area1), (loc c2 area2), (loc c3 area3) — all distinct.
  Not symmetric. Specific names forced; no existential relaxation.
Areas, transmitters, receivers: all named individually; no symmetry.
```

**Existential relaxation.**

```
Considered for (paired connector1 transmitter2/receiver1/receiver3):
  Could (exists (?c connector) (paired ?c r1)) replace? No — r1's only
  candidate at the boundary is c1 (c2 held, c3 untouched); the existential
  collapses to a singleton, no gain.
Rejected.
```

**Agent-location relaxation.**

```
Considered: drop (loc agent1 area3); let leg-2 move the agent.
  Cost analysis: leg-1 saves 1 action (4 instead of 5); leg-2 adds
  1 action (10 instead of 9). Leg-2 = 10 = cutoff exactly — risky.
Rejected: keep (loc agent1 area3) for safety margin on leg-2 budget.

Considered: replace with (or (loc agent1 area2) (loc agent1 area3)).
  Both endings are valid for leg-2 reachability, but only area3 saves
  the leg-2 move; symmetry pruning would not collapse them since the
  states differ.
Rejected.
```

**Calibrated subgoal.**

```lisp
(and (loc connector1 area2)         ; (R-redundant) (N)
     (paired connector1 transmitter2) ; (R) (N)
     (paired connector1 receiver1)  ; (N)
     (paired connector1 receiver3)  ; (R) (N)
     (holds agent1 connector2)      ; (R) (N)
     (loc agent1 area3))            ; (R) (N)
```

**Result.** The calibrated subgoal is identical to the candidate. No
relaxations were applied — every conjunct earns either an R or N role,
and the must-undo problem family has no symmetric objects to exploit.
This matches expectation: Class F problems run on tight budgets with
forced object assignments, so calibration typically confirms rather
than widens the candidate.

```
LEG-1 COST (calibrated):  5
LEG-2 LB (calibrated):    ~6 (empirical 9, both ≤ cutoff 10)
```

**Suggested REPL test expression:**

```lisp
(progn (ql:quickload :wouldwork) (in-package :ww))
;; Then evaluate:
(solve-subgoal '(and (loc connector1 area2)
                     (paired connector1 transmitter2)
                     (paired connector1 receiver1)
                     (paired connector1 receiver3)
                     (holds agent1 connector2)
                     (loc agent1 area3)))
```

#### Phase 6 — Diagnostic protocol

**Not invoked.** The Phase-5 calibrated subgoal succeeded empirically
in the prior session, factoring the 14-step optimal solution into legs
of 5 + 9 actions, both within `*depth-cutoff*` 10. No diagnostic
iteration was required.

```
EMPIRICAL OUTCOME (from prior session):
  Test:    (solve-subgoal '<calibrated subgoal>) → succeeded, 5 actions
           (solve)                               → succeeded, 9 actions
  Total:   14 actions to goal
  Mode:    n/a — both legs completed within cutoff
```

#### Verification — derived subgoal matches prior empirical success

The candidate subgoal derived by Phase 3 is identical to the subgoal
that empirically worked in the prior session, factoring the 14-step
optimal solution into legs of 5 + 9 actions. Each conjunct's role:

| Subgoal conjunct                | Source                                   |
|---------------------------------|------------------------------------------|
| `(loc connector1 area2)`        | phase-c1 anchor (Pass A2)                |
| `(paired connector1 transmitter2)` | phase-c1 anchor (Pass A2)             |
| `(paired connector1 receiver1)` | phase-c1 anchor (Pass A2)                |
| `(paired connector1 receiver3)` | phase-c1 anchor (Pass A2)                |
| `(holds agent1 connector2)`     | phase-c2 pickup transient (Pass C)       |
| `(loc agent1 area3)`            | Phase-3 strategic placement at gate1     |
|                                 | near-side                                |

Conjuncts deferred to leg 2:

- `(loc agent1 area4)` — Pass A1; the cross happens in leg 2.
- `(active receiver2)`, `(active receiver3)` — Pass A1; deferred.
- `(open gate1)`, `(active receiver1)` — Pass B transients; the bridge
  phase (phase-c2's connect) fires in leg 2 to open the gate, agent
  crosses, then the chain is reorganized for r2.
- All connector3 facts — phase-c3 happens entirely in leg 2.
- `(holds agent1 connector1)`, `(holds agent1 connector3)` — Pass C
  transients, mutex with the chosen `(holds agent1 connector2)`.

---

## Adding new instances

Each new problem analyzed should append a **Worked instance** subsection
to its assigned class, recording the Phase 1, Phase 2, Phase 3, Phase 4,
and Phase 5 outputs, and the Phase 6 outcome (either "not invoked" with
the empirical pass result, or a diagnostic history of attempts and the
final working subgoal). This corpus is the empirical base for refining
axis definitions, class boundaries, boundary heuristics, and the
diagnostic ladder itself.

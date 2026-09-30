# Separator-based traversal — implementation plan

Status: PLAN, agreed with D on 2026-09-29 (D1-D7 approved). Phases 0-3 closed 2026-09-29; Phase 4 next.
Base test case: probs/problem-rumin-topo.lisp. Then claustro-topo, corner-topo, crelay-topo;
phobia-topo and windtunnel-topo author no traverse-via facts but must still stage.
Implementation happens in a fresh session. Work one phase at a time; D reloads with
`(stage rumin-topo)` after every file change.

## 1. Goal

A traversal fact states what separates two locations. The kind of move (walk, stairs,
jump or vault, climb) is inferred per clause from the kinds of object in it and from the two
locations' levels. Every way to cross between one pair sits in one fact as DNF
alternatives, so the spec reads like the diagram: whatever is drawn between two locations
goes in the brackets.

```lisp
;; now                                                ;; new
(traverse-via stairway location2 () location4)        (traverse-via location2 ((staircase1) (edge1)) location4)
;(traverse-via jumping location2 () location4)
(traverse-via jumping location8 () location9)         (traverse-via location8 ((edge4)) location9)
(traverse-via stairway location9 ((gate4)) location10) (traverse-via location9 ((gate4 staircase3)) location10)
(traverse-via stairway location13 () location17)      (traverse-via location13 ((staircase2) (edge2) (edge3)) location17)
(traverse-via jumping location13 (edge2) location17)  ;  (malformed today)
(traverse-via> climbing location14 ((ladder2)) location5) (traverse-via> location14 ((ladder2)) location5)
```

`traverse-via` stays symmetric and `traverse-via>` directed, for every kind.

## 2. Representation

- Relations (in `-traversal.lisp`): `(traverse-via location $list location)` and
  `(traverse-via> location $list location)`. The mode argument and the `traversal-mode`
  type are removed. Payload stays DNF: `()` = nothing separates; otherwise OR over
  clauses, AND within a clause.
- One fact per (source destination) per relation. An authored duplicate is an init error
  whose message shows the combined family (decision D3).
- New static separator type for stairs, declared by `stairs.lisp` (decision D1: name
  `staircase`, instances such as `staircase1`). A staircase has no state and no position;
  it is always passable.
- Edges become legal clause members (static, always passable, no clearance).
- Ladders unchanged: a clause naming a ladder requires it positioned at the source, and a
  ladder fact must be `traverse-via>` (existing `ladder-init-check`, reworded).

### Clause kind inference (one function, used everywhere)

Evaluated per clause, in this order:

| Clause contains | Kind | Segment label | Level rule |
|---|---|---|---|
| a ladder | climb | LADDER | none (ladder decides) |
| a staircase | stairs | STAIRS | none |
| an edge or a wall | jump | JUMP, or VAULT when a closed gate/screen or a wall needs clearance | rise at most `*vertical-reach-limit*` from the launch |
| only gates/screens/gears, or nothing | walk | WALK | endpoints at the same level |

Gates, screens and gears may accompany any kind; they must be passable, except that in a
jump clause a closed gate/screen may be vaulted if low enough (today's jump rule, unchanged).
Rejected at init: a clause with more than one of {ladder, staircase, edge-or-wall}
kinds (e.g. staircase with edge).

crelay-topo's `(traverse-via jumping location4 ((gate2)) location6)` becomes
`(traverse-via location4 ((edge1 gate2)) location6)`: a jump up edge1 needing gate2 open,
which matches its geometry (gate2 sits on edge1).

## 3. Decisions

Resolved with D:
- R1 Separator facts replace mode facts; kind inferred as above.
- R2 Direction stays in the relation name for all kinds.
- R3 MOVE keeps its shape `(move agent route)`; segments stay `(label source witness destination)`,
  with the label inferred and the witness the clause used.
- R4 rumin-topo is the base case; other *-topo specs migrate afterwards.

Resolved with D on 2026-09-29 (each recommendation below approved as written):
- D1 Stairs type name: `staircase` (avoids any clash with the technology name `stairs` and
  the segment label STAIRS). Reversible.
- D2 Level check for walk kind: a walk-kind clause between locations at different levels is
  an init error ("name the staircase, edge or ladder that separates them").
  Recommendation: enforce only when both locations have `location-coords>` geometry;
  synthetic tests with bare levels keep level-based inference (a walk-kind clause across a
  level difference is then read as jump, as today's unnamed jumping facts are).
- D3 Duplicates: reject authored duplicates with a message giving the merged family
  (simple, explicit). Merging silently is rejected because it hides authoring slips.
- D4 Coordinate-derived walking facts (from `-walkability-coordinates`) and authored facts for
  the same pair: the derivation (a) emits nothing for pairs at different levels (walking
  could never use them) and (b) unions its family into an existing authored fact.
  Family minimization must be kind-aware: a clause subsumes another only if both have the
  same kind; otherwise `()` from walking would erase an authored jump clause and with it
  the support-transition options.
- D5 Grounded segment choice when several clauses of one fact succeed: offer one grounded
  segment per fact, preferring walk, stairs, climb, jump (cheapest first). Support
  transitions (jump onto/off a box or held tray) consider every jump-kind clause.
- D6 Replay leniency: a hand-written MOVE segment is accepted if its witness is any clause
  of the pair's fact that succeeds in that state with the stated label (not only the
  planner's preferred clause). Recommended; small change in `-mobility-action.lisp`.
- D7 Remote support landings over walk-kind pairs (e.g. jumping onto a box at a same-level
  neighbour): keep today's behaviour (only jump-kind clauses). Revisit only if a problem
  needs it.

## 4. Engine semantics

- `-traversal.lisp`: one builder dispatch. `traversal-segments-for-source` binds
  `traverse-via`/`traverse-via>` from the source (no mode loop), canonicalizes the family,
  infers each clause's kind and calls that kind's registered builder; returns at most one
  grounded segment per fact per D5. Registry becomes kind -> (builder obstacle-types);
  technologies register kinds (`walk`, `stairs`, `jump`, `climb`). The cache key and
  `*traversal-state-dependencies*` are unchanged in principle; re-verify with
  `*traversal-cache-paranoid*`.
- Builders keep their current predicates (`walking-segment-for-clause`,
  `stairs-segment-for-clause`, `jump-segment-for-clause`, `ladder-segment-for-clause`),
  but treat static separators (staircase, edge) as always clear and exclude them from
  `all-clear` and clearance computations.
- `jump.lisp` configuration transitions: read the pair's fact and use only jump-kind
  clauses (both the remote-support and supported-to-remote-ground loops).
- `ladder.lisp` configuration transitions: use climb-kind clauses of `traverse-via>` facts.

## 5. MOVE and printing

No change to the action's arguments. Examples in the new form:

```lisp
(move agent1 ((walk location1 (gate1) location17)
              (stairs location17 (staircase2) location13)))
(move agent1 ((jump location13 (edge3) location17)))
(move agent1 ((stairs location10 (gate4 staircase3) location9)))
(move agent1 ((ladder location14 (ladder2) location5)))
(move agent1 ((jump (location2 box1) (edge1) (location4 ground))))
```

Witness order stays canonical (sorted clause; ladder first for climbs, as now).

## 6. Init checks (new or changed)

1. Payload shape: list of list clauses (existing), members of registered separator types
   (gate screen wall edge staircase ladder gears...).
2. Clause kind consistency (section 2).
3. Duplicate (source destination) facts per relation (D3).
4. Named edge fits the pair: each edge in a clause must separate the source's zone from the
   destination's zone; reuse `-terrain-consistency`'s zone pairs
   (`terrain-edge-zone-pairs`). Skipped when geometry is absent.
5. Level rule for walk-kind clauses (D2).
6. Ladder: positioned at source, directed (existing check, adapted to the new shape).
7. Self-loops (existing).

## 7. File-by-file changes

tech/ (13 files read or write the relation):
- `-traversal.lisp` — relation shape, kind inference, registry, segment production, init
  checks 1-3 and 7, kind-aware family algebra (D4).
- `walkability.lisp`, `stairs.lisp`, `jump.lisp`, `ladder.lisp` — register kinds instead of
  modes; `stairs.lisp` declares `staircase`; `jump.lisp` accepts edges and rewrites its
  transition providers; `ladder.lisp` adapts its init check and transitions.
- `-walkability-coordinates.lisp` — emit the new shape (lines ~737-778); skip
  cross-level pairs; merge into authored facts (D4).
- `-terrain-consistency.lisp` — `*terrain-level-change-modes*` becomes a test on clause
  kinds; update line ~418's key test; header comment about rumin-topo's location2/location4.
- `-support-elevation.lisp` (~209-235) — jumping-fact relevance reads jump-kind clauses.
- `topo-lower-bound.lisp` (~403-430) — records have 4 elements, no mode; drop static
  separators (staircase, edge, ladder?) from relaxed gate prerequisites (keep ladders only
  if the bound currently treats them as prerequisites; check).
- `-mobility-action.lisp` — replay leniency (D6).
- `-stream-passability.lisp`, `-location-coordinates.lisp`, `-beam-los-coordinates.lisp` —
  comments only (verify no code reads the mode).
- `constraint-profile.lisp` — `*traversal-symmetric-relation*`/`-directed-relation*`
  readers, MC contracts (jump arcs ~5047, stairway arcs ~5125, contract text ~4810),
  S3 contraction and door lists (static separators are not doors), SD transit door sets.

src/:
- `ww-problem-tests.lisp` TEST-TOPO: re-record `problem-test-topo-geometry.lisp` after
  migration (state in the commit what moved and why).

doc/:
- `doc/constraint-method/Problem-Solving-Guide.md` — MOVE examples in "Quick static checks"
  and the spec-diagram checklist (drawn separators map to clause members; arrowheads to
  `traverse-via>`).

## 8. Tests

Test files that author the relation (17; update to the new shape, keep their intent):
problem-traversal-substrate-test, problem-jump-test, problem-stairs-test,
problem-ladder-test, problem-walkability-test, problem-walkability-coordinates-test,
problem-walkability-compound-door-test, problem-terrain-consistency-test,
problem-claustro-mobility-boundary-test, problem-beam-elevation-test,
problem-engine-propagation-strata-test, problem-gun-test,
problem-stream-passability-test, problem-recorder-isolation-test,
problem-recorder-playback-validation-test, problem-recorder-report-test,
problem-relaxed-hmax-test.
Also check for old-form MOVE segments or mode names (not facts): crelay-route-replay.lisp,
problem-engine-route-recording-test, problem-step-test, problem-passability-test,
problem-position-test, problem-gun-blower-test, problem-beam-los-coordinates-test.
(127 authored lines across 20 files: 3 in probs/, 17 in test/; see the Phase 0 result.)

New test problem `test/problem-traversal-separator-test.lisp` covering:
kind inference per row of the table; a pair with stairs and edge alternatives (grounded
move prefers stairs; support transition uses the edge); gate with stairs; edge plus gate
(crelay pattern); directed jump; each init check's rejection message; kind-aware
minimization (derived `()` does not erase an authored jump clause); replay of a
non-preferred clause (D6).

## 9. Phases and acceptance

Phase 0 — baseline (no code change). Run `(test-talos)` and `(test-topo)`; record pass
counts and any pre-existing failures in this doc. Recount authored facts.

Phase 0 result (2026-09-29, done):
- `(test-talos)`: 117 problems, 0 failures; 26 mutation cases, 0 surviving mutants.
- `(test-topo)`: 6 problems staged, 0 failures, after two baseline fixes to rumin-topo:
  (a) its two malformed `(traverse-via jumping location13 (edgeN) location17)` facts are
  commented out until Phase 1 restores them as edge clauses; (b) problem-test-topo-geometry.lisp
  re-recorded. The re-record was needed because two rumin facts were edited after the
  2026-09-13 recording: `stairway location13 () location17` added (+2 TRAVERSE-VIA rows,
  146 -> 148) and `stairway location9 () location10` given `((gate4))` (digest only).
  Verified: with those two facts reverted, the old recording matched exactly, so no
  derivation had moved.
- Authored `(traverse-via` lines (including commented ones): 127 across 20 files --
  3 in probs/ (claustro-topo 3, crelay-topo 7, rumin-topo 9; corner-topo authors none)
  and 17 in test/.
- Fixed, unrelated: restaging warned `redefining WOULDWORK::DERIVE-RECORDING-COPY-LITERALS in
  DEFUN`. It was the only plain DEFUN also registered as a problem function (via
  REGISTER-INIT-LITERAL-GENERATOR), so restage cleanup unbound it and the prescan installed a
  stub the real DEFUN then replaced. tech/-recorder-core.lisp now defines it with
  DEFINE-PROBLEM-HELPER. Re-run after the fix: (test-talos) 117/0, 26 mutants/0 surviving;
  (test-topo) 6/0.
- PHASE 0 CLOSED 2026-09-29. Next session starts Phase 1.

Phase 1 — core + rumin-topo. Change `-traversal.lisp`, the four mode technologies,
`-walkability-coordinates.lisp`, `-terrain-consistency.lisp`, `-mobility-action.lisp`.
Migrate rumin-topo (facts in section 1; add `staircase (staircase1 staircase2 staircase3)`
to its types). Acceptance: `(stage rumin-topo)` succeeds with no init complaints;
a hand-written replay of each rumin crossing in section 5 validates; `*traversal-cache-paranoid*`
run of a short rumin search shows no stale entries.

Phase 1 result (2026-09-29, done):
- `(stage rumin-topo)` stages with no init complaints.  First stage after the change printed
  four `redefining ... in DEFUN` warnings for the new plain DEFUNs
  (TRAVERSAL-CLAUSE-MARKER-KINDS, WALKABILITY-COORDINATES-MERGE-FAMILY,
  TERRAIN-EDGE-FIT-COMPLAINT(S)): the prescan stubs any DEFUN absent from the previously
  loaded src/problem.lisp.  One-time only; gone on the next reload + stage.
- `test/rumin-separator-replay.lisp` (new; loads and stages rumin-topo itself): 16
  hand-written MOVE cases, 10 legal and 6 illegal, plus the preferred-segment checks.
  PASS.  Grounded mobility from location13 is `((STAIRS LOCATION13 (STAIRCASE2) LOCATION17)
  (WALK LOCATION13 NIL LOCATION4))`.
- `*traversal-cache-paranoid*` rumin search at depth 8: no stale entries.
- Files changed: tech/-traversal.lisp (rewritten), walkability.lisp, stairs.lisp, jump.lisp,
  ladder.lisp, -walkability-coordinates.lisp, -terrain-consistency.lisp, -mobility.lisp,
  -mobility-action.lisp, topo-lower-bound.lisp; probs/problem-rumin-topo.lisp.
- Implementation choices beyond the plan text:
  - Registry entry is (KIND BUILDER MARKER-TYPES STATIC-TYPES PERMITTED-TYPES).  Kind is
    decided by marker types alone (walk = no marker), so the family algebra is kind-aware
    without a state; *TRAVERSAL-KIND-PREFERENCE* = (walk stairs climb jump) fixes D5.
    Registrations: walk (no markers; gate screen gears), stairs (staircase, static),
    climb (ladder), jump (edge static, wall; gate screen).
  - Static separators (staircase, edge) stay in the segment witness but are removed from
    the means handed to clearance tests (TRAVERSAL-CLAUSE-PROFILE).
  - D2's bare-level reading is applied at segment time (TRAVERSAL-CLAUSE-SEGMENT-KIND);
    the coords case is init check 5 in -traversal.
  - Walk and stairs clauses may no longer name a ladder: any clause with a ladder is a climb.
  - Ladder init check now applies per climb clause (CHECK-INIT-CLIMBING-CLAUSES): a
    directed fact may mix a climb clause with clauses of other kinds.
  - Init check 4 (edge fit) lives in -terrain-consistency with the edge-span invariant,
    since only it has the zone arrangement.
  - D6 goes through a replay-acceptor registry in -mobility, because step.lisp includes
    -mobility-action without any traversal technology.  Grounded segments only: a
    hand-written support transition must still be the provider's first choice.
  - topo-lower-bound.lisp's one destructuring fix moved from Phase 4 into Phase 1 (rumin
    includes it).  Its relaxation keeps only gates, so staircases, edges and ladders are
    free crossings there, as ladders already were.
- Behaviour changes to expect later:
  - rumin's location2/location4 fact gains the (edge1) jump the old spec had commented
    out; only the downward jump is within reach.
  - The walking derivation no longer emits cross-level walk facts, so TEST-TOPO's
    TRAVERSE-VIA rows change (re-record in Phase 3, with this as the cause).
  - -support-elevation's jump relevance diagnostic sees no jump facts until Phase 4.
  - Phase 2 test files to watch: problem-terrain-consistency-test (lines 186, 264-278 use
    *TERRAIN-LEVEL-CHANGE-MODES* and the old key layout); problem-traversal-substrate-test
    (REGISTER-TRAVERSAL-MODE, *TRAVERSAL-MODES*).
- PHASE 1 CLOSED 2026-09-29.  Next session starts Phase 2.

Phase 2 — unit tests. Migrate the test files in section 8 and add the new test problem.
Acceptance: `(test-talos)` matches the Phase 0 baseline plus the new test.

Phase 2 result (2026-09-29, done):
- `(test-talos)`: 118 problems, 0 failures; 26 mutation cases, 0 surviving mutants.
- All 17 fact-authoring test files migrated.  Most needed only the mode argument dropped.
  Changes of substance:
  - problem-traversal-substrate-test: rewritten against REGISTER-TRAVERSAL-KIND.  Three probe
    kinds (walk; stairs with a static RAMP marker; climb with LADDER) share one builder that
    labels by inferred kind.  New claims: schema (location list location), fluent index (2),
    symmetrics ((0 2)); D5 preference over canonical clause order; D6 replay; mixed-marker
    and duplicate-pair rejections.
  - Same-level jump lanes now name an edge (jump-test's six lanes, recorder-isolation's
    pickup-site/place-site): a same-level () is a walk, and D7 gives a walk no support
    landing.  Their expected witnesses gained the edge.
  - Every stairway fact names a staircase (stairs-test, terrain-consistency-test,
    claustro-mobility-boundary-test's stairs lane, jump-test lane 4).
  - claustro-mobility-boundary-test keeps () on its two cross-level jumps: it is a
    bare-level problem, so it now exercises D2's bare-level jump reading.
  - jump-test's edge-rejection claim is inverted (edges now mark jumps but are never
    vaulted); walkability-test's ladder-in-a-walk scenario is removed and replaced by a
    claim that such a clause is refused; ladder-test's authoring claim follows the per-clause
    check (two cases whose premise no longer exists were replaced by symmetric-ladder-in-a-
    second-clause and mixed-kind directed fact).
  - D4(a) is pinned in walkability-coordinates-test (the coincident loft pair gets no fact)
    and terrain-consistency-test (STAIR-LOW/STAIR-HIGH's family is exactly ((STAIR1))).
- New test/problem-traversal-separator-test.lisp, on coordinate geometry with real builders:
  staircase-or-edge preference vs the support transition's edge; gate-on-edge (crelay) open
  and shut; directed jump; kind-aware merge (derived () + authored ((LEDGE)) = (NIL (LEDGE)),
  the jump clause still used for a support landing); D2 rejection; edge-fit (init check 4)
  driven through TERRAIN-EDGE-FIT-COMPLAINT with TERRAIN-ARRANGEMENT-FOR-STATE.  Gates in a
  coordinate problem need GATE-SEGMENT> records; here both stand on EDGE1 as supported doors.
- Two tech fixes, pulled forward:
  - tech/-mobility.lisp, MOBILITY-SEGMENT-REPLAYABLE-P (Phase 1 bug): replay offered a
    support transition to the D6 acceptors, whose configuration endpoint crashed
    TRAVERSAL-PAIR-FAMILIES.  Acceptors are now consulted only for a segment ending at a
    location.  Exposed by jump-test's printed solution.
  - tech/-support-elevation.lisp (from Phase 4): VERTICAL-REACH-JUMP-FACT-RELEVANT-P reads the
    4-element fact and judges only jump-kind clauses; VERTICAL-REACH-JUMPING-RELEVANT-P no
    longer filters on 'JUMPING.
- Secondary files checked: engine-route-recording, step, passability, position, gun-blower
  need nothing; beam-los-coordinates had one stale comment.  test/crelay-route-replay.lisp
  is deferred to Phase 3 with crelay-topo (its jump witnesses gain EDGE1).
- PHASE 2 CLOSED 2026-09-29.  Next session starts Phase 3.

Phase 3 — other topo specs. Migrate claustro-topo, corner-topo, crelay-topo; confirm
phobia-topo and windtunnel-topo stage. Acceptance: `(test-topo)` passes after deliberate
re-recording, with each geometry change explained.

Phase 3 result (2026-09-29, done):
- `(test-topo)`: 6 problems staged, 0 failures, after a deliberate re-record of
  problem-test-topo-geometry.lisp.  Every digest moved because every TRAVERSE-VIA row lost its
  mode argument.  The row counts that moved were each checked against the arrangement: the
  drop is exactly two rows per same-zone pair at different levels, which the walking
  derivation no longer emits (D4a); no drop came from merging authored and derived facts.
    claustro 100 -> 96 (2 pairs), crelay 388 -> 350 (19), phobia 118 -> 100 (9) and
    TRAVERSE-VIA> 38 -> 32 (3 directed pairs), rumin 148 -> 106 (21).  corner (12) and
    windtunnel (24 / 6) unchanged in count.
- claustro-topo: `staircase (staircase1)` added; location10/location12 names edge1,
  location13/location11 names staircase1, the ladder fact drops its mode.  The edge1 comment
  no longer describes the inert cross-level walk the derivation used to emit.
- crelay-topo: the three alcove jumps name `(edge1 gate2)` (gate2 stands on edge1); the ladder
  facts drop their mode.  location20/location5 had nothing drawn between them: location20 is
  the top of blower1's stream.  Decision with D: a floor drive is a jump marker, static like an
  edge, so the fact is `(traverse-via location20 ((blower1)) location5)` -- a drop off the side
  of the stream.  A `()` there would have claimed a walk, and the bare-level jump reading does
  not apply with coordinates (D2).
- tech/jump.lisp: jump registers `floor-blower` and `floor-gears` as markers and statics
  (declared optional).  Wall drives are not markers: their destination is ordinary ground.
- test/crelay-route-replay.lisp: witnesses follow the new facts ((BLOWER1), (EDGE1 GATE2)).
  PASS; its 27-action sequence validates.
- corner-topo authors no traversal facts; phobia-topo and windtunnel-topo stage unchanged.
- The frozen crelay T10 checkpoint (t10-final-checkpoint.txt) holds three old-form jump
  witnesses, so its Restore replay no longer validates on the current tree.  Left
  byte-identical; doc/problems/crelay-topo/Handoff.md says it restores at commit cfb7c11 and
  gives the new witnesses.
- PHASE 3 CLOSED 2026-09-29.  Next session starts Phase 4.

Phase 4 — analysis tooling and docs. `topo-lower-bound.lisp`, `-support-elevation.lisp`,
`constraint-profile.lisp`, the Guide. Acceptance: regenerate rumin-topo's static profile;
S3 regions and SD door sets list no edges or staircases as doors; MC contracts describe
kinds.

## 10. Risks

- Kind-aware minimization (D4) is the subtle part; a wrong subsumption silently removes
  support-transition options. Covered by the new test.
- Traversal cache: dependencies should not change, but run the paranoid check (see
  claude/traversal-caching-plan.md).
- Recorder solutions, crelay route replays and any stored checkpoints hold old-form MOVE
  segments only if their witnesses change; labels are unchanged. Re-validate crelay's
  route replay in Phase 3.
- The frozen crelay-topo experiment (prediction register) must not be edited beyond the
  mechanical fact migration; note it in its records.

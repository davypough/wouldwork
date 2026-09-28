# Constraint-Led Method — Post-Mortem 2026

T17, opened 2026-09-25. Written with D section by section; a section is final
only when D has agreed it. Roles in the discussion: D supplies domain knowledge
and guidance (the puzzle, its tricks, what matters); the assistant supplies the
technical analysis and recommendations. Evidence is named by file; paths are
relative to `doc/problems/crelay-topo/` unless they start with `doc/` or `tech/`.

Sections: 1 Architecture (FINAL) · 2 Strategy (FINAL) · 3 Effectiveness (FINAL) ·
4 Weak points (FINAL) · 5 Improvements (filed as PROPOSED tasks when agreed).

## 1. Architecture — FINAL, agreed by D 2026-09-25

### 1.1 Shape

The method has a static part (discovery: a pure function of the staged
problem) and a dynamic part (interaction: ledger, recommender, ingester,
question generator, standalone checkpoints). The discussion settled that
these are neither separate stages nor free interleaving, but three loops at
different rates:

1. **Static, once per problem, as complete as possible.** Every constraint the
   problem definition and tech/ semantics determine, whether or not it is
   later used (D, 2026-09-25).
2. **Static queried against the current state, at every interactive step.**
   Before D proposes a move or a search is recommended, the static tables are
   read from the checkpoint's state, not only from the initial state.
3. **Schema growth.** A question the static part failed to ask, exposed by D's
   insight or by a search surprise, becomes a new or extended component.

In T10 only loop 1 existed, it ran once, and its output was then rarely
consulted. Loop 3 happened informally (RC/T16, G15–G17). Loop 2 did not exist;
`tech/constraint-state-probe.lisp` (one step ahead, applicable actions only)
is the nearest thing.

### 1.2 Finding: the plan did not use the loop's spine work

The final 87-action solution (`constraint-evidence/t10-final-checkpoint.txt`)
starts from a fresh stage (`constraint-evidence/validate-b2-ghost-tray-2026-09-24.lisp`,
then `validate-c3-alt-2026-09-25.lisp`). None of the 19 search-found spine
actions (lk1–lk4, lk7–lk9; `Constraint-Realization-Ledger.txt`) are in it,
and lk5/lk6 were never closed. The interactive machinery carried the spine
decomposition; the plan came from D's recorder-cycle design (pr18–pr22),
hand-derived and validated in stages, plus a 7-action search-found final leg.

### 1.3 Finding: static facts existed but were not consulted

- **lk4 exhaustion (bd1, 23 s, 9.25 GB).** S4's controller row already said
  switch2 is operable only from location14, off the spine, behind gate6 needing
  plates 4 and 5 held by two keepers other than the agent (pr15, pr16, grade 1;
  G16). Plan, T10 entry, "The first exhaustion".
- **Two devices already open at the start (pr14, grade 1)**, derivable from
  DEFINE-INIT and S1 before any search; noticed only after lk2/lk3 cost one walk
  each. Plan, T10 entry, finding 2.
- **Cycle 3 part A.** A grounded agent cannot place onto a held tray (top 3/2,
  reach 1). S5's placement matrix already said NO (register 7.32); it was
  rediscovered while writing the route. `constraint-evidence/b2-ghost-tray-loc5-check-2026-09-24.txt`.

### 1.4 Finding: where constraints come from

Four kinds appeared in crelay-topo:

| Kind | crelay-topo example | Found by |
|---|---|---|
| Fixed facts | switch2 only at location14; gate6 needs plates 4+5; held tray unreachable from ground | Static, up front (then ignored) |
| State-dependent rules | switch1 off opens gate2 but stops blower1 and drops a lifted agent | Static can list the rule; the current state says whether it bites |
| Plan-structure constraints | who holds plate3 while the ghost carries the stack; bodies off plates while gate8 is open | Need a proposed cycle plan (RO cannot run without a scenario) |
| Unasked questions | a door needs an off-route excursion (G16); a beam needs relay-to-relay links (G17); a toggle removes the actor's lift (G15) | Surfaced by interaction; computable up front once asked |

### 1.5 Finding: coverage follows crelay-topo's needs, not tech/'s mechanics

tech/ covers every object type a problem can use (D, 2026-09-25), so complete
up-front discovery means a static contract per tech mechanic. The profile does
not have one:

- **floor-blower** appears only in S0's type census
  (`Constraint-Static-Profile.txt`, lines 12–52). No extractor knows that
  blower1 lifts an agent from location4 to location20, or that switch1 couples
  the lift to gate2. The 2026-09-22 hand audit found that coupling from source
  and concluded no landing existed
  (`constraint-evidence/b2-revised-construction-analysis-2026-09-22.txt`,
  section 3); D supplied the landing, the ghost-held tray at location5 (pr19).
- **ladder** is included by the instance and also has no extractor.
- By a text search of `tech/constraint-profile.lisp` (indicative, not exact),
  gun, stairs, angled/wall blowers, gears, jammer, threat, beam-crossing and
  beam-direct have none either.

### 1.6 Component verdicts

| Component | Role in T10 | Verdict |
|---|---|---|
| S0–S2 (types, control algebra, pressure/supply) | Supplied pr1–pr10; none contradicted | Keep |
| S3 region spine | Drove the decomposition, but relaxes elevation and support (pr3), the conditions that mattered | Revise: a route candidate, not a decomposition |
| S4 controller/cut rows | Held pr14–pr16 before the searches that exposed them | Keep; join to the route (G16) |
| S5 height/reach lattice | Held the placement NO rediscovered in cycle 3 | Keep |
| S6 sightlines + RC relay chains | RC settled AM4b; chains 8/20 have the shape of D's pr21 beam | Keep; feed S4 and T6 (G17). Open: whether RC informed pr21 |
| S7 landmark graph | MISS on a location-only goal (register 7.36) | Revise: needs a route relaxation or keying on crossed devices |
| T6 budget arithmetic | Correct, not decisive | Keep; fold into the cycle-plan check (I2) |
| RO role obligations | CONDITIONAL throughout; needs a stated scenario (register 7.27) | Revise into the cycle-plan check (I2) |
| Ledger + ingester | Carried provenance; recorded pr17–pr22 | Keep |
| Recommender | Unit is a spine link; bypassed once D's hand-derive rule took over | Revise: recommend per proposed stage, not per arc |
| Question generator | qn1–qn9 never answered; only qn10 (pr17) | Replace with the domain interview (I4) |
| Standalone checkpoints | Restarts, final leg, final validation | Keep |

### 1.7 Architecture improvements (candidates for PROPOSED tasks)

- **I1 Loop 2: state-based static queries.** Static tables queryable from any
  checkpoint state; each checkpoint prints a short "what the static part says
  from here" report (reachable placements, live beam chains, controller
  locations and their access) before D chooses the next move.
- **I2 Cycle-plan check.** When D proposes a recorder-cycle plan, check it
  against plate, body and view budgets (T6, RO, RC) before any action is
  written out.
- **I3 Coupling census.** Flag every action or control that changes two
  subsystems at once (switch1 → blower1 and gate2), where latent constraints
  sit; makes G15-type findings routine.
- **I4 Domain interview.** Before search, D lists the problem's tricks in plain
  words; each becomes a hypothesis the static part checks at once. Replaces
  the question generator.
- **I5 Mechanic coverage.** Each tech mechanic has a declared static contract
  (what it controls, lifts or moves, and requires); the profile prints
  UNCOVERED for any included mechanic without one. In crelay-topo it would have
  printed floor-blower and ladder on day one.

## 2. Strategy — FINAL, agreed by D 2026-09-25

### 2.1 What T10 did

The static route came first: one milestone per S3 spine arc (lk1–lk9), each
realized by bounded search. When the route decomposition stalled (lk5/lk6
OPEN; B2 search exhausted at cutoff 12), D's working rule took over
(2026-09-24): hand-derive and validate what can be written out; search only
what cannot. D's recorder-cycle design (pr18–pr22) then produced 80 validated
actions, and one search found the 7-action final leg (section 1.2).
D's plan came last; it should come first.

### 2.2 Target sequence for a new problem (best case)

D = the user (domain knowledge, guidance); A = the assistant (technical
analysis); Engine = Wouldwork. crelay-topo is used as if new.

**Search settings (D, 2026-09-25).** D gives each problem its own maximum
search depth; every search runs at `*threads*` 16. Technical consequence: all
searches use the standalone checkpoint form (two-argument `SOLVE-SUBGOAL` from
a checkpoint or stated start), since the goal-chaining form requires
`*threads*` 0. A recommendation above D's maximum is not made; a need for more
depth becomes a question to D.

**Phase 0 — Static, no D input**
1. D writes and stages the instance and gives the maximum search depth. A runs the full static profile, including
   mechanic coverage (I5) and the coupling census (I3).
2. Coverage gate: any UNCOVERED mechanic gets its static contract before going
   on (crelay-topo: floor-blower, ladder).

**Phase 1 — Shared understanding, one exchange**
3. A briefs D in domain terms: the goal (location19 behind gate9, plates
   6/7/8); gates crossed and where their controllers are (switch2 only at
   location14, behind gate6 needing two plates); couplings (switch1 → blower1
   and gate2; switch2 → gate7 on, gate5 off); beam needs (plate3 held, at least
   two bodies off plates); body budget (more plates than bodies, so ghosts).
4. Domain interview (I4): D lists the tricks in plain words (ghost holds a tray
   as a step; ghost connector plus live connector light the beam; three
   cycles). A checks each against the static tables at once: consistent,
   contradicted, or needs a prerequisite.

**Phase 2 — Plan outline**
5. D proposes stages (cycle 1 connector1 to plate1; cycle 2 fetch box1; cycle 3
   build and carry the lit stack; final leg).
6. Cycle-plan check (I2): A checks each stage against plate, body and view
   budgets and reports conflicts (e.g. gate8 open needs plate3 held while the
   stack moves, so a ghost must stand there). D revises until it passes. The
   plan is recorded as D's premises before any action is written.

**Phase 3 — Stage loop, per stage**
7. "From here" report (I1) at the current checkpoint: reachable placements,
   live beam chains, accessible controllers.
8. A chooses the realization: hand-derived actions for D to validate, or a
   bounded search with its readings committed before the run.
9. Engine result → checkpoint export → ledger; A checks the endpoint against
   the stage's intent.
10. A surprise becomes a schema gap (loop 3) and, if needed, one question to D.

**Phase 4 — Closure**
11. Final search from the last checkpoint to the goal, then
    `validate-search-checkpoint`.
12. A records premises by provenance and the gaps found.

### 2.3 Best-case expectations

About three D turns (interview, plan, one mid-course answer); searches only
for legs that cannot be written out; no search spent on a fact the static part
already holds. T10 for comparison: 22 reported searches, the lk4 exhaustion
the profile predicted, and 80 actions from D's design after the spine
decomposition was abandoned.

## 3. Effectiveness — FINAL, agreed by D 2026-09-25

### 3.1 What the method derived, searched and was given

- **Derived (static).** pr1–pr10 and pr14–pr16 (`Constraint-Realization-Ledger.txt`):
  regions and gates on the route, plate demands, the two gates open at the
  start, switch2's off-route location behind a two-keeper gate6. The beam
  requirements: gate8 open needs plate3 held; at least two bodies off plates
  while lit (AM4/AM8, `constraint-evidence/am4-am8-rederivation-2026-09-24.txt`;
  RC, register 7.38).
- **Searched.** Seven of nine spine links (19 actions, none in the final plan);
  the 7-action final leg (cutoff 12 out of memory, cutoff 10 found it). 22
  reported searches in all.
- **Given by D.** The plan's core ideas, pr18–pr22: the setup cycle, the
  ghost-held tray as a landing, the two-connector beam, the three-cycle design.
  80 of 87 actions came from D's design. lk5/lk6 were never closed.
- **Extractor accuracy** (register section 7): S0–S2 regenerated after
  corrections; S3, S4 partial; S5 missed then regenerated; S6, S7 disclosed
  regeneration checks (S7 a MISS); RC five hits, one partial. Schema gaps
  G1–G17 record 17 questions the schema failed to ask.

### 3.2 Verdict

A reliable **checker**, not yet a **discoverer**. When consulted, the static
part was right about what is required or impossible; it did not produce the
ideas that solved the problem. D (2026-09-25): most of the crelay-topo ideas
came from D's knowledge of the puzzle. crelay-topo was the prototype and could
not test discovery, since its solution was known.

### 3.3 Aim for new problems (D, 2026-09-25)

Constraints and small searches should **suggest promising paths** that inform
D's intuition, not only check D's proposals afterwards.

### 3.4 Effectiveness improvements (candidates for PROPOSED tasks)

- **I6 Necessity hints.** The static part turns each limit into a candidate
  plan element, printed in the Phase 1 briefing. crelay-topo examples: more
  plates than live bodies → "ghosts must hold plates; at least k cycles";
  gate8 needs plate3 → "when crossing gate8 a non-crossing body stands on
  plate3"; RC → "candidate beams: ground connector at location9 plus raised
  connector at location15" (D's pr21); switch1 lift/gate2 coupling → "the lift
  needs a landing; look for a support at location5 or location6" (D's pr19).
- **I7 Probe battery.** Before D's interview, a set of small searches from the
  start state, one per resource or landmark (box1 out of the alcove; any body
  on plate3; agent at location20; a connector paired with the repeater), each
  within D's per-problem maximum depth at threads 16. Output: a map of
  cheap / not found within the cutoff / blocked subgoals. Not-found is a cost
  bound, never an impossibility.

Both run before the domain interview (section 2.2, between steps 3 and 4), so
they can shape D's intuition rather than check it afterwards.


## 4. Weak points — FINAL, agreed by D 2026-09-25

### 4.1 Documentation does not match the phased strategy — FINAL, agreed by D 2026-09-25

**Measured.** The constraint-method documents for one problem total about
1.1 MB. The start-of-session reading alone is the plan (1,931 lines) plus the
continuation prompt (1,950 lines). In the continuation prompt, about 420 lines
are stacked "current/previous handoff" blocks and about 1,050 lines are dated
updates from 2026-09-20 to 2026-09-22; the rules sit in between. Current state
is kept in three places (plan Current Task, continuation prompt, restart
checkpoint), and they drifted (e.g. the D1–D3 validation status). The ledger is
not a file of record: it is rebuilt by replaying 13 ingest scripts in order.
`constraint-evidence/` holds 107 files, flat, with no index.

**By phase (section 2.2):**

| Phase | Needs | Exists |
|---|---|---|
| 0 Static | profile, coverage report | profile (generated); no coverage report |
| 1 Understanding | briefing, D's tricks | none; Abstract-Model is a hand-written crelay-topo precursor; tricks entered the ledger after the fact (pr18–pr22) |
| 2 Plan outline | D's stages, cycle-plan check results | none; D's cycle plans live in a running evidence log |
| 3 Stage loop | per-stage status, checkpoints, results | ledger organized by spine link; restart checkpoint; flat evidence |
| 4 Closure | provenance, gaps | ledger, schema gaps, register |

**Per document.** Paths under `doc/`.

| Document | Content | Verdict |
|---|---|---|
| constraint-method/Constraint-Implementation-Plan.md | board, conventions, policy, and every task's full history | Revise: board, conventions, policy, current task only; completed task entries to an archive |
| constraint-method/Status-Algebra-and-Record-Schema.md | ledger specification | Keep; revise with the stage-organized ledger |
| constraint-method/Launch-Configuration-Checklist.md | G15 manual procedure, never applied | Merge into the specifications of I2/I3 |
| constraint-method/evidence/ | method-level check files | Keep |
| problems/crelay-topo/Constraint-Continuation-Prompt.txt | stacked handoffs, rules, dated history | Replace with a short Handoff (current state only); rules to the Method Guide; history to archive |
| problems/crelay-topo/Constraint-Restart-Checkpoint.txt | archive inventory, hashes, cold restore, ledger rebuild order, stacked handoffs | Merge current archives and restore commands into the Handoff; archive the rest |
| problems/crelay-topo/Constraint-Prediction-Register.txt | §3 extractor specifications (the only S0–S7 spec, domain-general); §4–6 predictions; §7 ~2,600 lines of scores | Move §3 to a method-level Extractor Specifications document; FREEZE the rest as the record of the crelay-topo validation experiment (D, 2026-09-25) |
| problems/crelay-topo/Constraint-Role-Obligations.txt | RO and G14 specifications plus history | Specifications to Extractor Specifications; archive the rest |
| problems/crelay-topo/Constraint-Abstract-Model.txt | hand model AM1–AM9 | Archive; it is the template for the generated Briefing |
| problems/crelay-topo/Constraint-Schema-Gaps.txt | G1–G17, domain-general by design | Move to method level, one register across problems, each gap tagged with its origin |
| problems/crelay-topo/Constraint-Static-Profile.txt | generated | Keep; add the coverage report (I5) |
| problems/crelay-topo/Constraint-Realization-Ledger.txt | spine-link ledger rebuilt from 13 scripts | Revise: organized by stage, and the file itself is the record |
| problems/crelay-topo/constraint-evidence/ | 107 files, flat | Keep; per-stage subfolders and an index for new problems |

**Decision (D, 2026-09-25): prediction registers are dropped.** With reading
unrestricted, new problems commit no predictions before the static run. The
crelay-topo register is frozen: no further commitments or scores are appended.

The Backward-\* and Forward-\* files belong to the earlier methods and are not
in this review.

**Target file set.**
- Method level: Plan (short), **Method Guide** (new: the section 2.2
  procedure, grades and working rules), **Extractor Specifications** (new: from
  register §3, the RO/G14 specifications, RC and T6), Schema Gaps, Record
  Schema, Post-Mortem, `archive/`.
- Per problem: **Briefing** (new, Phase 1: generated summary plus D's tricks
  and their checks), **Stage Plan** (new, Phase 2: D's stages, cycle-plan check
  results, per-stage status), Static Profile, Ledger, **Handoff** (one short
  current-state file), `constraint-evidence/` with an index.


### 4.2 Process overhead — FINAL, agreed by D 2026-09-25

Every T10 search needed its own approval from D, and most probes produced
three evidence files: proposal, recommendation, result (e.g.
`constraint-evidence/switch2-return-access-*`, `gate6-source-resource-*`).
Many D turns went to approvals rather than domain input.

**Decision (D, 2026-09-25).** Approval moves to the stage plan (Phase 2). Once
D approves a plan, the assistant issues as many searches as the plan's stages
need, within D's per-problem maximum depth, at `*threads*` 16, without asking
per search. Searches outside the approved plan, or above the maximum depth,
still need D. Technical default: a stage's searches go into one load file for
D's REPL on lumpy, with one evidence record per stage rather than three per
probe.

### 4.3 Tooling faults — FINAL, agreed by D 2026-09-25

| Fault | Cost | Status |
|---|---|---|
| Filesystem extension fails on every call (JSON-schema dialect) | a turn per session; all file work via the shell | Open, outside wouldwork; the Method Guide says use the shell |
| B1 checkpoint export crashed on a PowerShell pathname after a successful search | result rebuilt by replay (`constraint-evidence/b1-baseline-result-2026-09-22.txt`) | Worked around |
| Parallel searches did not measure cutoff truncation | early exhaustion readings unreliable | Fixed 2026-09-21 (`doc/constraint-method/evidence/cutoff-reporting-fix-2026-09-21.txt`) |
| Validation of mixed plain/timestamped action lists | spurious final-validation failure | Fixed 2026-09-25 (`src/ww-solution-validation.lisp`) |
| Cutoff-12 search from the 80-action checkpoint ran out of memory | one lost search; rerun at cutoff 10 | Open → I8 |

- **I8 Memory estimate before search.** From earlier searches' growth on
  lumpy, warn when a search at the requested depth is likely to exhaust
  memory, so the cutoff is lowered before the run rather than after a crash.

### 4.4 Contamination — CLOSED

Exposure from 2026-09-22 on is moot under the Reading policy and the frozen
register (section 4.1). Kept as history.

## 5. Improvements — FINAL, agreed by D 2026-09-25

Filed as PROPOSED tasks on the plan's board, in D's agreed build order. Each
needs its own acceptance criteria and D's approval before work starts.

| Order | Task | Improvement | Source |
|---|---|---|---|
| 1 | T18 | Document restructure: Method Guide, Extractor Specifications, method-level Schema Gaps, short Plan; per problem Briefing, Stage Plan, Handoff, stage ledger, indexed evidence; history archived | 4.1 |
| 2 | T19 | I5 Mechanic coverage: a static contract per tech mechanic; UNCOVERED report; first contracts floor-blower, ladder | 1.5 |
| 3 | T20 | I3 Coupling census | 1.4, 1.7 |
| 4 | T21 | I6 Necessity hints | 3.4 |
| 5 | T22 | I1 State-based static queries ("from here" report) | 1.1, 1.7 |
| 6 | T23 | I7 Probe battery | 3.4 |
| 7 | T24 | I2 Cycle-plan check (T6, RO, RC budgets) | 1.7 |
| with T18 | T25 | I4 Domain interview (procedure in the Method Guide; replaces the question generator) | 1.7 |
| any time | T26 | I8 Memory estimate before search | 4.3 |

Standing decisions from this post-mortem (not tasks): Reading policy (nothing
sealed); prediction registers dropped, crelay-topo register frozen; D gives
each problem a maximum search depth, all searches at `*threads*` 16 in the
checkpoint form; approving a stage plan pre-approves its searches within that
depth.

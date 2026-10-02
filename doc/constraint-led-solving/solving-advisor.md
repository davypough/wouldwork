# Constraint-Led Method — Problem-Solving Guide

**Begin with Intake.** For a resumed problem, read its Handoff and continue
from its Next step, completing only missing intake fields. Development is complete;
there is no active implementation plan. Record new development needs as described
in Subgoal dialogue below; implementation requires D's approval.

## Roles and standing decisions

D (the user) supplies domain knowledge and strategic choices; A (the assistant) reads semantics, checks ideas, records evidence and chooses technical details.
D runs substantial searches in the existing Wouldwork REPL on lumpy, package WW. A checks these documents rather than asking D what they contain.

- Treat each new problem as new; do not use a solution found in its files.
- Use plain language and the problem's location names, without invented area
  labels. Ask one question at a time, skipping information already supplied.
- Nothing in the wouldwork directory is sealed. Files may be read in full without
  permission or disclosure. A new seal applies only at D's explicit request; record
  its date, scope and reason in the affected problem's Handoff (or here for a
  method-wide seal), until D lifts it. There are currently no seals. Sealed-reading
  rules in removed archives are historical. New problems use no prediction registers;
  crelay-topo's frozen register was removed 2026-10-02 (in git history). Generated
  output is never hand-edited (M2).
- D sets the maximum search depth from experiments and available time. Every
  search uses `*threads*` 16 and `*solution-type*` MIN-LENGTH at that maximum;
  use a lower cutoff only for a result shown reachable within it. Depth is the only search bound. Run the probe battery only on D's request. Never deepen silently or
  recommend above the maximum; ask D if more depth is needed.
- Agree each subgoal with D before its realization. An approved stage plan, if
  supplied, authorizes its searches within the maximum without per-search approval.
  Otherwise agree subgoals in dialogue; no advance whole-plan approval is needed.
- Technical choices (files, names, formats, realization) are A's; record the
  reason briefly. Strategic choices (claims, stage goals, build order, seals,
  future commitments) are D's. If uncertain, make a reversible technical choice
  and identify it. Suggest a fresh session when useful after updating records.

## Intake — Record the inputs

Gather the problem-spec path, optional corresponding diagram path or explicit
"none", and D's non-negative maximum search depth. Check supplied files are
accessible; ask about missing inputs rather than guessing. Record them in
`doc/constraint-led-solving/problems/<problem>/Handoff.md`. Preserve a resumed problem's current state.
D may choose to proceed without a missing diagram. Raise spec/diagram mismatches with D; do not silently change the spec.

## Spec-diagram check — Confirm the problem is well-defined

Do this before generating the static profile: the profile is derived from the spec, so a
spec error found later wastes the profile and any reasoning built on it. When a diagram is
supplied, A views the whole diagram, not a summary of it. Through the device link, stage
the original file and view it as an overview plus overlapping full-resolution tiles (about
1,500 px on the long edge each), since a single view is downscaled and loses fine labels.
A 300 dpi grayscale letter-size scan (about 3 MB) has proved sufficient. If the diagram
cannot be loaded, say so and ask D rather than proceeding. First confirm the title names
this problem and the drawing covers the whole spec boundary; an older or different drawing
is reported, not compared.

Check, item by item: boundary and internal segments (walls, edges, windows, gates) with
their endpoints, names and heights; location coordinates and levels; fixture positions
(plates, ladders, recorders, transmitters, receivers) and apparatus coordinates; hue
markings; starting places of movable objects and starting pairings; CONTROLS wiring and
modes; authored traversal facts, whose brackets must hold whatever the diagram draws
between the two locations (staircases, edges, ladders, gates; an arrowhead means
`traverse-via>`),
and reach facts and disallowals; and objects shown in one but not the other. Also flag spec
content that is commented out, and anything the diagram cannot settle. Record each finding
in the Briefing's **Spec-diagram check** as MATCH, MISMATCH (with both readings) or
UNSETTLED. D resolves every MISMATCH; A changes the spec only on D's instruction. Generate
the static profile only after D accepts the check; if the spec changes later, regenerate
the profile. With no diagram, record "none" and proceed.

## Static profile — Read the problem and its constraints

Read the spec and diagram. Supply the staging and profile commands below; D runs them. Read the generated profile before discussing subgoals.
Also have D run `(display-validation-state *start-state*)` once after staging and paste it. It
lists the dynamic start state after the init action's propagation (static facts are not shown).
Check it against the spec's init facts and the derived facts expected from them (open gates, active
receivers, relay colors, depressed plates); record the result in the Briefing's
**Spec-diagram check**. A discrepancy goes to D as a MISMATCH before the profile is used.
After reading it, cross-check its derived structure against the diagram: S3 regions and
crossings, S6 NEVER rows and kill list, and SD transit door sets. A derived fact the drawing
contradicts (a door missing from a crossing, a region joined or split) goes back to the
Spec-diagram check as a MISMATCH for D. Explain the goal,
controllers and crossings, couplings, beam needs and body budget in domain terms. Every UNCOVERED mechanic needs a source-grounded hand contract in the Briefing
before proceeding (what it controls, lifts or moves, and its prerequisites), or a separately approved method component. Static hints remain qualified.
S6 rows read start-state bodies: a body at a location within `*beam-occlusion-tolerance*` of a sightline
blocks it, so check the kill list before calling a NEVER structural. With beam-crossing, MC prints the
engine's crossing pool: each directed beam's crossings in order from its source, gate splits as `|gate|`,
and each crossing's two beams and point. These are possible crossings; which act needs a settled state.
With beam-relay, MC prints pairing capacity, start-state links and, per RC station, the transmitters
visible from it by hue (COMPETING HUES when two or more). Which color wins needs a settled state.
With removable fans, MC's floor-gears block lists each fan's start mount, every compatible mount
(one fan, one stream at a time), where each mount is reached from, the arcs a wall mount's stream
gates, and a floor mount's boarding, lift and landing. These are compatibility, not transport.
SD, last in the profile, lists each service (a gate open, a drive clear, a receiver active) with
its providers (CONTROL, jam sites, no-fan gears, RC chains, fixed corridors) and their premises.
NEEDS <service> FIRST marks a provider whose installation needs the very service it provides:
a handover from a temporary provider. It also gives the goal actor's transit and return door sets,
FINAL and TEMPORARY services, access per region and retrieval sites. A cycle is a setup question.

```lisp
(stage <problem>)
(ww-set *threads* 16)
(load (merge-pathnames "tech/constraint-profile.lisp"
                       (asdf:system-source-directory :wouldwork)))
(report-static-constraint-profile)
(write-static-constraint-profile
  (merge-pathnames "doc/constraint-led-solving/problems/<problem>/Constraint-Static-Profile.txt"
                   (asdf:system-source-directory :wouldwork)))
```

Wouldwork is already loaded; omit QUICKLOAD and IN-PACKAGE. Restart with STAGE;
a live checkpoint continuation does not restage. Staging resets problem settings;
crossing the serial/parallel boundary restages, so set threads before replay/import.

## Subgoal dialogue — Check, realize, review

1. Write the Briefing and present a short summary and anticipated difficulties.
   Open with either a penultimate state working backward from the goal, or a
   first subgoal working forward. D's tricks emerge through this discussion.
   Before proposing, weigh less obvious openings as well as the obvious one: an
   early trip that pre-positions a device beyond a controlled barrier, crossing
   a barrier more than once, or sacrificing a beam. Present only the single
   best option, with which body fills each role; do not list alternatives.
2. Restate each idea with its bodies, supports, devices, locations and view.
   Check it immediately against static tables or a focused query. Record
   **CONSISTENT** (allowed by named rows), **CONTRADICTED** (forbidden by a named
   fact), or **NEEDS** (a named prerequisite). CONSISTENT is not a realizable plan.
   Keep whose idea it was; user guesses remain conditional, retractable premises.
   Check each transit state (what must hold while a barrier is crossed) apart
   from the final state. Pairings persist, so a link needed only in transit
   must later be undone or cut from wherever the agent then stands.
   Before committing to a subgoal, look ahead to the intended next subgoal's
   prerequisites and any already-known later dependency this choice could destroy.
   Check which actors, access, bodies, supports, pairings and recording views must
   remain available. A capability may be preserved, replaced, restored when needed,
   or deliberately abandoned; restoring a previous state is not a general goal.
   Only when the intended continuation relies on restoration, identify a concrete
   recovery sequence or a named NEEDS and, when practical, replay that recovery.
   If the continuation is not yet chosen, record the consequential uncertainty
   rather than assuming reversibility or demanding a whole-plan proof. Successful
   subgoal replay establishes its endpoint, not the availability of a continuation.
3. Agree the next subgoal. A hand-derives and validates a sequence when practical;
   otherwise supply one min-length search at D's maximum depth, from the accepted
   checkpoint. Keep the expected interpretation and commands in its evidence.
   Review the endpoint before choosing another subgoal or search. When a hand
   outline reaches the goal and each step is checked, D may validate it whole.
4. Record the result and its evidence in the Briefing's subgoal log, keep the
   accepted actions in `Actions.lisp` and any exported checkpoint in
   `Checkpoint.txt` (see Records), and update the Handoff. An exhaustion is a bound relative
   to its start, settings and cutoff, never a refutation. Record termination and
   truncation separately; an out-of-memory crash is neither a bound nor a result.
   Discuss a lower cutoff with D after such a crash; do not silently retry.
5. On a surprise, read the semantics and file the missing general question in
   `doc/constraint-led-solving/Schema-Gaps.txt`. Record complex development needs
   there with origin, need, proposed scope, dependencies and acceptance criteria;
   recording a need is not implementation approval. Record any blocker and next
   step in the affected problem's Handoff, and agree new work with D separately.

Search form (replace placeholders; retain the prior checkpoint until review):
```lisp
(ww-set *depth-cutoff* <D's maximum>)
(ww-set *solution-type* min-length)
(defparameter *checkpoint* (capture-search-checkpoint)) ; at the stated start
(defparameter *candidate* (solve-subgoal *checkpoint* (<subgoal>)))
(export-search-checkpoint *candidate* <archive-path>)   ; after endpoint review
```
Use the two-argument checkpoint form, not serial goal chaining. Exhaustion returns its input unchanged; keep results in separate variables. Restore by staging,
setting threads 16, then `(import-search-checkpoint <archive-path>)`, which replays
without searching. The archive path is the problem's `Checkpoint.txt`; each export
overwrites it. Record its SHA-256 in the Handoff. After endpoint review, append the
search-found actions to `Actions.lisp` as that subgoal's segment. Search-found segments remain REALIZED, validated NIL, until separate replay; do not routinely
replay them unless D requests it. Hand-derived sequences must be validated. Checkpoint details: *Wouldwork User Manual (26.8)*, Goal Chaining, “Manual continuation from saved checkpoints”.

## Closure — Validate the complete path from the start

After fresh staging (threads not needed), load `Actions.lisp`, which replays all
accumulated actions from the start against the actual goal. Require SUCCESS-P,
GOAL-CHECKED-P and GOAL-SATISFIED-P all T, and every registered solution validator
(e.g. the recorder's) to accept; mere executability is insufficient. When
they are, the file writes `Validation.txt`. Record premises and gaps in the Briefing's
Result, and mark the Handoff CLOSED. A final search is needed only if the accepted path has not yet reached the goal. Do not infer global shortest length
from minimum-length searches for individual subgoals.

## Quick static checks

After staging and loading the profile, test on a private hypothetical gate state:
```lisp
(defparameter *check-state*
  (sightline-state-with-open-gates (census-type-instances 'gate) '(gate2 gate3)))
(jammer-target-visible-from-placement *check-state* 'jammer1 'location5 'plate2 'jammer1 'gate1)
(reachable *check-state* 'location1 'location7)
```
These are claustro-topo syntax examples; substitute the current problem's objects. Only the listed gates are forced open; other start-state facts remain. This
bypasses propagation: gate bits are premises, not controller-consistent states. REACHABLE takes target then reacher and checks arm reach, not a walking route.
Jammer visibility alone proves neither legal placement nor availability; also
check `jam-disallowed>` and the action's placement/reach conditions.

MOVE takes a list of route transitions. Formats illustrated by claustro-topo:
```lisp
(move agent1 ((walk location1 (gate1 gate3) location4)))
(move agent1 ((ladder location7 (ladder1) location1)))
(move agent1 ((jump (location10 ground) nil (location10 box2))))
(move agent1 ((jump (location10 box2) (edge1) (location12 ground))))
(move agent1 ((stairs location13 (staircase1) location11)))
```
The witness is the clause of the pair's traversal fact that the move uses, staircases
and edges included; NIL means an empty clause. Plain MOVE is accepted. Actions
with multiple outcomes need their printed phrase to select the outcome, e.g.
`(jam-target > agent1 jams gate5 with jammer1 at location8 on ground)`.
Validation accepts mixed plain and timestamped action lists.

### Constructing hand-written action sequences

Before handing D a replay, A reads each action's current effect template in
`tech/` and fills every non-string slot, including bound locations and selected
placement outcomes. Replay arguments follow `action.effect-variables`, **not**
the `define-action` parameter list. Prefer the exact printed phrase with its
`>` and connective words. A pickup, for example, needs the agent's location:
`(pickup-jammer > agent1 picks up jammer1 at location1)`, not
`(pickup-jammer agent1 jammer1)`. Plain MOVE remains supported.

Every prepared replay script must first preflight **all** its forms against
the staged `*actions*`: find the action by `action.name`, parse its arguments
with `strip-display-connectives`, and require the resulting count to equal
`(length (action.effect-variables action))`. Unknown actions, malformed phrases
and missing values must stop before any replay. Do not use
`format-action-for-display` as a syntax check: it preserves malformed forms
instead of rejecting them. Preflight checks format only; replay still checks
applicability, outcomes and the requested endpoint.

On failure, record the first failure, correct the source sequence and preflight
every form again before retrying from the stated start. A malformed action is
not a planning refutation. Added after phobia-topo SG1 failed at action 2
because a pickup omitted its location.

For MOVE, do not reverse an outward route by copying its barrier lists.
Directed return segments may carry different lists, including NIL. When a
segment is uncertain or rejected, query `mobility-provider-segments` with the
replay's preserved pre-failure state, agent and segment source; use the exact
returned segment. Region-quotient rows alone do not specify every location arc.

## Optional tools, on D's request

Load diagnostics with `(load (merge-pathnames <file> (asdf:system-source-directory :wouldwork)))`.
Their contracts are in `doc/constraint-led-solving/Extractor-Specifications.md`.

- **Probe battery:** load `tech/constraint-probe-battery.lisp` after the profile.
  `(report-probe-battery-list)` lists without searching. At threads 16,
  `(run-probe-battery <max-depth> <results-path> [probes])`
  runs with depth as its only bound. Optional probes are
  `(d-probes '((<subject> <goal> "<provenance>") ...))`, alone or appended to
  `(probe-battery-probes)`. `(report-probe-battery <results-path>)` reports results.
- **From here (FH):** `(report-from-here <checkpoint>)` reports the state and
  one-step opportunities. For all applicable actions load
  `tech/constraint-state-probe.lisp` and use `(report-applicable-actions <action-prefix>)` after fresh staging.
- **Role obligations (RO) / cycle-plan check:** `(report-role-obligations <scenario>)`
  and `(report-cycle-plan-check <plan>)` use explicit data (specifications 4/13).
  PASS is not a plan witness; CONDITIONAL names a premise to settle with D.
  A stage's `:reservations` state each body's jobs over a phase range, e.g.
  `(:body <b> :role (:jam <target>) :from "<id>" :through "<id>" :purpose "<text>")`;
  roles are `:weight`, `:jam`, `:place`, `:hold`, `:mount`, `:support` (13.8).
  B5 then reports shared and conflicting jobs, capacity, the eligible pool and
  releases; a shortage refutes only that allocation. Nothing is reserved unless stated.
- **Ledger:** removed 2026-10-02 (`tech/constraint-ledger.lisp` and its schema,
  `Status-Algebra-and-Record-Schema.md`; both in git history). The Briefing's subgoal
  log covers its role. If a problem's premises and retractions outgrow the log,
  record the need in `Schema-Gaps.txt` and the problem's Handoff; agree with D
  whether to restore and adapt the ledger.
- **Supplied relay views:** `(report-relay-view-scenario <scenario>)` tests explicit
  complete-state scenarios (specification 6.1). Optional scenarios also go to
  REPORT-RELAY-CHAIN-TABLE, REPORT-NECESSITY-HINTS and profile report/write calls.
  Never combine hops from different scenarios; CLEAR proves no stable/reachable setup.
- **Supplied crossings:** `(report-beam-crossing-scenario (list :state <state> :provenance "<text>"))`
  evaluates one settled state, such as a replayed prefix's
  `(action-sequence-validation-final-state (validate-action-sequence *start-state* <actions>))`,
  with the engine's queries: gates read, active crossings, and each live beam's crossings labelled ACTIVE,
  REACHED/INACTIVE, BEYOND CUT or BEYOND CLOSED (specification 8.10). An unsettled or
  incomplete input is UNRESOLVED. It proves no reachability or composition.
- **Supplied relay lighting:** `(report-relay-lighting-scenario (list :state <state> :provenance "<text>"))`,
  optionally `:view :recording` (open cycle only) and `:chains <chains> :phase <phase>` for T33's
  verdicts, replays one settled state's lighting by propagation layer: each relay LIT, CONFLICT,
  LOCATION ALREADY LIT, UNREACHED or ABSENT; each stored link's sightline, cut and outcome
  (DELIVERED, IGNORED later, SOURCE DARK, NOT CLEAR); outgoing/incoming capacity; receivers,
  with engine agreement checked (specification 8.11). For that state only.
- **Supplied equipment:** `(report-equipment-scenario (list :state <state> :provenance "<text>"))`,
  optionally `:before <state> :before-provenance "<text>"`, reads one settled state's fans (MOUNTED,
  HELD, RESTING; STEPPABLE), mounts (EFFECTIVE STREAM, TURNING NO FAN, FAN MOUNTED STOPPED, VACANT
  STOPPED), boarding, mounting and removal verdicts with their failing conditions, and lifted
  occupants with what would drop them; with `:before`, what changed between the two states.
  Engine agreement is checked (specification 8.12). For that state or pair only.
- **Supplied service transition:** `(report-service-transition (list :before <state> :before-provenance "<text>"
  :state <state> :provenance "<text>"))`, optionally `:agent`, `:transit`/`:return` (locations) and
  `:final` (propositions), compares two settled states: each passage service KEPT, KEPT BY
  ALTERNATIVE (OVERRIDE when only jams remain), LOST or GAINED; supplies withdrawn or added and
  the devices they drive; arcs, mobility and retrieval lost or gained; requirements MET or NOT MET
  (specification 8.13). It says nothing about which action caused a change or about ordering.
- **Supplied stability:** load `tech/constraint-arrangement.lisp`; call
  `(report-relay-arrangement <scenario>)` or `(check-relay-arrangement <scenario>)`.
  Specification 6.2 requires a complete configuration, phase, provenance, view,
  receiver and chain, with primitive controls and no forced gate premises.
  Private-copy propagation and a second-pass fixed point distinguish working,
  failed, invalid/inconsistent and unresolved arrangements. Preserve plate-edge
  and recorder memory. Stability proves neither reachability nor survival through
  later moves, tray release or recorder boundaries; those require separate checks.
- **Supplied boundary or support change (BT):** load `tech/constraint-arrangement.lisp` and
  `tech/constraint-boundary.lisp`; call `(report-boundary-transition (list :state <state>
  :provenance "<text>" :event <event>))`, where the event is `(:stop <ghost agent>)`,
  `(:cancel <live agent>)` or `(:action <action form>)` for one action that changes a support
  (ON, a held tray, a fan mount) or the recorder session. Optional `:agents` and `:obligations`,
  each `(:fact <prop>)`, `(:body <b> :role <T44 role>)` or `(:reach (<agent> <location>))` with
  `:phase :until-event`, `:across` or `:after` and a `:purpose`. Prerequisites are itemized
  apart from effects; a STOP or CANCEL whose prerequisites fail is still closed by the engine's
  own closure as HYPOTHETICAL. Reports supports and chains (RETAINED, CHANGED, REMOVED), plates
  by occupant layer, pairings and receivers in both views, devices, each agent's route
  conditions in its own view, and obligations EXPENDED, SURVIVES, LOST, MET or NOT MET
  (specification 6.3). For that state and event only; not reachability or a plan.

## Rules, grades and engine facts

C1: derive extractors from instance/tech semantics, not hand-analysis answers.
C3: no problem object names in diagnostic code; use caller data and declared,
justified substrate interfaces. M2: never hand-edit generated output. M3: when
dialogue outweighs records, update files and the Handoff last before a fresh
session. M4: completion needs concrete links that compose under validation.
M5: investigate surprises and record general schema gaps. Prediction/scoring
rules C2, C4, M1, M6–M9 survive only as history in the frozen experiment.

Grades: **definitional** (definitions/static facts), **inductive** (initial case
and preservation by every applicable action), **cost bound** (cutoff and start),
**trace/ordering** (separate proof obligation). No inductive proof rests on a
cutoff. Label graph candidates, missing analysis and conditional bounds explicitly.

A grounded agent cannot place onto a held tray (top 3/2, placement reach 1);
use a raised location. CONNECT-CONNECTOR sets the connector down, never pairs
while held; pairing uses structural sightlines. Generated termini are reversed
(e.g. repeater then receiver): hand-written termini must follow reverse declaration
order (connectors, receivers, transmitters, each descending), since the validator
compares them strictly. HOLDING is bijective, printed HOLDING1/HOLDING2.
Connecting is refused at a location holding a lit connector. PICKUP-CONNECTOR clears
that connector's pairings and beams: the only undo at the agent's own location.
Beam crossings: two live beams that cross cut each other; a beam cut nearer its
source never reaches later crossings, so an earlier sacrificial crossing shields
them. A crossing on the source side of a closed gate still acts. A transmitter
pairing is live for cutting even while its connector is unlit. Relay lighting
is breadth-first: a relay keeps the hue that reaches it first and ignores later
ones (two hues in the same step leave it unlit), so one relay can pass a relayed
hue while its direct feed is cut and its own hue once the feed is restored.
Pairings persist while their beam is blocked or cut; only pickup clears them (the
retaining pickup keeps them, but a held connector carries no beam). Capacity counts
a connector's outgoing pairings only. A connector at a location whose connector is
already lit stays dark. A receiver needs a relay of its hue whose own pairing names it.
The Filesystem extension worked on 2026-09-27; write dollar-bearing files whole
or with literal quoting, avoiding replacement-string substitution. Hash with
`certutil -hashfile <file> SHA256`. Escape leading dollars in prose.

## Records and templates

`doc/constraint-led-solving/problems/<problem>/` is flat (no subdirectories) and holds only the files
below, under exactly these names; only the diagram's name and type vary.
Superseded material is deleted; git history is its archive. State lives in the
Handoff; the Briefing's log retains provenance and results. The method-level
`evidence/` folder (dated check scripts and run records, citing the pre-2026-09-30
layout) was removed 2026-10-02; it is in git history, and its check scripts run
only at their own commit.

- `Handoff.md` — current state (template below).
- `Briefing.md` — analysis and subgoal log (template below).
- `Constraint-Static-Profile.txt` — generated; never hand-edited (M2).
- `Actions.lisp` — every accepted action from the original start, once a subgoal
  is accepted (template below). A candidate is appended after a
  `;; SG<n> candidate` comment and replayed from the start; on acceptance the
  comment becomes `;; SG<n>`, on rejection the candidate is corrected or removed.
- `Checkpoint.txt` — the latest exported search checkpoint, only while a
  continuation needs one.
- `Validation.txt` — written by `Actions.lisp` when the goal is validated: the
  numbered `(validate-solution :verbose ...)` form for the complete solution,
  which D can evaluate after staging, followed by the validated final state.
- The diagram, if supplied.

Briefing.md: profile path/hash and maximum depth, then **Spec-diagram check**, **Summary**, **Difficulties**,
**Contracts** (including UNCOVERED mechanics), **Hints** (qualified, with sources),
**Subgoal log** (`subgoal | whose idea | check | result`, followed per subgoal by its
commands, expected interpretation, D's reported result and the endpoint review),
and **Result** (full-path validation and any limits).

Actions.lisp template. Load it after `(stage <problem>)` in a separate top-level
form; a form that stages and loads together can see the previous problem's GOAL-FN.
```lisp
;;; <problem> -- accepted actions from the original start.
(in-package :ww)

(defparameter *accepted-actions*
  '(;; SG1 <endpoint in a few words>
    (<action> ...)
    ;; SG2 candidate <endpoint in a few words>
    (<action> ...)))

(loop for form in *accepted-actions*
      for index from 1
      for action = (find (first form) *actions* :key #'action.name)
      do (unless action
           (error "Action ~D is unknown: ~S" index form))
         (unless (= (length (strip-display-connectives action (rest form)))
                    (length (action.effect-variables action)))
           (error "Action ~D is malformed: ~S" index form)))

(defparameter *accepted-validation*
  (validate-action-sequence *start-state* *accepted-actions*
                            :goal-test (symbol-function 'goal-fn) :verbose t))

(format t "~%~D actions: success=~S goal-checked=~S goal-satisfied=~S failure-index=~S reason=~S~%"
        (length *accepted-actions*)
        (action-sequence-validation-success-p *accepted-validation*)
        (action-sequence-validation-goal-checked-p *accepted-validation*)
        (action-sequence-validation-goal-satisfied-p *accepted-validation*)
        (action-sequence-validation-failure-index *accepted-validation*)
        (action-sequence-validation-failure-reason *accepted-validation*))
(display-validation-state (action-sequence-validation-final-state *accepted-validation*))

(defparameter *accepted-validators-p*
  (and (action-sequence-validation-goal-satisfied-p *accepted-validation*)
       (report-solution-validator-verdicts
         *accepted-actions* (action-sequence-validation-final-state *accepted-validation*))))

(when (and (action-sequence-validation-success-p *accepted-validation*)
           (action-sequence-validation-goal-checked-p *accepted-validation*)
           (action-sequence-validation-goal-satisfied-p *accepted-validation*)
           *accepted-validators-p*)
  (with-open-file (*standard-output*
                   (merge-pathnames "doc/constraint-led-solving/problems/<problem>/Validation.txt"
                                    (asdf:system-source-directory :wouldwork))
                   :direction :output :if-exists :supersede)
    (format t ";;; <problem> -- complete validated solution, ~D actions.~%" (length *accepted-actions*))
    (format t ";;; SUCCESS-P T; GOAL-CHECKED-P T; GOAL-SATISFIED-P T; solution validators accepted~%")
    (format t ";;; To re-validate: (stage <problem>), then evaluate this form separately.~%")
    (format t "(validate-solution :verbose")
    (let ((*print-case* :downcase)
          (*print-pretty* nil))
      (loop for form in *accepted-actions*
            for index from 1
            do (format t "~%  ~S" (list index form))))
    (format t ")~2%Final state:~%")
    (display-validation-state (action-sequence-validation-final-state *accepted-validation*))))
```

Handoff.md template:
```text
# <problem> — Handoff
Updated <date>.  Status: <phase, stage>.
Problem spec: <file path>.
Corresponding diagram: <file path or "none">.
Maximum search depth: <D's cutoff>, set <date>.
## Next step       one line: who does what
## State           accepted actions in Actions.lisp; current endpoint in domain terms
## Checkpoints     file | phases/actions | SHA-256 | what it is
## Restore         the exact REPL forms
## Open items      at most a few lines
```
When there are no accepted actions or no checkpoint, state that explicitly.

## Directory and development history

The current root is `doc/constraint-led-solving/`. This file is the solving
entry point; `Extractor-Specifications.md` describes diagnostics and
`Schema-Gaps.txt` records uncovered needs. Problem records are in `problems/`,
and the triangle pilot is in `constraint-pilot/`.

Development is complete through T46. The implementation plans (the final copy
`archive/Implementation-Plan-2026-10-02.md`, with its board, maintenance
conventions and pointers to earlier task records) and crelay-topo's experiment
record were in `archive/`, removed 2026-10-02; they are in git history. There is
no active work queue.

Path migration (2026-10-02): historical paths in verbatim quotations and
dated schema-gap entries are preserved. To locate moved files,
map `doc/constraint-method/` to this root, `doc/problems/` to its `problems/`,
and `doc/constraint-pilot/` to its `constraint-pilot/`. The former
`Problem-Solving-Guide.md` is now `solving-advisor.md`; the former active
`Constraint-Implementation-Plan.md` became the final plan copy named above.
Earlier removals and the `archive/` and `evidence/` removals still apply. These mappings
do not restore deleted files or make historical scripts runnable. The former
`doc/README.md` is absent; historical mentions are retained.

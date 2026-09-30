# phobia-topo — Briefing

Updated 2026-09-27. Maximum search depth: **25**, chosen by D on this date.
Profile: [Constraint-Static-Profile.txt](Constraint-Static-Profile.txt).
SHA-256: `23A4FE3704EE651A32E65AA867A0E0813344A31FFAB784B1CE84C7FABA43304D`.
Profile read before proposing a subgoal. No search or replay has run.

Legacy files deleted 2026-09-30 (retained in git history): sg1-retrieve-c1.lisp,
sg2-retrieve-c2.lisp, sg3-activate-receiver2.lisp, sg2-checkpoint.txt, sg4-final-goal.lisp,
sg4-equipment-location13.lisp, sg1-endpoint.txt, sg2-endpoint.txt, sg3-search-result.txt.
References below are historical; restore as in Handoff.md (sg3-checkpoint.txt, sg4a, sg4b, sg5).

## Summary

The goal is agent1 at location11, elevation 10 above location10. Fan1 starts on wgears1 at location2 and is the only removable fan. The intended final lift requires bringing it to fgears1 at location10, mounting it, and stepping onto it.

Receiver1 opens gate1. Gate2 has no controller and needs the jammer to open. Receiver2 stops wblower2 but starts wblower3; a jammer can override either blower off. Wblower4 is uncontrolled and normally running. Wgears1's stream disappears when fan1 is removed. These claims use the spec, profile S1/MC, and the source contracts below.

There are two connectors, one jammer, one fan and one agent, with no boxes, trays, plates or recorder. One jammer can stop/open only one target at a time (S2). Each connector has at most two pairings (spec). A beam setup, a barrier override and fan transport must therefore be scheduled rather than assigned unlimited bodies.

## Difficulties

- Entering location3 to collect connector2 requires both gates open during transit. A connector at location12 can power receiver1 while the jammer opens gate2, but that requires obtaining a connector first (S3, RC).
- Crossing wblower2 using receiver2 switches on wblower3. Transit and return conditions must be checked separately; a beam useful for one crossing may obstruct the next (S1, CC).
- RC reports zero chains to receiver2 in its start-state scope. This is not structural impossibility: S6 names location1 as an occluder on location12-to-receiver2, and jammer1 starts there. Moving it is a candidate way to clear the line, requiring a fresh check before claiming a working beam.
- S3 groups location10 and location11 together but explicitly omits elevation checks. This does not provide a walking route to the loft; floor launch is a separate obligation.

## Contracts

### Floor-gears — hand contract for the sole UNCOVERED mechanic

Historical: superseded on 2026-09-28 by MC's floor-gears contract (T42).

Sources: `tech/floor-gears.lisp`, `tech/-gears-fan.lisp` (blower-present, update-blower-status!, pickup-fan, mount-fan), `tech/-floor-blowing.lisp` (update-floor-blowing-status!, blow-occupants-away!, drop-occupants!), and `tech/step.lisp` (steppable-fixture-at, step-configuration-transitions).

- **Controls:** fgears1 has no controller, so turns unless jammed. It produces an effective stream only with fan1 mounted. Turning gears alone do not lift anything.
- **Moves/lifts:** a non-fan occupant ON the mounted fan is detached and relocated with its stack to the drive's AIMED-AT destination, location11. An unsupported occupant there drops to location10 if no active floor drive sustains it. This is ordinary physical view; no recorder is present.
- **Prerequisites:** retrieve fan1 with empty hands, within horizontal and vertical reach. Mounting requires holding it, reachable vacant gears, manipulation permission and vertical reach. A floor mount supplies the fan's location. Boarding requires the agent on ground at that location, a clear fan top and permitted support use. Step propagation then launches the agent when the drive is active. A fan merely lying on ground is not steppable.
- **Resource consequence:** fan1 cannot stay mounted at wgears1 and fgears1 simultaneously. Removing it also removes wgears1's horizontal stream. Mounting on running gears is legal.

### Covered mechanics relevant to the opening

The profile's jammer contract requires held jammer, reachable legal placement, target visibility from its placed top and no directional exclusion. Its instance rows allow wblower2 to be jammed from location4 on ground, without gate premises; no JAM-DISALLOWED rows are present. The action's remaining applicability conditions still need realization/replay.

Wall-blower streams transport contacted bodies and their cargo and gate walking transitions while active. Fixed wblower2/3/4 cannot be carried. Receiver2 initially inactive means wblower2 on and wblower3 off. Jamming wblower2 makes both off without powering receiver2.

## Hints

- **CONSISTENT, geometric only:** transmitter1 -> connector at location12 -> receiver1 uses one connector and no open-gate premise (RC chain 1). Stable placement and transit are not yet validated.
- **CONSISTENT, route conditions only:** after jamming wblower2, the S3 crossing from location1/location4 through location5 to location6/location13 is compatible with receiver2 remaining inactive. The return also has a directed route to location1/location4. Ground elevations match (S5). This supports collecting connector1 before tackling connector2.
- **CONTRADICTED in the SG2 endpoint:** D's fresh BEAM-VISIBLE query from location12 height 1 to receiver2 height 1 returned NIL despite location1 being empty. Identify remaining barriers/occluders before proposing another arrangement. The initial kill list did not establish that location1 was the only obstruction.

## Subgoal log

| Subgoal | Whose idea | Check | Result |
|---|---|---|---|
| SG1: agent1 back at location4 holding connector1; jammer1 on ground at location4 jamming wblower2; receiver2 inactive | A; agreed by D | SATISFIED by successful replay and D's supplied endpoint | ACCEPTED, 7 actions; `sg1-endpoint.txt` (deleted); no search |
| SG2: agent1 at location12 holding connector2; connector1 at location12 powering receiver1; jammer1 at location12 jamming gate2; fan1 parked on ground at location4 | A; agreed by D | SATISFIED by successful replay and D's endpoint | ACCEPTED, 17 additional actions; `sg2-endpoint.txt` (deleted); 24 accumulated actions |
| SG3: active receiver2, no other endpoint conditions | A; agreed by D | SATISFIED by search endpoint; transmitter1 -> C2 at location2 -> C1 at location5 -> receiver2 | ACCEPTED REALIZED, independently validated NIL; 10 additional actions, 34 cumulative; `sg3-search-result.txt` (deleted) |

Opening rationale: retrieve connector1 from location6 before attempting the two gates around connector2. The jammer supplies the outward crossing and remains at location4 for recovery. Connector1 can subsequently supply receiver1 from location12. Later goals are not agreed commitments. Fan1 and connector2 have no SG1 endpoint obligation; review their actual states after realization.

Proposed SG1 predicate:

```lisp
(and (has-location agent1 location4)
     (holding agent1 connector1)
     (has-location jammer1 location4)
     (not (on jammer1 fan1))
     (jamming jammer1 wblower2)
     (not (active receiver2)))
```

## Result

Current accepted endpoint is *phobia-sg3-candidate*: agent1 and C1 at location5,
C2 and jammer at location2, fan at location4. Receiver2 active; jammer still
stops wblower2; wblower3 and wblower4 blow. The two-gate task is finished and
both gates are closed. SG3 is search-realized, not independently replayed.
Proposed next subgoal: original final goal, agent1 at location11, using one
MIN-LENGTH search from SG3 at depth 25 and 16 threads. Await D's agreement.
This leaves transport and receiver2 switching to realization instead of
imposing an untested equipment arrangement. Complete-path validation remains
required if the search succeeds.

SG1 and SG2 accepted: 24 accumulated actions replayed successfully and D's endpoints satisfy their agreed predicates. Current state is the final-state of *phobia-sg2-validation*, not installed into planner globals. No archived search checkpoint or final-goal validation. Search settings, if needed, remain MIN-LENGTH at depth 25 with 16 threads. Before proposing SG3, check the location12-to-receiver2 beam in this endpoint now that location1 is clear.

SG2 rationale and prerequisites: location12 is reached across wgears1's band.
Free the jammer by putting C1 down at location4, then use the jammer to stop
wgears1 while removing fan1 and parking it at location4. Removal makes that
stream stay off when the jammer is recovered. C1 can then supply receiver1
from location12, opening gate1 so the jammer there can see and open gate2
(MC requires gate1 open for that jam). Both gates must remain open while the
agent goes to location3 and returns holding C2. C1 stays paired to transmitter1
and receiver1; the jammer stays on gate2. This is a static-consistent proposal,
not an action trace or proof of realizability. Exact directed routes, placement,
pickup reach and beam stability must be checked during realization.

## SG4 authorization update
D exported sg3-checkpoint.txt (34 actions); verified SHA-256:
4AD1C64B6212DAA7090166867823DEEC8ACE3952647D3EE3BC9AD9C6432A1B7A.
D approved one final-goal search from *phobia-sg3-candidate* for
(has-location agent1 location11): MIN-LENGTH, depth 25, 16 threads.
Next step: D loads constraint-evidence/sg4-final-goal.lisp in the current
session. Result stays separate as *phobia-final-candidate*. No runtime result
yet. Full-path goal validation is required after success, before closure.
This supersedes earlier export-pending and approval-pending notes.

## Revised SG4 approval
D replaced the slow final-goal search with an agreed equipment subgoal:
agent1, fan1 and jammer1 at location13, jammer stopping wblower2, receiver2
active, C1 at location5 and C2 at location2 supplying it. MC permits the jam
from location13; transfer remains to be realized. One MIN-LENGTH depth-25
search, 16 threads, from restored SG3. No final-search exhaustion/result was
reported. Stop the previous search before loading sg4-equipment-location13.lisp.
This supersedes the earlier final-goal-search next step; SG3 remains accepted.

## SG4 fatal heap exhaustion
D supplied: [Worker 8] New best bound: 25, followed by fatal SBCL heap
exhaustion during GC and entry to ldb>. Dynamic space 25,165,824,000 bytes
(24,000 MiB); allocated 25,092,660,400 bytes (99.7%). No accepted SG4 checkpoint.
Current source register-parallel-solution builds and pushes the solution onto
*solution-paths* before printing the improved bound. Thus the worker registered
a 25-action candidate in memory; no printed path or saved archive is available.
Minimum length and independent replay were not established. Fatal termination
is not exhaustion of the search space and establishes no depth bound.
Only SG2/SG3 checkpoint archives are present. SG3 remains the restart point.
Do not attempt normal REPL commands in LDB or automatically rerun the search.
Proposed next step: restart SBCL and restore SG3, then agree a smaller milestone
(fan1 placed at location13 with agent1 there, leaving the jammer at location2).
Prefer hand-derived replay for that transfer. If another search is needed,
discuss a lower cutoff with D before running it; maximum 25 remains unchanged.

## SG4a — approved hand-derived fan transfer
D approved carrying fan1 to location13 first, leaving jammer1 at location2
stopping wblower2. Prepared constraint-evidence/sg4a-fan-transfer.lisp:
restore the saved SG3 checkpoint, then four actions (walk to location4,
pick up fan, walk via location5 to location13, put fan on ground). The return
location5-to-location4 NIL segment and location5-to-location13 WBLOWER2
segment were supplied by D's earlier mobility query; location4-to-location5
WBLOWER2 replayed in SG1. Jammer remains fixed throughout; transit and beam
propagation still need replay. Templates checked in -gears-fan.lisp.
The script checks all formats first and explicitly tests the endpoint,
including receiver2 active and the jammer/connector locations, then prints
its final state. No run result yet, no search or automatic fallback.
D explicitly approved FIRST as a fallback at depth 25, 16 threads, overriding
this problem's MIN-LENGTH default. Prefer this replay first; inspect any failure
before invoking the fallback. This is not authorization to retry the failed
large equipment search automatically.
Next step: after restarting SBCL and loading Wouldwork/package WW, D loads
constraint-evidence/sg4a-fan-transfer.lisp and supplies result plus endpoint.

## SG4a accepted — 38 accumulated actions
D reports success=T, goal-checked=T, goal-satisfied=T, failure-index=NIL,
reason=NIL. Time 38.0, value 0.0. Agent1 and fan1 at location13; C1 at
location5, C2 and jammer1 at location2. Jammer still stops wblower2;
receiver2 active and both connectors red. Wblower3/wblower4 blow. C2 retains
receiver1/transmitter1 pairings; C1 retains C2/receiver2 pairings. This meets
all SG4a endpoint checks. Current accepted state is the final-state of
*phobia-sg4a-validation*; it is not installed in planner globals or archived
as a new search checkpoint. SG3 remains the saved restart checkpoint; loading
sg4a-fan-transfer.lisp restores SG3 and reconstructs this accepted endpoint.
No FIRST fallback was needed. Four new actions; 38 accumulated.

Proposed next subgoal SG4b: bring jammer1 to location13 and jam wblower2
from there, retaining fan1 and agent1 at location13, receiver2 active and
C1/C2 at location5/location2. MC permits the location13 jam. While the jammer
is carried, receiver2 must keep wblower2 stopped; transit occlusion could
interrupt that beam and sweep C1 from location5, so endpoint sightlines alone
do not establish this transfer. NEEDS a checked route and replay; awaiting
D's agreement. Keep the accepted SG4a state intact while investigating.

## SG4b approved — jammer transfer replay ready
D agreed SG4b. Prepared constraint-evidence/sg4b-jammer-transfer.lisp, four
hand-derived actions from the accepted final-state of *phobia-sg4a-validation*.
No restaging, search or automatic fallback. SG4a stays unchanged.
Sequence: one MOVE to location2, pickup jammer, one MOVE back to location13,
jam wblower2 on ground. Each MOVE contains explicit walking transitions.
Current -mobility-action.lisp checks transparent route segments against the
source state, then relocates the agent and propagates at the MOVE endpoint;
it does not propagate at each listed intermediate location. Thus intermediate
waypoints are not separate stopping states. The critical propagated states
are arrival/pickup at location2 and arrival/jam at location13. The accepted
SG3 already has C2 and jammer together at location2 with receiver2 active;
that does not by itself prove the agent's visit safe, which replay must check.
Pickup/jam templates are complete; all formats are preflighted before replay.
The reverse location13-to-location5 segment is supported by S3's symmetric
WBLOWER2 crossing, but exact runtime applicability still needs confirmation.
Final check requires agent/fan/jammer at location13, jammer stopping wblower2,
receiver2 active and both connector placements/supply pairings preserved.
Next step: D loads sg4b-jammer-transfer.lisp in the current WW session and
supplies its result and endpoint. No runtime result yet.

## SG4b accepted — 42 accumulated actions
D reports success=T, goal-checked=T, goal-satisfied=T, failure-index=NIL,
reason=NIL. Time 42.0, value 0.0. Agent1, fan1 and jammer1 at location13;
jammer1 jams wblower2. C1 at location5, C2 at location2; receiver2 active,
both connectors red, supply pairings preserved. Wblower3 and wblower4 blow.
Accepted state: final-state of *phobia-sg4b-validation*. Four new replayed
actions after SG4a, 42 accumulated. Script SHA-256:
08CCBCEC598CFBBED01C68A08A4B6058E5900171D8BD2F41FD104670D1DD33B7.
No new archive yet; SG3 plus the two replay scripts reconstruct this endpoint.

Proposed next milestone: original final goal at location11, hand-derived.
A's outline: while jammer still stops wblower2, pick up C1 at location5 to
extinguish receiver2 and stop wblower3; bring C1 back to location13 and put
it down unpaired. Then recover jammer at location13 and carry it via location6
to location8 to jam wblower4. Finally bring fan from location13 to fgears1
at location10, mount and board it. C2 stays at location2; its leftover
receiver1 pairing does not power receiver2. Static checks: S1 supplies the
receiver2-off/wblower3-off implication; MC allows wblower4 jam from location8;
S3 supplies the crossing candidates; floor-gears hand contract supplies final
lift. Exact walking witnesses and each propagated transit state NEED replay.
No realization of this outline authorized yet; awaiting D's agreement.

## SG5 approved — final replay prepared, 2026-09-28
D approved the hand-derived finish. constraint-evidence/sg5-final-replay.lisp
contains 12 candidate actions from SG4b, followed on success by independent
full-path replay from the original SG3 archive origin (54 accumulated actions).
Goal checks use the archive session's original goal function. All action formats
are checked before replay. Exact route witnesses and propagated endpoints still
need runtime verification. No search/restaging. Preserve the SG4b result.
If all full-path checks pass, the script saves complete-validated-path.txt;
otherwise no final solution is claimed. Next: D loads the script and supplies
SG5 summary, full-path summary if reached, and endpoint. Closure pending.

## SG5 first replay and route correction, 2026-09-28
D reports failure at action 8, (:STATE-MISMATCH), after seven successful
actions. Time 49: agent/jammer at location8, jammer stops wblower4; C1/fan
at location13, C2 at location2; receiver2 inactive, wblower3 off, wblower2 on.
D queried mobility-provider-segments in the preserved pre-failure state:
(WALK LOCATION8 NIL LOCATION6) and (WALK LOCATION6 NIL LOCATION13) exist.
The candidate incorrectly labelled location8-to-location6 with WBLOWER3.
Corrected action 8's first segment to NIL, preserving every other action.
The query also confirms location6-to-location8 retains WBLOWER3: return
witnesses are directional. Corrected full replay pending; no closure claim.
Next: reload sg5-final-replay.lisp without restaging. It starts from accepted
SG4b, preserving that endpoint, and runs complete original-start replay only
if the corrected final segment reaches the goal.

## CLOSED — full-path validation, 2026-09-28
D reports FULL PATH (54 actions): success=T; goal-checked=T;
goal-satisfied=T; failure-index=NIL; reason=NIL. A inspected the saved file:
original goal (HAS-LOCATION AGENT1 LOCATION11), 54 actions, all three flags T.
Final path: constraint-evidence/complete-validated-path.txt (relative to the
problem directory); SHA-256 EFD383E82ABDF792B46E465BE55948B5E9773DB53C4A02DC8039D0B6B5320E13.
All accepted segments compose from the original start. Stage lengths
7 + 17 + 10 + 4 + 4 + 12 = 54. No global shortest-path claim. FIRST fallback
was authorized but not used for the completed hand-derived finish. The earlier
OOM search is not a negative bound. No authored objects/geometry changed.
This closure supersedes all earlier pending-run and proposed-next-step notes.

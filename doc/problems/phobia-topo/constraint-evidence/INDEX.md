# phobia-topo evidence

Legacy files deleted 2026-09-30 (retained in git history): sg1-retrieve-c1.lisp,
sg2-retrieve-c2.lisp, sg3-activate-receiver2.lisp, sg2-checkpoint.txt, sg4-final-goal.lisp,
sg4-equipment-location13.lisp, sg1-endpoint.txt, sg2-endpoint.txt, sg3-search-result.txt.
References below are historical; restore as in Handoff.md (sg3-checkpoint.txt, sg4a, sg4b, sg5).

## SG1 — retrieve connector1

D agreed to retrieving connector1 first. Maximum search depth remains 25.
`sg1-retrieve-c1.lisp` (deleted) is A's seven-action hand-derived
candidate, not a validated path. D runs the replay in the existing WW REPL.
No search is needed for this attempt.

The explicit outward and return transitions use location5 to match the profile's
S3 wblower2-labelled adjacency rows. Jammer1 is placed on ground at location4,
whose MC visibility row permits wblower2 without gate premises. Receiver2 is not
activated, leaving wblower3 off. The candidate picks up each object at its own
location, avoiding assumptions about remote arm reach.

Expected endpoint: agent1 at location4 holding connector1; jammer1 on ground
at location4 jamming wblower2; receiver2 inactive. Fan1 remains on wgears1 and
connector2 remains at location3. Replay must establish executability, and the
printed endpoint must be reviewed against these expectations. No minimum-length
claim. No checkpoint accepted or exported yet.

First replay reported by D: success NIL, failure-index 2,
`Malformed action phrase: expected ">", got AGENT1.` Action 1 succeeded;
this was a syntax failure, not a route refutation. Both pickups incorrectly
used action parameters rather than effect-template values. Corrected both to
complete phrases including location1/location6. Added staged format preflight
of every action before replay. Corrected replay remains pending.

Second replay reported by D: success NIL, failure-index 7, reason
`(:STATE-MISMATCH)`. Actions 1–6 replayed successfully, including pickup of
connector1 at location6. The return MOVE has not been validated. Its proposed
route reverses the outward route, but stream-related directed segments need
not have the same witness in reverse. Source `replay-grounded-movement-result`
requires exact membership in `mobility-provider-segments` at each segment's
source. S3 also lists a direct unguarded return toward location4.

Next check: inspect movement segments from location6 and location5 in
`(action-sequence-validation-final-state *phobia-sg1-validation*)`, the preserved
state after action 6. This is a read-only query, not a search or a new replay.
Do not infer the exact replacement from the quotient table alone.

D's focused query at the state after action 6 returned
`(WALK LOCATION6 NIL LOCATION4)` and `(WALK LOCATION5 NIL LOCATION4)`.
The rejected final segment had incorrectly required WBLOWER2 for the latter.
Action 7 is now the directly supplied one-segment return from location6 to
location4. Actions 1–6 remain unchanged. Corrected full replay pending;
SG1 is not yet accepted.

Third replay reported by D: `success=T; failure-index=NIL; reason=NIL`.
All seven actions are now replay-validated for executability. The script did
not supply a goal-test, so the agreed SG1 endpoint still needs review of the
stored final state before acceptance. No final loft-goal claim or shortest-path
claim is made.

D then supplied the full endpoint: all agreed SG1 conditions hold. SG1 is
ACCEPTED. See `sg1-endpoint.txt` (deleted) for the reported state and
replay-source hash. The endpoint remains in the validation result, not in an
installed or exported search checkpoint.

## SG2 — retrieve connector2

D agreed the proposed SG2. `sg2-retrieve-c2.lisp` (deleted)
contains 17 hand-derived actions starting from the accepted SG1 validation
endpoint. It does not restage, search or overwrite the SG1 result. It preflights
all action formats, replays, and prints both the result and final state.

Templates checked against beam-relay.lisp, jammer.lisp and -gears-fan.lisp.
Pickup-fan includes both fan and agent locations; connection termini are in
reverse declaration order (receiver1 transmitter1). All manipulation occurs
at the object's location, consistent with -reachability's identity default.
The location2 return uses the directed stream-destination arc to location1
with NIL; other wgears1 crossings explicitly retain their barrier witness.
These exact location arcs still await runtime confirmation; S3 is only a
region quotient. Fan removal keeps wgears1 passable after the jammer leaves.
Receiver1 must remain active through both crossings to/from location3.

Expected endpoint: agent1 at location12 holding C2; C1 on ground at location12
paired to receiver1/transmitter1 and powering receiver1; jammer1 on ground at
location12 jamming gate2; fan1 on ground at location4. Gates1/2 open. No runtime
result or minimum-length claim. Combined accepted prefix plus candidate is
24 actions; the search-depth maximum remains 25 but no search is used here.

D reports SG2 replay success=T with failure-index/reason NIL and supplied the
full endpoint. All agreed conditions hold: SG2 ACCEPTED. See
`sg2-endpoint.txt` (deleted). Accepted state is the final-state of
*phobia-sg2-validation*. Next read-only check: BEAM-VISIBLE from location12
at height 1 to receiver2 at height 1 in that exact state, since the original
profile's location1 occluder has moved. A clear sightline alone will not prove
the proposed receiver2 arrangement stable or its crossings realizable.

D's SG2 endpoint query `(beam-visible state 'location12 1 'receiver2 1)`
returned NIL. The direct beam is blocked in this state despite location1 now
being empty. The original kill list names one obstruction, not necessarily
all obstructions. Next use `relay-view-hop-blockers` on the same state and
physical selector NIL to identify remaining barriers/occluders. SG3 remains
unagreed; no beam arrangement or crossing has been attempted.

D's blocker diagnostic returned boundary segments 7, 8, 21 and 22. The direct
location12-to-receiver2 beam at height 1 is structurally blocked in the given
geometry, not repaired by moving the original location1 occluder.

## SG3 — activate receiver2

D approved one search for `(active receiver2)` from accepted SG2. Script:
`sg3-activate-receiver2.lisp` (deleted). MIN-LENGTH, 16 threads,
depth cutoff 25, no other search bound, no deepening or automatic retry.
The script packages SG1+SG2's accepted 24 actions as one checkpoint phase
and exports sg2-checkpoint.txt before searching. This technical packaging keeps
the original loft goal and cumulative history; the two milestone records above
remain authoritative. Run only in the current staged session containing both
successful validation results, before any other search or restaging.

Candidate kept separately. Exhaustion returns SG2 unchanged and establishes
only this depth bound. A found result is REALIZED, independently validated NIL,
pending endpoint review; do not export it before review. Archive hash is pending
file creation by D's run. No search result yet.

SG3 result: D supplied `sg3-search-result.txt` (deleted).
MIN-LENGTH search found 10 additional actions in 6.617 seconds; checkpoint 2,
cumulative depth 34, new checkpoint T. Endpoint satisfies ACTIVE RECEIVER2:
agent/C1 at location5, C2 and jammer at location2, fan at location4; jammer
still stops wblower2. C2 pairs transmitter1 and receiver1; C1 pairs C2 and
receiver2. Both connectors red; receiver1 inactive, gates closed; wblower3
and wblower4 blow. Accepted as REALIZED, independently validated NIL.
No global shortest-path claim. SG3 checkpoint export pending D's REPL command.

The successful beam chain is transmitter1 -> C2 at location2 -> C1 at
location5 -> receiver2. This revises the static zero-chain hint: its start-state
scope did not establish impossibility. While transporting fan/jammer onward,
preserve this chain or deliberately replace its wblower2-stopping role; C1 is
at a swept location if wblower2 restarts. Wblower3 now blocks further progress.

Run after staging phobia-topo and setting threads to 16:

```lisp
(load (merge-pathnames
       "doc/problems/phobia-topo/constraint-evidence/sg1-retrieve-c1.lisp"
       (asdf:system-source-directory :wouldwork)))
```

## SG4 authorization update
D exported sg3-checkpoint.txt (34 actions); verified SHA-256:
4AD1C64B6212DAA7090166867823DEEC8ACE3952647D3EE3BC9AD9C6432A1B7A.
D approved one final-goal search from *phobia-sg3-candidate* for
(has-location agent1 location11): MIN-LENGTH, depth 25, 16 threads.
Next step: D loads constraint-evidence/sg4-final-goal.lisp in the current
session. Result stays separate as *phobia-final-candidate*. No runtime result
yet. Full-path goal validation is required after success, before closure.
This supersedes earlier export-pending and approval-pending notes.

## SG4 revised — equipment at location13

D reported the final-goal search was taking too long and approved a smaller
subgoal. No completed result or exhaustion was supplied; interruption completion
is not yet confirmed. Stop that search and its workers before the next load.

Approved endpoint: agent1, fan1 and jammer1 at location13, jammer1 stopping
wblower2, receiver2 active, C1 at location5 and C2 at location2 retaining the
transmitter1 -> C2 -> C1 -> receiver2 chain. The extra pairing conjuncts encode
the approved beam roles, not a new strategic constraint. The fan's location
requires it placed rather than held. The MC jammer table permits the
location13-to-wblower2 jam; receiver2 activation stops wblower2 during cargo
transfer. Transit still requires realization, especially while moving jammer1.

Script: sg4-equipment-location13.lisp. After the previous search has stopped,
restage, set 16 threads, import the hashed SG3 archive, then run exactly one
MIN-LENGTH search at depth 25. Import replays the accepted prefix without
search and avoids using planner globals left by an interrupted solve.
Retain *phobia-sg3-restored*; candidate separate. No run result yet. No retry or
deeper search authorized. Search-found success remains independently validated
NIL until separate validation; export only after endpoint review.

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

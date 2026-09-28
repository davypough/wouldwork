# windtunnel-topo — Static profile review

Reviewed 2026-09-27 against the problem spec, supplied sketch, and source.
Profile SHA-256: `8D0C7957C753B2FB9588CD2CD43F5FE214B9C47E182AEBD3B24F853071A06038`.
No search or replay performed; generated output unchanged.
D clarified that A6 means location6 and should not be crossed out.

T31 follow-up, 2026-09-27: findings 2 and 3 below describe the retained
pre-fix profile. T31 is now complete: regenerated S3 retains five rows,
S4 verifies reachability, and NH reports failure reasons when unavailable.
H2/H4 remain empty for this instance under successful analysis. Findings
1, 4 and 5 were addressed within approved T32's reporting scope later that
day: wall-blower is covered, horizontal transport is distinct, RC/NH flag
the live station conflict and explicitly withhold recording-view/stability
claims. General stability analysis is still unresolved. Findings below are
the original pre-fix review. Evidence and current profile hash are in the
Handoff and method evidence `t31-reachability-2026-09-27.md` and
`t32-wall-blower-2026-09-27.md`.

T33 follow-up, 2026-09-27: explicit supplied-state physical/recording
sightline and relay-chain checks are complete (61 focused checks). Evidence:
`doc/constraint-method/evidence/t33-view-sightlines-2026-09-27.md`. The mixed
fixture works physically with its ghost connector while its recording beam
lacks the live connector. Stability remains T34; original findings below
remain a historical review, not the current implementation status.

T34 follow-up, 2026-09-27: supplied-arrangement stability checking is complete
(155 focused assertions). The mixed-view arrangement remains intact and lit;
the role-swapped live-ground arrangement is swept away. Evidence:
`doc/constraint-method/evidence/t34-arrangement-stability-2026-09-27.md`.
The profile's unassigned candidates still have no automatic stability claim.

## Findings

1. **MC: wall-blower is UNCOVERED.** This matters here: the fan both bars
   walking and transports occupants. A beam station at location3 is not
   automatically sustainable. The hand contract below supplies the missing
   semantics for discussion; general diagnostic support remains proposed.
2. **S3/S4: the reduced adjacency spine loses reachability.** S3's full rows
   allow R1 to reach R2 when blower1 is clear, but its three retained spine
   rows have no outgoing edge from R1. S4 correctly reports
   `(:REACHABILITY-MISMATCH "R1" NIL)` and withholds directional and goal
   conclusions. Source: `quotient-row-composition` tests each edge against
   all other edges independently; mutually redundant alternatives can all
   be removed. `keeper-spine` detects the resulting loss. This is an
   extractor reduction defect, not evidence that the puzzle is unreachable.
3. **NH hides downstream unavailability.** `hint-route-context` sets its
   spine to NIL after S4 fails; H2 and H4 consequently print no hints.
   Their "none" cannot be read as a finding that no relevant constraint
   exists. They need an explicit unavailable reason.
4. **RC is geometric, not a stable beam witness.** Its one candidate is
   transmitter1 -> location1 -> repeater1 -> location3 -> receiver1,
   using two connectors and requiring gate1 open. In the physical view,
   gate1 open also means blower1 turning. A live ground connector at
   location3 is then swept to location6. RC deliberately forces gate bits
   without propagation and checks only physical sightlines. It cannot
   settle which recording copy can remain at location3, or the recording
   view's beam visibility. NH H3's necessity claim must retain that scope;
   it is not a proof that every possible recorder-view beam was enumerated.
   A difference between physical and recording fan state is a candidate
   mechanism to investigate, not an established solution.
5. **CC calls horizontal transport a lift.** `coupling-role-table` assigns
   the lift role to every object with AIMED-AT. Here blower1 transports
   horizontally from location3 to location6. The controller coupling is
   real, but "lift" must not imply elevation gain or a floor-blower landing.

## Correct readings and limits

- The spec's control equations match S1: plate1 toggles gate1 and blower1
  together; receiver1 controls gate2. These are same-view equations, not
  equalities between physical and recording state.
- Plate1 is a toggle plate. It does not need continuous weight after a
  clear-to-depressed transition. T6's absence of pressure-body costs is
  therefore reasonable; it does not remove beam or recorder resource needs.
- A traversal clause naming blower1 requires the fan to be inactive in
  the walking actor's view; it does not mean "activate this device".
- Gate1 labels no walking arc but obstructs the source beam. S3's
  movement-irrelevant label does not make it irrelevant to solving.
- Two connector identities in S0/RC mean one live connector and its
  recording copy. Only the live connector exists initially. RC's two-body
  cost is not two agents or two independently available initial connectors.
- S7 explicitly lacks movement-goal ordering extraction. S4's receiver
  position is unresolved because its location lookup does not locate the
  apparatus coordinate in a region; this is not a missing receiver in the
  spec. RO requires a separately supplied scenario, as the profile states.
- No contradiction was found in the listed control wiring, resource
  roster, ground heights, or geometric beam candidate. This source review
  is not an independent execution of every extractor or a solution replay.

## Wall-blower hand contract

Sources: `tech/wall-blower.lisp`, `tech/-gears-fan.lisp`,
`tech/-stream-passability.lisp`, and `tech/plate.lisp`.

- **Controls:** blower1 is a fixed complete fixture, controlled by plate1;
  no detachable fan or jammer exists in this instance. Plate1 also controls
  gate1. A clear-to-depressed transition flips the physical latch; stepping
  off does not flip it. Recorder shadows use their own controller state.
- **Moves:** while active in an object's own environmental view, blower1
  sweeps occupants at location3 whose base < stream elevation <= top to
  location6. Stream elevation defaults to 1. Ground agents (height 3/2)
  and connectors (height 1) are struck. Held cargo travels with its agent;
  stacked riders move with the relocated support. Pairing facts persist,
  but beam effects are recomputed at the destination.
- **Requires / passage:** the wall-stream band has authored width 4.
  Walking arcs labelled blower1 require it inactive in the actor's view.
  Ghost objects use recording-side turning; live objects use physical
  turning. A wall fixture is not a support or a vertical lift. A live
  connector cannot remain on the ground at location3 while its fan is on.

## Next step

Closeout 2026-09-27: D ran `validate-proposed-trace.lisp`; the saved output
`Proposed-Trace-Validation.txt` confirms integrated success and goal
satisfaction, plus independent recorder validation (T NIL). The following
candidate is now validated as the exact 17-action script, not merely latch
logic. Earlier review findings describe the static profile's limitations
and remain relevant to T31/T32. No search or optimality proof was performed.

Dialog candidate (2026-09-27), not replay-validated: one open recording
cycle permits interleaved live and ghost actions. Starting with both
latches off, the ghost presses and clears plate1, making both on. The live
agent sets its connector at location1 while gate1 is open, then presses
and clears plate1, making physical off / recording on, and reaches
location4. The ghost presses and clears plate1 again, making physical on /
recording off, then carries and connects its connector at location3.
The ghost must delay that final placement until its blower is off; an
earlier placement cannot survive an intervening recording-side activation.
This sequence checks latch logic only: connector setup, movement and the
mixed-view beam still require action-level validation. No search performed.

At closeout, no further Briefing is required to solve this instance.
The original profile's S4 failure and absent H2/H4 hints were unavailable
analysis, not facts. T31 resolves that defect in the regenerated profile;
S4 outputs retain their graph-only qualifications. T32 is complete within
its reporting/coverage scope; general recording-view stable feasibility
remains unresolved. No probe search is authorized by this review.

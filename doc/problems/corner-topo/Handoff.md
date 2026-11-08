# corner-topo — Handoff
Updated 2026-11-07.  Status: UNSOLVABLE under the connector pairing cap (*max-connector-pairings* 3,
pairings on either side counted, 2026-11-05) -- PROVEN 2026-11-07 by exhaustive backward
enumeration (below).  The 15-step Actions.lisp below predates the cap and the
select-termini/put-connector split; it no longer replays (fails at step 13) and is kept as history.

## Proof by backward enumeration (2026-11-07)
Spec probs/problem-corner-topo-advisor.lisp (same rules; enumerator declarations: base relations
has-location, holding, paired, selected; selected shares the cap of 3 with paired and exists only
for the held connector; state-feasible? enforces the rules on those facts; a goal-only filter
requires placed connectors paired in structural sight with receiver2, receiver3, transmitter1 and
transmitter2).  Run on lumpy.
- Goal states: 1,351 (find-goal-states :exclude-relations selected, serial, 4 h 51 min,
  134.3M states).  Leaving out pending selections loses no plan: they change no beam and only
  use up pairing room, so deleting the select steps that made leftover selections still ends
  in a goal state.
- Rule change since the proof: the 1,351 goal states (and the backward layers) predate the
  beam-crossing edits in eea88ff (a beam stops at a wall it cannot clear or at a body in its
  path and cuts nothing beyond).  Rerun 2026-11-08 under the current rules on the cap-3 copy
  probs/problem-corner-topo-advisor-cap3.lisp: 1,409 goal states, a strict superset of the
  1,351 (the 58 new ones all have three placed connectors, nothing held).  So the cap-3 proof
  holds for the old crossing rule only; not rerun, since the cap is now 4.  Saved as
  corner-topo-advisor-cap3-goals-new.lisp (project root, fact lists only).
- Enumerator speedup (2026-11-08): the spec query fluents-feasible? (held-or-placed) is now
  checked right after the fluent choices, before pairings are enumerated
  (*enumerator-fluent-check*, enum-make-fluent-check-action).  Cap-3 goal enumeration, serial:
  30.6M states, 2 h 12 min, against 134.3M states and 4 h 51 min with the check left to
  finalize.
- Threaded goal enumeration checked (2026-11-08, first check of this mode): the cap-3 copy on
  16 threads gives the same 1,409 goal states as the serial run, identical up to connector
  renaming, with the same 30,611,555 states processed; 15 min 25 s against 2 h 12 min.
  Saved as corner-topo-advisor-cap3-goals-threaded.lisp (project root).

## Cap 4 (2026-11-08)
*max-connector-pairings* 4 in problem-corner-topo.lisp and problem-corner-topo-advisor.lisp
(:max-per 4 for paired and selected).
- Goal states: 10,546 (find-goal-states :exclude-relations selected, 16 threads, 2 h 34 min,
  115.5M states; 63,276 before connector-renaming dedupe).  Saved as
  corner-topo-advisor-goals.lisp (project root, fact lists only).
- Backward layers (16 threads, all five actions): 1,211 / 1,125 / 61 / 144 new states, then
  layer 5 saturated.  Closed set: 3,892 states, every one with agent1 at location4, and no
  walk anywhere among their predecessors.  So no arrangement with agent1 east of the partition,
  the start included, can reach the goal.
- Check: find-state-predecessors, actions (move), on a replayed state just after a walk through
  an open gate1 lists all three walks in (from location1, location2, location3), so walk
  predecessors are generated.
- Three enumerator fixes were needed first (src/, 2026-11-07): fps-build-object-sets chooses
  the holder-held pair's holding fact before the held object's facts, so :requires tests see
  it; define-base-relation :shares-max-per (shared per-key cap across relations); and
  fps-action-object-set no longer narrows select-termini (it reaches the held connector through
  a bound variable, so its predecessors were silently skipped -- symptom "4/5 actions").
  Regression-tested 2026-11-07: (run-enumerator-checks) passes (corner, 13 goal states),
  (test-topo) and (test-talos) pass.  The narrowing change also un-narrows START-RECORDER in
  crelay-topo-2 (it writes the ghosts' facts but takes only the agent), a correction of the
  same kind.  Found on the way: (stage corner) hung in the symmetry safety check
  (flow-bind-parameters bound a nested (combination (?t1 ?t2) terminus) as one variable);
  fixed in src/ww-symmetry.lisp, which also now flags a dot-product at any depth.  corner's
  connector symmetry is now dropped by the check (gethash and funcall spots in its beam
  queries); its goal-state count is unchanged.
- Saved: corner-topo-advisor-cap3-predecessors-1..4.lisp in the project root (git-ignored;
  renamed 2026-11-08 so load-predecessors does not pick them up at cap 4).
Why the old 15-step plan worked: it needed connector1 at location2 with four pairings
(transmitter2, receiver3, receiver1, and connector3's pairing to it).  Before 2026-11-05 the
limit of 3 applied to the termini chosen in one connect, and pairings made to a connector by
another connector were unlimited.  The select-termini split only adds steps.

## Unsolvable under the cap -- hand argument (2026-11-06)
The last walk to location4 needs gate1 open (receiver1 red); after it only location4 can change.
- End state must have gate1 closed (with receiver1 red, every blue route to receiver3 crosses
  receiver1's feed).  That leaves (A) connector at location2 {transmitter2, receiver3} blue and
  connector at location3 {transmitter1, receiver2} red, or (B) blue via location1 -> location2
  with the same red, all three connectors east.  A location4 connector can only be fed from
  location3, and location3 -> location4 cuts location2 -> receiver3.
- The walk setup keeps the end state's east links, so receiver1 must be linked in it.  In A, on
  the location2 connector it needs a 4th link; on the location3 connector, location3 -> receiver1
  cuts transmitter2 -> location2 once location4's cutter is gone.  In B, location3 -> receiver1
  cuts location1 -> location2.
- Engine-confirmed by two replays from the start: adding connector3 at location3
  {receiver2 receiver1 transmitter1} activates CROSSING8 against connector2 at location2
  {receiver3 transmitter2} (connector2 dark, receiver3 off), and CROSSING35 against
  connector1 at location1 {transmitter2} -> connector2 at location2 {connector1 receiver3}
  (connector2 dark, receiver3 off).
- Search (16 threads, min-length, depth 19, topo-lower-bound): out of memory at 312 s,
  52.5M states, no solution.  Consistent with the argument, not a proof.
Spec changes: *depth-cutoff* 15 -> 19, (include-tech topo-lower-bound) added.
Details: project doc claude/connector-selection-plan.md, Phase 4b.

## Before the cap (history)
Status 2026-09-30: CLOSED (full path validated).
Note (2026-10-02): `archive/` and `evidence/` under doc/constraint-led-solving/ were removed; files cited there are in git history.
Problem spec: probs/problem-corner-topo.lisp.
Corresponding diagram: doc/problems/corner-topo/problem-corner-topo-plus.jpg.
Maximum search depth: 6 (threads 16), set 2026-09-27.

## Next step       None. (Termini-order rule documented in the Problem-Solving Guide instead of changing the validator, 2026-09-27.)
## State           Actions.lisp: SG1, 15 actions. Endpoint = goal: agent1 at location4 holding connector2; connector3 red at location3 lights receiver2; connector1 blue at location2 lights receiver3. Profile SHA-256 e284b00d81238c1e7939b0e020f60dbaef2c6525d975a39f5ddc927bd5a91d63 (regenerated by traversal-separator Phase 4, 2026-09-29; wording only).
## Checkpoints     None (hand-derived path, no search).
## Restore         (stage corner-topo), then in a separate form
                   (load (merge-pathnames "doc/problems/corner-topo/Actions.lisp" (asdf:system-source-directory :wouldwork)))
                   which validates against GOAL-FN and rewrites Validation.txt.
## Open items      wall3 (11,10)-(16,10), absent from the diagram, deleted from the spec at D's request 2026-09-27; spec and diagram now agree.
                   T40 (2026-09-28): MC now covers beam-crossing and prints the engine crossing order; it confirms the Briefing's hand crossing geometry.
                   T41 (2026-09-28): MC now has a beam-relay contract (stations 1-3 see both hues: COMPETING HUES). REPORT-RELAY-LIGHTING-SCENARIO on prefix 14 shows connector1's blue feed CUT and red arriving in layer 2 (receiver1 lit); on prefix 15, blue in layer 1 and connector3's red IGNORED. Confirms the Subgoal log's lighting reasoning.
                   T43 (2026-09-28): profile gains SD only. gate1 is TEMPORARY (transit and return, not final) via receiver1; receiver2/receiver3 are FINAL. REPORT-SERVICE-TRANSITION 14 -> 15: gate1 LOST with receiver1 withdrawn, return to location1 NOT MET, finals MET. Profile SHA-256 F4A60FD1EC6D87702753C8F6AB14E2DF25CE96248064AB32B225446EA50A7479. Evidence: doc/constraint-led-solving/evidence/t43-services-and-setup-2026-09-28.md.

Traversal-separator migration, 2026-09-30 (doc/traversal-separator-plan.md, removed by commit 693d9848; in git history).
The spec authors no traversal facts and is unchanged across the refactoring; Phase 3
left its fact count unchanged.  Phase 4 regenerated the profile; the diff is wording
only (kind for mode, traversal-mode type dropped, TRAVERSE-VIA signature (location list
location)); regions, crossings and doors unchanged.  The path uses walks only.
Revalidated 2026-09-30: all 15 actions replay (SUCCESS-P T); GOAL-FN T on the final
state, which matches validation-result.txt fact for fact.  (A first run that staged and
validated inside one top-level form reported goal NIL; the goal test there evidently saw
the previously staged problem's GOAL-FN.  Stage in a separate form before validating.)
Legacy problem-corner material deleted 2026-09-30 (in git history): README.md,
goal analysis/, solutions/, enumerator/, constraint-evidence/Problem-Solving-Guide.before-lessons.md.
Flat records layout, 2026-09-30: constraint-evidence/ (INDEX.md, full-path/candidate-actions.lisp,
full-path/validation-result.txt) replaced by Actions.lisp and Validation.txt; in git history.

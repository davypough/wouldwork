# triangle-xyz-6 — Handoff (constraint-method pilot, outside the talos records)
Updated 2026-10-01.  Status: CLOSED -- solved and validated (19 actions, last peg at 41).
Problem spec: probs/problem-triangle-xyz-6.lisp (copy of triangle-xyz with *N* 6; triangle-xyz stays N=5 for the regression tests).
Spec deleted 2026-10-04 (triangle problems consolidated); triangle-xyz is now the occupancy model, whose actions differ from Actions.lisp.  To replay, restore the spec from git: git show 0f16a51:probs/problem-triangle-xyz-6.lisp
Corresponding diagram: the board comment in the spec header (no separate drawing).
Maximum search depth: 4, set 2026-10-01.
## Next step       None.  Closed 2026-10-01; findings in the Briefing's Pilot findings.
## State           Actions.lisp: SG1-SG5, 19 actions, validated from the start against the goal; Validation.txt written.  Static facts derived by hand/script (Briefing, Hints).
## Checkpoints     None.
## Restore         (stage triangle-xyz-6), then in a separate form
                   (load (merge-pathnames "doc/constraint-led-solving/constraint-pilot/triangle-xyz-6/Actions.lisp" (asdf:system-source-directory :wouldwork)))
## Open items      Whether to fold any findings into the talos method documents is D's decision.
                   Pilot scope: run the Problem-Solving Guide by hand on a non-talos problem without tech/ extractors.
                   The Guide, Implementation Plan, Schema-Gaps and doc/constraint-led-solving/problems/ are not touched by this pilot.

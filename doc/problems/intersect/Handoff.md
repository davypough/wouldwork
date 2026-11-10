# intersect — Handoff
Updated 2026-11-10.  Status: solving advisor, goal reached by replay: 40 actions (SG1 8 +
SG2 11 + SG3 21); spec revision 4 (location10 (8, 1), location11 (2, 15) added; location4
and location5 removed for the 999-object limit).
Problem spec: probs/problem-intersect.lisp.
Corresponding diagram (Talos only): doc/problems/intersect/intersect-diagram.png (does not
yet show location10, location11).

## Next step
D: play the 40-action plan in the game.  Optional: a search for a shorter plan.

## State
Accepted actions in Actions.lisp: 40 (cloud replay and D's local validation, 2026-11-10: success T, goal satisfied).
Final state: agent1 at location7; connector1 at location10 (transmitter1, receiver1);
connector2 at location9 (transmitter2, receiver2); connector3 on plate1 at location6
(connector2, receiver4); gates 1, 2, 4 open.

## Checkpoints
None.  intersect has no recorder, so subgoals use the state start: replay Actions.lisp and
take (action-sequence-validation-final-state *accepted-validation*).

## Restore
(stage intersect)
(load (merge-pathnames "doc/problems/intersect/Actions.lisp" (asdf:system-source-directory :wouldwork)))

## Open items
- No static profile on file (the stale one was removed); regenerate if needed.
- The diagram does not show location10, location11 (D's drawing).
- 999-object limit: Schema-Gaps.txt, NEED of 2026-11-10 (open).

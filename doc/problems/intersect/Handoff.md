# intersect — Handoff
Updated 2026-11-08.  Status: solving advisor, SG1 and SG2 accepted; spec revision 3
(location9 at (10, 8)) made during the SG3 dialogue; SG3 next.
Problem spec: probs/problem-intersect.lisp.
Corresponding diagram (Talos only): doc/problems/intersect/intersect-diagram.png.
Search depth: SG2 used cutoff 14 (D's count of about 14 for the leg; shortest found 11),
set 2026-11-08.  SG3's cutoff is A's choice once SG3 is agreed.

## Next step
Fresh session: in-depth reanalysis of the domain for a missing nuance (project doc
claude/intersect-reanalysis-continuation-prompt.md).  As modelled and confirmed against
the game, no solution is found (Briefing, SG3 dialogue).

## State
Accepted actions in Actions.lisp: SG1 (8) + SG2 (11) = 19 actions (replays: success T,
goal not satisfied).
Endpoint: agent1 at location5 (west room) holding connector3; connector1 at location1
paired with transmitter1, receiver1, receiver3 (blue lit; gates 1 and 3 open);
connector2 at location2 paired with receiver2 only (unlit; gate2 closed); receiver4 dark.

## Checkpoints
None.  intersect has no recorder, so subgoals use the state start: replay Actions.lisp and
take (action-sequence-validation-final-state *accepted-validation*).

## Restore
(stage intersect)
(ww-set *threads* 16)
(load (merge-pathnames "doc/problems/intersect/Actions.lisp" (asdf:system-source-directory :wouldwork)))
(ww-set *depth-cutoff* 14)
(setf *min-steps-pruning-enabled* nil)
(solve-subgoal (action-sequence-validation-final-state *accepted-validation*)
  (and (has-location agent1 location3)
       (active receiver3)
       (exists (?c connector)
         (and (has-location ?c location9) (paired ?c transmitter2)))))

## Open items
- Static profile is stale since revision 3; regenerate (Talos tools) before relying on it.
- Spec revision 3 staging and walking/visibility review pending (spec-analysis check 15).
- Gate4's red: resolved in principle by location9 (Briefing, SG3 dialogue); unreplayed.
- Cloud workspace setup (AGENTS.md cloud route) is needed again in a new session; the
  cloud container had 2 cores on 2026-11-08 (SG2's 260 s run).

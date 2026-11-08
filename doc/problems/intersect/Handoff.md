# intersect — Handoff
Updated 2026-11-07.  Status: solving advisor, SG1 accepted, SG2 next.
Problem spec: probs/problem-intersect.lisp.
Corresponding diagram (Talos only): doc/problems/intersect/intersect-diagram.png.
Search depth: SG2 cutoff 14 (D's count for location3 -> location5 is about 14), set 2026-11-07.

## Next step
A agrees SG2 with D (from the SG1 endpoint toward agent1 at location5, or a nearer
precondition such as gate3 open), then searches it at cutoff 14 from the replayed SG1 state.

## State
Accepted actions in Actions.lisp: SG1, 8 actions (replays: success T, goal not satisfied).
Endpoint: agent1 at location3 (east corridor), empty-handed; connector1 at location1
paired with transmitter1, receiver1, receiver3; connector2 at location2 paired with
transmitter2, receiver2, receiver4; connector3 unused at location3.  Gates 1 and 2 open,
gate3 closed (connector2's beam to receiver4 cuts connector1's blue beam to receiver3 at
crossing169).

## Checkpoints
None.  intersect has no recorder, so subgoals use the state start: replay Actions.lisp and
take (action-sequence-validation-final-state *accepted-validation*).

## Restore
(stage intersect)
(ww-set *threads* 16)
(load (merge-pathnames "doc/problems/intersect/Actions.lisp" (asdf:system-source-directory :wouldwork)))
(ww-set *depth-cutoff* 14)
(setf *min-steps-pruning-enabled* nil)
(solve-subgoal (action-sequence-validation-final-state *accepted-validation*) <SG2 goal form>)

## Open items
- The search advisor's growth ladder predates the engine corrections (Briefing); not re-measured.
- Cloud workspace setup (AGENTS.md cloud route) is needed again in a new session; the
  Linux VM route needs D:\quicklisp access, which was refused this session.

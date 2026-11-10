# crelay-topo — Handoff
Updated 2026-11-10.  Status: SOLVED (candidate) - 84 actions; cycle 3 repaired 2026-11-10 for the wall-aware selection sight of 2026-11-07 (connector1 now selects repeater1 at location9 and receiver1 at location12); validated from the start in the Linux VM; awaiting D's Windows replay.
Problem spec: probs/problem-crelay-topo.lisp (cutoff 12, min-length, design notes, enumerator base-relation
              declarations; consolidated 2026-11-10 from the former crelay-topo, -1 and -2 specs, same rules).
Corresponding diagram: doc/problems/crelay-topo/crelay-topo-diagram.png.
Maximum search depth: 12, set 2026-10-16.
## Objective       D, 2026-10-16: derive a solution (any length) independently of the earlier
                   87-action solution, to test whether the solving advisor plus D's suggestions can
                   solve a problem this hard.  Every milestone after recorder cycle 2 must be inferred
                   from the problem domain (spec, profile, diagram, measured searches), not from it.
## Prior solution  The earlier 87-action solution's cycle 3 used insight from a previously given
                   solution.  Its records (prior-solution-*) were deleted 2026-10-18; they are in git history.
## Next step       D: replay on Windows: (stage crelay-topo), then (load "doc/problems/crelay-topo/Actions.lisp")
                   in a separate form; expect 84 actions, all three flags T, recorder validator ACCEPTED,
                   and Validation.txt written.  Then accept SG3 (A changes "SG3 candidate" to "SG3" and
                   marks the Handoff CLOSED).
## State           Actions.lisp: SG1, SG2 (28 actions, accepted) and SG3 candidate (56 actions), 84 in all;
                   replayed 2026-11-10 in the Linux VM (SBCL 2.1.11): goal reached, recorder validator accepted.
## Checkpoints     none; the cycle-2 Checkpoint.txt was deleted 2026-11-10 (superseded by Actions.lisp; in git history).
## Restore         Replay check: after (stage crelay-topo), load Actions.lisp in a separate form.
## Open items      Spec-diagram check done 2026-10-16 (no mismatch; unsettled items listed in the Briefing).
                   Profile regenerated 2026-10-16, unchanged.  Design note in spec-1 corrected 2026-10-16.

# crelay-topo — Handoff
Updated 2026-10-18.  Status: SOLVED (candidate) - 79 actions validated from the start in the container; awaiting D's Windows replay.
Problem spec: probs/problem-crelay-topo-1.lisp (same rules as probs/problem-crelay-topo.lisp; cutoff 12, min-length, design notes).
Corresponding diagram: doc/problems/crelay-topo/crelay-topo-diagram.png.
Maximum search depth: 12, set 2026-10-16.
## Objective       D, 2026-10-16: derive a solution (any length) independently of the earlier
                   87-action solution, to test whether the solving advisor plus D's suggestions can
                   solve a problem this hard.  Every milestone after recorder cycle 2 must be inferred
                   from the problem domain (spec, profile, diagram, measured searches), not from it.
## Prior solution  The earlier 87-action solution's cycle 3 used insight from a previously given
                   solution.  Its records (prior-solution-*) were deleted 2026-10-18; they are in git history.
## Next step       D: replay on Windows: (stage crelay-topo-1), then (load "doc/problems/crelay-topo/Actions.lisp")
                   in a separate form; expect 79 actions, all three flags T, recorder validator ACCEPTED,
                   and Validation.txt written.  Then accept SG3 (A changes "SG3 candidate" to "SG3" and
                   marks the Handoff CLOSED).
## State           Actions.lisp: SG1, SG2 (28 actions, accepted) and SG3 candidate (51 actions), 79 in all;
                   replayed 2026-10-18 in the container: goal reached, recorder validator accepted.
## Checkpoints     Checkpoint.txt | SG1-SG2, 28 actions | SHA-256 8C7885A8…6E7C | cycle-2 endpoint
## Restore         (stage crelay-topo-1)
                   (ww-set *threads* 16)
                   (defparameter *checkpoint* (import-search-checkpoint (merge-pathnames "doc/problems/crelay-topo/Checkpoint.txt" (asdf:system-source-directory :wouldwork))))
                   Replay check instead: after (stage crelay-topo-1), load Actions.lisp in a separate form.
## Open items      probs/problem-crelay-topo-2.lisp (2026-10-17): crelay-topo-1 plus
                   enumerator base-relation declarations, for the predecessor tool.
                   Spec-diagram check done 2026-10-16 (no mismatch; unsettled items listed in the Briefing).
                   Profile regenerated 2026-10-16, unchanged.  Design note in spec-1 corrected 2026-10-16.

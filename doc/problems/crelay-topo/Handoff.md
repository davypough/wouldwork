# crelay-topo — Handoff
Updated 2026-10-17 (R1 re-check).  Status: REOPENED — independent re-derivation; cycle-2 endpoint accepted (28 actions), cycle 3 next.
Problem spec: probs/problem-crelay-topo-1.lisp (same rules as probs/problem-crelay-topo.lisp; cutoff 12, min-length, design notes).
Corresponding diagram: doc/problems/crelay-topo/crelay-topo-diagram.png.
Maximum search depth: 12, set 2026-10-16.
## Objective       D, 2026-10-16: derive a solution (any length) independently of the earlier
                   87-action solution, to test whether the solving advisor plus D's suggestions can
                   solve a problem this hard.  Every milestone after recorder cycle 2 must be inferred
                   from the problem domain (spec, profile, diagram, measured searches), not from it.
## Prior solution  The earlier solution's cycle 3 used insight from a previously given solution.  Its
                   records are kept for history only, renamed with the prefix prior-solution-
                   (Briefing.md, Handoff.md, Actions.lisp, Validation.txt).  Do not read them for
                   this work.  Do not load prior-solution-Actions.lisp: it overwrites Validation.txt.
## Next step       D: approve the fix plan for the predecessor tool's settling gap (project doc
                   claude/settling-predecessors-plan.md, 2026-10-17); A then implements it and re-runs
                   the S4 hand-over check.  After that, continue the Briefing's "Backward analysis,
                   second pass" at step 4 from the revised C3.  Steps 1-3 re-checked with R1 on
                   2026-10-17 (verdicts in the Briefing).
## State           Actions.lisp: SG1, SG2, 28 actions, replayed 2026-10-16 (success).  Endpoint: agent1
                   at location1, empty-handed; connector1 on plate1 (gate1 open); box1 on plate2 (gate3
                   open); tray1 on the ground at location7; gate2, gate5 open; recorder cycle 2 closed.
## Checkpoints     Checkpoint.txt | SG1-SG2, 28 actions | SHA-256 8C7885A8…6E7C | cycle-2 endpoint
## Restore         (stage crelay-topo-1)
                   (ww-set *threads* 16)
                   (defparameter *checkpoint* (import-search-checkpoint (merge-pathnames "doc/problems/crelay-topo/Checkpoint.txt" (asdf:system-source-directory :wouldwork))))
                   Replay check instead: after (stage crelay-topo-1), load Actions.lisp in a separate form.
## Open items      probs/problem-crelay-topo-2.lisp (2026-10-17): crelay-topo-1 plus
                   enumerator base-relation declarations, for the predecessor tool.
                   Spec-diagram check done 2026-10-16 (no mismatch; unsettled items listed in the Briefing).
                   Profile regenerated 2026-10-16, unchanged.  Design note in spec-1 corrected 2026-10-16.

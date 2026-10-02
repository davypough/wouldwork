# crelay-topo — Handoff
Updated 2026-10-02.  Status: CLOSED — solved and validated (87 actions). The method's prototype (T10).
Note (2026-10-02): `archive/` and `evidence/` under doc/constraint-led-solving/ were removed; files cited there are in git history.
Problem spec: probs/problem-crelay-topo.lisp.
Corresponding diagram: doc/constraint-led-solving/problems/crelay-topo/crelay-topo-diagram.png.
Maximum search depth: not set under the current Guide (T10 used cutoffs 8-15; the final leg 10).
## Next step       None. If D reopens the problem, D says what for.
## State           Actions.lisp: SG1-SG4, 87 actions (11 + 20 + 49 + 7). Endpoint = goal: agent1 at location19; tray1 on plate8, connector1 on plate6, box1 on plate7; recorder cycle 3 open with the ghost on plate3. Profile SHA-256 063200E5…2BE2448F (traversal-separator Phase 4, 2026-09-29).
## Checkpoints     None (the T10 checkpoints are archived and superseded by Actions.lisp).
## Restore         (stage crelay-topo), then in a separate form
                   (load (merge-pathnames "doc/constraint-led-solving/problems/crelay-topo/Actions.lisp" (asdf:system-source-directory :wouldwork)))
                   which validates against GOAL-FN and the registered solution validators and rewrites Validation.txt.
## Open items      Prototype record (the frozen Constraint-Prediction-Register.txt, the version-1 Constraint-Realization-Ledger.txt with 22 premises, 9 spine links and 3 bounds, and constraint-evidence/ with every T10 checkpoint) moved 2026-09-30 to doc/constraint-led-solving/archive/crelay-topo-experiment/; that archive was removed 2026-10-02 and is in git history (its 22 .log files were untracked and are gone). Checkpoint hashes remain in doc/constraint-led-solving/evidence/t18-restructure-2026-09-25.txt. If restored from git, its checkpoints other than t10-final-checkpoint-migrated.txt carry pre-migration jump witnesses (commit cfb7c11), and its scripts' doc/problems/ paths no longer resolve.
                   T43 SD and T45 BT (both CANCEL-PLAYBACK boundaries, actions 11 and 31, agree with replay): evidence in doc/constraint-led-solving/evidence/.
                   Traversal-separator migration, 2026-09-29/30: three jump witnesses updated (location20 -> location5 names (blower1); the two alcove jumps name (edge1 gate2)); profile walk arcs 190 -> 171.
                   Flat records layout, 2026-09-30: Actions.lisp built from t10-final-checkpoint-migrated.txt; Briefing.md new.

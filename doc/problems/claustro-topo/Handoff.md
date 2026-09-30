# claustro-topo — Handoff
Updated 2026-09-30.  Status: CLOSED — solved and validated (36 actions).
Problem spec: probs/problem-claustro-topo.lisp (authoritative over the diagram, D 2026-09-27).
Corresponding diagram: doc/problems/claustro-topo/problem-claustro-topo-plus.drawio.
Maximum search depth: 8, set 2026-09-27.
## Next step       None. Closed 2026-09-27.
## State           Actions.lisp: SG1-SG6, 36 actions (8 + 4 + 6 + 6 + 7 + 5). Endpoint = goal: agent1 at location11; box1 on plate1, jammer1 on plate2 jamming gate5, jammer2 on plate3 jamming gate1, box2 at location10. Profile SHA-256 8964B11076661A24B7AFAF772AFC72CEEB6DDEE75CA944D3F46BA9A891AC847B (traversal-separator Phase 4, 2026-09-29).
## Checkpoints     None (the SG1 search checkpoint is superseded by Actions.lisp).
## Restore         (stage claustro-topo), then in a separate form
                   (load (merge-pathnames "doc/problems/claustro-topo/Actions.lisp" (asdf:system-source-directory :wouldwork)))
                   which validates against GOAL-FN and rewrites Validation.txt.
## Open items      G20-G22 resolved at their documented scope (T35-T37); T43 SD and T44 reservations confirm the solution's setup (evidence in doc/constraint-method/evidence/, t35-t44 files).
                   Traversal-separator migration, 2026-09-30: action 34's jump names (edge1), action 36's stairs name (staircase1); revalidated.
                   Flat records layout, 2026-09-30: Stage-Plan.md folded into the Briefing's subgoal log; constraint-evidence/ (cp1-jammer1-at-location7.txt, full-solution-candidate.lisp) replaced by Actions.lisp and Validation.txt; all in git history.

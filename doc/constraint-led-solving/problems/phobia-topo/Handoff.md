# phobia-topo — Handoff
Updated 2026-09-30.  Status: CLOSED — original goal validated.
Problem spec: probs/problem-phobia-topo.lisp.
Corresponding diagram: doc/constraint-led-solving/problems/phobia-topo/phobia-topo-diagram.png.
Maximum search depth: 25, set 2026-09-27.
## Next step       None. Problem solved; no further search authorized or needed.
## State           Actions.lisp: SG1-SG5, 54 actions (7 + 17 + 10 + 4 + 4 + 12). Endpoint = goal: agent1 at location11, lifted by fan1 mounted on fgears1 at location10. Profile SHA-256 CE2BE7BB640A5D797B5FB4DB6FB01DF1D784771EEFDAC142DF8C336CAEFBE2A9 (traversal-separator Phase 4, 2026-09-30).
## Checkpoints     None (the SG3 search checkpoint is superseded by Actions.lisp).
## Restore         (stage phobia-topo), then in a separate form
                   (load (merge-pathnames "doc/constraint-led-solving/problems/phobia-topo/Actions.lisp" (asdf:system-source-directory :wouldwork)))
                   which validates against GOAL-FN and rewrites Validation.txt.
## Open items      T42 (2026-09-28): MC's floor-gears contract supersedes the Briefing's hand contract. T43: SD added; REPORT-SERVICE-TRANSITION 43 -> 44 shows wblower2 KEPT BY ALTERNATIVE (jammer1), wblower3 GAINED (evidence: doc/constraint-led-solving/evidence/t43-services-and-setup-2026-09-28.md).
                   Traversal-separator migration, 2026-09-30: spec unchanged; profile regions renumbered (location10 and location11 now separate); path revalidated, 54 actions, all flags T.
                   Flat records layout, 2026-09-30: constraint-evidence/ (INDEX.md, sg3-checkpoint.txt, sg4a/sg4b/sg5 scripts, complete-validated-path.txt) replaced by Actions.lisp and Validation.txt; INDEX.md's narrative folded into the Briefing's log; all in git history.

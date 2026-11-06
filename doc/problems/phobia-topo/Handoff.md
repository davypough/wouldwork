# phobia-topo — Handoff
Updated 2026-09-30.  Status: CLOSED — original goal validated.
Note (2026-10-02): `archive/` and `evidence/` under doc/constraint-led-solving/ were removed; files cited there are in git history.
Problem spec: probs/problem-phobia-topo.lisp.
Corresponding diagram: doc/problems/phobia-topo/phobia-topo-diagram.png.
Maximum search depth: 25, set 2026-09-27.
## Next step       None. Problem solved; no further search authorized or needed.
## State           Actions.lisp: SG1-SG5, 57 actions (7 + 18 + 12 + 4 + 4 + 12). Endpoint = goal: agent1 at location11, lifted by fan1 mounted on fgears1 at location10. Profile SHA-256 CE2BE7BB640A5D797B5FB4DB6FB01DF1D784771EEFDAC142DF8C336CAEFBE2A9 (traversal-separator Phase 4, 2026-09-30).
## Checkpoints     None (the SG3 search checkpoint is superseded by Actions.lisp).
## Restore         (stage phobia-topo), then in a separate form
                   (load (merge-pathnames "doc/problems/phobia-topo/Actions.lisp" (asdf:system-source-directory :wouldwork)))
                   which validates against GOAL-FN and rewrites Validation.txt.
## Open items      T42 (2026-09-28): MC's floor-gears contract supersedes the Briefing's hand contract. T43: SD added; REPORT-SERVICE-TRANSITION 43 -> 44 shows wblower2 KEPT BY ALTERNATIVE (jammer1), wblower3 GAINED (evidence: doc/constraint-led-solving/evidence/t43-services-and-setup-2026-09-28.md).
                   Traversal-separator migration, 2026-09-30: spec unchanged; profile regions renumbered (location10 and location11 now separate); path revalidated, 54 actions, all flags T.
                   Connector selection (2026-11-06, connector-selection-plan Phase 4b): each connect-connector became select-termini + put-connector; the plain put-downs now read "puts C on ground at L".  Under the 2-link cap, connector2 at location2 now pairs transmitter1 only (receiver1/gate1 is no longer needed by then), leaving room for connector1's link at location5.  Replayed: all flags T, validators accepted, Validation.txt rewritten.
                   Flat records layout, 2026-09-30: constraint-evidence/ (INDEX.md, sg3-checkpoint.txt, sg4a/sg4b/sg5 scripts, complete-validated-path.txt) replaced by Actions.lisp and Validation.txt; INDEX.md's narrative folded into the Briefing's log; all in git history.

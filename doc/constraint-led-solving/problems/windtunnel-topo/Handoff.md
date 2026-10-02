# windtunnel-topo — Handoff
Updated 2026-09-30.  Status: CLOSED — stated goal solved and validated (17 actions).
Problem spec: probs/problem-windtunnel-topo.lisp.
Corresponding diagram: doc/constraint-led-solving/problems/windtunnel-topo/windtunnel diagram.png.
Maximum search depth: 8, set 2026-09-27 (the spec's staged *depth-cutoff* is 16).
## Next step       None. Solved; kept as a regression example.
## State           Actions.lisp: SG1, 17 actions. Endpoint = goal: agent1 at location5; connector1 at location1 and connector1* at location3 light receiver1 through repeater1; gate2 open; one recording cycle open. VALIDATE-RECORDER-SOLUTION accepts. Profile SHA-256 97272F0B64C997A67174272939C0CA8F32FCE3B6357ADBE57A0EC15E027B7EB6 (traversal-separator Phase 4, 2026-09-30).
## Checkpoints     None (no search).
## Restore         (stage windtunnel-topo), then in a separate form
                   (load (merge-pathnames "doc/constraint-led-solving/problems/windtunnel-topo/Actions.lisp" (asdf:system-source-directory :wouldwork)))
                   which validates against GOAL-FN and the registered solution validators and rewrites Validation.txt.
## Open items      G18 resolved by T31; G19 resolved at the scope of T32-T34. Unassigned candidates, future transitions and reachability remain outside those results. T43 SD is physical-view only; T45 BT shows stability does not carry through a STOP or CANCEL (evidence: doc/constraint-led-solving/evidence/, t31-t45 files).
                   Traversal-separator migration, 2026-09-30: spec unchanged; profile diff wording only; trace revalidated.
                   Flat records layout, 2026-09-30: Static-Profile-Review.md folded into the new Briefing; validate-proposed-trace.lisp and Proposed-Trace-Validation.txt replaced by Actions.lisp and Validation.txt; all in git history.

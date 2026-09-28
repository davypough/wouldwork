# claustro-topo — Handoff
Updated 2026-09-28.  Status: CLOSED -- solved and validated (36 actions).  Stage-Plan.md records the stages; no ledger (the full-path validation is the record).
Problem spec: probs/problem-claustro-topo.lisp.
Corresponding diagram: doc/problems/claustro-topo/problem-claustro-topo-plus.drawio.
Maximum search depth: 8, set 2026-09-27.
## Next step       None.  Closed 2026-09-27.
## State           no ledger.  Solution: constraint-evidence/full-solution-candidate.lisp, 36 actions, VALIDATE-ACTION-SEQUENCE from the staged start SUCCESS-P T, goal satisfied (2026-09-27).
## Checkpoints     constraint-evidence/cp1-jammer1-at-location7.txt | 8 actions from start | SHA-256 9eb901a4b949101e90ccd0495864edcd100a4d8e75fbf49d6f14e346a116c45d | agent1 at location7 holding jammer1; box1 at location2; gate4 open (search, min-length, cutoff 8)
## Restore         (stage claustro-topo) (ww-set *threads* 16) then (defparameter *cp1* (import-search-checkpoint <path>))
## Open items      None for this solved problem. G20-G22 resolved at their documented scope; T35-T39 complete.
tech/constraint-profile.lisp RELAY-CHAIN-STATIONS fixed 2026-09-27: no connector -> no stations (claustro has none).
Spec is authoritative over the diagram (D, 2026-09-27). Diagram differs: wall5 at x~3.1 (absent in spec), wall4 at x=7 (spec: edge1), jump legend one-way and L13-L11 as jump (spec: symmetric jump L10-L12, stairway L13-L11), reach arcs L4-L5/L5-L6 (not derived), slab labelled e2 (spec 3/2).

Method update, 2026-09-27 (T37): profile regenerated; only T6 changed.
Gate8/gate9 share one demand of 3; H1 is evaluated but gives no necessity
hint because four pooled bodies remain after the goal actor is excluded.
Jump-support allocation remains a separate domain obligation. The solved
36-action path was not rerun or changed. Profile at T37 SHA-256:
4C9A32F703770B5467623EB0D8C3A730CAE47E2E6E831FF01217D6122CA08B2F.
Evidence: doc/constraint-method/evidence/t37-equivalent-demands-2026-09-27.md.

Method update, 2026-09-27 (T36): G21 resolved. S4 verifies the unreduced
12-row quotient and gives gate8/gate9 directional verdicts. H2 has two
keeper hints; H4 evaluates with none. Graph and S1 qualifications remain;
the solved path is unchanged, with no new search or replay.
Profile at T36 SHA-256:
6C81E181C61AD29F9CF7C9EF4F6B12B315551AF72647D034613991A8CC8160B7.
Evidence: doc/constraint-method/evidence/t36-alternative-families-2026-09-27.md.

Method update, 2026-09-27 (T35): G20 resolved. MC has zero UNCOVERED mechanics;
fixed-beam, jammer and stairs contracts now have instance rows. RC/H3 name
gate1 and location2 for receiver1's fixed corridor. Sightline and corridor
results retain their geometry/view qualifications; no new solution claim.
The 36-action solution remains closed, without another search or replay.
Current profile SHA-256:
88303A097D3069ED6FF5A70096C96EA6F2DDD710DB9C9A01E0ACB89CFFAE0E78.
Evidence: doc/constraint-method/evidence/t35-contracts-2026-09-27.md.

Method update, 2026-09-28 (T43): profile regenerated; only the new SD section
was added. SD shows the gate-dependent jammer placement: gate1's plate-room jam
sites NEED gate1 FIRST through standing providers (gate2/gate3 -> receiver1's
fixed corridor -> gate1), the premise-free sites are location1, location2 and
location7, and the location2 site occupies receiver1's corridor. REPORT-SERVICE-
TRANSITION on the validated path: 5 -> 6 withdraws receiver1 (gate2/3/6/7 LOST,
gate4 GAINED); 28 -> 29 keeps gate1 by jammer2 (KEPT BY ALTERNATIVE). The solved
36-action path is unchanged; no search. Profile SHA-256
6B19AF9D7BD52390787D4F1FA1BB85BF2C99A6ADFB5B98CEF7982DE6CE5EC356.
Evidence: doc/constraint-method/evidence/t43-services-and-setup-2026-09-28.md.

Method update, 2026-09-28 (T44): CP accepts explicit reservations (specification
13.8). The final phase (gate8+gate9; box2 reserved at location10 as the jump
support; jammer1 jamming gate5, jammer2 jamming gate1; box1 free) matches the
three plates with both jammers doubling as weights: jammer2 on a plate needs
gate2 and gate3 open to see gate1. Reserving box1 elsewhere as well gives a B1
shortage that refutes only that allocation. The profile is unchanged (SHA-256
as at T43); the solved path was not rerun. Evidence:
doc/constraint-method/evidence/t44-role-reservations-2026-09-28.md.

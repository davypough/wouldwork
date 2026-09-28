# T43 — Temporary requirements, setup dependencies and service withdrawal

Approved by D, 2026-09-28; completed 2026-09-28 in its own session.
Specification 8.13 was written and saved to D's folder before code.

## Result and scope

New profile section **SD** (grade 2), printed last, after NH. A service is a
gate open, a drive named by a traversal clause clear, or a receiver active.
Each service lists its providers, each a conjunction of premises:
S1 CONTROL options (the aggregate expanded to primitive literals, INVERTED
and drive polarity from UPDATE-GATE-STATUS! and UPDATE-BLOWER-STATUS!), MC
jam sites (OVERRIDE; premises the site's required-open gates; JAM-DISALLOWED>
as notes), gears with no fan (EQUIPMENT), RC chains to a receiver (grouped by
RC class and gate set) and fixed corridors (gates, occupancy locations clear).
An AND-OR closure classifies each option DIRECT, SUPPORTED, NEEDS <service>
FIRST (with a dependency path) or UNSUPPORTED IN SCOPE; a second pass,
"through standing providers", repeats it without spending another jam on a
premise, exposing setup dependencies that a further jammer would hide. RC's
LATCH chains come out as NEEDS <receiver> FIRST, as they should. The section
also gives the goal actor's transit and return door sets (clause-aware
minimal sets over S3's unreduced rows), FINAL and TEMPORARY services (with
their controlling primitives), access per region, retrieval start places,
OPPOSED CONTROLS and a setup-dependency summary.

New optional **SW**, `report-service-transition` / `service-transition-result`:
two settled states and an agent; per passage service KEPT, KEPT BY
ALTERNATIVE (OVERRIDE when only jams remain), LOST or GAINED with the
providers involved; primitive withdrawals and establishments with the devices
they drive (CC's driven facts); jam, pairing and fan-mount changes; arcs,
mobility and retrieval lost and gained; and stated transit, return and final
requirements. Engine agreement: the provider reading equals OBSTACLE-CLEAR in
both states. Recorder problems are UNRESOLVED (views are T45's).

Not claimed: an order, simultaneous availability, body or reach allocation,
occupancy, view, or a realized setup; that the second SW state follows from
the first or which action caused a change; a safe transfer; future
reachability. A cycle is a setup question, not an impossibility. Access is to
a site's own region (placement through a window from another location is not
modelled). No technology semantics, search setting or engine code changed.
No search ran; replay only rebuilt prefixes of validated paths.

## Validation

`t43-service-checks-2026-09-28.lisp`; log and SW reports in
`t43-service-run-2026-09-28.txt`. 126 claustro, 56 corner and 58 phobia
checks passed, plus 2-3 general checks on each of six problems.

- A1 claustro SD: gate1 has only OVERRIDE options; DIRECT at location1,
  location2 and location7 (ground); the eight plate-room sites are SUPPORTED
  only with a further jam and, through standing providers, NEED gate1 FIRST
  (path gate2 or gate3 open -> receiver1 active -> gate1 open). The location2
  site is flagged: it occupies receiver1's fixed corridor, which needs gate1.
  gate2/3/6/7 CONTROL via receiver1 active, gate4 via receiver1 inactive;
  OPPOSED CONTROLS receiver1. jammer2 is at location9 (R6), whose access sets
  all need gate5.
- A2 claustro SW: 5 -> 6 (box1 put at location2) withdraws receiver1: gate2,
  gate3, gate6, gate7 LOST, gate4 GAINED, gate1 KEPT by its jam. 28 -> 29
  (jammer1 picked up after the handover): gate1 KEPT BY ALTERNATIVE (OVERRIDE),
  jammer1 lost, jammer2 remaining; nothing LOST.
- A3 corner SD: gate1 TRANSIT and RETURN necessary, not FINAL: TEMPORARY via
  receiver1; FINAL receiver2 and receiver3 active with chain options; no
  OVERRIDE (no jammer).
- A4 corner SW 14 -> 15: gate1 LOST with receiver1 withdrawn; RETURN to
  location1 NOT MET; FINAL receiver2/receiver3 MET; retrieval of connector1
  and connector3 lost.
- A5 phobia SD: wblower2 CONTROL needs receiver2 active, wblower3 receiver2
  inactive (OPPOSED); both have OVERRIDE sites; wgears1 has EQUIPMENT. receiver2
  has no provider in RC's start-state scope (RC's documented occluder limit),
  so wblower2's CONTROL option is UNSUPPORTED IN SCOPE, not impossible.
- A6 phobia SW 43 -> 44 (connector1 picked up at location5): receiver2
  withdrawn, affecting wblower2 and wblower3; connector1's two pairings
  removed; wblower2 KEPT BY ALTERNATIVE (OVERRIDE, jammer1); wblower3 GAINED;
  nothing LOST; transit to location8 and return from location13 MET.
- A7 synthetic closure tables: SETUP QUESTION, NEEDS FIRST beside a DIRECT
  option, NO PROVIDER, and a dependency hidden by another jam but exposed
  through standing providers; clause-aware door sets respect direction.
- A8 each UNRESOLVED reason prints only its reason; caller states unchanged;
  engine agreement at every evaluated state.
- A9 T40 (462), T41 corner (228) and windtunnel (45), T42 phobia (166) and
  absent-equipment (2 each on five problems) still pass. COMPILE-FILE:
  WARNINGS-P NIL with corner and phobia staged; with claustro staged the only
  warning is the pre-existing undefined BEAM-COORDINATES-CROSSING-RECORDS,
  identical for the pre-T43 file. No problem name, LABELS or FLET in T43 code.

Profiles (SHA-256, before -> after; each "before" equals the stored file,
rumin has no stored profile). Every change is the SD section alone, inserted
before the closing RO note (`t43-<problem>-profile-diff-2026-09-28.txt`):

| Problem | Before | After |
|---|---|---|
| claustro-topo | 88303A09…FFAE0E78 | 6B19AF9D…CE5EC356 |
| corner-topo | CA6EC36C…20320657 | F4A60FD1…A50A7479 |
| crelay-topo | 263947F3…9FE6A9EE | 038FC689…6DC56700 |
| phobia-topo | 22BFB877…B88A08B2 | 23262EC2…FDEFF7EA |
| rumin-topo | B8C70B24…6D58629F | 4AD2684D…867E667B (not stored) |
| windtunnel-topo | 82DBE9A2…B0D7BA04 | 8C64BED2…8FB7B085 |

Schema gaps: G16 (a route names a door, not the work of opening it) is
addressed in part, at the scope above; a status note was added and G16 stays
OPEN for switch reach and route-joined excursions.

## Reproduction

```lisp
(stage claustro-topo)                  ; or corner-topo, phobia-topo
(load "tech/constraint-profile.lisp")
(load "doc/constraint-method/evidence/t43-service-checks-2026-09-28.lisp")
(t43-run-claustro-checks)              ; T43 CLAUSTRO CHECKS PASSED: 126
                                       ; corner 56, phobia 58
(t43-run-general-checks)               ; any problem
```

Ran on SBCL 2.2.9 in a scratch copy of the repository (Debian cl-alexandria,
cl-iterate, cl-lparallel). The transfer archive and compiled files were removed.

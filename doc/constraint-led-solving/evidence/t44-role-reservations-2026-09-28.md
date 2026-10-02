# T44 — Compatible object roles and explicit resource reservations

Approved by D, 2026-09-28; completed 2026-09-28 in its own session.
Specification 13.8 was written and saved to D's folder before code.

## Result and scope

CP (`report-cycle-plan-check`) gains a stage key `:reservations`: each entry
names a body, one role, an optional purpose and a phase range (`:from`,
`:through`, segment ids; default the whole stage). Roles: `:weight plate`,
`:jam target [location]`, `:place location`, `:hold agent`, `:mount gears`,
`:support occupant`, each read from the relation it asserts (ON, JAMMING,
HAS-LOCATION, HOLDING, MOUNTED-ON). New family **B5**, printed only when a
segment has an active or released reservation:

- per reserved body: its roles (SHARED when of two or more kinds) and the
  same-body rules C1-C7 (held versus stationary with the tray exception, one
  location, one value per functional relation, a mounted fan on no support,
  support surfaces, off-plate commitments, types);
- jam sightlines from MC's survey (JAMMER-SIGHTLINE-ROWS): a plate site is
  exact, other locations PASS or CONDITIONAL, the recording view CONDITIONAL;
  premises are the gates whose single closure blocks the sightline;
- capacity K1-K4: an occupant on two supports, contending occupants on one
  support (live and ghost share, per SUPPORT-OCCUPANCY-CONFLICT-P), an agent
  holding two bodies, gears with two fans, a reserved ghost while absent;
- the eligible pool: per reserved body, the unpinned plates it may also
  weight, with premises, or "reserved off plates"; then the free witnesses;
- releases: a reservation that ended at the previous segment; the body counts
  only as the segment states it.

B1 now pins plates held by `:weight` roles and matches the rest with
per-plate eligibility (free witnesses plus reserved bodies whose roles admit
that plate). With reservations, a shortage adds "for the supplied
reservations, refutes this allocation only". Without reservations CP prints
exactly what it printed before.

Not claimed: a reservation's necessity or destination (none is inferred),
reach, transport, placement legality (JAM-DISALLOWED> depends on the agent's
location), order within a segment, or survival of recorder boundaries (T45).
No technology, engine or profile section changed; no search or replay.

## Validation

`t44-role-reservation-checks-2026-09-28.lisp`; log and reports in
`t44-role-reservation-run-2026-09-28.txt`. 63 claustro-topo and 28
phobia-topo checks passed.

- A1 claustro final phase (gate8+gate9; box1 free; box2 `:place location10`
  jump support; jammer1 `:jam gate5`; jammer2 `:jam gate1`): plan PASS, three
  B1 rows (the shared plates once), box2 reserved off plates, both jammers
  eligible for all three plates, jammer2's premise gate2 and gate3 open. The
  pinned end arrangement gives PASS, holders box1/jammer1/jammer2 and SHARED
  weight-and-jam rows sighted from their plates.
- A2 CONFLICTs: held and jamming (C1), two jam targets (C3), the step
  weighting plate1 (C2), box1 and jammer1 on plate1 (K1), a box asked to jam
  (C7). The refuted-sightline branch, which claustro's geometry never
  reaches, is exercised on a copy of the survey lacking one plate site: that
  plate becomes ineligible, another stays eligible. An unsighted location is
  CONDITIONAL. Reserving box1 at location3 gives a B1 shortage (three plates
  against jammer1 and jammer2) with the supplied-allocation text.
- A3 phases: jammer1 jamming gate1 from location1 in s1 (sighted, ground);
  in s2 a release row, jammer1 not a free witness unless s2 lists it, and
  jammer2's reservation starting at s2. An unknown phase or role signals.
- A4 phobia: fan1 mounted on fgears1 and supporting agent1 is SHARED, PASS;
  on wgears1 with a support role, C5; held and mounted, C1; wall-mounted and
  placed, C2 (no location). K1-K4 and live/ghost sharing on constructed
  commitments (capacity reads no types). Recording view: CONDITIONAL.
- A5 T24's crelay-topo checks: 19 passed; the CP output of its plan and three
  one-segment plans is byte-identical to the pre-change capture. Caller plans
  unchanged. No problem name, LABELS or FLET in the CP code. COMPILE-FILE with
  claustro, crelay or phobia staged: only the pre-existing undefined
  BEAM-COORDINATES-CROSSING-RECORDS style warning, identical for the pre-T44
  file. The claustro profile regenerates byte-identical (CP is not in it).

Schema gaps: G12 status note added (addressed in part; OPEN). G8 unchanged.

## Reproduction

```lisp
(stage claustro-topo)                  ; or phobia-topo
(load "tech/constraint-profile.lisp")
(load "doc/constraint-method/evidence/t44-role-reservation-checks-2026-09-28.lisp")
(t44-run-claustro-checks)              ; T44 CLAUSTRO CHECKS PASSED: 63
(t44-run-phobia-checks)                ; on phobia-topo: 28
```

Ran on SBCL 2.2.9 in a scratch copy of the repository (Debian cl-alexandria,
cl-iterate, cl-lparallel). The transfer archive and compiled files were removed.

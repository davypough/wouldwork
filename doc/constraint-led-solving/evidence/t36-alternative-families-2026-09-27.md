# T36 — Alternative door families

Approved and completed 2026-09-27. Specification 10.7 written before code.

## Implementation

Chosen approved option: keep every quotient row when a multi-clause family
occurs. The existing deterministic row order and final reachability comparison
remain. S4 now evaluates the retained graph instead of declaring the entire
problem unsupported; single-clause reduction is unchanged.

KEEPER-ROW-AVOIDS-P already implements DNF correctly: any clause avoiding the
excluded device suffices. The door-name union identifies affected devices;
it never substitutes for clause semantics in the reachability walk. H2 now
rejects a necessary-device claim when another clause of that same row avoids
it. Empty clauses and directedness are preserved.

Claustro S4 reports VERIFIED-AGAINST-QUOTIENT with all 12 rows, explicitly
labels the unreduced strategy, and gives directional verdicts for gate8/gate9.
H2 yields two hints; H4 evaluates successfully with zero applicable hints.
These remain relaxed graph results subject to S1 qualifications, not proofs
of a legal move, full configuration, recorder lifecycle or permanent stranding.

## Evidence and validation

11,490 checks passed, plus byte-identical windtunnel full-profile comparison:

- 4,120 adapted T31 synthetic checks and 39 windtunnel staged checks. The
  original T31 script remains untouched; the T36 copy changes only task
  naming and its obsolete expectation that alternatives are unsupported.
- 7,173 further DNF checks: all 64 directed topologies on three regions,
  four clause/direction assignments, full/reduced versus independent closure
  for every region and exclusion, input reversal, S3/S4 agreement and no
  input mutation. Includes empty clauses, shared doors and uncontrolled doors.
- Two H2 fixtures: an optional device produces no necessary keeper hint;
  a device shared by every clause can produce a hint.
- 156 claustro staged checks, including all-device reachability and H2/H4.
- COMPILE-FILE returned WARNINGS-P NIL and FAILURE-P NIL.

One fixture setup correction: the H2 positive example needs a pressure plate,
so it runs after staging claustro, not windtunnel (whose plate is a toggle).
No production change was required for that fixture correction.

Windtunnel before/after SHA-256 (identical to its current saved profile):
F499513674CF4F1E56F0EB0E595DD9E954F1ED8FE28ED0371505F498312FCF04.
Claustro regenerated profile SHA-256:
6C81E181C61AD29F9CF7C9EF4F6B12B315551AF72647D034613991A8CC8160B7.
The retained diff changes S4 and NH only. Generated output was not hand-edited.

## Reproduce

Use a separate SBCL process with WOULDWORK_INSTANCE=t36 and an isolated ASDF
cache. Quickload Wouldwork, then from the repository directory:

```lisp
(stage windtunnel-topo)
(load "tech/constraint-profile.lisp")
(load "doc/constraint-method/evidence/t36-reachability-checks-2026-09-27.lisp")
(t36-staged-checks)
(load "doc/constraint-method/evidence/t36-alternative-checks-2026-09-27.lisp")
(t36-alternative-checks)
(write-static-constraint-profile "doc/constraint-method/evidence/t36-windtunnel-after-2026-09-27.txt")
(stage claustro-topo)
(load "tech/constraint-profile.lisp")
(load "doc/constraint-method/evidence/t36-alternative-checks-2026-09-27.lisp")
(t36-h2-alternatives)
(t36-claustro-checks)
*t36-checks* ; 11490
(write-static-constraint-profile "doc/problems/claustro-topo/Constraint-Static-Profile.txt")
```

For compilation verification, compile/load the profile after first staging and
assert both warning and failure flags are NIL. Load goal-sensitive definitions
after each stage. Compare the windtunnel output with the retained before file.
SBCL 2.6.8, 4096 MB dynamic space. No search or action replay ran; temporary
launchers, compiled diagnostic and cache removed after validation.

## Artifacts

- `t36-reachability-checks-2026-09-27.lisp`: adapted T31 regression checks.
- `t36-alternative-checks-2026-09-27.lisp`: DNF and clause-aware H2 checks.
- `t36-reachability-run-2026-09-27.txt`: final compilation and checks log.
- `t36-windtunnel-before-2026-09-27.txt`, `t36-windtunnel-after-2026-09-27.txt`.
- `t36-claustro-profile-before-2026-09-27.txt` and
  `t36-claustro-profile-diff-2026-09-27.txt`.

G21 resolved; plan, Briefing and Handoff synchronized. Next: T35, proposed,
awaiting approval.

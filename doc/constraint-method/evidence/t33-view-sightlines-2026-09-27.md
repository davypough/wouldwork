# T33 physical and recording-view sightlines — 2026-09-27

COMPLETE. D approved implementation and focused checks in this session.
All 61 assertions passed. The diagnostic compiled without warnings or
failure. No search, puzzle solve, solution replay or arrangement settling ran.

## Result and limits

`RELAY-VIEW-RESULTS` and `REPORT-RELAY-VIEW-SCENARIO` accept a complete
reference-state scenario (specification 6.1). RC, NH and the profile reporter
accept the same optional scenario; the writer accepts it after pathname.
The ordinary profile reports both scenario views UNRESOLVED when none is
supplied. Its existing geometric candidates and bounds are retained.

Each supplied scenario carries phase, provenance, explicit hops/chains and
optional physical/recording gate premises. Placements, supports, heights,
pairings and presence come from the reference state and staged engine.
No body assignments or ghosts are synthesized. Gate bits are overridden only
on private copies, with no controller-consistency claim. Physical receiver
checks use freshly computed relay colors on that copy, not stale input colors.

Windtunnel's injected mixed scenario is PHYSICAL CLEAR and RECORDING BLOCKED:
the physical beam can use the ghost connector, but the recording view lacks
the required live connector. The recording gate is also closed in the supplied
hop fixture. This is not a new puzzle solution, replay or stable arrangement.
The old solution evidence remains unchanged. T34 still requires approval.

CLEAR means all supplied checks pass in this state under these premises.
BLOCKED means a tested hop/chain failed; INVALID identifies malformed chain
resources; missing/unsupported context is UNRESOLVED. Gate/location failures
are named. A complete-state declaration is a caller premise, not full state
validation; malformed list shapes surface errors. T34 owns general structural
validation, settling and controller feedback. No cross-scenario hop joining
or exhaustive recording arrangement enumeration is claimed.

## Acceptance coverage

- Six input checks execute on loading the check file before explicit staging.
- All four physical/recording gate combinations agree with direct visibility.
- Supplied mixed windtunnel chain agrees with engine lighting and receiver
  evaluation; mapped live relay absence is reported in the recording view.
- The recorder-gate-shadow fixture checks mapped live blockers excluded from
  recording, ghost blockers included, and live versus recorded relay presence.
- Pairing capacity, ghost-to-live dependency policy, reused identity, missing
  connector, closed recording context, phase conflict and conflicting gate
  premises are checked. No recording cycle is inferred from missing data.
- Ordinary beam-relay physical results agree with engine receiver evaluation;
  recording is unavailable. Missing visibility technology cannot use a neutral
  hook as a false negative.
- RC/NH optional scenario integration, per-view provenance, named reasons,
  original-state/static preservation and reference preservation on invalid
  input and exceptions are checked.

The final run contains normal staging function-redefinition warnings. There
are no diagnostic compilation warnings, undefined-function warnings or errors.
During implementation, the initial LOOP form was corrected; recording mappings
were changed to canonicalize bijective storage indexes; final pairing tests
were changed to read relation facts rather than call relations as functions.
These corrections preceded the retained successful run.

## Reproduce

From the repository directory in WW, with Wouldwork loaded:

```lisp
(load "tech/constraint-profile.lisp")
(load "doc/constraint-method/evidence/t33-view-checks-2026-09-27.lisp")
(stage windtunnel-topo)
(t33-wind-checks)
(stage recorder-gate-shadow-test)
(t33-shadow-checks)
(stage beam-relay-test)
(t33-ordinary-checks)
(stage windtunnel-topo)
*t33-checks* ; 61
(report-relay-view-scenario
 (t33-scenario (t33-wind-state) :open '((location1 1 transmitter1 1))
               '((transmitter1 connector1 repeater1 connector1* receiver1))
               '((:physical gate1 t) (:recording gate1 nil))))
(write-static-constraint-profile
 "doc/problems/windtunnel-topo/Constraint-Static-Profile.txt")
```

Execution used SBCL 2.6.8 with 4096 MB dynamic space and normal Quicklisp
initialization. A temporary workspace ASDF cache kept writes local. The
temporary scripts, compiled file, baseline source copy and cache were removed.
Final staging was windtunnel-topo. Technology semantics and problem specs were
unchanged. The source diff is against the pre-T33 working copy, preserving all
earlier uncommitted development.

## Retained files

All paths below are in this evidence directory:

- `t33-view-checks-2026-09-27.lisp`: executable assertions and supplied fixtures.
- `t33-view-run-2026-09-27.txt`: final 61-check and warning-free compilation log.
- `t33-windtunnel-scenario-2026-09-27.txt`: generated mixed-view report.
- `t33-windtunnel-profile-before-2026-09-27.txt`: byte-preserved prior profile.
- `t33-windtunnel-profile-diff-2026-09-27.txt`: regenerated profile differences.
- `t33-source-diff-2026-09-27.txt`: diagnostic change against session baseline.

Profile SHA-256 before:
`C326323159AE2D40FE2F17B36B5E0318E7808A050C968993C77A8ACC721C9F55`.
Profile SHA-256 after:
`F499513674CF4F1E56F0EB0E595DD9E954F1ED8FE28ED0371505F498312FCF04`.

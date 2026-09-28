# T32 wall-blower coverage and relay qualifications — 2026-09-27

Approved by D on the proposed scope. COMPLETE: 57 focused checks passed.
No search, puzzle solve or solution replay was run. Technology physics and
problem specs are unchanged; the changes are in the loadable diagnostic.

## Result

MC now covers fixed and mountable wall drives, including stream dimensions,
body-strike interval, presence, per-object environmental view, support/cargo
motion, and pairing retention. CC distinguishes horizontal transport from
floor lift; its existing lift/barrier analysis is unchanged.

RC preserves geometric candidates while reporting wall-stream exposure and
explicit recording-view/occupancy uncertainty. A live station conflict needs
both stream contact and an S1-proven state/aggregate equivalence between a
required physical gate and a fixed wall blower. Live overrides or opposite
polarity block that inference. It never proves a ghost's fan activity.
NH bounds apply within RC's physical geometric enumeration, not every
recording-view beam. An absent geometric bootstrap candidate no longer
asserts receiver impossibility.

Windtunnel: all eight public technologies covered; the one two-connector
geometric chain remains. The ground connector at location3 conflicts with
blower1 while physical gate1 is open. The existing mixed-view solution
remains validated by its unchanged prior evidence. G19's reporting gap is
addressed; general recording sightlines and stable occupancy remain OPEN
and explicitly UNRESOLVED. T32 does not implement a stability solver.

## Checks and execution

57 assertions: eight pure boundary/role checks before explicit diagnostic
staging, 33 windtunnel checks, eight mountable-fan checks, five high-stream
checks, and three floor regression checks. The copied-state fixtures invoke
the real SWEEP-OCCUPANTS-AWAY! update. They cover all four live/ghost activity
combinations, pairing retention, vacant/present fans, flush plate landing,
below-stream and inclusive-top bodies, and support detachment. Injected
states test mechanics; they are not claimed to be reachable puzzle states.
The high-stream instance declares elevation before staging. No search runs.

The floor baseline was captured from pre-T32 code on staged crelay-topo:
contract text, instance output, motion roles and lift/barrier rows compare
exactly. Final windtunnel profile changes only MC, RC, CC and NH; S0-S7 and
other sections compare unchanged. Added diagnostic source contains no
problem object names and no LABELS/FLET. Diagnostic cold loading succeeded.
Successful runs have expected staging and old/new function-redefinition
warnings, no undefined-function warnings or errors.

Test development corrections: global lexical *STATIC-DB* cannot be LET-bound;
an attempted post-staging static-height injection did not exercise the
intended compiled height. It was replaced by an authored isolated fixture.
The fixture's derived TURNING assertion was moved from DEFINE-INIT into its
copied test state, per the engine's initialization rule. The generated
problem was recovered through ENSURE-PROBLEM-STAGED, never hand-edited.
The retained run is the final successful run.

Execution used SBCL 2.6.8, 4096 MB dynamic space, normal initialization and
Quicklisp. ASDF output was redirected to a temporary workspace cache to
respect filesystem permissions. Temporary launch files, source snapshot and
cache were removed at closeout. Final staging was windtunnel-topo.

## Reproduce checks

Run in WW from the repository directory. The checks file runs its pure
checks when loaded; the following calls run the staged parts, total 57.
The saved floor baseline is data, read in WW, not loaded as code.

```lisp
(load "tech/constraint-profile.lisp")
(load "doc/constraint-method/evidence/t32-wall-blower-checks-2026-09-27.lisp")
(stage windtunnel-topo)
(t32-windtunnel-checks)
(stage wall-blower-test)
(t32-mountable-checks)
(stage "doc/constraint-method/evidence/problem-t32-high-stream.lisp")
(t32-high-stream-checks)
(stage crelay-topo)
(t32-floor-checks
 (with-open-file (stream "doc/constraint-method/evidence/t32-floor-baseline-2026-09-27.lisp")
   (read stream)))
(stage windtunnel-topo)
```

Regenerate the profile with `(write-static-constraint-profile
"doc/problems/windtunnel-topo/Constraint-Static-Profile.txt")` after staging
windtunnel-topo and loading the diagnostic. No hand edits to generated output.

## Artifacts

All paths below are relative to this evidence directory.

- `t32-wall-blower-checks-2026-09-27.lisp`: acceptance checks.
- `problem-t32-high-stream.lisp`: authored isolated height fixture.
- `t32-wall-blower-run-2026-09-27.txt`: final successful 57-check run.
- `t32-floor-baseline-2026-09-27.lisp`: captured pre-T32 floor data.
- `t32-profile-generation-2026-09-27.txt`: baseline capture and profile generation.
- `t32-windtunnel-profile-before-2026-09-27.txt`: byte-preserved pre-T32 profile.
- `t32-windtunnel-profile-diff-2026-09-27.txt`: generated profile diff.
- `t32-source-diff-2026-09-27.txt`: diagnostic changes against the existing
  pre-T32 working file, excluding earlier uncommitted work.

Profile SHA-256 before:
`553B5F4E388773214AB1A2276DEAC0C5775DE971790217AED2649C02EEF0415F`.
Profile SHA-256 after:
`C326323159AE2D40FE2F17B36B5E0318E7808A050C968993C77A8ACC721C9F55`.

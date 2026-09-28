# T31 reachability and hint availability — 2026-09-27

Approved by D on the proposed T31 scope. Complete. No search or replay.

## Changes and acceptance

S3/S4 share deterministic sequential reduction. A removed row has a
same-door or cheaper replacement in each permitted direction, and deletion
preserves full graph reachability from all regions with no exclusion and
with every named device excluded. Unsupported alternative families remain
explicitly unsupported. S4 still verifies the final graph. S3 calls its
spine a representative graph rather than a physical doorway count.

Composition now distinguishes a found door-free replacement from no
replacement. NH carries S4's reason separately from a valid empty graph;
H2/H4 print UNAVAILABLE on failure, without counting failures as hints.

4,120 synthetic checks passed before staging. They include both mutual
redundancy regression fixtures, asymmetric reverse paths, door-free
replacement, all 64 directed three-region topologies with three door-label
assignments, input reversal, S3/S4 agreement, an independent transitive
closure oracle under every exclusion, unsupported families and NH output.
39 staged checks passed (4,159 total), including each controlled device,
upstream failure propagation into NH, and restored R1 outgoing reachability.
Windtunnel has twelve full rows and five retained rows; S4 reports
:VERIFIED-AGAINST-QUOTIENT. H2/H4 legitimately remain empty here.

The profile diff changes only S3/S4. T32 coverage/relay qualifications are
still pending. Existing puzzle replay evidence and its hashes are unchanged.

## Reproduce in package WW

```lisp
(load "tech/constraint-profile.lisp")
(load "doc/constraint-method/evidence/t31-reachability-checks-2026-09-27.lisp")
(stage windtunnel-topo)
(t31-staged-checks)
(write-static-constraint-profile
 "doc/problems/windtunnel-topo/Constraint-Static-Profile.txt")
```

Executed locally with SBCL 2.6.8, 4096 MB dynamic space, normal initialization
and `(ql:quickload :wouldwork)`. The first launch stopped before diagnostic
loading because the sandbox could not write the external ASDF cache.
Successful launches redirected this repository's ASDF outputs to a temporary
workspace cache; no permission escalation or machine setting change.
That cache and its launch files were removed after completion.
The diagnostic loaded without warnings. Staging emitted the existing
DERIVE-RECORDING-COPY-LITERALS redefinition warning; no other warning or
error occurred in the successful runs.

## Evidence files

All paths below are relative to this evidence directory.

- `t31-reachability-checks-2026-09-27.lisp`: retained acceptance checks.
- `t31-reachability-run-2026-09-27.txt`: successful load, checks and S4 output.
- `t31-profile-generation-2026-09-27.txt`: successful profile generation.
- `t31-windtunnel-profile-before-2026-09-27.txt`: byte-preserved original profile.
- `t31-windtunnel-profile-diff-2026-09-27.txt`: generated before/after diff.

Profile SHA-256 before:
`8D0C7957C753B2FB9588CD2CD43F5FE214B9C47E182AEBD3B24F853071A06038`.
Profile SHA-256 after:
`553B5F4E388773214AB1A2276DEAC0C5775DE971790217AED2649C02EEF0415F`.

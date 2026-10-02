# T34 supplied-arrangement stability — 2026-09-27

COMPLETE. D approved T34 after T33. All 155 focused assertions passed;
`constraint-arrangement.lisp` compiled without warnings or failure.
No puzzle solve, search, action enumeration or solution replay ran.

## Result

The new optional diagnostic `tech/constraint-arrangement.lisp` is loaded
after the static profile diagnostic and staging. It checks one supplied
complete state, validates its configuration, runs the engine's existing
bounded propagation, validates the result, verifies a second pass is a fixed
point, and uses T33 plus the actual derived receiver fact to check the beam.
It does not run implicitly in any static report.

`CHECK-RELAY-ARRANGEMENT` returns a plist; `REPORT-RELAY-ARRANGEMENT` prints
and returns it. Input is the T33 scenario plus `:view` and `:receiver`, with
at least one identity chain to that receiver and no forced gate premises.
See Extractor Specifications 6.2. The entire physical configuration is the
requested arrangement, including incidental agents, supports and pairings.

Verdicts: STABLE-AND-BEAM-WORKING, SETTLED-BUT-FAILED, INVALID,
INCONSISTENT, or UNRESOLVED. Results include the settled private state,
changed facts, configuration changes, receiver state and view results where
available. Propagation rejection or its existing cap is not an impossibility
claim. Exceptions retain their cause and stage and do not escape as success.

Windtunnel's supplied mixed configuration is stable and lights receiver1.
The role-swapped live-ground configuration is geometrically clear under the
required gate premise before settling, but propagation sweeps the live
connector from location3 to location6 and the required beam fails. These are
supplied hypotheses, not an independently discovered solution or new replay.
The original solution evidence and static profile remain byte-unchanged.

## Checks

- Windtunnel: intact mixed-view configuration, failed recording beam,
  stationary unlit configuration, all asymmetric/both-active fan splits,
  forbidden forced gate inputs, pairing capacity, role-swapped live-ground
  failure, and no repeated toggle on an already-depressed plate.
- Authored isolated fixture: ordinary working arrangement, swept support
  stack and preserved rider link, landing on a controller closing a required
  gate and extinguishing the receiver, floor-hover drop with stack intact,
  and a real active two-blower transport cycle hitting the existing cap.
- Structural failures: relation argument type, support cycle, location
  disagreement, held-and-located connector, grounded tray used as support,
  support capacity, invalid wall-mounted fan location, and missing live body.
- Existing support-settling fixture: runtime live-on-ghost-held-tray exception
  remains valid; nested ON/HOLDING cycles are rejected before recursive TOP.
- Fault injection at the diagnostic propagation boundary: an error after
  changing the private copy returns UNRESOLVED; second-pass metadata drift
  also returns UNRESOLVED. The original function is restored by UNWIND-PROTECT.
  These are error-path tests, not altered technology physics.
- Every checked scenario compares the reference state/metadata, input plist,
  static facts, type table and relation tables before and after. Success,
  failure, invalidity, nonconvergence and exceptions all preserve them.

There are normal staging/redefinition warnings in the retained run, but no
diagnostic compilation warnings, undefined functions or errors. During test
development, the scenario snapshot was corrected to copy the plist directly,
and nullary relation signatures were normalized from the engine's T marker
before strict arity checking. The retained run is the final successful run.

## Limits

All geometry/wiring comes from the staged problem. Supplied configuration,
primitive controls and edge memory are premises; gate/fan/color/receiver
caches are rederived normally. The check does not clear pressure edge memory,
seed a recorder cycle or synthesize a press. It validates the supplied state's
physical schema, not reachability or the truth of its claimed history.

Tray-release highest-below settling is event-driven. Merely checking a state
does not release a tray. Wall/floor propagation includes its own transport,
support displacement and landings. Future toggles, recorder closure,
cancellation and release transitions need separately supplied checks.
Scheduled happenings are unsupported. No arrangement enumeration or general
gravity simulation is introduced. No static candidate inherits a stability
verdict without an explicit supplied-state check.

## Reproduce

Run in WW from the repository directory, with Wouldwork loaded:

```lisp
(load "tech/constraint-profile.lisp")
(load "tech/constraint-arrangement.lisp")
(load "doc/constraint-method/evidence/t33-view-checks-2026-09-27.lisp")
(load "doc/constraint-method/evidence/t34-arrangement-checks-2026-09-27.lisp")
(stage windtunnel-topo)
(t34-wind-checks)
(t34-edge-memory-check)
(t34-live-ground-check)
(stage "doc/constraint-method/evidence/problem-t34-arrangement.lisp")
(t34-fixture-checks)
(t34-exception-checks)
(stage support-settling-test)
(t34-held-tray-checks)
(stage windtunnel-topo)
*t34-checks* ; 155
(report-relay-arrangement
 (t34-scenario (t34-wind-state t nil) :open 'receiver1
               '(transmitter1 connector1 repeater1 connector1* receiver1)))
(report-relay-arrangement
 (t34-scenario (t34-live-ground-state) :open 'receiver1
               '(transmitter1 connector1* repeater1 connector1 receiver1)))
```

SBCL 2.6.8, 4096 MB dynamic space, normal Quicklisp initialization.
A temporary workspace ASDF cache kept writes local; temporary launcher,
cache, compiled diagnostic and scratch files were removed at closeout.
The final staged problem was windtunnel-topo. No existing technology or
problem definition was edited; the new authored test is retained evidence.

## Artifacts

Files in this evidence directory:

- `problem-t34-arrangement.lisp`: authored isolated scenario fixture.
- `t34-arrangement-checks-2026-09-27.lisp`: reproducible checks and supplied data.
- `t34-arrangement-run-2026-09-27.txt`: final 155-check and compilation log.
- `t34-windtunnel-arrangements-2026-09-27.txt`: generated success/failure reports.

Unchanged windtunnel static profile SHA-256:
`F499513674CF4F1E56F0EB0E595DD9E954F1ED8FE28ED0371505F498312FCF04`.

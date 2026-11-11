# Support loss and settling

Local held-tray release is the initial support-loss event. Merely sharing a location,
carrier movement, recorder stop/cancellation, and blower transport do not trigger this
new selection policy. Across-reach tray release retains its existing ground unloading.

## Release transaction

`place-held-object!` snapshots direct riders and their bases while the tray is held.
It relocates the tray and dependent stacks, removes the direct rider links, and places
the released tray. It then selects every rider's landing from that resulting geometry
before assigning any landing or running beam/plate/gate propagation. Internal stack
links and connector pairings are preserved. Heights remain derived from ON/HOLDING.

A candidate must be at the same **named location**, permitted by recorder policy,
clear for the arriving layer, and independent of the falling group. Its top must be
between the destination floor and the rider's previous base, inclusive. Select the
highest top. A support at floor height takes precedence over bare ground; ground is
used only when no eligible support remains. Multiple highest supports signal an error
with rider, location, height, and candidates. Joint capacity conflicts also signal errors.
No lexical or hash iteration order resolves ambiguity and no ambiguous successor is
silently discarded. Falling groups cannot catch one another during the same release.

There are no inferred widths, nearby-location catches, or solid-volume collision tests.
An occupied surface is unavailable; its eligible upper box can itself be a candidate.
The newly placed tray consumes normal capacity on its destination and is not a support.
Support/holding cycles are errors. Candidate surfaces dependent on the rider are excluded
before calculating their heights. Shared relocation follows ON and held trays carried by
supported agents, so their cargo moves with them; ordinary held cargo retains no location.

## Recorder boundaries and search

Ghost support selection excludes live mobile supports. Live cargo can use a ghost-held
tray while recording is in progress, but mixed-layer box support stays prohibited.
Authored initial ON facts still require same-layer mobile supports; the runtime tray
exception does not relax initialization. Stop still checks physical boundary safety.
Cancellation still removes ghost references and normalizes the surviving state without
catching live riders. Pairings to disappearing ghost endpoints follow existing cleanup.

Settling is deterministic where geometry is unambiguous, has no extra action cost, and
adds no stored falling flags, caches, or asynchronous propagation pass. Ordinary relation
updates participate in the existing successor-copy and hash-invalidation machinery.
Box pickup still requires a clear top. No Crelay puzzle feasibility claim follows from
these capability tests; its historical 35-action checkpoint has not been replayed here.

## Ownership and verification

`-support-motion` owns shared surface enumeration, dependency traversal and relocation,
and the existing blower landing queries/updates extracted from `-gears-fan`.
`-placement` keeps manual reach and actor policy. `-support-settling` owns highest-below
selection and the release batch. Blower selection retains its original support kinds,
category order and optional exact-height constraint, and excludes dependency cycles.

`test/talos/problem-support-settling-test.lisp` exercises real tray-release successors, beam
clearance and pairings, recorder layers, lower stacks, ground fallback, ordinary movement,
remote release, ambiguity, capacity and cycle exclusions, and cancellation. Existing
tray, box, jump, ladder, step, blower and recorder fixtures cover shared-mechanism regressions.

## Verification record (2026-09-11)

All 19 focused settling claims passed, including an observation at the propagation
boundary proving that an equal-height catch exposes no grounded intermediate state.
The fixture also checks independent successor copies, graph hashes, propagation
idempotence, highest-support ranking, lower ties, and normal stop after handoff.

The existing harness ran claims and small searches successfully for these 16 fixtures:
tray, box, jump, ladder, step, floor-gears, wall-blower, angled-blower,
recorder-isolation, recorder-core, recorder-occupancy, recorder-cycle-state,
recorder-cancel, recorder-two-cycle, recorder-floor-blower, and beam-elevation.
Their expected solution lengths were checked by `talos-problem-failed-p`.
The new fixture's zero-action harness also passed before its additional characterization
claims were added; the final 19 claims were subsequently staged and run directly.
This was a focused regression selection, not the complete Talos or mutation suite.
No Crelay solve, subgoal search, or solution/recorder replay was run.

To rerun the focused characterization after loading Wouldwork:

```lisp
(stage "test/talos/problem-support-settling-test.lisp")
(run-test-claims)
```

# T42 — Removable equipment and mounted-fan consequences

Approved by D, 2026-09-28; completed 2026-09-28 in its own session.
Specification 8.12 was written and saved before code; a dated clarification
beneath it records what implementation settled.

## Result and scope

MC's floor-gears entry is now a source-grounded contract (it was UNCOVERED),
so phobia-topo reports zero UNCOVERED. The step entry now names both
boarding contracts, closing T28's "gears-mounted fans have no component".
The floor-gears block lists every removable fan with its start place and
every gears instance as a compatible mount, with the one-fan-one-stream
limit (phobia: fan1 for wgears1 and fgears1, at most one stream). Per mount:
endpoints and destination level, working height, control, start occupancy
(fgears1 VACANT, TURNING, NO FAN; wgears1 EFFECTIVE STREAM), reach sites by
the engine's REACHABLE with the vertical-reach test, the removal
consequence (stream stops, gears keep turning, a jam becomes redundant, the
32 arcs wgears1's stream gates), and for a floor mount the installation,
boarding, lift and landing requirements with the destination's exits. Wall
stream physics stays in the wall-blower contract; fixed floor blowers are
unchanged.

New optional `report-equipment-scenario` / `equipment-scenario-result` (EQ)
reads one settled state with the engine's queries: fans (MOUNTED, WALL-HUNG,
HELD, RESTING; BLOWING; STEPPABLE), mounts (EFFECTIVE STREAM, TURNING NO FAN,
FAN MOUNTED STOPPED, VACANT STOPPED, with jammers), boarding (BOARDABLE or
the first failing condition; NOT STEPPABLE for a resting fan), mounting and
removal (MOUNTABLE / PICKUP POSSIBLE or every failing condition), and each
occupant held aloft with the drives sustaining it. Mounting and removal
verdicts agree with the engine's successor generator, boarding with its step
provider. With `:before` it compares two settled states: fan places, mount
statuses (STREAM GAINED / LOST), moved objects, aloft gained / lost.
Missing, unsettled or inconsistent input, no fan or gears, or a spliced
recorder is UNRESOLVED with only its reason.

Not claimed: that a fan can be carried between mounts, a mount reached or a
lift used; reachability of any state; which action caused a change between
two states; stability beyond the fixed-point check. No technology semantics,
search setting or engine code changed. No search ran; replay only rebuilt
prefixes of phobia's validated 54-action path.

## Validation

`t42-equipment-checks-2026-09-28.lisp`; log and reports in
`t42-equipment-run-2026-09-28.txt`. 166 phobia checks passed; 2 checks each
on corner, crelay, windtunnel, claustro and rumin; T40 (462) and T41 corner
(228) still pass.

- A1 MC: floor-gears a contract, zero UNCOVERED; fan1 WALL-HUNG on wgears1,
  compatible mounts fgears1 and wgears1; fgears1 VACANT, TURNING, NO FAN;
  wgears1's gated-arc count equals every arc naming it; fgears1 boarding at
  location10, lift to location11 (level 10), drop-back to location10 and
  exits. No problem name, LABELS or FLET in the T42 code.
- A2 prefix 11 -> 12 (jammer1 already jamming wgears1): fan1 WALL-HUNG ->
  HELD, wgears1 FAN MOUNTED STOPPED -> VACANT STOPPED; 18 -> 19 (jammer
  picked up): VACANT STOPPED -> TURNING NO FAN. Fixture (start, fan1 moved
  to the ground): wgears1 STREAM LOST while turning; engine
  STREAM-OBSTACLE-CLEAR for wgears1 false at the start, true in the fixture.
- A3 prefix 14: fan1 RESTING on the ground at location4, NOT STEPPABLE, no
  engine step; the unmounted fixture at location10 likewise.
- A4 prefix 52: fgears1 MOUNTABLE, wgears1 OUT OF REACH; reach limit -1:
  BEYOND VERTICAL REACH; OCCUPIED on the reason function with a second
  mounted fan in its fact list (phobia has one fan).
- A5 52 -> 53 STREAM GAINED, fan1 STEPPABLE, agent1 BOARDABLE with the engine
  step offered; 53 -> 54 agent1 lifted to location11, SUSTAINED by fgears1.
- A6 start: fgears1 TURNING NO FAN, nothing aloft.
- A7 fixtures from the final state, fgears1 jammed from location9 and fan1
  unmounted: each STREAM LOST, agent1 back at location10, aloft lost.
- A8 each UNRESOLVED reason (missing state, missing and empty provenance,
  inconsistent, unsettled state and before state, missing before
  provenance, recorder spliced) prints only its reason; caller and before
  states unchanged; engine agreement at every evaluated state. COMPILE-FILE:
  WARNINGS-P NIL with corner staged; with phobia staged the only warning is
  the pre-existing undefined BEAM-COORDINATES-CROSSING-RECORDS, identical
  for the pre-T42 file.

Profiles (SHA-256, before -> after, regenerated here; each "before" equals
the stored file):

| Problem | Before | After |
|---|---|---|
| claustro-topo | 88303A09…FFAE0E78 | identical |
| corner-topo | CA6EC36C…20320657 | identical |
| crelay-topo | FEC5986F…449C6828 | 263947F3…9FE6A9EE (step line) |
| phobia-topo | E5F1425C…9D78D892 | 22BFB877…B88A08B2 (MC only) |
| rumin-topo | D01D7EEE…97BF784F | B8C70B24…6D58629F (step line; no stored profile) |
| windtunnel-topo | 816FD8D9…6B56F39E | 82DBE9A2…B0D7BA04 (step line) |

Phobia's hunks all lie above the S0 header
(`t42-phobia-profile-diff-2026-09-28.txt`). No floor-blower or wall-blower
row changed anywhere.

Schema gaps: none open for fans or gears, so none reconciled.

## Reproduction

```lisp
(stage phobia-topo)
(load "tech/constraint-profile.lisp")
(load "doc/constraint-method/evidence/t42-equipment-checks-2026-09-28.lisp")
(t42-run-phobia-checks)                 ; T42 PHOBIA CHECKS PASSED: 166
(stage corner-topo)                     ; or crelay, windtunnel, claustro, rumin
(load "tech/constraint-profile.lisp")
(load "doc/constraint-method/evidence/t42-equipment-checks-2026-09-28.lisp")
(t42-run-absent-equipment-checks)       ; ... PASSED (CORNER-TOPO): 2
```

Ran on SBCL 2.2.9 in a scratch copy of the repository (Debian cl-alexandria,
cl-iterate, cl-lparallel). Temporary copies, transfer archive and compiled
files were removed.

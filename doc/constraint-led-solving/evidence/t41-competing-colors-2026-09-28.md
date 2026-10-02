# T41 — Competing colors and persistent connector links

Approved by D, 2026-09-28; completed 2026-09-28 in its own session.
Specification 8.11 was written and saved before code.

## Result and scope

MC's beam-relay entry is now a source-grounded contract (it was "extractors
S6 RC"). The contract states the engine's rules: lighting in propagation
layers from the transmitters; a relay settles in the first layer any clear
link reaches it; two hues in that layer leave it dark and it feeds nothing;
later hues are ignored; a connector at an already-lit location stays dark;
pairings persist while blocked or cut and only pickup clears them; capacity
counts outgoing pairings only; a receiver needs a relay of its hue whose own
link names it. Instance rows give capacity, the hue pools, start-state
links, and per RC station the transmitters visible from it by hue, marked
COMPETING HUES when two or more (corner: location1-3; rumin: four stations).
The competition rows reproduce RC's hop geometry exactly; they claim no
joint feasibility.

New optional `report-relay-lighting-scenario` / `relay-lighting-scenario-
result` (RL) evaluates one settled state in one view. It replays the
engine's layers through the engine's own link query and reports each relay
(LIT, CONFLICT, LOCATION ALREADY LIT, UNREACHED, ABSENT), each stored link in
each direction it can carry (sightline, cut, DELIVERED / IGNORED later /
SOURCE DARK / NOT CLEAR / ENDPOINT ABSENT), outgoing and incoming counts
(OVER CAPACITY flagged), and each receiver's feeders. Agreement with the
engine's lighting, stored colors and receiver status is computed and printed.
With `:chains` and `:phase` it adds T33's chain verdicts for the same state.
Unsettled or missing input, or a recording view without an open cycle, is
UNRESOLVED with only its reason.

Not claimed: reachability of any state, stability beyond the fixed-point
check, or composition of separately evaluated colors. No technology
semantics, search setting or engine code changed. No search ran; replay was
used only to rebuild reference states from validated traces and two short
corner variants (prefix 13, walk to location2, pickup / retaining pickup).

## Validation

`t41-lighting-checks-2026-09-28.lisp`; log and reports in
`t41-lighting-run-2026-09-28.txt`. 228 corner checks and 45 windtunnel
checks passed; the T40 suite (462) still passes.

- A1 corner MC: beam-relay a contract, zero UNCOVERED, capacity 3, no start
  links, 3 COMPETING HUES stations; every station's feeds equal RC's hops.
- A2 prefix 14: crossing7 active; transmitter2->connector1 sight CLEAR but
  CUT, NOT CLEAR; connector3 red in layer 1; connector1 red in layer 2 from
  connector3 (DELIVERED); receiver1 active; receiver3 WRONG HUE; connector2
  unreached with its feed cut. Prefix 15: connector1 blue in layer 1,
  connector3's red IGNORED at it; receiver1 dark, receiver3 active.
- A3 fixture (connector2 paired to both transmitters): CONFLICT in layer 1
  with both hues DELIVERED into it; connector3 fed only by it is UNREACHED
  (SOURCE DARK); receiver3 link DARK and inactive.
- A4 prefix 7 has connector1's pairings; prefix 8 (after pickup, gate1
  closed) has none, while connector2's pairing with transmitter1 persists
  with sightline BLOCKED. Variant pickup removes connector3's pairing that
  names connector1 (connector3 2/3); the retaining pickup keeps it
  (connector1 3/3, incoming 1) with connector1 ABSENT and the link ENDPOINT
  ABSENT, receiver1 dark.
- A5 prefix 13: connector1 at capacity 3 with 1 incoming; a four-pairing
  fixture is OVER CAPACITY.
- A6 T33 chain transmitter1 connector3 connector1 receiver1: CLEAR physically
  at prefix 14, BLOCKED at prefix 15 (connector1 not red); recording view
  UNRESOLVED in an ordinary problem.
- A7 windtunnel's validated 17-action final state (cycle open): physical
  lights connector1 -> repeater1 -> connector1* (receiver1 ACTIVE);
  recording has the live connector ABSENT and receiver1 dark; both agree
  with the engine; recording without an open cycle is UNRESOLVED.
- A8 at every evaluated state: lighting, color and receiver agreement; each
  link's engine clear reading equals its sightline and cut readings; no
  UNEXPLAINED outcome; caller and start states unchanged. Unsettled hand
  edit, missing/empty provenance, missing state, bad view and a recording
  view in a recorder-free problem are UNRESOLVED. COMPILE-FILE of the
  profile: WARNINGS-P NIL, FAILURE-P NIL (corner staged).

Profiles (SHA-256, before -> after, generated here):

| Problem | Before | After |
|---|---|---|
| claustro-topo | 88303A09…FFAE0E78 | identical |
| corner-topo | 6722C679…C9FC4B47 | CA6EC36C…20320657 |
| crelay-topo | 92BC7938…2F81F811 | FEC5986F…449C6828 |
| phobia-topo | 23A4FE37…BA43304D | E5F1425C…9D78D892 |
| rumin-topo | 0DD9CC9E…5FC69929 | D01D7EEE…97BF784F (no stored profile) |
| windtunnel-topo | F4995136…312FCF04 | 816FD8D9…6B56F39E |

Every before -> after diff lies above the S0 header, i.e. within MC
(`t41-corner-profile-diff-2026-09-28.txt`; hunks for all in the run file).
The stored crelay profile (1CB34F16…) predates T33 and lacked its four RC
scenario lines; the regenerated file includes them.

Schema gaps: none open for relay colors or pairing, so none reconciled.

## Reproduction

```lisp
(stage corner-topo)
(load "tech/constraint-profile.lisp")
(load "doc/constraint-method/evidence/t41-lighting-checks-2026-09-28.lisp")
(t41-run-corner-checks)          ; T41 CORNER CHECKS PASSED: 228
(stage windtunnel-topo)
(load "tech/constraint-profile.lisp")
(load "doc/constraint-method/evidence/t41-lighting-checks-2026-09-28.lisp")
(t41-run-windtunnel-checks)      ; T41 WINDTUNNEL CHECKS PASSED: 45
```

Ran on SBCL 2.2.9 in a scratch copy of the repository (Debian cl-alexandria,
cl-iterate, cl-lparallel). Reproduced by D on lumpy, 2026-09-28:
T41 CORNER CHECKS PASSED: 228. Temporary copies, transfer archive and compiled
files were removed.

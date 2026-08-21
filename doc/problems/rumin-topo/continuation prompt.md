# Continuation prompt — make the rumin-topo solution searchable

Paste the section below into a new session.

---

## Task

`probs/problem-rumin-topo.lisp` is solved: `doc/problems/rumin-topo/rumin-topo solution (90 steps).lisp`
validates end to end, goal `(and (has-location agent1 location16))`.
But it uses only **two** recorder cycles, 52 actions and 38 actions, and each
`(solve-subgoal ...)` chunk must be a whole cycle — so neither chunk is searchable. For
comparison, the original 14-step subgoal1 search took 450 s.

Redesign the solution to use **4–5 recorder cycles**, each doing a modest amount of work,
so every chunk is within reach of a real search. Then verify the chain by actually running
`(solve-subgoal ...)` for each and `(solve)` for the last, not just `validate-solution`.

Raise `(ww-set *max-recorder-cycles* ...)` in the problem file to match.

## Run the code, don't reason about it

Follow the repository's current agent instructions. From PowerShell in the project
directory, start `sbcl --dynamic-space-size 4096`, then load Wouldwork and enter its package
with `(progn (ql:quickload :wouldwork) (in-package :ww))`. Keep that session running while
performing the related checks. Several confident geometric claims made while designing
this solution turned out to be wrong; the ones that survived were measured.

Beyond `validate-solution`, `define-query` bodies are callable directly against a state
built by `validate-action-sequence`:
`(funcall (symbol-function 'beam-visible) state loc anchor apparatus elev)`, likewise
`placement-options`, `movement-results`, `pickup-clear`, `reachable`, `base`, `top`.

## How goal chaining actually behaves — this drives the whole design

`recorder-guided-cycle-goal` (`tech/-recorder-cycle-chaining.lisp`) rewrites every subgoal:

```lisp
`(and ,subgoal (recorder-cycles-used ,cycle-number) (ghost-stops-recorder))
```

- Subgoals therefore land **only on closed cycle boundaries**. A mid-cycle subgoal is
  impossible: `validate-recorder-cycle-orchestration` errors with "Guided recorder chaining
  requires a closed starting state" whenever `(recording-in-progress)` is present.
- `solve-recorder-final` strengthens the *problem* goal the same way, so the final cycle
  must close too. That is why the solution ends with the ghost returning to loc3 and
  stopping.
- `recorder-completed-cycle-made-progress-p` only requires the boundary state to change, so
  a cycle whose useful work is nearly all live pre-recording work is legitimate. This is
  what makes a 4–5 cycle split possible at all.
- Each guided call consumes exactly one cycle, and binds `*max-recorder-cycles*` to that
  cycle number for the duration.

## Proposed shape (unvalidated — this is the work)

Each chunk is [live pre-recording work] → `start-recorder` → [interleaved live + ghost] →
ghost stops at loc3. The subgoal is evaluated at the stop.

1. Ferry the agent and tray1 east. Ghost holds tray1* at loc2 with connector1 on it, which
   lights blue and opens gate1; agent crosses; ghost drops the tray and stops.
   Subgoal ≈ `(and (has-location tray1 location1) (has-location agent1 location1))`.
2. tray1 onto pplate1 → gate3 → box1 out of loc7 to loc8. Agent returns west.
   Subgoal ≈ `(has-location box1 location8)`.
3. Jump to loc9, tray1 onto pplate2 → gate4 → connector2 out of loc10 → connector2 to
   loc17; tray1 onto pplate3. Subgoal ≈ `(and (depressed pplate3) (has-location connector2
   location17))`.
4. Rebuild blue with live objects: box1 to loc2, connector1 on it paired to transmitter1 and
   connector2@loc17. Subgoal ≈ `(and (active receiver1) (on connector1 box1))`.
5. Final: hand blue to the ghosts, build the red chain, gate5, the loc15 ledge, loc16.

Cycles 2–4 each need their own ghost performance to reopen gate1 for whatever must move
west — that is the part most likely to break the shape, and the first thing to test.

## Measured facts — reuse, don't rederive

Anchor = a connector's `top` = its structural `base` plus its height (default 1).

- transmitter1 and transmitter2 are beam-visible **only** from loc2 and loc3, and only at
  anchor ≥ 2 (walls 10 and 12 are height 3/2).
- loc3 is a closet: wall2 blocks every outbound sightline except to loc2.
- receiver2 is visible from loc11 and loc12 at any anchor, loc7 at ≥ 4.5.
- receiver1 is visible from loc10 and loc17 at any anchor, loc13 at ≥ 2, loc1/loc3 at ≥ 4.5.
- Only one connector per location can be lit, so blue and red can never both source at loc2.
- Achievable anchors: ground elev+1, on a box elev+2, on a tray held by a standing agent
  elev+2.5. `support-use-allowed` permits only *live connector on ghost-held tray*.
- A ghost may pair only to fixed apparatus or another ghost; a live actor may pair a live
  connector **to** a ghost terminus. Live/ghost links must be created from the live side.
- `pickup-connector` wipes every pairing on the connector it lifts, both directions.
- `on` is bijective on the support: one occupant per support, and a forked ghost `on` fact
  displaces the live one.
- The ghost toggles a beam by walking its held tray out of a transmitter's window and back —
  reversible, and how gate1 is shut and reopened within one cycle.
- **East → west without gate1:** `loc11 →(GATE2)→ loc5 →ladder1→ loc13 → loc4 → loc2`,
  available whenever receiver1 is dark. This is what lets the agent return to the recorder
  to open another cycle.
- Ladders require empty hands and `traversal-via>` in `climbing` mode is directional, so nothing can be carried up
  to the loc15 ledge and there is no way back down to loc14.
- The final leg: on the loc15 ledge the agent is at elevation 2; tray1 on box1 at loc14 is
  at 1 (liftable, gap = `*vertical-reach-limit*`); box1 at 0 is not. Verified:
  `PICKUP-CLEAR tray1 from loc15 = T`, `PICKUP-CLEAR box1 from loc15 = NIL`.

Grounding gotchas for `validate-solution`: `$TERMINI` is printed in the action's own cons
order — connectors, then receivers, then transmitters, so `(CONNECTOR2 TRANSMITTER1)` and
`(RECEIVER2 TRANSMITTER2)` match and the reverse orders are rejected. `loc9 <-> loc10` is
`(STAIRS ... NIL ...)`, not `(... (GATE4) ...)`. Routes are the realized ones, so
`loc8 -> loc13` is three legs via loc2 and the loc2→loc4 stairs. The ladder segment is
`(LADDER LOCATION14 (LADDER2) LOCATION15)` and plate mounts are
`(STEP (LOCATION15 GROUND) NIL (LOCATION15 PPLATE4))`.

## Repo state

Already committed and passing `(test-talos)` — 93 problems, 0 failures, 25 mutation cases,
0 surviving mutants:

- `tech/-recorder-fork-registry.lisp` (new) plus changes to `tech/-recorder-session.lisp`,
  `tech/beam-relay.lisp`, `tech/jammer.lisp`, `tech/-gears-fan.lisp`, `src/ww-settings.lisp`,
  `src/ww-installer.lisp`, `src/ww-initialize.lisp`, `src/ww-preliminaries.lisp` — the
  START-RECORDER ghost fork now defers installation to `init` and collects fork clauses from
  each relation's owner, so `(include-tech ...)` order no longer matters. Documented as
  Trap 10 in `doc/load-ordering/ordering-of-operations.md`.
- `tech/reachability.lisp` — directional `reach-via>`.
- `probs/problem-rumin-topo.lisp` — `location17` at (18 5); `(has-elevation location15 2)`,
  `(has-elevation location16 2)`, `(has-height edge5 2)`; `(reach-via> location15 ()
  location14)`; `(ww-set *max-recorder-cycles* 2)`.

## Working agreements

Locations are the user's guesses and may be proposed freely — objects, wall/gate geometry,
apparatus coordinates, controller wiring, chromas, elevations and heights are given. See
the repository's current agent instructions for the full modeling and workflow rules.
Explain the purpose before editing and run the appropriate REPL checks afterwards.

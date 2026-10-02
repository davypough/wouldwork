# T45 — Dependencies across recorder boundaries and support changes

Approved by D, 2026-09-28; completed 2026-09-28 in its own session.
Specification 6.3 was written and saved to D's folder before code.

## Result and scope

New optional diagnostic **BT**, `tech/constraint-boundary.lisp`, loaded after
`constraint-profile.lisp` and `constraint-arrangement.lisp`:
`boundary-transition-result` returns and `report-boundary-transition` prints
what one event does to one supplied settled state. The event is
`(:stop <ghost agent>)`, `(:cancel <live agent>)`, or `(:action <form>)` for one
engine action that changes a support (ON, a held tray, a fan mount) or the
recorder session; a pure move or toggle without a support change is
UNRESOLVED (SW or T34 cover those).

- **Prerequisites apart from effects.** STOP: agent a mapped ghost, a cycle
  recording, every ghost agent at a recorder and empty-handed, no live/ghost
  HOLDING or ON (each listed). CANCEL: a live agent at a recorder,
  empty-handed. Each agent's RECORDING-AGENT-CAN-CLOSE is information only.
  The itemized verdict is checked against the engine's applicability.
- **Effects.** STOP and CANCEL are always closed on a private copy with the
  session facts the action asserts and the engine's own
  CLOSE-RECORDER-CYCLE-STATE!. With prerequisites met, the engine successor
  must equal that closure (ENGINE); otherwise the effect is HYPOTHETICAL. An
  action has effects only when it applies. Successors must be consistent, a
  fixed point on a second pass and valid under T34's structural rules.
- **Consequences.** Support facts and each object's support chain with engine
  BASE (RETAINED, CHANGED, REMOVED); plates by occupant layer with physical
  and recording depression; pairings and receivers in both views; every other
  changed fact by relation, and S1 primitives with the devices they drive;
  ghost objects in removed support/pairing facts; each agent's route
  conditions in its own view (passage, arcs, mobility); obligations
  (`:fact`, a T44 role, or `:reach`) with phases `:until-event`, `:across`,
  `:after` giving EXPENDED/KEPT, SURVIVES/LOST, MET/NOT MET.

Not claimed: reachability of the state or the event, a complete plan, global
necessity, or availability of a HYPOTHETICAL closure. No recorder,
support-loss or engine semantics changed; no search ran. SW (T43) still
declines recorder problems; BT is the recorder-view transition check for
boundary and support events.

## Validation

`t45-boundary-checks-2026-09-28.lisp`; log `t45-boundary-run-2026-09-28.txt`;
generated reports `t45-boundary-reports-2026-09-28.txt`. 246 checks passed:
rumin 165, windtunnel 39, crelay 21, 7 each on claustro, corner and phobia.

- A1 rumin's historical 91-action trace (`doc/problems/rumin-topo/rumin-topo
  solution (91 steps).lisp`) fails under current semantics at action 7:
  CONNECT-CONNECTOR and PUT-CONNECTOR phrases use the old argument order
  (location before place). Normalizing only that order, all 91 actions replay
  to the goal. The replay is reference evidence, not a newly found solution;
  the historical file is unchanged.
- A2 rumin after action 90, STOP by agent1*: MET, ENGINE, closure agrees,
  successor equals the replayed action 91. Gate5's open state is lost (plate3
  loses ghost tray1*, receiver2 goes dark as the red chain's ghost pairings
  go); gate6 stays open on plate4, held by live tray1 before and after.
  Obligations: gate5 open EXPENDED, gate6 open SURVIVES, tray1 weight plate4
  SURVIVES, agent1 at location16 MET. agent1* REMOVED; agent1 keeps
  location16 and loses only its arcs through gate5.
- A3 the same state, CANCEL by agent1: NOT MET (not at a recorder), engine
  agrees; HYPOTHETICAL closure differs from A2 only by the stopped-by-ghost
  flag, with the same physical losses.
- A4 rumin after action 49, PUT-TRAY by agent1* at location2: live connector1,
  riding ghost-held tray1*, lands on live box1 (chain on tray1* / held agent1*
  -> on box1, base 3/2 -> 1); pairings kept; tray1 on plate3 RETAINED;
  successor equals the replayed action 50.
- A5 the same state: STOP NOT MET with agent1* away from the recorder,
  holding tray1*, and `(on connector1 tray1*)` across layers; CANCEL NOT MET;
  its HYPOTHETICAL closure leaves connector1 on the ground at location2, no
  catch, as the settling policy states.
- A6 windtunnel, validated 17-action state: STOP and CANCEL NOT MET; before
  closure receiver1 is physically active and not recording-active; the
  hypothetical closure removes connector1*, receiver1's physical ACTIVE and
  gate2's OPEN, and agent1 loses gate2. T34's STABLE-AND-BEAM-WORKING mixed
  arrangement, closed the same way, loses receiver1 too. agent1 stepping onto
  plate1 (from the 8-action state) is a support change that toggles plate1,
  gate1 and blower1 in the physical view only; the ghost's route conditions
  are unchanged.
- A7 engine agreement at rumin's STOP-RECORDER actions 52 and 91 and at
  crelay's CANCEL-PLAYBACK actions 11 and 31 (T10 final checkpoint, 87
  actions): prerequisites, closure, engine successor and replay agree. A
  CANCEL given as a display phrase is read as a CANCEL.
- A8 UNRESOLVED reasons print only their reason: no scenario, no provenance,
  an unsettled or inconsistent state, a malformed event, an unknown action, a
  STOP naming no agent, no open cycle, bad `:agents`, malformed obligations, a
  move without support change, an inapplicable action (effects unresolved,
  its refusal still reported), and `:stop` without the recorder (claustro,
  corner, phobia). Caller state, scenario and static database are unchanged
  in every check. No problem name, LABELS or FLET in `constraint-boundary.lisp`;
  COMPILE-FILE WARNINGS-P NIL. T33 (61) and T34 (155) checks pass. The
  windtunnel and crelay profiles regenerate byte-identical to the stored files
  (`8C64BED2…8FB7B085`, `038FC689…6DC56700`).

Observation outside T45: staging uninterns GOAL-FN (RESET-USER-SYMS), so a
file that quotes `'goal-fn` and is loaded before `(stage ...)` names the
previous problem's goal. The T45 check file looks the symbol up at run time;
profiles must be loaded after staging, as the Handoffs already say.

Files (SHA-256): `tech/constraint-boundary.lisp`
`91232AA61930F8CFF5B723C7742199E30B351CE89750B64188537BF8DFA987FC`;
`t45-boundary-checks-2026-09-28.lisp`
`DA90762E4E6219A54008D9C8FA44D0B1FB0BA78BCEE53AB3534986AA9637EF9D`. Schema gaps: G19 and G15 status note.

## Reproduction

One fresh image per problem (after `(progn (ql:quickload :wouldwork) (in-package :ww))`):

```lisp
(stage rumin-topo)                     ; or windtunnel-topo, crelay-topo, claustro-topo
(load (merge-pathnames "tech/constraint-profile.lisp" (asdf:system-source-directory :wouldwork)))
(load (merge-pathnames "tech/constraint-arrangement.lisp" (asdf:system-source-directory :wouldwork)))
(load (merge-pathnames "tech/constraint-boundary.lisp" (asdf:system-source-directory :wouldwork)))
(load (merge-pathnames "doc/constraint-method/evidence/t33-view-checks-2026-09-27.lisp" (asdf:system-source-directory :wouldwork)))
(load (merge-pathnames "doc/constraint-method/evidence/t34-arrangement-checks-2026-09-27.lisp" (asdf:system-source-directory :wouldwork)))
(load (merge-pathnames "doc/constraint-method/evidence/t45-boundary-checks-2026-09-28.lisp" (asdf:system-source-directory :wouldwork)))
(t45-run-rumin-checks)                 ; T45 RUMIN CHECKS PASSED: 165
                                       ; windtunnel: (t45-run-windtunnel-checks) 39
                                       ; crelay: (t45-run-crelay-checks) 21
                                       ; others: (t45-run-general-checks) 7
```

Ran on SBCL 2.2.9 (Debian cl-alexandria, cl-iterate, cl-lparallel) in a scratch
copy of the repository. The transfer archive was moved to `_to_delete/`.

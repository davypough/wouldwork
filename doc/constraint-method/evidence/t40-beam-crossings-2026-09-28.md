# T40 — Beam crossings and cut order

Approved by D, 2026-09-28; completed 2026-09-28 in its own session.
Specification 8.10 was written and saved before code; one implementation
clarification (orientation of a beam live both ways) is dated beneath it.

## Result and scope

MC now carries a source-grounded beam-crossing contract; corner-topo reports
zero UNCOVERED technologies. Its instance rows read the engine's own crossing
data: the published pool (37 crossings on corner-topo), each stored directed
beam's crossings in order from its source, each gate's split of that order
as `|gate|`, the count of potential beams with no crossing, and a crossing
index with both beams, the position on each and the point. Points are
recomputed only with the engine's BEAM-COORDINATES-CROSSING-RECORDS against
the published pool. They agree with the hand geometry in the corner Briefing
(for example crossing7 at (9.75,2.08), crossing8 at (9.27,5.91), crossing12
at (8.49,4.06)), which is now confirmed from engine data.

Evaluated cuts need an explicit state: REPORT-BEAM-CROSSING-SCENARIO and
BEAM-CROSSING-SCENARIO-RESULT take `(:state <problem-state> :provenance
"<text>")`. The state must be a propagation fixed point (a private copy is
propagated and compared); otherwise, or with any missing input, the result is
UNRESOLVED with its reason. Gates are read from the state, not forced. Each
beam live for cutting is reported once, in the engine's evaluated
orientation, with each crossing labelled ACTIVE; REACHED, INACTIVE (other
beam NOT LIVE, or NOT REACHING with its reason); BEYOND CUT at an earlier
active crossing; or BEYOND CLOSED gate. Non-reached labels are printed only
where the engine's BEAM-REACHES-CROSSING agrees; anything the source rules
do not explain would print UNEXPLAINED. The engine fixed point of the stored
active set is also reported.

Not claimed: joint feasibility of the beams in a static row, reachability of
any supplied state, stability beyond the fixed-point check, composition of
separately evaluated beams, or any height filtering of crossings (the engine
has none). No technology semantics, search setting or engine code changed.

## Validation

462 focused checks passed (`t40-crossing-checks-2026-09-28.lisp`,
`t40-crossing-run-2026-09-28.txt`):

- A1 static: every stored beam printed in stored order; transmitter1->location4,
  transmitter2->location2, location2->receiver1, location3->receiver1,
  transmitter1->location1 and both location1/location4 directions checked
  crossing by crossing. gate1 follows all crossings of transmitter1->location4
  and transmitter2->location4, precedes all of location4->location1 and
  follows all of location1->location4. Every crossing lies on exactly two
  canonical beams, and the engine records' parameters strictly increase along
  every stored order.
- A2 inactive beams: the start state has no live beam or active crossing;
  trace prefix 2 has only transmitter1->location1 live, its three crossings
  REACHED, INACTIVE with the other beam NOT LIVE.
- A3 shielding, on the validated corner trace (reference evidence, replayed
  from `doc/problems/corner-topo/constraint-evidence/full-path/`): prefix 14
  has crossing7 as the only active crossing; transmitter1->location4 and
  transmitter2->location2 are cut; transmitter1->location4's crossings 15,
  14, 13, 12 and 11 are BEYOND CUT at crossing7; crossing12 is reached by
  location2->receiver1 but inactive because the other beam is NOT REACHING;
  receiver1 is active and gate1 open. Prefix 15 has no active crossing and
  transmitter2->location2 uncut.
- A4 gate, on supplied fixtures settled by the engine's propagation: with
  gate1 closed, transmitter1->location1 x transmitter2->location4 (crossing3)
  is active on the source side of the gate and cuts the later crossings; in a
  second fixture location4->location1 is live, crossings 30 and 36 are BEYOND
  CLOSED gate1, crossing36 is reached by location3->receiver1 and inactive
  for that reason, and location3<->location4, live both ways, is reported
  once from location3.
- Engine agreement at every evaluated state: reported beams are exactly the
  engine-live ones in its trial orientation; BEAM-CUT, reaching, ACTIVE and
  the stored CROSSING-ACTIVE set agree; no label is UNEXPLAINED; the stored
  set is an engine fixed point.
- A5: unsettled hand edit, missing or empty provenance, and a missing state
  are UNRESOLVED and print only the reason. Caller states and the start state
  are unchanged after every evaluation.
- A6: COMPILE-FILE of the profile returned WARNINGS-P NIL, FAILURE-P NIL.
  Full profiles before/after (SHA-256):

| Problem | Before | After |
|---|---|---|
| crelay-topo | 92BC79385B9964C185E0F330B664F29B53192F67FB46E9F0C4541B4E2F81F811 | identical |
| windtunnel-topo | F499513674CF4F1E56F0EB0E595DD9E954F1ED8FE28ED0371505F498312FCF04 | identical |
| claustro-topo | 88303A097D3069ED6FF5A70096C96EA6F2DDD710DB9C9A01E0ACB89CFFAE0E78 | identical |
| phobia-topo | 23A4FE3704EE651A32E65AA867A0E0813344A31FFAB784B1CE84C7FABA43304D | identical |
| rumin-topo | 0DD9CC9EE71553A8FFB7C92616695E0922801040622E14B0BC7696435FC69929 | identical |
| corner-topo | FEF019A61002A72E959F0E171F55CA7AD72762139914D8A91BEC56459AA031D3 | 6722C679DD7D01C914F4BABB519E8DC0D70315ACC2B0B2B584143958C9FC4B47 |

The corner-topo "before" profile generated here is byte-identical to the
stored one of 2026-09-27, and the crelay, windtunnel and claustro hashes equal
T35's, so this environment reproduces D's. corner-topo's diff lies wholly in
MC (`t40-corner-profile-diff-2026-09-28.txt`). No problem object names were
added to diagnostic code (C3). No search ran; replay was used only to rebuild
reference states from the validated trace.

Schema gaps: none was open for crossings (corner-topo handled the uncovered
mechanic with a hand contract), so none needed reconciling.

## Reproduction

Separate SBCL process, WOULDWORK_INSTANCE=t40, 4096 MB dynamic space. The
checks here ran on SBCL 2.2.9 in a scratch copy of the repository, with
Wouldwork's dependencies copied from D's quicklisp dists.

```lisp
(stage corner-topo)
(load "tech/constraint-profile.lisp")
(load "doc/constraint-method/evidence/t40-crossing-checks-2026-09-28.lisp")
(t40-run-checks)                 ; T40 CHECKS PASSED: 462
(write-static-constraint-profile "doc/problems/corner-topo/Constraint-Static-Profile.txt")
```

Reproduced by D on lumpy (SBCL 2.6.8), 2026-09-28: T40 CHECKS PASSED: 462.

## Artifacts

- `t40-crossing-checks-2026-09-28.lisp`: focused reproducible checks.
- `t40-crossing-run-2026-09-28.txt`: compilation result and check log.
- `t40-corner-profile-before-2026-09-28.txt`, `t40-corner-profile-diff-2026-09-28.txt`.

Temporary archives, scratch copies and compiled files were removed.

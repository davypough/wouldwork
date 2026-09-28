# claustro-topo — Stage Plan
Approved by D: 2026-09-27, retrospectively (the stages were agreed one at a time in the
interview).  Max search depth: 8.  Plan data: constraint-evidence/full-solution-candidate.lisp.

## Stages

| # | Intent | Premises | Check | Realization | Actions |
|---|---|---|---|---|---|
| 1 | Get jammer1 to the location7 side with gate4 open | D: box1 at location2 cuts the beam and reopens gate4 | sightline and reach checks (Briefing) | search, min-length, cutoff 8; checkpoint cp1 | 1-8 |
| 2 | Recover jammer2: jam gate5 from location8 | D: gate5 visible from location8 | static check T | hand, validated | 9-12 |
| 3 | Park jammer2 on plate3 holding gate5; fetch jammer1; jam gate1 from location7 through the window | D: placing at location1 from location7 is not disallowed for gate1 | static checks T | hand | 13-18 |
| 4 | Ladder to location1, clear location2 so the beam lights, box1 onto plate1 | A | — | hand | 19-24 |
| 5 | Handover: jammer2 jams gate1 from plate3; jammer1 onto plate2 jamming gate5 | A, from the agreed end arrangement | plate sightlines to gate1 need the beam lit (check) | hand | 25-31 |
| 6 | Exit: gate5/6/7, onto box2, jump to location12, cross gate8/9, stairs | agreed penultimate state | — | hand | 32-36 |

All 36 actions validated together from the staged start (VALIDATE-ACTION-SEQUENCE,
SUCCESS-P T, goal satisfied), 2026-09-27.  No ledger was kept: the full-path validation is
the record.

## Check results

No conflicts.  The one open link (how to reach location7 while the beam is lit) was
resolved by D's box-at-location2 idea before any search.

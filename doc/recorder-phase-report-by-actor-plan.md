# Plan: reorganize the recorder phase report by actor

Use this file as the opening prompt for a new session. It is self-contained.

## Objective

Change the recorder cycle report so each phase has exactly one actor:

- **Recording phase** — AGENT1\* only (the recorded ghost sequence), headed by the live
  agent's `START-RECORDER`, closed by the ending. `(PAUSE)` marks a pause point.
- **Playback phase** — AGENT1 only (the live agent acting during playback). `(RESUME)`
  marks where the agent releases the ghost. No `(PAUSE)`.

`(PAUSE)` and `(RESUME)` then stand one-to-one, which is the domain semantics:

> Rule 6, `tech/Recorder-ghost operations in the Talos Principle.txt`: "During the recording
> phase, the agent can create pause points, which will pause ghost action during the
> subsequent playback phase, until the agent resumes the playback."

This is a **reporting change only**. `(PAUSE)`/`(RESUME)` stay report markers, not planner
actions — they are fully determined by the interleaving the search already chooses, a
pause point is created in a phase the planner deliberately does not search (rule 9 discards
the recording phase's physical effects, so only the action sequence is modelled), and making
them actions would inflate the action-denominated cost model and require special-casing in
`prune-recorder-interleaving-successor-p` and `recorder-recording-path`.

## Current behavior

All in `tech/-recorder-solution.lisp`:

- `recorder-cycle-recording-sequence` — start, ghost moves, one `(pause)` per contiguous
  live block, ending. `previous-side` starts `NIL`, so a live block opening the window also
  emits one.
- `recorder-cycle-playback-sequence` — **both** actors' in-window moves, `(pause)` entering
  a live block, `(resume)` leaving one. Excludes `START-RECORDER` and a normal
  `STOP-RECORDER`; includes a live `CANCEL-PLAYBACK`.
- `print-recorder-cycle-report` — prints Setup phase, Recording phase, Playback phase,
  `Closure:`, then cycle metrics.

## Target behavior

1. **Recording phase** — unchanged except the pause-point test. Emit `(pause)` for a live
   block **iff at least one ghost move follows it in the recording sequence**, counting the
   synthesized return moves from `recorder-return-moves`. A live block opening the window
   still earns one (ghost action follows it), so no change there.
2. **Playback phase** — live moves only. Emit `(resume)` after a live block iff that block
   earned a pause point. Emit no `(pause)`. A live `CANCEL-PLAYBACK` stays as the final
   playback entry.
3. `print-recorder-cycle-report` — no structural change; consider naming the actor in each
   heading (see Open question 2).

Implementation note: both sequences now need the same predicate. Extract one helper that
returns the cycle's runs as `(side . moves)` and whether ghost action follows each run, and
build both sequences from it, rather than duplicating the lookahead. Per project style, make
it a separate top-level function — no `labels`/`flet`.

## Verification: applies to every existing cycle

| cycle | window | runs (L = AGENT1, G = AGENT1\*) | pause pts | resumes |
|---|---|---|---:|---:|
| rumin 1 | 2-24 | L[2-4] G[5-6] L[7-10] G[11] L[12-20] G[21] L[22] G[23-24] | 4 | 4 |
| rumin 2 | 30-51 | G[30-34] L[35-48] G[49-50] | 1 | 1 |
| rumin 3 | 56-60 | L[56-60] | 0 | 0 |
| rumin 4 | 63-90 | L[63-78] G[79-81] L[82-89] G[90] | 2 | 2 |
| windtunnel 1 | 8-17 | G[8-10] L[11] G[12-16] L[17] + synthesized return | 2 | 2 |

Only rumin cycle 3 loses a marker. Cycle 2 confirms a ghost-opened window correctly takes no
leading pause point (already true today).

## Two edge cases that must not be got wrong

**1. Synthesized returns count as ghost action.** Windtunnel's cycle closes `:synthesized`,
so `recorder-return-moves` appends a ghost return before the synthesized `(stop-recorder)`.
Its trailing live block (step 17) therefore *does* have ghost action after it and keeps its
pause point. A naive "drop the last live block's pause" would silently break windtunnel.
Test the predicate against the assembled recording tail, not against `cycle.moves`.

**2. Does a bare `STOP-RECORDER` count as remaining ghost action?** This is the only real
judgment call, and it decides rumin cycle 3.

- *Recommended — no.* Treat a normal stop as a boundary, consistent with
  `recorder-cycle-playback-sequence` already excluding it. Cycle 3 then shows a recording of
  `(55 START-RECORDER) (61 STOP-RECORDER)` with nothing between and a playback of five live
  actions with no markers — the honest rendering of a cycle that records nothing, and the
  reading already adopted for this solution ("cycle 3 is a pure action sequence").
- *Alternative — yes.* The stop is the recorded final act under rule 8, so pausing before it
  is meaningful. Cycle 3 then keeps one pause point and gains one resume.

Pick one and state it in the docstring; the tests below assume the recommendation.

## Implementation steps

1. Edit `tech/-recorder-solution.lisp` only. **Do not edit `src/problem.lisp`** — it is the
   staging artifact (gitignored, regenerated by `(stage <problem>)`), and its lines ~4459-4688
   are a spliced copy of this file.
2. Add the runs/lookahead helper as a top-level function, high-level-first per project style.
3. Rewrite `recorder-cycle-recording-sequence` to use it for the pause-point test.
4. Rewrite `recorder-cycle-playback-sequence` to emit live moves plus trailing `(resume)`,
   preserving the `CANCEL-PLAYBACK` tail. Update its docstring — the current one says
   "pausing live blocks and resuming ghost blocks," which stops being true.
5. Update the three test claims below.
6. Regenerate the two documentation files.

## Test claims to update

`test/problem-recorder-report-test.lisp`:

- Main report claim (~line 155). `:recording` is **unchanged** (all three live blocks are
  followed by ghost action, the last by the synthesized return). `:playback` becomes
  `(list (third path) '(resume) (fifth path) '(resume) (seventh path) '(resume))`,
  replacing the current alternating pause/resume list.
- `recorder-report-legacy-no-boundary-contract` (~line 215). `:recording` becomes
  `'((start-recorder) (stop-recorder))`; `:playback` becomes `(list (first path))`.

`test/problem-recorder-cancel-test.lisp` (~line 169):

- `:recording` becomes `'((1.0 (start-recorder live-agent)))`; `:playback` becomes
  `'((2.0 (mark-cancel-progress live-agent)) (3.0 (cancel-playback live-agent)))`.

Also grep `test/` for other recorder report assertions before running the suite; these three
are the ones that name `pause`/`resume` explicitly.

## Documentation to regenerate

- `doc/problems/rumin-topo/rumin-topo phases (96 steps, 4 cycles).txt` — currently generated
  under the old convention and its header claims to be "the decomposition wouldwork itself
  reports." Regenerate and rewrite that header sentence.
- `doc/problems/windtunnel-topo/Wind Tunnel Solution.txt` — a captured REPL transcript in an
  older format (no Setup phase; `START-RECORDER` shown inside the playback listing). The real
  end-to-end check is to re-run the search with the patched code and recapture it. Budget for
  it: the recorded run took 699 s.

## REPL verification

```lisp
(progn (ql:quickload :wouldwork) (in-package :ww))
(stage rumin-topo)
```

Then, for each cycle, confirm the marker counts in the table above hold, and that
`(length (remove-if-not (lambda (e) (equal e '(pause))) recording))` equals
`(length (remove-if-not (lambda (e) (equal e '(resume))) playback))` for every cycle in
`(build-recorder-report ...)`. That equality is the invariant this change establishes and is
the cheapest regression check to keep.

## Open questions for the implementer

1. **Does `(RESUME)` need to name what it releases?** A live-only playback listing no longer
   shows the ghost moves, so the interleaving is recoverable only by cross-referencing step
   numbers against the recording listing. Recommended: keep the engine marker bare
   `(resume)` (minimal change, markers stay simple forms) and let the documentation generator
   add a `;; AGENT1* runs recorded steps N-M` annotation. The alternative is to extend the
   marker to `(resume <first> <last>)`, which changes the report data shape and all three
   tests again.
2. **Should the phase headings name the actor?** e.g. "Recording phase (AGENT1\*)" /
   "Playback phase (AGENT1)". Cheap and clarifying, but `print-recorder-report-sequence`
   takes a plain heading string, and the actor names are problem-specific — it would have to
   read them off `recorder-recording-agents` rather than hardcode them.
3. **`:setup`/`:recording`/`:playback` are consumed elsewhere.** `build-recorder-report`
   notes these top-level aliases are "used by guided chaining." Confirm no goal-chaining code
   depends on the playback sequence containing ghost moves before shipping.

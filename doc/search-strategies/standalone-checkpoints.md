# Standalone searches from saved checkpoints

`solve-subgoal` accepts a search checkpoint as its first argument. The checkpoint
contains the actual endpoint, the original goal, and every accepted action segment.
It supports serial or parallel searches without automatic predecessor backtracking.

```lisp
(stage my-problem)
(ww-set *threads* 16)
(defparameter *checkpoint* (capture-search-checkpoint))
(ww-set *depth-cutoff* 8)
(setf *checkpoint* (solve-subgoal *checkpoint* (my-milestone)))
(export-search-checkpoint *checkpoint* "my-progress.txt")
```

Each successful search returns a new checkpoint. An exhausted search returns the
same checkpoint and NIL as its second value. It never increases the cutoff or
searches an earlier milestone. Retain a previous checkpoint variable or archive to
branch from an earlier endpoint. `ww-undo` restores planner globals but does not
undo a user's assignment to `*checkpoint*`.

The first argument is a checkpoint object, not a goal formula. Passing
`(search-checkpoint-state *checkpoint*)` instead uses the older raw-state API and
does not append the result to the checkpoint's history.

## Restart without searching the prefix

```lisp
(stage my-problem)
(ww-set *threads* 16)
(defparameter *checkpoint* (import-search-checkpoint "my-progress.txt"))
```

Set threads before import: crossing the serial/parallel boundary rebuilds the
staging and invalidates old in-memory checkpoints. Import replays saved actions,
checks each milestone and exact symbolic endpoint, and returns a fresh checkpoint.
It checks dynamic facts, happening facts, event cursors, time, value, and the
technology's policy context. It restores the calling planner session even if replay
fails. Old result buffers can survive staging; the archive's original goal and
origin state must still match the current staging exactly.

`export-subgoal-progress` archives from an existing serial chain can be imported
directly. In a still-running old image, export **before** loading updated engine
files or restarting. `capture-search-checkpoint` also captures a live chain once
the new API is loaded. Search-found segments are saved without routine replay;
restoration necessarily replays them to reconstruct the endpoint.

## Final acceptance

Search for the original final goal as another explicit two-argument subgoal, then:

```lisp
(validate-search-checkpoint *checkpoint*)
```

This calls `validate-action-sequence` on the complete accumulated path from its
original state. Require `SUCCESS-P`, `GOAL-CHECKED-P`, and `GOAL-SATISFIED-P` all T.
A replayed endpoint differing from the saved endpoint signals an error. This
validation preserves the current planner session; it does not publish a cumulative
solution into `*solution-paths*`. That variable holds the latest standalone segment.
An ordinary `(solve)` does not finish a checkpoint object automatically.

Changing thread count can change which endpoint a search finds. A prior exhaustion
remains a bound relative to its own start and settings. Faster search does not by
itself justify a particular larger depth cutoff.

Regression coverage: `test/search-checkpoint-checks.lisp` exercises serial-to-parallel
migration, immutable prior checkpoints, exhaustion, failed-import rollback, open
recorder-cycle persistence, and complete final replay, using synthetic problems.

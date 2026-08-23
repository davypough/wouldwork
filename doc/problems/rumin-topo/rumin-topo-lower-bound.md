# Which lever: `heuristic?`, `min-steps-remaining?`, or something else

Your instinct is right, and the reason is in how the engine consumes each of them.

## The three levers do different jobs

**`heuristic?`** is only ever used for **ordering**. `ww-planner.lisp` sets
`problem-state.heuristic` on each child and nothing else touches it in serial depth-first
search. Ordering is exactly the lever that needs a gradient — it pays off when "closer to
the goal" is a meaningful thing to say about a state. You are right that rumin-topo does not
offer that: the state barely improves for twenty actions and then three actions decide
everything. **Skip it.**

**`min-steps-remaining?`** is a **lower bound used for pruning**, not for ordering.
`ww-searcher.lisp` uses it as:

```lisp
(when (or (and (> *depth-cutoff* 0) (> (+ depth lb) *depth-cutoff*))
          (and *solution-paths* (member *solution-type* '(min-length first))
               (>= (+ depth lb) (solution.depth (first *solution-paths*)))))
  ... (return-from df-bnb1 nil))
```

It needs no gradient at all. It needs one thing: a count of remaining work that is
**unavoidable**. And it is unusually well suited to this chain, because each chunk runs with
`*depth-cutoff*` set to a length we already know is achievable — so there is little slack,
and any honest count of forced remaining actions bites almost immediately.

**`prune-state?`** is the third lever, and worth keeping in reserve. `ww-planner.lisp`
refuses to expand a node when it returns true. It is not bound by admissibility in the
arithmetic sense, only by soundness: it may encode "no solution lies below this state" from
domain reasoning that a numeric bound cannot express.

**Recommendation: `min-steps-remaining?` first, `prune-state?` if it stalls, `heuristic?` not
at all.**

## Measured, on chunk 4

Same start state, same settings, `first`, graph, cutoff 10, symmetry off, prefix pruning on:

| | states | wall clock | result |
|---|---|---|---|
| no bound | 2,200,000+ | 32 min | **no solution**, still running |
| with the bound below | **6,472** | **3 s** | solved at depth 10, cycle committed |

`min-steps-remaining? pruned 2,943 nodes, 45.5% of total states.` Roughly a 340x reduction
in states explored, and the difference between "does not finish" and "finishes instantly".

It also found a *better-shaped* solution than my hand-built one — it closes the cycle at
action 8 and does the plate3 work afterwards, rather than inside the cycle. Same length,
and it sidesteps the ghost/plate displacement trap by a different route.

## The bound, and why each term is safe

```lisp
(define-query rt4-blue-cost ()
  (if (active receiver1)
    0
    (if (rt-some-agent-holds-connector) 1 2)))

(define-query rt4-plate-cost ()
  (if (depressed plate3)
    0
    (if (rt-some-agent-holds-weight) 1 2)))

(define-query rt4-session-cost ()
  (do (assign $cycles (recorder-cycle-count))
      (if (< $cycles 4) 2 (if (recording-in-progress) 1 0))))

(define-query rt4-move-cost ()
  (do (bind (has-location agent1 $agent-location))
      (if (eql $agent-location 'location3) 0 1)))
```

Summed, these give 7 at the chunk-4 start state against a true remaining cost of 10.

The soundness argument follows the one in `problem-corner.lisp`: each term counts actions of
a **disjoint kind** — manipulation of the connector, manipulation of the weight, session
actions, agent movement — so they add without double counting. Two terms needed a
problem-specific check before I trusted them:

* *blue*: lighting `receiver1` from dark could in principle happen by opening a gate that
  was occluding the beam, rather than by touching a connector. It cannot here — the only
  plate-driven gates are gate3/4/5/6, all east, and the `loc2 -> loc17 -> receiver1` chain
  never reaches them. So a CONNECT really is unavoidable, and a PICKUP too unless someone
  already holds a connector.
* *plate*: a box depresses a plate as well as a tray, and a **ghost** can do the depressing,
  so the "holds a weight" test scans every agent and both types. Missing that would have
  made the bound inadmissible the moment a ghost picked something up.

Verified admissible by replaying the known 10-action plan and checking `lb <= true
remaining` at every depth — it holds at all eleven, with equality from depth 7 on.

## Why chunk 3 did not fall to the same trick, and what would fix it

The number that matters is not the bound but **where pruning starts, at `cutoff - lb`**:

* chunk 4: cutoff 10, bound 7 at the start -> pruning from depth 3 -> 6,472 states.
* chunk 3: cutoff 15, bound 6 at the start -> pruning from depth 9 -> 500,000 states in
  5 minutes and still going, with the bound killing 36% of nodes but far too late.

My chunk-3 bound only counts what box1 costs (pickup, put, and the two moves to fetch it),
which is 6 of the true 15. The missing 9 are the **bootstrap**: while gate1 is shut and box1
is east of it, the agent cannot reach box1 at all until receiver1 is lit, and lighting it
requires the ghost to take tray1\* to loc2 and the live agent to pick up and place a
connector on it. Those are forced actions and can be counted:

* ghost PICKUP-TRAY, and a ghost MOVE if the tray is not already where the ghost stands
* live PICKUP-CONNECTOR and CONNECT-CONNECTOR
* a live MOVE to the elevated loc4, since the tray top at 5/2 is out of reach from loc2's floor

That is four to six more, taking the bound from 6 to about 10-11 of 15 and moving the
pruning frontier from depth 9 to depth 4 or 5 — the same regime that made chunk 4 collapse.
The care needed is that the extra terms must not double-count the moves the box term already
charges; keeping them on disjoint action kinds (ghost actions vs live manipulation vs live
movement) is what keeps the sum honest.

Chunks 1, 2 and 5 want the same treatment, and their bounds have more forced structure to
draw on, not less — chunk 5 in particular has the whole four-node red chain as unavoidable
work. Whether that is enough to bring a 30- or 35-deep chunk into range is an open question
I would not want to promise either way; chunk 3 is the honest next test, because it is the
first one where the bootstrap has to be counted.

## One aside on your current settings

`(ww-set *max-recorder-cycles* 5)` in the problem file does not inflate the search space per
chunk, despite the note in `tech/README.html` that raising the maximum "adds legal starts and
therefore search states". `run-recorder-cycle-search` rebinds it to that chunk's own cycle
number for the duration of the search, so with `recorder-cycles-used` already at N-1 only one
further START-RECORDER is ever legal. The global 5 is doing exactly what you intended.

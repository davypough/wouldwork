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

The current problem combines the general `topo-lm-cut-resource-bound` with the older
cycle-specific term described below.  The general term is itself the maximum of LM-cut and
a finite-domain location/resource bound.  The cycle term is zero at the first two subgoal
boundaries, where the general terms contribute without requiring a Rumin-specific
configuration.  The outer `max` avoids double-counting actions shared with the cycle term
and preserves admissibility wherever that term's documented argument applies.

## Initial LM-cut integration result

The staged first-subgoal formula is `(and (has-location box1 location8)
(has-location tray1 location2) (ghost-stops-recorder))`.  Without starting a search, the
compiled model measured as follows on 2026-08-24:

| measure | result |
|---|---:|
| complete relaxed operators | 7,823 |
| goal-relevant operators | 7,268 (7.1% removed) |
| h-max at the initial state | 2 |
| LM-cut at the initial state | 4 |
| known plan length | 30 |
| ten full-model LM-cut evaluations | 0.323 s, 60.9 MB allocated |
| ten relevance-sliced LM-cut evaluations | 0.293 s, 51.2 MB allocated |
| ten indexed relevance-sliced evaluations | 0.035 s, 10.0 MB allocated |

Relevance compilation by itself helps LM-cut by roughly 9% in time and 16% in allocation on
this goal.  Integer-indexing the compiled slice then makes evaluation about 8.4 times faster
and cuts allocation about fivefold, from approximately 29 ms and 5.1 MB per state to 3.5 ms
and 1.0 MB.  The bound is still only 4, leaving almost the entire 30-level frontier exposed,
and evaluation remains much more expensive than an ordinary Rumin node expansion.  Do not
use this result as evidence that the 30-step search is now practical.  The following probe
tests whether relaxed-state memoization can amortize that cost.

### Relaxed-state memoization probe

Memoization was characterized without retaining any cache implementation.  The probe still
computed the lower bound but returned zero, so LM-cut could not prune the shallow sample and
change which states were measured.  Graph duplicate detection happens before a surviving
node is expanded and before its lower bound is evaluated; consequently only different
concrete states sharing one indexed relaxed-fact bitset could produce a cache hit.

| cutoff | generated states | bound calls | cache hits | unique relaxed keys |
|---:|---:|---:|---:|---:|
| 2 | 33 | 6 | 0 | 6 |
| 3 | 134 | 17 | 0 | 17 |

At cutoff 3 the keys were 395-bit vectors.  The uncached indexed probe took 0.064 s and
allocated 30.9 MB; adding lookup and key retention took 0.081 s and allocated 42.3 MB, while
serving no hits.  The 54 reported repeated concrete states never reached the bound call.
Permanent relaxed-state memoization is therefore rejected: on the measured first-subgoal
frontier it adds roughly 27% time and 37% allocation without avoiding one LM-cut evaluation.

### General LM-cut cut analysis

The diagnostic is implemented in the engine rather than in Rumin.  Any compiled relaxed
model can call `relaxed-indexed-lm-cut-analysis`; any staged Topo problem can call
`analyze-topo-relaxed-lm-cut` or `report-topo-relaxed-lm-cut-analysis` with a state and goal.
Each cut record retains the selected goal, its current h-max value, the cut cost, and every
crossing operator's name, residual cost, justification supporter, complete preconditions,
and goal-entering effects.

The first-subgoal start state produces four unit-cost cuts:

| cut | selected goal | operator families |
|---:|---|---|
| 1 | `has-location box1 location8` | 168 `put-on-at`, 4 `release-tray-and-riders`, 2 `put-ground` |
| 2 | `ghost-stops-recorder` | 1 `relaxed-stop-recorder` |
| 3 | `has-location box1 location8` | 6 unconstrained `pickup` alternatives |
| 4 | `ghost-stops-recorder` | 1 `relaxed-start-recorder` |

The broad `release-tray-and-riders` operator is the important modeling defect.  Merely
holding any tray gives every support occupant every location as a joint effect, without
requiring the occupant to be on that tray.  This lets a tray pickup stand in for moving
BOX1 and merges BOX1 and TRAY1 into the same disjunctive pickup/placement cuts.

The broad fallback has now been removed from the general Topo relaxation.  The ordinary
tray placement already sets the tray location, and the zero-cost `supported-location`
consequence propagates it through the retained `on` support chain.  A focused fixture
confirms that one placement moves a tray's nested riders jointly at cost 1, while an object
outside that chain cannot inherit the destination through the tray path.

On the first-subgoal start state the permanent change raises the bound from 4 to 7 and
produces seven separate cuts: BOX1 placement, movement, and pickup; TRAY1 pickup and
movement; and recorder start and stop.  The complete model shrinks from 7,823 to 7,819
operators and its goal-relevant slice from 7,268 to 7,264.  Ten combined bound-and-analysis
evaluations took 0.041 s and allocated 12.0 MB, or roughly 4.1 ms and 1.2 MB each.  The
slightly greater evaluation cost buys three additional admissible cut rounds.

### Receiver-capability prototype rejected

A general structural receiver variant distinguished three cases: a fixed or already-paired
beam route could be exposed by one otherwise required action; an unpaired relay required a
connector pickup followed by a connect; and an unclassified custom provider retained the
one-action fallback.  Focused synthetic checks produced the intended costs for fixed,
unpaired, already-held, live-paired, and recorder-shadow-paired cases.

On the Rumin first-subgoal start state this raised h-max from 2 to 3 but left LM-cut at 7.
All six orderings of the three goal literals also returned 7.  Meanwhile the complete model
grew from 7,819 to 7,833 operators and the relevant slice from 7,264 to 7,278.  The prototype
was therefore removed: the active LM-cut bound gained no pruning strength while evaluation
would have more work.  The broad receiver trigger is not the next useful optimization.

The retained abstraction also has an explicit seven-action relaxed witness: pick up BOX1
(whose shared propagation trigger can activate RECEIVER1 and open GATE1), move from
LOCATION3 to LOCATION8, put BOX1, pick up TRAY1, reuse the still-retained LOCATION3 fact to
move to LOCATION2, start the recorder, and stop it.  The carried-tray consequence supplies
TRAY1's destination at zero additional cost.  Since LM-cut proves 7 and this witness attains
7, the estimate is the exact optimum of the current relaxed model.  Raising it materially
requires a richer abstraction, especially one that does not let an agent retain every old
location and service independent cargo tasks from all of them.

### Finite-domain location/resource bound

The retained companion bound keeps exactly one location for each currently present agent.
For supported unsatisfied cargo-location goals it minimizes movement over all task orders,
agent assignments, and authored across-reach pickup/placement vantages.  It then adds only
disjoint action categories: required pickup/placement actions, MOVE actions, and an explicit
recorder start/stop obligation.  A carried tray needs no final PUT merely to acquire a
location; objects already riding in a tray chain are omitted; and domains with propagating
relocators or exogenous happenings abstain from the affected estimate.

At the first-subgoal start state the new term is 9, versus LM-cut's exact relaxed optimum of
7.  It was checked at all 31 states of the known 30-action prefix and never exceeded the
true remaining suffix length.  The four-action topology fixture produces exact bounds
`(4 3 2 1 0)`, and focused checks cover two-agent task assignment, carried-tray relocation,
tray riders, and happening abstention.

Static traversal and reach indexes are compiled once.  The combined evaluator also shares
one extraction of concrete state facts between LM-cut and the finite-domain term.  On the
Rumin start state, 100 combined evaluations took 0.421 s and allocated 124.9 MB, about
4.21 ms and 1.25 MB each.  This is close to the prior LM-cut-only 4.1 ms and 1.2 MB while
raising the usable bound by two steps.

#### Per-node fact extraction

That shared extraction was itself the dominant per-node cost.  `topo-relaxed-state-facts`
rebuilt the static half of the fact set on every call: `list-database` decoded all 1,461
propositions of `*static-idb*` and consed one `format` string per proposition in order to
sort them by relation name, so that seven `has-position` facts could be kept and the rest
discarded.  The state's own database went through the same printer, and a quadratic
`remove-duplicates` ran over the concatenation.

The extraction now caches the static `has-position` facts once per staged problem, decodes
the state's own idb directly with `convert-to-proposition` and
`convert-to-fluent-proposition`, and removes duplicates through a hash table.
`list-database` is unchanged; it remains the sorted diagnostic printer that the state
printers, `ww-symmetry.lisp` and the static model builders use.  The two sources cannot
overlap, because a static relation never appears in a state's own database, so the only
duplicates are the bijective `holding1`/`holding2` and `on1`/`on2` pairs normalized back to
their public form -- at most four per state over the 30-action prefix.

Measured under SBCL 2.2.9 on Linux, at the first-subgoal start state:

| measure, per 100 evaluations | before | after |
|---|---:|---:|
| `topo-relaxed-state-facts` | 0.192 s, 66.3 MB | 0.0002 s, 97.7 KB |
| `topo-finite-resource-bound`, the precheck | 0.184 s, 66.7 MB | 0.0012 s, 553 KB |
| `topo-lm-cut-resource-bound`, the aggregate | 0.820 s, 121.1 MB | 0.604 s, 54.6 MB |

Fact extraction was 99.3% of the precheck's allocation, and the precheck is what serial
search evaluates on every selected node before deciding whether the LM-cut fallback is
needed at all.  Its cost per node falls from about 1.84 ms and 667 KB to 0.011 ms and
5.5 KB.  Bound values are unchanged: the three terms reproduce the recorded values at all 31
states of the 30-action prefix, and the extracted fact set is set-equal to the old one, and
duplicate-free, at every one of them.

`topo-relaxed-static-propositions` is memoized the same way, so `list-database` runs once per
staged problem rather than once per caller.  The static model builders read it a handful of
times while staging, but `topo-beam-structurally-linked-p` reads it on every evaluation of
the beam term, which is why that term measured so much worse than the precheck it extends:

| measure, per 100 evaluations | before | after |
|---|---:|---:|
| `topo-finite-beam-resource-bound` | 1.112 s, 205.0 MB | 0.116 s, 6.5 MB |

The cached list is the sorted one `list-database` returns, so every caller sees the order it
saw before; only the rebuilding is gone.  Sharing one list across callers was checked rather
than assumed -- after all model building and two full sweeps of every bound at all 31 prefix
states, the cached list is still `equal` to a fresh `list-database` of `*static-idb*`, and
the bound table is identical on the second sweep.

The beam term is not on the `min-steps-remaining?` path today, so this changes no production
throughput; it matters if that term is ever put there.  The earlier measurement of that
option -- 2.44 times slower, 209 GB rather than 92 GB -- was taken against the uncached
extraction and would need redoing.

#### Bounded first-subgoal search sample

A deterministic serial comparison used the strengthened first-subgoal goal, cutoff 30,
prefix pruning on, symmetry and randomization off, and a 30-second hard wall-clock cap per
run.  Both runs computed LM-cut and the finite-domain term at every selected node, so the
comparison gives the LM-cut-only policy no artificial evaluation-cost advantage.  Neither
run was allowed to continue into a full search.  These diagnostic wrappers did not retain
the production combined evaluator's shared fact extraction, so this first table is evidence
about graph pruning and bound dominance, not the final production throughput comparison.

| pruning policy | generated states | bound evaluations | lower-bound prunes | prune share of evaluations | maximum depth |
|---|---:|---:|---:|---:|---:|
| LM-cut only | 20,307 | 5,829 | 3,596 | 61.7% | 28 |
| max(LM-cut, finite resource) | 15,477 | 5,952 | 4,420 | 74.3% | 27 |
| finite resource only | 66,616 | 20,374 | 13,406 | 65.8% | 27 |

The resource term exceeded LM-cut at every LM-cut-only sample node and at 5,948 of 5,952
combined-run nodes; the remaining four were ties.  Its improvement averaged about 1.04
steps and reached 3.  In the same time, the combined policy evaluated 2.1% more nodes while
generating 23.8% fewer states, and raised the selected-node prune rate by 12.6 percentage
points.  This is a significant local pruning improvement, although the bounded sample does
not estimate total time to a first solution.

The finite-resource-only run omitted LM-cut evaluation and used the same 30-second cap.  It
reached the combined run's 15,477-state mark in 7.96 seconds, about 3.77 times faster, and
within 30 seconds evaluated 3.42 times as many nodes and generated 4.30 times as many
states.  Its later aggregate prune share was lower than the combined run's shorter sample,
so this does not establish that LM-cut remains redundant beyond the shared frontier.  It
does show that always paying for LM-cut is expensive enough to overwhelm much of its
additional pruning value here.  The 3.77 factor is relative to the instrumented eager run,
not the production fact-sharing evaluator.

Wouldwork now supports cost-ordered, short-circuit evaluation of admissible lower-bound
contributors.  A registered cheap bound is tested against the active depth cutoff and any
incumbent solution before the existing aggregate `min-steps-remaining?` query.  If it proves
pruning, evaluation stops.  Without adaptive fallback sampling, the aggregate query still
runs on every admitted node, preserving every problem-specific term and exactly the prior
pruning semantics.  Contributor registrations are cleared when another problem is staged.

A fair production comparison used the same deterministic 30-second setup, changing only
whether the finite-resource precheck registry was enabled:

| production policy | generated states | selected nodes | aggregate LM-cut calls | lower-bound prunes |
|---|---:|---:|---:|---:|
| eager combined | 17,310 | 7,008 | 7,008 | 5,321 |
| short-circuit combined | 33,364 | 10,921 | 3,961 | 6,960 |

The short-circuit path skipped 6,960 of 10,921 aggregate evaluations (63.7%), evaluated
1.56 times as many nodes, and generated 1.93 times as many states in the same time.  All
6,960 prunes in that longer bounded sample were already established by the finite-resource
precheck; the fallback remained available on the other 3,961 nodes.  Neither bounded run
found a solution or continued into a full search.

The production cascade was then extended to a 100,000-state hard cap, with a 120-second
backup wall cap.  It reached 100,024 states in 81.97 seconds; the small state overshoot is
one generated successor batch.  Checkpoints consistently found no incremental LM-cut
pruning:

| generated states | selected nodes | LM-cut fallbacks | finite-resource prunes | additional LM-cut prunes |
|---:|---:|---:|---:|---:|
| 25,001 | 9,148 | 3,067 | 6,081 | 0 |
| 50,025 | 15,762 | 5,383 | 10,379 | 0 |
| 75,030 | 22,174 | 7,635 | 14,539 | 0 |
| 100,024 | 28,645 | 9,716 | 18,929 | 0 |

Thus the precheck avoided 66.1% of LM-cut evaluations, while all 9,716 LM-cut fallbacks
failed to reject a node that the finite-resource term admitted.  The sample reached maximum
depth 27, found no solution, and stopped at its state cap.  This is strong evidence that
LM-cut's current abstraction is not paying for itself on this first-subgoal frontier, but it
is not a domain-independent dominance proof and does not justify removing the fallback from
other Topo goals.

The general search engine now adapts that remaining aggregate work during serial search.
After 512 consecutive aggregate evaluations add no prune beyond the cheap contributors, it
samples one aggregate fallback per 64 admitted nodes.  A sampled unique prune or a new
incumbent solution immediately restores eager evaluation.  Setting the interval to 1 also
keeps eager behavior, as do parallel searches.  Skipping an admissible lower bound may expand
the explored graph, but it cannot remove a solution or accept a solution beyond the cutoff.

The same 100,000-state Rumin setup measured the adaptive policy under identical caps:

| policy | wall time | generated states | selected nodes | aggregate evaluations | aggregate skips | unique aggregate prunes |
|---|---:|---:|---:|---:|---:|---:|
| eager fallback | 81.97 s | 100,024 | 28,645 | 9,716 | 0 | 0 |
| adaptive fallback | 45.22 s | 100,024 | 28,645 | 655 | 9,061 | 0 |

The adaptive run sampled only 6.7% of aggregate opportunities and reduced wall time by 44.8%
(1.81 times the throughput).  It retained the same 18,929 cheap lower-bound prunes, maximum
depth 27, selected-node count, and generated-state count, and found no solution before the
state cap.  None of its samples caused reactivation.  This is the intended behavior for this
frontier: retain a bounded check for new evidence while avoiding repeated LM-cut work that has
not changed a decision.  It remains an empirical performance result, not a claim that every
Topo problem makes LM-cut redundant.

An exploratory 232,854-state run with the problem-wide five-cycle maximum reported 154
unique aggregate prunes.  That number was initially, but incorrectly, attributed to LM-cut.
A component diagnostic found 20 such prunes in its first 10,008 states, all with the same
decisive values: depth 25, five steps remaining, finite-resource bound 5, LM-cut 4, and the
Rumin cycle-specific bound 6.  Every state had already entered recorder cycle 2 even though
the active goal required exactly `(recorder-cycles-used 1)`.  The cycle-specific term, not
LM-cut, proved those prunes.  The exploratory harness had also captured its search wrapper
before staging force-reloaded the engine, so its throughput and adaptive-event counts are
not retained as comparative measurements.

For the actual first-subgoal graph, the diagnostic narrowed `*max-recorder-cycles*` to 1.
This is the same restriction guided recorder chaining applies for the next requested cycle,
and it cannot remove a solution whose exact goal count is 1.  A corrected, post-staging
diagnostic then ran to a 750,000-state cap:

| generated states | wall time | selected nodes | finite-resource prunes | aggregate evaluations | aggregate skips | unique aggregate prunes |
|---:|---:|---:|---:|---:|---:|---:|
| 750,001 | 342.51 s | 201,184 | 128,734 | 1,636 | 70,814 | 0 |

The finite-resource term made every lower-bound prune.  It admitted 72,450 selected nodes;
after the 512-check warmup, adaptation evaluated the aggregate on 2.26% of them and sampled
no unique prune.  Because the cheap term rejected most selected nodes first, LM-cut ran on
only 0.81% of all selected nodes.  The run found no solution, reached maximum depth 27, and
stopped at its state cap after 342.5 seconds.

This substantially extends the zero-incremental-value result on the feasible first-cycle
frontier.  It still is not a domain-independent dominance proof: LM-cut remains available
and sampled for other Topo problems and goals.  For this subgoal, however, the evidence does
not support spending the next optimization effort on LM-cut.  The remaining problem is the
strength of the cheap finite-resource bound and the large frontier it admits.

### Finite-resource component and slack analysis

The general Topo service now exposes `analyze-topo-finite-resource-bound` and
`report-topo-finite-resource-bound-analysis`.  The analysis record contains the exact
manipulation, routing, and recorder-session values summed by the production bound, together
with its supported goals and retained cargo tasks.  It is diagnostic only: the production
query obtains its total from the same component function, so this did not introduce a second
estimate or change pruning semantics.  The synthetic resource fixture checks the decomposition
`2 + 2 + 0 = 4` and equality with the production query.

Replaying the known 30-action first-cycle prefix against the explicit one-cycle goal gave:

| depth | true actions remaining | manipulation | routing | session | total | slack |
|---:|---:|---:|---:|---:|---:|---:|
| 0 | 30 | 3 | 4 | 2 | 9 | 21 |
| 10 | 20 | 3 | 4 | 1 | 8 | 12 |
| 15 | 15 | 2 | 3 | 1 | 6 | 9 |
| 20 | 10 | 2 | 2 | 1 | 5 | 5 |
| 24 | 6 | 0 | 1 | 1 | 2 | 4 |
| 26 | 4 | 0 | 0 | 1 | 1 | 3 |
| 28 | 2 | 0 | 0 | 1 | 1 | 1 |
| 29 | 1 | 0 | 0 | 1 | 1 | 0 |
| 30 | 0 | 0 | 0 | 0 | 0 | 0 |

Thus the estimate is safe along this witness, but begins 21 actions below the known suffix
and becomes exact only for the final stop.  Its early omissions are structural rather than
an arithmetic defect: goal cargo and the recorder boundary are counted, while prerequisite
control setup, relay construction, ghost-side setup, and the internal traversal segments of
one MOVE are deliberately relaxed away.

A second post-staging diagnostic sampled every 64th admitted node at depth 20 or greater in
a one-cycle run, capped at 250,000 generated states.  It stopped at 250,002 states after
118.93 seconds: 73,134 nodes had reached lower-bound evaluation, 23,309 near-cutoff nodes
were admitted, and 364 were sampled.  Their remaining allowance after the finite-resource
bound was:

| slack | sampled states | share |
|---:|---:|---:|
| 0 | 268 | 73.6% |
| 1 | 72 | 19.8% |
| 2 | 19 | 5.2% |
| 3 | 5 | 1.4% |

The sample is deterministic along this DFS traversal, not a random sample of the whole
graph.  Even so, it identifies a high-leverage target: 93.4% of sampled admitted nodes would
be rejected by one additional admissible step.  The next candidate should therefore be a
general, disjoint control/setup component derived from route prerequisites and required
non-resource actions.  It must account for overlap with cargo PUT actions that themselves
depress plates; simply adding a gate or controller count would double-count manipulation.
The safe design problem is to cost a small precedence graph jointly, classifying concrete
action kinds already charged by manipulation, routing, and session rather than naming Rumin
connectors, gates, locations, or coordinates.

### Diagnostic control/setup cost partition

`analyze-topo-control-setup` now performs that classification without contributing to
`min-steps-remaining?`.  It copies the goal-relevant relaxed model and sets already-charged
operators to zero: MOVE when the finite routing component is positive, pickup/placement for
the retained task objects whose manipulation component is positive, and recorder start/stop
when the session component is positive.  LM-cut then measures residual actions in two views:

* **shared effect** preserves every explicit and generic effect of the zero-cost resource
  operators.  A covered action may therefore satisfy a controller at no additional cost.
* **dedicated effect candidate** removes only `(:topo-action-taken)` from those covered
  operators.  Explicit overlap remains: for example, a zero-cost goal-cargo PUT still creates
  `on`, whose zero-cost consequence may depress its plate.  The difference isolates costs
  that depend specifically on proving that generic controller propagation needs another
  action.

Synthetic checks distinguish the intended cases.  A route controlled by a plate needs one
residual action when nothing covered can depress it.  A covered cargo placement onto that
plate reduces both views to zero.  A receiver that may be activated by a covered action has
shared cost zero and dedicated candidate one.  These are cost-partitioning diagnostics, not
new pruning claims.

On the known first-cycle replay, the shared value is zero at every depth.  The dedicated
candidate is one from depth 0 through depth 9, raising the initial candidate total from 9 to
10, then becomes zero immediately after action 10, `CONNECT-CONNECTOR`.  It never exceeds
the known remaining suffix.  The initial dedicated LM cut is one broad disjunctive cut whose
families include pickup, placement, plate change, generic action, and gate fallback; it does
not by itself prove which concrete family must pay the step.

The same deterministic 250,000-state diagnostic sampled every 64th admitted node at depth
20 or greater.  It stopped at 250,002 states after 120.91 seconds, with the same 73,134
lower-bound evaluations, 23,309 admitted near-cutoff nodes, and 364 samples as the component
run.  Every sample had shared cost zero and dedicated candidate one.  The candidate would
have newly rejected all 268 zero-slack samples, 73.6% of the sample, and none of the other
96.

This establishes leverage, not admissibility.  Making the dedicated value a pruning term
would currently be unsound because the shared model explicitly permits a resource action's
generic propagation trigger to activate the controller.  The next proof step is a general
typed-effect trigger model: installed Topo capabilities should declare which action families
can change plates, receivers, gates, and other controllers.  LM-cut can then preserve real
overlap while excluding impossible overlap, without referring to Rumin object names or
coordinates.  Only a residual cost that survives that capability-aware shared model should
be added to the finite-resource bound.

#### Typed trigger result

The diagnostic now also replaces `:topo-action-taken` with capability-specific plate,
receiver, and relocation triggers.  The mapping comes from the installed object roles and
abstract operator family: movement or manipulation can change a receiver only for a
`beam-blocker`; plate changes and their gate consequences can change receivers; recorder
transitions retain receiver effects; and the one-action fallback supplies every typed effect
for an unmodeled capability.  Covered operators still cost zero.  The typed dedicated view
removes their typed side effects but leaves explicit effects such as `on` untouched.

Focused checks show that this distinction works mechanically.  A covered manipulation of an
untyped object no longer activates a receiver.  Giving that object the installed
`beam-blocker` role changes the typed shared/dedicated result from `1/1` to `0/1`.  Plate
placement overlap remains `0/0`.  The complete synthetic relaxed-heuristic claims pass.

It does not strengthen the safe Rumin result.  Along the 30-action witness, typed
shared/dedicated remains `0/1` through action 9 and `0/0` from the connector action at depth
10 onward.  At the initial state the reporter identifies the conservative overlap sources
in the goal-relevant slice:

| covered family supplying receiver change | operators |
|---|---:|
| `put-on-at` | 672 |
| `move` | 608 |
| `put-ground` | 42 |
| `pickup` | 2 |
| recorder start/stop | 1 each |

BOX1 and the agents are beam blockers, so the typed model must still allow their covered
movement or manipulation to alter a receiver.  Merely knowing the action and object role
cannot establish that they are irrelevant to the particular beam route.

The like-for-like 250,002-state run completed in 122.88 seconds.  All 364 samples again had
typed shared/dedicated `0/1`; the dedicated value would reject the same 268 zero-slack states,
but the typed shared value rejects none.  Typed action capabilities therefore preserve the
candidate's measured leverage while confirming that they are insufficient for admissibility.

The next useful discriminator is general beam provenance and corridor geometry.  For a
receiver required by a route gate, the analysis should identify structurally viable direct
and relay sources, then allow a covered beam blocker to overlap activation only when one of
its possible transitions touches an occluder location on such a source-to-receiver chain.
Existing pairings, unpaired connector setup, and unknown/custom providers need separate
fallbacks.  This remains reusable across beam-enabled Topo domains and avoids Rumin names or
coordinate cutoffs.

#### Beam-provenance conclusion

The final discriminator shows that corridor geometry is not needed for the useful case.
The diagnostic builds a permissive structural beam graph from directional `coupled` facts
and undirected `paired` facts, starting at every transmitter.  Hue, visibility, occluders,
and crossing state are ignored.  If that graph already reaches a receiver, the receiver is
free in this diagnostic: an otherwise-required beam-blocker action may expose it.  If no
graph path exists, however, moving an occluder cannot create one.  Under the standard
`beam-relay` provider, at least one `CONNECT-CONNECTOR` is then required before that receiver
can become active.

The link action is represented by one shared unit-cost effect for every unlinked receiver;
all other relaxed operators cost zero.  The result is therefore binary.  It is one only when
even the permissive Topo route model needs an unlinked receiver, and zero when an alternate
route avoids it.  It also abstains when relay support is absent, happenings are installed,
no connector exists, or a connector has a positive finite-resource manipulation task.  The
last case matters because `CONNECT-CONNECTOR` places its connector and could otherwise
overlap a placement action already counted by the finite-resource term.

Focused checks cover an unavoidable unlinked receiver (one), an existing paired or coupled
path (zero), and a gate-free alternate route (zero).  The full relaxed-heuristic claim suite
passes.  Replaying the known 30-action first cycle produced link cost one from depth 0
through depth 9, zero immediately after action 10 establishes the live source chain, and
zero thereafter.  The sum with the finite-resource term never exceeded the known remaining
suffix.

The corrected like-for-like frontier sample used the exact one-cycle goal, cutoff 30,
prefix pruning on, one-cycle maximum, and a 250,000-state cap.  It stopped at 250,002 states
after 183.88 seconds, with 73,134 selected nodes and 23,309 resource-admitted nodes at depth
20 or greater.  Sampling every 64th admitted node reproduced the earlier 364-state slack
distribution exactly:

| beam-link result | sampled states |
|---:|---:|
| 0 | 37 |
| 1 | 327 |

Of the 268 zero-slack samples, 244 still required the link.  Adding this one admissible step
would therefore reject 244 of 364 sampled admitted states (67.0%), including 91.0% of the
zero-slack group.  The 24 zero-slack exceptions are the important correction to the typed
dedicated estimate: their current structural links or alternate relaxed routes make another
link action unnecessary.

The production experiment compiled a reusable binary h-max model and checked the same
250,000-state search cap three ways:

| production wiring | seconds | selected | lower-bound prunes | aggregate evaluations | max depth |
|---|---:|---:|---:|---:|---:|
| finite-resource baseline | 117.13 | 73,134 | 49,740 | 869 | 27 |
| beam term on every selected node | 285.57 | 80,382 | 56,378 | 879 | 27 |
| beam term in adaptive aggregate | 538.11 | 80,382 | 56,378 | 66,252 | 27 |

The beam term really did add 6,638 lower-bound prunes, but it also redirected the traversal
to 7,248 more selected nodes before generating the same number of states.  Unconditional
evaluation was 2.44 times slower than baseline and allocated 209 GB rather than 92 GB.  In
the adaptive aggregate, sampled beam prunes continually restored eager fallback evaluation,
so the run became 4.59 times slower and allocated 259 GB.  Neither form reached deeper than
27 or found a solution under the cap.

**Conclusion:** the location cutoff, generic LM-cut, typed-trigger, and current relaxed
beam-link production branches should not be pursued further for this first subgoal.  Beam
provenance is an admissible and informative discriminator, and its focused safety checks
and explicit `topo-finite-beam-resource-bound` query remain useful for analysis across
standard relay-enabled Topo domains.  It does not, however, turn out to be a useful search
promotion in this implementation: the production contributor and aggregate remain the
finite-resource bound and ordinary adaptive LM-cut.  A future beam promotion would need a
direct structural test substantially cheaper than running a second relaxed model; merely
sampling the present model is not enough because successful samples reactivate it.

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

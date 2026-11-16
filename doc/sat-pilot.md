# Wouldwork and SAT planning pilot

This stand-alone test asks both solvers whether an agent can reach the same goal
within a given number of Talos actions. It uses Wouldwork's depth-first graph
search and Z3's propositional SAT solver. It is independent of the regular test
suites.

## Running the comparison

From the Wouldwork project directory, with Python 3.10 or newer, SBCL and the
usual Quicklisp startup available, run either command. The `uv` launcher supplies
the Python dependency.

```powershell
uv run --no-project --with z3-solver python -B test/search/sat-pilot.py
uv run --no-project --with z3-solver python -B test/search/sat-pilot.py --threads 4
```

If your Python already has the `z3-solver` package, run
`python -B test/search/sat-pilot.py` directly. Add `--shelves 6` to increase the
extra placement sites from three to six. The runner makes a temporary copy of
the planner, checks which project ASDF selected, and stages the puzzles in that
copy using instance-specific filenames. Each run prints its results and removes
the temporary project on exit.

## What the cases measure

| Case | Action bound | Expected outcome |
|---|---:|---|
| Existing switch test | 0, 1 | The initial goal is false. Toggling the accessible switch satisfies the full Wouldwork goal. |
| Basic gate | 4, 5 | Two pickups, two placements and one crossing are necessary and sufficient. |
| Extra choices | 4, 5 | Additional reachable floor locations give each box more legal placements. The shortest plan still takes five actions. |
| One-box shortage | 5 | A single box cannot depress both plates during the crossing. |
| Shortage with explicit pruning | 5 | Both solvers receive the same deduction that the exit cannot be reached with fewer boxes than plates. |

The synthetic gate fixtures have one walk segment from the hub to the exit.
From the hub the agent can pick up or put down a box on any reachable floor
location or clear plate. Both boxes have height two, placing their tops above
the agent's reach. A box therefore cannot be stacked on another box. The
fixture includes walking, box, reachability, pressure plate and gate technology.
The agent has no action to step onto a plate. The only gate control requires
both ordinary plates to be depressed simultaneously.

The SAT encoding is written specifically for these fixtures. It represents each
box's position and each action choice with Boolean variables at each step.
An idle step permits a shorter plan to satisfy a larger bound. On crossing,
the SAT plan can end. The existing switch test reduces to a single toggle
because its other switches are inaccessible and the carried cargo is invariant.
SAT plans are decoded into ordinary Talos actions and replayed against the
staged Wouldwork goal. The runner checks the expected answer from each solver
and reports an error on a disagreement, failed replay or unexpected interruption.

## Five-box comparison

Add `--large` to either command to run the five-box puzzle. Five ordinary
plates control the same gate, one per required box. All five boxes start on
the hub floor. The agent can also set a box down at six extra floor locations.
The ordinary Wouldwork operations and SAT transition rules are the same as in
the basic gate case. Neither solver receives the minimum length as a pruning
rule.

The two decision questions use bounds of 10 and 11 actions. Every successful
crossing requires five pickups and five separate plate placements before the
move. One pickup and placement per box followed by the crossing achieves 11.
The runner uses this argument to check completed answers. Each SAT run gets
60 seconds including formula construction. Each Wouldwork search gets 60
seconds after staging. A timeout is reported independently for either solver
and leaves that solver's answer inconclusive. SAT plans are replayed even if
Wouldwork's own search times out.

### Five-box results

These Windows runs used SBCL 2.6.10 and Z3 5.4.1. All four comparisons
finished within the limit. Both returned SAT plans passed Wouldwork replay.
Each time is one wall-clock observation in seconds. Wouldwork staged once
per thread setting, taking 4.505 seconds for serial and 4.513 seconds for
four workers.

| Bound | Result | Threads | SAT build | SAT check | SAT total | WW search | WW states processed |
|---:|---|---:|---:|---:|---:|---:|---:|
| 10 | Impossible | 1 | 0.142 | 13.862 | 14.003 | 24.639 | 9,417,611 |
| 10 | Impossible | 4 | 0.141 | 5.151 | 5.292 | 9.095 | 9,384,141 |
| 11 | Plan found | 1 | 0.150 | 0.160 | 0.310 | 9.844 | 3,455,117 |
| 11 | Plan found | 4 | 0.153 | 0.053 | 0.206 | 7.126 | 6,853,428 |

Four threads sped up the impossibility proof by 2.65 times for SAT including
encoding and 2.71 times for staged Wouldwork. On the successful four-thread
run, SAT spent more time building the formula than solving it. Search order
can affect the time to the first plan and the number of states processed.

### Five-box feature comparison

The `--five-features` option runs the same five-box layout four times at each
bound with baseline graph search, symmetry pruning alone, the topology lower bound
alone, and both together. Use `--threads 4` for the four-worker comparison.
The SAT encoding is identical across these settings and runs once per bound
and thread count. Each Wouldwork configuration is staged separately and replays
the SAT plan when one is found. The feature lines report whether canonical
symmetry was active, the number of detected families, and lower-bound
evaluations and prunes.

Each entry below gives elapsed milliseconds and Wouldwork states processed in
parentheses. SAT times include formula construction. Each cell represents one
run on Windows with SBCL 2.6.10 and Z3 5.4.1. Every completed search returned
the expected result, and the 11-action SAT plan passed replay in all eight
Wouldwork configurations across the two thread counts.

| Configuration | 10 actions, serial | 11 actions, serial | 10 actions, four threads | 11 actions, four threads |
|---|---:|---:|---:|---:|
| Baseline | 24,629.3 (9,417,611) | 9,729.5 (3,455,117) | 9,021.5 (9,384,211) | 7,142.4 (6,855,092) |
| Symmetry | 1,713.2 (107,858) | 1,192.3 (82,293) | 509.9 (98,085) | 753.2 (156,010) |
| Lower bound | 1.3 (1) | 2.8 (153) | 1.1 (1) | 8.0 (2,001) |
| Both | 0.5 (1) | 3.3 (107) | 1.1 (1) | 13.1 (614) |
| SAT | 14,141.31 | 313.95 | 5,285.40 | 203.36 |

Symmetry was active with one detected family whenever enabled. At bound 10,
every lower-bound configuration evaluated and pruned the starting state once.
At bound 11, the serial lower-bound-only run pruned 61 times in 75 evaluations
and the serial combined run pruned 15 times in 26 evaluations. The four-worker
counts were 83 prunes in 234 evaluations and 296 in 359, respectively.
The engine derives the minimum from gate prerequisites, the cost of filling
each required plate and the final move. SAT received ordinary transition
rules. The runner's independent expected length is used only to check answers.

Staging took 4.44–4.78 seconds per configuration, reused across both bounds.
SAT formula-build times were 147.74 and 151.70 ms for serial bounds 10 and 11,
and 140.16 and 150.27 ms for four threads. The 10-action proof still favored
lower-bound Wouldwork if charged a full fresh stage in either thread setting.
For the 11-action plan alone, SAT was quicker if Wouldwork needed a fresh stage.

## Reading the times

The original pilot and five-box fixture used bounded depth-first graph search,
including duplicate-state detection. Symmetry pruning defaults to off and those
fixtures do not load `topo-lower-bound` or define a remaining-steps estimate.
Only the one-box explicit-pruning case supplies a custom state-pruning rule.
Failed-state memory is an alternative for tree search. The engine rejects that
setting with graph search.

* **SAT build** measures construction of the Boolean formula through the Python
  interface. **SAT check** times the solver call, including its internal
  preprocessing. Add the two for the cost of one bounded SAT question.
* **WW stage** includes Wouldwork's normal recompilation and initialization.
  The runner stages once per puzzle and repeats that figure on each bound's
  row. **WW search** times a bounded query on that staged puzzle. At bound zero
  the runner calls the goal test directly, since zero means unlimited search
  in Wouldwork.
* Process startup, creating the temporary checkout, and checking the returned
  plans are outside the timed intervals. One thread means ordinary serial
  Wouldwork search. Four threads selects four Wouldwork workers or requests
  four threads from Z3. Each timing is one observation.

The SAT build time covers a hand-written encoding. Wouldwork staging covers a
general planner reload. For interactive use with an already staged puzzle,
compare WW search against SAT build plus check. To compare fresh puzzle setup,
include the staging cost once. Sub-millisecond differences on these fixtures
are sensitive to overhead and run order.

## Measured pilot

Windows, SBCL 2.6.10 and Z3 5.4.1, with the default three extra sites. Both
solvers gave the expected answer on all eight bounds in each thread setting.
All six returned SAT plans passed Wouldwork replay. Times below are milliseconds
for one run per setting. SAT includes formula construction and checking.

| Case and bound | WW serial | SAT serial | WW four workers | SAT four threads |
|---|---:|---:|---:|---:|
| Basic, 4 | 0.7 | 11.56 | 1.8 | 11.40 |
| Basic, 5 | 0.8 | 13.15 | 1.5 | 13.05 |
| Extra choices, 4 | 0.8 | 15.15 | 4.2 | 15.12 |
| Extra choices, 5 | 1.1 | 18.92 | 3.7 | 18.71 |
| One-box shortage, 5 | 0.5 | 7.84 | 3.5 | 8.08 |
| Shortage with pruning, 5 | 1.2 | 7.75 | 1.0 | 7.88 |

Wouldwork staging took 4.25–4.55 seconds per fixture across the two runs. On
the serial four-action failure, extra sites raised Wouldwork's processed state
count from 65 to 155. The explicit shortage deduction reduced the state count
from 11 to one at both thread settings.

## Sequence of gates with box reuse

Add `--sequence` to the serial or four-thread command. This fixture uses the
layout from `probs/problem-boxes.lisp` with public Talos walking, plate, gate
and box operations. The SAT encoding permits a single MOVE to traverse any
open route, including multiple gate segments. A held box travels with the
agent. Manipulation requires being in the same area, and the boxes are too
tall to stack.

Box 1 starts beside plates 1 and 2. Opening gate 1 lets the agent fetch box 2
and bring it back. Box 2 can then hold gate 2 open while the agent carries box
1 to plate 3 and opens the exit. Both solvers rejected a nine-action bound
and found a ten-action plan. Each SAT plan passed replay against both staged
Wouldwork configurations.

Both Wouldwork configurations use the same puzzle, goal, action bounds and
graph search. The tuned version enables symmetry pruning and includes the
topology lower-bound technology. The benchmark checks whether canonical
symmetry is active and records actual lower-bound evaluations and prunes. SAT
runs once per bound and thread count and serves as the common comparison.

| Bound | Threads | WW baseline ms (states) | WW tuned ms (states) | SAT build ms | SAT solve ms |
|---:|---:|---:|---:|---:|---:|
| 9 | 1 | 0.7 (46) | 0.8 (24) | 34.51 | 8.08 |
| 10 | 1 | 0.9 (55) | 1.1 (30) | 33.89 | 5.41 |
| 9 | 4 | 2.0 (46) | 1.6 (24) | 34.24 | 7.12 |
| 10 | 4 | 1.6 (49) | 1.9 (34) | 36.04 | 5.77 |

One symmetry family was active in all tuned runs. The serial lower bound
pruned five times at depth bound nine and three times at bound ten. These
figures measure the features together. Each elapsed time is a single
observation after staging. Baseline staging took 4.42–4.47 seconds and tuned
staging took 4.49–4.60 seconds. The measurements used SBCL 2.6.10 and Z3 5.4.1.

## Corner, stage 1 — fixed trace and one-step alternatives

Run from the project directory with the same SBCL and Quicklisp setup as the pilot.

```powershell
uv run --no-project --with z3-solver python -B test/search/sat_corner.py
```

The runner stages the four-pairing `corner-topo` puzzle in a temporary copy.
It replays the accepted 19 actions from `doc/problems/corner-topo/Actions.lisp`,
checks the goal and validators, and records the initial state and each successor.
At each of the 20 states it also asks Wouldwork for every legal one-action
successor, with symmetry pruning disabled to retain named connector choices.

The Boolean model represents each action, including its selected subset of
termini, and carries positions, holding, directed pairings and pending selections
from one step to the next. It represents lighting, receivers, gate state and
each active crossing with separate Boolean variables. For this fixture, the
two hand slots always agree and are represented by one held connector.

Beam constraints are generated on demand. Wouldwork supplies the staged
walking and sight geometry, ordered crossing lists, and visibility probes for
empty space and individual blockers. When Z3 proposes positions, pairings and
a prior gate state, an independent Python implementation computes relay colors
in rounds, resolves crossing cuts and gate feedback, and checks for clashes.
It adds a Boolean implication for that arrangement at that step. A stable
arrangement fixes all its derived facts, including those that must be false.
An inconsistent arrangement is excluded. A candidate is accepted only after
all its steps have been covered. This trades a potentially large initial beam
formula for repeated checks as new arrangements appear.

### Recorded Windows run

SBCL 2.6.10 and Z3 5.4.1, one run. The fixed trace was satisfiable. Requiring
any state fact to differ from the Wouldwork replay was unsatisfiable. At every
trace state the complete sets of legal next actions and resulting states
matched, totaling 371 comparisons. The counts after 0 through 19 actions were
3, 3, 101, 3, 5, 5, 18, 29, 4, 6, 34, 18, 11, 5, 5, 101, 3, 5, 5 and 7.

| Measurement | Result |
|---|---:|
| Temporary checkout, staging, geometry export, replay and reference successors | 10.038 s |
| Fixed-trace build and verification, total | 0.311 s |
| Of that total, action-model construction / beam evaluation and encoding / Z3 checks | 0.196 / 0.030 / 0.020 s |
| Full one-step comparisons, after reference export | 1.575 s |
| Peak working set of Python and Z3 in one process, excluding SBCL | 61.5 MiB |
| Fixed-trace formula | 2,160 state bits, 2,083 top-level assertions including 29 beam implications and the disagreement query |

At step 18, crossings 7 and 34 are active, gate1 is open, and connector1 is
red. The final retaining pickup leaves every pairing intact, clears both
crossings, closes the gate and turns connector1 blue. Receiver2 stays active
and receiver3 becomes active. Both models also allow an ordinary pickup from
the same position. It produces the same lighting there and clears connector2's
links. The checks distinguish these two outcomes.

These measurements cover one known path and all immediate departures from
it. They leave open how many arrangements the solver would have to examine
to discover a path.

## Corner, stage 2 — unrestricted discovery within 19 actions

```powershell
uv run --no-project --with z3-solver python -B test/search/sat_corner.py --search --seconds 60
```

The discovery runner exports the staged geometry, starting state and goal from
a temporary copy of the four-pairing puzzle. It leaves out the recorded action
file. Every real step is a free action choice. Unused trailing steps preserve
the full state, so a shorter solution also satisfies the bound. A completed
plan is decoded and replayed against Wouldwork's goal and validators.

One Windows run with Z3 5.4.1 and one solver thread reached the 60-second
limit without finding a plan. Staging and export took 10.069 seconds separately.
The 60.014 seconds of elapsed search time included 0.192 seconds building the
initial formula, 23.517 seconds inside Z3 checks, and 20.868 seconds evaluating
beams and adding their constraints. Remaining time included reading proposed
states from Z3 and coordinating successive checks. The Python and Z3 process
peaked at 134.9 MiB, excluding SBCL.

The run generated 19,555 beam constraints for 12,154 distinct combinations of
positions, pairings and prior gate status. The same combination at a different
action step currently receives a separate constraint. The fixed trace needed
29 constraints. The unrestricted result identifies a cost of learning lighting
one arrangement at a time. It leaves the time to discover a plan unknown.

A separate control supplied the known 19 actions with one unused trailing
step. The decoder returned 19 actions and Wouldwork accepted the replay in a
fresh temporary checkout. The Stage 1 trace and all 371 successor comparisons
also passed after the search option was added.

### Corner receiver-rule comparison

Add `--receiver-rule` to either Corner command to give Z3 one necessary
condition for each active receiver at each state. The same connector must be
placed, paired to that receiver, and lit in its required color. Corner has no
fixed direct feeds. Crossings and obstructions can still interrupt a link,
so the implication goes only from receiver activity to a qualifying connector.
The independent beam calculation still determines which receivers actually light.

```powershell
uv run --no-project --with z3-solver python -B test/search/sat_corner.py --receiver-rule
uv run --no-project --with z3-solver python -B test/search/sat_corner.py --search --seconds 60 --receiver-rule
```

The fixed-trace model gained 60 assertions (three receivers in 20 states).
It accepted the 19 actions, excluded every disagreement with the reference
trace, and matched all 371 one-step choices and outcomes.

| Unrestricted run, 19 actions, 60 s | Original | Receiver rule |
|---|---:|---:|
| Result | Timeout | Timeout |
| Beam constraints generated | 19,555 | 19,104 |
| Distinct arrangement/gate inputs | 12,154 | 12,624 |
| Z3 check time | 23.517 s | 23.842 s |
| Beam evaluation and encoding | 20.868 s | 20.454 s |
| Python and Z3 peak working set | 134.9 MiB | 135.7 MiB |

The receiver-rule run spent 0.202 s on the initial build and 60.007 s in
total, with 10.009 s for temporary setup and export separately. Each column
is one observation. Counts at timeout measure the work performed within the
budget. They do not measure how many arrangements remain before a solution.

A two-action check isolated a remaining gap. From the start, request a move to
location2 and then through gate1 to location4, with receiver1 dark immediately
before the crossing. The initial SAT formula with the receiver rule admitted
these moves by assigning gate1 open. The exact beam check rejected them.
Adding the puzzle's wiring equality (gate1 open iff receiver1 active) directly
at the intermediate state also rejected the moves, before beam evaluation.
That diagnostic used the wiring equality at the intermediate state only.

### Corner gate-wiring comparison

The `--gate-rule` option adds the same equality at every state. With
`--receiver-rule` also enabled, a gate crossing requires a placed connector
paired to receiver1 and lit red at the time of the crossing.

```powershell
uv run --no-project --with z3-solver python -B test/search/sat_corner.py --receiver-rule --gate-rule
uv run --no-project --with z3-solver python -B test/search/sat_corner.py --search --seconds 60 --receiver-rule --gate-rule
```

The combined model matched all 20 trace states and all 371 one-step choices
and outcomes. The fixed trace required 25 beam implications, compared with
29 under the receiver rule alone.

| Unrestricted run, 19 actions, 60 s | Receiver rule | Receiver and gate rules |
|---|---:|---:|
| Result | Timeout | Timeout |
| Beam constraints generated | 19,104 | 18,240 |
| Distinct arrangement/gate inputs | 12,624 | 12,533 |
| Z3 check time | 23.842 s | 23.988 s |
| Beam evaluation and encoding | 20.454 s | 19.812 s |
| Python and Z3 peak working set | 135.7 MiB | 136.3 MiB |

The combined run spent 9.948 s on temporary setup and export, then 60.001 s
in the timed experiment, including 0.202 s building the initial formula.
Each column is a single run. Both reached the limit without a plan.

A four-action diagnostic showed where beam evaluation still matters. From the
start, pick up connector1, select only receiver1, put the connector down, and
try to cross to location4. The initial formula admitted this sequence by
assigning connector1 red, even though it had no link to a transmitter. The
exact beam check rejected it. A further candidate deduction is that a lit
connector must have a path of placed, paired connectors to a transmitter of
the matching color. Whether encoding that deduction improves discovery is open.

### Corner source-path comparison

`--source-rule` requires each lit connector to have a path of placed connectors
to the transmitter of its color. Starting at a transmitter, the model first
marks placed connectors paired to it, then extends the path across connector
pairings in either direction. Two extensions suffice for Corner's three
connectors. A structural path can still be cut or blocked, so exact beam
evaluation determines whether it carries light.

```powershell
uv run --no-project --with z3-solver python -B test/search/sat_corner.py --receiver-rule --gate-rule --source-rule
uv run --no-project --with z3-solver python -B test/search/sat_corner.py --search --seconds 60 --receiver-rule --gate-rule --source-rule
uv run --no-project --with z3-solver python -B test/search/sat_corner.py --search --seconds 1200 --receiver-rule --gate-rule --source-rule
```

With all three rules, the fixed trace matched all 20 reference states and
all 371 one-step choices and outcomes. Trace verification took 0.329 s and
required 23 learned beam implications, down from 25 with the two earlier rules.

| Unrestricted 19-action search | Receiver and gate rules, 60 s | All three rules, 60 s | All three rules, 20 min |
|---|---:|---:|---:|
| Result | Timeout | Timeout | Timeout |
| Beam constraints generated | 18,240 | 10,820 | 68,752 |
| Distinct arrangement/gate inputs | 12,533 | 7,437 | 47,435 |
| Z3 check time | 23.988 s | 20.592 s | 921.117 s |
| Beam evaluation and encoding | 19.812 s | 22.979 s | 159.089 s |
| Python and Z3 peak working set | 136.3 MiB | 114.9 MiB | 367.0 MiB |

The 60-second all-rules run took 9.936 s for setup and export, then 60.001 s
including 0.224 s for the initial formula. The fresh 20-minute run took
10.120 s for setup and export, then 1,200.003 s including a 0.229 s build.
Z3 checks accounted for 77% of the longer budget. Each column is a single
run. The time to a first plan and the benefit of the source rule at 20 minutes
remain unknown. Wouldwork's recorded 1,017 s meeting search used five
precomputed backward layers (see `doc/problems/corner-topo/Handoff.md`).

The four-action receiver-only crossing described above became unsatisfiable
in the initial SAT formula. A different check fixed only the base facts from
step 18 of the validated plan and asked whether all three receivers could be
active at once. The initial formula admitted that combination by assigning
connector1 both red and blue. Forbidding those two colors simultaneously on
that connector rejected the combination before exact beam evaluation. The
color restriction was used only in the diagnostic.

### Corner one-color comparison

Add `--one-color` to enforce color exclusivity at every state. A connector
can be dark, red, or blue. Its color can change between actions. For the
19-action bound this adds one clause for each of three connectors at each
of 20 states.

```powershell
uv run --no-project --with z3-solver python -B test/search/sat_corner.py --receiver-rule --gate-rule --source-rule --one-color
uv run --no-project --with z3-solver python -B test/search/sat_corner.py --search --seconds 60 --receiver-rule --gate-rule --source-rule --one-color
uv run --no-project --with z3-solver python -B test/search/sat_corner.py --search --seconds 1200 --receiver-rule --gate-rule --source-rule --one-color
```

With the four rules enabled, the model matched all 20 states of the accepted
trace and all 371 single-action alternatives and outcomes. With the placements
and pairings from step 18, the earlier formula allowed all three receivers to
be active. The new formula rejected that combination before beam evaluation.
The accepted trace still gave connector1 red at step 18 and blue at step 19.

| Unrestricted 19-action search, 60 s | Three earlier rules | With one-color rule |
|---|---:|---:|
| Result | Timeout | Timeout |
| Beam constraints generated | 10,820 | 10,341 |
| Distinct arrangement/gate inputs | 7,437 | 7,053 |
| Z3 check time | 20.592 s | 18.817 s |
| Beam evaluation and encoding | 22.979 s | 23.365 s |
| Python and Z3 peak working set | 114.9 MiB | 114.4 MiB |

The four-rule run took 10.000 s for setup and export, then 60.002 s including
0.225 s for the initial formula. Each column is a single run. Neither run
established how long discovery takes.

| Unrestricted 19-action search, 20 min | Three earlier rules | With one-color rule |
|---|---:|---:|
| Result | Timeout | Timeout |
| Beam constraints generated | 68,752 | 69,241 |
| Distinct arrangement/gate inputs | 47,435 | 46,754 |
| Z3 check time | 921.117 s | 927.667 s |
| Beam evaluation and encoding | 159.089 s | 151.674 s |
| Python and Z3 peak working set | 367.0 MiB | 367.7 MiB |

The fresh four-rule run took 10.104 s for setup and export, then 1,200.002 s
including 0.230 s for the initial formula. With one observation per setting
and both runs reaching the deadline, the effect on time to a first plan
remains unknown.

### Discovering a continuation from a recorded state

With `--search --after N`, Wouldwork replays the first N accepted actions.
SAT receives that state's facts, the normal geometry and goal, and up to 19−N
free actions. The recorded continuation is omitted from the formula. On
success, a fresh Wouldwork checkout replays the prefix and decoded continuation
from the original start and checks the goal and solution validators.

```powershell
uv run --no-project --with z3-solver python -B test/search/sat_corner.py --search --after 14 --seconds 60 --receiver-rule --gate-rule --source-rule --one-color
uv run --no-project --with z3-solver python -B test/search/sat_corner.py --search --after 12 --seconds 60 --receiver-rule --gate-rule --source-rule --one-color
uv run --no-project --with z3-solver python -B test/search/sat_corner.py --search --after 10 --seconds 60 --receiver-rule --gate-rule --source-rule --one-color
uv run --no-project --with z3-solver python -B test/search/sat_corner.py --search --after 9 --seconds 60 --receiver-rule --gate-rule --source-rule --one-color
uv run --no-project --with z3-solver python -B test/search/sat_corner.py --search --after 9 --seconds 1200 --receiver-rule --gate-rule --source-rule --one-color
uv run --no-project --with z3-solver python -B test/search/sat_corner.py --search --after 8 --seconds 1200 --receiver-rule --gate-rule --source-rule --one-color
uv run --no-project --with z3-solver python -B test/search/sat_corner.py --search --after 7 --seconds 1200 --receiver-rule --gate-rule --source-rule --one-color
```

| Start after action | Free actions | Result | Search including build | Separate setup/export | Beam constraints | Distinct arrangement/gate inputs |
|---|---:|---|---:|---:|---:|---:|
| 14 | 5 | Validated plan | 0.365 s | 10.872 s | 112 | 96 |
| 12 | 7 | Validated plan | 1.167 s | 10.240 s | 456 | 378 |
| 10 | 9 | Validated plan | 47.854 s | 9.963 s | 11,131 | 8,143 |
| 9 | 10 | Timeout | 60.003 s | 9.983 s | 9,254 | 6,689 |
| 9 | 10 | Validated plan (20-minute budget) | 205.624 s | 10.089 s | 19,389 | 14,592 |
| 8 | 11 | Validated plan (20-minute budget) | 294.798 s | 10.021 s | 28,679 | 21,902 |
| 7 | 12 | Timeout (20-minute budget) | 1,200.017 s | 10.030 s | 54,394 | 41,045 |

From step 14, SAT picked up connector3 at location3, selected connector1,
receiver2, receiver3 and transmitter1, and put it down there. It then walked
to location4 and picked up connector2 with an ordinary pickup. The recorded
ending selected connector2 in place of receiver3 and retained connector2's
pairings on the final pickup. Wouldwork validated all 19 actions of the
alternative plan. At step 9, connector2 still has to be positioned in the
recorded plan. The timeout leaves open which of the additional choices costs
the most search time. Each timing is a single run.

At step 12 the agent holds connector2 at location4 with transmitter1 already
selected. SAT put it down, walked back to location3, picked up connector3,
selected connector1, connector2, receiver2 and transmitter1, and put it down.
The agent then crossed to location4 and picked up connector2. The first six
actions matched the recorded continuation. The ordinary final pickup cleared
connector2's pairings. Wouldwork validated the complete 19-action plan.

At step 10 the agent held connector2 at location2 with no termini selected.
SAT selected transmitter1 alone, carried connector2 through gate1 to location4,
put it down, and returned to location3. It picked up connector3, selected only
connector1, receiver2 and transmitter1, and put it down. It then crossed to
location4 and made an ordinary pickup of connector2. Wouldwork validated the
ten recorded actions followed by these nine choices. The final connector3
setup thus worked with three pairings under the puzzle's four-pairing cap.
The recorded prefix itself uses four, so this run does not settle the cap-three
puzzle.

In the fresh 20-minute run from step 9, SAT first picked up connector2 and
selected receiver1, receiver3 and transmitter1. It carried connector2 to
location4, put it down, returned to location3 and picked up connector3. For
connector3 it selected connector1, receiver2 and transmitter1. It put
connector3 down, crossed to location4 and picked up connector2 retaining its
pairings. Wouldwork validated the nine recorded actions plus these ten
discovered actions from the original start. The search spent 118.546 s inside
Z3 and 40.630 s evaluating and encoding beams. Peak Python and Z3 memory was
135.2 MiB.

At step 7 the agent holds connector1 at location2. Its link from connector3
is retained. The next recorded action selects receiver3, receiver1 and
transmitter2, and step 9 puts connector1 down. Starting before those two
choices, SAT spent 954.192 s inside Z3 and 124.144 s evaluating and encoding
beams; peak Python and Z3 memory was 274.2 MiB. It returned no plan before
the deadline. The recorded 12-action continuation remains a valid witness.

At step 8 the three selected termini are pending and connector1 is still held
at location2. SAT put it down first. It then picked up connector2, selected
receiver1 and transmitter1, carried it to location4, set it down and returned
to location3. It picked up connector3, selected connector1, receiver2 and
transmitter1, put it down, crossed to location4 and picked up connector2
retaining its pairings. Wouldwork validated all 19 actions from the original
start. Z3 checks took 204.190 s and beam evaluation and encoding took 39.501 s.
Peak Python and Z3 memory was 171.2 MiB. The recorded selection was sufficient
for this search to find a finish; the step-7 and step-8 trials also differed
by one free action, so their times do not isolate the cost of selecting links.

### Wouldwork versus SAT from step 8

```powershell
python -B test/search/sat_corner.py --ww-search --after 8 --seconds 1200
```

Both searches started with connector1 held at location2 and the same pending
selection. They used one thread, the original goal, an 11-action cutoff, and
a 20-minute budget to find the first solution. Wouldwork used depth-first
graph search with canonical symmetry active and the topology lower bound.
SAT used all four optional rules. Both full 19-action plans passed Wouldwork
replay and the solution validators.

| Solver | Search time | Discovered actions |
|---|---:|---:|
| Wouldwork | 141.631 s | 11 |
| SAT, including formula construction | 294.798 s | 11 |

Wouldwork processed 3,847,863 states and recorded 641,958 lower-bound
evaluations and 115,940 lower-bound prunes. Total elapsed time including
temporary setup and replay was 151.952 s. SAT's temporary setup and export
took 10.021 s outside its search timer. In this pair of single runs
Wouldwork reached the first plan in about 48% of SAT's search time.

The two continuations followed the same route. The only action choice that
differed was the selection for connector2 at location2. Wouldwork selected
transmitter1 alone. SAT selected receiver1 as well. Both selected connector1,
receiver2 and transmitter1 for connector3, and both finished by picking up
connector2 while retaining its pairings.

### Wouldwork versus SAT from step 7

```powershell
python -B test/search/sat_corner.py --ww-search --after 7 --seconds 1200
```

With 12 free actions and a 20-minute budget, serial Wouldwork exhausted
SBCL's 4 GiB heap before finding a continuation. SBCL reported 99.8% of its
heap allocated when garbage collection failed. The last progress report,
at 2 min 30 s, showed 4,354,251 states processed, 1,114,484 entries in the
graph search table and search still in the first of 29 initial branches.
The lower bound had pruned 216,177 times in 810,517 evaluations. These are
interim counts; the fatal exit produced no final search time or totals.

SAT reached its 20-minute deadline from the same state, with a 274.2 MiB
peak Python and Z3 working set. That working-set measurement and SBCL's heap
limit measure different quantities. Neither run found a continuation; the
recorded 12-action finish remains valid.


# Backtracking performance investigation — 2026-10-07

Status: IDB-copy removal and compact physical-entry undo are implemented. Focused
regressions passed, and the user subsequently confirmed a fresh `(test-bt)` run:
13 problems, zero failures, validating compact undo. The bounded planning benchmark
below is complete; further profiling or engine changes require separate approval.

## Findings

1. Before this change, `generate-choices-for-single-combination-bt` in
   `src/ww-backtracker.lisp` unconditionally copied the parent IDB before an effect.
   The saved copy is used only for hash-table (snapshot) updates. Ordinary
   backtracking ASSERT translation returns forward/inverse operation lists.
2. Snapshot producers are real: `make-update-from` in
   `src/ww-enumerator-build.lisp` wraps an IDB. Its fluent, symmetric-batch,
   subset, and finalize action callers first copy the input state and modify
   that copy. They leave the parent's database intact.
3. Backtracking's `detect-path-cycle` checks only immediate inverse operations.
   DFS planning/tree search uses `on-current-path`, checking the successor IDB
   against every ancestor. Equal settings therefore do not imply equal search
   work or identical accepted path counts on cyclic planning problems.
4. The existing `bench-depth&back` helper does not measure allocation or effect
   calls. Its state/cycle counters have different meanings in the two engines,
   and its solution-list length is not a COUNT-mode goal count. COUNT evidence
   must use `*solution-count*`.

## Bounded measurements

SBCL 2.6.9, current working checkout, serial/tree, COUNT, deterministic order,
debug 0, probe NIL. Each problem was staged, settings applied, and REFRESH called
to regenerate actions for the selected algorithm. Timing excludes staging and
compilation but includes `ww-solve` initialization. Each solve had a 10-second
timeout; none timed out. No heuristics were added. No pruning policies were
changed. Algorithm runs were sequential, DFS first.

Counters came from a separate instrumented solve, wrapping action effect
functions and `copy-idb`; timing runs had no such instrumentation. Snapshot
counts distinguish hash-table updates from operation-list updates. Copy counts
include solve-level setup and retained goal examples, not just action generation.

| Problem / cutoff | Algorithm | Effect calls | IDB copies | Snapshot updates | Accepted paths |
|---|---|---:|---:|---:|---:|
| queens4 / 4 | DFS | 232 | 233 | 232 | 48 |
| queens4 / 4 | Backtracking | 232 | 235 | 0 | 48 |
| blocks3 / 6 | DFS | 50 | 51 | 50 | 4 |
| blocks3 / 6 | Backtracking | 286 | 289 | 0 | 24 |
| hanoi / 8 | DFS | 415 | 416 | 415 | 5 |
| hanoi / 8 | Backtracking | 654 | 657 | 0 | 5 |

The queens count is labeled-queen construction paths, not 48 distinct geometric
boards. Blocks and Hanoi are not equal-work overhead comparisons: cycle policies
and cutoff processing differ. DFS may probe a cutoff node's successors to report
whether search was truncated; backtracking returns at the cutoff directly.

Initial individual solve timings were roughly 0.2–1.2 ms, too short for useful
ratios. Queens was therefore repeated without increasing size or depth: one
warmup, then five batches of 200 solves, full GC before each batch. Standard and
trace output were discarded during measurement. Elapsed time used
`get-internal-real-time`; allocation used `sb-ext:get-bytes-consed` differences.

| Measure | DFS | Backtracking |
|---|---:|---:|
| Median elapsed, 200 solves | 0.088993 s | 0.123218 s |
| Median elapsed per solve | 0.445 ms | 0.616 ms |
| Batch elapsed range | 0.086418–0.092285 s | 0.121316–0.125508 s |
| Median bytes allocated, 200 solves | 81,947,360 | 114,835,088 |
| Expansions reported per solve | 185 | 185 |

This baseline shows about 38% more elapsed time and 40% more allocation for
backtracking on this small equal-work case. It does not measure the isolated cost
of copying and does not predict a speedup from removing it. Fixed solve overhead
is included; repeated small solves are not a substitute for a sustained search.
Both measurement processes emitted `BT-INVESTIGATION-COMPLETE` and exited normally.

## First implementation step — implemented

Removed the unconditional `copy-idb` at choice generation; retain the parent IDB
reference instead. Incremental choices continue using their inverse operation
lists. Snapshot choices retain the unchanged parent table as their inverse
snapshot; the existing forward/undo helpers copy snapshots when installing them.

This relies on an explicit effect contract: snapshot-producing effects modify
their own copied state, while incremental effects modify the working state and
return undo operations. All identified enumeration snapshot producers follow
that contract. Snapshot support and copying when installing snapshots remain.

`test/search/backtracking-updates.lisp` adds generation-level checks for an ordinary
incremental choice, two changed snapshot siblings, an incremental child beneath
each accepted snapshot, and constraint-rejected snapshots. Assertions check the
successor database and value, restored parent database and metadata, empty choice
stack, and preservation of the stored snapshots.

The new checks, existing heuristic checks, pruning, goal-chain rejection, move
lower bounds, bounding-function checks, and COUNT regressions all passed in an
isolated SBCL process. The run emitted `BT-COPY-REMOVAL-VERIFIED` and exited normally.

To run the new focused checks from the project directory at the REPL:

```lisp
(load "test/search/backtracking-updates.lisp")
(test-backtracking-updates)
```

The user subsequently ran `(test-bt)` at their REPL: 13 problems, zero failures,
failed problems NIL, return value T. No cycle-policy or heuristic ordering change
is included in this step.

### Controlled before/after result

Repeated the same queens4 COUNT comparison in one process: five batches of 200
solves per version, warmup and full GC as above. The old copy operation was
restored only in a temporary source file outside the project. Each backtracking
version was loaded **after staging and REFRESH**, because staging reloads the
engine. An initial comparison that loaded the baseline before staging measured
the new code twice and was discarded. The corrected run asserted copy counts
of 233 for DFS, 235 for old backtracking, and 3 for new backtracking in separate
instrumented solves. Every timing batch retained the expected count of 48;
backtracking also restored the initial database and emptied its choice stack.

| Median, 200 solves | DFS | Backtracking before | Backtracking after |
|---|---:|---:|---:|
| Elapsed time | 0.090909 s | 0.122699 s | 0.110953 s |
| Bytes allocated | 81,947,360 | 114,835,088 | 100,721,968 |
| IDB copies per solve | 233 | 235 | 3 |
| Accepted paths per solve | 48 | 48 | 48 |

Sorted batch times, seconds:

- DFS: 0.088035, 0.090052, 0.090909, 0.093390, 0.093839.
- Before: 0.121332, 0.121772, 0.122699, 0.122769, 0.123021.
- After: 0.110268, 0.110285, 0.110953, 0.111631, 0.116284.

The change reduced elapsed time by about 9.6% and allocation by 12.3% relative
to old backtracking. New backtracking still took about 22% longer than DFS on
this workload. These are small-problem measurements, including repeated solve
initialization; no claim is made about larger state databases or sustained runs.
Temporary scripts, baseline source, logs, and compiler cache were removed after
recording these results.

## Test and benchmark roles

- Keep `(test-bt)` as the representative correctness suite; do not lengthen all
  its normal runs merely to obtain timing data.
- `test/search/backtracking-heuristic.lisp` covers absent/present heuristics,
  stable ties, constraint rejection, recursive restoration, multiple ASSERTs,
  and heuristic errors. Its existing snapshot check constructs a choice directly
  with identical parent/successor databases: useful for metadata restoration,
  but insufficient to validate generation or a real snapshot state change.
- `test/search/backtracking-prune.lisp` covers pruning, rejected goal-chain
  candidates, lower bounds, bounding modes, and restoration. These are focused
  correctness tests, not sustained performance workloads.
- `test/search/count-solutions.lisp` is relevant to accepted-goal accounting.
- `queens4` provides an unchanged equal-work smoke benchmark; repeated batches
  are preferable to changing its normal problem definition.
- `probs/problem-queensN-csp.lisp` is a promising scalable workload with fixed
  row order, bit masks, forward pruning, and optional symmetry-class counting.
  Its current defaults are N=13 and 16 threads: do not run those defaults for
  this investigation. Propose an explicitly small serial variant first.
- `queens8` uses labeled queens and can multiply equivalent construction paths;
  its FIRST mode also makes timing sensitive to branch order. It is less suitable
  than the row-ordered CSP for a clean scalable comparison.
- Bounded `blocks3` and `hanoi` are useful for studying cycle-policy differences,
  separately from per-branch implementation overhead. No larger Hanoi variant
  was run.

Future benchmark reporting should include elapsed time, allocation/GC, effect
calls, accepted goals, and relevant settings. Preserve native correctness
fixtures; keep longer benchmarks explicitly invoked and bounded.

## Second investigation: N=11 CSP profile

Approved investigation only: no additional engine or authoritative problem
changes. Used external copies of `problem-queensN-csp.lisp` with N=11, zero
workers, COUNT/tree/CSP, depth cutoff 11, and class counting disabled. The forward
pruning hook remained unchanged. Each solve was capped at 10 seconds; none of
the executed solves timed out. Both algorithms completed the full board depth.

The variant needed a distinct Filename header: retaining the original header
caused staging to recover the original N=13 source. A pre-solve size assertion
caught this. Other setup corrections concerned algorithm selection and staging's
NIL return value; no search ran during those failed setup attempts. The final
temporary variants selected the algorithm with a setup SETF before installing
actions, since WW-SET prohibits algorithm declarations inside problem specs.
Before solving, assertions verified N=11, serial execution, CSP, the intended
algorithm, and disabled class counting.

### Equal-work comparison

One warmup followed by three uninstrumented solves per algorithm, with full GC
before each measured solve. Separate wrappers counted precondition calls, effect
calls, and update signatures. Staging and instrumentation were outside timing.

| Measure | DFS | Backtracking |
|---|---:|---:|
| Median elapsed per solve | 0.389241 s | 0.350536 s |
| Median allocated bytes | 559,451,728 | 558,076,176 |
| Effect calls | 127,441 | 127,441 |
| Precondition calls | 836,814 | 836,814 |
| Update signature calls | 0 | 0 |
| Accepted boards | 2,680 | 2,680 |

DFS times: 0.388127, 0.389485, 0.389241 seconds. Backtracking times: 0.349575,
0.350536, 0.353712 seconds. Reported GC times were zero for DFS and 0, 0.015625,
0 seconds for backtracking. The counters were asserted equal across algorithms;
backtracking also restored its initial database and emptied its choice stack.
The run emitted `BT-PROFILE-COMPLETE` and exited normally.

Backtracking was about 9.9% faster here, with nearly equal total allocation.
This differs from the small labeled-queen planning benchmark: CSP disables
signature generation, and larger databases make copying relatively more costly.
The measurements do not separately quantify those two explanations. In
particular, this profile says nothing about the cost of planning-mode signatures.

### What the profile identified

SBCL's instrumenting profiler identified database writes as the principal
allocation hotspot. It added substantial overhead (roughly 0.74 seconds for
DFS and 1.88 seconds for backtracking in the broad profile), so its adjusted
function timings must not be treated as reliable percentages of normal runtime.
Zero adjusted time does not mean a function is free. Calls and allocation
attribution are more useful here.

For each of 127,441 backtracking effects:

- Three incremental writes call UPDATE-BT.
- Undo passes three inverse literals through REVISE/UPDATE: two restorations
  and one removal.
- Two old fluent-value tuples are reconstructed as literals. The broad profile
  recorded 254,882 reconstructions, allocating about 14.2 MB in that run.
- REGISTER-CHOICE-BT allocated about 13.8 MB and choice generation about 17.6 MB
  in the broad profile. These are separate from nested profiled callees.

The follow-up write profile recorded 637,205 ADD-PROPOSITION calls, 1,911,615
ADD-PROP/FOLD-STORE calls, and 127,441 removals. ADD-PROPOSITION itself was
attributed about 358 MB, excluding separately profiled fluent extraction.
GET-PROP-FLUENTS accounted for about 87.8 MB. These instrumented allocation
figures are approximate and are not substitutes for total uninstrumented bytes.
No deep-hash calls were observed in this backtracking write profile.

### Concrete benchmark issue: unintended symmetry expansion

The `occupied` relation declares three integer fluents storing ordered masks for
columns, sum diagonals, and difference diagonals. The relation installer infers
symmetry between repeated argument types unless the name ends in `>`.
Live inspection confirmed:

```lisp
;; Entry in *symmetrics*:
(occupied ((0 1 2)))
;; Fluent indices:
(1 2 3)
```

`ADD-PROPOSITION` therefore generates six permutations of each mask tuple and
writes all six to the same fluentless key. A one-write audit for `(1 2 4)`
observed permutations `(4 2 1)`, `(2 4 1)`, `(1 4 2)`, `(4 1 2)`, `(2 1 4)`,
then `(1 2 4)`. The final stored value was the original `(1 2 4)`.

That explains the 15 physical stores per backtracking effect: six for the new
mask tuple, one each for the row assignment and next row, then six for restoring
the old masks and one for restoring next row. Removing the row assignment adds
one removal. DFS also incurs mask permutation expansion, but does not undo.

Source pointers: `install-dynamic-relations` in `src/ww-installer.lisp`,
`add-proposition` and `generate-proposition-permutations` in `src/ww-support.lisp`,
and the occupied declaration and uses in `probs/problem-queensN-csp.lisp`.
The targeted write profile and single-write audit emitted
`BT-WRITE-PROFILE-COMPLETE` and `BT-WRITE-AUDIT-COMPLETE` respectively.

### Directed-relation correction — approved and implemented

Renamed this ordered relation to `occupied>` at its five declaration/use sites
in `problem-queensN-csp.lisp`, and updated `doc/search/queens-symmetry.md`.
Dependency checking found no direct references in the existing queens test
scripts requiring edits. The N=11 board count and search-work counts remained
unchanged in the bounded comparison below. This makes the intended roles explicit and removes
permutation work that obscures the cost of the backtracking mechanism itself.
Do not change the global symmetry inference policy as part of that correction.

After that baseline is established, the main engine-level candidate is a compact
undo record containing the affected database key, previous value, and presence
flag. It could avoid reconstructing old literals and passing them through
relation lookup, key conversion, and symmetry expansion during undo. This is a
larger design change: records must capture all actual writes, including symmetric,
bijective, and complementary entries, and preserve change/hash bookkeeping.
The current profile supports investigating it, not yet implementing it blindly.

No new test suite or permanent benchmark variant was added. External variants,
profiling scripts, logs, and isolated compiler cache were removed after recording
the results. The authoritative N=13/16-worker problem settings remain unchanged.

## Directed-relation retest

Used an external temporary copy of the current checkout, with the ASDF source
directory pinned and asserted. Two registered N=11 test copies differed only in
whether the relation was `occupied>` or `occupied`; their normal initial defaults
otherwise matched the original problem. Neither had an algorithm declaration.
Staged each normally, then used `ww-set` to select zero workers, the algorithm,
cutoff 11, COUNT, and tree mode. Set the problem-specific class-count switch to
NIL after those reloads. N=11, serial execution, and the intended algorithm were
asserted before solving. This follows the user's clarification: STAGE loads
defaults; WW-SET applies individual search-parameter overrides.

Each case had one warmup, three uninstrumented solves with full GC before each,
and one separately instrumented solve. Every solve had a 10-second timeout.
Runs were directed DFS, directed backtracking, undirected DFS, then undirected
backtracking. No timeout occurred and no larger search was attempted.

| Median per solve | Undirected DFS | Directed DFS | Undirected BT | Directed BT |
|---|---:|---:|---:|---:|
| Elapsed seconds | 0.384244 | 0.215758 | 0.356483 | 0.147501 |
| Allocated bytes | 559,451,728 | 239,382,304 | 558,076,176 | 131,961,072 |
| Effect calls | 127,441 | 127,441 | 127,441 | 127,441 |
| Precondition calls | 836,814 | 836,814 | 836,814 | 836,814 |
| Accepted boards | 2,680 | 2,680 | 2,680 | 2,680 |

Sorted elapsed times:

- Directed DFS: 0.215354, 0.215758, 0.217990 seconds.
- Directed BT: 0.146520, 0.147501, 0.148499 seconds.
- Undirected DFS: 0.381923, 0.384244, 0.388393 seconds.
- Undirected BT: 0.350722, 0.356483, 0.356967 seconds.

The directed relation reduced backtracking time by about 58.6% and allocation
by 76.4%. On the corrected problem, backtracking took about 31.6% less time than
DFS and allocated 44.9% fewer bytes. These are bounded N=11 measurements, not
claims about all problems or N=13.

Validation passed:

- All four cases asserted 2,680 boards and identical effect/precondition counts.
- The directed relation was present in the relation table and absent from the
  inferred-symmetry table; the reference retained its three-position symmetry.
- The existing independent encoding check passed 638 prefixes at N=11.
- First-row reflection pruning passed; forward pruning passed 3,106 prefixes.
- Independent symmetry regressions passed for N=1 through N=10.
- Retained examples passed the independent queen-placement legality check.
- Backtracking restored its initial database and emptied its choice stack.
- The process emitted `QUEENS-DIRECTED-RETEST-PASSED` and exited normally.

No engine change or new permanent benchmark/test file was needed. The original
N=13 default and 16-worker default are preserved. Temporary test checkout,
variants, scripts, logs, and compiler cache were removed after recording results.
Next boundary: user REPL check, then approval for compact-undo design work.

## Compact undo implementation and bounded comparison (2026-10-07)

Following approval of the compact-undo design, backtracking now records physical
database writes and restores them newest first. This avoids reconstructing and
re-expanding inverse propositions during rollback. Planning still builds inverse
literals for its unchanged immediate-inverse cycle check. CSP omits those literals
and their list cells. Forward changes remain available for reapplication and replay.

Each effect application owns one frame. A single incremental result retains that
frame; multiple results restore the whole generation frame before exploring siblings.
Reapplication, including after heuristic scoring, creates fresh records. Snapshot
choices retain their copy-based handling. Errors during effects, reapplication, or
registration restore the database and applicable parent metadata. Missing incremental
undo records signal an error rather than falling back to literal inverses.

Physical capture includes symmetric, bijective, complementary, and translated static
writes. Folding helpers reuse their existing old-entry lookup; missing removals create
no record. Repeated stores remain separate records. No pooling, key deduplication,
new cycle pruning, or change to problem defaults was introduced. See
`backtracking-compact-undo-design.md` for the consumer audit and capture boundary.

### Validation

`test/search/backtracking-updates.lisp` now checks exact restoration for repeated
writes, present NIL, absent deletion, idempotent addition, asymmetric starting
contents of symmetric relations, displaced bijective partners, complements, and
static tables. It also checks overlapping siblings, snapshot children/siblings,
constraint rejection, partial-effect errors, registration errors, and errors during
reapplication. Standard/split hash accumulators, change flags, choice-stack cleanup,
metadata, and cache invalidation have focused checks.

The update, heuristic, pruning, rejected-goal-chain, minimum-steps, bounding, and
COUNT checks all passed. The final process emitted
`COMPACT-UNDO-EXISTING-CHECKS-PASSED`. The user subsequently confirmed a fresh
13/13 `(test-bt)` result after this implementation, with zero failures.

### Measurements

Compared separate external copies of the checkout, with the pre-change versions
of the four initially edited engine files restored in the baseline. The baseline
already included the earlier IDB-copy removal and directed queens relation.
ASDF's exact-name system searcher was pinned to each copy, and the selected source
directory asserted. An initial source-selection assertion stopped a setup attempt
before any measurements; its output was not used.

The registered temporary queens problem used N=11 and otherwise retained native
defaults. After STAGE, WW-SET selected zero threads, the algorithm, depth 11,
COUNT, and tree mode. Class counting was disabled after parameter reloads. Each
case had one warmup and five uninstrumented solves, with full GC before each,
followed by a separate instrumented solve. Every solve had a 10-second timeout;
none timed out. Runs were baseline DFS/BT, then final DFS/BT. These short sequential
runs show a modest local benefit, not a general performance guarantee.

| Median per solve | Before DFS | After DFS | Before BT | After BT |
|---|---:|---:|---:|---:|
| Elapsed seconds | 0.212086 | 0.211592 | 0.144692 | 0.135680 |
| Allocated bytes | 239,382,160 | 239,382,160 | 131,960,928 | 125,859,696 |
| Effect calls | 127,441 | 127,441 | 127,441 | 127,441 |
| Precondition calls | 836,814 | 836,814 | 836,814 | 836,814 |
| Accepted boards | 2,680 | 2,680 | 2,680 | 2,680 |

Elapsed samples in execution order:

- Before DFS: .212086, .211313, .212526, .210625, .212280.
- After DFS: .211592, .210406, .211096, .213106, .213127.
- Before BT: .144759, .147444, .144030, .144692, .143571.
- After BT: .133241, .135680, .137190, .137213, .133676.

BT median time decreased 6.2%, and allocation decreased 4.6%. DFS allocation was
unchanged and its timing essentially unchanged. An intermediate implementation
with redundant lookups and NIL inverse-list cells showed little allocation benefit;
those two costs were removed within this step.

Planning comparisons used serial BT, tree mode, EVERY, Blocks3 cutoff 6 and
three-disk Hanoi cutoff 8. Before and after produced identical ordered paths:

| Problem | Paths | Effect calls | Precondition calls |
|---|---:|---:|---:|
| Blocks3 | 24 | 291 | 1,943 |
| Hanoi | 5 | 670 | 2,027 |

These are bounded search/path comparisons, not independent full-path replay proofs.
Both benchmark processes emitted `COMPACT-UNDO-BENCH-PASSED`; BT restored its
initial database and emptied its choice stack. Temporary checkouts, benchmark
scripts, logs, generated problem copies, and compiler caches were removed after
recording the evidence. The subsequent user `(test-bt)` check passed; the next
approved measurement is recorded below.

## Bounded planning measurement: triangle-xyz (2026-10-07)

Compared DFS with **current compact-undo BT**, not pre/post compact undo. The
deleted pre-change checkout was not reconstructed, and Git HEAD was not used as
a baseline. No engine or problem changes were made.

The unchanged N=5 problem starts with 14 pegs and needs 13 jumps to reach one.
Each jump removes two occupancy facts, adds one, and decrements peg-count.
Serial planning/tree/COUNT search was bounded at depth 10. Zero accepted goals
are expected; this is partial exploration, not proof that the goal is unreachable.

### Method and execution

Used SBCL 2.6.9 with 4096 MB dynamic space and an external copy of the current
working tree. Pinned ASDF's exact-name Wouldwork system searcher and asserted
the source directory before loading and before each solve. Source hashes for
the copied engine, problems, and technology files matched the working checkout.
STAGE loaded native defaults, then individual WW-SET overrides selected zero
threads, BT or DFS, tree, COUNT, cutoff 10, deterministic order, debug 0, and
probe NIL. REFRESH regenerated actions. Native pruning and heuristic settings
were unchanged (no heuristic or lower-bound hook).

One BT pilot completed in 1.824165 seconds. Then five uninstrumented solves per
algorithm measured elapsed time, allocated bytes, and SBCL GC time, with full GC
before each solve. Timing included WW-SOLVE initialization and excluded staging,
compilation, and post-run checks. Standard and trace output were suppressed.
BT timings ran first; DFS timings ran in a later process. There was no additional
warmup in either measurement process. First measured allocations were slightly
higher than the remaining four samples, so medians are reported.

Separate instrumented solves wrapped the action precondition/effect functions
and counted effects by parent time (unit-duration jumps make this the depth).
A final restoration-only solve compared the returned BT state against an
independent frozen signature captured immediately before SEARCH-BACKTRACKING.
Every solve had a 10-second timeout; none timed out. No depth increase occurred.

Setup-only failures involved an ephemeral temporary path and compiler-cache
configuration; they ran no searches. The final cache used explicit directory
translations from D:/quicklisp/ and the external checkout into separate external
cache roots, preserving relative paths. Mapping all files into one flat cache
caused filename collisions on reuse and was discarded. The first counting
attempt also stopped before its first effect because the harness indexed an
array with floating-point state time; converting the unit-duration time to an
integer corrected the harness. The five completed BT timings were not repeated.

### Results

| Measure | DFS | Current BT |
|---|---:|---:|
| Median elapsed seconds | 1.524634 | 1.858182 |
| Elapsed range seconds | 1.519064–1.539017 | 1.850213–1.865210 |
| Median allocated bytes | 1,017,607,776 | 1,625,893,600 |
| Median reported GC seconds | 0 | 0 |
| Effect calls | 713,553 | 713,552 |
| Precondition calls | 28,386,540 | 28,386,450 |
| Accepted goals (`*solution-count*`) | 0 | 0 |
| Reported program cycles | 713,553 | 315,405 |
| Reported states processed | 713,553 | 315,406 |
| Reported cutoff hits | 398,148 | 0 |

Elapsed samples in execution order:

- BT: 1.853106, 1.850213, 1.863587, 1.865210, 1.858182 seconds.
- DFS: 1.526011, 1.539017, 1.522179, 1.519064, 1.524634 seconds.
- BT allocated bytes: 1,626,639,616 followed by four samples of 1,625,893,600.
- DFS allocated bytes: 1,019,331,888 followed by four samples of 1,017,607,776.
- BT GC seconds: 0, 0.015625, 0, 0, 0.015625.
- DFS GC seconds: 0, 0, 0.015625, 0, 0.015625.

BT took 21.9% more elapsed time and allocated 59.8% more than DFS in these
bounded samples. This does not measure compact undo's isolated benefit or
regression relative to the previous implementation, and sequential runs across
processes are not a randomized performance study.

Both algorithms had identical effect counts at parent depths 0 through 9:
`2, 8, 42, 222, 1074, 5072, 21530, 75758, 211696, 398148`.
DFS additionally called 90 preconditions and one effect at parent depth 10 to
confirm truncation. `df-bnb1` probes cutoff nodes only until one successor proves
truncation. BT's `backtrack` returns immediately at the cutoff, before updating
statistics. Its cutoff-hit count remains zero and its truncation flag remains
NIL. Therefore native cycle/state totals are not comparable units of work, and
BT's flag must not be read as proof of complete search. The observed action work
matches below the boundary; each move reduces peg-count, precluding ancestor
cycles in this workload.

### Restoration and completion

All completed measurement runs preserved both static tables and had zero accepted
goals. BT also emptied its choice stack. The initial post-solve comparison against
the live start state was insufficiently independent because the IDB may be shared.
The final restoration audit instead froze IDB contents, name, time, value,
heuristic, and instantiations before BT began, then checked exact equality for
both the returned BT state and the start state. It also verified both static
tables and empty choice stack. This checks full-search return to the parent;
it does not separately snapshot every intermediate undo.

The measurement continuation emitted `TRIANGLE-BENCHMARK-PASSED`; the independent
audit emitted `TRIANGLE-FROZEN-RESTORATION-PASSED`. Both exited normally. Audit and
instrumented timings are excluded from the medians above. Temporary checkout,
scripts, logs, and compiler caches were removed after recording results.

The subsequent bounded planning-specific profile was approved and is recorded
below. No cutoff-reporting fix, new pre/post baseline, or further optimization
has been implemented.

## Planning-specific profile: triangle-xyz (2026-10-07)

Following approval, ran two selected-function SB-PROFILE passes on current BT,
using the same unchanged N=5, serial planning/tree/COUNT, cutoff-10 settings.
Each pass performed one solve with a 10-second timeout. Neither timed out; no
depth increase or timeout retry occurred. No engine or problem source was edited.

Used a fresh external working-tree copy, a distinct WOULDWORK_INSTANCE, and
external compiler caches with separate directory mappings for dependencies and
the copied project. Pinned the exact-name ASDF system searcher, cleared/reloaded
the system definition, and asserted both source and translated cache roots.
STAGE preceded individual WW-SET overrides and REFRESH. Source hashes for the
copied engine, problems, and technology files matched the working checkout.
Each profiling pass began after staging/compilation, reset the profiler, and
performed full GC. Search output was suppressed.

### First pass: signatures, effects, rollback, registration

| Profiled function | Calls | Attributed bytes | Adjusted seconds |
|---|---:|---:|---:|
| UPDATE-SET-SIGNATURE | 1,427,104 | 795,731,168 | 0.603 |
| JUMP-EFF-FN | 713,552 | 570,058,032 | 0.255 |
| RESTORE-BT-UNDO | 713,552 | 0 | 0.130 |
| REGISTER-CHOICE-BT | 713,552 | 72,938,272 | 0.020 |

The signature function ran twice per effect, once for the forward operations
and once for inverse literals. It constructs a fresh EQUAL hash table to suppress
duplicate operations, then returns a three-element count/XOR/sum signature.
Its attributed allocation is about 49% of the prior uninstrumented BT median
allocation. That makes signature construction the leading allocation target in
this profile; it does not establish how much total runtime a replacement saves.

Direct physical rollback allocated zero bytes in this pass. The effect function's
570 MB attribution includes its unprofiled callees, so the second pass split out
write recording and inverse-fluent reconstruction.

The first harness reported 2.926899 seconds and 1,626,004,032 bytes, but those
totals included profiler reporting/calibration and unprofiling after the solve.
They are not clean solve measurements and must not replace the benchmark medians.
The search itself completed within its 10-second timeout. SB-PROFILE estimated
0.89 seconds of instrumentation overhead.

### Second pass: writes, undo records, and inverse literals

| Profiled function | Calls | Attributed bytes | Adjusted seconds |
|---|---:|---:|---:|
| JUMP-EFF-FN | 713,552 | 285,896,560 | 0 |
| UPDATE-BT | 2,854,208 | 72,706,656 | 0.164 |
| RECORD-BT-UNDO | 2,854,208 | 136,358,256 | 0 |
| RECONSTRUCT-LITERAL-WITH-FLUENT-VALUES | 713,552 | 72,676,688 | 0 |
| BEGIN-BT-UNDO | 713,552 | 33,079,280 | 0 |
| DETECT-PATH-CYCLE | 713,552 | 0 | 0.022 |

The four authored writes per jump account for 2,854,208 UPDATE-BT and physical
recording calls. Each effect creates one undo frame and reconstructs one old
peg-count literal. The table separates nested profiled functions: JUMP-EFF-FN's
second-pass attribution excludes the now-profiled write functions, unlike its
first-pass value. Do not add both passes' effect allocations together. Allocation
attribution is approximate and can shift across function boundaries; the
reconstruction row is not a complete measurement of all inverse-list allocation.

This pass measured 4.177030 seconds and 1,626,051,216 bytes around the instrumented
solve, including the restoration audit but excluding profiler reporting. The
profiler estimated 2.14 seconds of instrumentation overhead. Its adjusted times
for several allocating functions clamped to zero; zero does not mean free.
Therefore these times cannot reliably apportion normal runtime. Calls and
allocation are the stronger evidence.

### Validation and next boundary

Both passes asserted 315,405 native BT program cycles and zero accepted goals;
the effect counts matched the prior 713,552-call benchmark. Immediately before
SEARCH-BACKTRACKING, the harness froze the root IDB and name/time/value/heuristic/
instantiations. Both passes restored that exact signature in the returned BT
state and the start state, preserved both static tables, and emptied the choice
stack. Each emitted `TRIANGLE-PROFILE-RESTORATION-PASSED` and
`TRIANGLE-PLANNING-PROFILE-PASSED`, then exited normally.

The evidence favors investigating signature allocation before undo-record pooling
or deduplication. A potential next design is to avoid a fresh hash table for each
short operation list while preserving order independence, duplicate suppression,
and the exact set-equality check following a signature match. The general case
must still be considered: an operation-list scan can cost more on long lists.
No cycle checks or inverse literals were removed, and no replacement was built.
Any design/implementation and its bounded before/after measurements remain a
separate approval boundary. Temporary copied checkout, profiling scripts, logs,
and compiler caches were removed after recording these results.

## Signature-allocation design and implementation (2026-10-07)

Status: approved design implemented; focused checks and the user's subsequent
13-problem `(test-bt)` run passed with zero failures. The approved bounded
before/after comparison below shows lower time and allocation on triangle-xyz.

### Recommendation

Change duplicate detection inside UPDATE-SET-SIGNATURE for short operation lists.
For at most eight operations, traverse the existing list tails and include an
operation only if no EQUAL operation appears later in the list. MEMBER with
:TEST #'EQUAL can perform that check without constructing a separate seen set.
For longer lists, retain the existing EQUAL hash-table method. A bounded tail
check, such as NTHCDR 8 on the existing proper-list input, selects the path without
traversing a long list just to compute its length.

Both paths feed the same unique-count, SXHASH XOR, and unbounded integer sum
accumulators and return the existing three-element list. A single common
accumulation loop should keep the logic clear; no local LABELS/FLET, new user
setting, shared scratch table, or new signature representation is needed.
Document eight as an internal initial cutoff, not a benchmark-derived optimum.

With four unique operations, suffix scanning performs at most six EQUAL
comparisons per signature. At eight operations, the bound is 28 comparisons.
Larger updates retain hash-based duplicate suppression, avoiding an unbounded
quadratic scan. EQUAL itself can cost more for large nested literals, so even
the short-list path needs measurement. This proposal does not claim that eight
is universally the fastest threshold.

### Semantic contract

- Return exactly the old signature for the same operation list within the same
  Lisp process: number of distinct EQUAL operations, XOR of their SXHASH values,
  and arithmetic sum of those values. The empty list remains `(0 0 0)`.
- Preserve order independence and duplicate suppression, including separately
  allocated but EQUAL literals. Processing the last instead of first occurrence
  is equivalent because EQUAL objects have equal SXHASH values and the three
  accumulators are order independent.
- Do not mutate, sort, copy, or deduplicate the caller's operation list in place.
  Preserve integer arithmetic; do not introduce truncation or fixnum-only sums.
- Keep both forward/inverse signature slots and planning inverse literals.
  Keep DETECT-PATH-CYCLE's final ALEXANDRIA:SET-EQUAL check unchanged. A signature
  match is still only a prefilter, never proof that two operation sets match.
- Keep CSP's signature omission, snapshot handling, physical undo, successor
  ordering, and heuristic/nonheuristic rejection paths unchanged.

The current caller audit found signature creation only in CHOICE-FROM-UPDATE-BT,
twice for incremental planning choices. DETECT-PATH-CYCLE is the consumer and
is called by both SCORE-CHOICE-BT and EXPLORE-CHOICE-BT. No other production
signature consumers or existing direct signature tests were found.

### Focused validation plan

Extend test/search/backtracking-updates.lisp, rather than introduce another test
folder or suite. Use a test-only copy of the current hash-table implementation
as a differential oracle for exact signature equality. Cover empty/singleton
lists, positive/negative/fluent literals, reordered lists, repeated literals,
separately allocated EQUAL literals, and lengths 7, 8, 9 plus a longer list.
Include repeated literals on both sides of the short/long boundary and assert
that inputs are unchanged. A small deterministic generated set of operation
lists should supplement representative cases.

Test the consumer as well: an immediate inverse with reordered/duplicate literals
is detected; a noninverse is not. Deliberately give unequal operation sets the
same stored signature in a constructed choice pair, and require exact comparison
to reject that false match. This exercises collision safety without depending
on finding an actual SXHASH collision. Check the existing CSP bypass.

Run the existing update, heuristic, and pruning checks to cover restoration and
both cycle-check call paths. Then stop for the user's `(test-bt)` REPL boundary.
The expected production edit is confined to src/ww-backtracker.lisp, with focused
tests and documentation updated alongside it.

### Separate performance boundary

Before any implementation edit, preserve a verifiable external baseline from the
current working tree, including source hashes. Do not use Git HEAD as a proxy or
restore unrelated files. Keep that baseline outside the repository until the
approved comparison is recorded, then remove temporary artifacts.

After correctness approval, propose the same unchanged triangle N=5, serial
planning/tree/COUNT, cutoff-10 before/after BT comparison, with every solve capped
at 10 seconds and no automatic timeout retry or depth increase. Compare allocation,
elapsed time, effect/precondition counts, and independently frozen-parent
restoration. Timing must exclude instrumentation. Small direct signature timings
around the threshold and for longer lists can check the dispatch tradeoff without
larger searches. The prior 796 MB attribution includes the whole signature
function; retaining its result list and arithmetic means it is not a promised
796 MB saving. Retain the change only if measured benefit justifies its complexity.

### Implementation and focused results

UPDATE-SET-SIGNATURE now uses suffix membership for lists of at most eight
operations and allocates the existing EQUAL hash table only for longer lists.
Both paths share one accumulation loop and preserve the original three-element
signature. No other production function changed in this step.

Extended test/search/backtracking-updates.lisp with the original implementation
as a test-only oracle. Checks cover 377 representative/generated input lists,
their reversals and duplicated copies, threshold boundaries, input preservation,
immediate inverse detection, CSP bypass, and rejection of a deliberately forced
signature collision by the unchanged exact comparison.

An isolated external copy passed the signature checks and existing update,
heuristic, pruning, rejected-goal-chain, minimum-steps, and bounding checks. The
process emitted `SIGNATURE-CHANGE-FOCUSED-CHECKS-PASSED` and exited normally.
Source and compiler-cache directories were asserted before loading. No triangle
benchmark, full `(test-bt)`, or additional performance search was run in this step.

Before edits, the full working-tree baseline was copied and every copied file's
SHA256 verified against its source. It was retained for the comparison at:

`C:/Users/user/.codex/visualizations/2026/10/07/01a11861-500a-7d71-b065-7764ee8e205c/signature-change/baseline/`

The adjacent `baseline-sha256.json` recorded the source manifest. Temporary
validation checkout, harness, logs, and compiler caches were removed after
recording results. The baseline and manifest were retained until the approved
comparison below completed, then removed. Unrelated working-tree changes were
preserved.

The user also raised path-wide cycle pruning. DFS's ON-CURRENT-PATH checks cached
IDB hashes and verifies equality against ancestor IDBs. BT currently checks only
immediate inverse operation sets. Extending BT could prune longer cycles, but
requires a collision-safe way to verify ancestor states while its IDB is mutated
in place, plus a clear state-equivalence policy for time/history-sensitive problems.
It also changes accepted path counts. It cannot help triangle-xyz, where peg-count
strictly decreases. No path-wide pruning was added; that is a separate design and
performance question after this signature-only change is evaluated.

## Signature-only before/after comparison (2026-10-07)

The user reported `(test-bt)`: 13 problems, zero failures, failed problems NIL,
return value T, then approved this bounded performance comparison.

Before execution, every file in the preserved pre-change baseline was verified
against its saved SHA256 manifest. A fresh external copy represented the current
checkout. Comparing the engine/problem/technology sources showed only the
approved UPDATE-SET-SIGNATURE change (generated staging files were excluded).
The relevant source hashes for src/ww-backtracker.lisp were:

- Before: `7FB5A32B2DCDD453CC9D972AFCD9BF5837A192A61E462357AFCD0558EA236496`
- After: `1736F6F339B5A21639E8282C7AE295D65A197344D69947FEB664598BB9E7EB4D`

Both versions already include physical compact undo. This isolates the new
short-list signature implementation; it is not another compact-undo comparison.

### Method

SBCL 2.6.9, 4096 MB dynamic space, unchanged triangle-xyz N=5. STAGE preceded
individual WW-SET overrides and REFRESH: serial, BT, planning, tree, COUNT,
cutoff 10, deterministic order, debug 0, probe NIL. Source and translated cache
roots were pinned and asserted separately for each external checkout.

Ran five uninstrumented solves followed by one instrumented counting/restoration
solve per version. No extra warmup solve was run. Each solve had full GC beforehand
and a 10-second timeout; none timed out. The before process completed first, then
the after process. Timing included WW-SOLVE initialization and excluded staging,
compilation, explicit pre-run GC, and post-run checks. Standard/trace output was
suppressed. No cutoff increase, full solution run, or unrelated engine edit occurred.

### Results

| Measure | Before signature change | After signature change |
|---|---:|---:|
| Median elapsed seconds | 1.875937 | 1.527806 |
| Elapsed range seconds | 1.866204–1.878654 | 1.524522–1.529439 |
| Median allocated bytes | 1,625,893,504 | 1,009,598,480 |
| Median reported GC seconds | 0.015625 | 0 |
| Effect calls | 713,552 | 713,552 |
| Precondition calls | 28,386,450 | 28,386,450 |
| Native BT program cycles | 315,405 | 315,405 |
| Accepted goals | 0 | 0 |

Elapsed time decreased **18.6%** and allocation **37.9%**, saving 616,295,024
bytes per solve at the medians. These are bounded local measurements from
sequential processes, not a general speed guarantee or evidence that eight is
the optimal threshold. No new DFS run was included; the earlier DFS measurements
should not be treated as a paired control for this comparison.

Samples in execution order:

- Before seconds: 1.867502, 1.875937, 1.878654, 1.866204, 1.876349.
- After seconds: 1.529439, 1.527806, 1.526383, 1.528953, 1.524522.
- Before bytes: 1,627,365,488 followed by four samples of 1,625,893,504.
- After bytes: 1,010,995,312 followed by four samples of 1,009,598,480.
- Before GC seconds: 0.015625, 0, 0.015625, 0.015625, 0.031250.
- After GC seconds: 0, 0, 0, 0, 0.

### Validation and completion

Every solve asserted the configuration, zero accepted goals, unchanged static
tables, empty choice stack, and expected native cycle count. The separate
instrumented solve for each version asserted the exact effect/precondition
counts above and effect counts at parent depths 0 through 9:
`2, 8, 42, 222, 1074, 5072, 21530, 75758, 211696, 398148`, with none deeper.
It independently froze the root IDB and name/time/value/heuristic/instantiations
immediately before BT began, then verified exact restoration of both the returned
BT state and start state. These instrumented timings were excluded from medians.

Both processes emitted `FROZEN-PARENT-RESTORATION-PASSED` and
`SIGNATURE-BENCHMARK-PASSED` with their before/after labels and exited normally.
Zero goals are expected at cutoff 10; the one-peg goal requires 13 jumps.

Recommendation: retain the signature change. The focused checks, subsequent
13/13 user suite, and this equal-work comparison support it. Path-wide cycle
pruning remains a separate unimplemented proposal. The temporary baseline,
manifest, after checkout, comparison harness, logs, and compiler caches were
removed after recording the results. No further optimization or search was run.

## Next design boundary

The user approved the path-wide cycle-pruning design and then its implementation
with focused tests. The opt-in PATH mode is now implemented; IMMEDIATE remains
the default. See `backtracking-path-cycle-design.md` for goal-first ordering,
exact scratch ancestor reconstruction, compatibility limits, parameter usage,
and validation evidence. The isolated run emitted `PATH-CYCLE-FOCUSED-CHECKS-PASSED`.
The user subsequently reported `(test-bt)`: 13 problems, zero failures, failed
problems NIL, return value T. The separately approved comparison follows.

## Current-mode Blocks3 and Hanoi comparison (2026-10-07)

Compared IMMEDIATE BT, PATH BT, and DFS in the current implementation. This is a
pruning-policy comparison, not a pre/post compact-undo or pre/post path-code
comparison. No engine changes or default changes were made in this step.

### Setup and validation

SBCL 2.6.9, 4096 MB dynamic space. Unchanged Blocks3 cutoff 6 and three-disk Hanoi
cutoff 8; serial planning, tree search, deterministic order, debug 0, no probe,
no heuristic. STAGE preceded individual WW-SET overrides and REFRESH. Exact ASDF
source and compiler-cache roots were pinned and asserted in an external copy.
The seven changed path-mode engine files and both problem files matched the main
checkout after the run. Current ww-backtracker.lisp SHA256:
`3F2CDC9FEEC9200C749E2E339704C238ABC37FB7F88D2AC590E0022C359B9E5B`.
The preserved pre-path baseline's 438 files matched its manifest; that baseline
was not executed, and Git HEAD was not used as a performance baseline.

First ran one instrumented EVERY solve for each problem/mode, then compared its
complete action-path set against a separate enumerator. This enumerator used
three integer supports for Blocks3 and three integer peg positions for Hanoi,
with direct legal-move and goal rules transcribed from the unchanged models. It
did not call engine preconditions, effects, hashing, or undo. It rejected a return
to the preceding state for IMMEDIATE, or any ancestor for PATH/DFS, accepted goals
before ancestor pruning, and stopped at the same cutoff. Duplicate output paths
were also forbidden. All six exact path-set comparisons passed:

| Problem | Immediate BT | Path BT | DFS |
|---|---:|---:|---:|
| Blocks3 cutoff 6 | 24 | 4 | 4 |
| Hanoi cutoff 8 | 5 | 5 | 5 |

Every retained solution was replayed through `%validate-solution`: 47 successful
replays across the six runs. The Blocks3 count reduction is an intentional policy
difference, not lost valid acyclic paths. Hanoi eliminates cyclic branches without
changing the accepted path set at this bound.

Then ran five uninstrumented COUNT solves and one instrumented COUNT solve for
each case. All COUNT results matched the independent expectations above, using
`*solution-count*`. Execution order was Blocks3 IMMEDIATE/PATH/DFS, followed by
Hanoi IMMEDIATE/PATH/DFS, in a single process. There was no additional warmup solve;
the EVERY checks had already run, and each COUNT case was restaged. Full GC preceded
each solve. Timing included WW-SOLVE initialization and reporting to suppressed
output streams, but excluded staging/compilation, explicit GC, and post-run checks.
All 42 solves had a 10-second timeout. None timed out; none was retried or deepened.

The eight instrumented BT solves independently froze the root IDB and
name/time/value/heuristic/instantiations immediately before SEARCH-BACKTRACKING,
then required exact restoration of both start and returned working states.
All solves checked unchanged static tables; BT also checked empty choice and
fingerprint stacks and an inactive static-write guard. All checks passed.

### COUNT measurements

Medians of five uninstrumented samples; allocation is SBCL GET-BYTES-CONSED delta.
All timed samples reported zero GC time.

| Problem / mode | Median ms | Median bytes | Effects | Preconditions | Goals |
|---|---:|---:|---:|---:|---:|
| Blocks3 immediate BT | 0.538 | 818,832 | 286 | 1,926 | 24 |
| Blocks3 path BT | 0.228 | 294,800 | 50 | 378 | 4 |
| Blocks3 DFS | 0.206 | 229,264 | 50 | 378 | 4 |
| Hanoi immediate BT | 0.717 | 785,936 | 654 | 1,998 | 5 |
| Hanoi path BT | 0.629 | 884,160 | 412 | 1,242 | 5 |
| Hanoi DFS | 0.639 | 850,928 | 415 | 1,251 | 5 |

Effect/precondition counts come from the separate instrumented COUNT solve.
EVERY's normal solution reporting replays paths and adds calls, so its totals
were not used as search-work measurements. Compared with immediate BT, path BT
reduced effect calls by 82.5% for Blocks3 and 37.0% for Hanoi. Blocks3 PATH/DFS
work matched. Hanoi DFS's additional 3 effects and 9 preconditions reflect its
cutoff truncation probe; its accepted paths still matched PATH exactly.

| Problem / mode | Native cycles | Native states | Reported repeats | Cutoff hits | Truncated |
|---|---:|---:|---:|---:|---|
| Blocks3 immediate BT | 107 | 108 | 0 | 0 | NIL |
| Blocks3 path BT | 21 | 22 | 26 | 0 | NIL |
| Blocks3 DFS | 21 | 51 | 26 | 0 | NIL |
| Hanoi immediate BT | 222 | 223 | 0 | 0 | NIL |
| Hanoi path BT | 138 | 139 | 189 | 0 | NIL |
| Hanoi DFS | 219 | 413 | 189 | 81 | T |

These native counters have different accounting between BT and DFS. IMMEDIATE's
inverse rejections are not reported in the repeat counter. BT returns at the
depth boundary before counting cutoff hits; its NIL truncation flag is not proof
of an exhaustive search beyond the specified bound.

PATH counted 47 fingerprints, 58 scratch transition restorations, and 116 trail
entries for Blocks3; Hanoi counted 408, 502, and 502 respectively. Verified repeat
rejections were 26 and 189. These counts include the root fingerprint. Hash-match
attempts and false hash collisions were not separately counted in this benchmark;
the earlier focused checks establish collision handling.

Raw samples in execution order:

| Problem / mode | Milliseconds | Allocated bytes |
|---|---|---|
| Blocks3 immediate BT | 0.633, 0.593, 0.499, 0.538, 0.513 | 917088, 818832, 884352, 589520, 294640 |
| Blocks3 path BT | 0.300, 0.207, 0.228, 0.208, 0.273 | 360352, 294832, 294800, 294800, 262048 |
| Blocks3 DFS | 0.278, 0.204, 0.202, 0.206, 0.219 | 294800, 229264, 229296, 196448, 196480 |
| Hanoi immediate BT | 0.779, 0.683, 0.736, 0.717, 0.674 | 1342928, 1310112, 785936, 556592, 556592 |
| Hanoi path BT | 0.732, 0.608, 0.629, 0.666, 0.602 | 1244544, 1178992, 884160, 491072, 491072 |
| Hanoi DFS | 0.797, 0.751, 0.639, 0.616, 0.632 | 1342432, 1178576, 850928, 523360, 523360 |

Every solve was sub-millisecond, and allocation readings changed substantially
within cases despite stable native search counters. Consequently these medians
are descriptive, not reliable estimates of steady-state speedup or allocation
savings. In particular Hanoi PATH's median allocation is higher than IMMEDIATE's,
while its final two samples are lower; selecting either to claim a general
advantage would be misleading. No cause for the allocation variation was isolated.
The strongest evidence here is exact path correctness, restoration, and reduced
search work. No additional timing run was added to overcome this limitation.

The process emitted `ALL-PATH-VALIDATIONS-PASSED` and
`PATH-MODE-COMPARISON-PASSED` and exited normally. `git diff --check` passed.
After recording the evidence, the external path-cycle-change directory (baseline,
manifest, current copy, harness, log, and compiler caches) was removed. Only these
two investigation/design documents changed in this step; unrelated work remained.

Recommendation: retain PATH as opt-in and IMMEDIATE as default. Next propose the
unchanged triangle-xyz N=5, serial tree COUNT cutoff 10 as the no-benefit control:
compare current immediate/path BT, one pilot per mode followed by five timed runs
and one work/restoration check per mode if both pilots complete. Cap every solve
at 10 seconds and stop on timeout without retry or increased depth. Peg-count
strictly decreases, so this measures checking overhead rather than pruning gains.
The user subsequently approved that experiment; its results follow. Any further
optimization still requires separate approval.

## Triangle no-benefit control: current immediate/path BT (2026-10-07)

The approved experiment completed without timeout. This compares two modes in
the current implementation, not pre/post compact undo or pre/post path-code
versions. No DFS run, engine edit, model edit, or default change was included.

### Method and bounds

SBCL 2.6.9, 4096 MB dynamic space; unchanged triangle-xyz N=5, serial planning,
tree, COUNT, cutoff 10, deterministic order, debug 0, probe NIL, no heuristic.
STAGE preceded individual WW-SET overrides and REFRESH. The source directory and
translated compiler-cache root were explicitly pinned and asserted in a fresh
external copy. Engine/problem/technology copies matched source hashes before
execution; a repository-wide file-hash comparison afterwards found no changes
before recording these notes. Relevant SHA256 hashes:

- ww-backtracker.lisp: `3F2CDC9FEEC9200C749E2E339704C238ABC37FB7F88D2AC590E0022C359B9E5B`
- problem-triangle-xyz.lisp: `2FCFEB82F1E02CB88B3C88EDB725C4A489BD8B191AFFA6C2B69B17301E6F2A5A`

One pilot per mode completed first: IMMEDIATE 1.538166 seconds, PATH 1.664561
seconds. Then five uninstrumented timed solves per mode ran in alternating pairs:
IMMEDIATE/PATH on rounds 1, 3, 5; PATH/IMMEDIATE on rounds 2, 4. A separate
instrumented work/restoration solve followed for each mode. The same staged
problem and process were reused, with WW-SET changing only the mode between runs.
Pilots warmed the process; there were no additional warmup solves.

Every one of the 14 solves had a 10-second timeout and full GC beforehand.
Timing included WW-SOLVE initialization and reporting to suppressed output,
excluding staging/compilation, mode switching, explicit pre-run GC, and post-run
assertions. The pilots and instrumented solves are excluded from timed medians.
No timeout, retry, cutoff increase, or complete solution search occurred. The
initial 14 pegs require 13 jumps to reach the one-peg goal; zero accepted goals
are expected at cutoff 10.

### Results

| Measure | Immediate BT | Path BT |
|---|---:|---:|
| Median elapsed seconds | 1.537712 | 1.673523 |
| Elapsed range seconds | 1.527253–1.551578 | 1.667152–1.684972 |
| Median allocated bytes | 1,009,598,336 | 1,166,216,384 |
| Median reported GC seconds | 0 | 0.015625 |
| Effect calls | 713,552 | 713,552 |
| Precondition calls | 28,386,450 | 28,386,450 |
| Native program cycles | 315,405 | 315,405 |
| Native states | 315,406 | 315,406 |
| Accepted goals / reported repeats | 0 / 0 | 0 / 0 |

PATH took **8.8% more time** and allocated **15.5% more bytes**, an additional
156,618,048 bytes at the medians, for identical search work. These are local
bounded measurements of net mode overhead; they do not separately attribute
cost to hashing, ancestor filtering, or other control-flow differences. PATH
still constructs planning inverse literals and signatures in this implementation.

Timed samples by round:

- IMMEDIATE seconds: 1.536595, 1.542110, 1.527253, 1.537712, 1.551578.
- PATH seconds: 1.671598, 1.673523, 1.667152, 1.684972, 1.673571.
- IMMEDIATE bytes: 1009598336, 1009598336, 1009598480, 1009598336, 1009593888.
- PATH bytes: 1166216128, 1166216304, 1166216624, 1166221056, 1166216384.
- IMMEDIATE GC seconds: 0, 0, 0.015625, 0, 0.015625.
- PATH GC seconds: 0.015625, 0.031250, 0.015625, 0, 0.

The separate instrumented runs asserted both effect/precondition totals and
identical effect counts at parent depths 0 through 9:
`2, 8, 42, 222, 1074, 5072, 21530, 75758, 211696, 398148`, with none at depth 10.
PATH computed 713,553 fingerprints (including root), performed 713,552 ancestor
checks, and had zero matching ancestor fingerprints, zero scratch restorations,
and zero repeat rejections. Peg-count strictly decreases, so this absence of
ancestor equality is expected. Native cutoff hits remained 0 and truncation NIL
in both modes because BT returns before that reporting at the depth boundary;
these flags do not establish exhaustive exploration.

Both pilots and both work checks independently froze the root IDB and
name/time/value/heuristic/instantiations immediately before SEARCH-BACKTRACKING,
then asserted exact restoration of both start and returned working states. All
14 solves checked unchanged static tables, empty choice/fingerprint stacks, and
an inactive static-write guard. Work-check elapsed times were 1.845834 seconds
(IMMEDIATE) and 1.996070 seconds (PATH), including instrumentation, not performance
samples. All assertions passed; the process emitted
`TRIANGLE-PATH-CONTROL-PASSED` and exited normally.

Recommendation: keep IMMEDIATE as default and PATH opt-in. Blocks3/Hanoi establish
useful additional pruning and validated path behavior; this control establishes
a measurable cost when that pruning cannot help. A possible next step is a
read-only consumer audit of planning inverse literals/signatures to determine
whether PATH could safely omit them. That audit would precede a separately
approved implementation and paired performance comparison; no omission is
assumed safe or implemented here.

After recording results and passing `git diff --check`, the external
`triangle-path-control` directory was removed, including its copied checkout,
hash manifest, harness, log, saved status, and compiler caches. Only the
investigation and path-cycle design documents changed; unrelated work was
preserved.

## Subsequent incremental cycle-data omission

The user approved a read-only consumer audit, then implementation and focused
checks for omitting PATH's incremental inverse literals and forward/inverse
signatures. The implementation is confined to ww-backtracker.lisp and
ww-support.lisp; forward literals, physical undo, snapshot parents, and standalone
UPDATE-BT inverse returns remain. Runtime mode switching does not recompile effects.
See `backtracking-path-cycle-design.md` for the audit and validation details.

The triangle measurements immediately above describe the implementation BEFORE
this omission. They must not be presented as measurements of the new version or
used alone to claim a speedup. A hash-verified 434-file pre-omission baseline is
retained externally under `path-literal-omission/baseline/` with its manifest
until the user's fresh `(test-bt)` result and separately approved comparison below.
No performance run or default-mode change was made in this implementation step.

## PATH cycle-data omission: paired before/after (2026-10-08)

The user reported `(test-bt)`: 13 problems, zero failures, failed problems NIL,
return T, then approved this comparison. All 434 preserved baseline files matched
their manifest before execution. Comparing src/probs/tech against the current
checkout found only the approved ww-backtracker.lisp and ww-support.lisp edits.
The after copy's engine/problem/technology files matched the main checkout after
execution. No Git HEAD substitution or unrelated change was included.

SHA256 source identities:

| File | Before | After |
|---|---|---|
| ww-backtracker.lisp | 3F2CDC9FEEC9200C749E2E339704C238ABC37FB7F88D2AC590E0022C359B9E5B | 87145219B6978F435FFAAFA70A72CEA4F744998D802E1BDDE603E04E8F273EF1 |
| ww-support.lisp | A57D41C3138DEB2CE32CC3C72AE1EAB2DD63E16545F3ADAA69D0C9FAAB4C323C | 1D0C559818C1EF16CBCA910AF5BB45456E963BA709C20A948769C0C8891993A4 |

SBCL 2.6.9, 4096 MB dynamic space, unchanged triangle-xyz N=5, PATH BT, planning,
serial tree COUNT, cutoff 10, deterministic order, debug 0, probe NIL. STAGE
preceded individual WW-SET overrides and REFRESH. Each external checkout had
explicitly pinned and asserted ASDF source and translated cache roots.

Ran a before pilot (1.626334 seconds) and after pilot (1.438051 seconds), each in
its own process. Both passed before proceeding. Then a fresh before process ran
five timed solves and one instrumented work/restoration check, followed by a fresh
after process with the same sequence. Pilots therefore did not warm the measurement
processes; there was no extra warmup solve. Full GC preceded each solve. Timings
included WW-SOLVE initialization and reporting to suppressed streams, excluding
staging/compilation, pre-run GC, and assertions. Every solve had a 10-second limit;
all 14 completed, with no retry, increased cutoff, or complete solution search.

| Measure | Before | After |
|---|---:|---:|
| Median seconds | 1.640655 | 1.441080 |
| Seconds range | 1.626929–1.671907 | 1.432521–1.456895 |
| Median allocated bytes | 1,166,221,184 | 902,382,912 |
| Median GC seconds | 0.015625 | 0 |
| Effect calls | 713,552 | 713,552 |
| Precondition calls | 28,386,450 | 28,386,450 |
| Signature calls | 1,427,104 | 0 |
| Fingerprint calls | 713,553 | 713,553 |
| Scratch restorations | 0 | 0 |
| Program cycles / states | 315,405 / 315,406 | 315,405 / 315,406 |
| Goals / repeat rejections | 0 / 0 | 0 / 0 |

Median time decreased **12.2%** and allocation **22.6%**, saving 263,838,272 bytes.
This is an equal-work comparison of the combined inverse-literal/signature omission,
not a new compact-undo comparison. Sequential processes and a single workload limit
generalization. No fresh IMMEDIATE or DFS measurement was included, so earlier
measurements of those modes are not paired controls for these results.

Samples in execution order:

- Before seconds: 1.671907, 1.644524, 1.626929, 1.637296, 1.640655.
- After seconds: 1.434912, 1.432521, 1.441080, 1.448559, 1.456895.
- Before bytes: 1167327456, then four samples of 1166221184.
- After bytes: 903490800, then four samples of 902382912.
- Before GC seconds: 0.015625, 0.015625, 0, 0.015625, 0.
- After GC seconds: 0.015625, 0, 0, 0, 0.

The pilots and instrumented checks froze the root IDB and metadata immediately
before SEARCH-BACKTRACKING, and asserted exact restoration of both the working
and start states. Every solve checked static tables, empty choice/fingerprint
stacks, inactive static-write guard, zero accepted goals/repeats, and expected
cycle count. Both work checks asserted the same effect histogram at depths 0–9:
`2, 8, 42, 222, 1074, 5072, 21530, 75758, 211696, 398148`, with none at depth 10.
The goal requires 13 jumps; zero goals at this partial bound are expected.
All four processes emitted their labelled `OMISSION-COMPARISON-PASSED` markers
and exited normally. `git diff --check` passed.

Recommendation: retain the omission, leave PATH opt-in, and conclude this general
engine-tuning round. The latest change still gave a worthwhile gain. Further
improvements are possible, but should follow profiling of an actual slow workload
rather than another speculative engine change. Incremental fingerprinting, for
example, would touch mutation and rollback correctness and needs evidence that its
benefit justifies the added complexity. No such change is approved or implemented.

After recording results, removed the external `path-literal-omission` directory:
baseline, manifest, after copy, benchmark harness, four logs, and compiler caches.
Only the two investigation/design documents changed in this step; unrelated
working-tree changes were preserved.

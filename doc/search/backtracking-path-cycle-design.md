# Backtracking path-cycle pruning

Status: approved implementation, bounded Blocks3/Hanoi comparison, and triangle
overhead control completed, 2026-10-07. The user reported 13/13 passing `(test-bt)`
problems in default mode.
Independent path sets, replay, and restoration checks passed for the comparison.
IMMEDIATE remains the default; PATH remains opt-in. See the performance
investigation for measurements and their limits. Triangle showed 8.8% more elapsed
time and 15.5% more allocation for PATH with identical search work and no repeats.
Any further optimization requires separate approval.

Update (2026-10-08): the separately approved omission of incremental inverse
literals and signatures in PATH passed focused checks and the user's fresh
13/13 `(test-bt)` run. The approved paired triangle comparison reduced median
PATH time by 12.2% and allocation by 22.6% with identical work and restoration.
See the performance investigation. IMMEDIATE remains the default.

Parameter update (2026-10-08): `*bt-cycle-check*` now accepts only `nil` and `t`.
`nil` (default) retains immediate inverse-cycle checking; `t` enables full-path
checking with the existing serial, state-based planning restrictions. Historical
IMMEDIATE/PATH labels below describe these behaviors, respectively.
The obsolete 17-value recorder-layout migration was removed because it collides
with the current boolean parameter layout. Focused update/restoration, path-search,
and parameter checks passed. Run the parameter check last because it restages
the problem: `test-backtracking-updates`, `test-backtracking-path`, then
`test-bt-path-parameters`. The full `(test-bt)` rerun remains for the user.

## Recommendation and scope

Add an opt-in path-cycle mode for serial planning BT. Preserve the existing
immediate-inverse mode as the default until correctness and workload measurements
justify any default change. Parameter: `*bt-cycle-check*`, with values
`nil` (immediate inverse checking) and `t` (full-path checking), selected through WW-SET. This is a pruning policy change,
not an implementation-only optimization: solution path counts can change.

The first path mode targets ordinary state-based planning, such as Blocks3 and
three-disk Hanoi. Repeating a dynamic database must mean that the future planning
possibilities are equivalent. Do not enable it for a problem where elapsed time,
accumulated reward, path history, or external mutable data can make a revisit
useful unless those distinctions are represented in the dynamic database.
Enabling the mode is an explicit modelling assertion; arbitrary user predicates
cannot be automatically audited for that property.

Initially reject path mode with CSP, parallel execution, graph mode, registered
path/solution validators, goal-chain continuation/rejection machinery, or recorder
history policy. Preserve existing restrictions on happenings. Identify the
actual conflicting setting/hook in the error. Known unsupported configurations
must fail at solve setup rather than silently reverting to immediate mode.
Further compatibility requires separate analysis; these are proposed first-step
limits, not claims that those combinations can never be supported.

Static database writes during path-mode search must also signal an explicit
unsupported-operation error through the existing partial-effect cleanup path.
IDB equality cannot establish equivalence if a shared static table changes.
Ordinary snapshot IDB updates remain supported, under their existing contract
that the effect leaves the parent and shared static environment unchanged.

## Current source behavior

- DFS tree/planning calls ON-CURRENT-PATH after its goal-acceptance processing.
  It compares cached hashes against every ancestor, including the parent and
  root, and verifies matching candidates with EQUALP on their IDBs.
- BT's DETECT-PATH-CYCLE compares the candidate's forward operations with the
  preceding choice's inverse operations. It is called before registration by
  both SCORE-CHOICE-BT and EXPLORE-CHOICE-BT. It is not full-state equality and
  does not cover longer cycles or snapshot transitions.
- BT has one evolving working state. Choice undo frames retain actual old
  entries, including presence bits and repeated writes. Snapshot choices retain
  their parent database. RESTORE-BT-UNDO consumes frames, so it cannot be used
  merely to inspect an ancestor.

Relevant files: src/ww-backtracker.lisp (search, scoring, registration, undo),
src/ww-support.lisp (physical trail), src/ww-structures.lisp (frame/entry fields),
src/ww-searcher.lisp (ON-CURRENT-PATH and hash computation).

## Ancestor representation: fingerprints plus existing undo records

Keep a dynamically scoped stack of ancestor IDB fingerprints, nearest ancestor
first. Include the root before generating its successors. Store scalar hash and
entry count, never a reference presented as an immutable ancestor state.
The existing live choice stack supplies the transitions back to each ancestor.

For the first implementation, compute the raw IDB fingerprint with the existing
COMPUTE-IDB-HASH plus HASH-TABLE-COUNT. Do not depend on mutable state cache fields,
activate canonical symmetry, or add incremental hashing to the write path in
the same change. Pass a candidate's computed fingerprint into recursion so entry
to the child does not rescan the same database unnecessarily.

A hash is only a filter. On a hash/count match with an ancestor:

1. Copy the current candidate IDB into one scratch table. Keep the live candidate
   unchanged as the reference for exact comparisons.
2. Walk choices from the candidate back towards the root. For an incremental
   transition, read its frame entries newest first and restore old values or
   remove absent entries in the scratch table. Do not consume frame heads.
3. For a snapshot transition, replace the scratch contents with a copy of that
   choice's saved inverse snapshot. Never mutate the saved snapshot.
4. At matching ancestor fingerprints, require EQUALP between scratch and live
   candidate IDBs. Reject only on exact equality. After a collision, continue to
   older matching ancestors; a failed nearest comparison does not end the check.

The scratch restoration helper uses direct GETHASH/REMHASH on scratch only.
It must not call effects, UPDATE, folding helpers, actual undo, constraints, or
other user hooks. It must not alter live hash accumulators, propagated-change
flags, frame ownership, metadata, or static tables. Missing required transition
records are an invariant error, not grounds to skip an ancestor silently.

If no ancestor fingerprint matches, allocate no scratch database. No persistent
database copy is added per depth. Parent/root matches, no-op effects, repeated
physical writes, and mixed incremental/snapshot paths follow the same procedure.

The existing DEEP-SXHASH is not established here as a universal hash for all
EQUALP-equivalent values (for example mixed numeric representations or strings
of different case). Exact verification prevents false-positive pruning, but
hash mismatches can miss such repeats. Initial benchmark fixtures use stable
representations. Do not claim universal equality coverage or fix shared hashing
as part of this change; add explicit coverage/qualification if broader models
are proposed.

## Placement and lifetime

In immediate mode, leave existing rejection order and signature behavior intact.
In path mode, bypass the early immediate-inverse rejection: retaining it there
would still discard some candidates before the goal-first decision.

For actual exploration in path mode:

1. Apply/register the candidate using existing partial-failure cleanup.
2. Enter the existing UNWIND-PROTECT that always undoes the registered choice.
3. Keep inconsistency and prefix-policy handling in their established positions.
4. Attempt goal acceptance. An accepted goal is recorded before ancestor pruning,
   matching DFS's placement; do not remove it merely because its IDB repeats.
5. For an unaccepted candidate, check ancestors. On a verified repeat, increment
   the repeat/duplicate-depth reporting once and return through normal undo.
6. Otherwise descend with the candidate fingerprint dynamically added to the
   ancestor stack. Binding/unwinding removes it on return, rejection, or error.

The registered candidate is already at the head of the choice stack, but must
not yet be in the ancestor-fingerprint stack when checking it. Otherwise every
candidate would compare equal to itself. The hash stack and undo transitions
must stay aligned, including at root depth and after rejected siblings.

Do not apply path pruning during heuristic scoring in this first version.
Scoring uses disposable state and does not perform the actual goal-acceptance
decision; early pruning there could discard a goal that exploration should
accept. It may score candidates later rejected during exploration. Do not cache
that decision across reapplication. Existing scoring cleanup remains mandatory.

Retain inverse literals and signature construction initially, even though path
mode bypasses their rejection check. Removing them is a distinct optimization
and consumer audit. This first comparison should attribute changes to path
pruning, not combine it with another allocation change.

## Cost and alternatives

Let S be database entries, D path depth, and W the trail entries traversed for
verification. First-version fingerprinting costs O(S) per checked candidate;
ancestor filtering costs O(D). A match needs O(S + W) scratch reconstruction,
plus O(S) per exact ancestor comparison. Snapshot crossings can add further
database-copy work. Hash and deep equality costs also depend on stored values.

Additional persistent space is O(D) fingerprints; verification needs O(S)
temporary database space in addition to the existing undo trail. Hashing itself
allocates in the current implementation, so this is not an allocation-free path.
Worst-case repeated hashes can make reconstruction expensive. Measure actual
matches, rejected cycles, reconstruction work, total allocation, and elapsed time.

Copying every ancestor would be simpler but restores systematic O(D*S) retained
databases and O(S) copying per descended node. Hash-only pruning risks discarding
valid states on collision. Incremental fingerprint maintenance may later reduce
scanning, but touches mutation/rollback and snapshot handling; defer it until the
first comparison identifies hashing as the next cost worth addressing.

Triangle-xyz is a no-benefit control because peg-count decreases every jump.
Blocks3 and Hanoi can revisit ancestors and are the appropriate benefit cases.
Even if work decreases, runtime can increase; leave path mode opt-in unless
measurements support more.

## Tests and approval sequence

Implementation would touch the BT control flow and small scratch/fingerprint
helpers, plus settings/defaults/persistence, WW-SET validation and parameter
display. Audit all parameter registries rather than adding an ad hoc global.
STAGE should reset the default to immediate; REFRESH should retain the selection.
Add focused cases to existing test/search files; no new directory is required.

Focused checks must cover:

- A three-action return to root that immediate-inverse checking misses; a repeat
  of a non-root ancestor; and a no-op candidate equal to its parent.
- A diamond with the same state on different sibling paths: both paths remain
  eligible. This is ancestor pruning, not a global visited set.
- Forced equal fingerprints for unequal IDBs, followed by an older true match;
  exact comparison rejects only the actual repeat.
- Repeated writes, presence versus stored NIL, relation expansion, snapshot
  siblings, and incremental children below snapshots. Live and stored databases
  and undo-frame contents must be unchanged by scratch verification.
- Goal-first behavior with a repeat candidate; heuristic on/off agreement;
  rejected constraints, scoring/verification errors, and early search exit.
- Empty choice/hash stacks on completion and exact frozen-root restoration;
  unsupported configurations and static writes produce named errors and unwind.
- Mode default/reset/refresh behavior; immediate mode retains existing results.

After focused tests, stop for the user's `(test-bt)` run in default immediate mode.
That suite result does not by itself validate the opt-in mode; targeted tests do.

Separately propose bounded serial tree/COUNT or EVERY comparisons of immediate
BT, path BT, and DFS: Blocks3 cutoff 6 and three-disk Hanoi cutoff 8. Require
known expected path sets/counts from a small independent fixture and replay of
retained representative solutions, rather than treating fewer solutions as a
performance success. COUNT uses *solution-count*. Goal ordering, cutoff probing,
and other engine behavior mean DFS agreement must be checked, not assumed.
Path mode can suppress cycles even at the boundary, and counts can differ.

Each approved search should retain a 10-second timeout, with no automatic retry
or depth increase. Propose triangle cutoff 10 separately as the overhead control.
Do not alter recorded default expectations to conceal a mode difference. Before
any implementation, preserve an external verified source baseline; remove it
after approved comparisons are recorded. No default change or deletion of the
immediate-inverse mechanism is included in this change.

## Implemented behavior and validation

The parameter is registered with default/reset, persisted parameter loading,
WW-SET, value validation, help, and parameter displays. STAGE restores IMMEDIATE;
REFRESH and saved-parameter loading retain the selected value. Existing saved
parameter lists receive the appended default. Solve entry points reject known
incompatible configurations before goal-chain dispatch or search. Registered
prefix validators are rejected even if currently disabled, since their enabling
predicate may change along a path. No automatic claim is made about arbitrary
problem predicates being state-based.

The initial PATH implementation bypassed the old immediate-inverse rejection but
retained inverse literals and their signatures. The separately approved omission
below supersedes that retention. PATH computes fingerprints after unsuccessful goal acceptance,
verifies matching ancestors using read-only traversal of the live undo data into
scratch tables, and returns through existing undo on rejection or error. The
ancestor stack and static-write guard are dynamically scoped to the search.
Static writes through folding helpers, including snapshot code outside an undo
frame, signal a named error before the static mutation. Raw user GETHASH writes
remain outside the existing update API contract.

The production changes are in ww-backtracker.lisp, ww-support.lisp,
ww-settings.lisp, ww-set.lisp, ww-validator.lisp, ww-interface.lisp, and
ww-searcher.lisp. No problem definition, solution expectation, cutoff handling,
canonical hashing, or objective pruning was changed.

Extended test/search/backtracking-updates.lisp with TEST-BACKTRACKING-PATH and
TEST-BT-PATH-PARAMETERS. The path checks use small synthetic graphs, including
20 bounded search cases with cutoff 5; these are regression fixtures rather than
performance workloads. They exercise:

- Three-action root cycles, non-root cycles, and parent no-ops; immediate mode
  still permits the tested longer cycle up to the bound.
- Diamond paths whose shared state must remain reachable through both siblings.
- Goal-first handling of a repeat state, explicitly manufactured with a
  time-sensitive test goal to test ordering, not as a supported modelling example.
- Heuristic on/off agreement, snapshot siblings and incremental descendants,
  early FIRST exit, constraint rejection, scoring errors, and verification errors.
- Forced fingerprint collisions, including a nearer collision followed by an
  older real match; exact comparison is decisive.
- Repeated writes, present NIL, absent entries, empty frames, symmetric/bijective/
  complementary expansion, and preservation of live data and frame ownership.
- Named incompatible-mode errors, recorder rejection, static-write rejection
  after partial dynamic effects, and static deletion outside an active frame.
- Parameter value validation, old-list default filling, persistence, REFRESH,
  and STAGE reset.

Each synthetic search independently freezes the root database and metadata before
BT begins, then requires exact restoration and empty choice/fingerprint stacks,
including on injected errors. Tests restore their temporary action and hook
bindings afterwards. The existing update/signature, heuristic, pruning,
goal-chain rejection, minimum-steps, and bounding checks also run in default
immediate mode. The subsequent user `(test-bt)` run reported 13 problems, zero
failures, failed problems NIL, and return value T.

The isolated SBCL process emitted `PATH-CYCLE-FOCUSED-CHECKS-PASSED` and exited
normally. Source and compiler-cache roots were pinned and asserted. One initial
load failed on a missing closing parenthesis in the new test harness; it was
corrected before the successful runs. `git diff --check` passed. No performance
comparison was included in that focused-validation step; the subsequently
approved Blocks3/Hanoi comparison is recorded in the performance investigation.

A verified external baseline of the pre-path implementation was retained at:

`C:/Users/user/.codex/visualizations/2026/10/07/01a11861-500a-7d71-b065-7764ee8e205c/path-cycle-change/baseline/`

The adjacent `baseline-sha256.json` recorded every copied source hash. All 438
files matched the manifest before the approved comparison. That comparison used
three modes of the current source, not pre/post implementation timing. After
recording its results, the baseline, manifest, current comparison copy, harness,
log, and caches were removed. Unrelated working-tree changes were preserved.

To select the new mode after staging an eligible problem and selecting BT:

```lisp
(ww-set *bt-cycle-check* t)
```

To return to the existing behavior:

```lisp
(ww-set *bt-cycle-check* nil)
```

Enabling PATH asserts that IDB equality is an appropriate repetition criterion
for the model. It may change path counts. The bounded measurements confirm useful
pruning in Blocks3 and Hanoi, but their sub-millisecond timing and variable
allocation readings do not establish a general speed or allocation advantage.
The subsequent triangle cutoff-10 control measured net PATH overhead with no
pruning benefit: median 1.673523 versus 1.537712 seconds and 1,166,216,384 versus
1,009,598,336 allocated bytes. Both modes performed identical search work and
restored the frozen parent exactly. This supports retaining the explicit mode
choice; it does not establish which component of PATH is worth optimizing next.

## Incremental cycle-data omission (2026-10-07)

The read-only consumer audit found that incremental inverse literals and both
signatures serve IMMEDIATE's inverse-cycle check, not physical restoration or
PATH ancestor verification. Forward literals remain necessary for reapplication,
inconsistency checks, and replay. Parent snapshots remain necessary for both
snapshot rollback and ancestor reconstruction. Detailed BT narration was the
other inverse-literal consumer; it now explicitly reports omission in PATH.

The user approved implementation and focused validation, separately from timing:

- CYCLE-CHECK-ENABLED-BT requires IMMEDIATE planning, so PATH computes neither
  signature. DETECT-PATH-CYCLE uses the same predicate.
- UPDATE-BT returns NIL as its second value in an active undo frame for PATH,
  extending the existing CSP omission. Calls without an active frame retain
  their inverse-return contract, including fluent reconstruction.
- Incremental PATH choices retain no inverse list, including lists supplied by
  custom effect producers. Snapshot choices retain their parent IDB snapshots.
  IMMEDIATE behavior and the existing CSP handling are preserved.
- The translator still returns `(forward-list inverse-list)` and already skips
  NIL inverse values. No translation change or recompilation on mode switching
  is needed. Physical undo, forward updates, hashing, and pruning order are unchanged.

Production edits are confined to ww-backtracker.lisp and ww-support.lisp.
TEST-BT-PATH-CYCLE-DATA is included in TEST-BACKTRACKING-UPDATES. It checks
IMMEDIATE/PATH/IMMEDIATE switching via WW-SET with the identical compiled effect,
two/zero/two signature calls, omitted choice fields, and exact restoration. Direct
UPDATE-BT checks cover positive and negative literals, existing and absent fluent
values, with and without a trail. Snapshot acceptance/rejection, overlapping
siblings, and effect/constraint/reapplication errors are checked in PATH.

The existing path tests cover heuristic on/off, longer cycles, goal-first
acceptance, collision verification, scratch trails, static-write rejection,
parameter behavior, and frozen-root restoration. The heuristic suite also runs
in PATH/TREE, including reapplication, snapshots, and injected scoring errors.
Default-mode signature/update, pruning, goal-chain rejection, minimum-steps, and
bounding checks are included. Initial harness issues (a missing parenthesis in
a new helper and invoking PATH heuristic checks with the fixture's GRAPH default)
were corrected; the latter correctly produced a named configuration error.

The final isolated run emitted `PATH-LITERAL-OMISSION-CHECKS-PASSED` and exited
normally, with all listed suite markers present. `git diff --check` passed.
The two production files and test file matched the validated copy by SHA256.

Before editing, an external baseline of 434 files was copied and hash-verified.
It and `baseline-sha256.json` were retained under
`C:/Users/user/.codex/visualizations/2026/10/07/01a11861-500a-7d71-b065-7764ee8e205c/path-literal-omission/`
for the separately approved paired comparison, completed 2026-10-08. Validation used a separate copy
with pinned ASDF source and cache roots. Temporary validation files and caches
are removed after recording results. No new project directory, full `(test-bt)`
run, or performance measurement was included in the implementation step. The
user then reported 13 problems, zero failures, failed problems NIL, return T.
After recording the subsequent comparison, the entire external directory,
including baseline and manifest, was removed. The feature is complete; further
optimization is deferred until a representative workload identifies a bottleneck.

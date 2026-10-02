# Wouldwork Loading and Initialization

Reference for framing analysis. Every claim below is cited to a file and to the named function or
form it concerns, so it can be re-checked rather than remembered. Citations deliberately omit line
numbers, which go stale on the first edit above them. Scope runs from ASDF bootstrap through the end
of `init()`; search-time execution is deliberately out of scope. Source-checked on 2026-10-02;
historical outcomes in the worked example were not rerun for this review.

The reason this document exists: wouldwork makes *ordering* load-bearing across five distinct
time regimes — ASDF bootstrap, textual tech splicing, form pre-scan, form evaluation, and the
`init()` phase sequence. Most ordering bugs are not "wrong phase order" but "assumed two things
happened in the same phase when they didn't." The **Freezes** entry on each stage is the part
that matters: later changes to a type population do not retroactively change a translated
literal domain. Deferred installation and explicit recompilation are identified separately below.

---

## Stage 0 — ASDF bootstrap

**Where:** `wouldwork.asd`, the `eval-when (:compile-toplevel :load-toplevel :execute)` preceding
`defsystem`.

Runs before the `:wouldwork` package exists. If `src/problem.lisp` is absent, it plain-copies
`probs/problem-blocks3.lisp` into place and deletes `vals.lisp` to force a rebuild.

These are the default-instance filenames. When `WOULDWORK_INSTANCE` is set before SBCL starts,
the ASDF bootstrap and `always-compile-file` component use `src/problem-<instance>.lisp` and
`vals-<instance>.lisp`. The runtime staging helpers use the same suffix.

This is a `uiop:copy-file`, **not** a tech-splicing copy — `copy-problem-with-tech-includes` and
`ensure-problem-staged` are symbols that do not yet exist. Hence the invariant stated in the
`.asd` itself: *problem-blocks3.lisp must never contain an `(include-tech ...)` directive*, or the
copy leaves an unexpanded directive behind and the subsequent compile fails.

**Freezes:** nothing analytically interesting. This stage only guarantees `src/problem.lisp` exists.

---

## Stage 1 — Serial component load

**Where:** `wouldwork.asd`, `:components` — the `src` module is `:serial t`.

Load order:

```
ww-packages, ww-utilities, ww-hstack, ww-preliminaries, ww-settings,
ww-problem-lifecycle, ww-structures, ww-action-format, ww-worker-read-snapshots,
ww-relaxed-heuristics, ww-converter, ww-validator, ww-frequencies,
ww-support, ww-happenings, ww-translator,
ww-init-validator, ww-installer, ww-propagation-order, ww-patroller-installer,
ww-interface, ww-test-characterization, ww-problem-tests, ww-set, ww-command-tests, ww-enumerator-build,
ww-enumerator-run,
    >>> problem <<<
ww-action-trace, ww-goal-chaining, ww-advisor, ww-solution-validation,
ww-goal-chain-persistence, ww-search-checkpoint, ww-backward,
ww-planner, ww-symmetry, ww-searcher, ww-backtracker,
ww-parallel-infrastructure, ww-parallel, ww-initialize
```

Two positions in this list carry weight. `ww-translator` and `ww-installer` load *before*
`problem`, which is why translation machinery is available the instant a `define-query` form is
evaluates. And `ww-initialize` loads *last*, which is what makes `init()` run at the end of system load
(Stage 5).

**Ordering guarantee:** installers and translators are available before problem evaluation.
Stages 2–4 happen within this serial sequence. The `problem` component is always recompiled.

---

## Stage 2 — Tech splicing

**Where:** `ww-preliminaries.lisp` / `ensure-problem-staged`, invoked by the `eval-when
(:load-toplevel :execute)` immediately following its definition — so it fires when
`ww-preliminaries` loads, i.e. fourth in the Stage 1 list.

`copy-problem-with-tech-includes` textually splices each `(include-tech X)` directive's target file
into `src/problem.lisp`, recursively. Properties worth knowing:

- **Deduplicated by construction.** Each technology is spliced at most once per problem copy. Skipped
  repeats leave a visible marker in `src/problem.lisp`, of the form
  `;; (include-tech -propagation): already included -- skipped`.
- **Content compared before writing.** Identical spliced content leaves the generated file
  unchanged; staging still reloads the system.
- **Splice order is semantically significant.** It is the seed the derived propagation driver orders
  its updates by.
- Traced in `*tech-inclusion-trace*` and `*spliced-tech-names*` (`ww-preliminaries.lisp`).

The output is a single flat file. There is no runtime `include-tech` macro — by the time anything is
evaluated, the tech bodies are ordinary top-level forms in `problem.lisp`.

**Freezes:** the complete text of `src/problem.lisp`, including the relative order of every tech
body. Nothing downstream can reorder tech forms.

---

## Stage 3 — Pre-scan

**Where:** `wouldwork.asd`, the `problem` component's `:around-compile` thunk, which calls
`(find-symbol "PRESCAN-PROBLEM-FILE" "WOULDWORK")` and also sets `*WW-LOADING*` to `t`.

The ASDF call site is outside `src/` and resolves the function by its string name.

`prescan-problem-file` (`ww-installer.lisp`) reads `src/problem.lisp` with plain `read`
(`read-problem-forms`) — **no evaluation** — and registers forward-reference metadata in
three passes:

| Pass | Effect |
|---|---|
| `prescan-problem-function-names` | `pushnew` every `define-query`/`define-update` name into `*query-names*`/`*update-names*`; every `define-happening`/`define-patroller` name into `*happening-names*`; installs erroring stubs for every `defun` name not yet `fboundp` |
| `prescan-problem-type-names` | For every `define-types` / `define-optional-types` form, `predeclare-type-names` does `(setf (gethash type *types*) nil)` |
| `prescan-problem-relation-signatures` | Registers `inconsistent-state` into `*relations*`, then every dynamic / static / complementary relation signature |

The type pass deserves emphasis: after pre-scan, **every type name exists in `*types*` with zero
instances**. The name is present; the extension is empty. That distinction drives Trap 3 below.

The function-name pass is what lets a tech file's `define-query` reference a query defined further
down the spliced file: `translate` checks membership in `*query-names*`, which pre-scan already
populated.

**Freezes:** the set of known query, update, and happening names; all relation signatures; the set
of known type *names*.

---

## Stage 4 — `problem.lisp` evaluation

Forms evaluate top to bottom. This is the stage where authoring order inside the file has direct
semantic consequence. Installers live in `ww-installer.lisp` unless otherwise noted;
initialization check registration lives in `ww-init-validator.lisp`, and problem-owned helper
registration lives in `ww-problem-lifecycle.lisp`.

| Form | Installer | What happens |
|---|---|---|
| `ww-set` | — | Sets globals |
| `defparameter` | — | Ordinary evaluation |
| `define-types` | `install-types` | Evaluates a leading backquote or `(compute <form>)` **once, here**; `check-type-signature-consistency` enforces cross-file agreement; asserts `(something X)` and `(type X)` into `*static-db*` |
| `define-optional-types` | `install-optional-types` | Fills the type only if `*type-signatures*` has no real entry — order-independent w.r.t. a real `define-types` |
| `define-dynamic-relations` / `define-static-relations` | — | Full installation (signatures were already pre-scanned) |
| `define-derived-relations` | `install-derived-relations` | Marks declared dynamic relations as computed initial state that `define-init` must not author |
| `define-init-check` | `register-init-check` | Defines and registers a technology-owned raw-literal check; optional `(:consumes ...)` metadata credits types used only inside untyped list payloads |
| `define-problem-helper` / `define-init-check-helper` | `register-problem-function` | Defines a problem-owned Lisp helper and records it for cleanup when reloading/restaging; the init-check form expands to `define-problem-helper` |
| `define-query` | `install-query` | **Calls `(translate body 'pre)` immediately** |
| `define-update` | `install-update` | **Calls `(translate body 'eff)` immediately**, in both the `$vars` and the no-`$vars` branch |
| `define-action` | `install-action` | Translates precondition and effect |
| `define-init-action` | `install-init-action` | Translates; **skips the init-action entirely** if `check-action-parameter-instantiability` finds a pre-param type with no instances |
| `define-init` | `install-init` | Evaluates backquoted literals, runs registered literal generators, pads registered argument defaults, validates propositions and engine/technology invariants, then asserts the initial facts |
| `define-goal` | `install-goal` | Installs the problem goal; rejects a quoted goal |

The single most consequential fact in this document: **`install-query` and `install-update` call
`translate` during ordinary problem evaluation.** Static `doall` domains are fixed in the
translated bodies at that point; integer compilation follows in `init()`. Deferred action
installers instead translate their actions at the start of `init()`, and the derived propagation
driver is installed there too. Query-domain loops continue to resolve their domains at runtime.

Consequently a problem file's own `define-types` must appear **above** its `(include-tech ...)`
block. `probs/problem-corner-topo.lisp` does exactly this: `define-types` first, tech includes
after. The file's own comment says so — *"Leaf object types the problem instantiates live
here, ahead of the technology includes."*

**Freezes:** the static domains in bodies translated during this stage. Deferred bodies have
not yet been translated. Type extensions are populated as `install-types` runs.

Raw initialization validation belongs to this load phase, not to `init()`: it finishes before the
`define-init` facts are asserted and before any init-action or search can run. A standalone problem with no
technology includes receives only the engine checks; including a technology also includes the
semantic checks that technology owns.

---

## Stage 5 — `init()`

**Where:** `ww-initialize.lisp` / `init`, called at top level from the bottom of that same file —
the last component in the Stage 1 list.

| # | Step | Notes |
|---|---|---|
| 0 | `run-deferred-action-installers` | `ww-installer.lisp`; drains `*deferred-action-installers*` before the `nreverse` below, so a deferred action is installed with every type, relation and instance already present. See Trap 10 |
| 1 | `nreverse` `*query-names*`, `*update-names*`, `*actions*`, `*init-actions*` | After this, `*init-actions*` is in splice/file order |
| 2 | `report-propagation-diagnostics` | `ww-propagation-order.lisp`; a reaction-order violation **errors here**, deliberately before init-actions run |
| 3 | `install-derived-propagation-driver` | `ww-propagation-order.lisp`; replaces the sentinel `propagate-consequences!` with an order derived from splice order. Silent no-op if the problem authored its own driver |
| 4 | Sort `*happening-names*` by first event time | |
| 5 | `init-start-state` | Updates `db` and `static-db` — **not** `idb`/`hidb` |
| 6 | `vals.lisp` save/read globals | Three-way: if `*refreshing*`, `save-globals`; else if `vals.lisp` exists, `read-globals`; else `save-globals`. See `parameter-precedence.md` |
| 7 | `do-integer-conversion` | `ww-converter.lisp` → `clrhash *prop-key-cache*`, `associate-objects-with-integers`, `convert-databases-to-integers`, `compile-all-functions`. Integer codes are assigned from `*types*` contents |
| 8 | `finalize-patroller-happenings` | |
| 9 | `do-init-action-updates *start-state*` | `ww-planner.lisp`; iterates `*init-actions*` **in order**, compiling each precondition and effect with a plain `compile` |
| 10 | `convert-databases-to-integers` | New propositions only; no recompilation |
| 11 | `initialize-symmetry-detection` | Deliberately placed *after* init-actions so exact static automorphism checks see derived static facts |
| 12 | `validate-start-state-consistency` | |
| 13 | Set `*inconsistent-state-key*` | |
| 14 | Compute `*min-action-duration*` | |
| 15 | Apply `heuristic?`, available minimum-step bounds, `bounding-function?`; compatibility checks | Minimum-step bounds may include registered contributors, not just `min-steps-remaining?` |

**Established here:** baseline integer object codes and compiled functions at step 7;
init-actions can register additional objects at step 9. Step 10 synchronizes newly derived facts,
and step 11 establishes symmetry families. Init-action bodies are compiled separately at step 9.

---

## Traps

Ordering mistakes the structure above makes easy. Each is a real property of the code, not a
hypothetical.

**1. Ordinary query translation happens during problem evaluation.**
`install-query` calls `translate` (`ww-installer.lisp`). Reasoning about "what `init()` phase
does the domain get resolved in" is already the wrong frame — it was resolved before `init()` was
called. Deferred action installation is the explicit exception described in Stage 5, step 0.

**2. `doall` over a bare type name bakes a literal; over a query it does not.**
`translate-doall` (`ww-translator.lisp`) has two branches. `static-single-p` emits
`(dolist (var '<literal domain> t) ...)`. A type slot holding a query call instead emits
`(ut::transpose (eval-instantiated-spec ',type-inst state))` — evaluated at runtime
against state. This is the only escape hatch for a population that isn't known at load time, and
`probs/problem-corner.lisp` uses it: `beam ()` is declared empty, and every iteration site says
`(doall (?b (get-current-beams)))`.

**3. Pre-scan makes type names exist with zero instances.**
`predeclare-type-names` sets `(gethash type *types*)` to `nil` (`ww-installer.lisp`).
`static-single-quantifier-domain` (`ww-translator.lisp`) checks `present-p`, which is now true
with an empty domain — so a `doall` translated before the real `install-types` runs collapses via
`translate-empty-static-quantifier` into a silent no-op. Not an error. This is the mechanism that
punishes putting `define-types` below the tech includes.

**4. Init-actions are silently skipped, not errored, when a pre-param type is empty.**
`install-init-action` prints `skipped (no instances for type...)` and moves on.
Easy to miss in load output.

**5. Init-action effects are compiled without int-code substitution.**
`do-init-action-updates` uses a plain `(compile nil effect-lambda)` (`ww-planner.lisp`),
so a newly-asserted static fact lands only in the symbolic `*static-db*`. Every `define-query` was
already compiled against the integer-keyed `*static-idb*` back at step 7. An init-action whose
facts must be visible to a later init-action — or to `propagate-changes!` — has to call
`convert-databases-to-integers` itself. `establish-beam-coordinates` and `derive-los-from-segments`
both do, and both explain why in their own comments.

**6. Init-action firing order is splice/file order, not the numeric duration argument.**
`do-init-action-updates` iterates `*init-actions*` linearly. The leading `0` in a
`define-init-action` form is a duration, not a priority.

**7. Symmetry families are detected after init-actions fire — this was not always true.**
`initialize-symmetry-detection` (step 11) runs *after* `do-init-action-updates` (step 9), so its
complete static-database transposition checks see static facts derived by init-actions. Coupling
relations such as recorder's directional `recording-copy>` are converted into ordered rows before
ordinary one-object families are considered. What remains true: candidates come from `*types*`,
and `register-dynamic-object` never adds to `*types*` (Trap 8), so dynamically minted objects still
do not appear in them.

**8. `register-dynamic-object` does not update `*types*`.**
`register-dynamic-object` (`ww-converter.lisp`) assigns an integer code and asserts
`(type-name object)` into `*static-idb*`. It does **not** touch `(gethash type *types*)`, and cannot
retroactively edit an already-compiled `dolist` literal. Dynamically registered objects are
reachable through a predicate test or a query-domain `doall`, never through a static-domain one. It
also errors past 999 total planning objects.

**9. The `.asd` bootstrap cannot splice.**
`problem-blocks3.lisp` must stay free of `include-tech` directives. Documented in the `.asd`
comment; nothing enforces it mechanically.

**10. Sibling `(include-tech ...)` order is the problem author's; splicing does not sort dependencies.**
`write-tech-include` (`ww-preliminaries.lisp`) is a purely textual, depth-first expander: nested
includes are inlined before the rest of their includer, each technology is spliced once at its
first mention, and top-level directives are expanded in the order the problem file lists them.
The splicer computes no dependency ordering. Propagation diagnostics later check reaction order,
but do not reorder top-level definitions or repair an already translated empty domain.
A technology therefore sees only what happens to
precede it — and Trap 3 means it cannot tell "type absent" from "type declared but not yet
populated", because pre-scan has already made the name exist with an empty domain.

This is a real bug that shipped. `-recorder-session.lisp` guarded its optional `paired` fork
clause on `(nth-value 1 (gethash 'terminus *types*))` — the *name*, true from pre-scan onward —
while the clause body needed `terminus`'s *instances*, which only exist once `beam-relay.lisp` is
spliced. `probs/problem-rumin-topo.lisp` listed `(include-tech recorder)` above
`(include-tech beam-relay)`, so START-RECORDER installed a fork whose `(doall (?terminus terminus))`
ranged over nothing. Ghost connectors forked with no pairings at all, silently, in flat
contradiction of the file's own docstring and of rule 11 of the recorder specification.

**The fix, and the pattern to reuse.** Do not test for a peer technology's presence at splice
position. Instead:

- the technology that *owns* the optional relation registers its own contribution — see
  `tech/-recorder-fork-registry.lisp`, which both sides `include-tech`, so whichever is spliced
  first brings it in and deduplication hands the second the same registry;
- the technology that *consumes* the contributions defers building its action to `init` via
  `register-deferred-action-installer` (Stage 5, step 0), by which point every technology has been
  spliced, every type populated, and every relation declared.

`-recorder-session.lisp` now does both, and its three relation redeclarations are gone — each
owner already declares its own. Include order no longer affects the result. The same shape applies
to any future action whose definition depends on an optional peer.

---

## Worked example — how `crossing` was eliminated

`probs/problem-corner-topo.lisp` used to declare a 26-instance `crossing` pool that had to match its
computed geometry exactly. It no longer declares one at all. The reasoning came straight off this
document, and the result is a compact illustration of Traps 1, 2 and 8.

**Why the obvious fix was impossible.** A type extension must be final before the
`(include-tech beam-crossing)` directive is evaluated, because beam-crossing.lisp's five
`(doall (?x crossing))` sites translate at that moment (Stage 4, Trap 1). No `init()` reordering
reaches back that far, so "declare a placeholder and grow it during init" cannot work.

**What was done instead.** The five sites were converted to query-domain `doall` —
`(doall (?x (get-current-beam-crossings)))` — selecting `translate-doall`'s runtime branch (Trap 2).
`establish-beam-coordinates` now mints one crossing per computed intersection via
`register-dynamic-object` and publishes the pool as `current-beam-crossings`. Because
`register-dynamic-object` never touches `*types*` (Trap 8), the type extension stays permanently
empty and the objects are reachable only through that relation.

**Where the ordering knowledge paid off.**

- Symmetry was a non-issue either way: crossings never formed a usable symmetry group, because
  `prune-inoperative-symmetry-groups` drops any group no action parameter references, and no action
  takes a `crossing` parameter. Confirmed from the load printout before any code changed. (The
  analysis originally leaned on signatures being computed before init-actions; that ordering has
  since changed — see Trap 7 — but the conclusion does not depend on it.)
- Stage 3's pre-scan behavior — query names registered before evaluation — is what makes the
  forward reference from `update-crossing-status!` to `get-current-beam-crossings` legal despite the
  query being defined further down the file.
- Trap 5 explains why `establish-beam-coordinates` must keep its trailing
  `convert-databases-to-integers` for the newly asserted `current-beam-crossings` to be visible to the
  next init-action.

The genuinely dangerous step was the second consumer. `derive-beam-crossings-before-gate` recomputes the
same geometry; had it kept minting its own pool it would have produced same-named but distinct
symbols that no other fact referred to — silent wrong answers rather than an error. It now reads the
pool back from `current-beam-crossings`, and the length check in `beam-coordinates-crossing-records`
became a real cross-check that the two passes agree.

Historical verification recorded for this change: 26 crossings, 4 `beam-crossings-before-gate>`
facts, unchanged solution. This is a record of the earlier validation, not a fresh replay result.

---

## Where this document stops

Everything above ends the moment `init()` returns. What happens next — node expansion, successor
generation, backtracking, pruning, and the parallel search machinery — is a separate subject with
its own ordering rules, and is not covered here. Starting points for that reading: `ww-searcher.lisp`
(the main search loop), `ww-backtracker.lisp`, `ww-planner.lisp` (action application),
`ww-symmetry.lisp` (run-time symmetry pruning), and `ww-parallel.lisp` /
`ww-parallel-infrastructure.lisp` for threaded search.

# Wouldwork Ordering of Operations

Reference for framing analysis. Every claim below is cited to a file and line so it can be
re-checked rather than remembered. Scope runs from ASDF bootstrap through the end of `init()`;
search-time execution is deliberately out of scope.

The reason this document exists: wouldwork makes *ordering* load-bearing across five distinct
time regimes — ASDF bootstrap, textual tech splicing, form pre-scan, form evaluation, and the
`init()` phase sequence. Most ordering bugs are not "wrong phase order" but "assumed two things
happened in the same phase when they didn't." The **Freezes** entry on each stage is the part
that matters: once something freezes, no later stage can retroactively change it.

---

## Stage 0 — ASDF bootstrap

**Where:** `wouldwork.asd`, the `eval-when (:compile-toplevel :load-toplevel :execute)` preceding
`defsystem`.

Runs before the `:wouldwork` package exists. If `src/problem.lisp` is absent, it plain-copies
`probs/problem-blocks3.lisp` into place and deletes `vals.lisp` to force a rebuild.

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
ww-packages, ww-utilities, ww-hstack, ww-preliminaries, ww-settings, ww-structures,
ww-converter, ww-validator, ww-frequencies, ww-support, ww-happenings, ww-translator,
ww-init-validator, ww-installer, ww-propagation-order, ww-patroller-installer,
ww-interface, ww-problem-tests, ww-set, ww-command-tests, ww-enumerator-build,
ww-enumerator-run,
    >>> problem <<<
ww-action-trace, ww-goal-chaining, ww-advisor, ww-solution-validation, ww-backward,
ww-planner, ww-symmetry, ww-searcher, ww-backtracker,
ww-parallel-infrastructure, ww-parallel, ww-initialize
```

Two positions in this list carry weight. `ww-translator` and `ww-installer` load *before*
`problem`, which is why translation machinery is available the instant a `define-query` form is
read. And `ww-initialize` loads *last*, which is what makes `init()` run at the end of system load
(Stage 5).

**Freezes:** the availability of every installer and translator entry point. Stages 2–4 all happen
inside this list.

---

## Stage 2 — Tech splicing

**Where:** `ww-preliminaries.lisp:548` (`ensure-problem-staged`), invoked by the `eval-when
(:load-toplevel :execute)` immediately following its definition — so it fires when
`ww-preliminaries` loads, i.e. fourth in the Stage 1 list.

`copy-problem-with-tech-includes` textually splices each `(include-tech X)` directive's target file
into `src/problem.lisp`, recursively. Properties worth knowing:

- **Deduplicated by construction.** Each technology is spliced at most once per problem copy. Skipped
  repeats leave a visible marker: `;; (include-tech -propagation): already included -- skipped`
  (see `src/problem.lisp:213`, `:385`, and many others).
- **Content-addressed.** Full spliced content is computed and compared first, so a re-stage with
  identical content is a no-op (`ww-preliminaries.lisp:145`).
- **Splice order is semantically significant.** It is the seed the derived propagation driver orders
  its updates by (`ww-preliminaries.lisp:239`).
- Traced in `*tech-inclusion-trace*` and `*spliced-tech-names*` (`ww-preliminaries.lisp:115`).

The output is a single flat file. There is no runtime `include-tech` macro — by the time anything is
evaluated, the tech bodies are ordinary top-level forms in `problem.lisp`.

**Freezes:** the complete text of `src/problem.lisp`, including the relative order of every tech
body. Nothing downstream can reorder tech forms.

---

## Stage 3 — Pre-scan

**Where:** `wouldwork.asd`, the `problem` component's `:around-compile` thunk, which calls
`(find-symbol "PRESCAN-PROBLEM-FILE" "WOULDWORK")` and also sets `*WW-LOADING*` to `t`.

> Note: this is why grepping for `prescan-problem-file` in `src/` finds no callers — the call site
> passes the name as a *string*, not a symbol.

`prescan-problem-file` (`ww-installer.lisp:417`) reads `src/problem.lisp` with plain `read`
(`read-problem-forms`, `:354`) — **no evaluation** — and registers forward-reference metadata in
three passes:

| Pass | Line | Effect |
|---|---|---|
| `prescan-problem-function-names` | `:361` | `pushnew` every `define-query`/`define-update` name into `*query-names*`/`*update-names*`; every `define-happening`/`define-patroller` name into `*happening-names*`; installs erroring stubs for every `defun` name not yet `fboundp` |
| `prescan-problem-type-names` | `:384` | For every `define-types` / `define-optional-types` form, `predeclare-type-names` (`:43`) does `(setf (gethash type *types*) nil)` |
| `prescan-problem-relation-signatures` | `:394` | Registers `inconsistent-state` into `*relations*` (`:403`), then every dynamic / static / complementary relation signature |

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
semantic consequence.

| Form | Installer | Line | What happens |
|---|---|---|---|
| `ww-set` | — | — | Sets globals |
| `defparameter` | — | — | Ordinary evaluation |
| `define-types` | `install-types` | `ww-installer.lisp:70` | Evaluates a leading backquote (`:76`) or `(compute <form>)` (`:83`) **once, here**; `check-type-signature-consistency` (`:59`) enforces cross-file agreement; asserts `(something X)` and `(type X)` into `*static-db*` (`:92`–`:93`) |
| `define-optional-types` | `install-optional-types` | `:96` | Fills the type only if `*type-signatures*` has no real entry — order-independent w.r.t. a real `define-types` |
| `define-dynamic-relations` / `define-static-relations` | — | — | Full installation (signatures were already pre-scanned) |
| `define-query` | `install-query` | `:493` | **Calls `(translate body 'pre)` immediately** (`:519`–`:520`) |
| `define-update` | `install-update` | `:528` | **Calls `(translate body 'eff)` immediately** (`:558`, and again at `:565` in the no-`$vars` branch) |
| `define-action` | `install-action` | `:590` | Translates precondition and effect |
| `define-init-action` | `install-init-action` | `:608` | Translates; **skips the init-action entirely** if `check-action-parameter-instantiability` finds a pre-param type with no instances (`:615`–`:618`) |
| `define-init` | `install-init` | `:807` | Asserts the problem's initial facts |
| `define-goal` | — | — | Builds `goal-fn` |

The single most consequential fact in this document: **`install-query` and `install-update` call
`translate` at load time, not during `init()`.** Everything `translate` decides — including the
domain of every `doall` — is baked into compiled code before `init()` begins.

Consequently a problem file's own `define-types` must appear **above** its `(include-tech ...)`
block. `probs/problem-corner-topo.lisp` does exactly this: `define-types` at line 62, tech includes
at lines 91–95. The file's own comment says so — *"Leaf object types the problem instantiates live
here, ahead of the technology includes."*

**Freezes:** every translated query, update, action, and init-action body — including all static
`doall` domains. Also the full type extensions, once `install-types` has run.

---

## Stage 5 — `init()`

**Where:** `ww-initialize.lisp:15`, called at top level from `ww-initialize.lisp:230` — the last
component in the Stage 1 list.

| # | Line | Step | Notes |
|---|---|---|---|
| 1 | `:17`–`:20` | `nreverse` `*query-names*`, `*update-names*`, `*actions*`, `*init-actions*` | After this, `*init-actions*` is in splice/file order |
| 2 | `:21` | `report-propagation-diagnostics` | `ww-propagation-order.lisp:106`; a reaction-order violation **errors here**, deliberately before init-actions run |
| 3 | `:22` | `install-derived-propagation-driver` | `ww-propagation-order.lisp:1282`; replaces the sentinel `propagate-consequences!` with an order derived from splice order. Silent no-op if the problem authored its own driver |
| 4 | `:23` | Sort `*happening-names*` by first event time | |
| 5 | `:25` | `init-start-state` | `:148`; updates `db` and `static-db` — **not** `idb`/`hidb` |
| 6 | `:29`–`:35` | `vals.lisp` save/read globals | |
| 7 | `:36` | `initialize-symmetry-detection` | Reads type extensions as they stand *now* |
| 8 | `:37` | `do-integer-conversion` | `ww-converter.lisp:108` → `clrhash *prop-key-cache*`, `associate-objects-with-integers` (`:117`), `convert-databases-to-integers`, `compile-all-functions`. Integer codes are assigned from `*types*` contents at `:137`–`:140` |
| 9 | `:38` | `initialize-initial-signatures` | Symmetry signatures |
| 10 | `:39` | `finalize-patroller-happenings` | |
| 11 | `:40` | `do-init-action-updates *start-state*` | `ww-planner.lisp:18`; iterates `*init-actions*` **in order**, compiling each precondition and effect with a plain `compile` (`:29`–`:30`) |
| 12 | `:41` | `convert-databases-to-integers` | New propositions only; no recompilation |
| 13 | `:43` | `validate-start-state-consistency` | |
| 14 | `:43`–`:44` | Set `*inconsistent-state-key*` | |
| 15 | `:45`–`:47` | Compute `*min-action-duration*` | |
| 16 | `:48`+ | Apply `heuristic?`, `min-steps-remaining?`, `bounding-function?`; compatibility checks | |

**Freezes:** integer object codes and all compiled functions at step 8; symmetry signatures at
step 9; the start-state database at step 12.

---

## Traps

Ordering mistakes the structure above makes easy. Each is a real property of the code, not a
hypothetical.

**1. Translation happens at load, not at `init()`.**
`install-query` calls `translate` at `ww-installer.lisp:520`. Reasoning about "what `init()` phase
does the domain get resolved in" is already the wrong frame — it was resolved before `init()` was
called.

**2. `doall` over a bare type name bakes a literal; over a query it does not.**
`translate-doall` (`ww-translator.lisp:834`) has two branches. `static-single-p` emits
`(dolist (var '<literal domain> t) ...)` at `:860`. A type slot holding a query call emits
`(ut::transpose (eval-instantiated-spec ',type-inst state))` at `:864` — evaluated at runtime
against state. This is the only escape hatch for a population that isn't known at load time, and
`probs/problem-corner.lisp` uses it: `beam ()` is declared empty, and every iteration site says
`(doall (?b (get-current-beams)))`.

**3. Pre-scan makes type names exist with zero instances.**
`predeclare-type-names` sets `(gethash type *types*)` to `nil` (`ww-installer.lisp:47`).
`static-single-quantifier-domain` (`ww-translator.lisp:452`) checks `present-p`, which is now true
with an empty domain — so a `doall` translated before the real `install-types` runs collapses via
`translate-empty-static-quantifier` into a silent no-op. Not an error. This is the mechanism that
punishes putting `define-types` below the tech includes.

**4. Init-actions are silently skipped, not errored, when a pre-param type is empty.**
`install-init-action` at `:615`–`:618` prints `skipped (no instances for type...)` and moves on.
Easy to miss in load output.

**5. Init-action effects are compiled without int-code substitution.**
`do-init-action-updates` uses a plain `(compile nil effect-lambda)` (`ww-planner.lisp:29`–`:30`),
so a newly-asserted static fact lands only in the symbolic `*static-db*`. Every `define-query` was
already compiled against the integer-keyed `*static-idb*` back at step 8. An init-action whose
facts must be visible to a later init-action — or to `propagate-changes!` — has to call
`convert-databases-to-integers` itself. `establish-beam-coordinates` and `derive-los-from-segments`
both do, and both explain why in their own comments.

**6. Init-action firing order is splice/file order, not the numeric duration argument.**
`do-init-action-updates` iterates `*init-actions*` linearly. The leading `0` in a
`define-init-action` form is a duration, not a priority.

**7. Symmetry signatures are computed before init-actions fire.**
Step 7 and step 9 both precede step 11. Objects or facts created by an init-action are not
reflected in the initial signatures.

**8. `register-dynamic-object` does not update `*types*`.**
`ww-converter.lisp:198` assigns an integer code and asserts `(type-name object)` into
`*static-idb*`. It does **not** touch `(gethash type *types*)`, and cannot retroactively edit an
already-compiled `dolist` literal. Dynamically registered objects are reachable through a predicate
test or a query-domain `doall`, never through a static-domain one. It also errors past 999 total
planning objects (`:227`–`:228`).

**9. The `.asd` bootstrap cannot splice.**
`problem-blocks3.lisp` must stay free of `include-tech` directives. Documented in the `.asd`
comment; nothing enforces it mechanically.

---

## Applying this to the `crossing` question

The type extension for `crossing` must be final before the `(include-tech beam-crossing)` directive
in `probs/problem-corner-topo.lisp:93` is evaluated — because beam-crossing.lisp's five
`(doall (?x crossing))` sites translate at that moment (Stage 4, Trap 1). No `init()` reordering
reaches back that far, which rules out the placeholder-then-update approach in its literal form.

The workable routes are the two identified separately: compute the pool at `define-types` time via
`(compute ...)` (`install-types:83`), or convert those five sites to query-domain `doall` and
populate via `register-dynamic-object`, following the `beam` precedent in
`probs/problem-corner.lisp`. Trap 7 is the open question for the second route.

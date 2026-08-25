# Parameter Precedence

Companion to `ordering-of-operations.md`. That document answers *when* things happen; this one
answers *where a parameter's value came from and who wins*. The question it exists for is the
recurring one: "I set this at the REPL, then loaded a problem, and it reverted — why?"

Citations name files and functions, not line numbers.

---

## The four value sources

In the order they act during a system load:

**1. `vals.lisp` early restore.** `read-init-vals` (`ww-preliminaries.lisp`) fires from an
`eval-when` while `ww-preliminaries` loads — fourth in the serial component list. It reads
`vals.lisp` and restores only four parameters by position: `*problem-name*` (0), `*algorithm*` (2),
`*debug*` (11), `*threads*` (13). It also sets or clears the `:ww-debug` feature.

These four and only these four are restored this early, because each one changes how the rest of the
system compiles: `*algorithm*` selects translations, `*debug*` gates conditional compilation,
`*threads*` determines whether hash tables are `:synchronized` and whether the global-mutation macros
expand atomically. `reset-global-hash-tables` runs immediately after, from its own `eval-when`, and
depends on `*threads*` already being correct.

**2. `defvar` / `sb-ext:defglobal` defaults.** `ww-settings.lisp` establishes a value for every
parameter not already bound. Anything step 1 restored keeps its restored value.

**3. Problem-file `ww-set` forms.** Evaluated as `src/problem.lisp` loads. These override steps 1
and 2 — with exceptions, below.

**4. `vals.lisp` full restore inside `init()`.** Late in the load, `init()` chooses one of three
branches:

- `*refreshing*` is true → `save-globals` (write current values out; do *not* read)
- otherwise `vals.lisp` exists → `read-globals` (**overrides everything set in steps 1–3**)
- otherwise → `save-globals`

The middle branch is the one that surprises people. On an ordinary load with a `vals.lisp` present,
the problem file's own `ww-set` forms are applied in step 3 and then overwritten in step 4 by
whatever was saved from your last session. That is deliberate — it is what makes settings persist
across SBCL restarts — but it means a `ww-set` you add to a problem file will not appear to take
effect until `vals.lisp` is discarded or re-saved.

---

## What `vals.lisp` persists

`save-globals` and `read-globals` (`ww-interface.lisp`) write and read a single 18-element list.
Position matters — `read-init-vals` indexes into it directly.

| Pos | Parameter | Default |
|---|---|---|
| 0 | `*problem-name*` | `unspecified` |
| 1 | `*depth-cutoff*` | `0` |
| 2 | `*algorithm*` | `depth-first` |
| 3 | `*tree-or-graph*` | `graph` |
| 4 | `*problem-type*` | `planning` |
| 5 | `*solution-type*` | `first` |
| 6 | `*progress-reporting-interval*` | `100000` |
| 7 | `*randomize-search*` | `nil` |
| 8 | `*branch*` | `-1` |
| 9 | `*probe*` | `nil` |
| 10 | `*symmetry-pruning*` | `nil` |
| 11 | `*debug*` | `0` |
| 12 | `*goal*` | `nil` |
| 13 | `*threads*` | `0` |
| 14 | `*recorder-prefix-pruning*` | `nil` |
| 15 | `*max-recorder-cycles*` | `1` |
| 16 | `*min-steps-fallback-warmup*` | `512` |
| 17 | `*min-steps-fallback-sample-interval*` | `64` |

All managed defaults live in `*problem-parameter-defaults*`; the persisted subset and its
save/read order live in `*persisted-problem-parameters*`. `*default-parameters*` is derived from
those two registries. `read-globals` pads a short list from the defaults tail, so adding a
persisted parameter to the end of the list does not invalidate an existing `vals.lisp`.
The loader also recognizes the former 17-value format whose final two positions were the
retired recorder interleaving flags and replaces that tail with current defaults.

**Anything not in this table is not persisted.** That includes `*auto-wait*`, the
technology-specific `*max-connector-pairings*`, `*beam-occlusion-tolerance*`,
`*boundary-wall-height*`, and `*vertical-reach-limit*` parameters, and every parallel-search tuning
parameter — `*tasks-per-thread*`, `*min-tasks*`, `*split-depth-max*`,
`*bound-refresh-interval*`, `*donation-check-interval*`, `*donation-threshold*`,
`*donation-fraction*`, `*enable-work-donation*`, `*num-closed-shards*`. The non-persisted search
settings call `save-globals`, which writes the 18-element list and silently omits them;
the technology-specific settings only reprint the current parameters. Their REPL overrides survive a
`(refresh)`, but not restaging or restart. Staging restores every managed default first and then
applies the new problem specification's `ww-set` overrides.

---

## What `ww-set` does, by parameter

`ww-set` (`ww-set.lisp`) is a macro. Its first act is `(unless (and *refreshing* *ww-loading*) ...)`
— so during a `(refresh)`, every `ww-set` form in the problem file is skipped entirely, which is how
refresh preserves what you set at the REPL. Its second act is `check-problem-parameter`
(`ww-validator.lisp`), which rejects out-of-range values before anything is assigned.

| Parameters | Settable in problem file? | Settable at REPL? | Effect of a REPL set |
|---|---|---|---|
| `*depth-cutoff*`, `*progress-reporting-interval*`, `*randomize-search*`, `*branch*`, `*auto-wait*`, `*tasks-per-thread*`, `*min-tasks*`, `*split-depth-max*`, `*bound-refresh-interval*`, `*donation-*`, `*enable-work-donation*`, `*recorder-prefix-pruning*`, `*max-recorder-cycles*` | yes | yes | `save-globals` + reprint |
| `*max-connector-pairings*`, `*beam-occlusion-tolerance*`, `*boundary-wall-height*`, `*vertical-reach-limit*` | yes | yes | reprint only; a REPL override survives refresh but not restaging or restart; each is displayed only when its consuming technology and relevant problem objects or facts are present; vertical reach uses a conservative structural guard for a possible nonzero manipulation, landing, or barrier-clearance comparison and never performs search |
| `*solution-type*` | yes | yes | as above; warns if `backtracking` is paired with an optimizing type |
| `*num-closed-shards*` | yes | yes | as above; also recomputes `*closed-shard-mask*` |
| `*tree-or-graph*` | yes | yes | as above; refuses `graph` under `backtracking` |
| `*symmetry-pruning*` | yes | yes | **full `asdf:load-system :force t`** |
| `*problem-type*` | yes | yes | **full reload** (plain `setf` when loading) |
| `*debug*` | **no — errors** | yes | updates `:ww-debug`, then **full reload** |
| `*algorithm*` | **no — errors** | yes | may force `*tree-or-graph*` to `tree`, then **full reload** |
| `*probe*` | **no — errors** | yes | updates `:ww-debug`, zeroes `*debug*`, then **full reload** |
| `*threads*` | yes | yes | **full reload only when crossing the 0 ↔ non-zero boundary**, otherwise just reprints |
| `*problem-name*` | **yes — required here** | no — prints a refusal | — |

The three erroring parameters — `*debug*`, `*algorithm*`, `*probe*` — are rejected in a problem file
because each requires recompilation to take effect, and a problem file is read during the very
compile they would need to influence. The error message tells you to set them at the REPL after
staging.

`*threads*` is the mirror image: crossing the 0 boundary changes the `:synchronized` flag on every
global hash table and the atomic-vs-plain expansion of `increment-global`, `push-global`, and
friends, so it reloads. Changing 4 → 8 changes neither, so it does not. No SBCL restart is needed in
either case.

---

## Which entry point does what

| Command | `src/problem.lisp` | Problem-file `ww-set` forms | `vals.lisp` |
|---|---|---|---|
| `(ql:quickload :wouldwork)` | staged by `ensure-problem-staged` with no argument (autodetect) | applied, then overridden by `read-globals` if `vals.lisp` exists | read |
| `(stage <problem>)` / `(load-problem "<name>")` | re-spliced from the named source, then full reload | applied — this is how you get a problem's intended defaults | read if present |
| `(refresh)` | re-spliced from the *current* problem | **skipped** (`*refreshing*` is true) | written, not read |
| `(ww-reset)` | deleted, then rebuilt from `problem-blocks3.lisp` | applied from blocks3 | deleted |

So: **`(stage X)` gives you the problem's own settings; `(refresh)` gives you yours.** That
distinction is the whole point of the `*refreshing*` flag, and it is why `init()`'s vals branch is
three-way rather than two-way.

`ww-reset` is the escape hatch when a bad `vals.lisp` or a broken problem file prevents loading at
all — it deletes both and starts from the default problem.

---

## Staging is splicing, not copying

Worth stating here because the older version of this document got it wrong: every path that puts
content into `src/problem.lisp` goes through `ensure-problem-staged`, which calls
`copy-problem-with-tech-includes` — a recursive textual splice of every `(include-tech ...)` target
from `tech/`, not a file copy. The single exception is the ASDF bootstrap in `wouldwork.asd`, which
runs before those functions exist and does a plain `uiop:copy-file` of `problem-blocks3.lisp`. That
is why `problem-blocks3.lisp` must never contain an `include-tech` directive.

With no argument, `ensure-problem-staged` picks its source in three steps: if `src/problem.lisp` is
absent, splice blocks3 and delete `vals.lisp`; else if `vals.lisp` names a problem whose source file
exists, splice that; else delete the inconsistent `vals.lisp` and recover the source from
`problem.lisp`'s own snapshot header, re-splicing from it if one is found.

See `ordering-of-operations.md`, Stage 2, for what splicing freezes.

---

## Common confusions, answered

**"I edited a `ww-set` in my problem file and reloaded, but nothing changed."**
Step 4 read it back from `vals.lisp`. Use `(stage <problem>)`, or delete `vals.lisp`.

**"I set `*depth-cutoff*` at the REPL, then ran `(refresh)`, and it survived — but `(stage)` reset
it."**
Working as designed. `(refresh)` skips problem-file `ww-set` forms; `(stage)` applies them.

**"I tuned `*tasks-per-thread*`, restarted SBCL, and it's back to default."**
It isn't in the 18-element `vals.lisp` list. Nothing outside that list persists.

**"Setting `*threads*` reloaded the whole system."**
Only because the value crossed 0. Within either regime it is a plain assignment.

**"I put `(ww-set *debug* 3)` in my problem file and got an error."**
Intentional. `*debug*` gates conditional compilation; set it at the REPL after staging.

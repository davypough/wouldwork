# Wouldwork
  
*A classical AI planner in Common Lisp — PDDL-flavored, Lisp-powered, and REPL-friendly.*

Wouldwork is a **classical planning / search** system: you describe a world (objects, relations, actions, constraints, goals…), and it searches for **plans** that reach your goal. It’s “old-school AI” in the best way — but with a modern, developer-friendly workflow: **stage → solve → tweak → refresh**.

If you like planners, constraint-y domains, debugging search, or just want to feel the joy of a Lisp REPL doing AI… you’re home.

---

## What you get

Wouldwork supports a bunch of “classic planner” features, but with a **Lisp-native** problem language:

- **Objects, types, relations** (dynamic + static)
- **Actions** with preconditions/effects
- **Goals**, **constraints**, and **derived relations**
- **Functions** (including on-the-fly and even recursive computations)
- **Exogenous events** (things that happen independently of the agent)
- **Temporal planning** (action schedules / timestamps)
- **Search control**: depth cutoffs, tree vs graph search, randomization, branching
- **Solution modes**: first solution, shortest plan, min-time, min/max objective, enumerate all, …
- **Optional parallel search** (SBCL is the sweet spot)
- **Diagnostics & debugging hooks** (including step-through search)

For a gentle narrative intro, see the Medium article:
https://medium.com/@davypough/traditional-ai-problem-solving-with-wouldwork-fcb0c4a71226

---

## Installation (pick your flavor)

Wouldwork is available via **Quicklisp**, **Ultralisp**, and **GitHub / Roswell**.

### Option A — Quicklisp (simplest)
```lisp
(ql:quickload :wouldwork)
(in-package :ww)
(test)           ; sanity check
(help)           ; see available commands
```

### Option B — Ultralisp
```lisp
(ql-dist:install-dist "http://dist.ultralisp.org/" :prompt nil)
(ql:quickload :wouldwork)
(in-package :ww)
(test)           ; sanity check
(help)           ; see available commands
```

### Option C — GitHub via Roswell (nice if you don’t already have SBCL/Quicklisp)

Install Roswell (examples):
```bash
# macOS
brew install roswell

# Debian/Ubuntu
sudo apt update && sudo apt install libcurl4-openssl-dev automake
# (or use your distro’s roswell package)
```

Install SBCL + Quicklisp:
```bash
ros install sbcl
ros install quicklisp
```

(Optional but recommended: nicer REPL history/editing)
```bash
# macOS
brew install rlwrap

# Debian/Ubuntu
sudo apt install rlwrap
```

Install Wouldwork from GitHub:
```bash
ros install davypough/wouldwork
```

Run SBCL and load Wouldwork:
```bash
rlwrap ros run
```

Inside SBCL:
```lisp
(ql:quickload :wouldwork)
(in-package :ww)
(test)           ; sanity checks
(help)           ; see available commands
```

---

## Ultra-Quickstart (the “coffee break” version)

Start SBCL, then:

```lisp
(ql:quickload :wouldwork)
(in-package :ww)

(help)     ; shows the REPL command menu
(test)     ; runs a bunch of example problems

(run "blocks3")   ; stage + solve a sample domain
```

That’s it. You’ll see a plan printed, plus search diagnostics depending on your settings.

---

## The Wouldwork REPL workflow

Wouldwork is designed for iterative development: write a problem file, run it, tweak it, re-run it.

### Core commands

```lisp
(stage <problem-name>)   ; load a problem spec (compile + prepare), don't solve yet
(solve)                  ; solve the currently staged problem
(refresh)                ; recompile/reload the current problem after edits
(run <problem-name>)     ; stage + solve in one go
```

Useful helpers:

```lisp
(list-all-problems)      ; (probs) in the REPL help text
(display-current-parameters)  ; (params) in the REPL help text
(test)                   ; solve the included test problems
```

If anything gets into a weird state:

```lisp
(ww-reset)               ; reinitialize Wouldwork after an error (and continue)
```

### Settings you’ll actually touch

Wouldwork settings are “earmuffed” globals, and you change them with `ww-set`:

```lisp
(ww-set *solution-type* 'first)       ; default
(ww-set *solution-type* 'min-length)
(ww-set *solution-type* 'min-time)
(ww-set *solution-type* 'min-value)
(ww-set *solution-type* 'max-value)
(ww-set *solution-type* 'every)

(ww-set *tree-or-graph* 'graph)       ; or 'tree
(ww-set *depth-cutoff* 0)             ; 0 = no limit (go as deep as needed)
(ww-set *progress-reporting-interval* 100000)

(ww-set *randomize-search* t)         ; random DFS
(ww-set *branch* -1)                  ; -1 = all branches
```

Debugging knobs:

```lisp
(ww-set *debug* 5)    ; step through search one expansion at a time
;; *debug* = 0..4 gives increasing levels of debug output
```

> Practical tip: set most domain-specific parameters in your problem file,  
> then use `ww-set` at the REPL while experimenting. It’s a nice rhythm.

---

## Enumerator backend

The goal-state enumerator now has a single execution path.

Use:

```lisp
(find-goal-states)
```

---

## “Wait… it remembers my last session?” (Yes.)

Wouldwork keeps your working context in a small file (`vals.lisp`) inside the system directory.  
That means when you restart your Lisp image, it can restore:

- the last problem you staged
- your parameter choices (algorithm, search settings, debug level, …)

This is deliberate: planning is exploratory, and retyping 12 parameters every session is pain.

There’s also a small but important design choice: when you call `refresh`, Wouldwork avoids clobbering the values you just set at the REPL (so your live tuning stays live).  

If you ever want to wipe state clean and go back to defaults, use the reset command from the REPL.

---

## Solution types (how the planner “aims”)

Wouldwork can search for different notions of “best,” depending on what you’re doing:

- `first` — find the first goal-satisfying plan encountered (fastest feedback)
- `min-length` — shortest number of actions
- `min-time` — least total time (when actions have durations)
- `min-value` / `max-value` — optimize an objective function (even without a goal)
- `every` — enumerate solutions (careful: state spaces explode)

Worth knowing:

- Tree search vs graph search matters a lot for `every`.
- With graph search, repeated states are pruned (so alternative paths to the *same* state may be skipped).
- There’s a **hybrid mode** when `every` + graph search + a positive depth cutoff are combined, to enumerate distinct action sequences up to that bound while staying memory-conscious.

After a run:

- `*solutions*` contains all found solution paths.
- `*unique-solutions*` contains one solution path per unique goal state.

---

## Debugging: invariants + “troubleshoot mode” (surprisingly nice)

When you’re writing action rules, the hardest part is usually not “search” — it’s *domain consistency*.

Wouldwork gives you a very practical tool for that: **invariants**, both local (inside updates) and global (checked across generated states). When an invariant fails, Wouldwork can drop you into a troubleshooting environment that helps you see what happened and why.

The user manual has several good examples (including invariant checks like “this list must be a set” and “cargo cannot be both held and located”).

During development you can also sprinkle in diagnostics:
```lisp
(ut::prt some-expression other-expression)
```

…and later remove them once the domain is stable.

---

## A small taste of “Lisp-powered PDDL”

Wouldwork’s problem files are **PDDL-inspired**, but they’re not trying to be PDDL.  
The point is: you get a familiar planning structure *plus* the ability to use Lisp where it helps.

Example (high-level sketch):

- `define-types` — objects and types
- `define-dynamic-relations` / `define-static-relations`
- `define-actions` — actions with preconditions/effects
- `define-invariant` — global safety conditions
- `define-query` / functions — computed relations / heuristics / bounds

And yes: you can include **arbitrary Common Lisp** where it makes the domain clearer or faster.

---

## Performance notes (SBCL recommended)

Wouldwork was originally designed to take advantage of SBCL performance features and can optionally use parallelism there. It also runs on CCL via more generic libraries, but expect large problems to be noticeably slower.

If you run into memory limits for bigger searches, give SBCL more heap:

```bash
sbcl --dynamic-space-size 10000
```

(Scale as needed: 2048, 4096, 8192, 24000, … depending on your machine.)

---

## Documentation

- **User Manual**: `Wouldwork User Manual(26.7).docx` (in this repo)
- **Parameter loading notes** (for the curious): `parameter loading sequence.txt`
- **Medium intro**: https://medium.com/@davypough/traditional-ai-problem-solving-with-wouldwork-fcb0c4a71226

---

## Want a fun example?

If you like “plans as timelines” and multi-agent-ish traces, check out the recorder / windtunnel artifacts in this repo (there’s a full integrated sequence table showing how a solution can be post-processed into recording/playback phases).

---

## Contact

For questions, feedback, or domain-wrangling war stories, contact the author:

Dave Brown — davypough@gmail.com

For questions or suggestions for improvements of the user interface, you can also alternatively poke:

Gwang-Jin Kim - gwang.jin.kim.phd@gmail.com

---

## Contributing & Support

- If you have a question, idea, or bug report: open a **GitHub issue**.
  Please search first (including closed issues) to avoid duplicates. 
  Tip: use GitHub search filters like:

```
is:issue is:closed <your-keywords>
```

- **Collaborators are welcome**. 
  Reasonable, well-scoped suggestions and PRs are appreciated — especially 
  when they come with a small repro, a test, or a concrete example domain.


---

## License

(See the LICENSE file.)

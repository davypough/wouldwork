# Quickstart

Goal: **run Wouldwork once** and understand what “success” looks like.

## 0) Prereqs
- You need a Common Lisp implementation (the original quickstart uses SBCL).
- You need the Wouldwork sources available and loadable in your Lisp.

## 1) Run SBCL the way the manual recommends

Use the same launch flags as in the manual (important for larger searches).

```lisp
> sbcl --dynamic-space-size 2048
```

If you hit memory pressure later, this is the first knob to revisit.

## 2) Load Wouldwork and run a sample problem

Pick *one* of the appendix problems as your first run (Blocks World is usually ideal).

1. Open `PROBLEM-COOKBOOK.md`
2. Copy the full problem specification into a file (or use the provided sample file if present)
3. Start the planner run as described in that recipe

## 3) What success looks like
- The planner prints a **plan** (sequence of actions), or
- It tells you there is **no solution** within the given bounds/settings.

## 4) If you get stuck
- Reduce problem size (fewer objects).
- Relax constraints.
- Check that action preconditions/effects actually allow progress.
- Use trace/debug output (see `REFERENCE.md` and `FEATURES.md`).

## Source extract

<details>
<summary><strong>Quickstart with SBCL (source extract)</strong></summary>

Install the SBCL Common Lisp release for your computer from http://www.sbcl.org/platform-table.html
Install Quicklisp from https://www.quicklisp.org/beta/
Clone or download the Wouldwork repository to your computer from https://github.com/davypough/quick-wouldwork placing it in your ~/quicklisp/local-projects/ directory (the path to the project files should then be ~/quicklisp/local-projects/quick-wouldwork/)
Start SBCL from your terminal command prompt.
At the SBCL prompt, enter (ql:quickload :wouldwork :force t)
Enter (in-package :ww) to switch the current package from cl-user to wouldwork.
Enter (run-test-problems), or just (test), to verify everything is loaded and running properly.
Review the printout from any of the test problems to see the format of solutions.
Look at the sample problem specifications (eg, problem-blocks3.lisp) in the src directory to become acquainted with how problems are defined.

Once you have the SBCL Common Lisp environment installed (I like SBCL because it is open-source and compiles into speedy code), you can add your own problem specifications or modify the source code. Note that the environment will need Quicklisp and ASDF pre-installed as a baseline, plus the :alexandria, :iterate, and :lparallel Quicklisp libraries (automatically installed when loading Wouldwork). Check the file wouldwork.asd for detailed information about what Wouldwork loads.

Also note that SBCL loads with 1024MB = 1GB of memory by default. If a planning problem exits with an out-of-memory condition (heap exhausted), you will need to increase the default by loading SBCL at the command prompt with additional memory, for example:

```lisp
> sbcl –dynamic-space-size 2048

```
which doubles the default (or 4096, 8192, etc, depending how much memory you have).

Planner parameters (*depth-cutoff*, *solution-type*, etc) are either set in the problem specification, at the REPL after loading Wouldwork, or simply left at their default values. Finally, executing (run <problem-name>) runs the planner on the specification file. To see a list of all currently defined problems in the /src directory, enter (list-problem-names). Enter (help) at the REPL to see a list of all available commands.

</details>

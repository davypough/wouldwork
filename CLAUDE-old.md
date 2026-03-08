# Wouldwork Planner — Claude Context

## Current Focus (edit this section as work shifts)

**Area:** CSP enumeration archetecture
**Goal:** Develop meet-in-the-middle strategy for solving difficult problems
**Subgoal:** Implement (solve-meeting-point :depth-cutoff N) efficiently
**Key files:** `ww-enumerator-build.lisp`, `ww-enumerator-run.lisp`
**Recent findings:** Solve-meeting-point is partially working
**Next step:** Debug the CSP enumeration architecture & codebase

---

## Collaboration Conventions

- After developing an implementation plan in a .md file, put it in `wouldwork/doc` so we can easily reference it when we start a new development session
- Common Lisp expert tone; step-by-step for procedural/coding tasks
- Ask for clarifications to avoid rumination
- Present entire revised functions with changed lines marked
- One issue at a time; test before moving to the next
- Avoid: `labels`/`flet` (use separate functions), one-line helper functions (inline instead), defensive programming (let errors surface)
- Code style: two blank lines between top-level definitions, no blank lines inside functions, high-level before low-level ordering
- Always read current files before suggesting changes — never rely on prior conversation state
- After edits, provide a REPL test expression
- User will do the testing in the local environment and report back results
- Assume user always reloads fully before testing: `(progn (ql:quickload :wouldwork) (in-package :ww))`

## Project Structure

```
wouldwork/
  wouldwork.asd          -- ASDF system definition, serial load order
  src/
    ww-packages.lisp     -- Package defs (:wouldwork/:ww, :utilities/:ut, :hstack/:hs)
    ww-utilities.lisp    -- General utilities
    ww-hstack.lisp       -- Hash-based stack data structure
    ww-preliminaries.lisp -- Forward declarations, early setup
    ww-settings.lisp     -- All *parameter* defaults and ww-set macro
    ww-structures.lisp   -- Core structs: state, action, problem, etc.
    ww-converter.lisp    -- Problem spec → internal representation
    ww-validator.lisp    -- Problem spec validation
    ww-frequencies.lisp  -- Macro operator frequency analysis
    ww-support.lisp      -- Shared support functions
    ww-happenings.lisp   -- Exogenous event scheduling
    ww-translator.lisp   -- Translates user specs into executable forms
    ww-installer.lisp    -- Installs translated problem into runtime
    ww-patroller-installer.lisp -- Patroller (mobile exogenous object) setup
    ww-interface.lisp    -- User-facing commands: run, stage, solve, etc.
    ww-problem-tests.lisp -- Built-in test problem registry
    ww-set.lisp          -- ww-set implementation
    ww-command-tests.lisp -- Command validation from REPL
    ww-enumerator-build.lisp -- Constructs the CSP actions from user specs
    ww-enumerator-run.lisp   -- CSP enumeration engine (generates variable assignments)
    problem.lisp         -- Currently loaded problem spec (always-recompiled)
    ww-action-trace.lisp -- Action trace recording
    ww-goal-chaining.lisp -- Sequential subgoal continuation
    ww-solution-validation.lisp -- Solution correctness checking
    ww-backward.lisp     -- Backward search / regression
    ww-planner.lisp      -- Core planning loop
    ww-symmetry.lisp     -- Symmetry detection and pruning
    ww-searcher.lisp     -- Depth-first search driver
    ww-backtracker.lisp  -- Backtracking search driver
    ww-parallel-infrastructure.lisp -- Thread pool, work splitting
    ww-parallel.lisp     -- Parallel search coordination
    ww-initialize.lisp   -- System initialization, entry point
  problem-*.lisp         -- Sample problem specifications
```

Files load serially in the order listed in `wouldwork.asd`. `problem.lisp` is always recompiled (via `always-compile-file` ASDF class). It is pre-scanned at compile time to extract defun names, query names, update names, and happening names for forward-reference stubs.

## Architecture Overview

Wouldwork is a classical planner that searches state spaces via depth-first or backtracking algorithms. It solves two problem types:

1. **Planning** — find action sequences transforming an initial state into a goal state
2. **CSP** — find variable assignments satisfying constraints (no action sequencing)

It contains a set of parameters that the user can set specifying how the problem is to be solved, for example:
  *PROBLEM-NAME* => CORNER
  *PROBLEM-TYPE* => PLANNING
  *ALGORITHM* => DEPTH-FIRST
  *TREE-OR-GRAPH* => GRAPH
  *SOLUTION-TYPE* => MIN-LENGTH
  *DEPTH-CUTOFF* => 15
  *PROGRESS-REPORTING-INTERVAL* => 1000000
  *THREADS* => 0
  *RANDOMIZE-SEARCH* => NIL
  *DEBUG* => 0
  *PROBE* => NIL
  *GOAL* => (AND (LOC AGENT1 AREA4) (ACTIVE RECEIVER2) (ACTIVE RECEIVER3))
  *AUTO-WAIT* => NIL
  *SYMMETRY-PRUNING* => T
  BRANCH TO EXPLORE => ALL
  HEURISTIC? => NIL
  EXOGENOUS HAPPENINGS => NIL
  BOUNDING FUNCTION? => NIL
  MIN STEPS REMAINING? => YES

### Core Execution Flow

`(stage "problem-name")` → (load/compile problem spec) → `(solve)` (execute search)

During staging: problem spec is translated into internal action representations, relations are indexed, types are resolved, happenings are scheduled.

During solving: the searcher expands states by instantiating action rules against the current state's propositions. Each action's parameter list drives variable generation (standard permutations, combinations, or dot-products). Action preconditions filter, effects produce successor states.

### State Representation

States are lists of propositions (ground literals). The database supports efficient lookup of propositions by relation name. Fluent variables (`$var`) bind via database lookup; generated variables (`?var`) enumerate type instances.

### Search Strategies

- **Depth-first** (`ww-searcher.lisp`): Uses OPEN/CLOSED lists. Supports graph search (duplicate detection) and tree search. Handles heuristic ordering.
- **Backtracking** (`ww-backtracker.lisp`): No CLOSED — more memory efficient for trees but can't detect repeated states.
- **Parallel** (`ww-parallel.lisp`): Splits top-level branches across thread pool.

### CSP Mode

When `*problem-type*` is `csp`, Wouldwork treats each action rule as a variable-assignment operator

### Enumerator Mode

The enumerator (`ww-enumerator.lisp`) is an off-line analysis tool for problem solving using a meet-in-the-middle strategy for difficult problems. It works backwards from candidate goal states (find-goal-states) to earlier states (find-predecessors :direction forward) which then serve as intermediate goal states for a normal forward search.  The :direction forward mode in find-predecessors is intended to be sound and complete.

### Key Parameters

| Parameter | Default | Purpose |
|-----------|---------|---------|
| `*problem-type*` | `planning` | `planning` or `csp` |
| `*algorithm*` | `depth-first` | `depth-first` or `backtracking` |
| `*solution-type*` | `first` | `first`, `every`, `min-length`, `min-time`, `min-value`, `max-value`, or N |
| `*tree-or-graph*` | `graph` | `tree` or `graph` |
| `*depth-cutoff*` | `0` | Max depth (0 = unlimited) |
| `*symmetry-pruning*` | `nil` | Detect and prune symmetric states |
| `*debug*` | `0` | 0-5, controls diagnostic output |
| `*threads*` | `0` | Number of parallel worker threads |

### Problem Spec Anatomy

```lisp
(define-types ...)              -- Objects and type hierarchy
(define-dynamic-relations ...)  -- State-changing relations (with optional :bijective)
(define-static-relations ...)   -- Unchanging relations
(define-complementary-relations ...) -- Auto-maintained negation pairs
(define-action name duration params precond effect-params effect)
(define-query name (args) body)   -- Read-only state analysis
(define-update name (args) body)  -- State-modifying functions
(define-init ...)               -- Initial propositions
(define-goal ...)               -- Goal condition (predicate logic)
(define-constraint ...)         -- Global kill-switch constraint
(define-happening ...)          -- Exogenous events
(define-patroller ...)          -- Mobile exogenous objects
(define-init-action ...)        -- One-time initialization actions
```

### Variable Conventions in Problem Specs

- `?var` — generated variable: enumerates all instances of its type
- `$var` — fluent variable: binds by database lookup (efficient, no enumeration)
- `$objective-value` — reserved fluent for min-value/max-value optimization

### Coding Conventions

- Avoid defensive programming that could hide legitimate errors
- Avoid using `labels` and `flet` functions; use first-class functions instead
- Keep function size modest to avoid parenthesis errors
- Break large functions into smaller functions after editing
- Make changes directly in the wouldwork project directory to avoid subsequent merging
- Note that we are editing on a Windows 11 platform to avoid whitespace and CRLF errors

## Testing

```lisp
;; Solve a problem
(progn (ql:quickload :wouldwork) (in-package :ww))  ;; Compile and load wouldwork
(stage "blocks3")           ;; Compile and load a problem
(solve)                     ;; Solve the staged problem

;; Inspect results
*solutions*               ;; All solution paths
*unique-solutions*        ;; Deduplicated by goal state

;; Run a test suite
(run-test-problems)       ;; Full test suite (aliased as (test))
```

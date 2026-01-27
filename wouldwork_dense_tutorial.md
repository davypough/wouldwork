# Wouldwork Planning Software — Condensed Hands‑on Manual

> **Source:** Synthesised from the original *Wouldwork Planning Software User Manual (26.7, 2025)*. This rewrite keeps all essential concepts and examples while drastically reducing length.

## Table of Contents

1. [Introduction](#introduction)
2. [Quickstart](#quickstart)
3. [Modelling Tutorial](#modelling-tutorial)
   - [Objects & Types](#objects--types)
   - [Relations](#relations)
   - [Actions](#actions)
   - [Initial State](#initial-state)
   - [Goal Condition](#goal-condition)
   - [Debugging & Scaling](#debugging--scaling)
4. [Reference](#reference)
5. [Optional Features](#optional-features)
6. [Search Strategies](#search-strategies)
7. [Problem Cookbook](#problem-cookbook)

---

## Introduction

**Wouldwork** is a **classical planner/general problem solver** implemented in Common Lisp.  You supply a *problem specification*—objects, relations, possible actions, the initial world state, and the desired goal—and the planner searches for a sequence of actions that transforms the initial state into the goal state.

### When to use Wouldwork

- Your problem can be expressed in terms of **discrete, symbolic facts** and **deterministic actions**.
- You need **explainability** or an explicit plan trace.
- You want to **enforce complex logical constraints** (e.g. non‑overlapping tasks, dependencies).

Common fits include puzzle‑like domains (e.g. blocks world, jugs, solitaire), scheduling with explicit resource constraints, logistics problems, or simple constraint satisfaction problems (CSPs).

### When *not* to use Wouldwork

- Problems dominated by **continuous control** or heavy **uncertainty/noise**.  
- Tasks requiring **learning from data**; in those cases consider machine‑learning or probabilistic methods instead.  
- Domains with vast branching factors and no domain structure—state explosion may make search infeasible.

A rule of thumb: if you can clearly write down the possible actions and their effects, and you care about the exact sequence of steps, Wouldwork is a good fit.

### How Wouldwork works

Wouldwork performs a **depth‑first search** over the space of possible action sequences.  It enumerates every legal instantiation of each action at each step, checking the preconditions and applying the effects to generate successor states.  You can control search parameters (e.g. depth limit, number of solutions, parallelism) to trade off completeness vs. runtime.  The planner can find *all* solutions or stop at the first (shortest) one.

---

## Quickstart

This section gets you from a blank machine to a first working plan.

### 0. Install prerequisites

Wouldwork runs on any Common Lisp implementation, but the **Steel Bank Common Lisp** (SBCL) compiler is recommended for performance.  You also need **Quicklisp** for package management.  

1. Install SBCL from the official site.  
2. Install Quicklisp from [quicklisp.org](https://www.quicklisp.org/).  
3. Download or clone the Wouldwork source into your Quicklisp local projects directory (e.g. `~/quicklisp/local-projects/`).  

### 1. Start the planner

Start SBCL with enough heap memory (important for non‑trivial searches):

```lisp
sbcl --dynamic-space-size 2048  ;; 2 GB heap, increase if you run out of memory
```

Then load the package and switch to its namespace:

```lisp
(ql:quickload :wouldwork :force t)
(in-package :ww)
```

### 2. Run a sample problem

Pick one of the **Problem Cookbook** recipes below (Blocks World is a good first example).  Copy the problem specification into a file (e.g. `problem-blocks3.lisp`) and load it:

```lisp
(run 'blocks3)  ;; or whatever the problem name is
```

The planner will either print a **plan** (a sequence of actions) or report that no solution exists within the given bounds.

### 3. Next steps and troubleshooting

- **Out of memory?** Increase the `--dynamic-space-size` argument when starting SBCL.  
- **No solution found?** Check your preconditions, effects, and goal for correctness.  Relax constraints or reduce object counts.  
- **Debugging/tracing:** Use the parameters `*debug*` or `*progress-reporting-interval*` to get more output.  See [Reference](#reference).

---

## Modelling Tutorial

This tutorial walks you through the core modelling steps.  Each step includes a pattern and an example from the **Blocks World** problem, where the goal is to stack three blocks (`A`, `B`, `C`) on a table `T`.

### Objects & Types

First define the **types** and **objects** in your domain.  Types can form hierarchies, and you can define “either” types to refer to unions of types (e.g. *support* in Blocks World is either a block or the table).

```lisp
(define-types
  block  (A B C)
  table  (T)
  support (either block table))  ; supports are either blocks or the table
)
```

Objects can also be numbers or arbitrary Lisp objects for discrete values (e.g. locations on a grid).

### Relations

Next define **dynamic relations**—facts that can change between states.  For Blocks World there is only one: `(on block support)` indicating a block is on another block or on the table.

```lisp
(define-dynamic-relations
  (on block support))
```

Static relations never change during planning (e.g. `supports? block support` might be static).  Static relations are declared similarly but are fixed.

### Actions

Actions describe how to change the state.  Each action has:

1. A name.
2. A duration (in time units).
3. A list of **free parameters** (variables used in preconditions).
4. A **precondition**, a logical expression that must hold before the action can occur.
5. A list of **effect parameters** (variables used in effects).
6. An **effect**, describing how the state changes.

In Blocks World there is one action: **`put`**—move a block onto a support.  The precondition ensures both the block and target support have clear tops, and the effect updates the `(on ...)` relation accordingly.

```lisp
(define-action put
  1                      ;; duration
  (?block block          ;; free variables
   (?block-support ?support) support)
  (and                  ;; precondition
   (not (exists (?b block) (on ?b ?block)))        ; ?block is clear
   (on ?block ?block-support)                      ; ?block is on something
   (not (exists (?b block) (on ?b ?support))))     ; target support is clear
  (?block ?block-support ?support)                 ;; effect variables
  (assert (on ?block ?support)                    ;; effect: put ?block on ?support
          (not (on ?block ?block-support))))
```

Under the hood Wouldwork instantiates all combinations of parameters and tests the precondition for each.  Successful instantiations produce successor states when the effect is applied.

For complex logic, you can use **functions** and **fluents** to simplify expressions (e.g. `(cleartop? ?block)` returns true if no block is on top of `?block`).  See [Optional Features](#optional-features).

### Initial State

Describe the world at time 0 by listing all true facts.  Facts not listed are assumed false (closed‑world assumption).

```lisp
(define-init
  (on A T)
  (on B T)
  (on C T))
```

### Goal Condition

The goal is a logical expression that must be true for a plan to be a solution.  When the planner encounters a state in which the goal holds, it records the path (plan) to that state.

```lisp
(define-goal
  (and
   (on C T)
   (on B C)
   (on A B)))
```

The planner can search for **every** solution or stop at the first one, depending on settings (see `*solution-type*`).

### Debugging & Scaling

Classical planning problems often fail because the model is wrong or too large.  Use this checklist:

- **Variables and objects:** Are all necessary objects defined?  Are there unnecessary ones that blow up the state space?  
- **Preconditions/effects:** Do actions have complete and correct preconditions?  Do effects remove and add exactly the right facts?  
- **Goal:** Is the goal reachable?  Are there mutually exclusive constraints?  
- **Incremental build:** Start with the smallest instance; test each action individually; add constraints gradually.

When scaling up, watch out for **state explosion**.  Use domain‑specific heuristics or bounding functions (see [Search Strategies](#search-strategies)) to prune the search.  You can also exploit symmetry (e.g. identical objects) by adding symmetry‑breaking constraints.

---

## Reference

This section lists key planner parameters and functions.  Parameters can be set within problem specifications, at the REPL, or left at their default values.

### Core parameters

- `*problem-name*` – Name of the current problem specification file.  
- `*problem-type*` – `planning` or `csp`.  
- `*solution-type*` – `every` to enumerate all solutions, `shortest` for minimal plan length, `first` for the first solution found.  
- `*depth-cutoff*` – Maximum search depth (0 = unbounded).  
- `*tree-or-graph*` – Whether to treat the search space as a tree or a graph (graph avoids revisiting states).  
- `*threads*` – Number of threads for parallel search.  
- `*randomize-search*` – If true, randomize action order (useful for avoiding deterministic worst‑case behaviour).  
- `*debug*` – Controls diagnostic output.  A larger number prints more detail.  
- `*progress-reporting-interval*` – Frequency (in node expansions) for status updates.  

### Useful commands (within `:ww` package)

- `(list-problem-names)` – List all loaded problem specifications.  
- `(run <problem-symbol>)` – Run the planner on the given problem.  
- `(test)` or `(run-test-problems)` – Run a suite of built‑in sample problems.  
- `(help)` – Show built‑in documentation.  

### User functions and Lisp integration

You can define arbitrary Lisp functions in problem files and call them in action rules, preconditions, or effects.  Use them to compute numerical fluents, custom predicates, or parameter bindings (e.g. `(cleartop? x)`).

Fluents (variables that take numeric values) are declared in optional sections (see [Optional Features](#optional-features)).  You can combine fluents with arithmetic or other functions in preconditions or effects.

### Output and debugging tools

Wouldwork can print detailed traces of action instantiations, state transitions, and goal checks.  Use `*debug*` ≥ 1 to see high‑level steps, or ≥ 2 for full instantiation details.  The environment also includes functions to inspect plans and validate solutions.

---

## Optional Features

Wouldwork extends standard PDDL in several directions.  These features are optional but powerful; they allow you to model more complex domains succinctly.

### Type hierarchies and mixing types

You can define hierarchies beyond simple unions.  Types can have subtypes, and parameters can accept multiple types.  For example:

```lisp
(define-types
  vehicle   (car truck bike)
  electric-vehicle (either car truck)  ; subset of vehicle
)
```

### Static vs. dynamic relations

Relations that never change can be declared static.  The planner then omits them from successor state generation, improving performance.  Use `(define-static-relations ...)`.

### Extended logical constructs

Wouldwork’s precondition and effect language includes:

- `exists` / `forall` – Quantification.  
- `and`, `or`, `not`, `if` – Standard logical connectives.  
- Named relations and user functions (predicate logic).  
- Embedded Lisp code.  

Arguments can be typed variables, fluents, integers, real numbers, or Lisp expressions.

### Fluents and binding

Fluents are variables whose values change over time (e.g. fuel level, distance).  They can be integers or reals.  Use `(define-fluents ...)` to declare them.  You can bind them in preconditions using `(bind ...)` and update them in effects with `(assert (assign ...))`.

### Global and dynamic variables

You can declare global variables that persist across the entire plan (e.g. a global time counter), and dynamic variables that are scoped to particular actions or effects.  Use `(define-global-vars ...)` and `(define-dynamic-vars ...)`.

### Durative actions and exogenous events

Actions can take non‑unit time using the duration field.  Wouldwork supports **temporal planning** where actions overlap.  Exogenous events (occurring independently of the agent) can be declared to happen at certain times or under certain conditions.

### Complementary relations

Complementary relations allow you to automatically maintain mutually exclusive facts (e.g. `(on x y)` implies `(not (on x z))` for all other `z`).  Declare them in the problem file to simplify action rules.

### Temporal operators and plan monitoring

The language supports temporal operators like `next` and `finally` to refer to future states, and allows asserting constraints across time.  You can also embed plan monitors that check invariants or goal conditions during search.

### Parameter lists and iteration

Parameters may be grouped or iterated over.  You can specify parameter lists in action definitions to restrict instantiations (e.g. grouping a pair of related variables).  Use `for` loops or comprehensions to iterate over numeric ranges or sets in fluents.

### Query/update functions

During planning you can query the current state using user‑defined functions and update local variables on the fly.  This allows dynamic programming constructs within a largely declarative specification.

### Solution validation

Wouldwork includes tools to **validate** a computed plan by replaying the actions from the initial state and checking that the goal condition holds at the end.  Use this to catch modelling bugs before deploying plans.

---

## Search Strategies

Although Wouldwork’s default algorithm is a depth‑first search, it can be tuned to solve different problem classes more effectively.  Here are some practical tips:

- **Bounded DFS:** Set `*depth-cutoff*` to limit the search depth.  Combined with `*solution-type* 'first` this can find shallow solutions quickly.  
- **Heuristic pruning:** Define a **bounding function** that estimates progress to the goal; if the function returns a value exceeding a threshold, the branch is pruned.  See the examples in the original manual for custom heuristics.  
- **Symmetry breaking:** If your domain has symmetric objects (e.g. three identical jugs), add constraints that order them to reduce redundant search.  
- **Macros/macro‑actions:** Combine frequently used sequences of actions into single actions to reduce branching.  
- **Bidirectional search:** Implemented as a custom strategy, this searches forward from the initial state and backward from the goal, meeting in the middle.  Works well for problems with shallow solutions.  
- **Constraint satisfaction:** When a problem is purely a CSP (no sequential actions), you can represent it as a planning problem with a single “assign” action and use search parameters to explore variable assignments.  
- **Parallel search:** Use multiple threads (`*threads*`) to explore different branches concurrently; this can speed up difficult searches on multi‑core machines.

When choosing a strategy, consider the **branching factor** and **goal depth**.  Shallow, narrow problems are easily solved by brute‑force; deep or wide problems need heuristics and symmetry reductions.

---

## Problem Cookbook

The appendix of the original manual contains several fully worked examples.  Here each is presented as a **recipe** with a problem story, modelling highlights, and instructions to run.

### Blocks World (3 blocks)

**Story:** Three blocks (`A`, `B`, `C`) start on a table `T`.  Goal: stack them so that `C` is on `T`, `B` on `C`, and `A` on `B`.

**Highlights:**  
- Types: `block`, `table`, `support` (either block or table).  
- Relation: `(on block support)`.  
- Action: `put` as defined in [Actions](#actions).  
- Initial state: `(on A T)`, `(on B T)`, `(on C T)`.  
- Goal: `(on C T)`, `(on B C)`, `(on A B)`.  

**Run:** Save the problem specification (the above definitions) to `blocks3.lisp`.  Load Wouldwork and run `(run 'blocks3)`.  The planner will print the two‑step plan:

1. `(put B C)`  
2. `(put A B)`

### Blocks World (3 blocks, alternative)

Same as above but using the fluent `cleartop?` function to simplify preconditions:

```lisp
(define-action put
  1
  (?block block (?block-support ?support) support)
  (and (cleartop? ?block)
       (bind (on ?block $block-support))
       (cleartop? $support))
  (?block ?block-support ?support)
  (assert (on ?block ?support)
          (not (on ?block ?block-support))))
```

### Blocks World (4 blocks)

Extend the previous specification by adding block `D`.  Increase `*depth-cutoff*` if necessary; the minimal plan has three steps.  Otherwise the modelling is identical.

### Boxes

**Story:** You have boxes that can contain other boxes.  Each box can hold only one item at a time.  The goal is to arrange boxes inside each other according to a target nesting.

**Highlights:**  
- Objects: boxes and a table.  
- Relation: `(in box container)` where `container` can be another box or the table.  
- Actions: `put-in` (place a box inside another) and `remove` (take a box out).  
- Additional constraints ensure no box contains itself and maintain single occupancy.

**Run:** Follow the same pattern as Blocks World: define types, relation `(in ...)`, actions `put-in` and `remove`, initial state, goal state, and run `(run 'boxes)`.

### 2‑Jugs (Water Jug Problem)

**Story:** You have two jugs of different capacities (e.g. 3 L and 5 L) and a fountain.  The goal is to measure a specific amount of water (e.g. 4 L) using fill, empty, and pour actions.

**Highlights:**  
- Use numeric fluents to represent jug volumes.  
- Actions: `fill`, `empty`, `pour`.  
- Preconditions and effects modify fluent values (e.g. `(assign vol‑jug1 (+ vol‑jug1 amount))`).  
- Goal: `(= vol-jug1 4)` or `(= vol-jug2 4)`.

### Sentry

**Story:** A robot must patrol several checkpoints on a grid and return to base while avoiding obstacles.

**Highlights:**  
- Objects: cells of a grid, obstacles, the robot.  
- Relation: `(at robot cell)`, `(connected cell cell)`.  
- Actions: `move` between adjacent cells.  
- Goal: visit all checkpoints and return to the start.  
- Use quantifiers to express “all checkpoints visited”.

### 4‑Queens

**Story:** Place 4 queens on a 4×4 chessboard so none attack each other.

**Highlights:**  
- Objects: rows and columns (1..4).  
- Relation: `(queen row col)`.  
- Use static constraints: no two queens share a row, column, or diagonal.  
- Action: `place` a queen in a specific row and column.  
- Search can be pruned by enforcing ordering (e.g. place queens row by row).

### Smallspace

**Story:** Arrange furniture in a small apartment to satisfy size and adjacency constraints.  Each item occupies a certain number of squares on a grid.

**Highlights:**  
- Objects: furniture pieces and grid cells.  
- Relations: `(occupies item cell)`, `(adjacent cell cell)`, `(fits item cell)`.  
- Use durative actions if moving furniture takes time; otherwise use instantaneous `place` actions.  
- Combine constraints to ensure no overlap and maintain required adjacencies.

### Capt John’s Journey

**Story:** A fictional captain must plan a trip visiting several locations in order, subject to time windows and travel times.

**Highlights:**  
- Objects: locations, times, the captain.  
- Fluents: current time, travel time.  
- Actions: `travel` between locations, consuming time; `wait` to align with time windows.  
- Goal: visit all locations and return home within a total time bound.

### Triangle Peg Puzzle

**Story:** Move pegs on a triangular board to get a single peg left.  Each move consists of jumping one peg over an adjacent peg into an empty hole, removing the jumped peg.

**Highlights:**  
- Objects: holes, pegs.  
- Relations: `(peg-in hole)` and `(adjacent hole hole)`.  
- Actions: `jump` from one hole over an adjacent hole into a third hole.  
- Use static relations to encode adjacency and jump moves.

### Knapsack

**Story:** Choose a subset of items with given weights and values to maximise value without exceeding a weight limit.

**Highlights:**  
- Objects: items.  
- Fluents: total weight, total value.  
- Action: `select-item` adds an item to the knapsack, updating weight and value.  
- Goal: weight ≤ capacity; maximise value (achieved by using a bounding function to prune low‑value branches).

---

## Further Reading

Wouldwork draws heavily on decades of research in artificial intelligence planning.  For a gentle introduction to planning problems and how Wouldwork extends them, see the developer’s Medium article [“Traditional AI problem solving with Wouldwork”](https://medium.com/@davypough/traditional-ai-problem-solving-with-wouldwork-fcb0c4a71226).  For more examples and optional features, explore the original manual.

---

## Appendix: Coverage Notes

This rewrite touches every section of the original manual.  The Introduction covers classical planning, planner features, and the disclaimer.  The Quickstart condenses the installation and SBCL setup instructions.  The Modelling Tutorial compacts sections on specifying objects, relations, actions, initial conditions, and goals.  The Reference summarises parameters and program control.  The Optional Features section merges all of Part 2, including type hierarchies, quantifiers, fluents, durative actions, exogenous events, and parameter lists.  The Search Strategies section condenses Part 3’s advice on heuristics and search control.  Finally, the Problem Cookbook rewrites all nine appendix problems as runnable recipes.


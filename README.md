# Wouldwork
  
*Wouldwork is a classical AI planning environment implemented in SBCL Common Lisp, PDDL-flavored, REPL-friendly.*

Users describe a **world** (objects, relations, actions, constraints, goals, ...), and Wouldwork searches for **plans** that reach your goal.  It enables users to solve planning and constraint satisfaction problems without extensive programming experience.  The typical workflow is to **create** a problem spec → **stage** the problem → **set** search parameters → **solve** the problem → **review** results.  The environment is highly interactive to support experimentation, exploration, debugging, and analysis.


## What is offered

Wouldwork supports most “classic planner” features, including an accessible **Lisp-native** foundation:

- **Planning and Constraint Satisfaction Problems**
- **Objects, types, relations** (dynamic + static)
- **Actions** with preconditions/effects
- **Goals**, **constraints**, and **derived relations**
- **Functions** (including on-the-fly and recursive computations)
- **Exogenous events** (things that happen independently of the agent)
- **Temporal planning** (action schedules / timestamps)
- **Search control**: depth cutoffs, tree vs graph search, randomization, branching
- **Solution modes**: first solution, shortest plan, min-time, min/max objective, all solutions, ...
- **Optional parallel search** (using SBCL & Bordeau Threads primitives)
- **Diagnostics & debugging hooks** (including step-through search)

For a gentle narrative intro, see the Medium article:
https://medium.com/@davypough/traditional-ai-problem-solving-with-wouldwork-fcb0c4a71226

GitHub: https://github.com/davypough/wouldwork

See the User Manual for tutorials and explanations on how to use the program

Questions/Comments/Suggestions: davypough@gmail.com


## Installation

- First install SBCL and Quicklisp
- Start SBCL and at the prompt enter (progn (ql:quickload :wouldwork) (in-package :ww))
- Then at the prompt enter (help) or (test) to run Wouldwork on all the test problems to verify correctness


## Additional Workflow Comments

Wouldwork keeps your working context in a small file (`vals.lisp`) inside the system directory.
 
That means when you restart SBCL and Wouldwork, it will restore:
- the problem you were last working on
- all your parameter choices for that problem (algorithm, search settings, debug level, ...).

This is deliberate: planning development is exploratory, and it avoids re-entering all the search control parameters every new session.

There’s also a small but important design choice:
- when you call (stage problem-name), Wouldwork loads that problem spec with fresh defaults
- when you call (refresh), Wouldwork reloads the current problem spec, but avoids clobbering any parameter values you just set at the REPL.  

If you ever want to start over, as if beginning a new session, enter (ww-reset) from the REPL.

Wouldwork includes a lot of features for handling/specifying complex problems (see the User Manual). Start with thoroughly understanding and solving a simple problem like problem-blocks3.lisp first, before tackling more complex problems. Selecting a good problem representation and solution strategy may determine whether Wouldwork can find a solution. Note that modern LLMs like Claude or chatGPT are quite good at drafting a problem specification, if given an accurate problem description along with the user manual.

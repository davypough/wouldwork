You are a senior classical-planning engineer. I want a runnable Wouldwork solution.

Problem name: <SHORT_NAME>

Goal:
- Solve the following problem using Wouldwork (Common Lisp).
- The GitHub repository is `davypough/wouldwork`. Read the README.md and the <wouldwork_manual_combined.md OR wouldwork_dense_tutorial.m>
- Produce a complete, runnable problem specification file (a single .lisp file) and explain it.

Problem X description (self-contained):
<WRITE THE STORY + REQUIREMENTS HERE>
- Objects involved:
- What counts as a state (facts that can be true/false):
- Allowed actions/operators (what can change the state):
- Hard constraints/invariants (must never be violated):
- Goal condition (what must be true at the end):
- Optional objectives (min steps, min time, max value, etc):
- Bounds (max objects, max steps, search depth expectations):
- Any edge cases / tricky parts:

Wouldwork output requirements:
1) Provide ONE complete Wouldwork problem file including:
   - (ww-set *problem-name* ...)
   - (ww-set *problem-type* ...)  ; planning or csp
   - (ww-set *tree-or-graph* ...)
   - (ww-set *solution-type* ...)
   - (ww-set *depth-cutoff* ...)  ; give a sensible starting cutoff
   - define-types
   - define-(dynamic|static)-relations
   - define-action rules (with preconditions + effects)
   - define-init
   - define-goal (or nil goal if optimizing without goal)
   - any needed optional features (bind, fluents, complementary relations, etc)

2) Provide step-by-step REPL commands to run it:
   - (ql:quickload :wouldwork)
   - (in-package :ww)
   - (stage '<problem-name>) / (run '<problem-name>) / (solve) etc

3) Provide a tutorial explanation in this exact structure:
   A. Modeling choices: what are objects/types/relations and why
   B. Action semantics: each action in plain English + its precondition/effect
   C. Why the search will terminate (tree vs graph, cutoff)
   D. Debugging checklist: “if no solution found, check X, Y, Z”
   E. How to make it faster: reduce branching, use fluents/bind, add constraints, etc

4) Quality bar:
   - Use Wouldwork idioms (e.g., bind + fluents when the relation is functional).
   - Keep actions small and non-ambiguous.
   - Do NOT invent non-existent Wouldwork API; if unsure, implement via standard define-action / logical forms.
   - Include at least one miniature test instance (tiny version) and the full instance.

Before writing code:
- Restate the problem as a planner model (state variables + actions + goal) in 10 lines.
- List any ambiguities you must resolve. If any ambiguity is fatal, ask me 3 targeted questions; otherwise pick the simplest reasonable interpretation and continue.

Now generate the full solution.

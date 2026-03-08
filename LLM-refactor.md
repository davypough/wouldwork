# Refactoring Guidelines

This file defines how AI agents should perform **refactoring tasks on working codebases**.

The primary objective is to **improve structure, clarity, and maintainability while preserving behavior**.

---

# Core Principle

Refactoring must **not change observable behavior**.

Any structural change should preserve:

* function outputs
* side effects
* error conditions
* performance characteristics (unless explicitly requested)

When uncertain whether a change preserves behavior, prefer leaving the code unchanged and asking for clarification.

---

# Refactoring Goals

Refactoring should aim to improve:

* readability
* maintainability
* clarity of control flow
* explicit invariants
* modularity
* reduction of duplication

Prefer structural improvements that make the code **easier to reason about**.

---

# Behavior Preservation

Assume the current code works correctly.

Do not:

* change algorithms
* modify external interfaces
* alter return types
* introduce new side effects

If the current behavior appears incorrect, **do not fix it during refactoring**.
Instead, identify it and ask whether it should be treated as a bug fix.

---

# Refactoring Techniques

Safe refactorings include:

* renaming variables and functions for clarity
* extracting helper functions
* consolidating duplicated logic
* simplifying control flow
* introducing guard clauses
* reorganizing code into clearer functional units

Avoid introducing new abstractions unless they clearly simplify the design.

---

# Structural Simplification

Prefer:

* shallow control flow
* early returns instead of deep nesting
* smaller functions with clear responsibilities
* direct data flow

Avoid:

* speculative abstraction
* unnecessary helper layers
* complex macro systems
* unnecessary wrapper functions

---

# Invariants and Contracts

Clarify invariants when possible.

Prefer making assumptions **explicit in the code** rather than implicit.

Example improvements:

* clear argument expectations
* explicit preconditions
* well-defined return values

However, do not add defensive programming that changes behavior.

---

# Scope of Changes

Refactoring changes should be **localized and incremental**.

Prefer:

* small, reviewable changes
* improving one function or module at a time

Avoid:

* large architectural rewrites
* touching unrelated files
* sweeping stylistic changes across the entire codebase

If a broader redesign appears beneficial, propose it separately.

---

# Editing Workflow

Before editing:

1. Explain the refactoring you intend to perform.
2. Explain why it improves the code.

After editing:

* summarize the structural changes
* confirm that behavior should remain unchanged

---

# Testing

Refactoring should always preserve existing tests.

If tests exist:

* ensure they still pass after the change.

If tests do not exist:

* suggest simple verification steps that confirm behavior remains the same.

---

# When to Stop

Stop refactoring when:

* the code becomes easier to read and reason about
* duplication has been reduced
* control flow is clear
* further changes would risk altering behavior

Refactoring should **clarify the code, not endlessly restructure it**.

---

## Common Lisp: Dynamic vs Lexical Binding
Preserve binding intent:
- Do not convert special variables to lexicals or vice versa.
- Respect naming conventions for specials (e.g., *like-this*).
- Avoid introducing closures that capture variables unless behavior is clearly unchanged.
- Preserve uses of SYMBOL-VALUE / DYNAMIC binding patterns.

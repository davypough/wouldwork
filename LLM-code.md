# Collaboration Preferences

## Response Style

* Keep responses concise but clear.
* Prefer plain language and direct answers.
* Don't use terminology or phrasing that assumes I know what you are referring to unless we have previously used it; be clear.
* Avoid unnecessary background unless requested.
* Do not condense or drop prior items mentioned in our discussion.
* Never end mid-sentence; before sending, verify the final line is complete and coherent.
* To reduce missing/incomplete text, default to stricter verification steps.
* Don't delete text from our ongoing conversation; I may later need to reread what you previously said.
* If my request is ambiguous, ask for clarification to prevent rumination.

---

# Coding Workflow

## General Philosophy

* Don't program defensively; inconsistencies should surface as errors during development.
* If multiple fixes are possible, prefer the one that improves the long-term clarity of the codebase rather than the one with the smallest diff.
* This codebase is not a public library; it is maintained and used by a single developer.
* Backward compatibility is not required unless explicitly stated.
* Avoid large functions and macros; avoid deeply nested expressions.

---

## Editing Workflow

Before editing files:

* State what you are about to change and why.
* Keep this pre-edit explanation visible.

After edits:

* Briefly explain what you changed.

Testing:

* I will run tests locally.
* Tell me exactly what test or command I should run.

---

## Bug Fix Policy

When fixing bugs:

1. Prefer fixing the **root cause** rather than applying a superficial patch.
2. Small refactors are encouraged if they improve clarity or remove structural problems.
3. If multiple fixes are possible, prefer the one that **reduces long-term complexity**.

Avoid fixes that introduce:

* extra wrapper functions
* compatibility layers
* duplicated validation logic
* additional special-case branching

---

## Refactor Triggers

Refactor as part of a bug fix if the minimal patch would:

* add another special-case conditional
* duplicate existing logic
* increase nesting or complexity
* rely on subtle ordering or hidden assumptions
* obscure the real invariant being enforced

In these cases, prefer a small structural improvement over a local patch.

---

## Function Invariants

Function invariants and input validation should normally live **inside the function** rather than being enforced by wrapper functions.

Prefer:

```
(process-data x)
  -> validates x internally
```

Avoid patterns like:

```
safe-process-data -> wrapper
process-data      -> real logic
```

Wrappers are acceptable only for:

* logging / instrumentation
* caching
* retries / timeouts
* clearly defined policy layers

---

## API Changes

Interface changes are allowed.

Agents may:

* modify function signatures
* consolidate redundant APIs
* remove unused functions
* simplify call graphs

When changing an interface:

* update all call sites in the repository
* update related documentation
* ensure behavior remains clear and testable.

---

## Code Structure Preferences

Prefer:

* direct fixes to the underlying function
* simple data flow
* clear invariants
* smaller focused functions

Avoid:

* unnecessary abstraction layers
* wrapper chains
* speculative abstractions
* deeply nested control flow
* labels and flet expressions

---

## Complexity Limits

Avoid increasing code complexity when fixing a bug.

Prefer:
- guard clauses / early returns
- extracting small helper functions
- simplifying control flow

Avoid:
- adding deeply nested conditionals
- stacking multiple special-case branches
- expanding already complex functions

If a fix would increase nesting beyond two levels, consider a small refactor instead.

---

## Patch Scope

Keep changes tightly scoped to the problem being solved.

Prefer:
- modifying the existing function where the bug occurs
- small refactors adjacent to the change

Avoid:
- editing unrelated modules
- large structural rewrites
- speculative cleanup across multiple files

If a larger architectural change seems appropriate, propose it first instead of implementing it immediately.

---

## Tests

When fixing a bug:

1. Identify how the bug can be reproduced.
2. Suggest a test that demonstrates the failure.
3. After the fix, confirm the test should pass.

If existing behavior is unclear, suggest creating a **characterization test** before refactoring.

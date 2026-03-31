# Wouldwork Planner — Problem Specification Template

> **Usage:** Attach this file when starting a new problem specification session.
> Provide a preliminary description of your problem in the prompt.


---

## Section 0: Process Guide

### Startup
The user provides a preliminary problem description and attaches this template.
Claude reads the DSL reference (Section 1) and the template structure (Section 2),
then begins the structured interview.

### Interview Strategy
1. **One question at a time.** Wait for user's answer before proceeding.
2. **Build the template incrementally.** After each answer, present the updated
   template so the user can catch misunderstandings early.
3. **Question ordering — most consequential ambiguities first:**
   - Mechanics / rules (what happens when actions execute)
   - Derived effects and chaining (does action X trigger Y automatically?)
   - Action constraints (legality conditions)
   - Goal conditions
   - Search parameters (solution-type, depth-cutoff)
   - Variability (fixed puzzle vs. parameterized)
4. **Conditional branches:** Skip questions that don't apply. If the user
   describes a pure combinatorial problem, skip physics/propagation questions.
   If there are no derived effects, skip cascade questions.
5. **Don't ask about representation early.** Capture the user's conceptual
   model first. Representation decisions belong in the Implementation Notes
   phase after the template is complete.
6. **Infer where possible.** If the user's description unambiguously answers
   a question, note the inference in the template rather than asking.

### Post-Interview Phases
1. **Template review:** Present the final template for user confirmation.
2. **Implementation notes:** Discuss representation strategy, performance
   considerations, and how the problem maps to Wouldwork constructs.
3. **Spec drafting:** Write the `.lisp` file using the DSL reference below.
4. **Testing:** Provide a REPL expression the user can run to test.


---

## Section 1: Wouldwork DSL Reference

### 1.1 File Structure

Every problem spec is a `.lisp` file in the `src/` directory.
Required package declaration: `(in-package :ww)`

Sections appear in this canonical order:
1. `ww-set` declarations (problem parameters)
2. `define-types` (object types and instances)
3. `define-dynamic-relations` (state relations that change)
4. `define-static-relations` (state relations that don't change)
5. Query functions (`define-query`)
6. Update functions (`define-update`)
7. Actions (`define-action`)
8. `define-init` (initial state assertions)
9. `define-goal` (goal condition)


### 1.2 Problem Parameters (`ww-set`)

```lisp
(ww-set *problem-name* <symbol>)          ; e.g., match3a
(ww-set *problem-type* planning)          ; or csp (constraint satisfaction)
(ww-set *solution-type* <type>)           ; first, every, min-length, min-time,
                                          ; min-value, max-value, or integer N
(ww-set *tree-or-graph* <mode>)           ; tree or graph
(ww-set *depth-cutoff* <integer>)         ; max search depth (0 = no limit)
(ww-set *symmetry-pruning* <bool>)        ; t or nil
(ww-set *progress-reporting-interval* <integer>)  ; e.g., 1000000
```


### 1.3 Type System (`define-types`)

Defines object types and their instances (ground atoms).

```lisp
(define-types
  block  (A B C)
  table  (T)
  support (either block table))   ; union type
```

- `either` creates a union of previously defined types.
- Type names become unary predicates: `(block A)` is automatically true.
- Numeric instances are permitted: `row (0 1 2 3)`.
- `(compute <form>)` can generate instances programmatically.


### 1.4 Relations

#### Dynamic Relations (`define-dynamic-relations`)

Relations that change during search. Stored in the mutable state database (IDB).

```lisp
(define-dynamic-relations
  (on block support)              ; non-fluent: all args are typed
  (loc agent $area)               ; fluent: $area is a value looked up via bind
  (cell row col $fixnum)          ; fluent: $fixnum is the stored value
  (holds agent $cargo)            ; fluent
  (color relay $hue))             ; fluent
```

- **Non-fluent args** (no `$` prefix): Part of the database key.
  The relation either exists or doesn't.
- **Fluent args** (`$` prefix): Stored as the value associated with the key.
  Use `bind` to retrieve. A relation can have multiple fluent args.
- `(inconsistent-state)` is a built-in nullary dynamic relation. Asserting it
  causes Wouldwork to prune the current search branch.

#### Static Relations (`define-static-relations`)

Relations that never change. Asserted in `define-init` and stored separately.

```lisp
(define-static-relations
  (coords area $rational $rational $rational)
  (controls receiver gate)
  (max-row $fixnum))
```


### 1.5 Variable Conventions

| Prefix | Meaning | Scope |
|--------|---------|-------|
| `?var` | Parameter variable | Bound by quantifier headers or action parameters |
| `$var` | Local/scratch variable | Bound by `setq`, `bind`, `let`, `mvsetq` |

- `?variables` are typed — they iterate over type instances.
- `$variables` are untyped — they hold computed values.


### 1.6 Query Functions (`define-query`)

Read-only functions that examine state. Cannot modify the database.

```lisp
(define-query <n> (<args>)
  <body>)    ; MUST be a single expression — use (do ...) to group multiple statements
```

- Args are `?variables` passed by the caller.
- **Single-expression body:** The body must be exactly one expression. To
  execute multiple statements, wrap them in `(do ...)`.
- Body is translated in `pre` (precondition/read) context.
- Return value is the value of the last expression in the body.
- Can use all DSL operators: `bind`, `exists`, `forall`, `doall`, `ww-loop`,
  `setq`, `do`, `if`, `cond`, `let`, `mvsetq`, etc.
- Query functions call other queries freely.
- Use `(return-from <n> <value>)` for early return.

Example:
```lisp
(define-query cleartop? (?block)
  (not (exists (?b block)
         (on ?b ?block))))
```


### 1.7 Update Functions (`define-update`)

Functions that modify the database (assert/retract relations).

```lisp
(define-update <n> (<args>)
  <body>)    ; MUST be a single expression — use (do ...) to group multiple statements
```

- **Single-expression body:** The body must be exactly one expression. To
  execute multiple statements, wrap them in `(do ...)`.
- Body is translated in `eff` (effect/write) context.
- **Naming convention:** Must end with `!` (e.g., `propagate-changes!`,
  `apply-gravity!`). The translator uses this suffix to identify update functions.
- Can assert relations by writing them as bare forms: `(cell 0 0 3)`
- Can retract relations with `(not (cell 0 0 3))`
- Can call other update functions and query functions.
- `(inconsistent-state)` — asserts the nullary inconsistent-state relation,
  causing branch pruning.

**The `propagate-changes!` pattern** — convergence loop for derived effects:
```lisp
(define-update propagate-changes! ()
  (ww-loop for $iteration from 1 to <max>
           do (if (not (<single-pass-fn>))
                (return t))       ; converged, no changes
           finally (inconsistent-state) (return nil)))  ; failed to converge
```
Returns T on convergence, NIL on failure.


### 1.8 Actions (`define-action`)

The primary search operators. Each action has 6 components:

```lisp
(define-action <n>
    <cost>                    ; numeric cost (usually 1)
  <parameter-header>          ; typed iteration variables
  <precondition>              ; boolean test (pre context)
  <description-variables>     ; variables captured for solution trace
  <effect>)                   ; state modifications (starts pre, shifts to eff in assert)
```

#### Parameter Headers

Control how action parameters are instantiated:

- **`standard`** (default if omitted): All distinct; cartesian product with
  deduplication. `(standard ?a type1 ?b type2)` means `?a ≠ ?b` when same type.
- **`combination`**: Ordered combinations (no duplicates, no permutations).
  `(combination (?a ?b) type)` generates `?a < ?b` pairs.
- **`dot-product`**: Element-wise pairing of parallel type lists.
  `(dot-product ?dr delta-row ?dc delta-col)` pairs corresponding elements.
- **Multiple groups**: `(?agent agent (combination (?t1 ?t2) terminus))`
- **Dynamic queries in headers**: A type position can be a query call like
  `(get-current-beams)` that returns a list at runtime.

#### Preconditions

Boolean expressions in read-only (`pre`) context. Key operators:

- `(bind (relation key... $fluent...))` — Look up fluent values. Returns T/NIL.
- `(exists (?v type) <body>)` — Existential: true if any instance satisfies body.
- `(forall (?v type) <body>)` — Universal: true if all instances satisfy body.
- `(different ?a ?b)` — Symbol inequality.
- Standard CL: `and`, `or`, `not`, `if`, `cond`, `=`, `<`, `>`, `eql`, etc.
- `setq` — Capture intermediate values for use in effects.

#### Effects

State modifications. The effect body starts in `pre` context and shifts to
`eff` (write) context inside `assert`:

```lisp
(assert (cell ?row ?col $new-sym)           ; assert new relation
        (not (cell ?row ?col $old-sym))     ; retract old relation
        (finally (propagate-changes!)))     ; trigger derived effects after assert
```

- **`assert`**: Groups writes into a single state transition. Copies state first
  (for depth-first search).
- **`finally`**: Schedules an update function to run after the assert completes.
  Typically used for `(finally (propagate-changes!))`.
- **Multiple `assert` blocks**: An action can contain multiple `assert` blocks
  (e.g., inside `doall`), each generating a separate successor state.
- **`setq` in effects**: `(setq $place 'ground)` captures values for the
  description-variables trace.

#### Description Variables

A list of variables whose values are captured for the solution trace:
```lisp
(?agent ?connector $area)    ; printed as action description in solution
```


### 1.9 Iteration and Quantification

| Form | Context | Meaning |
|------|---------|---------|
| `(exists (?v type) body)` | pre | True if any instance satisfies body |
| `(forall (?v type) body)` | pre | True if all instances satisfy body |
| `(doall (?v type) body)` | eff | Execute body for each instance (side effects) |
| `(ww-loop ...)` | any | Translates to CL `loop` — use for counted/conditional iteration |

- **Single-expression body constraint:** `exists`, `forall`, and `doall` each
  take exactly one body expression. To execute multiple statements, wrap them
  in `(do ...)`. This is the most common source of translation errors.
- Multi-variable quantifiers: `(exists ((?a ?b) type) ...)` or
  `(doall ((?x ?y) type) ...)`.
- Type can be a query call for dynamic iteration: `(doall (?b (get-current-beams)) ...)`.
- **Performance note:** For dynamic query types, nested `doall`s with a guard
  are more efficient than `(doall (combination (?a ?b) (dynamic-query)) ...)`
  because `combination`/`standard` perform runtime product/dedup on every call.


### 1.10 Initialization (`define-init`)

Asserts the initial state — both dynamic and static relations:

```lisp
(define-init
  ;; Dynamic
  (loc agent1 area1)
  (cell 0 0 3)
  ;; Static
  (max-row 3)
  (coords area1 9 1 0))
```

- If a relation name is in `define-dynamic-relations`, it goes to the dynamic DB.
- If a relation name is in `define-static-relations`, it goes to the static DB.
- `(not (...))` retracts a relation (rarely needed in init).


### 1.11 Goal (`define-goal`)

A boolean expression evaluated in `pre` context:

```lisp
(define-goal
  (and (loc agent1 area4)
       (active receiver2)))
```

- Can use `exists`, `forall`, `bind`, `not`, `and`, `or`, and query calls.
- **Update functions in goals:** `propagate-changes!` can be called in
  `define-goal` for post-validation (the translator permits update calls
  in goal/`pre` context). On goal states this modifies in-place since
  goal states are leaf nodes — no copy needed.
- Goal is evaluated against each candidate state during search.


### 1.12 Key Patterns

#### Functional Relations (single value per key)
When a relation like `(cell row col $fixnum)` has typed keys and a fluent value,
asserting with the same key replaces the previous value (hash table semantics).

#### Database Key Validity
Wouldwork encodes relation keys as integers using the type system. All
non-fluent arguments in a `bind` or relation lookup **must be known type
instances**. Computed values (e.g., `(1+ ?col)`) used as key arguments will
crash the integer encoder if they fall outside the declared type range.

**Always validate computed indices before using them in `bind` or relation
assertions.** For example, a `swap-right` action computing `$next-col = (1+ ?col)`
must check `(< ?col $max-col)` *before* `(bind (cell ?row $next-col $sym))`.
This applies to any arithmetic on grid coordinates, array indices, or similar
computed key values.

#### State Copying
`(copy-problem-state state)` creates a deep copy of the current state.
Used when you need to test modifications without corrupting the search state
(e.g., gated move preconditions that propagate on a copy).

#### `register-dynamic-object`
`(register-dynamic-object <symbol> <type>)` registers a new object at runtime
(e.g., creating new beam entities during propagation).

#### Common Lisp Integration
Arbitrary CL code is allowed in query/update bodies: `push`, `incf`, `cons`,
`member`, `make-hash-table`, `gethash`, `setf`, `format`, `maphash`,
`lambda`, `funcall`, `mapcar`, `some`, `every`, `sqrt`, `abs`, `min`, `max`,
`values`, `multiple-value-setq` (via `mvsetq`), etc.

Functions defined with `defun`/`defparameter` at top level are available
globally (they are standard CL, not translated by the DSL).


### 1.13 Running

```lisp
(progn (ql:quickload :wouldwork) (in-package :ww))
(run)                            ; solve the currently loaded problem
(run "match3a")                  ; load and solve by name
(stage "match3a")                ; load without solving
(solve)                          ; solve the staged problem
(refresh)                        ; reload after editing the problem file
(params)                         ; display current parameters
```


---

## Section 2: Problem Description Template

Fill in progressively during the interview. Mark sections `(pending)` until answered.

```
1. DOMAIN STRUCTURE:
   - (pending)

2. OBJECT TYPES & PROPERTIES:
   - (pending)

3. ACTIONS:
   - (pending)

4. DERIVED EFFECTS:
   - (pending)

5. ACTION CONSTRAINTS:
   - (pending)

6. GOAL:
   - (pending)

7. SEARCH PARAMETERS:
   - (pending)

8. IMPLEMENTATION NOTES: (deferred to spec drafting phase)
   - Representation strategy
   - Performance considerations
```

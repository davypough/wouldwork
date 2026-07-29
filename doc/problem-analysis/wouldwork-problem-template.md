# Wouldwork Planner — Problem Specification Template

> **Usage:** Attach this file when starting a new problem specification session.
> Provide a preliminary description of your problem in the prompt.

> **Status:** Where this file and the *Wouldwork User Manual* disagree, this file is
> currently the correct one — the Manual has pending updates tracked in
> `doc/user-manual-sync.md`.

**Companion documents.** This file covers writing a spec. Once a spec exists and needs
analysis, `working-reference-builder.md` normalizes its scattered facts into a working
reference, and `inferring-missing-relations.md` uses that reference to locate an omitted
relation in a spec that is correct but unsolvable. For the technology library,
`tech/README.html`; for relation signatures, `tech/Talos Technology  Relations.txt`; for
what the engine does at load time, `../load-ordering/ordering-of-operations.md`.


---

## Section 0: Process Guide

### Startup
The user provides a preliminary problem description and attaches this template.
Claude reads the DSL reference (Section 1) and the template structure (Section 2),
then begins the structured interview.

### Step 1 — Establish the problem family, before anything else

This fork determines the entire shape of the specification. Settle it first; do not
begin the interview proper until it is decided.

**Path A — a new Talos Principle problem.**
Build it on the `tech/` technology library. These files implement a *topological*
representation of Talos mechanics — beams, connectors, gates, walkability,
visibility, elevation — as reusable roles that splice into the spec via
`include-tech` (Section 1.5). A new Talos problem should be assembled from the
existing technologies rather than hand-authoring the mechanics, so that behavior
stays consistent across problems and fixes propagate.

Read `tech/README.html` before drafting. It is authoritative for the technology
library: the role system, the tier picture, the file inventory, hard vs. soft
dependencies, and the integration checklist.

**Path B — everything else.**
Hand-authored specifications with no `include-tech`. This includes all non-Talos
problems *and* the legacy Talos specs — `problem-corner.lisp` is the reference
example — which predate the technology library and are not written against it. Do
not retrofit `tech/` onto a legacy spec as part of a new problem session; treat the
two approaches as separate.

On Path B, resolve one more question before the interview:

**Planning or CSP?** Set via `(ww-set *problem-type* planning)` or `csp`.

- *Planning* — actions represent changes to the problem state; the solution is a
  sequence of steps. Usually paired with `depth-first` + `graph`.
- *CSP* — actions represent sequential assignments of values to variables; the
  solution is a complete assignment satisfying the constraints.
  `problem-captjohn.lisp` is the reference example. Usually paired with
  `backtracking` + `tree`, and leave `*depth-cutoff*` at 0, since search must reach
  a depth equal to the number of rules.

A CSP can be expressed as a planning problem, but the CSP specialization prunes far
more aggressively and matters greatly at scale. Ask which the user's problem is;
"find an arrangement satisfying constraints" indicates CSP, "find a sequence of steps"
indicates planning.

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
5. **On Path A, ask which technologies apply, not how they work.** The mechanics are
   already specified in `tech/`. The interview's job is to identify which technologies
   the puzzle needs and what the problem must supply to them — leaf types, geometry,
   initial facts — not to re-elicit beam or gate behavior.
6. **Don't ask about representation early.** Capture the user's conceptual
   model first. Representation decisions belong in the Implementation Notes
   phase after the template is complete.
7. **Infer where possible.** If the user's description unambiguously answers
   a question, note the inference in the template rather than asking.

### Post-Interview Phases
1. **Template review:** Present the final template for user confirmation.
2. **Implementation notes:** Discuss representation strategy, performance
   considerations, and how the problem maps to Wouldwork constructs.
   On Path A, this is where the technology selection is finalized against
   `tech/README.html`'s integration checklist.
3. **Spec drafting:** Write the `.lisp` file using the DSL reference below.
4. **Testing:** Provide a REPL expression the user can run to test.


---

## Section 1: Wouldwork DSL Reference

### 1.1 File Structure

Every problem spec is a `.lisp` file in the **`probs/`** directory. Test problems
exercising a single technology go in **`test/`**. Wouldwork resolves a problem name
by searching `probs/` first, then `test/`.

**Do not write a spec in `src/`.** `src/problem.lisp` is a *generated* file — staging
any problem overwrites it. It is the spliced snapshot the engine compiles, not a
source file.

Required package declaration: `(in-package :ww)`

Sections appear in this canonical order:

1. `ww-set` declarations (problem parameters)
2. `defparameter` / `defun` helpers, if any
3. `define-types` (object types and instances) — and `define-optional-types`
4. `include-tech` directives — **Path A only; must come after `define-types`**
5. `define-dynamic-relations` (state relations that change)
6. `define-static-relations` (state relations that don't change)
7. Query functions (`define-query`)
8. Update functions (`define-update`)
9. Actions (`define-action`)
10. `define-happening` / `define-patroller` (exogenous events), if any
11. `define-init` (initial state assertions)
12. `define-init-action` (derivations run once at initialization), if any
13. `define-goal` (goal condition)

Only one of these orderings is a hard requirement rather than a convention:
**`define-types` must precede every `include-tech` directive.** Section 1.5 explains
why, and why violating it fails silently. The rest is house style — forward references
among queries and updates are resolved by a pre-scan pass, so definition order among
them does not matter.


### 1.2 Problem Parameters (`ww-set`)

```lisp
(ww-set *problem-name* <symbol>)          ; e.g., match3a
(ww-set *problem-type* planning)          ; or csp (constraint satisfaction)
(ww-set *solution-type* <type>)           ; first, every, all-paths, min-length,
                                          ; min-time, min-value, max-value, or integer N
(ww-set *tree-or-graph* <mode>)           ; tree or graph
(ww-set *depth-cutoff* <integer>)         ; max search depth (0 = no limit)
(ww-set *symmetry-pruning* <bool>)        ; t or nil
(ww-set *progress-reporting-interval* <integer>)  ; e.g., 1000000
```

**Three parameters must NOT appear in a problem file. Each signals an error if it
does:**

| Parameter | Why |
|---|---|
| `*debug*` | gates conditional compilation |
| `*algorithm*` | selects which translations are generated |
| `*probe*` | gates conditional compilation, and validates against loaded actions |

Each requires recompilation to take effect, and the problem file is read during the
very compile it would need to influence. Set all three at the REPL after staging; each
triggers an automatic reload. `*problem-name*` is the mirror-image case — it must be
set in the problem file and is refused at the REPL.

Note also that on an ordinary load, a saved `vals.lisp` overrides the problem file's
`ww-set` values. Use `(stage <problem>)` to get the problem's own intended settings.
See `doc/load-ordering/parameter-precedence.md`.


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
- A type declared with an empty instance list — `beam ()` — is legal, and is the
  pattern for populations that only exist at runtime. See Section 1.9.


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


### 1.5 Technology Includes (`include-tech`) — Path A

New Talos Principle problems assemble their mechanics from the `tech/` library rather
than hand-authoring them:

```lisp
(include-tech gate)                  ;controls; energized; update-gate-status!
(include-tech beam-relay)            ;paired; color; pickup/put/connect actions
(include-tech beam-crossing)         ;crossing-active; crossings-along-beam>
(include-tech walkability)          ;walk-via; walkable-locations; walkable; walk
(include-tech visibility)            ;los-to-apparatus; visible; visible-clear
```

**What it does.** `include-tech` is not a runtime macro. Before compilation, each
directive's target file is *textually spliced* into the generated `src/problem.lisp`,
recursively — a technology may include others. Each technology is spliced at most once;
repeats leave a `;; ... already included -- skipped` marker. By the time anything is
evaluated, the tech bodies are ordinary top-level forms.

**The one hard ordering rule: `define-types` must appear above the includes.**

A `define-query` body is translated the instant its form is evaluated, and a `doall`
over a bare type name is compiled into a *literal* domain list at that moment. A type
declared below the includes is therefore already known by name — the pre-scan pass
registers it — but with an empty instance list. Every tech query iterating over it
collapses into a silent no-op. **This is not an error.** Nothing warns you; the problem
just fails to do anything. `problem-corner-topo.lisp` carries a comment block warning
about exactly this.

**Division of labor.** Composite types a technology needs (`mobile-object`, `cargo`,
`support`, `target`, and so on) are declared inside the technology file itself, so no
tech file depends on a declaration living in the problem. The problem declares only the
leaf types it instantiates. Where both declare the same type, consistency is enforced —
they must resolve to the same instance list.

**Further reading.** `tech/README.html` is authoritative for the library: the role
system, tiers, file inventory, hard vs. soft dependencies, and the integration
checklist. `doc/load-ordering/ordering-of-operations.md` covers splicing and its
failure modes in engine terms (Stage 2, and Traps 1–3).


### 1.6 Variable Conventions

| Prefix | Meaning | Scope |
|--------|---------|-------|
| `?var` | Parameter variable | Bound by action/quantifier domains or passed to a query/update |
| `$var` | Local/scratch variable | Bound by `setq`, `bind`, `let`, `mvsetq` |

- Action and quantifier `?variables` have domains and iterate over their instances.
- Query/update `?variables` may independently declare a Wouldwork object type, but
  they are passed by the caller rather than instantiated by the function.
- `$variables` hold local or computed Lisp values. They are not permitted as
  query/update formal parameters.


### 1.7 Query Functions (`define-query`)

Read-only functions that examine state. Cannot modify the database.

```lisp
(define-query <name>
    (?object <object-type>
     ?value
     ?other-object (either <object-type-1> <object-type-2>))
  <body>)    ; MUST be a single expression — use (do ...) to group multiple statements
```

- Every formal parameter is a `?variable`; `$variables` are not allowed here.
- Each parameter may independently be followed by a Wouldwork object type or an
  inline `(either ...)` object type. Typed and untyped parameters may appear in
  any order.
- Type object parameters when doing so states a useful requirement. Leave
  computed Lisp values—numbers, strings, lists, hash tables, booleans, and
  `nil`—untyped. Wouldwork does not currently accept Lisp type declarations in
  query/update signatures.
- These annotations validate compatible uses and calls. They do not enumerate,
  convert, or otherwise change the value supplied by the caller.
- A literal planning object supplied by a caller must belong to the declared
  type. A computed expression remains permissible when its result type cannot
  be established while the problem is translated.
- Action parameter headers (`standard`, `combination`, etc.) and query-valued
  action domains do not belong in a query/update signature.
- An empty optional object type is still a valid annotation. The function is
  installed normally; an action or quantifier that enumerates that type simply
  produces no calls. This permits a typed null-default technology hook to remain
  present and return its neutral result when the corresponding capability has no
  objects in the current problem.
- **Single-expression body:** The body must be exactly one expression. To
  execute multiple statements, wrap them in `(do ...)`.
- Body is translated in `pre` (precondition/read) context.
- Return value is the value of the last expression in the body.
- Can use all DSL operators: `bind`, `exists`, `forall`, `doall`, `ww-loop`,
  `setq`, `do`, `if`, `cond`, `let`, `mvsetq`, etc.
- Query functions call other queries freely, including forward references to queries
  defined later in the file.
- Use `(return-from <name> <value>)` for early return.

Example:
```lisp
(define-query cleartop? (?block block)
  (not (exists (?b block)
         (on ?b ?block))))
```

Mixed object/value example:
```lisp
(define-query beam-visible
    (?location location
     ?near-elevation
     ?object (either apparatus location)
     ?far-elevation)
  ...)
```

Here `?location` and `?object` are planning objects. The two elevations are
computed numeric values, so they deliberately have no Wouldwork object type.


### 1.8 Update Functions (`define-update`)

Functions that modify the database (assert/retract relations).

```lisp
(define-update <name> (<parameters>)
  <body>)    ; MUST be a single expression — use (do ...) to group multiple statements
```

- Parameter lists use exactly the same `?variable` and optional Wouldwork
  object-type syntax as queries. Computed Lisp-value parameters remain untyped.
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
Returns T on convergence, NIL on failure. On Path A this driver is usually derived
automatically from the splice order of the included technologies; author one only if
the problem needs to override that.


### 1.9 Actions (`define-action`)

The primary search operators. Each action has 6 components:

```lisp
(define-action <name>
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


### 1.10 Iteration and Quantification

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
- **Static vs. dynamic domain.** A bare type name compiles to a literal list at
  translation time. A query call — `(doall (?b (get-current-beams)) ...)` — is
  evaluated at runtime against state instead. The query form is the only way to
  iterate a population that isn't known when the file loads; declare the type empty
  (`beam ()`) and publish the pool through a relation.
- **Performance note:** For dynamic query types, nested `doall`s with a guard
  are more efficient than `(doall (combination (?a ?b) (dynamic-query)) ...)`
  because `combination`/`standard` perform runtime product/dedup on every call.


### 1.11 Initialization (`define-init`)

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

`define-init-action` runs a derivation once during initialization — computing static
facts from raw geometry, for instance. Two cautions: init-actions fire in file/splice
order, *not* by the numeric duration argument; and an init-action is **silently
skipped** if any of its parameter types has no instances. See
`doc/load-ordering/ordering-of-operations.md`, Traps 4–6.


### 1.12 Goal (`define-goal`)

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


### 1.13 Key Patterns

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
(e.g., gated walk preconditions that propagate on a copy).

#### `register-dynamic-object`
`(register-dynamic-object <symbol> <type>)` registers a new object at runtime
(e.g., creating new beam entities during propagation). It assigns an integer code and
asserts the type proposition, but it does **not** add the object to the type's
extension — so the object is reachable only through a predicate test or a query-domain
`doall`, never through a static-domain one. Total planning objects are capped at 999.

#### Common Lisp Integration
Arbitrary CL code is allowed in query/update bodies: `push`, `incf`, `cons`,
`member`, `make-hash-table`, `gethash`, `setf`, `format`, `maphash`,
`lambda`, `funcall`, `mapcar`, `some`, `every`, `sqrt`, `abs`, `min`, `max`,
`values`, `multiple-value-setq` (via `mvsetq`), etc.

Functions defined with `defun`/`defparameter` at top level are available
globally (they are standard CL, not translated by the DSL).


### 1.14 Running

```lisp
(progn (ql:quickload :wouldwork) (in-package :ww))
(stage match3a)                  ; stage a problem without solving it
(solve)                          ; solve the currently staged problem
(run match3a)                    ; stage and solve in one step
(refresh)                        ; reload after editing the current problem file
(params)                         ; display current parameters
(list-problem-names)             ; list available problems
(help)                           ; list all commands
(ww-reset)                       ; discard generated problem and saved settings
```

`run` and `stage` are macros; each requires a problem name, given either as a bare
symbol or as a string — `(run match3a)` and `(run "match3a")` are equivalent. There is
no zero-argument `(run)`; use `(solve)` to solve what is already staged.

`(stage X)` applies the problem file's own `ww-set` settings. `(refresh)` deliberately
skips them, preserving whatever you set at the REPL.


---

## Section 2: Problem Description Template

Fill in progressively during the interview. Mark sections `(pending)` until answered.

```
0. PROBLEM FAMILY:
   - Path A (new Talos, tech/-based) or Path B (hand-authored): (pending)
   - If Path A, technologies required: (pending)
   - If Path B, planning or csp: (pending)

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

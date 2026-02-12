# Prompt: Implement Penultimate State Enumeration in WouldWork

## Objective

Extend `ww-enumerator.lisp` to support enumerating **penultimate states** — concrete base-relation states from which a single action application reaches a goal state. The current `find-goal-states` mechanism enumerates goal states directly; we need an analogous `find-penultimate-states` that enumerates states one step before the goal.

This closes a completeness gap in backward regression (`ww-backwards.lisp`): the regression approach cannot reconstruct prior `paired` facts destroyed/created by `pickup-connector` and `connect-to-n-terminus` actions. Forward enumeration of penultimate states is complete by construction because `propagate-changes!` is deterministic for any concrete base state.

## Relevant Files

- `src/ww-enumerator.lisp` — CSP-based enumeration infrastructure (primary file to modify)
- `src/ww-backwards.lisp` — backward regression + forward validation (has utility functions to reuse)
- `src/problem-corner.lisp` — test domain; goal is `(and (active receiver2) (active receiver3) (loc agent1 area4))`

## Existing Infrastructure to Leverage

The enumerator already provides everything needed for the CSP layer:

- `define-base-relations (loc paired)` — the two non-derived dynamic relations
- `define-enum-relation` metadata for `loc` (`:fluent`, `:allow-unassigned (:types cargo)`), `paired` (`:subset`, `:max-per-key 4`, `:key-types (:types connector)`), `holds` (`:fluent`), `elevation` (`:derived`)
- `define-prefilter corner-paths` — prunes base states where transmitter→receiver paired-paths don't exist
- `find-goal-states` / `find-goal-states-fn` — orchestrates enumeration with goal checking
- `enumerate-state-csp` — runs the CSP solver with generated enum-actions
- `generate-enum-actions` — builds the CSP action sequence from metadata

**Note**: The `define-enum-relation`, `define-base-relations`, and `define-prefilter` declarations in `problem-corner.lisp` are currently commented out inside `#| |#` blocks. They must be uncommented for the enumerator to function. No changes to their content are needed.

## Approach: One-Step-to-Goal Predicate

The key insight is that penultimate state enumeration uses **the same CSP machinery** as goal state enumeration — the same base-relation schema, same enum-actions, same state space. The only difference is the **goal predicate** applied at each leaf:

- **Goal enumeration**: propagate base state → check goal predicate
- **Penultimate enumeration**: propagate base state → generate candidate last actions → for each, forward-apply action + propagate result → check goal predicate → if any succeeds, this is a valid penultimate state

The enumerator does not need structural changes. We need:

1. A **one-step-to-goal predicate function** that, given a propagated state, tests whether any single action from that state reaches the goal.
2. A **user-facing entry point** (`find-penultimate-states`) analogous to `find-goal-states`.
3. A **candidate last-action generator** appropriate for the enumeration context.

## Implementation Plan

### Step 1: Candidate Last-Action Generation for Enumerated States

`ww-backwards.lisp` already has `bw-candidate-last-actions` which generates MOVE, PICKUP, and CONNECT candidates from a state. However, it relies on `*types*` being populated and uses `bw--` utility functions.

Create a function (in `ww-enumerator.lisp` or `ww-backwards.lisp`) that generates candidate last actions from an enumerated state. Requirements:

- For **MOVE**: if agent is at area X, generate `(move agent from X)` for each area `from` ≠ X where accessibility holds. Note: the penultimate state's gate/open status determines accessibility — this is available after propagation.
- For **PICKUP-CONNECTOR**: if agent holds a connector (derived from no `loc` for that connector), generate `(pickup-connector agent connector agent-area)`. This is the reverse: in the penultimate state the connector is placed; picking it up is the last action that leads to a goal state where the agent holds it.
  - **Important**: the candidate is generated when a connector IS placed at the agent's location AND the goal state has the agent holding that connector (or more generally, when pickup is a plausible last action).
  - More precisely: generate pickup candidates for every connector that is at the agent's location. The forward application + goal check determines validity.
- For **CONNECT-TO-N-TERMINUS**: if the agent holds a connector (no `loc` for it), generate connect candidates with plausible terminus combinations. The terminus candidates come from fixtures and other placed connectors that are `selectable` from the agent's area.
  - Generate 1-, 2-, and 3-terminus combinations.
  - The forward validation (apply + propagate + goal check) handles correctness.

The existing `bw-candidate-last-actions` or `bw-corner-candidate-actions` can serve as a reference or be called directly, provided the `*actions*` context issue is resolved (see below).

### Step 2: Forward Action Application within the Enumerator

**Critical context issue**: During enumeration, `enumerate-state-csp` temporarily replaces `*actions*` with the generated enum-actions. If the one-step-to-goal predicate calls `apply-action-to-state`, that function may look up action structs from `*actions*` by name — and will find enum-actions instead of the problem's real actions.

Two approaches to resolve this:

**Approach A — Save and bind original actions**: Before enumeration starts, capture the original `*actions*` list. Pass it into the goal predicate via closure. The predicate temporarily rebinds `*actions*` when calling `apply-action-to-state`.

```lisp
;; Sketch:
(let ((problem-actions saved-actions))
  (lambda (state)
    (let ((*actions* problem-actions))
      (one-step-to-goal-p state original-goal-fn candidate-actions))))
```

**Approach B — Direct state manipulation**: Instead of using `apply-action-to-state`, implement lightweight forward-apply functions that directly modify a copied state. For problem-corner:

- MOVE: change `(loc agent area)`, call `propagate-changes!`
- PICKUP: delete `(loc connector area)`, delete all `(paired connector X)` and `(paired X connector)`, delete `(color connector hue)`, call `propagate-changes!`
- CONNECT-TO-N: add `(loc connector area)`, add `(paired connector T1)` etc., call `propagate-changes!`

This avoids the `*actions*` context issue entirely but duplicates action semantics.

**Recommendation**: Approach A is cleaner and stays in sync with action definitions. The key is ensuring `apply-action-to-state` uses `*actions*` for lookup (verify this in the codebase). If it uses some other mechanism, adjust accordingly.

An alternative sub-approach: store the problem actions in a dedicated variable (e.g., `*problem-actions*`) before enumeration begins, and have `apply-action-to-state` accept an optional actions list parameter.

### Step 3: The One-Step-to-Goal Predicate

```lisp
;; Conceptual structure:
(defun make-penultimate-goal-fn (goal-fn problem-actions
                                 &key action-generator)
  "Return a predicate that returns T if any single action from STATE reaches GOAL-FN.
   PROBLEM-ACTIONS: the original *actions* list from the problem spec.
   ACTION-GENERATOR: function of (state) -> list of action forms."
  (lambda (state)
    (let ((*actions* problem-actions))  ; restore problem action context
      (some (lambda (action-form)
              (multiple-value-bind (next-state ok reason)
                  (apply-action-to-state action-form state nil nil)
                (declare (ignore reason))
                (when ok
                  ;; propagate-changes! is called by the action's (finally ...)
                  ;; but verify this — if not, call it explicitly here
                  (funcall goal-fn next-state))))
            (funcall action-generator state)))))
```

**Note on propagation**: In problem-corner, every action's effect includes `(finally (propagate-changes!))`, so `apply-action-to-state` should trigger propagation as part of the action effect. Verify this. If the enumerator's context interferes with the `(finally ...)` mechanism, explicit propagation after `apply-action-to-state` may be needed.

**Note on `propagate-changes!` context**: The `propagate-changes!` update function references `forward-list` and `inverse-list` as special variables. The `bw-normalize!` function in `ww-backwards.lisp` binds these before calling propagation:

```lisp
(let (forward-list inverse-list)
  (declare (special forward-list inverse-list))
  (funcall (symbol-function 'propagate-changes!) state))
```

The one-step-to-goal predicate may need the same bindings if `apply-action-to-state` doesn't provide them. Check whether the action's `(finally (propagate-changes!))` establishes these bindings or expects them to exist.

### Step 4: Prefilter Considerations

The existing `define-prefilter corner-paths` checks that `transmitter1→receiver2` and `transmitter2→receiver3` are reachable via `paired` paths. This is correct for goal states but **potentially too strict for penultimate states**:

- **Last action = MOVE**: The beam network is fully established in the penultimate state. The prefilter is correct — both paths must exist.
- **Last action = PICKUP**: All connectors are placed in the penultimate state, beam network is complete. The prefilter is correct.
- **Last action = CONNECT**: One connector is held (no `loc`, no `paired` facts). The paired-path to one or both receivers may be incomplete in the penultimate state — it becomes complete only after the connect action. **The prefilter would incorrectly reject these states.**

Options:

1. **Disable the prefilter** for penultimate enumeration (safe, complete, but more expensive).
2. **Weaken the prefilter**: require that the paired paths are complete OR that an unassigned connector exists (indicating a connect action could complete them).
3. **Use a penultimate-specific prefilter**: check that for each required path, either the path exists OR there's a held connector that could plausibly complete it (the held connector's type allows pairing with the missing terminus).

For initial development, **option 1 (disable prefilter)** ensures completeness. Optimize with option 2 or 3 after correctness is validated.

The `find-goal-states` macro already supports `:prefilter nil` to disable the installed prefilter.

### Step 5: User-Facing Entry Point

Create `find-penultimate-states` (macro + function) analogous to `find-goal-states`:

```lisp
(defmacro find-penultimate-states (goal-spec &key ...)
  ...)

(defun find-penultimate-states-fn (goal-spec &key
                                   (algorithm *algorithm*)
                                   (solution-type 'every)
                                   (prefilter nil)  ; default off for completeness
                                   (action-families '(:move :pickup :connect))
                                   exclude-relations
                                   include-relations)
  "Enumerate penultimate states: base-relation states from which a single
   action application reaches GOAL-SPEC.
   ACTION-FAMILIES controls which last-action types to consider."
  ...)
```

The function should:

1. Capture the current `*actions*` before enumeration begins.
2. Build the one-step-to-goal predicate from the original goal + captured actions.
3. Call `enumerate-state-csp` (or `find-goal-states-fn`'s internal path) with the penultimate predicate as the goal function.
4. Build and print a report (reuse `fgs-build-report` pattern).
5. Store results in `*enumerated-penultimate-states*` or similar.

### Step 6: Report Structure

The penultimate report should include, for each valid penultimate state:

- The base-fact propositions
- Which last action(s) reach the goal from this state
- The resulting goal state(s)

This is more informative than the goal-state report because the user needs to know *how* each penultimate state connects to the goal.

```lisp
;; Report structure:
(:goal <goal-form>
 :penultimate-states <count>
 :results (
   (:state <problem-state>
    :base-props <proposition-list>
    :reaching-actions (
      (:action <action-form> :goal-state <problem-state>)
      ...))
   ...))
```

## Key Design Decisions to Resolve During Implementation

1. **How does `apply-action-to-state` resolve action structs?** Check whether it uses `*actions*` or another mechanism. This determines whether Approach A (rebind `*actions*`) works.

2. **Does `(finally (propagate-changes!))` execute within `apply-action-to-state`?** If yes, the predicate doesn't need explicit propagation. If no (e.g., `finally` is handled only by the searcher), the predicate must call `propagate-changes!` explicitly after action application.

3. **Should `elevation` be included in the base schema?** Currently `elevation` is `:derived` in the enum metadata. In problem-corner, `elevation` has been commented out of the actions (all objects are at elevation 0). If elevation is relevant in other domains, it may need to be a base relation for penultimate enumeration. For problem-corner, `:derived` is correct.

4. **Should `holds` be enumerated or derived?** Currently `holds` has enum metadata as `:fluent`, but `derive-holds!` in `propagate-consequences!` computes `holds` from the absence of `loc` for cargo. If `holds` is derived by propagation, it should NOT be in the base schema for enumeration — it would create contradictions. Verify that `holds` is excluded from `define-base-relations (loc paired)`. It is: only `loc` and `paired` are declared. The `define-enum-relation holds` metadata exists but won't generate enum-actions because `holds` is not in the base schema.

5. **Performance**: For problem-corner, the base-state space is bounded by ~125 loc assignments × paired subsets (pruned by prefilter). Each surviving state requires propagation + N candidate action applications (each with its own propagation). With ~20 candidate actions per state and propagation taking ~1ms, a space of 1000 base states would take ~20 seconds. Acceptable for development. Monitor and optimize if needed.

## Testing Plan

### Phase 1: Validate with Known Solution

Problem-corner's optimal solution is 15 actions. State S14 (penultimate) is known from `*corner-state-props*` in `ww-backwards.lisp`. The penultimate enumeration must find a state equivalent to S14 (on base facts) among its results.

```lisp
;; After enumeration:
(some (lambda (ps)
        (bw-equivalent-p ps (bw-corner-state 14)
                         :relations *bw-base-fact-relations*))
      *enumerated-penultimate-states*)
;; => T
```

### Phase 2: Completeness Check

For the known solution trace, states S0–S14 are all penultimate to their respective successor states. Test that for each Si (i < 15), the state's base-fact pattern appears in the penultimate enumeration for the goal `Si+1`'s derived properties. This is a more thorough test but requires parameterizing the goal.

### Phase 3: Cross-Validate with Backward Regression

Compare penultimate states found by enumeration against those found by `bw-regressed-predecessor-report`. The enumeration should find a superset — everything regression finds, plus the states regression misses due to the paired-fact gap.

```lisp
;; Regression results for comparison:
(let ((report (bw-regressed-predecessor-report
               (bw-corner-valid-goal-states)
               (bw-candidate-last-actions ...)
               :verbose nil)))
  (bw-top-milestones report :n 20 :return :props))
```

### Phase 4: Verify Completeness Gap is Closed

Specifically test that penultimate states involving CONNECT-TO-N-TERMINUS as the last action are found — these are the states that regression misses because it can't reconstruct prior paired facts.

## Summary of Required Changes

| Component | Change | Location |
|---|---|---|
| Candidate action generator | Reuse or adapt from `ww-backwards.lisp` | `ww-enumerator.lisp` or `ww-backwards.lisp` |
| One-step-to-goal predicate | New function | `ww-enumerator.lisp` |
| `find-penultimate-states` macro + fn | New entry point | `ww-enumerator.lisp` |
| Penultimate report builder | New function | `ww-enumerator.lisp` |
| Prefilter handling | Pass `:prefilter nil` or weaker filter | `ww-enumerator.lisp` |
| `*actions*` context management | Save/restore around forward application | `ww-enumerator.lisp` |
| `problem-corner.lisp` enum specs | Uncomment existing declarations | `problem-corner.lisp` |
| Parameters for results | `*enumerated-penultimate-states*` etc. | `ww-enumerator.lisp` |

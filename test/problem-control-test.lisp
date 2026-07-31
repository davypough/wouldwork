;;; Filename: problem-control-test.lisp

;;; Control regression covering the shared DNF control substrate and gate behavior.
;;; Independent fixtures characterize:
;;;
;;;   1. Single normal and inverted controls in both energized states.
;;;   2. A two-controller AND clause at its exact all-on boundary.
;;;   3. Multiple OR clauses with a later true clause, and with no true clause.
;;;   4. The distinct empty-DNF boundaries: no clauses, versus one empty clause.
;;;   5. An uncontrolled gate, which defaults closed.
;;;   6. A jammed inverted gate, which stays open despite its control result.
;;;   7. ENERGIZED directly for depressed and clear plates and an inactive receiver.
;;;   8. A lifecycle pair sharing one plate: normal open/inverted closed initially,
;;;      then normal closed/inverted open after the sole action removes the weight.
;;;
;;; The goal is a characterization query over all stable fixtures and the lifecycle's
;;; final derived state.  Important absent facts are asserted explicitly: false control
;;; branches do not open their gates, the uncontrolled gate has no control declaration
;;; and remains closed, the clear plate and inactive receiver are not energized, and the
;;; released lifecycle plate is neither depressed nor energized.
;;;
;;; Expected minimum solution (1 step): RELEASE-LIFECYCLE-CONTROL.


(in-package :ww)


(ww-set *problem-name* control-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 1)


;;;; TYPES ;;;;


(define-types
  agent (agent1)
  location (idle control-site lifecycle-site jammer-site)
  plate (on-plate-a on-plate-b off-plate lifecycle-plate)
  box (weight-a weight-b lifecycle-weight)
  receiver (inactive-receiver)
  gate (normal-on-gate normal-off-gate
        inverted-on-gate inverted-off-gate
        and-all-gate and-partial-gate
        or-one-gate or-none-gate
        empty-clauses-normal-gate empty-clauses-inverted-gate
        empty-clause-normal-gate empty-clause-inverted-gate
        uncontrolled-gate jammed-inverted-gate
        lifecycle-normal-gate lifecycle-inverted-gate)
  jammer (test-jammer)
  mode (normal inverted))


;;;; TECHNOLOGY INCLUDES ;;;;


(include-tech plate)
(include-tech gate)
(include-tech jammer)


;;;; INITIALIZATION ;;;;


(define-init
  ;; Keep the agent away from the jammer so JAMMER's manipulation actions cannot
  ;; provide an alternate plan within the one-step cutoff.
  (has-location agent1 idle)
  (has-location test-jammer jammer-site)

  ;; Two depressed controls and two clear controls.  The inactive receiver has no beam
  ;; input and supplies the non-plate ENERGIZED negative case.
  (has-position on-plate-a control-site)
  (has-position on-plate-b control-site)
  (has-position off-plate control-site)
  (has-position lifecycle-plate lifecycle-site)
  (has-location weight-a control-site)
  (has-location weight-b control-site)
  (has-location lifecycle-weight lifecycle-site)
  (on weight-a on-plate-a)
  (on weight-b on-plate-b)
  (on lifecycle-weight lifecycle-plate)

  ;; Single-clause controls in both modes and both source states.
  (controls ((on-plate-a)) normal-on-gate normal)
  (controls ((off-plate)) normal-off-gate normal)
  (controls ((on-plate-a)) inverted-on-gate inverted)
  (controls ((off-plate)) inverted-off-gate inverted)

  ;; Conjunction is exact: every member of one clause must be energized.
  (controls ((on-plate-a on-plate-b)) and-all-gate normal)
  (controls ((on-plate-a off-plate)) and-partial-gate normal)

  ;; Disjunction is across clauses.  OR-ONE-GATE's first clause is false and its later
  ;; singleton clause is true; OR-NONE-GATE has no true clause.
  (controls ((off-plate inactive-receiver) (on-plate-a)) or-one-gate normal)
  (controls ((off-plate) (inactive-receiver)) or-none-gate normal)

  ;; Empty outer DNF means no clause succeeds.  One empty clause succeeds vacuously.
  (controls () empty-clauses-normal-gate normal)
  (controls () empty-clauses-inverted-gate inverted)
  (controls (()) empty-clause-normal-gate normal)
  (controls (()) empty-clause-inverted-gate inverted)

  ;; UNCONTROLLED-GATE deliberately has no CONTROLS fact.  Jamming dominates the false
  ;; result of JAMMED-INVERTED-GATE's inverted, vacuously true empty clause.
  (controls (()) jammed-inverted-gate inverted)
  (jamming test-jammer jammed-inverted-gate)

  ;; The lifecycle pair must swap states after the one planned action.
  (controls ((lifecycle-plate)) lifecycle-normal-gate normal)
  (controls ((lifecycle-plate)) lifecycle-inverted-gate inverted))


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!)))


;;;; LIFECYCLE ACTION ;;;;


(define-action release-lifecycle-control
  1
  ()
  (and (on lifecycle-weight lifecycle-plate)
       (depressed lifecycle-plate)
       (open lifecycle-normal-gate)
       (not (open lifecycle-inverted-gate)))
  ("> test weight releases the lifecycle plate")
  (assert (not (on lifecycle-weight lifecycle-plate))
          (finally (propagate-changes!))))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query control-scenarios-valid ()
  (and
    ;; Direct source and ENERGIZED characterization.
    (depressed on-plate-a)
    (depressed on-plate-b)
    (not (depressed off-plate))
    (energized on-plate-a)
    (energized on-plate-b)
    (not (energized off-plate))
    (not (active inactive-receiver))
    (not (energized inactive-receiver))

    ;; Normal/inverted singleton cases.
    (open normal-on-gate)
    (not (open normal-off-gate))
    (not (open inverted-on-gate))
    (open inverted-off-gate)

    ;; Exact AND and OR behavior.
    (open and-all-gate)
    (not (open and-partial-gate))
    (open or-one-gate)
    (not (open or-none-gate))

    ;; Exact empty-DNF boundaries.
    (not (open empty-clauses-normal-gate))
    (open empty-clauses-inverted-gate)
    (open empty-clause-normal-gate)
    (not (open empty-clause-inverted-gate))

    ;; Default and jammer override behavior.
    (not (bind (controls $clauses uncontrolled-gate $mode)))
    (not (open uncontrolled-gate))
    (jamming test-jammer jammed-inverted-gate)
    (open jammed-inverted-gate)

    ;; Final lifecycle state: the weight stays at the site but no longer occupies the
    ;; plate, so the shared source clears and the two gate modes swap.
    (has-location lifecycle-weight lifecycle-site)
    (not (on lifecycle-weight lifecycle-plate))
    (cleartop lifecycle-plate)
    (not (depressed lifecycle-plate))
    (not (energized lifecycle-plate))
    (not (open lifecycle-normal-gate))
    (open lifecycle-inverted-gate)))


(define-goal
  (control-scenarios-valid))

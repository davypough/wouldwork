;;; Dedicated zero-action regression for -PROPAGATION's successful fixpoint
;;; boundary.
;;;
;;; A test-owned update changes PASS-COUNT only while it is below 3.  It also
;;; increments the copied state's VALUE slot on every invocation; VALUE is not a
;;; proposition, so this records the final unchanged confirmation pass without
;;; falsely setting *PROPAGATED-STATE-CHANGED*.
;;;
;;; Propagation on an isolated copy must therefore perform three changing passes,
;;; execute one unchanged fourth pass, return T immediately, and leave the copy
;;; consistent at PASS-COUNT 3 with VALUE 4.  The real planner state remains at
;;; PASS-COUNT 0 and VALUE 0.  No action or initialization action exists, so the
;;; expected minimum path length is 0.

(in-package :ww)

(ww-set *problem-name* propagation-convergence-success-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 1)


;;;; TECHNOLOGY INCLUDE ;;;;


(include-tech -propagation)


;;;; TEST-OWNED CONVERGENT STATE ;;;;


(define-dynamic-relations
  (pass-count $fixnum))


(define-update increment-pass-count-until-three! ()
  (do
    ;; This copied-state metadata records calls without signaling a propagated
    ;; proposition change.
    (incf (problem-state.value state))
    (bind (pass-count $count))
    (if (< $count 3)
      (pass-count (1+ $count)))))


(define-update propagate-consequences! ()
  (let ((*propagated-state-changed* nil))
    (increment-pass-count-until-three!)
    *propagated-state-changed*))


;;;; INITIALIZATION ;;;;


(define-init
  (pass-count 0))


;;;; CHARACTERIZATION HELPER ;;;;


(setf
  (symbol-function 'propagation-convergence-success-valid-p)
  (lambda (state)
    (let* ((before (database state))
           (before-value (problem-state.value state))
           (trial (copy-problem-state state))
           (result
             (funcall
               (symbol-function 'propagate-changes!)
               trial)))
      (and
        ;; Three changing passes followed by one unchanged pass converge.
        result
        (equal (database trial) '((pass-count 3)))
        (= (problem-state.value trial) 4)
        (not (state-is-inconsistent trial))

        ;; The authored consequence driver contains only the bounded update.
        (equal
          (authored-propagation-order
            (authored-propagation-driver-body))
          '(increment-pass-count-until-three!))

        ;; The isolated run must not leak into the real planner state.
        (equal before '((pass-count 0)))
        (equal (database state) before)
        (= before-value 0)
        (= (problem-state.value state) before-value)
        (not (state-is-inconsistent state))
        (null *init-actions*)
        (null *actions*)))))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query propagation-convergence-success-scenarios-valid ()
  (and
    (pass-count 0)
    (propagation-convergence-success-valid-p state)))


(define-goal
  (propagation-convergence-success-scenarios-valid))

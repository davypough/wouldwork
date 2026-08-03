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
(setf *expected-min-length* 0)
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


;;;; CHARACTERIZATION CLAIM ;;;;


(define-test-claim propagation-convergence-success-contract
  (let* ((before (database *start-state*))
         (before-value (problem-state.value *start-state*))
         (trial (copy-problem-state *start-state*))
         (result
           (funcall 'propagate-changes! trial)))
    (and
      result
      (equal (database trial) '((pass-count 3)))
      (= (problem-state.value trial) 4)
      (not (state-is-inconsistent trial))
      (equal before '((pass-count 0)))
      (equal (database *start-state*) before)
      (= before-value 0)
      (= (problem-state.value *start-state*) before-value)
      (not (state-is-inconsistent *start-state*))))
  (equal
    (authored-propagation-order
      (authored-propagation-driver-body))
    '(increment-pass-count-until-three!))
  (expect-registrations :init-action '())
  (expect-registrations :action '()))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query propagation-convergence-success-scenarios-valid ()
  (pass-count 0))


(define-goal
  (propagation-convergence-success-scenarios-valid))

;;; Dedicated zero-action regression for -PROPAGATION's exact nonconvergence
;;; boundary.
;;;
;;; A test-owned update increments PASS-COUNT on every consequence pass, so
;;; propagation can never report convergence.  The characterization goal copies
;;; the planner state and runs PROPAGATE-CHANGES! only on that copy.  The loop
;;; must execute exactly ten passes, leave PASS-COUNT at 10, add
;;; INCONSISTENT-STATE, and return NIL.
;;;
;;; The real planner state must remain consistent and unchanged at PASS-COUNT 0.
;;; No action or initialization action exists, so the expected minimum path
;;; length is 0.

(in-package :ww)

(ww-set *problem-name* propagation-convergence-limit-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 1)
(setf *expected-min-length* 0)


;;;; TECHNOLOGY INCLUDE ;;;;


(include-tech -propagation)


;;;; TEST-OWNED NONCONVERGENT STATE ;;;;


(define-dynamic-relations
  (pass-count $fixnum))


(define-update increment-pass-count! ()
  (do
    (bind (pass-count $count))
    (pass-count (1+ $count))))


(define-update propagate-consequences! ()
  (let ((*propagated-state-changed* nil))
    (increment-pass-count!)
    *propagated-state-changed*))


;;;; INITIALIZATION ;;;;


(define-init
  (pass-count 0))


;;;; CHARACTERIZATION CLAIM ;;;;


(define-test-claim propagation-convergence-limit-contract
  (let* ((before (database *start-state*))
         (trial (copy-problem-state *start-state*))
         (result
           (funcall 'propagate-changes! trial)))
    (and
      (null result)
      (state-is-inconsistent trial)
      (equal
        (database trial)
        '((inconsistent-state)
          (pass-count 10)))
      (equal before '((pass-count 0)))
      (equal (database *start-state*) before)
      (not (state-is-inconsistent *start-state*))))
  (equal
    (authored-propagation-order
      (authored-propagation-driver-body))
    '(increment-pass-count!))
  (expect-registrations :init-action '())
  (expect-registrations :action '()))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query propagation-convergence-limit-scenarios-valid ()
  (pass-count 0))


(define-goal
  (propagation-convergence-limit-scenarios-valid))

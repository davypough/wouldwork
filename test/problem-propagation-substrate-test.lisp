;;; Dedicated zero-action regression for the shared -propagation substrate's
;;; empty-driver boundary.
;;;
;;; With no technology-contributed update, initialization must leave
;;; PROPAGATE-CONSEQUENCES!'s sentinel intact rather than installing a silent
;;; no-op driver.  A test-local condition helper invokes PROPAGATE-CHANGES! and
;;; requires the sentinel's explicit error while confirming that the failed call
;;; does not alter or invalidate the state.
;;;
;;; The initial and final dynamic states are empty.  No action or initialization
;;; action is defined, so the expected minimum path length is 0.

(in-package :ww)

(ww-set *problem-name* propagation-substrate-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 1)
(setf *expected-min-length* 0)


;;;; TECHNOLOGY INCLUDE ;;;;


(include-tech -propagation)


;;;; SENTINEL CONDITION CHARACTERIZATION ;;;;


(define-test-helper empty-propagation-driver-signals-p (state)
  (let ((before (database state))
        (condition nil))
    (setf condition
      (handler-case
          (progn
            (funcall
              'propagate-changes!
              state)
            nil)
        (error (error-condition)
          error-condition)))
    (and
      condition
      (search
        "still holding tech/-propagation.lisp's sentinel body"
        (princ-to-string condition))
      (equal (database state) before)
      (not (state-is-inconsistent state)))))


;;;; CHARACTERIZATION CLAIM ;;;;


(define-test-claim propagation-substrate-schema
  (expect-registrations
    :update '(propagate-changes! propagate-consequences!))
  (null (driver-candidate-updates))
  (equal
    (get 'propagate-consequences! :raw-body)
    *propagation-driver-sentinel*)
  (null (authored-propagation-driver-body))
  (expect-types '())
  (expect-relations :dynamic '(inconsistent-state))
  (expect-relations :static '())
  (expect-registrations :init-action '())
  (expect-registrations :action '())
  (null (database *start-state*))
  (not (state-is-inconsistent *start-state*)))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query propagation-substrate-scenarios-valid ()
  ;; The empty order remains explicit and fails before doing any state work.
  (empty-propagation-driver-signals-p state))


(define-goal
  (propagation-substrate-scenarios-valid))

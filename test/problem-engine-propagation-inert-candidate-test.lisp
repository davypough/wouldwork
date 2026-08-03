;;; Dedicated zero-action regression for propagation-driver installation when
;;; a technology contributes a real candidate update whose quantified type is
;;; empty.
;;;
;;; -BEAM-SUBSTRATE contributes UPDATE-RECEIVER-STATUS!, so the raw candidate
;;; list must contain that update.  This problem declares no receivers, however,
;;; making the update provably inert.  Installation must filter it out and leave
;;; PROPAGATE-CONSEQUENCES!'s explicit sentinel intact rather than installing a
;;; silent no-op driver.
;;;
;;; A characterization helper invokes PROPAGATE-CHANGES! and requires the
;;; sentinel error while confirming that the failed call changes no state.
;;; The initial and final dynamic states are empty, no action or initialization
;;; action exists, and the expected minimum path length is 0.

(in-package :ww)

(ww-set *problem-name* engine-propagation-inert-candidate-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 1)
(setf *expected-min-length* 0)


;;;; TECHNOLOGY INCLUDE ;;;;


(include-tech -beam-substrate)


;;;; SENTINEL CONDITION CHARACTERIZATION ;;;;


(define-test-helper inert-candidate-driver-signals-p (state)
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


(define-test-claim propagation-inert-candidate-contract
  (equal (driver-candidate-updates) '(update-receiver-status!))
  (update-quantifies-only-over-empty-types-p 'update-receiver-status!)
  (null
    (remove-if
      #'update-quantifies-only-over-empty-types-p
      (driver-candidate-updates)))
  (equal
    (get 'propagate-consequences! :raw-body)
    *propagation-driver-sentinel*)
  (null (authored-propagation-driver-body))
  (expect-empty-type 'receiver)
  (expect-registrations :init-action '())
  (expect-registrations :action '())
  (null (database *start-state*))
  (not (state-is-inconsistent *start-state*)))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query propagation-inert-candidate-scenarios-valid ()
  (inert-candidate-driver-signals-p state))


(define-goal
  (propagation-inert-candidate-scenarios-valid))

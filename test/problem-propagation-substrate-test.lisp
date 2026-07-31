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


(setf
  (symbol-function 'empty-propagation-driver-signals-p)
  (lambda (state)
    (let ((before (database state))
          (condition nil))
      (setf condition
        (handler-case
            (progn
              (funcall
                (symbol-function 'propagate-changes!)
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
        (not (state-is-inconsistent state))))))


;;;; CHARACTERIZATION HELPERS ;;;;


(setf
  (symbol-function 'propagation-substrate-metadata-valid-p)
  (lambda (state)
    (and
      (equal
        *update-names*
        '(propagate-changes! propagate-consequences!))
      (null (driver-candidate-updates))
      (equal
        (get 'propagate-consequences! :raw-body)
        *propagation-driver-sentinel*)
      (null (authored-propagation-driver-body))
      (zerop (hash-table-count *types*))
      (= (hash-table-count *relations*) 1)
      (nth-value 1 (gethash 'inconsistent-state *relations*))
      (zerop (hash-table-count *static-relations*))
      (null *init-actions*)
      (null *actions*)
      (null (database state))
      (not (state-is-inconsistent state)))))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query propagation-substrate-scenarios-valid ()
  (and
    ;; The empty order remains explicit and fails before doing any state work.
    (empty-propagation-driver-signals-p state)

    ;; No driver candidate, authored replacement, or stateful behavior leaks in.
    (propagation-substrate-metadata-valid-p state)))


(define-goal
  (propagation-substrate-scenarios-valid))

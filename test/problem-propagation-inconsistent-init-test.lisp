;;; Dedicated zero-action regression for -PROPAGATION's initialization-time
;;; inconsistency boundary: VALIDATE-START-STATE-CONSISTENCY.
;;;
;;; Unlike a nonconvergent successor -- GENERATE-CHILDREN silently discards the
;;; candidate and search continues on other branches, characterized by
;;; problem-engine-propagation-inconsistent-successor-test.lisp -- an
;;; inconsistent INITIAL state is fatal: INIT calls
;;; VALIDATE-START-STATE-CONSISTENCY immediately after DO-INIT-ACTION-UPDATES,
;;; and it signals a hard Lisp ERROR rather than discarding anything, since
;;; there is no other candidate state to fall back to.
;;;
;;; That call happens automatically, deep inside ordinary problem loading, so
;;; it cannot be exercised in situ here without also aborting TEST-TALOS's own
;;; sweep for this file.  Instead the characterization helper isolates it: it
;;; copies the real (consistent) start state, marks only the copy inconsistent,
;;; rebinds *START-STATE* to that copy for the duration of one direct call to
;;; VALIDATE-START-STATE-CONSISTENCY, and confirms the documented error is
;;; signaled -- then confirms the real search state was never touched.
;;;
;;; The real planner state must remain consistent throughout.  No action or
;;; init-action exists, so the expected minimum path length is 0.

(in-package :ww)

(ww-set *problem-name* propagation-inconsistent-init-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 1)
(setf *expected-min-length* 0)


;;;; TECHNOLOGY INCLUDE ;;;;


(include-tech -propagation)


;;;; CHARACTERIZATION HELPER ;;;;


(setf
  (symbol-function 'propagation-inconsistent-init-valid-p)
  (lambda (state)
    (let* ((before (database state))
           (trial (copy-problem-state state)))
      (setf (gethash *inconsistent-state-key* (problem-state.idb trial)) t)
      (let* ((*start-state* trial)
             (condition
               (handler-case
                   (progn (validate-start-state-consistency) nil)
                 (error (error-condition) error-condition))))
        (and
          ;; VALIDATE-START-STATE-CONSISTENCY signals the documented fatal error.
          condition
          (search "Initial state is inconsistent" (princ-to-string condition))

          ;; The isolated probe must not leak into the real planner state.
          (equal (database state) before)
          (not (state-is-inconsistent state))
          (null *init-actions*)
          (null *actions*))))))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query propagation-inconsistent-init-scenarios-valid ()
  (and (propagation-inconsistent-init-valid-p state)))


(define-goal
  (propagation-inconsistent-init-scenarios-valid))

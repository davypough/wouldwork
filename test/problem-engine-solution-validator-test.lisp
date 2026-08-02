;;; Filename: problem-engine-solution-validator-test.lisp

;;; Focused search-lifecycle problem for candidate solution validators.  The first action
;;; reaches a nominal goal that the registered path validator rejects.  Search must retain
;;; that state as an ordinary successor, apply the repair action, and accept only depth 2.


(in-package :ww)


(ww-set *problem-name* engine-solution-validator-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 2)


(define-types
  validator-stage (validator-start validator-rejected validator-accepted))


(define-dynamic-relations
  (validator-at validator-stage))


(define-action advance-to-rejected-goal
    1
  ()
  (validator-at validator-start)
  ()
  (assert (validator-at validator-rejected)
          (not (validator-at validator-start))))


(define-action repair-rejected-goal
    1
  ()
  (validator-at validator-rejected)
  ()
  (assert (validator-at validator-accepted)
          (not (validator-at validator-rejected))))


(define-init
  (validator-at validator-start))


(define-goal
  (or (validator-at validator-rejected)
      (validator-at validator-accepted)))


(defun accept-only-repaired-validator (start-state path goal-state)
  "Accept only the repaired goal, returning a diagnostic for the rejected prefix."
  (declare (ignore start-state path))
  (if (member '(validator-at validator-accepted)
              (list-database (problem-state.idb goal-state))
              :test #'equal)
    (values t nil)
    (values nil '(:reason :repair-required))))


(register-solution-validator 'accept-only-repaired-validator)

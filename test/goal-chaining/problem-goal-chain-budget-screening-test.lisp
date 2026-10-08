;;; Filename: problem-goal-chain-budget-screening-test.lisp

;;; Contextual budget screening.  Every A checkpoint needs exactly two more actions
;;; to reach the original goal.  A one-step final request therefore proves both A
;;; alternatives impossible within that request, while a later two-step request must
;;; ignore the stale proof context and succeed.

(in-package :ww)


(ww-set *problem-name* goal-chain-budget-screening-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* first)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 3)


(define-types
  budget-stage (budget-start budget-a budget-mid budget-c)
  budget-choice (budget-bad budget-good))


(define-dynamic-relations
  (budget-at budget-stage)
  (budget-choice-selected budget-choice))


(define-init
  (budget-at budget-start))


(define-action choose-good-budget-a
  1 () (budget-at budget-start) ()
  (assert (not (budget-at budget-start))
          (budget-at budget-a)
          (budget-choice-selected budget-good)))


(define-action choose-bad-budget-a
  1 () (budget-at budget-start) ()
  (assert (not (budget-at budget-start))
          (budget-at budget-a)
          (budget-choice-selected budget-bad)))


(define-action advance-budget-route
  1 () (budget-at budget-a) ()
  (assert (not (budget-at budget-a)) (budget-at budget-mid)))


(define-action finish-budget-route
  1 () (budget-at budget-mid) ()
  (assert (not (budget-at budget-mid)) (budget-at budget-c)))


(define-test-helper screen-budget-checkpoint (state context)
  (let ((budget
          (candidate-screening-context-remaining-depth-budget context)))
    (when (and budget
               (member '(budget-at budget-a)
                       (list-database (problem-state.idb state)) :test #'equal)
               (< budget 2))
      (make-candidate-screening-result
        :status :impossible :source :fixture-admissible-bound
        :reason :lower-bound-exceeds-budget
        :evidence (list :lower-bound 2 :budget budget)))))


(register-candidate-state-screener
  'budget-checkpoint-bound 'screen-budget-checkpoint :priority 10)


(define-test-helper budget-choice-selected-p (choice)
  (member `(budget-choice-selected ,choice)
          (list-database (problem-state.idb *start-state*)) :test #'equal))


(define-test-helper call-with-isolated-budget-screening-chain (thunk)
  (let* ((saved-goal-function (symbol-function 'goal-fn))
         (saved-goal-value (symbol-value 'goal-fn))
         (saved-goal-form (get 'goal-fn :form))
         (saved-solutions *solution-paths*)
         (saved-valid *solutions-valid*))
    (unwind-protect
        (let ((*start-state* (copy-problem-state *start-state*))
              (*goal* (copy-tree *goal*))
              (*final-goal* nil)
              (*goal-chain-session* nil)
              (*undo-stack* nil)
              (*threads* 0)
              (*solution-type* 'first)
              (*tree-or-graph* 'graph)
              (*depth-cutoff* 2))
          (setf *solution-paths* nil *solutions-valid* nil)
          (install-compiled-goal '(budget-at budget-c))
          (funcall thunk))
      (setf (symbol-function 'goal-fn) saved-goal-function
            (symbol-value 'goal-fn) saved-goal-value
            (get 'goal-fn :form) saved-goal-form
            *solution-paths* saved-solutions
            *solutions-valid* saved-valid))))


(define-test-claim budget-screening-is-contextual-and-undoable
  (call-with-isolated-budget-screening-chain
    (lambda ()
      (let ((*depth-cutoff* 1))
        (solve-subgoal (budget-at budget-a))
        (solve))
      (let ((first-was-bad (budget-choice-selected-p 'budget-bad))
            (bounded-rejections
              (length
                (goal-chain-session-screening-rejections
                  *goal-chain-session*))))
        (let ((*depth-cutoff* 2))
          (solve))
        (let ((raised-cutoff-succeeded
                (and *solutions-valid*
                     (= (solution.depth (first *solution-paths*)) 3))))
          (ww-undo)
          (let ((first-undo-kept-proofs
                  (and
                    (= (length
                         (goal-chain-session-screening-rejections
                           *goal-chain-session*))
                       bounded-rejections)
                    (budget-choice-selected-p 'budget-bad))))
            (ww-undo)
            (and first-was-bad
                 (= bounded-rejections 2)
                 raised-cutoff-succeeded
                 first-undo-kept-proofs
                 (null
                   (goal-chain-session-screening-rejections
                     *goal-chain-session*))
                 (budget-choice-selected-p 'budget-bad))))))))


(define-goal
  (budget-at budget-c))

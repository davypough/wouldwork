;;; Filename: problem-goal-chain-backtracking-test.lisp

;;; Focused synthetic coverage for contextual cross-checkpoint recovery.  Broad milestone
;;; A has a dead and a viable exact state.  Broad milestone B has two dead states beneath
;;; dead A, but only the viable A/B suffix can reach C.  No recorder technology is involved.

(in-package :ww)


(ww-set *problem-name* goal-chain-backtracking-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* first)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 1)


(define-types
  backtrack-stage (bt-start bt-a bt-b bt-c bt-unreachable)
  backtrack-choice (bt-bad bt-good)
  backtrack-branch (bt-dead-one bt-dead-two bt-live))


(define-dynamic-relations
  (bt-at backtrack-stage)
  (bt-choice backtrack-choice)
  (bt-branch backtrack-branch))


(define-init
  (bt-at bt-start))


;; Search uses a LIFO frontier, so the later bad action is the first A endpoint.
(define-action choose-good-a
  1 () (bt-at bt-start) ()
  (assert (not (bt-at bt-start)) (bt-at bt-a) (bt-choice bt-good)))


(define-action choose-bad-a
  1 () (bt-at bt-start) ()
  (assert (not (bt-at bt-start)) (bt-at bt-a) (bt-choice bt-bad)))


(define-action bad-a-to-second-dead-b
  1 () (and (bt-at bt-a) (bt-choice bt-bad)) ()
  (assert (not (bt-at bt-a)) (bt-at bt-b) (bt-branch bt-dead-two)))


(define-action bad-a-to-first-dead-b
  1 () (and (bt-at bt-a) (bt-choice bt-bad)) ()
  (assert (not (bt-at bt-a)) (bt-at bt-b) (bt-branch bt-dead-one)))


(define-action good-a-to-live-b
  1 () (and (bt-at bt-a) (bt-choice bt-good)) ()
  (assert (not (bt-at bt-a)) (bt-at bt-b) (bt-branch bt-live)))


(define-action live-b-to-c
  1 () (and (bt-at bt-b) (bt-choice bt-good) (bt-branch bt-live)) ()
  (assert (not (bt-at bt-b)) (bt-at bt-c)))


(define-goal
  (bt-at bt-a))


(define-test-helper bt-state-has-p (state proposition)
  (member proposition (list-database (problem-state.idb state)) :test #'equal))


(define-test-helper bt-current-choice-p (choice)
  (bt-state-has-p *start-state* `(bt-choice ,choice)))


(define-test-helper call-with-isolated-backtracking-chain (thunk)
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
              (*depth-cutoff* 1))
          (setf *solution-paths* nil *solutions-valid* nil)
          (install-compiled-goal '(bt-at bt-c))
          (funcall thunk))
      (setf (symbol-function 'goal-fn) saved-goal-function
            (symbol-value 'goal-fn) saved-goal-value
            (get 'goal-fn :form) saved-goal-form
            *solution-paths* saved-solutions
            *solutions-valid* saved-valid))))


(define-test-helper bt-phase-endpoint-has-p (index proposition)
  (let ((phase (nth index (goal-chain-session-phases *goal-chain-session*))))
    (and phase
         (bt-state-has-p (solution.goal (goal-chain-phase-solution phase))
                         proposition))))


(define-test-claim goal-chain-automatic-second-a-state
  (call-with-isolated-backtracking-chain
    (lambda ()
      (solve-subgoal (bt-at bt-a))
      (let ((first-was-bad (bt-current-choice-p 'bt-bad)))
        (solve-subgoal (and (bt-at bt-b) (bt-choice bt-good)))
        (and first-was-bad
             (= (length (goal-chain-session-phases *goal-chain-session*)) 2)
             (bt-phase-endpoint-has-p 0 '(bt-choice bt-good))
             (bt-state-has-p *start-state* '(bt-at bt-b))
             (= (length (goal-chain-session-nogoods *goal-chain-session*)) 1))))))


(define-test-claim goal-chain-multiple-level-propagation
  (call-with-isolated-backtracking-chain
    (lambda ()
      (solve-subgoal (bt-at bt-a))
      (solve-subgoal (bt-at bt-b))
      (let ((dead-prefix
              (and (bt-current-choice-p 'bt-bad)
                   (bt-state-has-p *start-state* '(bt-at bt-b)))))
        (solve-subgoal (bt-at bt-c))
        (and dead-prefix
             (= (length (goal-chain-session-phases *goal-chain-session*)) 3)
             (bt-phase-endpoint-has-p 0 '(bt-choice bt-good))
             (bt-phase-endpoint-has-p 1 '(bt-branch bt-live))
             (bt-state-has-p *start-state* '(bt-at bt-c))
             (>= (length (goal-chain-session-nogoods *goal-chain-session*)) 3))))))


(define-test-claim goal-chain-all-alternatives-propagate
  (call-with-isolated-backtracking-chain
    (lambda ()
      (solve-subgoal (bt-at bt-a))
      (let ((stable-key
              (goal-chain-phase-endpoint-key
                (first (goal-chain-session-phases *goal-chain-session*)))))
        (solve-subgoal (bt-at bt-unreachable))
        (and (= (length (goal-chain-session-phases *goal-chain-session*)) 1)
             (goal-chain-state-key-equal-p
               stable-key
               (goal-chain-phase-endpoint-key
                 (first (goal-chain-session-phases *goal-chain-session*))))
             (bt-current-choice-p 'bt-bad)
             (= (length (goal-chain-session-nogoods *goal-chain-session*)) 2))))))


(define-test-claim goal-chain-nogood-context-includes-goal-and-cutoff
  (call-with-isolated-backtracking-chain
    (lambda ()
      (solve-subgoal (bt-at bt-a))
      (solve-subgoal (bt-at bt-unreachable))
      (let ((old-nogoods (length (goal-chain-session-nogoods *goal-chain-session*))))
        (let ((*depth-cutoff* 2))
          (solve-subgoal (and (bt-at bt-b) (bt-choice bt-good))))
        (and (= old-nogoods 2)
             (= (length (goal-chain-session-phases *goal-chain-session*)) 2)
             (bt-phase-endpoint-has-p 0 '(bt-choice bt-good))
             (= (cdr
                  (assoc '*depth-cutoff*
                    (goal-chain-search-settings-bindings
                      (goal-chain-request-settings
                        (goal-chain-phase-request
                          (first
                            (goal-chain-session-phases
                              *goal-chain-session*)))))))
                1)
             (= (cdr
                  (assoc '*depth-cutoff*
                    (goal-chain-search-settings-bindings
                      (goal-chain-request-settings
                        (goal-chain-phase-request
                          (second
                            (goal-chain-session-phases
                              *goal-chain-session*)))))))
                2)
             ;; The changed request did not erase the old contextual facts or mistake
             ;; them for global invalidity.
             (> (length (goal-chain-session-nogoods *goal-chain-session*))
                old-nogoods))))))


(define-test-helper bt-unknown-outcome-does-not-reject (reason)
  (call-with-isolated-backtracking-chain
    (lambda ()
      (solve-subgoal (bt-at bt-a))
      (let ((saved-ww-solve (symbol-function 'ww-solve))
            (before (length (goal-chain-session-nogoods *goal-chain-session*))))
        (unwind-protect
            (progn
              (setf (symbol-function 'ww-solve)
                    (lambda ()
                      (setf *solution-paths* nil
                            *solutions-valid* nil
                            *last-search-outcome*
                              (make-search-outcome
                                :status :unknown :reason reason))))
              (solve-subgoal-form '(bt-at bt-b)))
          (setf (symbol-function 'ww-solve) saved-ww-solve))
        (and (= before (length (goal-chain-session-nogoods *goal-chain-session*)))
             (= (length (goal-chain-session-phases *goal-chain-session*)) 1)
             (bt-current-choice-p 'bt-bad))))))


(define-test-claim goal-chain-unknown-results-create-no-nogoods
  (every #'bt-unknown-outcome-does-not-reject
         '(:interrupted :resource-cap :out-of-memory)))


(define-test-claim goal-chain-recovery-is-one-undo-transaction
  (call-with-isolated-backtracking-chain
    (lambda ()
      (solve-subgoal (bt-at bt-a))
      (solve-subgoal (and (bt-at bt-b) (bt-choice bt-good)))
      (let ((recovered (bt-current-choice-p 'bt-good)))
        (ww-undo)
        (let ((restored-bad
                (and (= (length (goal-chain-session-phases *goal-chain-session*)) 1)
                     (bt-current-choice-p 'bt-bad))))
          (ww-undo)
          (and recovered restored-bad
               (null *goal-chain-session*)
               (bt-state-has-p *start-state* '(bt-at bt-start))))))))


(define-test-claim goal-chain-graph-solution-types-remain-stable
  (every
    (lambda (solution-type)
      (call-with-isolated-backtracking-chain
        (lambda ()
          (let ((*solution-type* solution-type))
            (solve-subgoal (bt-at bt-a))
            (solve-subgoal (and (bt-at bt-b) (bt-choice bt-good)))
            (and (= (length (goal-chain-session-phases *goal-chain-session*)) 2)
                 (bt-current-choice-p 'bt-good))))))
    '(first min-length 2)))

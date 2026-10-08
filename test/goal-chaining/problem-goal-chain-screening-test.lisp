;;; Filename: problem-goal-chain-screening-test.lisp

;;; Sound tri-state screening for generic goal-chain checkpoints.  The relaxed model
;;; contains every concrete transition but removes deletes.  The first broad milestone
;;; endpoint is therefore provably unable to reach the original final goal, while the
;;; second endpoint remains possible only in the abstract and must be retained.

(in-package :ww)


(ww-set *problem-name* goal-chain-screening-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* first)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 2)


(define-types
  screening-stage (screen-start screen-a screen-c)
  screening-choice (screen-bad screen-good))


(define-dynamic-relations
  (screen-at screening-stage)
  (screen-choice screening-choice))


(define-init
  (screen-at screen-start))


;; The LIFO frontier selects the later bad action first.
(define-action choose-good-screen-a
  1 () (screen-at screen-start) ()
  (assert (not (screen-at screen-start))
          (screen-at screen-a)
          (screen-choice screen-good)))


(define-action choose-bad-screen-a
  1 () (screen-at screen-start) ()
  (assert (not (screen-at screen-start))
          (screen-at screen-a)
          (screen-choice screen-bad)))


(define-action finish-good-screen-route
  1 () (and (screen-at screen-a) (screen-choice screen-good)) ()
  (assert (not (screen-at screen-a)) (screen-at screen-c)))


(define-test-helper screening-state-facts (state)
  (list-database (problem-state.idb state)))


(define-test-helper build-complete-screening-relaxation (state goal)
  (declare (ignore goal))
  (let ((operators
          (list
            (make-relaxed-hmax-operator
              :name 'choose-good-screen-a
              :preconditions '((screen-at screen-start))
              :effects '((screen-at screen-a) (screen-choice screen-good)))
            (make-relaxed-hmax-operator
              :name 'choose-bad-screen-a
              :preconditions '((screen-at screen-start))
              :effects '((screen-at screen-a) (screen-choice screen-bad)))
            (make-relaxed-hmax-operator
              :name 'finish-good-screen-route
              :preconditions
                '((screen-at screen-a) (screen-choice screen-good))
              :effects '((screen-at screen-c))))))
    (make-relaxed-hmax-model
      :facts (screening-state-facts state)
      :operators operators
      :goals '((screen-at screen-c))
      :validated-p t
      :unreachability-complete-p t)))


(define-test-helper build-incomplete-screening-relaxation (state goal)
  (let ((model (build-complete-screening-relaxation state goal)))
    (setf (relaxed-hmax-model.unreachability-complete-p model) nil)
    model))


(define-test-helper unknown-screening-probe (state context)
  (declare (ignore state context))
  (make-candidate-screening-result
    :status :unknown :source :bounded-probe :reason :probe-inconclusive))


(register-relaxed-hmax-model-builder 'build-complete-screening-relaxation)
(register-candidate-state-screener
  'unknown-bounded-probe 'unknown-screening-probe :priority 100)


(define-test-helper screening-context ()
  (make-candidate-screening-context
    :final-goal '(screen-at screen-c)
    :final-goal-function (symbol-function 'goal-fn)))


(define-test-helper fabricated-screening-state (choice)
  (let ((state (copy-problem-state *start-state*)))
    (delete-proposition '(screen-at screen-start) (problem-state.idb state))
    (add-proposition '(screen-at screen-a) (problem-state.idb state))
    (add-proposition `(screen-choice ,choice) (problem-state.idb state))
    (invalidate-problem-state-hash state)
    state))


(define-test-helper call-with-isolated-screening-chain (thunk)
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
          (install-compiled-goal '(screen-at screen-c))
          (funcall thunk))
      (setf (symbol-function 'goal-fn) saved-goal-function
            (symbol-value 'goal-fn) saved-goal-value
            (get 'goal-fn :form) saved-goal-form
            *solution-paths* saved-solutions
            *solutions-valid* saved-valid))))


(define-test-claim screening-rejects-only-proven-checkpoint
  (call-with-isolated-screening-chain
    (lambda ()
      (solve-subgoal (screen-at screen-a))
      (and
        (member '(screen-choice screen-good)
                (screening-state-facts *start-state*) :test #'equal)
        (= (length (goal-chain-session-phases *goal-chain-session*)) 1)
        (= (length
             (goal-chain-session-screening-rejections
               *goal-chain-session*))
           1)
        (null (goal-chain-session-nogoods *goal-chain-session*))))))


(define-test-claim screening-tri-state-contract
  (let* ((bad-state (fabricated-screening-state 'screen-bad))
         (good-state (fabricated-screening-state 'screen-good))
         (goal-state (copy-problem-state *start-state*))
         (context (screening-context))
         (saved-builders *relaxed-hmax-model-builders*))
    (delete-proposition '(screen-at screen-start) (problem-state.idb goal-state))
    (add-proposition '(screen-at screen-c) (problem-state.idb goal-state))
    (invalidate-problem-state-hash goal-state)
    (unwind-protect
        (and
          (eq (candidate-screening-result-status
                (screen-candidate-state bad-state context))
              :impossible)
          (eq (candidate-screening-result-status
                (screen-candidate-state good-state context))
              :unknown)
          (eq (candidate-screening-result-status
                (screen-candidate-state goal-state context))
              :possible)
          (progn
            (setf *relaxed-hmax-model-builders*
                  '(build-incomplete-screening-relaxation))
            (eq (candidate-screening-result-status
                  (screen-candidate-state bad-state context))
                :unknown))
          (let ((inconsistent (copy-problem-state bad-state)))
            (add-proposition '(inconsistent-state)
                             (problem-state.idb inconsistent))
            (eq (candidate-screening-result-status
                  (screen-candidate-state inconsistent context))
                :impossible)))
      (setf *relaxed-hmax-model-builders* saved-builders))))


(define-goal
  (screen-at screen-c))

;;; Filename: problem-goal-chaining-test.lisp

;;; Focused characterization of generic goal chaining.  The ordinary problem path is two
;;; deterministic steps; synthetic alternatives exercise selection, snapshot independence,
;;; failure recovery, single-thread enforcement, and advisor continuation without recorder
;;; behavior.  Expected minimum path length: two.

(in-package :ww)


(ww-set *problem-name* goal-chaining-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 2)

(setf *expected-min-length* 2)


(define-types
  chain-stage (chain-start chain-middle chain-end chain-unreachable))


(define-dynamic-relations
  (chain-at chain-stage))


(define-init
  (chain-at chain-start))


(define-action advance-chain-to-middle
  1
  ()
  (chain-at chain-start)
  ()
  (assert (not (chain-at chain-start))
          (chain-at chain-middle)))


(define-action advance-chain-to-end
  1
  ()
  (chain-at chain-middle)
  ()
  (assert (not (chain-at chain-middle))
          (chain-at chain-end)))


(define-goal
  (chain-at chain-end))


(define-test-helper goal-chaining-state (stage &key (time 0) (value 0))
  "Return a copy of the staged start state with its sole CHAIN-AT fact replaced."
  (let ((state (copy-problem-state *start-state*)))
    (dolist (proposition (list-database (problem-state.idb state)))
      (when (eql (first proposition) 'chain-at)
        (delete-proposition proposition (problem-state.idb state))))
    (add-proposition `(chain-at ,stage) (problem-state.idb state))
    (setf (problem-state.time state) time
          (problem-state.value state) value)
    (invalidate-problem-state-hash state)
    state))


(define-test-helper goal-chaining-state-at-p (state stage)
  (member `(chain-at ,stage)
          (list-database (problem-state.idb state))
          :test #'equal))


(define-test-helper goal-chaining-solution (stage depth time value)
  (make-solution
    :depth depth
    :time time
    :value value
    :path (loop for index from 1 to depth collect (list index '(synthetic-action)))
    :goal (goal-chaining-state stage :time time :value value)))


(define-test-helper call-with-isolated-goal-chaining-session (thunk)
  "Run THUNK with isolated session globals and restore GOAL-FN's global definition."
  (let* ((missing (gensym "MISSING-GOAL-FORM"))
         (saved-goal-function-bound-p (boundp 'goal-fn))
         (saved-goal-function-value
           (and saved-goal-function-bound-p (symbol-value 'goal-fn)))
         (saved-goal-function-defined-p (fboundp 'goal-fn))
         (saved-goal-function
           (and saved-goal-function-defined-p (symbol-function 'goal-fn)))
         (saved-goal-form (get 'goal-fn :form missing))
         (saved-solutions *solution-paths*)
         (saved-solutions-valid *solutions-valid*))
    (unwind-protect
        (let ((*start-state* (copy-problem-state *start-state*))
              (*goal* (copy-tree *goal*))
              (*final-goal* nil)
              (*undo-stack* nil)
              (*threads* 0))
          (setf *solution-paths* nil
                *solutions-valid* nil)
          (funcall thunk))
      (setf *solution-paths* saved-solutions
            *solutions-valid* saved-solutions-valid)
      (if saved-goal-function-bound-p
        (setf (symbol-value 'goal-fn) saved-goal-function-value)
        (when (boundp 'goal-fn)
          (makunbound 'goal-fn)))
      (if saved-goal-function-defined-p
        (setf (symbol-function 'goal-fn) saved-goal-function)
        (when (fboundp 'goal-fn)
          (fmakunbound 'goal-fn)))
      (if (eq saved-goal-form missing)
        (remprop 'goal-fn :form)
        (setf (get 'goal-fn :form) saved-goal-form)))))


(define-test-claim goal-chaining-solution-selection
  (let* ((short (goal-chaining-solution 'chain-middle 2 10 5))
         (fast (goal-chaining-solution 'chain-middle 4 3 7))
         (low (goal-chaining-solution 'chain-middle 5 12 1))
         (high (goal-chaining-solution 'chain-middle 6 15 20))
         (solutions (list high low fast short)))
    (and
      (let ((*solution-type* 'first))
        (eq short (select-continuation-solution solutions)))
      (let ((*solution-type* 'min-length))
        (eq short (select-continuation-solution solutions)))
      (let ((*solution-type* 'every))
        (eq short (select-continuation-solution solutions)))
      (let ((*solution-type* 'all-paths))
        (eq short (select-continuation-solution solutions)))
      (let ((*solution-type* 4))
        (eq short (select-continuation-solution solutions)))
      (let ((*solution-type* 'min-time))
        (eq fast (select-continuation-solution solutions)))
      (let ((*solution-type* 'min-value))
        (eq low (select-continuation-solution solutions)))
      (let ((*solution-type* 'max-value))
        (eq high (select-continuation-solution solutions))))))


(define-test-claim goal-chaining-state-continuation
  (call-with-isolated-goal-chaining-session
    (lambda ()
      (let* ((old-start *start-state*)
             (goal-state (goal-chaining-state 'chain-middle :time 8 :value 13)))
        (setf (problem-state.happenings goal-state) '((clock (1 8 forward))))
        (update-start-state-from-goal goal-state)
        (and
          (not (eq *start-state* old-start))
          (not (eq *start-state* goal-state))
          (not (eq (problem-state.idb *start-state*)
                   (problem-state.idb goal-state)))
          (not (eq (problem-state.happenings *start-state*)
                   (problem-state.happenings goal-state)))
          (goal-chaining-state-at-p *start-state* 'chain-middle)
          (= (problem-state.time *start-state*) 8)
          (= (problem-state.value *start-state*) 13)
          (eql (problem-state.name *start-state*) 'continuation)
          (null (problem-state.instantiations *start-state*)))))))


(define-test-claim goal-chaining-undo-restores-session
  (call-with-isolated-goal-chaining-session
    (lambda ()
      (let* ((initial-state (goal-chaining-state 'chain-start :time 2 :value 3))
             (preceding-solution
               (goal-chaining-solution 'chain-middle 1 4 6)))
        (setf *start-state* initial-state
              *solution-paths* (list preceding-solution)
              *solutions-valid* t)
        (install-compiled-goal '(chain-at chain-end))
        (continue-from-solution '(chain-at chain-unreachable))
        ;; Mutate the live continuation after checkpointing; undo must not see this change.
        (setf (problem-state.time *start-state*) 99)
        (ww-undo)
        (and
          (goal-chaining-state-at-p *start-state* 'chain-start)
          (= (problem-state.time *start-state*) 2)
          (= (problem-state.value *start-state*) 3)
          (equal *goal* '(chain-at chain-end))
          (null *final-goal*)
          *solutions-valid*
          (= (length *solution-paths*) 1)
          (not (eq (first *solution-paths*) preceding-solution))
          (goal-chaining-state-at-p
            (solution.goal (first *solution-paths*)) 'chain-middle)
          (funcall (symbol-function 'goal-fn)
                   (goal-chaining-state 'chain-end))
          (not (funcall (symbol-function 'goal-fn)
                        (goal-chaining-state 'chain-unreachable))))))))


(define-test-claim goal-chaining-first-subgoal-undo
  (call-with-isolated-goal-chaining-session
    (lambda ()
      (install-compiled-goal '(chain-at chain-end))
      (continue-from-solution '(chain-at chain-middle))
      (ww-undo)
      (and (null *final-goal*)
           (equal *goal* '(chain-at chain-end))
           (null *solution-paths*)
           (not *solutions-valid*)))))


(define-test-claim goal-chaining-single-thread-enforcement
  (call-with-isolated-goal-chaining-session
    (lambda ()
      (install-compiled-goal '(chain-at chain-end))
      (let ((*threads* 1)
            (signaled nil))
        (handler-case
            (continue-from-solution '(chain-at chain-middle))
          (error ()
            (setf signaled t)))
        (and signaled
             (null *undo-stack*)
             (null *final-goal*)
             (equal *goal* '(chain-at chain-end))
             (goal-chaining-state-at-p *start-state* 'chain-start))))))


(define-test-claim goal-chaining-failure-and-retry
  (call-with-isolated-goal-chaining-session
    (lambda ()
      (let ((preceding-solution
              (goal-chaining-solution 'chain-middle 1 1 0)))
        (install-compiled-goal '(chain-at chain-end))
        (setf *solution-paths* (list preceding-solution)
              *solutions-valid* t)
        (solve-subgoal-form '(chain-at chain-unreachable))
        (let ((failed-cleanly
                (and (goal-chaining-state-at-p *start-state* 'chain-middle)
                     (null *solution-paths*)
                     (not *solutions-valid*)
                     (not (null *undo-stack*)))))
          (ww-undo)
          (and failed-cleanly
               (goal-chaining-state-at-p *start-state* 'chain-start)
               *solutions-valid*
               (= (length *solution-paths*) 1)
               (goal-chaining-state-at-p
                 (solution.goal (first *solution-paths*)) 'chain-middle)))))))


(define-test-claim goal-chaining-interruption-cleanup
  (call-with-isolated-goal-chaining-session
    (lambda ()
      (let ((saved-ww-solve (symbol-function 'ww-solve))
            (preceding-solution
              (goal-chaining-solution 'chain-middle 1 1 0))
            (signaled nil))
        (install-compiled-goal '(chain-at chain-end))
        (setf *solution-paths* (list preceding-solution)
              *solutions-valid* t)
        (unwind-protect
            (progn
              (setf (symbol-function 'ww-solve)
                    (lambda () (error "simulated interruption")))
              (handler-case
                  (solve-subgoal-form '(chain-at chain-unreachable))
                (error ()
                  (setf signaled t))))
          (setf (symbol-function 'ww-solve) saved-ww-solve))
        (let ((cleaned
                (and signaled
                     (null *solution-paths*)
                     (not *solutions-valid*)
                     (not (null *undo-stack*)))))
          (ww-undo)
          (and cleaned
               *solutions-valid*
               (= (length *solution-paths*) 1)
               (goal-chaining-state-at-p
                 (solution.goal (first *solution-paths*)) 'chain-middle)))))))


(define-test-claim goal-chaining-final-solve-lifecycle
  (call-with-isolated-goal-chaining-session
    (lambda ()
      (install-compiled-goal '(chain-at chain-middle))
      (setf *solution-paths*
            (list (goal-chaining-solution 'chain-middle 1 1 0))
            *solutions-valid* t
            *final-goal* '(chain-at chain-end))
      (solve)
      (and *solutions-valid*
           (null *final-goal*)
           (= (solution.depth (select-continuation-solution)) 1)
           (goal-chaining-state-at-p
             (solution.goal (select-continuation-solution)) 'chain-end)))))


(define-test-claim goal-chaining-advisor-continuation
  (call-with-isolated-goal-chaining-session
    (lambda ()
      (let ((strategy
              (make-strategy
                :name 'chain-characterization
                :parameters nil
                :applicability nil
                :phases '((chain-at chain-middle) (chain-at chain-end)))))
        (install-compiled-goal '(chain-at chain-end))
        (and (execute-strategy-phases strategy (strategy-phases strategy))
             *solutions-valid*
             (goal-chaining-state-at-p *start-state* 'chain-middle)
             (goal-chaining-state-at-p
               (solution.goal (select-continuation-solution)) 'chain-end))))))

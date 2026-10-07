;;; Focused regression checks; stages the small three-disk Hanoi problem.
(in-package :ww)

;; Staging reloads and replaces hook symbols. Read the tests only afterwards.
(stage hanoi)

(defun test-backtracking-prune ()
  (let ((*algorithm* 'backtracking)
        (*threads* 0)
        (*solution-type* 'first)
        (*depth-cutoff* 7)
        (saved-hook (and (fboundp 'prune-state?)
                         (symbol-function 'prune-state?)))
        (calls 0))
    (unwind-protect
        (progn
          ;; An absent hook retains ordinary search behavior.
          (fmakunbound 'prune-state?)
          (solve)
          (assert (= 7 (solution.depth (first *solution-paths*))))
          ;; A pruned root must generate no choices.
          (setf (symbol-function 'prune-state?)
                (lambda (state) (declare (ignore state)) (incf calls) t))
          (solve)
          (assert (= calls 1))
          (assert (null *solution-paths*))
          (assert (null *choice-stack*))
          ;; Visit both legal first moves, pruning each before its descendants.
          ;; This also checks that undo restores the state for the next sibling.
          (setf calls 0
                (symbol-function 'prune-state?)
                (lambda (state)
                  (incf calls)
                  (assert (<= (problem-state.time state) 1))
                  (plusp (problem-state.time state))))
          (solve)
          (assert (= calls 3))
          (assert (null *solution-paths*))
          (assert (null *choice-stack*))
          (assert (equalp (problem-state.idb *start-state*)
                          (problem-state.idb *backtrack-state*)))
          ;; A reached goal remains acceptable even when the hook would prune it.
          (setf (symbol-function 'prune-state?)
                (lambda (state) (funcall 'goal-fn state)))
          (solve)
          (assert (= 7 (solution.depth (first *solution-paths*))))
          (format t "~&BACKTRACKING-PRUNE-CHECKS-PASSED~%"))
      (if saved-hook
          (setf (symbol-function 'prune-state?) saved-hook)
          (fmakunbound 'prune-state?)))))


(defun test-backtracking-goal-chain-rejection ()
  "Check milestone rejection using Hanoi moves and a goal after every move."
  (let ((*algorithm* 'backtracking)
        (*threads* 0)
        (*tree-or-graph* 'tree)
        (*solution-type* 'first)
        (*depth-cutoff* 2)
        (*goal-chain-candidate-rejector* nil)
        (saved-goal (symbol-function 'goal-fn))
        (rejected-path nil)
        (calls 0))
    (unwind-protect
        (progn
          (setf (symbol-function 'goal-fn)
                (lambda (state) (plusp (problem-state.time state))))
          ;; Without a rejector the first reached goal is accepted.
          (solve)
          (assert (= 1 (solution.depth (first *solution-paths*))))
          ;; Reject every reached endpoint, including those at the depth cutoff.
          (setf *goal-chain-candidate-rejector*
                (lambda (path state)
                  (assert (= (length path) (problem-state.time state)))
                  (incf calls)
                  t))
          (solve)
          (assert (plusp calls))
          (assert (null *solution-paths*))
          ;; Reject a first-move goal but accept its own next-move descendant.
          (setf calls 0
                *goal-chain-candidate-rejector*
                (lambda (path state)
                  (assert (= (length path) (problem-state.time state)))
                  (incf calls)
                  (when (= (length path) 1)
                    (setf rejected-path (copy-tree path))
                    t)))
          (solve)
          (assert (= calls 2))
          (assert (= 2 (solution.depth (first *solution-paths*))))
          (assert (equal rejected-path
                         (subseq (solution.path (first *solution-paths*)) 0 1)))
          (assert (null *choice-stack*))
          (assert (equalp (problem-state.idb *start-state*)
                          (problem-state.idb *backtrack-state*)))
          (format t "~&BACKTRACKING-GOAL-CHAIN-REJECTION-CHECKS-PASSED~%"))
      (setf (symbol-function 'goal-fn) saved-goal))))


(defun test-backtracking-min-steps-cutoff ()
  "Use the known seven-move Hanoi optimum as a sound remaining-move bound."
  (let ((*algorithm* 'backtracking)
        (*threads* 0)
        (*solution-type* 'first)
        (*depth-cutoff* 6)
        (*min-steps-pruning-enabled* t)
        (*min-steps-remaining-contributors* nil)
        (saved-hook (and (fboundp 'min-steps-remaining?)
                         (symbol-function 'min-steps-remaining?)))
        (contributor (gensym "BT-MOVE-BOUND-"))
        (calls 0))
    (unwind-protect
        (progn
          (setf (symbol-function 'min-steps-remaining?)
                (lambda (state)
                  (incf calls)
                  (max 0 (- 7 (problem-state.time state)))))
          (solve)
          (assert (= calls 1))
          (assert (= *lower-bound-pruned* 1))
          (assert (null *solution-paths*))
          ;; A contributor alone must work, with no aggregate query defined.
          (setf (symbol-function contributor)
                (symbol-function 'min-steps-remaining?))
          (fmakunbound 'min-steps-remaining?)
          (register-min-steps-remaining-contributor contributor)
          (solve)
          (assert (= *min-steps-contributor-prunes* 1))
          (assert (= *lower-bound-pruned* 1))
          (assert (null *solution-paths*))
          ;; A nonpruning contributor must fall through to the aggregate query.
          (setf (symbol-function 'min-steps-remaining?)
                (symbol-function contributor)
                (symbol-function contributor)
                (lambda (state) (declare (ignore state)) 0))
          (solve)
          (assert (zerop *min-steps-contributor-prunes*))
          (assert (= *min-steps-fallback-unique-prunes* 1))
          (assert (= *lower-bound-pruned* 1))
          (assert (null *solution-paths*))
          (setf (symbol-function contributor)
                (symbol-function 'min-steps-remaining?))
          ;; Let the root through; both first-move branches still exceed the cutoff.
          (setf *min-steps-remaining-contributors* nil
                (symbol-function 'min-steps-remaining?)
                (lambda (state)
                  (if (zerop (problem-state.time state))
                      0
                      (max 0 (- 7 (problem-state.time state))))))
          (solve)
          (assert (= *lower-bound-pruned* 2))
          (assert (null *choice-stack*))
          (assert (equalp (problem-state.idb *start-state*)
                          (problem-state.idb *backtrack-state*)))
          ;; Equality with the cutoff must preserve the seven-move solution.
          (setf *depth-cutoff* 7)
          (solve)
          (assert (= 7 (solution.depth (first *solution-paths*))))
          ;; The comparison switch must bypass both contributor and query calls.
          (setf calls 0
                *min-steps-pruning-enabled* nil
                (symbol-function 'min-steps-remaining?)
                (symbol-function contributor))
          (register-min-steps-remaining-contributor contributor)
          (solve)
          (assert (zerop calls))
          (assert (zerop *lower-bound-pruned*))
          (assert (= 7 (solution.depth (first *solution-paths*)))))
      (fmakunbound contributor)
      (if saved-hook
          (setf (symbol-function 'min-steps-remaining?) saved-hook)
          (fmakunbound 'min-steps-remaining?)))))


(defun test-backtracking-min-steps-incumbent ()
  "A goal after two moves tests incumbent pruning without a depth cutoff."
  (let ((*algorithm* 'backtracking)
        (*threads* 0)
        (*solution-type* 'min-length)
        (*depth-cutoff* 0)
        (*min-steps-pruning-enabled* t)
        (*min-steps-remaining-contributors* nil)
        (saved-goal (symbol-function 'goal-fn))
        (saved-hook (and (fboundp 'min-steps-remaining?)
                         (symbol-function 'min-steps-remaining?)))
        (calls 0))
    (unwind-protect
        (progn
          (setf (symbol-function 'goal-fn)
                (lambda (state) (>= (problem-state.time state) 2))
                (symbol-function 'min-steps-remaining?)
                (lambda (state)
                  (assert *solution-paths*)
                  (incf calls)
                  (max 0 (- 2 (problem-state.time state)))))
          (solve)
          (assert (plusp calls))
          (assert (plusp *lower-bound-pruned*))
          (assert (every (lambda (solution) (= (solution.depth solution) 2))
                         *solution-paths*))
          (assert *solution-paths*))
      (setf (symbol-function 'goal-fn) saved-goal)
      (if saved-hook
          (setf (symbol-function 'min-steps-remaining?) saved-hook)
          (fmakunbound 'min-steps-remaining?)))))


(defun test-backtracking-min-steps ()
  (test-backtracking-min-steps-cutoff)
  (test-backtracking-min-steps-incumbent)
  (format t "~&BACKTRACKING-MIN-STEPS-CHECKS-PASSED~%"))


(defun test-backtracking-bounding-case (mode algorithm)
  "Exercise the shared bound contract with synthetic positive and negated costs."
  (let ((*algorithm* algorithm)
        (*threads* 0)
        (*tree-or-graph* 'tree)
        (*solution-type* 'first)
        (*depth-cutoff* 7)
        (saved-goal (symbol-function 'goal-fn))
        (saved-hook (and (fboundp 'bounding-function?)
                         (symbol-function 'bounding-function?)))
        (calls 0))
    (unwind-protect
        (progn
          (setf (symbol-function 'bounding-function?)
                (lambda (state)
                  (incf calls)
                  (ecase mode
                    (:root (values 1000001 1000001))
                    (:branches
                     (if (zerop (problem-state.time state))
                         (values 0 5)
                         (values 6 6)))
                    (:negative
                     (if (zerop (problem-state.time state))
                         (values -20 -10)
                         (values -9 -9)))
                    (:equal (values 5 5))
                    (:goal
                     (assert (zerop (problem-state.time state)))
                     (values 0 5)))))
          (when (eq mode :goal)
            (setf (symbol-function 'goal-fn)
                  (lambda (state) (plusp (problem-state.time state)))))
          (solve)
          (ecase mode
            (:root
             (assert (= calls 1))
             (assert (= *bounding-pruned* 1))
             (assert (null *solution-paths*)))
            ((:branches :negative)
             (assert (= calls 3))
             (assert (= *bounding-pruned* 2))
             (assert (= *upper-bound* (if (eq mode :negative) -10 5)))
             (assert (null *solution-paths*)))
            (:equal
             (assert (plusp calls))
             (assert (= *upper-bound* 5))
             (assert (zerop *bounding-pruned*))
             (assert (= 7 (solution.depth (first *solution-paths*)))))
            (:goal
             (assert (= calls 1))
             (assert (= 1 (solution.depth (first *solution-paths*))))))
          (when (eq algorithm 'backtracking)
            (assert (null *choice-stack*))
            (assert (equalp (problem-state.idb *start-state*)
                            (problem-state.idb *backtrack-state*)))))
      (setf (symbol-function 'goal-fn) saved-goal)
      (if saved-hook
          (setf (symbol-function 'bounding-function?) saved-hook)
          (fmakunbound 'bounding-function?)))))


(defun test-backtracking-bounding ()
  (dolist (algorithm '(backtracking depth-first))
    (dolist (mode '(:root :branches :negative :equal :goal))
      (test-backtracking-bounding-case mode algorithm)))
  (format t "~&BACKTRACKING-BOUNDING-CHECKS-PASSED~%"))

;;; Exercises the coordinator's real task generator without launching workers.
(in-package :ww)
(stage hanoi)

(defun test-parallel-root-invariant-case (mode)
  (let ((*algorithm* 'depth-first)
        (*threads* 0)
        (*tree-or-graph* 'tree)
        (*solution-type* 'every)
        (*depth-cutoff* 2)
        (*split-depth-max* 1)
        (*min-tasks* 8)
        (saved-invariants *global-invariants*)
        (saved-goal (symbol-function 'goal-fn))
        (invariant (gensym "ROOT-TASK-TEST-INVARIANT-"))
        (checks 0)
        (failures 0)
        (tasks nil))
    (unwind-protect
        (progn
          (setf *solution-paths* nil
                *unique-solution-states* nil
                *troubleshoot-current-node* nil
                *global-invariants* (list invariant)
                (symbol-function invariant)
                (lambda (state)
                  (incf checks)
                  (case mode
                    (:error nil)
                    (:all-valid t)
                    (otherwise
                     (not (member '(loc disk1 peg2)
                                  (list-database (problem-state.idb state))
                                  :test #'equal)))))
                (symbol-function 'goal-fn)
                (lambda (state)
                  (declare (ignore state))
                  (member mode '(:error :goals))))
          (handler-case
              (handler-bind
                  ((error
                     (lambda (condition)
                       ;; Only continue the intended invariant diagnostic.
                       (when (and (not (eq mode :error))
                                  (search (symbol-name invariant)
                                          (princ-to-string condition))
                                  (find-restart 'continue condition))
                         (incf failures)
                         (invoke-restart 'continue)))))
                (setf tasks
                      (generate-root-tasks
                        (make-node :state (copy-problem-state *start-state*)
                                   :depth 0))))
            (error (condition)
              (unless (and (eq mode :error)
                           (search (symbol-name invariant)
                                   (princ-to-string condition)))
                (error condition))
              (incf failures)))
          (ecase mode
            (:error
             (assert (= checks 1))
             (assert (= failures 1))
             (assert (null tasks))
             (assert (null *solution-paths*)))
            (:all-valid
             (assert (= checks 2))
             (assert (zerop failures))
             (assert (= (length tasks) 2)))
            ((:tasks :goals)
             (assert (= checks 2))
             (assert (= failures 1))
             (let ((survivors (if (eq mode :tasks)
                                 (mapcar #'node.state tasks)
                                 (mapcar #'solution.goal *solution-paths*))))
               (assert (= (length survivors) 1))
               (assert (member '(loc disk1 peg3)
                               (list-database (problem-state.idb (first survivors)))
                               :test #'equal)))
             (if (eq mode :tasks)
                 (assert (null *solution-paths*))
                 (assert (null tasks)))))
          t)
      (setf *global-invariants* saved-invariants
            (symbol-function 'goal-fn) saved-goal
            *troubleshoot-current-node* nil)
      (fmakunbound invariant))))

(defun test-parallel-root-invariants ()
  (dolist (mode '(:error :all-valid :tasks :goals))
    (test-parallel-root-invariant-case mode))
  (format t "~&PARALLEL-ROOT-INVARIANT-CHECKS-PASSED~%"))

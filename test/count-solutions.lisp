;;; Load after Wouldwork, then call (ww::test-count-solutions).
;;; Uses the tiny COUNT-GOALS fixture; leaves it staged.
(in-package :ww)

(defvar *count-test-goal-depth*)
(defvar *count-test-early-goal*)

(defun count-test-validator (start path goal-state)
  (declare (ignore start path))
  (funcall 'count-test-even? goal-state))

(defun check-count-run (expected &key (goal-depth 4) early validator (cutoff 0))
  (setf *count-test-goal-depth* goal-depth
        *count-test-early-goal* early
        *solution-validators* (when validator '(count-test-validator))
        *depth-cutoff* cutoff)
  (let ((output (make-string-output-stream)))
    (let ((*standard-output* output)) (solve))
    (let ((report (get-output-stream-string output)))
      (assert (search (format nil "Total accepted goals counted = ~:D" expected) report))
      (assert (eq (not (null (search "One accepted solution" report)))
                  (plusp expected)))))
  (assert (= expected *solution-count*))
  (assert (null *solution-paths*))
  (assert (null *unique-solution-states*))
  (assert (eq (not (null *count-example*)) (plusp expected)))
  (when *count-example*
    (assert (goal (solution.goal *count-example*)))
    (assert (= (solution.depth *count-example*)
               (length (solution.path *count-example*))))
    (when (zerop goal-depth) (assert (null (solution.path *count-example*))))
    (when validator
      (assert (count-test-validator nil (solution.path *count-example*)
                                    (solution.goal *count-example*)))))
  (when validator
    (assert (= expected *accepted-solution-candidates*)))
  (format t "~&COUNT PASS: ~A, ~D threads, split ~D, expected ~D~%"
          *algorithm* *threads* *split-depth-max* expected))

(defun check-count-cases ()
  (check-count-run 16)
  (check-count-run 16) ; reset, not cumulative
  (check-count-run 0 :goal-depth 5)
  (check-count-run 0 :cutoff 2)
  (check-count-run 1 :goal-depth 0)
  (check-count-run 8 :validator t)
  (check-count-run 9 :early t))

(defun test-count-solutions ()
  (stage count-goals)
  (ww-set *threads* 0)
  (check-count-cases)
  (ww-set *solution-type* every)
  (setf *count-test-early-goal* nil)
  (let ((*standard-output* (make-broadcast-stream))) (solve))
  (assert (= 16 (length *solution-paths*)))
  (assert (null *count-example*))
  (ww-set *solution-type* count)
  (ww-set *tree-or-graph* graph)
  (check-count-run 16)
  (ww-set *tree-or-graph* tree)
  (ww-set *threads* 4)
  (setf *split-depth-max* 20)
  (check-count-cases) ; all goals during root-task generation
  (setf *split-depth-max* 1)
  (check-count-cases) ; worker goals and mixed root/worker goals
  (assert (= 8 (loop for stats across *worker-stats-vector*
                    sum (ws-solutions-found stats))))
  (ww-set *threads* 0)
  (ww-set *algorithm* backtracking)
  (check-count-cases)
  (format t "~&COUNT REGRESSIONS PASSED~%")
  t)

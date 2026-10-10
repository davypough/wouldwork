;;; Load after Wouldwork, then call (ww::test-parallel-statistics).
;;; Uses the finite binary COUNT-GOALS tree; leaves it staged.
(in-package :ww)

(defvar *count-test-goal-depth*)
(defvar *count-test-early-goal*)

(defun check-parallel-statistics-run (split-depth goal-depth solution-type
                                      expected-cycles expected-states expected-depth
                                      expected-dead-ends expected-goals)
  (setf *split-depth-max* split-depth
        *count-test-goal-depth* goal-depth
        *count-test-early-goal* nil
        *solution-type* solution-type)
  (let ((output (make-string-output-stream)))
    (let ((*standard-output* output)) (solve))
    (let ((report (get-output-stream-string output)))
      (assert (search (format nil "Program cycles = ~:D" expected-cycles) report))
      (assert (search (format nil "Maximum depth explored = ~:D" expected-depth) report))
      (assert (eq (not (null (search "Average branching factor =" report)))
                  (plusp expected-cycles)))))
  (assert (= expected-cycles *program-cycles*))
  (assert (= expected-states *total-states-processed*))
  (assert (= expected-depth *max-depth-explored*))
  (assert (= expected-dead-ends *dead-end-num-paths*))
  (assert (= (* 4 expected-dead-ends) *dead-end-accumulated-depths*))
  (assert (zerop *repeated-states*))
  (assert (zerop *duplicate-num-paths*))
  (assert (= expected-goals
             (if (eq solution-type 'count)
                 *solution-count*
                 (length *solution-paths*))))
  (when *count-example*
    (assert (= 4 (solution.depth *count-example*))))
  (when *solution-paths*
    (assert (= goal-depth (solution.depth (first *solution-paths*)))))
  (let ((worker-cycles (loop for stats across *worker-stats-vector*
                             sum (ws-program-cycles stats)))
        (worker-states (loop for stats across *worker-stats-vector*
                             sum (ws-states-processed stats))))
    (if (= split-depth 1)
        (progn
          ;; Root expansion contributes one cycle and two successors.
          (assert (plusp worker-cycles))
          (assert (= expected-cycles (1+ worker-cycles)))
          (assert (= expected-states (+ 3 worker-states))))
        (progn
          (assert (zerop worker-cycles))
          (assert (zerop worker-states)))))
  (format t "~&PARALLEL STATISTICS PASS: split ~D, goal depth ~D, ~A~%"
          split-depth goal-depth solution-type))

(defun test-parallel-statistics ()
  (stage count-goals)
  (ww-set *algorithm* depth-first)
  (ww-set *threads* 16)
  (ww-set *depth-cutoff* 0)
  (ww-set *symmetry-pruning* nil)
  (ww-set *randomize-search* nil)
  ;; Known binary tree: 15 internal nodes, 16 leaves, 30 edges.
  (check-parallel-statistics-run 20 4 'count 15 31 4 0 16)
  (check-parallel-statistics-run 20 4 'count 15 31 4 0 16) ; reset
  ;; An unreachable goal forces expansion of all 16 terminal leaves.
  (check-parallel-statistics-run 20 5 'count 31 31 4 16 0)
  ;; Identical totals when only the root is expanded before workers start.
  (check-parallel-statistics-run 1 4 'count 15 31 4 0 16)
  (check-parallel-statistics-run 1 5 'count 31 31 4 16 0)
  ;; Exiting inside successor handling must retain the root expansion's counts.
  (check-parallel-statistics-run 20 1 'first 1 3 1 0 1)
  ;; A start-state goal requires no expansion.
  (check-parallel-statistics-run 20 0 'first 0 1 0 0 1)
  (format t "~&PARALLEL STATISTICS CHECKS PASSED~%")
  t)

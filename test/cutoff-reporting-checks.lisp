;;; Run after loading Wouldwork, in an isolated WOULDWORK_INSTANCE.
(in-package :ww)
(defvar *cutoff-reporting-check-count* 0)

(defun cutoff-reporting-check (condition)
  (assert condition)
  (incf *cutoff-reporting-check-count*))

(defun check-cutoff-reporting-run (cutoff truncated hits)
  (setf *depth-cutoff* cutoff)
  (solve)
  (cutoff-reporting-check (eq :exhausted-no-solution (search-outcome-status *last-search-outcome*)))
  (cutoff-reporting-check (eq (if truncated :depth-cutoff-truncated :complete)
                              (search-outcome-reason *last-search-outcome*)))
  (cutoff-reporting-check (eql truncated *depth-cutoff-truncated*))
  (cutoff-reporting-check (= hits *depth-cutoff-hits*))
  (cutoff-reporting-check (not *solutions-valid*))
  (cutoff-reporting-check (null *solution-paths*))
  (when (plusp *threads*)
    (cutoff-reporting-check
      (= hits (loop for stats across *worker-stats-vector*
                    sum (ws-depth-cutoff-hits stats))))
    (cutoff-reporting-check
      (eql truncated (loop for stats across *worker-stats-vector*
                            thereis (ws-depth-cutoff-truncated stats))))))

(setf *cutoff-reporting-check-count* 0)
(stage cutoff-reporting-test)
(ww-set *threads* 0)
(check-cutoff-reporting-run 1 t 1)
(check-cutoff-reporting-run 2 nil 1)
(check-cutoff-reporting-run 3 nil 0)
(ww-set *threads* 16)
(ww-set *split-depth-max* 1)
(check-cutoff-reporting-run 1 t 1)
(check-cutoff-reporting-run 2 nil 1)
(check-cutoff-reporting-run 3 nil 0)
;; Repeat after a nontruncated run to exercise reset and subsequent aggregation.
(check-cutoff-reporting-run 1 t 1)
(setf *depth-cutoff* 1)
(solve-subgoal ((cutoff-at cutoff-start)) (cutoff-at cutoff-end))
(cutoff-reporting-check (not *solutions-valid*))
(cutoff-reporting-check (null *solution-paths*))
(cutoff-reporting-check *depth-cutoff-truncated*)
(setf *depth-cutoff* 2)
(solve-subgoal ((cutoff-at cutoff-start)) (cutoff-at cutoff-end))
(cutoff-reporting-check *solutions-valid*)
(cutoff-reporting-check (= 2 (solution.depth (first *solution-paths*))))
(format t "~&CUTOFF-REPORTING-CHECKS PASSED: ~D assertions.~%"
        *cutoff-reporting-check-count*)

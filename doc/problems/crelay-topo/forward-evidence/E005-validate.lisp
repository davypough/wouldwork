;;; Run in the same REPL after the C005 -> box pickup search, before another search/stage.
(in-package :ww)

(defun forward-e005-candidate (solution index origin parent prefix)
  (let* ((segment (validate-action-sequence parent (solution.path solution)))
         (actions (append (copy-tree prefix) (copy-tree (solution.path solution))))
         (replay (validate-action-sequence origin actions
                   :goal-test (symbol-function 'goal-fn)))
         (signature (make-subgoal-progress-state-signature (solution.goal solution))))
    (assert (action-sequence-validation-success-p segment))
    (assert (equalp signature
                   (make-subgoal-progress-state-signature
                     (action-sequence-validation-final-state segment))))
    (assert (action-sequence-validation-success-p replay))
    (assert (action-sequence-validation-goal-satisfied-p replay))
    (assert (equalp signature
                   (make-subgoal-progress-state-signature
                     (action-sequence-validation-final-state replay))))
    (multiple-value-bind (valid diagnostic)
        (validate-recorder-solution origin actions (solution.goal solution))
      (format t "~&E005-CANDIDATE-~D RECORDER=~S ~S~%" index valid diagnostic)
      (assert valid () "Candidate ~D failed cumulative recorder validation: ~S"
              index diagnostic))
    (format t "~&E005-CANDIDATE-~D SEGMENT=~S~%" index (solution.path solution))
    (format t "~&E005-CANDIDATE-~D ENDPOINT=~S~%" index signature)
    (list :candidate index :parent 'c005 :actions actions :endpoint signature)))

(defun forward-e005-validate ()
  "Certify and save all current candidates; performs no search."
  (assert (eq *problem-name* 'crelay-topo))
  (assert (= *threads* 16))
  (assert *solutions-valid*)
  (assert *solution-paths*)
  (assert (equal *goal* '(holding agent1 box1)))
  (assert (equalp (make-subgoal-progress-state-signature *start-state*)
                  (getf *crelay-box-checkpoint* :endpoint)))
  (assert (equalp (make-subgoal-progress-state-signature *crelay-box-start*)
                  (getf *crelay-box-checkpoint* :endpoint)))
  (with-open-file (log "doc/problems/crelay-topo/forward-evidence/E005-validation.txt"
                       :direction :output :if-exists :error :if-does-not-exist :create)
    (let ((*standard-output* (make-broadcast-stream *standard-output* log)))
      (format t "~&E005 candidates=~D; cumulative validation, no search.~%"
              (length *solution-paths*))
      (let ((records
              (loop for solution in *solution-paths* for index from 1
                    collect (forward-e005-candidate
                              solution index *crelay-box-origin* *crelay-box-start*
                              (getf *crelay-box-checkpoint* :actions)))))
        (with-open-file (stream "doc/problems/crelay-topo/forward-evidence/E005-candidates.sexp"
                                :direction :output :if-exists :error
                                :if-does-not-exist :create)
          (let ((*print-readably* t) (*print-pretty* t) (*package* (find-package :ww)))
            (write (list :problem 'crelay-topo :experiment 'e005
                         :goal '(holding agent1 box1)
                         :origin (make-subgoal-progress-state-signature *crelay-box-origin*)
                         :candidates records) :stream stream)
            (terpri stream)))
        (format t "~&E005-ACCEPTED-CANDIDATES=~D; saved; original final goal not checked.~%"
                (length records))
        (format t "~&E005: equal endpoint signatures do not by themselves merge recorder histories.~%")
        :validated))))

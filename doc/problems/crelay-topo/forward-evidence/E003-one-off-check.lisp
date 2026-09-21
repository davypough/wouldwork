;;; Load E001-interface-check.lisp first. One depth-2 search, configured threads 16.
(in-package :ww)

(defun forward-e003-validate-result (origin actions solution)
  "Check the segment and cumulative recorder path and print durable evidence."
  (let* ((path (append actions (copy-tree (solution.path solution))))
         (replay (validate-action-sequence origin path
                    :goal-test (symbol-function 'goal-fn))))
    (assert (action-sequence-validation-success-p replay))
    (assert (action-sequence-validation-goal-satisfied-p replay))
    (assert (equalp
             (make-subgoal-progress-state-signature (solution.goal solution))
             (make-subgoal-progress-state-signature
               (action-sequence-validation-final-state replay))))
    (multiple-value-bind (valid diagnostic)
        (validate-recorder-solution origin path (solution.goal solution))
      (format t "~&E003-CUMULATIVE-RECORDER=~S ~S~%" valid diagnostic)
      (assert valid () "E003 cumulative recorder rejected: ~S" diagnostic))
    (format t "~&E003-SEGMENT-ACTIONS=~S~%" (solution.path solution))
    (format t "~&E003-CUMULATIVE-ACTIONS=~S~%" path)
    (format t "~&E003-ENDPOINT=~S~%"
            (make-subgoal-progress-state-signature (solution.goal solution)))
    (format t "~&E003-ACCEPTED-PREFIX actions=~D; not a final solution.~%" (length path))))

(defun forward-e003-run ()
  (assert (eq *problem-name* 'crelay-topo))
  (assert (= *threads* 16))
  (assert (= *depth-cutoff* 2))
  (assert (eq *solution-type* 'first))
  (let* ((archive (forward-e001-read-archive))
         (origin (copy-problem-state *start-state*))
         (checkpoint
           (with-open-file (stream "doc/problems/crelay-topo/forward-evidence/C003-checkpoint.sexp")
             (let ((*read-eval* nil) (*package* (find-package :ww)))
               (read stream))))
         (actions (copy-tree (getf checkpoint :actions)))
         (replay (validate-action-sequence origin actions)))
    (assert (equalp (make-subgoal-progress-state-signature origin)
                    (getf archive :origin)) () "Stage CRELAY-TOPO afresh first.")
    (assert (action-sequence-validation-success-p replay))
    (let ((parent (action-sequence-validation-final-state replay)))
      (assert (equalp (make-subgoal-progress-state-signature parent)
                      (getf checkpoint :endpoint)))
      (assert (= (length actions) 15))
      (install-compiled-goal '(holding agent1* tray1*))
      (assert (funcall (symbol-function 'goal-fn) parent))
      (multiple-value-bind (valid diagnostic)
          (validate-recorder-solution origin actions parent)
        (format t "~&E003-PARENT-RECORDER=~S ~S~%" valid diagnostic)
        (assert valid))
      (format t "~&E003 threads=~D cutoff=~D mode=~S~%" *threads* *depth-cutoff* *solution-type*)
      (format t "~&E003-EXPECTED-START=~S~%" (make-subgoal-progress-state-signature parent))
      (solve-subgoal parent
        (and (holding agent1 connector1) (holding agent1* tray1*)))
      (assert (equalp (make-subgoal-progress-state-signature *start-state*)
                      (make-subgoal-progress-state-signature parent)))
      (format t "~&E003-START-IDENTITY=PASS~%")
      (format t "~&E003-OUTCOME=~S REASON=~S~%"
              (search-outcome-status *last-search-outcome*)
              (search-outcome-reason *last-search-outcome*))
      (if *solutions-valid*
          (let* ((solution (select-continuation-solution))
                 (segment (validate-action-sequence parent (solution.path solution))))
            (assert (action-sequence-validation-success-p segment))
            (assert (equalp (make-subgoal-progress-state-signature (solution.goal solution))
                            (make-subgoal-progress-state-signature
                              (action-sequence-validation-final-state segment))))
            (forward-e003-validate-result origin actions solution))
          (format t "~&E003: no accepted witness; review exact outcome before drawing conclusions.~%")))))

(defun forward-e003 (&optional (output "doc/problems/crelay-topo/forward-evidence/E003-output.txt"))
  "One bounded search; preserve output and refuse to overwrite an earlier run."
  (with-open-file (stream output :direction :output :if-exists :error
                         :if-does-not-exist :create)
    (let ((*standard-output* (make-broadcast-stream *standard-output* stream)))
      (forward-e003-run))))

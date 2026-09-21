;;; Replay-only qualification. Load after staging CRELAY-TOPO with threads = 16.
;;; No solving, serial chaining, validator disabling, or invented start facts.
(in-package :ww)

(defun forward-e001-read-archive ()
  (with-open-file (stream "doc/problems/crelay-topo/test13-progress.txt")
    (let* ((*read-eval* nil)
           (*package* (find-package :ww))
           (record (read stream)))
      (assert (eq (first record) :wouldwork-subgoal-progress))
      (assert (eq (getf (rest record) :problem) 'crelay-topo))
      (assert (= (getf (rest record) :version) 1))
      (rest record))))

(defun forward-e001-check-return (origin)
  "Exercise partial success, goal success and illegal-action failure."
  (let ((*start-state* origin)
        (saved-goal (copy-tree *goal*)))
    (unwind-protect
         (progn
           (install-compiled-goal '(has-location agent1 location19))
           (assert (not (funcall (symbol-function 'goal-fn) origin)))
           (assert (problem-state-p (validate-solution)))
           (install-compiled-goal '(has-location agent1 location1))
           (assert (problem-state-p (validate-solution)))
           (format t "~&E001: the next unknown-action diagnostic is intentional.~%")
           (assert (null (validate-solution (forward-e001-nonexistent-action))))
           (format t "~&E001-PARTIAL/FULL/FAILURE-RETURNS=PASS~%"))
      (install-compiled-goal saved-goal))))

(defun forward-e001-prefix-states (origin actions)
  "Reconstruct every boundary independently from the original state."
  (let ((states (make-array (1+ (length actions)))))
    (setf (aref states 0) origin)
    (loop for end from 1 to (length actions)
          for result = (validate-action-sequence origin (subseq actions 0 end))
          do (assert (action-sequence-validation-success-p result) ()
                     "E001 replay failed at prefix ~D: ~S" end
                     (action-sequence-validation-failure-reason result))
             (setf (aref states end)
                   (action-sequence-validation-final-state result)))
    states))

(defun forward-e001-validation-result (validator start path endpoint)
  (multiple-value-bind (valid diagnostic) (funcall validator start path endpoint)
    (list valid diagnostic)))

(defun forward-e001-compare (validator origin parent whole suffix endpoint split end)
  "Report acceptance disagreement; different failure wording alone is not one."
  (let ((cumulative (forward-e001-validation-result validator origin whole endpoint))
        (local (forward-e001-validation-result validator parent suffix endpoint)))
    (unless (eq (not (null (first cumulative))) (not (null (first local))))
      (format t "~&E001-DISAGREEMENT validator=~S split=~D end=~D~%  cumulative=~S~%  local=~S~%"
              validator split end cumulative local)
      t)))

(defun forward-e001-check-splits (origin actions states)
  "Compare candidate and recorder-prefix checks at every open-cycle split."
  (let ((checks 0) (differences 0) (size (length actions)))
    (loop for split from 1 below size
          for parent = (aref states split)
          when (recorder-state-recording-open-p parent)
          do (let* ((suffix (subseq actions split))
                    (replay (validate-action-sequence parent suffix)))
               (assert (action-sequence-validation-success-p replay))
               (assert (equalp
                        (make-subgoal-progress-state-signature (aref states size))
                        (make-subgoal-progress-state-signature
                         (action-sequence-validation-final-state replay))))
               (incf checks)
               (when (forward-e001-compare
                      #'validate-recorder-solution origin parent actions suffix
                      (aref states size) split size)
                 (incf differences)))
             (loop for end from (1+ split) to size
                   for whole = (subseq actions 0 end)
                   for suffix = (subseq actions split end)
                   for endpoint = (aref states end)
                   do (incf checks)
                      (when (forward-e001-compare
                             #'validate-recorder-recording-prefix origin parent
                             whole suffix endpoint split end)
                        (incf differences))
                      (when (recorder-cycle-ending-action-p
                             (recorder-move-action-name (car (last whole))))
                        (incf checks)
                        (when (forward-e001-compare
                               #'validate-recorder-cycle-boundary-prefix
                               origin parent whole suffix endpoint split end)
                          (incf differences)))))
    (format t "~&E001-COMPARISONS=~D DISAGREEMENTS=~D~%" checks differences)
    (format t "~&E001: agreement on this prefix is not a general equivalence proof.~%")
    differences))

(defun forward-e001-run ()
  (assert (eq *problem-name* 'crelay-topo))
  (assert (= *threads* 16))
  (assert (null *goal-chain-session*))
  (let* ((archive (forward-e001-read-archive))
         (origin (copy-problem-state *start-state*))
         (checkpoints (getf archive :checkpoints))
         (actions (loop for checkpoint in checkpoints
                        append (copy-tree (getf checkpoint :actions))))
         (saved-goal (copy-tree *goal*)))
    (assert (= (length actions) 14))
    (assert (equalp (make-subgoal-progress-state-signature origin)
                    (getf archive :origin)) ()
            "Stage CRELAY-TOPO afresh; current origin differs from the archive.")
    (format t "~&E001 threads=~D cutoff=~D actions=~D; replay only.~%"
            *threads* *depth-cutoff* (length actions))
    (forward-e001-check-return origin)
    (unwind-protect
         (let ((states (forward-e001-prefix-states origin actions))
               (end 0))
           (dolist (checkpoint checkpoints)
             (incf end (length (getf checkpoint :actions)))
             (assert (equalp (make-subgoal-progress-state-signature (aref states end))
                             (getf checkpoint :endpoint)) ()
                     "Current replay differs from archived endpoint ~D." end))
           (install-compiled-goal (getf (car (last checkpoints)) :goal))
           (multiple-value-bind (valid diagnostic)
               (validate-recorder-solution origin actions (aref states (length actions)))
             (format t "~&E001-CUMULATIVE-RECORDER=~S ~S~%" valid diagnostic)
             (assert valid () "Cumulative recorder baseline rejected: ~S" diagnostic))
           (forward-e001-check-splits origin actions states)
           (format t "~&E001-COMPLETE; original goal restored on return.~%"))
      (install-compiled-goal saved-goal))))

(defun forward-e001 (&optional (output "doc/problems/crelay-topo/forward-evidence/E001-output.txt"))
  "Write a new transcript; use a different filename for a repeat run."
  (with-open-file (stream output :direction :output :if-exists :error
                         :if-does-not-exist :create)
    (let ((*standard-output* (make-broadcast-stream *standard-output* stream)))
      (forward-e001-run))))

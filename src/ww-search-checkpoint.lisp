;;; Filename: ww-search-checkpoint.lisp
;;; Standalone milestone searches with replay-based persistence.  Checkpoints
;;; retain history but never retry a predecessor or silently increase a cutoff.

(in-package :ww)


(defmethod print-object ((checkpoint search-checkpoint) stream)
  (print-unreadable-object (checkpoint stream :type t)
    (let ((phases (goal-chain-session-phases
                    (search-checkpoint-session checkpoint))))
      (format stream "~A: ~D checkpoints, ~D actions"
              (search-checkpoint-problem checkpoint) (length phases)
              (loop for phase in phases
                    sum (solution.depth (goal-chain-phase-solution phase)))))))


(defun require-current-search-checkpoint (checkpoint)
  "Reject stage-local objects retained across staging or a thread-mode rebuild."
  (check-type checkpoint search-checkpoint)
  (unless (and (eql (search-checkpoint-stage-generation checkpoint)
                    *goal-chain-stage-generation*)
               (eq (search-checkpoint-problem checkpoint) *problem-name*)
               (equalp (search-checkpoint-policy checkpoint)
                       (goal-chaining-policy-signature)))
    (error "Checkpoint belongs to an earlier staging. Import its archive after setting threads."))
  checkpoint)


(defun search-checkpoint-from-session (session)
  "Wrap an independently owned SESSION in its current staging identity."
  (%make-search-checkpoint
    :session session :stage-generation *goal-chain-stage-generation*
    :problem *problem-name* :policy (goal-chaining-policy-signature)))


(defun capture-search-checkpoint ()
  "Capture the accepted live chain, or the initial state of a fresh staging.
Export before changing thread mode; import after changing it. No search runs."
  (when *goal-chain-session*
    (return-from capture-search-checkpoint
      (search-checkpoint-from-session
        (copy-goal-chain-session-deeply))))
  (when (or *final-goal* *undo-stack*)
    (error "Capture requires a live chain or a fresh staging; retain the checkpoint returned by SOLVE-SUBGOAL."))
  (search-checkpoint-from-session
    (make-goal-chain-session
      :origin-state (copy-problem-state *start-state*)
      :original-goal (copy-tree *goal*)
      :original-goal-function (symbol-function 'goal-fn))))


(defun search-checkpoint-state (checkpoint)
  "Return an independent copy of CHECKPOINT's actual endpoint."
  (require-current-search-checkpoint checkpoint)
  (let* ((session (search-checkpoint-session checkpoint))
         (last-phase (car (last (goal-chain-session-phases session)))))
    (copy-problem-state
      (if last-phase
        (solution.goal (goal-chain-phase-solution last-phase))
        (goal-chain-session-origin-state session)))))


(defun solve-search-checkpoint (checkpoint goal-form)
  "Search once from CHECKPOINT, returning a new checkpoint only on success.
The second value reports whether a new checkpoint was found. Search-found
segments are retained without routine replay; import and final validation replay."
  (require-current-search-checkpoint checkpoint)
  (let* ((source (search-checkpoint-state checkpoint))
         (request
           (make-goal-chain-request
             :goal (copy-tree goal-form) :final-p nil
             :settings (capture-goal-chain-search-settings))))
    (solve-subgoal-from-form source goal-form #'run-goal-chain-planner)
    (unless *solutions-valid*
      (format t "~&No new checkpoint; saved endpoint and history retained.~%")
      (return-from solve-search-checkpoint (values checkpoint nil)))
    (let* ((session
             (copy-goal-chain-session-deeply
               (search-checkpoint-session checkpoint)))
           (phases (goal-chain-session-phases session))
           (phase
             (make-goal-chain-phase-from-solution
               request source phases (select-continuation-solution))))
      (setf (goal-chain-session-phases session) (append phases (list phase)))
      (format t "~&Saved checkpoint ~D; cumulative depth ~D. Not independently replayed.~%"
              (length (goal-chain-session-phases session))
              (length (goal-chain-cumulative-path
                        (goal-chain-session-phases session))))
      (values (search-checkpoint-from-session session) t))))


(defun export-search-checkpoint (checkpoint path)
  "Save CHECKPOINT's exact endpoint signatures and complete action history."
  (require-current-search-checkpoint checkpoint)
  (let* ((target (subgoal-progress-pathname path))
         (record
           (goal-chain-session-progress-record
             (search-checkpoint-session checkpoint))))
    (validate-subgoal-progress-record record)
    (write-subgoal-progress-file record target)
    (format t "~&Exported ~D actions to ~A.~%"
            (subgoal-progress-checkpoint-action-count record) target)
    target))


(defun import-search-checkpoint (path)
  "Replay an archive into a checkpoint in the current serial or parallel staging.
Set threads first. No search runs and the current planning session is restored
on both success and failure. Existing subgoal-progress archives are accepted."
  (let* ((record (validate-subgoal-progress-record
                   (read-subgoal-progress-file path)))
         (body (rest record)))
    ;; Unlike IMPORT-SUBGOAL-PROGRESS, this installs no live goal chain and
    ;; therefore needs no single-thread restriction or chain commit hooks.
    (when (or *goal-chain-session* *final-goal* *undo-stack*)
      (error "Stage afresh and set threads before importing a search checkpoint."))
    (validate-subgoal-progress-staging record)
    (let ((session
            (make-goal-chain-session
              :origin-state (copy-problem-state *start-state*)
              :original-goal (copy-tree (getf body :original-goal))
              :original-goal-function (symbol-function 'goal-fn)))
          (saved nil))
      (save-undo-checkpoint)
      (setf saved (pop *undo-stack*))
      (unwind-protect
          (progn
            (setf (goal-chain-session-phases session)
                  (build-imported-goal-chain-phases record session))
            (format t "~&Restored ~D checkpoints, ~D actions by replay; no search.~%"
                    (length (goal-chain-session-phases session))
                    (subgoal-progress-checkpoint-action-count record))
            (search-checkpoint-from-session session))
        (restore-undo-checkpoint saved)))))


(defun validate-search-checkpoint (checkpoint)
  "Replay the whole retained path against the original goal and exact endpoint.
Returns an ACTION-SEQUENCE-VALIDATION; does not search or publish a solution."
  (require-current-search-checkpoint checkpoint)
  (let* ((session (search-checkpoint-session checkpoint))
         (phases (goal-chain-session-phases session))
         (saved nil))
    (save-undo-checkpoint)
    (setf saved (pop *undo-stack*))
    (unwind-protect
        (progn
          (install-compiled-goal (goal-chain-session-original-goal session))
          (let ((validation
                  (validate-action-sequence
                    (goal-chain-session-origin-state session)
                    (goal-chain-cumulative-path phases)
                    :goal-test (symbol-function 'goal-fn))))
            (when (and (action-sequence-validation-success-p validation)
                       (not (equalp
                              (make-subgoal-progress-state-signature
                                (action-sequence-validation-final-state validation) phases)
                              (make-subgoal-progress-state-signature
                                (search-checkpoint-state checkpoint) phases))))
              (error "Cumulative replay differs from the saved exact endpoint."))
            validation))
      (restore-undo-checkpoint saved))))

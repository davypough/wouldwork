;;; Focused standalone checkpoint regression checks; loads only synthetic problems.
;;; Run in an isolated WOULDWORK_INSTANCE. No crelay-topo search is performed.
(in-package :ww)

(defvar *checkpoint-check-count* 0)
(defvar *checkpoint-test-path*
  (merge-pathnames "wouldwork-standalone-checkpoint-test.txt"
                   (uiop:temporary-directory)))

(defun checkpoint-check (value)
  (assert value)
  (incf *checkpoint-check-count*))

(defun checkpoint-signals-p (thunk)
  (handler-case (progn (funcall thunk) nil)
    (error () t)))

(defun check-generic-search-checkpoints ()
  (stage goal-chain-backtracking-test)
  (ww-set *threads* 0)
  (let* ((initial (capture-search-checkpoint))
         (found (solve-subgoal initial
                  (and (bt-at bt-a) (bt-choice bt-good)))))
    (checkpoint-check (not (eq initial found)))
    (checkpoint-check (null (goal-chain-session-phases
                             (search-checkpoint-session initial))))
    (checkpoint-check (null *goal-chain-session*))
    (checkpoint-check (action-sequence-validation-goal-satisfied-p
                        (validate-search-checkpoint found)))
    (export-search-checkpoint found *checkpoint-test-path*)
    (let ((signature (make-subgoal-progress-state-signature
                       (search-checkpoint-state found))))
      (ww-set *threads* 16)
      (checkpoint-check (checkpoint-signals-p
                          (lambda () (search-checkpoint-state found))))
      (stage goal-chain-backtracking-test)
      (ww-set *threads* 16)
      (let ((restored (import-search-checkpoint *checkpoint-test-path*)))
        (checkpoint-check (= *threads* 16))
        (checkpoint-check (equalp signature (make-subgoal-progress-state-signature
                                             (search-checkpoint-state restored))))
        (checkpoint-check (null *goal-chain-session*))
        (checkpoint-check (null *undo-stack*))
        (multiple-value-bind (same success)
            (solve-subgoal restored (bt-at bt-unreachable))
          (checkpoint-check (eq same restored))
          (checkpoint-check (not success)))
        (let ((next (solve-subgoal restored (bt-at bt-b))))
          (checkpoint-check (not (eq next restored)))
          (checkpoint-check (= 2 (length (goal-chain-cumulative-path
                                           (goal-chain-session-phases
                                             (search-checkpoint-session next))))))
          (checkpoint-check (not (action-sequence-validation-goal-satisfied-p
                                  (validate-search-checkpoint next)))))))
    ;; A tampered endpoint must fail without installing partial progress.
    (stage goal-chain-backtracking-test)
    (ww-set *threads* 16)
    (let* ((record (read-subgoal-progress-file *checkpoint-test-path*))
           (endpoint (getf (first (getf (rest record) :checkpoints)) :endpoint))
           (origin (make-subgoal-progress-state-signature *start-state*)))
      (incf (getf endpoint :time))
      (write-subgoal-progress-file record *checkpoint-test-path*)
      (checkpoint-check (checkpoint-signals-p
                          (lambda () (import-search-checkpoint *checkpoint-test-path*))))
      (checkpoint-check (equalp origin (make-subgoal-progress-state-signature *start-state*)))
      (checkpoint-check (equal *goal* '(bt-at bt-a)))
      (checkpoint-check (null *undo-stack*))
      (checkpoint-check (null *goal-chain-session*)))))

(defun check-recorder-search-checkpoints ()
  (stage recorder-cycle-orchestration-test)
  (ww-set *threads* 0)
  ;; Migrate a live chain using the existing archive, then continue standalone.
  (solve-subgoal (cycle-at cycle-middle))
  (export-subgoal-progress *checkpoint-test-path*)
  (let ((captured (capture-search-checkpoint)))
    (checkpoint-check (funcall (symbol-function 'recorder-state-recording-open-p)
                        (search-checkpoint-state captured)))
    (checkpoint-check (= 1 (length (goal-chain-session-phases
                                     (search-checkpoint-session captured))))))
  (stage recorder-cycle-orchestration-test)
  (ww-set *threads* 16)
  (let* ((restored (import-search-checkpoint *checkpoint-test-path*))
         (signature (make-subgoal-progress-state-signature
                      (search-checkpoint-state restored))))
    (checkpoint-check (funcall (symbol-function 'recorder-state-recording-open-p)
                        (search-checkpoint-state restored)))
    (checkpoint-check (= 1 (funcall (symbol-function 'recorder-state-cycle-count)
                             (search-checkpoint-state restored))))
    (checkpoint-check (null (symbol-value '*recorder-subgoal-chain*)))
    (let ((next (solve-subgoal restored (cycle-at cycle-end))))
      (checkpoint-check (not (eq next restored)))
      (checkpoint-check (equalp signature (make-subgoal-progress-state-signature
                                           (search-checkpoint-state restored))))
      (checkpoint-check (funcall (symbol-function 'recorder-state-recording-open-p)
                          (search-checkpoint-state next)))
      (export-search-checkpoint next *checkpoint-test-path*)
      (let ((endpoint (make-subgoal-progress-state-signature
                        (search-checkpoint-state next))))
        (stage recorder-cycle-orchestration-test)
        (ww-set *threads* 16)
        (let* ((again (import-search-checkpoint *checkpoint-test-path*))
               (final (solve-subgoal again
                        (and (cycle-at cycle-end) (ghost-stops-recorder))))
               (validation (validate-search-checkpoint final)))
          (checkpoint-check (equalp endpoint (make-subgoal-progress-state-signature
                                               (search-checkpoint-state again))))
          (checkpoint-check (action-sequence-validation-success-p validation))
          (checkpoint-check (action-sequence-validation-goal-checked-p validation))
          (checkpoint-check (action-sequence-validation-goal-satisfied-p validation))
          (checkpoint-check (= 4 (action-sequence-validation-action-count validation)))
          (checkpoint-check (not (funcall (symbol-function 'recorder-state-recording-open-p)
                                  (search-checkpoint-state final))))
          (export-search-checkpoint final *checkpoint-test-path*)
          (stage recorder-cycle-orchestration-test)
          (ww-set *threads* 16)
          (checkpoint-check
            (action-sequence-validation-goal-satisfied-p
              (validate-search-checkpoint
                (import-search-checkpoint *checkpoint-test-path*)))))))))

(unwind-protect
    (progn
      (setf *checkpoint-check-count* 0)
      (check-generic-search-checkpoints)
      (check-recorder-search-checkpoints)
      (stage goal-chain-backtracking-test)
      (ww-set *threads* 0)
      (run-test-claims '(goal-chain-progress-export-import-round-trip
                         goal-chain-progress-import-is-transactional))
      (stage recorder-cycle-orchestration-test)
      (ww-set *threads* 0)
      (run-test-claims '(recorder-open-checkpoint-export-import-round-trip))
      (format t "~&SEARCH-CHECKPOINT-CHECKS PASSED: ~D assertions.~%"
              *checkpoint-check-count*)
      (format t "LEGACY-PERSISTENCE-CHECKS PASSED: 3 claims.~%"))
  (uiop:delete-file-if-exists *checkpoint-test-path*))

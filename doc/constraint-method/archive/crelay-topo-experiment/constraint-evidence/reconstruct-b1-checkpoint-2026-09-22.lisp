;;; Reconstruct the approved B1 checkpoint from its search-found action sequence.
;;; Run after loading Wouldwork.  This performs one action-sequence replay and no search.

(in-package :ww)

(defparameter *t10-b1-actions*
  '((start-recorder > agent1 starts the recorder)
    (move > agent1* moves via ((walk location1 nil location2)))
    (move > agent1* moves via ((step (location2 ground) nil (location2 plate1))))
    (move > agent1 moves via ((walk location1 (gate1) location7)))
    (pickup-tray > agent1 picks up tray1 at location7 from location7)
    (move > agent1 moves via ((walk location7 (gate1) location2)))
    (put-tray > agent1 puts tray1 on plate1 at location2)
    (move > agent1 moves via ((walk location2 nil location1)))
    (cancel-playback > agent1 cancels recorder playback)))

(defparameter *t10-b1-goal*
  '(and (recorder-cycle-ended)
        (on tray1 plate1)
        (has-location agent1 location1)))

(defun b1-endpoint-satisfies-goal-p (state)
  (let ((facts (database state)))
    (and (funcall (symbol-function 'recorder-cycle-ended) state)
         (member '(on tray1 plate1) facts :test #'equal)
         (member '(has-location agent1 location1) facts :test #'equal))))

(defun reconstruct-t10-b1-checkpoint ()
  (stage crelay-topo)
  (ww-set *threads* 16)
  (ww-set *depth-cutoff* 12)
  (let* ((initial-checkpoint (capture-search-checkpoint))
         (source (search-checkpoint-state initial-checkpoint))
         (validation (validate-action-sequence source *t10-b1-actions*)))
    (unless (action-sequence-validation-success-p validation)
      (error "B1 replay failed: ~S" validation))
    (let ((endpoint (action-sequence-validation-final-state validation)))
      (unless (b1-endpoint-satisfies-goal-p endpoint)
        (error "B1 replay reached an endpoint that does not satisfy ~S."
               *t10-b1-goal*))
      (let* ((request
               (make-goal-chain-request
                 :goal (copy-tree *t10-b1-goal*)
                 :final-p nil
                 :settings (capture-goal-chain-search-settings)))
             (solution
               (make-solution
                 :depth (length *t10-b1-actions*)
                 :time (problem-state.time endpoint)
                 :value (problem-state.value endpoint)
                 :path (copy-tree *t10-b1-actions*)
                 :goal (copy-problem-state endpoint)))
             (session
               (copy-goal-chain-session-deeply
                 (search-checkpoint-session initial-checkpoint)))
             (phase
               (make-goal-chain-phase-from-solution request source nil solution)))
        (setf (goal-chain-session-phases session) (list phase))
        (let ((checkpoint (search-checkpoint-from-session session)))
          (export-search-checkpoint
            checkpoint
            #p"doc/problems/crelay-topo/constraint-evidence/t10-b1-checkpoint.txt")
          (format t "~&B1 replay/export complete: ~S~%" checkpoint)
          checkpoint)))))

(reconstruct-t10-b1-checkpoint)

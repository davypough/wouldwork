;;; Reconstruct the approved B1-prime checkpoint from its search-found action sequence.
;;; Run after loading Wouldwork.  This performs one action-sequence replay and no search.

(in-package :ww)

(defparameter *t13-b1-prime-actions*
  '((start-recorder > agent1 starts the recorder)
    (move > agent1* moves via ((walk location1 nil location2)))
    (move > agent1* moves via ((step (location2 ground) nil (location2 plate1))))
    (move > agent1 moves via ((walk location1 (gate1 gate3) location9)))
    (pickup-connector-retaining-pairings >
      agent1 picks up connector1 retaining pairings at location9)
    (move > agent1 moves via ((walk location9 (gate1 gate3) location2)))
    (connect-connector >
      agent1 connects connector1 on plate1 to
      (repeater1 connector1* receiver1) at location2)
    (move > agent1 moves via ((walk location2 nil location1)))
    (cancel-playback > agent1 cancels recorder playback)))

(defparameter *t13-b1-prime-goal*
  '(and (recorder-cycle-ended)
        (on connector1 plate1)
        (on tray1 plate2)
        (has-location agent1 location1)))

(defun b1-prime-endpoint-satisfies-goal-p (state)
  (let ((facts (database state)))
    (and (funcall (symbol-function 'recorder-cycle-ended) state)
         (member '(on connector1 plate1) facts :test #'equal)
         (member '(on tray1 plate2) facts :test #'equal)
         (member '(has-location agent1 location1) facts :test #'equal))))

(defun reconstruct-t13-b1-prime-checkpoint ()
  (stage crelay-topo)
  (ww-set *threads* 16)
  (ww-set *depth-cutoff* 12)
  (let* ((initial-checkpoint (capture-search-checkpoint))
         (source (search-checkpoint-state initial-checkpoint))
         (validation (validate-action-sequence source *t13-b1-prime-actions*)))
    (unless (action-sequence-validation-success-p validation)
      (error "B1-prime replay failed: ~S" validation))
    (let ((endpoint (action-sequence-validation-final-state validation)))
      (unless (b1-prime-endpoint-satisfies-goal-p endpoint)
        (error "B1-prime replay reached an endpoint that does not satisfy ~S."
               *t13-b1-prime-goal*))
      (let* ((request
               (make-goal-chain-request
                 :goal (copy-tree *t13-b1-prime-goal*)
                 :final-p nil
                 :settings (capture-goal-chain-search-settings)))
             (solution
               (make-solution
                 :depth (length *t13-b1-prime-actions*)
                 :time (problem-state.time endpoint)
                 :value (problem-state.value endpoint)
                 :path (copy-tree *t13-b1-prime-actions*)
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
            #p"doc/problems/crelay-topo/constraint-evidence/t13-b1-prime-checkpoint.txt")
          (format t "~&B1-prime replay/export complete: ~S~%" checkpoint)
          checkpoint)))))

(reconstruct-t13-b1-prime-checkpoint)

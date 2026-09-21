;;; One-time migration from T10's permitted realization ledger. REPLAY ONLY.
;;; Load after staging crelay-topo and setting threads to 16. No solve call.
(in-package :ww)
(load (merge-pathnames "tech/constraint-ledger.lisp"
                       (asdf:system-source-directory :wouldwork)))

(defun t10-replay-ledger-link (session link)
  (let* ((phases (goal-chain-session-phases session))
         (source (if phases
                   (solution.goal (goal-chain-phase-solution (car (last phases))))
                   (goal-chain-session-origin-state session)))
         (goal (getf link :search-goal))
         (actions (let ((*read-eval* nil))
                    (mapcar #'read-from-string (getf link :evidence))))
         (request (make-goal-chain-request
                    :goal goal :final-p nil
                    :settings (capture-goal-chain-search-settings))))
    (assert (member (getf link :status) '(:closed :realized)))
    (assert actions)
    (install-compiled-goal goal)
    (let ((validation (validate-action-sequence
                        source actions :goal-test (symbol-function 'goal-fn))))
      (unless (and (action-sequence-validation-success-p validation)
                   (action-sequence-validation-goal-satisfied-p validation))
        (error "T10 ~A replay failed: ~S" (getf link :id) validation))
      (let* ((endpoint (action-sequence-validation-final-state validation))
             (solution (make-solution
                         :path actions :depth (length actions) :goal endpoint
                         :time (problem-state.time endpoint)
                         :value (problem-state.value endpoint)))
             (phase (make-goal-chain-phase-from-solution request source phases solution)))
        (setf (goal-chain-session-phases session) (append phases (list phase)))
        (format t "~&T10 ~A replayed, cumulative depth ~D.~%"
                (getf link :id)
                (length (goal-chain-cumulative-path (goal-chain-session-phases session))))))))

(defun build-t10-search-checkpoint ()
  (assert (eq *problem-name* 'crelay-topo))
  (assert (= *threads* 16))
  (let* ((root (asdf:system-source-directory :wouldwork))
         (ledger (read-realization-ledger
                   (merge-pathnames "doc/problems/crelay-topo/Constraint-Realization-Ledger.txt" root)))
         (checkpoint (capture-search-checkpoint))
         (session (search-checkpoint-session checkpoint))
         (saved nil))
    (assert (null (goal-chain-session-phases session)))
    (save-undo-checkpoint)
    (setf saved (pop *undo-stack*))
    (unwind-protect
        (progn
          (dolist (id '(lk1 lk2 lk3 lk7 lk8 lk9 lk4))
            (t10-replay-ledger-link session (ledger-record ledger id)))
          (assert (= 19 (length (goal-chain-cumulative-path
                                 (goal-chain-session-phases session)))))
          (let ((state (search-checkpoint-state checkpoint)))
            (assert (funcall (symbol-function 'recorder-state-recording-open-p) state))
            (assert (= 1 (funcall (symbol-function 'recorder-state-cycle-count) state)))
            (let ((facts (list-database (problem-state.idb state))))
              (dolist (fact '((has-location agent1 location15) (switched-on switch2)
                              (has-location box1 location6) (has-location tray1 location13)
                              (has-location connector1 location12) (on tray1 plate4)
                              (on connector1 plate5) (on agent1* plate1) (on tray1* plate2)
                              (has-location agent1* location2) (has-location tray1* location7)
                              (has-location connector1* location9) (has-location box1* location6)
                              (open gate1) (open gate2) (open gate3) (open gate6) (open gate7)
                              (recording-open gate1) (recording-open gate2)
                              (recording-open gate3) (recording-open gate5)))
                (assert (member fact facts :test #'equal)))
              (assert (not (member '(open gate5) facts :test #'equal))))
            (format t "~&T10 RESTORED ENDPOINT:~%~A~%" state))
          (export-search-checkpoint checkpoint
            (merge-pathnames "doc/problems/crelay-topo/constraint-evidence/t10-location15-checkpoint.txt" root))
          checkpoint)
      (restore-undo-checkpoint saved))))

(defparameter *t10-checkpoint* (build-t10-search-checkpoint))

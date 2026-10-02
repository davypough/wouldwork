;;; Validate the revised B2 route (D, 2026-09-24): box1 on plate2, connector1 kept on plate1,
;;; tray1 on the ground at location7, cycle 2 closed.  Supersedes validate-b2-box-plate2-2026-09-24.lisp.
;;; Extends validate-b2-ghost-tray-2026-09-24.lisp (actions 1-23) with the rearrangement
;;; for the next fork.  One action-sequence replay; no search.
;;; Run after Wouldwork is loaded:
;;;   (load (merge-pathnames "doc/problems/crelay-topo/constraint-evidence/validate-b2-box-plate2-rev-2026-09-24.lisp"
;;;                          (asdf:system-source-directory :wouldwork)))

(in-package :ww)


(defparameter *b2-box-plate2-rev-actions*
  '(;; Cycle 1 -- setup: ghost holds plate1; live agent brings connector1 to plate1; cancel.
    (start-recorder > agent1 starts the recorder)
    (move > agent1* moves via ((walk location1 nil location2)))
    (move > agent1* moves via ((step (location2 ground) nil (location2 plate1))))
    (move > agent1 moves via ((walk location1 (gate1) location7)))
    (move > agent1 moves via ((walk location7 (gate3) location9)))
    (pickup-connector > agent1 picks up connector1 at location9)
    (move > agent1 moves via ((walk location9 (gate3) location7)))
    (move > agent1 moves via ((walk location7 (gate1) location2)))
    (put-connector > agent1 puts connector1 on plate1 without pairings at location2)
    (move > agent1 moves via ((walk location2 nil location1)))
    (cancel-playback > agent1 cancels recorder playback)
    ;; Cycle 2 -- ghost holds tray1* at location5; live agent reaches location6 and the box.
    (start-recorder > agent1 starts the recorder)
    (move > agent1* moves via ((walk location1 (gate1) location7)))
    (pickup-tray > agent1* picks up tray1* at location7 from location7)
    (move > agent1* moves via ((walk location7 nil location5)))
    (move > agent1 moves via ((walk location1 (gate1) location4)))
    (move > agent1 moves via ((step (location4 ground) nil (location4 blower1))))
    (toggle-switch > agent1 toggles switch1)
    (move > agent1 moves via ((jump (location20 ground) nil (location5 tray1*))))
    (toggle-switch > agent1 toggles switch1)
    (move > agent1 moves via ((jump (location5 tray1*) (gate2) (location6 ground))))
    (pickup-box > agent1 picks up box1 at location6 from location6)
    (move > agent1 moves via ((jump location6 (gate2) location5)))
    ;; Cycle 2 continued (revised per D) -- box1 to plate2, tray1 left on the ground at
    ;; location7, connector1 stays on plate1.
    (move > agent1 moves via ((walk location5 nil location7)))
    (put-box > agent1 puts box1 on ground at location7)
    (pickup-tray > agent1 picks up tray1 at location7 from location7)
    (put-tray > agent1 puts tray1 on ground at location7)
    (pickup-box > agent1 picks up box1 at location7 from location7)
    (put-box > agent1 puts box1 on plate2 at location7)
    (move > agent1 moves via ((walk location7 (gate1) location1)))
    (cancel-playback > agent1 cancels recorder playback)))


(defun b2-box-plate2-rev-goal-p (state)
  (let ((facts (database state)))
    (and (member '(on connector1 plate1) facts :test #'equal)
         (member '(on box1 plate2) facts :test #'equal)
         (member '(has-location tray1 location7) facts :test #'equal)
         (notany (lambda (fact) (and (eql (first fact) 'on) (eql (second fact) 'tray1))) facts)
         (member '(has-location agent1 location1) facts :test #'equal)
         (not (member '(recording-in-progress) facts :test #'equal)))))


(defun validate-b2-box-plate2-rev ()
  (stage crelay-topo)
  (let* ((source (search-checkpoint-state (capture-search-checkpoint)))
         (validation (validate-action-sequence source *b2-box-plate2-rev-actions*
                                               :goal-test #'b2-box-plate2-rev-goal-p)))
    (format t "~&SUCCESS-P ~S  ACTION-COUNT ~S  GOAL-CHECKED-P ~S  GOAL-SATISFIED-P ~S~%"
            (action-sequence-validation-success-p validation)
            (action-sequence-validation-action-count validation)
            (action-sequence-validation-goal-checked-p validation)
            (not (null (action-sequence-validation-goal-satisfied-p validation))))
    (if (action-sequence-validation-success-p validation)
      (format t "FINAL STATE:~%~S~%"
              (database (action-sequence-validation-final-state validation)))
      (format t "FAILED at action ~S: ~S~%REASON: ~A~%"
              (action-sequence-validation-failure-index validation)
              (action-sequence-validation-failure-action validation)
              (action-sequence-validation-failure-reason validation)))
    validation))


(validate-b2-box-plate2-rev)

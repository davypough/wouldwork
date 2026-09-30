;;; Validate the B2 route through box1 on plate2 (pr18 proposed) from a fresh start.
;;; Extends validate-b2-ghost-tray-2026-09-24.lisp (actions 1-23) with the rearrangement
;;; for the next fork.  One action-sequence replay; no search.
;;; Run after Wouldwork is loaded:
;;;   (load (merge-pathnames "doc/problems/crelay-topo/constraint-evidence/validate-b2-box-plate2-2026-09-24.lisp"
;;;                          (asdf:system-source-directory :wouldwork)))

(in-package :ww)


(defparameter *b2-box-plate2-actions*
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
    ;; Cycle 2 continued -- rearrange for the next fork: connector1 to ground at location9,
    ;; tray1 to plate1, box1 to plate2.  Ghost connector1* on plate1 keeps physical gate1 open.
    (move > agent1 moves via ((walk location5 nil location7)))
    (put-box > agent1 puts box1 on ground at location7)
    (move > agent1 moves via ((walk location7 (gate1) location2)))
    (pickup-connector > agent1 picks up connector1 at location2)
    (move > agent1 moves via ((walk location2 (gate1) location7)))
    (move > agent1 moves via ((walk location7 (gate3) location9)))
    (put-connector > agent1 puts connector1 on ground without pairings at location9)
    (move > agent1 moves via ((walk location9 (gate3) location7)))
    (pickup-tray > agent1 picks up tray1 at location7 from location7)
    (move > agent1 moves via ((walk location7 (gate1) location2)))
    (put-tray > agent1 puts tray1 on plate1 at location2)
    (move > agent1 moves via ((walk location2 (gate1) location7)))
    (pickup-box > agent1 picks up box1 at location7 from location7)
    (put-box > agent1 puts box1 on plate2 at location7)
    (move > agent1 moves via ((walk location7 (gate1) location1)))
    (cancel-playback > agent1 cancels recorder playback)))


(defun b2-box-plate2-goal-p (state)
  (let ((facts (database state)))
    (and (member '(on tray1 plate1) facts :test #'equal)
         (member '(on box1 plate2) facts :test #'equal)
         (member '(has-location connector1 location9) facts :test #'equal)
         (member '(has-location agent1 location1) facts :test #'equal)
         (not (member '(recording-in-progress) facts :test #'equal)))))


(defun validate-b2-box-plate2 ()
  (stage crelay-topo)
  (let* ((source (search-checkpoint-state (capture-search-checkpoint)))
         (validation (validate-action-sequence source *b2-box-plate2-actions*
                                               :goal-test #'b2-box-plate2-goal-p)))
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


(validate-b2-box-plate2)

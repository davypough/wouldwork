;;; Validate cycle 3 parts A and B (D's handoff plan, 2026-09-24) from a fresh start: the
;;; 31-action B2 route; the ghost holds tray1* at location5; box1 goes on it from location20;
;;; connector1 is paired at location9 with receiver1 and repeater1 and placed on box1.
;;; One action-sequence replay; no search.
;;; Run after Wouldwork is loaded:
;;;   (load (merge-pathnames "doc/problems/crelay-topo/constraint-evidence/validate-c3-part-b-2026-09-24.lisp"
;;;                          (asdf:system-source-directory :wouldwork)))

(in-package :ww)


(defparameter *c3-part-b-actions*
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
    (cancel-playback > agent1 cancels recorder playback)
    ;; Cycle 3, part A (D's plan) -- ghost holds tray1* at location5; live box1 goes onto it.
    (start-recorder > agent1 starts the recorder)
    (move > agent1* moves via ((walk location1 (gate1) location7)))
    (pickup-tray > agent1* picks up tray1* at location7 from location7)
    (move > agent1* moves via ((walk location7 nil location5)))
    (move > agent1 moves via ((walk location1 (gate1) location7)))
    (pickup-box > agent1 picks up box1 at location7 from location7)
    ;; Revised (D, 2026-09-24): a grounded agent cannot place onto a held tray (top 3/2,
    ;; placement reach 1; S5 matrix NO).  Ride blower1 to location20 and place from there.
    (move > agent1 moves via ((walk location7 nil location4)))
    (move > agent1 moves via ((step (location4 ground) nil (location4 blower1))))
    (toggle-switch > agent1 toggles switch1)
    (put-box > agent1 puts box1 on tray1* at location5)
    ;; Cycle 3, part B (D, 2026-09-24) -- pair connector1 at location9 with repeater1 and
    ;; receiver1, then place it on box1 from location20.  Blower1 stays on throughout.
    ;; Physical gate1 stays open via connector1* on plate1; gate3 via box1* on plate2.
    (move > agent1 moves via ((jump location20 nil location5)))
    (move > agent1 moves via ((walk location5 nil location7)))
    (move > agent1 moves via ((walk location7 (gate1) location2)))
    (pickup-connector > agent1 picks up connector1 at location2)
    (move > agent1 moves via ((walk location2 (gate1) location7)))
    (move > agent1 moves via ((walk location7 (gate3) location9)))
    (connect-connector > agent1 connects connector1 on ground to (repeater1 receiver1) at location9)
    (pickup-connector-retaining-pairings > agent1 picks up connector1 retaining pairings at location9)
    (move > agent1 moves via ((walk location9 (gate3) location7)))
    (move > agent1 moves via ((walk location7 nil location4)))
    (move > agent1 moves via ((step (location4 ground) nil (location4 blower1))))
    (put-connector > agent1 puts connector1 on box1 without pairings at location5)))


(defun c3-holding-fact-p (fact agent cargo)
  ;; HOLDING is bijective, so the database lists it as HOLDING1/HOLDING2.
  (and (member (first fact) '(holding holding1 holding2))
       (member agent (rest fact))
       (member cargo (rest fact))))


(defun c3-part-b-goal-p (state)
  (let ((facts (database state)))
    (and (member '(on box1 tray1*) facts :test #'equal)
         (member '(on connector1 box1) facts :test #'equal)
         (member '(paired connector1 receiver1) facts :test #'equal)
         (member '(paired connector1 repeater1) facts :test #'equal)
         (some (lambda (fact) (c3-holding-fact-p fact 'agent1* 'tray1*)) facts)
         (member '(has-location agent1* location5) facts :test #'equal)
         (member '(recording-in-progress) facts :test #'equal))))


(defun validate-c3-part-b ()
  (stage crelay-topo)
  (let* ((source (search-checkpoint-state (capture-search-checkpoint)))
         (validation (validate-action-sequence source *c3-part-b-actions*
                                               :goal-test #'c3-part-b-goal-p)))
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


(validate-c3-part-b)

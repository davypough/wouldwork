;;; Validate the complete crelay-topo route (2026-09-25) from a fresh start: the 80-action
;;; alternative cycle 3 (validate-c3-alt-2026-09-25.lisp, validated by D: stack at location15,
;;; receiver1 active, gate8 open), then a final leg: through gate8 to location16, tray1 on
;;; plate6, connector1 on plate7, box1 on plate8 (gate9), and agent1 through gate9 to
;;; location19, the problem goal.  One action-sequence replay; no search.
;;; Run after Wouldwork is loaded:
;;;   (load (merge-pathnames "doc/problems/crelay-topo/constraint-evidence/validate-c3-final-2026-09-25.lisp"
;;;                          (asdf:system-source-directory :wouldwork)))

(in-package :ww)


(defparameter *c3-final-actions*
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
    ;; Cycle 3 (D's alternative, 2026-09-25) -- D's actions 2-50.
    (START-RECORDER AGENT1)
    (MOVE AGENT1* ((WALK LOCATION1 (GATE1) LOCATION7)))
    (PICKUP-TRAY AGENT1* TRAY1* LOCATION7 LOCATION7)
    (MOVE AGENT1* ((WALK LOCATION7 NIL LOCATION5)))
    (MOVE AGENT1 ((WALK LOCATION1 NIL LOCATION2)))
    (PICKUP-CONNECTOR AGENT1 CONNECTOR1 LOCATION2)
    (MOVE AGENT1 ((WALK LOCATION2 (GATE1) LOCATION7)))
    (CONNECT-CONNECTOR AGENT1 CONNECTOR1 GROUND (REPEATER1 RECEIVER1) LOCATION7)
    (PICKUP-BOX AGENT1 BOX1 LOCATION7 LOCATION7)
    (MOVE AGENT1 ((WALK LOCATION7 NIL LOCATION4)))
    (MOVE AGENT1 ((STEP (LOCATION4 GROUND) NIL (LOCATION4 BLOWER1))))
    (TOGGLE-SWITCH AGENT1 SWITCH1)
    (PUT-BOX AGENT1 BOX1 TRAY1* LOCATION5)
    (TOGGLE-SWITCH AGENT1 SWITCH1)
    (MOVE AGENT1 ((WALK LOCATION4 NIL LOCATION7)))
    (PICKUP-CONNECTOR AGENT1 CONNECTOR1 LOCATION7)
    (MOVE AGENT1 ((WALK LOCATION7 NIL LOCATION4)))
    (MOVE AGENT1 ((STEP (LOCATION4 GROUND) NIL (LOCATION4 BLOWER1))))
    (TOGGLE-SWITCH AGENT1 SWITCH1)
    (CONNECT-CONNECTOR AGENT1 CONNECTOR1 BOX1 (REPEATER1 RECEIVER1) LOCATION5)
    (TOGGLE-SWITCH AGENT1 SWITCH1)
    (MOVE AGENT1 ((WALK LOCATION4 NIL LOCATION7)))
    (PICKUP-TRAY AGENT1 TRAY1 LOCATION7 LOCATION7)
    (MOVE AGENT1 ((WALK LOCATION7 (GATE3 GATE5) LOCATION13)))
    (PUT-TRAY AGENT1 TRAY1 PLATE4 LOCATION13)
    (MOVE AGENT1* ((WALK LOCATION5 (GATE3 GATE5) LOCATION12)))
    (MOVE AGENT1* ((STEP (LOCATION12 GROUND) NIL (LOCATION12 PLATE5))))
    (MOVE AGENT1 ((WALK LOCATION13 (GATE6 SCREEN1) LOCATION14)))
    (TOGGLE-SWITCH AGENT1 SWITCH2)
    (MOVE AGENT1 ((WALK LOCATION14 (GATE6 SCREEN1) LOCATION13)))
    (PICKUP-TRAY AGENT1 TRAY1 LOCATION13 LOCATION13)
    (MOVE AGENT1 ((WALK LOCATION13 NIL LOCATION12)))
    (MOVE AGENT1 ((STEP (LOCATION12 GROUND) NIL (LOCATION12 PLATE5))))
    (PUT-TRAY AGENT1* TRAY1* GROUND LOCATION12)
    (PICKUP-TRAY AGENT1* TRAY1* LOCATION12 LOCATION12)
    (MOVE AGENT1* ((STEP (LOCATION12 PLATE5) NIL (LOCATION12 GROUND))))
    (MOVE AGENT1* ((WALK LOCATION12 (GATE1 GATE3 GATE5) LOCATION2)))
    (PUT-TRAY AGENT1* TRAY1* GROUND LOCATION2)
    (PICKUP-CONNECTOR AGENT1* CONNECTOR1* LOCATION2)
    (PUT-CONNECTOR AGENT1* CONNECTOR1* GROUND LOCATION2)
    (PICKUP-TRAY AGENT1* TRAY1* LOCATION2 LOCATION2)
    (PUT-TRAY AGENT1* TRAY1* PLATE1 LOCATION2)
    (PICKUP-CONNECTOR AGENT1* CONNECTOR1* LOCATION2)
    (MOVE AGENT1* ((WALK LOCATION2 (GATE1 GATE3) LOCATION9)))
    (CONNECT-CONNECTOR AGENT1* CONNECTOR1* GROUND (REPEATER1 TRANSMITTER1) LOCATION9)
    (MOVE AGENT1* ((WALK LOCATION9 NIL LOCATION10)))
    (MOVE AGENT1* ((STEP (LOCATION10 GROUND) NIL (LOCATION10 PLATE3))))
    (MOVE AGENT1 ((STEP (LOCATION12 PLATE5) NIL (LOCATION12 GROUND))))
    (MOVE AGENT1 ((WALK LOCATION12 (GATE7) LOCATION15)))
    ;; Final leg (2026-09-25) -- through gate8 while receiver1 holds it open; tray1, connector1
    ;; and box1 go on plates 6, 7 and 8 (gate9); agent1 walks through gate9 to location19.
    ;; Technical choice (reversible): the tray, with its riders, goes down on plate6 first, so
    ;; the stack is unloaded from floor height (connector base 1, reach 1).
    (MOVE AGENT1 ((WALK LOCATION15 (GATE8) LOCATION16)))
    (PUT-TRAY AGENT1 TRAY1 PLATE6 LOCATION16)
    (PICKUP-CONNECTOR AGENT1 CONNECTOR1 LOCATION16)
    (MOVE AGENT1 ((WALK LOCATION16 NIL LOCATION17)))
    (PUT-CONNECTOR AGENT1 CONNECTOR1 PLATE7 LOCATION17)
    (MOVE AGENT1 ((WALK LOCATION17 NIL LOCATION16)))
    (PICKUP-BOX AGENT1 BOX1 LOCATION16 LOCATION16)
    (MOVE AGENT1 ((WALK LOCATION16 NIL LOCATION18)))
    (PUT-BOX AGENT1 BOX1 PLATE8 LOCATION18)
    (MOVE AGENT1 ((WALK LOCATION18 (GATE9) LOCATION19)))))


(defun c3-final-goal-p (state)
  (let ((facts (database state)))
    (and (member '(has-location agent1 location19) facts :test #'equal)
         (member '(on tray1 plate6) facts :test #'equal)
         (member '(on connector1 plate7) facts :test #'equal)
         (member '(on box1 plate8) facts :test #'equal))))


(defun validate-c3-final ()
  (stage crelay-topo)
  (let* ((source (search-checkpoint-state (capture-search-checkpoint)))
         (validation (validate-action-sequence source *c3-final-actions*
                                               :goal-test #'c3-final-goal-p)))
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


(validate-c3-final)

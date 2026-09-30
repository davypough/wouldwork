;;; Validate cycle 3 part D2 (D's Question 8 answer, 2026-09-24) from a fresh start: the
;;; 65-action part D1 prefix, then agent1 passes gate6/screen1 empty-handed, turns switch2 on,
;;; returns, lifts tray1 off plate4, joins the ghost on plate5, and the ghost releases tray1*
;;; so the stack settles onto the live held tray1, with gate7 open.
;;; One action-sequence replay; no search.
;;; Run after Wouldwork is loaded:
;;;   (load (merge-pathnames "doc/problems/crelay-topo/constraint-evidence/validate-c3-part-d2-2026-09-24.lisp"
;;;                          (asdf:system-source-directory :wouldwork)))
;;;
;;; STATUS (recorded 2026-09-25): SUPERSEDED; VALIDATED by D 2026-09-25 (D3 replay, 73 actions,
;;; all flags T, so D1 and D2 pass as its prefixes).  Written in the late 2026-09-24 session after
;;; the handoff, so no state file recorded it at the time.  The route reaches location15 with the
;;; stack but UNLIT: receiver1 not active, gate8 closed (connector1* still on plate1).  The
;;; route it tests (parts A-C handoff at location12, then D1-D3) was replaced by D's
;;; alternative cycle 3, validate-c3-alt-2026-09-25.lisp, which T10's accepted path uses.
;;; Kept as evidence of the earlier route; not part of any chain, archive or ledger record.
;;; D1 (65 actions) and D2 (71) are prefixes of D3 (73).  See
;;; b2-ghost-tray-loc5-check-2026-09-24.txt, section D1-D3 FILES.

(in-package :ww)


(defparameter *c3-part-d2-actions*
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
    (put-connector > agent1 puts connector1 on box1 without pairings at location5)
    ;; Cycle 3, part C (D, 2026-09-24) -- live agent fetches tray1 and waits at location12
    ;; holding it; the ghost carries tray1* (box1 + paired connector1 riding) to location12
    ;; and puts it down; the riders settle onto the live agent's held tray1.
    (move > agent1 moves via ((jump location20 nil location5)))
    (move > agent1 moves via ((walk location5 nil location7)))
    (pickup-tray > agent1 picks up tray1 at location7 from location7)
    (move > agent1 moves via ((walk location7 (gate3 gate5) location12)))
    (move > agent1* moves via ((walk location5 nil location7)))
    (move > agent1* moves via ((walk location7 (gate3 gate5) location12)))
    (put-tray > agent1* puts tray1* on ground at location12)
    ;; Cycle 3, part D1 (D, 2026-09-24) -- hand the stack back to the ghost, ghost on plate5,
    ;; live tray1 (empty) on plate4, so plates 4 and 5 open gate6.  Technical choice: the
    ;; transfer happens with both agents on the ground at location12 (equal tray tops), and
    ;; the ghost steps onto plate5 afterwards; a local release settles riders onto the
    ;; highest eligible surface at or below their base (-support-settling).
    (pickup-tray > agent1* picks up tray1* at location12 from location12)
    (put-tray > agent1 puts tray1 on ground at location12)
    (move > agent1* moves via ((step (location12 ground) nil (location12 plate5))))
    (pickup-tray > agent1 picks up tray1 at location12 from location12)
    (put-tray > agent1 puts tray1 on plate4 at location13)
    ;; Cycle 3, part D2 (D, 2026-09-24) -- empty-handed through gate6/screen1, switch2 on
    ;; (gate7 opens, gate5 closes), back, lift tray1 off plate4, stand superimposed with the
    ;; ghost on plate5 (plates are flush), and the ghost releases tray1* locally so the stack
    ;; settles onto the live held tray1.
    (move > agent1 moves via ((walk location12 (gate6 screen1) location14)))
    (toggle-switch > agent1 toggles switch2)
    (move > agent1 moves via ((walk location14 (gate6 screen1) location12)))
    (pickup-tray > agent1 picks up tray1 at location13 from location12)
    (move > agent1 moves via ((step (location12 ground) nil (location12 plate5))))
    (put-tray > agent1* puts tray1* on ground at location12)))


(defun c3-holding-fact-p (fact agent cargo)
  ;; HOLDING is bijective, so the database lists it as HOLDING1/HOLDING2.
  (and (member (first fact) '(holding holding1 holding2))
       (member agent (rest fact))
       (member cargo (rest fact))))


(defun c3-part-d2-goal-p (state)
  (let ((facts (database state)))
    (and (member '(on box1 tray1) facts :test #'equal)
         (member '(on connector1 box1) facts :test #'equal)
         (member '(paired connector1 receiver1) facts :test #'equal)
         (member '(paired connector1 repeater1) facts :test #'equal)
         (some (lambda (fact) (c3-holding-fact-p fact 'agent1 'tray1)) facts)
         (member '(on agent1 plate5) facts :test #'equal)
         (member '(switched-on switch2) facts :test #'equal)
         (member '(open gate7) facts :test #'equal)
         (not (member '(open gate5) facts :test #'equal))
         (member '(recording-in-progress) facts :test #'equal))))


(defun validate-c3-part-d2 ()
  (stage crelay-topo)
  (let* ((source (search-checkpoint-state (capture-search-checkpoint)))
         (validation (validate-action-sequence source *c3-part-d2-actions*
                                               :goal-test #'c3-part-d2-goal-p)))
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


(validate-c3-part-d2)

;;; crelay-topo -- accepted actions from the original start.
(in-package :ww)

(defparameter *accepted-actions*
  '(;; SG1 recorder cycle 1: connector1 onto plate1 at location2; cancelled
    (start-recorder > agent1 starts the recorder)
    (move agent1* ((walk location1 nil location2)))
    (move agent1* ((step (location2 ground) nil (location2 plate1))))
    (move agent1 ((walk location1 (gate1) location7)))
    (move agent1 ((walk location7 (gate3) location9)))
    (pickup-connector > agent1 picks up connector1 at location9)
    (move agent1 ((walk location9 (gate3) location7)))
    (move agent1 ((walk location7 (gate1) location2)))
    (put-connector > agent1 puts connector1 on plate1 without pairings at location2)
    (move agent1 ((walk location2 nil location1)))
    (cancel-playback > agent1 cancels recorder playback)
    ;; SG2 recorder cycle 2: box1 fetched from the alcove, landing on the ghost-held tray at location5; box1 onto plate2; cancelled
    (start-recorder > agent1 starts the recorder)
    (move agent1* ((walk location1 (gate1) location7)))
    (pickup-tray > agent1* picks up tray1* at location7 from location7)
    (move agent1* ((walk location7 nil location5)))
    (move agent1 ((walk location1 (gate1) location4)))
    (move agent1 ((step (location4 ground) nil (location4 blower1))))
    (toggle-switch > agent1 toggles switch1)
    (move agent1 ((jump (location20 ground) (blower1) (location5 tray1*))))
    (toggle-switch > agent1 toggles switch1)
    (move agent1 ((jump (location5 tray1*) (edge1 gate2) (location6 ground))))
    (pickup-box > agent1 picks up box1 at location6 from location6)
    (move agent1 ((jump location6 (edge1 gate2) location5)))
    (move agent1 ((walk location5 nil location7)))
    (put-box > agent1 puts box1 on ground at location7)
    (pickup-tray > agent1 picks up tray1 at location7 from location7)
    (put-tray > agent1 puts tray1 on ground at location7)
    (pickup-box > agent1 picks up box1 at location7 from location7)
    (put-box > agent1 puts box1 on plate2 at location7)
    (move agent1 ((walk location7 (gate1) location1)))
    (cancel-playback > agent1 cancels recorder playback)
    ;; SG3 recorder cycle 3 (left open): lit stack connector1 on box1 on tray1, carried by agent1 to location15
    (start-recorder agent1)
    (move agent1* ((walk location1 (gate1) location7)))
    (pickup-tray agent1* tray1* location7 location7)
    (move agent1* ((walk location7 nil location5)))
    (move agent1 ((walk location1 nil location2)))
    (pickup-connector agent1 connector1 location2)
    (move agent1 ((walk location2 (gate1) location7)))
    (connect-connector agent1 connector1 ground (repeater1 receiver1) location7)
    (pickup-box agent1 box1 location7 location7)
    (move agent1 ((walk location7 nil location4)))
    (move agent1 ((step (location4 ground) nil (location4 blower1))))
    (toggle-switch agent1 switch1)
    (put-box agent1 box1 tray1* location5)
    (toggle-switch agent1 switch1)
    (move agent1 ((walk location4 nil location7)))
    (pickup-connector agent1 connector1 location7)
    (move agent1 ((walk location7 nil location4)))
    (move agent1 ((step (location4 ground) nil (location4 blower1))))
    (toggle-switch agent1 switch1)
    (connect-connector agent1 connector1 box1 (repeater1 receiver1) location5)
    (toggle-switch agent1 switch1)
    (move agent1 ((walk location4 nil location7)))
    (pickup-tray agent1 tray1 location7 location7)
    (move agent1 ((walk location7 (gate3 gate5) location13)))
    (put-tray agent1 tray1 plate4 location13)
    (move agent1* ((walk location5 (gate3 gate5) location12)))
    (move agent1* ((step (location12 ground) nil (location12 plate5))))
    (move agent1 ((walk location13 (gate6 screen1) location14)))
    (toggle-switch agent1 switch2)
    (move agent1 ((walk location14 (gate6 screen1) location13)))
    (pickup-tray agent1 tray1 location13 location13)
    (move agent1 ((walk location13 nil location12)))
    (move agent1 ((step (location12 ground) nil (location12 plate5))))
    (put-tray agent1* tray1* ground location12)
    (pickup-tray agent1* tray1* location12 location12)
    (move agent1* ((step (location12 plate5) nil (location12 ground))))
    (move agent1* ((walk location12 (gate1 gate3 gate5) location2)))
    (put-tray agent1* tray1* ground location2)
    (pickup-connector agent1* connector1* location2)
    (put-connector agent1* connector1* ground location2)
    (pickup-tray agent1* tray1* location2 location2)
    (put-tray agent1* tray1* plate1 location2)
    (pickup-connector agent1* connector1* location2)
    (move agent1* ((walk location2 (gate1 gate3) location9)))
    (connect-connector agent1* connector1* ground (repeater1 transmitter1) location9)
    (move agent1* ((walk location9 nil location10)))
    (move agent1* ((step (location10 ground) nil (location10 plate3))))
    (move agent1 ((step (location12 plate5) nil (location12 ground))))
    (move agent1 ((walk location12 (gate7) location15)))
    ;; SG4 goal (search-found, cutoff 10): plates 6-8 weighted, agent1 through gate9 to location19
    (move agent1 ((walk location15 (gate8) location18)))
    (put-tray agent1 tray1 plate8 location18)
    (pickup-connector-retaining-pairings agent1 connector1 location18)
    (connect-connector agent1 connector1 plate6 (repeater1 receiver1) location16)
    (pickup-box agent1 box1 location18 location18)
    (put-box agent1 box1 plate7 location17)
    (move agent1 ((walk location18 (gate9) location19)))))


(loop for form in *accepted-actions*
      for index from 1
      for action = (find (first form) *actions* :key #'action.name)
      do (unless action
           (error "Action ~D is unknown: ~S" index form))
         (unless (= (length (strip-display-connectives action (rest form)))
                    (length (action.effect-variables action)))
           (error "Action ~D is malformed: ~S" index form)))


(defparameter *accepted-validation*
  (validate-action-sequence *start-state* *accepted-actions*
                            :goal-test (symbol-function 'goal-fn) :verbose t))


(format t "~%~D actions: success=~S goal-checked=~S goal-satisfied=~S failure-index=~S reason=~S~%"
        (length *accepted-actions*)
        (action-sequence-validation-success-p *accepted-validation*)
        (action-sequence-validation-goal-checked-p *accepted-validation*)
        (action-sequence-validation-goal-satisfied-p *accepted-validation*)
        (action-sequence-validation-failure-index *accepted-validation*)
        (action-sequence-validation-failure-reason *accepted-validation*))
(display-validation-state (action-sequence-validation-final-state *accepted-validation*))


(defparameter *accepted-validators-p*
  (and (action-sequence-validation-goal-satisfied-p *accepted-validation*)
       (report-solution-validator-verdicts
         *accepted-actions* (action-sequence-validation-final-state *accepted-validation*))))


(when (and (action-sequence-validation-success-p *accepted-validation*)
           (action-sequence-validation-goal-checked-p *accepted-validation*)
           (action-sequence-validation-goal-satisfied-p *accepted-validation*)
           *accepted-validators-p*)
  (with-open-file (*standard-output*
                   (merge-pathnames "doc/problems/crelay-topo/Validation.txt"
                                    (asdf:system-source-directory :wouldwork))
                   :direction :output :if-exists :supersede)
    (format t ";;; crelay-topo -- complete validated solution, ~D actions.~%" (length *accepted-actions*))
    (format t ";;; SUCCESS-P T; GOAL-CHECKED-P T; GOAL-SATISFIED-P T; solution validators accepted~%")
    (format t ";;; To re-validate: (stage crelay-topo), then evaluate this form separately.~%")
    (format t "(validate-solution :verbose")
    (let ((*print-case* :downcase)
          (*print-pretty* nil))
      (loop for form in *accepted-actions*
            for index from 1
            do (format t "~%  ~S" (list index form))))
    (format t ")~2%Final state:~%")
    (display-validation-state (action-sequence-validation-final-state *accepted-validation*))))

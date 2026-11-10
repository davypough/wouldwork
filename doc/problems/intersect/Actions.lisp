;;; intersect -- accepted actions from the original start.
(in-package :ww)

(defparameter *accepted-actions*
  '(;; SG1 agent1 at location3 (east corridor); gates 1 and 2 open, gate3 closed
    (pickup-connector > agent1 picks up connector1 at location1)
    (select-termini > agent1 selects (receiver3 receiver1 transmitter1) for connector1 at location1)
    (put-connector > agent1 puts connector1 on ground at location1)
    (move > agent1 moves via ((walk location1 (gate1) location2)))
    (pickup-connector > agent1 picks up connector2 at location2)
    (select-termini > agent1 selects (receiver4 receiver2 transmitter2) for connector2 at location2)
    (put-connector > agent1 puts connector2 on ground at location2)
    (move > agent1 moves via ((walk location2 (gate2) location3)))
    ;; SG2 agent1 at location11 (west room; location5 until spec revision 4) holding connector3; gate3 open, gate2 closed
    (pickup-connector > agent1 picks up connector3 at location3)
    (select-termini > agent1 selects (transmitter2) for connector3 at location3)
    (put-connector > agent1 puts connector3 on ground at location3)
    (move > agent1 moves via ((ladder location3 (ladder1) location2)))
    (pickup-connector > agent1 picks up connector2 at location2)
    (select-termini > agent1 selects (connector3 receiver2) for connector2 at location2)
    (put-connector > agent1 puts connector2 on ground at location2)
    (move > agent1 moves via ((walk location2 nil location8)))
    (move > agent1 moves via ((walk location8 (gate2) location3)))
    (pickup-connector > agent1 picks up connector3 at location3)
    (move > agent1 moves via ((walk location3 (gate3) location11)))
    ;; SG3 agent1 at location7 (goal): blue rerouted location10 -> location11 -> receiver3
    (select-termini > agent1 selects (receiver3) for connector3 at location11)
    (put-connector > agent1 puts connector3 on ground at location11)
    (move > agent1 moves via ((walk location11 (gate3) location3)))
    (move > agent1 moves via ((ladder location3 (ladder1) location2)))
    (pickup-connector > agent1 picks up connector2 at location2)
    (move > agent1 moves via ((walk location2 nil location9)))
    (select-termini > agent1 selects (receiver2 transmitter2) for connector2 at location9)
    (put-connector > agent1 puts connector2 on ground at location9)
    (move > agent1 moves via ((walk location9 (gate1) location1)))
    (pickup-connector > agent1 picks up connector1 at location1)
    (move > agent1 moves via ((walk location1 nil location10)))
    (select-termini > agent1 selects (connector3 receiver1 transmitter1) for connector1 at location10)
    (put-connector > agent1 puts connector1 on ground at location10)
    (move > agent1 moves via ((walk location10 (gate1) location2)))
    (move > agent1 moves via ((walk location2 (gate2) location3)))
    (move > agent1 moves via ((walk location3 (gate3) location11)))
    (pickup-connector > agent1 picks up connector3 at location11)
    (move > agent1 moves via ((walk location11 nil location6)))
    (select-termini > agent1 selects (connector2 receiver4) for connector3 at location6)
    (put-connector > agent1 puts connector3 on plate1 at location6)
    (move > agent1 moves via ((walk location6 (gate4) location7)))))

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
                   (merge-pathnames "doc/problems/intersect/Validation.txt"
                                    (asdf:system-source-directory :wouldwork))
                   :direction :output :if-exists :supersede)
    (format t ";;; intersect -- complete validated solution, ~D actions.~%" (length *accepted-actions*))
    (format t ";;; SUCCESS-P T; GOAL-CHECKED-P T; GOAL-SATISFIED-P T; solution validators accepted~%")
    (format t ";;; To re-validate: (stage intersect), then evaluate this form separately.~%")
    (format t "(validate-solution :verbose")
    (let ((*print-case* :downcase)
          (*print-pretty* nil))
      (loop for form in *accepted-actions*
            for index from 1
            do (format t "~%  ~S" (list index form))))
    (format t ")~2%Final state:~%")
    (display-validation-state (action-sequence-validation-final-state *accepted-validation*))))

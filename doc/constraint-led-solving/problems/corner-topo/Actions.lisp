;;; corner-topo -- accepted actions from the original start.
(in-package :ww)

(defparameter *accepted-actions*
  '(;; SG1 goal: gate1 opened, connector2 carried to location4, east setup, connector2 picked up
    (pickup-connector > agent1 picks up connector1 at location1)
    (connect-connector > agent1 connects connector1 on ground to (receiver1 transmitter1) at location1)
    (move agent1 ((walk location1 nil location2)))
    (pickup-connector > agent1 picks up connector2 at location2)
    (move agent1 ((walk location2 (gate1) location4)))
    (connect-connector > agent1 connects connector2 on ground to (transmitter1) at location4)
    (move agent1 ((walk location4 (gate1) location1)))
    (pickup-connector > agent1 picks up connector1 at location1)
    (move agent1 ((walk location1 nil location2)))
    (connect-connector > agent1 connects connector1 on ground to (receiver3 receiver1 transmitter2) at location2)
    (move agent1 ((walk location2 nil location3)))
    (pickup-connector > agent1 picks up connector3 at location3)
    (connect-connector > agent1 connects connector3 on ground to (connector1 receiver2 transmitter1) at location3)
    (move agent1 ((walk location3 (gate1) location4)))
    (pickup-connector > agent1 picks up connector2 at location4)))


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
                   (merge-pathnames "doc/constraint-led-solving/problems/corner-topo/Validation.txt"
                                    (asdf:system-source-directory :wouldwork))
                   :direction :output :if-exists :supersede)
    (format t ";;; corner-topo -- complete validated solution, ~D actions.~%" (length *accepted-actions*))
    (format t ";;; SUCCESS-P T; GOAL-CHECKED-P T; GOAL-SATISFIED-P T; solution validators accepted~%")
    (format t ";;; To re-validate: (stage corner-topo), then evaluate this form separately.~%")
    (format t "(validate-solution :verbose")
    (let ((*print-case* :downcase)
          (*print-pretty* nil))
      (loop for form in *accepted-actions*
            for index from 1
            do (format t "~%  ~S" (list index form))))
    (format t ")~2%Final state:~%")
    (display-validation-state (action-sequence-validation-final-state *accepted-validation*))))

;;; windtunnel-topo -- accepted actions from the original start.
(in-package :ww)

(defparameter *accepted-actions*
  '(;; SG1 goal: agent1 at location5, one recording cycle left open
    (start-recorder agent1)
    (pickup-connector agent1* connector1* location1)
    (move agent1* ((walk location1 nil location2)))
    (move agent1* ((step (location2 ground) nil (location2 plate1))))
    (move agent1* ((step (location2 plate1) nil (location2 ground))))
    ;; physical and recording latches both on; the live connector sees the source
    (pickup-connector agent1 connector1 location1)
    (connect-connector agent1 connector1 ground (repeater1 transmitter1) location1)
    (move agent1 ((walk location1 nil location2)))
    (move agent1 ((step (location2 ground) nil (location2 plate1))))
    (move agent1 ((step (location2 plate1) nil (location2 ground))))
    ;; physical off, recording on; pass through location3
    (move agent1 ((walk location2 (blower1) location3)))
    (move agent1 ((walk location3 (blower1) location4)))
    (move agent1* ((step (location2 ground) nil (location2 plate1))))
    (move agent1* ((step (location2 plate1) nil (location2 ground))))
    ;; physical on, recording off; the ghost places its connector
    (move agent1* ((walk location2 (blower1) location3)))
    (connect-connector agent1* connector1* ground (repeater1 receiver1) location3)
    (move agent1 ((walk location4 (gate2) location5)))))


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
                   (merge-pathnames "doc/constraint-led-solving/problems/windtunnel-topo/Validation.txt"
                                    (asdf:system-source-directory :wouldwork))
                   :direction :output :if-exists :supersede)
    (format t ";;; windtunnel-topo -- complete validated solution, ~D actions.~%" (length *accepted-actions*))
    (format t ";;; SUCCESS-P T; GOAL-CHECKED-P T; GOAL-SATISFIED-P T; solution validators accepted~%")
    (format t ";;; To re-validate: (stage windtunnel-topo), then evaluate this form separately.~%")
    (format t "(validate-solution :verbose")
    (let ((*print-case* :downcase)
          (*print-pretty* nil))
      (loop for form in *accepted-actions*
            for index from 1
            do (format t "~%  ~S" (list index form))))
    (format t ")~2%Final state:~%")
    (display-validation-state (action-sequence-validation-final-state *accepted-validation*))))

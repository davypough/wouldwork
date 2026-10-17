;;; crelay-topo -- accepted actions from the original start.
;;; Load after (stage crelay-topo-1), in a separate top-level form.
(in-package :ww)

(defparameter *accepted-actions*
  '(
    ;; SG1 connector1 on plate1; recorder cycle 1 closed
    (start-recorder agent1)
    (move agent1* ((walk location1 nil location2)))
    (move agent1* ((step (location2 ground) nil (location2 plate1))))
    (move agent1 ((walk location1 (gate1 gate3) location9)))
    (pickup-connector agent1 connector1 location9)
    (move agent1 ((walk location9 (gate1 gate3) location2)))
    (put-connector agent1 connector1 plate1 location2)
    (move agent1 ((walk location2 nil location1)))
    (cancel-playback agent1)
    ;; SG2 box1 on plate2; recorder cycle 2 closed (legs 2a, 2b)
    (start-recorder agent1)
    (move agent1 ((walk location1 (gate1) location4)))
    (move agent1 ((step (location4 ground) nil (location4 blower1))))
    (move agent1* ((walk location1 (gate1) location7)))
    (pickup-tray agent1* tray1* location7 location7)
    (move agent1* ((walk location7 nil location5)))
    (toggle-switch agent1* switch1)
    (move agent1 ((jump (location20 ground) (blower1) (location5 tray1*))))
    (toggle-switch agent1 switch1)
    (move agent1 ((jump (location5 tray1*) (edge1 gate2) (location6 ground))))
    (pickup-box agent1 box1 location6 location6)
    (move agent1 ((jump location6 (edge1 gate2) location4) (walk location4 nil location7)))
    (put-box agent1 box1 ground location7)
    (pickup-tray agent1 tray1 location7 location7)
    (put-tray agent1 tray1 ground location7)
    (pickup-box agent1 box1 location7 location7)
    (put-box agent1 box1 plate2 location7)
    (move agent1 ((walk location7 (gate1) location1)))
    (cancel-playback agent1)))

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

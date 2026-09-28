;;; Approved SG1; hand-derived candidate. Replay only, no search.
(in-package :ww)

(defparameter *phobia-sg1-actions*
  '((move agent1 ((walk location4 nil location1)))
    (pickup-jammer > agent1 picks up jammer1 at location1)
    (move agent1 ((walk location1 nil location4)))
    (jam-target > agent1 jams wblower2 with jammer1 at location4 on ground)
    (move agent1 ((walk location4 (wblower2) location5)
                  (walk location5 (wblower2) location6)))
    (pickup-connector > agent1 picks up connector1 at location6)
    (move agent1 ((walk location6 nil location4)))))

;;; Preflight every form before replay. Arguments follow effect templates.
(loop for form in *phobia-sg1-actions*
      for index from 1
      for action = (find (first form) *actions* :key #'action.name)
      do (unless action
           (error "SG1 action ~D is unknown: ~S" index (first form)))
         (let ((values (strip-display-connectives action (rest form))))
           (unless (= (length values) (length (action.effect-variables action)))
             (error "SG1 action ~D has ~D values; expected ~D."
                    index (length values) (length (action.effect-variables action))))))

(defparameter *phobia-sg1-validation*
  (validate-action-sequence *start-state* *phobia-sg1-actions* :verbose t))

(format t "~%SG1 replay: success=~S; failure-index=~S; reason=~S~%"
        (action-sequence-validation-success-p *phobia-sg1-validation*)
        (action-sequence-validation-failure-index *phobia-sg1-validation*)
        (action-sequence-validation-failure-reason *phobia-sg1-validation*))

;;; Review the verbose final state against the agreed SG1 before acceptance.
;;; This does not change the current checkpoint or test the final loft goal.

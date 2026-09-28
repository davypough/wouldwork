;;; Approved SG4b: replay from accepted SG4a, no restaging or search.
(in-package :ww)

(defparameter *phobia-sg4b-actions*
  '((move agent1 ((walk location13 (wblower2) location5)
                  (walk location5 nil location4)
                  (walk location4 nil location1)
                  (walk location1 (wgears1) location2)))
    (pickup-jammer > agent1 picks up jammer1 at location2)
    (move agent1 ((walk location2 nil location1)
                  (walk location1 nil location4)
                  (walk location4 (wblower2) location5)
                  (walk location5 (wblower2) location13)))
    (jam-target > agent1 jams wblower2 with jammer1 at location13 on ground)))

(loop for form in *phobia-sg4b-actions*
      for index from 1
      for action = (find (first form) *actions* :key #'action.name)
      do (unless action
           (error "SG4b action ~D is unknown: ~S" index (first form)))
         (let ((values (strip-display-connectives action (rest form))))
           (unless (= (length values) (length (action.effect-variables action)))
             (error "SG4b action ~D has ~D values; expected ~D."
                    index (length values) (length (action.effect-variables action))))))

(defparameter *phobia-sg4b-validation*
  (validate-action-sequence
    (action-sequence-validation-final-state *phobia-sg4a-validation*)
    *phobia-sg4b-actions*
    :goal-test
    (lambda (state)
      (let ((facts (database state)))
        (every (lambda (fact) (not (null (member fact facts :test #'equal))))
               '((has-location agent1 location13)
                 (has-location fan1 location13)
                 (has-location jammer1 location13)
                 (jamming jammer1 wblower2)
                 (has-location connector1 location5)
                 (has-location connector2 location2)
                 (paired connector2 transmitter1)
                 (paired connector1 connector2)
                 (paired connector1 receiver2)
                 (active receiver2)))))
    :verbose t))

(format t "~%SG4b replay: success=~S; goal-checked=~S; goal-satisfied=~S; failure-index=~S; reason=~S~%"
        (action-sequence-validation-success-p *phobia-sg4b-validation*)
        (action-sequence-validation-goal-checked-p *phobia-sg4b-validation*)
        (action-sequence-validation-goal-satisfied-p *phobia-sg4b-validation*)
        (action-sequence-validation-failure-index *phobia-sg4b-validation*)
        (action-sequence-validation-failure-reason *phobia-sg4b-validation*))
(display-validation-state
  (action-sequence-validation-final-state *phobia-sg4b-validation*))

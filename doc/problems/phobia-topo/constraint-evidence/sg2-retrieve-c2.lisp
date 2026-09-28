;;; Approved SG2, hand-derived candidate; replay only, no search.
(in-package :ww)

(defparameter *phobia-sg2-actions*
  '((put-connector > agent1 puts connector1 on ground without pairings at location4)
    (pickup-jammer > agent1 picks up jammer1 at location4)
    (jam-target > agent1 jams wgears1 with jammer1 at location4 on ground)
    (move agent1 ((walk location4 nil location1)
                  (walk location1 (wgears1) location2)))
    (pickup-fan > agent1 picks up fan1 at location2 from location2)
    (move agent1 ((walk location2 nil location1)
                  (walk location1 nil location4)))
    (put-fan > agent1 puts fan1 on ground at location4)
    (pickup-connector > agent1 picks up connector1 at location4)
    (move agent1 ((walk location4 nil location1)
                  (walk location1 (wgears1) location2)
                  (walk location2 (wgears1) location12)))
    (connect-connector > agent1 connects connector1 on ground
                       to (receiver1 transmitter1) at location12)
    (move agent1 ((walk location12 (wgears1) location2)
                  (walk location2 nil location1)
                  (walk location1 nil location4)))
    (pickup-jammer > agent1 picks up jammer1 at location4)
    (move agent1 ((walk location4 nil location1)
                  (walk location1 (wgears1) location2)
                  (walk location2 (wgears1) location12)))
    (jam-target > agent1 jams gate2 with jammer1 at location12 on ground)
    (move agent1 ((walk location12 (gate1 gate2) location3)))
    (pickup-connector > agent1 picks up connector2 at location3)
    (move agent1 ((walk location3 (gate1 gate2) location12)))))

;;; Reject malformed forms throughout the sequence before any replay.
(loop for form in *phobia-sg2-actions*
      for index from 1
      for action = (find (first form) *actions* :key #'action.name)
      do (unless action
           (error "SG2 action ~D is unknown: ~S" index (first form)))
         (let ((values (strip-display-connectives action (rest form))))
           (unless (= (length values) (length (action.effect-variables action)))
             (error "SG2 action ~D has ~D values; expected ~D."
                    index (length values) (length (action.effect-variables action))))))

(defparameter *phobia-sg2-validation*
  (validate-action-sequence
    (action-sequence-validation-final-state *phobia-sg1-validation*)
    *phobia-sg2-actions* :verbose t))

(format t "~%SG2 replay: success=~S; failure-index=~S; reason=~S~%"
        (action-sequence-validation-success-p *phobia-sg2-validation*)
        (action-sequence-validation-failure-index *phobia-sg2-validation*)
        (action-sequence-validation-failure-reason *phobia-sg2-validation*))
(display-validation-state
  (action-sequence-validation-final-state *phobia-sg2-validation*))

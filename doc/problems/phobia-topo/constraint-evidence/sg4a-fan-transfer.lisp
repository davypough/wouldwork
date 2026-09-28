;;; Approved hand-derived transfer. Restore SG3, then replay four actions.
;;; No search and no automatic fallback.
(in-package :ww)

(stage phobia-topo)
(ww-set *threads* 16)
(defparameter *phobia-sg3-restored*
  (import-search-checkpoint
    (merge-pathnames
      "doc/problems/phobia-topo/constraint-evidence/sg3-checkpoint.txt"
      (asdf:system-source-directory :wouldwork))))

(defparameter *phobia-sg4a-actions*
  '((move agent1 ((walk location5 nil location4)))
    (pickup-fan > agent1 picks up fan1 at location4 from location4)
    (move agent1 ((walk location4 (wblower2) location5)
                  (walk location5 (wblower2) location13)))
    (put-fan > agent1 puts fan1 on ground at location13)))

(loop for form in *phobia-sg4a-actions*
      for index from 1
      for action = (find (first form) *actions* :key #'action.name)
      do (unless action
           (error "SG4a action ~D is unknown: ~S" index (first form)))
         (let ((values (strip-display-connectives action (rest form))))
           (unless (= (length values) (length (action.effect-variables action)))
             (error "SG4a action ~D has ~D values; expected ~D."
                    index (length values) (length (action.effect-variables action))))))

(defparameter *phobia-sg4a-validation*
  (validate-action-sequence
    (search-checkpoint-state *phobia-sg3-restored*)
    *phobia-sg4a-actions*
    :goal-test
    (lambda (state)
      (let ((facts (database state)))
        (every (lambda (fact) (not (null (member fact facts :test #'equal))))
               '((has-location agent1 location13)
                 (has-location fan1 location13)
                 (has-location jammer1 location2)
                 (jamming jammer1 wblower2)
                 (has-location connector1 location5)
                 (has-location connector2 location2)
                 (active receiver2)))))
    :verbose t))

(format t "~%SG4a replay: success=~S; goal-checked=~S; goal-satisfied=~S; failure-index=~S; reason=~S~%"
        (action-sequence-validation-success-p *phobia-sg4a-validation*)
        (action-sequence-validation-goal-checked-p *phobia-sg4a-validation*)
        (action-sequence-validation-goal-satisfied-p *phobia-sg4a-validation*)
        (action-sequence-validation-failure-index *phobia-sg4a-validation*)
        (action-sequence-validation-failure-reason *phobia-sg4a-validation*))
(display-validation-state
  (action-sequence-validation-final-state *phobia-sg4a-validation*))

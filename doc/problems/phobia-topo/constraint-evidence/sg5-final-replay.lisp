;;; Approved hand-derived finish from SG4b. No search or restaging.
(in-package :ww)

(defparameter *phobia-sg5-actions*
  '((move agent1 ((walk location13 (wblower2) location5)))
    (pickup-connector > agent1 picks up connector1 at location5)
    (move agent1 ((walk location5 (wblower2) location13)))
    (put-connector > agent1 puts connector1 on ground without pairings at location13)
    (pickup-jammer > agent1 picks up jammer1 at location13)
    (move agent1 ((walk location13 nil location6)
                  (walk location6 (wblower3) location8)))
    (jam-target > agent1 jams wblower4 with jammer1 at location8 on ground)
    (move agent1 ((walk location8 nil location6)
                  (walk location6 nil location13)))
    (pickup-fan > agent1 picks up fan1 at location13 from location13)
    (move agent1 ((walk location13 nil location6)
                  (walk location6 (wblower3) location8)
                  (walk location8 (wblower4) location9)
                  (walk location9 (wblower4) location10)))
    (mount-fan > agent1 mounts fan1 on fgears1 at location10)
    (move agent1 ((step (location10 ground) nil (location10 fan1))))))

(loop for form in *phobia-sg5-actions*
      for index from 1
      for action = (find (first form) *actions* :key #'action.name)
      do (unless action
           (error "SG5 action ~D is unknown: ~S" index (first form)))
         (let ((values (strip-display-connectives action (rest form))))
           (unless (= (length values) (length (action.effect-variables action)))
             (error "SG5 action ~D has ~D values; expected ~D."
                    index (length values) (length (action.effect-variables action))))))

(defparameter *phobia-sg5-validation*
  (validate-action-sequence
    (action-sequence-validation-final-state *phobia-sg4b-validation*)
    *phobia-sg5-actions*
    :goal-test (goal-chain-session-original-goal-function
                 (search-checkpoint-session *phobia-sg3-restored*))
    :verbose t))

(format t "~%SG5 replay: success=~S; goal-checked=~S; goal-satisfied=~S; failure-index=~S; reason=~S~%"
        (action-sequence-validation-success-p *phobia-sg5-validation*)
        (action-sequence-validation-goal-checked-p *phobia-sg5-validation*)
        (action-sequence-validation-goal-satisfied-p *phobia-sg5-validation*)
        (action-sequence-validation-failure-index *phobia-sg5-validation*)
        (action-sequence-validation-failure-reason *phobia-sg5-validation*))
(display-validation-state
  (action-sequence-validation-final-state *phobia-sg5-validation*))

;;; Independently compose and replay everything from the original problem start.
(when (and (action-sequence-validation-success-p *phobia-sg5-validation*)
           (action-sequence-validation-goal-satisfied-p *phobia-sg5-validation*))
  (defparameter *phobia-complete-actions*
    (append
      (normalize-validation-actions
        (goal-chain-cumulative-path
          (goal-chain-session-phases
            (search-checkpoint-session *phobia-sg3-restored*))))
      *phobia-sg4a-actions* *phobia-sg4b-actions* *phobia-sg5-actions*))
  (defparameter *phobia-complete-validation*
    (let ((session (search-checkpoint-session *phobia-sg3-restored*)))
      (validate-action-sequence
        (goal-chain-session-origin-state session)
        *phobia-complete-actions*
        :goal-test (goal-chain-session-original-goal-function session)
        :verbose nil)))
  (format t "~%FULL PATH (~D actions): success=~S; goal-checked=~S; goal-satisfied=~S; failure-index=~S; reason=~S~%"
          (length *phobia-complete-actions*)
          (action-sequence-validation-success-p *phobia-complete-validation*)
          (action-sequence-validation-goal-checked-p *phobia-complete-validation*)
          (action-sequence-validation-goal-satisfied-p *phobia-complete-validation*)
          (action-sequence-validation-failure-index *phobia-complete-validation*)
          (action-sequence-validation-failure-reason *phobia-complete-validation*))
  (when (and (action-sequence-validation-success-p *phobia-complete-validation*)
             (action-sequence-validation-goal-checked-p *phobia-complete-validation*)
             (action-sequence-validation-goal-satisfied-p *phobia-complete-validation*))
    (with-open-file
        (out (merge-pathnames
               "doc/problems/phobia-topo/constraint-evidence/complete-validated-path.txt"
               (asdf:system-source-directory :wouldwork))
             :direction :output :if-exists :supersede)
      (format out "Original goal: ~S~%Actions: ~D~%SUCCESS-P T; GOAL-CHECKED-P T; GOAL-SATISFIED-P T~%"
              (goal-chain-session-original-goal
                (search-checkpoint-session *phobia-sg3-restored*))
              (length *phobia-complete-actions*))
      (dolist (action *phobia-complete-actions*)
        (format out "~A~%" (format-action-for-display action))))))

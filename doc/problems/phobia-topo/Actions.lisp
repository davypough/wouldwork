;;; phobia-topo -- accepted actions from the original start.
(in-package :ww)

(defparameter *accepted-actions*
  '(;; SG1 agent1 at location4 holding connector1; jammer1 at location4 jamming wblower2
    (move agent1 ((walk location4 nil location1)))
    (pickup-jammer > agent1 picks up jammer1 at location1)
    (move agent1 ((walk location1 nil location4)))
    (jam-target > agent1 jams wblower2 with jammer1 at location4 on ground)
    (move agent1 ((walk location4 (wblower2) location5) (walk location5 (wblower2) location6)))
    (pickup-connector > agent1 picks up connector1 at location6)
    (move agent1 ((walk location6 nil location4)))
    ;; SG2 agent1 at location12 holding connector2; connector1 powering receiver1; jammer1 jamming gate2
    (put-connector > agent1 puts connector1 on ground without pairings at location4)
    (pickup-jammer > agent1 picks up jammer1 at location4)
    (jam-target > agent1 jams wgears1 with jammer1 at location4 on ground)
    (move agent1 ((walk location4 nil location1) (walk location1 (wgears1) location2)))
    (pickup-fan > agent1 picks up fan1 at location2 from location2)
    (move agent1 ((walk location2 nil location1) (walk location1 nil location4)))
    (put-fan > agent1 puts fan1 on ground at location4)
    (pickup-connector > agent1 picks up connector1 at location4)
    (move agent1 ((walk location4 nil location1) (walk location1 (wgears1) location2) (walk location2 (wgears1) location12)))
    (connect-connector > agent1 connects connector1 on ground to (receiver1 transmitter1) at location12)
    (move agent1 ((walk location12 (wgears1) location2) (walk location2 nil location1) (walk location1 nil location4)))
    (pickup-jammer > agent1 picks up jammer1 at location4)
    (move agent1 ((walk location4 nil location1) (walk location1 (wgears1) location2) (walk location2 (wgears1) location12)))
    (jam-target > agent1 jams gate2 with jammer1 at location12 on ground)
    (move agent1 ((walk location12 (gate1 gate2) location3)))
    (pickup-connector > agent1 picks up connector2 at location3)
    (move agent1 ((walk location3 (gate1 gate2) location12)))
    ;; SG3 receiver2 active (search-found): transmitter1 -> connector2 at location2 -> connector1 at location5 -> receiver2
    (move agent1 ((walk location12 (wgears1) location2)))
    (connect-connector > agent1 connects connector2 on ground to (receiver1 transmitter1) at location2)
    (move agent1 ((walk location2 (wgears1) location12)))
    (pickup-jammer > agent1 picks up jammer1 at location12)
    (move agent1 ((walk location12 (wgears1) location2)))
    (jam-target > agent1 jams wblower2 with jammer1 at location2 on ground)
    (move agent1 ((walk location2 (wgears1) location12)))
    (pickup-connector > agent1 picks up connector1 at location12)
    (move agent1 ((walk location12 (wblower2 wgears1) location5)))
    (connect-connector > agent1 connects connector1 on ground to (connector2 receiver2) at location5)
    ;; SG4a agent1 and fan1 at location13
    (move agent1 ((walk location5 nil location4)))
    (pickup-fan > agent1 picks up fan1 at location4 from location4)
    (move agent1 ((walk location4 (wblower2) location5) (walk location5 (wblower2) location13)))
    (put-fan > agent1 puts fan1 on ground at location13)
    ;; SG4b jammer1 at location13 jamming wblower2
    (move agent1 ((walk location13 (wblower2) location5) (walk location5 nil location4) (walk location4 nil location1) (walk location1 (wgears1) location2)))
    (pickup-jammer > agent1 picks up jammer1 at location2)
    (move agent1 ((walk location2 nil location1) (walk location1 nil location4) (walk location4 (wblower2) location5) (walk location5 (wblower2) location13)))
    (jam-target > agent1 jams wblower2 with jammer1 at location13 on ground)
    ;; SG5 goal: agent1 at location11, lifted by fan1 mounted on fgears1
    (move agent1 ((walk location13 (wblower2) location5)))
    (pickup-connector > agent1 picks up connector1 at location5)
    (move agent1 ((walk location5 (wblower2) location13)))
    (put-connector > agent1 puts connector1 on ground without pairings at location13)
    (pickup-jammer > agent1 picks up jammer1 at location13)
    (move agent1 ((walk location13 nil location6) (walk location6 (wblower3) location8)))
    (jam-target > agent1 jams wblower4 with jammer1 at location8 on ground)
    (move agent1 ((walk location8 nil location6) (walk location6 nil location13)))
    (pickup-fan > agent1 picks up fan1 at location13 from location13)
    (move agent1 ((walk location13 nil location6) (walk location6 (wblower3) location8) (walk location8 (wblower4) location9) (walk location9 (wblower4) location10)))
    (mount-fan > agent1 mounts fan1 on fgears1 at location10)
    (move agent1 ((step (location10 ground) nil (location10 fan1))))))


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
                   (merge-pathnames "doc/problems/phobia-topo/Validation.txt"
                                    (asdf:system-source-directory :wouldwork))
                   :direction :output :if-exists :supersede)
    (format t ";;; phobia-topo -- complete validated solution, ~D actions.~%" (length *accepted-actions*))
    (format t ";;; SUCCESS-P T; GOAL-CHECKED-P T; GOAL-SATISFIED-P T; solution validators accepted~%")
    (format t ";;; To re-validate: (stage phobia-topo), then evaluate this form separately.~%")
    (format t "(validate-solution :verbose")
    (let ((*print-case* :downcase)
          (*print-pretty* nil))
      (loop for form in *accepted-actions*
            for index from 1
            do (format t "~%  ~S" (list index form))))
    (format t ")~2%Final state:~%")
    (display-validation-state (action-sequence-validation-final-state *accepted-validation*))))

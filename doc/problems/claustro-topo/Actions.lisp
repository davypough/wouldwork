;;; claustro-topo -- accepted actions from the original start.
(in-package :ww)

(defparameter *accepted-actions*
  '(;; SG1 agent1 at location7 holding jammer1, gate4 open (search-found): jam gate1, cut the beam with box1 at location2
    (pickup-jammer > agent1 picks up jammer1 at location1)
    (jam-target > agent1 jams gate1 with jammer1 at location1 on ground)
    (move agent1 ((walk location1 (gate1 gate3) location4)))
    (pickup-box > agent1 picks up box1 at location4 from location4)
    (move agent1 ((walk location4 nil location3)))
    (put-box > agent1 puts box1 on ground at location2)
    (move agent1 ((walk location3 (gate4 screen1) location7)))
    (pickup-jammer > agent1 picks up jammer1 at location7)
    ;; SG2 jam gate5 from location8, fetch jammer2
    (move agent1 ((walk location7 nil location8)))
    (jam-target > agent1 jams gate5 with jammer1 at location8 on ground)
    (move agent1 ((walk location8 (gate4 gate5 screen1) location9)))
    (pickup-jammer > agent1 picks up jammer2 at location9)
    ;; SG3 park jammer2 on plate3 holding gate5; fetch jammer1; jam gate1 through the window
    (move agent1 ((walk location9 (gate5) location6)))
    (jam-target > agent1 jams gate5 with jammer2 at location6 on plate3)
    (move agent1 ((walk location6 (gate4 screen1) location8)))
    (pickup-jammer > agent1 picks up jammer1 at location8)
    (move agent1 ((walk location8 nil location7)))
    (jam-target > agent1 jams gate1 with jammer1 at location1 on ground)
    ;; SG4 ladder to location1, clear location2 (beam lights), box1 onto plate1
    (move agent1 ((ladder location7 (ladder1) location1)))
    (move agent1 ((walk location1 nil location2)))
    (pickup-box > agent1 picks up box1 at location2 from location2)
    (move agent1 ((walk location2 nil location1)))
    (move agent1 ((walk location1 (gate1 gate3) location4)))
    (put-box > agent1 puts box1 on plate1 at location4)
    ;; SG5 handover: jammer2 jams gate1 from plate3; jammer1 jams gate5 from plate2
    (move agent1 ((walk location4 nil location6)))
    (pickup-jammer > agent1 picks up jammer2 at location6)
    (jam-target > agent1 jams gate1 with jammer2 at location6 on plate3)
    (move agent1 ((walk location6 (gate1 gate3) location1)))
    (pickup-jammer > agent1 picks up jammer1 at location1)
    (move agent1 ((walk location1 (gate1 gate3) location5)))
    (jam-target > agent1 jams gate5 with jammer1 at location5 on plate2)
    ;; SG6 goal: through gate5-7, onto box2, jump to the slab, cross gate8/9, stairs to location11
    (move agent1 ((walk location5 (gate5 gate6 gate7) location10)))
    (move agent1 ((jump (location10 ground) nil (location10 box2))))
    (move agent1 ((jump (location10 box2) (edge1) (location12 ground))))
    (move agent1 ((walk location12 (gate8 gate9) location13)))
    (move agent1 ((stairs location13 (staircase1) location11)))))


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
                   (merge-pathnames "doc/problems/claustro-topo/Validation.txt"
                                    (asdf:system-source-directory :wouldwork))
                   :direction :output :if-exists :supersede)
    (format t ";;; claustro-topo -- complete validated solution, ~D actions.~%" (length *accepted-actions*))
    (format t ";;; SUCCESS-P T; GOAL-CHECKED-P T; GOAL-SATISFIED-P T; solution validators accepted~%")
    (format t ";;; To re-validate: (stage claustro-topo), then evaluate this form separately.~%")
    (format t "(validate-solution :verbose")
    (let ((*print-case* :downcase)
          (*print-pretty* nil))
      (loop for form in *accepted-actions*
            for index from 1
            do (format t "~%  ~S" (list index form))))
    (format t ")~2%Final state:~%")
    (display-validation-state (action-sequence-validation-final-state *accepted-validation*))))

;;; corner-topo -- accepted actions from the original start (cap 4, 18 actions).
(in-package :ww)

(defparameter *accepted-actions*
  '(;; Cap 4 (2026-11-16): the 14-step meet-in-the-middle plan of 2026-10-14 (found under
    ;; the old connect action), with each connect rewritten as select-termini +
    ;; put-connector.  Replaces the 19-action plan of 2026-11-10.
    ;; connector1 to location2 {receiver3 receiver1 transmitter2}
    (pickup-connector > agent1 picks up connector1 at location1)
    (move agent1 ((walk location1 nil location2)))
    (select-termini > agent1 selects (receiver3 receiver1 transmitter2) for connector1 at location2)
    (put-connector > agent1 puts connector1 on ground at location2)
    ;; connector2 to location3 {connector1 receiver1 transmitter1} opens gate1
    (pickup-connector > agent1 picks up connector2 at location2)
    (move agent1 ((walk location2 nil location3)))
    (select-termini > agent1 selects (connector1 receiver1 transmitter1) for connector2 at location3)
    (put-connector > agent1 puts connector2 on ground at location3)
    ;; connector3 carried through gate1 to location4 {transmitter1}
    (pickup-connector > agent1 picks up connector3 at location3)
    (select-termini > agent1 selects (transmitter1) for connector3 at location3)
    (move agent1 ((walk location3 (gate1) location4)))
    (put-connector > agent1 puts connector3 on ground at location4)
    ;; connector2 re-placed at location3 {connector1 receiver2 transmitter1}
    (move agent1 ((walk location4 (gate1) location3)))
    (pickup-connector > agent1 picks up connector2 at location3)
    (select-termini > agent1 selects (connector1 receiver2 transmitter1) for connector2 at location3)
    (put-connector > agent1 puts connector2 on ground at location3)
    ;; last walk west; lifting connector3 closes gate1 and turns connector1 blue
    (move agent1 ((walk location3 (gate1) location4)))
    (pickup-connector-retaining-pairings > agent1 picks up connector3 retaining pairings at location4)))


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
                   (merge-pathnames "doc/problems/corner-topo/Validation.txt"
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

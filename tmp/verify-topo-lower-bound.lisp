;;; Filename: verify-topo-lower-bound.lisp

;;; Throughput and value check for tech/topo-lower-bound.lisp.  Stage rumin-topo first, then
;;; (load "tmp/verify-topo-lower-bound.lisp") and call (verify-topo-lower-bound).  It prints
;;; the finite-resource, LM-cut and combined bounds at all 31 states of the known 30-action
;;; first-cycle prefix, then times 100 evaluations of each term on the start state.  The
;;; recorded values are in doc/problems/rumin-topo/rumin-topo-lower-bound.md.


(in-package :ww)


(defparameter *verify-topo-subgoal*
  '(and (has-location box1 location8)
        (has-location tray1 location2)
        (ghost-stops-recorder))
  "First subgoal of the five-cycle chain; the goal the recorded bounds were measured under.")


(defparameter *verify-topo-prefix*
  '(
    (START-RECORDER AGENT1)
    (MOVE AGENT1 ((WALK LOCATION3 NIL LOCATION2) (STAIRS LOCATION2 NIL LOCATION4) (WALK LOCATION4 NIL LOCATION13)))
    (PICKUP-CONNECTOR AGENT1 CONNECTOR1 LOCATION13)
    (MOVE AGENT1 ((WALK LOCATION13 NIL LOCATION4)))
    (MOVE AGENT1* ((WALK LOCATION3 NIL LOCATION2) (STAIRS LOCATION2 NIL LOCATION4) (WALK LOCATION4 NIL LOCATION13)))
    (PICKUP-CONNECTOR AGENT1* CONNECTOR1* LOCATION13)
    (CONNECT-CONNECTOR AGENT1* CONNECTOR1* LOCATION13 GROUND (RECEIVER1))
    (PICKUP-TRAY AGENT1* TRAY1* LOCATION13 LOCATION13)
    (MOVE AGENT1* ((WALK LOCATION13 NIL LOCATION4) (JUMP LOCATION4 NIL LOCATION2)))
    (CONNECT-CONNECTOR AGENT1 CONNECTOR1 LOCATION2 TRAY1* (CONNECTOR1* TRANSMITTER1))
    (MOVE AGENT1 ((WALK LOCATION4 NIL LOCATION13)))
    (PICKUP-TRAY AGENT1 TRAY1 LOCATION13 LOCATION13)
    (MOVE AGENT1 ((WALK LOCATION13 NIL LOCATION4) (JUMP LOCATION4 NIL LOCATION2) (WALK LOCATION2 (GATE1) LOCATION1)))
    (MOVE AGENT1* ((WALK LOCATION2 NIL LOCATION3)))
    (MOVE AGENT1 ((WALK LOCATION1 (GATE2) LOCATION6)))
    (PUT-TRAY AGENT1 TRAY1 PLATE1 LOCATION6)
    (MOVE AGENT1 ((WALK LOCATION6 (GATE3) LOCATION7)))
    (PICKUP-BOX AGENT1 BOX1 LOCATION7 LOCATION7)
    (MOVE AGENT1 ((WALK LOCATION7 (GATE3) LOCATION6)))
    (MOVE AGENT1 ((WALK LOCATION6 (GATE2) LOCATION8)))
    (PUT-BOX AGENT1 BOX1 GROUND LOCATION8)
    (MOVE AGENT1 ((WALK LOCATION8 (GATE2) LOCATION6)))
    (PICKUP-TRAY AGENT1 TRAY1 LOCATION6 LOCATION6)
    (MOVE AGENT1 ((WALK LOCATION6 (GATE2) LOCATION1)))
    (MOVE AGENT1* ((WALK LOCATION3 NIL LOCATION2)))
    (MOVE AGENT1 ((WALK LOCATION1 (GATE1) LOCATION2)))
    (PUT-TRAY AGENT1 TRAY1 GROUND LOCATION2)
    (PUT-TRAY AGENT1* TRAY1* GROUND LOCATION2)
    (MOVE AGENT1* ((WALK LOCATION2 NIL LOCATION3)))
    (STOP-RECORDER AGENT1*)
   )
  "The 30-action first cycle of doc/problems/rumin-topo/rumin-topo subgoal chain (5 cycles).lisp.")


(defun verify-topo-lower-bound ()
  "Print the bound table over the 30-action prefix, then time 100 evaluations of each term."
  (let ((states (verify-topo-prefix-states)))
    (format t "~&~%depth  finite  lm-cut  combined~%")
    (loop for state in states
          for depth from 0
          do (format t "~&~5D  ~6D  ~6D  ~8D~%"
                     depth
                     (topo-finite-resource-bound-for state *verify-topo-subgoal*)
                     (registered-relaxed-lm-cut-bound state *verify-topo-subgoal*)
                     (topo-lm-cut-resource-bound-for state *verify-topo-subgoal*))))
  (let ((state (copy-problem-state *start-state*)))
    (format t "~&~%100 x topo-relaxed-state-facts:~%")
    (topo-relaxed-state-facts state)
    (sb-ext:gc :full t)
    (time (dotimes (index 100) (topo-relaxed-state-facts state)))
    (format t "~&~%100 x topo-finite-resource-bound, the search precheck:~%")
    (topo-finite-resource-bound-for state *verify-topo-subgoal*)
    (sb-ext:gc :full t)
    (time (dotimes (index 100)
            (topo-finite-resource-bound-for state *verify-topo-subgoal*)))
    (format t "~&~%100 x topo-lm-cut-resource-bound, the aggregate min-steps-remaining? term:~%")
    (topo-lm-cut-resource-bound-for state *verify-topo-subgoal*)
    (sb-ext:gc :full t)
    (time (dotimes (index 100)
            (topo-lm-cut-resource-bound-for state *verify-topo-subgoal*)))))


(defun verify-topo-prefix-states ()
  "Replay the prefix one action at a time and collect the 31 states it passes through."
  (let ((states (list (copy-problem-state *start-state*))))
    (loop for length from 1 to (length *verify-topo-prefix*)
          for result = (validate-action-sequence
                         *start-state*
                         (subseq *verify-topo-prefix* 0 length))
          do (unless (action-sequence-validation-success-p result)
               (error "Prefix action ~D did not apply: ~S"
                      (action-sequence-validation-failure-index result)
                      (action-sequence-validation-failure-reason result)))
             (push (action-sequence-validation-final-state result) states))
    (nreverse states)))

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
    (format t "~&~%depth  finite  lm-cut  combined  remaining  admissible~%")
    (loop for state in states
          for depth from 0
          for remaining = (- (length *verify-topo-prefix*) depth)
          for combined = (topo-lm-cut-resource-bound-for state *verify-topo-subgoal*)
          do (format t "~&~5D  ~6D  ~6D  ~8D  ~9D  ~A~%"
                     depth
                     (topo-finite-resource-bound-for state *verify-topo-subgoal*)
                     (registered-relaxed-lm-cut-bound state *verify-topo-subgoal*)
                     combined
                     remaining
                     (if (<= combined remaining) "yes" "VIOLATION"))))
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


(defun verify-topo-agent-goal-task ()
  "Check that an agent's own location goal now contributes a repositioning MOVE.

AGENT1 starts at LOCATION3.  Before agent goals became routing tasks this goal scored the
session term alone; it must now also carry one MOVE, provided the static walking graph holds
any LOCATION3 -> LOCATION8 route.  The task line printed below names AGENT1 as both the
required and the only eligible agent."
  (let ((goal '(and (has-location agent1 location8)
                    (ghost-stops-recorder)))
        (state (copy-problem-state *start-state*)))
    (format t "~&~%Agent-location goal finite-resource bound = ~D~%"
            (topo-finite-resource-bound-for state goal))
    (report-topo-finite-resource-bound-analysis state goal)))


(defparameter *verify-topo-route-pairs*
  '((location3 location2) (location2 location1) (location1 location6)
    (location6 location7) (location6 location8) (location7 location8)
    (location3 location7) (location3 location8) (location13 location2))
  "Representative legs the chunk-1 routing tasks can use, for the route-family probe.")


(defun verify-topo-route-families ()
  "Print the open gates, the retained chunk-1 tasks, and the DNF prerequisites of each leg.

The barrier term under design reads exactly these three things: a leg is blocked when every
clause of its family contains a currently closed gate, and a gate is unavoidable for the leg
when it appears in every clause."
  (let ((routes (nth-value 2 (topo-resource-ensure-static-context)))
        (facts (topo-relaxed-state-facts (copy-problem-state *start-state*))))
    (format t "~&~%Open gates: ~S~%"
            (remove-if-not (lambda (fact) (eq (first fact) 'open)) facts))
    (format t "~&~%Controls records: ~S~%"
            (remove-if-not (lambda (fact) (eq (first fact) 'controls))
                           (topo-relaxed-static-propositions)))
    (format t "~&~%Recording sides: ~S~%"
            (let ((pairs nil))
              (maphash (lambda (object side) (push (list object side) pairs))
                       (topo-resource-recording-sides))
              pairs))
    (format t "~%")
    (report-topo-finite-resource-bound-analysis *start-state* *verify-topo-subgoal*)
    (format t "~&~%leg gate prerequisites, in DNF:~%")
    (dolist (pair *verify-topo-route-pairs*)
      (format t "~&  ~S -> ~S : ~S~%"
              (first pair) (second pair) (gethash pair routes)))))

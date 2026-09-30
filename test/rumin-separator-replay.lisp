;;; Filename: rumin-separator-replay.lisp

;;; Phase 1 acceptance for the separator-based traversal plan (claude/traversal-separator-
;;; plan.md): every kind of crossing rumin-topo authors, replayed as a hand-written MOVE.
;;; Load after Quickloading Wouldwork.  Each case starts from a copy of *START-STATE* with
;;; the agent placed at the crossing's source -- and a box or an open gate where the case
;;; needs one -- so a crossing is tested on its own rather than through a planned route.
;;; Expected NIL cases are the rules the separator form must still enforce.

(in-package :ww)
(stage rumin-topo)


(defparameter *rumin-separator-replay-cases*
  '(;; Stairs, the preferred clause, both ways.
    (t   ((has-location agent1 location17))
         ((stairs location17 (staircase2) location13)))
    (t   ((has-location agent1 location13))
         ((stairs location13 (staircase2) location17)))
    ;; Non-preferred jump clauses of the same fact (D6), and a walk the fact never offers.
    (t   ((has-location agent1 location13))
         ((jump location13 (edge3) location17)))
    (t   ((has-location agent1 location13))
         ((jump location13 (edge2) location17)))
    (nil ((has-location agent1 location13))
         ((walk location13 nil location17)))
    (nil ((has-location agent1 location13))
         ((jump location13 (edge1) location17)))
    ;; Staircase with a gate: the gate must be open, the staircase never matters.
    (t   ((has-location agent1 location10) (open gate4))
         ((stairs location10 (gate4 staircase3) location9)))
    (nil ((has-location agent1 location10))
         ((stairs location10 (gate4 staircase3) location9)))
    ;; Jumps over an edge: down is free, up is bounded by *vertical-reach-limit*.
    (t   ((has-location agent1 location9))
         ((jump location9 (edge4) location8)))
    (nil ((has-location agent1 location8))
         ((jump location8 (edge4) location9)))
    (t   ((has-location agent1 location4))
         ((jump location4 (edge1) location2)))
    (nil ((has-location agent1 location2))
         ((jump location2 (edge1) location4)))
    (t   ((has-location agent1 location2))
         ((stairs location2 (staircase1) location4)))
    ;; Directed climb.
    (t   ((has-location agent1 location14))
         ((ladder location14 (ladder2) location5)))
    ;; Support transition over an edge clause; a stairs clause offers no support landing.
    (t   ((has-location box1 location2) (has-location agent1 location2) (on agent1 box1))
         ((jump (location2 box1) (edge1) (location4 ground))))
    (nil ((has-location box1 location2) (has-location agent1 location2) (on agent1 box1))
         ((jump (location2 box1) (staircase1) (location4 ground)))))
  "(EXPECTED FACTS ROUTE) triples: whether (MOVE AGENT1 ROUTE) succeeds from *START-STATE*
   with FACTS added.")


(defun test-rumin-separator-replay ()
  "Replay every case and check the preferred grounded segments, then report PASS or signal
   an error listing every case that disagreed."
  (let ((failures nil))
    (dolist (case *rumin-separator-replay-cases*)
      (destructuring-bind (expected facts route) case
        (unless (eq expected (rumin-separator-move-succeeds-p facts route))
          (push case failures))))
    (unless (equal (mobility-provider-segments *start-state* 'agent1 'location13)
                   '((stairs location13 (staircase2) location17)
                     (walk location13 nil location4)))
      (push :location13-provider-segments failures))
    (unless (member '(stairs location2 (staircase1) location4)
                    (mobility-provider-segments *start-state* 'agent1 'location2)
                    :test #'equal)
      (push :location2-prefers-stairs failures))
    (when failures
      (error "~%RUMIN-SEPARATOR-REPLAY failed:~%~{  ~S~%~}" (nreverse failures)))
    (format t "~%RUMIN-SEPARATOR-REPLAY: PASS (~D cases)~%"
            (length *rumin-separator-replay-cases*))))


(defun rumin-separator-move-succeeds-p (facts route)
  "T when (MOVE AGENT1 ROUTE) applies to a copy of *START-STATE* extended with FACTS, else
   NIL.  A fluent fact replaces its prior value, as ADD-PROPOSITION does."
  (let ((state (copy-problem-state *start-state*)))
    (dolist (fact facts)
      (add-proposition fact (problem-state.idb state)))
    (and (nth-value 1 (apply-action-to-state (list 'move 'agent1 route) state nil))
         t)))


(test-rumin-separator-replay)

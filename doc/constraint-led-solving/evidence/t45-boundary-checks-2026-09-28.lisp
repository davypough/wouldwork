;;; T45 focused checks (Extractor-Specifications.md section 6.3).  Load, in WW, after
;;; tech/constraint-profile.lisp, tech/constraint-arrangement.lisp and
;;; tech/constraint-boundary.lisp; stage each problem before its checks.
;;;   rumin-topo:      (t45-run-rumin-checks)
;;;   windtunnel-topo: also load the T33 and T34 check files, then (t45-run-windtunnel-checks)
;;;   crelay-topo:     (t45-run-crelay-checks)       ; imports the T10 final checkpoint by replay
;;;   any other:       (t45-run-general-checks)      ; a problem without the recorder
;;; No search.  Reference states replay prefixes of retained validated paths.
(in-package :ww)


(defvar *t45-count* 0)


(defun t45-check (label value)
  (assert value () "T45 failed: ~A" label)
  (incf *t45-count*)
  (format t "~&PASS ~A~%" label))


(defun t45-path (relative)
  (merge-pathnames relative (asdf:system-source-directory :wouldwork)))


(defun t45-goal-test ()
  "The staged goal test.  Staging uninterns GOAL-FN (RESET-USER-SYMS), so a quoted symbol read
   when this file was loaded before staging would name the previous problem's goal."
  (symbol-function (find-symbol "GOAL-FN" :ww)))


(defun t45-prefix (actions count)
  (let ((validation (validate-action-sequence *start-state* (subseq actions 0 count))))
    (t45-check (list "prefix replays" *problem-name* count) (action-sequence-validation-success-p validation))
    (action-sequence-validation-final-state validation)))


(defun t45-result (state provenance event &rest options)
  (apply #'list :state state :provenance provenance :event event options))


(defun t45-preserving (scenario)
  "BT's result for SCENARIO, checking that the caller's state, scenario and static data are
   unchanged."
  (let* ((state (getf scenario :state))
         (snapshot (when (typep state 'problem-state) (arrangement-state-snapshot state)))
         (static (copy-tree (list-static-db)))
         (data (copy-tree scenario))
         (result (boundary-transition-result scenario)))
    (t45-check (list "caller scenario preserved" (getf scenario :event)) (equal data scenario))
    (when snapshot
      (t45-check (list "caller state preserved" (getf scenario :event))
                 (equal snapshot (arrangement-state-snapshot state))))
    (t45-check (list "static database preserved" (getf scenario :event)) (equal static (list-static-db)))
    result))


(defun t45-row (result key object)
  (find object (getf result key) :key #'first))


(defun t45-relation (result key relation)
  (find relation (getf result key) :key #'first))


(defun t45-route (result agent)
  (find agent (getf result :routes) :key (lambda (row) (getf row :agent))))


(defun t45-verdicts (result)
  (mapcar #'fourth (getf result :obligations)))


(defun t45-boundary-agreement (actions index kind agent)
  "At a recorder boundary INDEX of ACTIONS: prerequisites met, the engine agrees, the
   hypothetical closure agrees, and the successor equals the replayed next state."
  (let* ((before (t45-prefix actions (1- index)))
         (after (t45-prefix actions index))
         (result (t45-preserving (t45-result before (format nil "replay prefix ~D" (1- index))
                                             (list kind agent)))))
    (t45-check (list "boundary evaluated" *problem-name* index) (eq (getf result :status) :evaluated))
    (t45-check (list "itemized prerequisites met and engine applies" index)
               (and (getf (getf result :prerequisites) :met) (getf (getf result :prerequisites) :engine)))
    (t45-check (list "effects ENGINE, hypothetical closure agrees" index)
               (and (eq (getf result :effects) :engine) (getf result :closure-agrees)))
    (t45-check (list "successor equals replayed next state" index)
               (equal (arrangement-facts (getf result :successor)) (arrangement-facts after)))
    result))


;;;; RUMIN-TOPO ;;;;


(defun t45-rumin-historical ()
  (with-open-file (in (t45-path "doc/problems/rumin-topo/rumin-topo solution (91 steps).lisp"))
    (rest (read in))))


(defun t45-rumin-normalized (actions)
  "Only the argument order of CONNECT-CONNECTOR and PUT-CONNECTOR phrases changes: the old
   (agent connector location place [termini]) becomes the current effect-variable order."
  (mapcar (lambda (form)
            (case (first form)
              (connect-connector (destructuring-bind (agent connector location place termini) (rest form)
                                   (list 'connect-connector agent connector place termini location)))
              (put-connector (destructuring-bind (agent connector location place) (rest form)
                               (list 'put-connector agent connector place location)))
              (t form)))
          actions))


(defun t45-rumin-trace-checks ()
  (let* ((historical (t45-rumin-historical))
         (normalized (t45-rumin-normalized historical))
         (old (validate-action-sequence *start-state* historical))
         (new (validate-action-sequence *start-state* normalized :goal-test (t45-goal-test))))
    (t45-check "historical trace has 91 actions" (= 91 (length historical)))
    (t45-check "historical phrases fail under current semantics at action 7 (argument order)"
               (and (not (action-sequence-validation-success-p old))
                    (= 7 (action-sequence-validation-failure-index old))))
    (t45-check "normalization changes only argument order"
               (every (lambda (a b)
                        (and (eq (first a) (first b))
                             (equal (sort (mapcar #'prin1-to-string (rest a)) #'string<)
                                    (sort (mapcar #'prin1-to-string (rest b)) #'string<))))
                      historical normalized))
    (t45-check "normalization touches only connect/put-connector"
               (every (lambda (a b) (or (equal a b) (member (first a) '(connect-connector put-connector))))
                      historical normalized))
    (t45-check "normalized trace replays all 91 actions to the goal"
               (and (action-sequence-validation-success-p new) (action-sequence-validation-goal-satisfied-p new)))
    normalized))


(defun t45-rumin-stop-checks (actions)
  "A2: the final STOP, with obligations."
  (let* ((result (t45-boundary-agreement actions 91 :stop 'agent1*))
         (obligations '((:fact (open gate5) :phase :until-event :purpose "route through gate5")
                        (:fact (open gate6) :phase :across :purpose "gate6 behind agent1")
                        (:body tray1 :role (:weight plate4) :phase :across :purpose "live plate support")
                        (:fact (has-location agent1 location16) :phase :after :purpose "goal place")))
         (with (t45-preserving (t45-result (t45-prefix actions 90) "rumin replay prefix 90" '(:stop agent1*)
                                           :obligations obligations)))
         (devices (getf with :devices)))
    (t45-check "gate5 open lost" (member '(open gate5) (second (t45-relation result :devices 'open)) :test #'equal))
    (t45-check "gate6 open kept" (member '(open gate6) (arrangement-facts (getf result :successor)) :test #'equal))
    (t45-check "receiver2 physically active before, dark after"
               (equal (cdr (t45-row result :receivers 'receiver2)) '((t nil) (nil nil))))
    (t45-check "plate3 held by ghost tray1* before, empty after"
               (equal (cdr (t45-row result :plates 'plate3)) '(((tray1* :ghost)) nil (t t) (nil nil))))
    (t45-check "plate4 held by live tray1 before and after"
               (equal (subseq (t45-row result :plates 'plate4) 1 3) '(((tray1 :live)) ((tray1 :live)))))
    (t45-check "red-chain ghost pairings removed"
               (subsetp '((paired connector1* transmitter2) (paired connector1* connector2*) (paired connector1 connector2*))
                        (second (t45-relation result :pairings 'paired)) :test #'equal))
    (t45-check "ghost attribution names the removed ghost objects"
               (equal (getf result :ghosts) '(box1* connector1* connector2* tray1*)))
    (t45-check "gate5 physical primitives withdrawn"
               (and (find 'plate3 (getf result :primitives) :key #'first)
                    (find 'receiver2 (getf result :primitives) :key #'first)))
    (t45-check "tray1 chain on plate4 retained" (eq :retained (second (t45-row result :chains 'tray1))))
    (t45-check "ghost agent removed" (eq :removed (getf (t45-route result 'agent1*) :status)))
    (t45-check "agent1 loses gate5 passage, keeps its place"
               (and (equal (getf (t45-route result 'agent1) :passage) '((gate5 t nil)))
                    (equal (getf (t45-route result 'agent1) :locations) '(location16 location16))))
    (t45-check "agent1's lost arcs all name gate5"
               (and (getf (t45-route result 'agent1) :arcs-lost)
                    (every (lambda (arc) (member 'gate5 (alexandria:flatten (fourth arc))))
                           (getf (t45-route result 'agent1) :arcs-lost))))
    (t45-check "obligations: EXPENDED, SURVIVES, SURVIVES, MET"
               (equal (t45-verdicts with) '(:expended :survives :survives :met)))
    (t45-check "obligations do not change the consequences" (equal devices (getf result :devices)))
    (t45-check "report prints" (search "EXPENDED" (with-output-to-string (*standard-output*)
                                                   (report-boundary-transition
                                                     (t45-result (t45-prefix actions 90) "rumin replay prefix 90"
                                                                 '(:stop agent1*) :obligations obligations)))))
    result))


(defun t45-rumin-cancel-checks (actions stop)
  "A3: CANCEL at the same state: not met, hypothetical, same physical losses."
  (let* ((state (t45-prefix actions 90))
         (result (t45-preserving (t45-result state "rumin replay prefix 90" '(:cancel agent1))))
         (rows (getf (getf result :prerequisites) :rows)))
    (t45-check "cancel evaluated" (eq (getf result :status) :evaluated))
    (t45-check "cancel prerequisites NOT MET; engine refuses; agreement"
               (and (not (getf (getf result :prerequisites) :met)) (not (getf (getf result :prerequisites) :engine))
                    (getf (getf result :prerequisites) :agrees)))
    (t45-check "the unmet row is agent1 at a recorder"
               (equal (mapcar #'first (remove-if #'second rows)) '("agent1 at a recorder")))
    (t45-check "effects HYPOTHETICAL" (eq (getf result :effects) :hypothetical))
    (t45-check "hypothetical closure differs from STOP only in stopped-by-ghost"
               (equal (set-exclusive-or (arrangement-facts (getf result :successor))
                                        (arrangement-facts (getf stop :successor)) :test #'equal)
                      '((recorder-cycle-stopped-by-ghost))))
    (t45-check "same physical losses as STOP" (equal (getf result :primitives) (getf stop :primitives)))))


(defun t45-rumin-support-checks (actions)
  "A4 and A5: the tray release with a live rider, and both closures at the same state."
  (let* ((state (t45-prefix actions 49))
         (release (t45-preserving (t45-result state "rumin replay prefix 49" '(:action (put-tray agent1* tray1* ground location2)))))
         (stop (t45-preserving (t45-result state "rumin replay prefix 49" '(:stop agent1*))))
         (cancel (t45-preserving (t45-result state "rumin replay prefix 49" '(:cancel agent1)))))
    (t45-check "release evaluated with ENGINE effects"
               (and (eq (getf release :status) :evaluated) (eq (getf release :effects) :engine)))
    (t45-check "release successor equals replayed action 50"
               (equal (arrangement-facts (getf release :successor)) (arrangement-facts (t45-prefix actions 50))))
    (t45-check "connector1 lands on live box1 by the engine's settling"
               (equal (cdr (t45-row release :chains 'connector1))
                      (list :changed '((on tray1*) (held agent1*) (ground location2)) '((on box1) (ground location2)) 3/2 1)))
    (t45-check "tray1 on plate3 retained" (eq :retained (second (t45-row release :chains 'tray1))))
    (t45-check "pairings kept through the release" (null (getf release :pairings)))
    (t45-check "stop NOT MET, hypothetical" (and (not (getf (getf stop :prerequisites) :met)) (eq (getf stop :effects) :hypothetical)))
    (t45-check "stop rows name agent1*'s place, hands and the cross-layer ON"
               (equal (mapcar #'first (remove-if #'second (getf (getf stop :prerequisites) :rows)))
                      '("ghost agent1* at a recorder" "ghost agent1* empty-handed" "no live/ghost HOLDING or ON dependency")))
    (t45-check "cross-layer detail names connector1 on tray1*"
               (search "(on connector1 tray1*)" (third (car (last (getf (getf stop :prerequisites) :rows))))))
    (t45-check "engine agrees on both refusals"
               (and (getf (getf stop :prerequisites) :agrees) (getf (getf cancel :prerequisites) :agrees)))
    (t45-check "cancel NOT MET, hypothetical" (and (not (getf (getf cancel :prerequisites) :met)) (eq (getf cancel :effects) :hypothetical)))
    (t45-check "cancel: connector1 loses its support and rests on the ground, no catch"
               (equal (fourth (t45-row cancel :chains 'connector1)) '((ground location2))))))


(defun t45-rumin-agreement-checks (actions)
  "A7: every STOP in the replayed rumin trajectory."
  (loop for form in actions
        for index from 1
        when (eq (first form) 'stop-recorder)
          do (t45-boundary-agreement actions index :stop (second form))))


(defun t45-unresolved (label scenario expected)
  (let ((result (t45-preserving scenario)))
    (t45-check (list "UNRESOLVED" label) (eq (getf result :status) :unresolved))
    (t45-check (list "reason" label) (search expected (getf result :reason)))
    (t45-check (list "only its reason" label)
               (null (set-difference (loop for (key) on result by #'cddr collect key)
                                     '(:status :reason :event :kind :provenance :prerequisites :effects :stage))))
    result))


(defun t45-rumin-unresolved-checks (actions)
  "A8 on rumin: every input reason, scope limits, and an inapplicable action."
  (let* ((state (t45-prefix actions 90))
         (unsettled (%copy-problem-state state t))
         (marked (%copy-problem-state state t)))
    (revise (problem-state.idb unsettled) '((not (open gate6))))
    (revise (problem-state.idb marked) '((inconsistent-state)))
    (t45-unresolved "no scenario" nil "missing problem-state")
    (t45-unresolved "no provenance" (list :state state :event '(:stop agent1*)) "provenance missing")
    (t45-unresolved "unsettled state" (t45-result unsettled "edited copy" '(:stop agent1*)) "fixed point")
    (t45-unresolved "marked state" (t45-result marked "edited copy" '(:stop agent1*)) "inconsistent")
    (t45-unresolved "malformed event" (t45-result state "x" '(:close agent1*)) "malformed")
    (t45-unresolved "unknown action" (t45-result state "x" '(:action (fly agent1))) "no action named")
    (t45-unresolved "stop names no agent" (t45-result state "x" '(:stop tray1)) "one agent")
    (t45-unresolved "no open cycle" (t45-result (t45-prefix actions 91) "x" '(:stop agent1*)) "no recording cycle")
    (t45-unresolved "bad agents" (t45-result state "x" '(:stop agent1*) :agents '(tray1)) ":AGENTS")
    (t45-unresolved "bad obligation" (t45-result state "x" '(:stop agent1*) :obligations '((:fact (open gate6) :phase :later)))
                    "phase")
    (t45-unresolved "two subjects" (t45-result state "x" '(:stop agent1*)
                                               :obligations '((:fact (open gate6) :reach (agent1 location16) :phase :after)))
                    "exactly one")
    (t45-unresolved "move without support change"
                    (t45-result state "x" '(:action (move agent1* ((walk location3 nil location2)))))
                    "outside T45")
    (let ((result (t45-unresolved "inapplicable action" (t45-result state "x" '(:action (put-tray agent1 tray1 ground location2)))
                                  "not applicable")))
      (t45-check "inapplicable action still reports its prerequisite refusal"
                 (not (getf (getf result :prerequisites) :engine))))
    (t45-check "STOP given as an action takes the STOP kind"
               (eq :stop (getf (boundary-transition-result (t45-result state "x" '(:action (stop-recorder agent1*)))) :kind)))))


(defun t45-run-rumin-checks ()
  (let* ((actions (t45-rumin-trace-checks))
         (stop (t45-rumin-stop-checks actions)))
    (t45-rumin-cancel-checks actions stop)
    (t45-rumin-support-checks actions)
    (t45-rumin-agreement-checks actions)
    (t45-rumin-unresolved-checks actions)
    (format t "~&T45 RUMIN CHECKS PASSED: ~D~%" *t45-count*)))


;;;; WINDTUNNEL-TOPO ;;;;


(defun t45-windtunnel-actions ()
  '((start-recorder agent1)
    (pickup-connector agent1* connector1* location1)
    (move agent1* ((walk location1 nil location2)))
    (move agent1* ((step (location2 ground) nil (location2 plate1))))
    (move agent1* ((step (location2 plate1) nil (location2 ground))))
    (pickup-connector agent1 connector1 location1)
    (connect-connector agent1 connector1 ground (repeater1 transmitter1) location1)
    (move agent1 ((walk location1 nil location2)))
    (move agent1 ((step (location2 ground) nil (location2 plate1))))
    (move agent1 ((step (location2 plate1) nil (location2 ground))))
    (move agent1 ((walk location2 (blower1) location3)))
    (move agent1 ((walk location3 (blower1) location4)))
    (move agent1* ((step (location2 ground) nil (location2 plate1))))
    (move agent1* ((step (location2 plate1) nil (location2 ground))))
    (move agent1* ((walk location2 (blower1) location3)))
    (connect-connector agent1* connector1* ground (repeater1 receiver1) location3)
    (move agent1 ((walk location4 (gate2) location5)))))


(defun t45-windtunnel-closure-checks (result label)
  (t45-check (list label "NOT MET, engine agrees, HYPOTHETICAL")
             (and (eq (getf result :status) :evaluated) (not (getf (getf result :prerequisites) :met))
                  (getf (getf result :prerequisites) :agrees) (eq (getf result :effects) :hypothetical)))
  (t45-check (list label "connector1* removed") (eq :removed (second (t45-row result :chains 'connector1*))))
  (t45-check (list label "receiver1 physical ACTIVE lost")
             (equal (second (t45-relation result :devices 'active)) '((active receiver1))))
  (t45-check (list label "gate2 physical OPEN lost")
             (member '(open gate2) (second (t45-relation result :devices 'open)) :test #'equal))
  (t45-check (list label "agent1 loses gate2 in its own view")
             (member '(gate2 t nil) (getf (t45-route result 'agent1) :passage) :test #'equal)))


(defun t45-run-windtunnel-checks ()
  (let* ((actions (t45-windtunnel-actions))
         (final (t45-prefix actions 17))
         (cancel (t45-preserving (t45-result final "windtunnel validated trace, 17 actions" '(:cancel agent1))))
         (stop (t45-preserving (t45-result final "windtunnel validated trace, 17 actions" '(:stop agent1*)))))
    (t45-check "validated trace still reaches the goal"
               (action-sequence-validation-goal-satisfied-p
                 (validate-action-sequence *start-state* actions :goal-test (t45-goal-test))))
    (t45-check "views differ before closure: receiver1 physically active, not recording-active"
               (equal (second (t45-row cancel :receivers 'receiver1)) '(t nil)))
    (t45-windtunnel-closure-checks cancel "cancel")
    (t45-windtunnel-closure-checks stop "stop")
    (let* ((arrangement (check-relay-arrangement
                          (t34-scenario (t34-wind-state t nil) :open 'receiver1
                                        '(transmitter1 connector1 repeater1 connector1* receiver1))))
           (closed (t45-preserving (t45-result (getf arrangement :settled-state) "T34 stable mixed-view arrangement"
                                               '(:cancel agent1)))))
      (t45-check "T34 arrangement stable and beam working" (eq (getf arrangement :status) :stable-and-beam-working))
      (t45-windtunnel-closure-checks closed "T34 arrangement closed"))
    (let ((toggle (t45-preserving (t45-result (t45-prefix actions 8) "windtunnel prefix 8"
                                              '(:action (move agent1 ((step (location2 ground) nil (location2 plate1)))))))))
      (t45-check "step onto plate1 is a support change with ENGINE effects"
                 (and (eq (getf toggle :status) :evaluated) (eq (getf toggle :effects) :engine)
                      (equal (third (t45-relation toggle :supports 'on)) '((on agent1 plate1)))))
      (t45-check "successor equals replayed action 9"
                 (equal (arrangement-facts (getf toggle :successor)) (arrangement-facts (t45-prefix actions 9))))
      (t45-check "physical view toggles: plate1 latch, gate1, blower1"
                 (and (find 'plate1 (getf toggle :primitives) :key #'first)
                      (t45-relation toggle :devices 'open) (t45-relation toggle :devices 'turning)))
      (t45-check "no recording-view fact changes"
                 (notany (lambda (change) (search "RECORDING" (symbol-name (first change)))) (getf toggle :devices)))
      (t45-check "ghost route conditions unchanged in its own view"
                 (let ((ghost (t45-route toggle 'agent1*)))
                   (and (null (getf ghost :passage)) (null (getf ghost :arcs-lost)) (null (getf ghost :arcs-gained)))))
      (t45-check "live agent's route conditions change in its view"
                 (getf (t45-route toggle 'agent1) :passage)))
    (format t "~&T45 WINDTUNNEL CHECKS PASSED: ~D~%" *t45-count*)))


;;;; CRELAY-TOPO ;;;;


(defun t45-crelay-actions ()
  (let ((checkpoint (import-search-checkpoint
                      (t45-path "doc/problems/crelay-topo/constraint-evidence/t10-final-checkpoint.txt"))))
    (normalize-validation-actions
      (goal-chain-cumulative-path (goal-chain-session-phases (search-checkpoint-session checkpoint))))))


(defun t45-run-crelay-checks ()
  (let ((actions (t45-crelay-actions)))
    (t45-check "crelay final path has 87 actions" (= 87 (length actions)))
    (loop for form in actions
          for index from 1
          when (member (first form) '(stop-recorder cancel-playback))
            do (t45-boundary-agreement actions index
                                       (if (eq (first form) 'stop-recorder) :stop :cancel)
                                       (find-if (lambda (argument) (member argument (census-type-instances 'agent)))
                                                (rest form))))
    (t45-check "a CANCEL phrase with connectives is read as a CANCEL"
               (eq :cancel (getf (boundary-transition-result
                                   (t45-result (t45-prefix actions 10) "crelay prefix 10" (list :action (nth 10 actions))))
                                 :kind)))
    (format t "~&T45 CRELAY CHECKS PASSED: ~D~%" *t45-count*)))


;;;; ANY PROBLEM WITHOUT THE RECORDER ;;;;


(defun t45-run-general-checks ()
  (t45-check "problem has no recorder" (not (boundary-recorder-p)))
  (t45-unresolved (list "no recorder" *problem-name*)
                  (t45-result (copy-problem-state *start-state*) "start state" '(:stop agent1))
                  "not spliced")
  (format t "~&T45 GENERAL CHECKS PASSED: ~D~%" *t45-count*))

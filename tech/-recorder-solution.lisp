;;; Filename: -recorder-solution.lisp

;;; Solution-time recorder services: candidate validation and the two-phase report.  Nested
;;; by recorder.lisp, which makes the services available; an integrated recorder problem
;;; activates both with ENABLE-RECORDER-SOLUTION.  Nothing here is a substrate hook, and
;;; nothing here reads recording-side state.  The recorder shadow components derive
;;; RECORDING-DEPRESSED, RECORDING-LATCHED, RECORDING-TURNING, RECORDING-ACTIVE and
;;; RECORDING-OPEN during propagation, while this file runs once per completed candidate
;;; path and touches none of them.  What it does read is identity from -recorder-core --
;;; LIVE-RECORDING-OBJECT and GHOST-RECORDING-OBJECT -- plus location, position, and the
;;; walking closure.
;;;
;;; A recording is closed by walking back to a recorder and stopping it, which gives a
;;; problem two sensible places to stop searching.  The default is to stop when the problem's
;;; own goal is met, leaving the ghost wherever its last useful action left it; the recording
;;; is still performable because the ghost can walk back afterward, and the report supplies
;;; that return trip.  A problem that would rather see the return spelled out in the solution
;;; path adds GHOST-STOPS-RECORDER as a goal conjunct and pays the extra actions.
;;; VALIDATE-RECORDER-SOLUTION enforces only the weaker of the two -- a validator vetoes
;;; every candidate, so it must admit both styles -- and RECORDING-AGENT-CAN-CLOSE is that
;;; weaker rule.  A ghost already standing on a recorder satisfies it trivially, so the
;;; conjunct strengthens the goal without ever contradicting the validator.
;;;
;;; REQUIRES:
;;;   nested : -location ((has-location ...)); -position (recorder has-position role);
;;;            -walkability (walkable-locations -- the identity default reduces
;;;            RECORDING-AGENT-CAN-CLOSE to standing on the recorder, which is the right
;;;            reading for a problem with no walking technology)
;;;   soft   : -recorder-core's identity queries, assembled by recorder.lisp
;;; PROVIDES:
;;;   queries  : ghost-stops-recorder (optional goal conjunct), recording-agent-can-close,
;;;              recording-agent-return-route, recording-agent-at-recorder
;;;   functions: enable-recorder-solution (activates validation and reporting together),
;;;              validate-recorder-solution, build-recorder-report, print-recorder-report

(include-tech -location)
(include-tech -position)
(include-tech -walkability)

(in-package :ww)


(define-query ghost-stops-recorder ()
  ;; Optional goal conjunct.  Every mapped ghost agent has walked back to a recorder, so the
  ;; return trip appears in the solution path and its length counts toward min-length.  A
  ;; problem that omits this conjunct stops at its own goal and lets the report supply the
  ;; return instead.  Place it after the problem's own goal literals: the conjunction is
  ;; evaluated in order, so this walks the ghost roster only on states that already qualify.
  (forall (?agent agent)
    (or (not (ghost-recording-object ?agent))
        (recording-agent-at-recorder ?agent))))


(define-query recording-agent-can-close (?agent agent)
  ;; The recording remains closable: some recorder's location lies in ?agent's current
  ;; walking closure, which for a ghost is computed against recording-side gate and gears
  ;; state.  An agent resting on a support steps off before walking, and a step-off is not
  ;; itself a walking obstacle, so support occupancy is not consulted here.
  (do (bind (has-location ?agent $agent-location))
      (assign $reachable (walkable-locations ?agent $agent-location))
      (exists (?recorder recorder)
        (exists (?location location)
          (and (has-position ?recorder ?location)
               (member ?location $reachable))))))


(define-query recording-agent-return-route (?agent agent)
  ;; (?agent from to) for the walk that closes ?agent's recording, or nil when ?agent
  ;; already stands on a recorder and no return trip is outstanding.  Consumed by the
  ;; report, which appends the walk a goal-terminated search stopped short of.
  (do (bind (has-location ?agent $agent-location))
      (assign $outstanding (not (recording-agent-at-recorder ?agent)))
      (assign $reachable (walkable-locations ?agent $agent-location))
      (assign $route nil)
      (doall (?recorder recorder)
        (doall (?location location)
          (if (and $outstanding
                   (not $route)
                   (has-position ?recorder ?location)
                   (member ?location $reachable))
            (assign $route (list ?agent $agent-location ?location)))))
      $route))


(define-query recording-agent-at-recorder (?agent agent)
  (exists (?recorder recorder)
    (exists (?location location)
      (and (has-position ?recorder ?location)
           (has-location ?agent ?location)))))

;;;; CANDIDATE VALIDATION AND TWO-PHASE REPORT ;;;;


(defun enable-recorder-solution ()
  "Activate recorder candidate validation and two-phase solution reporting for the staged
   problem."
  (register-solution-validator 'validate-recorder-solution)
  (register-solution-report-printer 'print-recorder-report))


(defun recorder-move-agents (move)
  "The agents named among MOVE's action arguments, or NIL when MOVE is not an
   (index (action argument...)) pair at all."
  (when (and (listp move)
             (= (length move) 2)
             (listp (second move)))
    (remove-duplicates
      (remove-if-not
        (lambda (argument)
          (member argument (gethash 'agent *types*)))
        (rest (second move))))))


(defun recorder-report-agent (move)
  "Return the single agent named by a recorded solution MOVE."
  (let ((agents (recorder-move-agents move)))
    (unless (= (length agents) 1)
      (error "Recorder report move must name exactly one agent: ~S" move))
    (first agents)))


(defun recorder-report-agent-side (state agent)
  "Classify AGENT through RECORDING-COPY>, using STATE for the compiled queries."
  (cond
    ((funcall (symbol-function 'live-recording-object) state agent)
     :live)
    ((funcall (symbol-function 'ghost-recording-object) state agent)
     :ghost)
    (t
     (error "Recorder report agent is not mapped by RECORDING-COPY>: ~S" agent))))


(defun recorder-report-move-side (state move)
  (recorder-report-agent-side state (recorder-report-agent move)))


(defun recorder-path-moves-on-side (state integrated-path side)
  "Return the moves performed by SIDE in INTEGRATED-PATH."
  (remove-if-not
    (lambda (move)
      (eql side (recorder-report-move-side state move)))
    integrated-path))


(defun recorder-recording-agents (state)
  "Return the mapped ghost agents that can act during recording."
  (remove-if-not
    (lambda (agent)
      (funcall (symbol-function 'ghost-recording-object) state agent))
    (gethash 'agent *types*)))


(defun recorder-action-failure-diagnostic (phase validation)
  (list :phase phase
        :reason :action-failed
        :step (action-sequence-validation-failure-index validation)
        :action (action-sequence-validation-failure-action validation)
        :detail (action-sequence-validation-failure-reason validation)))


(defun validate-recorder-solution (start-state integrated-path goal-state)
  "Validate recording and playback under the recorder's snapshot-reset semantics.

The recording is the ghost-only subsequence replayed from START-STATE.  Every mapped ghost
agent must still be able to reach a recorder when it ends -- the weaker of the two
termination rules, so that a search stopping at the problem's own goal and a search
carrying GHOST-STOPS-RECORDER as a goal conjunct are both admissible.  Playback then
restores START-STATE and replays the complete integrated path under the ordinary action
rules."
  (declare (ignore goal-state))
  (let* ((ghost-path
           (recorder-path-moves-on-side start-state integrated-path :ghost))
         (recording-validation
           (validate-action-sequence start-state ghost-path)))
    (unless (action-sequence-validation-success-p recording-validation)
      (return-from validate-recorder-solution
        (values nil
                (recorder-action-failure-diagnostic
                  :recording recording-validation))))
    (let* ((recording-state
             (action-sequence-validation-final-state recording-validation))
           (recording-agents (recorder-recording-agents recording-state))
           (stranded-agents
             (remove-if
               (lambda (agent)
                 (funcall (symbol-function 'recording-agent-can-close)
                          recording-state agent))
               recording-agents)))
      (when (null recording-agents)
        (return-from validate-recorder-solution
          (values nil '(:phase :recording :reason :no-recording-agent))))
      (when stranded-agents
        (return-from validate-recorder-solution
          (values nil
                  (list :phase :recording
                        :reason :agents-cannot-close
                        :agents stranded-agents)))))
    (let ((playback-validation
            (validate-action-sequence
              start-state integrated-path
              :goal-test (symbol-function 'goal-fn))))
      (unless (action-sequence-validation-success-p playback-validation)
        (return-from validate-recorder-solution
          (values nil
                  (recorder-action-failure-diagnostic
                    :playback playback-validation))))
      (unless (action-sequence-validation-goal-satisfied-p playback-validation)
        (return-from validate-recorder-solution
          (values nil '(:phase :playback :reason :goal-not-satisfied))))
      (values t nil))))


(defun recorder-recording-sequence (state integrated-path)
  "Extract ghost moves, replace each live-action block with one PAUSE marker, and close with
whatever return walk the searched path stopped short of."
  (let ((sequence (list '(start-recorder)))
        (previous-side nil))
    (dolist (move integrated-path)
      (let ((side (recorder-report-move-side state move)))
        (when (and (eql side :live)
                   (not (eql previous-side :live)))
          (setf sequence (nconc sequence (list '(pause)))))
        (when (eql side :ghost)
          (setf sequence (nconc sequence (list move))))
        (setf previous-side side)))
    (nconc sequence (recorder-return-walks state) (list '(stop-recorder)))))


(defun recorder-return-walks (state)
  "Return one (WALK agent from to) marker per ghost agent still away from a recorder.

STATE is the completed integrated state.  A ghost's location there, and the recording-side
gate and gears state its walking closure is computed against, are the same as at the end of
the ghost-only recording, so no second replay is needed.  A path that already carries the
return -- the GHOST-STOPS-RECORDER goal style -- yields no markers.  These are report
markers, not planner actions, and carry no step number for that reason."
  (loop for agent in (recorder-recording-agents state)
        for route = (funcall (symbol-function 'recording-agent-return-route) state agent)
        when route
          collect (cons 'walk route)))


(defun recorder-playback-sequence (state integrated-path)
  "Retain the integrated moves, pausing live blocks and resuming following ghost blocks."
  (let ((sequence nil)
        (previous-side nil))
    (dolist (move integrated-path sequence)
      (let ((side (recorder-report-move-side state move)))
        (when (not (eql side previous-side))
          (cond
            ((eql side :live)
             (setf sequence (nconc sequence (list '(pause)))))
            ((eql previous-side :live)
             (setf sequence (nconc sequence (list '(resume)))))))
        (setf sequence (nconc sequence (list move)))
        (setf previous-side side)))))


(defun build-recorder-report (&optional (solution (first *solution-paths*)))
  "Build recording/playback sequences for a completed integrated SOLUTION.

The returned plist retains the original path under :INTEGRATED and provides the derived
sequences under :RECORDING and :PLAYBACK.  Report markers are not planner actions."
  (unless solution
    (error "No completed solution is available for a recorder report."))
  (unless (solution-p solution)
    (error "Recorder report requires a SOLUTION, not ~S" solution))
  (let ((path (solution.path solution))
        (state (solution.goal solution)))
    (list :integrated path
          :recording (recorder-recording-sequence state path)
          :playback (recorder-playback-sequence state path))))


(defun print-recorder-report
    (&optional (solution (first *solution-paths*)) (stream *standard-output*))
  "Print and return the two-phase report derived from SOLUTION."
  (let ((report (build-recorder-report solution)))
    (format stream "~&~%Recording phase:~%")
    (dolist (entry (getf report :recording))
      (format stream "~S~%" entry))
    (format stream "~&Playback phase:~%")
    (dolist (entry (getf report :playback))
      (format stream "~S~%" entry))
    report))

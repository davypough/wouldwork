;;; Filename: recorder.lisp

;;; Recorder technology substrate: explicit identity between each live movable object and
;;; the ghost that replays its recording.  RECORDING-COPY> is directional and functional
;;; from live object to ghost.  Initialization validation additionally makes the relation
;;; one-to-one, keeps the two sides disjoint, and requires both members of a pair to share a
;;; MOBILE-OBJECT leaf category (agent-to-agent, connector-to-connector, and so on).
;;;
;;; The mapping is authoritative, not exhaustive over MOBILE-OBJECT.  MOBILE-OBJECT denotes
;;; a capability: an instance such as a welded wall fan can belong to a mobile-capable leaf
;;; type while remaining fixed in this problem.  An unmapped object is therefore neither a
;;; live recording object nor a ghost recording object.
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
;;;   nested : -location (mobile-object); -position (recorder has-position role and plate
;;;            types); -support-occupancy (on); -propagation; -interaction-policy
;;;            (neutral action hooks); -recording-shadow-policy (neutral state-view hooks);
;;;            -controls (controller wiring and receiver beam substrate); -gate
;;;            (actor-aware gate view); -walkability (walkable-locations -- the identity
;;;            default reduces RECORDING-AGENT-CAN-CLOSE to standing on the recorder, which
;;;            is the right reading for a problem with no walking technology)
;;; PROVIDES:
;;;   type     : recorder (optional)
;;;   relation : recording-copy> (live mobile-object -> ghost mobile-object)
;;;              recording-depressed, recording-latched, recording-turning,
;;;              recording-active, recording-open
;;;   queries  : live-recording-object, ghost-recording-object, same-recording-side;
;;;              ghost-stops-recorder (optional goal conjunct), recording-agent-can-close,
;;;              recording-agent-return-route, recording-agent-at-recorder;
;;;              recorder overrides object-manipulation-allowed, support-use-allowed,
;;;              connector-pairing-allowed, recording-shadow-object, and
;;;              recording-shadow-turning, recording-shadow-gate-open
;;;   updates  : update-recording-plate-status!, update-recording-receiver-status!,
;;;              update-recording-gate-status!, update-recording-gears-status!
;;;   functions: validate-recorder-solution, build-recorder-report,
;;;              print-recorder-report (registered as the problem's post-solution report
;;;              printer)
;;;
;;; The recording shadow covers the Windtunnel controls: plates, wall gears, gates, and
;;; direct or relay-fed gate receivers.  Recording beam evaluation excludes mapped live
;;; objects and uses recording-side gate transparency.  Beam crossings and jamming remain
;;; outside this Windtunnel-scoped model.  There are no searched recorder controls.

(include-tech -location)
(include-tech -position)
(include-tech -support-occupancy)
(include-tech -propagation)
(include-tech -interaction-policy)
(include-tech -recording-shadow-policy)
(include-tech -controls)
(include-tech -gate)
(include-tech -walkability)

(in-package :ww)


(define-optional-types recorder connector wall-gears gate receiver)


(define-static-relations
  (recording-copy> mobile-object $mobile-object))


(define-dynamic-relations
  (recording-depressed plate)
  (recording-latched toggle-plate)
  (recording-turning wall-gears)
  (recording-active receiver)
  (recording-open gate))


(define-query live-recording-object (?object mobile-object)
  (exists (?ghost mobile-object)
    (recording-copy> ?object ?ghost)))


(define-query ghost-recording-object (?object mobile-object)
  (exists (?live mobile-object)
    (recording-copy> ?live ?object)))


(define-query same-recording-side (?object1 mobile-object ?object2 mobile-object)
  (or (and (live-recording-object ?object1)
           (live-recording-object ?object2))
      (and (ghost-recording-object ?object1)
           (ghost-recording-object ?object2))))


(define-query recording-shadow-object (?object)
  (and (mobile-object ?object)
       (ghost-recording-object ?object)))


(define-query recording-shadow-object-present (?object)
  ;; Fixed apparatus and genuinely unmapped objects exist in both views.  Of each mapped
  ;; pair, only the ghost copy existed while the recording was made.
  (or (not (mobile-object ?object))
      (ghost-recording-object ?object)
      (and (not (live-recording-object ?object))
           (not (ghost-recording-object ?object)))))


(define-query recording-shadow-turning (?gears)
  (and (wall-gears ?gears)
       (recording-turning ?gears)))


(define-query recording-shadow-gate-open (?gate)
  (and (gate ?gate)
       (recording-open ?gate)))


(define-query object-manipulation-allowed (?actor ?object)
  ;; Recorder participants may manipulate only mapped objects on their own side.
  (and (mobile-object ?actor)
       (mobile-object ?object)
       (same-recording-side ?actor ?object)))


(define-query support-use-allowed (?occupant ?support)
  ;; Fixed supports such as plates are shared environmental apparatus.  A mobile support
  ;; (box or floor-mounted fan) is usable only by an occupant on the same recording side.
  (or (not (mobile-object ?support))
      (and (mobile-object ?occupant)
           (same-recording-side ?occupant ?support))))


(define-query connector-pairing-allowed (?actor ?connector ?terminus)
  ;; Fixed beam apparatus is shared.  During playback a live connector may use either
  ;; layer's connector as a terminus, while a ghost connector may depend only on another
  ;; ghost connector -- never on a live movable connector absent from its recording.
  (and (object-manipulation-allowed ?actor ?connector)
       (or (not (connector ?terminus))
           (and (live-recording-object ?actor)
                (or (live-recording-object ?terminus)
                    (ghost-recording-object ?terminus)))
           (and (ghost-recording-object ?actor)
                (ghost-recording-object ?terminus)))))


(define-query recording-plate-occupied (?plate plate)
  (exists (?occupant support-occupant)
    (and (ghost-recording-object ?occupant)
         (on ?occupant ?plate))))


(define-query recording-controller-energized (?controller (either receiver plate))
  (or (and (receiver ?controller)
           (recording-active ?controller))
      (and (pressure-plate ?controller)
           (recording-depressed ?controller))
      (and (toggle-plate ?controller)
           (recording-latched ?controller))))


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


(define-update update-recording-plate-status! ()
  ;; The recording view contains only mapped ghost occupants.  During initialization its
  ;; toggle latch starts from the authored playback latch; afterward it changes only on a
  ;; ghost-only clear-to-depressed transition.
  (doall (?plate plate)
    (do (if (and *applying-init-action*
                 (toggle-plate ?plate))
          (if (latched ?plate)
            (recording-latched ?plate)
            (not (recording-latched ?plate))))
        (if (recording-plate-occupied ?plate)
          (do (if (and (not *applying-init-action*)
                       (toggle-plate ?plate)
                       (not (recording-depressed ?plate)))
                (if (recording-latched ?plate)
                  (not (recording-latched ?plate))
                  (recording-latched ?plate)))
              (recording-depressed ?plate))
          (not (recording-depressed ?plate))))))


(define-update update-recording-receiver-status! ()
  (doall (?receiver receiver)
    (if (recording-shadow-beam-reaches-receiver ?receiver)
      (recording-active ?receiver)
      (not (recording-active ?receiver)))))


(define-update update-recording-gate-status! ()
  ;; Recording gates use the ordinary DNF polarity, but their controllers read ghost-only
  ;; plates and receivers.  Windtunnel has no gate jamming; recording-side jamming is not
  ;; approximated here.
  (doall (?gate gate)
    (do (assign $control-on nil)
        (if (bind (controls $clauses ?gate $mode))
          (do (assign $any-clause-on
                (ww-loop for $clause in $clauses
                         thereis
                           (ww-loop for $controller in $clause
                                    always
                                      (recording-controller-energized $controller))))
              (if (eql $mode 'normal)
                (assign $control-on $any-clause-on)
                (if (eql $mode 'inverted)
                  (assign $control-on (not $any-clause-on))))))
        (if $control-on
          (recording-open ?gate)
          (not (recording-open ?gate))))))


(define-update update-recording-gears-status! ()
  ;; Windtunnel's recording-side output: uncontrolled wall gears turn; controlled wall
  ;; gears evaluate their DNF against recording-side plate state.  Receiver-controlled
  ;; wall gears remain outside this Windtunnel-scoped recording shadow.
  (doall (?gears wall-gears)
    (do (assign $control-on t)
        (if (bind (controls $clauses ?gears $mode))
          (do (assign $any-clause-on
                (ww-loop for $clause in $clauses
                         thereis
                           (ww-loop for $controller in $clause
                                    always
                                      (if (plate $controller)
                                        (recording-controller-energized $controller)
                                        (error
                                          "Recording-side wall-gears controls support only plates: ~S"
                                          $controller)))))
              (if (eql $mode 'normal)
                (assign $control-on $any-clause-on)
                (if (eql $mode 'inverted)
                  (assign $control-on (not $any-clause-on))))))
        (if $control-on
          (recording-turning ?gears)
          (not (recording-turning ?gears))))))


;;;; TWO-PHASE SOLUTION REPORT ;;;;


(defun recorder-report-agent (move)
  "Return the single agent named by a recorded solution MOVE."
  (unless (and (listp move)
               (= (length move) 2)
               (listp (second move)))
    (error "Malformed recorder solution move: ~S" move))
  (let ((agents
          (remove-duplicates
            (remove-if-not
              (lambda (argument)
                (member argument (gethash 'agent *types*)))
              (rest (second move))))))
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


(register-solution-report-printer 'print-recorder-report)

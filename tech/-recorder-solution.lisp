;;; Filename: -recorder-solution.lisp

;;; Recorder path services: optional recording-prefix pruning, automatic exact live/ghost
;;; interleaving canonicalization, candidate validation, and the two-phase report.  Nested
;;; by recorder.lisp, whose public assembly installs pruning, validation, reporting,
;;; and goal chaining after all recorder components have been defined.  Lower-level tests may
;;; include this file for its mechanics without installing those public services.  Nothing
;;; here is a substrate hook, and nothing here reads recording-side state.  The recorder
;;; shadow components derive
;;; RECORDING-DEPRESSED, RECORDING-LATCHED, RECORDING-TURNING, RECORDING-ACTIVE and
;;; RECORDING-OPEN during propagation, while these services run only during prefix or
;;; candidate replay and touch none of them.  What they do read is identity from -recorder-core --
;;; LIVE-RECORDING-OBJECT and GHOST-RECORDING-OBJECT -- plus location, position, the
;;; mobility closure, and (since GHOST-STOPS-RECORDER was strengthened to require a
;;; genuinely closed session) RECORDING-IN-PROGRESS, which is session lifecycle state, not
;;; one of the per-apparatus shadow views above.
;;;
;;; A recording is closed by moving back to a recorder and stopping it, which gives a
;;; problem two sensible places to stop searching.  The default is to stop when the problem's
;;; own goal is met, leaving the ghost wherever its last useful action left it; the recording
;;; is still performable because the ghost can move back afterward, and the report supplies
;;; that return trip.  A problem that would rather see the return spelled out in the solution
;;; path adds GHOST-STOPS-RECORDER as a goal conjunct and pays the extra actions.
;;; VALIDATE-RECORDER-SOLUTION enforces only the weaker of the two -- a validator vetoes
;;; every candidate, so it must admit both styles -- and RECORDING-AGENT-CAN-CLOSE is that
;;; weaker rule.  A ghost already standing on a recorder satisfies it trivially, so the
;;; conjunct strengthens the goal without ever contradicting the validator.
;;;
;;; REQUIRES:
;;;   nested : -location ((has-location ...)); -position (recorder has-position role);
;;;            -mobility (mobility-results -- the identity default reduces
;;;            RECORDING-AGENT-CAN-CLOSE to standing on the recorder, which is the right
;;;            reading for a problem with no walking technology); -holding (cargo, holding
;;;            -- nested here rather than left to cargo-carrying techs, so a recording
;;;            session's closure rule is well-defined even in a cargo-free recorder
;;;            problem; empty CARGO there makes RECORDING-AGENT-EMPTY-HANDED a no-op)
;;;   soft   : -recorder-core's identity queries and RECORDING-IN-PROGRESS, assembled by
;;;            recorder.lisp before this component
;;; PROVIDES:
;;;   queries  : ghost-stops-recorder (optional goal conjunct), recording-agent-can-close,
;;;              recording-agent-return-route, recording-agent-at-recorder,
;;;              recording-agent-empty-handed
;;;   functions: validate-recorder-solution, build-recorder-report, print-recorder-report;
;;;              validate-recorder-recording-prefix;
;;;              automatic recorder interleaving pruning;
;;;              recorder-recording-path, recorder-recording-window,
;;;              recorder-recording-snapshot and their helpers locate the real
;;;              START-RECORDER/STOP-RECORDER moves when the searched path contains them,
;;;              falling back to the whole path when it does not

(include-tech -location)
(include-tech -position)
(include-tech -mobility)
(include-tech -holding)

(in-package :ww)


(define-query recording-agent-empty-handed (?agent agent)
  ;; Operating a recorder requires empty hands.  In particular, closing a recording session
  ;; -- either the strict GHOST-STOPS-RECORDER style below or the weaker
  ;; RECORDING-AGENT-CAN-CLOSE style -- requires the ghost to have already set down whatever
  ;; it was carrying, not merely to be standing at or within reach of a recorder.
  (not (bind (holding ?agent $anything))))


(define-query ghost-stops-recorder ()
  ;; Optional goal conjunct, and the closure the recorder-cycle-chaining machinery requires
  ;; before treating a search as a genuinely closed cycle.  Position and holding alone are
  ;; not enough: recording must actually have been stopped by a real STOP-RECORDER action,
  ;; not merely be positionally consistent with having stopped, or a chained cycle could
  ;; commit a boundary where recording never genuinely closed.  Every mapped ghost agent
  ;; has moved back to a recorder empty-handed, so the return trip -- and any cargo it had
  ;; to set down first -- appears in the solution path and its length counts toward
  ;; min-length.  A problem that omits this conjunct stops at its own goal and lets the
  ;; report supply the return instead.  Place it after the problem's own goal literals: the
  ;; conjunction is evaluated in order, so this walks the ghost roster only on states that
  ;; already qualify.
  (and (not (recording-in-progress))
       (forall (?agent agent)
         (or (not (ghost-recording-object ?agent))
             (and (recording-agent-at-recorder ?agent)
                  (recording-agent-empty-handed ?agent))))))


(define-query recording-agent-can-close (?agent agent)
  ;; The recording remains closable: ?agent is already empty-handed, and some recorder's
  ;; location lies in its current mobility closure, which for a ghost is computed against
  ;; recording-side gate and gears state.  An agent resting on a support changes to ground
  ;; before moving, and that configuration transition is not itself a traversal obstacle, so
  ;; support occupancy is not consulted here.  Reachability alone would let the report
  ;; silently append the walk back to a recorder, but it cannot also silently set down
  ;; cargo, so empty-handedness is checked now instead of deferred to the report.
  (and (recording-agent-empty-handed ?agent)
       (do (bind (has-location ?agent $agent-location))
           (assign $reachable (mobility-locations ?agent $agent-location))
           (exists (?recorder recorder)
             (exists (?location location)
               (and (has-position ?recorder ?location)
                    (member ?location $reachable)))))))


(define-query recording-agent-return-route (?agent agent)
  ;; (?agent route) for the move that closes ?agent's recording, or nil when ?agent already
  ;; stands on a recorder and no return trip is outstanding.  Consumed by the report, which
  ;; appends the move a goal-terminated search stopped short of.
  (do (bind (has-location ?agent $agent-location))
      (assign $outstanding (not (recording-agent-at-recorder ?agent)))
      (assign $results (mobility-results ?agent $agent-location))
      (assign $return-move nil)
      (ww-loop for $result in $results
               do (assign $location (first $result))
                  (if (and $outstanding
                           (not $return-move)
                           (exists (?recorder recorder)
                             (has-position ?recorder $location)))
                    (assign $return-move
                            (list ?agent
                                  (second $result)))))
      $return-move))


(define-query recording-agent-at-recorder (?agent agent)
  (exists (?recorder recorder)
    (exists (?location location)
      (and (has-position ?recorder ?location)
           (has-location ?agent ?location)))))

;;;; CANDIDATE VALIDATION AND TWO-PHASE REPORT ;;;;


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


(defun recorder-interleaving-pruning-enabled-p ()
  "Whether this search path supports automatic recorder interleaving pruning."
  (and (eql *algorithm* 'depth-first)
       (zerop *threads*)
       (not *hybrid-mode*)))


(defun recorder-interleaving-action-side (state move)
  "Return MOVE's recorder side, or NIL unless it names exactly one mapped agent."
  (let ((agents (recorder-move-agents move)))
    (when (= (length agents) 1)
      (recorder-report-agent-side state (first agents)))))


(defun recorder-interleaving-inversion-p (state first-move second-move)
  "Whether the adjacent moves form a ghost-before-live canonical-order inversion."
  (let ((first-action (second first-move))
        (second-action (second second-move)))
    (when (or (member (first first-action) '(start-recorder stop-recorder))
              (member (first second-action) '(start-recorder stop-recorder)))
      (return-from recorder-interleaving-inversion-p nil))
    (let ((first-side (recorder-interleaving-action-side state first-move))
          (second-side (recorder-interleaving-action-side state second-move)))
      (and (eql first-side :ghost)
           (eql second-side :live)))))


(defun recorder-interleaving-other-prefix-validation-enabled-p ()
  "Whether certification must construct alternate paths for a non-recorder policy."
  (search-prefix-validation-enabled-p
    '(validate-recorder-recording-prefix)))


(defun recorder-interleaving-state-valid-p (state path)
  "Whether an alternate replay state passes non-redundant search validity checks."
  (and (not (state-is-inconsistent state))
       (or (not (and (boundp 'constraint-fn)
                     (symbol-value 'constraint-fn)))
           (funcall (symbol-function 'constraint-fn) state))
       (every (lambda (invariant)
                (funcall (symbol-function invariant) state))
              *global-invariants*)
       (candidate-search-prefix-valid-p
         path state '(validate-recorder-recording-prefix))))


(defun recorder-interleaving-equivalent-state-p (state1 state2)
  "Whether two action orders produce the same complete search-relevant state."
  (and (equalp (problem-state.idb state1)
               (problem-state.idb state2))
       (= (problem-state.time state1)
          (problem-state.time state2))
       (= (problem-state.value state1)
          (problem-state.value state2))
       (equalp (problem-state.happenings state1)
               (problem-state.happenings state2))))


(defun replay-recorder-interleaving-step
    (action state next-action path validate-prefix-p)
  "Replay ACTION and return its state, extended path, and validity."
  (multiple-value-bind (next-state valid-p reason)
      (apply-action-to-state action state next-action)
    (declare (ignore reason))
    (unless valid-p
      (return-from replay-recorder-interleaving-step
        (values nil nil nil)))
    (let ((next-path
            (when validate-prefix-p
              (append path (list (record-move next-state))))))
      (if (recorder-interleaving-state-valid-p next-state next-path)
        (values next-state next-path t)
        (values nil nil nil)))))


(defun recorder-interleaving-swap-certified-p
    (source-node first-action second-action actual-final-state)
  "Whether replaying SECOND-ACTION before FIRST-ACTION yields the same valid state."
  (let* ((validate-prefix-p
           (recorder-interleaving-other-prefix-validation-enabled-p))
         (source-state (node.state source-node))
         (source-path
           (when validate-prefix-p
             (record-solution-path source-node))))
    (multiple-value-bind (alternate-first first-path first-valid-p)
        (replay-recorder-interleaving-step
          second-action source-state first-action source-path validate-prefix-p)
      (unless first-valid-p
        (return-from recorder-interleaving-swap-certified-p nil))
      (multiple-value-bind (alternate-final final-path final-valid-p)
          (replay-recorder-interleaving-step
            first-action alternate-first nil first-path validate-prefix-p)
        (declare (ignore final-path))
        (and final-valid-p
             (recorder-interleaving-equivalent-state-p
               alternate-final actual-final-state))))))


(defun prune-recorder-interleaving-successor-p (current-node successor-state)
  "Discard exactly certified ghost-before-live successors in favor of live-before-ghost."
  (let ((source-node (node.parent current-node)))
    (unless (node-p source-node)
      (return-from prune-recorder-interleaving-successor-p nil))
    (let* ((first-move (record-move (node.state current-node)))
           (second-move (record-move successor-state))
           (first-action (second first-move))
           (second-action (second second-move)))
      (and (recorder-interleaving-inversion-p
             (node.state source-node) first-move second-move)
           (recorder-interleaving-swap-certified-p
             source-node first-action second-action successor-state)))))


(defun recorder-path-moves-on-side (state integrated-path side)
  "Return the moves performed by SIDE in INTEGRATED-PATH."
  (remove-if-not
    (lambda (move)
      (eql side (recorder-report-move-side state move)))
    integrated-path))


(defun recorder-move-action-name (move)
  "The action name MOVE invokes, or NIL when MOVE is not an (index (action ...)) pair."
  (when (and (listp move)
             (= (length move) 2)
             (listp (second move)))
    (first (second move))))


(defun recorder-explicit-start (integrated-path)
  "The real START-RECORDER move in INTEGRATED-PATH, or NIL when none was searched."
  (find 'start-recorder integrated-path :key #'recorder-move-action-name))


(defun recorder-explicit-stop (integrated-path)
  "The real STOP-RECORDER move in INTEGRATED-PATH, or NIL when none was searched."
  (find 'stop-recorder integrated-path :key #'recorder-move-action-name))


(defun recorder-boundary-diagnostic (integrated-path)
  "Return a diagnostic when INTEGRATED-PATH does not contain one well-ordered session."
  (let ((start-positions nil)
        (stop-positions nil))
    (loop for move in integrated-path
          for position from 0
          for action = (recorder-move-action-name move)
          when (eql action 'start-recorder)
            do (push position start-positions)
          when (eql action 'stop-recorder)
            do (push position stop-positions))
    (cond
      ((> (length start-positions) 1)
       '(:phase :recording :reason :invalid-boundary :detail :multiple-starts))
      ((> (length stop-positions) 1)
       '(:phase :recording :reason :invalid-boundary :detail :multiple-stops))
      ((and stop-positions (null start-positions))
       '(:phase :recording :reason :invalid-boundary :detail :stop-without-start))
      ((and start-positions stop-positions
            (< (first stop-positions) (first start-positions)))
       '(:phase :recording :reason :invalid-boundary :detail :stop-before-start)))))


(defun recorder-path-after (integrated-path move)
  "The tail of INTEGRATED-PATH strictly after MOVE, or the whole path when MOVE is NIL."
  (if move
    (rest (member move integrated-path))
    integrated-path))


(defun recorder-path-before (integrated-path move)
  "The prefix of INTEGRATED-PATH strictly before MOVE, or the whole path when MOVE is NIL."
  (if move
    (subseq integrated-path 0 (position move integrated-path))
    integrated-path))


(defun recorder-pre-recording-path (integrated-path)
  "Return the actions before INTEGRATED-PATH's real START-RECORDER.

There is no pre-recording prefix in the legacy no-explicit-start form."
  (let ((explicit-start (recorder-explicit-start integrated-path)))
    (and explicit-start
         (recorder-path-before integrated-path explicit-start))))


(defun recorder-recording-window (integrated-path)
  "INTEGRATED-PATH narrowed to strictly between its real START-RECORDER and STOP-RECORDER
moves.  Either or both edges default to the path's own start/end when the searched path
never invoked the real action -- exactly the pre-restructuring behavior, where recording
had no path-local edges at all."
  (recorder-path-before
    (recorder-path-after integrated-path (recorder-explicit-start integrated-path))
    (recorder-explicit-stop integrated-path)))


(defun recorder-recording-path (state integrated-path)
  "The path segment VALIDATE-RECORDER-SOLUTION treats as one recording: the real
START-RECORDER move when the searched path contains one, every ghost move within the
window it opens, and the real STOP-RECORDER move when present.  A ghost action's own
precondition now requires recording to be in progress, so the isolated replay needs
START-RECORDER's fork included to be viable at all -- it is no longer purely a ghost-only
subsequence.  Falls back to the whole path's ghost-only moves, with no edges, when neither
real action was searched."
  (let ((explicit-start (recorder-explicit-start integrated-path))
        (explicit-stop (recorder-explicit-stop integrated-path)))
    (append (and explicit-start (list explicit-start))
            (recorder-path-moves-on-side
              state (recorder-recording-window integrated-path) :ghost)
            (and explicit-stop (list explicit-stop)))))


(defun recorder-action-failure-diagnostic (phase validation)
  (list :phase phase
        :reason :action-failed
        :step (action-sequence-validation-failure-index validation)
        :action (action-sequence-validation-failure-action validation)
        :detail (action-sequence-validation-failure-reason validation)))


(defun recorder-recording-snapshot (start-state integrated-path)
  "Return the state captured immediately before START-RECORDER and any replay diagnostic.

An explicit recording begins from the result of replaying every pre-recording action from
START-STATE.  The legacy form with no explicit START-RECORDER continues to use START-STATE
directly, because its focused tests author the already-open recording state there."
  (unless (recorder-explicit-start integrated-path)
    (return-from recorder-recording-snapshot
      (values (copy-problem-state start-state) nil)))
  (let ((validation
          (validate-action-sequence
            start-state (recorder-pre-recording-path integrated-path))))
    (if (action-sequence-validation-success-p validation)
      (values (action-sequence-validation-final-state validation) nil)
      (values nil (recorder-action-failure-diagnostic :snapshot validation)))))


(defun recorder-recording-agents (state)
  "Return the mapped ghost agents that can act during recording."
  (remove-if-not
    (lambda (agent)
      (funcall (symbol-function 'ghost-recording-object) state agent))
    (gethash 'agent *types*)))


(defun recorder-prefix-pruning-enabled-p ()
  "Whether recorder recording-prefix pruning is enabled for the current search."
  *recorder-prefix-pruning*)


(defun recorder-recording-prefix-changed-p (start-state integrated-path)
  "Whether the newest move changes the isolated recording sequence.

Ordinary live moves are absent from that sequence.  Their pre-recording effects are
captured when START-RECORDER is eventually checked, while live moves after the start
cannot alter a recording prefix already accepted at its preceding ghost move."
  (let* ((move (car (last integrated-path)))
         (action (recorder-move-action-name move)))
    (or (member action '(start-recorder stop-recorder))
        (some (lambda (agent)
                (funcall (symbol-function 'ghost-recording-object)
                         start-state agent))
              (recorder-move-agents move)))))


(defun validate-recorder-recording-prefix (start-state integrated-path current-state)
  "Accept a search prefix while its isolated recording sub-path remains replayable.

This is deliberately narrower than VALIDATE-RECORDER-SOLUTION.  It checks session
boundaries, reconstructs the START-RECORDER snapshot, and replays the recording actions
seen so far.  It does not require the ghost to be able to close the recording and does not
validate playback or the goal, because later actions can still change those conditions.
Once an action in the isolated recording sequence fails, however, extending the integrated
  path cannot repair that earlier prefix, so the branch is safe to prune."
  (declare (ignore current-state))
  (unless (recorder-recording-prefix-changed-p start-state integrated-path)
    (return-from validate-recorder-recording-prefix (values t nil)))
  (let ((boundary-diagnostic (recorder-boundary-diagnostic integrated-path)))
    (when boundary-diagnostic
      (return-from validate-recorder-recording-prefix
        (values nil boundary-diagnostic))))
  (multiple-value-bind (snapshot-state snapshot-diagnostic)
      (recorder-recording-snapshot start-state integrated-path)
    (when snapshot-diagnostic
      (return-from validate-recorder-recording-prefix
        (values nil snapshot-diagnostic)))
    (let ((recording-validation
            (validate-action-sequence
              snapshot-state
              (recorder-recording-path snapshot-state integrated-path))))
      (if (action-sequence-validation-success-p recording-validation)
        (values t nil)
        (values nil
                (recorder-action-failure-diagnostic
                  :recording recording-validation))))))


(defun validate-recorder-solution (start-state integrated-path goal-state)
  "Validate recording and playback under the recorder's snapshot-reset semantics.

When the path contains a real START-RECORDER, its pre-recording prefix is first replayed
from START-STATE to reconstruct the snapshot captured immediately before that action.  The
recording path -- START-RECORDER, its ghost moves, and STOP-RECORDER when present -- is
then replayed from that snapshot.  The legacy no-explicit-start form continues to replay
its ghost moves directly from START-STATE.  The search terminates as soon as the problem's
own goal is met; a ghost left holding cargo or away from a recorder no longer vetoes that
candidate.  A problem that wants the return trip spelled out in the solution path adds
GHOST-STOPS-RECORDER as a goal conjunct instead -- the playback goal check below enforces
it directly.  Playback independently replays the complete integrated path from START-STATE
under the ordinary action rules."
  (declare (ignore goal-state))
  (let ((boundary-diagnostic (recorder-boundary-diagnostic integrated-path)))
    (when boundary-diagnostic
      (return-from validate-recorder-solution (values nil boundary-diagnostic))))
  (multiple-value-bind (snapshot-state snapshot-diagnostic)
      (recorder-recording-snapshot start-state integrated-path)
    (when snapshot-diagnostic
      (return-from validate-recorder-solution (values nil snapshot-diagnostic)))
    (let* ((ghost-path
             (recorder-recording-path snapshot-state integrated-path))
           (recording-validation
             (validate-action-sequence snapshot-state ghost-path)))
      (unless (action-sequence-validation-success-p recording-validation)
        (return-from validate-recorder-solution
          (values nil
                  (recorder-action-failure-diagnostic
                    :recording recording-validation))))
      (let* ((recording-state
               (action-sequence-validation-final-state recording-validation))
             (recording-agents (recorder-recording-agents recording-state)))
        (when (null recording-agents)
          (return-from validate-recorder-solution
            (values nil '(:phase :recording :reason :no-recording-agent))))))
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
  "Extract ghost moves within the recording window, replace each live-action block with one
PAUSE marker, and open/close with the real START-RECORDER/STOP-RECORDER moves when the
searched path contains them.  Any pre-recording moves before a real START-RECORDER fall
outside the window entirely, rather than appearing as an inferred leading pause.  Falls
back to a synthesized opening marker, and to a synthesized closing marker plus whatever
return move the searched path stopped short of, exactly as before real actions existed,
when the corresponding real move is absent."
  (let* ((explicit-start (recorder-explicit-start integrated-path))
         (explicit-stop (recorder-explicit-stop integrated-path))
         (sequence (list (or explicit-start '(start-recorder))))
         (previous-side nil))
    (dolist (move (recorder-recording-window integrated-path))
      (let ((side (recorder-report-move-side state move)))
        (when (and (eql side :live)
                   (not (eql previous-side :live)))
          (setf sequence (nconc sequence (list '(pause)))))
        (when (eql side :ghost)
          (setf sequence (nconc sequence (list move))))
        (setf previous-side side)))
    (nconc sequence
           (if explicit-stop
             (list explicit-stop)
             (nconc (recorder-return-moves state) (list '(stop-recorder)))))))


(defun recorder-return-moves (state)
  "Return one (MOVE agent route) marker per ghost away.

STATE is the completed integrated state.  A ghost's location there, and the recording-side
gate and gears state its mobility closure is computed against, are the same as at the end of
the ghost-only recording, so no second replay is needed.  A path that already carries the
return -- the GHOST-STOPS-RECORDER goal style -- yields no markers.  These are report
markers, not planner actions, and carry no step number for that reason."
  (loop for agent in (recorder-recording-agents state)
        for move = (funcall (symbol-function 'recording-agent-return-route) state agent)
        when move
          collect (cons 'move move)))


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

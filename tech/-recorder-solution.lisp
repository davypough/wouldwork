;;; Filename: -recorder-solution.lisp

;;; Recorder path services: multi-window parsing, mandatory completed-cycle validation and
;;; no-progress rejection, closed-boundary cycle-resource dominance, optional open
;;; recording-prefix pruning, automatic exact live/ghost interleaving canonicalization,
;;; candidate validation, and integrated multi-cycle reporting.  Nested
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
;;; Every intermediate recording is physically ended by STOP-RECORDER or CANCEL-PLAYBACK.
;;; The parser also
;;; admits a final open cycle when the problem goal is reached first; its isolated recording
;;; must be executable, but physical closure is not added to that goal.  A problem that wants
;;; the final return and stop spelled out adds GHOST-STOPS-RECORDER as a goal conjunct.
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
;;;   queries  : recorder-cycle-ended (generic closed-cycle goal), ghost-stops-recorder
;;;              (strict normal-stop goal), recording-agent-can-close,
;;;              recording-agent-return-route, recording-agent-at-recorder,
;;;              recording-agent-empty-handed
;;;   functions: parse-recorder-path, validate-recorder-solution,
;;;              validate-recorder-cycle-boundary-prefix,
;;;              validate-recorder-recording-prefix,
;;;              recorder-boundary-identity-state,
;;;              prune-recorder-boundary-dominated-successor-p,
;;;              build-recorder-report, print-recorder-report;
;;;              automatic recorder interleaving pruning;
;;;              recorder-recording-path, recorder-recording-window,
;;;              recorder-recording-snapshot and their helpers locate the real
;;;              START-RECORDER and either explicit ending when the searched path contains
;;;              them,
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


(define-query recorder-cycle-ended ()
  ;; Both a completed ghost recording and a live cancellation create this generic clean
  ;; boundary.  The action's atomic followup removes all ghost state and normalizes the
  ;; recording shadow, so no positional ghost test remains meaningful here.
  (and (recorder-cycle-closed)
       (not (recording-in-progress))
       (recorder-closed-ghost-free)))


(define-query ghost-stops-recorder ()
  ;; Retain the stricter condition for goals that specifically require the recorded ghost
  ;; to reach its authored STOP rather than having the live agent cancel playback.
  (and (recorder-cycle-stopped-by-ghost)
       (recorder-cycle-ended)))


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

;;;; CANDIDATE VALIDATION AND REPORTING ;;;;


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
    (when (or (member (first first-action)
                      '(start-recorder stop-recorder cancel-playback))
              (member (first second-action)
                      '(start-recorder stop-recorder cancel-playback)))
      (return-from recorder-interleaving-inversion-p nil))
    (let ((first-side (recorder-interleaving-action-side state first-move))
          (second-side (recorder-interleaving-action-side state second-move)))
      (and (eql first-side :ghost)
           (eql second-side :live)))))


(defun recorder-interleaving-other-prefix-validation-enabled-p ()
  "Whether certification must construct alternate paths for a non-recorder policy."
  (search-prefix-validation-enabled-p
    '(validate-recorder-cycle-boundary-prefix
      validate-recorder-recording-prefix)))


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
         path state '(validate-recorder-cycle-boundary-prefix
                      validate-recorder-recording-prefix))))


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


(defun recorder-cycle-ending-action-p (action)
  "Whether ACTION explicitly ends an open recorder cycle."
  (member action '(stop-recorder cancel-playback)))


(defun recorder-cycle-normal-stop-p (move)
  "Whether MOVE is the recording-side normal STOP-RECORDER boundary."
  (eql (recorder-move-action-name move) 'stop-recorder))


(defun recorder-cycle-cancellation-p (move)
  "Whether MOVE is the live-side CANCEL-PLAYBACK boundary."
  (eql (recorder-move-action-name move) 'cancel-playback))


(defstruct (recorder-path-cycle (:conc-name recorder-path-cycle.))
  "One parsed recorder cycle in an integrated planner path."
  number
  setup
  start
  moves
  ending)


(defun recorder-boundary-error (cycle detail)
  "Return a cycle-numbered malformed-boundary diagnostic."
  (list :phase :recording
        :reason :invalid-boundary
        :cycle cycle
        :detail detail))


(defun parse-recorder-path (start-state integrated-path)
  "Parse INTEGRATED-PATH into ordered recorder cycles and trailing setup.

Returns three values: cycles, trailing setup moves, and a diagnostic.  An authored start
state with RECORDING-IN-PROGRESS supplies one legacy implicit open cycle."
  (let* ((cycles-used
           (funcall (symbol-function 'recorder-cycle-count) start-state))
         (open-cycle
           (when (member '(recording-in-progress)
                         (database start-state)
                         :test #'equal)
             (make-recorder-path-cycle
               :number (max 1 cycles-used)
               :setup nil
               :start nil
               :moves nil
               :ending nil)))
         (cycles nil)
         (setup-reversed nil))
    (when (> cycles-used *max-recorder-cycles*)
      (return-from parse-recorder-path
        (values nil nil
                (recorder-boundary-error cycles-used :maximum-exceeded))))
    (dolist (move integrated-path)
      (case (recorder-move-action-name move)
        (start-recorder
          (when open-cycle
            (return-from parse-recorder-path
              (values nil nil
                      (recorder-boundary-error
                        (recorder-path-cycle.number open-cycle)
                        :multiple-starts))))
          (incf cycles-used)
          (when (> cycles-used *max-recorder-cycles*)
            (return-from parse-recorder-path
              (values nil nil
                      (recorder-boundary-error
                        cycles-used :maximum-exceeded))))
          (setf open-cycle
                (make-recorder-path-cycle
                  :number cycles-used
                  :setup (nreverse setup-reversed)
                  :start move
                  :moves nil
                  :ending nil)
                setup-reversed nil))
        ((stop-recorder cancel-playback)
          (unless open-cycle
            (return-from parse-recorder-path
              (values nil nil
                      (recorder-boundary-error
                        (1+ cycles-used)
                        (if (eql (recorder-move-action-name move) 'stop-recorder)
                          :stop-without-start
                          :cancel-without-start)))))
          (setf (recorder-path-cycle.moves open-cycle)
                (nreverse (recorder-path-cycle.moves open-cycle))
                (recorder-path-cycle.ending open-cycle) move)
          (push open-cycle cycles)
          (setf open-cycle nil))
        (otherwise
          (if open-cycle
            (push move (recorder-path-cycle.moves open-cycle))
            (push move setup-reversed)))))
    (when open-cycle
      (setf (recorder-path-cycle.moves open-cycle)
            (nreverse (recorder-path-cycle.moves open-cycle)))
      (push open-cycle cycles))
    (values (nreverse cycles) (nreverse setup-reversed) nil)))


(defun recorder-explicit-start (integrated-path)
  "The real START-RECORDER move in INTEGRATED-PATH, or NIL when none was searched."
  (find 'start-recorder integrated-path :key #'recorder-move-action-name))


(defun recorder-explicit-stop (integrated-path)
  "The real STOP-RECORDER move in INTEGRATED-PATH, or NIL when none was searched."
  (find 'stop-recorder integrated-path :key #'recorder-move-action-name))


(defun recorder-explicit-ending (integrated-path)
  "The real STOP-RECORDER or CANCEL-PLAYBACK move in INTEGRATED-PATH, if present."
  (find-if
    (lambda (move)
      (recorder-cycle-ending-action-p (recorder-move-action-name move)))
    integrated-path))


(defun recorder-boundary-diagnostic
    (integrated-path &optional (start-state *start-state*))
  "Return the state-machine parser diagnostic for INTEGRATED-PATH, if any."
  (nth-value 2 (parse-recorder-path start-state integrated-path)))


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
  "INTEGRATED-PATH narrowed to strictly between its real START-RECORDER and explicit ending
moves.  Either or both edges default to the path's own start/end when the searched path
never invoked the real action -- exactly the pre-restructuring behavior, where recording
had no path-local edges at all."
  (recorder-path-before
    (recorder-path-after integrated-path (recorder-explicit-start integrated-path))
    (recorder-explicit-ending integrated-path)))


(defun recorder-recording-path (state integrated-path)
  "The path segment VALIDATE-RECORDER-SOLUTION treats as one recording: the real
START-RECORDER move when the searched path contains one, every ghost move within the
window it opens, and the real STOP-RECORDER move when present.  CANCEL-PLAYBACK belongs
only to the live playback path.  A ghost action's own
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


(defun recorder-action-failure-diagnostic (phase validation &optional cycle)
  (append
    (list :phase phase
          :reason :action-failed
          :step (action-sequence-validation-failure-index validation)
          :action (action-sequence-validation-failure-action validation)
          :detail (action-sequence-validation-failure-reason validation))
    (when cycle (list :cycle cycle))))


(defun recorder-path-before-cycle (integrated-path cycle)
  "Return the integrated prefix strictly before CYCLE's explicit start."
  (let ((start (recorder-path-cycle.start cycle)))
    (and start (recorder-path-before integrated-path start))))


(defun recorder-path-cycle-recording-path (state cycle)
  "Return CYCLE's isolated start, ghost moves, and optional normal-stop sequence."
  (append
    (when (recorder-path-cycle.start cycle)
      (list (recorder-path-cycle.start cycle)))
    (recorder-path-moves-on-side
      state (recorder-path-cycle.moves cycle) :ghost)
    (when (recorder-cycle-normal-stop-p (recorder-path-cycle.ending cycle))
      (list (recorder-path-cycle.ending cycle)))))


(defun recorder-path-cycle-snapshot (start-state integrated-path cycle)
  "Return CYCLE's pre-start snapshot and any cycle-numbered replay diagnostic."
  (unless (recorder-path-cycle.start cycle)
    (return-from recorder-path-cycle-snapshot
      (values (copy-problem-state start-state) nil)))
  (let ((validation
          (validate-action-sequence
            start-state (recorder-path-before-cycle integrated-path cycle))))
    (if (action-sequence-validation-success-p validation)
      (values (action-sequence-validation-final-state validation) nil)
      (values nil
              (recorder-action-failure-diagnostic
                :snapshot validation (recorder-path-cycle.number cycle))))))


(defun recorder-boundary-identity-state (state)
  "Return a copy of closed STATE with only its consumed-cycle resource removed.

The open/closed marker, live state, ghost state, recording shadow, and every ordinary
dynamic relation remain part of identity.  The cycle count is excluded because it is a
monotone search resource: otherwise equal closed boundaries compare it by dominance."
  (let ((identity-state (copy-problem-state state)))
    (dolist (proposition (database identity-state))
      (when (eql (first proposition) 'recorder-cycles-used)
        (delete-proposition proposition (problem-state.idb identity-state))))
    (invalidate-problem-state-hash identity-state)
    identity-state))


(defun recorder-boundary-identity-equal-p (identity1 identity2)
  "Whether two already-projected recorder boundary states are equal."
  (if (use-canonical-symmetry-p)
    (canonical-state-equal-p identity1 identity2)
    (equalp (problem-state.idb identity1)
            (problem-state.idb identity2))))


(defun recorder-boundary-equivalent-p (state1 state2)
  "Whether closed states have the same future-facing state apart from cycle usage."
  (let ((identity1 (recorder-boundary-identity-state state1))
        (identity2 (recorder-boundary-identity-state state2)))
    (recorder-boundary-identity-equal-p identity1 identity2)))


(defun recorder-cycle-objective-improved-p (start-state end-state)
  "Whether END-STATE strictly improves the active non-length objective."
  (case *solution-type*
    (min-time
     (< (problem-state.time end-state)
        (problem-state.time start-state)))
    (min-value
     (< (problem-state.value end-state)
        (problem-state.value start-state)))
    (max-value
     (> (problem-state.value end-state)
        (problem-state.value start-state)))
    (otherwise nil)))


(defun recorder-completed-cycle-made-progress-p (start-state end-state)
  "Whether a completed cycle changes persistent state or improves its objective.

Wouldwork permits graph search to ignore elapsed time only when there are no exogenous
happenings.  With happenings present, even an otherwise unchanged cycle may advance toward
an event, so this conservative test retains it."
  (or *happening-names*
      (recorder-cycle-objective-improved-p start-state end-state)
      (not (recorder-boundary-equivalent-p start-state end-state))))


(defun recorder-no-progress-diagnostic (cycle)
  (list :phase :recording
        :reason :no-persistent-progress
        :cycle (recorder-path-cycle.number cycle)))


(defstruct (recorder-boundary-dominance-entry
             (:conc-name recorder-boundary-dominance-entry.))
  "One nondominated normalized boundary in the current graph search."
  identity-state
  cycles-used
  cost)


(defvar *recorder-boundary-dominance-table*
  (make-hash-table :test #'eql)
  "Search-wide Pareto frontier keyed by recorder boundary identity hash.")


(defvar *recorder-boundary-dominance-pruned* 0
  "Number of generated boundaries rejected by recorder cycle-resource dominance.")


(defun reset-recorder-boundary-dominance ()
  "Clear recorder boundary dominance state before a new search."
  (setf *recorder-boundary-dominance-table*
        (make-hash-table :test #'eql)
        *recorder-boundary-dominance-pruned* 0))


(defun recorder-boundary-dominance-enabled-p ()
  "Whether this search can safely discard resource-dominated closed boundaries."
  (and (> *max-recorder-cycles* 1)
       (eql *tree-or-graph* 'graph)
       (not *hybrid-mode*)
       (null *happening-names*)
       (member *solution-type*
               '(first min-length min-time min-value max-value))))


(defun recorder-normalized-boundary-p (state)
  "Whether STATE is a ghost-free boundary at which cycle dominance applies."
  (and (not (member '(recording-in-progress)
                    (database state)
                    :test #'equal))
       (funcall (symbol-function 'recorder-closed-ghost-free) state)))


(defun recorder-boundary-dominance-cost (state depth)
  "Return a lower-is-better scalar for the active graph-search objective."
  (case *solution-type*
    ((first min-length) depth)
    (min-time (problem-state.time state))
    (min-value (problem-state.value state))
    (max-value (- (problem-state.value state)))))


(defun recorder-boundary-entry-matches-p (entry identity-state)
  (recorder-boundary-identity-equal-p
    (recorder-boundary-dominance-entry.identity-state entry)
    identity-state))


(defun recorder-boundary-entry-dominates-p (entry cycles-used cost)
  "Whether ENTRY has strictly more cycle capacity and no worse path cost."
  (and (< (recorder-boundary-dominance-entry.cycles-used entry)
          cycles-used)
       (<= (recorder-boundary-dominance-entry.cost entry) cost)))


(defun recorder-boundary-candidate-dominates-p (cycles-used cost entry)
  "Whether the candidate has strictly more cycle capacity and no worse path cost."
  (and (< cycles-used
          (recorder-boundary-dominance-entry.cycles-used entry))
       (<= cost (recorder-boundary-dominance-entry.cost entry))))


(defun update-recorder-boundary-dominance
    (identity-state cycles-used cost)
  "Record a nondominated boundary, returning true when an earlier entry dominates it."
  (ensure-idb-hash identity-state)
  (let* ((key (problem-state.idb-hash identity-state))
         (bucket (gethash key *recorder-boundary-dominance-table*))
         (matches
           (remove-if-not
             (lambda (entry)
               (recorder-boundary-entry-matches-p entry identity-state))
             bucket)))
    (when (some (lambda (entry)
                  (recorder-boundary-entry-dominates-p
                    entry cycles-used cost))
                matches)
      (incf *recorder-boundary-dominance-pruned*)
      (return-from update-recorder-boundary-dominance t))
    (when (some (lambda (entry)
                  (and (= cycles-used
                          (recorder-boundary-dominance-entry.cycles-used entry))
                       (<= (recorder-boundary-dominance-entry.cost entry)
                           cost)))
                matches)
      ;; Ordinary graph duplicate handling owns equal-resource rejection.  The dominance
      ;; frontier merely keeps its best representative for comparisons with other counts.
      (return-from update-recorder-boundary-dominance nil))
    (let ((new-bucket
            (remove-if
              (lambda (entry)
                (and (recorder-boundary-entry-matches-p entry identity-state)
                     (or
                       (recorder-boundary-candidate-dominates-p
                         cycles-used cost entry)
                       (and (= cycles-used
                               (recorder-boundary-dominance-entry.cycles-used entry))
                            (<= cost
                                (recorder-boundary-dominance-entry.cost entry))))))
              bucket)))
      (push
        (make-recorder-boundary-dominance-entry
          :identity-state identity-state
          :cycles-used cycles-used
          :cost cost)
        new-bucket)
      (setf (gethash key *recorder-boundary-dominance-table*) new-bucket))
    nil))


(defun prune-recorder-boundary-dominated-successor-p
    (current-node successor-state)
  "Discard a normalized boundary dominated by an equal state with fewer cycles used."
  (unless (recorder-normalized-boundary-p successor-state)
    (return-from prune-recorder-boundary-dominated-successor-p nil))
  (let ((identity-state (recorder-boundary-identity-state successor-state))
        (cycles-used
          (funcall (symbol-function 'recorder-cycle-count) successor-state))
        (cost
          (recorder-boundary-dominance-cost
            successor-state (1+ (node.depth current-node)))))
    (bt:with-lock-held (*search-lock*)
      (update-recorder-boundary-dominance
        identity-state cycles-used cost))))


(defun recorder-recording-snapshot (start-state integrated-path)
  "Return the state captured immediately before START-RECORDER and any replay diagnostic.

An explicit recording begins from the result of replaying every pre-recording action from
START-STATE.  The legacy form with no explicit START-RECORDER continues to use START-STATE
directly, because its focused tests author the already-open recording state there."
  (multiple-value-bind (cycles trailing-setup diagnostic)
      (parse-recorder-path start-state integrated-path)
    (declare (ignore trailing-setup))
    (when diagnostic
      (return-from recorder-recording-snapshot (values nil diagnostic)))
    (if cycles
      (recorder-path-cycle-snapshot start-state integrated-path (first cycles))
      (values (copy-problem-state start-state) nil))))


(defun recorder-recording-agents (state)
  "Return the mapped ghost agents that can act during recording."
  (remove-if-not
    (lambda (agent)
      (funcall (symbol-function 'ghost-recording-object) state agent))
    (gethash 'agent *types*)))


(defun recorder-prefix-pruning-enabled-p ()
  "Whether recorder recording-prefix pruning is enabled for the current search."
  *recorder-prefix-pruning*)


(defun recorder-cycle-boundary-validation-enabled-p ()
  "Completed recorder cycles are always validated during search."
  t)


(defun recorder-ending-prefix-trigger-p (start-state newest-move current-state)
  "Whether NEWEST-MOVE ends a cycle and therefore needs mandatory validation."
  (declare (ignore start-state current-state))
  (recorder-cycle-ending-action-p (recorder-move-action-name newest-move)))


(defun recorder-recording-prefix-trigger-p
    (start-state newest-move current-state)
  "Whether NEWEST-MOVE can change the isolated recording sequence."
  (declare (ignore current-state))
  (let ((action (recorder-move-action-name newest-move)))
    (or (member action '(start-recorder stop-recorder cancel-playback))
        (some (lambda (agent)
                (funcall (symbol-function 'ghost-recording-object)
                         start-state agent))
              (recorder-move-agents newest-move)))))


(defun recorder-recording-prefix-changed-p (start-state integrated-path)
  "Whether the newest move changes the isolated recording sequence.

Ordinary live moves are absent from that sequence.  Their pre-recording effects are
captured when START-RECORDER is eventually checked, while live moves after the start
cannot alter a recording prefix already accepted at its preceding ghost move."
  (recorder-recording-prefix-trigger-p
    start-state (car (last integrated-path)) start-state))


(defun validate-recorder-path-cycle (start-state integrated-path cycle)
  "Validate CYCLE's isolated recording from its exact integrated snapshot."
  (multiple-value-bind (snapshot-state snapshot-diagnostic)
      (recorder-path-cycle-snapshot start-state integrated-path cycle)
    (when snapshot-diagnostic
      (return-from validate-recorder-path-cycle
        (values nil snapshot-diagnostic)))
    (let* ((cycle-number (recorder-path-cycle.number cycle))
           (recording-path
             (recorder-path-cycle-recording-path snapshot-state cycle))
           (validation
             (validate-action-sequence snapshot-state recording-path)))
      (unless (action-sequence-validation-success-p validation)
        (return-from validate-recorder-path-cycle
          (values nil
                  (recorder-action-failure-diagnostic
                    :recording validation cycle-number))))
      (unless (recorder-recording-agents
                (action-sequence-validation-final-state validation))
        (return-from validate-recorder-path-cycle
          (values nil
                  (list :phase :recording
                        :reason :no-recording-agent
                        :cycle cycle-number))))
      (values t nil snapshot-state))))


(defun validate-recorder-cycle-boundary-prefix
    (start-state integrated-path current-state)
  "Validate the completed cycle ending at the newest explicit boundary successor."
  (multiple-value-bind (cycles trailing-setup diagnostic)
      (parse-recorder-path start-state integrated-path)
    (declare (ignore trailing-setup))
    (when diagnostic
      (return-from validate-recorder-cycle-boundary-prefix
        (values nil diagnostic)))
    (let ((cycle (car (last cycles))))
      (unless (and cycle (recorder-path-cycle.ending cycle))
        (return-from validate-recorder-cycle-boundary-prefix
          (values nil
                  (recorder-boundary-error
                    (if cycle (recorder-path-cycle.number cycle) 1)
                    :stop-without-start))))
      (multiple-value-bind (valid-p cycle-diagnostic snapshot-state)
          (validate-recorder-path-cycle start-state integrated-path cycle)
        (unless valid-p
          (return-from validate-recorder-cycle-boundary-prefix
            (values nil cycle-diagnostic)))
        (if (recorder-completed-cycle-made-progress-p
              snapshot-state current-state)
          (values t nil)
          (values nil (recorder-no-progress-diagnostic cycle)))))))


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
  (multiple-value-bind (cycles trailing-setup boundary-diagnostic)
      (parse-recorder-path start-state integrated-path)
    (declare (ignore trailing-setup))
    (when boundary-diagnostic
      (return-from validate-recorder-recording-prefix
        (values nil boundary-diagnostic)))
    (let ((open-cycle
            (find-if-not #'recorder-path-cycle.ending cycles :from-end t)))
      (if open-cycle
        (validate-recorder-path-cycle start-state integrated-path open-cycle)
        (values t nil)))))


(defun validate-recorder-solution (start-state integrated-path goal-state)
  "Validate every recording cycle and the complete integrated playback path.

Each completed or final open cycle is replayed independently from the snapshot immediately
before its own START-RECORDER.  That snapshot includes every preceding integrated cycle,
its atomic stop normalization, and the following setup.  The final playback check then
replays the whole path once and applies the problem goal."
  (declare (ignore goal-state))
  (multiple-value-bind (cycles trailing-setup boundary-diagnostic)
      (parse-recorder-path start-state integrated-path)
    (declare (ignore trailing-setup))
    (when boundary-diagnostic
      (return-from validate-recorder-solution
        (values nil boundary-diagnostic)))
    (dolist (cycle cycles)
      (multiple-value-bind (valid-p diagnostic)
          (validate-recorder-path-cycle start-state integrated-path cycle)
        (unless valid-p
          (return-from validate-recorder-solution
            (values nil diagnostic)))))
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


(defun recorder-cycle-recording-sequence (state cycle)
  "Build CYCLE's recording-side presentation from its accepted path segment.

Each contiguous live block becomes one PAUSE marker.  Real boundary actions remain in
place.  A legacy implicit opening or final goal-terminated closing is synthesized only in
the report; an open cycle also receives whatever ghost return moves STATE can supply."
  (let* ((start (recorder-path-cycle.start cycle))
         (ending (recorder-path-cycle.ending cycle))
         (sequence (list (or start '(start-recorder))))
         (previous-side nil))
    (dolist (move (recorder-path-cycle.moves cycle))
      (let ((side (recorder-report-move-side state move)))
        (when (and (eql side :live)
                   (not (eql previous-side :live)))
          (setf sequence (nconc sequence (list '(pause)))))
        (when (eql side :ghost)
          (setf sequence (nconc sequence (list move))))
        (setf previous-side side)))
    (nconc sequence
           (cond
             ((recorder-cycle-normal-stop-p ending) (list ending))
             ((recorder-cycle-cancellation-p ending) nil)
             (t (nconc (recorder-return-moves state)
                       (list '(stop-recorder))))))))


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


(defun recorder-cycle-playback-sequence (state cycle)
  "Present CYCLE's in-window moves, pausing live blocks and resuming ghost blocks.

Setup, START-RECORDER, and normal STOP-RECORDER are not playback actions.  A live
CANCEL-PLAYBACK is the final playback action."
  (let ((sequence nil)
        (moves
          (append
            (recorder-path-cycle.moves cycle)
            (when (recorder-cycle-cancellation-p
                    (recorder-path-cycle.ending cycle))
              (list (recorder-path-cycle.ending cycle)))))
        (previous-side nil))
    (dolist (move moves sequence)
      (let ((side (recorder-report-move-side state move)))
        (when (not (eql side previous-side))
          (cond
            ((eql side :live)
             (setf sequence (nconc sequence (list '(pause)))))
            ((eql previous-side :live)
             (setf sequence (nconc sequence (list '(resume)))))))
        (setf sequence (nconc sequence (list move)))
        (setf previous-side side)))))


(defun recorder-cycle-integrated-path (cycle)
  "Return CYCLE's searched setup, opening, window, and optional closing in order."
  (append
    (recorder-path-cycle.setup cycle)
    (when (recorder-path-cycle.start cycle)
      (list (recorder-path-cycle.start cycle)))
    (recorder-path-cycle.moves cycle)
    (when (recorder-path-cycle.ending cycle)
      (list (recorder-path-cycle.ending cycle)))))


(defun recorder-report-metrics (start-state end-state depth)
  "Return path-local DEPTH, elapsed time, and value change between two states."
  (list :depth depth
        :elapsed-time (- (problem-state.time end-state)
                         (problem-state.time start-state))
        :value-change (- (problem-state.value end-state)
                         (problem-state.value start-state))))


(defun replay-recorder-report-segment (start-state path description)
  "Replay PATH for reporting, surfacing an accepted-path inconsistency as an error."
  (let ((validation (validate-action-sequence start-state path)))
    (unless (action-sequence-validation-success-p validation)
      (error "Recorder report cannot replay ~A at step ~D, action ~S: ~S"
             description
             (action-sequence-validation-failure-index validation)
             (action-sequence-validation-failure-action validation)
             (action-sequence-validation-failure-reason validation)))
    (action-sequence-validation-final-state validation)))


(defun build-recorder-cycle-report (start-state cycle)
  "Build one path-derived cycle report and return it with the cycle's ending state."
  (let* ((path (recorder-cycle-integrated-path cycle))
         (number (recorder-path-cycle.number cycle))
         (end-state
           (replay-recorder-report-segment
             start-state path (format nil "recorder cycle ~D" number)))
         (metrics (recorder-report-metrics start-state end-state (length path))))
    (values
      (append
        (list :number number
              :integrated path
              :setup (recorder-path-cycle.setup cycle)
              :recording (recorder-cycle-recording-sequence end-state cycle)
              :playback (recorder-cycle-playback-sequence end-state cycle)
              :closure
              (cond
                ((recorder-cycle-cancellation-p
                   (recorder-path-cycle.ending cycle))
                 :cancelled)
                ((recorder-path-cycle.ending cycle) :explicit)
                (t :synthesized)))
        metrics)
      end-state)))


(defun recorder-report-cycles (start-state cycles)
  "Build ordered CYCLES by replaying each one from its preceding accepted boundary."
  (let ((reports nil)
        (current-state start-state))
    (dolist (cycle cycles)
      (multiple-value-bind (report end-state)
          (build-recorder-cycle-report current-state cycle)
        (push report reports)
        (setf current-state end-state)))
    (values (nreverse reports) current-state)))


(defun recorder-legacy-report-cycle (path)
  "Represent the pre-boundary report form as one synthesized cycle."
  (make-recorder-path-cycle
    :number 1
    :setup nil
    :start nil
    :moves path
    :ending nil))


(defun recorder-complete-solution-metrics (solution)
  "Return totals recorded for the complete accepted planner solution."
  (list :depth (solution.depth solution)
        :elapsed-time (- (solution.time solution)
                         (problem-state.time *start-state*))
        :value-change (- (solution.value solution)
                         (problem-state.value *start-state*))))


(defun build-recorder-report (&optional (solution (first *solution-paths*)))
  "Build a complete path-derived recorder report for integrated SOLUTION.

The original path remains under :INTEGRATED.  :CYCLES contains one ordered report per
setup/start/window/stop segment, :TRAILING-SETUP preserves searched actions after the last
closed cycle, and :TOTALS describes the complete solution.  A legacy path without an
explicit boundary is represented as one synthesized cycle.  Single-cycle reports retain
the top-level :SETUP, :RECORDING, and :PLAYBACK aliases used by guided chaining.  Report
markers are not planner actions and do not contribute to any metric."
  (unless solution
    (error "No completed solution is available for a recorder report."))
  (unless (solution-p solution)
    (error "Recorder report requires a SOLUTION, not ~S" solution))
  (let ((path (solution.path solution)))
    (multiple-value-bind (cycles trailing-setup diagnostic)
        (parse-recorder-path *start-state* path)
      (when diagnostic
        (error "Recorder report cannot parse the integrated path: ~S" diagnostic))
      (let ((report-cycles (or cycles (list (recorder-legacy-report-cycle path))))
            (report-trailing-setup (and cycles trailing-setup)))
        (multiple-value-bind (cycle-reports cycle-end-state)
            (recorder-report-cycles *start-state* report-cycles)
          (let* ((trailing-end-state
                   (replay-recorder-report-segment
                     cycle-end-state report-trailing-setup
                     "trailing recorder setup"))
                 (trailing-metrics
                   (recorder-report-metrics
                     cycle-end-state trailing-end-state
                     (length report-trailing-setup)))
                 (report
                   (list :integrated path
                         :cycles cycle-reports
                         :cycle-count (length cycle-reports)
                         :trailing-setup report-trailing-setup
                         :trailing-metrics trailing-metrics
                         :totals (recorder-complete-solution-metrics solution))))
            (when (= (length cycle-reports) 1)
              (let ((cycle (first cycle-reports)))
                (setf report
                      (append report
                              (list :setup (getf cycle :setup)
                                    :recording (getf cycle :recording)
                                    :playback (getf cycle :playback))))))
            report))))))


(defun print-recorder-report-sequence (heading sequence stream)
  "Print HEADING and every entry in one report SEQUENCE."
  (format stream "~&~%~A:~%" heading)
  (dolist (entry sequence)
    (format stream "~S~%" entry)))


(defun print-recorder-report-metrics (heading metrics stream)
  "Print one compact local or total recorder metric line."
  (format stream
          "~&~A: depth ~D, elapsed time ~S, value change ~S.~%"
          heading
          (getf metrics :depth)
          (getf metrics :elapsed-time)
          (getf metrics :value-change)))


(defun print-recorder-cycle-report (cycle stream)
  "Print the three phases, closure status, and local metrics for CYCLE."
  (print-recorder-report-sequence "Setup phase" (getf cycle :setup) stream)
  (print-recorder-report-sequence "Recording phase" (getf cycle :recording) stream)
  (print-recorder-report-sequence "Playback phase" (getf cycle :playback) stream)
  (format stream "~&Closure: ~(~A~).~%" (getf cycle :closure))
  (print-recorder-report-metrics "Cycle metrics" cycle stream))


(defun print-recorder-report
    (&optional (solution (first *solution-paths*)) (stream *standard-output*))
  "Print and return SOLUTION's complete single- or multi-cycle recorder report."
  (let* ((report (build-recorder-report solution))
         (cycles (getf report :cycles)))
    (if (= (length cycles) 1)
      (print-recorder-cycle-report (first cycles) stream)
      (dolist (cycle cycles)
        (format stream "~&~%Recorder cycle ~D:~%" (getf cycle :number))
        (print-recorder-cycle-report cycle stream)))
    (when (getf report :trailing-setup)
      (print-recorder-report-sequence
        "Trailing setup" (getf report :trailing-setup) stream)
      (print-recorder-report-metrics
        "Trailing metrics" (getf report :trailing-metrics) stream))
    (print-recorder-report-metrics
      "Complete solution totals" (getf report :totals) stream)
    report))

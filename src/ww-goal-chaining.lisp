;;; Filename: ww-goal-chaining.lisp

;;; Generic ordered-milestone search.  User subgoals remain explicit; when a later
;;; milestone is exhaustively impossible from the selected checkpoint, this controller
;;; restarts the preceding milestone search while excluding only that exact checkpoint
;;; in the failed continuation context.  Problem technologies may register replay and
;;; lifecycle hooks, but checkpoint search, contextual nogoods, propagation, and undo
;;; remain generic planner facilities.

;;; Exhaustion is a proof only when the search actually explored its whole space.  A search
;;; the depth cutoff truncated -- one where a cut node still had successors -- reports
;;; :DEPTH-CUTOFF-TRUNCATED, rejects no checkpoint, and stops the recovery.  A cutoff that
;;; is merely too small therefore surfaces as a report to raise it, instead of an unbounded
;;; walk through every alternative predecessor state.

(in-package :ww)


(defstruct goal-chain-state-key
  stage-generation idb hidb happenings time value policy-context)


(defstruct goal-chain-search-settings bindings random-state policy-context)


(defstruct goal-chain-request goal final-p settings)


(defstruct goal-chain-phase request source-state source-key solution endpoint-key)


(defstruct goal-chain-nogood endpoint-key continuation-key)


(defstruct goal-chain-screening-rejection
  endpoint-key context-key source reason evidence)


(defstruct goal-chain-session
  origin-state original-goal original-goal-function phases nogoods
  screening-rejections)


(defstruct undo-checkpoint
  "Complete planning-session snapshot for one goal-chaining operation."
  start-state goal goal-function-bound-p final-goal solution-paths solutions-valid
  goal-chain-session extension-states)


(defparameter *goal-chain-session* nil
  "Active generic ordered-milestone session, including contextual nogoods.")


(defparameter *undo-stack* nil
  "Stack of independent planning-session snapshots.")


(defvar *final-goal* nil
  "Originally installed goal, restored by SOLVE after intermediate subgoals.")


(defparameter *goal-chain-setting-symbols*
  '(*depth-cutoff* *algorithm* *tree-or-graph* *problem-type* *solution-type*
    *randomize-search* *branch* *symmetry-pruning* *auto-wait*
    *auto-wait-max-time* *min-steps-pruning-enabled*)
  "Generic settings whose values affect milestone feasibility or search order.")


(defmacro solve-subgoal (goal-form)
  `(solve-subgoal-form ',goal-form))


(defun validate-candidate-screening-result (result)
  "Require RESULT to obey the sound tri-state screening contract."
  (unless (candidate-screening-result-p result)
    (error "Candidate screener returned an invalid result: ~S" result))
  (unless (member (candidate-screening-result-status result)
                  '(:impossible :possible :unknown))
    (error "Candidate screener returned an invalid status: ~S"
           (candidate-screening-result-status result)))
  (when (eq (candidate-screening-result-status result) :impossible)
    (unless (and (candidate-screening-result-source result)
                 (candidate-screening-result-reason result))
      (error "An impossible screening result requires a proof source and reason: ~S"
             result)))
  result)


(defun candidate-screening-witness-valid-p (state context witness)
  "Whether WITNESS is a concrete continuation from STATE to the original goal."
  (when (solution-p witness)
    (let ((validation
            (validate-action-sequence state (solution.path witness))))
      (and (action-sequence-validation-success-p validation)
           (funcall
             (candidate-screening-context-final-goal-function context)
             (action-sequence-validation-final-state validation))))))


(defun run-registered-candidate-screeners (state context)
  "Return the first conclusive registered screening result, or NIL."
  (dolist (screener *candidate-state-screeners*)
    (let ((result
            (funcall
              (symbol-function (candidate-state-screener-function screener))
              state context)))
      (when result
        (validate-candidate-screening-result result)
        (case (candidate-screening-result-status result)
          (:impossible (return result))
          (:possible
           (unless (candidate-screening-witness-valid-p
                     state context (candidate-screening-result-witness result))
             (error "Candidate screener ~S supplied an invalid continuation witness."
                    (candidate-state-screener-name screener)))
           (return result)))))))


(defun screen-candidate-state (state context)
  "Soundly classify STATE against the original final goal.

The result is :IMPOSSIBLE only for a proof, :POSSIBLE only for a concrete
continuation, and :UNKNOWN otherwise."
  (cond
    ((state-is-inconsistent state)
     (make-candidate-screening-result
       :status :impossible :source :engine
       :reason :inconsistent-state))
    ((funcall (candidate-screening-context-final-goal-function context) state)
     (make-candidate-screening-result
       :status :possible :source :engine :reason :goal-already-satisfied
       :witness
         (make-solution
           :depth 0 :time (problem-state.time state)
           :value (problem-state.value state) :path nil
           :goal (copy-problem-state state))))
    ((eq (registered-relaxed-goal-reachability
           state (candidate-screening-context-final-goal context))
         :unreachable)
     (make-candidate-screening-result
       :status :impossible :source :complete-relaxed-reachability
       :reason :final-goal-unreachable))
    (t
     (or (run-registered-candidate-screeners state context)
         (make-candidate-screening-result
           :status :unknown :source :engine :reason :no-proof)))))


(defun hash-table-exact-snapshot (table)
  "Return a stable, independent representation of TABLE for exact identity."
  (sort
    (loop for key being the hash-keys of table using (hash-value value)
          collect (cons (copy-tree key) (copy-tree value)))
    #'string< :key (lambda (entry) (write-to-string (car entry)))))


(defun goal-chain-policy-function (accessor)
  "Return the active policy function named by ACCESSOR, when present."
  (when *goal-chaining-policy*
    (funcall accessor *goal-chaining-policy*)))


(defun goal-chain-policy-state-context-value (state phases)
  "Return technology-owned identity context for STATE and tentative PHASES."
  (let ((function-name
          (goal-chain-policy-function #'goal-chaining-policy-state-context)))
    (when function-name
      (funcall (symbol-function function-name) state phases))))


(defun make-exact-goal-chain-state-key (state &optional phases)
  "Capture every generic field that makes STATE an exact reusable checkpoint."
  (make-goal-chain-state-key
    :stage-generation *goal-chain-stage-generation*
    :idb (hash-table-exact-snapshot (problem-state.idb state))
    :hidb (hash-table-exact-snapshot (problem-state.hidb state))
    :happenings (copy-tree (problem-state.happenings state))
    :time (problem-state.time state)
    :value (problem-state.value state)
    :policy-context
      (copy-tree (goal-chain-policy-state-context-value state phases))))


(defun goal-chain-state-key-equal-p (key1 key2)
  "Whether KEY1 and KEY2 identify the same exact staged checkpoint."
  (equalp key1 key2))


(defun capture-goal-chain-search-settings ()
  "Snapshot generic search settings plus technology-owned policy settings."
  (let ((snapshotter
          (goal-chain-policy-function
            #'goal-chaining-policy-settings-snapshotter)))
    (make-goal-chain-search-settings
      :bindings
        (loop for symbol in *goal-chain-setting-symbols*
              collect (cons symbol (copy-tree (symbol-value symbol))))
      :random-state (make-random-state *random-state*)
      :policy-context
        (when snapshotter
          (funcall (symbol-function snapshotter))))))


(defun call-with-goal-chain-search-settings (settings thunk)
  "Call THUNK under the exact generic and technology settings in SETTINGS."
  (let* ((bindings (goal-chain-search-settings-bindings settings))
         (symbols (mapcar #'car bindings))
         (values (mapcar #'cdr bindings))
         (runner
           (goal-chain-policy-function #'goal-chaining-policy-settings-runner)))
    (progv symbols values
      (let ((*random-state*
              (make-random-state
                (goal-chain-search-settings-random-state settings))))
        (if runner
          (funcall (symbol-function runner)
                   (goal-chain-search-settings-policy-context settings) thunk)
          (funcall thunk))))))


(defun goal-chain-search-settings-key (settings)
  "Return the feasibility-relevant identity of SETTINGS."
  (list (copy-tree (goal-chain-search-settings-bindings settings))
        (copy-tree (goal-chain-search-settings-policy-context settings))))


(defun goal-chain-request-key (request)
  "Return REQUEST's contextual identity for bounded nogoods."
  (list (copy-tree (goal-chain-request-goal request))
        (goal-chain-request-final-p request)
        (goal-chain-search-settings-key (goal-chain-request-settings request))))


(defun goal-chain-continuation-key (requests index)
  "Return the complete ordered continuation after milestone INDEX."
  (mapcar #'goal-chain-request-key (nthcdr (1+ index) requests)))


(defun copy-goal-chain-search-settings-deeply (settings)
  (make-goal-chain-search-settings
    :bindings (copy-tree (goal-chain-search-settings-bindings settings))
    :random-state
      (make-random-state (goal-chain-search-settings-random-state settings))
    :policy-context
      (copy-tree (goal-chain-search-settings-policy-context settings))))


(defun copy-goal-chain-request-deeply (request)
  (make-goal-chain-request
    :goal (copy-tree (goal-chain-request-goal request))
    :final-p (goal-chain-request-final-p request)
    :settings
      (copy-goal-chain-search-settings-deeply
        (goal-chain-request-settings request))))


(defun copy-goal-chain-phase-deeply (phase)
  (make-goal-chain-phase
    :request (copy-goal-chain-request-deeply (goal-chain-phase-request phase))
    :source-state (copy-problem-state (goal-chain-phase-source-state phase))
    :source-key (copy-goal-chain-state-key (goal-chain-phase-source-key phase))
    :solution (copy-solution-deeply (goal-chain-phase-solution phase))
    :endpoint-key (copy-goal-chain-state-key (goal-chain-phase-endpoint-key phase))))


(defun copy-goal-chain-session-deeply (&optional (session *goal-chain-session*))
  "Return an independent copy of SESSION for undo and transactional recovery."
  (when session
    (make-goal-chain-session
      :origin-state (copy-problem-state (goal-chain-session-origin-state session))
      :original-goal (copy-tree (goal-chain-session-original-goal session))
      :original-goal-function (goal-chain-session-original-goal-function session)
      :phases (mapcar #'copy-goal-chain-phase-deeply
                      (goal-chain-session-phases session))
      :nogoods
        (loop for nogood in (goal-chain-session-nogoods session)
              collect
                (make-goal-chain-nogood
                  :endpoint-key
                    (copy-goal-chain-state-key
                      (goal-chain-nogood-endpoint-key nogood))
                  :continuation-key
                    (copy-tree
                      (goal-chain-nogood-continuation-key nogood))))
      :screening-rejections
        (loop for rejection in
                (goal-chain-session-screening-rejections session)
              collect
                (make-goal-chain-screening-rejection
                  :endpoint-key
                    (copy-goal-chain-state-key
                      (goal-chain-screening-rejection-endpoint-key rejection))
                  :context-key
                    (copy-tree
                      (goal-chain-screening-rejection-context-key rejection))
                  :source (goal-chain-screening-rejection-source rejection)
                  :reason (goal-chain-screening-rejection-reason rejection)
                  :evidence
                    (copy-tree
                      (goal-chain-screening-rejection-evidence rejection)))))))


(defun capture-goal-chaining-extension-states ()
  "Capture every registered extension and retain the restorer with its snapshot."
  (loop for (name snapshotter restorer) in *goal-chaining-checkpoint-extensions*
        collect (list name restorer (funcall (symbol-function snapshotter)))))


(defun restore-goal-chaining-extension-states (states)
  "Restore checkpoint extension STATES captured by SAVE-UNDO-CHECKPOINT."
  (dolist (state states)
    (funcall (symbol-function (second state)) (third state)))
  t)


(defun install-compiled-goal (goal-form)
  "Install GOAL-FORM, compile GOAL-FN, and refresh goal-constrained symmetry."
  (install-goal goal-form)
  (when (boundp 'goal-fn)
    (compile 'goal-fn (subst-int-code (symbol-value 'goal-fn))))
  (refresh-symmetry-detection)
  goal-form)


(defun validate-continuation-preconditions ()
  "Verify that the current planning session can begin a continuation operation."
  (unless (zerop *threads*)
    (error "Goal chaining requires single-threaded mode. ~
            Set (ww-set *threads* 0) before using solve-subgoal."))
  (unless (boundp 'goal-fn)
    (error "No goal function currently defined."))
  (when (and *solutions-valid* (null *solution-paths*))
    (error "*SOLUTIONS-VALID* is true but *SOLUTION-PATHS* is empty."))
  t)


(defun select-continuation-solution (&optional (solutions *solution-paths*))
  "Select SOLUTIONS' canonical member using the search's own preference rule."
  (unless solutions
    (error "No completed solution is available for continuation."))
  (reduce
    (lambda (best candidate)
      (if (solution-better-p candidate best) candidate best))
    solutions))


(defun extract-goal-state-from-solution ()
  "Return the consistent final state of the canonical preceding solution."
  (let ((goal-state (solution.goal (select-continuation-solution))))
    (when (state-is-inconsistent goal-state)
      (error "Cannot continue from inconsistent goal state."))
    goal-state))


(defun update-start-state-from-goal (goal-state)
  "Replace *START-STATE* with an independent continuation copy of GOAL-STATE."
  (declare (type problem-state goal-state))
  (let ((continuation (copy-problem-state goal-state)))
    (setf (problem-state.name continuation) 'continuation
          (problem-state.instantiations continuation) nil
          *start-state* continuation)))


(defun save-undo-checkpoint ()
  "Push an independent snapshot of the current goal-chaining session."
  (push
    (make-undo-checkpoint
      :start-state (copy-problem-state *start-state*)
      :goal (copy-tree *goal*)
      :goal-function-bound-p (boundp 'goal-fn)
      :final-goal (copy-tree *final-goal*)
      :solution-paths (copy-solutions-deeply *solution-paths*)
      :solutions-valid *solutions-valid*
      :goal-chain-session (copy-goal-chain-session-deeply)
      :extension-states (capture-goal-chaining-extension-states))
    *undo-stack*))


(defun restore-checkpoint-goal (checkpoint)
  "Restore both the user goal and the executable GOAL-FN from CHECKPOINT."
  (if (undo-checkpoint-goal-function-bound-p checkpoint)
    (install-compiled-goal (copy-tree (undo-checkpoint-goal checkpoint)))
    (progn
      (setf *goal* (copy-tree (undo-checkpoint-goal checkpoint)))
      (when (boundp 'goal-fn) (makunbound 'goal-fn))
      (when (fboundp 'goal-fn) (fmakunbound 'goal-fn))
      (remprop 'goal-fn :form))))


(defun restore-undo-checkpoint (checkpoint)
  "Restore the complete planning session captured in CHECKPOINT."
  (setf *start-state* (undo-checkpoint-start-state checkpoint)
        *final-goal* (copy-tree (undo-checkpoint-final-goal checkpoint))
        *solution-paths* (undo-checkpoint-solution-paths checkpoint)
        *solutions-valid* (undo-checkpoint-solutions-valid checkpoint)
        *goal-chain-session* (undo-checkpoint-goal-chain-session checkpoint))
  (restore-goal-chaining-extension-states
    (undo-checkpoint-extension-states checkpoint))
  (restore-checkpoint-goal checkpoint)
  t)


(defun ww-undo ()
  "Undo one user goal-chaining command, including any automatic recovery it caused."
  (if (null *undo-stack*)
    (format t "~&Nothing to undo.~%")
    (let ((checkpoint (pop *undo-stack*)))
      (restore-undo-checkpoint checkpoint)
      (format t "~&Reverted the last goal-chaining command.~2%")
      (format t "Current State: ~%~A~%" *start-state*)
      (format t "Current Goal: ~%~A~2%" *goal*)
      t)))


(defun continue-from-solution (goal-form)
  "Legacy one-step continuation helper retained for direct callers."
  (validate-continuation-preconditions)
  (save-undo-checkpoint)
  (unless *final-goal* (setf *final-goal* (copy-tree *goal*)))
  (when *solutions-valid*
    (update-start-state-from-goal (extract-goal-state-from-solution)))
  (install-compiled-goal goal-form)
  (setf *solutions-valid* nil)
  *start-state*)


(defun start-goal-chain-session ()
  "Start a generic chain at the current baseline, consuming a prior solution if present."
  (when *solutions-valid*
    (update-start-state-from-goal (extract-goal-state-from-solution)))
  (setf *goal-chain-session*
        (make-goal-chain-session
          :origin-state (copy-problem-state *start-state*)
          :original-goal (copy-tree *goal*)
          :original-goal-function (symbol-function 'goal-fn)
          :phases nil :nogoods nil :screening-rejections nil)
        *final-goal* (copy-tree *goal*)
        *solution-paths* nil
        *solutions-valid* nil))


(defun make-goal-chain-phase-from-solution (request source-state prefix solution)
  "Build one candidate phase and its exact source and endpoint identities."
  (let ((phase
          (make-goal-chain-phase
            :request request
            :source-state (copy-problem-state source-state)
            :source-key (make-exact-goal-chain-state-key source-state prefix)
            :solution (copy-solution-deeply solution))))
    (setf (goal-chain-phase-endpoint-key phase)
          (make-exact-goal-chain-state-key
            (solution.goal solution) (append prefix (list phase))))
    phase))


(defun contextual-goal-chain-nogood-p (session endpoint-key continuation-key)
  "Whether ENDPOINT-KEY was proved unsuitable for this exact continuation."
  (find-if
    (lambda (nogood)
      (and (goal-chain-state-key-equal-p
             endpoint-key (goal-chain-nogood-endpoint-key nogood))
           (equalp continuation-key
                   (goal-chain-nogood-continuation-key nogood))))
    (goal-chain-session-nogoods session)))


(defun record-goal-chain-nogood (session endpoint-key continuation-key)
  "Record one proven bounded contextual rejection, without global invalidation."
  (unless (contextual-goal-chain-nogood-p session endpoint-key continuation-key)
    (push
      (make-goal-chain-nogood
        :endpoint-key (copy-goal-chain-state-key endpoint-key)
        :continuation-key (copy-tree continuation-key))
      (goal-chain-session-nogoods session)))
  (format t "~&Checkpoint rejected for this continuation after exhaustive search.~%"))


(defun goal-chain-request-depth-cutoff (request)
  "Return REQUEST's saved local depth cutoff."
  (cdr
    (assoc '*depth-cutoff*
           (goal-chain-search-settings-bindings
             (goal-chain-request-settings request)))))


(defun goal-chain-remaining-depth-budget (requests index)
  "Return a sound total remaining budget after INDEX, or NIL when unrestricted.

The sum is applicable only when the known suffix ends in the original final
request and every remaining phase has a positive local cutoff."
  (let ((suffix (nthcdr (1+ index) requests)))
    (when (and suffix
               (goal-chain-request-final-p (car (last suffix)))
               (every (lambda (request)
                        (let ((cutoff (goal-chain-request-depth-cutoff request)))
                          (and (integerp cutoff) (plusp cutoff))))
                      suffix))
      (reduce #'+ suffix :key #'goal-chain-request-depth-cutoff))))


(defun goal-chain-candidate-screening-context
    (session requests index continuation-key)
  "Build the exact sound-screening context for a candidate at INDEX."
  (make-candidate-screening-context
    :final-goal (copy-tree (goal-chain-session-original-goal session))
    :final-goal-function (goal-chain-session-original-goal-function session)
    :remaining-depth-budget
      (goal-chain-remaining-depth-budget requests index)
    :continuation-key (copy-tree continuation-key)))


(defun goal-chain-screening-context-key (context)
  "Return the cache identity of CONTEXT without its function object."
  (list (copy-tree (candidate-screening-context-final-goal context))
        (candidate-screening-context-remaining-depth-budget context)
        (copy-tree (candidate-screening-context-continuation-key context))))


(defun goal-chain-screening-rejection
    (session endpoint-key context-key)
  "Return a retained proof rejection for this exact state and context."
  (find-if
    (lambda (rejection)
      (and
        (goal-chain-state-key-equal-p
          endpoint-key
          (goal-chain-screening-rejection-endpoint-key rejection))
        (equalp context-key
                (goal-chain-screening-rejection-context-key rejection))))
    (goal-chain-session-screening-rejections session)))


(defun record-goal-chain-screening-rejection
    (session endpoint-key context-key result)
  "Retain one sound proof separately from exhaustive contextual nogoods."
  (unless (goal-chain-screening-rejection session endpoint-key context-key)
    (push
      (make-goal-chain-screening-rejection
        :endpoint-key (copy-goal-chain-state-key endpoint-key)
        :context-key (copy-tree context-key)
        :source (candidate-screening-result-source result)
        :reason (candidate-screening-result-reason result)
        :evidence (copy-tree (candidate-screening-result-evidence result)))
      (goal-chain-session-screening-rejections session)))
  (format t "~&Checkpoint proven impossible for the original goal (~A: ~A)"
          (candidate-screening-result-source result)
          (candidate-screening-result-reason result))
  (when (second context-key)
    (format t " within remaining depth ~D" (second context-key)))
  (format t "; trying an alternative.~%"))


(defun goal-chain-phase-screened-out-p
    (session phase screening-context)
  "Whether PHASE is cached or newly proven impossible in SCREENING-CONTEXT."
  (let* ((endpoint-key (goal-chain-phase-endpoint-key phase))
         (context-key (goal-chain-screening-context-key screening-context))
         (cached
           (goal-chain-screening-rejection session endpoint-key context-key)))
    (cond
      (cached
       (format t
               "~&Skipping a checkpoint already proven impossible (~A: ~A).~%"
               (goal-chain-screening-rejection-source cached)
               (goal-chain-screening-rejection-reason cached))
       t)
      (t
       (let ((result
               (screen-candidate-state
                 (solution.goal (goal-chain-phase-solution phase))
                 screening-context)))
         (when (eq (candidate-screening-result-status result) :impossible)
           (record-goal-chain-screening-rejection
             session endpoint-key context-key result)
           t))))))


(defun goal-chain-phase-reusable-p (phase request source-state prefix)
  "Whether stable PHASE is the same request from the same exact source context."
  (and (equalp (goal-chain-request-key (goal-chain-phase-request phase))
               (goal-chain-request-key request))
       (goal-chain-state-key-equal-p
         (goal-chain-phase-source-key phase)
         (make-exact-goal-chain-state-key source-state prefix))))


(defun run-goal-chain-planner ()
  "Run the active policy search runner, or ordinary WW-SOLVE."
  (let ((runner
          (goal-chain-policy-function #'goal-chaining-policy-search-runner)))
    (if runner (funcall (symbol-function runner)) (ww-solve))))


(defun validate-goal-chain-prefix-generically (session phases)
  "Replay every generic segment and require every exact endpoint to recur."
  (let ((current-state (copy-problem-state (goal-chain-session-origin-state session)))
        (replayed-phases nil))
    (dolist (phase phases t)
      (setf replayed-phases (append replayed-phases (list phase)))
      (let* ((solution (goal-chain-phase-solution phase))
             (validation
               (validate-action-sequence current-state (solution.path solution))))
        (unless (action-sequence-validation-success-p validation)
          (error "Milestone replay failed for ~S: ~S"
                 (goal-chain-request-goal (goal-chain-phase-request phase))
                 (action-sequence-validation-failure-reason validation)))
        (let ((replayed-state
                (action-sequence-validation-final-state validation)))
          (unless (goal-chain-state-key-equal-p
                    (make-exact-goal-chain-state-key replayed-state replayed-phases)
                    (make-exact-goal-chain-state-key
                      (solution.goal solution) replayed-phases))
            (error "Milestone replay did not reproduce checkpoint ~S."
                   (goal-chain-request-goal (goal-chain-phase-request phase))))
          (setf current-state replayed-state))))))


(defun validate-goal-chain-prefix (session phases)
  "Validate tentative PHASES through generic replay and the active policy hook."
  (validate-goal-chain-prefix-generically session phases)
  (let ((validator
          (goal-chain-policy-function #'goal-chaining-policy-prefix-validator)))
    (if validator (funcall (symbol-function validator) session phases) t)))


(defun phase-solution-from-path (path state)
  "Construct the solution record needed to identify a candidate during search."
  (make-solution :depth (length path) :time (problem-state.time state)
                 :value (problem-state.value state) :path (copy-tree path)
                 :goal (copy-problem-state state)))


(defun run-goal-chain-phase-search
    (session request source-state prefix continuation-key screening-context)
  "Search REQUEST once, excluding exact endpoints rejected for CONTINUATION-KEY."
  (call-with-goal-chain-search-settings
    (goal-chain-request-settings request)
    (lambda ()
      (setf *start-state* (copy-problem-state source-state))
      (install-compiled-goal (goal-chain-request-goal request))
      (let ((*goal-chain-candidate-rejector*
              (lambda (path state)
                (let* ((solution (phase-solution-from-path path state))
                       (phase
                         (make-goal-chain-phase-from-solution
                           request source-state prefix solution)))
                  (or
                    (contextual-goal-chain-nogood-p
                      session (goal-chain-phase-endpoint-key phase)
                      continuation-key)
                    (goal-chain-phase-screened-out-p
                      session phase screening-context))))))
        (run-goal-chain-planner))))
  (values *last-search-outcome*
          (when *solutions-valid*
            (make-goal-chain-phase-from-solution
              request source-state prefix (select-continuation-solution)))))


(defun try-goal-chain-from
    (session requests stable-phases index source-state prefix)
  "Recursively retain or replace milestones from INDEX through REQUESTS."
  (when (= index (length requests))
    (return-from try-goal-chain-from (values :solution prefix nil)))
  (let* ((request (nth index requests))
         (continuation-key (goal-chain-continuation-key requests index))
         (screening-context
           (goal-chain-candidate-screening-context
             session requests index continuation-key))
         (stable-phase (nth index stable-phases)))
    (when (and stable-phase
               (goal-chain-phase-reusable-p stable-phase request source-state prefix))
      (if (or
            (goal-chain-phase-screened-out-p
              session stable-phase screening-context)
            (contextual-goal-chain-nogood-p
              session (goal-chain-phase-endpoint-key stable-phase) continuation-key))
        (format t "~&Skipping a checkpoint already rejected for this continuation.~%")
        (multiple-value-bind (status phases reason)
            (try-goal-chain-from
              session requests stable-phases (1+ index)
              (solution.goal (goal-chain-phase-solution stable-phase))
              (append prefix (list stable-phase)))
          (case status
            (:solution
             (return-from try-goal-chain-from (values status phases reason)))
            (:unknown
             (return-from try-goal-chain-from (values status phases reason)))
            (:exhausted
             (record-goal-chain-nogood
               session (goal-chain-phase-endpoint-key stable-phase)
               continuation-key)
             (format t "Retrying milestone ~D from its preceding checkpoint.~%"
                     (1+ index)))))))
    (loop
      (multiple-value-bind (outcome phase)
          (run-goal-chain-phase-search
            session request source-state prefix continuation-key screening-context)
        (case (search-outcome-status outcome)
          ((:solution :exhausted-with-solutions)
           (let ((tentative-prefix (append prefix (list phase))))
             (validate-goal-chain-prefix session tentative-prefix)
             (multiple-value-bind (status phases reason)
                 (try-goal-chain-from
                   session requests nil (1+ index)
                   (solution.goal (goal-chain-phase-solution phase))
                   tentative-prefix)
               (case status
                 (:solution
                  (return-from try-goal-chain-from (values status phases reason)))
                 (:unknown
                  (return-from try-goal-chain-from (values status phases reason)))
                 (:exhausted
                  (record-goal-chain-nogood
                    session (goal-chain-phase-endpoint-key phase)
                    continuation-key)
                  (format t "Trying another distinct state for milestone ~D.~%"
                          (1+ index)))))))
          (:exhausted-no-solution
           (when (eq (search-outcome-reason outcome) :depth-cutoff-truncated)
             (format t "~&Milestone ~D search was truncated by the depth cutoff; ~
                        no checkpoint was rejected.~%"
                     (1+ index))
             (return-from try-goal-chain-from
               (values :unknown nil :depth-cutoff-truncated)))
           (format t "~&Milestone ~D has no remaining states for this continuation.~%"
                   (1+ index))
           (return-from try-goal-chain-from (values :exhausted nil :complete)))
          (otherwise
           (format t "~&Milestone search is unknown (~A); no checkpoint was rejected.~%"
                   (search-outcome-reason outcome))
           (return-from try-goal-chain-from
             (values :unknown nil (search-outcome-reason outcome)))))))))


(defun goal-chain-cumulative-path (phases)
  "Return a fresh concatenation of PHASES' action paths."
  (loop for phase in phases
        append (copy-tree (solution.path (goal-chain-phase-solution phase)))))


(defun make-goal-chain-cumulative-solution (phases)
  "Build one ordinary solution record from PHASES."
  (let* ((last-phase (car (last phases)))
         (last-solution (goal-chain-phase-solution last-phase))
         (path (goal-chain-cumulative-path phases)))
    (make-solution :depth (length path) :time (solution.time last-solution)
                   :value (solution.value last-solution) :path path
                   :goal (copy-problem-state (solution.goal last-solution)))))


(defun commit-generic-goal-chain (session phases final-p)
  "Install PHASES as the active generic chain and expose its stable result."
  (let* ((last-phase (car (last phases)))
         (last-solution (goal-chain-phase-solution last-phase)))
    (if final-p
      (progn
        (install-compiled-goal (goal-chain-session-original-goal session))
        (setf *start-state* (copy-problem-state (goal-chain-session-origin-state session))
              *solution-paths* (list (make-goal-chain-cumulative-solution phases))
              *solutions-valid* t *final-goal* nil))
      (progn
        (install-compiled-goal
          (goal-chain-request-goal (goal-chain-phase-request last-phase)))
        (update-start-state-from-goal (solution.goal last-solution))
        (setf *solution-paths* nil *solutions-valid* nil)))
    (format t "~&Milestone chain accepted through checkpoint ~D.~%" (length phases))
    *solution-paths*))


(defun commit-goal-chain (session phases final-p)
  "Commit a successful replacement through the active technology hook or generically."
  (setf (goal-chain-session-phases session) phases)
  (let ((handler
          (goal-chain-policy-function #'goal-chaining-policy-commit-handler)))
    (if handler
      (funcall (symbol-function handler) session phases final-p)
      (commit-generic-goal-chain session phases final-p))))


(defun restore-stable-goal-chain-baseline (stable-phases attempted-goal)
  "Restore the last committed prefix after an exhausted or unknown request."
  (if stable-phases
    (update-start-state-from-goal
      (solution.goal (goal-chain-phase-solution (car (last stable-phases)))))
    (setf *start-state*
          (copy-problem-state (goal-chain-session-origin-state *goal-chain-session*))))
  (install-compiled-goal attempted-goal)
  (setf *solution-paths* nil *solutions-valid* nil))


(defun run-goal-chain-request (goal-form final-p)
  "Append one user request and search the ordered milestones with backward recovery."
  (validate-continuation-preconditions)
  (when (and *goal-chain-session*
             (some (lambda (phase)
                     (goal-chain-request-final-p (goal-chain-phase-request phase)))
                   (goal-chain-session-phases *goal-chain-session*)))
    (error "The active goal chain is already complete."))
  (save-undo-checkpoint)
  (unless *goal-chain-session* (start-goal-chain-session))
  (let* ((session *goal-chain-session*)
         (stable-phases
           (mapcar #'copy-goal-chain-phase-deeply
                   (goal-chain-session-phases session)))
         (request
           (make-goal-chain-request
             :goal (copy-tree goal-form) :final-p final-p
             :settings (capture-goal-chain-search-settings)))
         (requests
           (append (mapcar #'goal-chain-phase-request stable-phases)
                   (list request)))
         (completed nil))
    (unwind-protect
        (multiple-value-bind (status phases reason)
            (try-goal-chain-from
              session requests stable-phases 0
              (goal-chain-session-origin-state session) nil)
          (declare (ignore reason))
          (setf completed t)
          (case status
            (:solution (commit-goal-chain session phases final-p))
            (:exhausted
             (restore-stable-goal-chain-baseline stable-phases goal-form)
             (format t "~&No milestone chain satisfies this bounded continuation. ~
                        The preceding committed chain is unchanged.~%")
             nil)
            (:unknown
             (restore-stable-goal-chain-baseline stable-phases goal-form)
             (format t "~&Recovery stopped without proof. The preceding committed chain ~
                        is unchanged and no unknown result became a nogood.~%")
             nil)))
      (unless completed
        (restore-stable-goal-chain-baseline stable-phases goal-form)
        (setf *last-search-outcome*
              (make-search-outcome :status :unknown :reason :interrupted))
        (format t "~&Goal-chain recovery was interrupted; no interrupted candidate was ~
                   rejected.~%")))))


(defun solve-generic-subgoal-form (goal-form)
  "Solve GOAL-FORM as the next generic ordered milestone."
  (run-goal-chain-request goal-form nil))


(defun solve-generic-final ()
  "Finish the generic milestone chain with its original final goal."
  (unless *goal-chain-session* (error "No goal chain is active."))
  (run-goal-chain-request
    (copy-tree (goal-chain-session-original-goal *goal-chain-session*)) t))


(defun solve-subgoal-form (goal-form)
  "Solve GOAL-FORM through the staged problem's goal-chaining policy."
  (if *goal-chaining-policy*
    (funcall
      (symbol-function
        (goal-chaining-policy-subgoal-solver *goal-chaining-policy*)) goal-form)
    (solve-generic-subgoal-form goal-form)))

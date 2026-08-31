;;; Filename: ww-goal-chain-persistence.lisp

;;; Human-readable persistence for accepted ordered-milestone progress.  The text archive
;;; stores goals, search settings, action segments, and symbolic checkpoint signatures.
;;; Import never installs serialized planner structures: it replays every action from the
;;; freshly staged origin, reruns generic and technology-owned validation, and constructs
;;; new stage-local state keys and recorder projections transactionally.

(in-package :ww)


(defparameter *subgoal-progress-format-version* 1)


(defparameter *subgoal-progress-temp-counter* 0)


(defun subgoal-progress-sort-key (object)
  "Return OBJECT's stable readable spelling for canonical sorting."
  (with-standard-io-syntax
    (let ((*package* (find-package :ww))
          (*print-readably* t))
      (write-to-string object))))


(defun canonical-subgoal-progress-propositions (table)
  "Return TABLE's propositions as stable, symbolic, human-readable data."
  (sort (copy-tree (list-database table))
        #'string< :key #'subgoal-progress-sort-key))


(defun make-subgoal-progress-state-signature (state &optional phases)
  "Return a readable exact replay signature for STATE in PHASES' policy context."
  (list
    :facts
      (canonical-subgoal-progress-propositions (problem-state.idb state))
    :happening-facts
      (canonical-subgoal-progress-propositions (problem-state.hidb state))
    :happenings (copy-tree (problem-state.happenings state))
    :time (problem-state.time state)
    :value (problem-state.value state)
    :policy-context
      (copy-tree (goal-chain-policy-state-context-value state phases))))


(defun goal-chaining-policy-signature ()
  "Return a readable identity for the active goal-chaining policy."
  (if (null *goal-chaining-policy*)
    :generic
    (list
      (goal-chaining-policy-subgoal-solver *goal-chaining-policy*)
      (goal-chaining-policy-final-solver *goal-chaining-policy*)
      (goal-chaining-policy-search-runner *goal-chaining-policy*)
      (goal-chaining-policy-prefix-validator *goal-chaining-policy*)
      (goal-chaining-policy-commit-handler *goal-chaining-policy*)
      (goal-chaining-policy-state-context *goal-chaining-policy*)
      (goal-chaining-policy-settings-snapshotter *goal-chaining-policy*)
      (goal-chaining-policy-settings-runner *goal-chaining-policy*))))


(defun active-goal-chain-policy-settings ()
  "Return the active policy's current feasibility-setting snapshot."
  (let ((snapshotter
          (goal-chain-policy-function
            #'goal-chaining-policy-settings-snapshotter)))
    (when snapshotter
      (funcall (symbol-function snapshotter)))))


(defun goal-chain-search-settings-progress-record (settings)
  "Return SETTINGS' readable, feasibility-relevant persisted representation."
  (list
    :bindings
      (copy-tree (goal-chain-search-settings-bindings settings))
    :policy-settings
      (copy-tree (goal-chain-search-settings-policy-context settings))))


(defun goal-chain-phase-progress-record (phase phases number)
  "Return one human-readable persistence record for PHASE."
  (let* ((request (goal-chain-phase-request phase))
         (solution (goal-chain-phase-solution phase)))
    (list
      :number number
      :goal (copy-tree (goal-chain-request-goal request))
      :final-p (goal-chain-request-final-p request)
      :settings
        (goal-chain-search-settings-progress-record
          (goal-chain-request-settings request))
      :depth (solution.depth solution)
      :time (solution.time solution)
      :value (solution.value solution)
      :actions (copy-tree (solution.path solution))
      :endpoint
        (make-subgoal-progress-state-signature
          (solution.goal solution) phases))))


(defun make-subgoal-progress-record ()
  "Build a replay archive from the active incomplete goal chain."
  (validate-continuation-preconditions)
  (unless *goal-chain-session*
    (error "No active subgoal chain is available to export."))
  (let ((phases (goal-chain-session-phases *goal-chain-session*)))
    (unless phases
      (error "The active subgoal chain has no accepted checkpoints."))
    (when (some (lambda (phase)
                  (goal-chain-request-final-p
                    (goal-chain-phase-request phase)))
                phases)
      (error "The active goal chain is already complete; export an incomplete checkpoint chain."))
    (when *solutions-valid*
      (error "A completed solution is active rather than incomplete subgoal progress."))
    (validate-goal-chain-prefix *goal-chain-session* phases)
    (unless
      (equalp
        (make-subgoal-progress-state-signature *start-state* phases)
        (make-subgoal-progress-state-signature
          (solution.goal
            (goal-chain-phase-solution (car (last phases))))
          phases))
      (error "The current search baseline does not match the last accepted checkpoint."))
    (let ((prefix nil)
          (checkpoints nil))
      (loop for phase in phases
            for number from 1
            do (setf prefix (append prefix (list phase)))
               (setf checkpoints
                     (append checkpoints
                             (list
                               (goal-chain-phase-progress-record
                                 phase prefix number)))))
      (list
        :wouldwork-subgoal-progress
        :version *subgoal-progress-format-version*
        :problem *problem-name*
        :policy (goal-chaining-policy-signature)
        :original-goal
          (copy-tree
            (goal-chain-session-original-goal *goal-chain-session*))
        :origin
          (make-subgoal-progress-state-signature
            (goal-chain-session-origin-state *goal-chain-session*) nil)
        :checkpoints checkpoints))))


(defun subgoal-progress-pathname (path)
  "Return PATH as an absolute .txt pathname."
  (let* ((provided (pathname path))
         (typed
           (if (pathname-type provided)
             provided
             (make-pathname :type "txt" :defaults provided))))
    (unless (string-equal (pathname-type typed) "txt")
      (error "Subgoal progress exports must use a .txt filename: ~A" path))
    (merge-pathnames typed *default-pathname-defaults*)))


(defun subgoal-progress-temporary-pathname (target)
  "Return an unused sibling temporary pathname for TARGET."
  (loop for counter from (1+ *subgoal-progress-temp-counter*)
        for candidate =
      (make-pathname
        :name
          (format nil ".~A-progress-~D-~D"
                  (pathname-name target)
                  (get-universal-time)
                  counter)
        :type "tmp"
        :defaults target)
        unless (probe-file candidate)
          do (setf *subgoal-progress-temp-counter* counter)
             (return candidate)))


(defun subgoal-progress-checkpoint-action-count (record)
  "Return RECORD's cumulative accepted action count."
  (loop for checkpoint in (getf (rest record) :checkpoints)
        sum (length (getf checkpoint :actions))))


(defun subgoal-progress-last-policy-context (record)
  "Return RECORD's final checkpoint policy context."
  (let* ((checkpoints (getf (rest record) :checkpoints))
         (endpoint (getf (car (last checkpoints)) :endpoint)))
    (getf endpoint :policy-context)))


(defun write-subgoal-progress-record (record stream)
  "Write RECORD to STREAM as commented, pretty, readable text."
  (let* ((body (rest record))
         (checkpoints (getf body :checkpoints))
         (policy-context (subgoal-progress-last-policy-context record)))
    (format stream ";;; Wouldwork subgoal progress~%")
    (format stream ";;; Problem: ~A~%" (getf body :problem))
    (format stream ";;; Accepted checkpoints: ~D~%" (length checkpoints))
    (format stream ";;; Cumulative actions: ~D~%"
            (subgoal-progress-checkpoint-action-count record))
    (when policy-context
      (let ((cycle-count (getf policy-context :cycle-count))
            (recording-open-p (getf policy-context :recording-open-p)))
        (when cycle-count
          (format stream ";;; Recorder: cycle ~D ~:[closed~;open~]~%"
                  cycle-count recording-open-p))))
    (format stream ";;; Restore after staging this problem with IMPORT-SUBGOAL-PROGRESS.~%")
    (format stream ";;; This file is readable data. Import replays and validates every action.~2%")
    (with-standard-io-syntax
      (let ((*package* (find-package :ww))
            (*print-pretty* t)
            (*print-right-margin* 100)
            (*print-readably* t))
        (pprint record stream)
        (terpri stream)))))


(defun write-subgoal-progress-file (record target)
  "Atomically replace TARGET with RECORD's human-readable text."
  (ensure-directories-exist target)
  (let ((temporary (subgoal-progress-temporary-pathname target))
        (completed nil))
    (unwind-protect
        (progn
          (with-open-file
              (stream temporary :direction :output :if-exists :error
                                :if-does-not-exist :create)
            (write-subgoal-progress-record record stream))
          (uiop:rename-file-overwriting-target temporary target)
          (setf completed t))
      (unless completed
        (uiop:delete-file-if-exists temporary))))
  target)


(defun export-subgoal-progress (path)
  "Export the active accepted subgoal chain to a human-readable .txt file."
  (let* ((target (subgoal-progress-pathname path))
         (record (make-subgoal-progress-record)))
    (write-subgoal-progress-file record target)
    (format t "~&Exported ~D checkpoint~:P and ~D action~:P to ~A.~%"
            (length (getf (rest record) :checkpoints))
            (subgoal-progress-checkpoint-action-count record)
            target)
    target))


(defun subgoal-progress-readable-data-p (object)
  "Whether OBJECT is plain, non-executable archive data."
  (cond
    ((consp object)
     (and (subgoal-progress-readable-data-p (car object))
          (subgoal-progress-readable-data-p (cdr object))))
    ((or (null object) (symbolp object) (stringp object)
         (numberp object) (characterp object))
     t)
    (t nil)))


(defun read-subgoal-progress-file (path)
  "Read exactly one plain data record from PATH with reader evaluation disabled."
  (let ((source (subgoal-progress-pathname path))
        (eof (gensym "EOF")))
    (with-open-file (stream source :direction :input)
      (with-standard-io-syntax
        (let ((*package* (find-package :ww))
              (*read-eval* nil))
          (let ((record (read stream nil eof)))
            (when (eq record eof)
              (error "Subgoal progress file is empty: ~A" source))
            (unless (eq (read stream nil eof) eof)
              (error "Subgoal progress file contains trailing data: ~A" source))
            (unless (subgoal-progress-readable-data-p record)
              (error "Subgoal progress file contains unsupported data: ~A" source))
            record))))))


(defun subgoal-progress-plist-keys (plist)
  "Return PLIST's keys after requiring a proper even-length property list."
  (unless (and (listp plist) (evenp (length plist)))
    (error "Malformed subgoal progress property list: ~S" plist))
  (loop for key in plist by #'cddr
        unless (keywordp key)
          do (error "Subgoal progress key is not a keyword: ~S" key)
        collect key))


(defun validate-subgoal-progress-plist (plist allowed required context)
  "Require PLIST to contain only ALLOWED keys and every REQUIRED key."
  (let ((keys (subgoal-progress-plist-keys plist)))
    (dolist (key keys)
      (unless (member key allowed)
        (error "Unknown ~A key: ~S" context key))
      (when (> (count key keys) 1)
        (error "Duplicate ~A key: ~S" context key)))
    (dolist (key required)
      (unless (member key keys)
        (error "Missing ~A key: ~S" context key))))
  plist)


(defun validate-subgoal-progress-state-signature (signature context)
  "Validate one persisted readable state SIGNATURE."
  (validate-subgoal-progress-plist
    signature
    '(:facts :happening-facts :happenings :time :value :policy-context)
    '(:facts :happening-facts :happenings :time :value :policy-context)
    context)
  (unless (and (listp (getf signature :facts))
               (listp (getf signature :happening-facts))
               (listp (getf signature :happenings))
               (realp (getf signature :time))
               (realp (getf signature :value)))
    (error "Malformed ~A state signature: ~S" context signature))
  signature)


(defun subgoal-progress-alist-keys (alist context)
  "Return ALIST's symbol keys after validating its shape."
  (unless (listp alist)
    (error "Malformed ~A settings: ~S" context alist))
  (loop for entry in alist
        unless (and (consp entry) (symbolp (car entry)))
          do (error "Malformed ~A setting: ~S" context entry)
        collect (car entry)))


(defun validate-subgoal-progress-settings (settings)
  "Validate one checkpoint's generic and policy setting records."
  (validate-subgoal-progress-plist
    settings '(:bindings :policy-settings) '(:bindings :policy-settings)
    "checkpoint settings")
  (let* ((bindings (getf settings :bindings))
         (binding-keys
           (subgoal-progress-alist-keys bindings "generic checkpoint"))
         (policy-settings (getf settings :policy-settings))
         (policy-keys
           (subgoal-progress-alist-keys policy-settings "policy checkpoint"))
         (current-policy-settings (active-goal-chain-policy-settings))
         (current-policy-keys
           (subgoal-progress-alist-keys
             current-policy-settings "active policy")))
    (unless (equal binding-keys *goal-chain-setting-symbols*)
      (error "Checkpoint setting names do not match this Wouldwork version: ~S"
             binding-keys))
    (unless (equal policy-keys current-policy-keys)
      (error "Checkpoint policy setting names do not match the staged problem: ~S"
             policy-keys)))
  settings)


(defun validate-subgoal-progress-checkpoint (checkpoint expected-number)
  "Validate one persisted CHECKPOINT's schema and basic invariants."
  (validate-subgoal-progress-plist
    checkpoint
    '(:number :goal :final-p :settings :depth :time :value :actions :endpoint)
    '(:number :goal :final-p :settings :depth :time :value :actions :endpoint)
    "checkpoint")
  (unless (eql (getf checkpoint :number) expected-number)
    (error "Checkpoint number ~S should be ~D."
           (getf checkpoint :number) expected-number))
  (when (getf checkpoint :final-p)
    (error "Completed final-goal phases cannot be imported as subgoal progress."))
  (unless (and (listp (getf checkpoint :actions))
               (integerp (getf checkpoint :depth))
               (not (minusp (getf checkpoint :depth)))
               (= (getf checkpoint :depth)
                  (length (getf checkpoint :actions)))
               (realp (getf checkpoint :time))
               (realp (getf checkpoint :value)))
    (error "Malformed checkpoint metrics or action path: ~S" checkpoint))
  (validate-subgoal-progress-settings (getf checkpoint :settings))
  (validate-subgoal-progress-state-signature
    (getf checkpoint :endpoint) "checkpoint endpoint")
  checkpoint)


(defun validate-subgoal-progress-record (record)
  "Validate RECORD's complete versioned archive schema."
  (unless (and (consp record)
               (eq (first record) :wouldwork-subgoal-progress))
    (error "Not a Wouldwork subgoal progress record."))
  (let ((body (rest record)))
    (validate-subgoal-progress-plist
      body
      '(:version :problem :policy :original-goal :origin :checkpoints)
      '(:version :problem :policy :original-goal :origin :checkpoints)
      "archive")
    (unless (eql (getf body :version) *subgoal-progress-format-version*)
      (error "Unsupported subgoal progress format version: ~S"
             (getf body :version)))
    (unless (symbolp (getf body :problem))
      (error "Malformed archived problem name: ~S" (getf body :problem)))
    (validate-subgoal-progress-state-signature
      (getf body :origin) "archive origin")
    (let ((checkpoints (getf body :checkpoints)))
      (unless (and (listp checkpoints) checkpoints)
        (error "A subgoal progress archive requires at least one checkpoint."))
      (loop for checkpoint in checkpoints
            for number from 1
            do (validate-subgoal-progress-checkpoint checkpoint number))))
  record)


(defun validate-clean-subgoal-progress-import-session ()
  "Require a freshly staged problem before importing checkpoint progress."
  (validate-continuation-preconditions)
  (when *goal-chain-session*
    (error "A goal chain is already active; stage the problem afresh before importing."))
  (when *final-goal*
    (error "A legacy continuation is active; stage the problem afresh before importing."))
  (when *undo-stack*
    (error "Undo history is active; stage the problem afresh before importing."))
  (when (or *solutions-valid* *solution-paths*)
    (error "Search results are active; stage the problem afresh before importing."))
  (dolist (extension (capture-goal-chaining-extension-states))
    (when (third extension)
      (error "Goal-chaining extension ~S is already active; stage the problem afresh."
             (first extension))))
  t)


(defun validate-subgoal-progress-staging (record)
  "Require RECORD to describe the freshly staged current problem exactly."
  (let ((body (rest record)))
    (unless (eql (getf body :problem) *problem-name*)
      (error "Progress belongs to ~A, but ~A is staged."
             (getf body :problem) *problem-name*))
    (unless (equalp (getf body :policy) (goal-chaining-policy-signature))
      (error "Progress uses a different goal-chaining policy than the staged problem."))
    (unless (equalp (getf body :original-goal) *goal*)
      (error "The staged original goal differs from the exported original goal."))
    (unless
      (equalp
        (getf body :origin)
        (make-subgoal-progress-state-signature *start-state* nil))
      (error "The freshly staged initial state differs from the exported origin.")))
  t)


(defun imported-goal-chain-search-settings (record)
  "Construct fresh runtime search settings from persisted RECORD data."
  (make-goal-chain-search-settings
    :bindings (copy-tree (getf record :bindings))
    :random-state (make-random-state *random-state*)
    :policy-context (copy-tree (getf record :policy-settings))))


(defun imported-goal-chain-phase
    (checkpoint source-state preceding-phases)
  "Replay CHECKPOINT from SOURCE-STATE and return its fresh runtime phase."
  (let* ((goal (copy-tree (getf checkpoint :goal)))
         (path (copy-tree (getf checkpoint :actions)))
         (request
           (make-goal-chain-request
             :goal goal
             :final-p nil
             :settings
               (imported-goal-chain-search-settings
                 (getf checkpoint :settings)))))
    ;; The replay validator uses the installed goal to disambiguate a final
    ;; action with multiple matching effects, just as the original search did.
    (install-compiled-goal goal)
    (let ((validation (validate-action-sequence source-state path)))
      (unless (action-sequence-validation-success-p validation)
        (error "Checkpoint ~D replay failed at action ~D, ~S: ~S"
               (getf checkpoint :number)
               (action-sequence-validation-failure-index validation)
               (action-sequence-validation-failure-action validation)
               (action-sequence-validation-failure-reason validation)))
      (let* ((endpoint (action-sequence-validation-final-state validation))
             (solution
               (make-solution
                 :depth (length path)
                 :time (problem-state.time endpoint)
                 :value (problem-state.value endpoint)
                 :path path
                 :goal (copy-problem-state endpoint))))
        (unless (funcall (symbol-function 'goal-fn) endpoint)
          (error "Checkpoint ~D replay does not satisfy its exported goal: ~S"
                 (getf checkpoint :number) goal))
        (unless (and (= (getf checkpoint :depth) (solution.depth solution))
                     (= (getf checkpoint :time) (solution.time solution))
                     (= (getf checkpoint :value) (solution.value solution)))
          (error "Checkpoint ~D replay metrics differ from the export."
                 (getf checkpoint :number)))
        (let* ((phase
                 (make-goal-chain-phase-from-solution
                   request source-state preceding-phases solution))
               (phases (append preceding-phases (list phase))))
          (unless
            (equalp
              (getf checkpoint :endpoint)
              (make-subgoal-progress-state-signature endpoint phases))
            (error "Checkpoint ~D replay state differs from the export."
                   (getf checkpoint :number)))
          phase)))))


(defun build-imported-goal-chain-phases (record session)
  "Replay RECORD and return fresh chronological goal-chain phases."
  (let ((source-state (copy-problem-state (goal-chain-session-origin-state session)))
        (phases nil))
    (dolist (checkpoint (getf (rest record) :checkpoints) phases)
      (let ((phase
              (imported-goal-chain-phase
                checkpoint source-state phases)))
        (setf phases (append phases (list phase))
              source-state
                (copy-problem-state
                  (solution.goal (goal-chain-phase-solution phase))))))))


(defun install-imported-subgoal-progress (record source)
  "Transactionally replay and install validated RECORD read from SOURCE."
  (let* ((body (rest record))
         (original-goal (copy-tree (getf body :original-goal)))
         (original-goal-function (symbol-function 'goal-fn))
         (session
           (make-goal-chain-session
             :origin-state (copy-problem-state *start-state*)
             :original-goal original-goal
             :original-goal-function original-goal-function
             :phases nil :nogoods nil :screening-rejections nil))
         (checkpoint nil)
         (completed nil))
    (save-undo-checkpoint)
    (setf checkpoint (pop *undo-stack*))
    (unwind-protect
        (let ((phases (build-imported-goal-chain-phases record session)))
          (validate-goal-chain-prefix session phases)
          (setf *goal-chain-session* session
                *final-goal* original-goal)
          (commit-goal-chain session phases nil)
          (push checkpoint *undo-stack*)
          (setf completed t)
          (format t "~&Imported ~D checkpoint~:P and ~D action~:P from ~A.~%"
                  (length phases)
                  (subgoal-progress-checkpoint-action-count record)
                  source)
          *start-state*)
      (unless completed
        (restore-undo-checkpoint checkpoint)))))


(defun import-subgoal-progress (path)
  "Import a human-readable .txt archive into a freshly staged problem."
  (let* ((source (subgoal-progress-pathname path))
         (record (validate-subgoal-progress-record
                   (read-subgoal-progress-file source))))
    (validate-clean-subgoal-progress-import-session)
    (validate-subgoal-progress-staging record)
    (install-imported-subgoal-progress record source)))

;;; Filename: ww-backtracker.lisp

;;; Backtracking search infrastructure for wouldwork


(in-package :ww)


;; Basic data structures for backtracking


(defstruct (choice (:conc-name choice.))
  "Represents a choice point in backtracking search"
  act             ; (action-name arg1 arg2 ...)
  forward-update  ; Forward literals or a successor IDB snapshot
  inverse-update  ; IMMEDIATE planning cycle literals or a parent IDB snapshot
  undo-frame      ; Actual writes for this application only; NIL after rollback
  forward-sig     ; Order-insensitive signature of forward-update
  inverse-sig     ; Order-insensitive signature of inverse-update
  level           ; Depth in the search tree
  (value 0)       ; Objective value returned by this effect
  (heuristic 0)   ; Cached score when choices are ordered
  parent-metadata ; Name, time, value, heuristic and arguments restored by undo
  pre-applied-p)  ; T when effect already applied to *backtrack-state*


(defparameter *backtrack-state* nil
  "The single working state for backtracking search")


(defparameter *choice-stack* nil
  "Stack of choices made during backtracking search; path back to start state.")

(defvar *bt-path-fingerprints* nil
  "Nearest-first ancestor (hash . entry-count) pairs; never includes the candidate.")


(defvar *bt-worker* nil
  "The running parallel backtracking worker (a BT-WORKER), or NIL in serial search
   and in task generation.")


(defvar *bt-forced-prefix* nil
  "Simple vector of choice ordinals a parallel worker replays from the root, or NIL.
   At level L below its length, only the choice at ordinal (svref prefix L) is explored.")


(defvar *bt-split-depth* nil
  "Level at which parallel task generation records a task instead of descending, or NIL.")


(defvar *bt-ordinal-path* nil
  "Nearest-first ordinals of the choices on the current path; maintained only during
   parallel task generation.")


(defvar *bt-collected-tasks* nil
  "Ordinal-prefix tasks recorded by the current task-generation pass, newest first.")

(defun validate-bt-path-mode ()
  "Reject known configurations whose future is not represented by the IDB alone."
  (check-problem-parameter '*bt-cycle-check* *bt-cycle-check*)
  (when *bt-cycle-check*
    (dolist (requirement '((*algorithm* . backtracking) (*problem-type* . planning)
                           (*tree-or-graph* . tree)))
      (unless (eql (symbol-value (car requirement)) (cdr requirement))
        (error "BT PATH cycle checking requires ~S = ~S, got ~S."
               (car requirement) (cdr requirement) (symbol-value (car requirement)))))
    (dolist (parameter '(*solution-validators* *search-prefix-validators*
                         *goal-chain-candidate-rejector* *goal-chain-session*
                         *goal-chaining-policy* *final-goal* *happening-names*
                         *auto-wait* *recorder-prefix-pruning*))
      (when (symbol-value parameter)
        (error "BT PATH cycle checking does not support active ~S." parameter)))
    (when (or (gethash 'recorder *types*)
              (member "recorder" *spliced-tech-names* :test #'string=))
      (error "BT PATH cycle checking does not support recorder history."))))

(defun bt-path-fingerprint (db)
  (cons (compute-idb-hash db) (hash-table-count db)))

(defun bt-scratch-parent (scratch choice)
  "Restore one transition into SCRATCH without consuming or mutating live undo data."
  (let ((frame (choice.undo-frame choice)))
    (cond
      (frame
       (loop for entry = (bt-undo-frame.head frame) then (bt-undo-entry.previous entry)
             while entry do
         (when (bt-undo-entry.secondary-db entry)
           (error "BT PATH ancestor has static undo records for ~S." (choice.act choice)))
         (if (bt-undo-entry.present-p entry)
             (setf (gethash (bt-undo-entry.key entry) scratch) (bt-undo-entry.old-value entry))
             (remhash (bt-undo-entry.key entry) scratch)))
       scratch)
      ((hash-table-p (choice.forward-update choice))
       (copy-idb (choice.inverse-update choice)))
      (t (error "Missing BT PATH ancestor undo records for ~S." (choice.act choice))))))

(defun bt-on-current-path-p (fingerprint)
  "Use fingerprints only to select exact, scratch-only ancestor comparisons."
  (when (member fingerprint *bt-path-fingerprints* :test #'equal)
    (unless (= (length *choice-stack*) (length *bt-path-fingerprints*))
      (error "BT PATH ancestor fingerprints and choice stack are misaligned."))
    (let* ((candidate (problem-state.idb *backtrack-state*))
           (scratch (copy-idb candidate)))
      (loop for ancestor in *bt-path-fingerprints*
            for choice in *choice-stack* do
        (setf scratch (bt-scratch-parent scratch choice))
        (when (and (equal fingerprint ancestor) (equalp candidate scratch))
          (return t))))))

(defun descend-choice-bt (level)
  "Check a registered, unaccepted candidate before descending, after goal handling."
  (if (null *bt-cycle-check*)
      (backtrack (1+ level))
      (let ((fingerprint (bt-path-fingerprint (problem-state.idb *backtrack-state*))))
        (if (bt-on-current-path-p fingerprint)
            (progn (increment-global *repeated-states*)
                   (finalize-duplicate-depth (1+ level))
                   nil)
            (backtrack (1+ level) fingerprint)))))


(defun cycle-check-enabled-bt ()
  "Enable immediate inverse-cycle detection only for planning problems."
  (and (null *bt-cycle-check*) (not (eql *problem-type* 'csp))))


(defun update-set-signature (ops)
  "Build an order-insensitive signature for OPS, ignoring duplicates.
   Used as a cheap prefilter before exact set-equality checks."
  ;; Eight is an initial small-list cutoff, not a measured optimum.
  ;; Longer lists retain hash-based duplicate detection rather than quadratic scans.
  (let ((seen (when (nthcdr 8 ops) (make-hash-table :test #'equal)))
        (unique-count 0)
        (xor-hash 0)
        (sum-hash 0))
    (loop for tail on ops
          for op = (car tail) do
      (unless (if seen (gethash op seen) (member op (cdr tail) :test #'equal))
        (when seen (setf (gethash op seen) t))
        (incf unique-count)
        (let ((h (sxhash op)))
          (setf xor-hash (logxor xor-hash h))
          (incf sum-hash h))))
    (list unique-count xor-hash sum-hash)))


(defun bt-choice-inconsistent-p (choice)
  "Return T when CHOICE encodes the inconsistent-state marker.
   Supports both incremental list updates and snapshot hash-table updates."
  (let ((update (choice.forward-update choice)))
    (etypecase update
      (list
       (member '(inconsistent-state) update :test #'equal))
      (hash-table
       (gethash (convert-to-integer-memoized '(inconsistent-state)) update)))))


(defun apply-update-forward-bt (update)
  "Apply UPDATE to *backtrack-state*.
   UPDATE may be a forward-op list or a hash-table idb snapshot."
  (etypecase update
    (list
     (dolist (literal update)
       (let* ((proposition (if (eq (car literal) 'not) (second literal) literal))
              (db (if (gethash (car proposition) *relations*)
                      (problem-state.idb *backtrack-state*) *static-db*)))
         (update db literal))))
    (hash-table
     (setf (problem-state.idb *backtrack-state*) (copy-idb update))))
  (invalidate-problem-state-hash *backtrack-state*))


(defun restore-choice-database-bt (choice)
  "Restore actual writes, or the parent snapshot for a snapshot choice."
  (cond ((choice.undo-frame choice)
         (restore-bt-undo (choice.undo-frame choice))
         (setf (choice.undo-frame choice) nil))
        ((hash-table-p (choice.forward-update choice))
         (setf (problem-state.idb *backtrack-state*)
               (copy-idb (choice.inverse-update choice))))
        (t (error "Missing backtracking undo records for ~S" (choice.act choice))))
  (invalidate-problem-state-hash *backtrack-state*)
  (setf (choice.pre-applied-p choice) nil))

(defun apply-choice-database-bt (choice)
  "Capture fresh undo records whenever a literal choice is reapplied."
  (if (hash-table-p (choice.forward-update choice))
      (apply-update-forward-bt (choice.forward-update choice))
      (let* ((frame (begin-bt-undo (problem-state.idb *backtrack-state*)))
             (*bt-undo-frame* frame)
             (complete nil))
        (unwind-protect
            (progn (apply-update-forward-bt (choice.forward-update choice))
                   (setf (choice.undo-frame choice) frame complete t))
          (unless complete
            (restore-bt-undo frame)
            (invalidate-problem-state-hash *backtrack-state*))))))


(defun search-backtracking ()
  "Runs backtracking after DFS has initialized shared search state."
  (validate-bt-path-mode)
  ;; Initialize backtracking-specific state infrastructure
  (setf *backtrack-state* (copy-problem-state *start-state*))
  (setf *choice-stack* nil)
  ;(clrhash *proposition-cache*)
  ;(setf *last-object-index* 0)
  #+:ww-debug (when (and (<= *debug* 2) (>= *debug* 1))
                (setf *search-tree* nil)
                ;; Add initial state at depth 0
                (push `(start-state                     ; no action for initial state
                       0                        ; depth 0
                       ""                       ; no message
                       ,@(case *debug*
                           (1 nil)
                           (2 (list (list-database (problem-state.idb *backtrack-state*))))))
                      *search-tree*))
  ;; Initiate recursive backtracking search
  (let ((*bt-path-fingerprints* nil)
        (*bt-path-search-active* *bt-cycle-check*))
    (backtrack 0))
  (set-bt-average-branching-factor)
  t)


(defun set-bt-average-branching-factor ()
  "Compute the final average branching factor of a serial or parallel backtracking search."
  (setf *average-branching-factor* (if (> *program-cycles* 0)
                                     (coerce (/ (1- *total-states-processed*) *program-cycles*)
                                             'single-float)
                                     0.0)))


(defun update-statistics (level)
  "Update search statistics; a parallel worker counts into its own worker stats."
  (when *bt-worker*
    (return-from update-statistics (record-bt-worker-node level)))
  (increment-global *program-cycles* 1)
  (increment-global *total-states-processed* 1)
  (when (> (1+ level) *max-depth-explored*)
    (setf *max-depth-explored* (1+ level)))
  (print-search-progress))


(defun backtrack (level &optional fingerprint)
  "Recursive backtracking search over new states from assert clauses."
  
  ;; Step 1: Enforce depth cutoff
  (when (and (> *depth-cutoff* 0) (>= level *depth-cutoff*))
    (return-from backtrack nil))

  ;; A parallel worker stops when another worker's result or a failure ends the search.
  (when (bt-worker-stop-p)
    (return-from backtrack nil))

  ;; Match EXPAND: prune descendants, not a goal already accepted by the caller.
  ;; This also checks the initial state before generating any choices.
  (when (and (fboundp 'prune-state?)
             (funcall (symbol-function 'prune-state?) *backtrack-state*))
    (return-from backtrack nil))

  (when (and (min-steps-remaining-available-p)
             (min-steps-remaining-prunes-node-p *backtrack-state* level))
    (increment-global *lower-bound-pruned* 1)
    (return-from backtrack nil))

  (when (eql (bound-search-state *backtrack-state* level) 'kill-node)
    (return-from backtrack nil))

  ;; Parallel task generation records the path here instead of descending.
  (when (eql level *bt-split-depth*)
    (push (coerce (reverse *bt-ordinal-path*) 'simple-vector) *bt-collected-tasks*)
    (return-from backtrack nil))

  ;; Step 2: Update search statistics
  (update-statistics level)

  (let ((found-a-solution nil)
        (*bt-path-fingerprints*
          (if *bt-cycle-check*
              (cons (or fingerprint (bt-path-fingerprint (problem-state.idb *backtrack-state*)))
                    *bt-path-fingerprints*)
              *bt-path-fingerprints*)))
    (visit-backtracking-choices
      level
      (lambda (choice action)
        (when (explore-choice-bt choice action level)
          (setf found-a-solution t))
        (or (and found-a-solution (solution-count-reached-p))
            (bt-worker-stop-p))))
    found-a-solution))


(defun backtracking-actions (level)
  "Preserve fixed action order for CSP levels."
  (if (and (eql *problem-type* 'csp) (< level (length *actions*)))
      (list (nth level *actions*))
      *actions*))


(defun visit-generated-choices-bt (level visitor)
  "Visit choices in generation order; stop when VISITOR returns true."
  (dolist (action (backtracking-actions level))
    (let ((combinations (if (action.dynamic action)
                            (eval-instantiated-spec
                              (action.precondition-type-inst action) *backtrack-state*)
                            (action.precondition-args action))))
      (when *symmetry-pruning*
        (setf combinations
              (filter-symmetric-instantiations action combinations *backtrack-state*)))
      (dolist (combination combinations)
        (let ((precondition-result
                (apply (action.pre-defun-name action) *backtrack-state* combination)))
          (when precondition-result
            (dolist (choice (generate-choices-for-single-combination-bt
                              action combination precondition-result level))
              (when (funcall visitor choice action)
                (return-from visit-generated-choices-bt t)))))))))


(defun visit-backtracking-choices (level visitor)
  "Visit the choices at LEVEL in search order; stop when VISITOR returns true.
   A parallel worker replaying its task prefix explores only the forced choice there;
   task generation tracks each choice's ordinal. Both count the same visitor calls."
  (let ((forced (when (and *bt-forced-prefix* (< level (length *bt-forced-prefix*)))
                  (svref *bt-forced-prefix* level))))
    (cond (forced
           (unless (visit-choices-in-order-bt level (forced-choice-visitor-bt forced visitor))
             (error "Parallel backtracking replay found no choice ~D at level ~D." forced level)))
          (*bt-split-depth*
           (visit-choices-in-order-bt level (ordinal-recording-visitor-bt visitor)))
          (t (visit-choices-in-order-bt level visitor)))))


(defun visit-choices-in-order-bt (level visitor)
  "Use heuristic order only when a heuristic is defined; otherwise stream choices.
   Returns T when VISITOR stopped the visit."
  (if (fboundp 'heuristic?)
      (dolist (entry (ordered-choices-bt level))
        (when (funcall visitor (second entry) (third entry))
          (return t)))
      (visit-generated-choices-bt level visitor)))


(defun forced-choice-visitor-bt (forced visitor)
  "Pass only the choice at ordinal FORCED to VISITOR, then stop the visit.
   A skipped choice applied during generation is restored, as EXPLORE-CHOICE-BT does."
  (let ((ordinal -1))
    (lambda (choice action)
      (incf ordinal)
      (cond ((= ordinal forced)
             (funcall visitor choice action)
             t)
            (t (when (choice.pre-applied-p choice)
                 (restore-choice-database-bt choice))
               nil)))))


(defun ordinal-recording-visitor-bt (visitor)
  "Extend *BT-ORDINAL-PATH* with each choice's ordinal while VISITOR explores it."
  (let ((ordinal -1))
    (lambda (choice action)
      (incf ordinal)
      (let ((*bt-ordinal-path* (cons ordinal *bt-ordinal-path*)))
        (funcall visitor choice action)))))


(defun ordered-choices-bt (level)
  "Score all choices on a disposable working copy; retain only updates and scores."
  (let ((*backtrack-state* (copy-problem-state *backtrack-state*))
        (scored nil))
    (visit-generated-choices-bt
      level
      (lambda (choice action)
        (let ((score (score-choice-bt choice action level)))
          (when score
            (push (list score choice action) scored)))
        nil))
    (stable-sort (nreverse scored) #'< :key #'first)))


(defun score-choice-bt (choice action level)
  "Evaluate a valid successor and undo it, including when the heuristic signals."
  (when (detect-path-cycle choice)
    (when (choice.pre-applied-p choice)
      (restore-choice-database-bt choice))
    (return-from score-choice-bt nil))
  (when (register-choice-bt choice action level nil)
    (unwind-protect
        (if (bt-choice-inconsistent-p choice)
            (progn (increment-global *inconsistent-states-dropped* 1) nil)
            (let ((score (funcall (symbol-function 'heuristic?) *backtrack-state*)))
              (check-type score real)
              (setf (choice.heuristic choice) score)
              score))
      (undo-choice-bt choice action level nil)
      (setf (choice.pre-applied-p choice) nil))))


(defun explore-choice-bt (choice action level)
  "Explore one successor, restoring the working state on every exit."
  (when (detect-path-cycle choice)
    (when (choice.pre-applied-p choice)
      (restore-choice-database-bt choice))
    (return-from explore-choice-bt nil))
  (when (register-choice-bt choice action level)
    (unwind-protect
        (cond
          ((bt-choice-inconsistent-p choice)
           (increment-global *inconsistent-states-dropped* 1)
           nil)
          ((search-prefix-pruned-p
             *backtrack-state*
             (lambda (move)
               (declare (ignore move))
               (list (reconstruct-solution-path))))
           nil)
          ((accept-goal-bt level) t)
          (t (descend-choice-bt level)))
      (undo-choice-bt choice action level))))


(defun accept-goal-bt (level)
  "Register an acceptable goal; rejected goals remain eligible for expansion."
  (when (is-complete-solution)
    (let ((path (reconstruct-solution-path)))
      (when (and (candidate-solution-valid-p path *backtrack-state*)
                 (not (goal-chain-candidate-rejected-p path *backtrack-state*)))
        (register-solution-bt (1+ level) path)
        (narrate-bt "Solution found ***" (first *choice-stack*) (1+ level))
        (when (> *debug* 0) (finish-output))
        t))))

(defun generate-choices-for-single-combination-bt (action param-combo precondition-result level)
  "Capture all effect writes; retain the trail only for a pre-applied choice."
  (declare (ignore param-combo))
  (let* ((pre-idb (problem-state.idb *backtrack-state*))
         (frame (begin-bt-undo pre-idb))
         (*bt-undo-frame* frame)
         (retained nil))
    (unwind-protect
        (let* ((effect-fn (action.eff-defun-name action))
               (updates (if (eql precondition-result t)
                            (funcall effect-fn *backtrack-state*)
                            (apply effect-fn *backtrack-state* precondition-result)))
               (choices (build-choices-bt action updates pre-idb level)))
          (when (and (= (length choices) 1) (choice.pre-applied-p (first choices)))
            (setf (choice.undo-frame (first choices)) frame retained t))
          choices)
      (unless retained
        (restore-bt-undo frame)
        (invalidate-problem-state-hash *backtrack-state*)))))

(defun choice-from-update-bt (action update pre-idb level single-p)
  "Keep literal cycle data separate from physical restoration data."
  (let* ((changes (update.changes update))
         (incremental-p (listp changes))
         (forward (if incremental-p (first changes) changes))
         (cycle-p (and incremental-p (cycle-check-enabled-bt)))
         (inverse (if incremental-p
                      (unless *bt-cycle-check* (second changes))
                      pre-idb)))
    (when changes
      (make-choice :act (cons (action.name action) (copy-tree (update.instantiations update)))
                   :forward-update forward :inverse-update inverse
                   :forward-sig (and cycle-p (update-set-signature forward))
                   :inverse-sig (and cycle-p (update-set-signature inverse))
                   :level level :value (update.value update)
                   :pre-applied-p (and incremental-p single-p)))))

(defun build-choices-bt (action updates pre-idb level)
  "Preserve effect-result order; the caller restores the complete generation trail."
  (let ((single-p (and (consp updates) (null (cdr updates)))))
    (loop for update in updates
          for choice = (choice-from-update-bt action update pre-idb level single-p)
          when choice collect choice)))

(defun register-choice-bt (choice action level &optional (report-p t))
  "Restore both partial application and metadata if registration rejects or signals."
  (let ((registered nil)
        (old-stack *choice-stack*))
    (setf (choice.parent-metadata choice)
          (list (problem-state.name *backtrack-state*) (problem-state.time *backtrack-state*)
                (problem-state.value *backtrack-state*) (problem-state.heuristic *backtrack-state*)
                (problem-state.instantiations *backtrack-state*)))
    (unwind-protect
        (setf registered (register-applied-choice-bt choice action level report-p))
      (unless registered
        (when (or (choice.undo-frame choice) (choice.pre-applied-p choice)
                  (hash-table-p (choice.forward-update choice)))
          (restore-choice-database-bt choice))
        (restore-choice-metadata-bt choice)
        (setf *choice-stack* old-stack)))))

(defun register-applied-choice-bt (choice action level report-p)
  "Register a choice by applying its forward operations to *backtrack-state*."
    
  #+:ww-debug
  (when (and report-p (>= *debug* 3))
    (format t "~%Current state: ~A~%" (list-database (problem-state.idb *backtrack-state*))))

  ;; Step 1: Apply forward operations unless already applied during generation
  (unless (choice.pre-applied-p choice)
    (apply-choice-database-bt choice))

  ;; Step 2: The registration wrapper has retained the parent metadata.
  (setf (problem-state.name *backtrack-state*) (action.name action))
  (setf (problem-state.value *backtrack-state*) (choice.value choice)
        (problem-state.heuristic *backtrack-state*) (choice.heuristic choice)
        (problem-state.instantiations *backtrack-state*) (rest (choice.act choice)))
  (incf (problem-state.time *backtrack-state*) (action.duration action))

  ;; Step 3: Global invariants
  (when *global-invariants*
    (unless (validate-global-invariants nil *backtrack-state*)
      (error "Global invariant violation in successor state from action ~A"
             (format-action-for-display (choice.act choice)))))

  ;; Step 4: Constraint
  (when (and (fboundp 'constraint-fn)
             (not (funcall (symbol-function 'constraint-fn) *backtrack-state*)))
    (return-from register-applied-choice-bt nil))

  ;; Step 5: Choice stack
  (push choice *choice-stack*)

  ;; Step 6: Debug output
  (when report-p (narrate-bt "" choice (1+ level)))

  t)


(defun restore-choice-metadata-bt (choice)
  "Restore the exact metadata saved before registering CHOICE."
  (destructuring-bind (name time value heuristic instantiations) (choice.parent-metadata choice)
    (setf (problem-state.name *backtrack-state*) name
          (problem-state.time *backtrack-state*) time
          (problem-state.value *backtrack-state*) value
          (problem-state.heuristic *backtrack-state*) heuristic
          (problem-state.instantiations *backtrack-state*) instantiations)))


(defun undo-choice-bt (choice action level &optional (report-p t))
  "Undo a choice from the current state with time reversal and stack management"
  (declare (ignore action))

  ;; Inverse state update
  (restore-choice-database-bt choice)

  ;; Reverse name & time
  (restore-choice-metadata-bt choice)

  ;; Debug
  (when report-p (narrate-bt "Backtracking to" choice level))

  ;; Stack handling
  (pop *choice-stack*)

  t)


(defun is-complete-solution ()
  "Hook: Check if we have reached a complete solution"
  ;; Use wouldwork's existing goal checking mechanism
  (when (fboundp 'goal-fn)
    (funcall (symbol-function 'goal-fn) *backtrack-state*)))


(defun reconstruct-solution-path ()
  "Reconstruct the solution path from the choice stack with correct cumulative time"
  (let ((cumulative-time 0.0)
        (path '()))
    ;; Process choices in reverse order (oldest first) to build cumulative time
    (dolist (choice (reverse *choice-stack*))
      (let* ((action-name (first (choice.act choice)))
             (action (find action-name *actions* :key #'action.name))
             (action-duration (action.duration action)))
        ;; Update cumulative time
        (setf cumulative-time (+ cumulative-time action-duration))
        ;; Build move record with correct time
        (push (list cumulative-time (choice.act choice)) path)))
    (nreverse path)))


(defun update-search-tree-bt (choice depth message)
  (declare (type choice choice) (type fixnum depth) (type string message))
  (when (and (not (> *threads* 0)) (<= *debug* 2) (>= *debug* 1))
    (push `(,(choice.act choice)    ; Already formatted as (action-name arg1 arg2 ...)
           ,depth
           ,message
           ,@(case *debug*
               (1 nil)
               (2 (list (list-database (problem-state.idb *backtrack-state*))))))
          *search-tree*)))


(defun narrate-bt (string choice depth)
  "Enhanced narration function with refined progressive debug level disclosure"
  ;(declare (ignorable string choice depth))
  
  ;; Debug levels 1 & 2: Build search tree (but not for backtracking messages)
  #+:ww-debug (when (and (<= *debug* 2) (>= *debug* 1))
                (unless (and string (string= string "Backtracking to"))
                  (update-search-tree-bt choice depth string)))
  
  ;; Debug levels 3+: Immediate console output (no search tree)
  #+:ww-debug (when (>= *debug* 3)
                ;; Special handling for backtrack operations - simplified output
                (if (and string (string= string "Backtracking to"))
                    (when choice
                      (format t "~%Backtracking to: ~A~%"
                              (format-action-for-display (choice.act choice))))
                    ;; Normal detailed output for non-backtrack operations
                    (progn
                      (when (and string (not (string= string "")))
                        (format t "~%~A:~%" string))
                      (when choice
                        (format t "~%Action: ~A~%"
                                (format-action-for-display (choice.act choice)))
                        (format t "Depth: ~A~%" depth)
                        (format t "Forward Update: ~A~%" (choice.forward-update choice))
                        (if (and *bt-cycle-check*
                                 (listp (choice.forward-update choice)))
                            (format t "Inverse cycle literals: omitted in PATH mode~%")
                            (format t "Inverse Update: ~A~%" (choice.inverse-update choice))))
                      (unless choice
                        (format t "Choice: <nil>~%"))
                      (format t "Successor State IDB: ~A~%" (list-database (problem-state.idb *backtrack-state*)))
                      (when (problem-state.hidb *backtrack-state*)
                        (format t "Successor State HIDB: ~A~%" (list-database (problem-state.hidb *backtrack-state*))))
                        (format t "Successor Time: ~A~%" (problem-state.time *backtrack-state*))
                        (format t "Successor Value: ~A~%" (problem-state.value *backtrack-state*)))))
  
  ;; Debug level 4+: Add choice stack visualization  
  #+:ww-debug (when (>= *debug* 4)
                (format t "--- Choice Stack (Length ~A) ---~%" (length *choice-stack*))
                (if *choice-stack*
                    (loop for i from 0
                          for stack-choice in *choice-stack*
                          do (format t "  [~A] ~A~A~%" 
                                     i 
                                     (format-action-for-display (choice.act stack-choice))
                                     ;; Highlight current choice being processed
                                     (if (and choice (eq stack-choice choice))
                                         " ← current"
                                         "")))
                    (format t "  <empty>~%")))
  
  ;; Debug level 5: Interactive breakpoint
  #+:ww-debug (when (>= *debug* 5)
                (simple-break))
  
  nil)


(defun register-solution-bt (level &optional (solution-path (reconstruct-solution-path)))
  "Register a solution found via backtracking using the choice stack"
  (when (eql *solution-type* 'count)
    (when (count-accepted-goal)
      (setf *count-example*
            (make-search-solution solution-path (copy-problem-state *backtrack-state*))))
    (return-from register-solution-bt nil))
  (let ((solution (make-solution
                    :depth (length *choice-stack*)
                    :time (problem-state.time *backtrack-state*)
                    :value (problem-state.value *backtrack-state*)
                    :path solution-path
                    :goal (copy-problem-state *backtrack-state*))))
    (if *bt-worker*
        (register-parallel-solution-bt solution)
        (progn (when (report-solution-found-p)
                 (report-solution-bt solution))
               (record-solution-bt solution)))))


(defun report-solution-bt (solution)
  "Announce a newly registered backtracking solution."
  (format t "~%New path to goal found at depth = ~:D" (solution.depth solution))
  (when (eql *solution-type* 'min-time)
    (format t "Time = ~:A~%" (solution.time solution)))
  (finish-output))


(defun record-solution-bt (solution)
  "Add SOLUTION to the solution lists, keeping one entry per goal database."
  (push solution *solution-paths*)
  (when (not (member (problem-state.idb (solution.goal solution)) *unique-solution-states*
                     :key (lambda (soln) (problem-state.idb (solution.goal soln)))
                     :test #'equalp))
    (push solution *unique-solution-states*)))


(defun detect-path-cycle (new-choice)
  "Check if new-choice immediately undoes the last choice using set-based comparison"
  (when (cycle-check-enabled-bt)
    (let ((last-choice (first *choice-stack*)))
    ;; Check if new forward-update matches last inverse-update (set comparison)
      (when last-choice
        (let ((new-forward (choice.forward-update new-choice))
              (last-inverse (choice.inverse-update last-choice)))
          (and (listp new-forward)
               (listp last-inverse)
               (equal (choice.forward-sig new-choice)
                      (choice.inverse-sig last-choice))
               (alexandria:set-equal new-forward
                                     last-inverse
                                     :test #'equal)))))))

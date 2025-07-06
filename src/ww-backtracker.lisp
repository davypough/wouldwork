;;; Filename: ww-backtracker.lisp

;;; Backtracking search infrastructure for wouldwork


(in-package :ww)


;; Basic data structures for backtracking


(defstruct choice
  "Represents a choice point in backtracking search"
  act             ; (action-name arg1 arg2 ...)
  forward-update  ; The update structure that applies this choice
  inverse-update  ; The update structure that undoes this choice
  level)          ; Depth in the search tree


(defparameter *backtrack-state* nil
  "The single working state for backtracking search")


(defparameter *choice-stack* nil
  "Stack of choices made during backtracking search")


(defun preprocess-state (state level)
  "Hook: Preprocessing with statistics tracking and debugging"
  (declare (ignore state))
  (increment-global *program-cycles* 1)
  (increment-global *total-states-processed* 1)
  (when (> level *max-depth-explored*)
    (setf *max-depth-explored* level))
  (print-search-progress)
  #+:ww-debug (when (>= *debug* 3)
                (format t "~&Exploring state at level ~A~%" level)))


(defun is-valid-partial-solution (state level)
  "Hook: Check if current partial solution is valid"
  (declare (ignore level))
  ;; Use wouldwork's existing constraint checking
  (and (or (not (fboundp 'constraint-fn))
           (not (symbol-value 'constraint-fn))
           (funcall (symbol-function 'constraint-fn) state))
       ;; Check global invariants if they exist
       (or (not *global-invariants*)
           (every (lambda (invariant-fn)
                    (funcall invariant-fn nil state))
                  *global-invariants*))))


(defun is-complete-solution (state level)
  "Hook: Check if we have reached a complete solution"
  (declare (ignore level))
  ;; Use wouldwork's existing goal checking mechanism
  (when (fboundp 'goal-fn)
    (funcall (symbol-function 'goal-fn) state)))


(defun generate-choices (state level)
  "Generate all valid choices from the current state using wouldwork's action system"
  (declare (ignore level))
  (let ((choices '()))
    ;; Iterate through all available actions
    (dolist (action *actions*)
      (let ((action-choices (generate-choices-for-action action state)))
        (setf choices (append choices action-choices))))
    choices))


(defun create-inverse-update (forward-literals)
  "Creates the inverse of a list of literals for undo operations"
  (mapcar (lambda (literal)
            (if (and (listp literal) (eq (car literal) 'not))
                (second literal)  ; Remove the 'not'
                `(not ,literal))) ; Add 'not'
          forward-literals))


(defun apply-choice (choice state level)
  "Apply a choice to the current state with strict validation and fail-fast error handling"
  
  ;; Strict validation - fail immediately if choice structure is invalid
  (unless (and choice (choice-forward-update choice))
    (error "Invalid choice structure: ~A. Choice must have valid forward-update." choice))
  
  ;; Debug: Beginning of choice application
  (narrate-bt "Beginning choice application" choice level)
  
  ;; Pre-application state validation
  (let ((pre-application-idb-size (hash-table-count (problem-state.idb state))))
    
    ;; Apply the forward update to modify current state
    (revise (problem-state.idb state) (choice-forward-update choice))
    
    ;; Post-application validation with immediate failure on problems
    (let ((post-application-idb-size (hash-table-count (problem-state.idb state))))
      
      ;; Debug: Successful choice application with new act format
      (narrate-bt (format nil "Applied choice: ~A -> ~A facts in database" 
                          (choice-act choice)        ; Now shows complete action
                          post-application-idb-size) 
                  choice level)
      
      ;; Global invariant validation - fail fast if violated
      (when *global-invariants*
        (unless (validate-global-invariants nil state)
          (error "Global invariant violation after applying choice ~A at state ~A" 
                 (choice-act choice) (list-database (problem-state.idb state)))))
      
      ;; Rest of function unchanged...
      (when *global-invariants*
        (narrate-bt "Global invariants validated after choice application" choice level))
      
      (push choice *choice-stack*)
      (narrate-bt "Choice successfully applied and added to stack" choice level)
      
      t)))


(defun undo-choice (choice state level)
  "Undo a choice from the current state with strict validation and fail-fast error handling"
  
  ;; Strict validation of choice structure - fail immediately if invalid
  (unless choice
    (error "Cannot undo null choice. This indicates a serious bug in choice stack management."))
  
  ;; Debug: Beginning of choice undo
  (narrate-bt "Beginning choice undo" choice level)
  
  ;; Verify that this choice is actually the most recent one on our stack
  (unless (and *choice-stack* (eq choice (first *choice-stack*)))
    (error "Choice stack corruption detected. Attempted to undo ~A but top of stack is ~A. Stack: ~A"
           choice (first *choice-stack*) *choice-stack*))
  
  ;; Validate that we have the inverse update needed for restoration
  (unless (choice-inverse-update choice)
    (error "Cannot undo choice ~A: no inverse update available. This choice cannot be reversed."
           (choice-act choice)))  ; CHANGED: Use choice-act instead of choice-action
  
  ;; Capture pre-undo state for validation purposes
  (let ((pre-undo-idb-size (hash-table-count (problem-state.idb state))))
    
    ;; Apply the inverse update to restore the previous state
    (revise (problem-state.idb state) (choice-inverse-update choice))
    
    ;; Verify that the undo operation produced a sensible result
    (let ((post-undo-idb-size (hash-table-count (problem-state.idb state))))
      
      ;; Debug: Successful choice undo with new act format
      (narrate-bt (format nil "Undid choice: ~A -> ~A facts in database" 
                       (choice-act choice)        ; CHANGED: Use choice-act
                       post-undo-idb-size) 
               choice level)
      
      ;; Critical validation: ensure global invariants still hold after undo
      (when *global-invariants*
        (unless (validate-global-invariants nil state)
          (error "Global invariant violation after undoing choice ~A. This indicates a bug in the inverse update logic for action ~A"
                 (choice-act choice) (first (choice-act choice)))))  ; CHANGED: Extract action name from act
      
      ;; Debug: Successful validation after undo
      (when *global-invariants*
        (narrate-bt "Global invariants validated after choice undo" choice level))
      
      ;; Remove the choice from our stack
      (pop *choice-stack*)
      
      ;; Debug: Choice successfully removed from stack
      (narrate-bt "Choice successfully undone and removed from stack" choice level)
      
      ;; Return success indicator
      t)))


(defun search-backtracking ()
  "Main entry point for backtracking search with full initialization"
  ;; Initialize search statistics (matching wouldwork's initialization)
  (setf *program-cycles* 0)
  (setf *total-states-processed* 0)
  (setf *max-depth-explored* 0)
  (setf *solutions* nil)
  (setf *unique-solutions* nil)
  (setf *solution-count* 0)
  (setf *start-time* (get-internal-real-time))
  
  ;; Initialize backtracking-specific state
  (setf *backtrack-state* (copy-problem-state *start-state*))
  (setf *choice-stack* nil)
  
  ;; Validate initial state against global invariants
  (when *global-invariants*
    (unless (validate-global-invariants nil *backtrack-state*)
      (format t "~%Invariant validation failed on initial state.~%")
      (return-from search-backtracking nil)))
  
  ;; Check if start state is already a goal
  (when (and (fboundp 'goal-fn)
             (funcall (symbol-function 'goal-fn) *backtrack-state*))
    (let ((dummy-choice (make-choice :act '(initial-state)
                                     :forward-update nil
                                     :inverse-update nil
                                     :level 0)))
      (narrate-bt "Solution found ***" dummy-choice 0)
      (register-solution-bt 0)
      (return-from search-backtracking *solutions*)))
  
  ;; Start the recursive backtracking search
  (let ((search-result (backtrack *backtrack-state* 0)))
    (setf *average-branching-factor* 
          (if (> *program-cycles* 0)
              (coerce (/ (1- *total-states-processed*) *program-cycles*) 'single-float)
              0.0))
    *solutions*))


(defun backtrack (state level)
  "Recursive backtracking search with proper choice-based debugging"
  
  ;; Prevent runaway recursion
  (when (> level 10)  ; Reasonable depth limit for blocks problems
    (return-from backtrack nil))
  
  (preprocess-state state level)
  
  (when (is-complete-solution state level)
    (register-solution-bt level)
    (when (and *choice-stack* (first *choice-stack*))
      (narrate-bt "Solution found ***" (first *choice-stack*) level))
    (finish-output)
    (unless (should-continue-search state level)
      (return-from backtrack t)))
  
  (unless (is-valid-partial-solution state level)
    (return-from backtrack nil))
  
  ;; FIXED: Separate solution found flag from early termination logic
  (let ((found-any-solution nil))
    (dolist (action *actions*)
      (let ((choices (generate-choices-for-action action state)))
        (dolist (choice choices)
          (when (apply-choice choice state level)
            (let ((deeper-result (backtrack state (1+ level))))
              (undo-choice choice state level)
              ;; CHANGED: Track if any solution was found but don't terminate early
              ;; unless specifically requested by solution type
              (when deeper-result
                (setf found-any-solution t)
                ;; Only terminate early for 'first solution type
                (when (eq *solution-type* 'first)
                  (return-from backtrack t))))))))
    found-any-solution))


(defun generate-choices-for-action (action state)
  "Generate all valid choices for a specific action in the given state"
  (let ((choices '()))
    ;; Use wouldwork's exact precondition checking pattern from generate-children
    (let ((precondition-fn (action.pre-defun-name action))
          (effect-fn (action.eff-defun-name action))
          (parameter-combinations (action.precondition-args action)))
      ;; Test each combination - mirrors generate-children logic exactly
      (let ((pre-results 
             (remove-if #'null (mapcar (lambda (pinsts)
                                         (apply precondition-fn state pinsts))
                                       parameter-combinations))))
        (when pre-results
          ;; Get the effect updates for valid precondition results
          (let ((updated-dbs 
                 (mapcan (lambda (pre-result)
                           (if (eql pre-result t)
                               (funcall effect-fn state)
                               (apply effect-fn state pre-result)))
                         pre-results)))
            ;; Create choices from the update structures
            (dolist (updated-db updated-dbs)
              (let* ((forward-literals (extract-literals-from-update updated-db))
                     (inverse-literals (create-inverse-update forward-literals))
                     ;; NEW: Create combined act slot instead of separate action/instantiations
                     (combined-act (cons (action.name action)
                                         (update.instantiations updated-db)))
                     (choice (make-choice 
                             :act combined-act              ; Combined action name + args
                             :forward-update forward-literals
                             :inverse-update inverse-literals
                             :level 0)))
                (push choice choices)))))))
    choices))


(defun get-action-parameter-combinations (action state)
  "Get all valid parameter combinations for an action in the given state"
  (declare (ignore state)) ; State not needed since combinations are precomputed
  ;; Use wouldwork's precomputed parameter combinations
  (action.precondition-args action))


(defun extract-literals-from-update (updated-db)
  "Convert wouldwork's update structure into a list of literals for backtracking.
   Wouldwork's update.changes contains the complete final state, so we must
   compute the difference against the current state to extract actual changes."
  (let ((literals '())
        (final-state-props (make-hash-table :test 'equal))
        (current-state-props (make-hash-table :test 'equal)))
    
    ;; Extract final state propositions from update.changes
    (maphash (lambda (int-proposition should-assert)
               (when should-assert  ; Only process assertions (true values)
                 (let ((proposition (convert-to-proposition int-proposition)))
                   (setf (gethash proposition final-state-props) t))))
             (update.changes updated-db))
    
    ;; Extract current state propositions
    (dolist (prop (list-database (problem-state.idb *backtrack-state*)))
      (setf (gethash prop current-state-props) t))
    
    ;; Compute additions: in final state but not in current state
    (maphash (lambda (prop value)
               (declare (ignore value))
               (unless (gethash prop current-state-props)
                 (push prop literals)))
             final-state-props)
    
    ;; Compute retractions: in current state but not in final state
    (maphash (lambda (prop value)
               (declare (ignore value))
               (unless (gethash prop final-state-props)
                 (push `(not ,prop) literals)))
             current-state-props)
    
    ;; Return changes in consistent order
    (reverse literals)))


(defun reconstruct-solution-path (choice-stack)
  "Reconstruct the solution path from the choice stack"
  (reverse 
    (mapcar (lambda (choice)
              (record-move-from-choice choice))
            choice-stack)))


(defun record-move-from-choice (choice)
  "Create a move record from a choice (matching wouldwork's record-move format)"
  (list 1.0  ; Time step (simplified for now)
        (choice-act choice)))  ; Already in proper format: (action-name arg1 arg2 ...)


(defun should-continue-search (state level)
  "Hook: Decide whether to continue searching based on *solution-type*"
  (declare (ignore state level))
  (case *solution-type*
    (first nil)     ; Stop after first solution
    (every t)       ; Continue for all solutions
    (min-length t)  ; Continue to find shortest (need to search all)
    (min-time t)    ; Continue to find fastest
    (min-value t)   ; Continue to find minimum value
    (max-value t)   ; Continue to find maximum value
    (otherwise nil)))


(defun solution-better-p (new-solution current-best)
  "Determine if new solution is better than current best"
  (case *solution-type*
    (min-length (< (solution.depth new-solution) (solution.depth current-best)))
    (min-time (< (solution.time new-solution) (solution.time current-best)))
    (min-value (< (solution.value new-solution) (solution.value current-best)))
    (max-value (> (solution.value new-solution) (solution.value current-best)))
    (otherwise nil)))


(defun format-backtrack-context (choice depth)
  "Format debugging output for backtracking context"
  (when choice
    (let ((act (choice-act choice)))
      (format t "Choice: ~A ~A at level ~A~%" 
              (first act)           ; Action name
              (rest act)            ; Arguments
              depth)))
  (when *backtrack-state*
    (format t "Current state: ~A~%" *backtrack-state*)))


(defun update-search-tree-bt (choice depth message)
  (declare (type choice choice) (type fixnum depth) (type string message))
  (when (and (not (> *threads* 0)) (>= *debug* 1))
    (push `(,(choice-act choice)    ; Already formatted as (action-name arg1 arg2 ...)
           ,depth
           ,message
           ,@(case *debug*
               (1 nil)
               (2 (list (list-database (problem-state.idb *backtrack-state*))
                        (list-database (problem-state.hidb *backtrack-state*))))))
          *search-tree*)))


(defun narrate-bt (string choice depth)
  (declare (ignorable string choice depth))
  #+:ww-debug (when (>= *debug* 3)
                (format t "~%~A:~%~A~%" string choice))
  #+:ww-debug (when (>= *debug* 1)
                (update-search-tree-bt choice depth string))
  nil)


(defun register-solution-bt (level)
  "Register a solution found via backtracking using the choice stack"
  (let* ((solution-depth level)  ; Use actual search level as solution depth
         (solution-path (reconstruct-solution-path *choice-stack*))
         (solution (make-solution
                     :depth solution-depth
                     :time (problem-state.time *backtrack-state*)
                     :value (problem-state.value *backtrack-state*)
                     :path solution-path
                     :goal (copy-problem-state *backtrack-state*))))
    (format t "~%New path to goal found at depth = ~:D~%" solution-depth)
    (when (eql *solution-type* 'min-time)
      (format t "Time = ~:A~%" (solution.time solution)))
    (push solution *solutions*)
    (when (not (member (problem-state.idb (solution.goal solution)) *unique-solutions* 
                       :key (lambda (soln) (problem-state.idb (solution.goal soln)))
                       :test #'equalp))
      (push solution *unique-solutions*))))

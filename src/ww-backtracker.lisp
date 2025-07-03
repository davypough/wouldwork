;;; Filename: ww-backtracker.lisp

;;; Backtracking search infrastructure for wouldwork


(in-package :ww)


;; Basic data structures for backtracking
(defstruct choice
  "Represents a choice point in backtracking search"
  action          ; The action being applied
  instantiations  ; Variable bindings for this action
  forward-update  ; The update structure that applies this choice
  inverse-update  ; The update structure that undoes this choice
  level)          ; Depth in the search tree


;; Global variables for backtracking state
(defparameter *backtrack-state* nil
  "The single working state for backtracking search")


(defparameter *choice-stack* nil
  "Stack of choices made during backtracking search")


(defun preprocess-state (state level)
  "Hook: Preprocessing with statistics tracking and debugging"
  (declare (ignore state))
  ;; Update search statistics (matching wouldwork's tracking)
  (increment-global *program-cycles* 1)
  (increment-global *total-states-processed* 1)
  ;; Update max depth explored
  (when (> level *max-depth-explored*)
    (setf *max-depth-explored* level))
  ;; ADDED: Initialize *unique-solutions* if not already done
  (unless (boundp '*unique-solutions*)
    (setf *unique-solutions* nil))
  ;; CHANGED: Use standard progress reporting with choice-stack length
  (print-search-progress)
  ;; Debug output
  (when (>= *debug* 3)
    (format t "~&Exploring state at level ~A~%" level)))


#+ignore (defun preprocess-state (state level)
  "Hook: Preprocessing with statistics tracking and debugging"
  (declare (ignore state))
  ;; Update search statistics (matching wouldwork's tracking)
  (increment-global *program-cycles* 1)
  (increment-global *total-states-processed* 1)
  ;; Update max depth explored
  (when (> level *max-depth-explored*)
    (setf *max-depth-explored* level))
  ;; ADDED: Initialize *unique-solutions* if not already done
  ;; This ensures the list exists even if no solutions are found
  (unless (boundp '*unique-solutions*)
    (setf *unique-solutions* nil))
  ;; Progress reporting
  (when (and (> *progress-reporting-interval* 0)
             (zerop (mod *total-states-processed* *progress-reporting-interval*)))
    (format t "~&Processed ~A states, current depth ~A~%" 
            *total-states-processed* level))
  ;; Debug output
  (when (>= *debug* 3)
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


(defun backtrack (state level)
  "Production version with computational safeguards"
  ;; Defensive depth limiting to prevent runaway recursion
  (when (> level 10)  ; Reasonable depth limit for blocks problems
    (return-from backtrack nil))
  
  ;; Resource monitoring - check periodically for excessive consumption
  (when (and (> level 5) (zerop (mod *total-states-processed* 100)))
    (format t "~%Progress: Level ~A, States ~A" level *total-states-processed*)
    (finish-output))
  
  (preprocess-state state level)
  
  ;; Goal checking with standard solution reporting
  (when (is-complete-solution state level)
    ;; CHANGED: Use standard register-solution instead of custom reporting
    (let ((mock-node (make-node :state state :depth level :parent nil)))
      (register-solution mock-node state))
    (unless (should-continue-search state level)
      (return-from backtrack t)))
  
  (unless (is-valid-partial-solution state level)
    (return-from backtrack nil))
  
  ;; Full exploration with progress monitoring
  (dolist (action *actions*)
    (let ((choices (generate-choices-for-action action state)))
      ;; Alert on unexpectedly high branching factors
      (when (> (length choices) 10)
        (format t "~%Warning: High branching factor ~A at level ~A" 
                (length choices) level)
        (finish-output))
      
      (dolist (choice choices)
        (when (apply-choice choice state level)
          (let ((deeper-result (backtrack state (1+ level))))
            (undo-choice choice state level)
            (when (and deeper-result 
                      (not (should-continue-search state level)))
              (return-from backtrack t)))))))
  
  nil)


#+ignore (defun backtrack (state level)
  "Production version with computational safeguards"
  ;; Defensive depth limiting to prevent runaway recursion
  (when (> level 10)  ; Reasonable depth limit for blocks problems
    (return-from backtrack nil))
  
  ;; Resource monitoring - check periodically for excessive consumption
  (when (and (> level 5) (zerop (mod *total-states-processed* 100)))
    (format t "~%Progress: Level ~A, States ~A" level *total-states-processed*)
    (finish-output))
  
  (preprocess-state state level)
  
  ;; Goal checking with debugging
  (when (is-complete-solution state level)
    (format t "~%SOLUTION FOUND at level ~A!" level)
    (finish-output)
    (register-backtrack-solution state level)
    (unless (should-continue-search state level)
      (return-from backtrack t)))
  
  (unless (is-valid-partial-solution state level)
    (return-from backtrack nil))
  
  ;; Full exploration with progress monitoring
  (dolist (action *actions*)
    (let ((choices (generate-choices-for-action action state)))
      ;; Alert on unexpectedly high branching factors
      (when (> (length choices) 10)
        (format t "~%Warning: High branching factor ~A at level ~A" 
                (length choices) level)
        (finish-output))
      
      (dolist (choice choices)
        (when (apply-choice choice state level)
          (let ((deeper-result (backtrack state (1+ level))))
            (undo-choice choice state level)
            (when (and deeper-result 
                      (not (should-continue-search state level)))
              (return-from backtrack t)))))))
  
  nil)


(defun create-inverse-update (forward-literals)
  "Creates the inverse of a list of literals for undo operations"
  (mapcar (lambda (literal)
            (if (and (listp literal) (eq (car literal) 'not))
                (second literal)  ; Remove the 'not'
                `(not ,literal))) ; Add 'not'
          forward-literals))


(defun apply-choice (choice state level)
  "Apply a choice to the current state with strict validation and fail-fast error handling"
  (declare (ignore level))
  
  ;; Strict validation - fail immediately if choice structure is invalid
  (unless (and choice (choice-forward-update choice))
    (error "Invalid choice structure: ~A. Choice must have valid forward-update." choice))
  
  ;; Pre-application state validation
  (let ((pre-application-idb-size (hash-table-count (problem-state.idb state))))
    
    ;; Apply the forward update to modify current state
    ;; No error handling here - let any problems bubble up immediately
    (revise (problem-state.idb state) (choice-forward-update choice))
    
    ;; Post-application validation with immediate failure on problems
    (let ((post-application-idb-size (hash-table-count (problem-state.idb state))))
      (when (>= *debug* 3)
        (format t "~%Applied choice: ~A -> ~A facts in database~%" 
                (choice-action choice)
                post-application-idb-size))
      
      ;; Global invariant validation - fail fast if violated
      (when *global-invariants*
        (unless (validate-global-invariants nil state)
          (error "Global invariant violation after applying choice ~A at state ~A" 
                 choice (list-database (problem-state.idb state)))))
      
      ;; Update choice stack for solution path reconstruction
      (push choice *choice-stack*)
      
      ;; Return success indicator
      t)))


(defun undo-choice (choice state level)
  "Undo a choice from the current state with strict validation and fail-fast error handling"
  (declare (ignore level))
  
  ;; Strict validation of choice structure - fail immediately if invalid
  (unless choice
    (error "Cannot undo null choice. This indicates a serious bug in choice stack management."))
  
  ;; Verify that this choice is actually the most recent one on our stack
  ;; This validation catches choice stack corruption early
  (unless (and *choice-stack* (eq choice (first *choice-stack*)))
    (error "Choice stack corruption detected. Attempted to undo ~A but top of stack is ~A. Stack: ~A"
           choice (first *choice-stack*) *choice-stack*))
  
  ;; Validate that we have the inverse update needed for restoration
  (unless (choice-inverse-update choice)
    (error "Cannot undo choice ~A: no inverse update available. This choice cannot be reversed."
           choice))
  
  ;; Capture pre-undo state for validation purposes
  (let ((pre-undo-idb-size (hash-table-count (problem-state.idb state))))
    
    ;; Apply the inverse update to restore the previous state
    ;; No error handling here - any failure should bubble up immediately
    (revise (problem-state.idb state) (choice-inverse-update choice))
    
    ;; Verify that the undo operation produced a sensible result
    (let ((post-undo-idb-size (hash-table-count (problem-state.idb state))))
      (when (>= *debug* 3)
        (format t "~%Undid choice: ~A -> ~A facts in database~%" 
                (choice-action choice)
                post-undo-idb-size))
      
      ;; Critical validation: ensure global invariants still hold after undo
      ;; If undoing a choice violates invariants, our inverse update logic is buggy
      (when *global-invariants*
        (unless (validate-global-invariants nil state)
          (error "Global invariant violation after undoing choice ~A. This indicates a bug in the inverse update logic for action ~A"
                 choice (choice-action choice))))
      
      ;; Remove the choice from our stack - this must happen last
      ;; If we remove it early and then encounter an error, our stack becomes inconsistent
      (pop *choice-stack*)
      
      ;; Return success indicator
      t)))


(defun search-backtracking ()
  "Main entry point for backtracking search with full initialization"
  ;; Initialize search statistics (matching wouldwork's initialization)
  (setf *program-cycles* 0)
  (setf *total-states-processed* 0)
  (setf *max-depth-explored* 0)
  (setf *solutions* nil)
  (setf *unique-solutions* nil)  ; ADDED: Initialize unique solutions list
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
  (when (and (fboundp 'goal-fn) (funcall (symbol-function 'goal-fn) *backtrack-state*))
    (let ((mock-node (make-node :state *backtrack-state* :depth 0 :parent nil)))
      (register-solution mock-node *backtrack-state*))
    ;; CHANGED: Remove summarization call - parent dfs function handles this
    (return-from search-backtracking *solutions*))
  ;; Start the recursive backtracking search
  (let ((search-result (backtrack *backtrack-state* 0)))
    ;; ADDED: Calculate average branching factor before returning
    (setf *average-branching-factor* 
          (if (> *program-cycles* 0)
              (coerce (/ (1- *total-states-processed*) *program-cycles*) 'single-float)
              0.0))
    ;; CHANGED: Remove summarization call - parent dfs function handles this
    *solutions*))


#+ignore (defun search-backtracking ()
  "Main entry point for backtracking search with full initialization"
  ;; Initialize search statistics (matching wouldwork's initialization)
  (setf *program-cycles* 0)
  (setf *total-states-processed* 0)
  (setf *max-depth-explored* 0)
  (setf *solutions* nil)
  (setf *unique-solutions* nil)  ; ADDED: Initialize unique solutions list
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
  (when (and (fboundp 'goal-fn) (funcall (symbol-function 'goal-fn) *backtrack-state*))
    (register-backtrack-solution *backtrack-state* 0)
    ;; CHANGED: Use standard summary function with appropriate condition
    (let ((*package* (find-package :ww)))
      (summarize-search-results 'first))
    (return-from search-backtracking *solutions*))
  ;; Start the recursive backtracking search
  (let ((search-result (backtrack *backtrack-state* 0)))
    ;; ADDED: Calculate average branching factor before summarizing
    (setf *average-branching-factor* 
          (if (> *program-cycles* 0)
              (coerce (/ (1- *total-states-processed*) *program-cycles*) 'single-float)
              0.0))
    ;; CHANGED: Use standard summary function instead of custom one
    (let ((*package* (find-package :ww)))
      (summarize-search-results (if (and search-result *solutions*) 
                                  'first 
                                  'exhausted)))
    *solutions*))


(defun summarize-backtrack-results (termination-reason)
  "DEPRECATED: Summarize backtracking search results in wouldwork's format
   
   This function has been replaced by the standard summarize-search-results
   function to ensure consistent output formatting between depth-first and
   backtracking search algorithms. The standard function provides more
   comprehensive statistics and matches the expected wouldwork output format.
   
   Legacy termination-reason values:
   - 'found-solution -> now handled as 'first in summarize-search-results
   - 'exhausted -> now handled as 'exhausted in summarize-search-results  
   - 'goal-at-start -> now handled as 'first in summarize-search-results"
  (declare (ignore termination-reason))
  (warn "summarize-backtrack-results is deprecated. Use summarize-search-results instead.")
  ;; For backward compatibility, we could call the standard function
  ;; but it's better to encourage migration to the standard approach
  (format t "~%Please update code to use the standard summarize-search-results function.~%"))


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
                     (choice (make-choice 
                             :action action
                             :instantiations (update.instantiations updated-db)
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
  "Convert wouldwork's update structure into a list of literals for backtracking"
  (let ((literals '()))
    ;; Extract changes from the update structure
    (maphash (lambda (int-proposition should-assert)
               ;; Convert integer code back to actual proposition
               (let ((proposition (convert-to-proposition int-proposition)))
                 (if should-assert
                     ;; Assertion: add the proposition as-is
                     (push proposition literals)
                     ;; Retraction: add the proposition wrapped with 'not'
                     (push `(not ,proposition) literals))))
             (update.changes updated-db))
    ;; Return literals in consistent order
    (reverse literals)))


#+ignore (defun process-bt-solution (state level)
  "Hook: Process a complete solution using wouldwork's solution recording system"
  (let ((solution (register-backtrack-solution state level)))
    
    ;; Report the solution
    (format t "~&New solution found at depth = ~A~%" level)
    (when (>= *debug* 1)
      (format t "~&Solution path: ~A~%" (solution.path solution)))
    
    t))


(defun reconstruct-solution-path (choice-stack)
  "Reconstruct the solution path from the choice stack"
  (reverse 
    (mapcar (lambda (choice)
              (record-move-from-choice choice))
            choice-stack)))


(defun record-move-from-choice (choice)
  "Create a move record from a choice (matching wouldwork's record-move format)"
  (list 1.0  ; Time step (simplified for now)
        (cons (action.name (choice-action choice))
              (choice-instantiations choice))))


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


#+ignore (defun register-backtrack-solution (state level)
  "Register a solution with correct structure definition and maintain unique solutions"
  (let ((current-solution (make-solution 
                           :depth level
                           :time (problem-state.time state)
                           :value (problem-state.value state)
                           :path (reconstruct-solution-path *choice-stack*)
                           :goal (copy-problem-state state))))
    ;; Handle different solution types
    (case *solution-type*
      ((first every)
       (push-global current-solution *solutions*)
       (increment-global *solution-count* 1)
       ;; ADDED: Maintain unique solutions list (matching depth-first behavior)
       (when (not (member (problem-state.idb (solution.goal current-solution)) *unique-solutions* 
                          :key (lambda (soln) (problem-state.idb (solution.goal soln)))
                          :test #'equalp))
         (push-global current-solution *unique-solutions*)))
      ((min-length min-time min-value max-value)
       ;; For optimization problems, only keep better solutions
       (when (or (null *solutions*)
                 (solution-better-p current-solution (first *solutions*)))
         (setf *solutions* (list current-solution))
         (setf *solution-count* 1)
         ;; ADDED: Reset unique solutions for optimization problems
         (setf *unique-solutions* (list current-solution)))))
    current-solution))


(defun solution-better-p (new-solution current-best)
  "Determine if new solution is better than current best"
  (case *solution-type*
    (min-length (< (solution.depth new-solution) (solution.depth current-best)))
    (min-time (< (solution.time new-solution) (solution.time current-best)))
    (min-value (< (solution.value new-solution) (solution.value current-best)))
    (max-value (> (solution.value new-solution) (solution.value current-best)))
    (otherwise nil)))


(defun test-backtracking-search ()
  "Test the complete backtracking search on current problem"
  (search-backtracking))

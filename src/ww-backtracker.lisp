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


(defun search-backtracking ()
  "Main entry point for backtracking search with algorithm-compatible processing"
  ;; Initialize search statistics (matching wouldwork's initialization pattern)
  (setf *program-cycles* 0)
  (setf *total-states-processed* 0)
  (setf *max-depth-explored* 0)
  (setf *solutions* nil)
  (setf *unique-solutions* nil)
  (setf *solution-count* 0)
  (setf *start-time* (get-internal-real-time))
  (setf *prior-total-states-processed* 0)
  (setf *prior-time* 0)
  (setf *average-branching-factor* 0.0)
  ;; Initialize backtracking-specific state infrastructure
  (setf *backtrack-state* (copy-problem-state *start-state*))
  (setf *choice-stack* nil)
  ;; Rigorous initial state validation
  (when *global-invariants*
    (unless (validate-global-invariants nil *backtrack-state*)
      (format t "~%Invariant validation failed on initial state.~%")
      (return-from search-backtracking nil)))
  ;; Check if start state satisfies goal condition
  (when (and (fboundp 'goal-fn)
             (funcall (symbol-function 'goal-fn) *backtrack-state*))
    (let ((dummy-choice (make-choice :act '(initial-state)
                                     :forward-update nil
                                     :inverse-update nil
                                     :level 0)))
      (narrate-bt "Solution found at initial state ***" dummy-choice 0)
      (register-solution-bt 0)
      (return-from search-backtracking *solutions*)))
  ;; Initiate recursive backtracking search
  (let ((search-result (backtrack *backtrack-state* 0)))
    (declare (ignore search-result))
    ;; Compute final statistics
    (setf *average-branching-factor* 
          (if (> *program-cycles* 0)
              (coerce (/ (1- *total-states-processed*) *program-cycles*) 'single-float)
              0.0))
    *solutions*))


(defun backtrack (state level)
  "Recursive backtracking search with depth-cutoff integration"
  
  ;; Step 1: Enforce depth cutoff for infinite loop prevention
  (when (and (> *depth-cutoff* 0) (> level *depth-cutoff*))
    (return-from backtrack nil))
  
  ;; Step 2: Update search statistics and perform debugging hooks
  (preprocess-state state level)
  
  ;; Step 3: Check for complete solution
  (when (is-complete-solution state level)
    (register-solution-bt level)
    (when (and *choice-stack* (first *choice-stack*))
      (narrate-bt "Solution found ***" (first *choice-stack*) level))
    (finish-output)
    (unless (should-continue-search state level)
      (return-from backtrack t)))
  
  ;; Step 4: Validate partial solution constraints
  (unless (is-valid-partial-solution state level)
    (return-from backtrack nil))
  
  ;; Step 5: Generate and explore all valid choices
  (let ((found-any-solution nil))
    (dolist (action *actions*)
      (let ((choices (generate-choices-for-action-bt action state level)))
        (dolist (choice choices)
          (unless (detect-path-cycle choice *choice-stack*)
            (when (apply-choice-bt choice state level)
              (let ((deeper-result (backtrack state (1+ level))))
                (undo-choice-bt choice state level)
                (when deeper-result
                  (setf found-any-solution t)
                  (when (eq *solution-type* 'first)
                    (return-from backtrack t)))))))))
    found-any-solution))


(defun apply-choice-bt (choice state level)
  "Apply choice with rigorous validation and state modification"
  
  ;; Step 1: Strict structural validation
  (unless (and choice 
               (choice-forward-update choice)
               (choice-inverse-update choice))
    (error "Invalid choice structure: ~A. Must have forward and inverse updates."
           choice))
  
  ;; Step 2: Validate choice structure integrity
  (unless (choice-forward-update choice)
    (error "Choice must have valid forward-update." choice))
  
  ;; Step 3: Beginning of choice application
  (narrate-bt "Beginning choice application" choice level)
  
  ;; Step 4: Apply the forward update to modify current state
  (let ((pre-application-idb-size (hash-table-count (problem-state.idb state))))
    
    (revise (problem-state.idb state) (choice-forward-update choice))
    
    ;; Step 5: Post-application validation
    (let ((post-application-idb-size (hash-table-count (problem-state.idb state))))
      
      ;; Debug output
      (narrate-bt (format nil "Applied choice: ~A -> ~A facts in database" 
                          (choice-act choice)        
                          post-application-idb-size) 
                  choice level)
      
      ;; Step 6: Global invariant validation
      (when *global-invariants*
        (unless (validate-global-invariants nil state)
          (error "Global invariant violation after applying choice ~A at state ~A" 
                 (choice-act choice) (list-database (problem-state.idb state)))))
      
      (when *global-invariants*
        (narrate-bt "Global invariants validated after choice application" choice level))
      
      ;; Step 7: Add choice to stack
      (push choice *choice-stack*)
      (narrate-bt "Choice successfully applied and added to stack" choice level)
      
      t)))


(defun undo-choice-bt (choice state level)
  "Undo a choice from the current state with strict validation"
  
  ;; Step 1: Strict validation of choice structure
  (unless choice
    (error "Cannot undo null choice. This indicates a serious bug in choice stack management."))
  
  ;; Step 2: Beginning of choice undo
  (narrate-bt "Beginning choice undo" choice level)
  
  ;; Step 3: Verify choice is the most recent one on stack
  (unless (and *choice-stack* (eq choice (first *choice-stack*)))
    (error "Choice stack corruption detected. Attempted to undo ~A but top of stack is ~A. Stack: ~A"
           choice (first *choice-stack*) *choice-stack*))
  
  ;; Step 4: Validate inverse update availability
  (unless (choice-inverse-update choice)
    (error "Cannot undo choice ~A: no inverse update available. This choice cannot be reversed."
           choice))
  
  ;; Step 5: Apply inverse update with error handling
  (handler-case 
      (progn
        (revise (problem-state.idb state) (choice-inverse-update choice))
        
        ;; Step 6: Validate inverse operation succeeded
        (when *global-invariants*
          (unless (validate-global-invariants nil state)
            (error "Global invariant violation after undoing choice ~A. 
                   This indicates a bug in the inverse update logic for action ~A"
                   (choice-act choice) (first (choice-act choice))))))
      
      (error (e)
        (error "Failed to undo choice ~A due to error: ~A. 
               This indicates corruption in inverse update logic." 
               (choice-act choice) e)))
  
  ;; Step 7: Successful validation after undo
  (when *global-invariants*
    (narrate-bt "Global invariants validated after choice undo" choice level))
  
  ;; Step 8: Remove the choice from stack
  (pop *choice-stack*)
  
  ;; Step 9: Completion confirmation
  (narrate-bt "Choice successfully undone and removed from stack" choice level)
  
  t)


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


(defun create-inverse-update (forward-literals)
  "Create logical inverse of forward update with systematic negation handling"
  (mapcar (lambda (literal)
            (cond
             ;; Case 1: Negated literal -> remove negation
             ((and (listp literal) (eq (car literal) 'not))
              (second literal))
             ;; Case 2: Positive literal -> add negation
             (t `(not ,literal))))
          forward-literals))


(defun generate-choices-for-action-bt (action state level)
  "Generate valid choices for action using algorithm-compatible effect processing"
  (declare (ignore level))
  
  ;; Step 1: Validate action structure
  (unless (and action (action.pre-defun-name action) (action.eff-defun-name action))
    (return-from generate-choices-for-action-bt nil))
  
  ;; Step 2: Execute precondition checking using WouldWork's mechanism
  (let ((precondition-fn (action.pre-defun-name action))
        (effect-fn (action.eff-defun-name action))
        (parameter-combinations (action.precondition-args action)))
    
    ;; Step 3: Filter valid precondition instantiations
    (let ((pre-results 
           (remove-if #'null 
                      (mapcar (lambda (pinsts)
                                (apply precondition-fn state pinsts))
                              parameter-combinations))))
      
      (when pre-results
        ;; Step 4: Generate effect updates using our new algorithm-compatible lambda
        (let ((updated-dbs 
               (mapcan (lambda (pre-result)
                         (if (eql pre-result t)
                             (funcall effect-fn state)
                             (apply effect-fn state pre-result)))
                       pre-results))
              (choices '()))
          
          ;; Step 5: Convert update structures to choice objects
          (dolist (updated-db updated-dbs)
            (let* ((forward-literals (create-inverse-update (update.changes updated-db)))
                   (inverse-literals (update.changes updated-db))
                   (combined-act (cons (action.name action)
                                       (update.instantiations updated-db)))
                   (choice (make-choice :act combined-act
                                        :forward-update forward-literals
                                        :inverse-update inverse-literals
                                        :level level)))
              (push choice choices)))
          
          choices)))))


(defun generate-children-for-action (action state)
  "Generate effect updates for a single action - compatible with our new approach"
  ;; Use WouldWork's existing precondition checking
  (let ((precondition-fn (action.pre-defun-name action))
        (effect-fn (action.eff-defun-name action))
        (parameter-combinations (action.precondition-args action)))
    (let ((pre-results 
           (remove-if #'null (mapcar (lambda (pinsts)
                                       (apply precondition-fn state pinsts))
                                     parameter-combinations))))
      (when pre-results
        ;; Get effect updates using our new lambda structure
        (mapcan (lambda (pre-result)
                  (if (eql pre-result t)
                      (funcall effect-fn state)
                      (apply effect-fn state pre-result)))
                pre-results)))))


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


(defun detect-path-cycle (new-choice choice-stack)
  "Check if applying new-choice would create an immediate reversal"
  (detect-immediate-reversal new-choice choice-stack))


(defun detect-immediate-reversal (new-choice choice-stack)
  "Check if new-choice immediately undoes the last choice"
  (when choice-stack
    (let ((last-choice (first choice-stack)))
      ;; Check if new forward-update matches last inverse-update
      (equal (choice-forward-update new-choice)
             (choice-inverse-update last-choice)))))
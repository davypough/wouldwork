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
  "Recursive backtracking search with corrected depth cutoff"
  
  ;; Step 1: Enforce depth cutoff - prevent processing AT the cutoff level
  (when (and (> *depth-cutoff* 0) (>= level *depth-cutoff*))
    (return-from backtrack nil))
  
  ;; Step 2: Update search statistics and perform debugging hooks
  (preprocess-state state level)
  
  ;; Step 3: Validate partial solution constraints
  (unless (is-valid-partial-solution state level)
    (return-from backtrack nil))
  
  ;; Step 4: Generate and explore all valid choices
  (let ((found-any-solution nil))
    (dolist (action *actions*)
      (let ((choices (generate-choices-for-action-bt action state level)))
        (dolist (choice choices)
          (unless (detect-path-cycle choice *choice-stack*)
            (when (apply-choice-bt choice state level)
              (if (is-complete-solution state level)
                  ;; Solution found at current level - register and handle continuation
                  (progn
                    (register-solution-bt level)
                    (when (and *choice-stack* (first *choice-stack*))
                      (narrate-bt "Solution found ***" (first *choice-stack*) level))
                    (finish-output)
                    (setf found-any-solution t)
                    (undo-choice-bt choice state level)
                    ;; Check if we should stop after first solution
                    (when (eq *solution-type* 'first)
                      (return-from backtrack t)))
                  ;; No solution yet - continue recursive exploration
                  (let ((deeper-result (backtrack state (1+ level))))
                    (undo-choice-bt choice state level)
                    (when deeper-result
                      (setf found-any-solution t)
                      (when (eq *solution-type* 'first)
                        (return-from backtrack t))))))))))
    found-any-solution))


(defun preprocess-state (state level)
  "Hook: Preprocessing with statistics tracking and debugging"
  (declare (ignore state))
  (increment-global *program-cycles* 1)
  (increment-global *total-states-processed* 1)
  (when (> (1+ level) *max-depth-explored*)
    (setf *max-depth-explored* (1+ level)))
  (print-search-progress)
  #+:ww-debug (when (>= *debug* 3)
                (format t "~2%Exploring state at level ~A~%" level)
                (finish-output)))


(defun apply-choice-bt (choice state level)
  "Apply choice with rigorous validation, time state management, and comprehensive debug support"
  
  ;; Step 1: Strict structural validation
  (unless (and choice 
               (choice-forward-update choice)
               (choice-inverse-update choice))
    (error "Invalid choice structure: ~A. Must have forward and inverse updates."
           choice))
  
  ;; Step 2: Validate choice structure integrity
  (unless (choice-forward-update choice)
    (error "Choice must have valid forward-update." choice))
  
  ;; Step 3: Capture pre-state for debug transition display
  (let ((pre-state (copy-problem-state state)))
    
    ;; Step 4: Look up action duration for time management
    (let* ((action-name (first (choice-act choice)))
           (action (find action-name *actions* :key #'action.name))
           (action-duration (if action (action.duration action) 1.0))
           (pre-application-idb-size (hash-table-count (problem-state.idb state))))
      
      ;; Step 5: Apply the forward update to modify current state database
      (revise (problem-state.idb state) (choice-forward-update choice))
      
      ;; Step 6: Update time field to reflect action duration
      (incf (problem-state.time state) action-duration)
      
      ;; Step 7: Post-application validation
      (let ((post-application-idb-size (hash-table-count (problem-state.idb state))))
        
        ;; Debug output with pre-state information
        (narrate-bt "" choice (1+ level) pre-state)
        
        ;; Step 8: Global invariant validation
        (when *global-invariants*
          (unless (validate-global-invariants nil state)
            (error "Global invariant violation after applying choice ~A at state ~A" 
                   (choice-act choice) (list-database (problem-state.idb state)))))
        
        (when *global-invariants*
          (narrate-bt "Global invariants validated after choice application" choice level pre-state))
        
        ;; Step 9: Add choice to stack
        (push choice *choice-stack*)
        
        t))))


#+ignore (defun apply-choice-bt (choice state level)
  "Apply choice with rigorous validation and time state management"
  
  ;; Step 1: Strict structural validation
  (unless (and choice 
               (choice-forward-update choice)
               (choice-inverse-update choice))
    (error "Invalid choice structure: ~A. Must have forward and inverse updates."
           choice))
  
  ;; Step 2: Validate choice structure integrity
  (unless (choice-forward-update choice)
    (error "Choice must have valid forward-update." choice))
  
  ;; Step 4: Look up action duration for time management
  (let* ((action-name (first (choice-act choice)))
         (action (find action-name *actions* :key #'action.name))
         (action-duration (if action (action.duration action) 1.0))
         (pre-application-idb-size (hash-table-count (problem-state.idb state))))
    
    ;; Step 5: Apply the forward update to modify current state database
    (revise (problem-state.idb state) (choice-forward-update choice))
    
    ;; Step 6: Update time field to reflect action duration
    (incf (problem-state.time state) action-duration)
    
    ;; Step 7: Post-application validation
    (let ((post-application-idb-size (hash-table-count (problem-state.idb state))))
      
      ;; Debug output
      (narrate-bt "" choice (1+ level))
      
      ;; Step 8: Global invariant validation
      (when *global-invariants*
        (unless (validate-global-invariants nil state)
          (error "Global invariant violation after applying choice ~A at state ~A" 
                 (choice-act choice) (list-database (problem-state.idb state)))))
      
      (when *global-invariants*
        (narrate-bt "Global invariants validated after choice application" choice level))
      
      ;; Step 9: Add choice to stack
      (push choice *choice-stack*)
      
      t)))


(defun undo-choice-bt (choice state level)
  "Undo a choice from the current state with strict validation and time reversal"
  
  ;; Step 1: Strict validation of choice structure
  (unless choice
    (error "Cannot undo null choice. This indicates a serious bug in choice stack management."))
  
  ;; Step 3: Verify choice is the most recent one on stack
  (unless (and *choice-stack* (eq choice (first *choice-stack*)))
    (error "Choice stack corruption detected. Attempted to undo ~A but top of stack is ~A. Stack: ~A"
           choice (first *choice-stack*) *choice-stack*))
  
  ;; Step 4: Look up action duration for time reversal
  (let* ((action-name (first (choice-act choice)))
         (action (find action-name *actions* :key #'action.name))
         (action-duration (if action (action.duration action) 1.0)))
    
    ;; Step 5: Validate inverse update availability
    (unless (choice-inverse-update choice)
      (error "Cannot undo choice ~A: no inverse update available. This choice cannot be reversed."
             choice))
    
    ;; Step 6: Apply inverse update with error handling
    (handler-case 
        (progn
          (revise (problem-state.idb state) (choice-inverse-update choice))
          
          ;; Step 7: Revert time field to previous state
          (decf (problem-state.time state) action-duration)
          
          ;; Step 8: Validate inverse operation succeeded
          (when *global-invariants*
            (unless (validate-global-invariants nil state)
              (error "Global invariant violation after undoing choice ~A. 
                     This indicates a bug in the inverse update logic for action ~A"
                     (choice-act choice) (first (choice-act choice))))))
        
        (error (e)
          (error "Failed to undo choice ~A due to error: ~A. 
                 This indicates corruption in inverse update logic." 
                 (choice-act choice) e)))
    
    ;; Step 9: Successful validation after undo
    (when *global-invariants*
      (narrate-bt "Global invariants validated after choice undo" choice level))
    
    ;; Step 10: Remove the choice from stack
    (pop *choice-stack*))
    
    t)


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
  "Reconstruct the solution path from the choice stack with correct cumulative time"
  (let ((cumulative-time 0.0)
        (path '()))
    ;; Process choices in reverse order (oldest first) to build cumulative time
    (dolist (choice (reverse choice-stack))
      (let* ((action-name (first (choice-act choice)))
             (action (find action-name *actions* :key #'action.name))
             (action-duration (if action (action.duration action) 1.0)))
        ;; Update cumulative time
        (setf cumulative-time (+ cumulative-time action-duration))
        ;; Build move record with correct time
        (push (list cumulative-time (choice-act choice)) path)))
    (nreverse path)))


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


(defun narrate-bt (string choice depth &optional pre-state)
  "Enhanced narration function with refined progressive debug level disclosure"
  (declare (ignorable string choice depth pre-state))
  #+:ww-debug (when (>= *debug* 1)
                (update-search-tree-bt choice depth string))
  #+:ww-debug (when (>= *debug* 2)
                (when (and string (not (string= string "")))
                  (format t "~%~A:~%" string))
                (when choice
                  (format t "~%Action: ~A~%" (choice-act choice))
                  (format t "Forward Update: ~A~%" (choice-forward-update choice))
                  (format t "Inverse Update: ~A~%" (choice-inverse-update choice)))
                (unless choice
                  (format t "Choice: <nil>~%"))
                (if pre-state
                    (progn
                      (format t "Pre-State IDB:  ~A~%" (list-database (problem-state.idb pre-state)))
                      (format t "Post-State IDB: ~A~%" (list-database (problem-state.idb *backtrack-state*)))
                      (when (problem-state.hidb pre-state)
                        (format t "Pre-State HIDB:  ~A~%" (list-database (problem-state.hidb pre-state))))
                      (when (problem-state.hidb *backtrack-state*)
                        (format t "Post-State HIDB: ~A~%" (list-database (problem-state.hidb *backtrack-state*))))
                      (format t "Time Change: ~A -> ~A~%" 
                              (problem-state.time pre-state) 
                              (problem-state.time *backtrack-state*))
                      (format t "Value Change: ~A -> ~A~%" 
                              (problem-state.value pre-state) 
                              (problem-state.value *backtrack-state*)))
                    ;; Fallback when pre-state not available
                    (progn
                      (format t "Current State IDB: ~A~%" (list-database (problem-state.idb *backtrack-state*)))
                      (when (problem-state.hidb *backtrack-state*)
                        (format t "Current State HIDB: ~A~%" (list-database (problem-state.hidb *backtrack-state*))))
                      (format t "Current Time: ~A~%" (problem-state.time *backtrack-state*))
                      (format t "Current Value: ~A~%" (problem-state.value *backtrack-state*)))))
  #+:ww-debug (when (>= *debug* 3)
                (format t "--- Choice Stack (Length ~A) ---~%" (length *choice-stack*))
                (if *choice-stack*
                    (loop for i from 0
                          for stack-choice in *choice-stack*
                          do (format t "  [~A] ~A~%" i (choice-act stack-choice)))
                    (format t "  <empty>~%")))
  #+:ww-debug (when (>= *debug* 5)
                (simple-break))
  nil)


(defun register-solution-bt (level)
  "Register a solution found via backtracking using the choice stack"
  (let* ((solution-depth (length *choice-stack*))
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
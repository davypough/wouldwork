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
  "Core recursive backtracking function"
  (preprocess-state state level)
  ;; Safety limit to prevent stack overflow (keep this from debug version)
  (when (> level 50)
    (return-from backtrack nil))
  ;; Check depth cutoff
  (when (and (> *depth-cutoff* 0) (>= level *depth-cutoff*))
    (return-from backtrack nil))
  ;; Validate partial solution
  (unless (is-valid-partial-solution state level)
    (return-from backtrack nil))
  ;; Check for complete solution
  (when (is-complete-solution state level)
    (register-backtrack-solution state level)
    (return-from backtrack (not (should-continue-search state level))))
  ;; Generate and try choices
  (let ((choices (generate-choices state level)))
    (when (zerop (length choices))
      (return-from backtrack nil))
    (dolist (choice choices)
      ;; Apply choice
      (when (apply-choice choice state level)
        ;; Recursive search
        (when (backtrack state (1+ level))
          ;; Found solution - undo and potentially return
          (undo-choice choice state level)
          (unless (should-continue-search state level)
            (return-from backtrack t)))
        ;; Undo choice for next iteration
        (undo-choice choice state level))))
  nil)


(defun create-inverse-update (forward-literals)
  "Creates the inverse of a list of literals for undo operations"
  (mapcar (lambda (literal)
            (if (and (listp literal) (eq (car literal) 'not))
                (second literal)  ; Remove the 'not'
                `(not ,literal))) ; Add 'not'
          forward-literals))


(defun apply-choice (choice state level)
  "Apply a choice to the current state using wouldwork's update system"
  (declare (ignore level))
  (when (choice-forward-update choice)
    ;; Use wouldwork's existing revise function to apply updates
    (revise (problem-state.idb state) (choice-forward-update choice))
    (push choice *choice-stack*)
    t))


(defun undo-choice (choice state level)
  "Undo a choice from the current state"
  (declare (ignore level))
  (when (choice-inverse-update choice)
    ;; Apply the inverse update to restore previous state
    (revise (problem-state.idb state) (choice-inverse-update choice))
    (pop *choice-stack*)
    t))


(defun search-backtracking ()
  "Main entry point for backtracking search with full initialization"
  ;; Initialize search statistics (matching wouldwork's initialization)
  (setf *program-cycles* 0)
  (setf *total-states-processed* 0)
  (setf *max-depth-explored* 0)
  (setf *solutions* nil)
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
    (summarize-backtrack-results 'goal-at-start)
    (return-from search-backtracking *solutions*))
  ;; Start the recursive backtracking search
  (let ((search-result (backtrack *backtrack-state* 0)))
    (summarize-backtrack-results (if search-result 'found-solution 'exhausted))
    *solutions*))


(defun summarize-backtrack-results (termination-reason)
  "Summarize backtracking search results in wouldwork's format"
  (let ((elapsed-time (/ (- (get-internal-real-time) *start-time*) 
                         internal-time-units-per-second))
        (*package* (find-package :ww)))
    (format t "~%~%In problem ~A, performed BACKTRACKING search for ~A solution.~%" 
            *problem-name* *solution-type*)
    (case termination-reason
      (found-solution (format t "Search process completed normally.~%"))
      (exhausted (format t "Search exhausted without finding solution.~%"))
      (goal-at-start (format t "Goal satisfied by start state.~%")))
    (format t "Maximum depth explored = ~A~%" *max-depth-explored*)
    (format t "Program cycles = ~A~%" *program-cycles*)
    (format t "Total states processed = ~A~%" *total-states-processed*)
    (when (> *program-cycles* 1)
      (format t "Average branching factor = ~A~%" 
              (coerce (/ (1- *total-states-processed*) *program-cycles*) 'single-float)))
    (format t "~%Start state:~%~A~%" (list-database (problem-state.idb *start-state*)))
    (when (fboundp 'goal-fn)
      (format t "~%Goal: <defined>~%"))
    (format t "~%Total solution paths found = ~A~%" *solution-count*)
    (when *solutions*
      (let ((best-solution (first *solutions*)))
        (format t "~%Number of steps in solution = ~A~%" (solution.depth best-solution))
        (format t "~%Solution path:~%")
        (dolist (step (solution.path best-solution))
          (format t "~A~%" step))
        (format t "~%Final state:~%~A~%" 
                (list-database (problem-state.idb *backtrack-state*)))))
    (format t "~%Evaluation took: ~A seconds~%" elapsed-time)))


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


(defun process-bt-solution (state level)
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


(defun register-backtrack-solution (state level)
  "Register a solution with correct structure definition"
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
       (increment-global *solution-count* 1))
      ((min-length min-time min-value max-value)
       ;; For optimization problems, only keep better solutions
       (when (or (null *solutions*)
                 (solution-better-p current-solution (first *solutions*)))
         (setf *solutions* (list current-solution))
         (setf *solution-count* 1))))
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

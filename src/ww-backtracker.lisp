;;; Filename: ww-backtracker.lisp

;;; Backtracking search infrastructure for wouldwork


(in-package :ww)


;; Basic data structures for backtracking


(defstruct (choice (:conc-name choice.))
  "Represents a choice point in backtracking search"
  act             ; (action-name arg1 arg2 ...)
  forward-update  ; The update structure that applies this choice
  inverse-update  ; The update structure that undoes this choice
  level)          ; Depth in the search tree


(defparameter *backtrack-state* nil
  "The single working state for backtracking search")


(defparameter *choice-stack* nil
  "Stack of choices made during backtracking search; path back to start state.")


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
  (setf *inconsistent-states-dropped* 0)
  ;; Initialize backtracking-specific state infrastructure
  (setf *backtrack-state* (copy-problem-state *start-state*))
  (setf *choice-stack* nil)
  ;(clrhash *proposition-cache*)
  ;(setf *last-object-index* 0)
  ;; initial state validation
  (when *global-invariants*
    (unless (validate-global-invariants nil *backtrack-state*)
      (format t "~%Invariant validation failed on initial state.~%")
      (return-from search-backtracking nil)))
  ;; Check if start state satisfies goal condition
  (when (is-complete-solution)
    (register-solution-bt 0)
    (narrate-bt "Solution found at start ***" nil 0)
    (return-from search-backtracking t))
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
  (backtrack 0)
  ;; Compute final statistics
  (setf *average-branching-factor* (if (> *program-cycles* 0)
                                     (coerce (/ (1- *total-states-processed*) *program-cycles*)
                                             'single-float)
                                     0.0))
  t)


(defun update-statistics (level)
  "Update search statistics."
  (increment-global *program-cycles* 1)
  (increment-global *total-states-processed* 1)
  (when (> (1+ level) *max-depth-explored*)
    (setf *max-depth-explored* (1+ level)))
  (print-search-progress))


(defun backtrack (level)
  "Recursive backtracking search over new states from assert clauses."
  
  ;; Step 1: Enforce depth cutoff
  (when (and (> *depth-cutoff* 0) (>= level *depth-cutoff*))
    (return-from backtrack nil))
  
  ;; Step 2: Update search statistics
  (update-statistics level)
  
  ;; Step 3: CSP-aware action selection and processing
  (let ((found-a-solution nil)
        ;; CSP RESTRICTION: Select actions based on problem type and level
        (actions (if (and (eql *problem-type* 'csp) (< level (length *actions*)))
                     (list (nth level *actions*))  ; CSP: single action per level
                     *actions*)))                   ; Planning: all actions
    
    (dolist (action actions)
      (let ((parameter-combinations (if (action.dynamic action)
                                      ;; Dynamic case: compute combinations from current state
                                      (eval-instantiated-spec (action.precondition-type-inst action) 
                                                              *backtrack-state*)
                                      ;; Static case: use pre-computed combinations
                                      (action.precondition-args action)))
            (precondition-fn (action.pre-defun-name action)))
        ;; Filter symmetric instantiations if symmetry pruning enabled
        (when *symmetry-pruning*
          (setf parameter-combinations 
                (filter-symmetric-instantiations action parameter-combinations *backtrack-state*)))
        ;; Process each parameter combination individually against current state
        (dolist (param-combo parameter-combinations)
          (let ((precondition-result (apply precondition-fn *backtrack-state* param-combo)))
            (when precondition-result  ; Only process if preconditions satisfied
              (let ((choices-from-combination 
                      (generate-choices-for-single-combination-bt action param-combo
                                                                  precondition-result level)))
                ;; Process each choice generated from this parameter combination
                ;; (Multiple choices possible due to multiple assert statements in an action)
                (dolist (choice choices-from-combination)
                  (unless (detect-path-cycle choice)
                    (when (register-choice-bt choice action level)
                      (unwind-protect
                        (if (is-complete-solution)
                          ;; Solution found at current level - register and handle continuation
                          (progn (register-solution-bt (1+ level))
                                 (narrate-bt "Solution found ***" (first *choice-stack*) (1+ level))
                                 (finish-output)
                                 (setf found-a-solution t)
                                 (when (eq *solution-type* 'first)
                                   (return-from backtrack t)))
                          ;; No solution yet - continue recursive exploration
                          (let ((deeper-result (backtrack (1+ level))))
                            (when deeper-result
                              (setf found-a-solution t)
                              (when (eq *solution-type* 'first)
                                (return-from backtrack t)))))
                        (undo-choice-bt choice action level)))))))))))
    found-a-solution))


(defun generate-choices-for-single-combination-bt (action param-combo precondition-result level)
  "Generate choices by executing effect with incremental updates to *backtrack-state*.
   Effect modifies state, then changes are undone. Returns choices with forward/inverse ops."
  
  (let ((effect-fn (action.eff-defun-name action))
        (choices '()))

    ;; Execute effect function - MODIFIES *backtrack-state* incrementally
    (let ((updated-dbs 
            (if (eql precondition-result t)
                (funcall effect-fn *backtrack-state*)
                (apply effect-fn *backtrack-state* precondition-result))))
      
      ;; Process each updated-db into separate choice structure
      (dolist (updated-db updated-dbs)
        (let ((change-lists (update.changes updated-db)))
          (when change-lists
            (destructuring-bind (forward-ops inverse-ops) change-lists
              (let* ((combined-act (cons (action.name action)
                                        (update.instantiations updated-db)))
                     (new-choice (make-choice :act combined-act
                                             :forward-update forward-ops
                                             :inverse-update inverse-ops
                                             :level level)))
                (push new-choice choices)
                
                ;; CRITICAL: Undo changes immediately after capturing
                ;;           So next action sees correct state
                (revise (problem-state.idb *backtrack-state*) inverse-ops))))))
      
      ;; Return choices in forward execution order
      (nreverse choices))))


(defun register-choice-bt (choice action level)
  "Register a choice by applying its forward operations to *backtrack-state*."
    
  #+:ww-debug
  (when (>= *debug* 3)
    (format t "~%Current state: ~A~%" (list-database (problem-state.idb *backtrack-state*))))

  ;; Step 1: Apply forward operations
  (revise (problem-state.idb *backtrack-state*) (choice.forward-update choice))

  ;; Step 2: Name and time update
  (setf (problem-state.name *backtrack-state*) (action.name action))
  (incf (problem-state.time *backtrack-state*) (action.duration action))

  ;; Step 3: Global invariants
  (when *global-invariants*
    (unless (validate-global-invariants nil *backtrack-state*)
      (revise (problem-state.idb *backtrack-state*) (choice.inverse-update choice))
      (decf (problem-state.time *backtrack-state*) (action.duration action))
      (error "Global invariant violation in successor state from action ~A" (choice.act choice))))

  ;; Step 4: Constraint
  (when (and (fboundp 'constraint-fn)
             (not (funcall (symbol-function 'constraint-fn) *backtrack-state*)))
    (revise (problem-state.idb *backtrack-state*) (choice.inverse-update choice))
    (decf (problem-state.time *backtrack-state*) (action.duration action))
    (return-from register-choice-bt nil))

  ;; Step 5: Choice stack
  (push choice *choice-stack*)

  ;; Step 6: Debug output
  (narrate-bt "" choice (1+ level))

  t)


(defun undo-choice-bt (choice action level)
  "Undo a choice from the current state with time reversal and stack management"

  ;; Inverse state update
  (revise (problem-state.idb *backtrack-state*) (choice.inverse-update choice))

  ;; Reverse name & time
  (setf (problem-state.name *backtrack-state*) 
        (if (> (length *choice-stack*) 1)
          (first (choice.act (second *choice-stack*)))  ; Previous action name
          'start))
  (decf (problem-state.time *backtrack-state*) (action.duration action))

  ;; Debug
  (narrate-bt "Backtracking to" choice level)

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
                      (format t "~%Backtracking to: ~A~%" (choice.act choice)))
                    ;; Normal detailed output for non-backtrack operations
                    (progn
                      (when (and string (not (string= string "")))
                        (format t "~%~A:~%" string))
                      (when choice
                        (format t "~%Action: ~A~%" (choice.act choice))
                        (format t "Depth: ~A~%" depth)
                        (format t "Forward Update: ~A~%" (choice.forward-update choice))
                        (format t "Inverse Update: ~A~%" (choice.inverse-update choice)))
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
                                     (choice.act stack-choice)
                                     ;; Highlight current choice being processed
                                     (if (and choice (eq stack-choice choice))
                                         " ← current"
                                         "")))
                    (format t "  <empty>~%")))
  
  ;; Debug level 5: Interactive breakpoint
  #+:ww-debug (when (>= *debug* 5)
                (simple-break))
  
  nil)


(defun register-solution-bt (level)
  "Register a solution found via backtracking using the choice stack"
  (let* ((solution-depth (length *choice-stack*))
         (solution-path (reconstruct-solution-path))
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


(defun detect-path-cycle (new-choice)
  "Check if new-choice immediately undoes the last choice using set-based comparison"
  (let ((last-choice (first *choice-stack*)))
    ;; Check if new forward-update matches last inverse-update (set comparison)
    (when last-choice
      (alexandria:set-equal (choice.forward-update new-choice)
                            (choice.inverse-update last-choice)
                            :test #'equal))))

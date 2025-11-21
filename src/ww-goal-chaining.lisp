;;; Filename: ww-goal-chaining.lisp

;;; Goal chaining capability for solving sequential subgoals.

(in-package :ww)


(defmacro ww-continue (goal-form)
  "Continue search from the final state of the previous solution.
   Installs GOAL-FORM as the new goal and updates *start-state*.
   
   Usage:
     (solve)                          ; Solve initial goal
     (ww-continue (at agent target))  ; Set up continuation
     (solve)                          ; Solve for new goal
   
   Updates only the starting conditions:
   - Previous goal state becomes new *start-state*
   - New goal installed and compiled
   - Heuristic re-applied to new start state
   
   The next (solve) call will automatically reset all search counters
   and initialize data structures as usual."
  `(continue-from-solution ',goal-form))


(defun continue-from-solution (goal-form)
  "Internal implementation of ww-continue.
   Updates *start-state* and goal, then lets normal (solve) flow handle the rest."
  
  ;; Validate preconditions
  (validate-continuation-preconditions)
  
  ;; Extract goal state from most recent solution
  (let ((goal-state (extract-goal-state-from-solution)))
    
    ;; Update *start-state* with goal state data
    (update-start-state-from-goal goal-state)
    
    ;; Install and compile new goal (uses existing infrastructure)
    (install-goal goal-form)
    (when (boundp 'goal-fn)
      (compile 'goal-fn (subst-int-code (symbol-value 'goal-fn))))
    
    ;; Re-apply heuristic to new start state if defined
    (when (fboundp 'heuristic?)
      (setf (problem-state.heuristic *start-state*)
            (funcall (symbol-function 'heuristic?) *start-state*))
      (format t "~&Heuristic applied to continuation state: ~A~%" 
              (problem-state.heuristic *start-state*)))
    
    ;; Display continuation status
    (format t "~2%Continuation initialized from solution state.")
    (format t "~%  Continuation time: ~A" (problem-state.time *start-state*))
    (format t "~%  Continuation value: ~A" (problem-state.value *start-state*))
    (format t "~%  State propositions: ~A" 
            (hash-table-count (problem-state.idb *start-state*)))
    (format t "~%~%Ready to (solve) for new goal.~2%")
    t))


(defun validate-continuation-preconditions ()
  "Verify system state allows continuation."
  (when (> *threads* 0)
    (error "Goal chaining requires single-threaded mode. ~
            Set (ww-set *threads* 0) before using ww-continue."))
  
  (unless (and (boundp '*solutions*) *solutions*)
    (error "No solution exists to continue from. ~
            First run (solve) successfully, then use (ww-continue <new-goal>)."))
  
  (unless (boundp 'goal-fn)
    (error "No goal function currently defined.")))


(defun extract-goal-state-from-solution ()
  "Extract the final goal state from most recent solution based on *solution-type*."
  (let ((solution (case *solution-type*
                    ((first min-length) 
                     ;; Find minimum depth solution
                     (reduce (lambda (s1 s2)
                               (if (< (solution.depth s1) (solution.depth s2)) s1 s2))
                             *solutions*))
                    (min-time
                     ;; Find minimum time solution
                     (reduce (lambda (s1 s2)
                               (if (< (solution.time s1) (solution.time s2)) s1 s2))
                             *solutions*))
                    (min-value
                     ;; Find minimum value solution
                     (reduce (lambda (s1 s2)
                               (if (< (solution.value s1) (solution.value s2)) s1 s2))
                             *solutions*))
                    (max-value
                     ;; Find maximum value solution
                     (reduce (lambda (s1 s2)
                               (if (> (solution.value s1) (solution.value s2)) s1 s2))
                             *solutions*))
                    (every
                     ;; For 'every', use minimum depth as canonical choice
                     (reduce (lambda (s1 s2)
                               (if (< (solution.depth s1) (solution.depth s2)) s1 s2))
                             *solutions*))
                    (t 
                     ;; Default: first solution in list
                     (first *solutions*)))))
    
    (unless solution
      (error "Could not extract solution from *solutions*."))
    
    (let ((goal-state (solution.goal solution)))
      ;; Validate state consistency before using
      (when (gethash 'inconsistent-state (problem-state.idb goal-state))
        (error "Cannot continue from inconsistent goal state."))
      goal-state)))


(defun update-start-state-from-goal (goal-state)
  "Update *start-state* with all components from goal-state.
   Preserves temporal continuity: time, happenings, databases."
  (declare (type problem-state goal-state))
  
  ;; Update basic state components
  (setf (problem-state.name *start-state*) 'continuation)
  (setf (problem-state.instantiations *start-state*) nil)
  
  ;; Preserve temporal state
  (setf (problem-state.time *start-state*) 
        (problem-state.time goal-state))
  (setf (problem-state.value *start-state*) 
        (problem-state.value goal-state))
  (setf (problem-state.heuristic *start-state*) 
        (problem-state.heuristic goal-state))
  
  ;; Preserve happenings - deep copy to avoid aliasing
  ;; This maintains temporal continuity with exogenous events
  (setf (problem-state.happenings *start-state*) 
        (copy-tree (problem-state.happenings goal-state)))
  
  ;; Copy databases - both proposition state and happening events
  (setf (problem-state.idb *start-state*) 
        (copy-idb (problem-state.idb goal-state)))
  (setf (problem-state.hidb *start-state*) 
        (copy-idb (problem-state.hidb goal-state)))
  (setf (problem-state.idb-hash *start-state*) 
        (problem-state.idb-hash goal-state)))

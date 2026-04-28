;;; Filename: ww-goal-chaining.lisp

;;; Goal chaining capability for solving sequential subgoals.

(in-package :ww)


(defstruct undo-checkpoint
  "Saved state for goal-chaining undo."
  start-state          ; Deep copy of *start-state*
  goal)                ; User's *goal* specification


(defparameter *undo-stack* nil
  "Stack of undo checkpoints. Each entry is a snapshot of the planning state.")


(defparameter *solutions-valid* nil
  "Indicates whether *solutions* contains a completed, valid solution set.")


(defvar *final-goal* nil
  "Snapshot of the originally installed goal, captured on first solve-subgoal
   call so that (solve) can reinstate it after a chain of subgoals.
   Cleared on refresh.")


;; ============================================================


(defmacro solve-subgoal (goal-form)
  `(let ((completed nil))
     ;; Prepare planning state (push undo, install goal, etc.)
     (continue-from-solution ',goal-form)
     ;; Clear any prior solutions before solving
     (setf *solutions* nil
           *solutions-valid* nil)
     (unwind-protect
         (progn
           ;; ---- RUN SOLVER ----
           (solve)
           ;; Mark success only if we actually got solutions
           (when *solutions*
             (setf *solutions-valid* t))
           (setf completed t))
       ;; ---- CLEANUP (runs on Ctrl-C or error) ----
       (unless completed
         (setf *solutions* nil
               *solutions-valid* nil)
         (format t "~&Solve interrupted. Use (ww-undo) to revert.~%")))))


(defun continue-from-solution (goal-form)
  "Prepare the system to solve a new subgoal, either by continuing from a
   previous solution or by overriding the goal before the first solve."
  ;; Capture the original final goal only once
  (unless *final-goal*
    (setf *final-goal* *goal*))
  ;; Save undo checkpoint BEFORE any mutation
  (save-undo-checkpoint)
  ;; Any new attempt invalidates prior solutions unless proven otherwise
  (setf *solutions-valid* nil)
  (if (and *solutions-valid* *solutions*)
      ;; --- Continuation branch ---
      (progn
        ;; NOTE: If update-start-state-from-goal expects a problem-state,
        ;; you may need (solution-goal (car *solutions*)) instead.
        (update-start-state-from-goal (car *solutions*))
        (install-goal goal-form)
        (when (boundp 'goal-fn)
          (compile 'goal-fn
                   (subst-int-code (symbol-value 'goal-fn))))
        (format t "~&Continuing from previous solution...~%"))
      ;; --- First subgoal / fresh attempt ---
      (progn
        ;; Do NOT modify *start-state*
        (install-goal goal-form)
        (when (boundp 'goal-fn)
          (compile 'goal-fn
                   (subst-int-code (symbol-value 'goal-fn))))
        (format t "~&Ready to solve subgoal.~%"))))


(defun validate-continuation-preconditions ()
  "Verify system state allows continuation."
  (when (> *threads* 0)
    (error "Goal chaining requires single-threaded mode. ~
            Set (ww-set *threads* 0) before using solve-subgoal."))
  ;(unless (and (boundp '*solutions*) *solutions*)
  ;  (error "No solution exists to continue from. ~
  ;          First run (solve) successfully, then use (solve-subgoal <new-goal>)."))
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


(defun save-undo-checkpoint ()
  "Push the current planning state onto the undo stack."
  (push (list :start-state *start-state*
              :goal *goal*
              :final-goal *final-goal*)
        *undo-stack*))


(defun ww-undo ()
  "Undo the most recent solve-subgoal by restoring the previous planning state."
  (if (null *undo-stack*)
      (format t "~&Nothing to undo.~%")
      (let ((checkpoint (pop *undo-stack*)))
        (setf *start-state* (getf checkpoint :start-state))
        (setf *goal*        (getf checkpoint :goal))
        (setf *final-goal*  (getf checkpoint :final-goal))
        ;; Clear solver artifacts
        (when (boundp '*solutions*)
          (setf *solutions* nil))
        (setf *solutions-valid* nil)
        (format t "~&Reverted to previous state.~2%")
        (format t "Current State: ~%~A~%" *start-state*)
        (format t "Current Goal: ~%~A~2%" *goal*))))
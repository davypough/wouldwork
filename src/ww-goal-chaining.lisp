;;; Filename: ww-goal-chaining.lisp

;;; Goal chaining capability for serial, sequential subgoals.  Each operation selects the
;;; preceding search's canonical solution, deep-copies its goal state into the next search
;;; baseline, and replaces the ordinary per-search solution results.  A deep undo checkpoint
;;; restores the complete preceding session.  A normal no-solution result deliberately leaves
;;; the prepared baseline available for a retry; WW-UNDO rolls the operation back instead.
;;;
;;; This generic substrate retains neither segment history nor cumulative metrics, and it does
;;; not optimize or backtrack across subgoal boundaries.  Specialized continuation policies can
;;; register checkpoint extensions for their own history, as recorder cycle chaining does.

(in-package :ww)


(defstruct undo-checkpoint
  "Complete planning-session snapshot for one goal-chaining operation."
  start-state
  goal
  goal-function-bound-p
  final-goal
  solution-paths
  solutions-valid
  extension-states)


(defparameter *undo-stack* nil
  "Stack of independent planning-session snapshots.")


(defvar *final-goal* nil
  "Originally installed goal, restored by SOLVE after intermediate subgoals.")


(defmacro solve-subgoal (goal-form)
  `(solve-subgoal-form ',goal-form))


(defun capture-goal-chaining-extension-states ()
  "Capture every registered extension and retain the restorer with its snapshot."
  (loop for (name snapshotter restorer) in *goal-chaining-checkpoint-extensions*
        collect (list name restorer (funcall (symbol-function snapshotter)))))


(defun restore-goal-chaining-extension-states (states)
  "Restore checkpoint extension STATES captured by SAVE-UNDO-CHECKPOINT."
  (dolist (state states)
    (funcall (symbol-function (second state)) (third state)))
  t)


(defun solve-subgoal-form (goal-form)
  "Solve GOAL-FORM as one subgoal and retain an undo checkpoint.

The preceding valid solution, when present, supplies the new start state.  A normal
no-solution result leaves that prepared start state available for a retry; WW-UNDO restores
the complete pre-call session, including the preceding solutions."
  (let ((completed nil)
        (undo-stack-before *undo-stack*))
    (unwind-protect
        (progn
          (continue-from-solution goal-form)
          ;; Call WW-SOLVE directly: SOLVE would replace this subgoal with *FINAL-GOAL*.
          (ww-solve)
          (setf completed t)
          (unless *solutions-valid*
            (format t "~&Subgoal produced no solution. Retry from the current state, ~
                       or use (ww-undo) to restore the preceding result.~%"))
          *solution-paths*)
      (unless completed
        (setf *solution-paths* nil
              *solutions-valid* nil)
        (if (eq *undo-stack* undo-stack-before)
          (format t "~&Subgoal solve stopped before the planning state changed.~%")
          (format t "~&Subgoal solve interrupted. Use (ww-undo) to restore the ~
                     preceding result.~%"))))))


(defun install-compiled-goal (goal-form)
  "Install GOAL-FORM and compile the translated GOAL-FN immediately."
  (install-goal goal-form)
  (when (boundp 'goal-fn)
    (compile 'goal-fn (subst-int-code (symbol-value 'goal-fn))))
  goal-form)


(defun continue-from-solution (goal-form)
  "Prepare a subgoal, continuing from the preceding valid solution when present."
  (validate-continuation-preconditions)
  ;; The checkpoint must precede both final-goal capture and start-state replacement.
  (save-undo-checkpoint)
  (unless *final-goal*
    (setf *final-goal* (copy-tree *goal*)))
  (if *solutions-valid*
    (progn
      (update-start-state-from-goal (extract-goal-state-from-solution))
      (install-compiled-goal goal-form)
      (format t "~&Continuing from previous solution...~%"))
    (progn
      (install-compiled-goal goal-form)
      (format t "~&Ready to solve subgoal.~%")))
  ;; The prior solution has now either supplied the baseline or was ineligible.
  (setf *solutions-valid* nil)
  *start-state*)


(defun validate-continuation-preconditions ()
  "Verify that the current planning session can begin a continuation operation."
  (unless (zerop *threads*)
    (error "Goal chaining requires single-threaded mode. ~
            Set (ww-set *threads* 0) before using solve-subgoal."))
  (unless (boundp 'goal-fn)
    (error "No goal function currently defined."))
  (when (and *solutions-valid* (null *solution-paths*))
    (error "*SOLUTIONS-VALID* is true but *SOLUTION-PATHS* is empty."))
  t)


(defun select-continuation-solution (&optional (solutions *solution-paths*))
  "Select SOLUTIONS' canonical member using the search's own preference rule."
  (unless solutions
    (error "No completed solution is available for continuation."))
  (reduce
    (lambda (best candidate)
      (if (solution-better-p candidate best) candidate best))
    solutions))


(defun extract-goal-state-from-solution ()
  "Return the consistent final state of the canonical preceding solution."
  (let ((goal-state
          (solution.goal (select-continuation-solution))))
    (when (state-is-inconsistent goal-state)
      (error "Cannot continue from inconsistent goal state."))
    goal-state))


(defun update-start-state-from-goal (goal-state)
  "Replace *START-STATE* with an independent continuation copy of GOAL-STATE."
  (declare (type problem-state goal-state))
  (let ((continuation (copy-problem-state goal-state)))
    (setf (problem-state.name continuation) 'continuation
          (problem-state.instantiations continuation) nil
          *start-state* continuation)))


(defun save-undo-checkpoint ()
  "Push an independent snapshot of the current goal-chaining session."
  (push
    (make-undo-checkpoint
      :start-state (copy-problem-state *start-state*)
      :goal (copy-tree *goal*)
      :goal-function-bound-p (boundp 'goal-fn)
      :final-goal (copy-tree *final-goal*)
      :solution-paths (copy-solutions-deeply *solution-paths*)
      :solutions-valid *solutions-valid*
      :extension-states (capture-goal-chaining-extension-states))
    *undo-stack*))


(defun restore-checkpoint-goal (checkpoint)
  "Restore both the user goal and the executable GOAL-FN from CHECKPOINT."
  (if (undo-checkpoint-goal-function-bound-p checkpoint)
    (install-compiled-goal (copy-tree (undo-checkpoint-goal checkpoint)))
    (progn
      (setf *goal* (copy-tree (undo-checkpoint-goal checkpoint)))
      (when (boundp 'goal-fn)
        (makunbound 'goal-fn))
      (when (fboundp 'goal-fn)
        (fmakunbound 'goal-fn))
      (remprop 'goal-fn :form))))


(defun ww-undo ()
  "Undo one goal-chaining operation by restoring its complete session snapshot."
  (if (null *undo-stack*)
    (format t "~&Nothing to undo.~%")
    (let ((checkpoint (first *undo-stack*)))
      (setf *start-state* (undo-checkpoint-start-state checkpoint)
            *final-goal* (copy-tree (undo-checkpoint-final-goal checkpoint))
            *solution-paths* (undo-checkpoint-solution-paths checkpoint)
            *solutions-valid* (undo-checkpoint-solutions-valid checkpoint))
      (restore-goal-chaining-extension-states
        (undo-checkpoint-extension-states checkpoint))
      (restore-checkpoint-goal checkpoint)
      (pop *undo-stack*)
      (format t "~&Reverted to previous state.~2%")
      (format t "Current State: ~%~A~%" *start-state*)
      (format t "Current Goal: ~%~A~2%" *goal*)
      t)))

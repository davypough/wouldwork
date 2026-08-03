;;; Filename: -recorder-cycle-boundary.lisp

;;; Recorder-specific continuation policy.  A chained cycle has a stronger termination
;;; contract than a standalone recorder solve: its searched path itself returns every
;;; mapped ghost agent to a recorder.  Report-only return markers therefore never become a
;;; committed boundary by accident.
;;;
;;; Preparing the next cycle operates on a fresh copy of that closed integrated playback
;;; state.  Every stateful shadow component resets its own relations; all resets finish
;;; before any component seeds stateful memory; ordinary propagation then derives the
;;; remaining shadow.  The chosen solution and its report boundary remain immutable.
;;;
;;; REQUIRES:
;;;   nested : -recorder-core (lifecycle registry); -recorder-solution
;;;            (GHOST-STOPS-RECORDER); -propagation
;;; PROVIDES:
;;;   functions : recorder-cycle-goal, prepare-recorder-cycle-state

(include-tech -recorder-core)
(include-tech -recorder-solution)
(include-tech -propagation)

(in-package :ww)


(defun recorder-cycle-goal (subgoal)
  "Return SUBGOAL strengthened with the recorder's physical closure requirement."
  `(and ,(copy-tree subgoal)
        (ghost-stops-recorder)))


(defun recorder-cycle-boundary-closed-p (state)
  "Whether STATE is ready to terminate one cycle and start the next."
  (funcall (symbol-function 'ghost-stops-recorder) state))


(defun reset-recorder-cycle-shadow! (state)
  "Run every capability-owned reset callback against STATE."
  (dolist (lifecycle *recorder-shadow-lifecycles* state)
    (funcall (symbol-function (second lifecycle)) state)))


(defun seed-recorder-cycle-shadow! (state)
  "Run every capability-owned seed callback against STATE."
  (dolist (lifecycle *recorder-shadow-lifecycles* state)
    (when (third lifecycle)
      (funcall (symbol-function (third lifecycle)) state))))


(defun recorder-cycle-state-consistent-p (state)
  "Normalize STATE and report whether propagation converged without inconsistency."
  (and (funcall (symbol-function 'propagate-changes!) state)
       (not (state-is-inconsistent state))))


(defun prepare-recorder-cycle-state (boundary-state)
  "Return a fresh next-cycle state from closed integrated BOUNDARY-STATE.

The original state is never modified.  Preparation fails if the accepted cycle was not
physically closed, shadow propagation is inconsistent, or normalization makes the state no
longer ready to start a recorder cycle."
  (unless (recorder-cycle-boundary-closed-p boundary-state)
    (error "Recorder cycle boundary is open; every mapped ghost agent must be at a recorder."))
  (let ((prepared-state (copy-problem-state boundary-state)))
    (reset-recorder-cycle-shadow! prepared-state)
    (seed-recorder-cycle-shadow! prepared-state)
    (unless (recorder-cycle-state-consistent-p prepared-state)
      (error "Recorder cycle shadow did not propagate to a consistent state."))
    (unless (recorder-cycle-boundary-closed-p prepared-state)
      (error "Recorder cycle preparation moved a ghost agent away from every recorder."))
    prepared-state))

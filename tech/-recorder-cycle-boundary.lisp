;;; Filename: -recorder-cycle-boundary.lisp

;;; Recorder-specific continuation policy.  A chained cycle has a stronger termination
;;; contract than a standalone recorder solve: its searched path itself reaches a normalized
;;; ending, through either normal ghost STOP or live cancellation.  Report-only return
;;; markers therefore never become a committed boundary by accident.
;;;
;;; STOP-RECORDER and CANCEL-PLAYBACK perform the same normalization directly in their
;;; successors: each removes every dynamic ghost reference, resets every capability-owned
;;; recording shadow, seeds stateful memory from the committed live playback baseline, and
;;; propagates consequences.
;;; PREPARE-RECORDER-CYCLE-STATE remains the copy-preserving entry point used by guided
;;; chaining, but delegates to that shared normalization.
;;;
;;; REQUIRES:
;;;   nested : -recorder-core (lifecycle registry); -recorder-solution
;;;            (RECORDER-CYCLE-ENDED); -propagation
;;; PROVIDES:
;;;   queries   : recorder-cycle-boundary-safe, recorder-closed-ghost-free
;;;   functions : recorder-cycle-goal, close-recorder-cycle-state!,
;;;               normalize-recorder-cycle-shadow!, prepare-recorder-cycle-state

(include-tech -recorder-core)
(include-tech -recorder-solution)
(include-tech -propagation)

(in-package :ww)


(defun recorder-object-side (state object)
  "Return OBJECT's mapped recorder side, or NIL when it is fixed or unmapped."
  (when (member object (gethash 'mobile-object *types*))
    (cond
      ((funcall (symbol-function 'live-recording-object) state object) :live)
      ((funcall (symbol-function 'ghost-recording-object) state object) :ghost))))


(defun recorder-value-contains-ghost-p (state value)
  "Whether VALUE, including a nested list value, names a mapped ghost."
  (if (consp value)
    (or (recorder-value-contains-ghost-p state (car value))
        (recorder-value-contains-ghost-p state (cdr value)))
    (eql (recorder-object-side state value) :ghost)))


(defun recorder-state-contains-ghost-reference-p (state)
  "Whether STATE's dynamic database contains any reference to a mapped ghost."
  (some (lambda (proposition)
          (recorder-value-contains-ghost-p state (rest proposition)))
        (list-database (problem-state.idb state))))


(define-query recorder-closed-ghost-free ()
  (not (recorder-state-contains-ghost-reference-p state)))


(defun recorder-cross-layer-arguments-p (state arguments)
  "Whether ARGUMENTS contain both live and ghost mapped objects."
  (and (some (lambda (argument)
               (eql (recorder-object-side state argument) :live))
             arguments)
       (some (lambda (argument)
               (eql (recorder-object-side state argument) :ghost))
             arguments)))


(defun recorder-boundary-reference-relations ()
  "Physical relations whose live/ghost dependencies cannot cross a disappearing boundary."
  ;; A PAIRED link may safely disappear with its ghost endpoint; it neither supports nor
  ;; relocates the surviving live connector.  HOLDING and ON would leave physical state
  ;; undefined, so those dependencies must be resolved before STOP-RECORDER.
  (append '(holding on)
          (copy-list (gethash 'holding *bijective-relations*))
          (copy-list (gethash 'on *bijective-relations*))))


(defun recorder-cross-layer-boundary-reference-p (state)
  "Whether STATE contains a live/ghost support or holding dependency."
  (let ((relations (recorder-boundary-reference-relations)))
    (some (lambda (proposition)
            (and (member (first proposition) relations)
                 (recorder-cross-layer-arguments-p state (rest proposition))))
          (list-database (problem-state.idb state)))))


(defun recorder-cycle-agents-ready-p (state)
  "Whether every mapped ghost agent is at a recorder and empty-handed."
  (every (lambda (agent)
           (or (not (eql (recorder-object-side state agent) :ghost))
               (and (funcall (symbol-function 'recording-agent-at-recorder)
                             state agent)
                    (funcall (symbol-function 'recording-agent-empty-handed)
                             state agent))))
         (gethash 'agent *types*)))


(defun recorder-cycle-boundary-safe-p (state)
  "Whether an open cycle can close without preserving a cross-layer dependency."
  (and (recorder-cycle-agents-ready-p state)
       (not (recorder-cross-layer-boundary-reference-p state))))


(define-query recorder-cycle-boundary-safe ()
  (recorder-cycle-boundary-safe-p state))


(defun recorder-cycle-goal (subgoal)
  "Return SUBGOAL strengthened with the recorder's physical closure requirement."
  `(and ,(copy-tree subgoal)
        (recorder-cycle-ended)))


(defun recorder-cycle-boundary-closed-p (state)
  "Whether STATE is ready to terminate one cycle and start the next."
  (funcall (symbol-function 'recorder-cycle-ended) state))


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


(defun normalize-recorder-cycle-shadow! (state)
  "Reset, seed, and propagate every recording shadow in STATE."
  (reset-recorder-cycle-shadow! state)
  (seed-recorder-cycle-shadow! state)
  (unless (recorder-cycle-state-consistent-p state)
    (add-proposition '(inconsistent-state) (problem-state.idb state))
    (invalidate-problem-state-hash state))
  state)


(defun remove-recorder-ghost-state! (state)
  "Remove every dynamic proposition that refers to a mapped ghost."
  (let ((idb (problem-state.idb state)))
    (dolist (proposition (list-database idb))
      (when (recorder-value-contains-ghost-p state (rest proposition))
        (delete-proposition proposition idb))))
  (invalidate-problem-state-hash state))


(defun close-recorder-cycle-state! (state)
  "Remove ended or cancelled ghosts and normalize the recording view from live state."
  (remove-recorder-ghost-state! state)
  (normalize-recorder-cycle-shadow! state)
  (when (recorder-state-contains-ghost-reference-p state)
    (add-proposition '(inconsistent-state) (problem-state.idb state))
    (invalidate-problem-state-hash state))
  state)


(defun prepare-recorder-cycle-state (boundary-state)
  "Return a fresh next-cycle state from closed integrated BOUNDARY-STATE.

The original state is never modified.  Preparation fails if the accepted cycle was not
explicitly closed, shadow propagation is inconsistent, or normalization retains a dynamic
ghost reference."
  (unless (recorder-cycle-boundary-closed-p boundary-state)
    (error "Recorder cycle boundary was not produced by STOP-RECORDER or CANCEL-PLAYBACK."))
  (let ((prepared-state (copy-problem-state boundary-state)))
    (close-recorder-cycle-state! prepared-state)
    (when (state-is-inconsistent prepared-state)
      (error "Recorder cycle shadow did not propagate to a consistent state."))
    (unless (recorder-cycle-boundary-closed-p prepared-state)
      (error "Recorder cycle preparation did not retain a closed ghost-free boundary."))
    prepared-state))

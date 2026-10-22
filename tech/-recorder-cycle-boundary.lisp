;;; Filename: -recorder-cycle-boundary.lisp

;;; Recorder-specific continuation policy.  A chained cycle has a stronger termination
;;; contract than a standalone recorder solve: its searched path itself reaches a normalized
;;; ending, through either normal ghost STOP or live cancellation.  Report-only return
;;; markers therefore never become a committed boundary by accident.
;;;
;;; STOP-RECORDER and CANCEL-PLAYBACK perform the same normalization directly in their
;;; successors, through the CLOSE-RECORDER-CYCLE-STATE! followup: it removes every dynamic
;;; ghost reference, resets every capability-owned recording shadow, seeds stateful memory
;;; from the committed live playback baseline, and propagates consequences.
;;; START-RECORDER's NORMALIZE-RECORDER-CYCLE-SHADOW! followup does the reset, seed and
;;; propagation alone.  Both are spec updates installed at INIT, so they call every shadow
;;; lifecycle registered by whichever capabilities the problem included, and the symmetry
;;; safety check follows them like any other spec code.
;;; PREPARE-RECORDER-CYCLE-STATE remains the copy-preserving entry point used by guided
;;; chaining, and delegates to the same update.
;;;
;;; REQUIRES:
;;;   nested : -recorder-core (lifecycle registry); -recorder-solution
;;;            (RECORDER-CYCLE-ENDED); -propagation
;;; PROVIDES:
;;;   queries   : recorder-ghost-objects, recorder-closed-ghost-free,
;;;               recorder-cross-layer-pair, recorder-cross-layer-dependency,
;;;               recorder-cycle-agents-ready, recorder-cycle-boundary-safe
;;;   updates   : normalize-recorder-cycle-shadow!, close-recorder-cycle-state!
;;;               (installed at INIT by install-recorder-cycle-updates from the
;;;               registered shadow lifecycle clauses)
;;;   functions : recorder-cycle-goal, recorder-cycle-boundary-closed-p,
;;;               recorder-cycle-state-consistent-p, prepare-recorder-cycle-state

(include-tech -recorder-core)
(include-tech -recorder-solution)
(include-tech -propagation)

(in-package :ww)


(define-query recorder-ghost-objects ()
  ;; Every mapped ghost.  RECORDING-COPY> is static, so the set is fixed for the problem.
  (do (assign $ghosts nil)
      (doall (?object mobile-object)
        (if (ghost-recording-object ?object)
          (push ?object $ghosts)))
      $ghosts))


(define-query recorder-closed-ghost-free ()
  (not (state-mentions-objects-p state (recorder-ghost-objects))))


(define-query recorder-cross-layer-pair (?object1 ?object2)
  ;; One of the two is a mapped live object and the other a mapped ghost.
  (and (mobile-object ?object1)
       (mobile-object ?object2)
       (or (and (live-recording-object ?object1)
                (ghost-recording-object ?object2))
           (and (ghost-recording-object ?object1)
                (live-recording-object ?object2)))))


(define-query recorder-cross-layer-dependency ()
  ;; A live/ghost ON or HOLDING dependency, which a disappearing ghost would leave
  ;; physically undefined, so it must be resolved before STOP-RECORDER.  A PAIRED link may
  ;; safely disappear with its ghost endpoint; it neither supports nor relocates the
  ;; surviving live connector.
  (or (exists (?occupant support-occupant)
        (and (bind (on ?occupant $support))
             (recorder-cross-layer-pair ?occupant $support)))
      (exists (?agent agent)
        (and (bind (holding ?agent $held))
             (recorder-cross-layer-pair ?agent $held)))))


(define-query recorder-cycle-agents-ready ()
  ;; Every mapped ghost agent is at a recorder and empty-handed.
  (forall (?agent agent)
    (or (not (ghost-recording-object ?agent))
        (and (recording-agent-at-recorder ?agent)
             (recording-agent-empty-handed ?agent)))))


(define-query recorder-cycle-boundary-safe ()
  ;; An open cycle can close without preserving a cross-layer dependency.
  (and (recorder-cycle-agents-ready)
       (not (recorder-cross-layer-dependency))))


(define-problem-helper install-recorder-cycle-updates ()
  "Install NORMALIZE-RECORDER-CYCLE-SHADOW! and CLOSE-RECORDER-CYCLE-STATE! as spec code
   splicing in every registered shadow lifecycle clause: all resets, then all seeds, in
   registration order, then propagation.  A propagation that fails to converge marks the
   state inconsistent itself.  Closing first removes every fact naming a mapped ghost, and
   marks the state inconsistent if propagation brings a ghost reference back.  Deferred to
   INIT because capabilities register their lifecycles from files spliced after this one.
   Installed only here, never by DEFINE-UPDATE: a zero-argument technology update is a
   propagation driver candidate, and these are followups that call the driver."
  (install-update 'normalize-recorder-cycle-shadow!
                  nil
                  `(do ,@(mapcar #'second *recorder-shadow-lifecycles*)
                       ,@(remove nil (mapcar #'third *recorder-shadow-lifecycles*))
                       (propagate-changes!)))
  (install-update 'close-recorder-cycle-state!
                  nil
                  '(do (remove-state-facts-mentioning! state (recorder-ghost-objects))
                       (normalize-recorder-cycle-shadow!)
                       (if (not (recorder-closed-ghost-free))
                         (inconsistent-state)))))


(register-deferred-action-installer 'install-recorder-cycle-updates)


(defun recorder-cycle-goal (subgoal)
  "Return SUBGOAL strengthened with the recorder's physical closure requirement."
  `(and ,(copy-tree subgoal)
        (recorder-cycle-ended)))


(defun recorder-cycle-boundary-closed-p (state)
  "Whether STATE is ready to terminate one cycle and start the next."
  (funcall (symbol-function 'recorder-cycle-ended) state))


(defun recorder-cycle-state-consistent-p (state)
  "Propagate STATE and report whether propagation converged without inconsistency."
  (and (funcall (symbol-function 'propagate-changes!) state)
       (not (state-is-inconsistent state))))


(defun prepare-recorder-cycle-state (boundary-state)
  "Return a fresh next-cycle state from closed integrated BOUNDARY-STATE.

The original state is never modified.  Preparation fails if the accepted cycle was not
explicitly closed, shadow propagation is inconsistent, or normalization retains a dynamic
ghost reference."
  (unless (recorder-cycle-boundary-closed-p boundary-state)
    (error "Recorder cycle boundary was not produced by STOP-RECORDER or CANCEL-PLAYBACK."))
  (let ((prepared-state (copy-problem-state boundary-state)))
    (funcall (symbol-function 'close-recorder-cycle-state!) prepared-state)
    (invalidate-problem-state-hash prepared-state)
    (when (state-is-inconsistent prepared-state)
      (error "Recorder cycle shadow did not propagate to a consistent state."))
    (unless (recorder-cycle-boundary-closed-p prepared-state)
      (error "Recorder cycle preparation did not retain a closed ghost-free boundary."))
    prepared-state))

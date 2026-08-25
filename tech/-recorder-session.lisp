;;; Filename: -recorder-session.lisp

;;; Recording session lifecycle.  START-RECORDER, STOP-RECORDER, and CANCEL-PLAYBACK are real planner
;;; actions, not report-only markers: the search itself chooses when a recording session
;;; opens and closes, giving ghost-state forking a well-defined, path-local moment instead
;;; of an inferred one.  START-RECORDER counts the cycle in planner state, clears the
;;; preceding closed marker, and performs the fork required by rules 5 and 11 of
;;; tech/Recorder-ghost operations in the Talos Principle.txt -- every mapped ghost object
;;; inherits its live counterpart's CURRENT has-location, holding, on, paired, jamming, and
;;; mounted-on state at the exact moment recording starts, not whatever -- if anything --
;;; define-init separately declared for it.  The agent operating the recorder must itself
;;; be empty-handed; holdings elsewhere in the mapped state still belong to the complete
;;; snapshot.  RECORDING-IN-PROGRESS also gates ghost
;;; existence itself (see -recorder-core.lisp's OBJECT-MANIPULATION-ALLOWED and
;;; CONNECTOR-PAIRING-ALLOWED): a ghost cannot act, and a live connector cannot reference a
;;; ghost terminus, before this action has run.  That gate is not just faithfulness to rule
;;; 5 -- it guarantees the live-side state START-RECORDER reads can never already contain a
;;; cross-layer reference, so the fork has no such edge case to handle.
;;;
;;; RECORDING-IN-PROGRESS is deliberately not named RECORDING-ACTIVE: that name is already
;;; taken by -recorder-receiver-shadow.lisp's per-receiver relation, which means something
;;; unrelated (recording-side beam power reaching one receiver).
;;;
;;; CONNECTOR, JAMMER, and FAN are redeclared optional here (idempotent and order-
;;; independent, per tech/README.html).  PAIRED, JAMMING, and MOUNTED-ON are not: each is
;;; owned by a technology a recorder problem may or may not include (beam-relay.lisp,
;;; jammer.lisp, -gears-fan.lisp), and a problem may type CONNECTOR or FAN instances
;;; without including that behavioral tech at all -- test/problem-recorder-test.lisp does
;;; exactly this, to exercise RECORDING-COPY>'s own mapping machinery in isolation.  Each
;;; owner therefore contributes its own fork clause through -recorder-fork-registry, and
;;; this file collects whatever was registered.  HAS-LOCATION, HOLDING, and ON need no such
;;; treatment -- they are already unconditionally available in any recorder problem via
;;; -recorder-core, -recorder-solution, and -recorder-controls-shadow's existing nesting.
;;;
;;; START-RECORDER is installed by INIT rather than at this file's splice position, through
;;; REGISTER-DEFERRED-ACTION-INSTALLER.  (include-tech ...) splices textually in whatever
;;; order a problem lists its directives, so anything this file inspects at splice time sees
;;; only the technologies that happen to precede it.  Deferring installation lets the action
;;; see every registered clause, and every type and relation, no matter where a problem
;;; lists (include-tech recorder).  STOP-RECORDER depends on none of that and stays an
;;; ordinary DEFINE-ACTION below.  INSTALL-ACTION is called directly rather than through
;;; DEFINE-ACTION only because the effect is assembled from registered clauses; the macro
;;; just quotes its argument text and calls the same function, so nothing is lost.
;;;
;;; REQUIRES:
;;;   nested : -recorder-cycle-boundary (cycle count, physical closure, ghost removal,
;;;            and shadow normalization); -location (HAS-LOCATION); -holding (HOLDING);
;;;            -support-occupancy (ON); -recorder-fork-registry (optional fork clauses)
;;; CANCEL-PLAYBACK is the live-side early ending.  The empty-handed live agent must reach
;;; a recorder, but the ghost need not have completed its recording or resolved its cargo:
;;; cancellation discards the unplayed remainder and every ghost dependency immediately.
;;;
;;; PROVIDES:
;;;   actions   : start-recorder, stop-recorder, cancel-playback

(include-tech -recorder-cycle-boundary)
(include-tech -location)
(include-tech -holding)
(include-tech -support-occupancy)
(include-tech -recorder-fork-registry)

(in-package :ww)


(define-optional-types recorder connector jammer fan)


(defun install-start-recorder ()
  "Install START-RECORDER, splicing in every fork clause its relation's owner registered.
   Called from INIT rather than at splice position, so the effect sees every technology the
   problem included regardless of the order it listed them in."
  (install-action
    'start-recorder
    1
    '(?agent agent)
    '(and (live-recording-object ?agent)
          (not (recording-in-progress))
          (recording-agent-at-recorder ?agent)
          (recording-agent-empty-handed ?agent)
          (recorder-closed-ghost-free)
          (assign $cycles-used (recorder-cycle-count))
          (assign $objective-value (problem-state.value state))
          (< $cycles-used *max-recorder-cycles*))
    '(">" ?agent "starts the recorder")
    `(assert
       (assign $next-cycle (1+ $cycles-used))
       (recorder-cycles-used $next-cycle)
       (not (recorder-cycle-closed))
       (not (recorder-cycle-stopped-by-ghost))
       (recording-in-progress)
       ;; has-location: every mapped mobile object, wherever it currently stands
       (doall (?live mobile-object)
         (if (bind (recording-copy> ?live $ghost))
           (if (bind (has-location ?live $loc))
             (has-location $ghost $loc))))
       ;; holding: mirror both the holder and whatever it currently holds.  The held object
       ;; is quantified rather than bound, so RECORDING-COPY>'s bijective lookup takes a
       ;; non-$ first argument and resolves live -> ghost at compile time.  Binding the
       ;; cargo into a $-variable instead would leave both of that lookup's arguments
       ;; $-variables, deferring the direction to a runtime test that demands exactly one
       ;; of them bound -- and effect variables outlive a DOALL iteration, so the ghost
       ;; from a preceding iteration would still be bound when the next one arrived.
       (doall (?live mobile-object)
         (doall (?cargo cargo)
           (if (holding ?live ?cargo)
             (if (bind (recording-copy> ?live $ghost))
               (if (bind (recording-copy> ?cargo $cargo-ghost))
                 (holding $ghost $cargo-ghost))))))
       ;; on: the support itself may or may not be a mapped mobile object, and is
       ;; quantified for the same reason as the cargo above.  A support with no ghost keeps
       ;; its live value, which covers every fixed support -- a plate, a gears-mounted fan,
       ;; a fixed blower.
       (doall (?live mobile-object)
         (doall (?support support)
           (if (on ?live ?support)
             (if (bind (recording-copy> ?live $ghost))
               (if (bind (recording-copy> ?support $support-ghost))
                 (on $ghost $support-ghost)
                 (on $ghost ?support))))))
       ,@(mapcar #'cdr *recorder-fork-clauses*)
       (finally (normalize-recorder-cycle-shadow!)))))


(register-deferred-action-installer 'install-start-recorder)


(define-action cancel-playback
  1
  (?agent agent)
  (and (live-recording-object ?agent)
       (recording-in-progress)
       (recording-agent-at-recorder ?agent)
       (recording-agent-empty-handed ?agent)
       (assign $cycles-used (recorder-cycle-count))
       (assign $objective-value (problem-state.value state)))
  (">" ?agent "cancels recorder playback")
  (assert (not (recording-in-progress))
          (recorder-cycles-used $cycles-used)
          (recorder-cycle-closed)
          (not (recorder-cycle-stopped-by-ghost))
          (finally (close-recorder-cycle-state!))))


(define-action stop-recorder
  1
  (?agent agent)
  (and (ghost-recording-object ?agent)
       (recording-in-progress)
       (recorder-cycle-boundary-safe)
       (assign $cycles-used (recorder-cycle-count))
       (assign $objective-value (problem-state.value state)))
  (">" ?agent "stops the recorder")
  (assert (not (recording-in-progress))
          (recorder-cycles-used $cycles-used)
          (recorder-cycle-closed)
          (recorder-cycle-stopped-by-ghost)
          (finally (close-recorder-cycle-state!))))

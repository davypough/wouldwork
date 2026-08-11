;;; Filename: -recorder-session.lisp

;;; Recording session lifecycle.  START-RECORDER and STOP-RECORDER are real planner
;;; actions, not report-only markers: the search itself chooses when a recording session
;;; opens and closes, giving ghost-state forking a well-defined, path-local moment instead
;;; of an inferred one.  START-RECORDER's effect is the fork required by rules 5 and 11 of
;;; tech/Recorder-ghost operations in the Talos Principle.txt -- every mapped ghost object
;;; inherits its live counterpart's CURRENT has-location, holding, on, paired, jamming, and
;;; mounted-on state at the exact moment recording starts, not whatever -- if anything --
;;; define-init separately declared for it.  RECORDING-IN-PROGRESS also gates ghost
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
;;; independent, per tech/README.html) so the PAIRED, JAMMING, and MOUNTED-ON forks compile
;;; for a problem that never includes beam-relay/beam-direct, jammer, or a fan-mounting
;;; technology at all: an empty-domain quantifier never translates a body that would
;;; otherwise reference an undeclared relation.  HAS-LOCATION, HOLDING, and ON need no such
;;; guard -- they are already unconditionally available in any recorder problem via
;;; -recorder-core, -recorder-solution, and -recorder-controls-shadow's existing nesting.
;;;
;;; REQUIRES:
;;;   nested : -recorder-core (RECORDING-COPY>, shadow lifecycle registry);
;;;            -recorder-solution (RECORDING-AGENT-AT-RECORDER, RECORDING-AGENT-EMPTY-HANDED);
;;;            -location (HAS-LOCATION); -holding (HOLDING); -support-occupancy (ON);
;;;            -propagation
;;; PROVIDES:
;;;   relation  : recording-in-progress
;;;   actions   : start-recorder, stop-recorder
;;;   lifecycle : reset-recording-session!

(include-tech -recorder-core)
(include-tech -recorder-solution)
(include-tech -location)
(include-tech -holding)
(include-tech -support-occupancy)
(include-tech -propagation)

(in-package :ww)


(define-optional-types recorder connector jammer fan)


(define-dynamic-relations
  (recording-in-progress))


(define-action start-recorder
  1
  (?agent agent)
  (and (live-recording-object ?agent)
       (not (recording-in-progress))
       (recording-agent-at-recorder ?agent))
  (">" ?agent "starts the recorder")
  (assert
    (recording-in-progress)
    ;; has-location: every mapped mobile object, wherever it currently stands
    (doall (?live mobile-object)
      (if (bind (recording-copy> ?live $ghost))
        (if (bind (has-location ?live $loc))
          (has-location $ghost $loc))))
    ;; holding: mirror both the holder and whatever it currently holds
    (doall (?live mobile-object)
      (if (bind (recording-copy> ?live $ghost))
        (if (bind (holding ?live $cargo))
          (if (bind (recording-copy> $cargo $cargo-ghost))
            (holding $ghost $cargo-ghost)))))
    ;; on: the support itself may or may not be a mapped mobile object
    (doall (?live mobile-object)
      (if (bind (recording-copy> ?live $ghost))
        (if (bind (on ?live $support))
          (if (bind (recording-copy> $support $support-ghost))
            (on $ghost $support-ghost)
            (on $ghost $support)))))
    ;; paired: the terminus may be another connector or shared fixed apparatus
    (doall (?live connector)
      (if (bind (recording-copy> ?live $ghost))
        (if (bind (paired ?live $terminus))
          (if (bind (recording-copy> $terminus $terminus-ghost))
            (paired $ghost $terminus-ghost)
            (paired $ghost $terminus)))))
    ;; jamming: the target (gate/wall-gears/wall-blower) is never itself mapped
    (doall (?live jammer)
      (if (bind (recording-copy> ?live $ghost))
        (if (bind (jamming ?live $target))
          (jamming $ghost $target))))
    ;; mounted-on: the gears are never themselves mapped
    (doall (?live fan)
      (if (bind (recording-copy> ?live $ghost))
        (if (bind (mounted-on ?live $gears))
          (mounted-on $ghost $gears))))
    (finally (propagate-changes!))))


(define-action stop-recorder
  1
  (?agent agent)
  (and (ghost-recording-object ?agent)
       (recording-in-progress)
       (recording-agent-at-recorder ?agent)
       (recording-agent-empty-handed ?agent))
  (">" ?agent "stops the recorder")
  (assert (not (recording-in-progress))
          (finally (propagate-changes!))))


(defun reset-recording-session! (state)
  "Clear the recording-session flag inherited from the preceding cycle."
  (clear-recorder-shadow-relation! state 'recording-in-progress))


(register-recorder-shadow-lifecycle
  'recording-session 'reset-recording-session!)

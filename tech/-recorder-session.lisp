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
;;; independent, per tech/README.html).  PAIRED, JAMMING, and MOUNTED-ON cannot be
;;; redeclared unconditionally the same way: each takes a composite TERMINUS / TARGET /
;;; GEARS argument type that only exists when the relation's real owner (beam-relay.lisp,
;;; jammer.lisp, -gears-fan.lisp) is also included, and a problem may type CONNECTOR or FAN
;;; instances without including that behavioral tech at all -- test/problem-recorder-test.lisp
;;; does exactly this, to exercise RECORDING-COPY>'s own mapping machinery in isolation.  The
;;; empty-domain quantifier guard that protects a relation reference when a TYPE has no
;;; instances does not help here, since CONNECTOR and FAN are not empty in that test.
;;;
;;; So START-RECORDER's relation declaration and its three optional fork clauses are built
;;; below with ordinary Lisp, calling INSTALL-DYNAMIC-RELATIONS and INSTALL-ACTION directly
;;; instead of going through the DEFINE-DYNAMIC-RELATIONS / DEFINE-ACTION macros -- both
;;; macros only quote their argument text and call these same functions, so nothing is lost.
;;; Each composite type's presence in *TYPES* is checked once and reused for both the
;;; relation declaration and the fork clause that depends on it, since a relation and its
;;; own composite argument type are always declared together in the same owning file.
;;; HAS-LOCATION, HOLDING, and ON need no such guard -- they are already unconditionally
;;; available in any recorder problem via -recorder-core, -recorder-solution, and
;;; -recorder-controls-shadow's existing nesting.
;;;
;;; REQUIRES:
;;;   nested : -recorder-core (RECORDING-COPY>, RECORDING-IN-PROGRESS, shadow lifecycle
;;;            registry); -recorder-solution (RECORDING-AGENT-AT-RECORDER,
;;;            RECORDING-AGENT-EMPTY-HANDED); -location (HAS-LOCATION); -holding (HOLDING);
;;;            -support-occupancy (ON); -propagation
;;; PROVIDES:
;;;   relations : paired, jamming, mounted-on -- redeclared, not owned, here, and only when
;;;               their own composite argument type is already present
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


(let* ((paired-p (nth-value 1 (gethash 'terminus *types*)))
       (jamming-p (nth-value 1 (gethash 'target *types*)))
       (mounting-p (nth-value 1 (gethash 'gears *types*)))
       relations
       optional-forks)
  ;; Each composite argument type's presence stands in for its relation's own owning tech
  ;; being included, since a relation and its composite argument type are always declared
  ;; together in the same file (TERMINUS with PAIRED in beam-relay.lisp; TARGET with
  ;; JAMMING in jammer.lisp; GEARS with MOUNTED-ON in -gears-fan.lisp).
  (when paired-p
    (push '(paired connector terminus) relations)
    ;; paired: PAIRED declares no fluent argument -- either side may be a plain connector
    ;; or fixed apparatus -- so BIND cannot extract a terminus the way it does for JAMMING
    ;; and MOUNTED-ON below.  The fork instead walks every stored (connector terminus)
    ;; pair directly.  A connector-to-connector pairing may have been stored with either
    ;; connector first, depending on which one was placed second during the recording, so
    ;; both sides are substituted with their own ghost independently; a side with no ghost
    ;; keeps its live value, which covers shared fixed apparatus and any unmapped connector.
    (push '(doall (?connector connector)
             (doall (?terminus terminus)
               (if (paired ?connector ?terminus)
                 (if (bind (recording-copy> ?connector $connector-ghost))
                   (if (bind (recording-copy> ?terminus $terminus-ghost))
                     (paired $connector-ghost $terminus-ghost)
                     (paired $connector-ghost ?terminus))
                   (if (bind (recording-copy> ?terminus $terminus-ghost))
                     (paired ?connector $terminus-ghost))))))
          optional-forks))
  (when jamming-p
    (push '(jamming jammer $target) relations)
    ;; jamming: the target (gate/wall-gears/wall-blower) is never itself mapped
    (push '(doall (?live jammer)
             (if (bind (recording-copy> ?live $ghost))
               (if (bind (jamming ?live $target))
                 (jamming $ghost $target))))
          optional-forks))
  (when mounting-p
    (push '(mounted-on fan $gears) relations)
    ;; mounted-on: the gears are never themselves mapped
    (push '(doall (?live fan)
             (if (bind (recording-copy> ?live $ghost))
               (if (bind (mounted-on ?live $gears))
                 (mounted-on $ghost $gears))))
          optional-forks))
  (when relations
    (install-dynamic-relations relations))
  (install-action
    'start-recorder
    1
    '(?agent agent)
    '(and (live-recording-object ?agent)
          (not (recording-in-progress))
          (recording-agent-at-recorder ?agent))
    '(">" ?agent "starts the recorder")
    `(assert
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
       ,@optional-forks
       (finally (propagate-changes!)))))


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

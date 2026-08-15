;;; Filename: -recorder-core.lisp

;;; Recorder identity and cross-layer interaction policy.  RECORDING-COPY> explicitly maps
;;; each live mobile object to the ghost that replays it.  The relation is directional and
;;; functional from live to ghost; recorder initialization adds the one-to-one, disjoint,
;;; leaf-category-compatible, and exhaustive loose-cargo invariants.  The relation also
;;; registers its ordered tuples as coupled symmetry rows.
;;;
;;; This file deliberately owns no apparatus shadow state and no propagation update.  It
;;; identifies the recording view, selects which mapped objects existed in that view, and
;;; keeps shared manipulation actions within the appropriate layer.  Capability-specific
;;; state lives in the -recorder-*-shadow components assembled by recorder.lisp.  The core
;;; does own their lifecycle registry: each stateful component registers how its own shadow
;;; is reset and, when necessary, seeded at a chained-cycle boundary.
;;;
;;; REQUIRES:
;;;   nested : -location (mobile-object); -holding (cargo and holding, needed to
;;;            recognize a ghost-held tray); -position (recorder has-position role);
;;;            -interaction-policy (neutral action hooks); -recording-shadow-policy
;;;            (neutral environmental-view hooks)
;;; RECORDING-IN-PROGRESS is declared here, rather than alongside the START-RECORDER /
;;; STOP-RECORDER actions that set it (-recorder-session.lisp), because
;;; OBJECT-MANIPULATION-ALLOWED and CONNECTOR-PAIRING-ALLOWED below need to read it, and
;;; this file is the first component -recorder.lisp assembles.  -recorder-session.lisp
;;; nests this file for the relation, not the other way around.
;;;
;;; PROVIDES:
;;;   type     : recorder, connector, tray (optional)
;;;   relation : recording-copy> (live mobile-object -> ghost mobile-object);
;;;              recording-in-progress, recorder-cycles-used,
;;;              recorder-cycle-closed
;;;   queries  : live-recording-object, ghost-recording-object, same-recording-side;
;;;              recorder-cycle-count;
;;;              overrides recording-shadow-object, recording-shadow-object-present,
;;;              object-manipulation-allowed, support-use-allowed, and
;;;              connector-pairing-allowed
;;;   functions: register-recorder-shadow-lifecycle,
;;;              clear-recorder-shadow-relation!

(include-tech -location)
(include-tech -holding)
(include-tech -position)
(include-tech -interaction-policy)
(include-tech -recording-shadow-policy)

(in-package :ww)


(define-optional-types recorder connector tray)


(define-static-relations
  (recording-copy> mobile-object $mobile-object))


(define-dynamic-relations
  (recording-in-progress)
  (recorder-cycles-used $fixnum)
  (recorder-cycle-closed))


(register-symmetry-coupling 'recording-copy>)


(defvar *recorder-shadow-lifecycles* nil
  "Registered (COMPONENT RESETTER SEEDER) callbacks in component assembly order.")

;; A staged recorder problem reloads this file in the same Lisp image.  Clear registrations
;; from the preceding problem before this recorder assembly registers its own components.
(setf *recorder-shadow-lifecycles* nil)


(defun register-recorder-shadow-lifecycle (component resetter &optional seeder)
  "Register COMPONENT's reset callback and optional seed callback for cycle preparation."
  (when (assoc component *recorder-shadow-lifecycles*)
    (error "Recorder shadow lifecycle registered twice for ~S." component))
  (setf *recorder-shadow-lifecycles*
        (append *recorder-shadow-lifecycles*
                (list (list component resetter seeder))))
  component)


(defun clear-recorder-shadow-relation! (state relation)
  "Remove every proposition for RELATION from STATE's dynamic database."
  (let ((idb (problem-state.idb state)))
    (dolist (proposition (list-database idb))
      (when (eql (first proposition) relation)
        (delete-proposition proposition idb))))
  (invalidate-problem-state-hash state))


(define-query live-recording-object (?object mobile-object)
  (exists (?ghost mobile-object)
    (recording-copy> ?object ?ghost)))


(define-query ghost-recording-object (?object mobile-object)
  (exists (?live mobile-object)
    (recording-copy> ?live ?object)))


(define-query same-recording-side (?object1 mobile-object ?object2 mobile-object)
  (or (and (live-recording-object ?object1)
           (live-recording-object ?object2))
      (and (ghost-recording-object ?object1)
           (ghost-recording-object ?object2))))


(define-query recorder-cycle-count ()
  ;; A pre-counter open state is one already-started legacy cycle.  A closed legacy state
  ;; has used none.  Materializing the count at the next transition makes both forms enter
  ;; the new state model without a compatibility layer outside the planner state.
  (if (bind (recorder-cycles-used $count))
    $count
    (if (recording-in-progress) 1 0)))


(define-query recording-shadow-object (?object)
  (and (mobile-object ?object)
       (ghost-recording-object ?object)))


(define-query recording-shadow-object-present (?object)
  ;; Fixed apparatus and genuinely unmapped objects exist in both views.  An explicit
  ;; closed-cycle marker removes mapped ghosts from the recording view.  Its absence also
  ;; preserves the legacy shadow-only view used by focused capability characterizations
  ;; that do not install the recorder session actions.
  (or (not (mobile-object ?object))
      (and (not (recorder-cycle-closed))
           (ghost-recording-object ?object))
      (and (not (live-recording-object ?object))
           (not (ghost-recording-object ?object)))))


(define-query object-manipulation-allowed (?actor ?object)
  ;; Recorder participants may manipulate only mapped objects on their own side.  A ghost
  ;; does not exist to act until recording is in progress (rule 5); a live actor is
  ;; unrestricted by session timing.
  (and (mobile-object ?actor)
       (mobile-object ?object)
       (same-recording-side ?actor ?object)
       (or (live-recording-object ?actor)
           (recording-in-progress))))


(define-query support-use-allowed (?occupant ?support)
  ;; Fixed supports such as plates are shared environmental apparatus.  Mobile supports
  ;; normally stay on their own recording side.  Rule 19 adds one directional playback
  ;; exception: a live occupant may use a ghost tray while a ghost is actively holding it.
  ;; The reverse dependency remains forbidden because the recorded ghost performance
  ;; cannot rely on a live support introduced during playback.
  (or (not (mobile-object ?support))
      (and (mobile-object ?occupant)
           (same-recording-side ?occupant ?support))
      (and (live-recording-object ?occupant)
           (ghost-recording-object ?support)
           (tray ?support)
           (recording-in-progress)
           (exists (?holder agent)
             (and (ghost-recording-object ?holder)
                  (holding ?holder ?support))))))


(define-query connector-pairing-allowed (?actor ?connector ?terminus)
  ;; Fixed beam apparatus is shared.  During playback a live connector may use either
  ;; layer's connector as a terminus, while a ghost connector may depend only on another
  ;; ghost connector -- never on a live movable connector absent from its recording.  The
  ;; live-to-ghost bridge additionally requires recording to be in progress: a ghost
  ;; terminus does not exist to reference before then (rule 5).  A ghost actor pairing with
  ;; a ghost terminus is already gated by OBJECT-MANIPULATION-ALLOWED above.
  (and (object-manipulation-allowed ?actor ?connector)
       (or (not (connector ?terminus))
           (and (live-recording-object ?actor)
                (or (live-recording-object ?terminus)
                    (and (ghost-recording-object ?terminus)
                         (recording-in-progress))))
           (and (ghost-recording-object ?actor)
                (ghost-recording-object ?terminus)))))

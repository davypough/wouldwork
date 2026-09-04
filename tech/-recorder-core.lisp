;;; Filename: -recorder-core.lisp

;;; Recorder identity and cross-layer interaction policy.  RECORDING-COPY> maps each live
;;; mobile object to the ghost that replays it.  The relation is directional and
;;; one-to-one, with static indexes in both directions; recorder initialization adds the disjoint,
;;; leaf-category-compatible, and exhaustive loose-cargo invariants.  The relation also
;;; registers its ordered tuples as coupled symmetry rows.
;;;
;;; A problem need not state the mapping.  DERIVE-RECORDING-COPY-LITERALS below reads it off
;;; the problem's own DEFINE-TYPES: a mobile object whose name ends in an asterisk is the
;;; recording copy of the object named without it.  The mapping carries no information the
;;; type declarations lack, because INIT-CHECK-RECORDING-CARGO-COMPLETENESS and
;;; INIT-CHECK-RECORDING-LOCATIONS already require every cargo instance and every located
;;; mobile object to sit on one side of a pair -- a problem has no freedom to map less than
;;; all of them.  An explicit RECORDING-COPY> literal remains legal and suppresses derivation
;;; for the objects it names, which is what lets a problem use unstarred ghost names.
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
;;; STOP-RECORDER / CANCEL-PLAYBACK actions that set it (-recorder-session.lisp), because
;;; OBJECT-MANIPULATION-ALLOWED and CONNECTOR-PAIRING-ALLOWED below need to read it, and
;;; this file is the first component -recorder.lisp assembles.  -recorder-session.lisp
;;; nests this file for the relation, not the other way around.
;;;
;;; PROVIDES:
;;;   type     : recorder, connector, tray (optional)
;;;   relation : recording-copy> (indexed live mobile-object -> ghost mobile-object);
;;;              recording-in-progress, recorder-cycles-used,
;;;              recorder-cycle-closed, recorder-cycle-stopped-by-ghost
;;;   generator: derive-recording-copy-literals (asterisk-named ghosts -> recording-copy>)
;;;   queries  : live-recording-object, ghost-recording-object, same-recording-side,
;;;              recording-shadow-view-object;
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
  (recording-copy> $mobile-object $mobile-object :bijective))


(define-dynamic-relations
  (recording-in-progress)
  (recorder-cycles-used $fixnum)
  (recorder-cycle-closed)
  (recorder-cycle-stopped-by-ghost))


(register-symmetry-coupling 'recording-copy>)


(defun derive-recording-copy-literals (literals)
  "Derive (RECORDING-COPY> <live> <live>*) for every mobile object whose name ends in an
   asterisk, so a problem that declares the ghost instance need not also declare the mapping.
   Runs as an initialization literal generator, before CHECK-PROPOSITION and before any
   initialization check, so a derived tuple reaches the relation's bijective indexes and
   every recorder check exactly as an authored one would.  A pair the problem states
   explicitly is left alone: the derived duplicate would collide on RECORDING-COPY>'s
   bijective storage keys, and a problem naming its ghosts some other way -- as the recorder
   characterization problems in test/ do -- must keep stating them.  A trailing asterisk with
   no live counterpart is an authoring error, since every ghost replays something."
  (let ((instances (init-type-instances 'mobile-object))
        (mapped (make-hash-table :test #'eq))
        (derived nil))
    (dolist (literal (positive-init-literals-with-relation 'recording-copy> literals))
      (let ((proposition (init-literal-proposition literal)))
        (setf (gethash (second proposition) mapped) t)
        (setf (gethash (third proposition) mapped) t)))
    (dolist (ghost instances (nreverse derived))
      (let ((name (symbol-name ghost)))
        (when (and (> (length name) 1)
                   (char= (char name (1- (length name))) #\*))
          (let* ((live-name (subseq name 0 (1- (length name))))
                 (live (find-symbol live-name (symbol-package ghost))))
            (unless (member live instances)
              (error "~%A ghost mobile object has no live counterpart.~%~
                      Ghost object: ~S~%~
                      Expected:     ~A~%~
                      A trailing asterisk names the recording copy of the object without ~
                      it, so both must be instances of the same type."
                     ghost live-name))
            (unless (or (gethash ghost mapped)
                        (gethash live mapped))
              (push (list 'recording-copy> live ghost) derived))))))))


(register-init-literal-generator 'derive-recording-copy-literals)


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
  (bind (recording-copy> ?object $ghost)))


(define-query ghost-recording-object (?object mobile-object)
  (bind (recording-copy> $live ?object)))


(define-query recording-shadow-view-object ()
  ;; Environmental shadow queries need a side selector, not every ghost's identity.  Any
  ;; mapped ghost selects the same recording view, so bind one representative once per
  ;; recording-side propagation pass.
  (do (assign $view nil)
      (exists (?live mobile-object)
        (bind (recording-copy> ?live $view)))
      $view))


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
  ;; A reverse mapping exists only for a mobile ghost, so a separate type lookup and
  ;; nested GHOST-RECORDING-OBJECT call would repeat work on this hot path.
  (bind (recording-copy> $live ?object)))


(define-query recording-shadow-object-present (?object)
  ;; Fixed apparatus and genuinely unmapped objects exist in both views.  An explicit
  ;; closed-cycle marker removes mapped ghosts from the recording view.  Its absence also
  ;; preserves the legacy shadow-only view used by focused capability characterizations
  ;; that do not install the recorder session actions.  Classify each mobile object once
  ;; through the two recording-copy indexes instead of repeating live/ghost query calls.
  (or (not (mobile-object ?object))
      (if (bind (recording-copy> $live ?object))
        (not (recorder-cycle-closed))
        (not (bind (recording-copy> ?object $ghost))))))


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


(define-query connector-location-conflict-p (?connector ?other)
  ;; A lit connector blocks another connector only within the same recorder layer.  Live
  ;; and ghost copies may occupy the same physical location because their mobile supports,
  ;; manipulation, and pairings are independently isolated above.
  (same-recording-side ?connector ?other))

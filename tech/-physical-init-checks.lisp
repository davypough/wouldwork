;;; Filename: -physical-init-checks.lisp

;;; Initialization validation for shared physical placement facts and live-cargo presence.
;;;
;;; A support top holds at most one occupant per recorder layer, not at most one occupant:
;;; the live and ghost worlds are superimposed during playback, so a live object and a
;;; ghost object may rest on the same support.  With no recorder in the problem nothing is
;;; on either layer, and the check reduces to the familiar single-occupant rule.  The
;;; RECORDING-COPY> literals are already complete here -- DERIVE-RECORDING-COPY-LITERALS
;;; runs as an initialization literal generator, ahead of every init check -- so
;;; asterisk-named ghosts are classified alongside authored ones.


(in-package :ww)


(define-init-check physical-state-init-check (literals)
  (check-init-object-placement-consistency literals))


(define-init-check cargo-physical-state-init-check (literals)
  (check-init-live-cargo-physical-state literals))


(define-init-check-helper physical-placement-impossible-p (state)
  "Predecessor-leaf screen: true when STATE fails the DEFINE-INIT placement check, eg a
   support cycle or a held tray under its own holder.  Such a guessed predecessor is
   physically impossible, and propagating it can recurse without end in TOP."
  (handler-case
      (progn
        (check-init-object-placement-consistency (state-placement-literals state))
        nil)
    (init-check-failure () t)))


(define-init-check-helper state-placement-literals (state)
  "STATE's dynamic facts in DEFINE-INIT form, plus the static HAS-POSITION and
   RECORDING-COPY> facts the placement check reads.  A bijective relation's two index
   facts become one fact under the relation's own name."
  (let ((literals nil))
    (dolist (proposition (list-database (problem-state.idb state)))
      (let ((canonical (gethash (car proposition) *bijective-canonical*)))
        (cond ((null canonical)
               (push proposition literals))
              ((eql (cdr canonical) 1)
               (push (cons (car canonical) (cdr proposition)) literals)))))
    (dolist (relation '(has-position recording-copy>) literals)
      (when (gethash relation *static-relations*)
        (let ((index-name (or (first (gethash relation *bijective-relations*)) relation)))
          (dolist (object (expand-type-spec-instances*
                            (init-relation-argument-type relation 1)))
            (let ((value (gethash (list index-name object) *static-db*)))
              (when value
                (push (list* relation object value) literals)))))))))


(register-predecessor-leaf-screen 'physical-placement-impossible-p)


(define-init-check-helper init-object-location (object locations positions)
  (or (gethash object locations)
      (gethash object positions)))


(define-init-check-helper init-location-valued-relation-p (relation)
  (init-type-spec-includes-type-p
    (init-relation-argument-type relation 2)
    'location))


(define-init-check-helper init-relation-can-locate-object-p (relation object)
  (and (init-location-valued-relation-p relation)
       (init-type-spec-member-p
         object
         (init-relation-argument-type relation 1))))


(define-init-check-helper init-on-location-consistency-required-p (object support)
  (and (init-relation-can-locate-object-p 'has-location object)
       (or (init-relation-can-locate-object-p 'has-location support)
           (init-relation-can-locate-object-p 'has-position support))))


(define-init-check-helper init-binary-on-literals (literals)
  (remove-if-not (lambda (literal)
                   (= (length (rest (init-literal-proposition literal))) 2))
                 (positive-init-literals-with-relation 'on literals)))


(define-init-check-helper init-held-objects (literals)
  (let ((held-objects (make-hash-table :test #'equal)))
    (dolist (literal (positive-init-literals-with-relation 'holding literals)
                     held-objects)
      (setf (gethash (third (init-literal-proposition literal)) held-objects) t))))


(define-init-check-helper init-mounted-objects (literals)
  (let ((mounted-objects (make-hash-table :test #'equal)))
    (dolist (literal (positive-init-literals-with-relation 'mounted-on literals)
                     mounted-objects)
      (setf (gethash (second (init-literal-proposition literal)) mounted-objects) t))))


(define-init-check-helper init-recording-ghost-objects (literals)
  (let ((ghost-objects (make-hash-table :test #'equal)))
    (dolist (literal (positive-init-literals-with-relation 'recording-copy> literals)
                     ghost-objects)
      (setf (gethash (third (init-literal-proposition literal)) ghost-objects) t))))


(define-init-check-helper init-recording-live-objects (literals)
  (let ((live-objects (make-hash-table :test #'equal)))
    (dolist (literal (positive-init-literals-with-relation 'recording-copy> literals)
                     live-objects)
      (setf (gethash (second (init-literal-proposition literal)) live-objects) t))))


(define-init-check-helper check-init-live-cargo-physical-state (literals)
  "Require every live cargo object to have one physical-state source."
  ;; The foundational -LOCATION and -HOLDING roles can be staged independently for
  ;; relation characterization.  Completeness becomes meaningful only when both are
  ;; installed.  A recorder ghost has no physical state until START-RECORDER forks it;
  ;; MOUNTED-ON accounts for a wall-mounted fan, whose drive owns its position.
  (when (and (init-relation-signature 'has-location)
             (init-relation-signature 'holding))
    (let ((locations (init-literal-map 'has-location literals 1 2))
          (held-objects (init-held-objects literals))
          (mounted-objects (init-mounted-objects literals))
          (ghost-objects (init-recording-ghost-objects literals)))
      (dolist (object (init-type-instances 'cargo))
        (unless (or (gethash object locations)
                    (gethash object held-objects)
                    (gethash object mounted-objects)
                    (gethash object ghost-objects))
          (fail-init-check nil "~%DEFINE-INIT gives live cargo no physical state.~%~
                  Object: ~S~%~
                  Add HAS-LOCATION, HOLDING, or MOUNTED-ON.  Only the ghost endpoint ~
                  of RECORDING-COPY> may begin absent."
                 object))))))


(define-init-check-helper init-check-tray-support-held
    (literal support held-objects)
  (when (and (init-type-member-p support 'tray)
             (not (gethash support held-objects)))
    (fail-init-check nil "~%DEFINE-INIT places an object on an unheld tray.~%~
            Literal: ~S~%~
            Tray:    ~S"
           literal support)))


(define-init-check-helper init-check-object-not-held-and-has-location (literals locations)
  ;; A tray is the one deviation: it keeps its has-location fact even while held (synced
  ;; to its holder's location), so a support-occupant resting on it keeps resolving a
  ;; location through the ordinary consumers.  Every other held cargo type must still
  ;; have no has-location.
  (dolist (literal (init-literals-with-relation 'holding literals))
    (destructuring-bind (agent object)
        (rest (init-literal-proposition literal))
      (declare (ignore agent))
      (when (and (gethash object locations)
                 (not (init-type-member-p object 'tray)))
        (fail-init-check nil "~%DEFINE-INIT object is both held and assigned HAS-LOCATION.~%~
                Literal: ~S~%~
                Object:  ~S"
               literal object)))))


(define-init-check-helper init-check-held-tray-location-consistency (literals locations)
  "Require a held tray's retained location to match its holder's location."
  (dolist (literal (positive-init-literals-with-relation 'holding literals))
    (destructuring-bind (holder object)
        (rest (init-literal-proposition literal))
      (when (init-type-member-p object 'tray)
        (let ((holder-location (gethash holder locations))
              (tray-location (gethash object locations)))
          (unless holder-location
            (fail-init-check
              literal
              "~%DEFINE-INIT gives a tray to a holder with no HAS-LOCATION.~%~
               Holder: ~S~%Tray:   ~S"
              holder object))
          (unless tray-location
            (fail-init-check
              literal
              "~%DEFINE-INIT held tray has no HAS-LOCATION.~%~
               Holder: ~S at ~S~%Tray:   ~S"
              holder holder-location object))
          (unless (eql holder-location tray-location)
            (fail-init-check
              literal
              "~%DEFINE-INIT held tray location does not match its holder location.~%~
               Holder: ~S at ~S~%Tray:   ~S at ~S"
              holder holder-location object tray-location)))))))


(define-init-check-helper init-check-held-object-not-on
    (literal object support held-objects)
  (when (gethash object held-objects)
    (fail-init-check
      literal
      "~%DEFINE-INIT object is both held and resting ON a support.~%~
       Object:  ~S~%Support: ~S"
      object support)))


(define-init-check-helper init-check-held-tray-does-not-support-holder
    (literal holder tray on-map)
  "Reject a held tray anywhere below its own holder's ON chain."
  (let ((current holder))
    (loop
      (let ((support (gethash current on-map)))
        (unless support
          (return))
        (when (eql support tray)
          (fail-init-check
            literal
            "~%DEFINE-INIT held tray supports its own holder.~%~
             Holder: ~S~%Tray:   ~S"
            holder tray))
        (setf current support)))))


(define-init-check-helper init-check-held-tray-support-cycles (literals on-map)
  (dolist (literal (positive-init-literals-with-relation 'holding literals))
    (destructuring-bind (holder object)
        (rest (init-literal-proposition literal))
      (when (init-type-member-p object 'tray)
        (init-check-held-tray-does-not-support-holder
          literal holder object on-map)))))


(define-init-check-helper init-occupants-may-share-p (object other live-objects ghost-objects)
  "True when OBJECT and OTHER sit on opposite recorder layers.  That is the one case in
   which two occupants share a support top: playback superimposes the live and ghost
   worlds rather than stacking them.  An object on neither layer contends with everything,
   which keeps the ordinary single-occupant rule for problems with no recorder."
  (or (and (gethash object live-objects)
           (gethash other ghost-objects))
      (and (gethash object ghost-objects)
           (gethash other live-objects))))


(define-init-check-helper init-check-support-occupants-compatible
    (literal object support support-occupants live-objects ghost-objects)
  (dolist (occupant (gethash support support-occupants))
    (unless (init-occupants-may-share-p object occupant live-objects ghost-objects)
      (fail-init-check nil "~%DEFINE-INIT places contending objects on the same support.~%~
              Literal:          ~S~%~
              Existing object:  ~S~%~
              New object:       ~S~%~
              Support:          ~S~%~
              Only a live object and its recorder ghost may share a support top."
             literal occupant object support)))
  (push object (gethash support support-occupants)))


(define-init-check-helper init-check-on-location-consistency
    (literal object support locations positions)
  (let ((object-location (gethash object locations))
        (support-location (init-object-location support locations positions)))
    (unless object-location
      (fail-init-check nil "~%DEFINE-INIT places an object on a support, but the object has no HAS-LOCATION.~%~
              Literal: ~S~%~
              Object:  ~S"
             literal object))
    (unless support-location
      (fail-init-check nil "~%DEFINE-INIT places an object on a support with no HAS-LOCATION or HAS-POSITION.~%~
              Literal: ~S~%~
              Support: ~S"
             literal support))
    (unless (eql object-location support-location)
      (fail-init-check nil "~%DEFINE-INIT object location does not match support location.~%~
              Literal:          ~S~%~
              Object location:  ~S~%~
              Support location: ~S"
             literal object-location support-location))))


(define-init-check-helper init-check-on-cycle (literal object on-map)
  (let ((seen nil)
        (current object))
    (loop
      (when (member current seen)
        (fail-init-check nil "~%DEFINE-INIT contains an ON cycle.~%~
                Literal: ~S~%~
                Cycle includes: ~S"
               literal current))
      (push current seen)
      (let ((support (gethash current on-map)))
        (unless support
          (return))
        (when (eql support current)
          (fail-init-check nil "~%DEFINE-INIT places an object on itself.~%~
                  Literal: ~S~%~
                  Object:  ~S"
                 literal current))
        (setf current support)))))


(define-init-check-helper check-init-object-placement-consistency (literals)
  "Checks physical consistency of HAS-LOCATION, HOLDING, ON, and HAS-POSITION facts."
  (let ((locations (init-literal-map 'has-location literals 1 2))
        (positions (init-literal-map 'has-position literals 1 2))
        (on-map (init-literal-map 'on literals 1 2))
        (held-objects (init-held-objects literals))
        (live-objects (init-recording-live-objects literals))
        (ghost-objects (init-recording-ghost-objects literals))
        (support-occupants (make-hash-table :test #'equal)))
    (init-check-object-not-held-and-has-location literals locations)
    (init-check-held-tray-location-consistency literals locations)
    (dolist (literal (init-binary-on-literals literals))
      (destructuring-bind (object support)
          (rest (init-literal-proposition literal))
        (let ((location-consistency-required-p
                (init-on-location-consistency-required-p object support)))
          (when (eql object support)
            (fail-init-check nil "~%DEFINE-INIT places an object on itself.~%~
                    Literal: ~S~%~
                    Object:  ~S"
                   literal object))
          (init-check-held-object-not-on
            literal object support held-objects)
          (init-check-tray-support-held literal support held-objects)
          (when location-consistency-required-p
            (init-check-support-occupants-compatible
              literal object support support-occupants live-objects ghost-objects)
            (init-check-on-location-consistency
              literal object support locations positions)))
        (init-check-on-cycle literal object on-map)))
    (init-check-held-tray-support-cycles literals on-map)))

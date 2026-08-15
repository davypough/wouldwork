;;; Filename: -physical-init-checks.lisp

;;; Initialization validation for shared physical placement facts.


(in-package :ww)


(define-init-check physical-state-init-check (literals)
  (check-init-object-placement-consistency literals))


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


(define-init-check-helper init-check-support-has-one-object (literal object support support-occupants)
  (let ((occupant (gethash support support-occupants)))
    (when occupant
      (fail-init-check nil "~%DEFINE-INIT places multiple objects on the same support.~%~
              Literal:          ~S~%~
              Existing object:  ~S~%~
              New object:       ~S~%~
              Support:          ~S"
             literal occupant object support))
    (setf (gethash support support-occupants) object)))


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
        (support-occupants (make-hash-table :test #'equal)))
    (init-check-object-not-held-and-has-location literals locations)
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
          (init-check-tray-support-held literal support held-objects)
          (when location-consistency-required-p
            (init-check-support-has-one-object
              literal object support support-occupants)
            (init-check-on-location-consistency
              literal object support locations positions)))
        (init-check-on-cycle literal object on-map)))))


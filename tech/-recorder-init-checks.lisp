;;; Filename: -recorder-init-checks.lisp

;;; Initialization validation for -recorder-core identity, recording-layer isolation, and
;;; the explicitly supported set of shadow components assembled by recorder.lisp.


(in-package :ww)


(define-init-check recorder-init-check (literals)
  (check-init-recorder-consistency literals)
  (init-check-recorder-supported-scope))


(define-init-check-helper check-init-recorder-consistency (literals)
  "Checks recorder mappings and cross-layer initial interactions."
  (let ((live-objects (make-hash-table :test #'equal))
        (ghost-objects (make-hash-table :test #'equal)))
    (dolist (literal
              (positive-init-literals-with-relation 'recording-copy> literals))
      (destructuring-bind (live ghost)
          (rest (init-literal-proposition literal))
        (when (eql live ghost)
          (fail-init-check nil "~%RECORDING-COPY> maps an object to itself.~%~
                  Literal: ~S~%~
                  Object:  ~S"
                 literal live))
        (when (gethash live live-objects)
          (fail-init-check nil "~%RECORDING-COPY> repeats a live object.~%~
                  Literal:    ~S~%~
                  Live object: ~S"
                 literal live))
        (when (gethash ghost ghost-objects)
          (fail-init-check nil "~%RECORDING-COPY> repeats a ghost object.~%~
                  Literal:     ~S~%~
                  Ghost object: ~S"
                 literal ghost))
        (when (or (gethash live ghost-objects)
                  (gethash ghost live-objects))
          (fail-init-check nil "~%RECORDING-COPY> uses an object on both live and ghost sides.~%~
                  Literal:     ~S~%~
                  Live object:  ~S~%~
                  Ghost object: ~S"
                 literal live ghost))
        (unless (init-recording-copy-compatible-p live ghost)
          (fail-init-check nil "~%RECORDING-COPY> connects incompatible object categories.~%~
                  Literal:     ~S~%~
                  Live object:  ~S~%~
                  Ghost object: ~S"
                 literal live ghost))
        (setf (gethash live live-objects) t)
        (setf (gethash ghost ghost-objects) t)))
    (when (init-relation-signature 'recording-copy>)
      (init-check-recording-locations literals live-objects ghost-objects)
      (init-check-recording-holdings literals live-objects ghost-objects)
      (init-check-recording-supports literals live-objects ghost-objects)
      (init-check-recording-pairings literals live-objects ghost-objects)
      (init-check-recording-jamming-facts literals live-objects ghost-objects)
      (init-check-recording-mapped-wall-fans live-objects ghost-objects)
      (init-check-recording-wall-gears-controls literals))))


(define-init-check-helper init-recording-copy-compatible-p (live ghost)
  (some (lambda (category)
          (and (init-type-member-p live category)
               (init-type-member-p ghost category)))
        (init-type-components 'mobile-object)))


(define-init-check-helper init-recording-side (object live-objects ghost-objects)
  (cond ((gethash object live-objects) 'live)
        ((gethash object ghost-objects) 'ghost)))


(define-init-check-helper init-same-recording-side-p (object1 object2 live-objects ghost-objects)
  (let ((side1 (init-recording-side object1 live-objects ghost-objects))
        (side2 (init-recording-side object2 live-objects ghost-objects)))
    (and side1 (eql side1 side2))))


(define-init-check-helper init-check-recording-locations
    (literals live-objects ghost-objects)
  "Requires every located mobile object to have an explicit recording side."
  (dolist (literal (positive-init-literals-with-relation 'has-location literals))
    (destructuring-bind (object location)
        (rest (init-literal-proposition literal))
      (declare (ignore location))
      (when (and (init-type-member-p object 'mobile-object)
                 (not (init-recording-side object live-objects ghost-objects)))
        (fail-init-check nil "~%HAS-LOCATION gives an unmapped mobile object physical state.~%~
                Literal: ~S~%~
                Object:  ~S~%~
                Every located mobile object in a recorder problem needs a RECORDING-COPY> pair."
               literal object)))))


(define-init-check-helper init-check-recording-holdings (literals live-objects ghost-objects)
  (dolist (literal (positive-init-literals-with-relation 'holding literals))
    (destructuring-bind (agent object)
        (rest (init-literal-proposition literal))
      (unless (init-same-recording-side-p
                agent object live-objects ghost-objects)
        (fail-init-check nil "~%HOLDING crosses recording layers or uses an unmapped object.~%~
                Literal: ~S~%~
                Agent:   ~S~%~
                Object:  ~S"
               literal agent object)))))


(define-init-check-helper init-check-recording-supports (literals live-objects ghost-objects)
  (dolist (literal (init-binary-on-literals literals))
    (destructuring-bind (occupant support)
        (rest (init-literal-proposition literal))
      (when (and (init-type-member-p support 'mobile-object)
                 (not (init-same-recording-side-p
                        occupant support live-objects ghost-objects)))
        (fail-init-check nil "~%ON crosses recording layers or uses an unmapped mobile support.~%~
                Literal:  ~S~%~
                Occupant: ~S~%~
                Support:  ~S"
               literal occupant support)))))


(define-init-check-helper init-check-recording-pairings (literals live-objects ghost-objects)
  (dolist (literal (positive-init-literals-with-relation 'paired literals))
    (destructuring-bind (connector terminus)
        (rest (init-literal-proposition literal))
      (let ((connector-side
              (init-recording-side connector live-objects ghost-objects))
            (terminus-side
              (init-recording-side terminus live-objects ghost-objects)))
        (unless (and connector-side
                     (or (not (init-type-member-p terminus 'connector))
                         (and terminus-side
                              (or (eql connector-side 'live)
                                  (and (eql connector-side 'ghost)
                                       (eql terminus-side 'ghost))))))
          (fail-init-check nil "~%PAIRED violates recorder connector isolation.~%~
                  Literal:  ~S~%~
                  Connector: ~S~%~
                  Terminus:  ~S"
                 literal connector terminus))))))


(define-init-check-helper init-check-recording-mapped-wall-fans
    (live-objects ghost-objects)
  "Rejects movable fan copies while wall-fan presence is not recording-view aware."
  (when (and (init-relation-signature 'mounted-on)
             (init-type-instances 'wall-gears))
    (dolist (fan (init-type-instances 'fan))
      (when (or (gethash fan live-objects)
                (gethash fan ghost-objects))
        (fail-init-check nil "~%Recorder technology does not yet support mapped wall-fan mounting.~%~
                Fan: ~S"
               fan)))))


(define-init-check-helper init-check-recording-wall-gears-controls (literals)
  "Rejects control sources that Stage 3's recording shadow cannot derive."
  (when (init-dnf-controls-relation-p)
    (dolist (literal (positive-init-literals-with-relation 'controls literals))
      (destructuring-bind (clauses controlled-object mode)
          (rest (init-literal-proposition literal))
        (declare (ignore mode))
        (when (init-type-member-p controlled-object 'wall-gears)
          (dolist (clause clauses)
            (dolist (controller clause)
              (unless (init-type-member-p controller 'plate)
                (fail-init-check nil "~%Recording-side wall-gears controls support only plates.~%~
                        Literal:          ~S~%~
                        Wall gears:       ~S~%~
                        Unsupported item: ~S"
                       literal controlled-object controller)))))))))


(define-init-check-helper init-check-recording-jamming-facts
    (literals live-objects ghost-objects)
  "Requires every initially active jammer to have an explicit recording side."
  (dolist (literal (positive-init-literals-with-relation 'jamming literals))
    (destructuring-bind (jammer target)
        (rest (init-literal-proposition literal))
      (declare (ignore target))
      (unless (init-recording-side jammer live-objects ghost-objects)
        (fail-init-check nil "~%JAMMING uses an unmapped jammer in a recorder problem.~%~
                Literal: ~S~%~
                Jammer:  ~S~%~
                Every active jammer needs a RECORDING-COPY> pair."
               literal jammer)))))


(define-init-check-helper init-check-recorder-unsupported-type (type capability)
  "Rejects instances of TYPE while CAPABILITY has no recording-side model."
  (let ((instances (init-type-instances type)))
    (when instances
      (fail-init-check nil "~%Recorder technology does not yet support ~A.~%~
              Type:   ~S~%~
              Object: ~S"
             capability type (first instances)))))


(define-init-check-helper init-check-recording-threats ()
  "Rejects threats until lethal state and safety are recording-view aware."
  (if (init-type-instances 'threat)
    (init-check-recorder-unsupported-type 'threat "recording-side threats")
    (init-check-recorder-unsupported-type 'gun "recording-side threats")))


(define-init-check-helper init-check-recording-beam-crossings ()
  "Rejects the beam-crossing peer until crossing and cut state have a recording view."
  (when (init-relation-signature 'crossings-along-beam>)
    (fail-init-check nil
      "Recorder technology does not yet support recording-side beam crossings.")))


(define-init-check-helper init-check-recorder-supported-scope ()
  "Rejects installed objects and capabilities outside the recorder shadow."
  (init-check-recorder-unsupported-type 'floor-gears "recording-side floor blowers")
  (init-check-recorder-unsupported-type 'angled-gears "recording-side angled blowers")
  (init-check-recording-threats)
  (init-check-recording-beam-crossings))

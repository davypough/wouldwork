;;; Filename: -stream-passability-init-checks.lisp

;;; Initialization validation for coordinate-derived air streams.


(in-package :ww)


(define-init-check stream-passability-init-check (literals)
  (check-init-stream-consistency literals))


(define-init-check-helper check-init-stream-consistency (literals)
  "In a coordinate-driven problem (WALL-SEGMENTS or BOUNDARY-WALL asserted), every
   declared wall-gears' air stream must be derivable: a positioned HAS-POSITION swept
   location and AIMED-AT destination sharing an axis coordinate and not coincident.
   Any STREAM-WIDTH override must name a wall-gears and give a positive width."
  (when (or (positive-init-literals-with-relation 'wall-segments literals)
            (positive-init-literals-with-relation 'boundary-wall literals))
    (let ((positions (init-location-coords-map literals)))
      (dolist (gears (init-type-instances 'wall-gears))
        (init-check-stream-derivable gears positions literals))))
  (dolist (literal (init-literals-with-relation 'stream-width literals))
    (destructuring-bind (gears width)
        (rest (init-literal-proposition literal))
      (unless (init-type-member-p gears 'wall-gears)
        (fail-init-check nil "~%STREAM-WIDTH names ~S, which is not a declared wall-gears instance.~%~
                Literal: ~S"
               gears (init-literal-proposition literal)))
      (unless (and (rationalp width) (plusp width))
        (fail-init-check nil "~%STREAM-WIDTH of ~S must be a positive rational.~%~
                Literal: ~S"
               gears (init-literal-proposition literal))))))


(define-init-check-helper init-check-stream-derivable (gears positions literals)
  (let* ((swept (init-gears-related-location 'has-position gears literals))
         (destination (init-gears-related-location 'aimed-at gears literals))
         (swept-point (and swept (gethash swept positions)))
         (destination-point (and destination (gethash destination positions))))
    (unless swept-point
      (fail-init-check nil "~%Wall-gears in a coordinate-driven problem has no positioned swept location.~%~
              ~S needs a HAS-POSITION location with LOCATION-COORDS> coordinates."
             gears))
    (unless destination-point
      (fail-init-check nil "~%Wall-gears in a coordinate-driven problem has no positioned destination.~%~
              ~S needs an AIMED-AT destination with LOCATION-COORDS> coordinates."
             gears))
    (when (equal swept-point destination-point)
      (fail-init-check nil "~%The air stream of ~S has coincident swept location and destination.~%~
              Swept location: ~S at ~S~%~
              Destination:    ~S at ~S"
             gears swept swept-point destination destination-point))
    (unless (or (= (first swept-point) (first destination-point))
                (= (second swept-point) (second destination-point)))
      (fail-init-check nil "~%The air stream of ~S is not axis-aligned.~%~
              Swept location: ~S at ~S~%~
              Destination:    ~S at ~S~%~
              The swept location and destination must share an X or Y coordinate."
             gears swept swept-point destination destination-point))))


(define-init-check-helper init-location-coords-map (literals)
  (let ((map (make-hash-table :test #'eql)))
    (dolist (literal (init-literals-with-relation 'location-coords> literals))
      (destructuring-bind (location x y)
          (rest (init-literal-proposition literal))
        (setf (gethash location map) (list x y))))
    map))


(define-init-check-helper init-gears-related-location (relation gears literals)
  (loop for literal in (init-literals-with-relation relation literals)
        when (eql (second (init-literal-proposition literal)) gears)
          return (third (init-literal-proposition literal))))



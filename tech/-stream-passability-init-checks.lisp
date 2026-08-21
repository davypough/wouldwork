;;; Filename: -stream-passability-init-checks.lisp

;;; Initialization validation for coordinate-derived air streams.


(in-package :ww)


(define-init-check stream-passability-init-check (literals)
  (check-init-stream-consistency literals))


(define-init-check-helper check-init-stream-consistency (literals)
  "In a coordinate-driven problem (WALL-SEGMENT>, EDGE-SEGMENT>, or BOUNDARY-WALL
   asserted), every declared wall-gears or wall-blower stream must be derivable: a positioned
   HAS-POSITION swept location and AIMED-AT destination.  Whenever both endpoint coordinates
   are known, they must be distinct and axis-aligned.  Endpoint levels, defaulting to zero
   when unauthored, must match because the stream is horizontal, and the stream elevation
   must be strictly above that floor.  Any STREAM-WIDTH override must name a
   wall-gears or wall-blower and give a positive width."
  (let ((coordinate-driven-p
          (or (positive-init-literals-with-relation 'wall-segment> literals)
              (positive-init-literals-with-relation 'edge-segment> literals)
              (positive-init-literals-with-relation 'boundary-wall literals)))
        (positions (init-location-xy-map literals))
        (levels (init-location-level-map literals)))
    (dolist (drive
              (append (init-type-instances 'wall-gears)
                      (init-type-instances 'wall-blower)))
      (init-check-wall-stream-geometry
        drive positions levels literals coordinate-driven-p)))
  (dolist (literal (init-literals-with-relation 'stream-width literals))
    (destructuring-bind (gears width)
        (rest (init-literal-proposition literal))
      (unless (or (init-type-member-p gears 'wall-gears)
                  (init-type-member-p gears 'wall-blower))
        (fail-init-check nil "~%STREAM-WIDTH names ~S, which is not a declared wall-gears or wall-blower instance.~%~
                Literal: ~S"
               gears (init-literal-proposition literal)))
      (unless (and (rationalp width) (plusp width))
        (fail-init-check nil "~%STREAM-WIDTH of ~S must be a positive rational.~%~
                Literal: ~S"
               gears (init-literal-proposition literal))))))


(define-init-check-helper init-check-wall-stream-geometry
    (drive positions levels literals coordinate-driven-p)
  (let* ((swept (init-blower-drive-related-location 'has-position drive literals))
         (destination (init-blower-drive-related-location 'aimed-at drive literals))
         (swept-point (and swept (gethash swept positions)))
         (destination-point (and destination (gethash destination positions))))
    (when (and coordinate-driven-p (not swept-point))
      (fail-init-check nil "~%Wall-gears in a coordinate-driven problem has no positioned swept location.~%~
              ~S needs a HAS-POSITION location with LOCATION-COORDS> coordinates."
             drive))
    (when (and coordinate-driven-p (not destination-point))
      (fail-init-check nil "~%Wall-gears in a coordinate-driven problem has no positioned destination.~%~
              ~S needs an AIMED-AT destination with LOCATION-COORDS> coordinates."
             drive))
    (when (and swept-point destination-point
               (equal swept-point destination-point))
      (fail-init-check nil "~%The air stream of ~S has coincident swept location and destination.~%~
              Swept location: ~S at ~S~%~
              Destination:    ~S at ~S"
             drive swept swept-point destination destination-point))
    (when (and swept-point destination-point
               (/= (first swept-point) (first destination-point))
               (/= (second swept-point) (second destination-point)))
      (fail-init-check nil "~%The air stream of ~S is not axis-aligned.~%~
              Swept location: ~S at ~S~%~
              Destination:    ~S at ~S~%~
              The swept location and destination must share an X or Y coordinate."
             drive swept swept-point destination destination-point))
    (let ((swept-level (gethash swept levels 0))
          (destination-level (gethash destination levels 0)))
      (when (/= swept-level destination-level)
        (fail-init-check
          nil
          "~%The horizontal air stream of ~S connects locations at different levels.~%~
           Swept location: ~S at level ~S~%~
           Destination:    ~S at level ~S~%~
           Give both endpoint locations the same level."
          drive swept swept-level destination destination-level))
      (let ((stream-literal
              (init-blower-drive-relation-literal 'has-elevation drive literals)))
        (let ((stream-level
                (if stream-literal
                  (third (init-literal-proposition stream-literal))
                  1)))
          (when (<= stream-level swept-level)
            (fail-init-check
              stream-literal
              "~%The air stream of ~S is not above its source floor.~%~
               Source:           ~S at level ~S~%~
               Stream elevation: ~S~%~
               Give HAS-ELEVATION an absolute level strictly above the floor."
              drive swept swept-level stream-level)))))))

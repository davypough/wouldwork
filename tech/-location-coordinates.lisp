;;; Filename: -location-coordinates.lisp

;;; Location-coordinates substrate: the fixed 2D placement of a location, as its own
;;; capability independent of walking, beams, or anything else.  Owns LOCATION-COORDS>
;;; so that walkability-tech's own coordinate-driven WALK-VIA derivation and beam-
;;; crossing-tech's -beam-coordinates substrate share one source of truth for where each
;;; location is, without either technology depending on the other.  A problem that only
;;; needs walking between locations, with no transmitters/receivers/connectors, never
;;; needs to know beam-crossing exists; a problem using both capabilities enters each
;;; location's coordinates exactly once.
;;;
;;; Self-contained; spliced by (include-tech -location-coordinates).
;;;
;;; REQUIRES:
;;;   types     : location  --  declared by the problem
;;; The third coordinate is the location's own level, and may be omitted: a problem whose
;;; floor is flat never writes it.  A problem with no coordinate geometry at all can still
;;; give a location a level through -elevation's HAS-ELEVATION, so the two coexist; a
;;; location naming both is cross-checked here rather than silently preferring one.
;;;
;;; PROVIDES:
;;;   relation  : (location-coords> location $rational $rational $rational)  --  z optional,
;;;               defaulting to 0
;;;   init      : location-coordinates-init-check

(in-package :ww)


(define-static-relations
  (location-coords> location $rational $rational $rational))


(register-init-literal-defaults 'location-coords> 0)


(define-init-check location-coordinates-init-check (literals)
  (check-init-location-level-agreement literals))


(define-init-check-helper check-init-location-level-agreement (literals)
  "A location's level is LOCATION-COORDS>'s third coordinate, or HAS-ELEVATION in a
   problem carrying no coordinates.  A location naming both must name the same number:
   nothing downstream reads the second one, so a disagreement would otherwise sit in the
   spec unnoticed."
  (let ((coordinate-levels (make-hash-table :test #'eql)))
    (dolist (literal (positive-init-literals-with-relation 'location-coords> literals))
      (destructuring-bind (location x y z) (rest (init-literal-proposition literal))
        (declare (ignore x y))
        (setf (gethash location coordinate-levels) z)))
    (dolist (literal (positive-init-literals-with-relation 'has-elevation literals))
      (destructuring-bind (object level) (rest (init-literal-proposition literal))
        (multiple-value-bind (coordinate-level presentp)
            (gethash object coordinate-levels)
          (when (and presentp (/= coordinate-level level))
            (fail-init-check literal
                             "~%Location ~S is given two different levels.~%~
                              LOCATION-COORDS> third coordinate: ~S~%~
                              HAS-ELEVATION: ~S~%~
                              Write the level once, in the coordinates."
                             object coordinate-level level)))))))

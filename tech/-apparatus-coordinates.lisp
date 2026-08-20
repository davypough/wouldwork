;;; Filename: -apparatus-coordinates.lisp

;;; Apparatus-coordinates substrate: the fixed placement of a beam fixture, as its own
;;; capability independent of beams, sightlines, or the vertical model.  Owns
;;; APPARATUS-COORDS> so that -beam-los-coordinates' sightline derivation and -vertical's
;;; BASE share one source of truth for where each fixture is, without either depending on
;;; the other -- the same reason -location-coordinates exists for locations, and the
;;; reason this declaration no longer sits inside the 500-line -beam-los-coordinates.
;;;
;;; The third coordinate is the fixture's mounting level and may be omitted, defaulting to
;;; 1: apparatus hangs on a wall at about chest height unless a problem says otherwise.
;;; That default is why a FLOOR-REPEATER is excluded below -- it stands on the floor, so
;;; its base defaults to 0, and it keeps HAS-ELEVATION.  A problem with no coordinate
;;; geometry can still give any fixture a level through -elevation's HAS-ELEVATION; a
;;; fixture naming both is cross-checked here rather than silently preferring one.
;;;
;;; Self-contained; spliced by (include-tech -apparatus-coordinates).
;;;
;;; REQUIRES:
;;;   types     : transmitter, receiver, floor-repeater, wall-repeater, gun  --  all
;;;               optional; a problem declaring none of them gets an empty relation
;;; PROVIDES:
;;;   relation  : (apparatus-coords>
;;;                 (either transmitter receiver floor-repeater wall-repeater gun)
;;;                 $rational $rational $rational)  --  z optional, defaulting to 1
;;;   init      : apparatus-coordinates-init-check

(in-package :ww)


(define-optional-types transmitter receiver floor-repeater wall-repeater gun)


(define-static-relations
  (apparatus-coords>
    (either transmitter receiver floor-repeater wall-repeater gun)
    $rational $rational $rational))


(register-init-literal-defaults 'apparatus-coords> 1)


(define-init-check apparatus-coordinates-init-check (literals)
  (:consumes floor-repeater)
  (check-init-apparatus-level-agreement literals))


(define-init-check-helper check-init-apparatus-level-agreement (literals)
  "A fixture's mounting level is APPARATUS-COORDS>'s third coordinate.  A fixture naming
   both that and HAS-ELEVATION must name the same number, so the level is written once.
   A FLOOR-REPEATER is exempt: it does not read the third coordinate at all -- its base is
   HAS-ELEVATION, defaulting to 0 rather than to the mounting default of 1 -- so the two
   numbers are not saying the same thing about it and are not compared."
  (let ((mounting-levels (make-hash-table :test #'eql)))
    (dolist (literal (positive-init-literals-with-relation 'apparatus-coords> literals))
      (destructuring-bind (apparatus x y z) (rest (init-literal-proposition literal))
        (declare (ignore x y))
        (unless (init-type-member-p apparatus 'floor-repeater)
          (setf (gethash apparatus mounting-levels) z))))
    (dolist (literal (positive-init-literals-with-relation 'has-elevation literals))
      (destructuring-bind (object level) (rest (init-literal-proposition literal))
        (multiple-value-bind (mounting-level presentp) (gethash object mounting-levels)
          (when (and presentp (/= mounting-level level))
            (fail-init-check literal
                             "~%Fixture ~S is given two different levels.~%~
                              APPARATUS-COORDS> third coordinate: ~S~%~
                              HAS-ELEVATION: ~S~%~
                              Write the level once, in the coordinates."
                             object mounting-level level)))))))

;;; Filename: -elevation.lisp

;;; Elevation substrate: the authored base level of an object that carries no coordinate
;;; relation of its own -- a segment fixture (gate, screen, wall, edge), a floor repeater,
;;; or anything at all in a problem with no coordinate geometry.  Default 0.
;;;
;;; A location's level is LOCATION-COORDS>'s optional third coordinate and a wall-mounted
;;; fixture's is APPARATUS-COORDS>'s; both cross-check against HAS-ELEVATION rather than
;;; silently preferring one.  The role-branching anchor queries that used to live here --
;;; REPEATER-MOUNT-ELEVATION, REPEATER-ANCHOR-ELEVATION, FIXTURE-ELEVATION, and
;;; APPARATUS-ANCHOR-ELEVATION -- are gone: each was a per-type rule for reaching a base or
;;; a top, and -vertical's BASE and TOP now compute both for every type from one table.
;;;
;;; WALL-GEARS and WALL-BLOWER keep HAS-ELEVATION for a different quantity entirely: the
;;; elevation of the air stream they emit, read by -gears-fan's BLOWER-ELEVATION.  They
;;; have no base in the vertical model and are absent from -vertical's table.
;;;
;;; PROVIDES:
;;;   nested   : -height, -location-coordinates
;;;   types    : elevated-object (either location gate screen wall edge transmitter
;;;              receiver gun wall-gears wall-blower floor-repeater wall-repeater)
;;;   relation : (has-elevation elevated-object $rational)
;;;   queries  : object-elevation, location-elevation  --  both superseded by -vertical's
;;;              BASE; retained for -floor-blowing's hover override of LOCATION-ELEVATION
;;;              and for problems including this substrate without the vertical model

(include-tech -height)
(include-tech -location-coordinates)

(in-package :ww)


(define-optional-types
  gate screen wall edge transmitter receiver gun wall-gears wall-blower)


(define-types
  elevated-object
    (either location gate screen wall edge transmitter receiver gun
            wall-gears wall-blower
            floor-repeater wall-repeater))


(define-static-relations
  (has-elevation elevated-object $rational))  ;fixed base/anchor level; absent default depends on role


(defparameter *object-elevation-cache* (make-hash-table :test #'eq)
  "Memoizes OBJECT-ELEVATION by object.  HAS-ELEVATION is static -- fixed for the whole
   problem instance and never asserted or retracted by any update -- so each object's
   elevation only needs computing once.  DEFPARAMETER (not DEFVAR) so the cache resets
   every time this file is respliced and loaded for a (possibly different) problem.")


(define-query object-elevation (?object elevated-object)
  ;; Declared fixed level of an elevated object, or zero if none was asserted.
  ;; Cached: see *OBJECT-ELEVATION-CACHE*.
  (multiple-value-bind (cached present) (gethash ?object *object-elevation-cache*)
    (if present
      cached
      (setf (gethash ?object *object-elevation-cache*)
            (if (bind (has-elevation ?object $level))
              $level
              0)))))


(define-query location-elevation (?location location)
  ;; A location's own floor level: LOCATION-COORDS>'s optional third coordinate, or
  ;; HAS-ELEVATION in a problem carrying no coordinates, or zero.  -vertical's FIXED-BASE
  ;; resolves the same two sources the same way for every kind of object; both go when
  ;; this file's queries do.
  (if (bind (location-coords> ?location $x $y $z))
    $z
    (object-elevation ?location)))

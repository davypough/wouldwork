;;; Filename: -elevation.lisp

;;; Elevation substrate: the fixed vertical level of a location's own floor, a fixed
;;; obstacle's base, or a fixed fixture's beam/sightline anchor.  Locations and barrier
;;; fixtures default to base elevation 0; transmitter and receiver beam anchors default to
;;; elevation 1.  Nondefault objects assert an explicit fact.  Declared identically by
;;; every tech file that reads it, so consumers nest-include this file instead of each
;;; re-declaring the relation and queries.
;;;
;;; PROVIDES:
;;;   types    : elevated-object (either location gate screen fence wall transmitter receiver)
;;;   relation : (has-elevation elevated-object $fixnum)
;;;   queries  : object-elevation, location-elevation, fixture-elevation

(in-package :ww)


(define-optional-types gate screen fence wall transmitter receiver)


(define-types
  elevated-object (either location gate screen fence wall transmitter receiver))


(define-static-relations
  (has-elevation elevated-object $fixnum))  ;fixed base/anchor level; absent default depends on role


(define-query object-elevation (?object elevated-object)
  ;; Declared fixed level of an elevated object, or zero if none was asserted.
  (if (bind (has-elevation ?object $level))
    $level
    0))


(define-query location-elevation (?location location)
  ;; Declared floor level of a location, or zero if none was asserted (ordinary ground).
  (object-elevation ?location))


(define-query fixture-elevation (?fixture (either gate transmitter receiver))
  ;; Declared fixed-fixture level.  Gates use base elevation 0 by default; transmitters and
  ;; receivers use beam-anchor elevation 1.  These roles have different physical meanings,
  ;; so fixture cannot have one universal omitted-value default.
  (if (bind (has-elevation ?fixture $level))
    $level
    (if (or (transmitter ?fixture)
            (receiver ?fixture))
      1
      0)))

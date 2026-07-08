;;; Filename: -elevation.lisp

;;; Elevation substrate: the fixed vertical level of a location's own floor, a fixed
;;; obstacle's base, or a fixed fixture's beam/sightline anchor.  Most objects are ordinary
;;; ground-level anchors and declare no fact; raised locations, obstacles, or fixtures assert
;;; one.  Declared identically by
;;; every tech file that reads it, so consumers nest-include this file instead of each
;;; re-declaring the relation and queries.
;;;
;;; PROVIDES:
;;;   types    : elevated-object (either location gate screen fence transmitter receiver)
;;;   relation : (has-elevation elevated-object $fixnum)
;;;   queries  : object-elevation, location-elevation, fixture-elevation

(in-package :ww)


(define-optional-types gate screen fence transmitter receiver)


(define-types
  elevated-object (either location gate screen fence transmitter receiver))


(define-static-relations
  (has-elevation elevated-object $fixnum))  ;fixed anchor level; absent means ground (0)


(define-query object-elevation (?object elevated-object)
  ;; Declared fixed level of an elevated object, or zero if none was asserted.
  (if (bind (has-elevation ?object $level))
    $level
    0))


(define-query location-elevation (?location location)
  ;; Declared floor level of a location, or zero if none was asserted (ordinary ground).
  (object-elevation ?location))


(define-query fixture-elevation (?fixture (either gate transmitter receiver))
  ;; Declared anchor level of a fixed LOS/beam fixture, or zero if none was asserted.
  (object-elevation ?fixture))

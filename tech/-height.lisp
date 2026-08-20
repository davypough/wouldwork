;;; Filename: -height.lisp

;;; Height substrate: the authored height of a heighted object, used for vertical reach,
;;; vaulting, and line-of-sight clearance checks.  This file owns the heighted-object type
;;; composition and the (has-height ...) relation, declared identically by every tech file
;;; that reads or writes it -- box, jump, jammer, and beam-direct -- so consumers
;;; nest-include this file instead of each re-declaring the same union and relation.
;;;
;;; The DECLARED-HEIGHT query is gone: -vertical's OBJECT-HEIGHT reads the same HAS-HEIGHT
;;; facts against the per-type constant table, returning identical values for every
;;; heighted-object leaf and covering the types this union never included.
;;; A repeater's height follows its mounting axis: vertical for a floor-repeater and
;;; horizontal for a wall-repeater.
;;;
;;; PROVIDES:
;;;   types    : repeater (either floor-repeater wall-repeater);
;;;              heighted-object (either box gate agent screen wall edge jammer connector
;;;              floor-repeater wall-repeater) -- what can have a declared height;
;;;              optional subtypes absent from the problem resolve to nil, a no-op
;;;   relation : (has-height heighted-object $rational)
;;;   query    : none.  Heights are read through -vertical's OBJECT-HEIGHT.

(in-package :ww)


(define-optional-types wall edge floor-repeater wall-repeater)


(define-types
  repeater (either floor-repeater wall-repeater)
  heighted-object
    (either box gate agent screen wall edge jammer connector floor-repeater wall-repeater))


(define-static-relations
  (has-height heighted-object $rational))

;;; Filename: -height.lisp

;;; Height substrate: the physical height of a heighted object, used for vertical reach and
;;; vaulting-clearance checks.  This file owns the heighted-object type composition and the
;;; (has-height ...) relation, declared identically by every tech file that reads or writes it --
;;; box, barrier, and agent -- so consumers nest-include this file instead of each
;;; re-declaring the same union and relation.
;;;
;;; PROVIDES:
;;;   type     : heighted-object (either box fence gate agent screen jammer)  --  what can have a
;;;              declared height; subtypes absent from the problem's own define-types resolve
;;;              to nil, a no-op
;;;   relation : (has-height heighted-object $fixnum)

(in-package :ww)


(define-types
  heighted-object (either box fence gate agent screen jammer))  ;what can have a declared height


(define-static-relations
  (has-height heighted-object $fixnum))

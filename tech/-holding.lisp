;;; Filename: -holding.lisp

;;; Holding substrate: what an agent carries.  This file owns the cargo type composition and
;;; the (holding ...) relation, declared identically by every tech file that reads or writes
;;; it -- box, jammer, and walkability -- so consumers nest-include this file instead
;;; of each re-declaring the same union and relation.
;;;
;;; PROVIDES:
;;;   type     : cargo (either box jammer connector fan)  --  what an agent can carry; subtypes
;;;              absent from the problem's own define-types resolve to nil, a no-op
;;;   relation : (holding agent $cargo)

(include-tech -physical-init-checks)

(in-package :ww)


(define-types
  cargo (either box jammer connector fan))  ;what an agent can carry


(define-dynamic-relations
  (holding agent $cargo))

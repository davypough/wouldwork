;;; Filename: -position.lisp

;;; Position substrate: the fixed placement of a fixture at a location.  This file
;;; owns the fixed-position-object type composition and the (has-position ...) relation, declared
;;; identically by every tech file that reads or writes it -- box, jammer, ladder, and recorder --
;;; so consumers nest-include this file instead of each re-declaring the same union and
;;; relation.
;;;
;;; PROVIDES:
;;;   type     : fixed-position-object (either plate ladder floor-gears wall-gears
;;;              angled-gears recorder)  --  what can be positioned at a fixed location; subtypes
;;;              absent from the problem's own define-types resolve to nil, a no-op.  The
;;;              gears leaf types appear directly because this file splices before
;;;              gears-fan installs the gears union.
;;;   relation : (has-position fixed-position-object $location)

(include-tech -plate-types)

(in-package :ww)


(define-types
  fixed-position-object (either plate ladder floor-gears wall-gears angled-gears recorder))  ;what can be positioned at a fixed location


(define-static-relations
  (has-position fixed-position-object $location))

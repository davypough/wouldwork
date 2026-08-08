;;; Filename: -location.lisp

;;; Location substrate: what "being at a location" means for any movable object.  This file
;;; owns the mobile-object type composition and the (has-location ...) relation, declared
;;; identically by every tech file that reads or writes it -- box, jammer, walkability,
;;; ladder, and beam-direct -- so consumers nest-include this file instead of each
;;; re-declaring the same union and relation.
;;;
;;; PROVIDES:
;;;   type     : mobile-object (either agent box jammer connector fan tray)  --  what can occupy
;;;              a location; subtypes absent from the problem's own define-types resolve to
;;;              nil, a no-op
;;;   relation : (has-location mobile-object $location)

(include-tech -physical-init-checks)

(in-package :ww)


(define-types
  mobile-object (either agent box jammer connector fan tray))  ;what can be at a location


(define-dynamic-relations
  (has-location mobile-object $location))

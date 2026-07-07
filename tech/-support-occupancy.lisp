;;; Filename: -support-occupancy.lisp

;;; Support occupancy substrate: whether the top of a support is unoccupied.  This file owns
;;; the support-occupant type composition, declared identically by every other tech file that
;;; reads or writes (on support-occupant $support).
;;;
;;; REQUIRES:
;;;   type     : support
;;; PROVIDES:
;;;   types    : support-occupant (either agent box jammer connector)  --  also declared
;;;              identically by box, jammer, accessibility, and ladder
;;;              support (either plate box)  --  also declared identically by box,
;;;              jammer, accessibility, and ladder
;;;   relation : (on support-occupant $support)  --  also declared identically by box,
;;;              jammer, accessibility, and ladder; multiple techs both read
;;;              and write it
;;;   query    : cleartop

(in-package :ww)


(define-types
  support-occupant (either agent box jammer connector)  ;also declared identically by box/jammer/accessibility/ladder
  support (either plate box))  ;also declared identically by box/jammer/accessibility/ladder; what a movable object can rest on


(define-dynamic-relations
  (on support-occupant $support))  ;also declared by box/jammer/accessibility/ladder; support an occupant rests on (absent if ground)


(define-query cleartop (?support support)
  ;; A support top is clear iff no support occupant rests on it.
  (not (exists (?x support-occupant)
         (on ?x ?support))))

;;; Filename: -support-occupancy.lisp

;;; Support occupancy substrate: whether the top of a support is unoccupied.  This file owns
;;; the support-occupant type composition, declared identically by every other tech file that
;;; reads or writes (on support-occupant $support).
;;;
;;; REQUIRES:
;;;   type     : support
;;; PROVIDES:
;;;   types    : support-occupant (either agent box jammer connector fan)  --  also declared
;;;              identically by box, jammer, walkability, and ladder
;;;              support (either plate box fan)  --  also declared identically by
;;;              box, jammer, walkability, and ladder.  Gears are not a support: only
;;;              a fan can occupy them, via -gears-fan's (mounted-on ...) attachment
;;;              rather than (on ...)
;;;   relation : (on support-occupant $support)  --  also declared identically by box,
;;;              jammer, walkability, and ladder; multiple techs both read
;;;              and write it
;;;   query    : cleartop

(in-package :ww)


(define-types
  support-occupant (either agent box jammer connector fan)  ;also declared identically by box/jammer/walkability/ladder
  support (either plate box fan))  ;also declared identically by box/jammer/walkability/ladder; what a movable object can rest on (fan-on-gears is an attachment, not support)


(define-dynamic-relations
  (on support-occupant $support))  ;also declared by box/jammer/walkability/ladder; support an occupant rests on (absent if ground)


(define-query cleartop (?support support)
  ;; A support top is clear iff no support occupant rests on it.
  (not (exists (?x support-occupant)
         (on ?x ?support))))

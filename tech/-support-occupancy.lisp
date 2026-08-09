;;; Filename: -support-occupancy.lisp

;;; Support occupancy substrate: whether the top of a support is unoccupied.  This file owns
;;; the support-occupant type composition, declared identically by every other tech file that
;;; reads or writes (on support-occupant $support).
;;;
;;; REQUIRES:
;;;   type     : support
;;; PROVIDES:
;;;   types    : support-occupant (either agent box jammer connector fan tray)  --  also
;;;              declared identically by box, jammer, walkability, and ladder
;;;              support (either pressure-plate toggle-plate box fan tray floor-blower
;;;              angled-blower)  --  also declared
;;;              identically by box, jammer, walkability, and ladder.  Gears are not a
;;;              support: only a fan can occupy them, via -gears-fan's (mounted-on ...)
;;;              attachment rather than (on ...).  A tray is a support only while held; on
;;;              the ground it is inert (see support-top-elevation)
;;;   relation : (on support-occupant $support)  --  also declared identically by box,
;;;              jammer, walkability, and ladder; multiple techs both read
;;;              and write it
;;;   nested   : -interaction-policy (neutral support-use-allowed hook)
;;;   query    : cleartop

(include-tech -plate-types)
(include-tech -interaction-policy)
(include-tech -physical-init-checks)

(in-package :ww)


(define-types
  support-occupant (either agent box jammer connector fan tray)  ;also declared identically by box/jammer/walkability/ladder
  support
    (either pressure-plate toggle-plate box fan tray floor-blower angled-blower))  ;fixed floor/angled blowers expose the same flush support surface as a mounted fan


(define-dynamic-relations
  (on support-occupant $support))  ;also declared by box/jammer/walkability/ladder; support an occupant rests on (absent if ground)


(define-query cleartop (?support support)
  ;; A support top is clear iff no support occupant rests on it.
  (not (exists (?x support-occupant)
         (on ?x ?support))))

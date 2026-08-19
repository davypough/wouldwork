;;; Filename: -support-occupancy.lisp

;;; Support occupancy substrate: whether the top of a support is unoccupied.  This file owns
;;; the support-occupant type composition, declared identically by every other tech file that
;;; reads or writes (on $support-occupant $support :bijective).
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
;;;              the ground it is inert (see -vertical's BASE, which gives a held tray
;;;              its holder's top and a grounded one its own location's level)
;;;   relation : (on $support-occupant $support :bijective)  --  also declared identically
;;;              by box, jammer, walkability, and ladder; multiple techs both read
;;;              and write it.  Bijective so CLEARTOP can look up a support's occupant
;;;              (if any) by reverse index instead of scanning every support-occupant --
;;;              safe because a support holds at most one occupant, an invariant
;;;              -PHYSICAL-INIT-CHECKS enforces at init and every placement action
;;;              enforces thereafter by checking CLEARTOP first
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
  (on $support-occupant $support :bijective))  ;also declared by box/jammer/walkability/ladder; support an occupant rests on (absent if ground)


(define-query cleartop (?support support)
  ;; A support top is clear iff no support occupant rests on it.  ON is bijective, so
  ;; this is a single reverse-indexed lookup instead of a scan over every support-occupant.
  (not (bind (on $occupant ?support))))

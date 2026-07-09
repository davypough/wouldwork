;;; Filename: plate.lisp

;;; Plate technology: pressure plates that depress under any movable object resting on them.
;;;
;;; REQUIRES:
;;;   type   : plate  --  declared optional here via define-optional-types, so a problem
;;;            with no plate instances need not declare plate itself
;;;   nested : -support-occupancy (support-occupant, support, on, cleartop)
;;;   driver : the master propagate-consequences! must call update-plate-status!
;;; PROVIDES:
;;;   type     : plate  --  declared optional here (define-optional-types); a problem with no
;;;              plates need not declare it.  Other techs (gate, jammer, box, beam-relay) still
;;;              declare their own plate-alias (either plate) for their own pre-params -- the
;;;              two names resolve compatibly and do not conflict.
;;;   relation : (depressed plate)  --  read by gate's energized
;;;   update   : update-plate-status!

(include-tech -support-occupancy)

(in-package :ww)


(define-optional-types plate)


(define-dynamic-relations
  (depressed plate))


(define-update update-plate-status! ()
  ;; A plate is depressed iff something rests on it.  Occupancy is delegated to the
  ;; query cleartop, so this update stays independent of the problem's support-occupant
  ;; roster (claustro3 inlined (either agent box jammer connector) here instead).  Sets or
  ;; clears (depressed ?p); change detection is automatic, so an unchanged re-assert is silent
  ;; and does not extend the fixpoint.
  (doall (?p plate)
    (if (cleartop ?p)
      (not (depressed ?p))
      (depressed ?p))))

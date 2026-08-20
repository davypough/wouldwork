;;; Filename: stairs.lisp

;;; Stairs mobility mode.  Registers the one predicate that makes a traversal edge a
;;; stairway: every enabling means in the chosen clause must be passable for the mover.
;;; Stairs deliberately impose no elevation-difference limit and no elevation-equality
;;; test -- an authored stairway is the answer to a level change, not a consequence of one.
;;;
;;; REQUIRES:
;;;   types     : agent, location
;;;   nested    : -passability; -threat; -traversal; -mobility-action
;;; PROVIDES:
;;;   mode      : stairway, registered with -traversal
;;;   action    : move (from -mobility-action)

(include-tech -passability)
(include-tech -threat)
(include-tech -traversal)
(include-tech -mobility-action)

(in-package :ww)


(define-problem-helper stairs-segment-for-clause
    (state agent source destination clause)
  "Return a normalized STAIRS segment when CLAUSE's means are all usable and the
   destination is safe."
  (let ((means (canonical-enabling-means clause)))
    (when (and (funcall (symbol-function 'all-clear) state agent means)
               (funcall (symbol-function 'safe) state destination))
      (list 'stairs source means destination))))


(register-traversal-mode 'stairway 'stairs-segment-for-clause
                         '(gate screen ladder
                           floor-gears wall-gears angled-gears
                           floor-blower wall-blower angled-blower))

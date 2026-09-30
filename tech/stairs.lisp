;;; Filename: stairs.lisp

;;; Stairs traversal kind.  A clause naming a STAIRCASE is a stairway, and this file
;;; registers the one predicate that decides it: every other means in the clause must be
;;; passable for the mover.  Stairs deliberately impose no elevation-difference limit and no
;;; elevation-equality test -- a staircase is the answer to a level change, not a
;;; consequence of one.
;;;
;;; A staircase is a static separator: it has no state and no position, and is always
;;; passable.  It stays in the segment's witness, so a printed route names the staircase it
;;; climbed, but -traversal's TRAVERSAL-CLAUSE-PROFILE removes it from the means the
;;; clearance test sees.
;;;
;;; REQUIRES:
;;;   types     : agent, location
;;;   nested    : -passability; -threat; -traversal; -mobility-action
;;; PROVIDES:
;;;   types     : staircase  --  declared optional; a problem names its staircases
;;;   kind      : stairs, registered with -traversal
;;;   action    : move (from -mobility-action)

(include-tech -passability)
(include-tech -threat)
(include-tech -traversal)
(include-tech -mobility-action)

(in-package :ww)


(define-optional-types staircase)


(define-problem-helper stairs-segment-for-clause
    (state agent source destination clause)
  "Return a normalized STAIRS segment when CLAUSE's means are all usable and the
   destination is safe.  The witness is the whole canonical clause, staircase included."
  (when (and (funcall (symbol-function 'all-clear)
                      state agent (second (traversal-clause-profile clause)))
             (funcall (symbol-function 'safe) state destination))
    (list 'stairs source (canonical-enabling-means clause) destination)))


(register-traversal-kind 'stairs 'stairs-segment-for-clause
                         '(staircase) '(staircase)
                         '(gate screen
                           floor-gears wall-gears angled-gears
                           floor-blower wall-blower angled-blower))

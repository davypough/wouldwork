;;; Filename: ladder.lisp

;;; Ladder mobility mode.  Registers the one predicate that makes a traversal edge a
;;; climb: one of the ladders named in the chosen clause must be positioned exactly at the
;;; source, and every enabling implement in that clause must be usable.  The segment
;;; witness places the selected ladder first, followed by the other canonicalized means.
;;;
;;; A climbing clause therefore reads in two registers at once -- its ladders are
;;; candidates, of which one must be in place, while every member including those ladders
;;; must also clear.  That is why a climb's payload was a flat list before -traversal and
;;; is a one-clause DNF now: the wrapping preserves it exactly, and a second clause offers
;;; a genuinely alternative set of means rather than a second candidate ladder, which the
;;; first clause already expresses by listing both.
;;;
;;; Climbing is directed: CLIMB is authored as TRAVERSAL-VIA>, since a ladder that carries
;;; an agent up need not carry it down the same way.
;;;
;;; REQUIRES:
;;;   types     : agent, location  --  ladder is declared optional here and by -passability
;;;   nested    : -position; -passability; -threat; -traversal; -mobility-action
;;; PROVIDES:
;;;   types     : ladder  --  declared optional; the two declarations resolve compatibly
;;;   mode      : climbing, registered with -traversal
;;;   queries   : usable-ladder-at-source, positioned-ladders-for-means
;;;   action    : move (from -mobility-action)

(include-tech -position)
(include-tech -passability)
(include-tech -threat)
(include-tech -traversal)
(include-tech -mobility-action)

(in-package :ww)


(define-optional-types ladder)


(define-query usable-ladder-at-source
    (?ladder ladder ?source location ?means)
  (and (member ?ladder ?means)
       (has-position ?ladder ?source)))


(define-query positioned-ladders-for-means (?source location ?means)
  (do (assign $ladders nil)
      (doall (?ladder ladder)
        (if (usable-ladder-at-source ?ladder ?source ?means)
          (assign $ladders (cons ?ladder $ladders))))
      (sort $ladders #'string< :key #'symbol-name)))


(define-problem-helper ladder-segment-witness (ladder means)
  "Place the selected LADDER first, followed by every other required enabling means."
  (cons ladder
        (remove ladder (canonical-enabling-means means)
                :test #'eq :count 1)))


(define-problem-helper ladder-segment-for-clause
    (state agent source destination clause)
  "Return a normalized LADDER segment when one of CLAUSE's ladders stands at the source
   and every means in it is usable."
  (let* ((means (canonical-enabling-means clause))
         (ladders (funcall (symbol-function 'positioned-ladders-for-means)
                           state source means)))
    (when (and ladders
               (funcall (symbol-function 'all-clear) state agent means)
               (funcall (symbol-function 'safe) state destination))
      (list 'ladder source
            (ladder-segment-witness (first ladders) means)
            destination))))


(register-traversal-mode 'climbing 'ladder-segment-for-clause
                         '(gate screen ladder))

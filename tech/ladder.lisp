;;; Filename: ladder.lisp

;;; Ladder traversal kind.  A clause naming a LADDER is a climb, and this file registers the
;;; one predicate that decides it: one of the ladders named in the clause must be
;;; positioned exactly at the source, and every enabling implement in that clause must be
;;; usable.  The segment witness places the selected ladder first, followed by the other
;;; canonicalized means.  A climb registers no static separators, so its means are its
;;; whole clause.
;;;
;;; A climbing clause therefore reads in two registers at once -- its ladders are
;;; candidates, of which one must be in place, while every member including those ladders
;;; must also clear.  That is why a climb's payload was a flat list before -traversal and
;;; is a one-clause DNF now: the wrapping preserves it exactly, and a second clause offers
;;; a genuinely alternative set of means rather than a second candidate ladder, which the
;;; first clause already expresses by listing both.
;;;
;;; Climbing is directed: CLIMB is authored as traverse-via>, since a ladder that carries
;;; an agent up need not carry it down the same way.
;;;
;;; Grounded climbs remain transparent mobility and may compose with adjacent grounded
;;; segments.  A supported agent instead crosses exactly one support-state boundary: this
;;; file exposes a singleton transition from its current support to ground at the climb's
;;; destination, using the same ladder, passability, and safety checks as the grounded edge.
;;;
;;; REQUIRES:
;;;   types     : agent, location  --  ladder is declared optional here and by -passability
;;;   nested    : -position; -passability; -threat; -traversal; -mobility-action
;;; PROVIDES:
;;;   types     : ladder  --  declared optional; the two declarations resolve compatibly
;;;   kind      : climb, registered with -traversal
;;;   queries   : usable-ladder-at-source, positioned-ladders-for-means,
;;;               ladder-configuration-transitions
;;;   provider  : ladder-configuration-transitions registered with
;;;               -configuration-transition
;;;   init      : ladder-init-check
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


(register-traversal-kind 'climb 'ladder-segment-for-clause
                         '(ladder) nil '(gate screen))


;;;; SUPPORT-CHANGING TRANSITIONS ;;;;


(define-problem-helper ladder-configuration-transition-for-family
    (state agent source-configuration destination family)
  "Return the first feasible supported-source LADDER transition among FAMILY's climb-kind
   clauses, or NIL."
  (let ((source (first source-configuration))
        (destination-configuration (list destination 'ground)))
    (loop for clause in (traversal-family-kind-clauses
                          state source destination family 'climb)
          for segment = (ladder-segment-for-clause
                          state agent source destination clause)
          when segment
            return (list 'ladder
                         source-configuration
                         (third segment)
                         destination-configuration))))


(define-query ladder-configuration-transitions
    (?agent agent ?source-configuration)
  (do (assign $source (first ?source-configuration))
      (assign $source-place (second ?source-configuration))
      (assign $transitions nil)
      (if (not (eql $source-place 'ground))
        (doall (?destination location)
          (if (bind (traverse-via> $source $family ?destination))
            (do (assign $transition
                        (ladder-configuration-transition-for-family
                          state ?agent ?source-configuration
                          ?destination $family))
                (if $transition
                  (assign $transitions
                          (cons $transition $transitions)))))))
      $transitions))


(register-configuration-transition-provider
  'ladder-configuration-transitions)


;;;; INITIALIZATION VALIDATION ;;;;


(define-init-check ladder-init-check (literals)
  (:consumes ladder)
  (check-init-climbing-clauses literals))


(define-init-check-helper check-init-climbing-clauses (literals)
  "A clause naming a ladder is a climb.  Every climb clause must sit in a directed fact and
   name at least one ladder fixed at that fact's source.  A symmetric climb is misleading:
   the ladder's functional HAS-POSITION can make it usable from only one endpoint.  A climb
   clause without a source-positioned ladder can never produce a segment.  Clauses of other
   kinds in the same fact are not this check's business."
  (dolist (literal
            (positive-init-literals-with-relation 'traverse-via literals))
    (when (some (lambda (clause)
                  (some (lambda (item) (init-type-member-p item 'ladder)) clause))
                (third (init-literal-proposition literal)))
      (fail-init-check literal
        "A clause naming a ladder is a climb, and a climb must be directed.  Author it in a traverse-via> fact with the ladder's location as the source.")))
  (dolist (literal
            (positive-init-literals-with-relation 'traverse-via> literals))
    (destructuring-bind (source family destination)
        (rest (init-literal-proposition literal))
      (declare (ignore destination))
      (dolist (clause family)
        (when (and (some (lambda (item) (init-type-member-p item 'ladder)) clause)
                   (notany (lambda (item)
                             (and (init-type-member-p item 'ladder)
                                  (some (lambda (position-literal)
                                          (equal (init-literal-proposition position-literal)
                                                 `(has-position ,item ,source)))
                                        (positive-init-literals-with-relation
                                          'has-position literals))))
                           clause))
          (fail-init-check literal
            "Climb clause ~S has no listed ladder positioned at its source ~S."
            clause source))))))

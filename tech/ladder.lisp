;;; Filename: ladder.lisp

;;; Ladder mobility provider.  A directed CLIMB-VIA> edge contributes a LADDER traversal
;;; segment when one of its listed ladder fixtures is positioned exactly at the segment
;;; source and every enabling implement is usable.  The segment witness places the selected
;;; ladder first, followed by the other canonicalized enabling means.
;;;
;;; REQUIRES:
;;;   types     : agent, location  --  ladder is declared optional here (define-optional-types)
;;;   nested    : -position; -passability; -threat; -mobility-action
;;; PROVIDES:
;;;   types     : ladder  --  declared optional here and by nested -passability; the
;;;               declarations resolve compatibly
;;;   relations : (climb-via> location $list location)
;;;   queries   : usable-ladder-at-source, positioned-ladders-for-means,
;;;               ladder-traversal-segments
;;;   provider  : ladder-traversal-segments registered with -mobility
;;;   action    : move (from -mobility-action)

(include-tech -position)
(include-tech -passability)
(include-tech -threat)
(include-tech -mobility-action)

(in-package :ww)


(define-optional-types ladder)


(define-static-relations
  (climb-via> location $list location))  ;directed climb edge; $list = enabling means


(define-init-check ladder-init-check (literals)
  (:consumes gate screen ladder)
  (check-init-list-relation-items-have-types
    literals 'climb-via> '(gate screen ladder)))


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


(define-query ladder-traversal-segments (?agent agent ?from location)
  (do (assign $segments nil)
      (doall (?to location)
        (if (bind (climb-via> ?from $raw-means ?to))
          (do (assign $means (canonical-enabling-means $raw-means))
              (assign $ladders
                      (positioned-ladders-for-means ?from $means))
              (if (and $ladders
                       (all-clear ?agent $means)
                       (safe ?to))
                (assign $segments
                        (cons
                          (list 'ladder ?from
                                (ladder-segment-witness
                                  (first $ladders) $means)
                                ?to)
                          $segments))))))
      $segments))


(register-mobility-provider 'ladder-traversal-segments)

;;; Filename: reachability.lisp

;;; Reachability background capability: whether one location is within placing/picking reach
;;; of another.  Two locations are in reach iff identical, or a reach edge joins them with
;;; every barrier gate open.
;;;
;;; REQUIRES:
;;;   types     : location
;;;   nested    : -reachability (identity-default reachable query overridden here);
;;;               -gate (gate optional type, (open gate) relation) -- shared with gate,
;;;               walkability (via -passability), visibility, beam-direct, and
;;;               beam-crossing, which all nest -gate instead of hand-declaring it
;;; PROVIDES:
;;;   relations : (reach-via location $list location)
;;;   queries   : reachable (overrides -reachability), reachable-clear

(include-tech -reachability)
(include-tech -gate)

(in-package :ww)


(define-static-relations
  (reach-via location $list location)  ;symmetric reach edge (eg through a wall opening); $list = barrier gates that must be open
  (reach-via> location $list location))  ;directional reach edge, reacher's location first


(define-init-check reachability-init-check (literals)
  (:consumes gate)
  (check-init-list-relation-items-have-types
    literals 'reach-via '(gate))
  (check-init-list-relation-items-have-types
    literals 'reach-via> '(gate)))


(define-query reachable (?location1 location ?location2 location)
  ;; Within reach iff the same location, a symmetric reach edge joins them, or a directional
  ;; edge runs from the reacher at ?location2 to the target at ?location1 -- every barrier open
  ;; in either case.  Callers pass the target first and the actor's own location second (see
  ;; PICKUP-CLEAR and the PUT-* actions), so a REACH-VIA> row reads reacher-then-target and
  ;; models a reach that works one way only: down from a ledge to the floor below, say, where
  ;; the return reach would be a climb.  Reach remains agent-independent; the separate
  ;; WITHIN-AGENT-VERTICAL-REACH test still bounds how far up or down the actor can act.
  (or (eql ?location1 ?location2)
      (and (bind (reach-via ?location1 $barriers ?location2))
           (ww-loop for $b in $barriers
                    always (reachable-clear $b)))
      (and (bind (reach-via> ?location2 $directed-barriers ?location1))
           (ww-loop for $b in $directed-barriers
                    always (reachable-clear $b)))))


(define-query reachable-clear (?barrier gate)
  ;; A reach barrier clears only as an open gate; a closed gate or any non-gate barrier blocks.
  (and (gate ?barrier)
       (open ?barrier)))

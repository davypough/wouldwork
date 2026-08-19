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
;;;   relations : (reach-via location $list location),
;;;               (reach-via> location $list location)
;;;   queries   : reachable (overrides -reachability), reachable-clear

(include-tech -reachability)
(include-tech -gate)

(in-package :ww)


(define-static-relations
  (reach-via location $list location)  ;symmetric reach edge (eg through a wall opening); $list = barrier gates that must be open
  (reach-via> location $list location))  ;directional reach edge, reacher's location first; for one-way openings only, never for a height difference


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
  ;; models an opening that admits an arm one way only: a letterbox slot, an overhang, or a
  ;; recess whose mouth faces one side.  Reserve it for asymmetry the actor cannot defeat by
  ;; standing higher.  A pure height difference belongs in symmetric REACH-VIA instead, since
  ;; the vertical tests already measure that difference from wherever the actor currently
  ;; stands -- encoding it here as well would freeze the one-way verdict at ground level and
  ;; wrongly deny an actor who has climbed onto a box.  Reach itself stays agent-independent:
  ;; WITHIN-AGENT-VERTICAL-REACH bounds lifting in both directions, and
  ;; WITHIN-AGENT-PLACEMENT-REACH bounds only how far above the actor a resting place may be.
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

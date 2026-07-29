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
  (reach-via location $list location))  ;reach edge (eg through a wall opening); $list = barrier gates that must be open


(define-query reachable (?location1 location ?location2 location)
  ;; Within reach iff the same location, or a reach edge joins them with every barrier open.
  ;; Agent-independent; reach-via is symmetric (both endpoints are locations).
  (or (eql ?location1 ?location2)
      (and (bind (reach-via ?location1 $barriers ?location2))
           (ww-loop for $b in $barriers
                    always (reachable-clear $b)))))


(define-query reachable-clear (?barrier gate)
  ;; A reach barrier clears only as an open gate; a closed gate or any non-gate barrier blocks.
  (and (gate ?barrier)
       (open ?barrier)))

;;; Filename: reachability.lisp

;;; Reachability background capability: whether one location is within placing/picking reach
;;; of another.  Two locations are in reach iff identical, or a reach edge joins them with
;;; every barrier gate open.
;;;
;;; REQUIRES:
;;;   types     : location  --  gate is declared optional here (define-optional-types),
;;;               coordinated with gate, accessibility, visibility, beam-direct, and
;;;               beam-crossing, which all convert gate together since they share the
;;;               (open gate) relation verbatim
;;; PROVIDES:
;;;   types     : gate  --  declared optional here; other techs (gate, accessibility,
;;;               visibility, beam-direct, beam-crossing, etc.) independently declare their
;;;               own gate-alias for their own pre-params; the bare and aliased forms
;;;               resolve compatibly
;;;   relations : (open gate)  --  also declared identically by gate, accessibility,
;;;               visibility, and beam-direct; only gate's update-gate-status!
;;;               ever asserts it
;;;               (reachable-via location $list location)
;;;   queries   : reachable, reachable-clear

(in-package :ww)


(define-optional-types gate)


(define-dynamic-relations
  (open gate))  ;also declared by gate/accessibility/visibility/beam-direct; only gate writes it


(define-static-relations
  (reachable-via location $list location))  ;reach edge (eg through a wall opening); $list = barrier gates that must be open


(define-query reachable (?location1 location ?location2 location)
  ;; Within reach iff the same location, or a reach edge joins them with every barrier open.
  ;; Agent-independent; reachable-via is symmetric (both endpoints are locations).
  (or (eql ?location1 ?location2)
      (and (bind (reachable-via ?location1 $barriers ?location2))
           (ww-loop for $b in $barriers
                    always (reachable-clear $b)))))


(define-query reachable-clear (?barrier gate)
  ;; A reach barrier clears only as an open gate; a closed gate or any non-gate barrier blocks.
  (and (gate ?barrier)
       (open ?barrier)))

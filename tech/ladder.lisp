;;; Filename: ladder.lisp

;;; Ladder technology: one-way traversal over a ladder-like object.  A one-way edge
;;; (traversable>) carries the agent from its current location to the edge's destination when
;;; every enabling implement is usable.  Unlike walking (accessibility), one-way edges are not
;;; folded into the reachable-set closure; they are explicit ladder actions.
;;;
;;; REQUIRES:
;;;   types     : agent, location  --  ladder is declared optional here (define-optional-types)
;;;   nested    : -support-occupancy (support-occupant, support, (on ...), cleartop);
;;;               -location (mobile-object, (has-location ...)); -position (fixed-position-object,
;;;               (has-position ...)); -passability (obstacle-clear, all-clear)  --  all
;;;               shared via nested include-tech rather than local declaration
;;; PROVIDES:
;;;   types     : ladder  --  declared optional here and by nested -passability; the
;;;               declarations resolve compatibly
;;;   relations : (traversable> location $list location)
;;;   query     : one-way-clear
;;;   action    : use-ladder

(include-tech -support-occupancy)
(include-tech -location)
(include-tech -position)
(include-tech -passability)

(in-package :ww)


(define-optional-types ladder)


(define-static-relations
  (traversable> location $list location))  ;one-way edge (> suppresses symmetry); $list = enabling means (eg ladders)


(define-query one-way-clear (?agent ?means)
  ;; Every implement enabling a one-way edge must be usable by ?agent.  Delegates to
  ;; passability's shared all-clear over the edge's means list, so use-ladder can
  ;; guard the hop without re-deriving passability inline.
  (all-clear ?agent ?means))


(define-action use-ladder
  ;; Use a one-way ladder-like object from ground.  The agent lands on ground at the
  ;; traversal destination.  The one-way edge starts at the agent's current location; the
  ;; agent must be standing at the ladder's fixed location to climb it, not merely within reach.
  1
  (?agent agent ?ladder ladder)
  (and (bind (has-location ?agent $a-location))
       (not (bind (on ?agent $anyplace)))
       (bind (has-position ?ladder $ladder-location))
       (eql $a-location $ladder-location)
       (bind (traversable> $a-location $means $dest))
       (member ?ladder $means)
       (one-way-clear ?agent $means))
  (":" ?agent "at" $a-location "uses" ?ladder "at" $ladder-location "to go to" $dest)
  (assert (has-location ?agent $dest)
          (finally (propagate-changes!))))

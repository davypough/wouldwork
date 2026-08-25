;;; Filename: box.lisp

;;; Box technology: a carryable, stackable support.  An agent can pick up a box within the
;;; fixed vertical reach limit and put a held box on reachable ground, a plate, or a clear
;;; box top.  A box
;;; is movable cargo that occupies its support via the (on ...) relation; it has no derived
;;; state of its own.  Agent jumping and support changes belong to the separate jump technology.
;;;
;;; REQUIRES:
;;;   types     : agent, location; plate comes from nested -plate-types and box is
;;;               declared optional here
;;;   nested    : -propagation (propagate-changes!, the master driver this file's effects
;;;               call);
;;;               -placement (placement-options, place-held-object!, reach policy, and
;;;               vertical/support geometry);
;;;               -reachability (identity-default reachable, overridden by reachability);
;;;               -pickup (pickup-clear, shared with jammer and beam-relay)
;;;   driver    : propagate-changes! (master), nested above rather than assumed from a peer
;;; PROVIDES:
;;;   types     : box -- declared optional here
;;;   actions   : pickup-box, put-box

(include-tech -propagation)
(include-tech -placement)
(include-tech -reachability)
(include-tech -pickup)

(in-package :ww)


(define-optional-types box)


(define-action pickup-box
  1
  (?agent agent ?box box)
  (and (bind (has-location ?agent $a-location))
       (bind (has-location ?box $box-location))
       (cleartop ?box)
       (pickup-clear ?agent $a-location ?box $box-location))
  (">" ?agent "picks up" ?box "at" $box-location "from" $a-location)
  (assert (holding ?agent ?box)
          (not (has-location ?box $box-location))
          (if (bind (on ?box $support))
            (not (on ?box $support)))
          (finally (propagate-changes!))))


(define-action put-box
  ;; Place a held box on the ground or on a clear support at a reachable location (including
  ;; the agent's own): one successor per legal placement-options result.  Each destination is
  ;; gated by manual reach -- its resting level may be no more than
  ;; *VERTICAL-REACH-LIMIT* above the agent's own base; lower placements are unrestricted.
  1
  (?agent agent ?box box ?location location)
  (and (holding ?agent ?box)
       (bind (has-location ?agent $a-location))
       (reachable ?location $a-location)
       (assign $places (placement-options ?agent ?location ?box)))
  (">" ?agent "puts" ?box "on" $place "at" ?location)
  (ww-loop for $placement-option in $places
           do (assert (assign $place $placement-option)
                      (place-held-object! ?agent ?box ?location $placement-option)
                      (finally (propagate-changes!)))))

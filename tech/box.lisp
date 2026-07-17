;;; Filename: box.lisp

;;; Box technology: a carryable, stackable support.  An agent can pick up a box within its own
;;; height of reach and put a held box on reachable ground, a plate, or a clear box top.  A box
;;; is movable cargo that occupies its support via the (on ...) relation; it has no derived
;;; state of its own.  Agent jumping and support changes belong to the separate jump technology.
;;;
;;; REQUIRES:
;;;   types     : agent, location  --  plate and box are declared optional here
;;;               (define-optional-types), so a problem lacking either need not declare it
;;;   nested    : -placement (placement-options, place-held-object!; also brings in
;;;               support occupancy, location, position, height, elevation, and holding);
;;;               -reachability (identity-default reachable, overridden by reachability);
;;;               -pickup (pickup-clear, shared with jammer and beam-relay)
;;;   driver    : propagate-changes! (master)
;;; PROVIDES:
;;;   types     : plate, box  --  declared optional here; other techs (plate, gate, jammer,
;;;               barrier, beam-relay, etc.) still declare their own plate-alias/box-alias
;;;               forms for their own pre-params; the bare and aliased forms resolve compatibly
;;;   actions   : pickup-box, put-box

(include-tech -placement)
(include-tech -reachability)
(include-tech -pickup)

(in-package :ww)


(define-optional-types plate box)


(define-action pickup-box
  1
  (?agent agent ?box box)
  (and (bind (has-location ?agent $a-location))
       (bind (has-location ?box $box-location))
       (cleartop ?box)
       (pickup-clear ?agent $a-location ?box $box-location))
  (":" ?agent "picks up" ?box "at" $box-location "from" $a-location)
  (assert (holding ?agent ?box)
          (not (has-location ?box $box-location))
          (if (bind (on ?box $support))
            (not (on ?box $support)))
          (finally (propagate-changes!))))


(define-action put-box
  ;; Place a held box on the ground or on a clear support at a reachable location (including
  ;; the agent's own): one successor per legal placement-options result.  Each destination is
  ;; gated by manual reach -- its resting level must be within the agent's height
  ;; (declared-height) of the agent's own level.
  1
  (?agent agent ?box box ?location location)
  (and (holding ?agent ?box)
       (bind (has-location ?agent $a-location))
       (reachable ?location $a-location)
       (assign $places (placement-options ?agent ?location ?box)))
  (":" ?agent "puts" ?box "on" $place "at" ?location)
  (ww-loop for $placement-option in $places
           do (assert (assign $place $placement-option)
                      (place-held-object! ?agent ?box ?location $placement-option)
                      (finally (propagate-changes!)))))

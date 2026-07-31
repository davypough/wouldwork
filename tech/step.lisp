;;; Filename: step.lisp

;;; Step technology: ground-level mounting and dismounting of flush supports (plates and
;;; gears-mounted fans).  A steppable's top sits exactly at its location's floor elevation,
;;; so stepping involves no elevation change -- that is what distinguishes step-on/step-off
;;; from the jump technology, which handles exclusively elevation-related support changes
;;; (box tops) and authored jump edges.  A fan is steppable only while mounted on gears: a
;;; fan lying on the ground or resting on a box top cannot be stepped on (nor jumped to).
;;; Stepping on a plate depresses it (plate's update-plate-status! derives depression from
;;; cleartop), so an agent can hold a gate or gears control active with its own weight;
;;; stepping on a fan whose gears are turning launches the agent to the gears' aimed-at>
;;; destination during the ensuing propagation.
;;;
;;; REQUIRES:
;;;   types     : agent, location  --  plate and fan are declared optional here
;;;               (define-optional-types)
;;;   nested    : -support-occupancy ((on ...), cleartop); -location ((has-location ...));
;;;               -position ((has-position ...))
;;;   conditional relations:
;;;               mounted-on (fan), guarded by fan  --  owned by gears-fan.lisp;
;;;               translation removes the guarded reference when the fan type is empty
;;;   driver    : propagate-changes! (master)
;;; PROVIDES:
;;;   types     : steppable (either plate fan)
;;;   actions   : step-on, step-off

(include-tech -support-occupancy)
(include-tech -location)
(include-tech -position)

(in-package :ww)


(define-optional-types plate fan)


(define-types
  steppable (either plate fan))


(define-action step-on
  ;; Step from ground onto a clear steppable fixture at the agent's own location.  A plate
  ;; is fixed (has-position); a fan is movable (has-location) and qualifies only while it
  ;; is mounted on gears, which keeps its top flush with the floor.
  1
  (?agent agent ?fixture steppable)
  (and (bind (has-location ?agent $a-location))
       (not (bind (on ?agent $anyplace)))
       (or (and (plate ?fixture)
                (bind (has-position ?fixture $f-location)))
           (and (fan ?fixture)
                (bind (mounted-on ?fixture $gears))
                (bind (has-location ?fixture $f-location))))
       (eql $a-location $f-location)
       (cleartop ?fixture))
  (">" ?agent "at" $a-location "steps onto" ?fixture)
  (assert (on ?agent ?fixture)
          (finally (propagate-changes!))))


(define-action step-off
  ;; Step from a steppable fixture back onto ground at the same location.  Box dismounts
  ;; belong to jump (a drop); an agent on a fan implies the fan is gears-mounted (step-on is
  ;; the only mount, and a fan cannot be picked up while occupied), and a launched agent is
  ;; already off its fan, so step-off from a blowing fan never arises.
  1
  (?agent agent)
  (and (bind (on ?agent $fixture))
       (steppable $fixture)
       (bind (has-location ?agent $a-location)))
  (">" ?agent "at" $a-location "steps off" $fixture)
  (assert (not (on ?agent $fixture))
          (finally (propagate-changes!))))

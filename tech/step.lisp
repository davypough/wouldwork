;;; Filename: step.lisp

;;; Step technology: provide ground-level mounting and dismounting transitions for flush
;;; supports (plates and
;;; gears-mounted fans).  A steppable's top sits exactly at its location's floor elevation,
;;; so stepping involves no elevation change -- that is what distinguishes step transitions
;;; from the jump technology, which handles exclusively elevation-related support changes
;;; (box tops) and authored jump edges.  A fan is steppable only while mounted on gears: a
;;; fan lying on the ground or resting on a box top cannot be stepped on (nor jumped to).
;;; Stepping on a plate depresses it (plate's update-plate-status! derives depression from
;;; cleartop), so an agent can hold a gate or gears control active with its own weight;
;;; stepping on a fan whose gears are turning launches the agent to the gears' aimed-at
;;; destination during the ensuing propagation.
;;;
;;; REQUIRES:
;;;   types     : agent, location; plate comes from nested -plate-types and fan is
;;;               declared optional here
;;;   nested    : -mobility-action (central MOVE action, configuration representation,
;;;               transition registry, support mutation, and propagation);
;;;               -position ((has-position ...))
;;;   conditional relations:
;;;               mounted-on (fan), guarded by fan  --  owned by -gears-fan.lisp;
;;;               translation removes the guarded reference when the fan type is empty
;;; PROVIDES:
;;;   types     : steppable-object (either pressure-plate toggle-plate fan)
;;;   queries   : step-source-can-mount, step-source-can-dismount,
;;;               steppable-fixture-at, step-configuration-transitions
;;;   provider  : step-configuration-transitions registered with
;;;               -configuration-transition
;;;   action    : move (from -mobility-action)

(include-tech -mobility-action)
(include-tech -position)

(in-package :ww)


(define-optional-types fan)


(define-types
  steppable-object (either pressure-plate toggle-plate fan))


(define-query step-source-can-mount (?source-place)
  (eql ?source-place 'ground))


(define-query step-source-can-dismount (?source-place)
  (and (not (eql ?source-place 'ground))
       (steppable-object ?source-place)))


(define-query steppable-fixture-at
    (?fixture steppable-object ?location location)
  ;; A plate is fixed.  A fan is steppable only while attached to gears and carrying a
  ;; floor location; a wall-mounted fan has no location and therefore cannot match.
  (or (and (plate ?fixture)
           (has-position ?fixture ?location))
      (and (fan ?fixture)
           (bind (mounted-on ?fixture $gears))
           (has-location ?fixture ?location))))


(define-query step-configuration-transitions (?agent agent ?source-configuration)
  (do (assign $location (first ?source-configuration))
      (assign $source-place (second ?source-configuration))
      (assign $transitions nil)
      (if (step-source-can-mount $source-place)
        (doall (?fixture steppable-object)
          (if (and (steppable-fixture-at ?fixture $location)
                   (cleartop ?fixture)
                   (support-use-allowed ?agent ?fixture))
            (assign $transitions
                    (cons
                      (list 'step ?source-configuration nil
                            (list $location ?fixture))
                      $transitions)))))
      (if (step-source-can-dismount $source-place)
        (assign $transitions
                (cons
                  (list 'step ?source-configuration nil
                        (list $location 'ground))
                  $transitions)))
      $transitions))


(register-configuration-transition-provider
  'step-configuration-transitions)

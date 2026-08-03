;;; Filename: jammer.lisp

;;; Jammer technology: a carried jammer that, when placed at a location with line of sight
;;; to a target, jams it.  Jamming forces a gate open (gate's update-gate-status!), forces
;;; gears stopped (-gears-fan's update-gears-status!), and forces a gun safe (gun's
;;; update-gun-status!) -- the same override with opposite polarity in each case: a jam
;;; always disables the barrier or threat.  A placed jammer is movable cargo: it may rest
;;; on a plate (depressing it) or a clear box top, and picking it up clears both its
;;; jamming and its support.
;;;
;;; REQUIRES (supplied by other techs):
;;;   types     : agent, location; plate comes from nested -plate-types, while jammer
;;;               and box are declared optional here
;;;   nested    : -placement (placement-options, place-held-object!; also brings in
;;;               support occupancy, location, position, height, elevation, and holding);
;;;               -reachability (identity-default reachable, overridden by reachability);
;;;               -visibility (null-default visible interface); -pickup (pickup-clear,
;;;               shared with box and beam-relay)  --  all shared via nested include-tech
;;;               rather than local declaration
;;;   extension : visibility overrides -visibility's null default with authored LOS
;;;   driver    : propagate-changes! (master); (jamming ...) is consumed by gate's
;;;               update-gate-status!
;;; PROVIDES:
;;;   types     : jammer, box -- declared optional here
;;;               target (either gate floor-gears wall-gears gun)  --  what a jammer can
;;;               jam; connector pairings use beam-relay's terminus instead
;;;   relations : (jamming jammer $target)
;;;               (jam-disallowed> location location target)
;;;   actions   : pickup-jammer, jam-target

(include-tech -placement)
(include-tech -reachability)
(include-tech -visibility)
(include-tech -pickup)

(in-package :ww)


(define-types
  target (either gate floor-gears wall-gears gun))  ;what a jammer can jam: a gate (forced open), gears (forced stopped), or a gun (forced safe); connector pairings use terminus


(define-optional-types jammer box floor-gears wall-gears gun)


(define-dynamic-relations
  (jamming jammer $target))


(define-static-relations
  (jam-disallowed> location location target))  ;agent location, jammer placement, target; directional


(define-action pickup-jammer
  1
  (?agent agent ?jammer jammer)
  (and (bind (has-location ?agent $a-location))
       (bind (has-location ?jammer $jammer-location))
       (pickup-clear ?agent $a-location ?jammer $jammer-location))
  (">" ?agent "picks up" ?jammer "at" $a-location)
  (assert (holding ?agent ?jammer)
          (not (has-location ?jammer $jammer-location))
          (if (bind (jamming ?jammer $any-target))
            (not (jamming ?jammer $any-target)))
          (if (bind (on ?jammer $support))
            (not (on ?jammer $support)))
          (finally (propagate-changes!))))


(define-action jam-target
  1
  (?agent agent ?target target ?location location)
  (and (bind (holding ?agent $any-jammer))
       (jammer $any-jammer)
       (bind (has-location ?agent $a-location))
       (reachable ?location $a-location)
       ;; A gate is an extended segment with its own derived LOS-TO-TARGET sightlines; a
       ;; gun is a point fixture with LOS-TO-APPARATUS entries instead, exactly like a
       ;; transmitter or receiver -- both resolve through visible with no HAS-POSITION
       ;; shortcut, since nothing can ever share a gun's position.  Gears hang at their
       ;; HAS-POSITION location, so their sightline resolves through that location's
       ;; ordinary LOS-TO-LOCATION entry instead -- or trivially when the jammer is placed
       ;; at the gears' own location.
       (or (and (or (gate ?target) (gun ?target))
                (visible ?location ?target))
           (and (or (floor-gears ?target) (wall-gears ?target))
                (bind (has-position ?target $t-location))
                (or (eql ?location $t-location)
                    (visible ?location $t-location))))
       (not (jam-disallowed> $a-location ?location ?target))
       (assign $places (placement-options ?agent ?location $any-jammer)))
  (">" ?agent "jams" ?target "with" $any-jammer "at" ?location "on" $place)
  (ww-loop for $placement-option in $places
           do (assert (assign $place $placement-option)
                      (jamming $any-jammer ?target)
                      (place-held-object! ?agent $any-jammer ?location $placement-option)
                      (finally (propagate-changes!)))))

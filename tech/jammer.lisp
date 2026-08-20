;;; Filename: jammer.lisp

;;; Jammer technology: a carried jammer that, when placed at a location with line of sight
;;; to a target, jams it.  Jamming forces a gate open (gate's update-gate-status!), forces
;;; blower drives stopped (-gears-fan's update-blower-status!), and forces a gun safe (gun's
;;; update-gun-status!) -- the same override with opposite polarity in each case: a jam
;;; always disables the barrier or threat.  A placed jammer is movable cargo: it may rest
;;; on a plate (depressing it) or a clear box top, and picking it up clears both its
;;; jamming and its support.  Put-jammer places it inertly -- no target required --
;;; exactly mirroring put-connector's relationship to connect-connector; jam-target is
;;; the active variant that also establishes jamming.
;;;
;;; REQUIRES (supplied by other techs):
;;;   types     : agent, location; plate comes from nested -plate-types, while jammer
;;;               and box are declared optional here
;;;   nested    : -placement (placement-options, place-held-object!; also brings in
;;;               support occupancy, location, position, height, elevation, and holding);
;;;               -reachability (identity-default reachable, overridden by reachability);
;;;               -visibility (null-default elevation-visible-for-object interface); -pickup (pickup-clear,
;;;               shared with box and beam-relay)  --  all shared via nested include-tech
;;;               rather than local declaration
;;;   extension : visibility overrides -visibility's null default with authored LOS
;;;   driver    : propagate-changes! (master); (jamming ...) is consumed by gate's
;;;               update-gate-status!
;;; PROVIDES:
;;;   types     : jammer, box -- declared optional here
;;;               target (either gate floor-gears wall-gears floor-blower wall-blower gun)
;;;               -- what a jammer can
;;;               jam; connector pairings use beam-relay's terminus instead
;;;   relations : (jamming jammer $target)
;;;               (jam-disallowed> location location target)
;;;   queries   : jammer-target-elevation, jammer-target-visible-from-placement,
;;;               jammer-visible-placement-options
;;;   actions   : pickup-jammer, put-jammer, jam-target

(include-tech -vertical)
(include-tech -placement)
(include-tech -reachability)
(include-tech -visibility)
(include-tech -pickup)
(include-tech -recorder-fork-registry)

(in-package :ww)


(define-types
  target (either gate floor-gears wall-gears floor-blower wall-blower gun))  ;what a jammer can jam: a gate, blower drive, or gun; connector pairings use terminus


(define-optional-types
  jammer box floor-gears wall-gears floor-blower wall-blower gun)


(define-dynamic-relations
  (jamming jammer $target))


;; JAMMING's contribution to the recorder's ghost fork, registered here because this file
;; owns the relation.  The target (gate/wall-gears/wall-blower) is never itself mapped.
(register-recorder-fork-clause 'jamming
  '(doall (?live jammer)
     (if (bind (recording-copy> ?live $ghost))
       (if (bind (jamming ?live $target))
         (jamming $ghost $target)))))


(define-static-relations
  (jam-disallowed> location location target))  ;agent location, jammer placement, target; directional


(define-query jammer-target-elevation (?target target)
  ;; Point fixtures use their functional elevation.  Extended gates are aimed at their
  ;; vertical midpoint.  Gears use the same working level as their mounted fan.
  (if (gate ?target)
    (+ (object-elevation ?target) (/ (object-height ?target) 2))
    (if (gun ?target)
      (fixture-elevation ?target)
      (if (or (wall-gears ?target)
              (wall-blower ?target))
        (if (bind (has-elevation ?target $level)) $level 1)
        (do (bind (has-position ?target $location))
            (location-elevation $location))))))


(define-query jammer-target-visible-from-placement
    (?view ?location location ?place ?jammer jammer ?target target)
  (do (assign $jammer-elevation
              (+ (placement-elevation ?location ?place)
                 (object-height ?jammer)))
      (assign $target-elevation (jammer-target-elevation ?target))
      (if (or (gate ?target) (gun ?target))
        (elevation-visible-for-object
          ?view ?location $jammer-elevation ?target $target-elevation)
        (do (bind (has-position ?target $target-location))
            (or (eql ?location $target-location)
                (elevation-visible-for-object
                  ?view ?location $jammer-elevation
                  $target-location $target-elevation))))))


(define-query jammer-visible-placement-options
    (?view ?location location ?jammer jammer ?target target ?places)
  ;; Preserve PLACEMENT-OPTIONS' ordering while discarding supports that leave the jammer's
  ;; top sight point too low for this target.
  (do (assign $visible-places nil)
      (ww-loop for $placement-option in ?places
               do (if (jammer-target-visible-from-placement
                        ?view ?location $placement-option ?jammer ?target)
                    (assign $visible-places
                            (cons $placement-option $visible-places))))
      (nreverse $visible-places)))


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


(define-action put-jammer
  1
  (?agent agent ?jammer jammer ?location location)
  (and (holding ?agent ?jammer)
       (bind (has-location ?agent $a-location))
       (reachable ?location $a-location)
       (assign $places (placement-options ?agent ?location ?jammer)))
  (">" ?agent "puts" ?jammer "on" $place "at" ?location "without jamming")
  (ww-loop for $placement-option in $places
           do (assert (assign $place $placement-option)
                      (place-held-object! ?agent ?jammer ?location $placement-option)
                      (finally (propagate-changes!)))))


(define-action jam-target
  1
  (?agent agent ?target target ?location location)
  (and (bind (holding ?agent $any-jammer))
       (jammer $any-jammer)
       (bind (has-location ?agent $a-location))
       (reachable ?location $a-location)
       (not (jam-disallowed> $a-location ?location ?target))
       (assign $places (placement-options ?agent ?location $any-jammer))
       (assign $visible-places
               (jammer-visible-placement-options
                 ?agent ?location $any-jammer ?target $places))
       (not (null $visible-places)))
  (">" ?agent "jams" ?target "with" $any-jammer "at" ?location "on" $place)
  (ww-loop for $placement-option in $visible-places
           do (assert (assign $place $placement-option)
                      (jamming $any-jammer ?target)
                      (place-held-object! ?agent $any-jammer ?location $placement-option)
                      (finally (propagate-changes!)))))

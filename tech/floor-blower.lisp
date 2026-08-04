;;; Filename: floor-blower.lisp

;;; Floor-blower technology: the floor mounting of the shared gears/fan machinery (see
;;; -gears-fan, which owns the types, the mounted-on attachment, aimed-at, control
;;; wiring, the turning/blowing derivation, and the fan actions).  Floor gears are a
;;; flush floor fixture; their mounted fan is a floor object with a has-location, so it
;;; is steppable (step.lisp) and a placement target (-placement), its zero-thickness top
;;; sitting flush with the floor.
;;;
;;; While the fan blows, every occupant resting on it -- agent, box, jammer, or
;;; connector -- is launched to the gears' aimed-at destination in the air, a box's
;;; stack riding along still stacked.  The launched occupants hover at the destination,
;;; sustained only by the air stream: when no blowing floor-mounted fan aims at the
;;; destination any longer (the gears stop turning, or the fan is lifted off), every
;;; occupant hovering there falls to the ground at the gears' own location, landing on
;;; nothing.  A jamming jammer or a paired connector stays jamming/paired through launch
;;; and fall alike: its effect is re-derived by propagation from wherever it ends up,
;;; never retracted by the ride.  Only another fan is too flat to catch the stream: it
;;; is merely toppled off the fan's top onto the ground at the fan's own location.  The
;;; destination is an ordinary location whose floor elevation defaults to 10 (the
;;; in-the-air hover level, via this file's location-elevation override); a problem
;;; overrides that by declaring the destination's has-elevation fact.
;;;
;;; An agent mounts a floor-mounted fan via step technology's step-on and is launched by
;;; the ensuing propagation when the gears are turning.  The return from the lofted
;;; destination is an authored downward jump edge (jump technology, whose downward
;;; landings are unrestricted), or simply cutting the gears' power, which drops the
;;; hovering occupants at the gears' location.
;;;
;;; REQUIRES:
;;;   types     : agent, location  --  floor-gears and fan come from nested -gears-fan
;;;   nested    : -gears-fan (types, mounted-on, aimed-at, turning/blowing,
;;;               gears-elevation, update-gears-status!, relocate-stack!, fan actions;
;;;               nests -support-occupancy, -location, -position, -elevation, -controls,
;;;               -placement, -reachability, and -pickup)
;;;   driver    : the master propagate-consequences! must call
;;;               update-floor-blower-status! after update-gears-status!
;;; PROVIDES:
;;;   query     : location-elevation  --  overrides -elevation's version: an undeclared
;;;               location that is some floor-gears' aimed-at destination floats at
;;;               default elevation 10 instead of 0
;;;   updates   : update-floor-blower-status!, blow-occupants-away!, drop-occupants!

(include-tech -propagation)
(include-tech -gears-fan)

(in-package :ww)


(define-query location-elevation (?location location)
  ;; Overrides -elevation's ground default of 0: an undeclared location that is some
  ;; floor-gears' aimed-at destination floats at the default in-the-air hover level of
  ;; 10.  A declared has-elevation fact always wins.  The override is safe because
  ;; include-tech splices each file once, so no later include restores -elevation's
  ;; version.  Wall-gears destinations are ordinary ground locations and take no part.
  (if (bind (has-elevation ?location $level))
    $level
    (if (exists (?g floor-gears)
          (and (bind (aimed-at ?g $dest))
               (eql $dest ?location)))
      10
      0)))


(define-update update-floor-blower-status! ()
  ;; Floor-mounting consequences of the blowing state that update-gears-status! derived.
  ;; Pass 1: every occupant resting on a blowing floor-mounted fan is launched, which
  ;; empties the fan's top, so the fixpoint terminates.  Pass 2: a destination no blowing
  ;; floor-mounted fan aims at has lost its supporting air stream, so every agent or box
  ;; hovering there falls to the ground at the gears' location.  Change detection is
  ;; automatic, so an unchanged re-assert is silent.
  (do (doall (?f fan)
        (if (and (blowing ?f)
                 (bind (mounted-on ?f $gears))
                 (floor-gears $gears))
          (blow-occupants-away! ?f $gears)))
      (doall (?g floor-gears)
        (do (bind (aimed-at ?g $destination))
            (if (not (exists (?f fan)
                       (and (blowing ?f)
                            (bind (mounted-on ?f $f-gears))
                            (floor-gears $f-gears)
                            (bind (aimed-at $f-gears $f-destination))
                            (eql $f-destination $destination))))
              (drop-occupants! ?g $destination))))))


(define-update blow-occupants-away! (?fan fan ?gears floor-gears)
  ;; Launch every occupant resting on ?fan -- agent, box, jammer, or connector -- to
  ;; ?gears' aimed-at destination via relocate-stack!, each box's riders traveling still
  ;; stacked.  A jamming jammer or a paired connector stays jamming/paired through the
  ;; ride: its effect is re-derived by propagation from the destination, not retracted
  ;; here.  Only another fan is too flat to catch the stream: it is merely toppled off
  ;; the fan's top and rests on ground at the fan's own location, which is already its
  ;; has-location.  An armed gun (or other threat) at the destination is not this file's
  ;; concern: -threat's enforce-threat-safety! backstop drops the whole resulting state if
  ;; the ride lands the agent somewhere unsafe, so the physics here stay unconditional.
  (do (bind (aimed-at ?gears $destination))
      (doall (?x support-occupant)
        (if (on ?x ?fan)
          (do (not (on ?x ?fan))
              (if (not (fan ?x))
                (relocate-stack! ?x $destination)))))))


(define-update drop-occupants! (?gears floor-gears ?destination location)
  ;; The air stream sustaining ?destination has stopped: every occupant hovering there
  ;; falls to the ground at ?gears' own location, landing on nothing; a jamming jammer or
  ;; paired connector stays jamming/paired through the fall, its effect re-derived by
  ;; propagation from where it lands.  Only stack bases (occupants not on any support)
  ;; fall directly; everything stacked on a falling box rides along, still stacked, via
  ;; relocate-stack!.
  (do (bind (has-position ?gears $g-location))
      (doall (?x support-occupant)
        (if (and (not (fan ?x))
                 (bind (has-location ?x $x-location))
                 (eql $x-location ?destination)
                 (not (bind (on ?x $support))))
          (relocate-stack! ?x $g-location)))))

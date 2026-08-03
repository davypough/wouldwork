;;; Filename: angled-blower.lisp

;;; Angled-blower technology: the angled mounting of the shared gears/fan machinery (see
;;; -gears-fan, which owns the types, the mounted-on attachment, aimed-at>, control
;;; wiring, the turning/blowing derivation, and the fan actions).  Angled gears are a
;;; flush floor fixture, exactly like floor-gears: their mounted fan is a floor object
;;; with a has-location, so it is steppable (step.lisp) and a placement target
;;; (-placement), its zero-thickness top sitting flush with the floor.
;;;
;;; While the fan blows, its air stream launches every occupant resting on it -- agent,
;;; box, jammer, or connector -- along a 45-degree parabolic arc to the gears' aimed-at>
;;; destination, a box's stack riding along still stacked.  Unlike floor-blower's vertical
;;; launch, this is a one-shot delivery, not a sustained hover: the arc clears any wall
;;; standing between pad and destination (no obstacle-clear/passability check is
;;; modeled for it -- the whole point of the arc is that it flies over what a horizontal
;;; stream could not), and once the object lands it stays landed, even if the gears later
;;; stop turning or the fan is lifted away.  land-on-support! rests it on the first
;;; landing-support match at the destination -- a plate, a floor-mounted fan, or a box,
;;; at whatever height its top happens to be, since an arc (unlike wall-blower's flush
;;; horizontal delivery) can terminate at any elevation -- or on bare ground if none is
;;; clear.  If that support is itself a fan mounted on turning gears, the object is
;;; launched onward in a later propagation pass: blowers chain automatically through the
;;; shared fixpoint.
;;; A jamming jammer or a paired connector stays jamming/paired through the ride; its
;;; effect is re-derived by propagation from wherever it lands, never retracted here.
;;; Only another fan is too flat to catch the stream: it is merely toppled off the fan's
;;; top and rests on ground at the fan's own location, which is already its has-location.
;;;
;;; An agent mounts an angled-mounted fan via step technology's step-on, exactly like
;;; floor-blower, and is launched by the ensuing propagation when the gears are turning.
;;;
;;; Authoring obligation: aimed-at> destinations must not chain into a cycle across
;;; simultaneously-blowing angled (or wall) gears, directly or by landing on another
;;; blower's fan, or propagation's iteration cap trips inconsistent-state.  Acyclic chains
;;; are fine and settle within the cap (10 iterations).
;;;
;;; REQUIRES:
;;;   types     : agent, location  --  angled-gears and fan come from nested -gears-fan
;;;   nested    : -gears-fan (types, mounted-on, aimed-at>, turning/blowing,
;;;               gears-elevation, landing-support, land-on-support!,
;;;               update-gears-status!, relocate-stack!, fan actions; nests
;;;               -support-occupancy, -location, -position, -elevation, -controls,
;;;               -placement, -reachability, and -pickup)
;;;   driver    : the master propagate-consequences! must call
;;;               update-angled-blower-status! after update-gears-status!
;;; PROVIDES:
;;;   updates   : update-angled-blower-status!, arc-occupants-away!

(include-tech -propagation)
(include-tech -gears-fan)

(in-package :ww)


(define-update update-angled-blower-status! ()
  ;; Angled-mounting consequences of the blowing state that update-gears-status! derived:
  ;; every blowing angled-mounted fan launches its occupants along the arc.  Unlike
  ;; floor-blower, there is no second pass -- a landed object does not depend on the
  ;; stream continuing, so nothing needs to fall when the gears stop.  Change detection is
  ;; automatic, so an unchanged re-assert is silent.
  (doall (?f fan)
    (if (and (blowing ?f)
             (bind (mounted-on ?f $gears))
             (angled-gears $gears))
      (arc-occupants-away! ?f $gears))))


(define-update arc-occupants-away! (?fan fan ?gears angled-gears)
  ;; Launch every occupant resting on ?fan -- agent, box, jammer, or connector -- to
  ;; ?gears' aimed-at> destination via relocate-stack!, each box's riders traveling still
  ;; stacked.  land-on-support! then rests the base on its destination's landing-support
  ;; match, unconstrained by elevation, or leaves it on bare ground if none is clear.  A
  ;; jamming jammer or a paired connector stays jamming/paired through the ride: its
  ;; effect is re-derived by propagation from the destination, not retracted here.  Only
  ;; another fan is too flat to catch the stream: it is merely toppled off the fan's top
  ;; and rests on ground at the fan's own location, which is already its has-location.  An
  ;; armed gun (or other threat) at the destination is not this file's concern: -threat's
  ;; enforce-threat-safety! backstop drops the whole resulting state if the arc lands the
  ;; agent somewhere unsafe, so the physics here stay unconditional.
  (do (bind (aimed-at> ?gears $destination))
      (doall (?x support-occupant)
        (if (on ?x ?fan)
          (do (not (on ?x ?fan))
              (if (not (fan ?x))
                (do (relocate-stack! ?x $destination)
                    (land-on-support! ?x $destination nil))))))))

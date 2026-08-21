;;; Filename: wall-blower.lisp

;;; Wall-blower technology: the wall mounting of the shared gears/fan machinery (see
;;; -gears-fan, which owns the types, the mounted-on attachment, aimed-at, control
;;; wiring, the turning/blowing derivation, and the fan actions).  Wall gears are fixed
;;; on a wall of their has-position location -- the location the fan faces and sweeps --
;;; at the stream elevation given by blower-elevation (declared has-elevation or 1).  A
;;; wall-mounted fan hangs with NO has-location, so nothing can stand or rest on it and
;;; it is invisible to step and placement; the agent mounts and dismounts it with
;;; -gears-fan's mount-fan and pickup-fan, reaching to the stream elevation.  A fixed
;;; wall-blower is the complete wall fixture with no separate fan identity.
;;;
;;; While the fan's gears turn in an object's environmental view, its air stream sweeps
;;; the faced location horizontally at the stream elevation: an object standing at
;;; base elevation s is blown iff
;;; s < stream <= top -- the stream must strike its body.  With unit heights, gears at
;;; elevation 1 blow anything standing on the floor, while gears at elevation 2 pass
;;; over the floor and blow only objects standing at elevation 1 (e.g. on a box top).  A
;;; blown object is torn off whatever support it rests on, relocates to the gears'
;;; aimed-at destination (an ordinary ground location -- no hover), and carries its own
;;; stacked riders along; an agent's held cargo travels with it implicitly.  If a clear
;;; plate or floor-mounted fan sits at the destination -- its top exactly at the
;;; destination's own floor elevation, unlike a box's raised top -- the object lands on
;;; it instead of bare ground, so a plate there depresses and a fan there (if its own
;;; gears are turning) launches the object onward in a later propagation pass.  A jamming
;;; jammer or a paired connector stays jamming/paired through the ride: its effect is
;;; re-derived by propagation from the destination, not retracted here.  A fan is a
;;; zero-thickness disc, so a loose fan lying flat sits below every stream and stays
;;; put, and a mounted fan is likewise never blown.
;;; Because the sweep runs during propagation, moving into the faced location at stream
;;; level while the fan blows means immediate transport back out: a blowing wall fan
;;; makes its faced location impossible to occupy at that level, though traffic below
;;; (or above) the stream passes freely.
;;;
;;; Authoring obligations: every drive names its faced/swept location with HAS-POSITION
;;; and its delivery location with AIMED-AT; -gears-fan's shared init check rejects either
;;; missing endpoint.  Coordinate-known endpoints are axis-aligned on one floor level: the
;;; modeled wall stream is horizontal, and its absolute stream elevation must be strictly
;;; above that floor.  A destination graph may contain a cycle when controls or empty
;;; streams keep it inert.  What must not occur is an active occupant-transport loop through
;;; simultaneously blowing fans -- directly, or indirectly by landing on another blower's
;;; fan.  That state never converges, so propagation's iteration cap marks it inconsistent
;;; and search discards it.  Active acyclic chains (one fan blowing into another's swept
;;; location, or landing on another fan's flush top) are fine and settle within the cap.
;;;
;;; REQUIRES:
;;;   types     : agent, location  --  wall-gears and fan come from nested -gears-fan
;;;   nested    : -propagation (derived-state driver);
;;;               -vertical (base, top, location-elevation);
;;;               -gears-fan (types, mounted-on, aimed-at, turning/blowing,
;;;               blower-elevation, landing-support, land-on-support!,
;;;               update-blower-status!, relocate-stack!, fan actions; nests
;;;               -support-occupancy, -location, -position, -elevation, -controls,
;;;               -placement, -reachability, and -pickup); -stream-passability
;;;               (obstacle-clear's gears branch and the derived air-stream walking
;;;               bands, with stream-width's 3-unit default)
;;;   driver    : the master propagate-consequences! must call
;;;               update-wall-blower-status! after update-blower-status! and any
;;;               recording-side gears derivation
;;; PROVIDES:
;;;   updates   : update-wall-blower-status!, sweep-occupants-away!

(include-tech -propagation)
(include-tech -vertical)
(include-tech -gears-fan)
(include-tech -stream-passability)

(in-package :ww)


(define-update update-wall-blower-status! ()
  ;; Every mounted wall fan offers its sweep.  SWEEP-OCCUPANTS-AWAY! decides separately
  ;; for each occupant whether these gears turn in that object's playback or recording
  ;; view.  This is necessary even when ordinary BLOWING is false: a recorder ghost may
  ;; still see the recording-side fan as active.
  ;; Sweeping removes every struck occupant from that location, so the fixpoint
  ;; terminates for acyclic destination chains.  Change detection is automatic, so an
  ;; unchanged re-assert is silent.
  (doall (?drive (either wall-gears wall-blower))
    (if (blower-present ?drive)
      (sweep-occupants-away! ?drive))))


(define-update sweep-occupants-away! (?drive (either wall-gears wall-blower))
  ;; Blow every occupant of ?gears' faced location whose body the stream strikes -- an
  ;; object standing at elevation $standing is blown iff
  ;; $standing < stream <= its TOP -- to the aimed-at destination.  A blown
  ;; object is torn off whatever support it rests on (plate, box, fan) and relocates via
  ;; relocate-stack!, its own stacked riders traveling still stacked; jamming and
  ;; connector-pairing facts persist through the ride, their effects re-derived by
  ;; propagation from the destination.  After relocating, land-on-support! rests the
  ;; object on its destination's landing-support match constrained to the destination's
  ;; own floor elevation (a plate, or a floor-mounted fan) -- a box's raised top never
  ;; matches, so it is never landed on here -- or leaves it on bare ground if none is
  ;; flush.  A fan is zero-thickness, so no fan (loose or mounted) is ever struck;
  ;; occupants standing below or above the stream stay put.  An armed gun (or other
  ;; threat) at the destination is not this file's concern: -threat's
  ;; enforce-threat-safety! backstop drops the whole resulting state if the sweep lands
  ;; the agent somewhere unsafe, so the physics here stay unconditional.
  (do (bind (has-position ?drive $swept))
      (bind (aimed-at ?drive $destination))
      (assign $stream (blower-elevation ?drive))
      (doall (?x support-occupant)
        (if (and (not (fan ?x))  ;a fan is zero-thickness: no stream ever strikes it
                 (bind (has-location ?x $x-location))
                 (eql $x-location $swept)
                 (blower-active-for-object ?x ?drive))
          (do (assign $standing (base ?x))
              (if (and (< $standing $stream)
                       (<= $stream (top ?x)))
                (do (if (bind (on ?x $support))
                      (not (on ?x $support)))
                    (relocate-stack! ?x $destination)
                    (land-on-support! ?x $destination (location-elevation $destination)))))))))

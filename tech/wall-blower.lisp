;;; Filename: wall-blower.lisp

;;; Wall-blower technology: the wall mounting of the shared gears/fan machinery (see
;;; -gears-fan, which owns the types, the mounted-on attachment, aimed-at>, control
;;; wiring, the turning/blowing derivation, and the fan actions).  Wall gears are fixed
;;; on a wall of their has-position location -- the location the fan faces and sweeps --
;;; at the stream elevation given by gears-elevation (declared has-elevation or 1).  A
;;; wall-mounted fan hangs with NO has-location, so nothing can stand or rest on it and
;;; it is invisible to step and placement; the agent mounts and dismounts it with
;;; -gears-fan's mount-fan and pickup-fan, reaching to the stream elevation.
;;;
;;; While the fan blows, its air stream sweeps the faced location horizontally at the
;;; stream elevation: an object standing at elevation s with height h is blown iff
;;; s < stream <= s + h -- the stream must strike its body.  With unit heights, gears at
;;; elevation 1 blow anything standing on the floor, while gears at elevation 2 pass
;;; over the floor and blow only objects standing at elevation 1 (e.g. on a box top).  A
;;; blown object is torn off whatever support it rests on, relocates to the gears'
;;; aimed-at> destination (an ordinary ground location -- no hover), and carries its own
;;; stacked riders along; an agent's held cargo travels with it implicitly.  A jamming
;;; jammer or a paired connector stays jamming/paired through the ride: its effect is
;;; re-derived by propagation from the destination, not retracted here.  A fan is a
;;; zero-thickness disc, so a loose fan lying flat sits below every stream and stays
;;; put, and a mounted fan is likewise never blown.
;;; Because the sweep runs during propagation, moving into the faced location at stream
;;; level while the fan blows means immediate transport back out: a blowing wall fan
;;; makes its faced location impossible to occupy at that level, though traffic below
;;; (or above) the stream passes freely.
;;;
;;; Authoring obligation: aimed-at> destinations must not chain the swept locations of
;;; simultaneously-blowing wall fans into a cycle, or propagation's iteration cap trips
;;; inconsistent-state.  Acyclic chains (one fan blowing into another's swept location)
;;; are fine and settle within the cap.
;;;
;;; REQUIRES:
;;;   types     : agent, location  --  wall-gears and fan come from nested -gears-fan
;;;   nested    : -gears-fan (types, mounted-on, aimed-at>, turning/blowing,
;;;               gears-elevation, update-gears-status!, relocate-stack!, fan actions;
;;;               nests -support-occupancy, -location, -position, -elevation, -controls,
;;;               -placement, -reachability, and -pickup)
;;;   driver    : the master propagate-consequences! must call
;;;               update-wall-blower-status! after update-gears-status!
;;; PROVIDES:
;;;   updates   : update-wall-blower-status!, sweep-occupants-away!

(include-tech -gears-fan)

(in-package :ww)


(define-update update-wall-blower-status! ()
  ;; Wall-mounting consequences of the blowing state that update-gears-status! derived:
  ;; every blowing wall-mounted fan sweeps its gears' faced location at stream level.
  ;; Sweeping removes every struck occupant from that location, so the fixpoint
  ;; terminates for acyclic destination chains.  Change detection is automatic, so an
  ;; unchanged re-assert is silent.
  (doall (?f fan)
    (if (and (blowing ?f)
             (bind (mounted-on ?f $gears))
             (wall-gears $gears))
      (sweep-occupants-away! $gears))))


(define-update sweep-occupants-away! (?gears)
  ;; Blow every occupant of ?gears' faced location whose body the stream strikes -- an
  ;; object standing at elevation $standing with height $height is blown iff
  ;; $standing < stream <= $standing + $height -- to the aimed-at> destination.  A blown
  ;; object is torn off whatever support it rests on (plate, box, fan) and relocates via
  ;; relocate-stack!, its own stacked riders traveling still stacked; jamming and
  ;; connector-pairing facts persist through the ride, their effects re-derived by
  ;; propagation from the destination.  A fan is zero-thickness, so no fan (loose or
  ;; mounted) is ever struck; occupants standing below or above the stream stay put.
  (do (bind (has-position ?gears $swept))
      (bind (aimed-at> ?gears $destination))
      (assign $stream (gears-elevation ?gears))
      (doall (?x support-occupant)
        (if (and (not (fan ?x))  ;a fan is zero-thickness: no stream ever strikes it
                 (bind (has-location ?x $x-location))
                 (eql $x-location $swept))
          (do (assign $standing (occupant-elevation ?x))
              (assign $height (declared-height ?x))
              (if (and (< $standing $stream)
                       (<= $stream (+ $standing $height)))
                (do (if (bind (on ?x $support))
                      (not (on ?x $support)))
                    (relocate-stack! ?x $destination))))))))

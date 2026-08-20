;;; Filename: -floor-blowing.lisp

;;; Shared floor-blowing behavior for both forms of floor-directed blower:
;;;
;;;   * a removable fan mounted on floor-gears; and
;;;   * a fixed combined floor-blower.
;;;
;;; Floor-mounted fans and fixed floor blowers expose the same flush support surface.
;;; While blowing, they launch occupants to the drive's aimed-at destination and sustain
;;; them there.  When the stream stops, unsupported occupants at that destination fall
;;; back to the drive's location.  The public floor-gears.lisp and floor-blower.lisp
;;; technologies both include this file so a problem can name the kind of object it
;;; actually declares without duplicating the shared physics.
;;;
;;; A launched box carries its stack.  A jamming jammer or paired connector retains its
;;; relation through launch and fall; propagation re-derives the effect from its new
;;; location.  A fan resting on the source is too flat to catch the stream and is merely
;;; toppled onto the source location's ground.
;;;
;;; REQUIRES:
;;;   types     : agent, location
;;;   nested    : -gears-fan (types, mounting, shared blower state and cargo actions)
;;;   driver    : the derived propagation driver calls update-floor-blowing-status!
;;;               after update-blower-status!
;;; PROVIDES:
;;;   query     : location-elevation -- an undeclared floor-stream destination defaults
;;;               to the in-air hover elevation 10
;;;   updates   : update-floor-blowing-status!, blow-occupants-away!, drop-occupants!

(include-tech -propagation)
(include-tech -gears-fan)
(include-tech -location-coordinates)

(in-package :ww)


(define-query location-elevation (?location location)
  ;; Overrides -elevation's ground default of 0: an undeclared location that is some
  ;; floor drive's aimed-at destination floats at the default in-the-air hover level of
  ;; 10.  An authored level always wins, whether written as LOCATION-COORDS>'s third
  ;; coordinate or as HAS-ELEVATION.  Wall destinations remain ordinary ground locations.
  (if (bind (location-coords> ?location $x $y $z))
    $z
    (if (bind (has-elevation ?location $level))
      $level
      (if (exists (?g (either floor-gears floor-blower))
          (and (bind (aimed-at ?g $dest))
                 (eql $dest ?location)))
        10
        0))))


(define-update update-floor-blowing-status! ()
  ;; Pass 1 launches every occupant resting on a blowing floor source.  Pass 2 drops
  ;; occupants at a destination that no blowing floor drive still sustains.
  (do (doall (?source (either fan floor-blower wall-blower angled-blower))
        (if (blowing ?source)
          (do (assign $drive (blower-drive ?source))
              (if (or (floor-gears $drive)
                      (floor-blower $drive))
                (blow-occupants-away! ?source $drive)))))
      (doall (?g (either floor-gears floor-blower))
        (do (bind (aimed-at ?g $destination))
            (if (not (exists (?drive (either floor-gears floor-blower))
                       (and (blower-present ?drive)
                            (turning ?drive)
                            (bind (aimed-at ?drive $f-destination))
                            (eql $f-destination $destination))))
              (drop-occupants! ?g $destination))))))


(define-update blow-occupants-away!
    (?source (either fan floor-blower)
     ?drive (either floor-gears floor-blower))
  ;; Launch every non-fan occupant resting on the source, preserving any stack above it.
  ;; A fan is instead toppled onto the ground at the source location.
  (do (bind (aimed-at ?drive $destination))
      (doall (?x support-occupant)
        (if (on ?x ?source)
          (do (not (on ?x ?source))
              (if (not (fan ?x))
                (relocate-stack! ?x $destination)))))))


(define-update drop-occupants!
    (?drive (either floor-gears floor-blower) ?destination location)
  ;; When the sustaining stream stops, return each unsupported stack base to the drive.
  (do (bind (has-position ?drive $g-location))
      (doall (?x support-occupant)
        (if (and (not (fan ?x))
                 (bind (has-location ?x $x-location))
                 (eql $x-location ?destination)
                 (not (bind (on ?x $support))))
          (relocate-stack! ?x $g-location)))))

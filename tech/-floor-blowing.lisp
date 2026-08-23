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
;;; Like every blower drive, each floor drive must have exactly one HAS-POSITION source and
;;; AIMED-AT destination; -gears-fan owns that shared completeness check beside AIMED-AT.
;;; Each floor drive must also have its own destination.  Hover state records the destination,
;;; not a launch-source relation, so sharing one destination would make the eventual drop-back
;;; source ambiguous; FLOOR-BLOWING-INIT-CHECK rejects that floor-only error.
;;; A floor stream rises vertically.  When both its positioned source and destination have
;;; LOCATION-COORDS>, the same check therefore requires their x/y coordinates to match.
;;; Whenever both endpoint levels are authored through coordinates or HAS-ELEVATION, the
;;; destination must also be strictly above the source.  Problems omitting either piece of
;;; geometry retain the abstract source/destination representation.
;;;
;;; A launched box carries its stack.  A jamming jammer or paired connector retains its
;;; relation through launch and fall; propagation re-derives the effect from its new
;;; location.  A fan resting on the source is too flat to catch the stream and is merely
;;; toppled onto the source location's ground.
;;;
;;; REQUIRES:
;;;   types     : agent, location
;;;   nested    : -gears-fan (types, mounting, shared blower state and cargo actions);
;;;               -vertical (base, location-coords>, has-elevation, and the
;;;               location-level seam overridden here)
;;;   driver    : the derived propagation driver calls update-floor-blowing-status!
;;;               after update-blower-status!
;;; PROVIDES:
;;;   query     : location-level -- overrides -vertical's seam so an undeclared
;;;               floor-stream destination defaults to the in-air hover elevation 10.
;;;               LOCATION-ELEVATION itself is NOT overridden: it is -vertical's memoizing
;;;               entry point, and overriding it would discard the memo.  This override
;;;               reads only static relations -- LOCATION-COORDS>, HAS-ELEVATION, AIMED-AT
;;;               -- which is what keeps that memo sound
;;;   updates   : update-floor-blowing-status!, blow-occupants-away!, drop-occupants!
;;;   init check: floor-blowing-init-check -- destinations have unique drive owners,
;;;               and geometry-known lifts rise vertically

(include-tech -propagation)
(include-tech -gears-fan)
(include-tech -vertical)

(in-package :ww)


(define-init-check floor-blowing-init-check (literals)
  (check-init-floor-destinations-have-unique-drives literals)
  (check-init-floor-drive-geometry literals))


(define-init-check-helper check-init-floor-destinations-have-unique-drives (literals)
  "Reject two floor drives aimed at one destination, whose hover state could not retain a
   unique drop-back source."
  (let ((owners (make-hash-table :test #'eq)))
    (dolist (literal (positive-init-literals-with-relation 'aimed-at literals))
      (destructuring-bind (drive destination) (rest (init-literal-proposition literal))
        (when (init-floor-drive-p drive)
          (check-init-floor-destination-owner literal drive destination owners))))))


(define-init-check-helper init-floor-drive-p (drive)
  (or (init-type-member-p drive 'floor-gears)
      (init-type-member-p drive 'floor-blower)))


(define-init-check-helper check-init-floor-destination-owner
    (literal drive destination owners)
  (multiple-value-bind (owner presentp) (gethash destination owners)
    (when (and presentp (not (eql owner drive)))
      (fail-init-check
        literal
        "~%Floor drives must not share an AIMED-AT destination.~%~
         Destination:  ~S~%~
         First drive:  ~S~%~
         Second drive: ~S~%~
         A hovering occupant records only its destination, so it would have no unique ~
         source to return to when the streams stop."
        destination owner drive))
    (setf (gethash destination owners) drive)))


(define-init-check-helper check-init-floor-drive-geometry (literals)
  "Require a geometry-known floor stream to rise vertically above its source."
  (let ((positions (init-literal-map 'has-position literals 1 2))
        (coordinates (init-location-xy-map literals))
        (levels (init-location-level-map literals)))
    (dolist (literal (positive-init-literals-with-relation 'aimed-at literals))
      (destructuring-bind (drive destination) (rest (init-literal-proposition literal))
        (when (init-floor-drive-p drive)
          (let ((source (gethash drive positions)))
            (check-init-floor-drive-coordinate-pair
              literal drive source destination coordinates)
            (check-init-floor-drive-level-pair
              literal drive source destination levels)))))))


(define-init-check-helper check-init-floor-drive-coordinate-pair
    (literal drive source destination coordinates)
  (let ((source-point (and source (gethash source coordinates)))
        (destination-point (gethash destination coordinates)))
    (when (and source-point destination-point
               (not (equal source-point destination-point)))
      (fail-init-check
        literal
        "~%A coordinate-known floor stream must be vertical.~%~
         Drive:       ~S~%~
         Source:      ~S at ~S~%~
         Destination: ~S at ~S~%~
         Give its HAS-POSITION source and AIMED-AT destination the same x/y coordinates."
        drive source source-point destination destination-point))))


(define-init-check-helper check-init-floor-drive-level-pair
    (literal drive source destination levels)
  (multiple-value-bind (source-level source-known-p) (gethash source levels)
    (multiple-value-bind (destination-level destination-known-p)
        (gethash destination levels)
      (when (and source-known-p destination-known-p
                 (not (< source-level destination-level)))
        (fail-init-check
          literal
          "~%A level-known floor stream destination must be above its source.~%~
           Drive:       ~S~%~
           Source:      ~S at level ~S~%~
           Destination: ~S at level ~S~%~
           Give the destination a strictly greater level."
          drive source source-level destination destination-level)))))


(define-query location-level (?location location)
  ;; Overrides -vertical's plain BASE lookup: an undeclared location that is some floor
  ;; drive's aimed-at destination floats at the default in-the-air hover level of 10.  An
  ;; authored level always wins, whether written as LOCATION-COORDS>'s third coordinate or
  ;; as HAS-ELEVATION.  Wall destinations remain ordinary ground locations.
  (if (or (bind (location-coords> ?location $x $y $z))
          (bind (has-elevation ?location $level)))
    (base ?location)
    (if (exists (?g (either floor-gears floor-blower))
          (and (bind (aimed-at ?g $dest))
               (eql $dest ?location)))
      10
      0)))


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

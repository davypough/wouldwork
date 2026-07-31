;;; Filename: -stream-passability.lisp

;;; Stream passability substrate: the walking-side model of a wall fan's air stream.
;;; Overrides -passability.lisp's STREAM-OBSTACLE-CLEAR hook: gears pass unless a
;;; blowing fan is mounted on them, so a derived WALK-VIA/WALK-VIA> clause may name
;;; wall-gears as a conditional door -- the stream bars a walking crossing exactly
;;; while a blowing fan is mounted; vacant or stopped gears are clear, and a fan
;;; carried away or remounted elsewhere takes its stream with it automatically,
;;; since the check is keyed by the fixed gears rather than the portable fan.
;;;
;;; Also supplies the stream geometry to -walkability-coordinates' derivation by
;;; redefining WALKABILITY-COORDINATES-STREAM-SPECS: one spec per wall-gears, from
;;; the gears' HAS-POSITION location (the swept location), the AIMED-AT> destination,
;;; both locations' LOCATION-COORDS> coordinates, and the stream's width -- 3 units
;;; by default, overridable per gears with a (STREAM-WIDTH gears w) fact, the same
;;; default-with-override convention as elevation.  The derivation turns each spec
;;; into a barred band: center line from the backstop wall behind the fan to the
;;; destination, widened by half the width each side -- see -walkability-
;;; coordinates.lisp's header for the band, curtain, and ride semantics.
;;;
;;; Layering: -walkability-coordinates cannot gather the specs itself -- HAS-POSITION
;;; and AIMED-AT> belong to gears-fan, which walking must not depend on.  So this
;;; file, nested by wall-blower (the only mounting whose stream runs horizontally
;;; across walkable ground), nests gears-fan and REDEFINES WALKABILITY-COORDINATES-
;;; STREAM-SPECS whole, the same way it overrides STREAM-OBSTACLE-CLEAR by name --
;;; nesting a file guarantees its original definitions always splice before this one
;;; regardless of the problem's include order, so a later DEFINE-QUERY for the same
;;; name always wins.
;;;
;;; REQUIRES:
;;;   nested    : -passability (stream-obstacle-clear's null default, all-clear);
;;;               gears-fan (gears types, mounted-on, blowing, has-position, aimed-at>);
;;;               -walkability-coordinates (the derivation and the stream-specs default)
;;; PROVIDES:
;;;   relations : (stream-width wall-gears $rational)  --  optional per-gears override
;;;               of the 3-unit default stream width
;;;   queries   : stream-obstacle-clear  --  overrides -passability's null default;
;;;               walkability-coordinates-stream-specs  --  redefinition gathering
;;;               one spec per wall-gears

(include-tech -passability)
(include-tech gears-fan)
(include-tech -walkability-coordinates)

(in-package :ww)


(define-static-relations
  (stream-width wall-gears $rational))  ;optional; a wall fan's air stream is 3 units wide unless overridden


(define-query stream-obstacle-clear (?obstacle gears)
  ;; Gears pass unless a blowing fan is mounted on them -- their air stream is the
  ;; barrier, so vacant or stopped gears bar nothing.
  (not (exists (?f fan)
         (and (blowing ?f)
              (bind (mounted-on ?f $mount-gears))
              (eql $mount-gears ?obstacle)))))


(define-query walkability-coordinates-stream-specs ()
  ;; One (gears swept-location destination sx sy dx dy width) spec per wall-gears, for
  ;; -walkability-coordinates' band derivation.  Missing position, aim, or
  ;; coordinates error immediately -- a wall fan without a located stream is an
  ;; authoring mistake in a coordinate-driven problem.  Only called by
  ;; DERIVE-WALK-VIA-FROM-SEGMENTS, so a problem without segment geometry (hand-
  ;; authored walk-via) never evaluates it.
  (do (assign $specs nil)
      (doall (?g wall-gears)
        (do (if (not (bind (has-position ?g $swept-location)))
              (error "Wall-gears ~A has no HAS-POSITION swept location." ?g))
            (if (not (bind (aimed-at> ?g $destination)))
              (error "Wall-gears ~A has no AIMED-AT> destination." ?g))
            (if (not (bind (location-coords> $swept-location $sx $sy)))
              (error "The swept location ~A of ~A has no LOCATION-COORDS> coordinates."
                     $swept-location ?g))
            (if (not (bind (location-coords> $destination $dx $dy)))
              (error "The destination ~A of ~A has no LOCATION-COORDS> coordinates."
                     $destination ?g))
            (assign $width (if (bind (stream-width ?g $override)) $override 3))
            (push (list ?g $swept-location $destination $sx $sy $dx $dy $width) $specs)))
      $specs))

;;; Filename: -elevation.lisp

;;; Elevation substrate: the authored base level of an object that carries no coordinate
;;; relation of its own -- a floor repeater, a segment fixture in a coordinate-free model,
;;; or another ELEVATED-OBJECT in a problem with no coordinate geometry.  Default depends
;;; on the object's vertical role.
;;;
;;; A location's level is LOCATION-COORDS>'s optional third coordinate, a wall-mounted
;;; fixture's is APPARATUS-COORDS>'s, and a named segment's is its trailing coordinate;
;;; each cross-checks against HAS-ELEVATION rather than silently preferring one.  The
;;; role-branching anchor queries that used to live here --
;;; REPEATER-MOUNT-ELEVATION, REPEATER-ANCHOR-ELEVATION, FIXTURE-ELEVATION, and
;;; APPARATUS-ANCHOR-ELEVATION -- are gone: each was a per-type rule for reaching a base or
;;; a top, and -vertical's BASE and TOP now compute both for every type from one table.
;;;
;;; WALL-GEARS and WALL-BLOWER keep HAS-ELEVATION for a different quantity entirely: the
;;; elevation of the air stream they emit, read by -gears-fan's BLOWER-ELEVATION.  They
;;; have no base in the vertical model and are absent from -vertical's table.
;;;
;;; PROVIDES:
;;;   nested   : -height, -location-coordinates
;;;   types    : elevated-object (either location gate screen wall edge transmitter
;;;              receiver gun wall-gears wall-blower floor-repeater wall-repeater)
;;;   relation : (has-elevation elevated-object $rational)
;;;   queries  : none.  OBJECT-ELEVATION and LOCATION-ELEVATION are gone with the anchor
;;;              queries; -vertical's BASE reads this relation directly, and owns
;;;              LOCATION-ELEVATION as the seam -floor-blowing overrides

(include-tech -height)
(include-tech -location-coordinates)

(in-package :ww)


(define-optional-types
  gate screen wall edge transmitter receiver gun wall-gears wall-blower)


(define-types
  elevated-object
    (either location gate screen wall edge transmitter receiver gun
            wall-gears wall-blower
            floor-repeater wall-repeater))


(define-static-relations
  (has-elevation elevated-object $rational))  ;fixed base/anchor level; absent default depends on role

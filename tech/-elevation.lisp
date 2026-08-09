;;; Filename: -elevation.lisp

;;; Elevation substrate: the fixed vertical level of a location's own floor, a fixed
;;; obstacle's base, or a fixed fixture's beam/sightline anchor.  Locations and barrier
;;; fixtures default to base elevation 0; transmitter, receiver, and gun functional anchors
;;; default to elevation 1.  A floor-repeater's declared elevation is its base level (default 0) and
;;; its anchor adds its declared height; a wall-repeater's declared elevation is directly
;;; its mounting/anchor level (default 1).  Nondefault objects assert an explicit fact.
;;;
;;; PROVIDES:
;;;   nested   : -height (repeater, declared-height)
;;;   types    : elevated-object (either location gate screen wall edge transmitter
;;;              receiver gun wall-gears wall-blower floor-repeater wall-repeater)  --
;;;              wall gears and fixed wall blowers may declare their stream elevation
;;;              (default 1, via -gears-fan's blower-elevation)
;;;   relation : (has-elevation elevated-object $rational)
;;;   queries  : object-elevation, location-elevation, repeater-mount-elevation,
;;;              repeater-anchor-elevation, fixture-elevation, apparatus-anchor-elevation

(include-tech -height)

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


(define-query object-elevation (?object elevated-object)
  ;; Declared fixed level of an elevated object, or zero if none was asserted.
  (if (bind (has-elevation ?object $level))
    $level
    0))


(define-query location-elevation (?location location)
  ;; Declared floor level of a location, or zero if none was asserted (ordinary ground).
  (object-elevation ?location))


(define-query repeater-mount-elevation (?repeater repeater)
  ;; HAS-ELEVATION names the base level of a floor repeater and the mounting/anchor level
  ;; of a wall repeater.  The omitted-value default therefore depends on orientation.
  (do (assign $floor-mounted (floor-repeater ?repeater))
      (assign $wall-mounted (wall-repeater ?repeater))
      (if (eql $floor-mounted $wall-mounted)
        (error "~%Repeater must have exactly one mounting orientation.~%~
                Repeater: ~S"
               ?repeater))
      (if (bind (has-elevation ?repeater $level))
        $level
        (if $floor-mounted 0 1))))


(define-query repeater-anchor-elevation (?repeater repeater)
  ;; A floor repeater stands vertically, so its tip is one declared height above its base.
  ;; A wall repeater extends horizontally, leaving its tip at its mounting elevation.
  (if (floor-repeater ?repeater)
    (+ (repeater-mount-elevation ?repeater)
       (declared-height ?repeater))
    (repeater-mount-elevation ?repeater)))


(define-query fixture-elevation
    (?fixture (either gate transmitter receiver gun floor-repeater wall-repeater))
  ;; Declared fixed-fixture level.  Gates use base elevation 0, point-apparatus anchors
  ;; default to 1, and repeaters use their mounting-dependent anchor rule.
  (if (repeater ?fixture)
    (repeater-anchor-elevation ?fixture)
    (if (bind (has-elevation ?fixture $level))
      $level
      (if (or (transmitter ?fixture)
              (receiver ?fixture)
              (gun ?fixture))
        1
        0))))


(define-query apparatus-anchor-elevation
    (?apparatus (either transmitter receiver gun floor-repeater wall-repeater))
  ;; The vertical coordinate paired with APPARATUS-COORDS>'s horizontal functional point.
  ;; Transmitters, receivers, and guns are point apparatus; repeaters apply their mounting rule.
  (if (repeater ?apparatus)
    (repeater-anchor-elevation ?apparatus)
    (fixture-elevation ?apparatus)))

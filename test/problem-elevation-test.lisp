;;; Filename: problem-elevation-test.lisp

;;; Dedicated zero-action regression for the public elevation capability.  A
;;; complete matrix gives every ELEVATED-OBJECT leaf one explicit-elevation
;;; fixture and one fixture with no authored HAS-ELEVATION fact:
;;;
;;;   location, gate, screen, wall, edge, transmitter, receiver, gun, wall-gears,
;;;   floor-repeater, and wall-repeater.
;;;
;;; The characterization goal verifies exact authored bindings, the generic
;;; default of zero, and every role-specific lookup.  In particular,
;;; transmitter/receiver/gun functional anchors default to one despite their generic
;;; elevation of zero; a floor repeater's anchor is its base plus height; and a
;;; wall repeater's height is horizontal and cannot raise its anchor.  Initial
;;; and final states are identical.  Expected minimum path length: zero.

(in-package :ww)


(ww-set *problem-name* elevation-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 0)


;;;; TYPES ;;;;


(define-types
  ;; These four types are required by elevation's nested height role but are
  ;; intentionally outside ELEVATED-OBJECT.
  agent (type-agent)
  box (type-box)
  jammer (type-jammer)
  connector (type-connector)

  location (explicit-location default-location)
  gate (explicit-gate default-gate)
  screen (explicit-screen default-screen)
  wall (explicit-wall default-wall)
  edge (explicit-edge default-edge)
  transmitter (explicit-transmitter default-transmitter)
  receiver (explicit-receiver default-receiver)
  gun (explicit-gun default-gun)
  wall-gears (explicit-wall-gears default-wall-gears)
  floor-repeater (explicit-floor-repeater default-floor-repeater)
  wall-repeater (explicit-wall-repeater default-wall-repeater))


;;;; TECHNOLOGY INCLUDE ;;;;


(include-tech elevation)


;;;; INITIALIZATION ;;;;


(define-init
  ;; Distinct values make every authored branch independently observable.
  (has-elevation explicit-location 2)
  (has-elevation explicit-gate 3)
  (has-elevation explicit-screen 4)
  (has-elevation explicit-wall 6)
  (has-elevation explicit-edge 12)
  (has-elevation explicit-transmitter 7)
  (has-elevation explicit-receiver 8)
  (has-elevation explicit-gun 13)
  (has-elevation explicit-wall-gears 9)
  (has-elevation explicit-floor-repeater 10)
  (has-elevation explicit-wall-repeater 11)

  ;; Floor height is vertical; wall height is horizontal and must be ignored by
  ;; the anchor calculation.  DEFAULT-WALL-REPEATER defaults only its elevation.
  (has-height explicit-floor-repeater 2)
  (has-height explicit-wall-repeater 7)
  (has-height default-wall-repeater 9))


;;;; CHARACTERIZATION HELPERS ;;;;


(define-query explicit-elevation-valid
    (?object elevated-object ?expected-elevation)
  (and
    (has-elevation ?object ?expected-elevation)
    (do (bind (has-elevation ?object $bound-elevation))
        (= $bound-elevation ?expected-elevation))
    (= (object-elevation ?object) ?expected-elevation)))


(define-query default-elevation-valid (?object elevated-object)
  (and
    (not (bind (has-elevation ?object $authored-elevation)))
    (= (object-elevation ?object) 0)))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query elevation-scenarios-valid ()
  (and
    ;; Complete explicit and absent-fact matrices for ELEVATED-OBJECT.
    (explicit-elevation-valid explicit-location 2)
    (explicit-elevation-valid explicit-gate 3)
    (explicit-elevation-valid explicit-screen 4)
    (explicit-elevation-valid explicit-wall 6)
    (explicit-elevation-valid explicit-edge 12)
    (explicit-elevation-valid explicit-transmitter 7)
    (explicit-elevation-valid explicit-receiver 8)
    (explicit-elevation-valid explicit-gun 13)
    (explicit-elevation-valid explicit-wall-gears 9)
    (explicit-elevation-valid explicit-floor-repeater 10)
    (explicit-elevation-valid explicit-wall-repeater 11)

    (default-elevation-valid default-location)
    (default-elevation-valid default-gate)
    (default-elevation-valid default-screen)
    (default-elevation-valid default-wall)
    (default-elevation-valid default-edge)
    (default-elevation-valid default-transmitter)
    (default-elevation-valid default-receiver)
    (default-elevation-valid default-gun)
    (default-elevation-valid default-wall-gears)
    (default-elevation-valid default-floor-repeater)
    (default-elevation-valid default-wall-repeater)

    ;; Location and gate retain the generic zero-default convention.
    (= (location-elevation explicit-location) 2)
    (= (location-elevation default-location) 0)
    (= (fixture-elevation explicit-gate) 3)
    (= (fixture-elevation default-gate) 0)

    ;; Point apparatus defaults its functional anchor to one, not its generic
    ;; OBJECT-ELEVATION result of zero.
    (= (fixture-elevation explicit-transmitter) 7)
    (= (apparatus-anchor-elevation explicit-transmitter) 7)
    (= (fixture-elevation default-transmitter) 1)
    (= (apparatus-anchor-elevation default-transmitter) 1)
    (= (fixture-elevation explicit-receiver) 8)
    (= (apparatus-anchor-elevation explicit-receiver) 8)
    (= (fixture-elevation default-receiver) 1)
    (= (apparatus-anchor-elevation default-receiver) 1)
    (= (fixture-elevation explicit-gun) 13)
    (= (apparatus-anchor-elevation explicit-gun) 13)
    (= (fixture-elevation default-gun) 1)
    (= (apparatus-anchor-elevation default-gun) 1)

    ;; Floor-mounted: base 10 + height 2, and default base 0 + default height 1.
    (= (repeater-mount-elevation explicit-floor-repeater) 10)
    (= (declared-height explicit-floor-repeater) 2)
    (= (repeater-anchor-elevation explicit-floor-repeater) 12)
    (= (fixture-elevation explicit-floor-repeater) 12)
    (= (apparatus-anchor-elevation explicit-floor-repeater) 12)

    (not (bind (has-height default-floor-repeater $default-floor-height)))
    (= (repeater-mount-elevation default-floor-repeater) 0)
    (= (declared-height default-floor-repeater) 1)
    (= (repeater-anchor-elevation default-floor-repeater) 1)
    (= (fixture-elevation default-floor-repeater) 1)
    (= (apparatus-anchor-elevation default-floor-repeater) 1)

    ;; Wall-mounted: both explicit heights are horizontal projections.  Neither
    ;; may be added to the explicit or default mounting elevation.
    (= (declared-height explicit-wall-repeater) 7)
    (= (repeater-mount-elevation explicit-wall-repeater) 11)
    (= (repeater-anchor-elevation explicit-wall-repeater) 11)
    (not (= (repeater-anchor-elevation explicit-wall-repeater) 18))
    (= (fixture-elevation explicit-wall-repeater) 11)
    (= (apparatus-anchor-elevation explicit-wall-repeater) 11)

    (= (declared-height default-wall-repeater) 9)
    (= (repeater-mount-elevation default-wall-repeater) 1)
    (= (repeater-anchor-elevation default-wall-repeater) 1)
    (not (= (repeater-anchor-elevation default-wall-repeater) 10))
    (= (fixture-elevation default-wall-repeater) 1)
    (= (apparatus-anchor-elevation default-wall-repeater) 1)))


(define-goal
  (elevation-scenarios-valid))

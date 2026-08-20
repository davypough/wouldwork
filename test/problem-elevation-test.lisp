;;; Filename: problem-elevation-test.lisp

;;; Dedicated zero-action regression for the public elevation capability.  A
;;; complete matrix gives every ELEVATED-OBJECT leaf one explicit-elevation
;;; fixture and one fixture with no authored HAS-ELEVATION fact:
;;;
;;;   location, gate, screen, wall, edge, transmitter, receiver, gun, wall-gears,
;;;   floor-repeater, and wall-repeater.
;;;
;;; The characterization goal verifies exact authored bindings and the generic default
;;; of zero, for every leaf of ELEVATED-OBJECT.  This substrate no longer owns any
;;; role-specific anchor rule: REPEATER-MOUNT-ELEVATION, REPEATER-ANCHOR-ELEVATION,
;;; FIXTURE-ELEVATION, and APPARATUS-ANCHOR-ELEVATION were four per-type ways of reaching
;;; a base or a top, and -vertical's BASE and TOP now compute both from one table.  Their
;;; behavior, including the mounting defaults that moved into APPARATUS-COORDS>, is pinned
;;; by problem-vertical-test.  What remains here is the authored fact and its zero default.
;;; Initial and final states are identical.  Expected minimum path length: zero.

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
  wall-blower (explicit-wall-blower default-wall-blower)
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
  (has-elevation explicit-wall-blower 14)
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
    (explicit-elevation-valid explicit-wall-blower 14)
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
    (default-elevation-valid default-wall-blower)
    (default-elevation-valid default-floor-repeater)
    (default-elevation-valid default-wall-repeater)

    ;; Location and gate retain the generic zero-default convention.
    (= (location-elevation explicit-location) 2)
    (= (location-elevation default-location) 0)
    (= (object-elevation explicit-gate) 3)
    (= (object-elevation default-gate) 0)

    ;; Every remaining leaf reads its authored fact, or zero.  The functional and
    ;; mounting defaults these fixtures used to carry are properties of their
    ;; coordinates now, not of HAS-ELEVATION, and this problem authors no coordinates.
    (= (object-elevation explicit-transmitter) 7)
    (= (object-elevation default-transmitter) 0)
    (= (object-elevation explicit-receiver) 8)
    (= (object-elevation default-receiver) 0)
    (= (object-elevation explicit-gun) 13)
    (= (object-elevation default-gun) 0)
    (= (object-elevation explicit-floor-repeater) 10)
    (= (object-elevation default-floor-repeater) 0)
    (= (object-elevation explicit-wall-repeater) 11)
    (= (object-elevation default-wall-repeater) 0)

    ;; HAS-HEIGHT is readable but contributes nothing here: height is -vertical's
    ;; concern, and this substrate reports base alone.
    (= (declared-height explicit-floor-repeater) 2)
    (= (declared-height explicit-wall-repeater) 7)
    (= (declared-height default-wall-repeater) 9)))


(define-goal
  (elevation-scenarios-valid))

;;; Filename: problem-elevation-test.lisp

;;; Dedicated zero-action regression for the public elevation capability.  A
;;; complete matrix gives every ELEVATED-OBJECT leaf one explicit-elevation
;;; fixture and one fixture with no authored HAS-ELEVATION fact:
;;;
;;;   location, gate, screen, wall, edge, transmitter, receiver, gun, wall-gears,
;;;   floor-repeater, and wall-repeater.
;;;
;;; What this file pins is the relation itself: its schema, its type domain, exact
;;; binding of authored values, and the absence of any fact for an undeclared fixture.
;;; This substrate no longer owns a query at all.  OBJECT-ELEVATION, LOCATION-ELEVATION,
;;; and the four role-specific anchor rules -- REPEATER-MOUNT-ELEVATION,
;;; REPEATER-ANCHOR-ELEVATION, FIXTURE-ELEVATION, APPARATUS-ANCHOR-ELEVATION -- were six
;;; per-type ways of reaching a base or a top, and -vertical's BASE and TOP now compute
;;; both for every type from one table.  Their behavior, including the mounting defaults
;;; that moved into APPARATUS-COORDS>, is pinned by problem-vertical-test.  Initial and
;;; final states are identical.  Expected minimum path length: zero.

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
        (= $bound-elevation ?expected-elevation))))


(define-query absent-elevation-valid (?object elevated-object)
  (not (bind (has-elevation ?object $authored-elevation))))


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

    (absent-elevation-valid default-location)
    (absent-elevation-valid default-gate)
    (absent-elevation-valid default-screen)
    (absent-elevation-valid default-wall)
    (absent-elevation-valid default-edge)
    (absent-elevation-valid default-transmitter)
    (absent-elevation-valid default-receiver)
    (absent-elevation-valid default-gun)
    (absent-elevation-valid default-wall-gears)
    (absent-elevation-valid default-wall-blower)
    (absent-elevation-valid default-floor-repeater)
    (absent-elevation-valid default-wall-repeater)

    ;; What an object with no authored fact is worth is -vertical's business, not this
    ;; relation's: BASE resolves a location through LOCATION-COORDS>, a wall-mounted
    ;; fixture through APPARATUS-COORDS>, and anything else through the base-default
    ;; column of *VERTICAL-TYPE-CONSTANTS*.  problem-vertical-test pins all three.
    (not (bind (has-elevation default-location $any-location-level)))
    (not (bind (has-elevation default-wall-repeater $any-repeater-level)))))


(define-test-claim elevation-relation-contract
  (expect-relation-schema
    'has-elevation :static '(elevated-object rational)
    :fluent-indices '(2))
  (expect-condition
    (lambda ()
      (check-proposition '(has-elevation explicit-location 2.0)))
    'error
    :containing "not of specified type RATIONAL")
  (expect-condition
    (lambda ()
      (check-init-duplicate-fluent-keys
        '((has-elevation explicit-location 2)
          (has-elevation explicit-location 3))))
    'error
    :containing "Duplicate DEFINE-INIT fluent key"))


(define-goal
  (elevation-scenarios-valid))

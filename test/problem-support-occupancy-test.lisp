;;; Filename: problem-support-occupancy-test.lisp

;;; Dedicated regression for the shared -support-occupancy role.  A three-action
;;; lifecycle forces CLEARTOP to track four consecutive states of one plate:
;;;
;;;   1. Initially occupied by an agent and therefore not clear.
;;;   2. Empty after removing the agent and therefore clear.
;;;   3. Occupied by a box and therefore not clear again.
;;;   4. Empty after removing the box and therefore clear again.
;;;
;;; Independent fixtures characterize both sides of the zero-versus-one-occupant
;;; boundary for every support kind.  Together with the lifecycle agent and box,
;;; the occupied fixtures also cover jammer, connector, and fan occupants, completing
;;; the SUPPORT-OCCUPANT leaf matrix without creating an invalid multiply occupied
;;; support.  The goal verifies exact occupancy and important absent facts directly.
;;; Expected minimum path length: three.

(in-package :ww)


(ww-set *problem-name* support-occupancy-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 3)

(setf *expected-min-length* 3)


;;;; TYPES ;;;;


(define-types
  agent (lifecycle-agent)
  plate (lifecycle-plate clear-plate occupied-plate)
  box (lifecycle-box clear-box occupied-box)
  jammer (plate-jammer)
  connector (box-connector)
  fan (clear-fan occupied-fan fan-rider)
  support-occupancy-phase
    (cleared-once reoccupied lifecycle-complete))


;;;; TECHNOLOGY INCLUDE ;;;;


(include-tech -support-occupancy)


;;;; TEST LIFECYCLE STATE ;;;;


(define-dynamic-relations
  (occupancy-phase support-occupancy-phase))


;;;; ACTIONS ;;;;


(define-action remove-lifecycle-agent
  1
  ()
  (and
    (on lifecycle-agent lifecycle-plate)
    (not (cleartop lifecycle-plate))
    (not (exists (?phase support-occupancy-phase)
           (occupancy-phase ?phase))))
  ("remove the agent from the lifecycle plate")
  (assert
    (not (on lifecycle-agent lifecycle-plate))
    (occupancy-phase cleared-once)))


(define-action place-lifecycle-box
  1
  ()
  (and
    (occupancy-phase cleared-once)
    (cleartop lifecycle-plate)
    (not (exists (?occupant support-occupant)
           (on ?occupant lifecycle-plate))))
  ("place the box on the lifecycle plate")
  (assert
    (not (occupancy-phase cleared-once))
    (on lifecycle-box lifecycle-plate)
    (occupancy-phase reoccupied)))


(define-action remove-lifecycle-box
  1
  ()
  (and
    (occupancy-phase reoccupied)
    (on lifecycle-box lifecycle-plate)
    (not (cleartop lifecycle-plate)))
  ("remove the box from the lifecycle plate")
  (assert
    (not (occupancy-phase reoccupied))
    (not (on lifecycle-box lifecycle-plate))
    (occupancy-phase lifecycle-complete)))


;;;; INITIALIZATION ;;;;


(define-init
  ;; The lifecycle starts at the occupied side of the exact boundary.
  (on lifecycle-agent lifecycle-plate)

  ;; One occupied fixture for each remaining occupant leaf, distributed across
  ;; all three support kinds.  CLEAR-* deliberately have no ON facts.
  (on plate-jammer occupied-plate)
  (on box-connector occupied-box)
  (on fan-rider occupied-fan))


;;;; CHARACTERIZATION HELPERS ;;;;


(define-query support-occupied-only-by
    (?support support ?intended-occupant support-occupant)
  (and
    (on ?intended-occupant ?support)
    (not (exists (?other-occupant support-occupant)
           (and (different ?other-occupant ?intended-occupant)
                (on ?other-occupant ?support))))))


(define-query support-empty-and-clear (?support support)
  (and
    (not (exists (?occupant support-occupant)
           (on ?occupant ?support)))
    (cleartop ?support)))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query support-occupancy-scenarios-valid ()
  (and
    ;; The forced occupied -> clear -> occupied -> clear lifecycle completed,
    ;; with neither movable occupant nor an intermediate phase left behind.
    (occupancy-phase lifecycle-complete)
    (not (occupancy-phase cleared-once))
    (not (occupancy-phase reoccupied))
    (support-empty-and-clear lifecycle-plate)
    (not (on lifecycle-agent lifecycle-plate))
    (not (on lifecycle-box lifecycle-plate))

    ;; Zero occupants is the positive CLEARTOP boundary for every support kind.
    (support-empty-and-clear clear-plate)
    (support-empty-and-clear clear-box)
    (support-empty-and-clear clear-fan)

    ;; One exact occupant is the negative CLEARTOP boundary for every support
    ;; kind and completes the jammer/connector/fan occupant coverage.
    (support-occupied-only-by occupied-plate plate-jammer)
    (not (cleartop occupied-plate))

    (support-occupied-only-by occupied-box box-connector)
    (not (cleartop occupied-box))

    (support-occupied-only-by occupied-fan fan-rider)
    (not (cleartop occupied-fan))))


(define-goal
  (support-occupancy-scenarios-valid))

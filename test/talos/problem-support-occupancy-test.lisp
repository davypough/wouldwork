;;; Filename: problem-support-occupancy-test.lisp

;;; Dedicated regression for the shared -support-occupancy role, which owns two distinct
;;; questions about a support top and answers them differently:
;;;
;;;   SUPPORT-OCCUPIED  --  is anything at all resting here (layer-blind physics)
;;;   CLEARTOP          --  may this particular occupant take the top (manipulation gate)
;;;
;;; A three-action lifecycle forces both to track four consecutive states of one plate:
;;;
;;;   1. Initially occupied by an agent, so occupied and clear for nobody else.
;;;   2. Empty after removing the agent, so unoccupied and clear for anybody.
;;;   3. Occupied by a box and therefore not clear again.
;;;   4. Empty after removing the box and therefore clear again.
;;;
;;; This problem installs no recorder, so it also pins the neutral contention rule that
;;; -INTERACTION-POLICY supplies: with only one layer in play every pair of distinct
;;; occupants contends, and a support carrying one occupant is closed to every other.  The
;;; one exception the query itself guarantees is that an object never blocks itself, which
;;; is what lets PICKUP-BOX name the box being lifted as its own reference occupant.  The
;;; live/ghost exception to contention belongs to the recorder and is characterized by
;;; problem-recorder-occupancy-test.
;;;
;;; Independent fixtures characterize both sides of the zero-versus-one-occupant boundary
;;; for every support kind.  Together with the lifecycle agent and box, the occupied
;;; fixtures also cover jammer, connector, and fan occupants, completing the
;;; SUPPORT-OCCUPANT leaf matrix.  The goal verifies exact occupancy and important absent
;;; facts directly.  Expected minimum path length: three.

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
  pressure-plate (lifecycle-plate clear-plate occupied-plate)
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
    (support-occupied lifecycle-plate)
    (not (cleartop lifecycle-plate lifecycle-box))
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
    (not (support-occupied lifecycle-plate))
    (cleartop lifecycle-plate lifecycle-box)
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
    (support-occupied lifecycle-plate)
    (not (cleartop lifecycle-plate lifecycle-agent)))
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
  ;; Empty by the physical query, and clear by the manipulation query for two occupants of
  ;; unrelated leaf types -- an empty support is available to anyone.
  (and
    (not (exists (?occupant support-occupant)
           (on ?occupant ?support)))
    (not (support-occupied ?support))
    (cleartop ?support lifecycle-agent)
    (cleartop ?support lifecycle-box)))


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

    ;; Zero occupants is the positive boundary for every support kind.
    (support-empty-and-clear clear-plate)
    (support-empty-and-clear clear-box)
    (support-empty-and-clear clear-fan)

    ;; One exact occupant is the negative boundary for every support kind and
    ;; completes the jammer/connector/fan occupant coverage.  With no recorder
    ;; installed every distinct pair of occupants contends, so one occupant closes
    ;; the top to all the others.
    (support-occupied-only-by occupied-plate plate-jammer)
    (support-occupied occupied-plate)
    (not (cleartop occupied-plate lifecycle-agent))
    (not (cleartop occupied-plate lifecycle-box))

    (support-occupied-only-by occupied-box box-connector)
    (support-occupied occupied-box)
    (not (cleartop occupied-box lifecycle-agent))

    (support-occupied-only-by occupied-fan fan-rider)
    (support-occupied occupied-fan)
    (not (cleartop occupied-fan lifecycle-agent))

    ;; An object never blocks itself, which is the case PICKUP-BOX relies on when it
    ;; asks whether a box's own top is free of contending weight.
    (cleartop occupied-plate plate-jammer)
    (cleartop occupied-box box-connector)
    (cleartop occupied-fan fan-rider)))


(define-goal
  (support-occupancy-scenarios-valid))

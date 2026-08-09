;;; Dedicated zero-action regression for the shared -visibility substrate.
;;;
;;; One instance of every optional apparatus/gate leaf characterizes the exact
;;; REPEATER and APPARATUS unions.  The goal calls each neutral visibility hook
;;; across every valid target shape and several endpoint elevations; every call
;;; must remain false until public visibility overrides the hooks.  It also
;;; verifies that the public LOS/coordinate relations, helper queries,
;;; initialization, actions, and dynamic state remain absent.
;;;
;;; The initial and final dynamic states are empty.  The characterization query
;;; is true immediately after staging, so the expected minimum path length is 0.

(in-package :ww)

(ww-set *problem-name* visibility-substrate-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 1)
(setf *expected-min-length* 0)


;;;; TYPES ;;;;


(define-types
  location (near-site far-site)
  gate (sample-gate)
  transmitter (sample-transmitter)
  receiver (sample-receiver)
  floor-repeater (sample-floor-repeater)
  wall-repeater (sample-wall-repeater)
  gun (sample-gun))


;;;; TECHNOLOGY INCLUDE ;;;;


(include-tech -visibility)


;;;; CHARACTERIZATION CLAIM ;;;;


(define-test-claim visibility-substrate-schema
  (expect-type-absent 'fixture)
  (expect-type-components 'repeater '(floor-repeater wall-repeater))
  (expect-type-components
    'apparatus
    '(transmitter receiver floor-repeater wall-repeater gun))
  (expect-type-instances
    'repeater '(sample-floor-repeater sample-wall-repeater))
  (expect-type-instances
    'apparatus
    '(sample-transmitter sample-receiver
      sample-floor-repeater sample-wall-repeater sample-gun))
  (expect-relations :static '())
  (expect-relations :dynamic '(inconsistent-state))
  (expect-registered :query 'visible)
  (expect-registered :query 'visible-for-object)
  (expect-registered :query 'potentially-visible)
  (expect-registered :query 'beam-visible)
  (expect-registered :query 'beam-visible-for-object)
  (expect-registered :query 'elevation-visible-for-object)
  (expect-not-registered :query 'visible-clear)
  (expect-not-registered :query 'beam-elevation-at-location)
  (expect-registrations :init-action '())
  (expect-registrations :action '())
  (null (database *start-state*))
  (not (state-is-inconsistent *start-state*)))


(define-query visibility-substrate-gate-type-valid
    (?object gate)
  (do
    ?object
    t))


(define-query visibility-substrate-apparatus-type-valid
    (?object apparatus)
  (do
    ?object
    t))


(define-query visibility-substrate-gate-neutral
    (?object gate)
  (and
    (not (visible near-site ?object))
    (not (visible-for-object nil near-site ?object))
    (not (potentially-visible near-site ?object))
    (not (elevation-visible-for-object nil near-site 0 ?object 0))
    (not (elevation-visible-for-object nil near-site -1 ?object 3/2))))


(define-query visibility-substrate-apparatus-neutral
    (?object apparatus)
  (and
    (not (visible near-site ?object))
    (not (visible-for-object nil near-site ?object))
    (not (potentially-visible near-site ?object))
    (not (beam-visible near-site 0 ?object 0))
    (not (beam-visible near-site -1 ?object 3/2))
    (not (beam-visible-for-object nil near-site 0 ?object 0))
    (not (beam-visible-for-object nil near-site -1 ?object 3/2))
    (not (elevation-visible-for-object nil near-site 0 ?object 0))
    (not (elevation-visible-for-object nil near-site -1 ?object 3/2))))


(define-query visibility-substrate-location-neutral
    (?object location)
  (and
    (not (visible near-site ?object))
    (not (visible-for-object nil near-site ?object))
    (not (potentially-visible near-site ?object))
    (not (beam-visible near-site 0 ?object 0))
    (not (beam-visible near-site -1 ?object 3/2))
    (not (beam-visible-for-object nil near-site 0 ?object 0))
    (not (beam-visible-for-object nil near-site -1 ?object 3/2))
    (not (elevation-visible-for-object nil near-site 0 ?object 0))
    (not (elevation-visible-for-object nil near-site -1 ?object 3/2))))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query visibility-substrate-scenarios-valid ()
  (and
    ;; A gate is a valid sightline target but deliberately not an apparatus.
    (visibility-substrate-gate-type-valid sample-gate)
    (visibility-substrate-gate-neutral sample-gate)

    ;; Every point apparatus is a valid sightline target.
    (visibility-substrate-apparatus-type-valid sample-transmitter)
    (visibility-substrate-apparatus-type-valid sample-receiver)
    (visibility-substrate-apparatus-type-valid sample-floor-repeater)
    (visibility-substrate-apparatus-type-valid sample-wall-repeater)
    (visibility-substrate-apparatus-type-valid sample-gun)

    ;; Every valid apparatus shape exercises both equal and unequal elevations.
    (visibility-substrate-apparatus-neutral sample-transmitter)
    (visibility-substrate-apparatus-neutral sample-receiver)
    (visibility-substrate-apparatus-neutral sample-floor-repeater)
    (visibility-substrate-apparatus-neutral sample-wall-repeater)
    (visibility-substrate-apparatus-neutral sample-gun)

    ;; Locations are valid targets too, including a reflexive sightline request.
    (visibility-substrate-location-neutral near-site)
    (visibility-substrate-location-neutral far-site)))


(define-goal
  (visibility-substrate-scenarios-valid))

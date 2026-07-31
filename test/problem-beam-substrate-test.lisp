;;; Filename: problem-beam-substrate-test.lisp

;;; Dedicated zero-action regression for the shared -beam-substrate interface
;;; with no public direct, relay, or crossing capability installed.
;;;
;;; The characterization verifies the complete BEAM-NODE/fixed-endpoint type
;;; composition and every neutral hook.  One deliberately stale ACTIVE receiver
;;; is authored before initialization; the substrate's derived propagation driver
;;; must call UPDATE-RECEIVER-STATUS! and remove it because neither arrival hook
;;; reaches any receiver.  The planner start and final states are therefore empty.
;;; Expected minimum path length: zero.

(in-package :ww)


(ww-set *problem-name* beam-substrate-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 0)


;;;; TYPES ;;;;


(define-types
  hue (sample-hue)
  transmitter (sample-transmitter)
  receiver (stale-receiver clean-receiver)
  location (sample-location)
  floor-repeater (sample-floor-repeater)
  wall-repeater (sample-wall-repeater))


;;;; TECHNOLOGY INCLUDE ;;;;


(include-tech -beam-substrate)


;;;; INITIALIZATION ;;;;


(define-init
  ;; This stale derived fact must not survive the ordinary initialization
  ;; propagation pass when both arrival contributors are neutral.
  (active stale-receiver))


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert
    (propagate-changes!)))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query beam-substrate-transmitter-type-valid
    (?object transmitter)
  (and
    (beam-node ?object)
    (fixed-beam-source ?object)
    (not (fixed-beam-target ?object))))


(define-query beam-substrate-receiver-type-valid
    (?object receiver)
  (and
    (beam-node ?object)
    (fixed-beam-target ?object)
    (not (fixed-beam-source ?object))))


(define-query beam-substrate-floor-repeater-type-valid
    (?object floor-repeater)
  (and
    (repeater ?object)
    (floor-repeater ?object)
    (not (wall-repeater ?object))
    (beam-node ?object)
    (fixed-beam-source ?object)
    (fixed-beam-target ?object)))


(define-query beam-substrate-wall-repeater-type-valid
    (?object wall-repeater)
  (and
    (repeater ?object)
    (wall-repeater ?object)
    (not (floor-repeater ?object))
    (beam-node ?object)
    (fixed-beam-source ?object)
    (fixed-beam-target ?object)))


(define-query beam-substrate-location-type-valid
    (?object location)
  (and
    (beam-node ?object)
    (not (fixed-beam-source ?object))
    (not (fixed-beam-target ?object))))


(define-query beam-substrate-type-scenarios-valid ()
  (and
    ;; Transmitters emit fixed beams but cannot be their fixed target.
    (beam-substrate-transmitter-type-valid sample-transmitter)

    ;; Receivers terminate fixed beams but cannot be their fixed source.
    (beam-substrate-receiver-type-valid stale-receiver)
    (beam-substrate-receiver-type-valid clean-receiver)

    ;; Both mounting orientations are repeaters, nodes, sources, and targets.
    (beam-substrate-floor-repeater-type-valid sample-floor-repeater)
    (beam-substrate-wall-repeater-type-valid sample-wall-repeater)

    ;; A location is a relay/crossing endpoint but never a fixed apparatus end.
    (beam-substrate-location-type-valid sample-location)))


(define-query beam-substrate-neutral-hooks-valid ()
  (and
    ;; Neither absent arrival capability contributes to the composite.
    (not (direct-beam-reaches-receiver stale-receiver))
    (not (relay-beam-reaches-receiver stale-receiver))
    (not (beam-reaches-receiver stale-receiver))
    (not (direct-beam-reaches-receiver clean-receiver))
    (not (relay-beam-reaches-receiver clean-receiver))
    (not (beam-reaches-receiver clean-receiver))

    ;; Direct and relay cutting liveness remain neutral for apparatus and
    ;; location endpoint shapes.
    (not (direct-beam-live-for-cutting
           sample-transmitter stale-receiver))
    (not (relay-beam-live-for-cutting
           sample-transmitter stale-receiver nil))
    (not (beam-live-for-cutting
           sample-transmitter stale-receiver nil))
    (not (direct-beam-live-for-cutting
           sample-floor-repeater sample-wall-repeater))
    (not (relay-beam-live-for-cutting
           sample-location sample-wall-repeater
           '((ignored-relay sample-hue 1))))
    (not (beam-live-for-cutting
           sample-location sample-wall-repeater
           '((ignored-relay sample-hue 1))))

    ;; Crossing, corridor, and lighting hooks contribute no state.
    (not (beam-cut sample-transmitter stale-receiver))
    (not (beam-cut sample-floor-repeater sample-wall-repeater))
    (not (beam-cut-in
           sample-transmitter stale-receiver '(ignored-crossing)))
    (not (beam-cut-in
           sample-location sample-wall-repeater '(ignored-crossing)))
    (null (current-crossing-set))
    (not (fixed-beam-corridor-clear
           sample-transmitter stale-receiver))
    (not (fixed-beam-corridor-clear
           sample-floor-repeater sample-wall-repeater))
    (null (compute-relay-lighting nil))
    (null (compute-relay-lighting '(ignored-crossing)))

    ;; No relay source has a finite distance without beam-relay.
    (= (beam-relay-source-distance sample-transmitter nil)
       most-positive-fixnum)
    (= (beam-relay-source-distance
         sample-floor-repeater '((ignored-relay sample-hue 1)))
       most-positive-fixnum)
    (= (beam-relay-source-distance
         sample-location '((ignored-relay sample-hue 1)))
       most-positive-fixnum)))


(define-query beam-substrate-state-scenario-valid ()
  (and
    ;; Initialization must remove the deliberately stale receiver fact and
    ;; leave an already-clean receiver unchanged.
    (not (active stale-receiver))
    (not (active clean-receiver))

    ;; No static beam declarations can accidentally supply a positive result.
    (not (bind (has-chroma sample-transmitter $source-hue)))
    (not (bind (has-chroma stale-receiver $stale-hue)))
    (not (bind (has-chroma clean-receiver $clean-hue)))
    (not (coupled sample-transmitter stale-receiver))
    (not (coupled sample-floor-repeater sample-wall-repeater))
    (not (bind
           (beam-via
             sample-transmitter $direct-obstacles stale-receiver)))
    (not (bind
           (beam-via
             sample-floor-repeater
             $relay-obstacles
             sample-wall-repeater)))))


(define-query beam-substrate-scenarios-valid ()
  (and
    (beam-substrate-type-scenarios-valid)
    (beam-substrate-neutral-hooks-valid)
    (beam-substrate-state-scenario-valid)))


(define-goal
  (beam-substrate-scenarios-valid))

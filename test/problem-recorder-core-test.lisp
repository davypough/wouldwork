;;; Filename: problem-recorder-core-test.lisp

;;; Zero-action characterization of the private recorder core.  Identity, recording-side
;;; object presence, and cross-layer interaction policy are active, while every
;;; capability-specific shadow relation, query, and update remains absent.  This keeps the
;;; public recorder assembly extensible without letting apparatus state drift back into its
;;; identity layer.  Expected minimum path length: zero.

(in-package :ww)


(ww-set *problem-name* recorder-core-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 0)


;;;; TYPES ;;;;


(define-types
  agent (live-agent ghost-agent)
  connector (live-connector ghost-connector)
  fan (unmapped-fan)
  recorder (recorder1)
  location (recorder-site))


;;;; TECHNOLOGY INCLUDE ;;;;


(include-tech -recorder-core)


;;;; INITIALIZATION ;;;;


(define-init
  (recording-copy> live-agent ghost-agent)
  (recording-copy> live-connector ghost-connector)
  ;; OBJECT-MANIPULATION-ALLOWED and CONNECTOR-PAIRING-ALLOWED gate ghost action on this
  ;; flag; the characterization below expects both to be true for the ghost side.
  (recording-in-progress)
  (has-position recorder1 recorder-site))


;;;; SCHEMA BOUNDARY ;;;;


(define-test-claim recorder-core-schema
  (expect-relation-schema
    'recording-copy> :static '(mobile-object mobile-object)
    :fluent-indices '(2))
  (expect-registered :query 'live-recording-object)
  (expect-registered :query 'ghost-recording-object)
  (expect-registered :query 'same-recording-side)
  (expect-registered :query 'recording-shadow-object)
  (expect-registered :query 'recording-shadow-object-present)

  (expect-relation-absent 'recording-depressed :dynamic)
  (expect-relation-absent 'recording-latched :dynamic)
  (expect-relation-absent 'recording-active :dynamic)
  (expect-relation-absent 'recording-open :dynamic)
  (expect-relation-absent 'recording-turning :dynamic)
  (expect-not-registered :query 'recording-plate-occupied)
  (expect-not-registered :query 'recording-controller-energized)
  (expect-not-registered :query 'recording-control-on)
  (expect-not-registered :query 'recording-jammed)
  (expect-not-registered :update 'update-recording-plate-status!)
  (expect-not-registered :update 'update-recording-receiver-status!)
  (expect-not-registered :update 'update-recording-gate-status!)
  (expect-not-registered :update 'update-recording-gears-status!)
  (expect-registrations :solution-validator nil)
  (expect-registrations :solution-printer nil))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query recorder-core-scenarios-valid ()
  (and
    (recording-copy> live-agent ghost-agent)
    (recording-copy> live-connector ghost-connector)
    (live-recording-object live-agent)
    (ghost-recording-object ghost-agent)
    (not (recording-shadow-object live-agent))
    (recording-shadow-object ghost-agent)
    (not (recording-shadow-object unmapped-fan))

    (not (recording-shadow-object-present live-agent))
    (recording-shadow-object-present ghost-agent)
    (recording-shadow-object-present unmapped-fan)
    (recording-shadow-object-present recorder1)

    (object-manipulation-allowed live-agent live-connector)
    (object-manipulation-allowed ghost-agent ghost-connector)
    (not (object-manipulation-allowed live-agent ghost-connector))
    (not (object-manipulation-allowed live-agent unmapped-fan))

    (support-use-allowed live-agent recorder1)
    (support-use-allowed live-agent live-connector)
    (not (support-use-allowed ghost-agent live-connector))
    (not (support-use-allowed live-agent unmapped-fan))

    (connector-pairing-allowed live-agent live-connector ghost-connector)
    (connector-pairing-allowed ghost-agent ghost-connector ghost-connector)
    (not (connector-pairing-allowed
           ghost-agent ghost-connector live-connector))
    (has-position recorder1 recorder-site)))


(define-goal
  (recorder-core-scenarios-valid))

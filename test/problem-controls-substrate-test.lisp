;;; Dedicated zero-action regression for the shared -controls substrate.
;;;
;;; Five independent CONTROLS declarations exercise every valid controlled-device
;;; leaf and verify that each opaque DNF value and supported mode is preserved
;;; exactly.  A test-owned initialization action supplies one ACTIVE receiver
;;; without running beam propagation, giving ENERGIZED both its positive and
;;; negative receiver cases while the conditional plate branch remains absent.
;;; Public gate, gears, and gun behavior must not leak into this shared role.
;;;
;;; Direct validation probes additionally characterize malformed outer and inner
;;; DNF lists, invalid controller members, duplicate wiring for one target, and an
;;; unsupported mode.  The staged facts preserve both legal empty boundaries: ()
;;; has no clauses, while (()) has one vacuously true clause.
;;;
;;; The planner's initial and final dynamic states contain exactly
;;; (ACTIVE ACTIVE-RECEIVER).  No planning action is defined, so the expected
;;; minimum path length is 0.

(in-package :ww)

(ww-set *problem-name* controls-substrate-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 1)
(setf *expected-min-length* 0)


;;;; TYPES ;;;;


(define-types
  receiver (active-receiver inactive-receiver)
  gate (sample-gate)
  floor-gears (sample-floor-gears)
  wall-gears (sample-wall-gears)
  angled-gears (sample-angled-gears)
  gun (sample-gun))


;;;; TECHNOLOGY INCLUDE ;;;;


(include-tech -controls)


;;;; STATIC CONTROL WIRING ;;;;


(define-init
  ;; Each target leaf receives a distinguishable opaque DNF value.
  (controls
    ((active-receiver inactive-receiver) (active-receiver))
    sample-gate
    normal)
  (controls ((inactive-receiver)) sample-floor-gears inverted)
  (controls (()) sample-wall-gears normal)
  (controls () sample-angled-gears inverted)
  (controls ((active-receiver)) sample-gun normal))


;;;; TEST-OWNED INITIALIZATION ;;;;


(define-init-action initialize-controls-substrate-state
  0
  ()
  (always-true)
  ()
  (assert
    ;; ACTIVE is derived state and therefore cannot be authored in DEFINE-INIT.
    ;; Do not propagate: the nested beam role's neutral hooks would retract it.
    (active active-receiver)))


;;;; CHARACTERIZATION CLAIMS ;;;;


(define-test-claim controls-substrate-schema
  (expect-relation-kind 'active :dynamic)
  (expect-relation-kind 'controls :static)
  (expect-registered :query 'energized)
  (expect-registered :update 'update-receiver-status!)
  (expect-not-registered :update 'update-gate-status!)
  (expect-not-registered :update 'update-gears-status!)
  (expect-not-registered :update 'update-gun-status!)
  (expect-type-instances 'mode '(normal inverted))
  (expect-empty-type 'plate)
  (expect-relation-absent 'depressed)
  (expect-relation-absent 'open :dynamic)
  (expect-relation-absent 'turning :dynamic)
  (expect-registrations
    :init-action '(initialize-controls-substrate-state))
  (expect-registrations :action '())
  (equal (database *start-state*) '((active active-receiver)))
  (not (state-is-inconsistent *start-state*)))


(define-test-claim controls-substrate-validation
  (expect-condition
    (lambda ()
      (validate-init-literals
        '((controls active-receiver sample-gate normal))
        :checks '(controls-init-check)))
    'init-check-failure
    :containing "must use a DNF list"
    :check 'controls-init-check)
  (expect-condition
    (lambda ()
      (validate-init-literals
        '((controls (active-receiver) sample-gate normal))
        :checks '(controls-init-check)))
    'init-check-failure
    :containing "must use a DNF list"
    :check 'controls-init-check)
  (expect-condition
    (lambda ()
      (validate-init-literals
        '((controls ((sample-gate)) sample-floor-gears normal))
        :checks '(controls-init-check)))
    'init-check-failure
    :containing "Invalid item"
    :check 'controls-init-check)
  (expect-condition
    (lambda ()
      (validate-init-literals
        '((controls ((active-receiver)) sample-gate normal)
          (controls ((inactive-receiver)) sample-gate inverted))))
    'error
    :containing "Duplicate DEFINE-INIT fluent key")
  (expect-condition
    (lambda ()
      (validate-init-literals
        '((controls ((active-receiver)) sample-gate toggle))
        :checks '(controls-init-check)))
    'init-check-failure
    :containing "unsupported mode"
    :check 'controls-init-check))


;;;; CHARACTERIZATION QUERIES AND GOAL ;;;;


(define-query controls-substrate-family-is
    (?target (either gate floor-gears wall-gears angled-gears gun)
     ?expected
     ?mode mode)
  (do
    (bind (controls $actual ?target ?mode))
    (equal $actual ?expected)))


(define-query controls-substrate-scenarios-valid ()
  (and
    ;; CONTROLS accepts every declared target leaf and preserves its data.
    (controls-substrate-family-is
      sample-gate
      '((active-receiver inactive-receiver) (active-receiver))
      normal)
    (controls-substrate-family-is
      sample-floor-gears
      '((inactive-receiver))
      inverted)
    (controls-substrate-family-is sample-wall-gears '(()) normal)
    (controls-substrate-family-is sample-angled-gears nil inverted)
    (controls-substrate-family-is
      sample-gun
      '((active-receiver))
      normal)

    ;; ENERGIZED reads receiver state directly and distinguishes exact absence.
    (active active-receiver)
    (energized active-receiver)
    (not (active inactive-receiver))
    (not (energized inactive-receiver))))


(define-goal
  (controls-substrate-scenarios-valid))

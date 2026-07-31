;;; Dedicated zero-action regression for the shared -controls substrate.
;;;
;;; Five independent CONTROLS declarations exercise every valid controlled-device
;;; leaf and verify that each opaque DNF value and supported mode is preserved
;;; exactly.  A test-owned initialization action supplies one ACTIVE receiver
;;; without running beam propagation, giving ENERGIZED both its positive and
;;; negative receiver cases while the conditional plate branch remains absent.
;;; Public gate, gears, and gun behavior must not leak into this shared role.
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


;;;; TYPES ;;;;


(define-types
  receiver (active-receiver inactive-receiver)
  gate (sample-gate)
  floor-gears (sample-floor-gears)
  wall-gears (sample-wall-gears)
  angled-gears (sample-angled-gears)
  gun (sample-gun)
  mode (normal inverted))


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


;;;; CHARACTERIZATION HELPERS ;;;;


(setf
  (symbol-function 'controls-substrate-metadata-valid-p)
  (lambda (state)
    (and
      (nth-value 1 (gethash 'active *relations*))
      (not (nth-value 1 (gethash 'active *static-relations*)))
      (nth-value 1 (gethash 'controls *static-relations*))
      (not (nth-value 1 (gethash 'controls *relations*)))
      (member 'energized *query-names*)
      (member 'update-receiver-status! *update-names*)
      (not (member 'update-gate-status! *update-names*))
      (not (member 'update-gears-status! *update-names*))
      (not (member 'update-gun-status! *update-names*))
      (null (gethash 'plate *types*))
      (not (nth-value 1 (gethash 'depressed *relations*)))
      (not (nth-value 1 (gethash 'depressed *static-relations*)))
      (not (nth-value 1 (gethash 'open *relations*)))
      (not (nth-value 1 (gethash 'turning *relations*)))
      (equal
        (mapcar #'action.name *init-actions*)
        '(initialize-controls-substrate-state))
      (null *actions*)
      (equal (database state) '((active active-receiver)))
      (not (state-is-inconsistent state)))))


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
    (not (energized inactive-receiver))

    ;; No public consumer or conditional plate behavior may leak into the role.
    (controls-substrate-metadata-valid-p state)))


(define-goal
  (controls-substrate-scenarios-valid))

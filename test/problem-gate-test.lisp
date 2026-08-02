;;; Dedicated zero-action regression for gate initialization boundaries not
;;; already covered by the broader control characterization.
;;;
;;; NORMAL-GATE begins closed and must open from a recognized NORMAL control
;;; whose one empty clause succeeds vacuously.  STALE-UNCONTROLLED-GATE and
;;; STALE-FALSE-NORMAL-GATE receive stale OPEN facts from the init action before
;;; propagation, which must remove both.  The latter has an empty outer DNF, so
;;; its recognized NORMAL control aggregate is false.
;;;
;;; Before propagation the init action has asserted the two stale OPEN facts.
;;; The planner's initial and final dynamic states contain exactly
;;; (OPEN NORMAL-GATE).  No planning action is needed, so the expected minimum
;;; path length is 0.

(in-package :ww)

(ww-set *problem-name* gate-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 1)
(setf *expected-min-length* 0)


;;;; TYPES ;;;;


(define-types
  gate (normal-gate stale-uncontrolled-gate stale-false-normal-gate))


;;;; TECHNOLOGY INCLUDE ;;;;


(include-tech gate)


;;;; INITIALIZATION ;;;;


(define-init
  ;; One empty clause succeeds vacuously; an empty outer DNF has no successful
  ;; clause.  These exact boundaries must therefore produce opposite results.
  (controls (()) normal-gate normal)
  (controls () stale-false-normal-gate normal))


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert
    ;; Inject stale derived state immediately before propagation so DEFINE-INIT
    ;; remains valid while the lifecycle normalization branch is exercised.
    (open stale-uncontrolled-gate)
    (open stale-false-normal-gate)
    (propagate-changes!)))


;;;; CHARACTERIZATION HELPERS ;;;;


(setf
  (symbol-function 'gate-test-metadata-valid-p)
  (lambda (state)
    (and
      (member 'update-gate-status! *update-names*)
      (null (gethash 'jammer *types*))
      (not (nth-value 1 (gethash 'jamming *relations*)))
      (not (nth-value 1 (gethash 'jamming *static-relations*)))
      (null *actions*)
      (equal (database state) '((open normal-gate)))
      (not (state-is-inconsistent state)))))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query gate-scenarios-valid ()
  (and
    ;; The recognized mode supplies the necessary positive anchor.
    (controls (()) normal-gate normal)
    (open normal-gate)

    ;; An uncontrolled gate normalizes to closed, even from a stale OPEN seed.
    (not
      (bind
        (controls
          $unexpected-uncontrolled-clauses
          stale-uncontrolled-gate
          $unexpected-uncontrolled-mode)))
    (not (open stale-uncontrolled-gate))

    ;; A recognized NORMAL control with no DNF clauses also normalizes closed.
    (controls () stale-false-normal-gate normal)
    (not (open stale-false-normal-gate))

    ;; The metadata helper also requires the complete dynamic database to
    ;; contain no OPEN fact other than NORMAL-GATE.
    (gate-test-metadata-valid-p state)))


(define-goal
  (gate-scenarios-valid))

;;; Dedicated zero-action regression for preservation of a problem-authored
;;; propagation driver.
;;;
;;; GATE first brings in -PROPAGATION's sentinel and contributes a derivable
;;; UPDATE-GATE-STATUS! order.  The problem then authors its own semantically
;;; equivalent driver with a distinctive final (DO ...) wrapper.  Initialization
;;; must recognize that later definition, leave its raw body exactly unchanged,
;;; and never install the mechanically generated body over it.
;;;
;;; The initialization action seeds one stale OPEN fact and invokes propagation.
;;; The preserved authored driver must retract that fact through the ordinary
;;; uncontrolled-gate rule.  The planner's initial and final dynamic states are
;;; empty, no planning action exists, and the expected minimum path length is 0.

(in-package :ww)

(ww-set *problem-name* propagation-authored-driver-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 1)


;;;; TYPES ;;;;


(define-types
  gate (stale-gate))


;;;; TECHNOLOGY INCLUDE ;;;;


(include-tech gate)


;;;; PROBLEM-AUTHORED PROPAGATION DRIVER ;;;;


(define-update propagate-consequences! ()
  ;; The DO wrapper deliberately distinguishes this body from the generated
  ;; driver while preserving the same returned change flag.
  (let ((*propagated-state-changed* nil))
    (update-gate-status!)
    (do *propagated-state-changed*)))


;;;; INITIALIZATION ;;;;


(define-init-action initialize-authored-driver-state
  0
  ()
  (always-true)
  ()
  (assert
    ;; Derived state is injected immediately before propagation so the authored
    ;; driver's execution has an observable lifecycle effect.
    (open stale-gate)
    (propagate-changes!)))


;;;; CHARACTERIZATION HELPER ;;;;


(setf
  (symbol-function 'propagation-authored-driver-metadata-valid-p)
  (lambda (state)
    (let* ((expected-body
             '(let ((*propagated-state-changed* nil))
                (update-gate-status!)
                (do *propagated-state-changed*)))
           (generated-body
             (derived-propagation-driver-body
               '(update-gate-status!)))
           (candidates (driver-candidate-updates))
           (kept
             (remove-if
               #'update-quantifies-only-over-empty-types-p
               candidates))
           (raw-body
             (get 'propagate-consequences! :raw-body)))
      (and
        ;; A generated replacement was available, so preservation is meaningful.
        (equal
          candidates
          '(update-receiver-status! update-gate-status!))
        (equal kept '(update-gate-status!))

        ;; The later problem definition must survive initialization verbatim.
        (equal raw-body expected-body)
        (equal (authored-propagation-driver-body) expected-body)
        (equal
          (authored-propagation-order raw-body)
          '(update-gate-status!))
        (not (equal raw-body *propagation-driver-sentinel*))
        (not (equal raw-body generated-body))

        ;; The empty receiver branch remains absent from both the authored order
        ;; and the resulting state.
        (nth-value 1 (gethash 'receiver *types*))
        (null (gethash 'receiver *types*))

        ;; The stale OPEN witness was removed and no unrelated behavior remains.
        (equal
          (mapcar #'action.name *init-actions*)
          '(initialize-authored-driver-state))
        (null *actions*)
        (null (database state))
        (not (state-is-inconsistent state))))))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query propagation-authored-driver-scenarios-valid ()
  (and
    (not (open stale-gate))
    (not
      (bind
        (controls
          $unexpected-clauses
          stale-gate
          $unexpected-mode)))
    (propagation-authored-driver-metadata-valid-p state)))


(define-goal
  (propagation-authored-driver-scenarios-valid))

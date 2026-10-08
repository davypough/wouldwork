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

(ww-set *problem-name* engine-propagation-authored-driver-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(setf *expected-min-length* 0)
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


;;;; CHARACTERIZATION CLAIM ;;;;


(define-test-claim propagation-authored-driver-contract
  (equal
    (driver-candidate-updates)
    '(update-receiver-status! update-gate-status!))
  (equal
    (remove-if
      #'update-quantifies-only-over-empty-types-p
      (driver-candidate-updates))
    '(update-gate-status!))
  (equal
    (get 'propagate-consequences! :raw-body)
    '(let ((*propagated-state-changed* nil))
       (update-gate-status!)
       (do *propagated-state-changed*)))
  (equal
    (authored-propagation-driver-body)
    '(let ((*propagated-state-changed* nil))
       (update-gate-status!)
       (do *propagated-state-changed*)))
  (equal
    (authored-propagation-order
      (get 'propagate-consequences! :raw-body))
    '(update-gate-status!))
  (not
    (equal
      (get 'propagate-consequences! :raw-body)
      *propagation-driver-sentinel*))
  (not
    (equal
      (get 'propagate-consequences! :raw-body)
      (derived-propagation-driver-body '(update-gate-status!))))
  (expect-empty-type 'receiver)
  (expect-registrations
    :init-action '(initialize-authored-driver-state))
  (expect-registrations :action '())
  (null (database *start-state*))
  (not (state-is-inconsistent *start-state*)))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query propagation-authored-driver-scenarios-valid ()
  (and
    (not (open stale-gate))
    (not
      (bind
        (controls
          $unexpected-clauses
          stale-gate
          $unexpected-mode)))))


(define-goal
  (propagation-authored-driver-scenarios-valid))

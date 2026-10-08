;;; Dedicated zero-action regression for selective propagation-candidate
;;; filtering.
;;;
;;; GATE contributes UPDATE-GATE-STATUS! and, through its nested -CONTROLS and
;;; -BEAM-SUBSTRATE roles, UPDATE-RECEIVER-STATUS!.  This problem declares one
;;; gate but no receivers.  Candidate discovery must therefore find both
;;; updates, empty-type filtering must remove only the receiver update, and
;;; initialization must install a driver containing exactly the gate update.
;;;
;;; The characterization goal invokes PROPAGATE-CHANGES! directly.  The
;;; uncontrolled, unjammed gate remains closed, proving that the installed
;;; one-update driver executes normally without introducing unrelated gate
;;; control behavior.  The initial and final dynamic states are empty, no
;;; action or initialization action exists, and the expected minimum path
;;; length is 0.

(in-package :ww)

(ww-set *problem-name* engine-propagation-mixed-candidate-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 1)
(setf *expected-min-length* 0)


;;;; TYPES ;;;;


(define-types
  gate (sample-gate))


;;;; TECHNOLOGY INCLUDE ;;;;


(include-tech gate)


;;;; CHARACTERIZATION CLAIM ;;;;


(define-test-claim propagation-mixed-candidate-contract
  (equal
    (driver-candidate-updates)
    '(update-receiver-status! update-gate-status!))
  (update-quantifies-only-over-empty-types-p 'update-receiver-status!)
  (not (update-quantifies-only-over-empty-types-p 'update-gate-status!))
  (equal
    (remove-if
      #'update-quantifies-only-over-empty-types-p
      (driver-candidate-updates))
    '(update-gate-status!))
  (equal
    (derived-propagation-order '(update-gate-status!))
    '(update-gate-status!))
  (not
    (equal
      (get 'propagate-consequences! :raw-body)
      *propagation-driver-sentinel*))
  (equal
    (get 'propagate-consequences! :raw-body)
    (derived-propagation-driver-body '(update-gate-status!)))
  (expect-type-instances 'gate '(sample-gate))
  (expect-empty-type 'receiver)
  (let* ((trial (copy-problem-state *start-state*))
         (before (database trial)))
    (and
      (funcall 'propagate-changes! trial)
      (equal (database trial) before)))
  (expect-registrations :init-action '())
  (expect-registrations :action '())
  (null (database *start-state*))
  (not (state-is-inconsistent *start-state*)))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query propagation-mixed-candidate-scenarios-valid ()
  (and
    (not (open sample-gate))
    (not
      (bind
        (controls
          $unexpected-clauses
          sample-gate
          $unexpected-mode)))))


(define-goal
  (propagation-mixed-candidate-scenarios-valid))

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


;;;; CHARACTERIZATION HELPER ;;;;


(setf
  (symbol-function 'propagation-mixed-candidate-metadata-valid-p)
  (lambda (state)
    (let* ((candidates (driver-candidate-updates))
           (kept
             (remove-if
               #'update-quantifies-only-over-empty-types-p
               candidates))
           (expected-order '(update-gate-status!))
           (before (database state)))
      (and
        ;; The nested beam role still contributes its update before filtering.
        (equal
          candidates
          '(update-receiver-status! update-gate-status!))

        ;; Exactly the empty RECEIVER candidate is inert.
        (update-quantifies-only-over-empty-types-p
          'update-receiver-status!)
        (not
          (update-quantifies-only-over-empty-types-p
            'update-gate-status!))
        (equal kept expected-order)
        (equal (derived-propagation-order kept) expected-order)

        ;; A nonempty retained order must replace the sentinel with the exact
        ;; generated driver body.
        (not
          (equal
            (get 'propagate-consequences! :raw-body)
            *propagation-driver-sentinel*))
        (equal
          (get 'propagate-consequences! :raw-body)
          (derived-propagation-driver-body expected-order))

        ;; The type populations establish the filtering boundary.
        (equal (gethash 'gate *types*) '(sample-gate))
        (nth-value 1 (gethash 'receiver *types*))
        (null (gethash 'receiver *types*))

        ;; The installed driver must be callable and preserve this neutral
        ;; state rather than reaching the sentinel.
        (funcall
          (symbol-function 'propagate-changes!)
          state)
        (equal (database state) before)

        ;; No unrelated mechanism can satisfy the zero-step goal.
        (null *init-actions*)
        (null *actions*)
        (null (database state))
        (not (state-is-inconsistent state))))))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query propagation-mixed-candidate-scenarios-valid ()
  (and
    (not (open sample-gate))
    (not
      (bind
        (controls
          $unexpected-clauses
          sample-gate
          $unexpected-mode)))
    (propagation-mixed-candidate-metadata-valid-p state)))


(define-goal
  (propagation-mixed-candidate-scenarios-valid))

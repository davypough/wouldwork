;;; Dedicated zero-action regression for propagation-driver installation when
;;; a technology contributes a real candidate update whose quantified type is
;;; empty.
;;;
;;; -BEAM-SUBSTRATE contributes UPDATE-RECEIVER-STATUS!, so the raw candidate
;;; list must contain that update.  This problem declares no receivers, however,
;;; making the update provably inert.  Installation must filter it out and leave
;;; PROPAGATE-CONSEQUENCES!'s explicit sentinel intact rather than installing a
;;; silent no-op driver.
;;;
;;; A characterization helper invokes PROPAGATE-CHANGES! and requires the
;;; sentinel error while confirming that the failed call changes no state.
;;; The initial and final dynamic states are empty, no action or initialization
;;; action exists, and the expected minimum path length is 0.

(in-package :ww)

(ww-set *problem-name* propagation-inert-candidate-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 1)


;;;; TECHNOLOGY INCLUDE ;;;;


(include-tech -beam-substrate)


;;;; SENTINEL CONDITION CHARACTERIZATION ;;;;


(setf
  (symbol-function 'inert-candidate-driver-signals-p)
  (lambda (state)
    (let ((before (database state))
          (condition nil))
      (setf condition
        (handler-case
            (progn
              (funcall
                (symbol-function 'propagate-changes!)
                state)
              nil)
          (error (error-condition)
            error-condition)))
      (and
        condition
        (search
          "still holding tech/-propagation.lisp's sentinel body"
          (princ-to-string condition))
        (equal (database state) before)
        (not (state-is-inconsistent state))))))


;;;; CHARACTERIZATION HELPERS ;;;;


(setf
  (symbol-function 'propagation-inert-candidate-metadata-valid-p)
  (lambda (state)
    (let ((candidates (driver-candidate-updates)))
      (and
        ;; Candidate discovery must see the substrate update before filtering.
        (equal candidates '(update-receiver-status!))
        (update-quantifies-only-over-empty-types-p
          'update-receiver-status!)
        (null
          (remove-if
            #'update-quantifies-only-over-empty-types-p
            candidates))

        ;; An all-inert candidate set must not displace the loud sentinel.
        (equal
          (get 'propagate-consequences! :raw-body)
          *propagation-driver-sentinel*)
        (null (authored-propagation-driver-body))

        ;; The exact boundary is an installed but empty optional RECEIVER type.
        (nth-value 1 (gethash 'receiver *types*))
        (null (gethash 'receiver *types*))

        ;; No authored state or action can make the zero-step goal pass
        ;; independently of the propagation metadata being characterized.
        (null *init-actions*)
        (null *actions*)
        (null (database state))
        (not (state-is-inconsistent state))))))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query propagation-inert-candidate-scenarios-valid ()
  (and
    (inert-candidate-driver-signals-p state)
    (propagation-inert-candidate-metadata-valid-p state)))


(define-goal
  (propagation-inert-candidate-scenarios-valid))

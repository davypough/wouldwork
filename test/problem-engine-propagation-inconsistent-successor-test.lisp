;;; Dedicated action-level regression for -PROPAGATION's nonconvergent-successor
;;; lifecycle.
;;;
;;; TAKE-INVALID-SHORTCUT is deliberately applicable from every state.  It moves
;;; directly to FINISHED, enables a test-owned update that changes PASS-COUNT on
;;; every propagation pass, and calls PROPAGATE-CHANGES!.  The tenth changing
;;; pass must add INCONSISTENT-STATE, and GENERATE-CHILDREN must discard the
;;; resulting update rather than expose a one-step shortcut.
;;;
;;; PREPARE-VALID-ROUTE and FINISH-VALID-ROUTE call the same propagation driver
;;; without enabling nonconvergence.  Both must converge on their first unchanged
;;; pass, preserve PASS-COUNT 0, and produce the only solution.
;;;
;;; The goal directly characterizes the installed shortcut action: its
;;; precondition must be valid, it must produce no legitimate child, the
;;; discarded-inconsistency count must rise by exactly one during the isolated
;;; probe, and the probe must leave its parent unchanged.  The counter is then
;;; restored.
;;;
;;; Initial state: AT-PHASE START, SHORTCUT-READY PROBE1, PASS-COUNT 0.
;;; Final state: AT-PHASE FINISHED, SHORTCUT-READY PROBE1, PASS-COUNT 0.
;;; AT-PHASE START, AT-PHASE PREPARED, NONCONVERGENT, and INCONSISTENT-STATE
;;; must be absent.  Expected minimum solution (2 steps): PREPARE-VALID-ROUTE;
;;; FINISH-VALID-ROUTE.  The real search must abandon two inconsistent shortcut
;;; candidates, one from each expanded valid state.

(in-package :ww)

(ww-set *problem-name* engine-propagation-inconsistent-successor-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 2)

(setf *expected-min-length* 2)


;;;; TYPES AND TECHNOLOGY INCLUDE ;;;;


(define-types
  phase (start prepared finished)
  probe (probe1))


(include-tech -propagation)


;;;; TEST-OWNED PROPAGATED STATE ;;;;


(define-dynamic-relations
  (at-phase phase)
  (shortcut-ready probe)
  (nonconvergent probe)
  (pass-count $fixnum))


(define-update increment-pass-count-while-nonconvergent! ()
  (if (nonconvergent probe1)
    (do
      (bind (pass-count $count))
      (pass-count (1+ $count)))))


(define-update propagate-consequences! ()
  (let ((*propagated-state-changed* nil))
    (increment-pass-count-while-nonconvergent!)
    *propagated-state-changed*))


;;;; INITIALIZATION ;;;;


(define-init
  (at-phase start)
  (shortcut-ready probe1)
  (pass-count 0))


;;;; INVALID AND VALID ROUTES ;;;;


(define-action take-invalid-shortcut
  1
  (?probe probe)
  (shortcut-ready ?probe)
  ("> test shortcut attempts a nonconvergent finish")
  (assert
    (not (at-phase start))
    (not (at-phase prepared))
    (at-phase finished)
    (nonconvergent ?probe)
    (finally (propagate-changes!))))


(define-action prepare-valid-route
  1
  ()
  (at-phase start)
  ("> test route is prepared")
  (assert
    (not (at-phase start))
    (at-phase prepared)
    (finally (propagate-changes!))))


(define-action finish-valid-route
  1
  ()
  (at-phase prepared)
  ("> test route is finished")
  (assert
    (not (at-phase prepared))
    (at-phase finished)
    (finally (propagate-changes!))))


;;;; GENERATED-CHILD CHARACTERIZATION ;;;;


(defun propagation-inconsistent-shortcut-rejected-p (state probe)
  "Whether an applicable shortcut is discarded without changing STATE."
  (let* ((action
           (find 'take-invalid-shortcut *actions* :key #'action.name))
         (args (list probe))
         (before (database state))
         (saved-dropped-count *inconsistent-states-dropped*)
         (precondition-result
           (and
             (member args
                     (get-precondition-args action state)
                     :test #'equal)
             (apply (action.pre-defun-name action) state args))))
    (and
      precondition-result
      (unwind-protect
        (let* ((*actions* (list action))
               (children
                 (generate-children
                   (make-node :state state :depth 0))))
          (and
            (null children)
            (= *inconsistent-states-dropped*
               (1+ saved-dropped-count))
            (equal (database state) before)
            (not (state-is-inconsistent state))))
        (setf *inconsistent-states-dropped*
              saved-dropped-count)))))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query propagation-inconsistent-successor-scenarios-valid ()
  (and
    (at-phase finished)
    (shortcut-ready probe1)
    (pass-count 0)
    (not (at-phase start))
    (not (at-phase prepared))
    (not (nonconvergent probe1))
    (not (inconsistent-state))
    (propagation-inconsistent-shortcut-rejected-p state 'probe1)))


(define-goal
  (propagation-inconsistent-successor-scenarios-valid))

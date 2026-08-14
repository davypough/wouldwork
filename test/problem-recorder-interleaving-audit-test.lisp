;;; Filename: problem-recorder-interleaving-audit-test.lisp

;;; Focused characterization of audit-only live/ghost interleaving analysis.  Independent
;;; actions commute exactly, while the dependent live action requires the ghost's result
;;; and therefore cannot be moved before it.  The audit records both outcomes and never
;;; rejects either generated successor.

(in-package :ww)


(ww-set *problem-name* recorder-interleaving-audit-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 2)
(ww-set *recorder-interleaving-audit* t)

(setf *expected-min-length* 2)


(define-types
  agent (live-agent ghost-agent)
  recorder (recorder1)
  location (recorder-site))


(include-tech recorder)


(define-dynamic-relations
  (live-done)
  (ghost-done)
  (dependent-done))


(define-init
  (recording-copy> live-agent ghost-agent)
  (recording-in-progress)
  (has-location live-agent recorder-site)
  (has-location ghost-agent recorder-site)
  (has-position recorder1 recorder-site))


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!)))


(define-action ghost-independent
  1
  (?agent agent)
  (and (ghost-recording-object ?agent)
       (not (ghost-done)))
  (">" ?agent "does an independent ghost task")
  (assert (ghost-done)))


(define-action live-independent
  1
  (?agent agent)
  (and (live-recording-object ?agent)
       (not (live-done)))
  (">" ?agent "does an independent live task")
  (assert (live-done)))


(define-action live-dependent
  1
  (?agent agent)
  (and (live-recording-object ?agent)
       (ghost-done)
       (not (dependent-done)))
  (">" ?agent "uses the ghost task's result")
  (assert (dependent-done)))


(define-test-helper audit-recorder-action-pair (first-action second-action)
  "Generate FIRST-ACTION then SECOND-ACTION and submit the second successor to the audit."
  (multiple-value-bind (first-state first-valid-p first-reason)
      (apply-action-to-state first-action *start-state* second-action)
    (declare (ignore first-reason))
    (unless first-valid-p
      (return-from audit-recorder-action-pair nil))
    (multiple-value-bind (second-state second-valid-p second-reason)
        (apply-action-to-state second-action first-state nil)
      (declare (ignore second-reason))
      (unless second-valid-p
        (return-from audit-recorder-action-pair nil))
      (let* ((start-node
               (make-node :state (copy-problem-state *start-state*) :depth 0))
             (first-node
               (make-node :state first-state :depth 1 :parent start-node)))
        (audit-search-successor first-node second-state)
        second-state))))


(define-test-claim recorder-interleaving-audit-contract
  (find 'audit-recorder-interleaving-successor
        *search-successor-auditors*
        :key #'search-successor-auditor.auditor)
  (progn
    (reset-search-successor-audits)
    (audit-recorder-action-pair
      '(ghost-independent ghost-agent)
      '(live-independent live-agent))
    (audit-recorder-action-pair
      '(ghost-independent ghost-agent)
      '(live-dependent live-agent))
    (and (= *recorder-interleaving-opposite-side-pairs* 2)
         (= *recorder-interleaving-inversions* 2)
         (= *recorder-interleaving-certified* 1)
         (= (gethash
              '(ghost-independent live-independent :certified)
              *recorder-interleaving-audit-results*
              0)
            1)
         (= (gethash
              '(ghost-independent live-dependent :alternate-first-unavailable)
              *recorder-interleaving-audit-results*
              0)
            1)))
  (let ((output
          (with-output-to-string (stream)
            (print-search-successor-audit-statistics stream))))
    (and (search "Certified interchangeable inversions = 1" output)
         (search "GHOST-INDEPENDENT -> LIVE-INDEPENDENT / CERTIFIED = 1"
                 output))))


(define-goal
  (and (ghost-done) (live-done)))

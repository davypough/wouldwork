;;; Filename: problem-recorder-interleaving-pruning-test.lisp

;;; Focused characterization of exact live/ghost interleaving pruning.  The noncanonical
;;; ghost-then-live ordering of two independent actions is rejected after exact replay;
;;; the canonical live-then-ghost ordering and a causally dependent ghost-then-live pair
;;; are both retained.

(in-package :ww)


(ww-set *problem-name* recorder-interleaving-pruning-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 2)
(ww-set *recorder-interleaving-pruning* t)

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


(defparameter *recorder-interleaving-extra-prefix-enabled* nil)
(defparameter *recorder-interleaving-extra-prefix-calls* 0)


(define-test-helper recorder-interleaving-extra-prefix-enabled-p ()
  *recorder-interleaving-extra-prefix-enabled*)


(define-test-helper validate-recorder-interleaving-extra-prefix
    (start-state path current-state)
  "Reject the canonical independent pair when this focused test enables the policy."
  (declare (ignore start-state current-state))
  (incf *recorder-interleaving-extra-prefix-calls*)
  (not (equal
         (mapcar #'recorder-move-action-name (last path 2))
         '(live-independent ghost-independent))))


(register-search-prefix-validator
  'validate-recorder-interleaving-extra-prefix
  'recorder-interleaving-extra-prefix-enabled-p)


(define-test-helper pruning-recorder-action-pair (first-action second-action)
  "Generate an adjacent action pair and return whether successor pruning rejects it."
  (multiple-value-bind (first-state first-valid-p first-reason)
      (apply-action-to-state first-action *start-state* second-action)
    (declare (ignore first-reason))
    (unless first-valid-p
      (return-from pruning-recorder-action-pair nil))
    (multiple-value-bind (second-state second-valid-p second-reason)
        (apply-action-to-state second-action first-state nil)
      (declare (ignore second-reason))
      (unless second-valid-p
        (return-from pruning-recorder-action-pair nil))
      (let* ((start-node
               (make-node :state (copy-problem-state *start-state*) :depth 0))
             (first-node
               (make-node :state first-state :depth 1 :parent start-node)))
        (search-successor-pruned-p first-node second-state)))))


(define-test-helper pruning-recorder-prefix-call-count ()
  "Return whether the independent inversion is pruned and recorder-prefix call count."
  (let ((original-validator
          (symbol-function 'validate-recorder-recording-prefix))
        (calls 0))
    (unwind-protect
      (progn
        (setf (symbol-function 'validate-recorder-recording-prefix)
              (lambda (&rest arguments)
                (incf calls)
                (apply original-validator arguments)))
        (values
          (pruning-recorder-action-pair
            '(ghost-independent ghost-agent)
            '(live-independent live-agent))
          calls))
      (setf (symbol-function 'validate-recorder-recording-prefix)
            original-validator))))


(define-test-claim recorder-interleaving-reuses-accepted-recorder-prefix
  (progn
    (reset-search-successor-audits)
    (let ((*recorder-prefix-pruning* t))
      (multiple-value-bind (pruned recorder-prefix-calls)
          (pruning-recorder-prefix-call-count)
        (and pruned (zerop recorder-prefix-calls))))))


(define-test-claim recorder-interleaving-pruning-contract
  (find 'prune-recorder-interleaving-successor-p
        *search-successor-pruners*
        :key #'search-successor-pruner.pruner)
  (progn
    (reset-search-successor-audits)
    (let ((independent-inversion-pruned
            (pruning-recorder-action-pair
              '(ghost-independent ghost-agent)
              '(live-independent live-agent)))
          (dependent-inversion-pruned
            (pruning-recorder-action-pair
              '(ghost-independent ghost-agent)
              '(live-dependent live-agent)))
          (canonical-order-pruned
            (pruning-recorder-action-pair
              '(live-independent live-agent)
              '(ghost-independent ghost-agent))))
      (and independent-inversion-pruned
           (not dependent-inversion-pruned)
           (not canonical-order-pruned)
           (= *recorder-interleaving-opposite-side-pairs* 3)
           (= *recorder-interleaving-inversions* 2)
           (= *recorder-interleaving-certified* 1)
           (= *recorder-interleaving-pruned* 1))))
  (let ((output
          (with-output-to-string (stream)
            (print-search-successor-audit-statistics stream))))
    (and (search "Recorder live/ghost interleaving pruning" output)
         (search "Canonical interleavings pruned = 1" output))))


(define-test-claim recorder-interleaving-honors-other-prefix-validators
  (progn
    (reset-search-successor-audits)
    (let ((*recorder-interleaving-extra-prefix-enabled* t)
          (*recorder-interleaving-extra-prefix-calls* 0))
      (and
        (not
          (pruning-recorder-action-pair
            '(ghost-independent ghost-agent)
            '(live-independent live-agent)))
        (= *recorder-interleaving-extra-prefix-calls* 2)
        (zerop *recorder-interleaving-certified*)
        (zerop *recorder-interleaving-pruned*)
        (= (gethash
             '(ghost-independent live-independent :alternate-second-invalid)
             *recorder-interleaving-audit-results*
             0)
           1)))))


(define-goal
  (and (ghost-done) (live-done)))

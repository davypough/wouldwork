;;; Filename: problem-staging-goal-chaining-policy-test.lisp

;;; Characterizes staging isolation for goal-chaining policy.  This file sorts after the
;;; recorder orchestration test, so a full test run also verifies that staging a subsequent
;;; ordinary problem clears the recorder's policy rather than leaking recorder dispatch.
;;; Expected minimum path length: zero.

(in-package :ww)


(ww-set *problem-name* staging-goal-chaining-policy-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 0)

(setf *expected-min-length* 0)


(define-types
  marker (staging-policy-marker))


(define-dynamic-relations
  (policy-test-ready marker))


(define-init
  (policy-test-ready staging-policy-marker))


(define-test-claim staging-clears-goal-chaining-policy
  (and (boundp '*goal-chaining-policy*)
       (null (symbol-value '*goal-chaining-policy*))))


(define-goal
  (policy-test-ready staging-policy-marker))

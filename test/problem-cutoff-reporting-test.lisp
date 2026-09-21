;;; Filename: problem-cutoff-reporting-test.lisp

;;; A two-edge path with an unreachable goal distinguishes a live frontier,
;;; terminal nodes exactly at the cutoff, and exhaustion before the cutoff.
;;; This is an intentional no-solution Talos characterization; the dedicated
;;; cutoff-reporting checks validate the detailed cutoff counters and reasons.
(in-package :ww)
(ww-set *problem-name* cutoff-reporting-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* first)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 1)

(setf *expected-search-status* :exhausted-no-solution
      *expected-search-reason* :depth-cutoff-truncated)

(define-types cutoff-position (cutoff-start cutoff-middle cutoff-end cutoff-unreachable))
(define-dynamic-relations (cutoff-at cutoff-position))
(define-init (cutoff-at cutoff-start))
(define-action cutoff-first-step
  1 () (cutoff-at cutoff-start) ()
  (assert (not (cutoff-at cutoff-start)) (cutoff-at cutoff-middle)))
(define-action cutoff-last-step
  1 () (cutoff-at cutoff-middle) ()
  (assert (not (cutoff-at cutoff-middle)) (cutoff-at cutoff-end)))
(define-goal (cutoff-at cutoff-unreachable))

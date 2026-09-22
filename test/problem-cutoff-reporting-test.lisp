;;; Solvable two-edge fixture for the ordinary Talos sweep. The dedicated
;;; cutoff-reporting checks request CUTOFF-UNREACHABLE explicitly to distinguish
;;; live frontiers, terminal cutoff nodes, and exhaustion before the cutoff.
(in-package :ww)
(ww-set *problem-name* cutoff-reporting-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 2)
(setf *expected-min-length* 2)

(define-types cutoff-position (cutoff-start cutoff-middle cutoff-end cutoff-unreachable))
(define-dynamic-relations (cutoff-at cutoff-position))
(define-init (cutoff-at cutoff-start))
(define-action cutoff-first-step
  1 () (cutoff-at cutoff-start) ()
  (assert (not (cutoff-at cutoff-start)) (cutoff-at cutoff-middle)))
(define-action cutoff-last-step
  1 () (cutoff-at cutoff-middle) ()
  (assert (not (cutoff-at cutoff-middle)) (cutoff-at cutoff-end)))
(define-goal (cutoff-at cutoff-end))

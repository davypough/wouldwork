;;; Filename: problem-task-goal-incumbent.lisp
;;; Task generation must retain the value-10 incumbent when a later goal has 5.
;;; Depth fixes encounter order: BEST-FINISH is at depth 1, WORSE-FINISH at 2.
;;; With four threads and default task settings, the whole search finishes
;;; during task generation. MAX-VALUE records only BEST-FINISH (value 10).
;;; EVERY must still record both distinct goals. No worker scheduling is involved.

(in-package :ww)

(ww-set *problem-name* task-goal-incumbent)
(ww-set *problem-type* planning)
(ww-set *solution-type* max-value)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 2)

(define-types
  spot (start-spot middle-spot best-goal worse-goal))

(define-dynamic-relations
  (position spot))

(define-action best-finish
    1
  ()
  (position start-spot)
  ()
  (assert (position best-goal)
          (not (position start-spot))
          (assign $objective-value 10)))

(define-action detour
    1
  ()
  (position start-spot)
  ()
  (assert (position middle-spot)
          (not (position start-spot))
          (assign $objective-value 2)))

(define-action worse-finish
    1
  ()
  (position middle-spot)
  ()
  (assert (position worse-goal)
          (not (position middle-spot))
          (assign $objective-value 5)))

(define-init
  (position start-spot))

(define-goal
  (or (position best-goal) (position worse-goal)))

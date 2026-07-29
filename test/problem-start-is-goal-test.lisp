;;; Filename: problem-start-is-goal-test.lisp

;;; Focused search-lifecycle problem.  The initial position satisfies the goal,
;;; and two one-step actions reach distinct goal states with lower/higher values.


(in-package :ww)


(ww-set *problem-name* start-is-goal-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* first)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 1)


(define-types
  start-goal-place (start-goal-center start-goal-low start-goal-high))


(define-dynamic-relations
  (start-goal-position start-goal-place))


(define-action move-to-low-goal
    1
  ()
  (start-goal-position start-goal-center)
  ()
  (assert (start-goal-position start-goal-low)
          (not (start-goal-position start-goal-center))
          (assign $objective-value -10)))


(define-action move-to-high-goal
    1
  ()
  (start-goal-position start-goal-center)
  ()
  (assert (start-goal-position start-goal-high)
          (not (start-goal-position start-goal-center))
          (assign $objective-value 10)))


(define-init
  (start-goal-position start-goal-center))


(define-goal
  (or (start-goal-position start-goal-center)
      (start-goal-position start-goal-low)
      (start-goal-position start-goal-high)))

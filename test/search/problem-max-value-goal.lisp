;;; Filename: problem-max-value-goal.lisp


;;; Engine test problem: a max-value search with a goal, where the best path
;;; passes through low-value states. QUICK-FINISH reaches the goal at once
;;; with value 5; DETOUR, CONTINUE-DETOUR, FINISH visits values 2 and 3 before
;;; reaching the goal with value 10.
;;; Expected: a 3-step solution with value 10, in both serial and parallel search.
;;; With *split-depth-max* = 1 and *threads* = 4, task generation registers 5
;;; and hands MIDDLE-SPOT (value 2) to a worker. The worker must retain its
;;; CONTINUE-DETOUR successor at LAST-SPOT (value 3) and then reach value 10.
;;; This exercises both worker task pruning and non-goal successor pruning
;;; against an existing incumbent. Pruning either low-value state is unsound.


(in-package :ww)  ;required


(ww-set *problem-name* max-value-goal)

(ww-set *problem-type* planning)

(ww-set *solution-type* max-value)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 3)


(define-types
    spot (start-spot middle-spot last-spot goal-spot))


(define-dynamic-relations
    (position spot))


(define-action quick-finish
    1
  ()
  (position start-spot)
  ()
  (assert (position goal-spot)
          (not (position start-spot))
          (assign $objective-value 5)))


(define-action detour
    1
  ()
  (position start-spot)
  ()
  (assert (position middle-spot)
          (not (position start-spot))
          (assign $objective-value 2)))


(define-action continue-detour
    1
  ()
  (position middle-spot)
  ()
  (assert (position last-spot)
          (not (position middle-spot))
          (assign $objective-value 3)))


(define-action finish
    1
  ()
  (position last-spot)
  ()
  (assert (position goal-spot)
          (not (position last-spot))
          (assign $objective-value 10)))


(define-init
  (position start-spot))


(define-goal
  (position goal-spot))

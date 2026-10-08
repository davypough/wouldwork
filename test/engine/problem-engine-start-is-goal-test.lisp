;;; Filename: problem-engine-start-is-goal-test.lisp

;;; Focused search-lifecycle problem.  The initial position satisfies the goal, and two
;;; one-step actions reach distinct goal states with lower/higher values for the broader
;;; non-FIRST lifecycle matrix in TEST-START-IS-GOAL.
;;;
;;; In this file's focused FIRST configuration, only the initialized center may be
;;; accepted.  START-IS-GOAL-VALID signals if the search evaluates either one-step state
;;; as a possible fallback goal, so a broken start-state check cannot still appear to
;;; solve the problem at depth 1.


(in-package :ww)


(ww-set *problem-name* engine-start-is-goal-test)

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


(define-query start-is-goal-valid ()
  (if (start-goal-position start-goal-center)
    t
    (if (eql *solution-type* 'first)
      (error "FIRST search expanded past the already-satisfied start-state goal.")
      (or (start-goal-position start-goal-low)
          (start-goal-position start-goal-high)))))


(define-goal
  (start-is-goal-valid))

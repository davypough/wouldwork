;;; Small sibling-order fixture: parameters, separate actions, and multiple asserts.
(in-package :ww)
(ww-set *problem-name* backtracking-heuristic)
(ww-set *problem-type* planning)
(ww-set *solution-type* first)
(ww-set *depth-cutoff* 2)
(define-types score (1 2))
(define-dynamic-relations (ready) (done) (finished))
(define-init (ready))
(define-action parameter-choice
  1 (?score score) (ready) (?score)
  (assert (not (ready)) (done) (assign $objective-value ?score)))
(define-action later-choice
  2 () (ready) ()
  (assert (not (ready)) (done) (assign $objective-value 0)))
(define-action multiple-choices
  3 () (ready) ()
  (progn
    (assert (not (ready)) (done) (assign $objective-value 4))
    (assert (not (ready)) (done) (assign $objective-value 5))))
(define-goal (done))

(define-action finish-choice
  1 () (done) ()
  (assert (not (done)) (finished) (assign $objective-value 10)))

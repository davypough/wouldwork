;;; Filename: problem-count-goals.lisp
;;; A binary assignment tree with 16 distinct depth-four goals.
(in-package :ww)

(ww-set *problem-name* count-goals)
(ww-set *problem-type* planning)
(ww-set *tree-or-graph* tree)
(ww-set *solution-type* count)

(defparameter *count-test-goal-depth* 4)
(defparameter *count-test-early-goal* nil)

(define-types digit (0 1))
(define-dynamic-relations (progress $fixnum $fixnum))

(define-action append-digit
  1
  (?digit digit)
  (and (bind (progress $depth $code)) (< $depth 4))
  (?digit)
  (assert (progress (1+ $depth) (+ (* 2 $code) ?digit))))

(define-init (progress 0 0))
(define-goal
  (and (bind (progress $depth $code))
       (or (= $depth *count-test-goal-depth*)
           (and *count-test-early-goal* (= $depth 1) (= $code 0)))))

(define-query count-test-even? ()
  (and (bind (progress $depth $code)) (evenp $code)))

;;;; Filename: problem-jugs4.lisp

;;; Fluent problem specification for pouring between 4 jugs
;;; to achieve 10 gal.


(in-package :ww)  ;required

(ww-set *problem-name* jugs4)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *depth-cutoff* 6)


(define-types
    jug (jug1 jug2 jug3 jug4))


(define-dynamic-relations
    (contents jug $real))


(define-static-relations
    (capacity jug $real))


(define-action pour  ;A into B
    1
  ((?jugA ?jugB) jug)
  (and (bind (contents ?jugA $amtA))
       (> $amtA 0)
       (bind (contents ?jugB $amtB))
       (bind (capacity ?jugB $capB))
       (< $amtB $capB))
  (?jugA $amtA ?jugB $amtB $capB)
  (if (<= $amtA (- $capB $amtB))
    (assert (contents ?jugA 0)
            (contents ?jugB (+ $amtB $amtA)))
    (assert (contents ?jugA (- $amtA (- $capB $amtB)))
            (contents ?jugB $capB))))
                 

(define-init
    (contents jug1 9)
    (contents jug2 0)
    (contents jug3 0)
    (contents jug4 4)
    (capacity jug1 9)
    (capacity jug2 4)
    (capacity jug3 12)
    (capacity jug4 9))


(define-goal
    (contents jug3 10))

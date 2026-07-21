;;; Filename: problem-quern.lisp

;;; Fluent problem specification for pouring between 4 jugs
;;; to achieve 6 gal.


(in-package :ww)  ;required

(ww-set *problem-name* quern)

(ww-set *problem-type* planning)

(ww-set *depth-cutoff* 8)  ;set to expected # steps to goal

(ww-set *solution-type* first)  ; min-length)


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
  (assert (if (<= $amtA (- $capB $amtB))
            (do (contents ?jugA 0)
                (contents ?jugB (+ $amtB $amtA)))
            (do (contents ?jugA (- $amtA (- $capB $amtB)))
                (contents ?jugB $capB)))))
                 

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
    (contents jug3 6))

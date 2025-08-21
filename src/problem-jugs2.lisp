;;; Filename: problem-jugs2.lisp


;;; Fluent problem specification for pouring between jugs
;;; to achieve 1 gal given 2-gal jug & 5-gal jug.


(in-package :ww)  ;required

(ww-set *problem-name* jugs2)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-time)

(ww-set *tree-or-graph* graph)


(define-types
    jug (jug1 jug2))


(define-dynamic-relations
    (contents jug $integer))


(define-static-relations
    (capacity jug $integer))


(define-action fill
    2
  (?jug jug)
  (and (bind (contents ?jug $amt))
       (bind (capacity ?jug $cap))
       (< $amt $cap))
  (?jug $cap)
  (assert (contents ?jug $cap)))


(define-action empty
    10
  (?jug jug)
  (and (bind (contents ?jug $amt))
       (> $amt 0))
  (?jug)
  (assert (contents ?jug 0)))


(define-action pour  ;A into B
    3
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
    (assert (contents ?jugA (- (+ $amtA $amtB) $capB))
            (contents ?jugB $capB))))


(define-init
    (contents jug1 0)
    (contents jug2 0)
    (capacity jug1 2)
    (capacity jug2 5))


(define-goal
    (or (contents jug1 1)
        (contents jug2 1)))

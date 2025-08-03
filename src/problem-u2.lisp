;;; Filename: problem-u2.lisp

;;; Problem specification for Braingle U2 bridge problem.


(in-package :ww)  ;required

(ww-set *problem-name* u2)

(ww-set *problem-type* planning)

(ww-set *depth-cutoff* 5)

(ww-set *solution-type* min-length)


(define-types
  person        (bono edge adam larry)
  flashlight    (lite)
  object        (either person flashlight)
  side          (side1 side2))  ;bridge sides


(define-dynamic-relations
  (on object side)
  (current-time $fixnum))


(define-static-relations
  (walk-time person $fixnum))



(define-action move1
    1
  (?person person (?side1 ?side2) side)
  (and (on ?person ?side1)
       (on lite ?side1)
       (bind (walk-time ?person $walk-time))
       (bind (current-time $current-time))
       (<= (+ $current-time $walk-time) 17)) 
  (?person ?side1 ?side2 $walk-time $current-time)
  (assert (not (on ?person ?side1))
          (on ?person ?side2)
          (not (on lite ?side1))
          (on lite ?side2)
          (current-time (+ $current-time $walk-time))))


(define-action move2
    1
  ((?person1 ?person2) person (?side1 ?side2) side)
  (and (on ?person1 ?side1)
       (on ?person2 ?side1)
       (on lite ?side1)
       (bind (walk-time ?person1 $walk-time1))
       (bind (walk-time ?person2 $walk-time2))
       (setq $walk-time (max $walk-time1 $walk-time2))
       (bind (current-time $current-time))
       (<= (+ $current-time $walk-time) 17))
  (?person1 ?person2 ?side1 ?side2 $walk-time $current-time)
  (assert (not (on ?person1 ?side1))
          (on ?person1 ?side2)
          (not (on ?person2 ?side1))
          (on ?person2 ?side2)
          (not (on lite ?side1))
          (on lite ?side2)
          (current-time (+ $current-time $walk-time))))


(define-init
  ;dynamic
  (current-time 0)
  (on lite side1)
  (on bono side1)
  (on edge side1)
  (on adam side1)
  (on larry side1)
  ;static
  (walk-time bono 1)
  (walk-time edge 2)
  (walk-time adam 5)
  (walk-time larry 10)
)


(define-goal
  (and (on bono side2)
       (on edge side2)
       (on adam side2)
       (on larry side2)))

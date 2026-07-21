;;;; Filename: problem-hanoi.lisp

;;; Problem specification for the tower of hanoi.


(in-package :ww)  ;required

(ww-set *problem-name* hanoi)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)


(define-types
  base    (base1 base2 base3)  ;the 3 peg bases on which disks will be placed
  disk    (disk1 disk2 disk3)
  support (either base disk))  ;but disk1 (smallest) is not really a support (could itemize list)


(define-dynamic-relations
  (on disk $support))


(define-static-relations
  (size support $fixnum))


(define-query cleartop? (?support)
  (not (exists (?d disk)
         (on ?d ?support))))


(define-action move
    1
  (?disk disk (?support1 ?support2) support)
  (and (cleartop? ?disk)
       (bind (size ?disk $disk-size))
       (on ?disk ?support1)
       (cleartop? ?support2)
       (bind (size ?support2 $support-size))
       (< $disk-size $support-size))
  (?disk ?support1 ?support2)
  (assert (on ?disk ?support2)))


(define-init
    ;dynamic
  (on disk3 base1)
  (on disk2 disk3)
  (on disk1 disk2)    
  ;static
  (size disk1 1)
  (size disk2 2)
  (size disk3 3)
  (size base1 10)
  (size base2 10)
  (size base3 10)
)


(define-goal
  (and (on disk3 base3)
       (on disk2 disk3)
       (on disk1 disk2)))

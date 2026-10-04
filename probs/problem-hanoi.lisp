;;; Filename: problem-hanoi.lisp

;;; Problem specification for the tower of hanoi.
;;; Each disk records the peg it is on; the order of disks on a peg follows from size.


(in-package :ww)  ;required

(ww-set *problem-name* hanoi)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *depth-cutoff* 7)  ;the known optimum, 2^n - 1 moves for n disks on 3 pegs


(define-types
  peg   (peg1 peg2 peg3)
  disk  (disk1 disk2 disk3))  ;disk1 is the smallest


(define-dynamic-relations
  (loc disk $peg))


(define-static-relations
  (smaller> disk disk))  ;(smaller> ?d1 ?d2): ?d1 is smaller than ?d2


(define-action move
    1
  (?disk disk ?peg2 peg)
  (and (bind (loc ?disk $peg1))
       (not (eql $peg1 ?peg2))
       (not (exists (?d disk)
              (and (smaller> ?d ?disk)
                   (or (loc ?d $peg1)
                       (loc ?d ?peg2))))))
  (?disk $peg1 ?peg2)
  (assert (loc ?disk ?peg2)))


(define-init
  ;dynamic
  (loc disk3 peg1)
  (loc disk2 peg1)
  (loc disk1 peg1)
  ;static
  (smaller> disk1 disk2)
  (smaller> disk1 disk3)
  (smaller> disk2 disk3))


(define-goal
  (and (loc disk3 peg3)
       (loc disk2 peg3)
       (loc disk1 peg3)))

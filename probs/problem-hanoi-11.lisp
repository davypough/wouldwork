;;; Filename: problem-hanoi-11.lisp

;;; Problem specification for the tower of hanoi, 11 disks.
;;; Each disk records the peg it is on; the order of disks on a peg follows from size.


(in-package :ww)  ;required

(ww-set *problem-name* hanoi-11)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *depth-cutoff* 2047)  ;the known optimum, 2^n - 1 moves for n disks on 3 pegs


(define-types
  peg   (peg1 peg2 peg3)
  disk  (disk1 disk2 disk3 disk4 disk5 disk6 disk7 disk8 disk9 disk10 disk11))  ;disk1 is the smallest


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
  (loc disk11 peg1)
  (loc disk10 peg1)
  (loc disk9 peg1)
  (loc disk8 peg1)
  (loc disk7 peg1)
  (loc disk6 peg1)
  (loc disk5 peg1)
  (loc disk4 peg1)
  (loc disk3 peg1)
  (loc disk2 peg1)
  (loc disk1 peg1)
  ;static
  (smaller> disk1 disk2)
  (smaller> disk1 disk3)
  (smaller> disk1 disk4)
  (smaller> disk1 disk5)
  (smaller> disk1 disk6)
  (smaller> disk1 disk7)
  (smaller> disk1 disk8)
  (smaller> disk1 disk9)
  (smaller> disk1 disk10)
  (smaller> disk1 disk11)
  (smaller> disk2 disk3)
  (smaller> disk2 disk4)
  (smaller> disk2 disk5)
  (smaller> disk2 disk6)
  (smaller> disk2 disk7)
  (smaller> disk2 disk8)
  (smaller> disk2 disk9)
  (smaller> disk2 disk10)
  (smaller> disk2 disk11)
  (smaller> disk3 disk4)
  (smaller> disk3 disk5)
  (smaller> disk3 disk6)
  (smaller> disk3 disk7)
  (smaller> disk3 disk8)
  (smaller> disk3 disk9)
  (smaller> disk3 disk10)
  (smaller> disk3 disk11)
  (smaller> disk4 disk5)
  (smaller> disk4 disk6)
  (smaller> disk4 disk7)
  (smaller> disk4 disk8)
  (smaller> disk4 disk9)
  (smaller> disk4 disk10)
  (smaller> disk4 disk11)
  (smaller> disk5 disk6)
  (smaller> disk5 disk7)
  (smaller> disk5 disk8)
  (smaller> disk5 disk9)
  (smaller> disk5 disk10)
  (smaller> disk5 disk11)
  (smaller> disk6 disk7)
  (smaller> disk6 disk8)
  (smaller> disk6 disk9)
  (smaller> disk6 disk10)
  (smaller> disk6 disk11)
  (smaller> disk7 disk8)
  (smaller> disk7 disk9)
  (smaller> disk7 disk10)
  (smaller> disk7 disk11)
  (smaller> disk8 disk9)
  (smaller> disk8 disk10)
  (smaller> disk8 disk11)
  (smaller> disk9 disk10)
  (smaller> disk9 disk11)
  (smaller> disk10 disk11))


(define-goal
  (and (loc disk11 peg3)
       (loc disk10 peg3)
       (loc disk9 peg3)
       (loc disk8 peg3)
       (loc disk7 peg3)
       (loc disk6 peg3)
       (loc disk5 peg3)
       (loc disk4 peg3)
       (loc disk3 peg3)
       (loc disk2 peg3)
       (loc disk1 peg3)))

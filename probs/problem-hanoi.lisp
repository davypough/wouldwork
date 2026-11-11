;;; Filename: problem-hanoi.lisp

;;; Problem specification for the tower of hanoi.
;;; Each disk records the peg it is on; the order of disks on a peg follows from size.
;;; The number of disks is a setting: change *disks*, set *depth-cutoff* to match,
;;; and restage.


(in-package :ww)  ;required

(ww-set *problem-name* hanoi)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *depth-cutoff* 7)  ;the known optimum, 2^n - 1 moves for n disks; change with *disks*


(defparameter *disks* 3)  ;number of disks


(defparameter *disk-names* (loop for size from 1 to *disks*
                                 collect (intern (format nil "DISK~D" size)))
  "The disks from smallest to largest.")


(define-types
  peg   (peg1 peg2 peg3)
  disk  (compute *disk-names*))  ;disk1 is the smallest


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


;;; Every disk starts on peg1.
(define-init-action initialize-disks 0
  (?disk disk)
  (always-true)
  ()
  (assert (loc ?disk peg1)))


(define-init-action initialize-sizes 0
  (product ?d1 disk ?d2 disk)
  (< (position ?d1 *disk-names*) (position ?d2 *disk-names*))
  ()
  (assert (smaller> ?d1 ?d2)))


(define-goal
  (forall (?disk disk)
    (loc ?disk peg3)))

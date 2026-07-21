;;;; Filename: problem-queens4.lisp


;;; Problem specification for 4-queens.


(in-package :ww)  ;required

(ww-set *problem-name* queens4)

(ww-set *problem-type* planning)

(ww-set *solution-type* every)

(ww-set *tree-or-graph* tree)


(define-types
  queen   (queen1 queen2 queen3 queen4)
  column  (1 2 3 4))


(define-dynamic-relations
  (loc queen $fixnum $column)   ;row column of a queen
  (placed queen)
  (next-row $fixnum))


(define-action put
    1
  (?queen queen ?column column)
  (and (not (placed ?queen))
       (bind (next-row $row))
       (not (exists (?q queen)
              (and (placed ?q)
                   (bind (loc ?q $r $c))
                   (or ;(= $r $row)  superfluous, always considering next row
                       (= $c ?column)
                       (= (- $r $row) (- $c ?column))
                       (= (- $r $row) (- ?column $c)))))))
  (?queen $row ?column) 
  (assert (loc ?queen $row ?column)
          (placed ?queen)
          (next-row (1+ $row))))
   

(define-init
    (next-row 1))


(define-goal
  (next-row 5))

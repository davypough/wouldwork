;;; Filename: problem-array-path.lisp


;;; Basic problem specification for finding a path thru an array of allowed & disallowed points,
;;; not visiting any point more than once.


(in-package :ww)  ;required

(ww-set *problem-name* array-path)

(ww-set *problem-type* planning)

(ww-set *tree-or-graph* tree)

(ww-set *solution-type* min-length)


#.(defparameter *grid* (make-array '(5 5)
                      :initial-contents '((0 1 1 1 0)
                                          (1 1 1 1 1)
                                          (1 1 1 1 1)
                                          (1 1 1 1 1)
                                          (1 1 1 1 0))))

#.(defparameter *start* '(0 1))


(define-types
  row  (compute (alexandria:iota (array-dimension *grid* 0)))
  col  (compute (alexandria:iota (array-dimension *grid* 1))))


(define-dynamic-relations
  (visited row col)   ;current visitation status
  (num-visits $fixnum)     ;# of points visited so far
  (loc $fixnum $fixnum))     ;current location in the grid


(define-static-relations
  (total $fixnum))     ;total number of 1s in *grid*


(define-action move-right
  1
  ()
  (and (bind (loc $row $col))
       (setf $new-col (1+ $col))
       (< $new-col (array-dimension *grid* 1))  ;max col
       (= 1 (aref *grid* $row $new-col))
       (not (visited $row $new-col))
       (bind (num-visits $num-visits)))
  ()
  (assert (loc $row $new-col)
          (num-visits (1+ $num-visits))
          (visited $row $new-col)))


(define-action move-left
  1
  ()
  (and (bind (loc $row $col))
       (setf $new-col (1- $col))
       (>= $new-col 0)
       (= 1 (aref *grid* $row $new-col))
       (not (visited $row $new-col))
       (bind (num-visits $num-visits)))
  ()
  (assert (loc $row $new-col)
          (num-visits (1+ $num-visits))
          (visited $row $new-col)))


(define-action move-down
  1
  ()
  (and (bind (loc $row $col))
       (setf $new-row (1+ $row))
       (< $new-row (array-dimension *grid* 0))  ;max row
       (= 1 (aref *grid* $new-row $col))
       (not (visited $new-row $col))
       (bind (num-visits $num-visits)))
  ()
  (assert (loc $new-row $col)
          (num-visits (1+ $num-visits))
          (visited $new-row $col)))


(define-action move-up
  1
  ()
  (and (bind (loc $row $col))
       (setf $new-row (1- $row))
       (>= $new-row 0)
       (= 1 (aref *grid* $new-row $col))
       (not (visited $new-row $col))
       (bind (num-visits $num-visits)))
  ()
  (assert (loc $new-row $col)
          (num-visits (1+ $num-visits))
          (visited $new-row $col)))


(define-init
  (num-visits 1)
  (loc #.(first *start*) #.(second *start*))
  (visited #.(first *start*) #.(second *start*))
  (total #.(loop for row from 0 below (array-dimension *grid* 0)
                 sum (loop for col from 0 below (array-dimension *grid* 1)
                           count (= 1 (aref *grid* row col))))))


(define-goal  ;all 1s in *grid* visited
  (and (bind (total $total))
       (bind (num-visits $num-visits))
       (= $total $num-visits)))

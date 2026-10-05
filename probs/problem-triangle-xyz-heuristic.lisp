;;; Filename: problem-triangle-xyz-heuristic.lisp

;;; problem-triangle-xyz with a heuristic? search ordering, at N = 6.
;;; The heuristic compares the pegs with the holes inside the smallest
;;; x, y, z ranges that enclose all the pegs: a board whose pegs are
;;; packed together, with few holes among them, is explored first.
;;; Each position's coordinates are kept as a static fact for it.
;;; Lesson (measured 2026-10-04, first solution, program cycles with/without it):
;;; N=6 holes 11 12 13 22: 306/350 292/98 284/358 32/119;  N=7 holes 12 13 23:
;;; 3024/1117 2766/4107 16124/8569.  A heuristic only reorders the moves, and a
;;; plausible one can help on some boards and hurt on others.  Time any heuristic
;;; against none, on several starting boards, before relying on it.

;;; Positions have coordinates (x,y,z) measured from the triangle's
;;; right diagonal (/), left diagonal (\) and bottom (__), with x+y+z = N+2.
;;;           11
;;;         12  21
;;;       13  22  31
;;;     14  23  32  41
;;;   15  24  33  42  51
;;; 16  25  34  43  52  61


(in-package :ww)  ;required

(ww-set *problem-name* triangle-xyz-heuristic)

(ww-set *problem-type* planning)

(ww-set *solution-type* first)


(defparameter *N* 6)  ;the number of pegs on a side

(defparameter *size* (/ (* *N* (1+ *N*)) 2))  ;total number of positions

(defparameter *init-holes* `((1 1 ,*N*)))  ;coordinates of the initial holes

(defparameter *final-peg-count* 1)  ;number of pegs to be left at the end

(defparameter *directions*  ;direction name and (dx dy dz) step
  '((ld 0 1 -1) (ru 0 -1 1) (rd 1 0 -1) (lu -1 0 1) (rh 1 -1 0) (lh -1 1 0)))


(define-types
  position (compute (loop for x from 1 to *N*  ;p11, p12, ... named as in the diagram
                          append (loop for y from 1 to (- (1+ *N*) x)
                                       collect (intern (format nil "P~D~D" x y)))))
  direction (compute (mapcar #'first *directions*)))


(define-dynamic-relations
    (occupied position)        ;a peg is at the position
    (peg-count $integer))      ;pegs remaining on the board


(define-static-relations
    (jump-line> position direction $position $position)  ;over and to positions
    (coords> position $fixnum $fixnum $fixnum))          ;x, y, z of a position


(define-query heuristic? ()
  ;Lower is explored first: |pegs - enclosed holes - 1|
  (do (bind (peg-count $peg-count))
      (assign $x-min 100) (assign $y-min 100) (assign $z-min 100)
      (assign $x-max 0) (assign $y-max 0) (assign $z-max 0)
      (doall (?pos position)
        (if (occupied ?pos)
          (do (bind (coords> ?pos $x $y $z))
              (assign $x-min (min $x-min $x))
              (assign $y-min (min $y-min $y))
              (assign $z-min (min $z-min $z))
              (assign $x-max (max $x-max $x))
              (assign $y-max (max $y-max $y))
              (assign $z-max (max $z-max $z)))))
      (assign $enclosed-hole-count 0)
      (doall (?pos position)
        (if (not (occupied ?pos))
          (do (bind (coords> ?pos $x $y $z))
              (if (and (<= $x-min $x $x-max)
                       (<= $y-min $y $y-max)
                       (<= $z-min $z $z-max))
                (assign $enclosed-hole-count (1+ $enclosed-hole-count))))))
      (abs (- $peg-count $enclosed-hole-count 1))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-action jump
  1
  (?from position ?dir direction)
  (and (occupied ?from)
       (bind (jump-line> ?from ?dir $over $to))
       (occupied $over)
       (not (occupied $to))
       (bind (peg-count $peg-count)))
  (?from $to)
  (assert (not (occupied ?from))
          (not (occupied $over))
          (occupied $to)
          (peg-count (1- $peg-count))))


(progn (format t "~&Initializing database...~%")
  (let ((coords->pos (make-hash-table :test #'equal)))
    (loop for x from 1 to *N*
          do (loop for y from 1 to (- (1+ *N*) x)
                   for z = (- (1+ *N*) x) then (1- z)
                   for pos = (intern (format nil "P~D~D" x y))
                   do (setf (gethash (list x y z) coords->pos) pos)
                      (update *static-db* `(coords> ,pos ,x ,y ,z))
                      (unless (member (list x y z) *init-holes* :test #'equal)
                        (update *db* `(occupied ,pos)))))
    (loop for from-coords being the hash-keys of coords->pos using (hash-value from)
          do (loop for (dir dx dy dz) in *directions*
                   for over = (gethash (mapcar #'+ from-coords (list dx dy dz)) coords->pos)
                   for to = (gethash (mapcar #'+ from-coords (list (* 2 dx) (* 2 dy) (* 2 dz)))
                                     coords->pos)
                   when to
                     do (update *static-db* `(jump-line> ,from ,dir ,over ,to))))
    (update *db* `(peg-count ,(- *size* (length *init-holes*))))))


(define-goal  ;only one peg left
  `(peg-count ,*final-peg-count*))

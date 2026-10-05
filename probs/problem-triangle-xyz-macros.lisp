;;; Filename: problem-triangle-xyz-macros.lisp

;;; problem-triangle-xyz with a macro action added before the single jump.
;;; A double jump is two jumps along one line:  A B _ C  ->  _ C _ _
;;; A jumps over B into the gap, then C jumps back over A into B's old spot.
;;; So the start and far end are emptied, and the over position stays filled.
;;; Its (from, direction) -> (over, gap, far) lines are computed once at
;;; initialization, like the single jump lines.
;;; Lesson (measured 2026-10-04, first solution, program cycles / steps, macros vs
;;; plain triangle-xyz): N=5 11/10 vs 108/13;  N=6 1138/15 vs 350/19.  Macros shortened
;;; the plan both times, but they add moves to try at every state, and the N=5 saving
;;; reversed at N=6.  Time them against the base actions at the size you need.

;;; Positions have coordinates (x,y,z) measured from the triangle's
;;; right diagonal (/), left diagonal (\) and bottom (__), with x+y+z = N+2.
;;;         11
;;;       12  21
;;;     13  22  31
;;;   14  23  32  41
;;; 15  24  33  42  51


(in-package :ww)  ;required

(ww-set *problem-name* triangle-xyz-macros)

(ww-set *problem-type* planning)

(ww-set *solution-type* first)


(defparameter *N* 5)  ;the number of pegs on a side

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
    (jump-line> position direction $position $position)               ;over and to positions
    (double-jump-line> position direction $position $position $position))  ;over, gap, far


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-action double-jump
  1
  (?from position ?dir direction)
  (and (occupied ?from)
       (bind (double-jump-line> ?from ?dir $over $gap $far))
       (occupied $over)
       (not (occupied $gap))
       (occupied $far)
       (bind (peg-count $peg-count)))
  (?from $far)
  (assert (not (occupied ?from))
          (not (occupied $far))
          (peg-count (- $peg-count 2))))


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
                      (unless (member (list x y z) *init-holes* :test #'equal)
                        (update *db* `(occupied ,pos)))))
    (loop for from-coords being the hash-keys of coords->pos using (hash-value from)
          do (loop for (dir dx dy dz) in *directions*
                   for line = (loop for k from 1 to 3
                                    collect (gethash (mapcar #'+ from-coords
                                                             (list (* k dx) (* k dy) (* k dz)))
                                                     coords->pos))
                   when (second line)
                     do (update *static-db* `(jump-line> ,from ,dir ,(first line) ,(second line)))
                   when (third line)
                     do (update *static-db* `(double-jump-line> ,from ,dir ,@line))))
    (update *db* `(peg-count ,(- *size* (length *init-holes*))))))


(define-goal  ;only one peg left
  `(peg-count ,*final-peg-count*))

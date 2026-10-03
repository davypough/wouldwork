;;; Filename: problem-triangle-xyz-1.lisp

;;; Occupancy version of problem-triangle-xyz.
;;; Pegs are indistinguishable, so the state records only which positions
;;; are occupied.  Every (from, direction) -> (over, to) jump line is
;;; computed once at initialization as a static fact, so a single jump
;;; action replaces the six directional actions and their arithmetic.

;;; Positions have coordinates (x,y,z) measured from the triangle's
;;; right diagonal (/), left diagonal (\) and bottom (__), with x+y+z = N+2.
;;;         11
;;;       12  21
;;;     13  22  31
;;;   14  23  32  41
;;; 15  24  33  42  51


(in-package :ww)  ;required

(ww-set *problem-name* triangle-xyz-1)

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
    (jump-line> position direction $position $position))  ;over and to positions


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

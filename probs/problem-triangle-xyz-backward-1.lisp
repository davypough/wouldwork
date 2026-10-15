;;; Filename: problem-triangle-xyz-backward-1.lisp

;;; Backward search alone on the N=6 triangle, using the occupancy model of
;;; problem-triangle-xyz.lisp.  Variant of problem-triangle-xyz-backward.lisp:
;;; instead of stopping at the meeting boards, it searches back from the single
;;; final peg all the way to the starting board (one hole at 11).

;;; Design notes: undoing a jump on a board is the same as making a forward jump
;;; on the board with pegs and holes swapped.  So a backward search from a final
;;; peg at some position is just a forward search from a single hole at that
;;; position, and backward search alone never changes the kind of search for
;;; this puzzle; only the end position matters.  This spec exercises the
;;; procedure for backward search alone: solve, reverse the plan, and replay it
;;; on the forward spec.

;;; Procedure at the REPL:
;;;   (stage triangle-xyz-backward-1)
;;;   (solve)
;;;   (defparameter *backward-plan* (reverse (solution.path (first *solution-paths*))))
;;;   (stage triangle-xyz-forward)
;;;   (%validate-solution *backward-plan* nil)
;;; The replay ends with one peg at 33; the forward spec's own goal (a meeting
;;; board) is not expected to hold there.

;;; The reverse action is named jump and records (?from $to), like the forward
;;; jump, so the reversed plan reads as forward jumps.

;;; Positions have coordinates (x,y,z) measured from the triangle's
;;; right diagonal (/), left diagonal (\) and bottom (__), with x+y+z = N+2.
;;;           11
;;;         12  21
;;;       13  22  31
;;;     14  23  32  41
;;;   15  24  33  42  51
;;; 16  25  34  43  52  61


(in-package :ww)  ;required

(ww-set *problem-name* triangle-xyz-backward-1)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *depth-cutoff* 19)  ;every plan has exactly 19 jumps


(defparameter *N* 6)  ;the number of pegs on a side

(defparameter *size* (/ (* *N* (1+ *N*)) 2))  ;total number of positions

(defparameter *final-coords* '(3 3 2))  ;coordinates of the forward search's last peg

(defparameter *start-hole* 'p11)  ;the starting board's only hole, coordinates (1 1 6)

(defparameter *directions*  ;direction name and (dx dy dz) step
  '((ld 0 1 -1) (ru 0 -1 1) (rd 1 0 -1) (lu -1 0 1) (rh 1 -1 0) (lh -1 1 0)))


(define-types
  position (compute (loop for x from 1 to *N*  ;p11, p12, ... named as in the diagram
                          append (loop for y from 1 to (- (1+ *N*) x)
                                       collect (intern (format nil "P~D~D" x y)))))
  direction (compute (mapcar #'first *directions*)))


(define-dynamic-relations
    (occupied position)        ;a peg is at the position
    (peg-count $integer))      ;pegs on the board


(define-static-relations
    (jump-line> position direction $position $position))  ;over and to positions


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-action jump  ;reverse jump: the peg at $to returns to ?from, and $over is refilled
  1
  (?from position ?dir direction)
  (and (not (occupied ?from))
       (bind (jump-line> ?from ?dir $over $to))
       (not (occupied $over))
       (occupied $to)
       (bind (peg-count $peg-count)))
  (?from $to)
  (assert (occupied ?from)
          (occupied $over)
          (not (occupied $to))
          (peg-count (1+ $peg-count))))


(progn (format t "~&Initializing database...~%")
  (let ((coords->pos (make-hash-table :test #'equal)))
    (loop for x from 1 to *N*
          do (loop for y from 1 to (- (1+ *N*) x)
                   for z = (- (1+ *N*) x) then (1- z)
                   do (setf (gethash (list x y z) coords->pos)
                            (intern (format nil "P~D~D" x y)))))
    (update *db* `(occupied ,(gethash *final-coords* coords->pos)))
    (loop for from-coords being the hash-keys of coords->pos using (hash-value from)
          do (loop for (dir dx dy dz) in *directions*
                   for over = (gethash (mapcar #'+ from-coords (list dx dy dz)) coords->pos)
                   for to = (gethash (mapcar #'+ from-coords (list (* 2 dx) (* 2 dy) (* 2 dz)))
                                     coords->pos)
                   when to
                     do (update *static-db* `(jump-line> ,from ,dir ,over ,to))))
    (update *db* `(peg-count 1))))


(define-goal  ;the starting board: every position filled except the starting hole
  `(and (peg-count ,(1- *size*))
        (not (occupied ,*start-hole*))))

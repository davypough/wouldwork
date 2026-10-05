;;; Filename: problem-triangle-xyz-backward.lisp

;;; Backward half of a bidirectional search on the N=6 triangle, using the
;;; occupancy model of problem-triangle-xyz.lisp.  Partner spec:
;;; problem-triangle-xyz-forward.lisp.
;;; Starting from the single final peg, each reverse jump puts back the jumped
;;; peg and moves the jumper back to where it came from.  Every distinct board
;;; with *meet-peg-count* pegs is collected (one path per board).

;;; Procedure at the REPL:
;;;   (stage triangle-xyz-backward)
;;;   (solve)
;;;   (get-state-codes)              ;encode the collected boards into *state-codes*
;;;   (stage triangle-xyz-forward)   ;*state-codes* survives staging
;;;   (solve)                        ;forward path + reversed backward path

;;; The reverse action is named jump and records (?from $to), like the forward
;;; jump, so the reversed backward path reads as forward jumps and the combined
;;; plan replays on the forward spec.

;;; Lesson (measured 2026-10-04, N=6, 16 threads): the earlier named-peg backward
;;; spec (tree search) was dropped from the tests as too slow.  With occupancy and
;;; graph search this half collects all 16,253 distinct 8-peg boards in 8,865
;;; program cycles (0.05 sec), so tabulating the meeting boards is cheap.

;;; Positions have coordinates (x,y,z) measured from the triangle's
;;; right diagonal (/), left diagonal (\) and bottom (__), with x+y+z = N+2.
;;;           11
;;;         12  21
;;;       13  22  31
;;;     14  23  32  41
;;;   15  24  33  42  51
;;; 16  25  34  43  52  61


(in-package :ww)  ;required

(ww-set *problem-name* triangle-xyz-backward)

(ww-set *problem-type* planning)

(ww-set *solution-type* every)

(ww-set *depth-cutoff* 7)  ;backward depth; forward depth is 19 - 7 = 12


(defparameter *N* 6)  ;the number of pegs on a side

(defparameter *final-coords* '(3 3 2))  ;coordinates of the forward search's last peg

(defparameter *meet-peg-count* 8)  ;pegs on the boards where the two searches meet

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
  (clrhash *state-codes*)  ;discard codes from any earlier backward run
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


(define-goal  ;boards where the forward search can meet this one
  `(peg-count ,*meet-peg-count*))


;;;;;;;;;;;;;;;;;;;; Encoding Backward Search Boards ;;;;;;;;;;;;;;;;;


(defun encode-state (propositions)
  "Encodes a board as an integer with one bit per occupied position.
   Identical in problem-triangle-xyz-forward.lisp, so both searches agree."
  (let ((positions (gethash 'position *types*))
        (int 0))
    (loop for prop in propositions
          when (eql (first prop) 'occupied)
            do (setf int (dpb 1 (byte 1 (position (second prop) positions)) int)))
    int))

;;;; Filename: problem-triangle-backward.lisp

;;; Problem specification for triangle peg problem solving backwards
;;; to middle depth. Use with spec for solving forward to middle depth match,
;;; with problem-triangle-xyz.lisp

;;; Needs more than default --dynamic-space-size of 1024  (eg, 4096)

;;; After collecting states from backward search to some depth,
;;; run (get-state-codes) to create hash table of those coded states. Then
;;; run forward search to those states to find combined solution.

;;; The peg board positions have coordinates measured 
;;; from the triangle's right diagonal (/), left diagonal (\) and
;;; bottom (__)
;;; side lengths from 1 to *N* with pegs in all positions except 11.
;;; Jumps are by / up or down, \ up or down, - (horiz) right or left.
;;;         11
;;;       12  21
;;;     13  22  31
;;;   14  23  32  41
;;; 15  24  33  42  51


(in-package :ww)  ;required

(ww-set *problem-name* triangle-backward)

(ww-set *problem-type* planning)

(ww-set *solution-type* every)

(ww-set *tree-or-graph* tree)


(defparameter *N* 6)  ;the number of pegs on a side

(defparameter *size* (/ (* *N* (1+ *N*)) 2))  ;total number of positions

(defparameter *final-peg-count* 8)  ;depth-cutoff + 1
;(defparameter *final-peg-count* (1- *size*))  ;use if full search backward to start state

(defparameter *reversed-pegs* nil)  ;list of pegs in reverse order

(setq *depth-cutoff* 7)  ;add when searching bidirectional to partial depth
                          ;DepthBackward + DepthForward = DepthTotal
                          ;remove if full search backward to start state


(define-types
  position (compute (iter (for i from 1 to *size*)  ;pos1, pos2, ...
                          (collect (intern (format nil "POS~D" i)))))
  peg (compute (iter (for i from 1 to (- *size* 1))  ;peg1, peg2, ...
                     (collect (intern (format nil "PEG~D" i)))))
  index (compute (iter (for i from 1 to *N*)
                       (collect i))))


(define-dynamic-relations
    (loc peg $index $index $index)      ;location of a peg, x, y, z
    (contents> index index index $peg)  ;peg contents at a location
    (board-pegs $list)       ;list of pegs currently on the board
    (peg-count $integer))    ;pegs on the board


(define-static-relations
    (pos position $index $index $index))      ;location of a position


(define-query get-remaining-pegs? ()
  (do (bind (board-pegs $board-pegs))
      $board-pegs))
      

(define-update put! (?peg ?x ?y ?z)
  (do (loc ?peg ?x ?y ?z)
      (contents> ?x ?y ?z ?peg)))


#|
(define-action add-first-peg  ;place first peg on board at any position
  1                           ;ignores symmetry, significantly enlarges search space
  (?pos position)
  (and (bind (peg-count $peg-count))
       (= $peg-count 0)
       (bind (pos ?pos $x $y $z))
       (setq $next-peg (nth $peg-count *reversed-pegs*)))  ;(first $remaining-pegs)))
  (($x $y) fluent)
  (assert (loc $next-peg $x $y $z)
          (contents> $x $y $z $next-peg)
          (peg-count 1)
          (board-pegs (list $next-peg))))
          
          
(define-action delete-add-first-peg  ;delete both rules after running once
  0
  ()
  (always-true)
  ()
  (delete-actions 'add-first-peg 'delete-add-first-peg))
|#

(define-action add-peg
  1
  (?peg (get-remaining-pegs?))
  (and ;(>= $peg-count 1)  ;only needed if first two rules (above) are in play
       (bind (loc ?peg $x $y $z))
       (setq $x-2 (- $x 2))
       (setq $x-1 (- $x 1))
       (setq $x+1 (+ $x 1))
       (setq $x+2 (+ $x 2))
       (setq $y-2 (- $y 2))
       (setq $y-1 (- $y 1))
       (setq $y+1 (1+ $y))
       (setq $y+2 (+ $y 2))
       (setq $z-2 (- $z 2))
       (setq $z-1 (1- $z))
       (setq $z+1 (1+ $z))
       (setq $z+2 (+ $z 2))
       (bind (peg-count $peg-count))
       (bind (board-pegs $board-pegs))
       (setq $next-peg (nth $peg-count *reversed-pegs*)))
  ($x $y $a $b)
  (do (assert (if (and (<= $y (- *N* 2))
                       (>= $z 3)
                       (not (bind (contents> $x $y+1 $z-1 $any-peg)))
                       (not (bind (contents> $x $y+2 $z-2 $any-peg)))
                       (setq $a $x)
                       (setq $b $y+2))
                (do (not (contents> $x $y $z ?peg))
                    (put! $next-peg $x $y+1 $z-1)
                    (put! ?peg $x $y+2 $z-2)
                    (peg-count (1+ $peg-count))
                    (board-pegs (cons $next-peg $board-pegs)))))
      (assert (if (and (<= $z (- *N* 2))
                       (>= $y 3)
                       (not (bind (contents> $x $y-1 $z+1 $any-peg)))
                       (not (bind (contents> $x $y-2 $z+2 $any-peg)))
                       (setq $a $x)
                       (setq $b $y-2))
                (do (not (contents> $x $y $z ?peg))
                    (put! $next-peg $x $y-1 $z+1)
                    (put! ?peg $x $y-2 $z+2)
                    (peg-count (1+ $peg-count))
                    (board-pegs (cons $next-peg $board-pegs)))))
      (assert (if (and (<= $x (- *N* 2))
                       (>= $z 3)
                       (not (bind (contents> $x+1 $y $z-1 $any-peg)))
                       (not (bind (contents> $x+2 $y $z-2 $any-peg)))
                       (setq $a $x+2)
                       (setq $b $y))
                (do (not (contents> $x $y $z ?peg))
                    (put! $next-peg $x+1 $y $z-1)
                    (put! ?peg $x+2 $y $z-2)
                    (peg-count (1+ $peg-count))
                    (board-pegs (cons $next-peg $board-pegs)))))
      (assert (if (and (<= $z (- *N* 2))
                       (>= $x 3)
                       (not (bind (contents> $x-1 $y $z+1 $any-peg)))
                       (not (bind (contents> $x-2 $y $z+2 $any-peg)))
                       (setq $a $x-2)
                       (setq $b $y))
                (do (not (contents> $x $y $z ?peg))
                    (put! $next-peg $x-1 $y $z+1)
                    (put! ?peg $x-2 $y $z+2)
                    (peg-count (1+ $peg-count))
                    (board-pegs (cons $next-peg $board-pegs)))))
      (assert (if (and (<= $x (- *N* 2))
                       (>= $y 3)
                       (not (bind (contents> $x+1 $y-1 $z $any-peg)))
                       (not (bind (contents> $x+2 $y-2 $z $any-peg)))
                       (setq $a $x+2)
                       (setq $b $y-2))
                (do (not (contents> $x $y $z ?peg))
                    (put! $next-peg $x+1 $y-1 $z)
                    (put! ?peg $x+2 $y-2 $z)
                    (peg-count (1+ $peg-count))
                    (board-pegs (cons $next-peg $board-pegs)))))
      (assert (if (and (<= $y (- *N* 2))
                       (>= $x 3)
                       (not (bind (contents> $x-1 $y+1 $z $any-peg)))
                       (not (bind (contents> $x-2 $y+2 $z $any-peg)))
                       (setq $a $x-2)
                       (setq $b $y+2))
                (do (not (contents> $x $y $z ?peg))
                    (put! $next-peg $x-1 $y+1 $z)
                    (put! ?peg $x-2 $y+2 $z)
                    (peg-count (1+ $peg-count))
                    (board-pegs (cons $next-peg $board-pegs)))))))

          
(progn (format t "~&Initializing database...~%")
  (loop with pegs = (gethash 'peg *types*)
        with positions = (gethash 'position *types*)
    ;*db* is the name of the initial database
    ;update is the function that asserts a proposition
    ;into the database
    initially (setq *reversed-pegs* (reverse pegs))
              (update *db* `(peg-count 1))
              (update *db* `(loc ,(first *reversed-pegs*) 3 3 2))  ;set first peg
              (update *db* `(contents> 3 3 2 ,(first *reversed-pegs*)))
              (update *db* `(board-pegs ,(list (first *reversed-pegs*))))
    for x from 1 to *N*
      do (loop for y from 1 to (- (1+ *N*) x)
               for z = (- (1+ *N*) x) then (1- z)
               do (update *static-db* `(pos ,(pop positions) ,x ,y ,z)))))


(define-goal  ;pegs remaining
  `(peg-count ,*final-peg-count*))
  ;(not (contents> 1 1 6 $any-peg))))  ;use only if searching all the way to initial state
     
     
;;;;;;;;;;;;;;;;;;;; Encoding Backward Search Solutions ;;;;;;;;;;;;;;;;;


(defun encode-state (propositions)
  "Converts a state database of propositions to a unique positive integer."
  (let ((int 0))
    (iter (for prop in propositions)
          (when (eql (first prop) 'loc)  ;only locations of pegs are important
            (let ((row (1- (third prop)))
                  (col (1- (fourth prop))))
              (setq int (set-int-bit 1 int row col))))
          (finally (return int)))))
          
          
(defun set-int-bit (value int row col)
  "Returns the new bit-integer with bit-value replaced in int."
  (declare (type bit value) (type integer int) (type fixnum row col) (special *N*))
  (dpb value (byte 1 (+ (* (- *N* row 1) *N*) (- *N* col 1))) int))

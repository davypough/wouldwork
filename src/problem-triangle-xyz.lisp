;;; Filename: problem-triangle-xyz.lisp

;;; Three coordinates (x,y,z) allows easier computation of heuristic.

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

(ww-set *problem-name* triangle-xyz)

(ww-set *problem-type* planning)

(ww-set *solution-type* first)


(defparameter *N* 5)  ;the number of pegs on a side

(defparameter *size* (/ (* *N* (1+ *N*)) 2))  ;total number of positions

(defparameter *init-holes* `((1 1 ,*N*)))  ;coordinates of the initial holes

(defparameter *final-peg-count* 1)  ;number of pegs to be left at the end
;(defparameter *final-peg-count* 8)  ;number of pegs left at the end of backward search

;(setq *depth-cutoff* 12) ;add when searching bidirectional to partial depth
                           ;DepthBackward + DepthForward = DepthTotal


(define-types
  position (compute (loop for i from 1 to *size*  ;pos1, pos2, ...
                          collect (intern (format nil "POS~D" i))))
  peg (compute (loop for i from 1 to (- *size* (length *init-holes*))  ;peg1, peg2, ...
                     collect (intern (format nil "PEG~D" i))))
  index (compute (loop for i from 1 to *N*
                     collect i)))


(define-dynamic-relations
    (loc peg $index $index $index)      ;location of a peg
    (contents> index index index $peg)  ;peg contents at a location
    (board-pegs $list)   ;list of remaining pegs
    (peg-count $integer))    ;pegs remaining on the board


(define-static-relations
    (pos position $index $index $index))      ;location of a position


(define-query get-remaining-pegs? ()
  (do (bind (board-pegs $board-pegs))
      $board-pegs))


(define-update put! (?peg ?x ?y ?z)
  (do (loc ?peg ?x ?y ?z)
      (contents> ?x ?y ?z ?peg)))


(define-update discard! (?peg ?x ?y ?z)
  (do (not (loc ?peg ?x ?y ?z))
      (not (contents> ?x ?y ?z ?peg))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-action jump-LD
  1
  (?peg (get-remaining-pegs?))
  (and (bind (loc ?peg $x $y $z))
       (<= $y (- *N* 2))
       (>= $z 3)
       (setq $y+1 (1+ $y))
       (setq $z-1 (1- $z))
       (bind (contents> $x $y+1 $z-1 $adj-peg))
       (setq $y+2 (+ $y 2))
       (setq $z-2 (- $z 2))
       (not (bind (contents> $x $y+2 $z-2 $any-peg)))
       (bind (peg-count $peg-count))
       (bind (board-pegs $board-pegs)))
  ($x $y)
  (assert (not (contents> $x $y $z ?peg))
          (put! ?peg $x $y+2 $z-2)
          (discard! $adj-peg $x $y+1 $z-1)
          (peg-count (1- $peg-count))
          (board-pegs (remove $adj-peg $board-pegs))))


(define-action jump-RU
  1
  (?peg (get-remaining-pegs?))
  (and (bind (loc ?peg $x $y $z))
       (<= $z (- *N* 2))
       (>= $y 3)
       (setq $y-1 (1- $y))
       (setq $z+1 (1+ $z))
       (bind (contents> $x $y-1 $z+1 $adj-peg))
       (setq $y-2 (- $y 2))
       (setq $z+2 (+ $z 2))
       (not (bind (contents> $x $y-2 $z+2 $any-peg)))
       (bind (peg-count $peg-count))
       (bind (board-pegs $board-pegs)))
  ($x $y)
  (assert (not (contents> $x $y $z ?peg))
          (put! ?peg $x $y-2 $z+2)
          (discard! $adj-peg $x $y-1 $z+1)
          (peg-count (1- $peg-count))
          (board-pegs (remove $adj-peg $board-pegs))))


(define-action jump-RD
  1
  (?peg (get-remaining-pegs?))
  (and (bind (loc ?peg $x $y $z))
       (<= $x (- *N* 2))
       (>= $z 3)
       (setq $x+1 (+ $x 1))
       (setq $z-1 (1- $z))
       (bind (contents> $x+1 $y $z-1 $adj-peg))
       (setq $x+2 (+ $x 2))
       (setq $z-2 (- $z 2))
       (not (bind (contents> $x+2 $y $z-2 $any-peg)))
       (bind (peg-count $peg-count))
       (bind (board-pegs $board-pegs)))
  ($x $y)
  (assert (not (contents> $x $y $z ?peg))
          (put! ?peg $x+2 $y $z-2)
          (discard! $adj-peg $x+1 $y $z-1)
          (peg-count (1- $peg-count))
          (board-pegs (remove $adj-peg $board-pegs))))


(define-action jump-LU
  1
  (?peg (get-remaining-pegs?))
  (and (bind (loc ?peg $x $y $z))
       (<= $z (- *N* 2))
       (>= $x 3)
       (setq $x-1 (- $x 1))
       (setq $z+1 (1+ $z))
       (bind (contents> $x-1 $y $z+1 $adj-peg))
       (setq $x-2 (- $x 2))
       (setq $z+2 (+ $z 2))
       (not (bind (contents> $x-2 $y $z+2 $any-peg)))
       (bind (peg-count $peg-count))
       (bind (board-pegs $board-pegs)))
  ($x $y)
  (assert (not (contents> $x $y $z ?peg))
          (put! ?peg $x-2 $y $z+2)
          (discard! $adj-peg $x-1 $y $z+1)
          (peg-count (1- $peg-count))
          (board-pegs (remove $adj-peg $board-pegs))))


(define-action jump-RH
  1
  (?peg (get-remaining-pegs?))
  (and (bind (loc ?peg $x $y $z))
       (<= $x (- *N* 2))
       (>= $y 3)
       (setq $x+1 (+ $x 1))
       (setq $y-1 (1- $y))
       (bind (contents> $x+1 $y-1 $z $adj-peg))
       (setq $x+2 (+ $x 2))
       (setq $y-2 (- $y 2))
       (not (bind (contents> $x+2 $y-2 $z $any-peg)))
       (bind (peg-count $peg-count))
       (bind (board-pegs $board-pegs)))
  ($x $y)
  (assert (not (contents> $x $y $z ?peg))
          (put! ?peg $x+2 $y-2 $z)
          (discard! $adj-peg $x+1 $y-1 $z)
          (peg-count (1- $peg-count))
          (board-pegs (remove $adj-peg $board-pegs))))


(define-action jump-LH
  1
  (?peg (get-remaining-pegs?))
  (and (bind (loc ?peg $x $y $z))
       (<= $y (- *N* 2))
       (>= $x 3)
       (setq $x-1 (1- $x))
       (setq $y+1 (1+ $y))
       (bind (contents> $x-1 $y+1 $z $adj-peg))
       (setq $x-2 (- $x 2))
       (setq $y+2 (+ $y 2))
       (not (bind (contents> $x-2 $y+2 $z $any-peg)))
       (bind (peg-count $peg-count))
       (bind (board-pegs $board-pegs)))
  ($x $y)
  (assert (not (contents> $x $y $z ?peg))
          (put! ?peg $x-2 $y+2 $z)
          (discard! $adj-peg $x-1 $y+1 $z)
          (peg-count (1- $peg-count))
          (board-pegs (remove $adj-peg $board-pegs))))


(progn (format t "~&Initializing database...~%")
  (loop with pegs = (gethash 'peg *types*)
        with positions = (gethash 'position *types*)
    ;*db* is the name of the initial database
    ;update is the function that asserts a proposition
    ;into the database
    initially (update *db* `(peg-count ,(length pegs)))
              (update *db* `(board-pegs ,pegs))
    for x from 1 to *N*
      do (loop for y from 1 to (- (1+ *N*) x)
               for z = (- (1+ *N*) x) then (1- z)
               do (update *static-db* `(pos ,(pop positions) ,x ,y ,z))
               unless (member (list x y z) *init-holes* :test #'equal)
               do (let ((peg (pop pegs)))
                    (update *db* `(loc ,peg ,x ,y ,z))
                    (update *db* `(contents> ,x ,y ,z ,peg))))))


(define-goal  ;only one peg left
;  (and 
    `(peg-count ,*final-peg-count*))
;     (backward-path-exists state)))  ;only use if paired with backward search




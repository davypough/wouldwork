;;; Filename: problem-triangle-macros-one.lisp

;;; Adds two-jump (macro) moves to original one-jump problem-triangle-xyz.lisp

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

(ww-set *problem-name* triangle-macros-one)

(ww-set *problem-type* planning)

(ww-set *tree-or-graph* tree)

(ww-set *solution-type* first)


(defparameter *N* 5)  ;the number of pegs on a side

(defparameter *size* (/ (* *N* (1+ *N*)) 2))  ;total number of positions

(defparameter *init-holes* `((1 1 ,*N*)))  ;coordinates of the initial holes

(defparameter *final-peg-count* 1)  ;number of pegs to be left at the end


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


(define-update move! (?peg ?fm-x ?fm-y ?fm-z ?to-x ?to-y ?to-z)
  (do (not (contents> ?fm-x ?fm-y ?fm-z ?peg))
      (loc ?peg ?to-x ?to-y ?to-z)
      (contents> ?to-x ?to-y ?to-z ?peg)))


(define-update discard! (?peg ?x ?y ?z)
  (do (not (loc ?peg ?x ?y ?z))
      (not (contents> ?x ?y ?z ?peg))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-action double-jump  ; . . 0 .
  1
  (?peg (get-remaining-pegs?))
  (and (bind (loc ?peg $x $y $z))  
       (setq $x-3 (- $x 3))
       (setq $x-2 (- $x 2))
       (setq $x-1 (- $x 1))
       (setq $x+1 (+ $x 1))
       (setq $x+2 (+ $x 2))
       (setq $x+3 (+ $x 3))
       (setq $y-3 (- $y 3))
       (setq $y-2 (- $y 2))
       (setq $y-1 (- $y 1))
       (setq $y+1 (+ $y 1))
       (setq $y+2 (+ $y 2))
       (setq $y+3 (+ $y 3))
       (setq $z-3 (- $z 3))
       (setq $z-2 (- $z 2))
       (setq $z-1 (- $z 1))
       (setq $z+1 (+ $z 1))
       (setq $z+2 (+ $z 2))
       (setq $z+3 (+ $z 3))
       (bind (peg-count $peg-count))
       (bind (board-pegs $board-pegs)))
  ($x $y $dir)
  (do (if (and (<= $y (- *N* 3))
               (>= $z 4)
               (bind (contents> $x $y+1 $z-1 $adj-peg))
               (not (bind (contents> $x $y+2 $z-2 $any-peg)))
               (bind (contents> $x $y+3 $z-3 $2nd-peg))
               (setq $dir 'ld-ru))
        (assert (discard! ?peg $x $y $z)
                (discard! $adj-peg $x $y+1 $z-1)
                (move! $2nd-peg $x $y+3 $z-3 $x $y+1 $z-1)
                (peg-count (- $peg-count 2))
                (board-pegs (set-difference $board-pegs (list ?peg $adj-peg)))))

      (if (and (<= $z (- *N* 3))
               (>= $y 4)
               (bind (contents> $x $y-1 $z+1 $adj-peg))
               (not (bind (contents> $x $y-2 $z+2 $any-peg)))
               (bind (contents> $x $y-3 $z+3 $2nd-peg))
               (setq $dir 'ru-ld))
        (assert (discard! ?peg $x $y $z)
                (discard! $adj-peg $x $y-1 $z+1)
                (move! $2nd-peg $x $y-3 $z+3 $x $y-1 $z+1)
                (peg-count (- $peg-count 2))
                (board-pegs (set-difference $board-pegs (list ?peg $adj-peg)))))

      (if (and (<= $x (- *N* 3))
               (>= $z 4)
               (bind (contents> $x+1 $y $z-1 $adj-peg))
               (not (bind (contents> $x+2 $y $z-2 $any-peg)))
               (bind (contents> $x+3 $y $z-3 $2nd-peg))
               (setq $dir 'rd-lu))
        (assert (discard! ?peg $x $y $z)
                (discard! $adj-peg $x+1 $y $z-1)
                (move! $2nd-peg $x+3 $y $z-3 $x+1 $y $z-1)
                (peg-count (- $peg-count 2))
                (board-pegs (set-difference $board-pegs (list ?peg $adj-peg)))))

      (if (and (<= $z (- *N* 3))
               (>= $x 4)
               (bind (contents> $x-1 $y $z+1 $adj-peg))
               (not (bind (contents> $x-2 $y $z+2 $any-peg)))
               (bind (contents> $x-3 $y $z+3 $2nd-peg))
               (setq $dir 'lu-rd))
        (assert (discard! ?peg $x $y $z)
                (discard! $adj-peg $x-1 $y $z+1)
                (move! $2nd-peg $x-3 $y $z+3 $x-1 $y $z+1)
                (peg-count (- $peg-count 2))
                (board-pegs (set-difference $board-pegs (list ?peg $adj-peg)))))

      (if (and (<= $x (- *N* 3))
               (>= $y 4)
               (bind (contents> $x+1 $y-1 $z $adj-peg))
               (not (bind (contents> $x+2 $y-2 $z $any-peg)))
               (bind (contents> $x+3 $y-3 $z $2nd-peg))
               (setq $dir 'rh-lh))
        (assert (discard! ?peg $x $y $z)
                (discard! $adj-peg $x+1 $y-1 $z)
                (move! $2nd-peg $x+3 $y-3 $z $x+1 $y-1 $z)
                (peg-count (- $peg-count 2))
                (board-pegs (set-difference $board-pegs (list ?peg $adj-peg))))) 

      (if (and (<= $y (- *N* 3))
               (>= $x 4)
               (bind (contents> $x-1 $y+1 $z $adj-peg))
               (not (bind (contents> $x-2 $y+2 $z $any-peg)))
               (bind (contents> $x-3 $y+3 $z $2nd-peg))
               (setq $dir 'lh-rh))
        (assert (discard! ?peg $x $y $z)
                (discard! $adj-peg $x-1 $y+1 $z)
                (move! $2nd-peg $x-3 $y+3 $z $x-1 $y+1 $z)
                (peg-count (- $peg-count 2))
                (board-pegs (set-difference $board-pegs (list ?peg $adj-peg)))))))


(define-action jump
  1
  (?peg (get-remaining-pegs?))
  (and (bind (loc ?peg $x $y $z))
       (setq $x-2 (- $x 2))
       (setq $x-1 (- $x 1))
       (setq $x+1 (+ $x 1))
       (setq $x+2 (+ $x 2))
       (setq $y-2 (- $y 2))
       (setq $y-1 (- $y 1))
       (setq $y+1 (+ $y 1))
       (setq $y+2 (+ $y 2))
       (setq $z-2 (- $z 2))
       (setq $z-1 (- $z 1))
       (setq $z+1 (+ $z 1))
       (setq $z+2 (+ $z 2))
       (bind (peg-count $peg-count))
       (bind (board-pegs $board-pegs)))
  ($x $y $dir)
  (do (if (and (<= $y (- *N* 2))
               (>= $z 3)
               (bind (contents> $x $y+1 $z-1 $adj-peg))
               (not (bind (contents> $x $y+2 $z-2 $any-peg)))
               (setq $dir 'ld))
        (assert (move! ?peg $x $y $z $x $y+2 $z-2)
                (discard! $adj-peg $x $y+1 $z-1)
                (peg-count (1- $peg-count))
                (board-pegs (remove $adj-peg $board-pegs))))

      (if (and (<= $z (- *N* 2))
               (>= $y 3)
               (bind (contents> $x $y-1 $z+1 $adj-peg))
               (not (bind (contents> $x $y-2 $z+2 $any-peg)))
               (setq $dir 'ru))
        (assert (move! ?peg $x $y $z $x $y-2 $z+2)
                (discard! $adj-peg $x $y-1 $z+1)
                (peg-count (1- $peg-count))
                (board-pegs (remove $adj-peg $board-pegs))))

      (if (and (<= $x (- *N* 2))
               (>= $z 3)
               (bind (contents> $x+1 $y $z-1 $adj-peg))
               (not (bind (contents> $x+2 $y $z-2 $any-peg)))
               (setq $dir 'rd))
        (assert (move! ?peg $x $y $z $x+2 $y $z-2)
                (discard! $adj-peg $x+1 $y $z-1)
                (peg-count (1- $peg-count))
                (board-pegs (remove $adj-peg $board-pegs))))

      (if (and (<= $z (- *N* 2))
               (>= $x 3)
               (bind (contents> $x-1 $y $z+1 $adj-peg))
               (not (bind (contents> $x-2 $y $z+2 $any-peg)))
               (setq $dir 'lu))
        (assert (move! ?peg $x $y $z $x-2 $y $z+2)
                (discard! $adj-peg $x-1 $y $z+1)
                (peg-count (1- $peg-count))
                (board-pegs (remove $adj-peg $board-pegs))))

      (if (and (<= $x (- *N* 2))
               (>= $y 3)
               (bind (contents> $x+1 $y-1 $z $adj-peg))
               (not (bind (contents> $x+2 $y-2 $z $any-peg)))
               (setq $dir 'rh))
        (assert (move! ?peg $x $y $z $x+2 $y-2 $z)
                (discard! $adj-peg $x+1 $y-1 $z)
                (peg-count (1- $peg-count))
                (board-pegs (remove $adj-peg $board-pegs))))

      (if (and (<= $y (- *N* 2))
               (>= $x 3)
               (bind (contents> $x-1 $y+1 $z $adj-peg))
               (not (bind (contents> $x-2 $y+2 $z $any-peg)))
               (setq $dir 'lh))
        (assert (move! ?peg $x $y $z $x-2 $y+2 $z)
                (discard! $adj-peg $x-1 $y+1 $z)
                (peg-count (1- $peg-count))
                (board-pegs (remove $adj-peg $board-pegs))))))


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
  `(peg-count ,*final-peg-count*))




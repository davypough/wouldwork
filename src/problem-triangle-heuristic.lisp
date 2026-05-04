;;;; Filename: problem-triangle-heuristic.lisp

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


(ww-set *problem-name* triangle-heuristic)

(ww-set *problem-type* planning)

(ww-set *tree-or-graph* tree)

(ww-set *solution-type* first)


(defparameter *N* 6)  ;the number of pegs on a side

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


(define-query heuristic? ()
  ;Computes the difference between pegs and holes within a
  ;polygon enclosing the pegs
  (do (bind (peg-count $peg-count))  (bind (board-pegs $board-pegs))
      (setq $x-min 100) (setq $y-min 100) (setq $z-min 100)
      (setq $x-max 0) (setq $y-max 0) (setq $z-max 0)
      (setq $enclosed-hole-count 0)
      (doall (?peg (get-remaining-pegs?))
        (do (bind (loc ?peg $x $y $z))
            (if (< $x $x-min)
              (setq $x-min $x))   
            (if (< $y $y-min)
              (setq $y-min $y))
            (if (< $z $z-min)
              (setq $z-min $z))
            (if (> $x $x-max)
              (setq $x-max $x))
            (if (> $y $y-max)
              (setq $y-max $y))
            (if (> $z $z-max)
              (setq $z-max $z))))
      (doall (?pos position)
        (do (bind (pos ?pos $x $y $z))
            (if (and (not (bind (contents> $x $y $z $any-peg)))
                     (<= $x-min $x $x-max)
                     (<= $y-min $y $y-max)
                     (<= $z-min $z $z-max))
              (setq $enclosed-hole-count (1+ $enclosed-hole-count)))))
      (abs (- $peg-count $enclosed-hole-count 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (do (assert (if (and (<= $y (- *N* 2))
                       (>= $z 3)
                       (bind (contents> $x $y+1 $z-1 $adj-peg))
                       (not (bind (contents> $x $y+2 $z-2 $any-peg)))
                       (setq $dir 'ld))
                (do (move! ?peg $x $y $z $x $y+2 $z-2)
                    (discard! $adj-peg $x $y+1 $z-1)
                    (peg-count (1- $peg-count))
                    (board-pegs (remove $adj-peg $board-pegs)))))

      (assert (if (and (<= $z (- *N* 2))
                       (>= $y 3)
                       (bind (contents> $x $y-1 $z+1 $adj-peg))
                       (not (bind (contents> $x $y-2 $z+2 $any-peg)))
                       (setq $dir 'ru))
                (do (move! ?peg $x $y $z $x $y-2 $z+2)
                    (discard! $adj-peg $x $y-1 $z+1)
                    (peg-count (1- $peg-count))
                    (board-pegs (remove $adj-peg $board-pegs)))))

      (assert (if (and (<= $x (- *N* 2))
                       (>= $z 3)
                       (bind (contents> $x+1 $y $z-1 $adj-peg))
                       (not (bind (contents> $x+2 $y $z-2 $any-peg)))
                       (setq $dir 'rd))
                (do (move! ?peg $x $y $z $x+2 $y $z-2)
                    (discard! $adj-peg $x+1 $y $z-1)
                    (peg-count (1- $peg-count))
                    (board-pegs (remove $adj-peg $board-pegs)))))

      (assert (if (and (<= $z (- *N* 2))
                       (>= $x 3)
                       (bind (contents> $x-1 $y $z+1 $adj-peg))
                       (not (bind (contents> $x-2 $y $z+2 $any-peg)))
                       (setq $dir 'lu))
                (do (move! ?peg $x $y $z $x-2 $y $z+2)
                    (discard! $adj-peg $x-1 $y $z+1)
                    (peg-count (1- $peg-count))
                    (board-pegs (remove $adj-peg $board-pegs)))))

      (assert (if (and (<= $x (- *N* 2))
                       (>= $y 3)
                       (bind (contents> $x+1 $y-1 $z $adj-peg))
                       (not (bind (contents> $x+2 $y-2 $z $any-peg)))
                       (setq $dir 'rh))
                (do (move! ?peg $x $y $z $x+2 $y-2 $z)
                    (discard! $adj-peg $x+1 $y-1 $z)
                    (peg-count (1- $peg-count))
                    (board-pegs (remove $adj-peg $board-pegs)))))

      (assert (if (and (<= $y (- *N* 2))
                       (>= $x 3)
                       (bind (contents> $x-1 $y+1 $z $adj-peg))
                       (not (bind (contents> $x-2 $y+2 $z $any-peg)))
                       (setq $dir 'lh))
                (do (move! ?peg $x $y $z $x-2 $y+2 $z)
                    (discard! $adj-peg $x-1 $y+1 $z)
                    (peg-count (1- $peg-count))
                    (board-pegs (remove $adj-peg $board-pegs)))))))


(progn (format t "~&Initializing database...")
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
                    (update *db* `(contents> ,x ,y ,z ,peg))
                    ))))


(define-goal  ;only one peg left
  `(peg-count ,*final-peg-count*))




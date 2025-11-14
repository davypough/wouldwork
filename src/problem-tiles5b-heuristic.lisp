;;; Filename: problem-tiles5b-heuristic.lisp

;;; List problem specification for a blue/yellow tile shuffle in Islands of Insight.
;;; 4 rules

(in-package :ww)  ;required

(ww-set *problem-name* tiles5b-heuristic)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 30)

(ww-set *progress-reporting-interval* 100000)

;(ww-set *branch* 1)


(define-types
  D-tile (D1 D2)
  R-tile (R1 R2 R3)
  L-tile (L1 L2 L3)
  U-tile (U1 U2)
  Y-tile (Y1)
;  coord (compute (loop for row from 0 to 7   ;board coordinates
;                         append (loop for col from 0 to 6
;                                  collect (cons row col))))
  tile   (either D-tile R-tile L-tile U-tile Y-tile))


(define-dynamic-relations
  (loc tile $fixnum $fixnum)  ;location of a tile with row, col coordinates
  (emptys $list))  ;list of empty (row . col) coordinates


(define-static-relations
  (Y-goal $fixnum $fixnum))


(define-query heuristic? ()
  ;Get the manhattan distance from first coord of Y tile to the goal coord. Lower is better.
  (do (bind (loc Y1 $Y1-row $Y1-col))
      (bind (Y1-goal $Y1-goal-row $Y1-goal-col))
      (+ (abs (- $Y1-row $Y1-goal-row)))
         (abs (- $Y1-col $Y1-goal-col))))


(defun check-coord (new-row new-col)
  (when (and (<= 0 new-row 3)
             (<= 0 new-col 3))
    (cons new-row new-col)))


(defun sort-coords (coords)
  "Keeps coordinates lexicographically sorted."
  (sort coords (lambda (a b)
                 (or (< (car a) (car b))
                     (and (= (car a) (car b))
                          (< (cdr a) (cdr b)))))))


;------------------ Action Rules ------------------

(define-action move-D  ;move down-tile
  1
  (?tile D-tile)
  (always-true)
  (?tile $direction)
  (do (bind (loc ?tile $row $col))
      (bind (emptys $emptys))
      (if (and (setq $empty1-coord (check-coord $row (+ $col 2)))
               (member $empty1-coord $emptys :test #'equal)
               (setq $empty2-coord (check-coord (1+ $row) (1+ $col)))
               (member $empty2-coord $emptys :test #'equal))
        (assert (setq $direction 'right)
                (setq $new-emptys (copy-list $emptys))
                (alexandria:deletef $new-emptys $empty1-coord :test #'equal)
                (alexandria:deletef $new-emptys $empty2-coord :test #'equal)
                (push (cons $row (1- $col)) $new-emptys)
                (push (cons (1+ $row) $col) $new-emptys)
                (setq $new-emptys (sort-coords $new-emptys))
                (emptys $new-emptys)
                (loc ?tile $row (1+ $col))))
      (if (and (setq $empty1-coord (check-coord $row (- $col 2)))
               (member $empty1-coord $emptys :test #'equal)
               (setq $empty2-coord (check-coord (1+ $row) (1- $col)))
               (member $empty2-coord $emptys :test #'equal))
        (assert (setq $direction 'left)
                (setq $new-emptys (copy-list $emptys))
                (alexandria:deletef $new-emptys $empty1-coord :test #'equal)
                (alexandria:deletef $new-emptys $empty2-coord :test #'equal)
                (push (cons $row (1+ $col)) $new-emptys)
                (push (cons (1+ $row) $col) $new-emptys)
                (setq $new-emptys (sort-coords $new-emptys))
                (emptys $new-emptys)
                (loc ?tile $row (1- $col))))
      (if (and (setq $empty1-coord (check-coord (+ $row 1) (- $col 1))
               (member $empty1-coord $emptys :test #'equal)
               (setq $empty2-coord (check-coord (+ $row 2) $col))
               (member $empty2-coord $emptys :test #'equal)
               (setq $empty3-coord (check-coord (+ $row 1) (1+ $col)))
               (member $empty3-coord $emptys :test #'equal))
        (assert (setq $direction 'down)
                (setq $new-emptys (copy-list $emptys))
                (alexandria:deletef $new-emptys $empty1-coord :test #'equal)
                (alexandria:deletef $new-emptys $empty2-coord :test #'equal)
                (alexandria:deletef $new-emptys $empty3-coord :test #'equal)
                (push (cons $row $col) $new-emptys)
                (push (cons $row (1- $col)) $new-emptys)
                (push (cons $row (1+ $col)) $new-emptys)
                (setq $new-emptys (sort-coords $new-emptys))
                (emptys $new-emptys)
                (loc ?tile (1+ $row) $col)))
      (if (and (setq $empty1-coord (check-coord (1- $row) (1- $col)))
               (member $empty1-coord $emptys :test #'equal)
               (setq $empty2-coord (check-coord (1- $row) $col))
               (member $empty2-coord $emptys :test #'equal))
               (setq $empty3-coord (check-coord (1- $row) (1+ $col)))
               (member $empty3-coord $emptys :test #'equal))
        (assert (setq $direction 'up)
                (setq $new-emptys (copy-list $emptys))
                (alexandria:deletef $new-emptys $empty1-coord :test #'equal)
                (alexandria:deletef $new-emptys $empty2-coord :test #'equal)
                (alexandria:deletef $new-emptys $empty3-coord :test #'equal)
                (push (cons $row (1- $col)) $new-emptys)
                (push (cons (1+ $row) $col) $new-emptys)
                (push (cons $row (1+ $col)) $new-emptys)
                (setq $new-emptys (sort-coords $new-emptys))
                (emptys $new-emptys)
                (loc ?tile (1- $row) $col)))))


(define-action move-U
  1
  (?tile U-tile)
  (always-true)
  (?tile $direction)
  (do (bind (loc ?tile $row $col))
      (bind (emptys $emptys))
      (if (and (setq $empty1-coord (check-coord $row (+ $col 2)))
               (member $empty1-coord $emptys :test #'equal)
               (setq $empty2-coord (check-coord (1- $row) (1+ $col)))
               (member $empty2-coord $emptys :test #'equal))
        (assert (setq $direction 'right)
                (setq $new-emptys (copy-list $emptys))
                (alexandria:deletef $new-emptys $empty1-coord :test #'equal)
                (alexandria:deletef $new-emptys $empty2-coord :test #'equal)
                (push (cons $row (1- $col)) $new-emptys)
                (push (cons (1- $row) $col) $new-emptys)
                (setq $new-emptys (sort-coords $new-emptys))
                (emptys $new-emptys)
                (loc ?tile $row (1+ $col))))
      (if (and (setq $empty1-coord (check-coord $row (- $col 2)))
               (member $empty1-coord $emptys :test #'equal)
               (setq $empty2-coord (check-coord (1- $row) (1- $col)))
               (member $empty2-coord $emptys :test #'equal))
        (assert (setq $direction 'left)
                (setq $new-emptys (copy-list $emptys))
                (alexandria:deletef $new-emptys $empty1-coord :test #'equal)
                (alexandria:deletef $new-emptys $empty2-coord :test #'equal)
                (push (cons $row (1+ $col)) $new-emptys)
                (push (cons (1- $row) $col) $new-emptys)
                (setq $new-emptys (sort-coords $new-emptys))
                (emptys $new-emptys)
                (loc ?tile $row (1- $col))))
      (if (and (setq $empty1-coord (check-coord (+ $row 1) (- $col 1))
               (member $empty1-coord $emptys :test #'equal)
               (setq $empty2-coord (check-coord (+ $row 1) $col))
               (member $empty2-coord $emptys :test #'equal)
               (setq $empty3-coord (check-coord (+ $row 1) (1+ $col)))
               (member $empty3-coord $emptys :test #'equal))
        (assert (setq $direction 'down)
                (setq $new-emptys (copy-list $emptys))
                (alexandria:deletef $new-emptys $empty1-coord :test #'equal)
                (alexandria:deletef $new-emptys $empty2-coord :test #'equal)
                (alexandria:deletef $new-emptys $empty3-coord :test #'equal)
                (push (cons (1- $row) $col) $new-emptys)
                (push (cons $row (1- $col)) $new-emptys)
                (push (cons $row (1+ $col)) $new-emptys)
                (setq $new-emptys (sort-coords $new-emptys))
                (emptys $new-emptys)
                (loc ?tile (1+ $row) $col)))
      (if (and (setq $empty1-coord (check-coord (- $row 2) $col))
               (member $empty1-coord $emptys :test #'equal)
               (setq $empty2-coord (check-coord (1- $row) (1+ $col)))
               (member $empty2-coord $emptys :test #'equal))
               (setq $empty3-coord (check-coord (1- $row) (1- $col)))
               (member $empty3-coord $emptys :test #'equal))
        (assert (setq $direction 'up)
                (setq $new-emptys (copy-list $emptys))
                (alexandria:deletef $new-emptys $empty1-coord :test #'equal)
                (alexandria:deletef $new-emptys $empty2-coord :test #'equal)
                (alexandria:deletef $new-emptys $empty3-coord :test #'equal)
                (push (cons $row (1- $col)) $new-emptys)
                (push (cons $row $col) $new-emptys)
                (push (cons $row (1+ $col)) $new-emptys)
                (setq $new-emptys (sort-coords $new-emptys))
                (emptys $new-emptys)
                (loc ?tile (1- $row) $col)))))


(define-action move-R
  1
  (?tile R-tile)
  (always-true)
  (?tile $direction)
  (do (bind (loc ?tile $row $col))
      (bind (emptys $emptys))
      (if (and (setq $empty1-coord (check-coord (- $row 2) $col))
               (member $empty1-coord $emptys :test #'equal)
               (setq $empty2-coord (check-coord (1- $row) (1+ $col)))
               (member $empty2-coord $emptys :test #'equal))
        (assert (setq $direction 'up)
                (setq $new-emptys (copy-list $emptys))
                (alexandria:deletef $new-emptys $empty1-coord :test #'equal)
                (alexandria:deletef $new-emptys $empty2-coord :test #'equal)
                (push (cons $row (1+ $col)) $new-emptys)
                (push (cons (1+ $row) $col) $new-emptys)
                (setq $new-emptys (sort-coords $new-emptys))
                (emptys $new-emptys)
                (loc ?tile (1- $row) $col)))
      (if (and (setq $empty1-coord (check-coord (+ $row 2) $col))
               (member $empty1-coord $emptys :test #'equal)
               (setq $empty2-coord (check-coord (1+ $row) (1+ $col)))
               (member $empty2-coord $emptys :test #'equal))
        (assert (setq $direction 'down)
                (setq $new-emptys (copy-list $emptys))
                (alexandria:deletef $new-emptys $empty1-coord :test #'equal)
                (alexandria:deletef $new-emptys $empty2-coord :test #'equal)
                (push (cons $row (1+ $col)) $new-emptys)
                (push (cons (1- $row) $col) $new-emptys)
                (setq $new-emptys (sort-coords $new-emptys))
                (emptys $new-emptys)
                (loc ?tile (1+ $row) $col)))
      (if (and (setq $empty1-coord (check-coord (+ $row 1) (+ $col 1))
               (member $empty1-coord $emptys :test #'equal)
               (setq $empty2-coord (check-coord $row (+ $col 2)))
               (member $empty2-coord $emptys :test #'equal)
               (setq $empty3-coord (check-coord (- $row 1) (1+ $col)))
               (member $empty3-coord $emptys :test #'equal)))
        (assert (setq $direction 'right)
                (setq $new-emptys (copy-list $emptys))
                (alexandria:deletef $new-emptys $empty1-coord :test #'equal)
                (alexandria:deletef $new-emptys $empty2-coord :test #'equal)
                (alexandria:deletef $new-emptys $empty3-coord :test #'equal)
                (push (cons (1- $row) $col) $new-emptys)
                (push (cons $row $col) $new-emptys)
                (push (cons (1+ $row) $col) $new-emptys)
                (setq $new-emptys (sort-coords $new-emptys))
                (emptys $new-emptys)
                (loc ?tile $row (1+ $col))))
      (if (and (setq $empty1-coord (check-coord (+ $row 1) (1- $col)))
               (member $empty1-coord $emptys :test #'equal)
               (setq $empty2-coord (check-coord $row (1- $col)))
               (member $empty2-coord $emptys :test #'equal))
               (setq $empty3-coord (check-coord (1- $row) (1- $col)))
               (member $empty3-coord $emptys :test #'equal))
        (assert (setq $direction 'left)
                (setq $new-emptys (copy-list $emptys))
                (alexandria:deletef $new-emptys $empty1-coord :test #'equal)
                (alexandria:deletef $new-emptys $empty2-coord :test #'equal)
                (alexandria:deletef $new-emptys $empty3-coord :test #'equal)
                (push (cons (1- $row) $col) $new-emptys)
                (push (cons $row $col) $new-emptys)
                (push (cons (1+ $row) $col) $new-emptys)
                (setq $new-emptys (sort-coords $new-emptys))
                (emptys $new-emptys)
                (loc ?tile $row (1- $col)))))


(define-action D-right  ;move D-tile right
  1
  (?tile D-tile)
  (and (bind (loc ?tile $loc))
       (bind (emptys $emptys))
       (setq $empty1-coord (rel-coord $loc 0 2))
       (gethash $empty1-coord $emptys)
       (setq $empty2-coord (rel-coord $loc 1 1))
       (gethash $empty2-coord $emptys))
  (?tile)
  (assert (loc ?tile (rel-coord $loc 0 1))
          (setq $new-emptys (alexandria:copy-hash-table $emptys))
          (remhash $empty1-coord $new-emptys)
          (remhash $empty2-coord $new-emptys)
          (setf (gethash (rel-coord $loc 0 -1) $new-emptys) t)
          (setf (gethash (rel-coord $loc 1 0) $new-emptys) t)
          (emptys $new-emptys)))


(define-action D-down
  1
  (?tile D-tile)
  (and (bind (loc ?tile $loc))
       (bind (emptys $emptys))
       (setq $empty1-coord (rel-coord $loc 1 -1))
       (gethash $empty1-coord $emptys)
       (setq $empty2-coord (rel-coord $loc 2 0))
       (gethash $empty2-coord $emptys)
       (setq $empty3-coord (rel-coord $loc 1 1))
       (gethash $empty3-coord $emptys))
  (?tile)
  (assert (loc ?tile (rel-coord $loc 1 0))
          (setq $new-emptys (alexandria:copy-hash-table $emptys))
          (remhash $empty1-coord $new-emptys)
          (remhash $empty2-coord $new-emptys)
          (remhash $empty3-coord $new-emptys)
          (setf (gethash (rel-coord $loc 0 -1) $new-emptys) t)
          (setf (gethash (rel-coord $loc 0 0) $new-emptys) t)
          (setf (gethash (rel-coord $loc 0 1) $new-emptys) t)
          (emptys $new-emptys)))


(define-action D-left
  1
  (?tile D-tile)
  (and (bind (loc ?tile $loc))
       (bind (emptys $emptys))
       (setq $empty1-coord (rel-coord $loc 0 -2))
       (gethash $empty1-coord $emptys)
       (setq $empty2-coord (rel-coord $loc 1 -1))
       (gethash $empty2-coord $emptys))
  (?tile)
  (assert (loc ?tile (rel-coord $loc 0 -1))
          (setq $new-emptys (alexandria:copy-hash-table $emptys))
          (remhash $empty1-coord $new-emptys)
          (remhash $empty2-coord $new-emptys)
          (setf (gethash (rel-coord $loc 0 1) $new-emptys) t)
          (setf (gethash (rel-coord $loc 1 0) $new-emptys) t)
          (emptys $new-emptys)))


(define-action D-up
  1
  (?tile D-tile)
  (and (bind (loc ?tile $loc))
       (bind (emptys $emptys))
       (setq $empty1-coord (rel-coord $loc -1 -1))
       (gethash $empty1-coord $emptys)
       (setq $empty2-coord (rel-coord $loc -1 0))
       (gethash $empty2-coord $emptys)
       (setq $empty3-coord (rel-coord $loc -1 1))
       (gethash $empty3-coord $emptys))
  (?tile)
  (assert (loc ?tile (rel-coord $loc -1 0))
          (setq $new-emptys (alexandria:copy-hash-table $emptys))
          (remhash $empty1-coord $new-emptys)
          (remhash $empty2-coord $new-emptys)
          (remhash $empty3-coord $new-emptys)
          (setf (gethash (rel-coord $loc 0 -1) $new-emptys) t)
          (setf (gethash (rel-coord $loc 1 0) $new-emptys) t)
          (setf (gethash (rel-coord $loc 0 1) $new-emptys) t)
          (emptys $new-emptys)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-action U-right  ;move u-tile right
  1
  (?tile U-tile)
  (and (bind (loc ?tile $loc))
       (bind (emptys $emptys))
       (setq $empty1-coord (rel-coord $loc -1 1))
       (gethash $empty1-coord $emptys)
       (setq $empty2-coord (rel-coord $loc 0 2))
       (gethash $empty2-coord $emptys))
  (?tile)
  (assert (loc ?tile (rel-coord $loc 0 1))
          (setq $new-emptys (alexandria:copy-hash-table $emptys))
          (remhash $empty1-coord $new-emptys)
          (remhash $empty2-coord $new-emptys)
          (setf (gethash (rel-coord $loc -1 0) $new-emptys) t)
          (setf (gethash (rel-coord $loc 0 -1) $new-emptys) t)
          (emptys $new-emptys)))


(define-action U-down
  1
  (?tile U-tile)
  (and (bind (loc ?tile $loc))
       (bind (emptys $emptys))
       (setq $empty1-coord (rel-coord $loc 1 -1))
       (gethash $empty1-coord $emptys)
       (setq $empty2-coord (rel-coord $loc 1 0))
       (gethash $empty2-coord $emptys)
       (setq $empty3-coord (rel-coord $loc 1 1))
       (gethash $empty3-coord $emptys))
  (?tile)
  (assert (loc ?tile (rel-coord $loc 1 0))
          (setq $new-emptys (alexandria:copy-hash-table $emptys))
          (remhash $empty1-coord $new-emptys)
          (remhash $empty2-coord $new-emptys)
          (remhash $empty3-coord $new-emptys)
          (setf (gethash (rel-coord $loc 0 -1) $new-emptys) t)
          (setf (gethash (rel-coord $loc -1 0) $new-emptys) t)
          (setf (gethash (rel-coord $loc 0 1) $new-emptys) t)
          (emptys $new-emptys)))


(define-action U-left
  1
  (?tile U-tile)
  (and (bind (loc ?tile $loc))
       (bind (emptys $emptys))
       (setq $empty1-coord (rel-coord $loc 0 -2))
       (gethash $empty1-coord $emptys)
       (setq $empty2-coord (rel-coord $loc -1 -1))
       (gethash $empty2-coord $emptys))
  (?tile)
  (assert (loc ?tile (rel-coord $loc 0 -1))
          (setq $new-emptys (alexandria:copy-hash-table $emptys))
          (remhash $empty1-coord $new-emptys)
          (remhash $empty2-coord $new-emptys)
          (setf (gethash (rel-coord $loc 0 1) $new-emptys) t)
          (setf (gethash (rel-coord $loc -1 0) $new-emptys) t)
          (emptys $new-emptys)))


(define-action U-up
  1
  (?tile U-tile)
  (and (bind (loc ?tile $loc))
       (bind (emptys $emptys))
       (setq $empty1-coord (rel-coord $loc -1 -1))
       (gethash $empty1-coord $emptys)
       (setq $empty2-coord (rel-coord $loc -2 0))
       (gethash $empty2-coord $emptys)
       (setq $empty3-coord (rel-coord $loc -1 1))
       (gethash $empty3-coord $emptys))
  (?tile)
  (assert (loc ?tile (rel-coord $loc -1 0))
          (setq $new-emptys (alexandria:copy-hash-table $emptys))
          (remhash $empty1-coord $new-emptys)
          (remhash $empty2-coord $new-emptys)
          (remhash $empty3-coord $new-emptys)
          (setf (gethash (rel-coord $loc 0 -1) $new-emptys) t)
          (setf (gethash (rel-coord $loc 0 0) $new-emptys) t)
          (setf (gethash (rel-coord $loc 0 1) $new-emptys) t)
          (emptys $new-emptys)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-action R-up
  1
  (?tile R-tile)
  (and (bind (loc ?tile $loc))
       (bind (emptys $emptys))
       (setq $empty1-coord (rel-coord $loc -2 0))
       (gethash $empty1-coord $emptys)
       (setq $empty2-coord (rel-coord $loc -1 1))
       (gethash $empty2-coord $emptys))
  (?tile)
  (assert (loc ?tile (rel-coord $loc -1 0))
          (setq $new-emptys (alexandria:copy-hash-table $emptys))
          (remhash $empty1-coord $new-emptys)
          (remhash $empty2-coord $new-emptys)
          (setf (gethash (rel-coord $loc 1 0) $new-emptys) t)
          (setf (gethash (rel-coord $loc 0 1) $new-emptys) t)
          (emptys $new-emptys)))


(define-action R-right
  1
  (?tile R-tile)
  (and (bind (loc ?tile $loc))
       (bind (emptys $emptys))
       (setq $empty1-coord (rel-coord $loc -1 1))
       (gethash $empty1-coord $emptys)
       (setq $empty2-coord (rel-coord $loc 0 2))
       (gethash $empty2-coord $emptys)
       (setq $empty3-coord (rel-coord $loc 1 1))
       (gethash $empty3-coord $emptys))
  (?tile)
  (assert (loc ?tile (rel-coord $loc 0 1))
          (setq $new-emptys (alexandria:copy-hash-table $emptys))
          (remhash $empty1-coord $new-emptys)
          (remhash $empty2-coord $new-emptys)
          (remhash $empty3-coord $new-emptys)
          (setf (gethash (rel-coord $loc -1 0) $new-emptys) t)
          (setf (gethash (rel-coord $loc 0 0) $new-emptys) t)
          (setf (gethash (rel-coord $loc 1 0) $new-emptys) t)
          (emptys $new-emptys)))


(define-action R-down
  1
  (?tile R-tile)
  (and (bind (loc ?tile $loc))
       (bind (emptys $emptys))
       (setq $empty1-coord (rel-coord $loc 2 0))
       (gethash $empty1-coord $emptys)
       (setq $empty2-coord (rel-coord $loc 1 1))
       (gethash $empty2-coord $emptys))
  (?tile)
  (assert (loc ?tile (rel-coord $loc 1 0))
          (setq $new-emptys (alexandria:copy-hash-table $emptys))
          (remhash $empty1-coord $new-emptys)
          (remhash $empty2-coord $new-emptys)
          (setf (gethash (rel-coord $loc -1 0) $new-emptys) t)
          (setf (gethash (rel-coord $loc 0 1) $new-emptys) t)
          (emptys $new-emptys)))


(define-action R-left
  1
  (?tile R-tile)
  (and (bind (loc ?tile $loc))
       (bind (emptys $emptys))
       (setq $empty1-coord (rel-coord $loc -1 -1))
       (gethash $empty1-coord $emptys)
       (setq $empty2-coord (rel-coord $loc 0 -1))
       (gethash $empty2-coord $emptys)
       (setq $empty3-coord (rel-coord $loc 1 -1))
       (gethash $empty3-coord $emptys))
  (?tile)
  (assert (loc ?tile (rel-coord $loc 0 -1))
          (setq $new-emptys (alexandria:copy-hash-table $emptys))
          (remhash $empty1-coord $new-emptys)
          (remhash $empty2-coord $new-emptys)
          (remhash $empty3-coord $new-emptys)
          (setf (gethash (rel-coord $loc -1 0) $new-emptys) t)
          (setf (gethash (rel-coord $loc 0 1) $new-emptys) t)
          (setf (gethash (rel-coord $loc 1 0) $new-emptys) t)
          (emptys $new-emptys)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-action L-up
  1
  (?tile L-tile)
  (and (bind (loc ?tile $loc))
       (bind (emptys $emptys))
       (setq $empty1-coord (rel-coord $loc -1 -1))
       (gethash $empty1-coord $emptys)
       (setq $empty2-coord (rel-coord $loc -2 0))
       (gethash $empty2-coord $emptys))
  (?tile)
  (assert (loc ?tile (rel-coord $loc -1 0))
          (setq $new-emptys (alexandria:copy-hash-table $emptys))
          (remhash $empty1-coord $new-emptys)
          (remhash $empty2-coord $new-emptys)
          (setf (gethash (rel-coord $loc 0 -1) $new-emptys) t)
          (setf (gethash (rel-coord $loc 1 0) $new-emptys) t)
          (emptys $new-emptys)))


(define-action L-right
  1
  (?tile L-tile)
  (and (bind (loc ?tile $loc))
       (bind (emptys $emptys))
       (setq $empty1-coord (rel-coord $loc -1 1))
       (gethash $empty1-coord $emptys)
       (setq $empty2-coord (rel-coord $loc 0 1))
       (gethash $empty2-coord $emptys)
       (setq $empty3-coord (rel-coord $loc 1 1))
       (gethash $empty3-coord $emptys))
  (?tile)
  (assert (loc ?tile (rel-coord $loc 0 1))
          (setq $new-emptys (alexandria:copy-hash-table $emptys))
          (remhash $empty1-coord $new-emptys)
          (remhash $empty2-coord $new-emptys)
          (remhash $empty3-coord $new-emptys)
          (setf (gethash (rel-coord $loc -1 0) $new-emptys) t)
          (setf (gethash (rel-coord $loc 0 -1) $new-emptys) t)
          (setf (gethash (rel-coord $loc 1 0) $new-emptys) t)
          (emptys $new-emptys)))


(define-action L-down
  1
  (?tile L-tile)
  (and (bind (loc ?tile $loc))
       (bind (emptys $emptys))
       (setq $empty1-coord (rel-coord $loc 1 -1))
       (gethash $empty1-coord $emptys)
       (setq $empty2-coord (rel-coord $loc 2 0))
       (gethash $empty2-coord $emptys))
  (?tile)
  (assert (loc ?tile (rel-coord $loc 0 -1))
          (setq $new-emptys (alexandria:copy-hash-table $emptys))
          (remhash $empty1-coord $new-emptys)
          (remhash $empty2-coord $new-emptys)
          (setf (gethash (rel-coord $loc 0 -1) $new-emptys) t)
          (setf (gethash (rel-coord $loc -1 0) $new-emptys) t)
          (emptys $new-emptys)))


(define-action L-left
  1
  (?tile L-tile)
  (and (bind (loc ?tile $loc))
       (bind (emptys $emptys))
       (setq $empty1-coord (rel-coord $loc -1 -1))
       (gethash $empty1-coord $emptys)
       (setq $empty2-coord (rel-coord $loc 0 -2))
       (gethash $empty2-coord $emptys)
       (setq $empty3-coord (rel-coord $loc 1 -1))
       (gethash $empty3-coord $emptys))
  (?tile)
  (assert (loc ?tile (rel-coord $loc 0 -1))
          (setq $new-emptys (alexandria:copy-hash-table $emptys))
          (remhash $empty1-coord $new-emptys)
          (remhash $empty2-coord $new-emptys)
          (remhash $empty3-coord $new-emptys)
          (setf (gethash (rel-coord $loc -1 0) $new-emptys) t)
          (setf (gethash (rel-coord $loc 0 0) $new-emptys) t)
          (setf (gethash (rel-coord $loc 1 0) $new-emptys) t)
          (emptys $new-emptys)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-action Y-right
  1
  (?tile Y-tile)
  (and (bind (loc ?tile $loc))
       (bind (emptys $emptys))
       (setq $empty1-coord (rel-coord $loc 0 2))
       (gethash $empty1-coord $emptys)
       (setq $empty2-coord (rel-coord $loc 1 1))
       (gethash $empty2-coord $emptys))
  (?tile)
  (assert (loc ?tile (rel-coord $loc 0 1))
          (setq $new-emptys (alexandria:copy-hash-table $emptys))
          (remhash $empty1-coord $new-emptys)
          (remhash $empty2-coord $new-emptys)
          (setf (gethash (rel-coord $loc 0 0) $new-emptys) t)
          (setf (gethash (rel-coord $loc 1 -1) $new-emptys) t)
          (emptys $new-emptys)))


(define-action Y-down
  1
  (?tile Y-tile)
  (and (bind (loc ?tile $loc))
       (bind (emptys $emptys))
       (setq $empty1-coord (rel-coord $loc 2 -1))
       (gethash $empty1-coord $emptys)
       (setq $empty2-coord (rel-coord $loc 2 0))
       (gethash $empty2-coord $emptys)
       (setq $empty3-coord (rel-coord $loc 1 1))
       (gethash $empty3-coord $emptys))
  (?tile)
  (assert (loc ?tile (rel-coord $loc 0 -1))
          (setq $new-emptys (alexandria:copy-hash-table $emptys))
          (remhash $empty1-coord $new-emptys)
          (remhash $empty2-coord $new-emptys)
          (remhash $empty3-coord $new-emptys)
          (setf (gethash (rel-coord $loc 0 1) $new-emptys) t)
          (setf (gethash (rel-coord $loc 0 0) $new-emptys) t)
          (setf (gethash (rel-coord $loc 1 -1) $new-emptys) t)
          (emptys $new-emptys)))


(define-action Y-left
  1
  (?tile Y-tile)
  (and (bind (loc ?tile $loc))
       (bind (emptys $emptys))
       (setq $empty1-coord (rel-coord $loc 0 -1))
       (gethash $empty1-coord $emptys)
       (setq $empty2-coord (rel-coord $loc 1 -2))
       (gethash $empty2-coord $emptys))
  (?tile)
  (assert (loc ?tile (rel-coord $loc 0 -1))
          (setq $new-emptys (alexandria:copy-hash-table $emptys))
          (remhash $empty1-coord $new-emptys)
          (remhash $empty2-coord $new-emptys)
          (setf (gethash (rel-coord $loc 0 1) $new-emptys) t)
          (setf (gethash (rel-coord $loc 1 0) $new-emptys) t)
          (emptys $new-emptys)))


(define-action Y-up
  1
  (?tile Y-tile)
  (and (bind (loc ?tile $loc))
       (bind (emptys $emptys))
       (setq $empty1-coord (rel-coord $loc 0 -1))
       (gethash $empty1-coord $emptys)
       (setq $empty2-coord (rel-coord $loc -1 0))
       (gethash $empty2-coord $emptys)
       (setq $empty3-coord (rel-coord $loc -1 1))
       (gethash $empty3-coord $emptys))
  (?tile)
  (assert (loc ?tile (rel-coord $loc -1 0))
          (setq $new-emptys (alexandria:copy-hash-table $emptys))
          (remhash $empty1-coord $new-emptys)
          (remhash $empty2-coord $new-emptys)
          (remhash $empty3-coord $new-emptys)
          (setf (gethash (rel-coord $loc 1 -1) $new-emptys) t)
          (setf (gethash (rel-coord $loc 1 0) $new-emptys) t)
          (setf (gethash (rel-coord $loc 0 1) $new-emptys) t)
          (emptys $new-emptys)))

       
(define-init
  (loc D1 (0 . 2)) (loc Y1 (0 . 4))
  (loc R1 (1 . 0)) (loc L1 (1 . 6))
  (loc L2 (3 . 1)) (loc R2 (3 . 5))
  (loc R3 (5 . 0)) (loc L3 (5 . 6))
  (loc D2 (6 . 3))
  (loc U1 (7 . 1)) (loc U2 (7 . 5))
  (Y-goal (6 . 2)))


(define-init-action init-emptys
  0
  ()
  (setq $ht (make-hash-table :test #'equal))
  ()
  (assert (ww-loop for $row from 2 to 5 do  ;board coordinates
            (ww-loop for $col from 2 to 4 do
              (setf (gethash (cons $row $col) $ht) t)))
          (emptys $ht)))


(define-goal
  (loc Y1 (6 . 2)))

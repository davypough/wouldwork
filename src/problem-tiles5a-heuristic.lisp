;;; Filename: problem-tiles5a-heuristic.lisp

;;; List problem specification for a blue/yellow tile shuffle in Islands of Insight.
;;; 16 rules (too many)

(in-package :ww)  ;required

(ww-set *problem-name* tiles5a-heuristic)

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
  tile   (either D-tile R-tile L-tile U-tile Y-tile))


(define-dynamic-relations
  (loc tile $cons)  ;location of a tile with coordinates
  (emptys $hash-table))  ;location of empty spaces, ht of cons


(define-static-relations
  (Y-goal $cons))


(define-query heuristic? ()
  ;Get the manhattan distance from first coord of Y tile to the goal coord. Lower is better.
  (do (bind (loc Y1 $Y-coord))
      (bind (Y-goal $Y-goal-coord))
      (+ (abs (- (car $Y-coord) (car $Y-goal-coord)))
         (abs (- (cdr $Y-coord) (cdr $Y-goal-coord))))))


(defun rel-coord (loc drow dcol)
  (let ((new-row (+ (car loc) drow))
        (new-col (+ (cdr loc) dcol)))
    (when (and (<= 0 new-row 7)
               (<= 0 new-col 6))
      (cons new-row new-col))))


;------------------ Action Rules ------------------


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

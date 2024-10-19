;;; Filename: problem-tiles5c-heuristic.lisp


;;; List problem specification for a blue/yellow tile shuffle in Islands of Insight.
;;; One rule only
;;; Note that heuristic doesn't help with this problem.


(in-package :ww)  ;required

(ww-set *problem-name* tiles5c)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 50)

(ww-set *progress-reporting-interval* 1000000)




(define-types
  D-tile (D1 D2)
  R-tile (R1 R2 R3)
  L-tile (L1 L2 L3)
  U-tile (U1 U2)
  Y-tile (Y1)
  tile   (either D-tile R-tile L-tile U-tile Y-tile)
  delta-row (0 1 0 -1)  ;for moving right,down,left,up
  delta-col (1 0 -1 0))


(define-dynamic-relations
  (loc tile $list)  ;location of the coordinates of the parts of a tile; list of cons
  (empty $list))  ;list of cons


(define-static-relations
  (Y1-goal-coord $cons))


(define-query heuristic? ()
  ;Get the manhattan distance from first coord of Y tile to the goal coord. Lower is better.
  (do (bind (loc Y1 $Y1-coords))
      (bind (Y1-goal-coord $Y1-goal-coord))
      (+ (abs (- (caar $Y1-coords) (car $Y1-goal-coord)))  ;compare first coord of each
         (abs (- (cdar $Y1-coords) (cdr $Y1-goal-coord))))))


(defun sort-coords (coords)
  "Keeps coordinates lexicographically sorted."
  (sort coords (lambda (a b)
                 (or (< (car a) (car b))
                     (and (= (car a) (car b))
                          (< (cdr a) (cdr b)))))))


(define-action move
  1
  (?tile tile (dot-product ?d-row delta-row ?d-col delta-col))
  (and (bind (loc ?tile $tile-coords))
       (bind (empty $empty-coords))
       (setf $new-empty-coords (copy-list $empty-coords))
       (iter (for tile-coord in $tile-coords)  ;starting empty coords subsequently updated
             (for new-tile-coord = (cons (+ (car tile-coord) ?d-row) (+ (cdr tile-coord) ?d-col)))
             (push new-tile-coord $new-tile-coords)
             (if (member new-tile-coord $empty-coords :test #'equal)
               (setf $new-empty-coords (delete new-tile-coord $new-empty-coords :test #'equal))
               (unless (member new-tile-coord $tile-coords :test #'equal)  ;tile coord can move into spot vacated by other tile coord
                 (return nil)))  ;can't make this move
             (for opposite-coord = (cons (- (car tile-coord) ?d-row) (- (cdr tile-coord) ?d-col)))
             (when (or (member opposite-coord $empty-coords :test #'equal)  ;check before next
                       (not (member opposite-coord $tile-coords :test #'equal)))
               (push tile-coord $new-empty-coords))
             (finally (return t))))
  (?tile $direction)
  (do (setf $direction (cond ((= ?d-col 1) 'right)
                             ((= ?d-row 1) 'down)
                             ((= ?d-col -1) 'left)
                             ((= ?d-row -1) 'up)
                             (t (error "Incorrect direction"))))
      (assert (loc ?tile (sort-coords $new-tile-coords))
              (empty (sort-coords $new-empty-coords)))))
      

(define-init
  (loc D1 ((0 . 1) (0 . 2) (0 . 3) (1 . 2)))
  (loc D2 ((6 . 2) (6 . 3) (6 . 4) (7 . 3)))
  (loc R1 ((0 . 0) (1 . 0) (1 . 1) (2 . 0)))
  (loc R2 ((2 . 5) (3 . 5) (3 . 6) (4 . 5)))
  (loc R3 ((4 . 0) (5 . 0) (5 . 1) (6 . 0)))
  (loc L1 ((0 . 6) (1 . 5) (1 . 6) (2 . 6)))
  (loc L2 ((2 . 1) (3 . 0) (3 . 1) (4 . 1)))
  (loc L3 ((4 . 6) (5 . 5) (5 . 6) (6 . 6)))
  (loc U1 ((6 . 1) (7 . 0) (7 . 1) (7 . 2)))
  (loc U2 ((6 . 5) (7 . 4) (7 . 5) (7 . 6)))
  (loc Y1 ((0 . 4) (0 . 5) (1 . 3) (1 . 4)))
  (empty #.(loop for row from 2 to 5 append
                 (loop for col from 2 to 4 collect
                   (cons row col))))
  (Y1-goal-coord (6 . 2)))


(define-goal
  ;(loc Y1 ((1 . 4) (1 . 5) (2 . 3) (2 . 4))))
  (loc Y1 ((6 . 2) (6 . 3) (7 . 1) (7 . 2))))

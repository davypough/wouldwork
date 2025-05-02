;;; Filename: problem-tiles1a-heuristic.lisp


;;; List problem specification for a blue/yellow tile shuffle in Islands of Insight.
;;; Basic search using (row . col) for coordinates (fastest)

(in-package :ww)  ;required

(ww-set *problem-name* tiles1a-heuristic)

(ww-set *problem-type* planning)

(ww-set *solution-type* first)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 20)


(define-types
  tile   (SQ HOR VER L1 L2)  ;L2 is yellow
  delta-row (0 1 0 -1)  ;for moving right,down,left,up
  delta-col (1 0 -1 0))


(define-dynamic-relations
  (loc tile $list)  ;location of a tile with coordinates
  (empty $list))


(define-static-relations
  (Y-goal-coord $cons))


(define-query heuristic? ()
  ;Get the manhattan distance from first coord of Y tile to the goal coord. Lower is better.
  (do (bind (loc L2 $L2-coords))
      (bind (Y-goal-coord $Y-goal-coord))
      (+ (abs (- (caar $L2-coords) (car $Y-goal-coord)))  ;compare first coord of each
         (abs (- (cdar $L2-coords) (cdr $Y-goal-coord))))))


(defun in-bounds (coord)
  (and (<= 0 (car coord) 6)
       (<= 0 (cdr coord) 7)))


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
       ;(bind (blocked $blocked-coords))
       (setq $new-empty-coords (copy-list $empty-coords))
       (setq $new-tile-coords nil)
       (iter (for tile-coord in $tile-coords)  ;starting empty coords subsequently updated
             (for new-tile-coord = (cons (+ (car tile-coord) ?d-row) (+ (cdr tile-coord) ?d-col)))
             (when (or (not (in-bounds new-tile-coord)))
                       ;(member new-tile-coord $blocked-coords :test #'equal))  ;added for blocked coords
               (return nil))
             (if (member new-tile-coord $empty-coords :test #'equal)
               (setq $new-empty-coords (delete new-tile-coord $new-empty-coords :test #'equal))
               (when (not (member new-tile-coord $tile-coords :test #'equal))
                 (return nil)))  ;blocked by some other tile
             (push new-tile-coord $new-tile-coords)
             (for opposite-coord = (cons (- (car tile-coord) ?d-row) (- (cdr tile-coord) ?d-col)))
             (when (not (member opposite-coord $tile-coords :test #'equal))
               (push tile-coord $new-empty-coords))
          (finally (return t))))
  (?tile $direction)
  (do (setq $direction (cond ((= ?d-col 1) 'right)
                             ((= ?d-row 1) 'down)
                             ((= ?d-col -1) 'left)
                             ((= ?d-row -1) 'up)
                             (t (error "Incorrect direction"))))
      (assert (loc ?tile (sort-coords $new-tile-coords))
              (empty (sort-coords $new-empty-coords)))))
      

#|
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
|#      

(define-init
  (loc SQ ((3 . 2)))  ;initial locations of all parts of a tile
  (loc HOR ((1 . 2) (1 . 3)))
  (loc VER ((2 . 3) (3 . 3)))
  (loc L1 ((0 . 1) (1 . 0) (1 . 1)))
  (loc L2 ((2 . 1) (3 . 0) (3 . 1)))
  (empty ((0 . 0) (0 . 2) (0 . 3) (2 . 0) (2 . 2)))
  (Y-goal-coord (0 . 3)))  ;only need the first coord of the goal coords


(define-goal
  (loc L2 ((0 . 3) (1 . 2) (1 . 3))))

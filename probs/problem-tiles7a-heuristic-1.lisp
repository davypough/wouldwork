;;; Filename: problem-tiles7a-heuristic-1.lisp

;;; List problem specification for a blue/yellow tile shuffle in Islands of Insight.
;;; Uses (row . col) coordinates and pre/post move empty-coordinate templates for each tile.
;;; The five identical blue single squares are stored as one sorted list of cells.
;;; The full puzzle brings Y to 0,0 through two milestones, solved as a chain
;;; (each stage starts where the previous one ended; set the cutoff per stage):
;;;   (stage tiles7a-heuristic-1)
;;;   (solve)                                ;Y to 3,3: 22 moves
;;;   (ww-set *depth-cutoff* 48)
;;;   (solve-subgoal (loc Y 2 2))            ;48 moves, about 12 s
;;;   (ww-set *depth-cutoff* 36)
;;;   (solve-subgoal (loc Y 0 0))            ;36 moves, about 5 s
;;; The goal and every subgoal must have the form (loc Y row col):
;;; min-steps-remaining? reads its target cell from the installed goal.

(in-package :ww)  ;required

(ww-set *problem-name* tiles7a-heuristic-1)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 22)


(define-types
  tile   (DELL LELL 2HOR 3HOR CANE LKEY RKEY UTEE RTEE DTEE LTEE Y)
  direction (right left down up)
  row (0 1 2 3 4 5 6 7)
  col (0 1 2 3 4 5 6 7))


(define-dynamic-relations
  (loc tile $row $col)  ;ref location of a tile with row col coordinates
  (squares $list)  ;sorted coords of the five identical blue single squares
  (emptys $list))  ;sorted list of empty coordinates


(define-static-relations
  (pre-post-emptys> tile direction $list $list))  ;pre & post move empty coords


(define-query min-steps-remaining? ()
  ;Manhattan distance from the Y tile to the cell named by the installed goal
  ;(loc Y row col); never overestimates, since a move shifts Y by at most one cell.
  (do (bind (loc Y $Y-row $Y-col))
      (+ (abs (- $Y-row (third *goal*)))
         (abs (- $Y-col (fourth *goal*))))))


(define-query movable (?real-pre-emptys)
  ;Every pre-coord is empty.
  (do (bind (emptys $emptys))
      (every (lambda (coord)
               (member coord $emptys :test #'equal))
             ?real-pre-emptys)))


(define-query get-squares ()
  (do (bind (squares $squares))
      $squares))


(defun sort-coords (coords)
  ;Keeps coordinates lexicographically sorted. Sorts a copy: callers pass lists
  ;from REMOVE, whose tails are shared with the parent state's database values.
  (sort (copy-list coords) (lambda (a b)
                             (or (< (car a) (car b))
                                 (and (= (car a) (car b))
                                      (< (cdr a) (cdr b)))))))


(defun real-coords (row col coords)
  ;Translates relative coords to real coords.
  (mapcar (lambda (coord)
            (cons (+ row (car coord)) (+ col (cdr coord))))
          coords))


(define-action move
  1
  (?tile tile ?direction direction)
  (and (bind (loc ?tile $row $col))
       (bind (pre-post-emptys> ?tile ?direction $pre-emptys $post-emptys))
       (assign $real-pre-emptys (real-coords $row $col $pre-emptys))
       (movable $real-pre-emptys))
  (?tile ?direction)
  (assert (bind (emptys $emptys))
          (assign $real-post-emptys (real-coords $row $col $post-emptys))
          (case ?direction
            (right (loc ?tile $row (1+ $col)))
            (left (loc ?tile $row (1- $col)))
            (down (loc ?tile (1+ $row) $col))
            (up (loc ?tile (1- $row) $col)))
          (emptys (sort-coords (append (set-difference $emptys $real-pre-emptys :test #'equal)
                                       $real-post-emptys)))))


(define-action move-square
  1
  (?square (get-squares) ?direction direction)
  (and (assign $target (case ?direction
                         (right (cons (car ?square) (1+ (cdr ?square))))
                         (left (cons (car ?square) (1- (cdr ?square))))
                         (down (cons (1+ (car ?square)) (cdr ?square)))
                         (up (cons (1- (car ?square)) (cdr ?square)))))
       (movable (list $target)))
  (?square ?direction)
  (assert (bind (emptys $emptys))
          (bind (squares $squares))
          (squares (sort-coords (cons $target (remove ?square $squares :test #'equal))))
          (emptys (sort-coords (cons ?square (remove $target $emptys :test #'equal))))))


(define-init
  (loc Y 6 7)  ;uppermost leftmost reference coord for a tile
  (loc 2HOR 0 3)
  (loc 3HOR 7 5)
  (loc CANE 1 1)
  (loc LELL 4 6)
  (loc DELL 0 0)
  (loc RTEE 2 5)
  (loc LTEE 3 2)
  (loc DTEE 5 3)
  (loc UTEE 1 3)
  (loc LKEY 0 6)
  (loc RKEY 5 0)

  (pre-post-emptys> Y right ((0 . 1)) ((0 . 0)))  ;relative pre-move-emptys post-move-emptys
  (pre-post-emptys> Y left ((0 . -1)) ((0 . 0)))
  (pre-post-emptys> Y down ((1 . 0)) ((0 . 0)))
  (pre-post-emptys> Y up ((-1 . 0)) ((0 . 0)))






  (pre-post-emptys> 2HOR right ((0 . 2)) ((0 . 0)))
  (pre-post-emptys> 2HOR left ((0 . -1)) ((0 . 1)))
  (pre-post-emptys> 2HOR down ((1 . 0) (1 . 1)) ((0 . 0) (0 . 1)))
  (pre-post-emptys> 2HOR up ((-1 . 0) (-1 . 1)) ((0 . 0) (0 . 1)))

  (pre-post-emptys> 3HOR right ((0 . 3)) ((0 . 0)))
  (pre-post-emptys> 3HOR left ((0 . -1)) ((0 . 2)))
  (pre-post-emptys> 3HOR down ((1 . 0) (1 . 1) (1 . 2)) ((0 . 0) (0 . 1) (0 . 2)))
  (pre-post-emptys> 3HOR up ((-1 . 0) (-1 . 1) (-1 . 2)) ((0 . 0) (0 . 1) (0 . 2)))

  (pre-post-emptys> CANE right ((0 . 2) (1 . 1) (2 . 1)) ((0 . 0) (1 . 0) (2 . 0)))
  (pre-post-emptys> CANE left  ((0 . -1) (1 . -1) (2 . -1)) ((0 . 1) (1 . 0) (2 . 0)))
  (pre-post-emptys> CANE down  ((1 . 1) (3 . 0)) ((0 . 0) (0 . 1)))
  (pre-post-emptys> CANE up    ((-1 . 0) (-1 . 1)) ((0 . 1) (2 . 0)))

  (pre-post-emptys> LELL right ((0 . 1) (1 . 1) (2 . 1)) ((0 . 0) (1 . 0) (2 . -1)))
  (pre-post-emptys> LELL left  ((0 . -1) (1 . -1) (2 . -2)) ((0 . 0) (1 . 0) (2 . 0)))
  (pre-post-emptys> LELL down  ((3 . -1) (3 . 0)) ((0 . 0) (2 . -1)))
  (pre-post-emptys> LELL up    ((-1 . 0) (1 . -1)) ((2 . -1) (2 . 0)))

  (pre-post-emptys> DELL right ((0 . 3) (1 . 1)) ((0 . 0) (1 . 0)))
  (pre-post-emptys> DELL left  ((0 . -1) (1 . -1)) ((0 . 2) (1 . 0)))
  (pre-post-emptys> DELL down  ((1 . 1) (1 . 2) (2 . 0)) ((0 . 0) (0 . 1) (0 . 2)))
  (pre-post-emptys> DELL up    ((-1 . 0) (-1 . 1) (-1 . 2)) ((0 . 1) (0 . 2) (1 . 0)))

  (pre-post-emptys> DTEE right ((0 . 3) (1 . 2)) ((0 . 0) (1 . 1)))
  (pre-post-emptys> DTEE left  ((0 . -1) (1 . 0)) ((0 . 2) (1 . 1)))
  (pre-post-emptys> DTEE down  ((1 . 0) (2 . 1) (1 . 2)) ((0 . 0) (0 . 1) (0 . 2)))
  (pre-post-emptys> DTEE up    ((-1 . 0) (-1 . 1) (-1 . 2)) ((0 . 0) (1 . 1) (0 . 2)))

  (pre-post-emptys> UTEE right ((0 . 1) (1 . 2)) ((0 . 0) (1 . -1)))
  (pre-post-emptys> UTEE left  ((0 . -1) (1 . -2)) ((0 . 0) (1 . 1)))
  (pre-post-emptys> UTEE down  ((2 . -1) (2 . 0) (2 . 1)) ((0 . 0) (1 . -1) (1 . 1)))
  (pre-post-emptys> UTEE up    ((-1 . 0) (0 . -1) (0 . 1)) ((1 . -1) (1 . 0) (1 . 1)))

  (pre-post-emptys> RTEE right ((0 . 1) (1 . 2) (2 . 1)) ((0 . 0) (1 . 0) (2 . 0)))
  (pre-post-emptys> RTEE left  ((0 . -1) (1 . -1) (2 . -1)) ((0 . 0) (1 . 1) (2 . 0)))
  (pre-post-emptys> RTEE down  ((2 . 1) (3 . 0)) ((0 . 0) (1 . 1)))
  (pre-post-emptys> RTEE up    ((-1 . 0) (0 . 1)) ((1 . 1) (2 . 0)))

  (pre-post-emptys> LTEE right ((0 . 1) (1 . 1) (2 . 1)) ((0 . 0) (1 . -1) (2 . 0)))
  (pre-post-emptys> LTEE left  ((0 . -1) (1 . -2) (2 . -1)) ((0 . 0) (1 . 0) (2 . 0)))
  (pre-post-emptys> LTEE down  ((2 . -1) (3 . 0)) ((0 . 0) (1 . -1)))
  (pre-post-emptys> LTEE up    ((-1 . 0) (0 . -1)) ((1 . -1) (2 . 0)))

  (pre-post-emptys> LKEY right ((0 . 2) (1 . 2) (2 . 2)) ((0 . 0) (1 . -2) (2 . 0)))
  (pre-post-emptys> LKEY left  ((0 . -1) (1 . -3) (2 . -1)) ((0 . 1) (1 . 1) (2 . 1)))
  (pre-post-emptys> LKEY down  ((2 . -2) (2 . -1) (3 . 0) (3 . 1)) ((0 . 0) (0 . 1) (1 . -2) (1 . -1)))
  (pre-post-emptys> LKEY up    ((-1 . 0) (-1 . 1) (0 . -2) (0 . -1)) ((1 . -2) (1 . -1) (2 . 0) (2 . 1)))

  (pre-post-emptys> RKEY right ((0 . 2) (1 . 4) (2 . 2)) ((0 . 0) (1 . 0) (2 . 0)))
  (pre-post-emptys> RKEY left  ((0 . -1) (1 . -1) (2 . -1)) ((0 . 1) (1 . 3) (2 . 1)))
  (pre-post-emptys> RKEY down  ((2 . 2) (2 . 3) (3 . 0) (3 . 1)) ((0 . 0) (0 . 1) (1 . 2) (1 . 3)))
  (pre-post-emptys> RKEY up    ((-1 . 0) (-1 . 1) (0 . 2) (0 . 3)) ((1 . 2) (1 . 3) (2 . 0) (2 . 1)))

  (squares ((0 . 5) (3 . 3) (3 . 4) (4 . 3) (4 . 4)))
  (emptys ((2 . 0) (3 . 0) (3 . 7) (4 . 0) (4 . 7) (5 . 7) (7 . 2) (7 . 3) (7 . 4))))


(define-goal  ;first milestone; see the header for the full chain
  (loc Y 3 3))

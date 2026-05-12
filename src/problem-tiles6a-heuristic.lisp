;;; Filename: problem-tiles6a-heuristic.lisp


;;; List problem specification for a blue/yellow tile shuffle in Islands of Insight.
;;; Includes blocked coordinates

(in-package :ww)  ;required

(ww-set *problem-name* tiles6a-heuristic)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 10000)

(ww-set *progress-reporting-interval* 100000000)


(define-types
  tile   (GUN CANE VER1 VER2 UP L Y)  ;Y is yellow
  delta-row (0 1 0 -1)  ;for moving right,down,left,up
  delta-col (1 0 -1 0))


(define-dynamic-relations
  (loc tile $list)  ;location of a tile with coordinates
  (empty $list))


(define-static-relations
  (blocked $list)
  (Y-goal-coord $cons))  ;the ref coord of the goal location


;(define-query heuristic? ()
;  ;Get the manhattan distance from first coord of Y tile to the goal coord. Lower is better.
;  (do (bind (loc Y $Y-coords))
;      (bind (Y-goal-coord $Y-goal-coord))
;      (+ (abs (- (caar $Y-coords) (car $Y-goal-coord)))  ;compare first coord
;         (abs (- (cdar $Y-coords) (cdr $Y-goal-coord))))))


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
       (bind (blocked $blocked-coords))
       (assign $new-empty-coords (copy-list $empty-coords))
       (iter (for tile-coord in $tile-coords)  ;starting empty coords subsequently updated
             (for new-tile-coord = (cons (+ (car tile-coord) ?d-row) (+ (cdr tile-coord) ?d-col)))
             (when (or (not (in-bounds new-tile-coord))
                       (member new-tile-coord $blocked-coords :test #'equal))  ;added for blocked coords
               (return nil))
             (if (member new-tile-coord $empty-coords :test #'equal)
               (assign $new-empty-coords (delete new-tile-coord $new-empty-coords :test #'equal))
               (when (not (member new-tile-coord $tile-coords :test #'equal))
                 (return nil)))  ;blocked by some other tile
             (push new-tile-coord $new-tile-coords)
             (for opposite-coord = (cons (- (car tile-coord) ?d-row) (- (cdr tile-coord) ?d-col)))
             (when (not (member opposite-coord $tile-coords :test #'equal))
               (push tile-coord $new-empty-coords))
          (finally (return t))))
  (?tile $direction)
  (do (assign $direction (cond ((= ?d-col 1) 'right)
                             ((= ?d-row 1) 'down)
                             ((= ?d-col -1) 'left)
                             ((= ?d-row -1) 'up)
                             (t (error "Incorrect direction"))))
      (assert ;(unless (and (alexandria:setp $new-tile-coords :test #'equal)
              ;             (alexandria:setp $new-empty-coords :test #'equal)
              ;             (alexandria:length= 19 $new-empty-coords))
              ;  (error "not setp or length of 19"))
              (loc ?tile (sort-coords $new-tile-coords))
              (empty (sort-coords $new-empty-coords)))))
      

(define-init
  (loc GUN ((0 . 0) (0 . 1) (0 . 2) (1 . 0) (1 . 1) (2 . 0)))  ;initial locations of all parts of a tile
  (loc CANE ((1 . 2) (1 . 3) (2 . 2) (3 . 2)))
  (loc VER1 ((3 . 0) (4 . 0) (5 . 0)))
  (loc VER2 ((2 . 1) (3 . 1) (4 . 1)))
  (loc UP ((5 . 1) (6 . 0) (6 . 1) (6 . 2) (6 . 3)))
  (loc L ((4 . 2) (5 . 2) (5 . 3)))
  (loc Y  ((6 . 6) (6 . 7)))
  (empty ((0 . 3) (0 . 4) (0 . 5) (1 . 4) (1 . 5) (1 . 6) (1 . 7) (2 . 3) (2 . 7)
          (3 . 3) (3 . 7) (4 . 3) (4 . 7) (5 . 4) (5 . 5) (5 . 6) (5 . 7) (6 . 4) (6 . 5)))
  (blocked ((2 . 4) (2 . 5) (2 . 6) (3 . 4) (3 . 5) (3 . 6) (4 . 4) (4 . 5) (4 . 6)))
  (Y-goal-coord (0 . 6)))  ;only need the first coord of the goal coords


(define-goal
  (loc Y ((0 . 6) (0 . 7))))

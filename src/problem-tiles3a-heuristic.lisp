;;; Filename: problem-tiles3a-heuristic.lisp

;;; List problem specification for a blue/yellow tile shuffle in Islands of Insight.


(in-package :ww)  ;required

(ww-set *problem-name* tiles3a-heuristic)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 60)

(ww-set *progress-reporting-interval* 1000000)


(define-types
  tile   (UP LO SQ1 SQ2 SQ3 YSQ SQ4 SQ5)  ;YSQ is yellow
  delta-row (0 1 0 -1)  ;for moving right,down,left,up
  delta-col (1 0 -1 0))


(define-dynamic-relations
  (loc tile $list)  ;location of a tile with coordinates
  (empty $list))


(define-query heuristic? ()
  ;Get the closest minimum manhattan distance from H1 to the goal.
  (do (bind (loc YSQ $YSQ-coords))
      (setf $goal-coords (third *goal*))  ;eg, for a goal like (loc YSQ ((0 . 14)))
      (+ (abs (- (caar $YSQ-coords) (caar $goal-coords)))
         (abs (- (cdar $YSQ-coords) (cdar $goal-coords))))))


(defun merge-into-list (dotted-pair list)
  "Lexicographically and destructively merges a dotted pair into a list of dotted pairs."
  (merge 'list (list dotted-pair) list
         (lambda (a b)
           (or (< (car a) (car b))
               (and (= (car a) (car b))
                    (<= (cdr a) (cdr b)))))))


(define-action move
  1
  (standard ?tile tile (dot-product ?d-row delta-row ?d-col delta-col))
  (and (bind (loc ?tile $tile-coords))
       (bind (empty $empty-coords))
       (assign $new-empty-coords (copy-list $empty-coords))
       (iter (for tile-coord in $tile-coords)  ;starting empty coords subsequently updated
             (for new-tile-coord = (cons (+ (car tile-coord) ?d-row) (+ (cdr tile-coord) ?d-col)))
             (if (member new-tile-coord $empty-coords :test #'equal)
               (setf $new-empty-coords (remove new-tile-coord $new-empty-coords :test #'equal))  
               (unless (member new-tile-coord $tile-coords :test #'equal)  ;tile coord can move into spot vacated by other tile coord
                 (return nil)))  ;can't make this move
             (for opposite-coord = (cons (- (car tile-coord) ?d-row) (- (cdr tile-coord) ?d-col)))
             (when (or (member opposite-coord $empty-coords :test #'equal)  ;check before next
                       (not (member opposite-coord $tile-coords :test #'equal)))
               (setf $new-empty-coords (merge-into-list tile-coord $new-empty-coords))) ;move leaves an empty space behind
             (finally (setf $new-tile-coords (iter (for tile-coord in $tile-coords)  ;perform actual tile move
                                                   (collect (cons (+ (car tile-coord) ?d-row)
                                                                  (+ (cdr tile-coord) ?d-col)))))
                      (return t))))
  (?tile $direction)
  (do (assign $direction (cond ((= ?d-col 1) 'right)
                             ((= ?d-row 1) 'down)
                             ((= ?d-col -1) 'left)
                             ((= ?d-row -1) 'up)
                             (t (error "Incorrect direction"))))
      (assert (loc ?tile $new-tile-coords)
              (empty $new-empty-coords))))
      
       
(define-init
  (loc UP ((0 . 0) (0 . 1) (0 . 2) (0 . 3) (0 . 4) (0 . 5)(0 . 6) (0 . 7) (0 . 8) (0 . 9) (0 . 10)
           (1 . 0) (1 . 1) (1 . 3) (1 . 4) (1 . 5) (1 . 6) (1 . 7) (1 . 8) (1 . 9) (1 . 10)
           (2 . 0) (2 . 1) (2 . 3) (2 . 4) (2 . 5) (2 . 6) (2 . 7) (2 . 10)))
  (loc LO ((3 . 2) (3 . 3) (3 . 4) (3 . 6) (3 . 9) (3 . 10) (3 . 12) (3 . 13) (3 . 14)
           (4 . 2) (4 . 3) (4 . 4) (4 . 6) (4 . 7) (4 . 8) (4 . 9) (4 . 10) (4 . 11) (4 . 12) (4 . 13) (4 . 14)
           (5 . 2) (5 . 3) (5 . 4) (5 . 5) (5 . 6) (5 . 7) (5 . 8) (5 . 9) (5 . 10) (5 . 11) (5 . 12) (5 . 13) (5 . 14)))
  (loc YSQ ((3 . 5)))
  (loc SQ1 ((4 . 5)))
  (empty ((0 . 11) (0 . 12) (0 . 13) (0 . 14)
          (1 . 2) (1 . 11) (1 . 12) (1 . 13) (1 . 14)
          (2 . 2) (2 . 8) (2 . 9) (2 . 11) (2 . 12) (2 . 13) (2 . 14)
          (3 . 0) (3 . 1) (3 . 7) (3 . 8) (3 . 11)
          (4 . 0) (4 . 1)
          (5 . 0) (5 . 1)))
)


(define-goal
  (loc YSQ ((0 . 14))))

;;; Filename: problem-tiles2a-heuristic.lisp


;;; List problem specification for a blue/yellow tile shuffle in Islands of Insight
;;; using heuristic to guide movements.


(in-package :ww)  ;required

(ww-set *problem-name* tiles2a-heuristic)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 50)


(define-types
  tile   (H1 V1 SQ1 S1 H2 SQ2 S2 H3 V2 SQ3 H4 SQ4)  ;H1 is yellow
  delta-row (0 1 0 -1)  ;for moving right,down,left,up
  delta-col (1 0 -1 0))


(define-dynamic-relations
  (loc tile $list)  ;location of a tile with multiple coordinates
  (empty $list))


(define-static-relations
  (goal $list))


(define-query heuristic? ()
  ;Get the closest minimum manhattan distance from H1 to the goal.
  (do (bind (loc H1 $H1-coords))
      (bind (goal $goal-coords))
      (iter (for (row1 . col1) in $H1-coords)
            (minimizing (iter (for (row2 . col2) in $goal-coords)
                              (minimizing (+ (abs (- row1 row2)) (abs (- col1 col2)))))))))


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
  (loc H1 ((0 . 1) (0 . 2)))  ;initial locations of all parts of a tile
  (loc V1 ((0 . 3) (1 . 3)))
  (loc SQ1 ((0 . 4)))
  (loc S1 ((0 . 5) (0 . 6) (1 . 4) (1 . 5)))
  (loc H2 ((1 . 0) (1 . 1)))
  (loc SQ2 ((1 . 2)))
  (loc S2 ((2 . 1) (2 . 2) (3 . 0) (3 . 1)))
  (loc H3 ((2 . 3) (2 . 4)))
  (loc V2 ((2 . 5) (3 . 5)))
  (loc SQ3 ((2 . 6)))
  (loc H4 ((3 . 2) (3 . 3)))
  (loc SQ4 ((3 . 4)))
  (empty ((0 . 0) (1 . 6) (2 . 0) (3 . 6)))
  (goal ((1 . 2) (1 . 3))))


(define-goal
  (loc H1 ((1 . 2) (1 . 3))))

;;; Filename: problem-tiles1a.lisp


;;; List problem specification for a blue/yellow tile shuffle in Islands of Insight.


(in-package :ww)  ;required

(ww-set *problem-name* tiles1a)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 40)


(define-types
  tile   (SQ HOR VER L1 L2)  ;L2 is yellow
  delta-row (0 1 0 -1)  ;for moving right,down,left,up
  delta-col (1 0 -1 0))


(define-dynamic-relations
  (loc tile $list)  ;location of a tile with coordinates
  (empty $list))


(defun merge-into-list (dotted-pair list)
  "Lexicographically merges a dotted pair into a list of dotted pairs."
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
       (setf $new-empty-coords (copy-list $empty-coords))
       (iter (for tile-coord in $tile-coords)  ;starting empty coords subsequently updated
             (for new-tile-coord = (cons (+ (car tile-coord) ?d-row) (+ (cdr tile-coord) ?d-col)))
             (if (member new-tile-coord $empty-coords :test #'equal)
               (setf $new-empty-coords (delete new-tile-coord $new-empty-coords :test #'equal))
               (unless (member new-tile-coord $tile-coords :test #'equal)  ;tile coord can move into spot vacated by other tile coord
                 (return nil)))  ;can't make this move
             (for opposite-coord = (cons (- (car tile-coord) ?d-row) (- (cdr tile-coord) ?d-col)))
             (when (or (member opposite-coord $empty-coords :test #'equal)  ;check before next
                       (not (member opposite-coord $tile-coords :test #'equal)))
               (setf $new-empty-coords (merge-into-list (copy-list tile-coord) (copy-list $new-empty-coords)))) ;move leaves an empty space behind
             (finally (setf $new-tile-coords (iter (for tile-coord in $tile-coords)  ;perform actual tile move
                                                   (collect (cons (+ (car tile-coord) ?d-row)
                                                                  (+ (cdr tile-coord) ?d-col)))))
                      (return t))))
  (?tile $direction)
  (do (setf $direction (cond ((= ?d-col 1) 'right)
                             ((= ?d-row 1) 'down)
                             ((= ?d-col -1) 'left)
                             ((= ?d-row -1) 'up)
                             (t (error "Incorrect direction"))))
      (assert (loc ?tile $new-tile-coords)
              (empty $new-empty-coords))))
      

(define-init
  (loc SQ ((3 . 2)))  ;initial locations of all parts of a tile
  (loc HOR ((1 . 2) (1 . 3)))
  (loc VER ((2 . 3) (3 . 3)))
  (loc L1 ((0 . 1) (1 . 0) (1 . 1)))
  (loc L2 ((2 . 1) (3 . 0) (3 . 1)))
  (empty ((0 . 0) (0 . 2) (0 . 3) (2 . 0) (2 . 2))))


(define-goal
  (loc L2 ((0 . 3) (1 . 2) (1 . 3))))

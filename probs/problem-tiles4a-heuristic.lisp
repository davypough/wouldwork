;;; Filename: problem-tiles4a-heuristic.lisp

;;; List problem specification for a blue/yellow tile shuffle in Islands of Insight.


(in-package :ww)  ;required

(ww-set *problem-name* tiles4a-heuristic)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 60)

(ww-set *progress-reporting-interval* 1000000)


(defparameter *goal-coords* '((2 . 3) (2 . 4) (2 . 5) (3 . 3) (3 . 5)))


(define-types
  tile   (ARCH HORIZ1 HORIZ2 SQ R L)
  delta-row (0 1 0 -1)  ;for moving right,down,left,up
  delta-col (1 0 -1 0)
)


(define-dynamic-relations
  (loc tile $list)  ;location of a tile with coordinates
  (empty $list)
)


(define-query heuristic? ()
  ;Get the closest minimum manhattan distance from ARCH to the goal (upper left coord only).
  (do (bind (loc ARCH $ARCH-coords))
      ;(setf $goal-coords '((2 . 3) (2 . 4) (2 . 5) (3 . 3) (3 . 5)))  ;original goal
      ;(setf $goal-coords '((1 . 3) (1 . 4) (1 . 5) (2 . 3) (2 . 5)))  ;penultimate goal (unreachable)
      (+ (abs (- (caar $ARCH-coords) (caar *goal-coords*)))
         (abs (- (cdar $ARCH-coords) (cdar *goal-coords*)))))
)


(defun merge-into-list (dotted-pair list)
  "Lexicographically and destructively merges a dotted pair into a list of dotted pairs."
  (merge 'list (list dotted-pair) list
         (lambda (a b)
           (or (< (car a) (car b))
               (and (= (car a) (car b))
                    (<= (cdr a) (cdr b))))))
)


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
              (empty $new-empty-coords)))
)
      
       
(define-init
  (loc ARCH ((0 . 0) (0 . 1) (0 . 2)
             (1 . 0) (1 . 2)))
  (loc HORIZ1 ((0 . 3) (0 . 4) (0 . 5)))
  (loc HORIZ2 ((1 . 3) (1 . 4) (1 . 5)))
  (loc SQ ((1 . 1)))
  (loc R ((2 . 0) (2 . 1)
          (3 . 0)))
  (loc L ((2 . 2)
          (3 . 1) (3 . 2)))
  (empty ((2 . 3) (2 . 4) (2 . 5)
          (3 . 3) (3 . 4) (3 . 5)))
)


(define-goal
  `(loc ARCH ,*goal-coords*)
  ;(loc ARCH ((2 . 3) (2 . 4) (2 . 5)  ;original goal
  ;           (3 . 3) (3 . 5)))
  ;(and (loc ARCH ((1 . 3) (1 . 4) (1 . 5)  ;penultimate goal (unreachable)
  ;                (2 . 3) (2 . 5)))
  ;     (bind (empty $emptys))
  ;     (subsetp '((2 . 4)
  ;                (3 . 3) (3 . 5)) $emptys))
)
;;; Filename: problem-tiles1c.lisp


;;; Problem specification for a blue/yellow tile shuffle in Islands of Insight.
;;; Uses vector representation for coordinates. (slower than list)


(in-package :ww)  ;required

(ww-set *problem-name* tiles1c)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 40)


(define-types
  tile   (SQ HOR VER L1 L2)  ;L2 is yellow
  delta-row (0 1 0 -1)  ;for moving right,down,left,up
  delta-col (1 0 -1 0))


(define-dynamic-relations
  (loc tile $simple-vector)  ;location of a tile with coordinates
  (empty $simple-vector))


(defun merge-into-vector (pair pairs)
  "Lexicographically merges a vector pair into a vector of vector pairs."
  (merge 'vector (vector pair) pairs
         (lambda (a b)
           (or (< (svref a 0) (svref b 0))
               (and (= (svref a 0) (svref b 0))
                    (<= (svref a 1) (svref b 1)))))))


(define-action move
  1
  (standard ?tile tile (dot-product ?d-row delta-row ?d-col delta-col))
  (and (bind (loc ?tile $tile-coords))
       (bind (empty $empty-coords))
       (setf $new-empty-coords $empty-coords)
       (iter (for tile-coord in-vector $tile-coords)  ;starting empty coords subsequently updated
             (for new-tile-coord = (vector (+ (svref tile-coord 0) ?d-row) (+ (svref tile-coord 1) ?d-col)))
             (if (find new-tile-coord $empty-coords :test #'equalp)
               (setf $new-empty-coords (remove new-tile-coord $new-empty-coords :test #'equalp))  
               (unless (find new-tile-coord $tile-coords :test #'equalp)  ;tile coord can move into spot vacated by other tile coord
                 (return nil)))  ;can't make this move
             (for opposite-coord = (vector (- (svref tile-coord 0) ?d-row) (- (svref tile-coord 1) ?d-col)))
             (when (or (find opposite-coord $empty-coords :test #'equalp)  ;check before next
                       (not (find opposite-coord $tile-coords :test #'equalp)))
               (setf $new-empty-coords (merge-into-vector (copy-seq tile-coord) (copy-seq $new-empty-coords)))) ;move leaves an empty space behind
             (finally (setf $new-tile-coords (iter (for tile-coord in-vector $tile-coords)  ;perform actual tile move
                                                   (collect (vector (+ (svref tile-coord 0) ?d-row)
                                                                    (+ (svref tile-coord 1) ?d-col))
                                                            result-type 'simple-vector)))
                      (return t))))
  (?tile ?d-row ?d-col)  ; $direction fluent)
  (do ;(setf $direction (cond ((= ?d-col 1) 'right)
      ;                       ((= ?d-row 1) 'down)
      ;                       ((= ?d-col -1) 'left)
      ;                       ((= ?d-row -1) 'up)
      ;                       (t (error "Incorrect direction"))))
      (assert (loc ?tile $new-tile-coords)
              (empty $new-empty-coords))))


(define-init
  `(loc SQ ,(vectorize '((3 2))))  ;initial locations of all parts of a tile
  `(loc HOR ,(vectorize '((1 2) (1 3))))
  `(loc VER ,(vectorize '((2 3) (3 3))))
  `(loc L1 ,(vectorize '((0 1) (1 0) (1 1))))
  `(loc L2 ,(vectorize '((2 1) (3 0) (3 1))))
  `(empty ,(vectorize '((0 0) (0 2) (0 3) (2 0) (2 2)))))


(define-goal
  `(loc L2 ,(vectorize '((0 3) (1 2) (1 3)))))

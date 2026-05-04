;;; Filename: problem-tiles2b.lisp


;;; Vector problem specification for a blue/yellow tile shuffle in Islands of Insight.


(in-package :ww)  ;required

(ww-set *problem-name* tiles2b)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 26)


(define-types
  tile   (H1 V1 SQ1 S1 H2 SQ2 S2 H3 V2 SQ3 H4 SQ4)  ;H1 is yellow
  delta-row (0 1 0 -1)  ;for moving right,down,left,up
  delta-col (1 0 -1 0))


(define-dynamic-relations
  (loc tile $simple-vector)  ;location of a tile with coordinates
  (empty $simple-vector))


(defun merge-into-vector (pair pairs)
  "Lexicographically merges a pair into a vector of pairs."
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
  `(loc H1 ,(vectorize '((0 1) (0 2))))  ;initial locations of all parts of a tile
  `(loc V1 ,(vectorize '((0 3) (1 3))))
  `(loc SQ1 ,(vectorize '((0 4))))
  `(loc S1 ,(vectorize '((0 5) (0 6) (1 4) (1 5))))
  `(loc H2 ,(vectorize '((1 0) (1 1))))
  `(loc SQ2 ,(vectorize '((1 2))))
  `(loc S2 ,(vectorize '((2 1) (2 2) (3 0) (3 1))))
  `(loc H3 ,(vectorize '((2 3) (2 4))))
  `(loc V2 ,(vectorize '((2 5) (3 5))))
  `(loc SQ3 ,(vectorize '((2 6))))
  `(loc H4 ,(vectorize '((3 2) (3 3))))
  `(loc SQ4 ,(vectorize '((3 4))))
  `(empty ,(vectorize '((0 0) (1 6) (2 0) (3 6)))))


(define-goal
  `(loc H1 ,(vectorize '((1 2) (1 3)))))

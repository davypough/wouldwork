;;; Filename: problem-tiles2a.lisp


;;; Hash-table problem specification for a blue/yellow tile shuffle in Islands of Insight.


(in-package :ww)  ;required

(ww-set *problem-name* tiles2a)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 26)


(define-types
  tile   (H1 V1 SQ1 S1 H2 SQ2 S2 H3 V2 SQ3 H4 SQ4)  ;H1 is yellow
  delta-row (0 1 0 -1)  ;for moving right,down,left,up
  delta-col (1 0 -1 0))


(define-dynamic-relations
  (loc tile $hash-table)  ;note $coordinates represented as a hash-table set with keys (row col)
  (empty $hash-table))  ;note $coordinates represented as a hash-table set


(define-action move
  1
  (standard ?tile tile (dot-product ?d-row delta-row ?d-col delta-col))
  (and (bind (loc ?tile $tile-coords))  ;list of (row col) coordinates of a tile
       (bind (empty $empty-coords))
       (setf $new-empty-coords (copy-ht-set $empty-coords))
       (iter (for (tile-coord nil) in-hashtable $tile-coords)
             (for new-tile-coord = (cons (+ (car tile-coord) ?d-row)
                                         (+ (cdr tile-coord) ?d-col)))
             (if (gethash new-tile-coord $empty-coords)
               (remhash new-tile-coord $new-empty-coords)
               (unless (gethash new-tile-coord $tile-coords)
                 (return nil)))  ;can't make this move
             (for opposite-coord = (cons (- (car tile-coord) ?d-row)
                                         (- (cdr tile-coord) ?d-col)))
             (when (or (gethash opposite-coord $empty-coords)  ;check before next
                       (not (gethash opposite-coord $tile-coords)))
               (setf (gethash (copy-list tile-coord) $new-empty-coords) t)) ;move leaves an empty space behind
             (finally (setf $new-tile-coords (make-ht-set :test #'equal :size 4))
                      (iter (for (tile-coord nil) in-hashtable $tile-coords)
                            (setf (gethash (cons (+ (car tile-coord) ?d-row)
                                                 (+ (cdr tile-coord) ?d-col))
                                           $new-tile-coords)
                                    t))  ;(ut::show $tile-coords) (ut::show $new-tile-coords)
                      (return t))))
  (?tile ?d-row ?d-col)
  (do ;(setf $direction (cond ((and (= ?d-row 0) (= ?d-col 1)) 'right)
      ;                       ((and (= ?d-row 1) (= ?d-col 0)) 'down)
      ;                       ((and (= ?d-row 0) (= ?d-col -1)) 'left)
      ;                       ((and (= ?d-row -1) (= ?d-col 0)) 'up)
      ;                       (t (error "Incorrect direction"))))
      (assert (loc ?tile $new-tile-coords)
              (empty $new-empty-coords))))
      

(define-init
  (loc H1 #.(make-ht-set :test #'equal :size 2 :initial-contents '((0 . 1) (0 . 2))))  ;initial locations of all parts of a tile
  (loc V1 #.(make-ht-set :test #'equal :size 2 :initial-contents '((0 . 3) (1 . 3))))
  (loc SQ1 #.(make-ht-set :test #'equal :size 1 :initial-contents '((0 . 4))))
  (loc S1 #.(make-ht-set :test #'equal :size 4 :initial-contents '((0 . 5) (0 . 6) (1 . 4) (1 . 5))))
  (loc H2 #.(make-ht-set :test #'equal :size 2 :initial-contents '((1 . 0) (1 . 1))))
  (loc SQ2 #.(make-ht-set :test #'equal :size 1 :initial-contents '((1 . 2))))  ;initial locations of all parts of a tile
  (loc S2 #.(make-ht-set :test #'equal :size 4 :initial-contents '((2 . 1) (2 . 2) (3 . 0) (3 . 1))))
  (loc H3 #.(make-ht-set :test #'equal :size 2 :initial-contents '((2 . 3) (2 . 4))))
  (loc V2 #.(make-ht-set :test #'equal :size 2 :initial-contents '((2 . 5) (3 . 5))))
  (loc SQ3 #.(make-ht-set :test #'equal :size 1 :initial-contents '((2 . 6))))
  (loc H4 #.(make-ht-set :test #'equal :size 2 :initial-contents '((3 . 2) (3 . 3))))  ;initial locations of all parts of a tile
  (loc SQ4 #.(make-ht-set :test #'equal :size 1 :initial-contents '((3 . 4))))
  (empty #.(make-ht-set :test #'equal :size 4 :initial-contents '((0 . 0) (1 . 6) (2 . 0) (3 . 6)))))


(define-goal
  (loc H1 #.(make-ht-set :test #'equal :size 2 :initial-contents '((1 . 2) (1 . 3)))))

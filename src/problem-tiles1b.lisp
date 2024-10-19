;;; Filename: problem-tiles1b.lisp


;;; Problem specification for a blue/yellow tile shuffle in Islands of Insight.
;;; Hash table for tile & empty sets (slow)

(in-package :ww)  ;required

(ww-set *problem-name* tiles1b)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 40)


(define-types
  tile   (SQ HOR VER L1 L2)  ;L2 is yellow
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
               (setf (gethash tile-coord $new-empty-coords) t)) ;move leaves an empty space behind
             (finally (setf $new-tile-coords (make-ht-set :test #'equal :size 4))
                      (iter (for (tile-coord nil) in-hashtable $tile-coords)
                            (setf (gethash (cons (+ (car tile-coord) ?d-row)
                                                 (+ (cdr tile-coord) ?d-col))
                                           $new-tile-coords)
                                    t))
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
  (loc SQ #.(make-ht-set :test #'equal :size 1 :initial-contents '((3 . 2))))  ;initial locations of all parts of a tile
  (loc HOR #.(make-ht-set :test #'equal :size 2 :initial-contents '((1 . 2) (1 . 3))))
  (loc VER #.(make-ht-set :test #'equal :size 2 :initial-contents '((2 . 3) (3 . 3))))
  (loc L1 #.(make-ht-set :test #'equal :size 3 :initial-contents '((0 . 1) (1 . 0) (1 . 1))))
  (loc L2 #.(make-ht-set :test #'equal :size 3 :initial-contents '((2 . 1) (3 . 0) (3 . 1))))
  (empty #.(make-ht-set :test #'equal :size 5 :initial-contents '((0 . 0) (0 . 2) (0 . 3) (2 . 0) (2 . 2)))))


(define-goal
  (loc L2 #.(make-ht-set :test #'equal :size 2 :initial-contents '((0 . 3) (1 . 2) (1 . 3)))))

;;; Filename: problem-tiles1d.lisp


;;; Bit vector problem specification for a blue/yellow tile shuffle in Islands of Insight.


(in-package :ww)  ;required

(ww-set *problem-name* tiles1d)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 40)


#.(defparameter *row-dimension* 4)  ;the dimensions of the board
#.(defparameter *col-dimension* 4)
(defparameter *all-coordinates* (coerce (iter (for row from 0 below *row-dimension*)  ;coordinates ordered lexicographically
                                              (appending (iter (for col from 0 below *col-dimension*)
                                                               (collecting (cons row col)))))
                                        'simple-vector))


(define-types
  tile   (SQ HOR VER L1 L2)  ;L2 is yellow
  delta-row (0 1 0 -1)  ;for moving right,down,left,up
  delta-col (1 0 -1 0))


(define-dynamic-relations
  (loc tile $simple-bit-vector)  ;location of a tile with coordinates
  (empty $simple-bit-vector))


(defun merge-into-vector (pair pairs)
  "Lexicographically merges a vector pair into a vector of vector pairs."
  (let ((bit-vector-pair (coerce (list pair) 'simple-bit-vector)))
    (merge 'simple-bit-vector bit-vector-pair pairs
           (lambda (a b)
             (or (< (sbit a 0) (sbit b 0))
                 (and (= (sbit a 0) (sbit b 0))
                      (<= (sbit a 1) (sbit b 1))))))))


(defvar make-bv-set nil)
#.(defun make-bv-set (dotted-pairs)
  (let ((bv (make-array (* *row-dimension* *col-dimension*) :element-type 'bit :adjustable nil)))
    (dolist (pair dotted-pairs)
      (let* ((row (car pair))
             (col (cdr pair))
             (index (+ (* row *col-dimension*) col)))
        (setf (sbit bv index) 1)))
    bv))

(define-action move
  1
  (standard ?tile tile (dot-product ?d-row delta-row ?d-col delta-col))
  (and (bind (loc ?tile $tile-coord-bits))
       (bind (empty $empty-coord-bits))
       (setf $new-empty-coord-bits $empty-coord-bits)
       (iter (for tile-coord-bit in-vector $tile-coord-bits with-index i)  ;starting empty coords subsequently updated
             (when (= tile-coord-bit 0) (next-iteration))
             ;(for new-tile-coord = (cons (+ (svref tile-coord 0) ?d-row) (+ (svref tile-coord 1) ?d-col)))
             (if (= (sbit $empty-coord-bits i) 1)  ;coord is in empty
               (setf (sbit $new-empty-coord-bits i) 0)  ; (remove new-tile-coord $new-empty-coord-bits :test #'equalp))  
              ;(unless (find new-tile-coord $tile-coord-bits :test #'equalp)  ;tile coord can move into spot vacated by other tile coord
               (unless (= (sbit $tile-coord-bits i) 1)
                 (return nil)))  ;can't make this move
             ;(for opposite-coord = (vector (- (svref tile-coord 0) ?d-row) (- (svref tile-coord 1) ?d-col)))
             ;(when (or (find opposite-coord $empty-coord-bits :test #'equalp)
             ;          (not (find opposite-coord $tile-coord-bits :test #'equalp)))
             ;  (setf $new-empty-coord-bits (merge-into-vector tile-coord $new-empty-coord-bits))) ;move leaves an empty space behind
             (when (cond ((= ?d-col 1) (or (= (sbit $empty-coord-bits (- i 1)) 1)
                                           (= (sbit $tile-coord-bits (- i 1)) 0)))
                         ((= ?d-row 1) (or (= (sbit $empty-coord-bits (- i 3)) 1)
                                           (= (sbit $tile-coord-bits (- i 3)) 0)))
                         ((= ?d-col -1) (or (= (sbit $empty-coord-bits (+ i 1)) 1)
                                            (= (sbit $tile-coord-bits (+ i 1)) 0)))
                         ((= ?d-row -1) (or (= (sbit $empty-coord-bits (+ i 3)) 1)
                                            (= (sbit $tile-coord-bits (+ i 3)) 0))))
               (setf $new-empty-coord-bits (merge-into-vector tile-coord-bit
                                                              $new-empty-coord-bits))) ;move leaves an empty space behind
             (finally (setf $new-tile-coord-bits (iter (for tile-coord-bit in-vector $tile-coord-bits)  ;perform actual tile move
                                                   (collect (vector (+ (sbit tile-coord-bit 0) ?d-row)
                                                                    (+ (sbit tile-coord-bit 1) ?d-col))
                                                            result-type 'simple-bit-vector)))
                      (return t))))
  (?tile ?d-row ?d-col)  ; $direction fluent)
  (do ;(setf $direction (cond ((= ?d-col 1) 'right)
      ;                       ((= ?d-row 1) 'down)
      ;                       ((= ?d-col -1) 'left)
      ;                       ((= ?d-row -1) 'up)
      ;                       (t (error "Incorrect direction"))))
      (assert (loc ?tile $new-tile-coord-bits)
              (empty $new-empty-coord-bits))))


(define-init
  (loc SQ #.(make-bv-set '((3 . 2))))  ;initial locations of all parts of a tile
  (loc HOR #.(make-bv-set '((1 . 2) (1 . 3))))
  (loc VER #.(make-bv-set '((2 . 3) (3 . 3))))
  (loc L1 #.(make-bv-set '((0 . 1) (1 . 0) (1 . 1))))
  (loc L2 #.(make-bv-set '((2 . 1) (3 . 0) (3 . 1))))
  (empty #.(make-bv-set '((0 . 0) (0 . 2) (0 . 3) (2 . 0) (2 . 2)))))


(define-goal
  (loc L2 #.(make-bv-set '((0 . 3) (1 . 2) (1 . 3)))))

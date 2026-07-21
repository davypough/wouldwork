;;; Filename: problem-tiles1d.lisp


;;; Bit vector problem specification for a blue/yellow tile shuffle in Islands of Insight.
;;; Significantly better simple-bit-vector efficiency over list representation


(in-package :ww)  ;required

(ww-set *problem-name* tiles1d)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 40)


(defparameter *row-dimension* 4)  ;the dimensions of the board
(defparameter *col-dimension* 4)
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


(defun make-bv-set (dotted-pairs)
  (let ((bv (make-array (* *row-dimension* *col-dimension*) :element-type 'bit :adjustable nil)))
    (dolist (pair dotted-pairs)
      (let* ((row (car pair))
             (col (cdr pair))
             (index (+ (* row *col-dimension*) col)))
        (setf (sbit bv index) 1)))
    bv))


(defun coord-shift-index (i delta-row delta-col)
  "Returns the flat bit-vector index resulting from shifting cell I by
   (DELTA-ROW, DELTA-COL), or NIL if the shift goes off-board."
  (let ((row (+ (floor i *col-dimension*) delta-row))
        (col (+ (mod i *col-dimension*) delta-col)))
    (and (<= 0 row) (< row *row-dimension*)
         (<= 0 col) (< col *col-dimension*)
         (+ (* row *col-dimension*) col))))


(define-action move
  1
  (standard ?tile tile (dot-product ?d-row delta-row ?d-col delta-col))
  (and (bind (loc ?tile $tile-coord-bits))
       (bind (empty $empty-coord-bits))
       (assign $new-empty-coord-bits (copy-seq $empty-coord-bits))
       (iter (for tile-coord-bit in-vector $tile-coord-bits with-index i)
             (when (= tile-coord-bit 0) (next-iteration))
             (for dest-i = (coord-shift-index i ?d-row ?d-col))
             (unless dest-i (return nil))
             (if (= (sbit $empty-coord-bits dest-i) 1)
               (setf (sbit $new-empty-coord-bits dest-i) 0)
               (unless (= (sbit $tile-coord-bits dest-i) 1)
                 (return nil)))
             (for opp-i = (coord-shift-index i (- ?d-row) (- ?d-col)))
             (when (or (null opp-i)
                       (= (sbit $empty-coord-bits opp-i) 1)
                       (= (sbit $tile-coord-bits opp-i) 0))
               (setf (sbit $new-empty-coord-bits i) 1))
             (finally (setf $new-tile-coord-bits
                            (let ((bv (make-array (length $tile-coord-bits)
                                                  :element-type 'bit :initial-element 0))
                                  (offset (+ (* ?d-row *col-dimension*) ?d-col)))
                              (iter (for bit in-vector $tile-coord-bits with-index k)
                                    (when (= bit 1)
                                      (setf (sbit bv (+ k offset)) 1)))
                              bv))
                      (return t))))
  (?tile ?d-row ?d-col)
  (do (assert (loc ?tile $new-tile-coord-bits)
              (empty $new-empty-coord-bits))))


(define-init
  `(loc SQ ,(make-bv-set '((3 . 2))))  ;initial locations of all parts of a tile
  `(loc HOR ,(make-bv-set '((1 . 2) (1 . 3))))
  `(loc VER ,(make-bv-set '((2 . 3) (3 . 3))))
  `(loc L1 ,(make-bv-set '((0 . 1) (1 . 0) (1 . 1))))
  `(loc L2 ,(make-bv-set '((2 . 1) (3 . 0) (3 . 1))))
  `(empty ,(make-bv-set '((0 . 0) (0 . 2) (0 . 3) (2 . 0) (2 . 2)))))


(define-goal
  `(loc L2 ,(make-bv-set '((0 . 3) (1 . 2) (1 . 3)))))

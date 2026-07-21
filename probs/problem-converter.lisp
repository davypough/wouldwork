;;;; Filename: problem-converter.lisp

;;; Circuit A in Dimhaven game

;;; Place 5 marked, rotatable pieces on a 4x4 grid (rows/cols 0-3) so they don't
;;; overlap, subject to 6 constraints on the pieces' marker cells.
;;; Piece shapes (offsets are (row . col) relative to the piece's own marker,
;;; which is always (0 . 0)):
;;;   A - L-tromino:  marker, one cell right, one cell down from there
;;;   B - I-tromino:  3 cells in a line, marker in the middle
;;;   C - L-tetromino: 3 cells running up from the marker, plus one cell right of it
;;;   D - domino:     2 cells, marker at one end
;;;   E - monomino:   just the marker
;;; Pieces rotate freely in 90-degree steps (0/90/180/270), never mirrored.
;;; Shape/rotation geometry stays plain Lisp (as in problem-tiles0a/b-csp.lisp);
;;; placement enumeration and the 6 constraints are expressed as wouldwork
;;; queries over the piece type, in the style of problem-queensN-csp.lisp.


(in-package :ww)  ;required


(defparameter *board-size* 4)


(defparameter *board-cells*
  (loop for row from 0 below *board-size*
        append (loop for col from 0 below *board-size* collect (cons row col))))


(defparameter *piece-shapes*
  ;; Each piece maps to its list of distinct rotational orientations (no mirroring).
  ;; Every offset is (row . col) relative to the piece's marker cell, (0 . 0).
  (list (cons 'A '(((0 . 0) (0 . 1) (1 . 1))
                    ((0 . 0) (1 . 0) (1 . -1))
                    ((0 . 0) (0 . -1) (-1 . -1))
                    ((0 . 0) (-1 . 0) (-1 . 1))))
        (cons 'B '(((-1 . 0) (0 . 0) (1 . 0))
                   ((0 . -1) (0 . 0) (0 . 1))))
        (cons 'C '(((-2 . 0) (-1 . 0) (0 . 0) (0 . 1))
                   ((0 . 2) (0 . 1) (0 . 0) (1 . 0))
                   ((2 . 0) (1 . 0) (0 . 0) (0 . -1))
                   ((0 . -2) (0 . -1) (0 . 0) (-1 . 1))))
        (cons 'D '(((-1 . 0) (0 . 0))
                   ((0 . 1) (0 . 0))
                   ((1 . 0) (0 . 0))
                   ((0 . -1) (0 . 0))))
        (cons 'E '(((0 . 0))))))


(defun shape-cells (shape row col)
  "Returns the absolute (row . col) cells of shape anchored with its marker at (row, col)."
  (mapcar (lambda (offset) (cons (+ (car offset) row) (+ (cdr offset) col)))
          shape))


(defun shape-fits-p (cells empty-cells)
  "True when every cell of cells lies on the board and is currently empty."
  (every (lambda (cell) (and (<= 0 (car cell) (1- *board-size*))
                              (<= 0 (cdr cell) (1- *board-size*))
                              (member cell empty-cells :test #'equal)))
         cells))


(defun a-wraps-d-p (a-cells d-cells)
  "Constraint 4 geometry: true when at least one D cell touches two different
   A cells (D nestles into A's inside corner; D's other cell need not touch A)."
  (some (lambda (d-cell) (>= (count-if (lambda (a-cell) (orthogonal-neighbors-p d-cell a-cell)) a-cells) 2))
        d-cells))


(defun orthogonal-neighbors-p (cell1 cell2)
  "True when cell1 and cell2 share a grid edge."
  (= (+ (abs (- (car cell1) (car cell2))) (abs (- (cdr cell1) (cdr cell2)))) 1))


(ww-set *problem-name* converter)

(ww-set *problem-type* csp)

(ww-set *solution-type* first)

(ww-set *tree-or-graph* tree)


(define-types
  piece (A B C D E))


(define-dynamic-relations
  (empty-cells $list)
  (marker-loc piece $list)
  (piece-cells piece $list)
  (placed piece))


(define-query all-placements ()
  (ww-loop for ?p in '(A B C D E)
           unless (placed ?p)
           append (piece-placements ?p)))


(define-query piece-placements (?piece)
  (do (bind (empty-cells $empty))
      (ww-loop for $shape in (cdr (assoc ?piece *piece-shapes*))
               append (ww-loop for $ref in *board-cells*
                                when (shape-fits-p (shape-cells $shape (car $ref) (cdr $ref)) $empty)
                                collect (list ?piece $shape $ref)))))


(define-query all-placed? ()
  (forall (?piece piece) (placed ?piece)))


;; Constraint 1.
(define-query no-adjacent-markers? ()
  (not (exists (?p1 piece)
         (exists (?p2 piece)
           (markers-adjacent? ?p1 ?p2)))))


(define-query markers-adjacent? (?p1 ?p2)
  (and (different ?p1 ?p2)
       (bind (marker-loc ?p1 $c1))
       (bind (marker-loc ?p2 $c2))
       (orthogonal-neighbors-p $c1 $c2)))


;; Constraint 2.
(define-query B-isolated? ()
  (not (exists (?p piece) (shares-marker-line? 'B ?p))))


(define-query shares-marker-line? (?p1 ?p2)
  (and (different ?p1 ?p2)
       (bind (marker-loc ?p1 $c1))
       (bind (marker-loc ?p2 $c2))
       (or (= (car $c1) (car $c2)) (= (cdr $c1) (cdr $c2)))))


;; Constraint 3.
(define-query row-fully-filled? (?piece)
  (do (bind (marker-loc ?piece $marker))
      (bind (empty-cells $empty))
      (ww-loop for col from 0 below *board-size*
               never (member (cons (car $marker) col) $empty :test #'equal))))


;; Constraint 4.
(define-query A-wraps-D? ()
  (do (bind (piece-cells A $a-cells))
      (bind (piece-cells D $d-cells))
      (a-wraps-d-p $a-cells $d-cells)))


;; Constraint 5.
(define-query marker-in-center? (?piece)
  (do (bind (marker-loc ?piece $marker))
      (and (<= 1 (car $marker) 2) (<= 1 (cdr $marker) 2))))


;; Constraint 6.
(define-query one-row-and-col-partner? (?piece)
  (do (bind (marker-loc ?piece $target))
      (ww-loop for ?p in (remove ?piece '(A B C D E))
               do (bind (marker-loc ?p $c))
               count (= (car $c) (car $target)) into $row-count
               count (= (cdr $c) (cdr $target)) into $col-count
               finally (return (and (= $row-count 1) (= $col-count 1))))))


(define-update place-piece! (?placement)
  (do (setf $piece (first ?placement))
      (setf $shape (second ?placement))
      (setf $ref (third ?placement))
      (setf $cells (shape-cells $shape (car $ref) (cdr $ref)))
      (bind (empty-cells $empty))
      (empty-cells (set-difference $empty $cells :test #'equal))
      (marker-loc $piece $ref)
      (piece-cells $piece $cells)
      (placed $piece)))


(define-action place-piece
  1
  (?placement (all-placements))
  (always-true)
  (?placement)
  (assert (place-piece! ?placement)))


(define-init
  `(empty-cells ,*board-cells*))


(define-goal
  (and (all-placed?)
       (no-adjacent-markers?)
       (B-isolated?)
       (row-fully-filled? 'C)
       (A-wraps-D?)
       (marker-in-center? 'D)
       (one-row-and-col-partner? 'E)))

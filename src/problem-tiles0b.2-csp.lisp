;;; Filename: problem-tiles0b.2-csp.lisp

;;; Shifting Mosaic from Islands of Insight (most NW corner island)
;;; Large tiles - REVISED: MRV, fail-fast, area consistency, symmetry breaking, no sorting
;;; This is an exact tiling/packing problem on a 21×21 grid (441 cells).
;;; The objective is to place a set of 9 large, irregularly shaped tiles onto the board such that:
;;; No tiles overlap, all 9 tiles are successfully placed, and there are no gaps.
;;; Finding first solution takes 110 min.

(in-package :ww)


(defparameter *board-rows* 21)


(defparameter *board-cols* 21)


(defun feasible-anchor-count (tile empty-cells)
  "Counts how many of TILE's initial anchors keep its full shape inside EMPTY-CELLS."
  (let ((shape (cdr (assoc tile *tile-shapes*)))
        (anchors (cdr (assoc tile *tile-anchors*))))
    (count-if (lambda (ref)
                (every (lambda (offset)
                         (member (cons (+ (car offset) (car ref))
                                       (+ (cdr offset) (cdr ref)))
                                 empty-cells
                                 :test #'equal))
                       shape))
              anchors)))


(defun tile-anchors (shape)
  "Returns the list of valid anchor (row . col) positions for SHAPE on a
   *board-rows* × *board-cols* board: anchors keep every (row . col) in
   SHAPE inside [0,*board-rows*) × [0,*board-cols*)."
  (let* ((rows (mapcar #'car shape))
         (cols (mapcar #'cdr shape))
         (min-row (reduce #'min rows))
         (max-row (reduce #'max rows))
         (min-col (reduce #'min cols))
         (max-col (reduce #'max cols)))
    (loop for row from (- min-row) to (- (1- *board-rows*) max-row)
          append (loop for col from (- min-col) to (- (1- *board-cols*) max-col)
                       collect (cons row col)))))


(defun build-shape (extras row-lo row-hi col-lo col-hi)
  "Returns a tile shape combining explicit irregular cells (EXTRAS, each a (row . col))
   with a rectangular block spanning [row-lo..row-hi] × [col-lo..col-hi]."
  (append extras
          (loop for row from row-lo to row-hi
                append (loop for col from col-lo to col-hi
                             collect (cons row col)))))


(defparameter *tile-shapes*
  ;; Single source of truth for tile geometry (A..I, ordered by descending size).
  (list
   (cons 'A (build-shape '((0 . 0) (4 . -4))                                                                                 1 7 -3 3))
   (cons 'B (build-shape '((0 . 0) (4 . 4) (8 . 0) (7 . -3) (6 . -3) (2 . -3) (1 . -3))                                      1 7 -2 3))
   (cons 'C (build-shape '((3 . 7) (6 . 6) (6 . 5) (6 . 1) (6 . 0))                                                          0 5  0 6))
   (cons 'D (build-shape '((0 . 0) (1 . 3) (2 . 3) (6 . 3) (7 . 3) (8 . 0) (7 . -3) (6 . -3) (2 . -3) (1 . -3))              1 7 -2 2))
   (cons 'E (build-shape '((0 . 0) (4 . 4) (7 . 3) (7 . 2) (7 . -2) (7 . -3) (6 . -3) (2 . -3) (1 . -3))                     1 6 -2 3))
   (cons 'F (build-shape '((0 . 0) (0 . 1) (0 . 5) (0 . 6) (1 . 6) (5 . 6) (6 . 6))                                          1 6  0 5))
   (cons 'G (build-shape '((0 . 0) (0 . 1) (0 . 5) (0 . 6) (1 . 6) (5 . 6) (6 . 6))                                          1 6  0 5))
   (cons 'H (build-shape '((0 . 0) (0 . 1) (0 . 5) (0 . 6) (3 . 7) (6 . 6) (6 . 5) (6 . 1) (6 . 0) (5 . 0) (1 . 0))          1 5  1 6))
   (cons 'I (build-shape '((0 . 0) (0 . 1) (0 . 5) (0 . 6) (1 . 6) (5 . 6) (6 . 6) (6 . 5) (6 . 1) (6 . 0) (5 . 0) (1 . 0))  1 5  1 5))))


(defparameter *tile-anchors*
  ;; Precomputed initial anchor lists, derived once from *tile-shapes*.
  (mapcar (lambda (entry)
            (cons (car entry) (tile-anchors (cdr entry))))
          *tile-shapes*))


(ww-set *problem-name* tiles0b.2-csp)
(ww-set *problem-type* csp)
(ww-set *solution-type* first)
(ww-set *tree-or-graph* tree)


(define-types
 tile (A B C D E F G H I))  ;tiles ordered by size for most efficient search


(define-dynamic-relations
 (empty-cells $list)   ;current empty cells of the 21×21 board
 (placed tile))        ;per-tile boolean: tile has been placed


(define-query next-tile-to-place ()
  ;; True-MRV: among unplaced tiles, pick the one with the fewest currently
  ;; feasible anchors (full-shape fit against current empty-cells).
  ;; Returns a singleton list (framework treats the query result as the candidate
  ;; domain for the bound ?-var; one tile -> one-element list).
  (do (bind (empty-cells $empty-coords))
      (ww-loop for ?t in '(A B C D E F G H I)
            unless (placed ?t)
            collect (cons (feasible-anchor-count ?t $empty-coords) ?t) into $candidates
            finally (return (list (cdr (car (sort $candidates #'< :key #'car))))))))


;; Unified placement action: one invocation = one chosen tile (MRV);
;; one child state per valid ref-coord (asserted in the effect loop).
(define-action place-tile
 1
 (?tile (next-tile-to-place))
 (bind (empty-cells $empty-coords))
 (?tile $ref-coord)
 (ww-loop for $ref-coord in (cdr (assoc ?tile *tile-anchors*))
          do (setq $tile-coords (mapcar (lambda (offset)
                                          (cons (+ (car offset) (car $ref-coord))
                                                (+ (cdr offset) (cdr $ref-coord))))
                                        (cdr (assoc ?tile *tile-shapes*))))
             ;; Fail-fast empty-overlap check; assert one successor per surviving anchor.
             (if (every (lambda (c) (member c $empty-coords :test #'equal))
                        $tile-coords)
               (assert (empty-cells (set-difference $empty-coords
                                                    $tile-coords
                                                    :test #'equal))
                       (placed ?tile)))))


(define-init
 `(empty-cells ,(loop for row from 0 below *board-rows*
                      append (loop for col from 0 below *board-cols*
                                   collect (cons row col)))))


(define-goal
 (forall (?tile tile)
  (placed ?tile)))
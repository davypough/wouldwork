;;; Filename: problem-tiles0b.2-csp.lisp

;;; Shifting Mosaic from Islands of Insight (most NW corner island)
;;; Large tiles - REVISED: MRV, fail-fast, area consistency, symmetry breaking, no sorting
;;; This is an exact tiling/packing problem on a 21×21 grid (441 cells).
;;; The objective is to place a set of 9 large, irregularly shaped tiles onto the board such that:
;;; No tiles overlap, all 9 tiles are successfully placed, and there are no gaps.

(in-package :ww)


(ww-set *problem-name* tiles0b.2-csp)
(ww-set *problem-type* csp)
(ww-set *solution-type* first)
(ww-set *tree-or-graph* tree)
(ww-set *progress-reporting-interval* 100000)


(define-types
 tile (A B C D E F G H I)  ;tiles ordered by size for most efficient search
 void (EMPTY)  ;the empty object can be thought of as the current collection of empty coordinates
 object (either tile void))


(define-dynamic-relations
 (remaining object $list))   ;list of remaining coords to consider for a tile--eg, ((0 . 2) ...)


(define-static-relations
 (rel-coords tile $list)  ;relative tile coords; first coord is reference location
 (tile-size tile $fixnum))   ;number of cells in each tile for area consistency check


(define-query get-remaining (?obj)
 (do (bind (remaining ?obj $coords))
     $coords))


(define-query next-tile-to-place ()
  ;; MRV Heuristic: Dynamically selects the unplaced tile with the fewest valid anchors
  (ww-loop for ?t in '(A B C D E F G H I)
        do (bind (remaining ?t $coords))
        unless (eq $coords nil)
        collect (cons (length $coords) ?t) into $candidates
        finally (return (cdr (car (sort $candidates #'< :key #'car))))))


(define-query remaining-tile-area ()
  ;; Area Consistency: Sums cells of all unplaced tiles
  (ww-loop for ?t in '(A B C D E F G H I)
        do (bind (remaining ?t $coords))
        unless (eq $coords nil)
        do (bind (tile-size ?t $size))
        sum $size into $total
        finally (return $total)))


; Unified placement action replaces 9 individual put-X actions
(define-action place-tile
 1
 (?tile (next-tile-to-place) ?ref-coord (get-remaining ?tile))  ; Dynamically choose most constrained tile
 (and (bind (rel-coords ?tile $rel-tile-coords))
      (setq $tile-coords (mapcar (lambda (pair)
                                   (cons (+ (car pair) (car ?ref-coord)) (+ (cdr pair) (cdr ?ref-coord))))
                                 $rel-tile-coords))
      (bind (remaining EMPTY $empty-coords))
      ;; FAIL-FAST VALIDATION: Short-circuits on first overlap
      (every (lambda (c) (member c $empty-coords :test #'equal)) $tile-coords)
      ;; AREA CONSISTENCY: Prune if empty cells != sum of remaining tile cells
      (= (length $empty-coords) (remaining-tile-area))
      ;; SYMMETRY BREAKING: Constrain first large tile (A) to upper-left canonical region
      (or (not (eq ?tile 'A))
          (and (<= (car ?ref-coord) 10)
               (<= (car ?ref-coord) (cdr ?ref-coord)))))
 (?tile ?ref-coord)
 (assert (doall (?obj object)
           (do (bind (remaining ?obj $obj-coords))
               ;; REMOVE WITHOUT RESORTING: Preserves lexicographic order naturally
               (remaining ?obj (remove-if (lambda (c) (member c $tile-coords :test #'equal)) $obj-coords))))
         (remaining ?tile nil)))


(define-init
 ; Tile sizes computed exactly from rel-coords definitions
 `(tile-size A ,(length (append '((0 . 0) (4 . -4)) (loop for row from 1 to 7 append (loop for col from -3 to 3 collect (cons row col))))))
 `(tile-size B ,(length (append '((0 . 0) (4 . 4) (8 . 0) (7 . -3) (6 . -3) (2 . -3) (1 . -3)) (loop for row from 1 to 7 append (loop for col from -2 to 3 collect (cons row col))))))
 `(tile-size C ,(length (append '((3 . 7) (6 . 6) (6 . 5) (6 . 1) (6 . 0)) (loop for row from 0 to 5 append (loop for col from 0 to 6 collect (cons row col))))))
 `(tile-size D ,(length (append '((0 . 0) (1 . 3) (2 . 3) (6 . 3) (7 . 3) (8 . 0) (7 . -3) (6 . -3) (2 . -3) (1 . -3)) (loop for row from 1 to 7 append (loop for col from -2 to 2 collect (cons row col))))))
 `(tile-size E ,(length (append '((0 . 0) (4 . 4) (7 . 3) (7 . 2) (7 . -2) (7 . -3) (6 . -3) (2 . -3) (1 . -3)) (loop for row from 1 to 6 append (loop for col from -2 to 3 collect (cons row col))))))
 `(tile-size F ,(length (append '((0 . 0) (0 . 1) (0 . 5) (0 . 6) (1 . 6) (5 . 6) (6 . 6)) (loop for row from 1 to 6 append (loop for col from 0 to 5 collect (cons row col))))))
 `(tile-size G ,(length (append '((0 . 0) (0 . 1) (0 . 5) (0 . 6) (1 . 6) (5 . 6) (6 . 6)) (loop for row from 1 to 6 append (loop for col from 0 to 5 collect (cons row col))))))
 `(tile-size H ,(length (append '((0 . 0) (0 . 1) (0 . 5) (0 . 6) (3 . 7) (6 . 6) (6 . 5) (6 . 1) (6 . 0) (5 . 0) (1 . 0)) (loop for row from 1 to 5 append (loop for col from 1 to 6 collect (cons row col))))))
 `(tile-size I ,(length (append '((0 . 0) (0 . 1) (0 . 5) (0 . 6) (1 . 6) (5 . 6) (6 . 6) (6 . 5) (6 . 1) (6 . 0) (5 . 0) (1 . 0)) (loop for row from 1 to 5 append (loop for col from 1 to 5 collect (cons row col))))))


 ; Possible anchor coordinates pre-filtered for board boundaries
 `(remaining A ,(loop for row from 0 to 13 append (loop for col from 4 to 17 collect (cons row col))))
 `(remaining B ,(loop for row from 0 to 12 append (loop for col from 3 to 16 collect (cons row col))))
 `(remaining C ,(loop for row from 0 to 14 append (loop for col from 0 to 13 collect (cons row col))))
 `(remaining D ,(loop for row from 0 to 12 append (loop for col from 3 to 17 collect (cons row col))))
 `(remaining E ,(loop for row from 0 to 13 append (loop for col from 3 to 16 collect (cons row col))))
 `(remaining F ,(loop for row from 0 to 14 append (loop for col from 0 to 14 collect (cons row col))))
 `(remaining G ,(loop for row from 0 to 14 append (loop for col from 0 to 14 collect (cons row col))))
 `(remaining H ,(loop for row from 0 to 14 append (loop for col from 0 to 13 collect (cons row col))))
 `(remaining I ,(loop for row from 0 to 14 append (loop for col from 0 to 14 collect (cons row col))))
 `(remaining EMPTY ,(loop for row from 0 to 20 append (loop for col from 0 to 20 collect (cons row col))))

 ; Relative tile geometries
 `(rel-coords A ,(append '((0 . 0) (4 . -4)) (loop for row from 1 to 7 append (loop for col from -3 to 3 collect (cons row col)))))
 `(rel-coords B ,(append '((0 . 0) (4 . 4) (8 . 0) (7 . -3) (6 . -3) (2 . -3) (1 . -3)) (loop for row from 1 to 7 append (loop for col from -2 to 3 collect (cons row col)))))
 `(rel-coords C ,(append '((3 . 7) (6 . 6) (6 . 5) (6 . 1) (6 . 0)) (loop for row from 0 to 5 append (loop for col from 0 to 6 collect (cons row col)))))
 `(rel-coords D ,(append '((0 . 0) (1 . 3) (2 . 3) (6 . 3) (7 . 3) (8 . 0) (7 . -3) (6 . -3) (2 . -3) (1 . -3)) (loop for row from 1 to 7 append (loop for col from -2 to 2 collect (cons row col)))))
 `(rel-coords E ,(append '((0 . 0) (4 . 4) (7 . 3) (7 . 2) (7 . -2) (7 . -3) (6 . -3) (2 . -3) (1 . -3)) (loop for row from 1 to 6 append (loop for col from -2 to 3 collect (cons row col)))))
 `(rel-coords F ,(append '((0 . 0) (0 . 1) (0 . 5) (0 . 6) (1 . 6) (5 . 6) (6 . 6)) (loop for row from 1 to 6 append (loop for col from 0 to 5 collect (cons row col)))))
 `(rel-coords G ,(append '((0 . 0) (0 . 1) (0 . 5) (0 . 6) (1 . 6) (5 . 6) (6 . 6)) (loop for row from 1 to 6 append (loop for col from 0 to 5 collect (cons row col)))))
 `(rel-coords H ,(append '((0 . 0) (0 . 1) (0 . 5) (0 . 6) (3 . 7) (6 . 6) (6 . 5) (6 . 1) (6 . 0) (5 . 0) (1 . 0)) (loop for row from 1 to 5 append (loop for col from 1 to 6 collect (cons row col)))))
 `(rel-coords I ,(append '((0 . 0) (0 . 1) (0 . 5) (0 . 6) (1 . 6) (5 . 6) (6 . 6) (6 . 5) (6 . 1) (6 . 0) (5 . 0) (1 . 0)) (loop for row from 1 to 5 append (loop for col from 1 to 5 collect (cons row col))))))


(define-goal
 (forall (?tile tile)
  (remaining ?tile nil)))
;;; Filename: problem-tiles0b-csp.lisp

;;; Shifting Mosaic from Islands of Insight (most NW corner island)
;;; Large tiles


(in-package :ww)

(ww-set *problem-name* tiles0b-csp)

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
  (rel-coords tile $list))  ;current perimeter coords of a tile; first coord of a tile is the reference location


(define-query get-remaining (?obj)
  (do (bind (remaining ?obj $coords))
      $coords))


(defun sort-coords (coords)
  "Keeps coordinates lexicographically sorted."
  (sort coords (lambda (a b)
                 (or (< (car a) (car b))
                     (and (= (car a) (car b))
                          (< (cdr a) (cdr b)))))))


;Make one rule for each tile, largest tiles first


(define-action put-A
  1
  (?ref-coord (get-remaining A))  ;process all remaining possible places that A can be put
  (and (bind (rel-coords A $rel-tile-coords))  ;relative tile coords of A
       (setq $tile-coords (mapcar (lambda (pair)  ;current coords of the tile A
                                    (cons (+ (car pair) (car ?ref-coord)) (+ (cdr pair) (cdr ?ref-coord))))
                                  $rel-tile-coords))
       (bind (remaining EMPTY $empty-coords))  ;current empty coords
       (null (set-difference $tile-coords $empty-coords :test #'equal)))  ;all tile coords are empty spaces
  (?ref-coord)
  (assert (doall (?obj object)
            (do (bind (remaining ?obj $obj-coords))
                (remaining ?obj (sort-coords (set-difference $obj-coords $tile-coords :test #'equal)))))
          (remaining A nil)))  ;no longer needed


(define-action put-B
  1
  (?ref-coord (get-remaining B))
  (and (bind (rel-coords B $rel-tile-coords))
       (setq $tile-coords (mapcar (lambda (pair)
                                    (cons (+ (car pair) (car ?ref-coord)) (+ (cdr pair) (cdr ?ref-coord))))
                                  $rel-tile-coords))
       (bind (remaining EMPTY $empty-coords))
       (null (set-difference $tile-coords $empty-coords :test #'equal)))
  (?ref-coord)
  (assert (doall (?obj object)
            (do (bind (remaining ?obj $obj-coords))
                (remaining ?obj (sort-coords (set-difference $obj-coords $tile-coords :test #'equal)))))
          (remaining B nil)))


(define-action put-C
  1
  (?ref-coord (get-remaining C))
  (and (bind (rel-coords C $rel-tile-coords))
       (setq $tile-coords (mapcar (lambda (pair)
                                    (cons (+ (car pair) (car ?ref-coord)) (+ (cdr pair) (cdr ?ref-coord))))
                                  $rel-tile-coords))
       (bind (remaining EMPTY $empty-coords))
       (null (set-difference $tile-coords $empty-coords :test #'equal)))
  (?ref-coord)
  (assert (doall (?obj object)
            (do (bind (remaining ?obj $obj-coords))
                (remaining ?obj (sort-coords (set-difference $obj-coords $tile-coords :test #'equal)))))
          (remaining C nil)))


(define-action put-D
  1
  (?ref-coord (get-remaining D))
  (and (bind (rel-coords D $rel-tile-coords))
       (setq $tile-coords (mapcar (lambda (pair)
                                    (cons (+ (car pair) (car ?ref-coord)) (+ (cdr pair) (cdr ?ref-coord))))
                                  $rel-tile-coords))
       (bind (remaining EMPTY $empty-coords))
       (null (set-difference $tile-coords $empty-coords :test #'equal)))
  (?ref-coord)
  (assert (doall (?obj object)
            (do (bind (remaining ?obj $obj-coords))
                (remaining ?obj (sort-coords (set-difference $obj-coords $tile-coords :test #'equal)))))
          (remaining D nil)))


(define-action put-E
  1
  (?ref-coord (get-remaining E))
  (and (bind (rel-coords E $rel-tile-coords))
       (setq $tile-coords (mapcar (lambda (pair)
                                    (cons (+ (car pair) (car ?ref-coord)) (+ (cdr pair) (cdr ?ref-coord))))
                                  $rel-tile-coords))
       (bind (remaining EMPTY $empty-coords))
       (null (set-difference $tile-coords $empty-coords :test #'equal)))
  (?ref-coord)
  (assert (doall (?obj object)
            (do (bind (remaining ?obj $obj-coords))
                (remaining ?obj (sort-coords (set-difference $obj-coords $tile-coords :test #'equal)))))
          (remaining E nil)))


(define-action put-F
  1
  (?ref-coord (get-remaining F))
  (and (bind (rel-coords F $rel-tile-coords))
       (setq $tile-coords (mapcar (lambda (pair)
                                    (cons (+ (car pair) (car ?ref-coord)) (+ (cdr pair) (cdr ?ref-coord))))
                                  $rel-tile-coords))
       (bind (remaining EMPTY $empty-coords))
       (null (set-difference $tile-coords $empty-coords :test #'equal)))
  (?ref-coord)
  (assert (doall (?obj object)
            (do (bind (remaining ?obj $obj-coords))
                (remaining ?obj (sort-coords (set-difference $obj-coords $tile-coords :test #'equal)))))
          (remaining F nil)))


(define-action put-G
  1
  (?ref-coord (get-remaining G))
  (and (bind (rel-coords G $rel-tile-coords))
       (setq $tile-coords (mapcar (lambda (pair)
                                    (cons (+ (car pair) (car ?ref-coord)) (+ (cdr pair) (cdr ?ref-coord))))
                                  $rel-tile-coords))
       (bind (remaining EMPTY $empty-coords))
       (null (set-difference $tile-coords $empty-coords :test #'equal)))
  (?ref-coord)
  (assert (doall (?obj object)
            (do (bind (remaining ?obj $obj-coords))
                (remaining ?obj (sort-coords (set-difference $obj-coords $tile-coords :test #'equal)))))
          (remaining G nil)))


(define-action put-H
  1
  (?ref-coord (get-remaining H))
  (and (bind (rel-coords H $rel-tile-coords))
       (setq $tile-coords (mapcar (lambda (pair)
                                    (cons (+ (car pair) (car ?ref-coord)) (+ (cdr pair) (cdr ?ref-coord))))
                                  $rel-tile-coords))
       (bind (remaining EMPTY $empty-coords))
       (null (set-difference $tile-coords $empty-coords :test #'equal)))
  (?ref-coord)
  (assert (doall (?obj object)
            (do (bind (remaining ?obj $obj-coords))
                (remaining ?obj (sort-coords (set-difference $obj-coords $tile-coords :test #'equal)))))
          (remaining H nil)))


(define-action put-I
  1
  (?ref-coord (get-remaining I))
  (and (bind (rel-coords I $rel-tile-coords))
       (setq $tile-coords (mapcar (lambda (pair)
                                    (cons (+ (car pair) (car ?ref-coord)) (+ (cdr pair) (cdr ?ref-coord))))
                                  $rel-tile-coords))
       (bind (remaining EMPTY $empty-coords))
       (null (set-difference $tile-coords $empty-coords :test #'equal)))
  (?ref-coord)
  (assert (doall (?obj object)
            (do (bind (remaining ?obj $obj-coords))
                (remaining ?obj (sort-coords (set-difference $obj-coords $tile-coords :test #'equal)))))
          (remaining I nil)))


(define-init  ;possible coords of the reference location of each tile based on its shape initially
  `(remaining A  ,(loop for row from 0 to 13 append (loop for col from 4 to 17 collect (cons row col))))
  `(remaining B  ,(loop for row from 0 to 12 append (loop for col from 3 to 16 collect (cons row col))))
  `(remaining C  ,(loop for row from 0 to 14 append (loop for col from 0 to 13 collect (cons row col))))
  `(remaining D  ,(loop for row from 0 to 12 append (loop for col from 3 to 17 collect (cons row col))))
  `(remaining E  ,(loop for row from 0 to 13 append (loop for col from 3 to 16 collect (cons row col))))
  `(remaining F  ,(loop for row from 0 to 14 append (loop for col from 0 to 14 collect (cons row col))))
  `(remaining G  ,(loop for row from 0 to 14 append (loop for col from 0 to 14 collect (cons row col))))
  `(remaining H  ,(loop for row from 0 to 14 append (loop for col from 0 to 13 collect (cons row col))))
  `(remaining I  ,(loop for row from 0 to 14 append (loop for col from 0 to 14 collect (cons row col))))
  `(remaining EMPTY ,(loop for row from 0 to 20 append (loop for col from 0 to 20 collect (cons row col))))  ;initial 441 emptys

  ;relative locations of tile parts that matter for overlapping with other tile parts
  `(rel-coords A ,(append '((0 . 0) (4 . -4))
                          (loop for row from 1 to 7 append (loop for col from -3 to 3 collect (cons row col)))))

  `(rel-coords B ,(append '((0 . 0) (4 . 4) (8 . 0) (7 . -3) (6 . -3) (2 . -3) (1 . -3))
                          (loop for row from 1 to 7 append (loop for col from -2 to 3 collect (cons row col)))))

  `(rel-coords C ,(append '((3 . 7) (6 . 6) (6 . 5) (6 . 1) (6 . 0))
                          (loop for row from 0 to 5 append (loop for col from 0 to 6 collect (cons row col)))))

  `(rel-coords D ,(append '((0 . 0) (1 . 3) (2 . 3) (6 . 3) (7 . 3) (8 . 0) (7 . -3) (6 . -3) (2 . -3) (1 . -3))
                          (loop for row from 1 to 7 append (loop for col from -2 to 2 collect (cons row col)))))

  `(rel-coords E ,(append '((0 . 0) (4 . 4) (7 . 3) (7 . 2) (7 . -2) (7 . -3) (6 . -3) (2 . -3) (1 . -3))
                          (loop for row from 1 to 6 append (loop for col from -2 to 3 collect (cons row col)))))

  `(rel-coords F ,(append '((0 . 0) (0 . 1) (0 . 5) (0 . 6) (1 . 6) (5 . 6) (6 . 6))
                          (loop for row from 1 to 6 append (loop for col from 0 to 5 collect (cons row col)))))

  `(rel-coords G ,(append '((0 . 0) (0 . 1) (0 . 5) (0 . 6) (1 . 6) (5 . 6) (6 . 6))
                          (loop for row from 1 to 6 append (loop for col from 0 to 5 collect (cons row col)))))

  `(rel-coords H ,(append '((0 . 0) (0 . 1) (0 . 5) (0 . 6) (3 . 7) (6 . 6) (6 . 5) (6 . 1) (6 . 0) (5 . 0) (1 . 0))
                          (loop for row from 1 to 5 append (loop for col from 1 to 6 collect (cons row col)))))

  `(rel-coords I ,(append '((0 . 0) (0 . 1) (0 . 5) (0 . 6) (1 . 6) (5 . 6) (6 . 6) (6 . 5) (6 . 1) (6 . 0) (5 . 0) (1 . 0))
                          (loop for row from 1 to 5 append (loop for col from 1 to 5 collect (cons row col))))))

#|
  ;relative locations of tile parts that matter for overlapping with other tile parts
  (rel-coords A ((0 . 0) (1 . 1) (1 . 2) (1 . 3) (2 . 3) (3 . 3) (4 . 3) (5 . 3) (6 . 3) (7 . 3) (7 . 2) (7 . 1) (7 . 0) (7 . -1) (7 . -2) (7 . -3) (6 . -3) (5 . -3) (4 . -4) (3 . -3) (2 . -3) (1 . -3) (1 . -2) (1 . -1)))
  (rel-coords B ((0 . 0) (1 . 1) (1 . 2) (1 . 3) (2 . 3) (3 . 3) (4 . 4) (5 . 3) (6 . 3) (7 . 3) (7 . 2) (7 . 1) (8 . 0) (7 . -1) (7 . -2) (7 . -3) (6 . -3) (2 . -3) (1 . -3) (1 . -2) (1 . -1)))
  (rel-coords C ((0 . 0) (0 . 1) (0 . 2) (0 . 3) (0 . 4) (0 . 5) (0 . 6) (1 . 6) (2 . 6) (3 . 7) (4 . 6) (5 . 6) (6 . 6) (6 . 5) (6 . 1) (6 . 0) (5 . 0) (4 . 0) (3 . 0) (2 . 0) (1 . 0)))
  (rel-coords D ((0 . 0) (1 . 1) (1 . 2) (1 . 3) (2 . 3) (6 . 3) (7 . 3) (7 . 2) (7 . 1) (8 . 0) (7 . -1) (7 . -2) (7 . -3) (6 . -3) (2 . -3) (1 . -3) (1 . -2) (1 . -1)))
  (rel-coords E ((0 . 0) (1 . 1) (1 . 2) (1 . 3) (2 . 3) (3 . 3) (4 . 4) (5 . 3) (6 . 3) (7 . 3) (7 . 2) (7 . -2) (7 . -3) (6 . -3) (2 . -3) (1 . -3) (1 . -2) (1 . -1)))
  (rel-coords F ((0 . 0) (0 . 1) (0 . 5) (0 . 6) (1 . 6) (5 . 6) (6 . 6) (6 . 5) (6 . 4) (6 . 3) (6 . 2) (6 . 1) (6 . 0) (5 . 0) (4 . 0) (3 . 0) (2 . 0) (1 . 0)))
  (rel-coords G ((0 . 0) (0 . 1) (0 . 5) (0 . 6) (1 . 6) (5 . 6) (6 . 6) (6 . 5) (6 . 4) (6 . 3) (6 . 2) (6 . 1) (6 . 0) (5 . 0) (4 . 0) (3 . 0) (2 . 0) (1 . 0)))
  (rel-coords H ((0 . 0) (0 . 1) (0 . 5) (0 . 6) (1 . 6) (2 . 6) (3 . 7) (4 . 6) (5 . 6) (6 . 6) (6 . 5) (6 . 1) (6 . 0) (5 . 0) (1 . 0)))
  (rel-coords I ((0 . 0) (0 . 1) (0 . 5) (0 . 6) (1 . 6) (5 . 6) (6 . 6) (6 . 5) (6 . 1) (6 . 0) (5 . 0) (1 . 0))))
|#

(define-goal
  (forall (?tile tile)
    (remaining ?tile nil)))      

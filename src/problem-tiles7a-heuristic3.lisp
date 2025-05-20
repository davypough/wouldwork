;;; Filename: problem-tiles7a-heuristic3.lisp

;;; List problem specification for a blue/yellow tile shuffle in Islands of Insight.
;;; Basic search using (row . col) for coordinates (fastest)
;;; Using pre-post-emptys move empty coords for a tile

(in-package :ww)  ;required

(ww-set *problem-name* tiles7a-heuristic3)  ;continuation from heuristic2

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 36)  ;50)

(ww-set *progress-reporting-interval* 10000000)


(define-types
  tile   (DELL LELL 2HOR 3HOR CANE LKEY RKEY SQ1 SQ2 SQ3 SQ4 SQ5 UTEE RTEE DTEE LTEE Y)
  direction (right left down up))


(define-dynamic-relations
  (loc tile $fixnum $fixnum)  ;ref location of a tile with row col coordinates
  (emptys $list))  ;list of empty coordinates


(define-static-relations
  (pre-post-emptys tile direction $list $list)  ;pre & post move empty coords
  (Y-goal $fixnum $fixnum))  ;coords of the goal location


(define-query heuristic? ()
  ;Get the manhattan distance from first coord of Y tile to the goal coord. Lower is better.
  (do (bind (loc Y $Y-row $Y-col))
      (bind (Y-goal $Y-goal-row $Y-goal-col))
      (+ (abs (- $Y-row $Y-goal-row))
         (abs (- $Y-col $Y-goal-col)))))


(defun sort-coords (coords)
  ;Keeps coordinates lexicographically sorted.
  (sort coords (lambda (a b)
                 (or (< (car a) (car b))
                     (and (= (car a) (car b))
                          (< (cdr a) (cdr b)))))))


(define-query movable ($real-pre-emptys)
  ;Every pre-coord is empty.
  (do (bind (emptys $emptys))
      (every (lambda (coord)
               (member coord $emptys :test #'equal))
             $real-pre-emptys)))
      

(defun real-coords (row col coords)
  ;Translates relative coords to real coords.
  (mapcar (lambda (coord)
            (cons (+ row (car coord)) (+ col (cdr coord))))
          coords))


(defun list-difference (list1 list2 &key (test #'eql))
  "Subtracts elements in list2 from list1 respecting duplicates.
   Use for short lists, O(n^2)."
  (loop with new-list1 = list1
        for elem2 in list2
        do (setf new-list1 (remove elem2 new-list1 :test test :count 1))
        finally (return new-list1)))


(define-update check-emptys ()
  (do (bind (emptys $emptys))
      (unless (alexandria:setp $emptys :test #'equal)
        (troubleshoot "In CHECK-EMPTYS, $emptys is not setp: ~A" $emptys))
      (unless (alexandria:sequence-of-length-p $emptys 9)
        (troubleshoot "In CHECK-EMPTYS, $emptys length is not 9: ~A" (length $emptys)))))


(define-update check-tile-locs (?direction)
  (do (bind (emptys $emptys))
      (doall (?t tile)
        (if (bind (loc ?t $r $c))
          (when (member (cons $r $c) $emptys :test #'equal)
            (ut::prt ?t ?direction)
            (troubleshoot "tile loc is in $emptys: ~A ~A ~A" $r $c $emptys))
          (troubleshoot "tile does not have a location: ~A" ?t)))))


(define-action move
  1
  (?tile tile ?direction direction)
  (and (bind (loc ?tile $row $col))
       (bind (pre-post-emptys ?tile ?direction $pre-emptys $post-emptys))
       (setq $real-pre-emptys (real-coords $row $col $pre-emptys))
       (movable $real-pre-emptys))
  (?tile ?direction)
  (assert (bind (emptys $emptys))
          (setq $new-emptys (copy-list $emptys))
          (setq $real-post-emptys (real-coords $row $col $post-emptys))
          (case ?direction
            (right (loc ?tile $row (1+ $col)))
            (left (loc ?tile $row (1- $col)))
            (down (loc ?tile (1+ $row) $col))
            (up (loc ?tile (1- $row) $col)))
          (setq $new-emptys (list-difference $new-emptys $real-pre-emptys :test #'equal))
          (setq $new-emptys (append $new-emptys $real-post-emptys))
          (emptys (sort-coords $new-emptys))))
          ;(finally (check-emptys))
          ;(finally (check-tile-locs ?direction))))


(define-init
 (EMPTYS ((1 . 3) (2 . 0) (2 . 3) (2 . 4) (2 . 5) (3 . 0) (3 . 2) (3 . 4) (3 . 5)))
 (LOC Y 2 2) (LOC SQ1 0 5) (LOC SQ2 7 5) (LOC SQ3 7 4)
 (LOC SQ4 6 6) (LOC SQ5 3 7) (LOC 2HOR 0 3) (LOC 3HOR 7 0) (LOC CANE 1 1) (LOC LELL 5 7)
 (LOC DELL 0 0) (LOC RTEE 3 6) (LOC LTEE 4 5)
 (LOC DTEE 6 2) (LOC UTEE 3 3) (LOC LKEY 0 6) (LOC RKEY 4 0)

  (pre-post-emptys Y right ((0 . 1)) ((0 . 0)))  ;relative pre-move-emptys post-move-emptys
  (pre-post-emptys Y left ((0 . -1)) ((0 . 0)))
  (pre-post-emptys Y down ((1 . 0)) ((0 . 0)))
  (pre-post-emptys Y up ((-1 . 0)) ((0 . 0)))

  (pre-post-emptys SQ1 right ((0 . 1)) ((0 . 0)))
  (pre-post-emptys SQ1 left ((0 . -1)) ((0 . 0)))
  (pre-post-emptys SQ1 down ((1 . 0)) ((0 . 0)))
  (pre-post-emptys SQ1 up ((-1 . 0)) ((0 . 0)))

  (pre-post-emptys SQ2 right ((0 . 1)) ((0 . 0)))
  (pre-post-emptys SQ2 left ((0 . -1)) ((0 . 0)))
  (pre-post-emptys SQ2 down ((1 . 0)) ((0 . 0)))
  (pre-post-emptys SQ2 up ((-1 . 0)) ((0 . 0)))

  (pre-post-emptys SQ3 right ((0 . 1)) ((0 . 0)))
  (pre-post-emptys SQ3 left ((0 . -1)) ((0 . 0)))
  (pre-post-emptys SQ3 down ((1 . 0)) ((0 . 0)))
  (pre-post-emptys SQ3 up ((-1 . 0)) ((0 . 0)))

  (pre-post-emptys SQ4 right ((0 . 1)) ((0 . 0)))
  (pre-post-emptys SQ4 left ((0 . -1)) ((0 . 0)))
  (pre-post-emptys SQ4 down ((1 . 0)) ((0 . 0)))
  (pre-post-emptys SQ4 up ((-1 . 0)) ((0 . 0)))

  (pre-post-emptys SQ5 right ((0 . 1)) ((0 . 0)))
  (pre-post-emptys SQ5 left ((0 . -1)) ((0 . 0)))
  (pre-post-emptys SQ5 down ((1 . 0)) ((0 . 0)))
  (pre-post-emptys SQ5 up ((-1 . 0)) ((0 . 0)))

  (pre-post-emptys 2HOR right ((0 . 2)) ((0 . 0)))
  (pre-post-emptys 2HOR left ((0 . -1)) ((0 . 1)))
  (pre-post-emptys 2HOR down ((1 . 0) (1 . 1)) ((0 . 0) (0 . 1)))
  (pre-post-emptys 2HOR up ((-1 . 0) (-1 . 1)) ((0 . 0) (0 . 1)))

  (pre-post-emptys 3HOR right ((0 . 3)) ((0 . 0)))
  (pre-post-emptys 3HOR left ((0 . -1)) ((0 . 2)))
  (pre-post-emptys 3HOR down ((1 . 0) (1 . 1) (1 . 2)) ((0 . 0) (0 . 1) (0 . 2)))
  (pre-post-emptys 3HOR up ((-1 . 0) (-1 . 1) (-1 . 2)) ((0 . 0) (0 . 1) (0 . 2)))

  (pre-post-emptys CANE right ((0 . 2) (1 . 1) (2 . 1)) ((0 . 0) (1 . 0) (2 . 0)))
  (pre-post-emptys CANE left  ((0 . -1) (1 . -1) (2 . -1)) ((0 . 1) (1 . 0) (2 . 0)))
  (pre-post-emptys CANE down  ((1 . 1) (3 . 0)) ((0 . 0) (0 . 1)))
  (pre-post-emptys CANE up    ((-1 . 0) (-1 . 1)) ((0 . 1) (2 . 0)))

  (pre-post-emptys LELL right ((0 . 1) (1 . 1) (2 . 1)) ((0 . 0) (1 . 0) (2 . -1)))
  (pre-post-emptys LELL left  ((0 . -1) (1 . -1) (2 . -2)) ((0 . 0) (1 . 0) (2 . 0)))
  (pre-post-emptys LELL down  ((3 . -1) (3 . 0)) ((0 . 0) (2 . -1)))
  (pre-post-emptys LELL up    ((-1 . 0) (1 . -1)) ((2 . -1) (2 . 0)))

  (pre-post-emptys DELL right ((0 . 3) (1 . 1)) ((0 . 0) (1 . 0)))
  (pre-post-emptys DELL left  ((0 . -1) (1 . -1)) ((0 . 2) (1 . 0)))
  (pre-post-emptys DELL down  ((1 . 1) (1 . 2) (2 . 0)) ((0 . 0) (0 . 1) (0 . 2)))
  (pre-post-emptys DELL up    ((-1 . 0) (-1 . 1) (-1 . 2)) ((0 . 1) (0 . 2) (1 . 0)))

  (pre-post-emptys DTEE right ((0 . 3) (1 . 2)) ((0 . 0) (1 . 1)))
  (pre-post-emptys DTEE left  ((0 . -1) (1 . 0)) ((0 . 2) (1 . 1)))
  (pre-post-emptys DTEE down  ((1 . 0) (2 . 1) (1 . 2)) ((0 . 0) (0 . 1) (0 . 2)))
  (pre-post-emptys DTEE up    ((-1 . 0) (-1 . 1) (-1 . 2)) ((0 . 0) (1 . 1) (0 . 2)))

  (pre-post-emptys UTEE right ((0 . 1) (1 . 2)) ((0 . 0) (1 . -1)))
  (pre-post-emptys UTEE left  ((0 . -1) (1 . -2)) ((0 . 0) (1 . 1)))
  (pre-post-emptys UTEE down  ((2 . -1) (2 . 0) (2 . 1)) ((0 . 0) (1 . -1) (1 . 1)))
  (pre-post-emptys UTEE up    ((-1 . 0) (0 . -1) (0 . 1)) ((1 . -1) (1 . 0) (1 . 1)))

  (pre-post-emptys RTEE right ((0 . 1) (1 . 2) (2 . 1)) ((0 . 0) (1 . 0) (2 . 0)))
  (pre-post-emptys RTEE left  ((0 . -1) (1 . -1) (2 . -1)) ((0 . 0) (1 . 1) (2 . 0)))
  (pre-post-emptys RTEE down  ((2 . 1) (3 . 0)) ((0 . 0) (1 . 1)))
  (pre-post-emptys RTEE up    ((-1 . 0) (0 . 1)) ((1 . 1) (2 . 0)))

  (pre-post-emptys LTEE right ((0 . 1) (1 . 1) (2 . 1)) ((0 . 0) (1 . -1) (2 . 0)))
  (pre-post-emptys LTEE left  ((0 . -1) (1 . -2) (2 . -1)) ((0 . 0) (1 . 0) (2 . 0)))
  (pre-post-emptys LTEE down  ((2 . -1) (3 . 0)) ((0 . 0) (1 . -1)))
  (pre-post-emptys LTEE up    ((-1 . 0) (0 . -1)) ((1 . -1) (2 . 0)))

  (pre-post-emptys LKEY right ((0 . 2) (1 . 2) (2 . 2)) ((0 . 0) (1 . -2) (2 . 0)))
  (pre-post-emptys LKEY left  ((0 . -1) (1 . -3) (2 . -1)) ((0 . 1) (1 . 1) (2 . 1)))
  (pre-post-emptys LKEY down  ((2 . -2) (2 . -1) (3 . 0) (3 . 1)) ((0 . 0) (0 . 1) (1 . -2) (1 . -1)))
  (pre-post-emptys LKEY up    ((-1 . 0) (-1 . 1) (0 . -2) (0 . -1)) ((1 . -2) (1 . -1) (2 . 0) (2 . 1)))

  (pre-post-emptys RKEY right ((0 . 2) (1 . 4) (2 . 2)) ((0 . 0) (1 . 0) (2 . 0)))
  (pre-post-emptys RKEY left  ((0 . -1) (1 . -1) (2 . -1)) ((0 . 1) (1 . 3) (2 . 1)))
  (pre-post-emptys RKEY down  ((2 . 2) (2 . 3) (3 . 0) (3 . 1)) ((0 . 0) (0 . 1) (1 . 2) (1 . 3)))
  (pre-post-emptys RKEY up    ((-1 . 0) (-1 . 1) (0 . 2) (0 . 3)) ((1 . 2) (1 . 3) (2 . 0) (2 . 1)))

  (Y-goal 0 0))


(define-goal
  (loc Y 0 0))

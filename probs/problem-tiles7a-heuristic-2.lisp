;;; Filename: problem-tiles7a-heuristic-2.lisp

;;; List problem specification for a blue/yellow tile shuffle in Islands of Insight.
;;; Uses (row . col) coordinates and pre/post move empty-coordinate templates for each tile.
;;; The five identical blue single squares are stored as one sorted list of cells.
;;; The full puzzle brings Y to 0,0 through two milestones, solved as a chain
;;; (each stage starts where the previous one ended; set the cutoff per stage):
;;;   (stage tiles7a-heuristic-2)
;;;   (solve)                                ;Y to 3,3: 22 moves
;;;   (ww-set *depth-cutoff* 48)
;;;   (solve-subgoal (loc Y 2 2))            ;48 moves
;;;   (ww-set *depth-cutoff* 36)
;;;   (solve-subgoal (loc Y 0 0))            ;36 moves
;;; The goal and every subgoal must have the form (loc Y row col).

;;; Design notes (changes from problem-tiles7a-heuristic-1):
;;; - The lower bound on moves remaining is no longer the yellow tile's distance alone.
;;;   The spec now describes a relaxed version of the puzzle, in which a cell once empty
;;;   stays empty and a piece once somewhere stays there too.  The engine computes from it
;;;   how many moves at least any solution still needs (the LM-cut bound), counting the
;;;   pieces that must first move out of the yellow tile's way.
;;; - The relaxed puzzle allows every real move (each needs the same piece position and
;;;   empty cells, and produces the same new position and vacated cells), so it is never
;;;   harder than the real one, and the bound never overestimates.  It is at least the
;;;   yellow tile's distance, which it replaces.
;;; - Trade-off: the bound costs much more per state than the distance; it pays only if it
;;;   prunes enough more states.

(in-package :ww)  ;required

(ww-set *problem-name* tiles7a-heuristic-2)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 22)


(define-types
  tile   (DELL LELL 2HOR 3HOR CANE LKEY RKEY UTEE RTEE DTEE LTEE Y)
  direction (right left down up)
  row (0 1 2 3 4 5 6 7)
  col (0 1 2 3 4 5 6 7))


(define-dynamic-relations
  (loc tile $row $col)  ;ref location of a tile with row col coordinates
  (squares $list)  ;sorted coords of the five identical blue single squares
  (emptys $list))  ;sorted list of empty coordinates


(define-static-relations
  (pre-post-emptys> tile direction $list $list))  ;pre & post move empty coords


(define-query movable (?real-pre-emptys)
  ;Every pre-coord is empty.
  (do (bind (emptys $emptys))
      (every (lambda (coord)
               (member coord $emptys :test #'equal))
             ?real-pre-emptys)))


(define-query get-squares ()
  (do (bind (squares $squares))
      $squares))


(defun sort-coords (coords)
  ;Keeps coordinates lexicographically sorted. Sorts a copy: callers pass lists
  ;from REMOVE, whose tails are shared with the parent state's database values.
  (sort (copy-list coords) (lambda (a b)
                             (or (< (car a) (car b))
                                 (and (= (car a) (car b))
                                      (< (cdr a) (cdr b)))))))


(defun real-coords (row col coords)
  ;Translates relative coords to real coords.
  (mapcar (lambda (coord)
            (cons (+ row (car coord)) (+ col (cdr coord))))
          coords))


;;; Relaxed model for the engine's LM-cut lower bound (declare-relaxed-model).
;;; Facts: (loc tile row col), (empty (row . col)), (square (row . col)).


(defun relaxed-tile-moves (templates)
  ;One operator per template and reference position whose cells all lie on the board.
  ;Positions range past the board, since a reference cell need not belong to its piece.
  (loop for (tile direction pre-emptys post-emptys) in templates
        for offset = (direction-offset direction)
        nconc (loop for row from -3 to 10
                    nconc (loop for col from -3 to 10
                                for pre = (real-coords row col pre-emptys)
                                for post = (real-coords row col post-emptys)
                                when (every (lambda (cell) (and (<= 0 (car cell) 7) (<= 0 (cdr cell) 7)))
                                            (append pre post))
                                  collect (make-relaxed-hmax-operator
                                            :name (list 'move tile direction row col)
                                            :preconditions (cons (list 'loc tile row col)
                                                                 (mapcar (lambda (cell) (list 'empty cell)) pre))
                                            :effects (cons (list 'loc tile (+ row (car offset)) (+ col (cdr offset)))
                                                           (mapcar (lambda (cell) (list 'empty cell)) post)))))))


(defun relaxed-square-moves ()
  ;A square moves into an adjacent empty cell and vacates its own.
  (loop for row from 0 to 7
        nconc (loop for col from 0 to 7
                    for cell = (cons row col)
                    nconc (loop for direction in '(right left down up)
                                for offset = (direction-offset direction)
                                for target = (cons (+ row (car offset)) (+ col (cdr offset)))
                                when (and (<= 0 (car target) 7) (<= 0 (cdr target) 7))
                                  collect (make-relaxed-hmax-operator
                                            :name (list 'move-square cell direction)
                                            :preconditions (list (list 'square cell) (list 'empty target))
                                            :effects (list (list 'square target) (list 'empty cell)))))))


(defun direction-offset (direction)
  ;(row . col) shift of a move in DIRECTION.
  (ecase direction
    (right '(0 . 1))
    (left '(0 . -1))
    (down '(1 . 0))
    (up '(-1 . 0))))


(define-query relaxed-moves ()
  ;Relaxed operators for every tile and square move: needs the piece's place and its
  ;pre-move cells empty; produces the piece's new place and the cells it vacates.
  (do (assign $templates nil)
      (doall (?tile tile)
        (doall (?direction direction)
          (do (bind (pre-post-emptys> ?tile ?direction $pre-emptys $post-emptys))
              (push (list ?tile ?direction $pre-emptys $post-emptys) $templates))))
      (append (relaxed-tile-moves $templates) (relaxed-square-moves))))


(define-query relaxed-state-facts ()
  ;Every fact the relaxed operators read, true in the current state.
  (do (assign $facts nil)
      (doall (?tile tile)
        (do (bind (loc ?tile $row $col))
            (push (list 'loc ?tile $row $col) $facts)))
      (bind (emptys $emptys))
      (bind (squares $squares))
      (append $facts
              (mapcar (lambda (cell) (list 'empty cell)) $emptys)
              (mapcar (lambda (cell) (list 'square cell)) $squares))))


(declare-relaxed-model 'relaxed-moves 'relaxed-state-facts 'list)  ;goal facts: (list goal)


(define-action move
  1
  (?tile tile ?direction direction)
  (and (bind (loc ?tile $row $col))
       (bind (pre-post-emptys> ?tile ?direction $pre-emptys $post-emptys))
       (assign $real-pre-emptys (real-coords $row $col $pre-emptys))
       (movable $real-pre-emptys))
  (?tile ?direction)
  (assert (bind (emptys $emptys))
          (assign $real-post-emptys (real-coords $row $col $post-emptys))
          (case ?direction
            (right (loc ?tile $row (1+ $col)))
            (left (loc ?tile $row (1- $col)))
            (down (loc ?tile (1+ $row) $col))
            (up (loc ?tile (1- $row) $col)))
          (emptys (sort-coords (append (set-difference $emptys $real-pre-emptys :test #'equal)
                                       $real-post-emptys)))))


(define-action move-square
  1
  (?square (get-squares) ?direction direction)
  (and (assign $target (case ?direction
                         (right (cons (car ?square) (1+ (cdr ?square))))
                         (left (cons (car ?square) (1- (cdr ?square))))
                         (down (cons (1+ (car ?square)) (cdr ?square)))
                         (up (cons (1- (car ?square)) (cdr ?square)))))
       (movable (list $target)))
  (?square ?direction)
  (assert (bind (emptys $emptys))
          (bind (squares $squares))
          (squares (sort-coords (cons $target (remove ?square $squares :test #'equal))))
          (emptys (sort-coords (cons ?square (remove $target $emptys :test #'equal))))))


(define-init
  (loc Y 6 7)  ;uppermost leftmost reference coord for a tile
  (loc 2HOR 0 3)
  (loc 3HOR 7 5)
  (loc CANE 1 1)
  (loc LELL 4 6)
  (loc DELL 0 0)
  (loc RTEE 2 5)
  (loc LTEE 3 2)
  (loc DTEE 5 3)
  (loc UTEE 1 3)
  (loc LKEY 0 6)
  (loc RKEY 5 0)

  (pre-post-emptys> Y right ((0 . 1)) ((0 . 0)))  ;relative pre-move-emptys post-move-emptys
  (pre-post-emptys> Y left ((0 . -1)) ((0 . 0)))
  (pre-post-emptys> Y down ((1 . 0)) ((0 . 0)))
  (pre-post-emptys> Y up ((-1 . 0)) ((0 . 0)))






  (pre-post-emptys> 2HOR right ((0 . 2)) ((0 . 0)))
  (pre-post-emptys> 2HOR left ((0 . -1)) ((0 . 1)))
  (pre-post-emptys> 2HOR down ((1 . 0) (1 . 1)) ((0 . 0) (0 . 1)))
  (pre-post-emptys> 2HOR up ((-1 . 0) (-1 . 1)) ((0 . 0) (0 . 1)))

  (pre-post-emptys> 3HOR right ((0 . 3)) ((0 . 0)))
  (pre-post-emptys> 3HOR left ((0 . -1)) ((0 . 2)))
  (pre-post-emptys> 3HOR down ((1 . 0) (1 . 1) (1 . 2)) ((0 . 0) (0 . 1) (0 . 2)))
  (pre-post-emptys> 3HOR up ((-1 . 0) (-1 . 1) (-1 . 2)) ((0 . 0) (0 . 1) (0 . 2)))

  (pre-post-emptys> CANE right ((0 . 2) (1 . 1) (2 . 1)) ((0 . 0) (1 . 0) (2 . 0)))
  (pre-post-emptys> CANE left  ((0 . -1) (1 . -1) (2 . -1)) ((0 . 1) (1 . 0) (2 . 0)))
  (pre-post-emptys> CANE down  ((1 . 1) (3 . 0)) ((0 . 0) (0 . 1)))
  (pre-post-emptys> CANE up    ((-1 . 0) (-1 . 1)) ((0 . 1) (2 . 0)))

  (pre-post-emptys> LELL right ((0 . 1) (1 . 1) (2 . 1)) ((0 . 0) (1 . 0) (2 . -1)))
  (pre-post-emptys> LELL left  ((0 . -1) (1 . -1) (2 . -2)) ((0 . 0) (1 . 0) (2 . 0)))
  (pre-post-emptys> LELL down  ((3 . -1) (3 . 0)) ((0 . 0) (2 . -1)))
  (pre-post-emptys> LELL up    ((-1 . 0) (1 . -1)) ((2 . -1) (2 . 0)))

  (pre-post-emptys> DELL right ((0 . 3) (1 . 1)) ((0 . 0) (1 . 0)))
  (pre-post-emptys> DELL left  ((0 . -1) (1 . -1)) ((0 . 2) (1 . 0)))
  (pre-post-emptys> DELL down  ((1 . 1) (1 . 2) (2 . 0)) ((0 . 0) (0 . 1) (0 . 2)))
  (pre-post-emptys> DELL up    ((-1 . 0) (-1 . 1) (-1 . 2)) ((0 . 1) (0 . 2) (1 . 0)))

  (pre-post-emptys> DTEE right ((0 . 3) (1 . 2)) ((0 . 0) (1 . 1)))
  (pre-post-emptys> DTEE left  ((0 . -1) (1 . 0)) ((0 . 2) (1 . 1)))
  (pre-post-emptys> DTEE down  ((1 . 0) (2 . 1) (1 . 2)) ((0 . 0) (0 . 1) (0 . 2)))
  (pre-post-emptys> DTEE up    ((-1 . 0) (-1 . 1) (-1 . 2)) ((0 . 0) (1 . 1) (0 . 2)))

  (pre-post-emptys> UTEE right ((0 . 1) (1 . 2)) ((0 . 0) (1 . -1)))
  (pre-post-emptys> UTEE left  ((0 . -1) (1 . -2)) ((0 . 0) (1 . 1)))
  (pre-post-emptys> UTEE down  ((2 . -1) (2 . 0) (2 . 1)) ((0 . 0) (1 . -1) (1 . 1)))
  (pre-post-emptys> UTEE up    ((-1 . 0) (0 . -1) (0 . 1)) ((1 . -1) (1 . 0) (1 . 1)))

  (pre-post-emptys> RTEE right ((0 . 1) (1 . 2) (2 . 1)) ((0 . 0) (1 . 0) (2 . 0)))
  (pre-post-emptys> RTEE left  ((0 . -1) (1 . -1) (2 . -1)) ((0 . 0) (1 . 1) (2 . 0)))
  (pre-post-emptys> RTEE down  ((2 . 1) (3 . 0)) ((0 . 0) (1 . 1)))
  (pre-post-emptys> RTEE up    ((-1 . 0) (0 . 1)) ((1 . 1) (2 . 0)))

  (pre-post-emptys> LTEE right ((0 . 1) (1 . 1) (2 . 1)) ((0 . 0) (1 . -1) (2 . 0)))
  (pre-post-emptys> LTEE left  ((0 . -1) (1 . -2) (2 . -1)) ((0 . 0) (1 . 0) (2 . 0)))
  (pre-post-emptys> LTEE down  ((2 . -1) (3 . 0)) ((0 . 0) (1 . -1)))
  (pre-post-emptys> LTEE up    ((-1 . 0) (0 . -1)) ((1 . -1) (2 . 0)))

  (pre-post-emptys> LKEY right ((0 . 2) (1 . 2) (2 . 2)) ((0 . 0) (1 . -2) (2 . 0)))
  (pre-post-emptys> LKEY left  ((0 . -1) (1 . -3) (2 . -1)) ((0 . 1) (1 . 1) (2 . 1)))
  (pre-post-emptys> LKEY down  ((2 . -2) (2 . -1) (3 . 0) (3 . 1)) ((0 . 0) (0 . 1) (1 . -2) (1 . -1)))
  (pre-post-emptys> LKEY up    ((-1 . 0) (-1 . 1) (0 . -2) (0 . -1)) ((1 . -2) (1 . -1) (2 . 0) (2 . 1)))

  (pre-post-emptys> RKEY right ((0 . 2) (1 . 4) (2 . 2)) ((0 . 0) (1 . 0) (2 . 0)))
  (pre-post-emptys> RKEY left  ((0 . -1) (1 . -1) (2 . -1)) ((0 . 1) (1 . 3) (2 . 1)))
  (pre-post-emptys> RKEY down  ((2 . 2) (2 . 3) (3 . 0) (3 . 1)) ((0 . 0) (0 . 1) (1 . 2) (1 . 3)))
  (pre-post-emptys> RKEY up    ((-1 . 0) (-1 . 1) (0 . 2) (0 . 3)) ((1 . 2) (1 . 3) (2 . 0) (2 . 1)))

  (squares ((0 . 5) (3 . 3) (3 . 4) (4 . 3) (4 . 4)))
  (emptys ((2 . 0) (3 . 0) (3 . 7) (4 . 0) (4 . 7) (5 . 7) (7 . 2) (7 . 3) (7 . 4))))


(define-goal  ;first milestone; see the header for the full chain
  (loc Y 3 3))

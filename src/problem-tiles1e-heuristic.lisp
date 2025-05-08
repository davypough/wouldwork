;;; Filename: problem-tiles1e-heuristic.lisp

;;; List problem specification for a blue/yellow tile shuffle in Islands of Insight.
;;; Tiles of various shapes can move around on a 4x4 board.
;;; The yellow tile must end up in the goal position.
;;; Multiple rules as opposed to one rule.

(in-package :ww)  ;required

(ww-set *problem-name* tiles1e-heuristic)

(ww-set *problem-type* planning)

(ww-set *solution-type* first)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 40)


(define-types
  SQ-tile (SQ)
  HOR-tile (HOR)
  VER-tile (VER)
  L-tile (L1 YL2)  ;YL2 is yellow
  tile (either SQ-tile HOR-tile VER-tile L-tile))


(define-dynamic-relations
  (loc tile $fixnum $fixnum)  ;location of a tile with row, col coordinates
  (emptys $list))  ;list of empty (row . col) coordinates


(define-static-relations
  (YL2-goal $fixnum $fixnum))


(define-query heuristic? ()
  ;Get the manhattan distance from first coord of Y tile to the goal coord. Lower is better.
  (do (bind (loc YL2 $YL2-row $YL2-col))
      (bind (YL2-goal $YL2-goal-row $YL2-goal-col))
      (+ (abs (- $YL2-row $YL2-goal-row))
         (abs (- $YL2-col $YL2-goal-col)))))


(defun check-coord (new-row new-col)
  (when (and (<= 0 new-row 3)
             (<= 0 new-col 3))
    (cons new-row new-col)))


(defun sort-coords (coords)
  "Keeps coordinates lexicographically sorted."
  (sort coords (lambda (a b)
                 (or (< (car a) (car b))
                     (and (= (car a) (car b))
                          (< (cdr a) (cdr b)))))))


(define-action move-SQ
  1
  (?tile SQ-tile)
  (always-true)
  (?tile $direction)
  (do (bind (loc ?tile $row $col))
      (bind (emptys $emptys))
      (if (and (setq $empty1-coord (check-coord $row (1+ $col)))
               (member $empty1-coord $emptys :test #'equal))
        (assert (setq $direction 'right)
                (setq $new-emptys (copy-list $emptys))
                (alexandria:deletef $new-emptys $empty1-coord :test #'equal)
                (push (cons $row $col) $new-emptys)
                (setq $new-emptys (sort-coords $new-emptys))
                (emptys $new-emptys)
                (loc ?tile $row (1+ $col))))
      (if (and (setq $empty1-coord (check-coord $row (1- $col)))
               (member $empty1-coord $emptys :test #'equal))
        (assert (setq $direction 'left)
                (setq $new-emptys (copy-list $emptys))
                (alexandria:deletef $new-emptys $empty1-coord :test #'equal)
                (push (cons $row $col) $new-emptys)
                (setq $new-emptys (sort-coords $new-emptys))     
                (emptys $new-emptys)
                (loc ?tile $row (1- $col))))
      (if (and (setq $empty1-coord (check-coord (1+ $row) $col))
               (member $empty1-coord $emptys :test #'equal))
        (assert (setq $direction 'down)
                (setq $new-emptys (copy-list $emptys))
                (alexandria:deletef $new-emptys $empty1-coord :test #'equal)
                (push (cons $row $col) $new-emptys)
                (setq $new-emptys (sort-coords $new-emptys))     
                (emptys $new-emptys)
                (loc ?tile (1+ $row) $col)))
      (if (and (setq $empty1-coord (check-coord (1- $row) $col))
               (member $empty1-coord $emptys :test #'equal))
        (assert (setq $direction 'up)
                (setq $new-emptys (copy-list $emptys))
                (alexandria:deletef $new-emptys $empty1-coord :test #'equal)
                (push (cons $row $col) $new-emptys)
                (setq $new-emptys (sort-coords $new-emptys))
                (emptys $new-emptys)
                (loc ?tile (1- $row) $col)))))


(define-action move-HOR
  1
  (?tile HOR-tile)
  (always-true)
  (?tile $direction)
  (do (bind (loc ?tile $row $col))
      (bind (emptys $emptys))
      (if (and (setq $empty1-coord (check-coord $row (+ $col 2)))
               (member $empty1-coord $emptys :test #'equal))
        (assert (setq $direction 'right)
                (setq $new-emptys (copy-list $emptys))
                (alexandria:deletef $new-emptys $empty1-coord :test #'equal)
                (push (cons $row $col) $new-emptys)
                (setq $new-emptys (sort-coords $new-emptys))
                (emptys $new-emptys)
                (loc ?tile $row (1+ $col))))
      (if (and (setq $empty1-coord (check-coord $row (1- $col)))
               (member $empty1-coord $emptys :test #'equal))
        (assert (setq $direction 'left)
                (setq $new-emptys (copy-list $emptys))
                (alexandria:deletef $new-emptys $empty1-coord :test #'equal)
                (push (cons $row (1+ $col)) $new-emptys)
                (setq $new-emptys (sort-coords $new-emptys))
                (emptys $new-emptys)
                (loc ?tile $row (1- $col))))
      (if (and (setq $empty1-coord (check-coord (1+ $row) $col))
               (member $empty1-coord $emptys :test #'equal)
               (setq $empty2-coord (check-coord (1+ $row) (1+ $col)))
               (member $empty2-coord $emptys :test #'equal))
        (assert (setq $direction 'down)
                (setq $new-emptys (copy-list $emptys))
                (alexandria:deletef $new-emptys $empty1-coord :test #'equal)
                (alexandria:deletef $new-emptys $empty2-coord :test #'equal)
                (push (cons $row $col) $new-emptys)
                (push (cons $row (1+ $col)) $new-emptys)
                (setq $new-emptys (sort-coords $new-emptys))
                (emptys $new-emptys)
                (loc ?tile (1+ $row) $col)))
      (if (and (setq $empty1-coord (check-coord (1- $row) $col))
               (member $empty1-coord $emptys :test #'equal)
               (setq $empty2-coord (check-coord (1- $row) (1+ $col)))
               (member $empty2-coord $emptys :test #'equal))
        (assert (setq $direction 'up)
                (setq $new-emptys (copy-list $emptys))
                (alexandria:deletef $new-emptys $empty1-coord :test #'equal)
                (alexandria:deletef $new-emptys $empty2-coord :test #'equal)
                (push (cons $row $col) $new-emptys)
                (push (cons $row (1+ $col)) $new-emptys)
                (setq $new-emptys (sort-coords $new-emptys))
                (emptys $new-emptys)
                (loc ?tile (1- $row) $col)))))


(define-action move-VER
  1
  (?tile VER-tile)
  (always-true)
  (?tile $direction)
  (do (bind (loc ?tile $row $col))
      (bind (emptys $emptys))
      (if (and (setq $empty1-coord (check-coord (+ $row 2) $col))
               (member $empty1-coord $emptys :test #'equal))
        (assert (setq $direction 'down)
                (setq $new-emptys (copy-list $emptys))
                (alexandria:deletef $new-emptys $empty1-coord :test #'equal)
                (push (cons $row $col) $new-emptys)
                (setq $new-emptys (sort-coords $new-emptys))
                (emptys $new-emptys)
                (loc ?tile (1+ $row) $col)))
      (if (and (setq $empty1-coord (check-coord (1- $row) $col))
               (member $empty1-coord $emptys :test #'equal))
        (assert (setq $direction 'up)
                (setq $new-emptys (copy-list $emptys))
                (alexandria:deletef $new-emptys $empty1-coord :test #'equal)
                (push (cons (1+ $row) $col) $new-emptys)
                (setq $new-emptys (sort-coords $new-emptys))
                (emptys $new-emptys)
                (loc ?tile (1- $row) $col)))
      (if (and (setq $empty1-coord (check-coord $row (1+ $col)))
               (member $empty1-coord $emptys :test #'equal)
               (setq $empty2-coord (check-coord (1+ $row) (1+ $col)))
               (member $empty2-coord $emptys :test #'equal))
        (assert (setq $direction 'right)
                (setq $new-emptys (copy-list $emptys))
                (alexandria:deletef $new-emptys $empty1-coord :test #'equal)
                (alexandria:deletef $new-emptys $empty2-coord :test #'equal)
                (push (cons $row $col) $new-emptys)
                (push (cons (1+ $row) $col) $new-emptys)
                (setq $new-emptys (sort-coords $new-emptys))
                (emptys $new-emptys)
                (loc ?tile $row (1+ $col))))
      (if (and (setq $empty1-coord (check-coord $row (1- $col)))
               (member $empty1-coord $emptys :test #'equal)
               (setq $empty2-coord (check-coord (1+ $row) (1- $col)))
               (member $empty2-coord $emptys :test #'equal))
        (assert (setq $direction 'left)
                (setq $new-emptys (copy-list $emptys))
                (alexandria:deletef $new-emptys $empty1-coord :test #'equal)
                (alexandria:deletef $new-emptys $empty2-coord :test #'equal)
                (push (cons $row $col) $new-emptys)
                (push (cons (1+ $row) $col) $new-emptys)
                (setq $new-emptys (sort-coords $new-emptys))
                (emptys $new-emptys)
                (loc ?tile $row (1- $col))))))


(define-action move-L
  1
  (?tile L-tile)
  (always-true)
  (?tile $direction)
  (do (bind (loc ?tile $row $col))
      (bind (emptys $emptys))
      (if (and (setq $empty1-coord (check-coord $row (1+ $col)))
               (member $empty1-coord $emptys :test #'equal)
               (setq $empty2-coord (check-coord (1+ $row) (1+ $col)))
               (member $empty2-coord $emptys :test #'equal))
        (assert (setq $direction 'right)
                (setq $new-emptys (copy-list $emptys))
                (alexandria:deletef $new-emptys $empty1-coord :test #'equal)
                (alexandria:deletef $new-emptys $empty2-coord :test #'equal)
                (push (cons $row $col) $new-emptys)
                (push (cons (1+ $row) (1- $col)) $new-emptys)
                (setq $new-emptys (sort-coords $new-emptys))
                (emptys $new-emptys)
                (loc ?tile $row (1+ $col))))
      (if (and (setq $empty1-coord (check-coord $row (1- $col)))
               (member $empty1-coord $emptys :test #'equal)
               (setq $empty2-coord (check-coord (1+ $row) (- $col 2)))
               (member $empty2-coord $emptys :test #'equal))
        (assert (setq $direction 'left)
                (setq $new-emptys (copy-list $emptys))
                (alexandria:deletef $new-emptys $empty1-coord :test #'equal)
                (alexandria:deletef $new-emptys $empty2-coord :test #'equal)
                (push (cons $row $col) $new-emptys)
                (push (cons (1+ $row) $col) $new-emptys)
                (setq $new-emptys (sort-coords $new-emptys))
                (emptys $new-emptys)
                (loc ?tile $row (1- $col))))
      (if (and (setq $empty1-coord (check-coord (+ $row 2) $col))
               (member $empty1-coord $emptys :test #'equal)
               (setq $empty2-coord (check-coord (+ $row 2) (1- $col)))
               (member $empty2-coord $emptys :test #'equal))
        (assert (setq $direction 'down)
                (setq $new-emptys (copy-list $emptys))
                (alexandria:deletef $new-emptys $empty1-coord :test #'equal)
                (alexandria:deletef $new-emptys $empty2-coord :test #'equal)
                (push (cons $row $col) $new-emptys)
                (push (cons (1+ $row) (1- $col)) $new-emptys)
                (setq $new-emptys (sort-coords $new-emptys))
                (emptys $new-emptys)
                (loc ?tile (1+ $row) $col)))
      (if (and (setq $empty1-coord (check-coord (1- $row) $col))
               (member $empty1-coord $emptys :test #'equal)
               (setq $empty2-coord (check-coord $row (1- $col)))
               (member $empty2-coord $emptys :test #'equal))
        (assert (setq $direction 'up)
                (setq $new-emptys (copy-list $emptys))
                (alexandria:deletef $new-emptys $empty1-coord :test #'equal)
                (alexandria:deletef $new-emptys $empty2-coord :test #'equal)
                (push (cons (1+ $row) $col) $new-emptys)
                (push (cons (1+ $row) (1- $col)) $new-emptys)
                (setq $new-emptys (sort-coords $new-emptys))
                (emptys $new-emptys)
                (loc ?tile (1- $row) $col)))))


(define-init
  (loc SQ 3 2)  ;initial locations of the tiles
  (loc HOR 1 2)
  (loc VER 2 3)
  (loc L1 0 1)
  (loc YL2 2 1)
  (emptys ((0 . 0) (0 . 2) (0 . 3) (2 . 0) (2 . 2)))  ;locations of the empty spaces
  (YL2-goal 0 3))


(define-goal
  (loc YL2 0 3))

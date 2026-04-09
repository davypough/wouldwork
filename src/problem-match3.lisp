;;; Filename: problem-match3.lisp

;;; Match-3 puzzle: swap adjacent tiles on a grid to create lines of 3+ matching
;;; symbols. Matched tiles are removed, gravity pulls remaining tiles down, and
;;; cascading matches resolve until the board is stable. A swap is only legal if
;;; it produces at least one match. Objective: clear all tiles in minimum moves.
;;;
;;; Grid orientation: row 0 is top, row max-row is bottom.
;;; Gravity pulls tiles toward higher row numbers.
;;;
;;; To adapt for a different puzzle instance, change:
;;;   1. define-types row/col to match grid dimensions
;;;   2. the board grid in define-init
;;;   3. depth-cutoff as appropriate


(in-package :ww)


(ww-set *problem-name* match3)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 20)


(define-types
  row (0 1 2 3 4 5 6 7)
  col (0 1 2 3 4 5 6 7 8))


(define-dynamic-relations
  (cell row col $symbol))  ; (cell row col symbol)


(define-static-relations
  (board $list)             ; initial grid: list of rows, each row a list of symbol values
  (max-row $fixnum)         ; highest valid row index
  (max-col $fixnum)         ; highest valid col index
  (fixed row col))          ; marks a position as immovable (blocks gravity, not swappable)


;;;; UPDATE FUNCTIONS ;;;;


(define-update propagate-changes! ()
  ;; Match->remove->gravity cascade loop.
  ;; Preconditions guarantee at least one match exists on the first pass.
  ;; Loop handles cascading matches after gravity until stable.
  (ww-loop for $iteration from 1 to 20 do
    (setq $matches (find-all-matches))
    (if (not $matches)
      (return t))
    (remove-matched-cells! $matches)
    (apply-gravity!)
  finally (return t)))


(define-update remove-matched-cells! (?matches)
  ;; Retract all cell assertions at matched positions.
  (ww-loop for $pos in ?matches do
    (setq $r (car $pos))
    (setq $c (cdr $pos))
    (bind (cell $r $c $sym))
    (not (cell $r $c $sym))))


(define-update gravity-segment! (?col ?from ?to)
  ;; Compact tiles downward within a single column segment [?from..?to].
  ;; Segment boundaries are defined by fixed cells or board edges.
  (do
    (setq $symbols nil)
    (ww-loop for $r from ?from to ?to do
      (if (bind (cell $r ?col $sym))
        (do (push $sym $symbols)
            (not (cell $r ?col $sym)))))
    (setq $target-row ?to)
    (ww-loop for $sym in $symbols do
      (cell $target-row ?col $sym)
      (decf $target-row))))


(define-update apply-gravity! ()
  ;; For each column, find segments separated by fixed cells and
  ;; compact tiles downward within each segment independently.
  (do
    (bind (max-row $max-row))
    (doall (?c col)
      (do
        (setq $seg-start 0)
        (doall (?r row)
          (if (fixed ?r ?c)
            (do
              (if (< $seg-start ?r)
                (gravity-segment! ?c $seg-start (1- ?r)))
              (setq $seg-start (1+ ?r)))))
        (if (<= $seg-start $max-row)
          (gravity-segment! ?c $seg-start $max-row))))))


;;;; QUERY FUNCTIONS ;;;;


(define-query count-run (?r ?c ?dr ?dc ?sym)
  ;; Count consecutive cells matching ?sym starting one step from (?r,?c)
  ;; in direction (?dr,?dc). Returns 0, 1, or 2 (capped — sufficient for
  ;; detecting runs of 3).
  (do
    (bind (max-row $max-row))
    (bind (max-col $max-col))
    (setq $r1 (+ ?r ?dr))
    (setq $c1 (+ ?c ?dc))
    (if (or (< $r1 0) (> $r1 $max-row) (< $c1 0) (> $c1 $max-col))
      (return-from count-run 0))
    (if (not (bind (cell $r1 $c1 $s1)))
      (return-from count-run 0))
    (if (not (eql $s1 ?sym))
      (return-from count-run 0))
    (setq $r2 (+ $r1 ?dr))
    (setq $c2 (+ $c1 ?dc))
    (if (or (< $r2 0) (> $r2 $max-row) (< $c2 0) (> $c2 $max-col))
      (return-from count-run 1))
    (if (not (bind (cell $r2 $c2 $s2)))
      (return-from count-run 1))
    (if (eql $s2 ?sym) 2 1)))


(define-query position-matches? (?r ?c ?sym ?away-dr ?away-dc ?perp-dr ?perp-dc)
  ;; Does placing ?sym at (?r,?c) create a run of 3+?
  ;; Check parallel (away) direction, then perpendicular (both directions).
  (do
    (setq $away-count (count-run ?r ?c ?away-dr ?away-dc ?sym))
    (if (>= $away-count 2)
      (return-from position-matches? t))
    (setq $perp-pos (count-run ?r ?c ?perp-dr ?perp-dc ?sym))
    (setq $perp-neg (count-run ?r ?c (- ?perp-dr) (- ?perp-dc) ?sym))
    (>= (+ $perp-pos $perp-neg) 2)))


(define-query find-all-matches ()
  ;; Scan all rows for horizontal runs and all columns for vertical runs
  ;; of 3+ same symbol. Returns deduplicated list of (row . col) positions
  ;; to remove, or NIL if no matches found.
  (do
    (setq $to-remove nil)
    ;; --- Horizontal runs ---
    (doall (?r row)
      (do
        (setq $run-len 0)
        (setq $run-sym nil)
        (setq $run-start-col 0)
        (setq $prev-col -2)
        (doall (?c col)
          (if (bind (cell ?r ?c $sym))
            (do
              (if (and (eql $sym $run-sym) (= ?c (1+ $prev-col)))
                (do (incf $run-len)
                    (setq $prev-col ?c))
                (do (if (>= $run-len 3)
                      (ww-loop for $k from $run-start-col to $prev-col do
                        (pushnew (cons ?r $k) $to-remove :test #'equal)))
                    (setq $run-len 1)
                    (setq $run-sym $sym)
                    (setq $run-start-col ?c)
                    (setq $prev-col ?c))))
            (do (if (>= $run-len 3)
                  (ww-loop for $k from $run-start-col to $prev-col do
                    (pushnew (cons ?r $k) $to-remove :test #'equal)))
                (setq $run-len 0)
                (setq $run-sym nil)
                (setq $prev-col -2))))
        ;; Close final run in this row
        (if (>= $run-len 3)
          (ww-loop for $k from $run-start-col to $prev-col do
            (pushnew (cons ?r $k) $to-remove :test #'equal)))))
    ;; --- Vertical runs ---
    (doall (?c col)
      (do
        (setq $run-len 0)
        (setq $run-sym nil)
        (setq $run-start-row 0)
        (setq $prev-row -2)
        (doall (?r row)
          (if (bind (cell ?r ?c $sym))
            (do
              (if (and (eql $sym $run-sym) (= ?r (1+ $prev-row)))
                (do (incf $run-len)
                    (setq $prev-row ?r))
                (do (if (>= $run-len 3)
                      (ww-loop for $k from $run-start-row to $prev-row do
                        (pushnew (cons $k ?c) $to-remove :test #'equal)))
                    (setq $run-len 1)
                    (setq $run-sym $sym)
                    (setq $run-start-row ?r)
                    (setq $prev-row ?r))))
            (do (if (>= $run-len 3)
                  (ww-loop for $k from $run-start-row to $prev-row do
                    (pushnew (cons $k ?c) $to-remove :test #'equal)))
                (setq $run-len 0)
                (setq $run-sym nil)
                (setq $prev-row -2))))
        ;; Close final run in this column
        (if (>= $run-len 3)
          (ww-loop for $k from $run-start-row to $prev-row do
            (pushnew (cons $k ?c) $to-remove :test #'equal)))))
    $to-remove))


;;;; ACTIONS ;;;;


(define-action swap-right
    1
  (product ?row row ?col col)
  (and (bind (max-col $max-col))
       (< ?col $max-col)
       (setq $next-col (1+ ?col))
       (not (fixed ?row ?col))
       (not (fixed ?row $next-col))
       (bind (cell ?row ?col $sym1))
       (bind (cell ?row $next-col $sym2))
       (not (eql $sym1 $sym2))
       ;; sym2 placed at (?row,?col): away=left(0,-1), perp=vertical(1,0)
       ;; sym1 placed at (?row,$next-col): away=right(0,1), perp=vertical(1,0)
       (or (position-matches? ?row ?col $sym2 0 -1 1 0)
           (position-matches? ?row $next-col $sym1 0 1 1 0)))
  (?row ?col)
  (assert (cell ?row ?col $sym2)
          (cell ?row $next-col $sym1)
          (finally (propagate-changes!))))


(define-action swap-down
    1
  (product ?row row ?col col)
  (and (bind (max-row $max-row))
       (< ?row $max-row)
       (setq $next-row (1+ ?row))
       (not (fixed ?row ?col))
       (not (fixed $next-row ?col))
       (bind (cell ?row ?col $sym1))
       (bind (cell $next-row ?col $sym2))
       (not (eql $sym1 $sym2))
       ;; sym2 placed at (?row,?col): away=up(-1,0), perp=horizontal(0,1)
       ;; sym1 placed at ($next-row,?col): away=down(1,0), perp=horizontal(0,1)
       (or (position-matches? ?row ?col $sym2 -1 0 0 1)
           (position-matches? $next-row ?col $sym1 1 0 0 1)))
  (?row ?col)
  (assert (cell ?row ?col $sym2)
          (cell $next-row ?col $sym1)
          (finally (propagate-changes!))))


;;;; INITIALIZATION ;;;;


(define-init-action expand-board
    0
  ()
  (bind (board $grid))
  ()
  (assert
    (setq $max-row (1- (length $grid)))
    (setq $max-col (1- (length (car $grid))))
    (max-row $max-row)
    (max-col $max-col)
    (ww-loop for $row-data in $grid
             for $r from 0 do
      (ww-loop for $sym in $row-data
               for $c from 0 do
        (if (eql $sym '-)
          (fixed $r $c)
          (cell $r $c $sym))))))


(define-init
  (board ((- A B - - - C C -)
          (- D D E - B C C -)
          (C C F C - G F D D)
          (D A A G - D E B E)
          (- B E H - A G E -)
          (- E B G H B A G -)
          (- A H G D A C C -)
          (- - - D F D - - -))))


;;;; GOAL ;;;;


(define-goal
  (not (exists (?r row ?c col)
         (bind (cell ?r ?c $sym)))))

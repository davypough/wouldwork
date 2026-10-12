;;; Filename: problem-captjohn-advisor.lisp

;;; Captain John and the Wasp, built with the spec advisor
;;; (see doc/problems/captjohn-advisor/).
;;; First place John, the Wasp, his crew and the guards on the grid so that every
;;; clue holds; every cell left over is grass. Then find a shortest route for John,
;;; one king move at a time, that never revisits a cell or steps onto a guard,
;;; visits every crew mate, and ends on the Wasp.
;;;
;;; Design notes:
;;; - Two searches share this file: placement finds every layout, then the route
;;;   search starts from the saved layout.
;;; - Only the named tokens are placed; grass is simply an empty cell.
;;; - Tokens are placed one at a time in a fixed order, and tokens of the same
;;;   kind take cells in increasing order, so each layout appears only once.
;;; - Each clue is checked as soon as its tokens are on the grid; clues about
;;;   neighboring grass wait until the grid is full.
;;; - Each cell's neighbors are listed once at the start, so clues and moves
;;;   look only at nearby cells.


(in-package :ww)


;;; Settings for the placement search; the route search is set at the REPL.
(ww-set *problem-name* captjohn-advisor)
(ww-set *problem-type* planning)
(ww-set *solution-type* every)
(ww-set *tree-or-graph* tree)
(ww-set *depth-cutoff* 6)
(ww-set *threads* 16)  ;for max speed


(defparameter *captjohn-grid-size* 3)


(defvar *captjohn-layout* nil
  "The layout found by the placement search, saved as facts for the route search.")


(defun cell-name (row col)
  (intern (format nil "R~DC~D" row col)))


(define-types
  coordinate (compute (alexandria:iota *captjohn-grid-size* :start 1))
  cell       (compute (loop for row from 1 to *captjohn-grid-size*
                            append (loop for col from 1 to *captjohn-grid-size*
                                         collect (cell-name row col))))
  captain    (john)
  vessel     (ship)
  guard      (guard1 guard2)
  crew       (crew1 crew2)
  token      (either captain vessel guard crew))


;;; The layout pairs each placed token with its cell; John's current cell and
;;; the cells he has left belong to the route.
(define-dynamic-relations
  (located $token $cell :bijective)
  (next-to-place $token)
  (john-cell $cell)
  (visited cell))


;;; Cell positions and neighbor lists are filled in at the start.
(define-static-relations
  (cell-position> cell $coordinate $coordinate $fixnum)
  (vertical-neighbors cell $list)
  (diagonal-neighbors cell $list)
  (king-neighbors cell $list)
  (next-in-order> token $token)
  (after-same-kind> token $token)
  (fixed-grass cell))


(define-query same-row? (?cell1 cell ?cell2 cell)
  (do (bind (cell-position> ?cell1 $row1 $col1 $index1))
      (bind (cell-position> ?cell2 $row2 $col2 $index2))
      (= $row1 $row2)))


(define-query same-col? (?cell1 cell ?cell2 cell)
  (do (bind (cell-position> ?cell1 $row1 $col1 $index1))
      (bind (cell-position> ?cell2 $row2 $col2 $index2))
      (= $col1 $col2)))


;;; True when the tokens placed so far break a clue, one clue per line.
;;; Clue 7 is a fixed grass cell, so no token is ever placed there.
(define-query clue-broken? ()
  (or
    ;; 1. The ship is not in John's row or column.
    (and (bind (located john $john))
         (bind (located ship $ship))
         (or (same-row? $john $ship) (same-col? $john $ship)))
    ;; 2. John is not in the row or column of either guard.
    (and (bind (located john $john))
         (exists (?guard guard)
           (and (bind (located ?guard $guard))
                (or (same-row? $john $guard) (same-col? $john $guard)))))
    ;; 3. No guard is in column 3.
    (exists (?guard guard)
      (and (bind (located ?guard $guard))
           (bind (cell-position> $guard $row $col $index))
           (= $col 3)))
    ;; 5. The ship shares a row with one guard and a column with the other.
    (and (bind (located ship $ship))
         (forall (?guard guard)
           (bind (located ?guard $guard)))
         (not (exists ((?guard1 ?guard2) guard)
                (and (bind (located ?guard1 $guard1))
                     (bind (located ?guard2 $guard2))
                     (same-row? $ship $guard1)
                     (same-col? $ship $guard2)))))
    ;; 8. The two guards share no row or column.
    (exists ((?guard1 ?guard2) guard)
      (and (bind (located ?guard1 $guard1))
           (bind (located ?guard2 $guard2))
           (or (same-row? $guard1 $guard2) (same-col? $guard1 $guard2))))
    ;; The grass clues, once every token is placed.
    (and (not (bind (next-to-place $next)))
         (or
           ;; 4. Each guard has grass directly above or below it.
           (exists (?guard guard)
             (and (bind (located ?guard $guard))
                  (bind (vertical-neighbors $guard $cells))
                  (forall (?cell $cells)
                    (bind (located $token ?cell)))))
           ;; 6. Some grass cell touches both crew mates at a corner.
           (and (bind (located crew1 $crew1))
                (bind (diagonal-neighbors $crew1 $cells))
                (not (exists (?cell $cells)
                       (and (not (bind (located $token ?cell)))
                            (bind (diagonal-neighbors ?cell $around))
                            (forall (?crew crew)
                              (and (bind (located ?crew $crew))
                                   (member $crew $around)))))))))))


(define-update check-clues! ()
  (if (clue-broken?)
    (inconsistent-state)))


;;; Place the next token on any empty cell that is not fixed grass; a token
;;; following another of its kind takes a later cell than that one.
(define-action place 1
  ()
  (bind (next-to-place $token))
  ($token "at" $cell)
  (do
    (if (bind (after-same-kind> $token $earlier))
      (do (bind (located $earlier $earlier-cell))
          (bind (cell-position> $earlier-cell $row $col $floor)))
      (setq $floor 0))
    (doall (?cell cell)
      (if (and (not (fixed-grass ?cell))
               (not (bind (located $occupant ?cell)))
               (bind (cell-position> ?cell $row $col $index))
               (> $index $floor))
        (assert (setq $cell ?cell)
                (located $token ?cell)
                (if (bind (next-in-order> $token $following))
                  (next-to-place $following)
                  (not (next-to-place $token)))
                (if (captain $token)
                  (john-cell ?cell))
                (check-clues!))))))


;;; Move John one king step to an unvisited cell that holds no guard; the
;;; ship only once every crew mate's cell has been visited or is being left.
(define-action move 1
  ()
  (and (not (bind (next-to-place $next)))
       (bind (john-cell $from)))
  ($from "to" $to)
  (do (bind (king-neighbors $from $cells))
      (doall (?cell $cells)
        (if (and (not (visited ?cell))
                 (or (not (bind (located $occupant ?cell)))
                     (crew $occupant)
                     (and (vessel $occupant)
                          (forall (?crew crew)
                            (and (bind (located ?crew $crew-cell))
                                 (or (visited $crew-cell) (eql $crew-cell $from)))))))
          (assert (setq $to ?cell)
                  (visited $from)
                  (john-cell ?cell))))))


(define-init
  (next-to-place guard1)
  (next-in-order> guard1 guard2)
  (next-in-order> guard2 john)
  (next-in-order> john ship)
  (next-in-order> ship crew1)
  (next-in-order> crew1 crew2)
  (after-same-kind> guard2 guard1)
  (after-same-kind> crew2 crew1)
  (fixed-grass R1C2))


;;; Cell positions follow from the grid size; the index orders cells row by row.
(define-init-action initialize-cell-positions 0
  (product ?row coordinate ?col coordinate)
  (always-true)
  ()
  (assert (setq $cell (cell-name ?row ?col))
          (cell-position> $cell ?row ?col (+ (* (1- ?row) *captjohn-grid-size*) ?col))))


;;; Lists each cell's vertical, diagonal and king-move neighbors once.
(define-init-action initialize-neighbors 0
  (?cell cell)
  (always-true)
  ()
  (do (bind (cell-position> ?cell $row $col $index))
      (assert
        (doall (?other cell)
          (do (bind (cell-position> ?other $other-row $other-col $other-index))
              (mv-assign ($row-gap $col-gap)
                         (values (abs (- $other-row $row)) (abs (- $other-col $col))))
              (if (and (<= $row-gap 1) (<= $col-gap 1) (different ?other ?cell))
                (setq $king (cons ?other $king)))
              (if (and (= $row-gap 1) (= $col-gap 0))
                (setq $vertical (cons ?other $vertical)))
              (if (and (= $row-gap 1) (= $col-gap 1))
                (setq $diagonal (cons ?other $diagonal)))))
        (vertical-neighbors ?cell $vertical)
        (diagonal-neighbors ?cell $diagonal)
        (king-neighbors ?cell $king))))


;;; Every token is placed; the clue checks have already dropped any layout
;;; that breaks a clue.
(define-goal
  (not (bind (next-to-place $next))))

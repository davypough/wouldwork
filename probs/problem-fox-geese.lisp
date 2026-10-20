;;; Filename: problem-fox-geese.lisp

;;; Fox and geese on an n x n checkerboard, played only on squares of one color.
;;; The geese start on every playing square of row 1 and step diagonally toward
;;; row n, one goose per turn. The fox starts on a playing square of row n of its
;;; choice and steps diagonally in any direction. Pieces never share a square and
;;; there are no captures. A side passes only when it has no move. The fox wins by
;;; reaching row 1; the geese win when the fox has no move on its turn.
;;; In this version both sides choose their moves freely.
;;;
;;; Design notes:
;;; - Only playing-color squares exist, so every move stays on the playing color.
;;; - A position records just the fox's square, each goose's square, and whose turn
;;;   it is; empty squares and a trapped fox are worked out when needed.
;;; - Board size, playing color and first mover are settings: change them and
;;;   restage to play another variant.


(in-package :ww)


(ww-set *problem-name* fox-geese)
(ww-set *problem-type* planning)
(ww-set *tree-or-graph* graph)
(ww-set *symmetry-pruning* t)


(defparameter *n* 8)  ;board size, 3 to 10


(defparameter *plays-r1c1-color* t)  ;T plays on the color of row 1, column 1; NIL the other


(defparameter *fox-first* t)  ;T the fox moves first; NIL the geese


(defun square-name (row col)
  (intern (format nil "R~DC~D" row col)))


(define-types
  coordinate (compute (alexandria:iota *n* :start 1))
  goose  (compute (loop for col from 1 to *n*
                        when (eq (evenp (1+ col)) *plays-r1c1-color*)
                          collect (intern (format nil "GOOSE~D" col))))
  square (compute (loop for row from 1 to *n*
                        append (loop for col from 1 to *n*
                                     when (eq (evenp (+ row col)) *plays-r1c1-color*)
                                       collect (square-name row col)))))


(define-dynamic-relations
  (fox-at $square)
  (goose-at $goose $square :bijective)
  (geese-to-move))


;;; Board facts, filled in at the start from the settings.
(define-static-relations
  (square-position> square $coordinate $coordinate)  ;row, column
  (fox-steps square $list)    ;diagonal neighbors in all four directions
  (goose-steps square $list))  ;diagonal neighbors one row toward row n


(define-query geese-can-move? ()
  (do (bind (fox-at $fox))
      (exists (?goose goose)
        (do (bind (goose-at ?goose $from))
            (bind (goose-steps $from $steps))
            (exists (?to $steps)
              (and (different ?to $fox)
                   (not (bind (goose-at $other ?to)))))))))


;;; At least one step per row the fox still has to cross, plus its placement
;;; before it is on the board.
(define-query min-steps-remaining? ()
  (if (bind (fox-at $fox))
    (do (bind (square-position> $fox $row $col))
        (1- $row))
    *n*))


(define-action place-fox 1
  (?square square)
  (and (not (bind (fox-at $fox)))
       (bind (square-position> ?square $row $col))
       (= $row *n*))
  (?square)
  (assert (fox-at ?square)
          (if (not *fox-first*)
            (geese-to-move))))


(define-action move-fox 1
  ()
  (and (not (geese-to-move))
       (bind (fox-at $from)))
  ($from "to" $to)
  (do (bind (fox-steps $from $steps))
      (doall (?to $steps)
        (if (not (bind (goose-at $goose ?to)))
          (assert (assign $to ?to)
                  (fox-at ?to)
                  (geese-to-move))))))


(define-action move-goose 1
  (?goose goose)
  (and (geese-to-move)
       (bind (goose-at ?goose $from)))
  (?goose $from "to" $to)
  (do (bind (fox-at $fox))
      (bind (goose-steps $from $steps))
      (doall (?to $steps)
        (if (and (different ?to $fox)
                 (not (bind (goose-at $other ?to))))
          (assert (assign $to ?to)
                  (goose-at ?goose ?to)
                  (not (geese-to-move)))))))


;;; The geese pass only when no goose can move.
(define-action pass-geese 1
  ()
  (and (geese-to-move)
       (not (geese-can-move?)))
  ()
  (assert (not (geese-to-move))))


(define-init-action initialize-square-positions 0
  (product ?row coordinate ?col coordinate)
  (eq (evenp (+ ?row ?col)) *plays-r1c1-color*)
  ()
  (assert (square-position> (square-name ?row ?col) ?row ?col)))


(define-init-action initialize-steps 0
  (?square square)
  (always-true)
  ()
  (do (bind (square-position> ?square $row $col))
      (assert
        (doall (?other square)
          (do (bind (square-position> ?other $other-row $other-col))
              (if (= (abs (- $other-col $col)) 1)
                (do (if (= (abs (- $other-row $row)) 1)
                      (assign $fox-steps (cons ?other $fox-steps)))
                    (if (= $other-row (1+ $row))
                      (assign $goose-steps (cons ?other $goose-steps)))))))
        (fox-steps ?square $fox-steps)
        (goose-steps ?square $goose-steps))))


;;; Each goose starts on the row-1 square of the column it is named for.
(define-init-action initialize-geese 0
  (?col coordinate)
  (eq (evenp (1+ ?col)) *plays-r1c1-color*)
  ()
  (assert (goose-at (intern (format nil "GOOSE~D" ?col)) (square-name 1 ?col))))


;;; The fox wins by reaching row 1. A trapped fox has no move, so its path ends there.
(define-goal
  (and (bind (fox-at $fox))
       (bind (square-position> $fox $row $col))
       (= $row 1)))

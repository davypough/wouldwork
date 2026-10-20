;;; Filename: problem-fox-geese-geese-check.lisp

;;; Fox and geese, geese strategy check. Same game as problem-fox-geese.lisp: an
;;; n x n checkerboard played on one color; the geese start on every playing square
;;; of row 1 and step diagonally toward row n; the fox starts on a row-n square of its
;;; choice and steps diagonally in any direction; no captures; a side passes only when
;;; it has no move. The fox wins by reaching row 1; the geese win by trapping the fox.
;;; Here the geese do not choose: right after each fox move they reply by a fixed
;;; rule, and the search explores only the fox's choices. A solution is a line on
;;; which the fox beats the rule. An exhaustive search that finds none proves the
;;; rule wins for the geese from every fox start square.
;;;
;;; Design notes:
;;; - Each step is a fox move together with the geese's reply, so the search branches
;;;   only on the fox's choices.
;;; - The geese are unnamed: a position records only which squares hold geese, so
;;;   positions that differ only in which goose stands where are the same position,
;;;   and the rule ranks goose moves by the squares involved.
;;; - A line counts as a fox win as soon as the win is certain (the fox is past every
;;;   goose, or the geese are frozen by each other), not only when it reaches row 1.
;;; - Board size, playing color and first mover are settings: change them and
;;;   restage to check another variant.


(in-package :ww)


(ww-set *problem-name* fox-geese-geese-check)
(ww-set *problem-type* planning)
(ww-set *tree-or-graph* graph)
(ww-set *solution-type* first)
(ww-set *symmetry-pruning* nil)


(defparameter *n* 8)  ;board size, 3 to 10


(defparameter *plays-r1c1-color* t)  ;T plays on the color of row 1, column 1; NIL the other


(defparameter *fox-first* nil)  ;T the fox moves first; NIL the geese


(defparameter *lookahead* 4)  ;exchanges the geese's rule looks ahead (rule revision 3)


(defun square-name (row col)
  (intern (format nil "R~DC~D" row col)))


(define-types
  coordinate (compute (alexandria:iota *n* :start 1))
  square (compute (loop for row from 1 to *n*
                        append (loop for col from 1 to *n*
                                     when (eq (evenp (+ row col)) *plays-r1c1-color*)
                                       collect (square-name row col)))))


(define-dynamic-relations
  (fox-at $square)
  (goose-on square))


;;; Board facts, filled in at the start from the settings.
(define-static-relations
  (square-position> square $coordinate $coordinate)  ;row, column
  (fox-steps square $list)    ;diagonal neighbors in all four directions
  (goose-steps square $list))  ;diagonal neighbors one row toward row n


;;; The geese's reply when the fox stands on ?fox: (goose-square target-square), or
;;; NIL to pass when no goose can move. Every legal goose move is scored and the lowest
;;; score is played. The score's parts, most important first:
;;;   1. the fox's walk to row 1 is longest: the walk the fox would need, geese frozen,
;;;      after *lookahead* more exchanges in which the fox shortens it and the geese
;;;      lengthen it as much as they can (walk-value?);
;;;   2. the fox's walk to row 1 right now, geese frozen, is longest (fox-distance?);
;;;   3. goose nearest row 1;
;;;   4. leftmost goose;
;;;   5. leftmost target.
;;; Parts 3-5 identify the move by its squares, so no two moves tie.
(define-query geese-reply? (?fox square)
  (do (doall (?square square)
        (if (goose-on ?square)
          (assign $geese (cons ?square $geese))))
      (assign $moves (goose-moves? ?fox $geese))
      (assign $depth *lookahead*)
      (doall (?move $moves)
        (do (assign $from (first ?move))
            (assign $to (second ?move))
            (assign $after (substitute $to $from $geese))
            (bind (square-position> $from $from-row $from-col))
            (bind (square-position> $to $to-row $to-col))
            (assign $score (- 1000 (walk-value? ?fox $after $depth)))
            (assign $score (+ (* $score 128) (- 100 (fox-distance? ?fox $after))))
            (assign $score (+ (* $score 16) $from-row))
            (assign $score (+ (* $score 16) $from-col))
            (assign $score (+ (* $score 16) $to-col))
            (if (or (null $best-score) (< $score $best-score))
              (do (assign $best-score $score)
                  (assign $reply ?move)))))
      $reply))


;;; With the fox on ?fox to move and the geese on the squares ?geese: the fox's walk
;;; to row 1 (fox-distance?) after ?depth exchanges, the fox stepping to make it
;;; shortest and the geese replying to make it longest (a pass when no goose can
;;; move). -1 if the fox can step onto row 1; 999 if the fox has no step.
(define-query walk-value? (?fox ?geese ?depth)
  (if (= ?depth 0)
    (fox-distance? ?fox ?geese)
    (do (assign $value 999)
        (bind (fox-steps ?fox $steps))
        (doall (?next $steps)
          (if (not (member ?next ?geese))
            (do (bind (square-position> ?next $row $col))
                (if (= $row 1)
                  (assign $value -1)
                  (do (assign $moves (goose-moves? ?next ?geese))
                      (if $moves
                        (do (assign $best -1)
                            (doall (?move $moves)
                              (assign $best
                                      (max $best
                                           (walk-value? ?next
                                                        (substitute (second ?move) (first ?move) ?geese)
                                                        (1- ?depth))))))
                        (assign $best (walk-value? ?next ?geese (1- ?depth))))
                      (assign $value (min $value $best)))))))
        $value)))


;;; With the fox on ?fox and the geese on the squares ?geese: every legal goose
;;; move, as (goose-square target-square).
(define-query goose-moves? (?fox ?geese)
  (do (assign $geese ?geese)
      (doall (?from $geese)
        (do (bind (goose-steps ?from $steps))
            (doall (?to $steps)
              (if (and (different ?to ?fox)
                       (not (member ?to ?geese)))
                (assign $moves (cons (list ?from ?to) $moves))))))
      $moves))


;;; With the fox on ?fox and the geese on the squares ?geese, all frozen: the fewest
;;; fox steps to reach row 1, or 99 if the fox cannot reach it.
(define-query fox-distance? (?fox ?geese)
  (do (assign $frontier (list ?fox))
      (assign $visited (list ?fox))
      (assign $distance 0)
      (ww-loop while $frontier do
        (assign $distance (1+ $distance))
        (assign $next-frontier nil)
        (ww-loop for $square in $frontier do
          (bind (fox-steps $square $steps))
          (doall (?next $steps)
            (if (not (or (member ?next $visited)
                         (member ?next ?geese)))
              (do (assign $visited (cons ?next $visited))
                  (assign $next-frontier (cons ?next $next-frontier))
                  (bind (square-position> ?next $row $col))
                  (if (= $row 1)
                    (assign $result $distance))))))
        (if $result
          (assign $frontier nil)
          (assign $frontier $next-frontier)))
      (if (null $result)
        (assign $result 99))
      $result))


;;; The fox chooses its start square; if the geese move first, their opening reply
;;; is played in the same step.
(define-action place-fox 1
  (?square square)
  (and (not (bind (fox-at $fox)))
       (bind (square-position> ?square $row $col))
       (= $row *n*))
  (?square "geese" $goose-from $goose-to)
  (do (if (not *fox-first*)
        (assign $reply (geese-reply? ?square)))
      (assert (fox-at ?square)
              (assign $goose-from (first $reply))
              (assign $goose-to (second $reply))
              (if $reply
                (do (not (goose-on $goose-from))
                    (goose-on $goose-to))))))


;;; The fox steps to a free diagonal neighbor and the geese reply by the rule.
;;; A trapped fox has no step, so its line ends there.
(define-action move-fox 1
  ()
  (bind (fox-at $from))
  ($from "to" $to "geese" $goose-from $goose-to)
  (do (bind (fox-steps $from $steps))
      (doall (?to $steps)
        (if (not (goose-on ?to))
          (do (assign $reply (geese-reply? ?to))
              (assert (assign $to ?to)
                      (fox-at ?to)
                      (assign $goose-from (first $reply))
                      (assign $goose-to (second $reply))
                      (if $reply
                        (do (not (goose-on $goose-from))
                            (goose-on $goose-to)))))))))


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


;;; A goose starts on every playing square of row 1.
(define-init-action initialize-geese 0
  (?col coordinate)
  (eq (evenp (1+ ?col)) *plays-r1c1-color*)
  ()
  (assert (goose-on (square-name 1 ?col))))


;;; The fox wins on row 1, or as soon as its walk there is certain:
;;; S1 - every goose is on a higher row than the fox (geese never move back);
;;; S2 - every goose's forward squares all hold geese (the geese never move again,
;;;      and the fox always has an empty diagonal toward row 1).
(define-goal
  (and (bind (fox-at $fox))
       (bind (square-position> $fox $row $col))
       (or (= $row 1)
           (forall (?square square)
             (or (not (goose-on ?square))
                 (and (bind (square-position> ?square $goose-row $goose-col))
                      (> $goose-row $row))))
           (forall (?square square)
             (or (not (goose-on ?square))
                 (and (bind (goose-steps ?square $steps))
                      (forall (?next $steps)
                        (goose-on ?next))))))))

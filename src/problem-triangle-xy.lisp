;;; Filename: problem-triangle-xy.lisp


;;; Basic problem specification for triangle peg problem with peg-count.

;;; The peg board positions have coordinates measured 
;;; from the triangle's right diagonal (/) and left diagonal (\)
;;; side lengths from 1 to *N* with pegs in all positions except 11.
;;; Jumps are by / up or down, \ up or down, - (horiz) right or left.
;;;         11
;;;       12  21
;;;     13  22  31
;;;   14  23  32  41
;;; 15  24  33  42  51


(in-package :ww)  ;required

(ww-set *problem-name* triangle-xy)

(ww-set *problem-type* planning)

(ww-set *tree-or-graph* tree)

(ww-set *solution-type* first)


(defparameter *N* 5)  ;the number of pegs on a side

(defparameter *holes* '((1 1)))  ;coordinates of the initial holes


(define-types
  peg (compute (loop for i from 1  ;peg1, peg2, ...
                     to (- (/ (* *N* (1+ *N*)) 2) (length *holes*))
                   collect (intern (format nil "PEG~D" i))))
  row (compute (loop for i from 1 to *N*
                   collect i))
  col (compute (loop for j from 1 to *N*
                   collect j)))


(define-dynamic-relations
    (loc peg $fixnum $fixnum)      ;location of a peg
    (contents row col $peg)  ;peg contents at a location
    (remaining-pegs $list)   ;list of remaining pegs
    (peg-count $integer))    ;pegs remaining on the board


(define-query get-remaining-pegs? ()
  (do (bind (remaining-pegs $pegs))
      $pegs))


(define-action jump-left-down  ;jump downward in the / diagonal direction
    1
  (?peg (get-remaining-pegs?))  ;function type
  (and (bind (loc ?peg $r $c))
       (<= (+ $r $c) (- *N* 1))
       (assign $c+1 (1+ $c))
       (bind (contents $r $c+1 $adj-peg))
       (assign $c+2 (+ $c 2))
       (not (bind (contents $r $c+2 $any-peg)))
       (bind (peg-count $peg-count))
       (bind (remaining-pegs $pegs)))
  ($r $c)
  (assert (not (contents $r $c ?peg))  ;from
          (loc ?peg $r $c+2)           ;to
          (contents $r $c+2 ?peg)      ;update to
          (not (loc $adj-peg $r $c+1))       ;remove adj peg
          (not (contents $r $c+1 $adj-peg))  ;remove adj peg
          (peg-count (1- $peg-count))
          (remaining-pegs (remove $adj-peg $pegs))))


(define-action jump-right-up  ;jump upward in the / diagonal direction
    1
  (?peg (get-remaining-pegs?))  ;named type
  (and (bind (loc ?peg $r $c))
       (>= $c 3)
       (assign $c-1 (1- $c))
       (bind (contents $r $c-1 $adj-peg))
       (assign $c-2 (- $c 2))
       (not (bind (contents $r $c-2 $any-peg)))
       (bind (peg-count $peg-count))
       (bind (remaining-pegs $pegs)))
  ($r $c)
  (assert (not (contents $r $c ?peg))
          (loc ?peg $r $c-2)
          (contents $r $c-2 ?peg)
          (not (loc $adj-peg $r $c-1))
          (not (contents $r $c-1 $adj-peg))
          (peg-count (1- $peg-count))
          (remaining-pegs (remove $adj-peg $pegs))))


(define-action jump-right-down  ;jump downward in the \ diagonal direction
    1
  (?peg (get-remaining-pegs?))
  (and (bind (loc ?peg $r $c))
       (<= (+ $r $c) (- *N* 1))
       (assign $r+1 (+ $r 1))
       (bind (contents $r+1 $c $adj-peg))
       (assign $r+2 (+ $r 2))
       (not (bind (contents $r+2 $c $any-peg)))
       (bind (peg-count $peg-count))
       (bind (remaining-pegs $pegs)))
  ($r $c)
  (assert (not (contents $r $c ?peg))
          (loc ?peg $r+2 $c)
          (contents $r+2 $c ?peg)
          (not (loc $adj-peg $r+1 $c))
          (not (contents $r+1 $c $adj-peg))
          (peg-count (1- $peg-count))
          (remaining-pegs (remove $adj-peg $pegs))))


(define-action jump-left-up  ;jump upward in the \ diagonal direction
    1
  (?peg (get-remaining-pegs?))
  (and (bind (loc ?peg $r $c))
       (>= $r 3)
       (assign $r-1 (- $r 1))
       (bind (contents $r-1 $c $adj-peg))
       (assign $r-2 (- $r 2))
       (not (bind (contents $r-2 $c $any-peg)))
       (bind (peg-count $peg-count))
       (bind (remaining-pegs $pegs)))
  ($r $c)
  (assert (not (contents $r $c ?peg))
          (loc ?peg $r-2 $c)
          (contents $r-2 $c ?peg)
          (not (loc $adj-peg $r-1 $c))
          (not (contents $r-1 $c $adj-peg))
          (peg-count (1- $peg-count))
          (remaining-pegs (remove $adj-peg $pegs))))



(define-action jump-right-horiz  ;jump rightward in the horizontal direction
    1
  (?peg (get-remaining-pegs?))
  (and (bind (loc ?peg $r $c))
       (>= $c 3)
       (assign $r+1 (+ $r 1))
       (assign $c-1 (- $c 1))
       (bind (contents $r+1 $c-1 $adj-peg))
       (assign $r+2 (+ $r 2))
       (assign $c-2 (- $c 2))
       (not (bind (contents $r+2 $c-2 $any-peg)))
       (bind (peg-count $peg-count))
       (bind (remaining-pegs $pegs)))
  ($r $c)
  (assert (not (contents $r $c ?peg))
          (loc ?peg $r+2 $c-2)
          (contents $r+2 $c-2 ?peg)
          (not (loc $adj-peg $r+1 $c-1))
          (not (contents $r+1 $c-1 $adj-peg))
          (peg-count (1- $peg-count))
          (remaining-pegs (remove $adj-peg $pegs))))


(define-action jump-left-horiz  ;jump leftward in the horizontal direction
    1
  (?peg (get-remaining-pegs?))
  (and (bind (loc ?peg $r $c))
       (>= $r 3)
       (assign $r-1 (- $r 1))
       (assign $c+1 (+ $c 1))
       (bind (contents $r-1 $c+1 $adj-peg))
       (assign $r-2 (- $r 2))
       (assign $c+2 (+ $c 2))
       (not (bind (contents $r-2 $c+2 $any-peg)))
       (bind (peg-count $peg-count))
       (bind (remaining-pegs $pegs)))
  ($r $c)
  (assert (not (contents $r $c ?peg))
          (loc ?peg $r-2 $c+2)
          (contents $r-2 $c+2 ?peg)
          (not (loc $adj-peg $r-1 $c+1))
          (not (contents $r-1 $c+1 $adj-peg))
          (peg-count (1- $peg-count))
          (remaining-pegs (remove $adj-peg $pegs))))
      

(progn (format t "~&Initializing database...~%")
  (loop with pegs = (gethash 'peg *types*)
    ;*db* is the name of the initial database
    ;update is the function that asserts a proposition
    ;into the database
    initially (update *db* `(peg-count ,(length pegs)))
              (update *db* `(remaining-pegs ,pegs))
    for row from 1 to *N*
      do (loop for col from 1 to (- (1+ *N*) row)
             unless (member (list row col) *holes* :test #'equal)
             do (let ((peg (pop pegs)))
                  (update *db* `(loc ,peg ,row ,col))
                  (update *db* `(contents ,row ,col ,peg))))))


(define-goal  ;only one peg left
  (peg-count 1))

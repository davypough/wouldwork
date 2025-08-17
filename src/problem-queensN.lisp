;;;; Filename: problem-queensN.lisp


;;; Problem specification for N-queens.


(in-package :ww)  ;required


(defparameter *N* 4)


(ww-set *problem-name* queensN)
(ww-set *problem-type* csp)
(ww-set *algorithm* backtracking)
(ww-set *solution-type* every)
(ww-set *tree-or-graph* tree)
(ww-set *depth-cutoff* 4)


(define-types
  queen-row (compute (loop for i from 1 to *N* collect i))
  column    (compute (loop for j from 1 to *N* collect j)))


(define-dynamic-relations
  (remaining queen-row $list)
  (assigned queen-row $column)
  (next-row $fixnum))


(define-query get-remaining-columns? (?row)
  (do (bind (remaining ?row $cols))
      $cols))


(define-query conflict-with? (?row ?col)
  (exists (?placed-row queen-row)
    (and (bind (assigned ?placed-row $placed-col))
         (< ?placed-row ?row)
         (or (= ?col $placed-col)
             (= (abs (- ?row ?placed-row)) 
                (abs (- ?col $placed-col)))))))


(define-update make-queen-assignment! (?row ?col)
  (do (assigned ?row ?col)
      (doall (?r queen-row)
        (do (bind (remaining ?r $initial-cols))
            (if (= ?r ?row)
              (remaining ?r (list ?col))
              (remaining ?r (remove ?col $initial-cols)))))))


(define-action assign-queen-to-col
  1
  (?col column)
  (and (bind (next-row $current-row))
       (<= $current-row *N*)
       (setq $remaining-columns (get-remaining-columns? $current-row))
       (member ?col $remaining-columns)
       (not (conflict-with? $current-row ?col)))
  (?col)
  (assert (make-queen-assignment! $current-row ?col)
          (next-row (1+ $current-row))))
   

(define-init
  (next-row 1))


(define-init-action initialize-remaining-columns
  0
  (?row queen-row)
  (always-true)
  ()
  (assert (remaining ?row (loop for col from 1 to *N* collect col))))  ;recomputed every time, ok in init-action


(define-goal
  (and (bind (next-row $row))
       (> $row *N*)))

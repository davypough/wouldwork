;;; Filename: problem-knap4a.lisp

;;; Problem specification for a 4-item knapsack problem.
;;; First iteration.
;;; Basic definitions.


(in-package :ww)


(ww-set *problem-name* knap4a)

(ww-set *problem-type* planning)

(ww-set *tree-or-graph* tree)

(ww-set *solution-type* max-value)

;(ww-set *progress-reporting-interval* 10)  ;for debugging


(defparameter *items-values-weights*
  '((item1 10 2) (item2 10 4) (item3 12 6) (item4 18 9)))


(defparameter *sorted-items-values-weights*  ;sort according to value/weight ratio
  (sort (copy-list *items-values-weights*) #'>
        :key (lambda (x)
               (/ (second x) (third x)))))


(define-types
  item (compute (iter (for (item nil nil) in *sorted-items-values-weights*)
                      (collect item))))


(define-dynamic-relations
  (contents $list)  ;items in the knapsack
  (load $fixnum)
  (worth $fixnum))


(define-static-relations
  (capacity $fixnum)  ;capacity of the knapsack
  (value item $fixnum)
  (weight item $fixnum))


(define-action put
    1
  (?item item)
  (and (bind (contents $knapsack-items))
       (not (member ?item $knapsack-items))
       (bind (weight ?item $item-weight))
       (bind (load $knapsack-load))
       (setf $new-knapsack-load (+ $knapsack-load $item-weight))
       (bind (capacity $knapsack-capacity))
       (<= $new-knapsack-load $knapsack-capacity))
  (?item)
  (assert (setf $new-knapsack-items (cons ?item $knapsack-items))
          (contents $new-knapsack-items)
          (load $new-knapsack-load)
          (bind (worth $knapsack-worth))
          (bind (value ?item $item-value))
          (setf $new-knapsack-worth (+ $knapsack-worth $item-value))
          (worth $new-knapsack-worth)
          (setf $objective-value $new-knapsack-worth)))


(define-init
  (capacity 15)
  (contents nil)  ;initial knapsack items
  (load 0)
  (worth 0))


(define-init-action order-items-by-value/weight
  0
  ()
  (always-true)
  ()
  (assert (ww-loop for ($item $value $weight) in *sorted-items-values-weights*
            do (value $item $value)
               (weight $item $weight))))

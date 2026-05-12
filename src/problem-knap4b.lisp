;;;; Filename: problem-knap4b.lisp

;;; Problem specification for a 4-item knapsack problem. Based on problem-knap4a.lisp


(in-package :ww)  ;required


(ww-set *problem-name* knap4b)

(ww-set *problem-type* planning)

(ww-set *tree-or-graph* graph)

(ww-set *solution-type* max-value)


(defparameter *items-values-weights*
  '((item1 10 2) (item2 10 4) (item3 12 6) (item4 18 9)))


(defparameter *sorted-items-values-weights*  ;sort according to value/weight ratio
  (sort (copy-list *items-values-weights*) #'>
        :key (lambda (x)
               (/ (second x) (third x)))))


(defparameter *num-items* (length *items-values-weights*))


(define-types
  item (compute (iter (for (item nil nil) in *sorted-items-values-weights*)
                      (collect item)))
  item-id (compute (cdr (alexandria:iota (1+ *num-items*)))))


(define-dynamic-relations
  (contents $list)
  (content-ids $list)
  (load $fixnum)
  (worth $fixnum))


(define-static-relations
  (item-id item $fixnum)  ;ids used for bounding
  (capacity $fixnum)  ;capacity of the knapsack
  (value item $fixnum)
  (weight item $fixnum)
  (id-value item-id $fixnum)
  (id-weight item-id $fixnum))


(define-query compute-bounds? ($knapsack-item-ids)
  ;Computes cost and upper bounds 
  (do (bind (capacity $knapsack-capacity))   ;(ut::prt state)
      (setf $max-item-id (or (car (last $knapsack-item-ids)) 0))
      (setf $wt 0 $cost 0 $upper 0)
      (ww-loop for $item-id in (append $knapsack-item-ids
                                 (loop for id from (1+ $max-item-id) to *num-items*
                                       collect id))
        do (bind (id-weight $item-id $item-weight))
           (bind (id-value $item-id $item-value))  ;(ut::prt $item-id $item-weight $item-value)
           (if (<= (+ $wt $item-weight) $knapsack-capacity)
             (do (incf $wt $item-weight)  ;(ut::prt $wt)
                 (incf $cost $item-value)
                 (incf $upper $item-value))
             (do (assign $fraction (/ (- $knapsack-capacity $wt) $item-weight))
                 (incf $cost (* $fraction $item-value))  ;(ut::prt $fraction (- $cost) (- $upper))  (break)
                 (return-from compute-bounds? (values (- $cost) (- $upper))))))  ;(ut::prt (- $cost) (- $upper)) (break)
      (return-from compute-bounds? (values (- $cost) (- $upper)))))


(defun successors-p (ints)
  "Tests for a succession of integers."
  (iter (for int in ints)
        (for prev-item previous int)
        (when prev-item
          (always (= int (1+ prev-item))))))


(define-query bounding-function? ()
  (do (bind (content-ids $knapsack-item-ids))
      (if (successors-p $knapsack-item-ids)
        (if (= *cost* *upper* 0)
          (do (mv-assign (*cost* *upper*)
                                   (compute-bounds? $knapsack-item-ids))
              (values *cost* *upper*))
          (values *cost* *upper*))
        (do (setf *cost* 0 *upper* 0)
            (compute-bounds? $knapsack-item-ids)))))


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
  (assert (bind (contents $knapsack-items))
          (bind (content-ids $knapsack-item-ids))
          (bind (item-id ?item $item-id))
          (setf $new-knapsack-items (cons ?item $knapsack-items))
          (setf $new-knapsack-item-ids
            (merge 'list (list $item-id) (copy-list $knapsack-item-ids) #'<))
          (contents $new-knapsack-items)
          (content-ids $new-knapsack-item-ids)
          (load $new-knapsack-load)
          (bind (worth $knapsack-worth))
          (bind (value ?item $item-value))
          (setf $new-knapsack-worth (+ $knapsack-worth $item-value))
          (worth $new-knapsack-worth)
          (setf $objective-value $new-knapsack-worth)))


(define-init
  (capacity 15)
  (contents nil)
  (content-ids nil)
  (load 0)
  (worth 0))


(define-init-action initialize-weight&value&ids
  0
  ()
  (always-true)
  ()
  (assert (ww-loop for ($item $value $weight) in *sorted-items-values-weights*
            for $id from 1
            do (value $item $value)
               (weight $item $weight)
               (id-weight $id $weight)
               (id-value $id $value)
               (item-id $item $id))))

#|
(define-init-action initialize-weight&value-ids
  0
  ()
  (always-true)
  ()
  (assert (ww-loop for $item in (gethash 'item *types*)
            for $id from 1
            do (bind (weight $item $item-weight))
               (bind (value $item $item-value))
               (id-weight $id $item-weight)
               (id-value $id $item-value)
               (item-id $item $id))))            
|#

;;;; Filename: problem-knap30.lisp

;;; Problem specification for a 30-item knapsack problem.
;;; Same rules as problem-knap19-1.lisp, with the data from data-knap30.lisp.


(in-package :ww)  ;required


(ww-set *problem-name* knap30)

(ww-set *problem-type* planning)

(ww-set *tree-or-graph* graph)

(ww-set *solution-type* max-value)


;;; Read the item data when the file loads.  Items are numbered 1..n in order of
;;; decreasing value/weight, which bounding-function? relies on.


(defun read-knapsack-data (data-file)
  "Returns (capacity items), each item a (value weight) list, sorted by decreasing value/weight."
  (with-open-file (infile data-file :direction :input)
    (let* ((num-items (read infile))
           (capacity (read infile))
           (items (loop repeat num-items
                        collect (list (read infile) (read infile)))))
      (list capacity (sort items #'> :key (lambda (item) (/ (first item) (second item))))))))


(defparameter *knapsack* (read-knapsack-data (in-src "data-knap30.lisp")))


(define-types
  item-id (compute (alexandria:iota (length (second *knapsack*)) :start 1)))


(define-dynamic-relations
  (in item-id)  ;an item-id in the knapsack
  (contents $list)  ;the item-ids in the knapsack
  (load $fixnum)  ;the net weight of the knapsack
  (worth $fixnum))  ;the net worth of item-ids in the knapsack


(define-static-relations
  (capacity $fixnum)  ;weight capacity of the knapsack
  (value item-id $fixnum)  ;value of an item-id
  (weight item-id $fixnum))  ;weight of an item-id


(define-query bounding-function? ()
  ;Returns (values cost upper), negated for max-value.  Packs items in order of decreasing
  ;value/weight, skipping items numbered below the largest packed item that are not packed:
  ;upper is the value of the items that fit whole, cost adds a fraction of the first that
  ;does not.
  (do (bind (contents $item-ids))
      (bind (capacity $capacity))
      (setf $max-item-id (or (car (last $item-ids)) 0))
      (setf $wt 0 $upper 0)
      (ww-loop for $item-id in (gethash 'item-id *types*) do
        (if (and (or (member $item-id $item-ids) (> $item-id $max-item-id))
                 (bind (weight $item-id $item-weight))
                 (bind (value $item-id $item-value)))
          (if (<= (+ $wt $item-weight) $capacity)
            (do (incf $wt $item-weight)
                (incf $upper $item-value))
            (return-from bounding-function?
              (values (- (+ $upper (* (/ (- $capacity $wt) $item-weight) $item-value)))
                      (- $upper))))))
      (return-from bounding-function? (values (- $upper) (- $upper)))))


(define-action put
    1
  (?item-id item-id)
  (and (not (in ?item-id))
       (bind (weight ?item-id $item-weight))
       (bind (load $load))
       (assign $new-load (+ $load $item-weight))
       (bind (capacity $capacity))
       (<= $new-load $capacity))
  (?item-id)
  (assert (in ?item-id)
          (bind (contents $item-ids))
          (assign $new-item-ids
            (merge 'list (list ?item-id) (copy-list $item-ids) #'<))
          (contents $new-item-ids)
          (load $new-load)
          (bind (worth $worth))
          (bind (value ?item-id $item-value))
          (assign $new-worth (+ $worth $item-value))
          (worth $new-worth)
          (assign $objective-value $new-worth)))


(define-init-action init-item-weights&values
    0
  (?item-id item-id)
  (always-true)
  ()
  (assert (weight ?item-id (second (nth (1- ?item-id) (second *knapsack*))))
          (value ?item-id (first (nth (1- ?item-id) (second *knapsack*))))))


(define-init
  `(capacity ,(first *knapsack*))
  (contents nil)
  (load 0)
  (worth 0))

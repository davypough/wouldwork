;;;; Filename: problem-knap19.lisp

;;; Problem specification for a 19-item knapsack problem.


(in-package :ww)  ;required


(ww-set *problem-name* knap19)

(ww-set *problem-type* planning)

(ww-set *tree-or-graph* graph)

(ww-set *solution-type* max-value)


;;; First initialize & populate any auxiliary problem data structures.
;;; Use the reader macro #. to initialize structures at read time.

#.(unless (fboundp 'make-item)  ;don't keep redefining the defstruct
    (defstruct item  ;an item template
      (name nil :type symbol)
      (id -1 :type fixnum)
      (value -1 :type fixnum)
      (weight -1 :type fixnum)
      (value/weight -1 :type double-float)))

#.(defparameter *item-structures* nil)
#.(defparameter *num-items* -1)
#.(defparameter *max-weight* -1)

#.(unless (fboundp 'create-item-structures)
    (defun create-item-structures (data-file)
      (with-open-file (infile data-file :direction :input :if-does-not-exist nil)
        (when (not (streamp infile)) (error "File does not exist!"))
        (setf *num-items* (read infile))
        (setf *max-weight* (read infile))
        (loop for i from 1 upto *num-items* do
          (let ((value (read infile))
                (weight (read infile)))
            (push (make-item :name (intern (format nil "ITEM~D" i) :ww)
                             :value value
                             :weight weight
                             :value/weight (coerce (/ value weight)
                                                   'double-float))
                  *item-structures*))))))

#.(create-item-structures (in-src "data-knap19.lisp"))


;;; Then describe the problem for Wouldwork

(defparameter *sorted-item-structures*
  (loop with sorted-item-structures = (sort (copy-list *item-structures*) #'>
                                            :key #'item-value/weight)
        for item-structure in sorted-item-structures
        for index from 1
        do (setf (item-id item-structure) index)
        collect item-structure))


(define-types
  item-id (compute (mapcar #'item-id *sorted-item-structures*)))  ;item IDs 


(define-dynamic-relations
  (in item-id)  ;an item-id in the knapsack
  (contents $list)  ;the item-ids in the knapsack
  (load $fixnum)  ;the net weight of the knapsack
  (worth $fixnum))  ;the net worth of item-ids in the knapsack


(define-static-relations
  (capacity $fixnum)  ;weight capacity of the knapsack
  (value item-id $fixnum)  ;value of an item-id
  (weight item-id $fixnum))  ;weight of an item-id


(define-query compute-bounds? ($item-ids)
  ;Computes cost and upper bounds for a state
  (do (bind (capacity $capacity))   ;(ut::prt state)
      (setf $max-item-id (or (car (last $item-ids)) 0))
      (if (= (length $item-ids) $max-item-id)
        (setf $missing-item-ids nil)
        (do (setf $initial-item-ids (cdr (alexandria:iota (1+ $max-item-id))))
            (setf $missing-item-ids (set-difference $initial-item-ids $item-ids))))  ;(ut::prt $all-item-ids $missing-item-ids)
      (setf $all-item-ids (gethash 'item-id *types*))
      (setf $wt 0 $cost 0 $upper 0)
      (ww-loop for $item-id in $all-item-ids do  ;run thru all item-ids until capacity exceeded
        (if (and (not (member $item-id $missing-item-ids))  ;except those missing
                 (bind (weight $item-id $item-weight))
                 (bind (value $item-id $item-value))  ;(ut::prt $item-id $item-weight $item-value)
                 (if (<= (+ $wt $item-weight) $capacity)
                   (do (incf $wt $item-weight)  ;(ut::prt $wt)
                       (incf $cost $item-value)
                       (incf $upper $item-value))
                   (do (setq $fraction (/ (- $capacity $wt) $item-weight))
                       (incf $cost (* $fraction $item-value))  ;(ut::prt $fraction (- $cost) (- $upper))  (break)
                       (return-from compute-bounds? (values (- $cost) (- $upper))))))))  ;(ut::prt (- $cost) (- $upper)) (break)
      (return-from compute-bounds? (values (- $cost) (- $upper)))))


(defun successors-p (lst)
  "Tests for a succession of integers (ie, item-ids)."
  (iter (for item-id in lst)
        (for prev-item-id previous item-id)
        (when prev-item-id
          (always (= item-id (1+ prev-item-id))))))


(define-query bounding-function? ()
  (do (bind (contents $item-ids))
      (if (successors-p $item-ids)
        (if (= *cost* *upper* 0)
          (do (multiple-value-setq (*cost* *upper*)
                                   (compute-bounds? $item-ids))
                 (values *cost* *upper*))
          (values *cost* *upper*))
        (do (setf *cost* 0 *upper* 0)
            (compute-bounds? $item-ids)))))
        

(define-action put
    1
  (?item-id item-id)
  (and (not (in ?item-id))
       (bind (weight ?item-id $item-weight))
       (bind (load $load))
       (setq $new-load (+ $load $item-weight))
       (bind (capacity $capacity))
       (<= $new-load $capacity))
  (?item-id)
  (assert (in ?item-id)
          (bind (contents $item-ids))
          (setq $new-item-ids
            (merge 'list (list ?item-id) (copy-list $item-ids) #'<))
          (contents $new-item-ids)
          (load $new-load)
          (bind (worth $worth))
          (bind (value ?item-id $item-value))
          (setq $new-worth (+ $worth $item-value))
          (worth $new-worth)
          (setq $objective-value $new-worth)))


(define-init-action init-item-weights&values
    0
  (?item-id item-id)
  (always-true)
  ()
  (assert ;(ut::prt item-structure)
          (weight ?item-id (item-weight (find ?item-id *sorted-item-structures*
                                              :key #'item-id)))
          (value ?item-id (item-value (find ?item-id *sorted-item-structures*
                                            :key #'item-id)))))


(define-init
  (capacity #.*max-weight*)  ;read-time eval required for macro processing
  (contents nil)
  (load 0)
  (worth 0))
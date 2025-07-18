;;; Filename: ww-utilities.lisp

;;; Collection of utilities for the Wouldwork Planner.


(in-package :ut)


(set-pprint-dispatch '(array * (* *))
  (lambda (stream array)
    "User friendly printout for a state representation containing a 2D array."
    (declare (type simple-array array))
    (loop for i below (first (array-dimensions array))
      do (format stream "~%    ")
         (loop for j below (second (array-dimensions array))
           do (let ((cell (aref array i j)))
                (format stream "~A" cell))))))


(defmacro if-it (form then &optional else)
  "Binds the variable 'it' to the result of evaluating the form,
   which can then be used subsequently in the then or else clauses."
  `(let ((it ,form))
     (if it ,then ,else)))


(defmacro prt (&rest forms)
  "Print the names & values of given forms (ie,variables or accessors).
   Can wrap around an expression, returning its value."
  `(progn ,@(loop for form in forms
              collect `(format t "~&  ~S => ~S~%" ',form ,form))
          ,@(last `,forms)))


(defmacro prt-if (&rest args)
  "Conditional prt--eg, (ut::prt-if (?x 3) (?y 'abc) (ut::prt 'printing-x&y ?x ?y))"
  (let ((conditions '())
        (body-expr (car (last args))))
    ;; Parse condition pairs from beginning
    (loop for arg in (butlast args)
          do (assert (and (listp arg) (= (length arg) 2)) ()
                     "Each condition must be a 2-element list: ~A" arg)
             (push arg conditions))
    ;; Validate requirements
    (assert (> (length conditions) 0) () "At least one condition required")
    (assert body-expr () "Body expression required")
    ;; Generate conditional execution
    `(when (and ,@(mapcar (lambda (condition)
                           `(eql ,(first condition) ,(second condition)))
                         (reverse conditions)))
       ,body-expr)))


(defmacro mvb (vars values-form &rest body)
  "Abbreviation for multiple-value-bind."
  `(multiple-value-bind ,vars ,values-form ,@body))


(defmacro mvs (vars values-form &rest body)
  "Abbreviation for multiple-value-setq."
  `(multiple-value-setq ,vars ,values-form ,@body))


(define-modify-macro sortf (function &rest sort-key)  ;can include function in &rest
  sort
  "Modifies a referenced sequence by sorting it.")


(defun forget (sym)
  "Removes a symbol from the current package."
  (declare (type symbol sym))
  (makunbound sym)
  (fmakunbound sym)
  (setf (symbol-plist sym) nil))
  

(defun sort-symbols (list-of-symbols)
  "Sorts a list of symbols alphabetically."
  (sort (copy-list list-of-symbols) #'string< :key #'symbol-name))


(defun merge-sort (item items predicate)
  "Adds an item to list of items sorted."
  (declare (type function predicate))
  (merge 'list (list item) (copy-list items) predicate))
  
  
(defun remove-at-indexes (idxs lst)
  "Removes items at given indexes from a list."
  (loop for i from 0
    for elt in lst
    unless (member i idxs :test #'=)
    collect elt))


(defun collect-at-indexes (idxs lst)
  "Collect items at given indexes from a list."
  (loop for i from 0
    for elt in lst
    when (member i idxs :test #'=)
    collect elt))


(defun subst-items-at-ascending-indexes (items idxs lst)
  "Substitutes for elements at given indexes in a list.
   Indexes & items must correspond and be in ascending order."
  (loop for i from 0
    for elt in lst
    if (and idxs (= i (first idxs)))
      collect (first items)
      and do (pop idxs)
             (pop items)
      else collect elt))

#|
(defun segregate-plist (plist)
  "Returns two lists, the list of properties and the list of values in plist.
   Ex call: (destructuring-bind (properties values) (segregate-plist plist) ..."
  (loop with properties and values
    for (property value) on plist by #'cddr
    if (listp property)
      do (loop for item in property
           do (push item properties)
              (push value values))
      else do (push property properties)
              (push value values)
    finally (return (list (reverse properties) (reverse values)))))
(declaim (ftype (function (list) list) segregate-plist))
|#

(defun intern-symbol (&rest args)
  "Interns a symbol created by concatenating args.
   Based on symb in Let Over Lambda."
  (flet ((mkstr (&rest args)
           (with-output-to-string (s)
             (dolist (a args) (princ a s)))))
    (values (intern (apply #'mkstr args)))))
(declaim (ftype (function (&rest t) (values symbol &optional)) intern-symbol))


;(defun list-difference (lst sublst)
;  "Returns lst with elements in sublst removed."
;  (assert (and (listp lst) (listp sublst)))
;  (remove-if (lambda (item)
;               (member item sublst))
;    lst))
;(declaim (ftype (function (list list) list) list-difference))


(defun walk-tree (fun tree)
  "Apply fun to each cons and atom in tree."
  (the list (subst-if t (constantly nil) tree :key fun)))
(declaim (ftype (function (function list) list) walk-tree))


(defun map-into-tree-atoms (fn tree)
  "Returns a new tree with all atoms replaced by the value of fn."
  (declare (type function fn))
  (cond ((null tree) nil)
        ((atom tree) (let ((result (funcall fn tree)))
                       (if result result)))
        (t (remove-if #'null
                      (mapcar (lambda (subtree)
                                (map-into-tree-atoms fn subtree))
                              tree)))))


(defun ninsert-list (new-element position lst)
  "Destructively inserts new element in a list at given position <= length lst"
  (if (zerop position)
    (push new-element lst)
    (push new-element (cdr (nthcdr (1- position) lst))))
  lst)


(defun intersperse (element lst)
  "Returns a list with element inserted at odd indexed locations."
  (loop for item in lst
    append (list item element)))
(declaim (ftype (function (t list) list) intersperse))


(defun interleave+ (lst)
  "Inserts a + sign between list items."
  (format nil "~{~A~^+~}" lst))
(declaim (ftype (function (list) string) interleave+))


(defun transpose (list-of-equi-length-lists)  ;same as regroup-by-index ?
  "Regroups all first elements together, second elements together, etc into a new
   list-of-lists. Changes instantiate-types into arg format for some, every, etc.
   ((a b) (c d) (e f)) -> ((a c e) (b d f))"
  (apply #'mapcar #'list list-of-equi-length-lists))

#|
(defun quote-elements (lst)
  "Quotes the individual elements of a list."
  (or (mapcar (lambda (elem) `',elem) lst)
      '((quote nil))))
(declaim (ftype (function (list) list) quote-elements))


(defun map-product-less-bags (lists)
  "Performs alexandria:map-product but leaves out combination with duplicates."
  (the list (if (car lists)
              (mapcan (lambda (inner-val)
                        (mapcan (lambda (outer-val)
                                  (unless (member outer-val inner-val)
                                (list (cons outer-val inner-val))))
                                (car lists)))
                      (map-product-less-bags (cdr lists)))
              (list nil))))
(declaim (ftype (function (list) list) map-product-less-bags))
|#

(defgeneric show (object &rest rest)
  (:documentation "Displays an object in a user-friendly format."))


(defmethod show ((table hash-table) &key (sort-by 'key))
  "Displays a hash table line-by-line, sorted either by key or val."
  (declare (type hash-table table))
  (when (= (hash-table-count table) 0)
    (format t "~&~A Empty~%" table)
    (return-from show))
  (let (alist)
    (maphash
      (lambda (key val)
        (push (cons (princ-to-string key) (prin1-to-string val))
          alist))
      table)
    (setf alist 
      (sort (copy-list alist) #'string< :key (ecase sort-by (key #'car) (val #'cdr))))
    (loop for (key . val) in alist
      do (format t "~&~A ->~10T ~A~%" key val)))
  t)


(defmethod show ((fn function) &rest rest)
  (declare (ignore rest))
  (format t "~&~A~%" (function-lambda-expression fn))
  t)


(defmethod show ((lst list) &rest rest)
  (declare (ignore rest))
  (format t "~&~A~%" lst))


(defmethod show ((object t) &rest rest)
  "Prints any basic lisp object."
  (declare (ignore rest))
  (format t "~&~S~%" object)
  t)


(defun print-ht (ht) 
  "Prints a hash table line by line."
  (declare (type hash-table ht))
  (format t "~&~A" ht)
  (maphash (lambda (key val) (format t "~&~S ->~10T ~S" key val)) ht)
  (terpri)
  ht)


(defun hash-table-same-keys-p (ht1 ht2)
  "Returns t if two hash tables have the same keys."
  (declare (type hash-table ht1 ht2))
  (when (= (hash-table-count ht1) (hash-table-count ht2))
    (maphash (lambda (ht1-key ht1-value)
               (declare (ignore ht1-value))
               (unless (gethash ht1-key ht2)
                 (return-from hash-table-same-keys-p nil)))
             ht1)
    t))


(defun hash-table-present-p (key ht)
  "Determines if a key is present in ht."
  (declare (type hash-table ht))
  (mvb (* present) (gethash key ht)
    present))


(defun save (object file)
  "Saves an object to a file so it can be read in later."
  (with-open-file (out-stream file :direction :output)
    (let ((*print-readably* t))
      (pprint object out-stream))))


(defun retrieve (file)
  "Retrieves one or more objects from a file."
  (with-open-file (in-stream file :direction :input)
    (loop for object = (read in-stream nil in-stream)
      until (eql object in-stream)
      collect object into objects
      finally (return (if (= (length objects) 1)
                        (first objects)
                        objects)))))


(defun extract-symbols (x.y)
  "Splits a symbol, whose print name contains the delimiter #\., into two symbols."
  (let* ((x.y-string (string x.y))
         (index (position #\. x.y-string :test #'char=)))
    (values (intern (subseq x.y-string 0 index)) (intern (subseq x.y-string (1+ index))))))

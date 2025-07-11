;;; Filename: ww-support.lisp

;;; Support functions for planning.


(in-package :ww)


(defmacro mvsetq (var-list form)
  `(multiple-value-setq ,var-list ,form))


(defmacro when-debug>= (n &rest expressions)
  "Inserts debugging expressions when *debug* >= n, otherwise NIL"
  `(when (>= *debug* ,n)
     ,@expressions))


(defmacro equivalent (&rest forms)
  "Returns true if all forms evaluate to nil, or if all forms evaluate to non-nil."
  `(or (and ,@forms)
       (and ,@(mapcar (lambda (form) `(null ,form)) forms))))


(defun troubleshoot (error-msg &rest args)
  (apply #'cerror "Troubleshoot the current node" error-msg args)
  (setf *troubleshoot-current-node* t)
  nil)


(defun profile ()
  "Deterministically profiles Wouldwork."
  (sb-profile:reset)
  (sb-profile:profile "WOULDWORK")
  (ww-solve)
  (sb-profile:report))


;;;;;;;;;;;;; User Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun eql* (&rest arguments)
  (every #'eql arguments (rest arguments)))


(defun different (sym1 sym2)
  "Determines whether two symbols are different."
  (if (and (symbolp sym1) (symbolp sym2))
    (not (eql sym1 sym2))
    (error "Arguments must be symbols: ~A ~A" sym1 sym2)))


(defun delete-actions (&rest names)
  "Deletes named actions from *actions* at run-time."
  (setf *actions* (delete-if (lambda (name)
                               (member name names))
                             *actions*
                             :key #'action.name)))


(defun get-state-codes ()
  "User calls this after finding backwards *solutions*."
  (format t "~%Working ...~%")
  (clrhash *state-codes*)
  (iter (for soln in *solutions*)
        (for path = (solution.path soln))
        (for db-props = (list-database (problem-state.idb (solution.goal soln))))
        (setf (gethash (funcall (symbol-function 'encode-state) db-props) *state-codes*) path))
  *state-codes*)


(defun backward-path-exists (state)
  "Use in forward search goal to check existence of backward path."
  (declare (type problem-state state))
  (gethash (funcall (symbol-function 'encode-state) (list-database (problem-state.idb state))) *state-codes*))


(defun make-ht-set (&rest args &key (initial-contents nil initial-contents-p) &allow-other-keys)
  "Makes a wouldwork hash-table that works as a set container for the user."
  (let* ((ht-args (if initial-contents-p
                    (alexandria:remove-from-plist args :initial-contents)
                    args))
         (ht (apply #'make-hash-table ht-args)))
    (when initial-contents-p
      (dolist (key initial-contents)
        (setf (gethash key ht) t)))
    ht))
    
    
(defun union-ht-set (&rest set-hts)
  "Unions two hash tables keys. Assumes values are all t and have the same :test function."
  (let ((test-fn (hash-table-test (first set-hts))))
    (assert (and (every (lambda (ht) (typep ht 'hash-table)) set-hts)
                 (every (lambda (ht) (eql (hash-table-test ht) test-fn)) (rest set-hts)))
            () "All arguments must be hash tables, and have the same :test function in union-ht.")
    (let ((result-ht (make-hash-table :test test-fn)))
      (dolist (ht set-hts)
        (maphash (lambda (key value)
                   (setf (gethash key result-ht) value))
                 ht))
      result-ht)))
      
      
(defun set-difference-ht-set (ht1 ht2)
  "Returns a new hash table that represents the set difference of HT1 and HT2."
  (assert (and (typep ht1 'hash-table)
               (typep ht2 'hash-table)
               (eql (hash-table-test ht1) (hash-table-test ht2)))
          () "The two arguments must be hash tables and have the same :test function in set-difference-ht.")
  (let ((result-ht (make-hash-table :test (hash-table-test ht1))))
    (maphash (lambda (key value)
               (unless (gethash key ht2)
                 (setf (gethash key result-ht) value)))
             ht1)
    result-ht))


(defun copy-ht-set (set-ht)
  "Copy a set hash table (with t values)."
  (loop with new-ht = (make-hash-table
                        :test (hash-table-test set-ht)
                        :size (hash-table-size set-ht)
                        :rehash-size (hash-table-rehash-size set-ht)
                        :rehash-threshold (hash-table-rehash-threshold set-ht))
      for key being the hash-keys in set-ht ;using (hash-value value)
      do (setf (gethash key new-ht) t)
      finally (return new-ht)))


(defun vectorize (lists)
  "Turns a list of lists into vector vectors."
  (iter (for list in lists)
        (collect (apply #'vector list) result-type 'simple-vector)))


#|
(defun make-bv-set (dotted-pairs)
  "Makes a bit vector that works as a set container corresponding to board coordinates."
  (let ((bv (make-array (* *row-dimension* *col-dimension*) :element-type 'bit :adjustable nil)))
    (dolist (pair dotted-pairs)
      (let* ((row (car pair))
             (col (cdr pair))
             (index (+ (* row *col-dimension*) col)))
        (setf (sbit bv index) 1)))
    bv))
|#

;;;;;;;;;;;;;;;;; Program Support Functions ;;;;;;;;;;;;;;;;


(defun add-prop (proposition db)
  "Effectively adds an atomic proposition to the database."
  (declare (type hash-table db))
  (let ((int-db (eql (hash-table-test db) 'eql)))
    (if (get-prop-fluent-indices proposition)
      ;do if proposition has fluents
      (let ((fluentless-prop (get-fluentless-prop proposition))
            (fluent-values (get-prop-fluents proposition)))
        (if int-db
          (setf (gethash (convert-to-integer fluentless-prop) db) fluent-values)
          (setf (gethash fluentless-prop db) fluent-values))
        ;handle complement with fluents if it exists
        (when (gethash (car proposition) *complements*)
          (let ((complement-prop (get-complement-prop proposition)))
            (if int-db
              (remhash (convert-to-integer complement-prop) db)
              (remhash complement-prop db)))))
      ;do for non-fluent propositions
      (progn
        (if int-db
          (setf (gethash (convert-to-integer proposition) db) t)  
          (setf (gethash proposition db) t))
        ;handle complement without fluents
        (when (gethash (car proposition) *complements*)
          (if int-db
            (remhash (convert-to-integer (get-complement-prop proposition)) db)
            (remhash (get-complement-prop proposition) db)))))))


(defun del-prop (proposition db)
  "Effectively removes an atomic proposition from the database."
  (declare (type hash-table db))
  (let ((int-db (eql (hash-table-test db) 'eql)))
    (if (get-prop-fluent-indices proposition)
      ;do if proposition has fluents
      (let ((fluentless-prop (get-fluentless-prop proposition))
            (fluent-values (get-prop-fluents proposition)))
        (if int-db
          (remhash (convert-to-integer fluentless-prop) db)
          (remhash fluentless-prop db))
        ;handle complement with fluents if it exists
        (when (gethash (car proposition) *complements*)
          (let ((complement-prop (get-complement-prop proposition)))
            (if int-db
              (setf (gethash (convert-to-integer complement-prop) db) fluent-values)
              (setf (gethash complement-prop db) fluent-values)))))
      ;do for non-fluent propositions
      (progn
        (if int-db
          (remhash (convert-to-integer proposition) db)
          (remhash proposition db))
        ;handle complement without fluents
        (when (gethash (car proposition) *complements*)
          (if int-db
            (setf (gethash (convert-to-integer (get-complement-prop proposition)) db) t)
            (setf (gethash (get-complement-prop proposition) db) t)))))))


(defun add-proposition (proposition db)  
  "Adds an atomic proposition and all its symmetries to the database."
  (declare (type hash-table db))
  (let ((symmetric-indexes (gethash (car proposition) *symmetrics*)))
    (if (null symmetric-indexes)
      (add-prop proposition db)
      (let ((symmetric-variables
               (loop for indexes in symmetric-indexes
                     collect (loop for index in indexes
                                 collect (nth index (cdr proposition)))))
            (props (list (copy-list proposition))))
        (loop for vars in symmetric-variables
              for idxs in symmetric-indexes do
              (setf props (generate-new-propositions vars props idxs)))
        (loop for prop in props do
              (add-prop prop db))))))

              
(defun delete-proposition (proposition db)
  (declare (type hash-table db))
  (let ((symmetric-indexes (gethash (car proposition) *symmetrics*)))
    (if (null symmetric-indexes)
      (del-prop proposition db)
      (let ((symmetric-variables
               (loop for indexes in symmetric-indexes
                     collect (loop for index in indexes
                                 collect (nth index (cdr proposition)))))
            (props (list (copy-list proposition))))
        (loop for vars in symmetric-variables
              for idxs in symmetric-indexes do
              (setf props (generate-new-propositions vars props idxs)))
        (loop for prop in props do
              (del-prop prop db))))))


(defun generate-new-propositions (vars propositions idxs)
  "Collects new propositions for each prop in propositions."
  (loop for prop in propositions
        append (generate-proposition-permutations vars prop idxs)))


(defun generate-proposition-permutations (vars proposition idxs)
  "Returns list of propositions generated from given proposition replacing
   items at indices with vars, respectively."
  (let (propositions)
    (alexandria:map-permutations
      (lambda (perm)
        (push (ut::subst-items-at-ascending-indexes perm (mapcar #'1+ idxs) proposition)
              propositions))
      vars)
    propositions))


(defun revise (db literals)
  "Updates a database given a simple list of atomic propositions."
  (declare (type hash-table db))
  (loop for literal in literals
      do (update db literal)
      finally (return db)))


(defun update (db literal)
  "Single add or delete from db. Returns the literal for change tracking."
  (declare (type hash-table db))
  (when *print-updates*
    (ut::prt literal))
  (if (eql (car literal) 'not)
    (delete-proposition (second literal) db)
    (add-proposition literal db))
  literal)


#+ignore (defun update (db literal)
  "Single add or delete from db."
  (declare (type hash-table db))
  (when *print-updates*
    (ut::prt literal))
  (if (eql (car literal) 'not)
    (delete-proposition (second literal) db)
    (add-proposition literal db))
  db)


(defun revert-updates (db changes-list)
  "Undoes a list of changes to restore database to previous state"
  (declare (type hash-table db))
  (dolist (change changes-list)
    (if (eql (car change) 'not)
        ;; If we deleted something, add it back
        (add-proposition (second change) db)
        ;; If we added something, delete it
        (delete-proposition change db)))
  db)


(defun expand-into-plist (parameters)
  "Return alternating plist of variable/type from input parameter list."
  (loop for (vars type) on parameters by #'cddr
      if (listp vars)
      append (ut::intersperse type vars) into plist
      else append (list vars type) into plist
        finally (return plist)))


(defun get-fluentless-prop (proposition)
  "Derives the fluentless proposition counterpart from a full proposition."
  (let ((indices (get-prop-fluent-indices proposition)))
    (ut::remove-at-indexes indices proposition)))


(defun get-complement-prop (proposition)
  "Derives the complement proposition counterpart from a given proposition."
  (let* ((predicate (car proposition))
         (joint-patterns (gethash predicate *complements*))
         (prop-pattern (first joint-patterns))
         (comp-pattern (copy-tree (second (second joint-patterns)))))
    (loop for const in (cdr proposition)
          for pat in (cdr prop-pattern)
          when (member pat comp-pattern :test #'equal)
            do (nsubst const pat comp-pattern :test #'equal)
          finally (return comp-pattern))))


(defun dissect-pre-params (pre-param-list)
  (iter (for item in pre-param-list)
        (for prior-item previous item)
        (cond ((member item *parameter-headers*) (collecting item into types))  ;header
              ((?varp item) (collecting item into ?vars))  ;?variable
              ((and (listp item) (?varp (first item)))  ;list of ?variables
                 (appending item into ?vars))
              ((nth-value 1 (gethash item *types*))  ;type
                 (if (symbolp prior-item)  ;single prior ?variable
                   (collecting item into types)
                   (appending (make-list (length prior-item) :initial-element item) into types)))  ;multiple prior ?variables
              ((and (listp item)
                    (member (first item) *query-names*))  ;call to a query
                 (collecting item into types))
              ((eql (first item) 'either)  ;combo type
                 (let* ((new-type (intern (ut::interleave+ (ut::sort-symbols (cdr item)))))
                        (type-instances (mapcar (lambda (type) (gethash type *types*)) (cdr item)))
                        (combined-instances (if (every #'null type-instances)
                                              '(nil)
                                              (remove-duplicates (apply #'append type-instances)))))
                   (setf (gethash new-type *types*) combined-instances)
                   (if (symbolp prior-item)  ;single prior ?variable
                     (collecting new-type into types)
                     (appending (make-list (length prior-item) :initial-element new-type) into types))))
              ((member (first item) *parameter-headers*)  ;subparameter list
                 (multiple-value-bind (additional-?vars additional-types)
                   (dissect-pre-params item)
                   (collecting additional-?vars into ?vars)
                   (collecting additional-types into types)))
              (t (error "Problem detected in dissect-pre-params: ~A" pre-param-list)))
        (finally (return (values ?vars types)))))


(defun dissect-eff-params (eff-parameter-list)
  "Returns a list of primitive eff-parameter variables and types."
  (iter (for (var-form type-form) on eff-parameter-list by #'cddr)
        (cond ((atom var-form)
                 (collecting var-form into vars)
                 (collecting type-form into types))
              ((listp var-form)
                 (appending var-form into vars)
                 (appending (make-list (length var-form) :initial-element type-form) into types)))
        (finally (return (values vars types)))))

     
(defun instantiate-type-spec (pre-type-spec)
  "Given the pre-type-spec from dissect-pre-params,
   eg (product gate (get-remaining? ladder) (product fan fan)),
   instantiate all of the included types,
   eg (product (gate1 gate2) (get-remaining? ladder) (product (fan1 fan2) (fan1 fan2)))."
  (iter (for item in pre-type-spec)
        (cond ((member item *parameter-headers*) ;collect header
                 (collecting item))
              ((nth-value 1 (gethash item *types*))  ;collect type instances
                 (collecting (gethash item *types*)))
              ((and (listp item)  ;collect dynamic query
                    (member (first item) *query-names*))
                 (collecting item))
              ((and (listp item)
                    (member (first item) *parameter-headers*))
                 (collecting (instantiate-type-spec item))))))


(defun eval-instantiated-spec (instantiated-pre-type-spec &optional state-or-state+)
  "Receives possibly nested static or dynamic input from instantiate-type-spec,
   and evaluates it. Works with state, idb, or context-aware parameter."
  (iter (for item in instantiated-pre-type-spec)
        (cond ((member item *parameter-headers*)
                 (collecting item into instantiated-spec))
              ((and (listp item)
                    (member (first item) *query-names*))
                 (collecting (apply (first item) state-or-state+ (cdr item)) into instantiated-spec))
              ((and (listp item)
                    (member (first item) *parameter-headers*))
                 (collecting (eval-instantiated-spec item state-or-state+) into instantiated-spec))
              ((listp item)
                 (collecting item into instantiated-spec))
              (t (error "Unexpected item ~A in dynamic-spec ~A" item instantiated-pre-type-spec)))
        (finally (return (get-pre-lambda-arg-lists instantiated-spec)))))

        
(defun get-pre-lambda-arg-lists (instantiated-spec)
  "Returns list of instantiations as arg list for a rule precondition."
  (when (or (equal instantiated-spec '(standard))  ;no precondition parameters
            (equal instantiated-spec '(standard nil)))
    (return-from get-pre-lambda-arg-lists '((nil))))
  (let ((header (first instantiated-spec))
        (value-lists (cdr instantiated-spec)))
    ;(when (some #'null value-lists)
    ;  (return-from get-pre-lambda-arg-lists '())) ; Return empty - no valid instantiations
    (if (eql header 'dot-product)
      (apply #'mapcar #'list value-lists)
      (let ((product-values (apply #'alexandria:map-product 'list value-lists)))
        (if (eql header 'product)
          product-values
          (let ((set-values (remove-if-not (lambda (x) (alexandria:setp x :test #'equalp))
                                           product-values)))
            (if (eql header 'combination)
              (or (delete-duplicates set-values :test #'alexandria:set-equal)
                  (make-list (length value-lists) :initial-element nil))
              (if (eql header 'standard)
                (or set-values
                    (make-list (length value-lists) :initial-element nil))
                (error "Unknown header label ~A in an instantiated-spec: ~A"
                       header instantiated-spec)))))))))

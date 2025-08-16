;;; Filename: ww-installer.lisp

;;; Installs a user domain file.


(in-package :ww)


(defun either (&rest types)
  (loop for type in types
      append (gethash type *types*)))


(defun final-charp (final-char sym)
  "Determines if a symbol has a given final character in its name."
  (let ((str (symbol-name sym)))
    (char= (elt str (1- (length str))) final-char)))


(defmacro define-types (&rest types&values)
  `(install-types ',types&values))


(defun install-types (types&instances)
  (format t "~&Installing object types...")
  (check-type types&instances cons)
  (iter (for (type instances) on types&instances by #'cddr)
        (check-type type symbol)
        (check-type instances list)
        (when (eql (car instances) 'either)
          (setf instances (or (remove-if #'null (remove-duplicates (apply #'either (cdr instances))))
                              '(nil))))
        (when (eql (car instances) 'compute)
          (setf instances (eval (second instances))))
        (setf (gethash type *types*) instances)
        (unless (equal instances '(nil))
          (dolist (instance instances)
            (check-type instance (or symbol real list))
            (when (symbolp instance)
              (setf (gethash (list 'something instance) *static-db*) t)
              (setf (gethash (list type instance) *static-db*) t))))))


(defun symmetric-type-indexes (types)
  "Returns the set of type indexes for the multi-types of a symmetric relation."
  (let ((dups (remove-duplicates types)))
    (iter (for dup in dups)
          (collect (iter (for type in types)
                         (for i from 0)
                         (when (eql type dup)
                           (collect i)))
                   into indices)
          (finally (return (remove-if (lambda (elt)
                                        (alexandria:length= 1 (length elt)))
                                      indices))))))


(defun sort-either-types (relation)
  "Alphabetically sorts the 'either' types in a relation."
  (mapcar (lambda (item)  ;cannonically orders 'either' types
            (if (symbolp item)
              item
              (cons 'either (sort (copy-list (cdr item)) #'string<
                                  :key #'symbol-name))))
          relation))
            
            
(defmacro define-dynamic-relations (&rest relations)
  `(install-dynamic-relations ',relations))


(defun generate-fluent-instances (args-list)
  "Generate all combinations of instances for a relation signature, 
   where nil values represent fluent positions."
  (if (null args-list)
      (list nil)
      (let ((first-arg (car args-list))
            (rest-instances (generate-fluent-instances (cdr args-list))))
        (cond 
          ((null first-arg)  ; fluent argument
           (mapcar (lambda (rest) (cons nil rest)) rest-instances))
          ((and (listp first-arg) (eql (car first-arg) 'either))  ; 'either' type
           (alexandria:mappend 
            (lambda (type)
              (alexandria:mappend
               (lambda (instance)
                 (mapcar (lambda (rest) (cons instance rest))
                         rest-instances))
               (gethash type *types*)))
            (cdr first-arg)))
          (t  ; regular type
           (alexandria:mappend
            (lambda (instance)
              (mapcar (lambda (rest) (cons instance rest))
                      rest-instances))
            (gethash first-arg *types*)))))))


(defun install-dynamic-relations (relations)
  (format t "~&Installing dynamic relations...")
  (iter (for relation in relations)
        (check-relation relation)
        (setf (gethash (car relation) *relations*)
              (ut::if-it (cdr relation)
                (sort-either-types ut::it)
                t))
        (ut::if-it (iter (for arg in (cdr relation))
                         (for i from 1)
                         (when ($varp arg)
                           (collect i)))
          (setf (gethash (car relation) *fluent-relation-indices*)
                ut::it))
        (finally (maphash (lambda (key val)  ;install implied unary relations
                            (declare (ignore val))
                            (setf (gethash key *static-relations*) '(something)))
                          *types*)
                 (add-proposition '(always-true) *static-db*)
                 (setf (gethash 'always-true *static-relations*) '(always-true))))
  ;; Install symmetric relations
  (iter (for (key val) in-hashtable *relations*)
    (when (and (not (eql val t))
               (not (alexandria:setp val))  ;multiple types
               (not (final-charp #\> key)))   ;not explicitly directed
      (setf (gethash key *symmetrics*) (symmetric-type-indexes val))))
  t)


(defmacro define-static-relations (&rest relations)
  `(install-static-relations ',relations))


(defun install-static-relations (relations)
  (format t "~&Installing static relations...")
  (iter (for relation in relations)
        (check-relation relation)
        (setf (gethash (car relation) *static-relations*)
              (ut::if-it (cdr relation) 
                (sort-either-types ut::it)
                nil))
        (ut::if-it (iter (for arg in (cdr relation))
                         (for i from 1)
                         (when ($varp arg)
                           (collect i)))
          (setf (gethash (car relation) *fluent-relation-indices*) ut::it))
        (finally (maphash #'(lambda (key val)  ;install implied unary relations
                              (declare (ignore val))
                              (setf (gethash key *static-relations*) '(everything)))
                          *types*)))
  (iter (for (key val) in-hashtable *static-relations*)  ;install symmetric relations
    (when (and (not (eql val t))
               (not (alexandria:setp val))  ;multiple types
               (not (final-charp #\> key)))   ;not explicitly directed
      (setf (gethash key *symmetrics*) (symmetric-type-indexes val))))
  (setf (gethash 'always-true *static-relations*) t)
  (setf (gethash 'waiting *static-relations*) t)
  t)


(defmacro define-complementary-relations (&rest positives->negatives)
  `(install-complementary-relations ',positives->negatives))


(defun install-complementary-relations (positives->negatives)
  (format t "~&Installing complementary relations...")
  (iter (for (positive nil negative) on positives->negatives by #'cdddr)
        (check-relation positive)
        (check-relation (second negative))
        (let ((ordered-pos (sort-either-types positive))
              (ordered-neg (list 'not (sort-either-types (second negative)))))
          (setf (gethash (car positive) *complements*)
          (list ordered-pos ordered-neg)))))

        
(defmacro define-happening (object &rest plist)
  `(install-happening ',object ',plist))


(defun install-happening (object plist)
  (format t "~&Installing happening for ~A ..." object)
  (check-happening object plist)
  (setf (symbol-plist object) nil)  ;overwrite any settings from a prior problem
  (when (getf plist :inits)
    (setf (get object :inits) (getf plist :inits)))
  (when (getf plist :events)
    (setf (the simple-vector (get object :events))
          (coerce (getf plist :events) 'simple-vector)))
  (when (getf plist :repeat)
    (setf (get object :repeat) (getf plist :repeat)))
  (push object *happening-names*)
  (when (getf plist :interrupt)
    (setf (get object :interrupt) (getf plist :interrupt)))
  (ut::if-it (getf plist :interrupt)
    (let (($vars (get-all-nonspecial-vars #'$varp ut::it)))
      (setf (symbol-value object)  ;(get object :interrupt) 
        `(lambda (state)
           (let ,$vars
             ,(when $vars
                `(declare (ignorable @,$vars)))
                ,(translate (getf plist :interrupt) 'pre))))))
  (fix-if-ignore '(state) (symbol-value object))  ; (get object :interrupt))
  ;(when (get object :rebound) 
  ;  (setf (get object :rebound) `(lambda (state+)
  ;                                 ,(translate (get object :rebound) 'pre)))
  ;  (setf (the function (get object :rebound-fn)) (compile nil (get object :rebound))))
  (dolist (literal (get object :inits))
    (when (eql (char (format nil "~S" literal) 0) #\`)
      (setq literal (eval literal)))
    (if (eql (car literal) 'not)
      (when (gethash (caadr literal) *relations*)
        (delete-proposition (second literal) *hap-db*))
      (when (gethash (car literal) *relations*)
        (add-proposition literal *hap-db*)))))
           

(defun check-happening (happening-object property-list)
  (check-type happening-object symbol)
  (check-type property-list cons)
  (iter (for (happening-keyword happening-property) on property-list by #'cddr)
        (check-type happening-keyword keyword)
        (case happening-keyword
          (:inits (check-type happening-property list)
                  (iter (for proposition in happening-property)
                        (check-proposition proposition)))
          (:events (check-type happening-property list)
                   (iter (for happening-event in happening-property)
                         (check-type (first happening-event) (integer 1 *))
                         (iter (for proposition in (cdr happening-event))
                               (check-proposition proposition))))
          (:repeat (check-type happening-property boolean))
          (:interrupt (check-type happening-property list)
                      (translate happening-property 'pre)))))


(defmacro define-query (name args body)
  `(install-query ',name ',args ',body))


(defun install-query (name args body)
  "Revised query function installation with read-only semantics"
  (format t "~&Installing ~A query-fn..." name)
  (check-query/update-function name args body)
  (push name *query-names*)
  (let ((new-$vars (delete-duplicates 
                     (set-difference (get-all-nonspecial-vars #'$varp body) args))))
    (setf (symbol-value name)
      `(lambda (state ,@args)
         ,(format nil "~A query-fn" name)
         (declare (ignorable state))
         (block ,name
           (let (,@new-$vars)
             (declare (ignorable ,@new-$vars))
             ;; Use pre context for read-only query semantics
             ,(if (eql (car body) 'let)
                `(let ,(second body)
                   ,(third body)
                   ,(translate (fourth body) 'pre))
                (translate body 'pre))))))
    (fix-if-ignore '(state) (symbol-value name))))


(defmacro define-update (name args body)
  `(install-update ',name ',args ',body))


(defun install-update (name args body)
  "Revised update function installation with explicit effect context"
  (format t "~&Installing ~A update-fn..." name)
  (check-query/update-function name args body)
  (push name *update-names*)
  (let ((new-$vars (delete-duplicates
                     (set-difference
                       (get-all-nonspecial-vars #'$varp body) args))))
    (if new-$vars
        (setf (symbol-value name)
          `(lambda (state ,@args)
             ,(format nil "~A update-fn" name)
             (declare (special changes-list) (ignorable state))  ;changes-list only used if backtracking
             (let (updated-dbs ,@new-$vars)
               (declare (ignorable updated-dbs ,@new-$vars))
               ,(translate body 'eff)
               (problem-state.idb state))))
        (setf (symbol-value name)
          `(lambda (state ,@args)
             ,(format nil "~A update-fn" name)
             (declare (special changes-list) (ignorable state))
             (progn
               ,(translate body 'eff)
               (problem-state.idb state)))))
    (fix-if-ignore '(state) (symbol-value name))))

  
(defmacro define-constraint (form)
  `(install-constraint ',form))


(defun install-constraint (form)
  (format t "~&Installing constraint...")
  (check-type form list)
  (let (($vars (get-all-nonspecial-vars #'$varp form)))
    (setf (symbol-value 'constraint-fn)
      `(lambda (state)
         (let ,$vars
           ,(when $vars
              `(declare (ignorable ,@$vars)))
           ,(translate form 'pre)))))
  (fix-if-ignore '(state) (symbol-value 'constraint-fn)))
        

(defmacro define-action (name duration pre-params precondition eff-params effect)
  `(install-action ',name ,duration ',pre-params ',precondition ',eff-params ',effect))


(defun install-action (name duration pre-params precondition eff-params effect)
  (format t "~&Installing ~A action..." name)
  (push (create-action name duration pre-params precondition eff-params effect nil)
        *actions*))


(defmacro define-init-action (name duration pre-params precondition eff-params effect)
  `(install-init-action ',name ,duration ',pre-params ',precondition ',eff-params ',effect))


(defun install-init-action (name duration pre-params precondition eff-params effect)
  (declare (ignore duration))
  (format t "~&Installing ~A init action..." name)
  (push (create-action name 0 pre-params precondition eff-params effect t)
    *init-actions*))


(defun create-action (name duration pre-params precondition eff-params effect init-action)
  (check-type name symbol)
  (check-type duration (real 0 *) "zero or a positive number")
  (check-precondition-parameters pre-params)
  (check-effect-parameters eff-params)
  (unless (member (first pre-params) *parameter-headers*)
    (push 'standard pre-params))
  (multiple-value-bind (pre-param-?vars pre-param-types) (dissect-pre-params pre-params)
    (let ((eff-param-vars eff-params))
      (let* ((flat-pre-param-?vars (alexandria:flatten pre-param-?vars))
             (pre-?vars (delete-duplicates (get-all-nonspecial-vars #'?varp precondition) :from-end t))
             (pre-$vars (delete-duplicates (get-all-nonspecial-vars #'$varp precondition) :from-end t))
             (pre-special-$vars (get-special-vars precondition))
             (pre-type-inst (instantiate-type-spec pre-param-types))
             (pre-bound-?vars (get-bound-?vars precondition))
             (eff-$vars (delete-duplicates (get-all-nonspecial-vars #'$varp effect) :from-end t))
             (eff-args (append flat-pre-param-?vars pre-$vars pre-special-$vars))
             (eff-?vars (delete-duplicates (get-all-nonspecial-vars #'?varp effect) :from-end t))
             (eff-bound-?vars (get-bound-?vars effect))
             (eff-free-?vars (set-difference eff-?vars eff-bound-?vars))
             (eff-extra-$vars (set-difference (set-difference eff-$vars pre-$vars)
                                                   pre-special-$vars))
             (eff-extra-?vars (ut::if-it (set-difference eff-free-?vars flat-pre-param-?vars)
                                (error "Extra effect ?vars in action ~A: ~A" name ut::it)))
             (eff-missing-vars (set-difference eff-args (append eff-free-?vars eff-$vars)))
             (queries (intersection (alexandria:flatten pre-param-types) *query-names*))
             (action nil))
        ;(ut::prt pre-?vars pre-$vars pre-special-$vars pre-bound-?vars
        ;         eff-?vars eff-$vars eff-bound-?vars eff-free-?vars eff-args
        ;         eff-extra-?vars eff-extra-$vars eff-missing-vars)
        (check-variable-names name (append flat-pre-param-?vars pre-bound-?vars eff-bound-?vars)
                              precondition effect (append pre-$vars eff-$vars pre-?vars eff-?vars))
        (cond (init-action
                 (setq *objective-value-p* nil))  ;this is an init-action, disable $objective-value
              ((or (member '$objective-value pre-$vars)  ;used in translate-assert
                   (member '$objective-value eff-extra-$vars))
                 (setq *objective-value-p* t))  ;this is a normal action rule with optimization
              (t (setq *objective-value-p* nil)))  ;normal rule, but no optimization
        (setq *eff-param-vars* eff-param-vars)  ;used in translate-assert
        (setf action (make-action
                       :name name
                       :pre-defun-name (ut::intern-symbol name '-PRE-FN)
                       :eff-defun-name (ut::intern-symbol name '-EFF-FN)
                       :duration duration
                       :precondition-params pre-params
                       :precondition-variables (append flat-pre-param-?vars pre-$vars)
                       :precondition-types pre-param-types
                       :precondition-type-inst pre-type-inst
                       :dynamic (when queries pre-type-inst)
                       :precondition-args (if queries
                                            '(nil)
                                            (let ((evaluation (eval-instantiated-spec pre-type-inst)))
                                              (if (equal evaluation '((nil)))
                                                '(nil)
                                                evaluation)))
                       :precondition-lambda `(lambda (state &rest args)
                                               ,(format nil "~A precondition" name)
                                               (declare (ignorable state))
                                               (destructuring-bind ,pre-param-?vars args
                                                 (let ,pre-$vars
                                                   (declare (ignorable ,@pre-$vars))
                                                   ,(if (eql (car precondition) 'let)
                                                      `(let ,(second precondition)
                                                         ,(third precondition)
                                                         (when ,(translate (fourth precondition) 'pre)
                                                           ,(if eff-args
                                                              `(list ,@eff-args)
                                                              `t)))
                                                      `(when ,(translate precondition 'pre)
                                                         ,(if eff-args
                                                            `(list ,@eff-args)
                                                            `t))))))
                       :effect-variables eff-param-vars  ;user listed parameter variables
                       :effect-lambda `(lambda (state ,@eff-args ,@eff-extra-?vars)
                                         ,(format nil "~A effect" name)
                                         (declare (ignorable ,@eff-args))
                                         (let (updated-dbs followups ,@(set-difference (set-difference eff-extra-$vars      eff-args) eff-extra-?vars))
                                           (declare (ignorable ,@eff-extra-$vars))
                                           ,(translate effect 'pre)  ;start as pre, shift to eff in assert
                                           updated-dbs))
                       :effect-adds nil))
        (fix-if-ignore '(state) (action.precondition-lambda action))
        (fix-if-ignore `(state ,@eff-missing-vars) (action.effect-lambda action))
        action))))


(defun get-bound-?vars (tree)
  "Retrieves the bound ?vars from a code tree."
  (let (?var-list)
    (ut::walk-tree (lambda (x)
                     (when (and (listp x)
                                (member (first x) '(exists exist forsome forall forevery doall)))
                       (setf ?var-list
                             (append ?var-list
                                     (remove-if-not #'?varp (alexandria:flatten (second x)))))))
                   tree)
    (remove-duplicates ?var-list)))


(defun get-special-vars (tree)
  "Collects any special declared variables from tree."
  (when (listp tree)
    (let (special-vars)
      (ut::walk-tree (lambda (item)
                       (if (and (listp item) (eql (car item) 'special))
                         (alexandria:appendf special-vars (cdr item))))
                     tree)
      special-vars)))


(defun get-all-vars (fn tree)
  "Selects one each of all variables in the tree satisfying fn."
  (remove-duplicates (remove-if-not fn (alexandria:flatten tree))))


(defun get-all-nonspecial-vars (fn tree)
  "Selects one each of non-special variables in the tree satisfying fn."
  (remove-duplicates (remove-if-not fn (set-difference (alexandria:flatten tree)
                                                       (get-special-vars tree)))))


(defun fix-if-ignore (symbols lambda-expr)
  "Ignores variable symbols that are not in the lambda-body."
  (let ((ignores (set-difference
                    symbols (get-all-nonspecial-vars (lambda (x)
                                                       (member x symbols))
                                                     (cddr lambda-expr)))))
    (when ignores
      (push `(declare (ignorable ,@ignores)) (cddr lambda-expr)))))


#+ignore (defmacro define-init (&body args)  ;allows list of inits, but use define-init-action instead
  (labels ((maybe-unquote (x)
             (if (and (consp x) (eq (car x) 'quote))
               (cadr x) x))
           (list-of-lists-p (x)
             (and (listp x) (or (null x) (listp (first x))))))
    (let* ((single (and args (null (cdr args))))
           (arg1   (and single (maybe-unquote (car args))))
           (literals (if (and single (list-of-lists-p arg1))
                       arg1
                       args)))
      `(install-init ',literals))))


(defmacro define-init (&rest literals)
  `(install-init ',literals))


(defun install-init (literals)
  ;(declare (special *relations* *db* *static-db*))
  (format t "~&Creating initial propositional database...")
  (check-type literals cons)
  (dolist (literal literals)
    (if (eql (car literal) 'not)
      (check-proposition (second literal))
      (check-proposition literal))
    (if (eql (car literal) 'not)
      (if (gethash (caadr literal) *relations*)
        (delete-proposition (second literal) *db*)
        (delete-proposition (second literal) *static-db*))
      (if (gethash (car literal) *relations*)
        (add-proposition literal *db*)
        (add-proposition literal *static-db*)))))


(defmacro define-goal (form)
  `(install-goal ',form))


(defun install-goal (form)
  (format t "~&Installing goal...")
  (check-type form list)
  (when (and (null form)
             (not (eql *solution-type* 'min-value))
             (not (eql *solution-type* 'max-value)))
    (error "Goal is required unless searching for a *solution-type* of min-value or max-value."))
  (let (($vars (get-all-nonspecial-vars #'$varp form)))
    (setf (symbol-value 'goal-fn)
      `(lambda (state)  ;save uncoded goal translation
         (let ,$vars
           ,(when $vars
              `(declare (ignorable ,@$vars)))
           ,(translate form 'pre)))))
  (setf (get 'goal-fn :form) form)
  (fix-if-ignore '(state) (symbol-value 'goal-fn)))


(defmacro define-invariant (name args body)
  "Define an invariant condition that must always be true.
   Registers the invariant for global checking during planning."
  `(progn
     ;; First define as a query function
     (install-query ',name ',args ',body)
     ;; Then register in the global invariants list
     (pushnew ',name *global-invariants*)
     ',name))

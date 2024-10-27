;;; Filename: ww-validator.lisp

;;; Tests to verify structures created from user inputs are error free.


(in-package :ww)


(defun check-relation (relation)
  "Checks for errors in a user-defined relation--eg, (height ?obj $fixnum)."
  (check-type relation cons)
  (check-type (car relation) symbol)
  (iter (for arg in (cdr relation))
        (check-type arg (or symbol cons))
        (or (nth-value 1 (gethash arg *types*))  ;a user type
            (and ($varp arg)  ;a $var incorporating a user or lisp defined type
                 (user-or-lisp-type-p (trim-1st-char arg)))
            (and (consp arg)
                 (eql (car arg) 'either)
                 (consp (cdr arg))
                 (every (lambda (type)
                          (gethash type *types*))
                        (cdr arg)))
            (error "The argument ~A is not valid in the user-defined relation ~A."
                   arg relation))))


(defun check-query/update-function (fn-name args body)
  "Detects an error in the supplied arguments to a user-defined
   query or update function--eg, (?queen $row $col)."
  (check-type fn-name symbol)
  (check-type args list)
  (check-type body list)
  (iter (for arg in args)
        (check-type arg symbol)))

        ;(or (?varp arg)
        ;    ($varp arg)
        ;    (eql arg '&rest)
        ;    (error "The argument ~A is not valid in the query or update function named ~A."
        ;            arg fn-name))))


(defun check-proposition (proposition)
  "Detects an error in a proposition--eg, (height block1 3)
   or (loc ?queen $row (1+ $col))."
  (check-type proposition cons)
  (when (eql (first proposition) 'not)
    (setf proposition (second proposition)))
  (check-predicate proposition)
  (iter (for arg in (cdr proposition))
        (for type-def in (or (gethash (first proposition) *relations*)  ;the type that goes with arg
                             (gethash (first proposition) *static-relations*)))
        (or (?varp arg)  ;arg is a ?var
            ($varp arg)  ;arg is a $var
            (member arg (gethash type-def *types*))  ;arg is a value of a user defined type
            (and ($varp type-def)  ;arg is a value of a user defined $type
                 (member arg (gethash (trim-1st-char type-def) *types*)))
            (and ($varp type-def)  ;arg is a value of a lisp type
                 (typep arg (trim-1st-char type-def)))
            (and (listp type-def)  ;arg is a value of a type combo
                 (eql (first type-def) 'either)
                 (member arg (iter (for type in (cdr type-def))
                                   (unioning (gethash type *types*)))))
            (and (listp arg)  ;arg is a lisp function or special lisp op
                 (or (fboundp (car arg))
                     (and (symbolp arg) (special-operator-p (car arg)))))
            (error "The argument ~A is not of specified type ~A in proposition ~A"
                           arg type-def proposition))))
                           

(defun check-query/update-call (fn-call)
  "Checks the validity of a call to a query or update function
   during translation--eg, (cleartop? ?block)"
  (check-type fn-call cons)
  (check-type (car fn-call) symbol)
  (iter (for arg in (cdr fn-call))
        (or (?varp arg)
            ($varp arg)
            (member arg (reduce #'union (alexandria:hash-table-values *types*)))  ;arg is a value of a type
            (numberp arg)
            (characterp arg)
            (stringp arg)
            (and (listp arg)
                 (or (fboundp (car arg))  ;arg is a lisp function
                     (special-operator-p (car arg))))  ;arg is a special lisp op
            (error "Found a malformed query or update argument ~A in ~A" arg fn-call))))


(defun check-variable-names (action-name pre-param-?vars precondition effect all-detected-vars)
  "Checks the validity (eg, spelling) of vars in an action rule."
  (let ((valid-vars pre-param-?vars))
    (subst-if t (constantly nil) `(list ,precondition ,effect)  ;adds valid $vars
              :key (lambda (item)
                     (when (consp item)
                       (case (first item)
                         ((setq setf) (when (symbolp (second item)) (push (second item) valid-vars)))
                         (ww-loop     (when (eq (second item) 'for)
                                        (typecase (third item)
                                          (symbol (push (third item) valid-vars))
                                          (list (alexandria:appendf valid-vars (third item)))))
                                      (when (eq (sixth item) 'for)
                                        (typecase (seventh item)
                                          (symbol (push (seventh item) valid-vars))
                                          (list (alexandria:appendf valid-vars (seventh item))))))
                         ((bind let)  (alexandria:appendf valid-vars
                                        (remove-if-not #'varp (second item))))))))
    (ut::if-it (set-difference all-detected-vars valid-vars)
      (error "Check spelling of unknown variables ~A in ~A" ut::it action-name))))
                 
                    
(defun check-precondition-parameters (pre-parameter-list)
  "Checks a user precondition action or logical parameter list."
  (check-type pre-parameter-list list)
  (iter (with state = 0)  ;0 is starting state, 1 is after finding a ?var-form 
        (for item in pre-parameter-list)
        (case state
          (0 (or (when (header-p item)
                   (setf state 0))
                 (when (subspec-p item)
                   (check-precondition-parameters item)
                   (setf state 0))
                 (when (?var-or-?var-list-p item)
                   (setf state 1))
                 (error "Expecting ~A to be a ?var or ?var-list in ~A" item pre-parameter-list)))
          (1 (or (when (type-or-query-or-either-p item)
                   (setf state 0))
                 (error "Expecting ~A to be a type, query-list, or either-list in ~A"
                        item pre-parameter-list))))))


(defun check-effect-parameters (eff-parameter-list)
  "Checks a user action effect."
  (check-type eff-parameter-list list)
  (unless (every #'varp eff-parameter-list)
    (error "Expecting only variables with a ? or $ prefix in an effect parameter list: ~A" eff-parameter-list)))

        
(defun check-predicate (proposition)
  "Detects an error in the use of an unknown predicate."
  (or (nth-value 1 (gethash (car proposition) *relations*))
      (nth-value 1 (gethash (car proposition) *static-relations*))
      (error "The predicate ~A in proposition ~A is not previously defined in a relation."
             (car proposition) proposition)))


(defun check-form-body (form)
  "Detects an error in a ww translated form expression."
  (when (fourth form)
    (error "The body of ~A must contain only one expression; eg, use 'do' to group expressions."
           form)))


(defun check-problem-parameter (param val)
  (case param
    (*problem-name* t)
    (*depth-cutoff* (unless (typep val 'fixnum)
                      (error "Can't set *depth-cutoff* to ~S. Must be an integer
                              where n<=0 means no cutoff." val)))
    (*randomize-search* (unless (typep val 'boolean)
                          (error "Can't set *randomize-search* to ~S. Must be either T or NIL." val)))
    (*tree-or-graph* (unless (member val '(tree graph))
                       (error "Can't set *tree-or-graph* to ~S. Must be either tree or graph." val)))
    (*problem-type* (unless (member val '(planning csp))
                      (error "Can't set *problem-type* to ~S.
                              Must be either planning or csp (ie, constraint satisfaction problem)." val)))
    (*solution-type* (unless (member val '(first every min-length min-time min-value max-value))
                       (error "Can't set *solution-type* to ~S. Must be one of
                               first, every, min-length, min-time, min-value, or max-value." val)))
    (*progress-reporting-interval* (unless (and (typep val 'fixnum) (> val 0))
                                     (error "Can't set *progress-reporting-interval* to ~S.
                                             Must be an integer > 0." val)))
    (*branch* (unless (typep val 'fixnum)
                (error "Can't set *branch* to ~S. Must be an integer
                        where n < 1 means search all branches." val)))
    (*debug* (unless (and (typep val 'fixnum) (>= val 0) (<= val 5))
                (error "Can't set *debug* to ~S. Must be an integer between 0 and 5." val)))
    (*probe* (unless (or (null val)
                         (and (listp val) (>= (length val) 3) (<= (length val) 4) (symbolp (first val)) (listp (second val))
                              (and (typep (third val) 'fixnum) (> (third val) 0))))
                (error "Can't set *probe* to ~S. Must be a list whose first element is an action,
                        whose second element is a list of instances for that action,
                        whose third element is the depth>0,
                        and whose optional fourth element is how many times to skip over previous instances." val)))
    (*threads* (error "Cannot set *threads* in the REPL or in a problem specification.
                       Specify the number of *threads* in ww-settings.lisp instead (but only if running SBCL),
                       then exit SBCL, and reload."))
    (otherwise (error "~S is not a valid parameter name in (ww-set ~S ~S)." param param val))))


(defun header-p (item)
  (member item *parameter-headers*))  ;header


(defun subspec-p (item)
  (and (listp item)  ;subspec
       (member (first item) *parameter-headers*)))


(defun ?var-or-?var-list-p (item)
  (or (?varp item)  ;?variable
      (and (listp item) (every #'?varp item))))  ;?variable list


(defun $var-or-$var-list-p (item)
  (or ($varp item)  ;$variable
      (and (listp item) (every #'$varp item))))  ;$variable list


(defun type-or-query-or-either-p (item)
  (or (nth-value 1 (gethash item *types*))  ;type
      (and (listp item)
           (or (member (first item) *query-names*)  ;query
               (and (eql (first item) 'either)  ;combo type
                    (every (lambda (type)
                             (nth-value 1 (gethash type *types*)))
                           (cdr item)))))))


(defun trim-1st-char (sym)
  "Trims the first character from a symbol--eg, $block -> block."
  (declare (type symbol sym))
  (intern (subseq (symbol-name sym) 1)))
    
    
(defun user-or-lisp-type-p (type)
  "Determines if a symbol is either a user-defined type or a lisp type."
  (or (nth-value 1 (gethash type *types*))
      (member type '(array atom bignum bit bit-vector boolean character compiled-function
                     complex cons double-float extended-char fixnum float function
                     hash-table integer keyword list long-float nil null number package
                     pathname random-state ratio rational real readtable sequence
                     short-float simple-array simple-bit-vector simple-string simple-vector
                     single-float standard-char stream string string-stream symbol t
                     unsigned-byte vector))))


(defun $varp (item)
  (and (symbolp item)
       (char= (char (symbol-name item) 0) #\$)))


(defun ?varp (item)
  (and (symbolp item)
       (char= (char (symbol-name item) 0) #\?)))


(defun varp (sym)
  (or (?varp sym)
      ($varp sym)))

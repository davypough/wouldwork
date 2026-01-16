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
            (lisp-type-p arg)  ;a Common Lisp type as non-fluent argument
            (and (consp arg)
                 (eql (car arg) 'either)
                 (consp (cdr arg))
                 ;; Validate either clause for fluent/non-fluent types
                 (let* ((either-types (cdr arg))
                        (fluent-count (count-if #'$varp either-types))
                        (all-fluent (= fluent-count (length either-types)))
                        (all-non-fluent (zerop fluent-count)))
                   ;; Check for mixed fluent/non-fluent
                   (unless (or all-fluent all-non-fluent)
                     (error "Mixed fluent and non-fluent types in either clause: ~A~%~
                             All types in an either clause must be consistently fluent or non-fluent."
                            arg))
                   ;; Validate all types (stripping $ if present)
                   (every (lambda (type)
                            (let ((base-type (if ($varp type)
                                               (trim-1st-char type)
                                               type)))
                              (nth-value 1 (gethash base-type *types*))))
                          either-types)))
            (error "The argument ~A is not valid in the user-defined relation ~A."
                   arg relation))))


(defun check-bijective-relation (relation)
  "Validates that a bijective relation meets required constraints:
   1. Exactly 2 arguments (excluding relation name)
   2. All arguments must be fluent.
   Signals an error if constraints are violated."
  (let ((relation-name (car relation))
        (args (cdr relation)))
    ;; Check exactly 2 arguments
    (unless (= (length args) 2)
      (error "Bijective relation ~A must have exactly 2 arguments, found ~D: ~A"
             relation-name (length args) args))
    ;; Check all arguments are fluent
    (iter (for arg in args)
          (for position from 1)
          (unless (fluent-spec-p arg)
            (error "Bijective relation ~A requires all arguments to be fluent.~%~
                    Argument ~D (~A) is not fluent."
                   relation-name position arg)))))


(defun check-query/update-function (fn-name args body)
  "Detects an error in the supplied arguments to a user-defined
   query or update function--eg, (?queen $row $col)."
  (check-type fn-name symbol)
  (check-type args list)
  (check-type body list)
  (iter (for arg in args)
        (check-type arg symbol)))


(defun check-proposition (proposition)
  "Detects an error in a proposition--eg, (height block1 3)
   or (loc ?queen $row (1+ $col))."
  (check-type proposition cons)
  (when (eql (first proposition) 'not)
    (setf proposition (second proposition)))
  (check-predicate proposition)
  ;(check-fluent-consistency proposition)
  (let ((relation-def (or (gethash (first proposition) *relations*)
                          (gethash (first proposition) *static-relations*))))
    (when (listp relation-def)
      (iter (for arg in (cdr proposition))
            (for type-def in relation-def)
            (for position from 1)  ; <<< CHANGED: Add position tracking
            (or (?varp arg)  ;arg is a ?var
                ($varp arg)  ;arg is a $var
                (member arg (gethash type-def *types*))  ;arg is a value of a user defined type
                (and (member position (get-prop-fluent-indices proposition))  ; <<< CHANGED: was ($varp type-def)
                     (or (null arg)
                         (member arg (gethash type-def *types*))))
                (and (member position (get-prop-fluent-indices proposition))  ; <<< CHANGED: was ($varp type-def)
                     (lisp-type-p type-def)  ; <<< ADDED: Guard against composite types
                     (typep arg type-def))
                (and (lisp-type-p type-def)  ;arg is a value of a Common Lisp type
                     (typep arg type-def))
                (and (listp type-def)  ;arg is a value of a type combo
                     (eql (first type-def) 'either)
                     (member arg (iter (for type in (cdr type-def))
                                       (unioning (gethash type *types*)))))
                (and (listp arg)  ;arg is a lisp function or special lisp op
                     (or (fboundp (car arg))
                         (and (symbolp arg) (special-operator-p (car arg)))))
                (error "The argument ~A is not of specified type ~A in proposition ~A"
                       arg type-def proposition))))))


(defun add-fluent-marker (type-spec)
  "Add $ prefix to type specification to indicate fluent position."
  (if (and (listp type-spec) (eq (car type-spec) 'either))
    (cons 'either 
          (mapcar (lambda (typ) (intern (format nil "$~A" typ) :ww))
                  (cdr type-spec)))
    (intern (format nil "$~A" type-spec) :ww)))


(defun format-relation-with-fluents (relation-name)
  "Returns a displayable relation spec with $ prefixes indicating fluent positions."
  (let ((types (gethash relation-name *relations*))
        (fluent-positions (gethash relation-name *fluent-relation-indices*)))
    (when types
      (cons relation-name
            (iter (for type in types)
                  (for position from 1)
                  (collect (if (member position fluent-positions)
                             (add-fluent-marker type)
                             type)))))))


(defun check-bind-fluent-consistency (proposition)
  "Validates that bind statements contain at least one $variable in a fluent position.
   Fluent positions can contain:
   - $variable: receives the bound value from the database
   - ?variable: serves as a lookup key (already-bound pattern variable)
   - literal value: serves as a lookup key (validated by check-proposition)"
  (let ((relation-name (car proposition))
        (fluent-positions (get-prop-fluent-indices proposition)))
    (when fluent-positions
      (unless (iter (for arg in (cdr proposition))
                    (for position from 1)
                    (thereis (and (member position fluent-positions)
                                  ($varp arg))))
        (error "~%Bind statement is inconsistent with relation definition.~%~
                  Statement: ~S~%~
                  Relation:  ~S~%~
                  Error: At least one fluent position must contain a $variable"
               (list 'bind proposition)
               (format-relation-with-fluents relation-name))))))
                         

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
                         (mvsetq      (alexandria:appendf valid-vars
                                        (remove-if-not #'varp (second item))))
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
      (error "Check spelling or use of unknown variables ~A in ~A" ut::it action-name))))
                 
                    
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


(defun check-action-parameter-instantiability (action-name pre-param-types)
  "Checks if all static parameter types have at least one instance.
   Returns a list of uninstantiable type names, or NIL if all types are instantiable.
   Dynamic types (queries) are skipped as they are evaluated at runtime."
  (let ((uninstantiable-types nil))
    (labels ((check-types (spec)
               (dolist (item spec)
                 (cond
                   ;; Skip headers (standard, product, combination, dot-product)
                   ((member item *parameter-headers*)
                    nil)
                   ;; Skip query forms (dynamic - evaluated at runtime)
                   ((and (listp item)
                         (member (first item) *query-names*))
                    nil)
                   ;; Recurse into nested subspecs
                   ((and (listp item)
                         (member (first item) *parameter-headers*))
                    (check-types item))
                   ;; Static type symbol - check for instances
                   ((symbolp item)
                    (multiple-value-bind (instances exists-p) (gethash item *types*)
                      (when (and exists-p (null instances))
                        (pushnew item uninstantiable-types))))))))
      (check-types pre-param-types))
    (nreverse uninstantiable-types)))

        
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
    (*algorithm* (unless (member val '(depth-first backtracking))
                   (error "Can't set *algorithm* to ~S. Must be either depth-first or backtracking." val)))
    (*solution-type* (unless (member val '(first every min-length min-time min-value max-value))
                       (error "Can't set *solution-type* to ~S. Must be one of
                               first, every, min-length, min-time, min-value, or max-value." val)))
    (*progress-reporting-interval* (unless (and (typep val 'fixnum) (> val 0))
                                     (error "Can't set *progress-reporting-interval* to ~S.
                                             Must be an integer > 0." val)))
    (*branch* (unless (typep val 'fixnum)
                (error "Can't set *branch* to ~S. Must be an integer
                        where n < 1 means search all branches." val)))
    (*debug* (unless (or (and (typep val 'fixnum) (>= val 0) (<= val 5)) (= val 0.5))
                (error "Can't set *debug* to ~S. Must be an integer between 0 and 5." val)))
    (*probe* (unless (or (null val)
                         (and (listp val) (>= (length val) 3) (<= (length val) 4) (symbolp (first val))
                              (listp (second val)) (typep (third val) 'fixnum) (> (third val) 0)
                              (member (first val) (mapcar #'action.name *actions*))))
                (error "Can't set *probe* to ~S. Must be a list whose first element is an action,
                        whose second element is a list of instances for that action,
                        whose third element is the depth>0,
                        and whose optional fourth element is how many times to skip over previous instances." val)))
    (*auto-wait* (unless (typep val 'boolean)
                   (error "Can't set *auto-wait* to ~S. Must be either T or NIL." val))
                 (when val  ; Only check dependencies when enabling
                   (unless (eql *tree-or-graph* 'tree)
                     (error "Can't enable *auto-wait* when *tree-or-graph* is ~S. ~
                             *auto-wait* requires *tree-or-graph* = TREE." *tree-or-graph*))
                   (unless (eql *problem-type* 'planning)
                     (error "Can't enable *auto-wait* when *problem-type* is ~S. ~
                             *auto-wait* requires *problem-type* = PLANNING." *problem-type*))
                   (unless (zerop *threads*)
                     (error "Can't enable *auto-wait* when *threads* is ~D. ~
                             *auto-wait* requires *threads* = 0." *threads*))
                   (unless (eql *algorithm* 'depth-first)
                     (error "Can't enable *auto-wait* when *algorithm* is ~S. ~
                             *auto-wait* requires *algorithm* = DEPTH-FIRST." *algorithm*))))
                   ;(unless *happening-names*
                   ;  (error "Can't enable *auto-wait* without exogenous happenings. ~
                   ;          Define patrollers or other happening objects first.")))
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
      (lisp-type-p type)))


(defun lisp-type-p (type)
  "Determines if a symbol is a valid Common Lisp type."
  (and (symbolp type)
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


;;;;;;;;;;; User test for an action rule ;;;;;;;;;;;;;;

(defun check-action (action-name &key add)
  "Test an action by finding a valid instantiation and showing the effect.
   :ADD is a list of propositions to add to the test state to satisfy preconditions."
  (let ((action (find action-name *actions* :key #'action.name)))
    (unless action
      (format t "Action ~A not found.~%" action-name)
      (return-from check-action nil))
    
    ;; Create test state based on start state
    (let ((test-state (copy-problem-state *start-state*)))
      
      ;; Add any extra propositions needed to satisfy preconditions
      (when add
        (dolist (prop add)
          (add-proposition prop (problem-state.idb test-state)))
        (format t "~%Added propositions to test state:~%~S~%" add))
      
      (format t "~%TESTING ACTION: ~A~%~%" action-name)
      (format t "BEFORE STATE:~%~A~%~%" (list-database (problem-state.idb test-state)))
      
      ;; Try each precondition argument to find one valid instantiation
      (dolist (arg-set (action.precondition-args action) 
               (format t "FAILED: No valid instantiation found for action ~A~%" action-name))
        (let ((result (apply (action.pre-defun-name action) test-state arg-set)))
          (when result
            ;; Success - found a valid instantiation
            (format t "VALID INSTANTIATION: ~A~%" arg-set)
            
            ;; Map parameters from precondition result to variables
            (let* ((param-values (if (eq result t) nil result))
                   ;; Create a map of variable names to their resolved values
                   (var-map (if (and (listp result) (not (eq result t)))
                              (loop for val in result
                                    for var in (action.precondition-variables action)
                                    collect (cons var val))
                              nil))
                   ;; Get effect variables, with resolved values when available
                   (effect-var-values 
                     (mapcar (lambda (var)
                               (let ((val-pair (assoc var var-map)))
                                 (if val-pair (cdr val-pair) var)))
                             (action.effect-variables action))))
              
              ;; Show the action with as many resolved variables as possible
              (format t "ACTION: (~A~{ ~A~})~%~%" 
                      (action.name action)
                      (mapcar (lambda (x) 
                                (if (symbolp x) x (format nil "~A" x)))
                              effect-var-values)))
            
            ;; Apply effect function
            (let* ((updated-dbs (if (eql result t)
                                   (funcall (action.eff-defun-name action) test-state)
                                   (apply (action.eff-defun-name action) test-state result))))
              
              ;; Show each update result
              (dolist (update updated-dbs)
                (format t "AFTER STATE:~%~A~%~%" 
                        (list-database (update.changes update))))
              (return t))))))))
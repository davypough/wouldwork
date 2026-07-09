;;; Filename: ww-translator.lisp

;;; Translates a domain file containing formulas into lisp.


(in-package :ww)


(defparameter *proposition-read-mode* :unbound
  "Dynamic variable controlling read/write mode during translation.
   When :unbound: Use context-dependent behavior (default)
   When t: Force read operations (queries only) 
   When nil: Force write operations")


(defparameter *within-quantifier* nil
  "Dynamic variable indicating whether translation is occurring within a quantifier context.
   When T, IF statements return explicit T/NIL for control flow semantics.
   When NIL, IF statements preserve natural value-returning semantics.")


(defparameter *var-type-env* nil
  "Dynamic alist of (?var . declared-type) bindings in effect during translation of the
   current action precondition/effect or query/update body, including any locally-typed
   exists/forall/doall parameters encountered along the way. Consulted by CHECK-PROPOSITION
   and CHECK-QUERY/UPDATE-CALL to cross-check a ?var's declared type against the relation
   or query/update signature it's used against. NIL, or a ?var absent from the alist, means
   no declared type is known, in which case the usual permissive ?varp short-circuit applies.")


(defun type-spec-instances (type-spec)
  "Returns the list of instances a declared type-spec resolves to, or :unknown when
   TYPE-SPEC can't be statically resolved to a fixed instance set -- a dynamic query-call
   type, a bare Common Lisp type (eg, fixnum), or a type/either-composite whose declared
   instances are empty or the (nil) placeholder used for an alias whose base type is
   absent from the current problem. :unknown signals the caller to pass leniently rather
   than risk a false type mismatch against something it has no real information about."
  (let ((instances
          (cond ((and (consp type-spec) (eql (car type-spec) 'either))
                 (remove-duplicates
                   (apply #'append (mapcar (lambda (subtype) (copy-list (gethash subtype *types*)))
                                          (cdr type-spec)))))
                ((symbolp type-spec) (gethash type-spec *types*))
                (t nil))))
    (if (or (null instances) (equal instances '(nil)))
      :unknown
      instances)))


(defun type-specs-compatible-p (declared-type relation-type-def)
  "Returns T if DECLARED-TYPE (a ?var's declared parameter type) and RELATION-TYPE-DEF
   (a relation or query/update's declared type at the argument position in question)
   share at least one instance, so a ?var of DECLARED-TYPE could plausibly satisfy
   RELATION-TYPE-DEF at runtime. Passes leniently (returns T) when either side resolves
   to :unknown, since an inability to reason about a type-spec is not evidence of a
   mismatch."
  (let ((declared-instances (type-spec-instances declared-type))
        (relation-instances (type-spec-instances relation-type-def)))
    (or (eql declared-instances :unknown)
        (eql relation-instances :unknown)
        (and (intersection declared-instances relation-instances) t))))


(defun get-database-reference (form flag)
  "Determines appropriate database reference for proposition evaluation.
   Handles static relations, dynamic relations, and happening contexts consistently."
  (declare (ignore flag))  ; Flag no longer affects database selection
  (if (gethash (car form) *relations*)
      ;; Dynamic relation - check for happening context
      (if *happening-names*
          '(merge-idb-hidb state)
          `(problem-state.idb state))  ; Unified state reference
      ;; Static relation - always use static database
      '*static-db*))


(defun generate-fluent-bindings (prop-fluents)
  "Generates setf forms for binding fluent variables to retrieved values.
   Optimizes accessor patterns for common cases while handling arbitrary arity."
  (mapcan #'list 
          prop-fluents
          (loop for i from 0 below (length prop-fluents)
                collect (case i
                          (0 '(first vals))
                          (1 '(second vals))
                          (2 '(third vals))
                          (3 '(fourth vals))
                          (otherwise `(nth ,i vals))))))


(defun merge-idb-hidb (state)
  "Merges the two databases of state."
  (let ((idb (alexandria:copy-hash-table (problem-state.idb state))))
    (maphash (lambda (key val)
               (setf (gethash key idb) val))
             (problem-state.hidb state))
    idb))


(defun translate-value-expression (item flag)
  "Return Lisp code that evaluates ITEM as a proposition value."
  (cond ((or (varp item)
             (numberp item)
             (stringp item)
             (characterp item)
             (null item))
         item)
        ((and (listp item)
              (translatable-expression-form-p item))
         (translate item flag))
        ((listp item) `(quote ,item))
        (t `(quote ,item))))


(defun translate-list (form flag)
  "Most basic form translation."
  (check-proposition form)
  `(list ,@(iter (for item in form)
             (collect (translate-value-expression item flag)))))


(defun translate-simple-atom (form flag)
  "Translates propositions without fluent variables for database lookup.
   For bijective relations, uses the indexed lookup format (e.g., ON1)
   since the canonical key is not stored in the database."
  (let ((state-db (if (gethash (car form) *relations*)
                      (if *happening-names*
                          '(merge-idb-hidb state)
                          `(problem-state.idb state))
                      '*static-db*))
        (index-names (gethash (car form) *bijective-relations*)))
    ;; Bijective relation handling block
    (if index-names
        ;; Bijective relation: use index1 (keyed by position 1)
        ;; Storage format: (ON1 arg1) -> (arg2)
        ;; Check if retrieved value matches arg2
        (let* ((index1-name (first index-names))
               (arg1 (second form))
               (arg2 (third form)))
          `(equalp (gethash ,(translate-list (list index1-name arg1) flag) ,state-db)
                   (list ,(translate-value-expression arg2 flag))))
        ;; Non-bijective relation: direct lookup
        `(eql t (gethash ,(translate-list form flag) ,state-db)))))


(defun translate-fluent-atom (form flag)
  "Translates propositions with fluent variables using standardized database reference.
   For bijective relations, uses the indexed lookup format (e.g., ON1)
   since the canonical key is not stored in the database."
  (let* ((fluent-indices (get-prop-fluent-indices form))
         (database-ref (get-database-reference form flag))
         (index-names (gethash (car form) *bijective-relations*)))
    ;; Bijective relation handling block
    (if index-names
        ;; Bijective relation: use index1 (keyed by position 1)
        ;; Storage format: (ON1 arg1) -> (arg2)
        ;; Check if retrieved value matches arg2
        (let* ((index1-name (first index-names))
               (arg1 (second form))
               (arg2 (third form)))
          `(equalp (gethash ,(translate-list (list index1-name arg1) flag) ,database-ref)
                   (list ,(translate-value-expression arg2 flag))))
        ;; Non-bijective relation
        (multiple-value-bind (fluentless-atom fluents)
            (ut::split-at-indexes fluent-indices form)
          `(equalp (gethash ,(translate-list fluentless-atom flag) ,database-ref)
                   (list ,@(mapcar (lambda (x)
                                     (translate-value-expression x flag))
                                   fluents)))))))


(defun translate-proposition (form flag)
  "Distinguishes fluent from non-fluent propositions."
  (check-proposition form)
  (if (get-prop-fluent-indices form)
    (translate-fluent-atom form flag)
    (translate-simple-atom form flag)))


(defun write-operation-p (flag)
  "Returns t if we should perform write operations (database updates).
   Write operations occur in effect contexts unless read mode is explicitly forced."
  (and (eq flag 'eff) (not (forced-read-mode-p))))


(defun forced-read-mode-p ()
  "Returns t if read mode has been explicitly forced via *proposition-read-mode*."
  (and (boundp '*proposition-read-mode*)
       (not (eq *proposition-read-mode* :unbound))
       (eq *proposition-read-mode* t)))  ;eg, in the condition of an if statement in an effect


(defun translate-positive-relation (form flag)
  "Unified positive relation translation with context-aware read/write determination.
   For backtracking: applies changes incrementally while tracking forward and inverse.
   Routes static relations to *static-db* and dynamic relations to state idb."
  (declare (special forward-list inverse-list))
  (if (write-operation-p flag)
    (let ((db (if (gethash (car form) *relations*)              ; ← changed: dispatch on relation type
                  `(problem-state.idb state)
                  '*static-db*)))
      (if (eq *algorithm* 'backtracking)
        ;; Backtracking with incremental updates
        `(if (and (boundp 'forward-list) (boundp 'inverse-list))
           (multiple-value-bind (forward inverse) 
               (update-bt ,db ,(translate-list form flag))     ; ← changed: use db
             ;; UPDATE-BT applies the forward operation immediately and also returns inverse.
             (push forward forward-list)
             (push inverse inverse-list))
           ;; No active incremental-update context (eg, standalone propagation update):
           ;; apply directly without logging forward/inverse operations.
           (update ,db ,(translate-list form flag)))            ; ← changed: use db
        ;; Depth-first algorithm
        `(update ,db ,(translate-list form flag))))             ; ← changed: use db
    ;; Read operation
    (translate-proposition form flag)))


(defun translate-negative-relation (form flag)
  "Unified negative relation translation with incremental updates for backtracking.
   Routes static relations to *static-db* and dynamic relations to state idb."
  (declare (special forward-list inverse-list))
  (if (write-operation-p flag)
    (let ((db (if (gethash (car (second form)) *relations*)    ; ← changed: dispatch on relation type
                  `(problem-state.idb state)
                  '*static-db*)))
      (if (eq *algorithm* 'backtracking)
        ;; Backtracking with incremental updates
        `(if (and (boundp 'forward-list) (boundp 'inverse-list))
           (multiple-value-bind (forward inverse) 
               (update-bt ,db (list 'not ,(translate-list (second form) flag)))  ; ← changed: use db
             ;; UPDATE-BT applies the forward operation immediately and also returns inverse.
             (push forward forward-list)
             (push inverse inverse-list))
           ;; No active incremental-update context (eg, standalone propagation update):
           ;; apply directly without logging forward/inverse operations.
           (update ,db (list 'not ,(translate-list (second form) flag))))       ; ← changed: use db
        ;; Depth-first algorithm
        `(update ,db (list 'not ,(translate-list (second form) flag)))))        ; ← changed: use db
    ;; Read operation
    `(not ,(translate-positive-relation (second form) flag))))


(defun explicit-state-argument-p (function-name args)
  "Return true for the temporary legacy shape (query state ...)."
  (let ((raw-args (get function-name :raw-args :missing)))
    (and args
         (eq (first args) 'state)
         (not (eq raw-args :missing))
         (= (length args) (1+ (length raw-args))))))


(defun translate-function-call-argument (arg flag)
  "Translate one source-level query/update argument."
  (translate-value-expression arg flag))


(defun translate-function-call (form flag)
  "Translate a query or update call, adding the implicit state argument."
  (let* ((function-name (car form))
         (source-args (cdr form))
         (args (if (explicit-state-argument-p function-name source-args)
                 (cdr source-args)
                 source-args))
         (state-arg 'state)
         (fn-call (append (list function-name state-arg)
                          (mapcar (lambda (arg)
                                    (translate-function-call-argument arg flag))
                                  args))))
    (check-query/update-call (cons function-name args))
    ;; Enhanced validation with robust update function detection
    ;; Allow update calls in 'eff (action effects) and 'pre (goal validation) contexts
    (when (and (update-function-p function-name)
               (not (member flag '(eff pre))))
      (error "Update function ~A cannot be called in ~A context" 
             function-name flag))
    `,fn-call))


(defun update-function-p (function-name)
  "Robust update function detection using multiple criteria"
  (or (member function-name *update-names*)
      (and (symbolp function-name)
           (let ((name-string (symbol-name function-name)))
             (and (> (length name-string) 0)
                  (char= (char name-string (1- (length name-string))) #\!))))))


(defun get-prop-fluents (proposition &optional indices)
  "Returns the fluent values in an arbitrary proposition."
  (let ((fluent-indices (or indices (get-prop-fluent-indices proposition))))
    (when fluent-indices
      (loop with remaining-indices = fluent-indices
            for i from 0
            for item in proposition
            when (and remaining-indices (= i (first remaining-indices)))
              collect (if (and (symbolp item) (boundp item))
                          (symbol-value item)
                          item)
              and do (setf remaining-indices (rest remaining-indices))))))


(defun validate-bind-form (form)
  "Validates bind form structure and proposition format.
   Ensures proper syntax before translation proceeds."
  (unless (and (consp form)
               (eq (first form) 'bind)
               (consp (second form)))
    (error "Invalid bind form structure: ~A" form))
  (check-proposition (second form))
  (check-bind-relation-has-fluent (second form))    ;; ADDED: bind on a fluentless relation is an error
  (check-bind-fluent-consistency (second form)))


(defun select-bijective-index-proposition (proposition)
  "For a bijective relation in a bind form, returns index selection info.
   Input: proposition like (on $block $support)
   Returns: (index1-name index2-name $var1 $var2) if bijective, NIL otherwise.
   - index1-name: internal index keyed by position 1
   - index2-name: internal index keyed by position 2
   - $var1, $var2: the fluent variables at positions 1 and 2"
  (let* ((relation-name (car proposition))
         (args (cdr proposition))
         (index-names (gethash relation-name *bijective-relations*)))
    (when index-names
      (list (first index-names)    ; index1-name (e.g., ON1)
            (second index-names)   ; index2-name (e.g., ON2)
            (first args)           ; $var1 (e.g., $BLOCK)
            (second args)))))      ; $var2 (e.g., $SUPPORT)


(defun quote-if-constant (item)
  "Returns a form that evaluates to item. Variables are kept as references;
   constants are quoted."
  (if (varp item)
      item
      `',item))


(defun translate-bind (form flag)
  "Revised binding translation with unified state reference strategy.
   Translates binding operations like (bind (loc ?obj $area)) where fluent variables
   get bound to values retrieved from the database. Always performs read-only queries
   regardless of syntactic context.
   For bijective relations, performs compile-time analysis to determine which index
   to use based on variable types. Only $-variables may be unbound; ?-variables and
   constants are always bound. Only generates runtime selection when both variables
   are $-variables (binding state unknown at compile time).
   Returns:
   - t if proposition found and variables successfully bound
   - nil if proposition not found in database
   Side effects:
   - Sets fluent variables to corresponding values from retrieved proposition"
  ;; Input validation and structure extraction
  (validate-bind-form form)
  (let* ((proposition (second form))
         (bijective-info (select-bijective-index-proposition proposition))
         (database-ref (get-database-reference proposition flag)))
    (if bijective-info
        ;; Handle bijective relation with compile-time index selection when possible
        (destructuring-bind (index1-name index2-name var1 var2) bijective-info
          (let ((var1-unbound ($varp var1))
                (var2-unbound ($varp var2)))
            (cond
              ;; Both are non-$: neither can receive a value - error
              ((and (not var1-unbound) (not var2-unbound))
               (error "Bijective bind ~A: both arguments are bound (~A, ~A), ~
                       neither can receive a value" proposition var1 var2))
              ;; var1 is bound (not $var) - use index1 to look up var2
              ((not var1-unbound)
               `(multiple-value-bind (vals present-p)
                    (gethash (list ',index1-name ,(quote-if-constant var1)) ,database-ref)
                  (when present-p
                    (setf ,var2 (first vals))
                    t)))
              ;; var2 is bound (not $var) - use index2 to look up var1
              ((not var2-unbound)
               `(multiple-value-bind (vals present-p)
                    (gethash (list ',index2-name ,(quote-if-constant var2)) ,database-ref)
                  (when present-p
                    (setf ,var1 (first vals))
                    t)))
              ;; Both are $-variables: binding state unknown, need runtime check
              (t
               `(cond
                  ;; var1 bound, var2 unbound → use index1 (keyed by position 1)
                  ((and ,var1 (null ,var2))
                   (multiple-value-bind (vals present-p)
                       (gethash (list ',index1-name ,var1) ,database-ref)
                     (when present-p
                       (setf ,var2 (first vals))
                       t)))
                  ;; var2 bound, var1 unbound → use index2 (keyed by position 2)
                  ((and ,var2 (null ,var1))
                   (multiple-value-bind (vals present-p)
                       (gethash (list ',index2-name ,var2) ,database-ref)
                     (when present-p
                       (setf ,var1 (first vals))
                       t)))
                  (t (error "Bijective bind ~A requires exactly one variable bound, ~
                             got ~A=~A, ~A=~A"
                            ',proposition ',var1 ,var1 ',var2 ,var2)))))))
        ;; Handle normal relation (existing logic)
        (let* ((fluent-indices (get-prop-fluent-indices proposition))
               (fluentless-atom (ut::remove-at-indexes fluent-indices proposition))
               (prop-fluents (get-prop-fluents proposition)))
          ;; Generate database lookup and conditional binding
          `(multiple-value-bind (vals present-p)
               (gethash ,(translate-list fluentless-atom flag) ,database-ref)
             (declare (ignorable vals))
             (when present-p
               ,(cond
                  ;; Case 1: Fluent variables present - perform binding
                  (prop-fluents
                   `(progn (setf ,@(generate-fluent-bindings prop-fluents))
                           t))
                  ;; Case 2: No fluent variables - simple existence check
                  (t 't))))))))


(defun static-single-quantifier-domain (vars types)
  "Return VAR and static DOMAIN for a one-variable quantifier, if it is simple."
  (when (and (consp vars)
             (null (cdr vars))
             (consp types)
             (member (first types) *parameter-headers*)
             (consp (rest types))
             (null (cddr types)))
    (let ((type (second types)))
      (multiple-value-bind (domain present-p) (gethash type *types*)
        (when present-p
          (values (first vars)
                  (if (equal domain '(nil)) nil domain)
                  t))))))


(defun empty-quantifier-collection-p (collection)
  "Return true when COLLECTION would make a translated quantifier skip its body."
  (not (and collection (caar collection))))


(defun static-empty-quantifier-truth (form)
  "Return the truth value of a quantifier over a statically empty domain, if known."
  (when (and (consp form)
             (member (first form) '(forsome exists exist forall forevery doall))
             (consp (second form))
             (listp (second form)))
    (let ((parameters (copy-list (second form))))
      (handler-case
          (progn
            (unless (member (first parameters) *parameter-headers*)
              (setf parameters (cons 'standard parameters)))
            (multiple-value-bind (pre-param-?vars pre-param-types)
                (dissect-pre-params parameters)
              (let* ((queries (intersection (alexandria:flatten pre-param-types)
                                             *query-names*))
                     (type-inst (instantiate-type-spec pre-param-types))
                     (static-collection
                       (unless queries
                         (ut::transpose (eval-instantiated-spec type-inst)))))
                (multiple-value-bind (var domain static-single-p)
                    (and (null queries)
                         (static-single-quantifier-domain pre-param-?vars
                                                          pre-param-types))
                  (declare (ignore var))
                  (when (or (and static-single-p (null domain))
                            (and (null queries)
                                 (empty-quantifier-collection-p static-collection)))
                    (if (member (first form) '(forsome exists exist))
                      :false
                      :true))))))
        (error () :unknown)))))


(defun translate-empty-static-quantifier (body flag result)
  "Emit RESULT for an empty static quantifier without translating its unreachable BODY."
  (declare (ignore body flag))
  result)


(defun empty-static-type-p (type)
  "Return true when TYPE is known and has no possible instances in this problem."
  (multiple-value-bind (instances present-p) (gethash type *types*)
    (and present-p
         (or (null instances)
             (equal instances '(nil))))))


(defun empty-static-type-predicate-p (form)
  "Return true for unary type predicates over known empty static types."
  (and (consp form)
       (symbolp (first form))
       (null (cddr form))
       (empty-static-type-p (first form))))


(defun static-literal-value (form)
  "Return FORM's literal value and T when FORM is statically self-evaluating."
  (cond ((null form) (values nil t))
        ((eq form t) (values t t))
        ((or (keywordp form)
             (numberp form)
             (stringp form)
             (characterp form))
         (values form t))
        ((and (consp form)
              (eq (first form) 'quote)
              (= (length form) 2))
         (values (second form) t))
        (t (values nil nil))))


(defun case-default-clause-p (clause lastp)
  "Return true when CLAUSE is a final CASE default clause."
  (and lastp
       (member (first clause) '(t otherwise))))


(defun case-clause-matches-key-p (key clause lastp)
  "Return true when static CASE key KEY selects CLAUSE."
  (let ((keys (first clause)))
    (or (case-default-clause-p clause lastp)
        (if (consp keys)
          (member key keys :test #'eql)
          (eql key keys)))))


(defun static-case-selected-clause (key clauses)
  "Return the CASE clause selected by static KEY, if any."
  (loop for remaining on clauses
        for clause = (first remaining)
        do (unless (consp clause)
             (error "Invalid CASE clause (must be list): ~A" clause))
        when (case-clause-matches-key-p key clause (null (rest remaining)))
          do (return (values clause t))
        finally (return (values nil nil))))


(defun static-form-truth (form)
  "Classify simple forms whose truth is known from empty optional types.
Returns :TRUE, :FALSE, or :UNKNOWN.  This is deliberately narrow: non-empty type
predicates stay unknown because their argument may or may not be an instance."
  (labels ((truth (item)
             (multiple-value-bind (literal literalp) (static-literal-value item)
               (if literalp
                 (if literal :true :false)
                 (let ((empty-quantifier-truth (static-empty-quantifier-truth item)))
                   (cond
                     ((member empty-quantifier-truth '(:true :false))
                      empty-quantifier-truth)
                     ((empty-static-type-predicate-p item) :false)
                     ((atom item) :unknown)
                     ((and (eql (first item) 'not)
                           (= (length item) 2))
                      (case (truth (second item))
                        (:true :false)
                        (:false :true)
                        (otherwise :unknown)))
                     ((and (eql (first item) 'and)
                           (rest item))
                      (loop with all-true = t
                            for operand in (rest item)
                            for operand-truth = (truth operand)
                            when (eql operand-truth :false)
                              do (return :false)
                            when (not (eql operand-truth :true))
                              do (setf all-true nil)
                            finally (return (if all-true :true :unknown))))
                     ((and (eql (first item) 'or)
                           (rest item))
                      (loop with all-false = t
                            for operand in (rest item)
                            for operand-truth = (truth operand)
                            when (eql operand-truth :true)
                              do (return :true)
                            when (not (eql operand-truth :false))
                              do (setf all-false nil)
                            finally (return (if all-false :false :unknown))))
                     ((eql (first item) 'if) (if-truth item))
                     ((eql (first item) 'cond) (cond-truth item))
                     ((eql (first item) 'case) (case-truth item))
                     (t :unknown))))))
           (progn-truth (forms)
             (if forms
               (truth (car (last forms)))
               :false))
           (if-truth (item)
             (case (truth (second item))
               (:true (truth (third item)))
               (:false (if (fourth item)
                         (truth (fourth item))
                         :false))
               (otherwise :unknown)))
           (cond-truth (item)
             (if (not (cdr item))
               :unknown
               (dolist (clause (cdr item) :false)
                 (unless (consp clause)
                   (return :unknown))
                 (let ((test-truth (truth (first clause))))
                   (cond ((eql test-truth :true)
                          (return (if (rest clause)
                                    (progn-truth (rest clause))
                                    :true)))
                         ((not (eql test-truth :false))
                          (return :unknown)))))))
           (case-truth (item)
             (multiple-value-bind (key knownp) (static-literal-value (second item))
               (if knownp
                 (multiple-value-bind (clause foundp)
                     (static-case-selected-clause key (cddr item))
                   (if foundp
                     (progn-truth (rest clause))
                     :false))
                 :unknown))))
    (truth form)))


(defun static-read-context-p (flag)
  "Return true when connective translation is evaluating propositions, not writing them."
  (or (eq flag 'pre)
      (forced-read-mode-p)))


(defun translate-simplified-connective (operator operands flag)
  "Translate OPERATOR after pruning operands made unreachable by empty optional types."
  (case operator
    (and
     (cond ((some (lambda (operand)
                    (eql (static-form-truth operand) :false))
                  operands)
            nil)
           (t
            (let ((remaining (remove-if (lambda (operand)
                                          (eql (static-form-truth operand) :true))
                                        operands)))
              (cond ((null remaining) t)
                    ((null (rest remaining)) (translate (first remaining) flag))
                    (t `(and ,@(mapcar (lambda (operand)
                                          (translate operand flag))
                                        remaining))))))))
    (or
     (cond ((some (lambda (operand)
                    (eql (static-form-truth operand) :true))
                  operands)
            t)
           (t
            (let ((remaining (remove-if (lambda (operand)
                                          (eql (static-form-truth operand) :false))
                                        operands)))
              (cond ((null remaining) nil)
                    ((null (rest remaining)) (translate (first remaining) flag))
                    (t `(or ,@(mapcar (lambda (operand)
                                         (translate operand flag))
                                       remaining))))))))
    (not
     (let ((truth (static-form-truth (first operands))))
       (case truth
         (:true nil)
         (:false t)
         (otherwise `(not ,(translate (first operands) flag))))))
    (otherwise
     `(,operator ,@(mapcar (lambda (operand)
                             (translate operand flag))
                           operands)))))


(defun check-conditional-branch-form (branch form)
  "Reject IF branches whose surface syntax must be wrapped in DO."
  (when (and branch
             (listp branch)
             (eql (car branch) 'and))
    (error "AND not allowed in <then> or <else> clause of IF statement; use DO in effect: ~A"
           form)))


(defun translate-conditional-branch (branch flag form)
  "Translate a reachable IF branch in write mode."
  (check-conditional-branch-form branch form)
  (let ((*proposition-read-mode* nil))
    (translate branch flag)))


(defun translate-static-conditional (truth form flag)
  "Translate only the reachable side of an IF whose test has static truth."
  (ecase truth
    (:true
     (if (and (eq flag 'eff) *within-quantifier*)
       `(progn ,(translate-conditional-branch (third form) flag form) t)
       (translate-conditional-branch (third form) flag form)))
    (:false
     (cond ((fourth form)
            (if (and (eq flag 'eff) *within-quantifier*)
              `(progn ,(translate-conditional-branch (fourth form) flag form) t)
              (translate-conditional-branch (fourth form) flag form)))
           ((and *within-quantifier* (not (eq flag 'eff))) t)
           (t nil)))))


(defun translate-existential (form flag)
  "Existential translation with context-dependent semantics.
   Pre: Query semantics returning T/NIL based on satisfaction
   Eff: Assertion semantics - assert first satisfying instantiation"
  (check-form-body form)
  (let ((parameters (second form))
        (body (third form)))
    (check-precondition-parameters parameters)
    (unless (member (first parameters) *parameter-headers*)
      (push 'standard parameters))
    (multiple-value-bind (pre-param-?vars pre-param-types) (dissect-pre-params parameters)
      (let ((*var-type-env* (append (mapcar #'cons (alexandria:flatten pre-param-?vars) (flatten-param-types pre-param-types)) *var-type-env*)))
        (let* ((queries (intersection (alexandria:flatten pre-param-types) *query-names*))
               (type-inst (instantiate-type-spec pre-param-types))
               (static-collection (unless queries
                                    (ut::transpose (eval-instantiated-spec type-inst)))))
          (multiple-value-bind (var domain static-single-p)
              (and (null queries)
                   (static-single-quantifier-domain pre-param-?vars pre-param-types))
            (cond
              ((and static-single-p (null domain))
               (translate-empty-static-quantifier body flag nil))
              (static-single-p
               (ecase flag
                 (pre
                  (let ((*within-quantifier* t))
                    `(loop for ,var in ',domain
                           thereis ,(translate body flag))))
                 (eff
                  `(loop for ,var in ',domain
                         thereis ,(let ((*within-quantifier* t))
                                    (translate body 'eff))))))
              ((and (null queries) (empty-quantifier-collection-p static-collection))
               (translate-empty-static-quantifier body flag nil))
              (t
               (ecase flag
                 (pre
                  ;; Query semantics - return T if any instantiation satisfies body, NIL otherwise
                  (let ((*within-quantifier* t))
                    `(let ((collection ,(if queries
                                          `(ut::transpose (eval-instantiated-spec ',type-inst state))
                                          `',static-collection)))
                       (if (and collection (caar collection))
                           (apply #'some (lambda (&rest args)
                                          (destructuring-bind ,pre-param-?vars args
                                            ,(translate body flag)))
                                  collection)
                           nil))))
                 (eff
                  ;; Assertion semantics - execute body for suitable instantiations
                  `(let ((collection ,(if queries
                                        `(ut::transpose (eval-instantiated-spec ',type-inst state))
                                        `',static-collection)))
                     (if (and collection (caar collection))
                         (apply #'some (lambda (&rest args)
                                        (destructuring-bind ,pre-param-?vars args
                                          ,(let ((*within-quantifier* t))
                                             (translate body 'eff))))
                                collection)
                         nil))))))))))))


(defun translate-universal (form flag)
  "Universal translation with translation-time quantifier context."
  (check-form-body form)
  (let ((parameters (second form))
        (body (third form)))
    (check-precondition-parameters parameters)
    (unless (member (first parameters) *parameter-headers*)
      (push 'standard parameters))
    (when (eql flag 'eff)
      (warn "Found FORALL statement in effect; DOALL is often intended: ~A" form))
    (multiple-value-bind (pre-param-?vars pre-param-types) (dissect-pre-params parameters)
      (let ((*var-type-env* (append (mapcar #'cons (alexandria:flatten pre-param-?vars) (flatten-param-types pre-param-types)) *var-type-env*)))
        (let* ((queries (intersection (alexandria:flatten pre-param-types) *query-names*))
               (type-inst (instantiate-type-spec pre-param-types))
               (static-collection (unless queries
                                    (ut::transpose (eval-instantiated-spec type-inst)))))
          (multiple-value-bind (var domain static-single-p)
              (and (null queries)
                   (static-single-quantifier-domain pre-param-?vars pre-param-types))
            (cond
              ((and static-single-p (null domain))
               (translate-empty-static-quantifier body flag t))
              ((and (null queries) (empty-quantifier-collection-p static-collection))
               (translate-empty-static-quantifier body flag t))
              (t
               ;; Translation-time binding affects the translate call below
               (let ((*within-quantifier* t))
                 (if static-single-p
                     `(loop for ,var in ',domain
                            always ,(translate body flag))
                     `(let ((collection ,(if queries
                                           `(ut::transpose (eval-instantiated-spec ',type-inst state))
                                           `',static-collection)))
                        (if (and collection (caar collection))
                            (apply #'every (lambda (&rest args)
                                            (destructuring-bind ,pre-param-?vars args
                                              ,(translate body flag)))
                                   collection)
                            t))))))))))))


(defun translate-doall (form flag)
  "DOALL translation with translation-time quantifier context."
  (check-form-body form)
  (let ((parameters (second form))
        (body (third form)))
    (check-precondition-parameters parameters)
    (unless (member (first parameters) *parameter-headers*)
      (push 'standard parameters))
    (multiple-value-bind (pre-param-?vars pre-param-types) (dissect-pre-params parameters)
      (let ((*var-type-env* (append (mapcar #'cons (alexandria:flatten pre-param-?vars) (flatten-param-types pre-param-types)) *var-type-env*)))
        (let* ((queries (intersection (alexandria:flatten pre-param-types) *query-names*))
               (type-inst (instantiate-type-spec pre-param-types))
               (static-collection (unless queries
                                    (ut::transpose (eval-instantiated-spec type-inst)))))
          (multiple-value-bind (var domain static-single-p)
              (and (null queries)
                   (static-single-quantifier-domain pre-param-?vars pre-param-types))
            (cond
              ((and static-single-p (null domain))
               (translate-empty-static-quantifier body flag t))
              ((and (null queries) (empty-quantifier-collection-p static-collection))
               (translate-empty-static-quantifier body flag t))
              (t
               ;; Translation-time binding affects the translate call below
               (let ((*within-quantifier* t))
                 (if static-single-p
                     `(dolist (,var ',domain t)
                        ,(translate body flag))
                     `(progn
                        (let ((collection ,(if queries
                                             `(ut::transpose (eval-instantiated-spec ',type-inst state))
                                             `',static-collection)))
                          (when (and collection (caar collection))
                            (apply #'mapc (lambda (&rest args)
                                           (destructuring-bind ,pre-param-?vars args
                                             ,(translate body flag)))
                                   collection)))
                        t)))))))))))


(defun translate-connective (form flag)
  "Translates logical connectives (and, or, etc.) by recursively translating all operands
   with consistent context propagation. Preserves the original connective structure while
   ensuring each operand is translated according to the current context flag.
   Context Behaviors:
   - pre: All operands become read operations against original state
   - eff: All operands follow read/write determination based on syntactic context
   Read-mode propagation: Connectives preserve current *proposition-read-mode* context,
   allowing sub-forms to make appropriate read/write decisions.
   Examples:
   (and (connected ?a ?b) (color ?a blue))
   → Precondition: Both operands query state
   → Effect: Both operands update state (unless in read-mode)"
  ;; Input validation
  (check-type form cons "Connective form must be a list")
  (unless (member (car form) '(and or not))
    (warn "Translating non-standard connective: ~A" (car form)))
  (when (< (length form) 2)
    (error "Connective ~A requires at least one operand in form: ~A" (car form) form))
  (when (and (eql (car form) 'not)
             (not (= (length form) 2)))
    (error "NOT requires exactly one operand in form: ~A" form))
  ;; Simplified flag validation - removed context-aware
  (ecase flag
    ((pre eff)
     (if (static-read-context-p flag)
       (translate-simplified-connective (car form) (cdr form) flag)
       ;; Preserve connective structure, translate all operands with same context
       `(,(car form) ,@(mapcar (lambda (operand)
                                 (translate operand flag))
                               (cdr form)))))))


(defun translate-conditional (form flag)
  "Conditional translation with proper read-mode isolation."
  (let ((test-truth (static-form-truth (second form))))
    (if (member test-truth '(:true :false))
      (translate-static-conditional test-truth form flag)
      (progn
        (check-conditional-branch-form (third form) form)
        (when (fourth form)
          (check-conditional-branch-form (fourth form) form))
        ;; Test translation with forced read-mode
        (let ((test-translation (let ((*proposition-read-mode* t))
                                  (translate (second form) flag))))
          (cond
            ;; Special case: Effect context within quantifiers - ensure T return for success
            ((and (eq flag 'eff) *within-quantifier*)
             (if (fourth form)
                 ;; Explicit else clause exists
                 `(if ,test-translation
                    (progn ,(translate-conditional-branch (third form) flag form) t)
                    (progn ,(translate-conditional-branch (fourth form) flag form) t))
                 ;; No explicit else - return t for success, nil for no-match
                 `(if ,test-translation
                    (progn ,(translate-conditional-branch (third form) flag form) t)
                    nil)))

            ;; Quantifier context with proper forall semantics (non-effect cases)
            (*within-quantifier*
             (if (fourth form)
                 ;; Explicit else clause exists - return actual values from both branches
                 `(if ,test-translation
                    ,(translate-conditional-branch (third form) flag form)
                    ,(translate-conditional-branch (fourth form) flag form))
                 ;; No explicit else - implicit else should be t for forall semantics
                 `(if ,test-translation
                    ,(translate-conditional-branch (third form) flag form)
                    t)))  ; Neutral element for universal quantification

            ;; Value context - standard conditional behavior
            (t
             (if (fourth form)
                 `(if ,test-translation
                    ,(translate-conditional-branch (third form) flag form)
                    ,(translate-conditional-branch (fourth form) flag form))
                 `(if ,test-translation
                    ,(translate-conditional-branch (third form) flag form)
                    nil)))))))))


(defun translate-assert (form flag)
  "For depth-first, translates an assert statement with selective write-mode context."
  (ecase flag
    (eff (error "Nested ASSERT statements not allowed:~%~A" form))
    (pre `(let* ((parent-hash (unless (use-canonical-symmetry-p)  ;CHANGED: seed incremental idb-hash from parent (standard mode only)
                                (ensure-idb-hash state)))
                 (state (copy-problem-state state))
                 (*idb-hash-acc* parent-hash))  ;CHANGED: NIL here disables folding (canonical-symmetry mode)
            ,@(mapcar (lambda (statement)
                        ;; Bind read-mode to nil only for direct assert statements
                        (let ((*proposition-read-mode* nil))
                          (translate statement 'eff)))
                      (cdr form))
            (push (make-update :changes (problem-state.idb state)
                               :hash *idb-hash-acc*  ;CHANGED: carry incremental hash out of the effect
                               :value ,(if *objective-value-p*
                                         '$objective-value
                                         0.0)
                               :instantiations (list ,@*eff-param-vars*)
                               :followups (nreverse followups)
                               ,@(when *has-sim-state*
                                   '(:sim-state $sim-state)))
                  updated-dbs)))))


(defun translate-assert-bt (form flag)
  "For backtracking with incremental updates, translates an assert statement.
   Applies updates directly to state (incremental) while tracking both forward 
   and inverse operations."
  (ecase flag
    (eff (error "Nested ASSERT statements not allowed:~%~A" form))
    (pre `(let (forward-list inverse-list)
            (declare (special forward-list inverse-list))
            ;; Execute each statement, applying updates incrementally to state
            ,@(mapcar (lambda (statement)
                        (let ((*proposition-read-mode* nil))
                          (translate statement 'eff)))
                      (cdr form))
            ;; Create update structure with BOTH forward and inverse operations
            (push (make-update :changes (list (nreverse forward-list)    ; Forward ops
                                             inverse-list)              ; Inverse ops
                               :value ,(if *objective-value-p*
                                         '$objective-value
                                         0.0) 
                               :instantiations (list ,@*eff-param-vars*) 
                               :followups (reverse followups)
                               ,@(when *has-sim-state*
                                   '(:sim-state $sim-state)))
                  updated-dbs)
            updated-dbs))))


(defun translate-do (form flag)
  "Translates a simple set of clauses."
  `(progn ,@(iter (for statement in (cdr form))
              (collect (translate statement flag)))))


(defun translate-equivalent (form flag)
  "Translates equivalence by forcing read operations on all operands"
  `(equivalent ,@(let ((*proposition-read-mode* t))
                   (mapcar (lambda (statement)
                             (translate statement flag))
                           (cdr form)))))


(defun translate-let (form flag)
  "Translates a let clause, including binding forms."
  `(,(first form) ,(mapcar (lambda (binding)
                             (if (consp binding)
                                 ;; Binding with initial value - translate the value
                                 `(,(first binding) ,(translate (second binding) flag))
                                 ;; Just a variable name - keep as is
                                 binding))
                           (second form))
     ,@(iter (for statement in (cddr form))
             (collect (translate statement flag)))))


(defun translate-mv-assign (form flag)
  "Translates an mv-assign statement, always returning t as a conjunct."
  `(progn (multiple-value-setq ,(second form) ,(translate (third form) flag)) t))


(defun translate-assign (form flag)
  "Translates an assign statement, always returning t as a conjunct."
  `(progn (setq ,(second form) ,(translate (third form) flag)) t))


(defun translate-case-body (statements flag)
  "Translate the body of a selected CASE clause."
  `(progn ,@(mapcar (lambda (statement)
                      (translate statement flag))
                    statements)))


(defun translate-case (form flag)
  "Translates a case statement."
  (multiple-value-bind (key knownp) (static-literal-value (second form))
    (if knownp
      (multiple-value-bind (clause foundp)
          (static-case-selected-clause key (cddr form))
        (if foundp
          (translate-case-body (rest clause) flag)
          nil))
      `(case ,(translate (second form) flag)
         ,@(iter (for clause in (cddr form))
             (collect `(,(first clause) ,@(iter (for statement in (rest clause))
                                                (collect (translate statement flag))))))))))


(defun translate-cond (form flag)
  "Translates a cond statement by converting to nested if statements.
   Each cond clause (test result1 result2 ...) becomes an if branch.
   Leverages translate-conditional for all semantic complexity.
   Structure: (cond (test1 result1...) (test2 result2...) ...)
   Translation strategy:
   - Recursively converts clauses to nested if statements from back to front
   - Multiple result forms wrapped in do (translates to progn)
   - Bare test clauses return test value if true (standard CL semantics)
   - Final else defaults to nil
   - Delegates all context handling to translate-conditional"
  ;; Validate structure
  (unless (and (consp form) (eq (car form) 'cond))
    (error "Invalid cond form: ~A" form))
  (unless (cdr form)
    (error "COND requires at least one clause: ~A" form))
  ;; Convert to nested ifs recursively
  (labels ((convert-clauses (clauses)
             (if (null clauses)
                 nil  ; No more clauses - return nil as default else
                 (let* ((clause (car clauses))
                        (test (car clause))
                        (results (cdr clause)))
                   ;; Validate clause structure
                   (unless (consp clause)
                     (error "Invalid cond clause (must be list): ~A" clause))
                   ;; Build then-form based on number of result forms
                   (let ((then-form (cond
                                      ;; No result forms - use test value (CL semantics)
                                      ((null results) test)
                                      ;; Single result form - use as-is
                                      ((null (cdr results)) (car results))
                                      ;; Multiple result forms - wrap in do
                                      (t `(do ,@results))))
                         (else-form (convert-clauses (cdr clauses))))
                     ;; Build if statement with or without else clause
                     (if else-form
                         `(if ,test ,then-form ,else-form)
                         `(if ,test ,then-form)))))))
    ;; Generate nested if structure and translate
    (let ((if-form (convert-clauses (cdr form))))
      (translate-conditional if-form flag))))


(defun translate-print (form flag)
  "Translates a print statement for debugging actions."
  `(print ,(let ((*proposition-read-mode* t))
             (translate (second form) flag))))


(defun translate-ww-loop (form flag)
  "WW-loop translation with translation-time context override."
  (let ((*within-quantifier* nil))
    `(loop ,@(loop for item in (cdr form) 
                   collect (translate item flag)))))  ; Called with *within-quantifier* = nil


(defun translate-lambda-form (form flag)
  "Translate the body of a Lisp lambda expression."
  `(lambda ,(second form)
     ,@(mapcar (lambda (body-form)
                 (translate body-form flag))
               (cddr form))))


(defun translate-function-special-form (form flag)
  "Translate #'(lambda ...) while preserving named function references."
  (if (and (= (length form) 2)
           (consp (second form))
           (eq (first (second form)) 'lambda))
    `(function ,(translate-lambda-form (second form) flag))
    form))


(defun translate-binding-form (form flag)
  "Translate a binding form with one value expression and a body."
  `(,(first form) ,(second form) ,(translate (third form) flag)
     ,@(mapcar (lambda (body-form)
                 (translate body-form flag))
               (cdddr form))))


(defun translate-set-form (form flag)
  "Translate value positions in SETQ/SETF-style forms."
  `(,(first form)
     ,@(loop for (place value) on (rest form) by #'cddr
             append (list (translate place flag)
                          (translate value flag)))))


(defun translate-body-form (form flag)
  "Translate forms whose remaining elements are evaluated body forms."
  `(,(first form)
     ,@(mapcar (lambda (body-form)
                 (translate body-form flag))
               (rest form))))


(defun ordinary-lisp-call-p (form)
  "Return true when FORM is an ordinary function call, not a macro/special form."
  (let ((operator (first form)))
    (or (consp operator)
        (and (symbolp operator)
             (fboundp operator)
             (not (macro-function operator))
             (not (special-operator-p operator))))))


(defun translate-ordinary-lisp-call (form flag)
  "Translate evaluated arguments in an ordinary Lisp function call."
  `(,(if (consp (first form))
       (translate (first form) flag)
       (first form))
     ,@(mapcar (lambda (arg)
                 (translate arg flag))
               (rest form))))


(defun translate-lisp-form (form flag)
  "Translate Wouldwork subforms inside evaluated positions of Lisp forms."
  (case (first form)
    (quote form)
    (function (translate-function-special-form form flag))
    (lambda (translate-lambda-form form flag))
    ((let let*) (translate-let form flag))
    ((setq setf psetf) (translate-set-form form flag))
    ((multiple-value-bind destructuring-bind)
     (translate-binding-form form flag))
    ((progn locally) (translate-body-form form flag))
    (block
     `(block ,(second form)
        ,@(mapcar (lambda (body-form)
                    (translate body-form flag))
                  (cddr form))))
    (return-from
     `(return-from ,(second form)
        ,@(mapcar (lambda (body-form)
                    (translate body-form flag))
                  (cddr form))))
    (the
     `(the ,(second form) ,(translate (third form) flag)))
    (t
     (if (ordinary-lisp-call-p form)
       (translate-ordinary-lisp-call form flag)
       form))))


(defun translate-followup (form flag)
  ;Processes a trigger followup form for next & finally.
  (declare (ignore flag))
  (let ((base-form (second form)))
    `(push (list ',(car base-form) ,@(cdr base-form)) followups)))


(defun cl-symbol-p (item)
 "Return true if item is from the common-lisp package."
 (and (symbolp item)
      (eq (symbol-package item) 
          (find-package :common-lisp))))


(defun translate-simulate-happenings-until-true (form flag)
  "Translates (simulate-happenings-until-true max-wait-time target-condition).
   Compiles target-condition into a lambda that captures lexical $variables.
   The lambda takes sim-state (bound to 'state' for translated code compatibility).
   Returns call to runtime function simulate-happenings-until-true-fn."
  (declare (ignore flag))
  (unless (= (length form) 3)
    (error "simulate-happenings-until-true requires exactly 2 arguments: ~
            (simulate-happenings-until-true max-wait-time target-condition), got: ~A" form))
  (let ((max-wait-time (second form))
        (target-condition (third form)))
    ;; Translate target condition in 'pre mode (read-only query)
    ;; The lambda parameter is 'state' so translated code works unchanged
    (let ((translated-condition (translate target-condition 'pre)))
      `(simulate-happenings-until-true-fn 
         state 
         ,max-wait-time
         (lambda (state)
           (declare (ignorable state))
           ,translated-condition)))))


(defun translate (form flag)  ;test-then distinguishes between if stmt forms
  "Beginning translator for all forms in actions."
  (cond ((atom form) form)  ;atom or (always-true) translates as itself
        ((null form) t)  ;if form=nil simply continue processing
        ((equal form '(always-true)) (translate-simple-atom form flag))
        ((eql (car form) 'assert) (if (eq *algorithm* 'backtracking)
                                    (translate-assert-bt form flag)
                                    (translate-assert form flag)))
        ((member (car form) '(forsome exists exist)) (translate-existential form flag))  ;specialty first
        ((member (car form) '(forall forevery)) (translate-universal form flag)) ;removed every
        ((member (car form) '(finally next)) (translate-followup form flag))
        ((eql (car form) 'doall) (translate-doall form flag))
        ((eql (car form) 'if) (translate-conditional form flag))
        ((eql (car form) 'do) (translate-do form flag))
        ((eql (car form) 'equivalent) (translate-equivalent form flag))
        ((eql (car form) 'bind) (translate-bind form flag))
        ((eql (car form) 'ww-loop) (translate-ww-loop form flag))
        ((eql (car form) 'assign) (translate-assign form flag))
        ((member (car form) '(let let*)) (translate-let form flag))
        ((eql (car form) 'case) (translate-case form flag))
        ((eql (car form) 'cond) (translate-cond form flag))
        ((eql (car form) 'mv-assign) (translate-mv-assign form flag))
        ((eql (car form) 'declare) form)
        ((eql (car form) 'print) (translate-print form flag))
        ((eql (car form) 'simulate-happenings-until-true)
           (translate-simulate-happenings-until-true form flag))
        ((eql (char (format nil "~S" form) 0) #\`) (translate (eval form) flag))
        ((and (eql (car form) 'not)
              (consp (cadr form))
              (symbolp (caadr form))
              (gethash (caadr form) *relations*)) (translate-negative-relation form flag))
        ((member (car form) *connectives*) (translate-connective form flag))
        ((or (gethash (car form) *relations*) (gethash (car form) *static-relations*))
           (translate-positive-relation form flag))
        ((member (car form) (append *query-names* *update-names* '(apply-simulated-state!)))
           (translate-function-call form flag))
        ((and (listp form)
              (symbolp (car form))
              (not ($varp (car form)))
              (not (cl-symbol-p (car form)))
              (not (fboundp (car form)))
              (not (macro-function (car form)))
              (not (special-operator-p (car form))))
         (error "~2%If ~A is a query or update function, it is unrecognized as such (typo?).~%~
                 If it is a local variable, prefix it with $.)~2%"
                (car form) form))
        (t (translate-lisp-form form flag))))

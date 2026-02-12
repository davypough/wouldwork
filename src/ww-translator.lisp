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


(defun translate-list (form flag)
  "Most basic form translation."
  (declare (ignore flag))
  (check-proposition form)
  `(list ,@(iter (for item in form)
             (if (or (varp item)
                     (numberp item)
                     (stringp item)
                     (characterp item)
                     (listp item))
               (collect item)
               (collect `(quote ,item))))))


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
                   (list ,(if (or (varp arg2)
                                  (numberp arg2)
                                  (stringp arg2)
                                  (characterp arg2)
                                  (listp arg2))
                            arg2
                            `',arg2))))
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
                   (list ,(if (or (varp arg2)
                                  (and (consp arg2)
                                       (symbolp (car arg2))
                                       (or (fboundp (car arg2))
                                           (special-operator-p (car arg2)))))
                            arg2
                            `',arg2))))
        ;; Non-bijective relation
        (let* ((fluentless-atom (ut::remove-at-indexes fluent-indices form))
               (fluents (ut::collect-at-indexes fluent-indices form)))
          `(equalp (gethash ,(translate-list fluentless-atom flag) ,database-ref)
                   (list ,@(mapcar (lambda (x)
                                     (if (or (varp x)
                                             (and (consp x)
                                                  (symbolp (car x))
                                                  (or (fboundp (car x))
                                                      (special-operator-p (car x)))))
                                       x
                                       `',x))
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
   For backtracking: applies changes incrementally while tracking forward and inverse."
  (declare (special forward-list inverse-list))
  (if (write-operation-p flag)
    (if (eq *algorithm* 'backtracking)
      ;; Backtracking with incremental updates
      `(multiple-value-bind (forward inverse) 
           (update-bt (problem-state.idb state) ,(translate-list form flag))
         ;; UPDATE-BT applies the forward operation immediately and also returns inverse.
         (push forward forward-list)
         (push inverse inverse-list))
      ;; Depth-first algorithm
      `(update (problem-state.idb state) ,(translate-list form flag)))
    ;; Read operation
    (translate-proposition form flag)))


(defun translate-negative-relation (form flag)
  "Unified negative relation translation with incremental updates for backtracking."
  (declare (special forward-list inverse-list))
  (if (write-operation-p flag)
    (if (eq *algorithm* 'backtracking)
      ;; Backtracking with incremental updates
      `(multiple-value-bind (forward inverse) 
           (update-bt (problem-state.idb state) (list 'not ,(translate-list (second form) flag)))
         ;; UPDATE-BT applies the forward operation immediately and also returns inverse.
         (push forward forward-list)
         (push inverse inverse-list))
      ;; Depth-first algorithm
      `(update (problem-state.idb state) (list 'not ,(translate-list (second form) flag))))
    ;; Read operation
    `(not ,(translate-positive-relation (second form) flag))))


(defun translate-function-call (form flag)
  "Corrected function call translation with robust update function detection"
  (check-query/update-call form)
  (let* ((function-name (car form))
         (state-arg 'state)
         (fn-call (append (list function-name state-arg)
                          (mapcar (lambda (arg)
                                    (if (and (symbolp arg) (not (varp arg)))
                                       `(quote ,arg)
                                       arg))
                                  (cdr form)))))
    ;; Enhanced validation with robust update function detection
    (when (and (update-function-p function-name)
               (not (eq flag 'eff)))
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


(defun get-prop-fluents (proposition)
  "Returns the fluent values in an arbitrary proposition."
  (let ((indices (get-prop-fluent-indices proposition)))
    (when indices
      (mapcar (lambda (index)
                (let ((item (nth index proposition)))
                  (if (and (symbolp item) (boundp item))
                    (symbol-value item)
                    item)))
              indices))))


(defun validate-bind-form (form)
  "Validates bind form structure and proposition format.
   Ensures proper syntax before translation proceeds."
  (unless (and (consp form)
               (eq (first form) 'bind)
               (consp (second form)))
    (error "Invalid bind form structure: ~A" form))
  (check-proposition (second form))
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
      (let ((queries (intersection (alexandria:flatten pre-param-types) *query-names*))
            (type-inst (instantiate-type-spec pre-param-types)))
        (ecase flag
          (pre
           ;; Query semantics - return T if any instantiation satisfies body, NIL otherwise
           (let ((*within-quantifier* t))
             `(let ((collection ,(if queries
                                    `(ut::transpose (eval-instantiated-spec ',type-inst state))
                                    `(ut::transpose (quote ,(eval-instantiated-spec type-inst))))))
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
                                   `(ut::transpose (quote ,(eval-instantiated-spec type-inst))))))
               (if (and collection (caar collection))
                   (apply #'some (lambda (&rest args)
                                  (destructuring-bind ,pre-param-?vars args
                                      ,(let ((*within-quantifier* t))
                                    (translate body 'eff))))
                          collection)
                   nil))))))))


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
      (let ((queries (intersection (alexandria:flatten pre-param-types) *query-names*))
            (type-inst (instantiate-type-spec pre-param-types)))
        ;; Translation-time binding affects the translate call below
        (let ((*within-quantifier* t))
          `(let ((collection ,(if queries
                                 `(ut::transpose (eval-instantiated-spec ',type-inst state))
                                 `(ut::transpose (quote ,(eval-instantiated-spec type-inst))))))
             (if (and collection (caar collection))
                 (apply #'every (lambda (&rest args)
                                 (destructuring-bind ,pre-param-?vars args
                                   ,(translate body flag)))
                        collection)
                 t)))))))


(defun translate-doall (form flag)
  "DOALL translation with translation-time quantifier context."
  (check-form-body form)
  (let ((parameters (second form))
        (body (third form)))
    (check-precondition-parameters parameters)
    (unless (member (first parameters) *parameter-headers*)
      (push 'standard parameters))
    (multiple-value-bind (pre-param-?vars pre-param-types) (dissect-pre-params parameters)
      (let ((queries (intersection (alexandria:flatten pre-param-types) *query-names*))
            (type-inst (instantiate-type-spec pre-param-types)))
        ;; Translation-time binding affects the translate call below
        (let ((*within-quantifier* t))
          `(progn
             (let ((collection ,(if queries
                                   `(ut::transpose (eval-instantiated-spec ',type-inst state))
                                   `(ut::transpose (quote ,(eval-instantiated-spec type-inst))))))
               (when (and collection (caar collection))
                 (apply #'mapc (lambda (&rest args)
                                (destructuring-bind ,pre-param-?vars args
                                  ,(translate body flag)))
                        collection)))
             t))))))


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
  ;; Simplified flag validation - removed context-aware
  (ecase flag
    ((pre eff)
     ;; Preserve connective structure, translate all operands with same context
     `(,(car form) ,@(mapcar (lambda (operand)
                               (translate operand flag))
                             (cdr form))))))


(defun translate-conditional (form flag)
  "Conditional translation with proper read-mode isolation."
  (when (or (and (third form) (listp (third form)) (eql (car (third form)) 'and))
            (and (fourth form) (listp (fourth form)) (eql (car (fourth form)) 'and)))
    (error "AND not allowed in <then> or <else> clause of IF statement; use DO in effect: ~A" form))
  ;; Test translation with forced read-mode
  (let ((test-translation (let ((*proposition-read-mode* t))
                            (translate (second form) flag))))
    (cond
      ;; Special case: Effect context within quantifiers - ensure T return for success
      ((and (eq flag 'eff) *within-quantifier*)
       (if (fourth form)
           ;; Explicit else clause exists
           `(if ,test-translation
              (progn ,(let ((*proposition-read-mode* nil))
                        (translate (third form) flag)) t)
              (progn ,(let ((*proposition-read-mode* nil))
                        (translate (fourth form) flag)) t))
           ;; No explicit else - return t for success, nil for no-match
           `(if ,test-translation
              (progn ,(let ((*proposition-read-mode* nil))
                        (translate (third form) flag)) t)
              nil)))
      
      ;; Quantifier context with proper forall semantics (non-effect cases)
      (*within-quantifier*
       (if (fourth form)
           ;; Explicit else clause exists - return actual values from both branches
           `(if ,test-translation
              ,(let ((*proposition-read-mode* nil))
                 (translate (third form) flag))
              ,(let ((*proposition-read-mode* nil))
                 (translate (fourth form) flag)))
           ;; No explicit else - implicit else should be t for forall semantics
           `(if ,test-translation
              ,(let ((*proposition-read-mode* nil))
                 (translate (third form) flag))
              t)))  ; Neutral element for universal quantification
      
      ;; Value context - standard conditional behavior
      (t
       (if (fourth form)
           `(if ,test-translation
              ,(let ((*proposition-read-mode* nil))
                (translate (third form) flag))
              ,(let ((*proposition-read-mode* nil))
                (translate (fourth form) flag)))
           `(if ,test-translation
              ,(let ((*proposition-read-mode* nil))
                (translate (third form) flag))
              nil))))))


(defun translate-assert (form flag)
  "For depth-first, translates an assert statement with selective write-mode context."
  (ecase flag
    (eff (error "Nested ASSERT statements not allowed:~%~A" form))
    (pre `(let ((state (copy-problem-state state)))
            ,@(mapcar (lambda (statement)
                        ;; Bind read-mode to nil only for direct assert statements
                        (let ((*proposition-read-mode* nil))
                          (translate statement 'eff)))
                      (cdr form))
            (push (make-update :changes (problem-state.idb state)
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
                                             (nreverse inverse-list))   ; Inverse ops
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
  `(let ,(mapcar (lambda (binding)
                   (if (consp binding)
                       ;; Binding with initial value - translate the value
                       `(,(first binding) ,(translate (second binding) flag))
                       ;; Just a variable name - keep as is
                       binding))
                 (second form))
     ,@(iter (for statement in (cddr form))
             (collect (translate statement flag)))))


(defun translate-mvsetq (form flag)
  "Translates a multiple-value-setq clause."
  `(multiple-value-setq ,(second form) ,(translate (third form) flag)))


(defun translate-setq (form flag)
  "Translates a setq statement. Used to assign a variable the value of a function."
  `(setq ,(second form) ,(translate (third form) flag)))


(defun translate-case (form flag)
  "Translates a case statement."
  `(case ,(second form)
     ,@(iter (for clause in (cddr form))
         (collect `(,(first clause) ,@(iter (for statement in (rest clause))
                                            (collect (translate statement flag))))))))


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
        ((eql (car form) 'setq) (translate-setq form flag))
        ((eql (car form) 'let) (translate-let form flag))
        ((eql (car form) 'case) (translate-case form flag))
        ((eql (car form) 'cond) (translate-cond form flag))
        ((member (car form) '(mvsetq multiple-value-setq)) (translate-mvsetq form flag))
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
        (t form)))

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
  "Example usage of get-state-reference with proper database selection logic.
   Eg, (velocity ?car wheel1 50) -> (list 'velocity ?car 'wheel1 50) with no fluents."
  (let ((state-db (if (gethash (car form) *relations*)
                      (if *happening-names*
                          '(merge-idb-hidb state)
                          `(problem-state.idb state))  ;,(get-state-reference flag)))
                      '*static-db*)))
    `(eql t (gethash ,(translate-list form flag) ,state-db))))


(defun translate-fluent-atom (form flag)
  "Translates propositions with fluents using standardized database reference."
  (let* ((fluent-indices (get-prop-fluent-indices form))
         (fluentless-atom (ut::remove-at-indexes fluent-indices form))
         (fluents (ut::collect-at-indexes fluent-indices form))
         (database-ref (get-database-reference form flag)))
    `(equalp (gethash ,(translate-list fluentless-atom flag) ,database-ref)
             (list ,@(mapcar (lambda (x)
                               (if (or (varp x)
                                       (and (consp x)
                                         (symbolp (car x))
                                         (or (fboundp (car x))
                                             (special-operator-p (car x)))))
                                  x
                                  `',x))
                             fluents)))))


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
         ;; Apply forward operation immediately to state
         (update (problem-state.idb state) forward)
         ;; Track both for later use
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
         ;; Apply forward operation immediately to state
         (update (problem-state.idb state) forward)
         ;; Track both for later use
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


(defun translate-bind (form flag)
  "Revised binding translation with unified state reference strategy.
   Translates binding operations like (bind (loc ?obj $area)) where fluent variables
   get bound to values retrieved from the database. Always performs read-only queries
   regardless of syntactic context.
   Returns:
   - t if proposition found and variables successfully bound
   - nil if proposition not found in database
   Side effects:
   - Sets fluent variables to corresponding values from retrieved proposition"
  ;; Input validation and structure extraction
  (validate-bind-form form)
  (let* ((proposition (second form))
         (fluent-indices (get-prop-fluent-indices proposition))
         (fluentless-atom (ut::remove-at-indexes fluent-indices proposition))
         (prop-fluents (get-prop-fluents proposition))
         (database-ref (get-database-reference proposition flag)))
    ;; Generate database lookup and conditional binding
    `(multiple-value-bind (vals present-p)
         (gethash ,(translate-list fluentless-atom flag) ,database-ref)
       (when present-p
         ,(cond
            ;; Case 1: Fluent variables present - perform binding
            (prop-fluents
             `(progn (setf ,@(generate-fluent-bindings prop-fluents))
                     t))
            ;; Case 2: No fluent variables - simple existence check
            (t 't))))))


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


#+ignore (defun translate-existential (form flag)
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
             `(apply #'some (lambda (&rest args)
                              (destructuring-bind ,pre-param-?vars args
                                ,(translate body flag)))
                     ,(if queries
                        `(ut::transpose (eval-instantiated-spec ',type-inst state))
                        `(ut::transpose (quote ,(eval-instantiated-spec type-inst)))))))
          (eff
            ;; Assertion semantics - execute body for suitable instantiations
            `(apply #'some (lambda (&rest args)
                             (destructuring-bind ,pre-param-?vars args
                                 ,(let ((*within-quantifier* t))
                               (translate body 'eff))))
                           ,(if queries
                              `(ut::transpose (eval-instantiated-spec ',type-inst state))
                              `(ut::transpose (quote ,(eval-instantiated-spec type-inst)))))))))))


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


#+ignore (defun translate-universal (form flag)
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
            ;(state-ref (get-state-reference flag)))
        ;; Translation-time binding affects the translate call below
        (let ((*within-quantifier* t))
          `(apply #'every (lambda (&rest args)
                           (destructuring-bind ,pre-param-?vars args
                             ,(translate body flag)))  ; Called with *within-quantifier* = t
                  ,(if queries
                     `(ut::transpose (eval-instantiated-spec ',type-inst state))  ;,state-ref))
                     `(ut::transpose (quote ,(eval-instantiated-spec type-inst))))))))))


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


#+ignore (defun translate-doall (form flag)
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
            ;(state-ref (get-state-reference flag)))
        ;; Translation-time binding affects the translate call below
        (let ((*within-quantifier* t))
          `(progn
             (apply #'mapc (lambda (&rest args)
                            (destructuring-bind ,pre-param-?vars args
                              ,(translate body flag)))  ; Called with *within-quantifier* = t
                    ,(if queries
                       `(ut::transpose (eval-instantiated-spec ',type-inst state))  ;,state-ref))
                       `(ut::transpose (quote ,(eval-instantiated-spec type-inst)))))
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
                               :followups (nreverse followups))
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
                               :followups (reverse followups))
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
  "Translates a multiple-value-setq clause.
   Always returns t to maintain logical continuation semantics."
  `(progn (multiple-value-setq ,(second form) ,(translate (third form) flag))
          t))


(defun translate-setq (form flag)
  "Translates a setq statement. Used to assign a variable the value of a function.
   Always returns t, even if nil is assigned."
  `(progn (setq ,(second form) ,(translate (third form) flag))
          t))


(defun translate-case (form flag)
  "Translates a case statement."
  `(case ,(second form)
     ,@(iter (for clause in (cddr form))
         (collect `(,(first clause) ,@(iter (for statement in (rest clause))
                                            (collect (translate statement flag))))))))


;(defun translate-cond (form flag)
;  "Translates a cond statement."
;  `(cond ,@(iter (for clause in cddr form))
;             (collect `(,(first clause) ,@(iter (for statement in (rest clause))
;                                            (collect (translate statement flag)))))))


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
        ((member (car form) '(mvsetq multiple-value-setq)) (translate-mvsetq form flag))
        ((eql (car form) 'declare) form)
        ((eql (car form) 'print) (translate-print form flag))
        ((eql (char (format nil "~S" form) 0) #\`) (translate (eval form) flag))
        ((and (eql (car form) 'not)
              (consp (cadr form))
              (symbolp (caadr form))
              (gethash (caadr form) *relations*)) (translate-negative-relation form flag))
        ((member (car form) *connectives*) (translate-connective form flag))
        ((or (gethash (car form) *relations*) (gethash (car form) *static-relations*))
           (translate-positive-relation form flag))
        ((member (car form) (append *query-names* *update-names*)) (translate-function-call form flag))
        ((and (listp form)
              (symbolp (car form))
              (not ($varp (car form)))
              (not (?varp (car form)))
              (not (cl-symbol-p (car form)))
              (not (fboundp (car form)))
              (not (macro-function (car form)))
              (not (special-operator-p (car form))))
         (format t "~%Note: The use of ~A as a local variable in ~A is acceptable.~%~
                 But if this is a query or update function, ensure it's defined before it's used."
                (car form) form))
        (t form)))

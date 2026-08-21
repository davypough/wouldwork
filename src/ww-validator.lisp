;;; Filename: ww-validator.lisp

;;; Tests to verify structures created from user inputs are error free.


(in-package :ww)


(defun check-relation (relation)
  "Checks for errors in a user-defined relation--eg, (height ?obj $fixnum)."
  (check-type relation cons)
  (check-type (car relation) symbol)
  (let ((non-fluent-count (count-if-not #'fluent-spec-p (cdr relation))))
    (when (> non-fluent-count 5)
      (error "Design Limit Error: Relation ~A has ~D non-fluent arguments; the maximum is 5. ~
              $-typed fluent arguments are stored as proposition values and do not count toward this limit."
             (car relation) non-fluent-count)))
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
   query or update function--eg, (?location location ?elevation).
   Per-parameter shape and type validation is handled by DISSECT-QUERY-PARAMS,
   called separately by the installer."
  (check-type fn-name symbol)
  (check-type args list)
  (check-type body list))


(defun translatable-expression-form-p (form)
  "Return true when FORM can be evaluated after Wouldwork translation."
  (and (consp form)
       (let ((operator (car form)))
         (or (and (consp operator)
                  (eq (car operator) 'lambda))
             (and (symbolp operator)
                  (or (eq operator 'lambda)
                      (fboundp operator)
                      (macro-function operator)
                      (special-operator-p operator)
                      (member operator *query-names*)
                      (member operator *update-names*)
                      (gethash operator *relations*)
                      (gethash operator *static-relations*)))))))


(defun check-proposition (proposition)
  "Detects an error in a proposition--eg, (height block1 3)
   or (loc ?queen $row (1+ $col)).
   Tolerates fluentless atoms (used internally as db lookup keys) by filtering
   type-defs at fluent positions when the arg count is short of the relation-def."
  (check-type proposition cons)
  (when (eql (first proposition) 'not)
    (setf proposition (second proposition)))
  (check-predicate proposition)
  ;(check-fluent-consistency proposition)
  (let* ((relation-def (or (gethash (first proposition) *relations*)
                           (gethash (first proposition) *static-relations*)))
         (fluent-indices (get-prop-fluent-indices proposition))
         (fluentless-p (and fluent-indices
                            (listp relation-def)
                            (< (length (cdr proposition)) (length relation-def))))
         (effective-relation-def (if fluentless-p
                                   (loop for type-def in relation-def
                                         for i from 1
                                         unless (member i fluent-indices)
                                           collect type-def)
                                   relation-def)))
    (when (listp effective-relation-def)
      (iter (for arg in (cdr proposition))
            (for type-def in effective-relation-def)
            (for position from 1)  ; Add position tracking
            (or (and (?varp arg)  ;arg is a ?var of a compatible (or undeclared) type
                     (type-specs-compatible-p (cdr (assoc arg *var-type-env*)) type-def))
                ($varp arg)  ;arg is a $var
                (member arg (gethash type-def *types*))  ;arg is a value of a user defined type
                (and (member position (get-prop-fluent-indices proposition))
                     (or (null arg)
                         (member arg (gethash type-def *types*))))
                (and (member position (get-prop-fluent-indices proposition))
                     (lisp-type-p type-def)  ; Guard against composite types
                     (typep arg type-def))
                (and (lisp-type-p type-def)  ;arg is a value of a Common Lisp type
                     (typep arg type-def))
                (and (listp type-def)  ;arg is a value of a type combo
                     (eql (first type-def) 'either)
                     (member arg (iter (for type in (cdr type-def))
                                       (unioning (gethash type *types*)))))
                (translatable-expression-form-p arg)
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
                         

(defun check-bind-relation-has-fluent (proposition)
  "Validates that a bound relation declares at least one fluent to extract.
   A bind reads a fluent value out of the database; if the relation has no
   fluent position there is nothing to bind, and every $variable in the bind
   silently degrades into a literal lookup-key component (defaulting to NIL),
   so the generated probe is keyed wrong and never matches. Such a relation
   must be declared with a fluent, or tested with a plain proposition."
  (let* ((relation-name (car proposition))
         (relation-def (or (gethash relation-name *relations*)
                           (gethash relation-name *static-relations*))))
    (unless (get-prop-fluent-indices proposition)
      (error "~%Bind statement references a relation with no fluent to bind.~%~
                Statement: ~S~%~
                Relation:  ~S~%~
                Error: ~A declares no fluent ($-prefixed) argument, so bind has~%~
                  nothing to extract; the bound $variable becomes part of the~%~
                  lookup key (defaulting to NIL).~%~
                Fix: mark the looked-up argument as fluent in the relation~%~
                  declaration (eg, $gate), or use a plain proposition for an~%~
                  existence test instead of bind."
             (list 'bind proposition)
             (cons relation-name (if (listp relation-def) relation-def nil))
             relation-name))))


(defun query/update-type-member-p (value type-spec)
  "Whether VALUE is an instance of the Wouldwork object TYPE-SPEC."
  (let ((instances
          (if (and (consp type-spec)
                   (eq (first type-spec) 'either))
            (mapcan (lambda (type)
                      (copy-list (gethash type *types*)))
                    (rest type-spec))
            (copy-list (gethash type-spec *types*)))))
    (member value (remove nil instances) :test #'equal)))


(defun check-query/update-call (fn-call)
  "Checks the validity of a call to a query or update function
   during translation--eg, (cleartop? ?block).
   When the callee has declared parameter types (:param-types, set by
   DISSECT-QUERY-PARAMS at install time), cross-checks each ?var argument's
   declared type and each literal object's membership against the callee's
   declared type at that position. Computed expressions remain permissive
   because their result type is not generally known during translation."
  (check-type fn-call cons)
  (check-type (car fn-call) symbol)
  (let ((callee-param-types (get (car fn-call) :param-types))
        (all-object-values
          (reduce #'union
                  (alexandria:hash-table-values *types*)
                  :initial-value nil)))
    (iter (for arg in (cdr fn-call))
          (for position from 0)
          (for parameter-type = (nth position callee-param-types))
          (cond ((?varp arg)
                 (unless (type-specs-compatible-p
                           (cdr (assoc arg *var-type-env*))
                           parameter-type)
                   (error "The ?variable ~A is not compatible with parameter type ~A ~
                           at position ~A of ~A"
                          arg parameter-type position fn-call)))
                (($varp arg))
                ((and parameter-type (atom arg))
                 (unless (query/update-type-member-p arg parameter-type)
                   (error "The literal argument ~A is not an instance of parameter type ~A ~
                           at position ~A of ~A"
                          arg parameter-type position fn-call)))
                ((and parameter-type
                      (consp arg)
                      (eq (first arg) 'quote)
                      (= (length arg) 2))
                 (unless (query/update-type-member-p (second arg) parameter-type)
                   (error "The literal argument ~A is not an instance of parameter type ~A ~
                           at position ~A of ~A"
                          (second arg) parameter-type position fn-call)))
                ((and (null parameter-type)
                      (or (member arg all-object-values)
                          (numberp arg)
                          (characterp arg)
                          (stringp arg)
                          (keywordp arg)
                          (member arg '(nil t)))))
                ((translatable-expression-form-p arg))
                (parameter-type
                 (error "The argument ~A cannot be validated as parameter type ~A ~
                         at position ~A of ~A"
                        arg parameter-type position fn-call))
                (t
                 (error "Found a malformed query or update argument ~A in ~A"
                        arg fn-call))))))


(defun check-variable-names (action-name pre-param-?vars precondition effect all-detected-vars)
  "Checks the validity (eg, spelling) of vars in an action rule."
  (let ((valid-vars pre-param-?vars))
    (subst-if t (constantly nil) `(list ,precondition ,effect)  ;adds valid $vars
              :key (lambda (item)
                     (when (consp item)
                       (case (first item)
                         ((setq setf assign ww-assign)
                          (when (symbolp (second item)) (push (second item) valid-vars)))
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
  "Checks a user action effect parameter list. Each element must be a ? or $
   variable, or a string connective (a display-only filler, eg \"from\")."
  (check-type eff-parameter-list list)
  (unless (every (lambda (item) (or (varp item) (stringp item))) eff-parameter-list)
    (error "Expecting only ?/$ variables or string connectives in an effect parameter list: ~A"
           eff-parameter-list)))


(defun get-assert-established-$vars (assert-form)
  "Returns the $-variables established by SETQ, SETF, ASSIGN, WW-ASSIGN,
   MVSETQ, BIND, or LET occurring anywhere within ASSERT-FORM's own body
   (including nested inside IF/COND/DO). Mirrors the statement forms
   recognized by CHECK-VARIABLE-NAMES, restricted here to $-variables."
  (let (established)
    (ut::walk-tree (lambda (item)
                     (when (consp item)
                       (case (first item)
                         ((setq setf assign ww-assign)
                          (when ($varp (second item))
                            (push (second item) established)))
                         (mvsetq
                          (alexandria:appendf established
                                              (remove-if-not #'$varp (second item))))
                         ((bind let)
                          (alexandria:appendf established
                                              (remove-if-not #'$varp (second item)))))))
                   (cdr assert-form))
    (delete-duplicates established)))


(defun get-all-assert-established-$vars (effect-form)
  "Returns the union, across every ASSERT form anywhere in EFFECT-FORM, of
   the $-variables each ASSERT establishes within its own body via
   GET-ASSERT-ESTABLISHED-$VARS."
  (let (established)
    (ut::walk-tree (lambda (item)
                     (when (and (consp item) (eq (first item) 'assert))
                       (alexandria:appendf established (get-assert-established-$vars item))))
                   effect-form)
    (delete-duplicates established)))


(defun check-eff-param-var-provenance (action-name eff-param-vars pre-bound-$vars effect)
  "Warns when an eff-params $-variable is neither bound by the precondition
   (PRE-BOUND-$VARS) nor established within an ASSERT statement's own body.
   Such a variable's printed value in :instantiations then depends on
   wherever in the effect body it happens to be set -- eg, an outer
   IF-test that merely gates the ASSERT -- rather than on a guaranteed
   binding site local to the ASSERT that reports it. Does not trace
   through query/update function calls, matching CHECK-VARIABLE-NAMES's
   own scope."
  (let* ((eff-param-$vars (remove-if-not #'$varp eff-param-vars))
         (assert-established (get-all-assert-established-$vars effect))
         (unaccounted (set-difference eff-param-$vars
                                       (append pre-bound-$vars assert-established))))
    (when unaccounted
      (warn "Action ~A: eff-param $-variable~P ~{~A~^, ~} declared in the ~
             signature but not bound by the precondition nor established ~
             within an ASSERT statement's own body. Its value at the ~
             :instantiations capture point depends on incidental ~
             control-flow placement rather than a guaranteed binding site. ~
             Consider binding it in the precondition instead."
            action-name (length unaccounted) unaccounted))))


;;; ====================================================================
;;; Fluent-variable shadowing detection
;;;
;;; Walks an action's effect body looking for `bind` forms that overwrite
;;; a $-variable whose value came from the precondition or from an earlier
;;; setq/mvsetq in the same lexical scope. Such overwrites are usually a
;;; bug: the original value is silently lost. Reuse of $-variables across
;;; separate `bind` statements is a permitted idiom and is not flagged.
;;; ====================================================================

(defun walk-effect-shadow (action-name form owned)
  "Recursive walker over effect-body subforms. OWNED is the set of $-vars
   already written by the precondition or by a prior setq/mvsetq in the
   current lexical scope. A bind that targets a $-var in OWNED produces
   a shadowing warning. Bind itself does not extend OWNED, since reuse
   of $-variables across separate binds is permitted. Returns the updated
   OWNED set after walking FORM."
  (cond ((atom form) owned)
        ((eq (first form) 'quote) owned)
        ((eq (first form) 'bind)
         (process-shadow-bind action-name form owned))
        ((eq (first form) 'setq)
         (process-shadow-setq action-name form owned))
        ((member (first form) '(mvsetq multiple-value-setq))
         (process-shadow-mvsetq action-name form owned))
        ((eq (first form) 'let)
         (process-shadow-let action-name form owned))
        ((eq (first form) 'ww-loop)
         (process-shadow-ww-loop action-name form owned))
        ((eq (first form) 'if)
         (process-shadow-if action-name form owned))
        ((eq (first form) 'cond)
         (process-shadow-cond action-name form owned))
        ((member (first form) '(exists forsome exist forall forevery doall))
         (process-shadow-quantifier action-name form owned))
        (t (walk-shadow-sequence action-name form owned))))


(defun walk-shadow-sequence (action-name forms owned)
  "Walks each element of FORMS in source order, threading OWNED through
   each call. Used both for sequential bodies (do, progn, assert) and as
   the fallback for unspecialized list forms."
  (let ((current owned))
    (dolist (sub forms)
      (setf current (walk-effect-shadow action-name sub current)))
    current))


(defun process-shadow-bind (action-name form owned)
  "Inspects bind FORM. Warns once for each fluent-position $-variable
   target that is already in OWNED. Returns OWNED unchanged: bind writes
   do not extend OWNED."
  (let* ((proposition (second form))
         (relation (and (consp proposition) (first proposition)))
         (fluent-positions (and relation (get-prop-fluent-indices proposition))))
    (when fluent-positions
      (iter (for arg in (cdr proposition))
            (for pos from 1)
            (when (and (member pos fluent-positions)
                       ($varp arg)
                       (member arg owned))
              (terpri *error-output*)
              (warn "Fluent-variable overwrite in action ~A:~% ~
                     ~S writes to ~A,~% ~
                     which was already set by the precondition or an earlier setq/mvsetq.~% ~
                     Unless the overwrite is intentional, delete this bind statement.~%"
                    action-name form arg arg))))
    owned))


(defun process-shadow-setq (action-name form owned)
  "Walks setq's value sub-form, then adds the target $-variable to OWNED
   so subsequent bind statements can detect a shadow against it. setq
   itself does not warn."
  (let ((var (second form))
        (val (third form)))
    (let ((after-val (walk-effect-shadow action-name val owned)))
      (if (and (symbolp var) ($varp var))
          (adjoin var after-val)
          after-val))))


(defun process-shadow-mvsetq (action-name form owned)
  "Walks mvsetq's value sub-form, then adds all $-variable targets to
   OWNED so subsequent bind statements can detect a shadow against them.
   mvsetq itself does not warn."
  (let* ((vars (second form))
         (val (third form))
         (after-val (walk-effect-shadow action-name val owned))
         ($vars (remove-if-not #'$varp vars)))
    (union $vars after-val)))


(defun process-shadow-let (action-name form owned)
  "Handles let scoping. Each binding's value sub-form is walked in the
   outer scope; the body is walked with let-bound $-variables hidden from
   OWNED. On let exit, writes inside the body to let-bound $-variables
   are discarded and the outer-scope ownership of those names is restored."
  (let* ((bindings (second form))
         (let-vars (mapcar (lambda (b) (if (consp b) (first b) b)) bindings))
         (let-$vars (remove-if-not #'$varp let-vars))
         (after-bindings owned))
    (dolist (b bindings)
      (when (and (consp b) (cdr b))
        (setf after-bindings
              (walk-effect-shadow action-name (second b) after-bindings))))
    (let* ((inner (set-difference after-bindings let-$vars))
           (after-body (walk-shadow-sequence action-name (cddr form) inner)))
      (union (set-difference after-body let-$vars)
             (intersection after-bindings let-$vars)))))


(defun process-shadow-ww-loop (action-name form owned)
  "Handles ww-loop scoping. Iteration $-variables introduced by 'for'
   clauses are removed from OWNED inside the loop body and restored on
   exit, so a bind targeting a loop-iteration $-variable is not flagged
   on the basis of any outer-scope value of the same name."
  (let* ((iter-$vars (collect-ww-loop-iter-$vars form))
         (inner (set-difference owned iter-$vars))
         (after-body (walk-shadow-sequence action-name (cdr form) inner)))
    (union (set-difference after-body iter-$vars)
           (intersection owned iter-$vars))))


(defun process-shadow-if (action-name form owned)
  "Handles if scoping for fluent-variable shadow detection. TEST is walked
   in the incoming OWNED. The then-branch and else-branch (if present) are
   each walked starting from an independent copy of the post-test OWNED,
   isolating one branch's writes from the other, since only one branch
   executes at runtime. A missing else-branch is treated as an empty
   branch that writes nothing. Returns the merged OWNED: a $-variable is
   only considered reliably owned afterward if every branch wrote it."
  (let* ((test (second form))
         (then-branch (third form))
         (else-branch (fourth form))
         (after-test (walk-effect-shadow action-name test owned))
         (then-owned (walk-effect-shadow action-name then-branch after-test))
         (else-owned (if (cdddr form)
                       (walk-effect-shadow action-name else-branch after-test)
                       after-test)))
    (intersection then-owned else-owned)))


(defun process-shadow-cond (action-name form owned)
  "Handles cond scoping for fluent-variable shadow detection by converting
   FORM to an equivalent nested-if structure and delegating to
   PROCESS-SHADOW-IF, so cond clauses receive the same branch-isolating
   merge treatment as if."
  (process-shadow-if action-name (fluent-cond-clauses-to-if (cdr form)) owned))


(defun process-shadow-quantifier (action-name form owned)
  "Handles exists/forsome/exist/forall/forevery/doall scoping for fluent-
   variable shadow detection. The body may execute zero, one, or many
   times, so writes it performs cannot be assumed to have happened; returns
   the intersection of 'the body ran' and 'the body did not run' (OWNED
   unchanged), matching the same conservative policy used for if/cond."
  (let* ((body (third form))
         (after-body (walk-effect-shadow action-name body owned)))
    (intersection after-body owned)))


(defun collect-ww-loop-iter-$vars (form)
  "Returns the list of $-variables introduced as iteration variables by
   'for' clauses inside a ww-loop FORM."
  (let (vars)
    (loop for tail on (cdr form)
          while tail
          do (when (eq (first tail) 'for)
               (let ((spec (second tail)))
                 (typecase spec
                   (symbol (push spec vars))
                   (list (dolist (v spec) (push v vars)))))))
    (remove-if-not #'$varp vars)))


;;; ====================================================================
;;; Fluent-variable type tracking (Stage 4a: walker skeleton)
;;;
;;; Walks a precondition/effect/query body tracking an alist of
;;; ($var . declared-type) bindings established by `bind` statements at
;;; fluent positions. Mirrors the shadow-detection walker's dispatch/
;;; threading structure above, but tracks declared types rather than a
;;; written-vars set, and adds branch-isolating merge behavior for
;;; if/cond/quantifiers, which the shadow walker does not need. This is
;;; the environment-tracking skeleton only; cross-checking a $var's
;;; tracked type against a relation's/callee's declared type at point of
;;; use is Stage 4b, and wiring this walker into CREATE-ACTION and
;;; INSTALL-QUERY/INSTALL-UPDATE is Stage 4c.
;;; ====================================================================

(defun walk-fluent-types (action-name form env)
  "Recursive walker over precondition/effect/query-body subforms, tracking
   ENV, an alist of ($var . declared-type) bindings established by BIND
   statements at fluent positions. Dispatches on FORM's head to the
   appropriate scoping/merging handler; unspecialized forms fall through to
   WALK-FLUENT-SEQUENCE. Returns the updated ENV after walking FORM."
  (cond ((atom form) env)
        ((eq (first form) 'quote) env)
        ((eq (first form) 'bind)
         (process-fluent-bind action-name form env))
        ((member (first form) '(assign setq))
         (process-fluent-assign action-name form env))
        ((member (first form) '(mv-assign mvsetq multiple-value-setq))
         (process-fluent-mv-assign action-name form env))
        ((eq (first form) 'let)
         (process-fluent-let action-name form env))
        ((eq (first form) 'ww-loop)
         (process-fluent-ww-loop action-name form env))
        ((member (first form) '(exists forsome exist forall forevery doall))
         (process-fluent-quantifier action-name form env))
        ((eq (first form) 'if)
         (process-fluent-if action-name form env))
        ((eq (first form) 'cond)
         (process-fluent-cond action-name form env))
        ((or (gethash (first form) *relations*) (gethash (first form) *static-relations*))
         (check-fluent-proposition action-name form env)
         (walk-fluent-sequence action-name form env))
        ((member (first form) (append *query-names* *update-names*))
         (check-fluent-query/update-call action-name form env)
         (walk-fluent-sequence action-name form env))
        (t (walk-fluent-sequence action-name form env))))


(defun walk-fluent-sequence (action-name forms env)
  "Walks each element of FORMS in source order, threading ENV through each
   call. Used both for sequential bodies (do, progn, assert) and as the
   fallback for unspecialized list forms."
  (let ((current env))
    (dolist (sub forms)
      (setf current (walk-fluent-types action-name sub current)))
    current))


(defun process-fluent-bind (action-name form env)
  "Inspects bind FORM, extending ENV with the declared type of each
   $-variable bound at a fluent position, read off the target relation's
   declared signature. Returns the updated environment."
  (declare (ignore action-name))
  (let* ((proposition (second form))
         (relation (and (consp proposition) (first proposition)))
         (relation-def (and relation (or (gethash relation *relations*)
                                          (gethash relation *static-relations*))))
         (fluent-positions (and relation (get-prop-fluent-indices proposition))))
    (if (and fluent-positions (listp relation-def))
      (let ((updated env))
        (iter (for arg in (cdr proposition))
              (for type-def in relation-def)
              (for pos from 1)
              (when (and (member pos fluent-positions) ($varp arg))
                (setf updated (set-fluent-var-type updated arg type-def))))
        updated)
      env)))


(defun process-fluent-assign (action-name form env)
  "Walks the value sub-form of an assign/setq FORM, then marks the target
   $-variable :unknown in ENV, since an arbitrary computed value can't be
   attributed a declared type. Returns the updated environment."
  (let* ((var (second form))
         (val (third form))
         (after-val (walk-fluent-types action-name val env)))
    (if ($varp var)
      (set-fluent-var-type after-val var :unknown)
      after-val)))


(defun process-fluent-mv-assign (action-name form env)
  "Walks the value sub-form of an mv-assign/multiple-value-setq FORM, then
   marks each target $-variable :unknown in ENV. Returns the updated
   environment."
  (let* ((vars (second form))
         (val (third form))
         (after-val (walk-fluent-types action-name val env))
         ($vars (remove-if-not #'$varp vars))
         (result after-val))
    (dolist (var $vars result)
      (setf result (set-fluent-var-type result var :unknown)))))


(defun process-fluent-let (action-name form env)
  "Handles let scoping for fluent-variable type tracking. Each binding's
   value sub-form is walked in the outer environment; the body is walked
   with let-bound $-variables hidden from ENV. On exit, type information
   the body established for let-bound $-variables is discarded and any
   outer-scope type for those names is restored."
  (let* ((bindings (second form))
         (let-vars (mapcar (lambda (b) (if (consp b) (first b) b)) bindings))
         (let-$vars (remove-if-not #'$varp let-vars))
         (after-bindings env))
    (dolist (b bindings)
      (when (and (consp b) (cdr b))
        (setf after-bindings
              (walk-fluent-types action-name (second b) after-bindings))))
    (let* ((outer-entries (remove-if-not (lambda (entry) (member (car entry) let-$vars))
                                          after-bindings))
           (inner (remove-if (lambda (entry) (member (car entry) let-$vars))
                              after-bindings))
           (after-body (walk-fluent-sequence action-name (cddr form) inner)))
      (append (remove-if (lambda (entry) (member (car entry) let-$vars)) after-body)
              outer-entries))))


(defun process-fluent-ww-loop (action-name form env)
  "Handles ww-loop scoping for fluent-variable type tracking. Iteration
   $-variables introduced by 'for' clauses are hidden from ENV inside the
   loop body and their outer-scope type (if any) is restored on exit."
  (let* ((iter-$vars (collect-ww-loop-iter-$vars form))
         (inner (remove-if (lambda (entry) (member (car entry) iter-$vars)) env))
         (after-body (walk-fluent-sequence action-name (cdr form) inner))
         (outer-entries (remove-if-not (lambda (entry) (member (car entry) iter-$vars)) env)))
    (append (remove-if (lambda (entry) (member (car entry) iter-$vars)) after-body)
            outer-entries)))


(defun process-fluent-quantifier (action-name form env)
  "Handles exists/forsome/exist/forall/forevery/doall scoping for fluent-
   variable type tracking. The body may execute zero, one, or many times
   over its instantiation domain, so its $-variable bindings cannot be
   assumed to survive into code following the quantifier: only bindings
   consistent between 'the body ran' and 'the body did not run' (env
   unchanged) carry forward, per the same conservative merge policy used
   for if/cond."
  (let* ((body (third form))
         (after-body (walk-fluent-types action-name body env)))
    (merge-fluent-envs (list after-body env))))


(defun process-fluent-if (action-name form env)
  "Handles if scoping for fluent-variable type tracking. TEST is walked in
   the incoming ENV. The then-branch and else-branch (if present) are each
   walked starting from an independent copy of the post-test environment,
   isolating one branch's bindings from the other. A missing else-branch is
   treated as an empty branch that binds nothing. Returns the merged
   environment: a $-variable survives only if bound to the same type in
   every branch."
  (let* ((test (second form))
         (then-branch (third form))
         (else-branch (fourth form))
         (after-test (walk-fluent-types action-name test env))
         (then-env (walk-fluent-types action-name then-branch after-test))
         (else-env (if (cdddr form)
                     (walk-fluent-types action-name else-branch after-test)
                     after-test)))
    (merge-fluent-envs (list then-env else-env))))


(defun process-fluent-cond (action-name form env)
  "Handles cond scoping for fluent-variable type tracking by converting FORM
   to an equivalent nested-if structure and delegating to PROCESS-FLUENT-IF,
   so cond clauses receive the same branch-isolating merge treatment as if."
  (process-fluent-if action-name (fluent-cond-clauses-to-if (cdr form)) env))


(defun check-fluent-proposition (action-name proposition env)
  "Cross-checks each $-variable argument of PROPOSITION -- a plain
   (non-bind) relation reference -- against the relation's declared type at
   that argument position, using ENV's tracked bindings established by
   earlier bind statements. Mirrors CHECK-PROPOSITION's fluentless-position
   realignment so argument/type-def positions stay correctly paired even
   when PROPOSITION is short of its relation's full signature. Lenient, as
   usual, when either side is undeclared."
  (declare (ignore action-name))
  (let* ((relation-def (or (gethash (first proposition) *relations*)
                           (gethash (first proposition) *static-relations*)))
         (fluent-indices (get-prop-fluent-indices proposition))
         (fluentless-p (and fluent-indices
                            (listp relation-def)
                            (< (length (cdr proposition)) (length relation-def))))
         (effective-relation-def (if fluentless-p
                                   (loop for type-def in relation-def
                                         for i from 1
                                         unless (member i fluent-indices)
                                           collect type-def)
                                   relation-def)))
    (when (listp effective-relation-def)
      (iter (for arg in (cdr proposition))
            (for type-def in effective-relation-def)
            (when ($varp arg)
              (let ((declared-type (cdr (assoc arg env))))
                (unless (type-specs-compatible-p declared-type type-def)
                  (error "The $-variable ~A (declared ~A) is not compatible with type ~A ~
                          in proposition ~A"
                         arg declared-type type-def proposition))))))))


(defun check-fluent-query/update-call (action-name fn-call env)
  "Cross-checks each $-variable argument of FN-CALL -- a query or update
   function call -- against the callee's declared parameter type
   (:param-types, set by DISSECT-QUERY-PARAMS at install time) at that
   argument position, using ENV's tracked bindings. Lenient, as usual, when
   either side is undeclared."
  (declare (ignore action-name))
  (let ((callee-param-types (get (car fn-call) :param-types)))
    (iter (for arg in (cdr fn-call))
          (for position from 0)
          (when ($varp arg)
            (let ((declared-type (cdr (assoc arg env))))
              (unless (type-specs-compatible-p declared-type (nth position callee-param-types))
                (error "The $-variable ~A (declared ~A) is not compatible with the declared ~
                        parameter type ~A at position ~A of ~A"
                       arg declared-type (nth position callee-param-types) position fn-call)))))))


(defun fluent-cond-clauses-to-if (clauses)
  "Recursively converts a list of cond CLAUSES into an equivalent nested if
   form, purely for fluent-variable type-environment tracking. Kept as an
   independent implementation local to validation, mirroring (but not
   reusing) the clause-to-if conversion in TRANSLATE-COND, since that
   conversion is part of the translation path and this is not."
  (if (null clauses)
    nil
    (let* ((clause (car clauses))
           (test (car clause))
           (results (cdr clause))
           (then-form (cond ((null results) test)
                            ((null (cdr results)) (car results))
                            (t `(do ,@results))))
           (else-form (fluent-cond-clauses-to-if (cdr clauses))))
      (if else-form
        `(if ,test ,then-form ,else-form)
        `(if ,test ,then-form)))))


(defun merge-fluent-envs (envs)
  "Merges a list of $-variable type ENVS (each an alist of ($var . type))
   produced from isolated branches of a conditional or quantifier body.
   A $-variable survives into the merged environment only if it is bound
   to the same type in every member of ENVS; otherwise it is dropped, per
   the conservative branch-isolation policy for fluent-type tracking."
  (let ((candidate-vars (remove-duplicates (mapcar #'car (first envs)))))
    (iter (for var in candidate-vars)
          (let ((entries (mapcar (lambda (e) (assoc var e)) envs)))
            (when (and (every (lambda (entry) entry) entries)
                       (every (lambda (entry) (equal (cdr entry) (cdr (first entries)))) entries))
              (collect (cons var (cdr (first entries)))))))))


(defun set-fluent-var-type (env var type)
  "Returns a new $-variable type ENV with VAR's binding set to TYPE,
   replacing any existing entry for VAR."
  (acons var type (remove var env :key #'car)))


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


(defun check-precondition-type-instantiability (precondition)
  "Returns a list of static type names named by a top-level (TYPE $VAR) conjunct of
   PRECONDITION's outermost AND that have zero declared instances.  Such a conjunct
   tests type membership of an already-bound $-variable (typically from a preceding
   BIND) rather than declaring a formal parameter, but it is just as much a hard
   requirement: if TYPE has no instances the conjunct can never be true for any
   binding, so the whole action can never fire.  Only conjuncts directly inside the
   outermost AND are examined -- one nested inside OR or EXISTS is not a hard
   requirement, since a different branch may still succeed."
  (let ((uninstantiable-types nil))
    (when (and (consp precondition) (eq (first precondition) 'and))
      (dolist (conjunct (rest precondition))
        (when (and (consp conjunct)
                   (= (length conjunct) 2)
                   (symbolp (first conjunct))
                   ($varp (second conjunct)))
          (multiple-value-bind (instances exists-p) (gethash (first conjunct) *types*)
            (when (and exists-p (null instances))
              (pushnew (first conjunct) uninstantiable-types))))))
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
    (*solution-type* (unless (or (member val '(first every all-paths min-length min-time min-value max-value))
                                 (and (typep val 'fixnum) (> val 0)))
                       (error "Can't set *solution-type* to ~S. Must be one of~%~
                               first, every, all-paths, min-length, min-time, min-value, max-value,~%~
                               or a positive integer (to find that many solutions)." val)))
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
                             *auto-wait* requires *algorithm* = DEPTH-FIRST." *algorithm*))
                   (unless *happening-names*
                     (format t "~%It's normally inefficient to enable *auto-wait* without exogenous happenings.~%~
                                Define patrollers or other happening objects first.~2%"))))
    (*symmetry-pruning* nil)
    (*recorder-prefix-pruning*
      (unless (typep val 'boolean)
        (error "Can't set *recorder-prefix-pruning* to ~S. Must be either T or NIL." val)))
    (*max-recorder-cycles*
      (unless (and (typep val 'fixnum) (> val 0))
        (error "Can't set *max-recorder-cycles* to ~S. Must be a positive integer." val)))
    (*max-connector-pairings*
      (unless (and (typep val 'fixnum) (> val 0))
        (error "Can't set *max-connector-pairings* to ~S. Must be a positive integer." val)))
    ((*beam-occlusion-tolerance* *boundary-wall-height* *vertical-reach-limit*)
      (unless (and (rationalp val) (>= val 0))
        (error "Can't set ~S to ~S. Must be a non-negative rational." param val)))
    (*threads*
      (unless (and (typep val 'fixnum) (>= val 0))
        (error "Can't set *threads* to ~S. Must be a non-negative integer." val)))
    ((*num-closed-shards* *split-depth-max* *tasks-per-thread* *min-tasks*
      *bound-refresh-interval* *donation-check-interval* *donation-threshold*)
     (unless (and (typep val 'fixnum) (> val 0))
       (error "Can't set ~S to ~S. Must be a positive integer." param val)))
    ((*donation-fraction*)
     (unless (and (realp val) (>= val 0.0) (<= val 1.0))
       (error "Can't set *donation-fraction* to ~S. Must be a real between 0.0 and 1.0." val)))
    ((*enable-work-donation*)
     (unless (typep val 'boolean)
       (error "Can't set *enable-work-donation* to ~S. Must be T or NIL." val)))
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

;;; Filename: ww-installer.lisp

;;; Installs a user domain file.


(in-package :ww)


(defun either (&rest types)
  (apply #'append
         (mapcar (lambda (type)
                   (copy-list (gethash type *types*)))
                 types)))


(defun final-charp (final-char sym)
  "Determines if a symbol has a given final character in its name."
  (let ((str (symbol-name sym)))
    (char= (elt str (1- (length str))) final-char)))


(defmacro define-uniqueness (&key goal-key)
  "Problem-level hook. Example:
  (define-uniqueness :goal-key #'corner-goal-key)"
  `(setf *ww-goal-uniqueness-key-fn* ,goal-key))



(defmacro define-types (&rest types&values)
  `(install-types ',types&values))


(defmacro define-optional-types (&rest type-names)
  "Declares each of TYPE-NAMES as tolerant of the problem's own DEFINE-TYPES omitting it
   entirely -- registers the bare type as present in *TYPES* with no instances unless a
   real DEFINE-TYPES elsewhere in the spliced file already (or later) supplies one.  Lets
   a tech file use a bare type name directly in pre-params or relation signatures instead
   of an (either ...)-wrapped alias.  See INSTALL-OPTIONAL-TYPES for the guard that makes
   this order-independent."
  `(install-optional-types ',type-names))


(defun predeclare-type-names (types&instances)
  "Register type names before problem translation; real INSTALL-TYPES installs instances."
  (loop for (type) on types&instances by #'cddr
        do (check-type type symbol)
           (setf (gethash type *types*) nil)))


(defun predeclare-optional-type-names (type-names)
  "Register each of TYPE-NAMES before problem translation, exactly like
   PREDECLARE-TYPE-NAMES, so a bare optional type is a recognized *TYPES* key
   regardless of where its DEFINE-OPTIONAL-TYPES form falls in the spliced file."
  (dolist (type-name type-names)
    (check-type type-name symbol)
    (setf (gethash type-name *types*) nil)))


(defun check-type-signature-consistency (type instances)
  "Errors if TYPE is already registered in *type-signatures* under a different resolved
   instance list than INSTANCES, so two tech or problem files declaring the same type
   (eg, a composite type built with 'either') are forced to agree exactly."
  (multiple-value-bind (existing-instances foundp) (gethash type *type-signatures*)
    (when (and foundp (not (equal existing-instances instances)))
      (error "Type ~A is declared with conflicting instance lists: ~A vs ~A.  Every tech or ~
              problem file that declares ~A must use an identical definition."
             type existing-instances instances type))))


(defun install-types (types&instances)
  (format t "~&Installing object types...")
  (check-type types&instances cons)
  (iter (for (type instances) on types&instances by #'cddr)
        (check-type type symbol)
        (check-type instances list)
        (when (eql (char (format nil "~S" instances) 0) #\`)  ;eval backquoted form once at install
          (setf instances (eval instances))
          (check-type instances list))
        (when (eql (car instances) 'either)
          (setf (gethash type *type-components*) (cdr instances))  ;schema retained for the translator's leaf-type compatibility check
          (setf instances (or (remove-if #'null (remove-duplicates (apply #'either (cdr instances))))
                              '(nil))))
        (when (eql (car instances) 'compute)
          (setf instances (eval (second instances))))
        (check-type-signature-consistency type instances)
        (setf (gethash type *type-signatures*) instances)
        (setf (gethash type *types*) instances)
        (unless (equal instances '(nil))
          (dolist (instance instances)
            (check-type instance (or symbol real list))
            (when (symbolp instance)
              (setf (gethash (list 'something instance) *static-db*) t)
              (setf (gethash (list type instance) *static-db*) t))))))


(defun install-optional-types (type-names)
  "Ensures each of TYPE-NAMES is a recognized *TYPES* key with no instances, unless a real
   DEFINE-TYPES form -- anywhere in the spliced file, before or after this call -- already
   installed it with actual instances.  Consults *TYPE-SIGNATURES*, which only a real
   INSTALL-TYPES call ever populates, to detect that case and avoid clobbering it.  Unlike
   an (either ...)-wrapped alias, a type left at NIL here is a genuine empty instance list,
   so CHECK-ACTION-PARAMETER-INSTANTIABILITY correctly skips any action parameterized on it."
  (format t "~&Installing optional types...")
  (dolist (type-name type-names)
    (check-type type-name symbol)
    (unless (nth-value 1 (gethash type-name *type-signatures*))
      (setf (gethash type-name *types*) nil))))


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


(defun fluent-spec-p (spec)
  "Returns T if spec is or contains fluent variables."
  (cond
    ((symbolp spec) ($varp spec))
    ((and (listp spec) (eql (car spec) 'either))
     (some #'$varp (cdr spec)))
    (t nil)))


(defun normalize-fluent-spec (spec)
  "Strips $ prefix from fluent specifications."
  (cond
    ((symbolp spec)
     (if ($varp spec)
         (trim-1st-char spec)
         spec))
    ((and (listp spec) (eql (car spec) 'either))
     (cons 'either
           (mapcar (lambda (type)
                     (if ($varp type)
                         (trim-1st-char type)
                         type))
                   (cdr spec))))
    (t spec)))


(defun bijective-relation-p (relation)
  "Detects if a relation specification contains the :bijective annotation.
   Returns two values:
   1. The relation with :bijective removed (or unchanged if not present)
   2. T if :bijective was present, NIL otherwise."
  (let ((last-element (car (last relation))))
    (if (eql last-element :bijective)
        (values (butlast relation) t)
        (values relation nil))))


(defun create-bijective-indices (relation)
  "Creates dual internal indices for a validated bijective relation.
   For (on $block $support), creates:
   - ON1: keyed by position 1 (block), fluent at position 2
   - ON2: keyed by position 2 (support), fluent at position 1
   Registers all necessary data structures."
  (let* ((relation-name (car relation))
         (args (cdr relation))
         (normalized-args (mapcar #'normalize-fluent-spec args))
         (sorted-args (sort-either-types normalized-args))
         (index1-name (intern (format nil "~A1" relation-name)))
         (index2-name (intern (format nil "~A2" relation-name))))
    
    ;; Register canonical relation in *relations* for validation
    (setf (gethash relation-name *relations*) sorted-args)
    ;; Canonical relation has all positions as fluent
    (setf (gethash relation-name *fluent-relation-indices*) '(1 2))
    
    ;; Register ON1: keyed by position 1, fluent at position 2
    (setf (gethash index1-name *relations*) sorted-args)
    (setf (gethash index1-name *fluent-relation-indices*) '(2))
    
    ;; Register ON2: keyed by position 2, fluent at position 1
    (setf (gethash index2-name *relations*) sorted-args)
    (setf (gethash index2-name *fluent-relation-indices*) '(1))
    
    ;; Register bijective mappings
    (setf (gethash relation-name *bijective-relations*) 
          (list index1-name index2-name))
    (setf (gethash index1-name *bijective-canonical*) 
          (cons relation-name 1))
    (setf (gethash index2-name *bijective-canonical*) 
          (cons relation-name 2))))
            
            
(defmacro define-dynamic-relations (&rest relations)
  `(install-dynamic-relations ',relations))


(defun relation-signature-value (relation empty-value)
  (let ((normalized-args (mapcar #'normalize-fluent-spec (cdr relation))))
    (if normalized-args
      (sort-either-types normalized-args)
      empty-value)))


(defun relation-signature-fluent-indices (relation)
  (iter (for arg in (cdr relation))
        (for i from 1)
        (when (fluent-spec-p arg)
          (collect i))))


(defun check-relation-signature-consistency (relation table new-signature bijectivep)
  "Errors if RELATION's name is already registered in TABLE (*relations* or
   *static-relations*) under a different argument signature than NEW-SIGNATURE, or
   under a different :bijective status than BIJECTIVEP, so two tech files declaring
   the same relation are forced to agree exactly."
  (multiple-value-bind (existing-signature foundp) (gethash (car relation) table)
    (when (and foundp (not (equal existing-signature new-signature)))
      (error "Relation ~A is declared with conflicting signatures: ~A vs ~A.  Every tech or ~
              problem file that declares ~A must use an identical argument list."
             (car relation) existing-signature new-signature (car relation)))
    (when (and foundp (not (eql (and (gethash (car relation) *bijective-relations*) t) bijectivep)))
      (error "Relation ~A is declared :bijective in one place but not another.  Every tech or ~
              problem file that declares ~A must agree on its :bijective status."
             (car relation) (car relation)))))


(defun register-dynamic-relation-signature (raw-relation)
  (multiple-value-bind (relation bijectivep)
      (bijective-relation-p raw-relation)
    (check-relation relation)
    (let ((new-signature (relation-signature-value relation t)))
      (check-relation-signature-consistency relation *relations* new-signature bijectivep)
      (if bijectivep
        (progn
          (check-bijective-relation relation)
          (create-bijective-indices relation))
        (progn
          (setf (gethash (car relation) *relations*) new-signature)
          (ut::if-it (relation-signature-fluent-indices relation)
            (setf (gethash (car relation) *fluent-relation-indices*) ut::it)))))))


(defun register-static-relation-signature (relation)
  (check-relation relation)
  (let ((new-signature (relation-signature-value relation nil)))
    (check-relation-signature-consistency relation *static-relations* new-signature nil)
    (setf (gethash (car relation) *static-relations*) new-signature)
    (ut::if-it (relation-signature-fluent-indices relation)
      (setf (gethash (car relation) *fluent-relation-indices*) ut::it))))


(defun register-complementary-relation-signatures (positives->negatives)
  (iter (for (positive nil negative) on positives->negatives by #'cdddr)
        (check-relation positive)
        (check-relation (second negative))
        (let ((ordered-pos (sort-either-types positive))
              (ordered-neg (list 'not (sort-either-types (second negative)))))
          (setf (gethash (car positive) *complements*)
                (list ordered-pos ordered-neg)))))


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
  (iter (for raw-relation in relations)
        (register-dynamic-relation-signature raw-relation)
        (finally (maphash (lambda (key val)  ;install implied unary relations
                            (declare (ignore val))
                            (setf (gethash key *static-relations*) '(something)))
                          *types*)
                 (add-proposition '(always-true) *static-db*)
                 (setf (gethash 'always-true *static-relations*) '(always-true))))
  ;; Install symmetric relations (exclude bijective relations and their indices)
  (iter (for (key val) in-hashtable *relations*)
    (when (and (not (eql val t))
               (not (alexandria:setp val))  ;multiple types
               (not (final-charp #\> key))  ;not explicitly directed
               (not (gethash key *bijective-relations*))
               (not (gethash key *bijective-canonical*)))
      (setf (gethash key *symmetrics*) (symmetric-type-indexes val))))
  t)


(defmacro define-static-relations (&rest relations)
  `(install-static-relations ',relations))


(defun install-static-relations (relations)
  (format t "~&Installing static relations...")
  (iter (for relation in relations)
        (register-static-relation-signature relation)
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
  (register-complementary-relation-signatures positives->negatives))


(defun read-problem-forms (problem-path)
  (with-open-file (stream problem-path :direction :input)
    (loop for form = (read stream nil nil)
          while form
          collect form)))


(defun prescan-problem-function-names (forms)
  (let ((defun-names nil))
    (dolist (form forms)
      (when (and (consp form)
                 (symbolp (car form)))
        (case (car form)
          (define-query
            (pushnew (second form) *query-names*))
          (define-update
            (pushnew (second form) *update-names*))
          ((define-happening define-patroller)
            (pushnew (second form) *happening-names*))
          (defun
            (pushnew (second form) defun-names)))))
    (dolist (name defun-names)
      (unless (fboundp name)
        (let ((stub-name name))
          (setf (fdefinition name)
                (lambda (&rest args)
                  (declare (ignore args))
                  (error "Stub for ~A was called before real definition loaded" stub-name))))))))


(defun prescan-problem-type-names (forms)
  (dolist (form forms)
    (when (consp form)
      (case (car form)
        (define-types
          (predeclare-type-names (cdr form)))
        (define-optional-types
          (predeclare-optional-type-names (cdr form)))))))


(defun prescan-problem-relation-signatures (forms)
  ;; INCONSISTENT-STATE is the planner's own relation, not a problem's: PROPAGATE-CHANGES!
  ;; asserts it when the fixpoint fails to converge, and INIT-START-STATE tests for it.
  ;; It has to be in *RELATIONS* before anything is translated, or TRANSLATE reads
  ;; (INCONSISTENT-STATE) as a call to an unrecognized function.  It used to be registered
  ;; inside INSTALL-DYNAMIC-RELATIONS, which made its availability depend on a
  ;; DEFINE-DYNAMIC-RELATIONS form appearing above the fixpoint loop -- an accident of
  ;; layout that held only while every driver was written in the problem file, below its
  ;; declarations.  tech/-propagation.lisp now supplies the loop and splices above them.
  (setf (gethash 'inconsistent-state *relations*) t)
  (dolist (form forms)
    (when (consp form)
      (case (car form)
        (define-dynamic-relations
          (dolist (relation (cdr form))
            (register-dynamic-relation-signature relation)))
        (define-static-relations
          (dolist (relation (cdr form))
            (register-static-relation-signature relation)))
        (define-complementary-relations
          (register-complementary-relation-signatures (cdr form)))))))


(defun prescan-problem-file (problem-path)
  "Register forward-reference metadata needed before loading a problem file."
  (let ((*package* (find-package :ww)))
    (let ((forms (read-problem-forms problem-path)))
      (prescan-problem-function-names forms)
      (prescan-problem-type-names forms)
      (prescan-problem-relation-signatures forms))))

        
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
  ;; Happening name is registered in *happening-names* by the problem pre-scan;
  ;; do not push here.
  (when (getf plist :interrupt)
    (setf (get object :interrupt) (getf plist :interrupt)))
  ;; *eff-param-vars* is a global (not dynamically rebindable) that TRANSLATE-ASSERT
  ;; splices into an ASSERT's :instantiations. It only carries an action's own
  ;; signature from CREATE-ACTION, so reset it here to keep a stale value left over
  ;; from the last action installed from leaking into an ASSERT nested in this
  ;; happening's own interrupt clause.
  (setf *eff-param-vars* nil)
  (ut::if-it (getf plist :interrupt)
    (let (($vars (get-all-nonspecial-vars #'$varp ut::it)))
      (setf (get object :interrupt-lambda)
        `(lambda (state)
           (let ,$vars
             ,(when $vars
                `(declare (ignorable ,@$vars)))
              ,(translate (getf plist :interrupt) 'pre))))))
  (fix-if-ignore '(state) (get object :interrupt-lambda))
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
  "Revised query function installation with read-only semantics.
   Every parameter in ARGS is a ?variable optionally followed by a Wouldwork
   object type."
  (format t "~&Installing ~A query-fn..." name)
  (check-query/update-function name args body)
  (pushnew name *query-names*)
  (multiple-value-bind (flat-args param-types) (dissect-query-params args)
    (setf (get name :raw-body) body)  ;store for interprocedural symmetry walk
    (setf (get name :raw-args) flat-args)  ;store for interprocedural symmetry walk
    (setf (get name :param-types) param-types)  ;store for callee-side call-argument checking
    (walk-fluent-types name body nil)
    (let ((*var-type-env* (append (mapcar #'cons flat-args param-types) *var-type-env*))
          (new-$vars (delete-duplicates
                       (set-difference (get-all-nonspecial-vars #'$varp body) flat-args))))
      (setf (symbol-value name)
        `(lambda (state ,@flat-args)
           ,(format nil "~A query-fn" name)
           (declare (ignorable state ,@flat-args))
           (block ,name
             (let (,@new-$vars)
               (declare (ignorable ,@new-$vars))
               ;; Use pre context for read-only query semantics
               ,(if (eql (car body) 'let)
                  `(let ,(second body)
                     ,(third body)
                     ,(translate (fourth body) 'pre))
                  (translate body 'pre))))))
      (fix-if-ignore '(state) (symbol-value name)))))


(defmacro define-update (name args body)
  `(install-update ',name ',args ',body))


(defun install-update (name args body)
  "Installs a user-defined update function.
   Every parameter in ARGS is a ?variable optionally followed by a Wouldwork
   object type.
   Update functions translate according to current *algorithm* setting.
   Init-action processing (do-init-action-updates) handles both formats:
   - Depth-first: changes as hash-table with integer keys
   - Backtracking: changes as list of (forward inverse) pairs"
  (format t "~&Installing ~A update-fn..." name)
  (check-query/update-function name args body)
  (pushnew name *update-names*)
  (multiple-value-bind (flat-args param-types) (dissect-query-params args)
    (setf (get name :raw-body) body)  ;store for extract-effect-modified-relations
    (setf (get name :raw-args) flat-args)  ;store for interprocedural symmetry walk
    (setf (get name :param-types) param-types)  ;store for callee-side call-argument checking
    (walk-fluent-types name body nil)
    (let ((*var-type-env* (append (mapcar #'cons flat-args param-types) *var-type-env*))
          (new-$vars (delete-duplicates
                       (set-difference
                         (get-all-nonspecial-vars #'$varp body) flat-args))))
      ;; Translation uses current *algorithm* value
      (if new-$vars
        (setf (symbol-value name)
          `(lambda (state ,@flat-args)
             ,(format nil "~A update-fn" name)
             (declare (ignorable state ,@flat-args)
                      ,@(when (eq *algorithm* 'backtracking)
                          '((special forward-list inverse-list))))
             (let (updated-dbs ,@new-$vars)
               (declare (ignorable updated-dbs ,@new-$vars))
               ,(translate body 'eff))))
        (setf (symbol-value name)
          `(lambda (state ,@flat-args)
             ,(format nil "~A update-fn" name)
             (declare (ignorable state ,@flat-args)
                      ,@(when (eq *algorithm* 'backtracking)
                          '((special forward-list inverse-list))))
            ,(translate body 'eff))))
      (fix-if-ignore '(state) (symbol-value name)))))


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
  (let ((pre-param-types (nth-value 1 (dissect-pre-params
                                        (if (member (first pre-params) *parameter-headers*)
                                          pre-params
                                          (cons 'standard pre-params))))))
    (let ((uninstantiable (check-action-parameter-instantiability name pre-param-types)))
      (if uninstantiable
        (format t "skipped (no instances for type~P: ~{~A~^, ~})~%"
                (length uninstantiable) uninstantiable)
        (push (create-action name duration pre-params precondition eff-params effect nil)
              *actions*)))))


(defmacro define-init-action (name duration pre-params precondition eff-params effect)
  `(install-init-action ',name ,duration ',pre-params ',precondition ',eff-params ',effect))


(defun install-init-action (name duration pre-params precondition eff-params effect)
  (declare (ignore duration))
  (format t "~&Installing ~A init action..." name)
  (let ((pre-param-types (nth-value 1 (dissect-pre-params
                                        (if (member (first pre-params) *parameter-headers*)
                                          pre-params
                                          (cons 'standard pre-params))))))
    (let ((uninstantiable (check-action-parameter-instantiability name pre-param-types)))
      (if uninstantiable
        (format t "skipped (no instances for type~P: ~{~A~^, ~})~%"
                (length uninstantiable) uninstantiable)
        (push (create-action name 0 pre-params precondition eff-params effect t)
              *init-actions*)))))


(defun create-action (name duration pre-params precondition eff-params effect init-action)
  (check-type name symbol)
  (check-type duration (real 0 *) "zero or a positive number")
  (check-precondition-parameters pre-params)
  (check-effect-parameters eff-params)
  (unless (member (first pre-params) *parameter-headers*)
    (push 'standard pre-params))
  (multiple-value-bind (pre-param-?vars pre-param-types) (dissect-pre-params pre-params)
    (let ((eff-param-vars (remove-if #'stringp eff-params)))  ;pure var list, connectives stripped
      (let* ((flat-pre-param-?vars (alexandria:flatten pre-param-?vars))
             (*var-type-env* (append (mapcar #'cons flat-pre-param-?vars (flatten-param-types pre-param-types))
                                      *var-type-env*))
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
        (walk-effect-shadow name effect (append pre-$vars pre-special-$vars))
        (check-eff-param-var-provenance name eff-param-vars (append pre-$vars pre-special-$vars) effect)
        (let ((pre-fluent-env (walk-fluent-types name precondition nil)))
          (walk-fluent-types name effect pre-fluent-env))
        (cond (init-action
                 (setq *objective-value-p* nil))  ;this is an init-action, disable $objective-value
              ((or (member '$objective-value pre-$vars)  ;used in translate-assert
                   (member '$objective-value eff-extra-$vars))
                 (setq *objective-value-p* t))  ;this is a normal action rule with optimization
              (t (setq *objective-value-p* nil)))  ;normal rule, but no optimization
        (setq *eff-param-vars* eff-param-vars)  ;used in translate-assert
        (setq *has-sim-state* (member '$sim-state pre-$vars))
        (setf action (make-action
                       :name name
                       :pre-defun-name (ut::intern-symbol name '-PRE-FN)
                       :eff-defun-name (ut::intern-symbol name '-EFF-FN)
                       :duration duration
                       :precondition-form precondition  ;user's specified precondition
                       :effect-form effect              ;user's specified effect
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
                       :effect-variables eff-param-vars  ;pure var list, connectives stripped
                       :effect-format eff-params  ;annotated list w/ connectives, display only
                       :effect-lambda `(lambda (state ,@eff-args)
                                         ,(format nil "~A effect" name)
                                         (declare (ignorable ,@eff-args))
                                         (let (updated-dbs 
                                               followups 
                                               ,@(set-difference (set-difference eff-extra-$vars eff-args) eff-extra-?vars))
                                           (declare (ignorable ,@eff-extra-$vars))
                                           ,(translate effect 'pre)  ;start as pre, shift to eff in assert
                                           updated-dbs))
                       :effect-adds nil))
        (fix-if-ignore '(state) (action.precondition-lambda action))
        (fix-if-ignore `(state ,@eff-missing-vars) (action.effect-lambda action))
        (setf (action.effect-adds action)
              (extract-effect-modified-relations effect))
        action))))


(defun extract-effect-modified-relations (effect-form)
  "Walk EFFECT-FORM collecting dynamic relation symbols potentially modified.
   Transitively walks raw bodies of called update functions (stored on their
   symbol plists by install-update).  Skips quoted forms.
   Returns a list of relation symbols (conservative over-approximation)."
  (let ((modified (make-hash-table :test 'eq))
        (visited (make-hash-table :test 'eq)))
    (extract-effect-walk effect-form modified visited)
    (let ((result nil))
      (maphash (lambda (k v) (declare (ignore v)) (push k result)) modified)
      result)))


(defun extract-effect-walk (form modified visited)
  "Recursive worker for EXTRACT-EFFECT-MODIFIED-RELATIONS.
   Walks FORM collecting relation symbols into MODIFIED hash table.
   VISITED tracks already-processed update function names to prevent cycles."
  (cond
    ((null form) nil)
    ((symbolp form)
     (when (gethash form *relations*)
       (setf (gethash form modified) t))
     (when (and (member form *update-names* :test #'eq)
                (not (gethash form visited)))
       (setf (gethash form visited) t)
       (let ((raw-body (get form :raw-body)))
         (when raw-body
           (extract-effect-walk raw-body modified visited)))))
    ((and (consp form) (eq (car form) 'quote)) nil)
    ((and (consp form) (eq (car form) 'finally)) nil)
    ((consp form)
     (extract-effect-walk (car form) modified visited)
     (extract-effect-walk (cdr form) modified visited))))


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


(defmacro define-init (&rest literals)
  `(install-init ',literals))


(defun install-init (literals)
  ;(declare (special *relations* *db* *static-db*))
  (format t "~&Creating initial propositional database...")
  (check-type literals cons)
  (let ((literals (mapcar (lambda (literal)
                            (if (eql (char (format nil "~S" literal) 0) #\`)
                              (eval literal)
                              literal))
                          literals)))
    (dolist (literal literals)
      (if (eql (car literal) 'not)
        (check-proposition (second literal))
        (check-proposition literal)))
    (validate-init-literals literals)
    (dolist (literal literals)
      (if (eql (car literal) 'not)
        (if (gethash (caadr literal) *relations*)
          (delete-proposition (second literal) *db*)
          (delete-proposition (second literal) *static-db*))
        (if (gethash (car literal) *relations*)
          (add-proposition literal *db*)
          (add-proposition literal *static-db*))))))


(defmacro define-goal (form)
  `(install-goal ',form))


(defun install-goal (form)
  (format t "~&Installing goal...")
  (check-type form list)
  (when (eql (char (format nil "~S" form) 0) #\`)  ;eval backquoted form once at install
    (setf form (eval form)))
  (when (and (null form)
             (not (eql *solution-type* 'min-value))
             (not (eql *solution-type* 'max-value)))
    (error "Goal is required unless searching for a *solution-type* of min-value or max-value."))
  (setf *goal* form)
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

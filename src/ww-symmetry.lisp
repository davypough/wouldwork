;;; Filename: ww-symmetry.lisp

;;; Symmetry detection and pruning for Wouldwork planner.
;;; Identifies families of interchangeable rows based on type membership,
;;; complete static-database automorphisms, transition/goal references, and
;;; problem-local coupling relations.  An ordinary object is a one-column row;
;;; recorder live/ghost pairs are two-column rows whose columns never exchange.
;;; Provides two pruning strategies:
;;;   - Local (tree/backtracking): generation-time filtering of symmetric instantiations
;;;   - Global (graph): canonical closed-list hashing treats symmetric states as duplicates


(in-package :ww)


;;;; GLOBAL VARIABLES ;;;;


(defparameter *symmetry-groups* nil
  "Flattened object lists for detected symmetry families.
   Retained for reporting and enumerator compatibility; search uses *SYMMETRY-FAMILIES*.")


(defstruct (symmetry-family (:conc-name symmetry-family.))
  "Rows that may be permuted while each column retains its semantic role."
  rows)


(defstruct (symmetry-membership (:conc-name symmetry-membership.))
  "An object's family, row, and role column."
  family
  row-index
  column-index)


(defparameter *symmetry-families* nil
  "Detected row-permutation families used by local and canonical pruning.")


(defparameter *object-to-symmetry-group* (make-hash-table :test #'eq)
  "Maps each object to its flattened symmetry family, or NIL if singleton.")


(defparameter *object-to-symmetry-membership* (make-hash-table :test #'eq)
  "Maps each object to its SYMMETRY-MEMBERSHIP record.")


(defparameter *symmetric-type-parameters* (make-hash-table :test #'eq)
  "Maps action-name to list of parameter indices that have symmetric types.")


(defparameter *symmetry-safety-report* nil
  "The last flow check: an alist of (family . spot texts) in detection order, ending with
   (:all . spot texts) for spots that affect every family.")


(sb-ext:defglobal *symmetry-pruning-count* 0
  "Count of action instantiations pruned due to symmetry during search.")
(declaim (type fixnum *symmetry-pruning-count*))


(sb-ext:defglobal *symmetry-check-count* 0
  "Count of symmetry checks performed during search.")
(declaim (type fixnum *symmetry-check-count*))


(sb-ext:defglobal *symmetric-duplicates-pruned* 0
  "Count of states pruned as symmetric duplicates in canonical mode (global strategy).")
(declaim (type fixnum *symmetric-duplicates-pruned*))


(defvar *signature-element-string-cache* nil
  "When non-NIL, a hash-table used to memoize (prin1-to-string elem) results
   during signature sorting and canonical signature comparisons.")


(defvar *local-symmetry-state-form* nil
  "Dynamically bound symbolic state form used by local transposition checks.")


(defvar *local-symmetry-swap-cache* nil
  "Dynamically bound cache of local row-transposition results.")


(sb-ext:defglobal *symmetry-visit-domain-indices* (make-hash-table :test #'equal)
  "Maps each type name, and the contents of each *types* instance list, to its visit-domain
   index, or to NIL when it holds fewer than two family members.  Compiled code holds a
   copy of a type's list, so a loop names its type to look it up by symbol.  Filled at
   staging; read-only during search.")


(sb-ext:defglobal *symmetry-visit-domain-count* 0
  "Number of indexed visit domains.")


(defmacro with-signature-element-string-cache (&body body)
  "Evaluate BODY with a fresh cache for signature-element->string."
  `(let ((*signature-element-string-cache* (make-hash-table :test #'eql)))
     ,@body))


;;;; PRE-SEARCH SYMMETRY DETECTION ;;;;


(defun detect-symmetry-groups ()
  "Detect exact static row symmetries and populate search lookup structures."
  (setf *symmetry-groups* nil
        *symmetry-families* nil)
  (clrhash *object-to-symmetry-group*)
  (clrhash *object-to-symmetry-membership*)
  (clrhash *symmetric-type-parameters*)
  (reset-symmetry-statistics)
  (let* ((fixed-objects
           (union
             (extract-goal-object-references)
             (union (extract-transition-object-references)
                    (extract-happening-object-references)
                    :test #'eq)
             :test #'eq))
         (coupled-rows (collect-coupled-symmetry-rows))
         (coupled-objects
           (remove-duplicates
             (loop for rows in coupled-rows append (apply #'append rows))
             :test #'eq))
         (families
           (append
             (detect-coupled-symmetry-families coupled-rows fixed-objects)
             (detect-ordinary-symmetry-families coupled-objects fixed-objects))))
    (setf *symmetry-families*
          (remove-duplicate-symmetry-families families))
    (rebuild-symmetry-lookups)
    (identify-symmetric-action-parameters)
    (setf *symmetry-safety-report* (check-symmetry-family-flow *symmetry-families*))
    (prune-inoperative-symmetry-groups)
    *symmetry-groups*))


(defun rename-symmetry-tree (tree mapping)
  "Recursively replace object atoms in TREE according to MAPPING."
  (cond ((consp tree)
         (cons (rename-symmetry-tree (car tree) mapping)
               (rename-symmetry-tree (cdr tree) mapping)))
        (t
         (multiple-value-bind (replacement presentp) (gethash tree mapping)
           (if presentp replacement tree)))))


(defun make-row-transposition (row1 row2)
  "Return the complete column-preserving transposition of ROW1 and ROW2."
  (unless (= (length row1) (length row2))
    (error "Cannot transpose symmetry rows of different lengths: ~S and ~S" row1 row2))
  (let ((mapping (make-hash-table :test #'eq)))
    (loop for object1 in row1
          for object2 in row2
          do (setf (gethash object1 mapping) object2
                   (gethash object2 mapping) object1))
    mapping))


(defun static-transposition-preserves-p (row1 row2)
  "Whether exchanging ROW1 and ROW2 leaves the complete static database unchanged."
  (let ((mapping (make-row-transposition row1 row2)))
    (loop for key being the hash-keys of *static-db* using (hash-value value)
          always
            (let ((renamed-key (rename-symmetry-tree key mapping))
                  (renamed-value (rename-symmetry-tree value mapping)))
              (multiple-value-bind (stored-value presentp)
                  (gethash renamed-key *static-db*)
                (and presentp (equal stored-value renamed-value)))))))


(defun form-object-references (form all-objects)
  "Return object constants occurring literally in FORM."
  (let ((references nil)
        (stack (list form)))
    (loop while stack
          for item = (pop stack)
          do (cond ((consp item)
                    (push (car item) stack)
                    (push (cdr item) stack))
                   ((and (symbolp item) (member item all-objects :test #'eq))
                    (pushnew item references :test #'eq))))
    references))


(defun extract-transition-object-references ()
  "Return constants embedded directly in searched actions, queries, or updates."
  (let ((all-objects (collect-all-objects))
        (references nil))
    (dolist (action *actions*)
      (dolist (form (list (action.precondition-form action)
                          (action.effect-form action)))
        (setf references
              (union references (form-object-references form all-objects) :test #'eq))))
    (dolist (name (append *query-names* *update-names*))
      (setf references
            (union references
                   (form-object-references (get name :raw-body) all-objects)
                   :test #'eq)))
    references))


(defun extract-happening-object-references ()
  "Return happening owners and object constants embedded in their event programs."
  (let ((all-objects (collect-all-objects))
        (references nil))
    (dolist (object *happening-names*)
      (pushnew object references :test #'eq)
      (let ((events (get object :events)))
        (when events
          (setf references
                (union references
                       (form-object-references
                         (coerce events 'list) all-objects)
                       :test #'eq)))))
    references))


(defun symmetry-coupling-storage-relation (relation)
  "Return the stored relation that contains RELATION's ordered coupling rows."
  (or (first (gethash relation *bijective-relations*))
      relation))


(defun collect-coupled-symmetry-rows ()
  "Return one disjoint object-row list per registered static coupling relation."
  (let ((static-propositions (list-database *static-idb*))
        (all-objects (collect-all-objects))
        (seen-objects (make-hash-table :test #'eq)))
    (loop for relation in *symmetry-coupling-relations*
          for storage-relation = (symmetry-coupling-storage-relation relation)
          for rows = (loop for proposition in static-propositions
                           when (eq (first proposition) storage-relation)
                             collect (rest proposition))
          when rows
            do (let ((arity (length (first rows))))
                 (dolist (row rows)
                   (unless (= (length row) arity)
                     (error "Symmetry coupling ~S has inconsistent row arity: ~S"
                            relation row))
                   (dolist (object row)
                     (unless (member object all-objects :test #'eq)
                       (error "Symmetry coupling ~S contains a non-object: ~S"
                              relation object))
                     (when (gethash object seen-objects)
                       (error "Object ~S occurs in more than one symmetry coupling row."
                              object))
                     (setf (gethash object seen-objects) t))))
            and collect rows)))


(defun partition-interchangeable-rows (rows)
  "Partition ROWS into full static-transposition families."
  (let ((remaining (copy-list rows))
        (families nil))
    (loop while remaining
          for seed = (pop remaining)
          for matches = (remove-if-not
                          (lambda (row)
                            (static-transposition-preserves-p seed row))
                          remaining)
          do (setf remaining (set-difference remaining matches :test #'equal))
             (when matches
               (push (make-symmetry-family :rows (cons seed matches)) families)))
    (nreverse families)))


(defun detect-coupled-symmetry-families (coupled-row-sets fixed-objects)
  "Detect row symmetries supplied by registered coupling relations."
  (loop for rows in coupled-row-sets
        for eligible = (remove-if
                         (lambda (row)
                           (intersection row fixed-objects :test #'eq))
                         rows)
        append (partition-interchangeable-rows eligible)))


(defun detect-ordinary-symmetry-families (coupled-objects fixed-objects)
  "Detect exact one-column symmetries among objects outside coupled rows."
  (let ((families nil))
    (dolist (type-name (identify-candidate-types))
      (let* ((objects (gethash type-name *types*))
             (eligible
               (set-difference objects
                               (union coupled-objects fixed-objects :test #'eq)
                               :test #'eq)))
        (when (> (length eligible) 1)
          (setf families
                (nconc families
                       (partition-interchangeable-rows
                         (mapcar #'list eligible)))))))
    families))


(defun symmetry-family-objects (family)
  "Return every object in FAMILY in row-major order."
  (apply #'append (symmetry-family.rows family)))


(defun remove-duplicate-symmetry-families (families)
  "Remove families rediscovered through overlapping union types."
  (remove-duplicates
    families
    :test (lambda (family1 family2)
            (alexandria:set-equal
              (symmetry-family-objects family1)
              (symmetry-family-objects family2)))))


(defun rebuild-symmetry-lookups ()
  "Rebuild flattened compatibility groups and per-object row memberships."
  (setf *symmetry-groups*
        (mapcar #'symmetry-family-objects *symmetry-families*))
  (clrhash *object-to-symmetry-group*)
  (clrhash *object-to-symmetry-membership*)
  (dolist (family *symmetry-families*)
    (let ((group (symmetry-family-objects family)))
      (loop for row in (symmetry-family.rows family)
            for row-index from 0
            do (loop for object in row
                     for column-index from 0
                     do (setf (gethash object *object-to-symmetry-group*) group
                              (gethash object *object-to-symmetry-membership*)
                                (make-symmetry-membership
                                  :family family
                                  :row-index row-index
                                  :column-index column-index))))))
  *symmetry-groups*)


(defun identify-candidate-types ()
  "Return list of type names that have more than one object instance."
  (let (candidates)
    (maphash (lambda (type-name objects)
               (when (and (listp objects)
                          (> (length objects) 1))
                 (push type-name candidates)))
             *types*)
    candidates))


(defun compute-all-signatures (candidate-types)
  "Compute signatures for all objects in candidate types.
   Returns hash table: object -> signature."
  (let ((signatures (make-hash-table :test #'eq)))
    (dolist (type-name candidate-types)
      (let ((objects (gethash type-name *types*)))
        (dolist (object objects)
          (unless (gethash object signatures)  ; avoid recomputing for objects in multiple types
            (setf (gethash object signatures)
                  (compute-object-signature object))))))
    signatures))


(defun compute-object-signature (object)
  "Compute signature for an object based on its static relations only.
   Returns a normalized set of propositions with the object replaced by a placeholder.
   Checks both hash keys AND values to catch objects in fluent positions.
   Initial dynamic state is intentionally excluded because:
   - Dynamic relations represent manipulable/arbitrary initial assignments
   - Swapping symmetric objects with their initial values yields isomorphic problems
   - Only static relations define permanent structural asymmetries"
  (let (normalized-props)
    ;; Process static propositions only
    (maphash (lambda (key value)
               (let ((in-key (object-in-proposition-p object key))
                     (in-value (object-in-proposition-p object value)))
                 (when (or in-key in-value)
                   (let ((norm-key (if in-key
                                       (normalize-proposition key object)
                                       key))
                         (norm-value (if in-value
                                         (normalize-proposition value object)
                                         value)))
                     (push (list norm-key norm-value) normalized-props)))))
             *static-db*)
    ;; Return sorted list for consistent comparison
    (sort normalized-props #'signature-element-less-p)))


(defun object-in-proposition-p (object proposition)
  "Return T if OBJECT appears in PROPOSITION."
  (cond ((atom proposition) (eq proposition object))
        (t (or (object-in-proposition-p object (car proposition))
               (object-in-proposition-p object (cdr proposition))))))


(defun normalize-proposition (proposition object)
  "Replace OBJECT with placeholder symbol '_ in PROPOSITION."
  (cond ((eq proposition object) '_)
        ((atom proposition) proposition)
        (t (cons (normalize-proposition (car proposition) object)
                 (normalize-proposition (cdr proposition) object)))))


(defun signature-element->string (elem)
  "Return a cached printed representation of ELEM when cache is active."
  (let ((cache *signature-element-string-cache*))
    (if cache
        (multiple-value-bind (s presentp) (gethash elem cache)
          (if presentp
              s
              (setf (gethash elem cache) (prin1-to-string elem))))
        (prin1-to-string elem))))


(defun ww-object< (a b)
  "Deterministic total order over the object shapes used in signatures/canonical keys.
   Orders by type, then lexicographically for cons trees."
  (labels ((rank (x)
             (cond ((integerp x) 0)
                   ((symbolp x)  1)
                   ((consp x)    2)
                   (t            3)))
           (sym< (s1 s2)
             (let ((n1 (symbol-name s1))
                   (n2 (symbol-name s2)))
               (cond ((string< n1 n2) t)
                     ((string< n2 n1) nil)
                     (t
                      ;; Tie-break by package to ensure total order.
                      (let ((p1 (let ((p (symbol-package s1))) (if p (package-name p) "")))
                            (p2 (let ((p (symbol-package s2))) (if p (package-name p) ""))))
                        (string< p1 p2))))))
           (cons< (x y)
             ;; Lexicographic compare: car, then cdr
             (cond ((ww-object< (car x) (car y)) t)
                   ((ww-object< (car y) (car x)) nil)
                   (t (ww-object< (cdr x) (cdr y))))))
    (let ((ra (rank a))
          (rb (rank b)))
      (cond ((< ra rb) t)
            ((> ra rb) nil)
            ((integerp a) (< a b))
            ((symbolp a) (sym< a b))
            ((consp a)   (cons< a b))
            (t nil)))))


(defun signature-element-less-p (elem1 elem2)
  "Comparison function for sorting signature elements."
  ;; structural order, no prin1-to-string allocation
  (ww-object< elem1 elem2))


(defun partition-by-signature (candidate-types signatures)
  "Partition objects within each type by their signatures.
   Returns list of groups where each group has size > 1.
   Removes duplicate groups that may arise from objects appearing in multiple types."
  (let (groups)
    (dolist (type-name candidate-types)
      (let* ((objects (gethash type-name *types*))
             (partitions (make-hash-table :test #'equal)))  ; signature -> objects
        ;; Group objects by signature
        (dolist (object objects)
          (let ((sig (gethash object signatures)))
            (push object (gethash sig partitions))))
        ;; Collect groups with more than one member
        (maphash (lambda (sig objects)
                   (declare (ignore sig))
                   (when (> (length objects) 1)
                     (push (reverse objects) groups)))  ; reverse to preserve original order
                 partitions)))
    ;; Remove duplicate groups (same objects may appear in multiple types via 'either')
    (remove-duplicates groups :test #'alexandria:set-equal)))


(defun split-groups-by-goal-references (groups)
  "Split symmetry groups based on explicit goal references.
   Objects named in the goal are excluded from all symmetry groups
   to ensure correctness when goals have complex disjunctive structure."
  (let ((goal-objects (extract-goal-object-references)))
    (when (null goal-objects)
      (return-from split-groups-by-goal-references groups))
    (let (new-groups)
      (dolist (group groups)
        (let ((unreferenced (set-difference group goal-objects)))
          ;; Keep only unreferenced objects, and only if > 1 remain
          (when (> (length unreferenced) 1)
            (push unreferenced new-groups))))
      (nreverse new-groups))))


(defun extract-goal-object-references ()
  "Extract object constants that appear explicitly in the goal specification."
  (unless (boundp 'goal-fn)
    (return-from extract-goal-object-references nil))
  (let ((goal-source (get 'goal-fn :form))
        (all-objects (collect-all-objects))
        (referenced nil))
    (labels ((walk (form)
               (cond
                 ((and (atom form) (member form all-objects))
                  (pushnew form referenced))
                 ((consp form)
                  (walk (car form))
                  (walk (cdr form))))))
      (walk goal-source))
    referenced))


(defun collect-all-objects ()
  "Collect all object constants from *types*."
  (let (objects)
    (maphash (lambda (type-name type-objects)
               (declare (ignore type-name))
               (when (listp type-objects)
                 (dolist (obj type-objects)
                   (when obj
                     (pushnew obj objects)))))
             *types*)
    objects))


(defparameter *permutation-breaking-operators*
  '(+ - * / mod rem 1+ 1- abs floor ceiling truncate round
    expt log exp sqrt sin cos tan gcd lcm
    < > <= >= min max
    ash logand logior logxor lognot logbitp
    nth aref elt svref char schar subseq
    gethash getf assoc)
  "Operators whose semantics are not invariant under permutation of object
   identifiers. A parameter that appears (transitively) as an argument of any
   of these operators in an action's precondition cannot be soundly pruned by
   symmetry, because the action's truth value depends on the parameter's
   identity, not just on its role.")


(defun var-in-breaking-context-p (form var-name in-breaking &optional visited)
  "Walk FORM looking for VAR-NAME under a breaking-operator ancestor.
   IN-BREAKING is T when FORM itself is already inside such an operator.
   VISITED is a list of user-function names already entered, to prevent
   infinite recursion through self- or mutual-recursive calls.
   Returns T as soon as one breaking occurrence of VAR-NAME is found."
  (cond ((eq form var-name) in-breaking)
        ((atom form) nil)
        ((eq (car form) 'quote) nil)
        ;; Interprocedural step: descend into body of a known user function
        ;; if it receives VAR-NAME positionally and we haven't entered it yet.
        ((and (symbolp (car form))
              (or (member (car form) *update-names* :test #'eq)
                  (member (car form) *query-names*  :test #'eq))
              (not (member (car form) visited :test #'eq))
              (let ((formals (get (car form) :raw-args))
                    (body    (get (car form) :raw-body))
                    (actuals (cdr form)))
                (and formals body
                     (loop for actual in actuals
                           for formal in formals
                           thereis (and (eq actual var-name)
                                        (var-in-breaking-context-p
                                          body formal nil
                                          (cons (car form) visited)))))))
         t)
        (t (some (lambda (sub)
                   (var-in-breaking-context-p sub var-name
                                              (or in-breaking
                                                  (and (symbolp (car form))
                                                       (member (car form)
                                                               *permutation-breaking-operators*
                                                               :test #'eq)))
                                              visited))
                 (cdr form)))))


(defun identify-symmetric-action-parameters ()
  "Identify which actions have parameters of symmetric types.
   Excludes parameters that appear inside permutation-breaking operators
   (arithmetic, ordering, indexing) in the precondition or effect, since those
   parameters' identities affect action semantics and cannot be soundly pruned.
   Populates *symmetric-type-parameters*."
  ;; Build set of types that have symmetry groups
  (let ((symmetric-types (make-hash-table :test #'eq)))
    (dolist (group *symmetry-groups*)
      (dolist (object group)
        ;; Find types containing this object
        (maphash (lambda (type-name type-objects)
                   (when (and (listp type-objects)
                              (member object type-objects))
                     (setf (gethash type-name symmetric-types) t)))
                 *types*)))
    ;; Check each action's parameters
    (dolist (action *actions*)
      (let ((param-indices nil)
            (param-types (action.precondition-types action))
            (param-vars (action.precondition-variables action))
            (pre-form (action.precondition-form action))
            (eff-form (action.effect-form action))
            (inst-idx 0))  ; Index into instantiation (excludes headers)
        (dolist (ptype param-types)
          (cond ((member ptype *parameter-headers*)
                 nil)  ; Skip headers - don't increment inst-idx
                (t
                 (when (and (type-has-symmetric-objects-p ptype symmetric-types)
                            (not (var-in-breaking-context-p pre-form
                                                            (nth inst-idx param-vars)
                                                            nil))
                            (not (var-in-breaking-context-p eff-form
                                                            (nth inst-idx param-vars)
                                                            nil)))
                   (push inst-idx param-indices))
                 (incf inst-idx))))
        (when param-indices
          (setf (gethash (action.name action) *symmetric-type-parameters*)
                (nreverse param-indices)))))))


(defun prune-inoperative-symmetry-groups ()
  "Keep only families whose every action use is permutation-safe, and that some action
   parameter ranges over or that the flow check passes clean.  A single identity-sensitive
   action use invalidates a family for global canonicalization, even when another action
   could use that family safely."
  (setf *symmetry-families*
        (remove-if-not (lambda (family)
                         (member (symmetry-family-status family)
                                 '(:kept-clean :kept-flagged :query-only-clean)))
                       *symmetry-families*))
  (rebuild-symmetry-lookups))


(defun symmetry-family-status (family)
  "Classify FAMILY by the action-parameter checks and the flow check (report only):
   :UNSAFE-ACTION, :KEPT-CLEAN, :KEPT-FLAGGED, :QUERY-ONLY-CLEAN or :QUERY-ONLY-FLAGGED."
  (let ((group (symmetry-family-objects family))
        (clean (and (null (rest (assoc family *symmetry-safety-report*)))
                    (null (rest (assoc :all *symmetry-safety-report*))))))
    (cond ((notevery (lambda (action)
                       (action-uses-group-entirely-safely-p action group))
                     *actions*)
           :unsafe-action)
          ((some (lambda (action)
                   (action-uses-group-soundly-p action group))
                 *actions*)
           (if clean :kept-clean :kept-flagged))
          (clean :query-only-clean)
          (t :query-only-flagged))))


(defun action-uses-group-soundly-p (action group)
  "Returns T if ACTION has at least one kept (sound-for-pruning) parameter
   whose declared type includes some object in GROUP."
  (let ((kept-indices (gethash (action.name action) *symmetric-type-parameters*))
        (non-header-types (remove-if (lambda (ptype)
                                       (member ptype *parameter-headers*))
                                     (action.precondition-types action))))
    (some (lambda (idx)
            (type-includes-group-member-p (nth idx non-header-types) group))
          kept-indices)))


(defun action-group-parameter-indices (action group)
  "Return every ACTION parameter index whose declared type intersects GROUP."
  (let ((non-header-types
          (remove-if (lambda (ptype)
                       (member ptype *parameter-headers*))
                     (action.precondition-types action))))
    (loop for type in non-header-types
          for index from 0
          when (type-includes-group-member-p type group)
            collect index)))


(defun action-uses-group-entirely-safely-p (action group)
  "Whether every ACTION parameter that can bind GROUP passed the safety walk."
  (let ((used-indices (action-group-parameter-indices action group))
        (kept-indices
          (gethash (action.name action) *symmetric-type-parameters*)))
    (or (null used-indices)
        (subsetp used-indices kept-indices :test #'=))))


(defun type-includes-group-member-p (type-spec group)
  "Returns T if any object in GROUP is an instance of TYPE-SPEC.
   TYPE-SPEC may be a simple type name or an (either ...) form."
  (cond ((atom type-spec)
         (let ((objects (gethash type-spec *types*)))
           (and (listp objects)
                (some (lambda (obj) (member obj group)) objects))))
        ((and (consp type-spec) (eq (car type-spec) 'either))
         (some (lambda (sub) (type-includes-group-member-p sub group))
               (cdr type-spec)))
        (t nil)))


(defun type-has-symmetric-objects-p (type-spec symmetric-types)
  "Check if TYPE-SPEC includes any type in SYMMETRIC-TYPES.
   TYPE-SPEC may be a simple type name or an (either ...) form."
  (cond
    ((atom type-spec)
     (gethash type-spec symmetric-types))
    ((and (consp type-spec) (eq (car type-spec) 'either))
     (some (lambda (subtype) 
             (type-has-symmetric-objects-p subtype symmetric-types))
           (cdr type-spec)))
    (t nil)))


;;;; SAFETY FLOW CHECK ;;;;
;;; Follows every family's members through the code the search runs: action parameters,
;;; preconditions and effects, query and update bodies (interprocedurally), the goal, the
;;; propagation driver and the search hooks.  A value's taint is the list of families
;;; whose members it may hold.  A use is a spot when a member reaches something not known
;;; to be name-free: a plain Lisp function or Lisp operator outside the allow lists, a
;;; literal member name, or a dot-product quantifier.  Plain Lisp reached with the
;;; position is checked against the boundary rule: it may pass the position on to queries
;;; and updates, or to other plain Lisp that does the same, and nothing else; a query
;;; result that may hold members, handed back into plain Lisp, is a spot as well.
;;; Report only: families are not dropped on the check's account, except a family no
;;; action parameter ranges over, which is kept only when the check passes it clean.


(defparameter *flow-value-operators*
  '(list list* cons append nconc first second third fourth fifth sixth last butlast rest
    car cdr caar cadr cdar cddr caddr cdddr cadddr copy-list copy-tree reverse nreverse
    remove delete remove-duplicates delete-duplicates union intersection set-difference
    adjoin member find assoc rassoc values identity progn prog1 prog2 when unless and or
    do block the locally return pop alexandria:flatten alexandria:ensure-list subsets-up-to)
  "Name-free operators whose result carries their arguments' taint.")


(defparameter *flow-predicate-operators*
  '(eq eql equal different not null endp consp listp atom symbolp numberp integerp subsetp
    length count position print always-true error
    + - * / = /= < > <= >= min max abs 1+ 1- floor ceiling round truncate mod rem expt
    sqrt zerop plusp minusp)
  "Operators whose result never holds a family member and that never reveal a member's
   name: predicates, ERROR (which ends the search alike in every renamed position), and
   numeric operators, which signal an error on an object rather than read its name.")


(defparameter *flow-index-operators*
  '(nth nthcdr)
  "Name-free list accessors whose first argument is an index that must not be a member.")


(defparameter *flow-mapping-operators*
  '(mapcar mapc mapcan maplist mapcon some every notany notevery remove-if remove-if-not
    delete-if delete-if-not find-if find-if-not position-if count-if member-if)
  "Name-free operators that apply a function argument to list elements.")


(defparameter *flow-engine-state-readers* nil
  "Engine functions allowed to receive the position: content-based snapshots.")


(defparameter *flow-hook-names*
  '(heuristic? prune-state? bounding-function? min-steps-remaining?)
  "Search hooks evaluated on positions during search.")


(defvar *flow-families* nil
  "The families under check.")


(defvar *flow-member-families* nil
  "Maps each family member to the list of its one family.")


(defvar *flow-formal-taints* nil
  "Maps (query-or-update . formal) to the taint reaching that formal from any call.")


(defvar *flow-return-taints* nil
  "Maps a query or update to the taint of the values it returns.")


(defvar *flow-reached* nil
  "Queries and updates reached from the search's code.")


(defvar *flow-changed* nil
  "Set when a reached set or a formal or return taint grows during a pass.")


(defvar *flow-spots* nil
  "Maps a spot key (kind detail owner) to its families, or :ALL.")


(defvar *flow-owner* nil
  "Text naming the definition being walked.")


(defvar *flow-function* nil
  "The query or update whose body is being walked, for RETURN-FROM.")


(defvar *flow-env* nil
  "Maps each variable of the body being walked to its taint, over all its assignments.")


(defvar *flow-env-changed* nil
  "Set when a variable's taint grows during a walk.")


(defvar *flow-lisp-definitions* nil
  "Maps each plain Lisp function defined in the staged problem file to its lambda list
   and body.")


(defvar *flow-state-checks* nil
  "The (function . position) pairs already checked for receiving the position.")


(defvar *flow-callbacks* nil
  "The (plain Lisp function . query or update) pairs where Lisp calls back into spec code.")


(defvar *flow-registered-names* nil
  "An alist of (function name . registry variables) for each function name quoted in the
   problem file's top-level forms outside definitions, such as a provider registration.
   The registry variables are the special variables the registering function's body
   names, or the variable a DEFPARAMETER or DEFVAR defines.  A callback through a variable
   in plain Lisp may reach the names registered into a special variable its function names.")


(defvar *flow-position-taints* nil
  "Caches the taint of each relation argument position.")


(defun check-symmetry-family-flow (families)
  "Run the flow check over FAMILIES and return the report alist (see
   *SYMMETRY-SAFETY-REPORT*).  Passes repeat until no reached set or taint grows; every
   spot recorded on the way stays valid, since taints only grow."
  (let ((*flow-families* families)
        (*flow-member-families* (make-hash-table :test #'eq))
        (*flow-formal-taints* (make-hash-table :test #'equal))
        (*flow-return-taints* (make-hash-table :test #'eq))
        (*flow-reached* (make-hash-table :test #'eq))
        (*flow-spots* (make-hash-table :test #'equal))
        (*flow-state-checks* (make-hash-table :test #'equal))
        (*flow-callbacks* (make-hash-table :test #'equal))
        (*flow-position-taints* (make-hash-table :test #'equal))
        (*flow-lisp-definitions* nil)
        (*flow-registered-names* nil)
        (*flow-changed* t))
    (multiple-value-setq (*flow-lisp-definitions* *flow-registered-names*)
      (read-flow-lisp-definitions))
    (dolist (family families)
      (dolist (object (symmetry-family-objects family))
        (setf (gethash object *flow-member-families*) (list family))))
    (loop while *flow-changed*
          do (setf *flow-changed* nil)
             (dolist (action *actions*)
               (let ((*flow-owner* (format nil "action ~(~A~)" (action.name action)))
                     (*flow-function* nil))
                 (flow-walk-to-fixpoint (list (action.precondition-form action)
                                              (action.effect-form action))
                                        (action.precondition-params action)
                                        nil)))
             (when *goal*
               (let ((*flow-owner* "the goal")
                     (*flow-function* nil))
                 (flow-walk-to-fixpoint (list *goal*) nil nil)))
             (flow-reach-hooks)
             (dolist (name (loop for name being the hash-keys of *flow-reached* collect name))
               (flow-analyze-spec-function name)))
    (maphash (lambda (pair present)
               (declare (ignore present))
               (let ((taint (gethash (cdr pair) *flow-return-taints*)))
                 (when taint
                   (flow-spot :callback pair nil taint))))
             *flow-callbacks*)
    (flow-report families)))


(defun read-flow-lisp-definitions ()
  "Return a table of every plain Lisp function the staged problem file defines, mapping
   its name to (lambda-list . body), and the alist of registered names (see
   *FLOW-REGISTERED-NAMES*).  A later definition replaces an earlier one, as at load."
  (let ((table (make-hash-table :test #'eq))
        (registrations nil)
        (*package* (find-package :ww)))
    (with-open-file (in (instance-problem-file (asdf:system-source-directory :wouldwork)))
      (loop for form = (read in nil in)
            until (eq form in)
            do (cond ((not (consp form)))
                     ((and (member (first form) '(defun define-problem-helper))
                           (symbolp (second form)))
                      (setf (gethash (second form) table) (cddr form)))
                     ((not (or (eq (first form) 'defmacro)
                               (eql 0 (search "DEFINE-" (tech-form-head-name form)))))
                      (push form registrations)))))
    (values table
            (loop for form in registrations
                  for registry = (if (member (first form) '(defparameter defvar))
                                   (list (second form))
                                   (flow-special-variables (rest (gethash (first form) table))))
                  when registry
                    append (loop for symbol in (alexandria:flatten (rest form))
                                 when (and (symbolp symbol)
                                           (or (member symbol *query-names*)
                                               (member symbol *update-names*)
                                               (gethash symbol table)))
                                   collect (cons symbol registry))))))


(defun flow-special-variables (tree)
  "Return the special variables, named with earmuffs, occurring anywhere in TREE."
  (remove-duplicates
    (remove-if-not (lambda (symbol)
                     (and (symbolp symbol)
                          (let ((name (symbol-name symbol)))
                            (and (> (length name) 2)
                                 (char= (char name 0) #\*)
                                 (char= (char name (1- (length name))) #\*)))))
                   (alexandria:flatten tree))))


(defun flow-reach-hooks ()
  "Reach the search hooks: a hook query as spec code, a plain Lisp hook as a function
   receiving the position."
  (dolist (name (append *flow-hook-names*
                        (mapcar #'second *min-steps-remaining-contributors*)))
    (cond ((member name *query-names*)
           (flow-reach name))
          ((gethash name *flow-lisp-definitions*)
           (flow-check-state-passing name 0 "the search hooks")))))


(defun flow-analyze-spec-function (name)
  "Walk query or update NAME's body with its formals' taints, and record its return taint."
  (let* ((*flow-owner* (format nil "~:[update~;query~] ~(~A~)" (member name *query-names*) name))
         (*flow-function* name)
         (result (flow-walk-to-fixpoint
                   (list (get name :raw-body))
                   nil
                   (mapcar (lambda (formal)
                             (cons formal (gethash (cons name formal) *flow-formal-taints*)))
                           (get name :raw-args)))))
    (flow-merge-table *flow-return-taints* name result)))


(defun flow-walk-to-fixpoint (forms parameters bindings)
  "Walk FORMS in a fresh environment until no variable's taint grows, after binding the
   quantifier-style PARAMETERS and the (variable . taint) BINDINGS.  Return the taint of
   the last form.  A variable keeps the union of all its assignments, so walk order is
   irrelevant."
  (let ((*flow-env* (make-hash-table :test #'eq))
        (*flow-env-changed* t)
        (result nil))
    (loop while *flow-env-changed*
          do (setf *flow-env-changed* nil)
             (loop for (variable . taint) in bindings
                   do (flow-env-merge variable taint))
             (when parameters
               (flow-bind-parameters parameters))
             (dolist (form forms)
               (setf result (flow-walk form))))
    result))


(defun flow-walk (form)
  "Walk spec FORM, recording spots, and return its taint."
  (cond ((symbolp form) (flow-walk-symbol form))
        ((atom form) nil)
        ((not (alexandria:proper-list-p form))
         (union (flow-walk (car form)) (flow-walk (cdr form))))
        ((not (symbolp (first form)))
         (flow-walk-forms form))
        (t (case (first form)
             (quote (flow-walk-literal-tree (second form)))
             (function (flow-walk-function-argument form nil nil))
             (declare nil)
             (bind (flow-walk-relation (second form))
                   nil)
             ((exists exist forsome forall forevery doall)
              (flow-bind-parameters (second form))
              (flow-walk (third form))
              nil)
             ((assert next finally) (flow-walk-forms (rest form))
                                    nil)
             ((ww-loop loop) (flow-walk-loop form))
             ((assign setq setf) (flow-walk-assignment form))
             ((mv-assign multiple-value-bind destructuring-bind) (flow-walk-multiple-binding form))
             ((let let*) (flow-walk-let form))
             (dolist (flow-env-merge (first (second form)) (flow-walk (second (second form))))
                     (flow-walk-forms (cddr form)))
             ((push pushnew) (flow-walk-push form))
             (if (flow-walk (second form))
                 (union (flow-walk (third form)) (flow-walk (fourth form))))
             (cond (loop for clause in (rest form)
                         append (flow-walk-forms clause)))
             ((case ecase) (flow-walk (second form))
                           (loop for clause in (cddr form)
                                 append (flow-walk-forms (rest clause))))
             (return-from (flow-walk-return form))
             (lambda (flow-walk-lambda form nil))
             ((funcall apply) (flow-walk-funcall form))
             (t (flow-walk-call form))))))


(defun flow-walk-call (form)
  "Walk a call FORM: a relation, a query or update, or a Lisp operator."
  (let ((head (first form)))
    (if (or (gethash head *relations*) (gethash head *static-relations*))
      (progn (flow-walk-relation form)
             nil)
      (flow-call-taint head (rest form)
                       (flow-walk-arguments (rest form)
                                            (member head *flow-mapping-operators*))))))


(defun flow-call-taint (head argforms taints)
  "Return the taint of calling HEAD on ARGFORMS, whose taints are TAINTS, recording any
   spot the call makes."
  (cond ((or (member head *query-names*) (member head *update-names*))
         (flow-spec-call-taint head
                               (if (and argforms (eq (first argforms) 'state))
                                 (rest taints)
                                 taints)))
        ((or (member head *flow-value-operators*) (member head *flow-mapping-operators*))
         (reduce #'union taints :initial-value nil))
        ((member head *flow-predicate-operators*)
         nil)
        ((member head *flow-index-operators*)
         (when (first taints)
           (flow-spot :lisp-call (list head (first argforms)) *flow-owner* (first taints)))
         (reduce #'union (rest taints) :initial-value nil))
        (t (flow-lisp-call head argforms taints))))


(defun flow-lisp-call (head argforms taints)
  "Record the spots of passing ARGFORMS (with TAINTS) to plain Lisp or a Lisp operator
   HEAD: a member argument is a spot, the position is checked against the boundary rule,
   and a quoted query or update name is a callback.  Return no taint."
  (loop for argform in argforms
        for taint in taints
        for position from 0
        do (when taint
             (flow-spot :lisp-call (list head argform) *flow-owner* taint))
           (when (eq argform 'state)
             (flow-check-state-passing head position *flow-owner*))
           (when (and (consp argform)
                      (member (first argform) '(quote function))
                      (symbolp (second argform)))
             (flow-note-callback head (second argform))))
  nil)


(defun flow-spec-call-taint (name taints)
  "Merge TAINTS into query or update NAME's formals, reach NAME, and return its return taint."
  (loop for formal in (get name :raw-args)
        for taint in taints
        do (flow-merge-table *flow-formal-taints* (cons name formal) taint))
  (flow-reach name)
  (gethash name *flow-return-taints*))


(defun flow-walk-arguments (args mapping-p)
  "Return the taints of ARGS.  A function argument (a lambda, or a named function under a
   mapping operator, when MAPPING-P) is applied to the union of the other arguments' taints,
   and its taint is its result's."
  (let* ((functional (mapcar (lambda (arg)
                               (and (consp arg)
                                    (or (member (first arg) '(lambda function))
                                        (and mapping-p
                                             (eq (first arg) 'quote)
                                             (symbolp (second arg))
                                             (fboundp (second arg))))))
                             args))
         (taints (loop for arg in args
                       for function-p in functional
                       collect (unless function-p (flow-walk arg))))
         (others (reduce #'union taints :initial-value nil)))
    (loop for arg in args
          for function-p in functional
          for taint in taints
          collect (if function-p
                    (flow-walk-function-argument arg others mapping-p)
                    taint))))


(defun flow-walk-function-argument (form taint mapping-p)
  "Apply function form FORM -- (lambda ...), #'name or 'name -- to arguments of TAINT.
   A named function is called only under a mapping operator (MAPPING-P); elsewhere a
   query or update name is a reference reached as such."
  (let ((target (if (eq (first form) 'lambda) form (second form))))
    (cond ((and (consp target) (eq (first target) 'lambda))
           (flow-walk-lambda target taint))
          ((not (symbolp target))
           nil)
          (mapping-p
           (flow-call-taint target
                            (make-list (max 1 (length (get target :raw-args)))
                                       :initial-element :list-element)
                            (make-list (max 1 (length (get target :raw-args)))
                                       :initial-element taint)))
          ((or (member target *query-names*) (member target *update-names*))
           (flow-reach target)
           nil))))


(defun flow-walk-lambda (form taint)
  "Bind lambda FORM's parameters to TAINT, then return the taint of its body."
  (dolist (parameter (second form))
    (unless (member parameter lambda-list-keywords)
      (flow-env-merge (if (consp parameter) (first parameter) parameter) taint)))
  (flow-walk-forms (cddr form)))


(defun flow-walk-funcall (form)
  "FUNCALL or APPLY: a named function is called as such; a computed one is plain Lisp."
  (let* ((function-form (second form))
         (name (when (and (consp function-form)
                          (member (first function-form) '(quote function symbol-function)))
                 (let ((inner (second function-form)))
                   (if (and (consp inner) (eq (first inner) 'quote)) (second inner) inner)))))
    (if (and name (symbolp name))
      (flow-call-taint name (cddr form) (flow-walk-arguments (cddr form) nil))
      (progn (flow-walk function-form)
             (flow-lisp-call (first form) (cddr form) (flow-walk-arguments (cddr form) nil))))))


(defun flow-walk-relation (form)
  "A relation FORM binds each $variable argument to the taint of its argument position."
  (loop for arg in (rest form)
        for index from 0
        do (cond (($varp arg)
                  (flow-env-merge arg (flow-relation-position-taint (first form) index)))
                 ((consp arg)
                  (flow-walk arg)))))


(defun flow-relation-position-taint (relation index)
  "Return the families a value at argument INDEX of RELATION may hold.  A static relation's
   facts are scanned; a dynamic position holds its declared type's members, nothing for a
   number type, and anything for any other type (a list, say)."
  (let ((key (cons relation index)))
    (multiple-value-bind (taint present) (gethash key *flow-position-taints*)
      (if present
        taint
        (setf (gethash key *flow-position-taints*)
              (if (gethash relation *relations*)
                (let ((type (nth index (gethash relation *relations*))))
                  (cond ((member type '(rational real integer fixnum number float ratio))
                         nil)
                        ((or (and (consp type) (eq (first type) 'either))
                             (nth-value 1 (gethash type *types*)))
                         (flow-type-taint type))
                        (t *flow-families*)))
                (flow-tree-families
                  (loop for proposition in (list-database *static-idb*)
                        when (eq (first proposition) relation)
                          collect (nth (1+ index) proposition)))))))))


(defun flow-bind-parameters (parameters)
  "Bind quantifier-style PARAMETERS' variables to the taint of their domains.  A
   dot-product pairs its lists by name order, so one over a family is a spot."
  (let ((parameters (if (member (first parameters) *parameter-headers*)
                      parameters
                      (cons 'standard parameters))))
    (multiple-value-bind (variables types) (dissect-pre-params parameters)
      (let ((taints (mapcar (lambda (type)
                              (cond (($varp type) (gethash type *flow-env*))
                                    ((and (consp type) (member (first type) *query-names*))
                                     (flow-walk-call type))
                                    (t (flow-type-taint type))))
                            (rest types))))
        (when (and (eq (first types) 'dot-product) (some #'identity taints))
          (flow-spot :dot-product nil *flow-owner* (reduce #'union taints :initial-value nil)))
        (if (= (length variables) (length taints))
          (loop for variable in variables
                for taint in taints
                do (flow-env-merge variable taint))
          (dolist (variable variables)
            (flow-env-merge variable (reduce #'union taints :initial-value nil))))))))


(defun flow-walk-loop (form)
  "WW-LOOP or LOOP: a FOR, AS or WITH variable takes its list's or value's taint.  The
   result is the union of the clause forms' taints."
  (let ((items (rest form))
        (result nil))
    (loop for item in items
          for index from 0
          do (cond ((and (member item '(for as with))
                         (member (nth (+ index 2) items) '(in on across =)))
                    (dolist (variable (alexandria:flatten (list (nth (1+ index) items))))
                      (flow-env-merge variable (flow-walk (nth (+ index 3) items)))))
                   ((consp item)
                    (setf result (union result (flow-walk item))))))
    result))


(defun flow-walk-assignment (form)
  "ASSIGN, SETQ or SETF: a variable takes the value's taint; a member stored into any
   other place leaves spec code, so it is a spot."
  (let ((result nil))
    (loop for (place value) on (rest form) by #'cddr
          do (setf result (flow-walk value))
             (if (symbolp place)
               (flow-env-merge place result)
               (progn (flow-walk place)
                      (when result
                        (flow-spot :lisp-call (list (first form) place) *flow-owner* result)))))
    result))


(defun flow-walk-multiple-binding (form)
  "MV-ASSIGN, MULTIPLE-VALUE-BIND or DESTRUCTURING-BIND: every variable takes the value
   form's taint."
  (let ((taint (flow-walk (third form))))
    (dolist (variable (alexandria:flatten (second form)))
      (unless (member variable lambda-list-keywords)
        (flow-env-merge variable taint)))
    (if (eq (first form) 'mv-assign)
      taint
      (flow-walk-forms (cdddr form)))))


(defun flow-walk-let (form)
  "LET or LET*: each variable takes its value's taint."
  (dolist (binding (second form))
    (when (consp binding)
      (flow-env-merge (first binding) (flow-walk (second binding)))))
  (flow-walk-forms (cddr form)))


(defun flow-walk-push (form)
  "PUSH or PUSHNEW: the list variable takes the item's taint; a member pushed onto any
   other place leaves spec code, so it is a spot."
  (let ((taint (flow-walk (second form)))
        (place (third form)))
    (if (symbolp place)
      (flow-env-merge place taint)
      (progn (flow-walk place)
             (when taint
               (flow-spot :lisp-call (list (first form) place) *flow-owner* taint))))
    taint))


(defun flow-walk-return (form)
  "RETURN-FROM: the value is also a return value of the query or update being walked."
  (let ((taint (flow-walk (third form))))
    (when *flow-function*
      (flow-merge-table *flow-return-taints* *flow-function* taint))
    taint))


(defun flow-walk-forms (forms)
  "Walk FORMS and return the union of their taints."
  (loop for form in forms
        append (flow-walk form) into taint
        finally (return (remove-duplicates taint))))


(defun flow-walk-symbol (symbol)
  "A variable's taint, or a literal member name's family, which is a spot."
  (cond ((eq symbol 'state) nil)
        ((nth-value 1 (gethash symbol *flow-env*)) (gethash symbol *flow-env*))
        ((gethash symbol *flow-member-families*)
         (flow-spot :literal symbol *flow-owner* (gethash symbol *flow-member-families*))
         (gethash symbol *flow-member-families*))))


(defun flow-walk-literal-tree (tree)
  "A quoted TREE's taint: the families of any member names it holds, each a spot."
  (let ((taint (flow-tree-families tree)))
    (when taint
      (flow-spot :literal tree *flow-owner* taint))
    taint))


(defun flow-tree-families (tree)
  "Return the families of the members occurring anywhere in TREE."
  (cond ((consp tree) (union (flow-tree-families (car tree)) (flow-tree-families (cdr tree))))
        ((symbolp tree) (gethash tree *flow-member-families*))))


(defun flow-type-taint (type-spec)
  "Return the families with a member among TYPE-SPEC's instances."
  (let ((instances (type-spec-instances type-spec)))
    (unless (eq instances :unknown)
      (flow-tree-families instances))))


(defun flow-check-state-passing (function position via)
  "Check plain Lisp FUNCTION, which receives the position as argument POSITION from VIA,
   against the boundary rule.  A function without a body in the problem file (an engine
   or Common Lisp function) may receive it only when it is an engine snapshot."
  (let ((key (cons function position)))
    (unless (gethash key *flow-state-checks*)
      (setf (gethash key *flow-state-checks*) t)
      (let* ((definition (gethash function *flow-lisp-definitions*))
             (formal (when definition
                       (nth position (flow-lambda-list-variables (first definition))))))
        (cond ((member function *flow-engine-state-readers*))
              (formal
               (flow-walk-lisp-forms (rest definition) (list formal) function))
              (t (flow-spot :position-passed function via :all)))))))


(defun flow-lambda-list-variables (lambda-list)
  "Return LAMBDA-LIST's required and optional variables in order."
  (loop for item in lambda-list
        until (member item '(&rest &body &key &aux))
        unless (eq item '&optional)
          collect (if (consp item) (first item) item)))


(defun flow-walk-lisp-forms (forms state-variables function)
  "Walk plain Lisp FORMS of FUNCTION, where STATE-VARIABLES hold the position."
  (dolist (form forms)
    (flow-walk-lisp form state-variables function)))


(defun flow-walk-lisp (form state-variables function)
  "Walk plain Lisp FORM of FUNCTION.  The position may appear only as an argument passed
   on to a query or update, a callback through SYMBOL-FUNCTION, other plain Lisp that
   follows the same rule, or an engine snapshot; any other appearance reads it."
  (cond ((symbolp form)
         (when (member form state-variables)
           (flow-spot :position (list function nil) nil :all)))
        ((atom form) nil)
        ((not (alexandria:proper-list-p form))
         (flow-walk-lisp (car form) state-variables function)
         (flow-walk-lisp (cdr form) state-variables function))
        ((eq (first form) 'declare) nil)
        ((eq (first form) 'quote)
         (if (symbolp (second form))
           (flow-note-callback function (second form))
           (let ((taint (flow-tree-families (second form))))
             (when taint
               (flow-spot :literal (second form) (format nil "plain Lisp ~(~A~)" function) taint)))))
        ((eq (first form) 'function)
         (if (symbolp (second form))
           (flow-note-callback function (second form))
           (flow-walk-lisp-forms (cddr (second form)) state-variables function)))
        ((member (first form) '(let let*))
         (flow-walk-lisp-let form state-variables function))
        ((member (first form) '(multiple-value-bind destructuring-bind))
         (flow-walk-lisp-forms (cddr form) state-variables function))
        ((member (first form) '(dolist dotimes))
         (flow-walk-lisp-forms (cons (second (second form)) (cddr form)) state-variables function))
        ((eq (first form) 'cond)
         (dolist (clause (rest form))
           (flow-walk-lisp-forms clause state-variables function)))
        ((member (first form) '(case ecase typecase etypecase))
         (flow-walk-lisp (second form) state-variables function)
         (dolist (clause (cddr form))
           (flow-walk-lisp-forms (rest clause) state-variables function)))
        ((eq (first form) 'lambda)
         (flow-walk-lisp-forms (cddr form) state-variables function))
        ((not (symbolp (first form)))
         (flow-walk-lisp-forms form state-variables function))
        ((member (first form) '(funcall apply))
         (flow-walk-lisp-funcall form state-variables function))
        (t (flow-walk-lisp-call (first form) (rest form) state-variables function))))


(defun flow-walk-lisp-let (form state-variables function)
  "A LET variable bound to the position also holds the position."
  (let ((aliases nil))
    (dolist (binding (second form))
      (when (consp binding)
        (if (member (second binding) state-variables)
          (push (first binding) aliases)
          (flow-walk-lisp (second binding) state-variables function))))
    (flow-walk-lisp-forms (cddr form) (append aliases state-variables) function)))


(defun flow-walk-lisp-funcall (form state-variables function)
  "FUNCALL or APPLY in plain Lisp.  Through SYMBOL-FUNCTION, #' or a quoted name it is a
   call of that function; through SYMBOL-FUNCTION of a variable it is a callback whose
   name was quoted elsewhere; any other function value may not receive the position."
  (let* ((function-form (second form))
         (inner (when (consp function-form) (second function-form)))
         (name (cond ((not (consp function-form)) nil)
                     ((member (first function-form) '(quote function)) inner)
                     ((and (eq (first function-form) 'symbol-function)
                           (consp inner)
                           (eq (first inner) 'quote))
                      (second inner)))))
    (cond ((and name (symbolp name))
           (flow-walk-lisp-call name (cddr form) state-variables function))
          ((and (consp function-form) (eq (first function-form) 'symbol-function))
           (flow-walk-lisp (second function-form) state-variables function)
           (let ((registries (flow-special-variables
                               (rest (gethash function *flow-lisp-definitions*)))))
             (loop for (name . registry) in *flow-registered-names*
                   when (intersection registry registries)
                     do (flow-note-callback function name)))
           (flow-walk-lisp-call 'callback (cddr form) state-variables function))
          (t (flow-walk-lisp function-form state-variables function)
             (flow-walk-lisp-call (first form) (cddr form) state-variables function)))))


(defun flow-walk-lisp-call (head args state-variables function)
  "A call of HEAD in plain Lisp FUNCTION.  The position passes freely to a query, an
   update or a callback, to plain Lisp that is then checked in turn, and to an engine
   snapshot; to anything else it is read."
  (when (or (member head *query-names*) (member head *update-names*))
    (flow-note-callback function head))
  (loop for arg in args
        for position from 0
        do (cond ((not (member arg state-variables))
                  (flow-walk-lisp arg state-variables function))
                 ((or (member head *query-names*) (member head *update-names*)
                      (eq head 'callback) (member head *flow-engine-state-readers*)))
                 ((gethash head *flow-lisp-definitions*)
                  (flow-check-state-passing head position (format nil "plain Lisp ~(~A~)" function)))
                 (t (flow-spot :position (list function head) nil :all)))))


(defun flow-note-callback (function name)
  "NAME, quoted or called in plain Lisp FUNCTION: a query or update is a callback into
   spec code, whose results reach FUNCTION; a plain Lisp function is checked as receiving
   the position through its parameter named STATE, if it has one."
  (cond ((or (member name *query-names*) (member name *update-names*))
         (flow-reach name)
         (setf (gethash (cons function name) *flow-callbacks*) t))
        ((gethash name *flow-lisp-definitions*)
         (let ((position (position 'state (flow-lambda-list-variables
                                            (first (gethash name *flow-lisp-definitions*))))))
           (when position
             (flow-check-state-passing name position (format nil "plain Lisp ~(~A~)" function)))))))


(defun flow-reach (name)
  "Mark query or update NAME reached."
  (unless (gethash name *flow-reached*)
    (setf (gethash name *flow-reached*) t
          *flow-changed* t)))


(defun flow-env-merge (variable taint)
  "Add TAINT to VARIABLE's taint in the walk's environment."
  (let ((old (gethash variable *flow-env*)))
    (unless (and (nth-value 1 (gethash variable *flow-env*)) (subsetp taint old))
      (setf (gethash variable *flow-env*) (union old taint)
            *flow-env-changed* t))))


(defun flow-merge-table (table key taint)
  "Add TAINT to KEY's taint in TABLE, a formal or return taint table."
  (let ((old (gethash key table)))
    (unless (subsetp taint old)
      (setf (gethash key table) (union old taint)
            *flow-changed* t))))


(defun flow-spot (kind detail owner taint)
  "Record a spot of KIND with DETAIL in OWNER, affecting TAINT's families or :ALL."
  (let* ((key (list kind detail owner))
         (old (gethash key *flow-spots*)))
    (setf (gethash key *flow-spots*)
          (if (or (eq old :all) (eq taint :all)) :all (union old taint)))))


(defun flow-report (families)
  "Return the report alist: each family's own spot texts, then (:all . texts)."
  (let ((entries nil))
    (maphash (lambda (key taint)
               (push (cons (apply #'flow-spot-text key) taint) entries))
             *flow-spots*)
    (setf entries (sort entries #'string< :key #'car))
    (append (mapcar (lambda (family)
                      (cons family
                            (loop for (text . taint) in entries
                                  when (and (listp taint) (member family taint))
                                    collect text)))
                    families)
            (list (cons :all (loop for (text . taint) in entries
                                   when (eq taint :all)
                                     collect text))))))


(defun flow-spot-text (kind detail owner)
  "Return the text describing one spot."
  (let ((argument (when (and (eq kind :lisp-call) (consp detail))
                    (let ((text (format nil "~(~S~)" (second detail))))
                      (if (> (length text) 60)
                        (concatenate 'string (subseq text 0 57) "...")
                        text)))))
    (ecase kind
      (:lisp-call (format nil "~(~A~) receives ~A, in ~A" (first detail) argument owner))
      (:literal (format nil "literal object name in ~(~S~), in ~A" detail owner))
      (:dot-product (format nil "dot-product quantifier over the family, in ~A" owner))
      (:callback (format nil "plain Lisp ~(~A~) receives cargo from ~(~A~)" (car detail) (cdr detail)))
      (:position (if (second detail)
                   (format nil "plain Lisp ~(~A~) reads the position through ~(~A~)"
                           (first detail) (second detail))
                   (format nil "plain Lisp ~(~A~) reads the position" (first detail))))
      (:position-passed (format nil "~(~A~) receives the position, in ~A" detail owner)))))


;;;; GENERATION-TIME FILTERING ;;;;


(defun filter-symmetric-instantiations (action instantiations state)
  "Filter symmetric action instantiations using exact committed row ordering.
   For graph search with canonical hashing: returns all instantiations
   (closed list handles symmetry via canonical hash equality) -- generation-time
   filtering there checked far more instantiations than it pruned relative to
   the closed-list's own yield, at a real per-node allocation cost.
   For tree/backtracking: uses the committed-ordering approach below."
  (unless *symmetry-pruning*
    (return-from filter-symmetric-instantiations instantiations))
  (when (use-canonical-symmetry-p)
    (return-from filter-symmetric-instantiations instantiations))
  (let ((param-indices (gethash (action.name action) *symmetric-type-parameters*)))
    ;; Fast path: action has no symmetric parameters
    (unless param-indices
      (return-from filter-symmetric-instantiations instantiations))
    (let ((filtered nil)
          (*local-symmetry-state-form*
            (symbolic-idb-form (problem-state.idb state)))
          (*local-symmetry-swap-cache* (make-hash-table :test #'equal)))
      (dolist (inst instantiations)
        (increment-global *symmetry-check-count* 1)
        (if (instantiation-allowed-p inst param-indices state)
            (push inst filtered)
            (increment-global *symmetry-pruning-count* 1)))
      (nreverse filtered))))


;;;; EXACT CURRENT-STATE TRANSPOSITIONS ;;;;


(defun symbolic-idb-form (idb)
  "Return IDB as a recursively comparable sorted proposition list."
  (sort (copy-tree (list-database idb)) #'ww-object<))


(defun renamed-symbolic-form (form mapping)
  "Rename every object occurrence in symbolic state FORM and sort the result."
  (sort (mapcar (lambda (proposition)
                  (rename-symmetry-tree proposition mapping))
                form)
        #'ww-object<))


(defun symmetry-row-swap-preserves-state-p (family row-index1 row-index2 state)
  "Whether directly exchanging two FAMILY rows leaves STATE unchanged."
  (when (= row-index1 row-index2)
    (return-from symmetry-row-swap-preserves-state-p t))
  (let* ((low (min row-index1 row-index2))
         (high (max row-index1 row-index2))
         (cache-key (list family low high))
         (cache *local-symmetry-swap-cache*))
    (when cache
      (multiple-value-bind (answer presentp) (gethash cache-key cache)
        (when presentp
          (return-from symmetry-row-swap-preserves-state-p answer))))
    (let* ((rows (symmetry-family.rows family))
           (mapping
             (make-row-transposition (nth low rows) (nth high rows)))
           (state-form
             (or *local-symmetry-state-form*
                 (symbolic-idb-form (problem-state.idb state))))
           (answer
             (equal state-form (renamed-symbolic-form state-form mapping))))
      (when cache
        (setf (gethash cache-key cache) answer))
      answer)))


(defun objects-equivalent-in-state-p (obj1 obj2 state)
  "Check exact current-state interchangeability of two same-role row members."
  (let ((membership1 (gethash obj1 *object-to-symmetry-membership*))
        (membership2 (gethash obj2 *object-to-symmetry-membership*)))
    (and membership1
         membership2
         (eq (symmetry-membership.family membership1)
             (symmetry-membership.family membership2))
         (= (symmetry-membership.column-index membership1)
            (symmetry-membership.column-index membership2))
         (symmetry-row-swap-preserves-state-p
           (symmetry-membership.family membership1)
           (symmetry-membership.row-index membership1)
           (symmetry-membership.row-index membership2)
           state))))


;;;; GLOBAL SYMMETRY STRATEGY (GRAPH SEARCH) ;;;;
;;; Canonical hashing makes the closed list treat symmetrically-equivalent
;;; states as identical by computing permutation-invariant hash values.


(defun use-canonical-symmetry-p ()
  "Returns T if canonical symmetry hashing should be used.
   Canonical hashing is used for graph search with symmetry pruning enabled."
  (and *symmetry-pruning*
       *symmetry-families*            ; families were detected
       (eql *tree-or-graph* 'graph))) ; graph search only


(defun symmetry-permutations (items)
  "Return every permutation of ITEMS."
  (if (null items)
      (list nil)
      (loop for item in items
            append
              (loop for tail in (symmetry-permutations
                                  (remove item items :count 1 :test #'eq))
                    collect (cons item tail)))))


(defun symmetry-tree-objects (tree)
  "Return the distinct symmetry-family objects occurring anywhere in TREE."
  (cond ((consp tree)
         (union (symmetry-tree-objects (car tree))
                (symmetry-tree-objects (cdr tree))
                :test #'eq))
        ((nth-value 1 (gethash tree *object-to-symmetry-membership*))
         (list tree))))


(defun normalize-row-signature-tree
    (tree family self-row-index family-indices individuals)
  "Replace symmetry objects in TREE with row-invariant family/role markers.
   An object in an individualized row (INDIVIDUALS) keeps its row's order instead."
  (cond
    ((consp tree)
     (cons (normalize-row-signature-tree
             (car tree) family self-row-index family-indices individuals)
           (normalize-row-signature-tree
             (cdr tree) family self-row-index family-indices individuals)))
    (t
     (let ((membership (gethash tree *object-to-symmetry-membership*)))
       (cond
         ((null membership) tree)
         ((and individuals (gethash tree individuals))
          (list :individual
                (gethash tree individuals)
                (symmetry-membership.column-index membership)))
         ((eq family (symmetry-membership.family membership))
          (list (if (= self-row-index
                       (symmetry-membership.row-index membership))
                    :self-row
                    :peer-row)
                (symmetry-membership.column-index membership)))
         (t
          (list :other-family
                (gethash (symmetry-membership.family membership) family-indices)
                (symmetry-membership.column-index membership))))))))


(defun compute-row-state-signature
    (family row row-index annotated-form family-indices individuals)
  "Return an invariant complete-incidence signature for one FAMILY row.
   ANNOTATED-FORM holds (proposition . symmetry-objects) entries."
  (sort
    (loop for (proposition . objects) in annotated-form
          when (some (lambda (object) (member object row :test #'eq)) objects)
            collect (normalize-row-signature-tree
                      proposition family row-index family-indices individuals))
    #'ww-object<))


(defun family-state-signature-cells (family state-form family-indices individuals)
  "Partition FAMILY's non-individualized rows by invariant state signatures and sort the cells."
  (let ((partitions (make-hash-table :test #'equal)))
    (loop for row in (symmetry-family.rows family)
          for row-index from 0
          unless (and individuals (gethash (first row) individuals))
            do (push row (gethash (compute-row-state-signature
                                    family row row-index state-form family-indices
                                    individuals)
                                  partitions)))
    (sort
      (loop for signature being the hash-keys of partitions
              using (hash-value rows)
            collect (cons signature (nreverse rows)))
      #'ww-object<
      :key #'car)))


(defun same-symmetry-row-p (membership1 membership2)
  "Whether two memberships identify the same family row."
  (and (eq (symmetry-membership.family membership1)
           (symmetry-membership.family membership2))
       (= (symmetry-membership.row-index membership1)
          (symmetry-membership.row-index membership2))))


(defun symmetry-tree-row-memberships (tree)
  "Return the distinct symmetry rows referenced anywhere in TREE."
  (remove-duplicates
    (cond ((consp tree)
           (append (symmetry-tree-row-memberships (car tree))
                   (symmetry-tree-row-memberships (cdr tree))))
          (t
           (let ((membership
                   (gethash tree *object-to-symmetry-membership*)))
             (when membership (list membership)))))
    :test #'same-symmetry-row-p))


(defun symmetry-cell-rows-interact-p (rows annotated-form)
  "Whether a CELL row participates in a fact that couples distinct symmetry rows.
   ANNOTATED-FORM holds (proposition . symmetry-objects) entries."
  (some
    (lambda (entry)
      (and (some (lambda (row)
                   (some (lambda (object) (member object row :test #'eq))
                         (cdr entry)))
                 rows)
           (> (length (symmetry-tree-row-memberships (car entry))) 1)))
    annotated-form))


(defun symmetry-cell-row-orderings (cell state-form)
  "Return the row orderings needed for one equal-signature CELL.
   Independent equal-signature rows yield the same canonical form in every order."
  (let ((rows (cdr cell)))
    (if (symmetry-cell-rows-interact-p rows state-form)
        (symmetry-permutations rows)
        (list rows))))


(defun combine-symmetry-cell-orderings (cells state-form)
  "Return canonical row orderings formed from already signature-sorted CELLS."
  (if (null cells)
      (list nil)
      (loop for head-ordering in
              (symmetry-cell-row-orderings (first cells) state-form)
            append
              (loop for tail-ordering in
                      (combine-symmetry-cell-orderings (rest cells) state-form)
                    collect (append head-ordering tail-ordering)))))


(defun family-canonical-row-orderings (family state-form family-indices individuals)
  "Return only the exact row orderings unresolved by invariant state signatures.
   Individualized rows (INDIVIDUALS) lead every ordering, in their own order."
  (let ((individual-rows
          (sort (loop for row in (symmetry-family.rows family)
                      when (and individuals (gethash (first row) individuals))
                        collect row)
                #'< :key (lambda (row) (gethash (first row) individuals)))))
    (mapcar (lambda (ordering)
              (append individual-rows ordering))
            (combine-symmetry-cell-orderings
              (family-state-signature-cells family state-form family-indices individuals)
              state-form))))


(defun install-family-canonical-mapping
    (mapping family family-index ordered-rows)
  "Map ORDERED-ROWS onto canonical row positions while preserving columns."
  (loop for row in ordered-rows
        for row-position from 0
        do (loop for object in row
                 for column-position from 0
                 do (setf (gethash object mapping)
                          (list :ww-symmetry-marker
                                family-index row-position column-position))))
  family)


(defun clear-family-canonical-mapping (mapping family)
  "Remove FAMILY's objects from MAPPING."
  (dolist (object (symmetry-family-objects family))
    (remhash object mapping)))


(defun visit-symmetry-canonical-mappings
    (function families family-index state-form family-indices mapping individuals)
  "Recursive worker for MAP-SYMMETRY-CANONICAL-MAPPINGS."
  (if (null families)
      (funcall function mapping)
      (let ((family (first families)))
        (dolist (ordering
                  (family-canonical-row-orderings
                    family state-form family-indices individuals))
          (install-family-canonical-mapping
            mapping family family-index ordering)
          (visit-symmetry-canonical-mappings
            function (rest families) (1+ family-index)
            state-form family-indices mapping individuals)
          (clear-family-canonical-mapping mapping family)))))


(defun map-symmetry-canonical-mappings (function state-form individuals)
  "Call FUNCTION for every unresolved exact family-row ordering in STATE-FORM.
   INDIVIDUALS, when non-NIL, maps each object of an individualized row to that row's
   order; those rows lead their family's orderings and are never exchanged.
   Below this point the form travels annotated as (proposition . symmetry-objects)
   entries, so row-incidence tests scan each proposition's tree once per build."
  (let ((mapping (make-hash-table :test #'eq))
        (family-indices (make-hash-table :test #'eq))
        (annotated-form (mapcar (lambda (proposition)
                                  (cons proposition
                                        (symmetry-tree-objects proposition)))
                                state-form)))
    (loop for family in *symmetry-families*
          for family-index from 0
          do (setf (gethash family family-indices) family-index))
    (visit-symmetry-canonical-mappings
      function *symmetry-families* 0 annotated-form family-indices mapping individuals)))


(defun build-exact-canonical-idb-form (idb)
  "Return the lexicographically least exact IDB form under all family permutations.
   Second value: the visit order of the winning arrangement (see SYMMETRY-MAPPING-RANKS)."
  (canonical-symmetry-form-and-ranks (symbolic-idb-form idb) nil))


(defun canonical-symmetry-form-and-ranks (state-form objects)
  "Return the least renamed STATE-FORM over all unresolved family-row orderings, and the
   visit order (see SYMMETRY-MAPPING-RANKS) of the arrangement that produced it.  The
   rows of OBJECTS are individualized: they take the leading positions of their families,
   in order.
   Two winning arrangements differ by a symmetry of STATE-FORM, so either order serves."
  (let ((best nil)
        (best-ranks nil)
        (individuals (when objects
                       (symmetry-individual-indices objects))))
    (map-symmetry-canonical-mappings
      (lambda (mapping)
        (let ((candidate (renamed-symbolic-form state-form mapping)))
          (when (or (null best-ranks) (ww-object< candidate best))
            (setf best candidate
                  best-ranks (symmetry-mapping-ranks mapping)))))
      state-form individuals)
    (values best best-ranks)))


(defun symmetry-individual-indices (objects)
  "Map every object in the family rows of OBJECTS to its row's order of first appearance."
  (let ((individuals (make-hash-table :test #'eq))
        (row-order 0))
    (dolist (object objects)
      (let ((membership (gethash object *object-to-symmetry-membership*)))
        (when (and membership (not (gethash object individuals)))
          (dolist (row-object (nth (symmetry-membership.row-index membership)
                                   (symmetry-family.rows
                                     (symmetry-membership.family membership))))
            (setf (gethash row-object individuals) row-order))
          (incf row-order))))
    individuals))


(defun symmetry-mapping-ranks (mapping)
  "Return MAPPING's canonical visit order: a vector whose element 0 is an (object . rank)
   alist, ranking objects by family, then canonical row position, then column, and whose
   element I is the reordered I-th indexed visit domain, filled on first use."
  (let ((ranks nil)
        (order (make-array (1+ *symmetry-visit-domain-count*) :initial-element nil)))
    (maphash (lambda (object marker)
               (destructuring-bind (tag family-index row-position column-position) marker
                 (declare (ignore tag))
                 (push (cons object (+ (* family-index 1000000)
                                       (* row-position 1000)
                                       column-position))
                       ranks)))
             mapping)
    (setf (svref order 0) ranks)
    order))


;;;; NAME-FREE VISIT ORDER ;;;;
;;; Spec loops over a type that holds family members visit those members in canonical
;;; order, never by name, so a "first found" choice picks the same physical object in
;;; every renamed copy of a position.  During an action application the order is fixed
;;; by its SYMMETRY-VISIT-CELL: the parent's canonical order under the global strategy
;;; (every instantiation is still generated, so a tie only renames the set of children),
;;; and under the local strategy the canonical order with the instantiation's own family
;;; objects individualized (filtering drops an instantiation as a renaming of another,
;;; which holds only if the order respects the action's objects).  Outside an action --
;;; the goal and the hooks -- a loop uses the canonical order of the state it evaluates.


(defun symmetry-visit-domain (domain key state)
  "Return DOMAIN with its symmetry-family members in name-free canonical order.
   The members fill the slots family members occupy in DOMAIN; other objects stay put.
   KEY, DOMAIN's type name or DOMAIN itself, finds its index from staging, so a list
   without two family members costs one lookup, and a reordered list is computed once
   per visit order."
  (if *symmetry-visit-active*
      (let ((index (gethash key *symmetry-visit-domain-indices* :unindexed)))
        (cond ((null index) domain)
              ((eq index :unindexed)
               (reorder-symmetry-domain domain (svref (symmetry-visit-order state) 0)))
              (t (let ((order (symmetry-visit-order state)))
                   (or (svref order index)  ;a racing duplicate fill stores the same list
                       (setf (svref order index)
                             (reorder-symmetry-domain domain (svref order 0))))))))
      domain))


(defun reorder-symmetry-domain (domain ranks)
  "Return DOMAIN with the objects ranked in RANKS, an (object . rank) alist, sorted by rank
   within the slots they occupy, or DOMAIN itself when fewer than two are ranked."
  (let ((members (loop for object in domain
                       when (assoc object ranks :test #'eq)
                         collect object)))
    (if (null (rest members))
        domain
        (let ((sorted (sort members #'<
                            :key (lambda (object)
                                   (cdr (assoc object ranks :test #'eq))))))
          (mapcar (lambda (object)
                    (if (assoc object ranks :test #'eq)
                        (pop sorted)
                        object))
                  domain)))))


(defun index-symmetry-visit-domains ()
  "Number every distinct *types* instance list (by contents) holding two or more family
   members, map every other instance list to NIL, and give each type name its list's
   entry, for SYMMETRY-VISIT-DOMAIN."
  (clrhash *symmetry-visit-domain-indices*)
  (setf *symmetry-visit-domain-count* 0)
  (maphash (lambda (type-name objects)
             (when (listp objects)
               (unless (nth-value 1 (gethash objects *symmetry-visit-domain-indices*))
                 (setf (gethash objects *symmetry-visit-domain-indices*)
                       (when (< 1 (count-if (lambda (object)
                                              (gethash object *object-to-symmetry-membership*))
                                            objects))
                         (incf *symmetry-visit-domain-count*))))
               (setf (gethash type-name *symmetry-visit-domain-indices*)
                     (gethash objects *symmetry-visit-domain-indices*))))
           *types*)
  *symmetry-visit-domain-count*)


(defun symmetry-visit-collection (type-inst static-collection state)
  "Return a static quantifier's STATIC-COLLECTION, rebuilt from name-free ordered
   instance lists when TYPE-INST ranges over two or more members of a family."
  (let ((ordered (symmetry-visit-type-inst type-inst state)))
    (if (eq ordered type-inst)
        static-collection
        (ut::transpose (eval-instantiated-spec ordered)))))


(defun symmetry-visit-type-inst (type-inst state)
  "Return instantiated TYPE-INST with each instance list in name-free order, or TYPE-INST
   itself when no list ranges over two or more family members.  A dot-product keeps
   name order, since it pairs its lists by position."
  (if (or (not *symmetry-visit-active*)
          (eql (first type-inst) 'dot-product))
      type-inst
      (let* ((changed nil)
             (ordered
               (mapcar (lambda (item)
                         (let ((new-item
                                 (cond ((atom item) item)
                                       ((member (first item) *query-names*) item)
                                       ((member (first item) *parameter-headers*)
                                        (symmetry-visit-type-inst item state))
                                       (t (symmetry-visit-domain item item state)))))
                           (unless (eq new-item item)
                             (setf changed t))
                           new-item))
                       type-inst)))
        (if changed ordered type-inst))))


(defun symmetry-visit-order (state)
  "Return the visit order (see SYMMETRY-MAPPING-RANKS) governing family loops evaluated
   in STATE.  Under the global strategy an action's cell is simply its parent state."
  (let ((cell *symmetry-visit-cell*))
    (cond ((problem-state-p cell)
           (symmetry-state-ranks cell))
          (cell
           (or (symmetry-visit-cell.ranks cell)
               (setf (symmetry-visit-cell.ranks cell)
                     (nth-value 1 (canonical-symmetry-form-and-ranks
                                    (or (symmetry-visit-cell.form cell)
                                        (symbolic-idb-form
                                          (problem-state.idb (symmetry-visit-cell.state cell))))
                                    (symmetry-visit-cell.objects cell))))))
          ((use-canonical-symmetry-p)
           (symmetry-state-ranks state))
          (t (nth-value 1 (canonical-symmetry-form-and-ranks
                            (symbolic-idb-form (problem-state.idb state)) nil))))))


(defun symmetry-state-ranks (state)
  "Return complete STATE's canonical visit order under the global strategy, cached with
   its canonical form.  STATE is never an effect's working copy: every effect binds a cell."
  (ensure-idb-hash state)
  (or (problem-state.symmetry-ranks state)
      (setf (problem-state.symmetry-ranks state)
            (nth-value 1 (build-canonical-idb-form (problem-state.symmetry-idb state))))))


(defun make-action-symmetry-visit-cell (state instantiation effect-p)
  "Return the visit-order cell for one action application in STATE, or NIL when visit
   order is inactive.  The global strategy needs only the parent order, so its cell is
   STATE itself.  The local strategy individualizes INSTANTIATION's family objects, and
   a backtracking EFFECT-P mutates STATE in place, so its cell snapshots STATE first."
  (cond ((not *symmetry-visit-active*) nil)
        ((use-canonical-symmetry-p) state)
        (t (make-symmetry-visit-cell
             :state state
             :objects (loop for object in instantiation
                            when (gethash object *object-to-symmetry-membership*)
                              collect object)
             :form (when (and effect-p (eql *algorithm* 'backtracking))
                     (symbolic-idb-form (problem-state.idb state)))))))


;;;; STATISTICS AND REPORTING ;;;;


(defun reset-symmetry-statistics ()
  "Reset symmetry pruning statistics for a new search."
  (setf *symmetry-pruning-count* 0)
  (setf *symmetry-check-count* 0)
  (setf *symmetric-duplicates-pruned* 0))


(defun symmetry-pruning-percentage ()
  "Return percentage of instantiations pruned due to symmetry."
  (if (> *symmetry-check-count* 0)
      (* 100.0 (/ *symmetry-pruning-count* *symmetry-check-count*))
      0.0))


(defun format-symmetry-statistics ()
  "Return formatted string of symmetry statistics for progress reporting."
  (cond ((not *symmetry-pruning*) nil)
        ((use-canonical-symmetry-p)
         (if (> *total-states-processed* 0)
             (format nil "Symmetry: ~:D canonical duplicates pruned (~,1F% of total states)"
                     *symmetric-duplicates-pruned*
                     (* 100.0 (/ *symmetric-duplicates-pruned*
                                 *total-states-processed*)))
             (format nil "Symmetry: ~:D canonical duplicates pruned"
                     *symmetric-duplicates-pruned*)))
        (t
         (format nil "Symmetry: Local pruning ~,1F% (~:D/~:D instantiations filtered)"
                 (symmetry-pruning-percentage)
                 *symmetry-pruning-count*
                 *symmetry-check-count*))))


(defun report-symmetry-safety ()
  "Run symmetry detection and the flow check on the staged problem as if
   *SYMMETRY-PRUNING* were on, and print the report.  The problem's own symmetry
   settings and families are left untouched."
  (let ((*symmetry-pruning* t)
        (*symmetry-groups* nil)
        (*symmetry-families* nil)
        (*object-to-symmetry-group* (make-hash-table :test #'eq))
        (*object-to-symmetry-membership* (make-hash-table :test #'eq))
        (*symmetric-type-parameters* (make-hash-table :test #'eq))
        (*symmetry-safety-report* nil))
    (detect-symmetry-groups)
    (if (rest *symmetry-safety-report*)
      (print-symmetry-safety-report)
      (format t "~&No symmetry families detected.~%"))
    (values)))


(defun print-symmetry-safety-report ()
  "Print the flow check's verdict and spots for every detected family (report only)."
  (when (rest *symmetry-safety-report*)
    (format t "~&Symmetry safety check (report only):~%")
    (loop for (family . texts) in *symmetry-safety-report*
          unless (eq family :all)
            do (format t "  ~A: ~A~%"
                       (symmetry-family.rows family)
                       (ecase (symmetry-family-status family)
                         (:unsafe-action "dropped (an action parameter use is unsafe)")
                         (:kept-clean "kept; passes the full check")
                         (:kept-flagged "kept; the full check would drop it")
                         (:query-only-clean
                          "kept (no action parameter ranges over it); passes the full check")
                         (:query-only-flagged
                          "dropped (no action parameter ranges over it); the full check flags it")))
               (dolist (text texts)
                 (format t "      ~A~%" text)))
    (let ((shared (rest (assoc :all *symmetry-safety-report*))))
      (when shared
        (format t "  Spots affecting every family:~%")
        (dolist (text shared)
          (format t "      ~A~%" text))))))


(defun find-group-type (group)
  "Find the most specific type that contains all objects in GROUP.
   Returns the type name with the smallest object count that includes all group members."
  (let ((best-type nil)
        (best-size most-positive-fixnum))
    (maphash (lambda (type-name type-objects)
               (when (and (listp type-objects)
                          (subsetp group type-objects)
                          (< (length type-objects) best-size))
                 (setf best-type type-name
                       best-size (length type-objects))))
             *types*)
    best-type))


(defun format-object-list (objects)
  "Format a list of objects as 'A, B, and C' style string."
  (let ((len (length objects)))
    (case len
      (0 "")
      (1 (format nil "~A" (first objects)))
      (2 (format nil "~A and ~A" (first objects) (second objects)))
      (t (format nil "~{~A~^, ~}, and ~A" 
                 (butlast objects) 
                 (car (last objects)))))))


;;;; INITIALIZATION ;;;;


(defun first-equivalent-uncommitted-row
    (membership committed-rows state)
  "Return the first row equivalent to MEMBERSHIP's row after prior commitments."
  (let* ((family (symmetry-membership.family membership))
         (chosen-row (symmetry-membership.row-index membership)))
    (loop for row in (symmetry-family.rows family)
          for row-index from 0
          when (and (not (member row-index committed-rows :test #'=))
                    (symmetry-row-swap-preserves-state-p
                      family chosen-row row-index state))
            return row-index)))


(defun instantiation-allowed-p (instantiation param-indices state)
  "Returns T if INSTANTIATION should be kept (not pruned).
   Rows are interchangeable only when their complete column-preserving transposition
   leaves the current state unchanged.  Canonical row order is enforced per family."
  (let ((committed (make-hash-table :test #'eq)))
    (loop for idx in param-indices
          for obj = (nth idx instantiation)
          for membership = (gethash obj *object-to-symmetry-membership*)
          always 
          (cond
            ((null membership) t)
            (t
             (let* ((family (symmetry-membership.family membership))
                    (row-index (symmetry-membership.row-index membership))
                    (committed-rows (gethash family committed)))
               (cond
                 ((member row-index committed-rows :test #'=) t)
                 (t
                  (let ((first-equivalent
                          (first-equivalent-uncommitted-row
                            membership committed-rows state)))
                    (push row-index (gethash family committed))
                    (= row-index first-equivalent))))))))))


(defun initialize-symmetry-detection ()
  "Initialize symmetry detection if enabled. Must be called AFTER
   do-init-action-updates, so that exact transpositions see the static facts
   derived by init actions and not merely those authored in define-init."
  (setf *symmetry-visit-active* nil)
  (when *symmetry-pruning*
    (detect-symmetry-groups)
    (print-symmetry-safety-report)
    (when (and (boundp '*start-state*) *start-state*)
      (invalidate-problem-state-hash *start-state*))
    (cond
      (*symmetry-families*
       (format t "~2%Symmetry families detected: ~D~%" (length *symmetry-families*))
       (dolist (family *symmetry-families*)
         (let ((rows (symmetry-family.rows family))
               (objects (symmetry-family-objects family)))
           (format t "  ~A [~D interchangeable row~:P of type ~A]~%"
                   rows
                   (length rows)
                   (find-group-type objects))))
       ;; Explain goal reference impact and strategy
       (let ((goal-objects (extract-goal-object-references)))
         (when goal-objects
           (format t "  Note: Goal-referenced objects (~A) excluded from symmetry groups.~%"
                   (format-object-list goal-objects))
           (format t "        These objects participate in goal conditions, so any symmetries involving them will not be pruned.~%"))
         (format t "  Search will prune equivalent paths that simply swap interchangeable objects.~%")
         ;; Report which strategy will be used
         (if (eql *tree-or-graph* 'graph)
             (format t "  Strategy: Global — symmetric states are detected as duplicates in closed list and pruned.~%")
             (format t "  Strategy: Local — symmetric actions are detected at generation time and pruned.~%")))
       (terpri))
      (t
       (format t "~2%No symmetry families detected. Set *symmetry-pruning* = nil for greater efficiency.~2%"))))
  (index-symmetry-visit-domains)
  (setf *symmetry-visit-active* (and *symmetry-pruning* *symmetry-families* t)))


(defun refresh-symmetry-detection ()
  "Rebuild symmetry after a runtime goal change."
  (if *symmetry-pruning*
      (initialize-symmetry-detection)
      (progn
        (setf *symmetry-groups* nil
              *symmetry-families* nil
              *symmetry-visit-active* nil)
        (clrhash *object-to-symmetry-group*)
        (clrhash *object-to-symmetry-membership*)
        (clrhash *symmetric-type-parameters*)))
  *symmetry-families*)

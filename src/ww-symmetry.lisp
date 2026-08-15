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
  "Keep only operative families whose every transition use is permutation-safe.
   A single identity-sensitive action use invalidates a family for global
   canonicalization, even when another action could use that family safely."
  (setf *symmetry-families*
        (remove-if-not
          (lambda (family)
            (let ((group (symmetry-family-objects family)))
              (and (some (lambda (action)
                           (action-uses-group-soundly-p action group))
                         *actions*)
                   (every (lambda (action)
                            (action-uses-group-entirely-safely-p action group))
                          *actions*))))
          *symmetry-families*))
  (rebuild-symmetry-lookups))


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


(defun symmetry-tree-contains-row-p (tree row)
  "Whether TREE contains any object in ROW."
  (cond ((consp tree)
         (or (symmetry-tree-contains-row-p (car tree) row)
             (symmetry-tree-contains-row-p (cdr tree) row)))
        (t (member tree row :test #'eq))))


(defun normalize-row-signature-tree
    (tree family self-row-index family-indices)
  "Replace symmetry objects in TREE with row-invariant family/role markers."
  (cond
    ((consp tree)
     (cons (normalize-row-signature-tree
             (car tree) family self-row-index family-indices)
           (normalize-row-signature-tree
             (cdr tree) family self-row-index family-indices)))
    (t
     (let ((membership (gethash tree *object-to-symmetry-membership*)))
       (cond
         ((null membership) tree)
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
    (family row row-index state-form family-indices)
  "Return an invariant complete-incidence signature for one FAMILY row."
  (sort
    (loop for proposition in state-form
          when (symmetry-tree-contains-row-p proposition row)
            collect (normalize-row-signature-tree
                      proposition family row-index family-indices))
    #'ww-object<))


(defun family-state-signature-cells (family state-form family-indices)
  "Partition FAMILY rows by invariant state signatures and sort the cells."
  (let ((partitions (make-hash-table :test #'equal)))
    (loop for row in (symmetry-family.rows family)
          for row-index from 0
          for signature = (compute-row-state-signature
                            family row row-index state-form family-indices)
          do (push row (gethash signature partitions)))
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


(defun symmetry-cell-rows-interact-p (rows state-form)
  "Whether a CELL row participates in a fact that couples distinct symmetry rows."
  (some
    (lambda (proposition)
      (and (some (lambda (row)
                   (symmetry-tree-contains-row-p proposition row))
                 rows)
           (> (length (symmetry-tree-row-memberships proposition)) 1)))
    state-form))


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


(defun family-canonical-row-orderings (family state-form family-indices)
  "Return only the exact row orderings unresolved by invariant state signatures."
  (combine-symmetry-cell-orderings
    (family-state-signature-cells family state-form family-indices)
    state-form))


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
    (function families family-index state-form family-indices mapping)
  "Recursive worker for MAP-SYMMETRY-CANONICAL-MAPPINGS."
  (if (null families)
      (funcall function mapping)
      (let ((family (first families)))
        (dolist (ordering
                  (family-canonical-row-orderings
                    family state-form family-indices))
          (install-family-canonical-mapping
            mapping family family-index ordering)
          (visit-symmetry-canonical-mappings
            function (rest families) (1+ family-index)
            state-form family-indices mapping)
          (clear-family-canonical-mapping mapping family)))))


(defun map-symmetry-canonical-mappings (function state-form)
  "Call FUNCTION for every unresolved exact family-row ordering in STATE-FORM."
  (let ((mapping (make-hash-table :test #'eq))
        (family-indices (make-hash-table :test #'eq)))
    (loop for family in *symmetry-families*
          for family-index from 0
          do (setf (gethash family family-indices) family-index))
    (visit-symmetry-canonical-mappings
      function *symmetry-families* 0 state-form family-indices mapping)))


(defun build-exact-canonical-idb-form (idb)
  "Return the lexicographically least exact IDB form under all family permutations."
  (let ((state-form (symbolic-idb-form idb))
        (best nil))
    (map-symmetry-canonical-mappings
      (lambda (mapping)
        (let ((candidate (renamed-symbolic-form state-form mapping)))
          (when (or (null best) (ww-object< candidate best))
            (setf best candidate))))
      state-form)
    best))


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
  (when *symmetry-pruning*
    (detect-symmetry-groups)
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
       (format t "~2%No symmetry families detected. Set *symmetry-pruning* = nil for greater efficiency.~2%")))))


(defun refresh-symmetry-detection ()
  "Rebuild symmetry after a runtime goal change."
  (if *symmetry-pruning*
      (initialize-symmetry-detection)
      (progn
        (setf *symmetry-groups* nil
              *symmetry-families* nil)
        (clrhash *object-to-symmetry-group*)
        (clrhash *object-to-symmetry-membership*)
        (clrhash *symmetric-type-parameters*)))
  *symmetry-families*)

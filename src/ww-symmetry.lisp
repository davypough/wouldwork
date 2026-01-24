;;; Filename: ww-symmetry.lisp

;;; Symmetry detection and pruning for Wouldwork planner.
;;; Identifies groups of interchangeable objects based on type membership,
;;; static relations, initial dynamic state, and goal references.
;;; Provides two pruning strategies:
;;;   - Local (tree/backtracking): generation-time filtering of symmetric instantiations
;;;   - Global (graph): canonical closed-list hashing treats symmetric states as duplicates


(in-package :ww)


;;;; GLOBAL VARIABLES ;;;;


(defparameter *symmetry-groups* nil
  "List of symmetry groups. Each group is a list of interchangeable objects.")


(defparameter *object-to-symmetry-group* (make-hash-table :test #'eq)
  "Maps each object to its symmetry group, or NIL if singleton.")


(defparameter *symmetric-type-parameters* (make-hash-table :test #'eq)
  "Maps action-name to list of parameter indices that have symmetric types.")


(defparameter *initial-object-signatures* (make-hash-table :test #'eq)
  "Maps symmetric objects to their initial dynamic state signatures.
   Used to detect when an object has been distinguished by state changes.")


(define-global *symmetry-pruning-count* 0
  "Count of action instantiations pruned due to symmetry during search.")
(declaim (type fixnum *symmetry-pruning-count*))


(define-global *symmetry-check-count* 0
  "Count of symmetry checks performed during search.")
(declaim (type fixnum *symmetry-check-count*))


(define-global *symmetric-duplicates-pruned* 0
  "Count of states pruned as symmetric duplicates in canonical mode (global strategy).")
(declaim (type fixnum *symmetric-duplicates-pruned*))


(defvar *signature-element-string-cache* nil
  "When non-NIL, a hash-table used to memoize (prin1-to-string elem) results
   during signature sorting and canonical signature comparisons.")


(defvar *intkey-components-cache* nil
  "Global cache: packed int-key -> decoded component list.")


(defvar *symmetry-group-code-set-cache* nil
  "Global cache: group(list identity) -> hash-set of member int-codes.")


(defvar *symmetry-group-symbol-set-cache* nil
  "Global cache: group(list identity) -> hash-set of member object symbols.")


(defparameter +canon-marker-offset+ 1000
  "marker digits start here, to avoid colliding with normal component codes (<1000).")


(defparameter +canon-marker-scale+ 1000
  "marker = offset + group-index*scale + position (scale assumes position < 1000).")


(defparameter +canon-pack-base+ 1000000
  "smaller base to keep packed keys/bignums cheaper while still exceeding any digit.")



(defmacro with-signature-element-string-cache (&body body)
  "Evaluate BODY with a fresh cache for signature-element->string."
  `(let ((*signature-element-string-cache* (make-hash-table :test #'eql)))
     ,@body))


;;;; PRE-SEARCH SYMMETRY DETECTION ;;;;


(defun detect-symmetry-groups ()
  "Main entry point for pre-search symmetry detection.
   Populates *symmetry-groups*, *object-to-symmetry-group*, 
   and *symmetric-type-parameters*."
  ;; Reset data structures
  (setf *symmetry-groups* nil)
  (clrhash *object-to-symmetry-group*)
  (clrhash *symmetric-type-parameters*)
  (clrhash *initial-object-signatures*)
  (reset-symmetry-caches)
  (setf *symmetry-pruning-count* 0)
  (setf *symmetry-check-count* 0)
  ;; Step 1: Identify candidate types (types with multiple instances)
  (let ((candidate-types (identify-candidate-types)))
    (when (null candidate-types)
      (return-from detect-symmetry-groups nil))
    ;; Step 2: Compute signatures for all objects in candidate types
    (let ((signatures (compute-all-signatures candidate-types)))
      ;; Step 3: Group objects by (type, signature)
      (let ((groups (partition-by-signature candidate-types signatures)))
        ;; Step 4: Split groups by goal references
        (setf groups (split-groups-by-goal-references groups))
        ;; Step 5: Build lookup structures
        (setf *symmetry-groups* groups)
        (dolist (group groups)
          (dolist (object group)
            (setf (gethash object *object-to-symmetry-group*) group)))
        ;; Step 6: Identify which action parameters have symmetric types
        (identify-symmetric-action-parameters)
        *symmetry-groups*))))


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


(defun identify-symmetric-action-parameters ()
  "Identify which actions have parameters of symmetric types.
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
            (inst-idx 0))  ; Index into instantiation (excludes headers)
        (dolist (ptype param-types)
          (cond ((member ptype *parameter-headers*)
                 nil)  ; Skip headers - don't increment inst-idx
                (t
                 (when (type-has-symmetric-objects-p ptype symmetric-types)
                   (push inst-idx param-indices))
                 (incf inst-idx))))
        (when param-indices
          (setf (gethash (action.name action) *symmetric-type-parameters*)
                (nreverse param-indices)))))))


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


;;;; GENERATION-TIME FILTERING (LOCAL STRATEGY) ;;;;


(defun ensure-symmetry-caches ()
  "Ensure global symmetry caches are initialized to hash-tables."
  (unless (hash-table-p *intkey-components-cache*)
    (setf *intkey-components-cache* (make-hash-table :test #'eql)))
  (unless (hash-table-p *symmetry-group-code-set-cache*)
    (setf *symmetry-group-code-set-cache* (make-hash-table :test #'eq)))
  (unless (hash-table-p *symmetry-group-symbol-set-cache*)
    (setf *symmetry-group-symbol-set-cache* (make-hash-table :test #'eq)))
  t)


(defun reset-symmetry-caches ()
  "Clear global symmetry caches for a fresh problem/run."
  (ensure-symmetry-caches)
  (clrhash *intkey-components-cache*)
  (clrhash *symmetry-group-code-set-cache*)
  (clrhash *symmetry-group-symbol-set-cache*)
  t)


(defun filter-symmetric-instantiations (action instantiations state)
  "Filter INSTANTIATIONS to remove symmetric equivalents.
   For graph search with canonical hashing: returns all instantiations
   (closed list handles symmetry via canonical hash equality).
   For tree/backtracking: uses committed-ordering approach.
   Returns filtered list of instantiations."
  (unless *symmetry-pruning*
    (return-from filter-symmetric-instantiations instantiations))
  ;; For graph search with canonical hashing, closed list handles symmetry
  (when (use-canonical-symmetry-p)
    (return-from filter-symmetric-instantiations instantiations))
  (let ((param-indices (gethash (action.name action) *symmetric-type-parameters*)))
    ;; Fast path: action has no symmetric parameters
    (unless param-indices
      (return-from filter-symmetric-instantiations instantiations))
    (let ((filtered nil))
      (dolist (inst instantiations)
        (increment-global *symmetry-check-count* 1)
        (if (instantiation-allowed-p inst param-indices state)
            (push inst filtered)
            (increment-global *symmetry-pruning-count* 1)))
      (nreverse filtered))))


;;;; CURRENT-STATE SIGNATURES ;;;;


(defun compute-current-state-signature (object state)
  "Compute signature of OBJECT in current STATE (dynamic relations only).
   OBJECT is a symbol; STATE's idb contains integer-coded propositions.
   The signature is a sorted list of (normalized-proposition value) pairs
   where the object is replaced with a placeholder '_'.
   Two objects with identical signatures occupy equivalent positions in
   the current state."
  (let* ((idb (problem-state.idb state))
         (int-object (gethash object *constant-integers*))
         (normalized-props nil))
    ;; If object has no integer mapping, return empty signature
    (unless int-object
      (return-from compute-current-state-signature nil))
    ;; Collect all propositions involving this object
    (maphash (lambda (int-key value)
               (when (integerp int-key)  ; Keys are packed integers
                 (let ((components (extract-integer-components int-key)))
                   (when (member int-object components :test #'eql)
                     ;; Replace object's integer code with placeholder
                     (let ((norm (substitute '_ int-object components :test #'eql)))
                       (push (list norm value) normalized-props))))))
             idb)
    ;; Sort for consistent comparison
    (sort normalized-props #'signature-element-less-p)))


;; helper for uncached decoding
(defun extract-integer-components/uncached (int-key)
  "Extract component integer codes from a packed integer key.
   Keys are packed as: code0 + code1*1000 + code2*1000000 + ...
   Returns list of component codes in original order (predicate first)."
  (let ((components nil)
        (x int-key))
    (loop while (> x 0)
          do (multiple-value-bind (quotient remainder) (truncate x 1000)
               (push remainder components)
               (setf x quotient)))
    (nreverse components)))


(defun extract-integer-components (int-key)
  "Extract the component integer codes from a packed integer key.
   Keys are packed as: code0 + code1*1000 + code2*1000000 + ...
   Returns list of component codes in original order (predicate first)."
  (let ((components nil)
        (x int-key))
    (loop while (> x 0)
          do (multiple-value-bind (quotient remainder) (truncate x 1000)
               (push remainder components)
               (setf x quotient)))
    (nreverse components)))


(defun canonicalize-proposition-components (components canonical-map original-int-key)
  "Like CANONICALIZE-PROPOSITION-KEY, but takes decoded COMPONENTS.
   Returns ORIGINAL-INT-KEY if nothing maps (avoids consing)."
  (let ((result nil)
        (any-mapped nil))
    (dolist (code components)
      (let ((marker (gethash code canonical-map)))
        (if marker
            (progn (push marker result) (setf any-mapped t))
            (push code result))))
    (if any-mapped
        (nreverse result)
        original-int-key)))


(defun ensure-symmetry-group-sets (group)
  "Return two values: (code-set symbol-set) for GROUP.
   Memoized so we don't rebuild these sets per state."
  (let ((code-set (gethash group *symmetry-group-code-set-cache*))
        (symbol-set (gethash group *symmetry-group-symbol-set-cache*)))
    (unless (and code-set symbol-set)
      (setf code-set (make-hash-table :test #'eql))
      (setf symbol-set (make-hash-table :test #'eq))
      (dolist (obj group)
        (let ((code (gethash obj *constant-integers*)))
          (when code
            (setf (gethash code code-set) t)
            (let ((sym (gethash code *integer-constants*)))
              (when sym
                (setf (gethash sym symbol-set) t))))))
      (setf (gethash group *symmetry-group-code-set-cache*) code-set)
      (setf (gethash group *symmetry-group-symbol-set-cache*) symbol-set))
    (values code-set symbol-set)))


(defun objects-equivalent-in-state-p (obj1 obj2 state)
  "Check if OBJ1 and OBJ2 have equivalent roles in STATE.
   Both must be in the same symmetry group, and all their 
   current relations must be structurally identical."
  (let ((group (gethash obj1 *object-to-symmetry-group*)))
    (unless (and group (member obj2 group))
      (return-from objects-equivalent-in-state-p nil))
    ;; Compare current state signatures
    (equal (compute-current-state-signature obj1 state)
           (compute-current-state-signature obj2 state))))


;;;; GLOBAL SYMMETRY STRATEGY (GRAPH SEARCH) ;;;;
;;; Canonical hashing makes the closed list treat symmetrically-equivalent
;;; states as identical by computing permutation-invariant hash values.


(defun use-canonical-symmetry-p ()
  "Returns T if canonical symmetry hashing should be used.
   Canonical hashing is used for graph search with symmetry pruning enabled."
  (and *symmetry-pruning*
       *symmetry-groups*              ; groups were detected
       (eql *tree-or-graph* 'graph))) ; graph search only


(defun compute-canonical-idb-hash (idb)
  "Compute a hash of IDB invariant under permutation of symmetric objects."
  (declare (type hash-table idb))
  (ww-with-timing :symm/canon-hash
    ;; Predecode integer keys once per state (if you already have this, keep yours)
    (let ((decoded-int-keys (make-hash-table :test #'eql)))
      (maphash (lambda (k v)
                 (declare (ignore v))
                 (when (integerp k)
                   (setf (gethash k decoded-int-keys)
                         (extract-integer-components k))))
               idb)
      (let ((canonical-map (build-canonical-mapping idb decoded-int-keys))
            (hash 0))
        (declare (type fixnum hash))
        (maphash (lambda (key val)
                   (let* ((components (and (integerp key) (gethash key decoded-int-keys)))
                          ;; PH4C: pass COMPONENTS so canonicalize-proposition-key doesn't decode again
                          (canon-key (if (integerp key)
                                         (canonicalize-proposition-key key canonical-map components)  ;; PH4C
                                         key))
                          (canon-val (canonicalize-value val canonical-map)))
                     ;; keep your existing combiner here (whatever you currently use)
                     (setf hash (ldb (byte 62 0)
                                     (+ hash (sxhash (cons canon-key canon-val)))))))
                 idb)
        hash))))


(defun build-canonical-mapping (idb &optional decoded-int-keys)
  "Build a mapping from symmetric object int-codes to canonical markers.
   markers are fixnums in a reserved range:
   marker = +canon-marker-offset+ + group-index*+canon-marker-scale+ + position
   DECODED-INT-KEYS, when supplied, is used downstream (signature-building) to avoid repeated decoding."
  (ww-with-timing :symm/canon-map
    (let ((mapping (make-hash-table :test #'eql))
          (group-index 0))
      (dolist (group *symmetry-groups*)
        (let ((obj-sigs (compute-group-object-signatures group idb decoded-int-keys)))
          (setf obj-sigs (sort obj-sigs #'canonical-signature-less-p :key #'cdr))
          (let ((position 0)
                (prev-sig nil))
            (dolist (obj-sig obj-sigs)
              (let ((obj (car obj-sig))
                    (sig (cdr obj-sig)))
                (when (and prev-sig (not (equal sig prev-sig)))
                  (incf position))
                (setf prev-sig sig)
                (let ((int-code (gethash obj *constant-integers*)))
                  ;; PH4C: integer marker instead of (group-index . position)
                  (setf (gethash int-code mapping)
                        (+ +canon-marker-offset+
                           (* group-index +canon-marker-scale+)
                           position)))))))
        (incf group-index))
      mapping)))


(defun compute-group-object-signatures (group idb &optional decoded-int-keys)
  "PH4A: Compute signatures for all objects in GROUP with a single pass over IDB,
but avoid consing normalized KEY lists by packing them into integers (with terminator).

Returns an alist (obj . signature), where signature is a sorted list of
(packed-normalized-key . normalized-value) pairs."
  (ww-with-timing :symm/group-sigs
    (let* ((group-int-codes (remove nil
                                   (mapcar (lambda (obj)
                                             (gethash obj *constant-integers*))
                                           group)))
           (code-set (make-hash-table :test #'eql))
           (symbol-set (make-hash-table :test #'eq))
           (buckets (make-hash-table :test #'eql)))
      ;; membership + buckets
      (dolist (code group-int-codes)
        (setf (gethash code code-set) t)
        (setf (gethash code buckets) nil)
        (let ((sym (gethash code *integer-constants*)))
          (when sym
            (setf (gethash sym symbol-set) t))))

      ;; single pass over IDB
      (maphash
       (lambda (int-key value)
         (when (integerp int-key)
           (let* ((components (or (and decoded-int-keys (gethash int-key decoded-int-keys))
                                  (extract-integer-components int-key)))
                  (members nil))
             ;; find which group objects appear in this key
             (dolist (code components)
               (when (gethash code code-set)
                 (pushnew code members :test #'eql)))
             (when members
               (dolist (self members)
                 (let* ((self-symbol (gethash self *integer-constants*))
                        ;; PH4A: packed normalized key (no list allocation)
                        (packed-key (pack-normalized-components components self code-set))
                        (norm-val (cond ((eql value self-symbol) 0)
                                        ((and (symbolp value) (gethash value symbol-set)) 1)
                                        (t value))))
                   (push (cons packed-key norm-val)
                         (gethash self buckets))))))))
       idb)

      ;; build (obj . sorted-signature) alist
      (let ((obj-sigs nil))
        (dolist (obj group)
          (let ((code (gethash obj *constant-integers*)))
            (when code
              (push (cons obj
                          (sort (gethash code buckets) #'signature-element-less-p))
                    obj-sigs))))
        (nreverse obj-sigs)))))


(defun compute-idb-object-signature (int-object idb &optional symmetric-codes decoded-int-keys)
  "Compute signature of an object (by int-code) from current IDB state.
   if DECODED-INT-KEYS is supplied, use it to avoid repeated calls to
   EXTRACT-INTEGER-COMPONENTS for each INT-KEY."
  (let ((normalized-props nil)
        (self-symbol (gethash int-object *integer-constants*))
        (symmetric-symbols (when symmetric-codes
                             (remove nil
                                     (mapcar (lambda (code) (gethash code *integer-constants*))
                                             symmetric-codes)))))
    (maphash (lambda (int-key value)
               (when (integerp int-key)
                 (let ((components (or (and decoded-int-keys (gethash int-key decoded-int-keys))
                                       (extract-integer-components int-key))))
                   (when (member int-object components :test #'eql)
                     (let* ((norm-key (mapcar (lambda (code)
                                                (cond ((eql code int-object) 0)
                                                      ((and symmetric-codes
                                                            (member code symmetric-codes :test #'eql))
                                                       1)
                                                      (t code)))
                                              components))
                            (norm-val (cond ((eql value self-symbol) 0)
                                            ((and symmetric-symbols
                                                  (member value symmetric-symbols :test #'eql))
                                             1)
                                            (t value))))
                       (push (cons norm-key norm-val) normalized-props))))))
             idb)
    (sort normalized-props #'signature-element-less-p)))


(defun canonical-signature-less-p (sig1 sig2)
  "Lexicographic comparison of signatures (lists of (key . value) pairs).
   Used to sort objects within a symmetry group by their current state."
  (cond ((null sig1) (not (null sig2)))           ; empty < non-empty
        ((null sig2) nil)                          ; non-empty > empty
        (t (let ((elem1 (car sig1))
                 (elem2 (car sig2)))
             (cond ((signature-element-less-p elem1 elem2) t)
                   ((signature-element-less-p elem2 elem1) nil)
                   (t (canonical-signature-less-p (cdr sig1) (cdr sig2))))))))


(defun canonicalize-proposition-key (int-key canonical-map &optional components)
  "Replace symmetric object codes in INT-KEY with their canonical markers.
  - Returns INT-KEY unchanged when nothing maps.
  - Otherwise returns a packed integer with an arity-preserving terminator.
  - Optional COMPONENTS allows callers to supply decoded digits to avoid extraction."
  (let* ((components (or components (extract-integer-components int-key)))
         (digits nil)
         (any-mapped nil))
    (dolist (code components)
      (let ((marker (gethash code canonical-map)))
        (if marker
            (progn
              (push marker digits)
              (setf any-mapped t))
            (push code digits))))
    (if any-mapped
        (pack-canonical-digits (nreverse digits))
        int-key)))


(defun canonicalize-value (val canonical-map)
  "Canonicalize a value by replacing symmetric object symbols with canonical markers.
   - Symmetric object symbols: symbol → int-code → canonical-marker
   - Non-symmetric object symbols: symbol → int-code
   - Non-object symbols (T, NIL, keywords): pass through unchanged
   - Lists: recurse through elements"
  (cond
    ;; List: recurse through elements
    ((consp val)
     (mapcar (lambda (elem) (canonicalize-value elem canonical-map)) val))
    ;; Symbol: check if it's an object constant
    ((symbolp val)
     (let ((int-code (gethash val *constant-integers*)))
       (if int-code
           ;; Object constant - return marker if symmetric, else int-code
           (gethash int-code canonical-map int-code)
           ;; Not an object constant
           val)))
    ;; Other atoms (numbers, etc.): pass through
    (t val)))


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
         (if (> *repeated-states* 0)
             (format nil "Symmetry: ~:D canonical duplicates pruned (~,1F% of repeated states)"
                     *symmetric-duplicates-pruned*
                     (* 100.0 (/ *symmetric-duplicates-pruned* *repeated-states*)))
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


(defun initialize-initial-signatures ()
  "Record initial dynamic state signatures for all objects in symmetry groups.
   Must be called AFTER integer conversion so signatures match runtime format.
   Uses *start-state* which contains the initial idb."
  (clrhash *initial-object-signatures*)
  (dolist (group *symmetry-groups*)
    (dolist (object group)
      (let ((sig (compute-current-state-signature object *start-state*)))
        (setf (gethash object *initial-object-signatures*) sig)))))


(defun instantiation-allowed-p (instantiation param-indices state)
  "Returns T if INSTANTIATION should be kept (not pruned).
   Implements current-state equivalence: objects are interchangeable only if
   they have identical current state signatures. Among equivalent objects,
   canonical (group) order is enforced across parameters."
  (let ((committed (make-hash-table :test #'eq)))  ; group -> list of committed objects
    (loop for idx in param-indices
          for obj = (nth idx instantiation)
          for group = (gethash obj *object-to-symmetry-group*)
          always 
          (cond
            ;; Not in a symmetry group → allow
            ((null group) t)
            ;; Already committed this exact object → allow (same object in multiple params)
            ((member obj (gethash group committed) :test #'eq) t)
            ;; Check if obj is canonical among currently-equivalent uncommitted objects
            (t 
             (let* ((obj-sig (compute-current-state-signature obj state))
                    (already-committed (gethash group committed))
                    ;; Find first group member with same current signature, not yet committed
                    (first-equivalent
                      (find-if (lambda (member)
                                 (and (not (member member already-committed :test #'eq))
                                      (equal (compute-current-state-signature member state)
                                             obj-sig)))
                               group)))
               ;; Commit this object for future parameters
               (push obj (gethash group committed))
               ;; Object must be first equivalent (or no equivalent exists)
               (eq obj first-equivalent)))))))


(defun initialize-symmetry-detection ()
  "Initialize symmetry detection if enabled. Call after problem loading,
   before integer conversion."
  (when *symmetry-pruning*
    (detect-symmetry-groups)
    (cond
      (*symmetry-groups*
       (format t "~2%Symmetry groups detected: ~D~%" (length *symmetry-groups*))
       (dolist (group *symmetry-groups*)
         (format t "  ~A [~D potentially interchangeable objects of type ~A]~%"
                 group
                 (length group)
                 (find-group-type group)))
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
       (format t "~2%No symmetry groups detected. Set *symmetry-pruning* = nil for greater efficiency.~2%")))))


(defun pack-normalized-components (components self-code code-set)
  "Pack normalized COMPONENTS into an integer with an arity-preserving terminator.
Normalization:
  self-code -> 0
  other group member -> 1
  else unchanged
We append a terminator digit (a 1 in the next base-1000 place) so that
trailing zeros in normalized components do NOT collapse."
  (let ((packed 0)
        (mult 1))
    (dolist (code components)
      (let ((mapped (cond ((eql code self-code) 0)
                          ((and code-set (gethash code code-set)) 1)
                          (t code))))
        (incf packed (* mapped mult))
        (setf mult (* mult 1000))))
    ;; Arity terminator (prevents collisions when trailing normalized zeros occur)
    (+ packed mult)))


(defun pack-canonical-digits (digits)
  "Pack DIGITS into an integer with an arity-preserving terminator.
  We append a terminator digit (a 1 in the next base place) so trailing zeros
  do not collapse (e.g., (... 0) differs from (...))."
  (let ((packed 0)
        (mult 1))
    (dolist (d digits)
      (incf packed (* d mult))
      (setf mult (* mult +canon-pack-base+)))
    (+ packed mult)))  ; terminator

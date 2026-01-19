;;; Filename: ww-symmetry.lisp

;;; Symmetry detection and pruning for Wouldwork planner.
;;; Identifies groups of interchangeable objects based on type membership,
;;; static relations, initial dynamic state, and goal references.
;;; Provides generation-time filtering to prune symmetric action instantiations.


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


(defun signature-element-less-p (elem1 elem2)
  "Comparison function for sorting signature elements."
  (string< (prin1-to-string elem1) (prin1-to-string elem2)))


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


;;;; GENERATION-TIME FILTERING ;;;;


(defun filter-symmetric-instantiations (action instantiations state)
  "Filter INSTANTIATIONS to remove symmetric equivalents.
   Uses committed-ordering approach: among undistinguished symmetric objects,
   only the canonical (first) one is allowed. Once an object is distinguished
   (its dynamic state differs from initial), it's no longer interchangeable.
   Returns filtered list of instantiations."
  (unless *symmetry-pruning*
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


;;;; STATISTICS AND REPORTING ;;;;


(defun reset-symmetry-statistics ()
  "Reset symmetry pruning statistics for a new search."
  (setf *symmetry-pruning-count* 0)
  (setf *symmetry-check-count* 0))


(defun symmetry-pruning-percentage ()
  "Return percentage of instantiations pruned due to symmetry."
  (if (> *symmetry-check-count* 0)
      (* 100.0 (/ *symmetry-pruning-count* *symmetry-check-count*))
      0.0))


(defun format-symmetry-statistics ()
  "Return formatted string of symmetry statistics for progress reporting."
  (if *symmetry-pruning*
      (format nil "Symmetry: ~,1F% pruned (~:D/~:D)"
              (symmetry-pruning-percentage)
              *symmetry-pruning-count*
              *symmetry-check-count*)
      ""))


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


(defun object-distinguished-p (object state)
  "Returns T if OBJECT has been distinguished from its symmetry group.
   An object is distinguished if its current dynamic state signature
   differs from its initial signature. This is domain-independent:
   works for location changes, holding, activation, or any other
   dynamic property change.
   OBJECT is a symbol; STATE contains the current idb."
  (let ((initial-sig (gethash object *initial-object-signatures*)))
    ;; If no initial signature recorded, treat as undistinguished
    (unless initial-sig
      (return-from object-distinguished-p nil))
    (let ((current-sig (compute-current-state-signature object state)))
      (not (equal current-sig initial-sig)))))


(defun canonical-among-undistinguished-p (object state)
  "Returns T if OBJECT is the canonical choice among equivalent
   undistinguished objects in its symmetry group.
   Two objects compete for canonical status only if they have the 
   same current state signature (i.e., they're actually interchangeable).
   Returns T if OBJECT is the first such equivalent undistinguished object."
  (let ((group (gethash object *object-to-symmetry-group*)))
    ;; Not in a symmetry group - allow
    (unless group
      (return-from canonical-among-undistinguished-p t))
    ;; Get this object's current signature
    (let ((obj-sig (compute-current-state-signature object state)))
      ;; Find first undistinguished object with the same signature
      (dolist (member group)
        (unless (object-distinguished-p member state)
          (when (equal (compute-current-state-signature member state) obj-sig)
            ;; First equivalent undistinguished member - is it us?
            (return-from canonical-among-undistinguished-p (eq member object)))))
      ;; No equivalent undistinguished found (shouldn't happen if object itself is undistinguished)
      t)))


(defun instantiation-allowed-p (instantiation param-indices state)
  "Returns T if INSTANTIATION should be kept (not pruned).
   Implements committed ordering: for each symmetry group, undistinguished
   objects must be selected in canonical (group) order across parameters.
   - Distinguished objects are always allowed
   - For undistinguished objects from a group, each must be the first
     undistinguished member not yet committed by an earlier parameter"
  (let ((committed (make-hash-table :test #'eq)))  ; group -> list of committed objects
    (loop for idx in param-indices
          for obj = (nth idx instantiation)
          for group = (gethash obj *object-to-symmetry-group*)
          always 
          (cond
            ;; Not in a symmetry group → allow
            ((null group) t)
            ;; Distinguished → allow  
            ((object-distinguished-p obj state) t)
            ;; Undistinguished → must be first undistinguished among uncommitted
            (t 
             (let* ((already-committed (gethash group committed))
                    ;; Find first undistinguished not yet committed (group is in canonical order)
                    (first-available
                      (find-if (lambda (member)
                                 (and (not (member member already-committed :test #'eq))
                                      (not (object-distinguished-p member state))))
                               group)))
               ;; Commit this object for future parameters
               (push obj (gethash group committed))
               ;; Object must be the first available
               (eq obj first-available)))))))


(defun initialize-symmetry-detection ()
  "Initialize symmetry detection if enabled. Call after problem loading,
   before integer conversion."
  (when *symmetry-pruning*
    (detect-symmetry-groups)
    (cond
      (*symmetry-groups*
       (format t "~2%Symmetry groups detected: ~D~%" (length *symmetry-groups*))
       (dolist (group *symmetry-groups*)
         (format t "  ~A [~D interchangeable objects of type ~A]~%"
                 group
                 (length group)
                 (find-group-type group)))
       ;; Explain goal reference impact
       (let ((goal-objects (extract-goal-object-references)))
         (if goal-objects
             (format t "  Note: Goal-referenced objects (~A) excluded from symmetry groups.~%"
                     (format-object-list goal-objects))
             (format t "  Note: Goal contains no explicit object references.~%"))
         (format t "        Search will avoid exploring equivalent paths that simply swap interchangeable objects.~%"))
       (terpri))
      (t
       (format t "~2%No symmetry groups detected. Set *symmetry-pruning* = nil for greater efficiency.~2%")))))

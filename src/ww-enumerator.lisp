;;; Filename: ww-enumerator.lisp
;;;
;;; CSP-based enumeration support for goal-state generation.
;;;
;;; Provides:
;;;   1) DEFINE-BASE-RELATIONS — problem specs declare the "base" (non-derived)
;;;      relations/types that the enumerator branches on.
;;;   2) ENUMERATE-STATE — REPL entry point that runs a CSP or reachable-state
;;;      search and collects the set of unique goal states reached.
;;;   3) GENERATE-CORNERISH-ENUM-ACTIONS — data-driven generator that creates a
;;;      CSP enum-action sequence from the declared base relations, with
;;;      special-case support for HOLDS, PAIRED (subset enumeration with
;;;      symmetry breaking and global degree caps), and ELEVATION (defaulting).


(in-package :ww)


;;;; ----------------------------------------------------------------------
;;;; Parameters
;;;; ----------------------------------------------------------------------

(defvar *enumerator-base-relations* nil
  "Holds the most recently installed base-relation schema (a list of symbols).
   Intended to be set by DEFINE-BASE-RELATIONS within a problem specification.")


(defvar *enumerator-actions* nil
  "Most recently generated enum-actions (ACTION structs) for CSP enumeration.")


(defvar *enumerator-action-settings* nil
  "Most recently used keyword settings for CSP action generation (for reporting).")


(defvar *enumerated-unique-solutions* nil
  "After ENUMERATE-STATE, holds the list of unique SOLUTION structs (unique goal states).")


(defvar *enumerated-goal-states* nil
  "After ENUMERATE-STATE, holds the list of PROBLEM-STATE objects (unique goal states).")


;;;; ----------------------------------------------------------------------
;;;; Macros
;;;; ----------------------------------------------------------------------

(defmacro define-base-relations (schema)
  "Problem-spec macro: (define-base-relations ( ...symbols... ))"
  `(install-base-relations ',schema))

;;;; ----------------------------------------------------------------------
;;;; Top-level entry points
;;;; ----------------------------------------------------------------------


(defun find-goal-states (goal-spec &key (algorithm *algorithm*)
                                   (solution-type 'first)
                                   exclude-relations
                                   include-relations)
  "User-facing goal-state enumeration.

GOAL-SPEC can be:
  - a goal form, including quantifiers like (forall ...), (and ...), etc.
  - a partial goal-state shorthand: ((p a) (q b)) meaning (and (p a) (q b))

Keywords:
  :SOLUTION-TYPE  FIRST | EVERY | <integer N>    ;; UI restriction
  :ALGORITHM      enumeration/search algorithm (e.g., DEPTH-FIRST)
  :EXCLUDE-RELATIONS  list (or single symbol) to remove from base schema for this run
  :INCLUDE-RELATIONS  list (or single symbol) to add to base schema for this run

Returns:
  a report plist containing :GOAL-STATES and related fields."
  (labels ((ensure-list (x)
             (cond ((null x) nil)
                   ((listp x) x)
                   (t (list x))))
           ;; CHANGED: UI-normalize solution-type; message and default instead of ERROR.
           (normalize-solution-type (x)
             (cond
               ((eq x 'first) 'first)
               ((eq x 'every) 'every)
               ((integerp x)
                (if (plusp x)
                    x
                    (progn
                      (format t "~&[find-goal-states] SOLUTION-TYPE integer must be > 0; got ~S. Using FIRST.~%"
                              x)
                      'first)))
               (t
                (format t "~&[find-goal-states] SOLUTION-TYPE must be FIRST, EVERY, or an integer N; got ~S. Using FIRST.~%"
                        x)
                'first)))
           (compute-run-base-relations ()
             (let ((base (copy-list (get-base-relations))))
               (dolist (r (ensure-list exclude-relations))
                 (setf base (remove r base :test #'eq)))
               (dolist (r (ensure-list include-relations))
                 (unless (member r base :test #'eq)
                   (setf base (append base (list r)))))
               base)))
    (unless (get-base-relations)
      (error "FIND-GOAL-STATES: no base relations declared.  ~
              Use (define-base-relations (...)) in the problem specification."))

    (multiple-value-bind (norm-goal-spec goal-form)
        (enum-normalize-goal-spec goal-spec)
      (let* ((requested-solution-type solution-type)
             (normalized-solution-type (normalize-solution-type requested-solution-type)) ;; CHANGED
             (run-base-relations (compute-run-base-relations))
             ;; Save + temporarily override the installed base schema for this run.
             (problem (and (boundp '*problem-name*) *problem-name*))
             (saved-problem-schema (when problem (get problem :enumerator-base-relations)))
             (saved-global-schema *enumerator-base-relations*)
             (states nil))
        (unwind-protect
            (progn
              ;; Override schema for this run.
              (setf *enumerator-base-relations* run-base-relations)
              (when problem
                (setf (get problem :enumerator-base-relations) run-base-relations))

              ;; Run enumeration (ENUMERATE-STATE-CSP accepts broader solver modes,
              ;; but FIND-GOAL-STATES UI restricts to FIRST/EVERY/N).
              (setf states (enumerate-state norm-goal-spec
                                            :algorithm algorithm
                                            :solution-type normalized-solution-type)) ;; CHANGED

              ;; Build report (always).
              (fgs-build-report goal-form
                                ;; CHANGED: include both requested + normalized in settings for transparency.
                                (list :algorithm algorithm
                                      :solution-type normalized-solution-type
                                      :solution-type-requested requested-solution-type)
                                run-base-relations
                                states
                                :actions *enumerator-actions*
                                :unique-solutions *enumerated-unique-solutions*))
          ;; Restore schemas.
          (setf *enumerator-base-relations* saved-global-schema)
          (when problem
            (setf (get problem :enumerator-base-relations) saved-problem-schema)))))))


(defun enumerate-state (goal-spec &key (algorithm *algorithm*)
                                      (solution-type 'first)
                                      (max-pairs-per-connector 4))
  "Enumerate compatible goal states via CSP-based variable assignment.

GOAL-SPEC may be a goal form (including quantifiers) or a partial goal-state
shorthand ((p ...) (q ...)).

SOLUTION-TYPE may be any existing solver mode (FIRST, EVERY, MIN-LENGTH, etc.).
If SOLUTION-TYPE is a positive integer N, return N goal states (truncated)."
  (unless (get-base-relations)
    (error "ENUMERATE-STATE: no base relations declared.  ~
            Use (define-base-relations (...)) in the problem specification."))

  ;; CHANGED: only validate integer N; otherwise accept existing solution-type modes.
  (when (integerp solution-type)
    (unless (plusp solution-type)
      (error "ENUMERATE-STATE: integer SOLUTION-TYPE must be a positive N; got ~S"
             solution-type)))

  (enumerate-state-csp goal-spec
                       :algorithm algorithm
                       :solution-type solution-type
                       :max-pairs-per-connector max-pairs-per-connector))


(defun enumerate-state-csp (goal-spec &key (algorithm *algorithm*)
                                       (solution-type 'first)
                                       (max-pairs-per-connector 4))
  "Enumerate compatible states via a CSP-style, generated enum-action sequence.

SOLUTION-TYPE:
  - any existing solver mode (FIRST, EVERY, MIN-LENGTH, etc.)
  - or a positive integer N meaning: return N goal states (truncated)."
  (multiple-value-bind (norm-goal-spec goal-form)
      (enum-normalize-goal-spec goal-spec)
    (let* ((requested-solution-type solution-type)
           ;; CHANGED: integer N => run solver in EVERY mode and truncate after.
           (solver-solution-type (if (integerp requested-solution-type)
                                     'every
                                     requested-solution-type))
           (gf (coerce-goal norm-goal-spec))
           (enum-actions (generate-cornerish-enum-actions
                          :goal-form goal-form
                          :max-pairs-per-connector max-pairs-per-connector))
           (leaf-goal (lambda (state)
                        (and (eql (problem-state.name state) 'enum-finalize)
                             (funcall gf state))))

           ;; Save globals to restore.
           (saved-actions *actions*)
           (saved-init-actions *init-actions*)
           (saved-start-state *start-state*)
           (saved-problem-type *problem-type*)
           (saved-solution-type *solution-type*)
           (saved-depth-cutoff *depth-cutoff*)
           (saved-tree-or-graph *tree-or-graph*)
           (saved-algorithm *algorithm*)
           (saved-goal *goal*)
           (saved-goal-form (get 'goal-fn :form))
           (saved-goal-fn-expr (when (boundp 'goal-fn) (symbol-value 'goal-fn)))
           (saved-goal-fn-def (when (fboundp 'goal-fn) (symbol-function 'goal-fn))))

      ;; Validate integer N here too (defensive).
      (when (integerp requested-solution-type)
        (unless (plusp requested-solution-type)
          (error "ENUMERATE-STATE-CSP: integer SOLUTION-TYPE must be a positive N; got ~S"
                 requested-solution-type)))

      (unwind-protect
          (progn
            (setf (symbol-function 'goal-fn) leaf-goal)

            (setf *enumerator-actions* enum-actions)
            (setf *enumerator-action-settings*
                  (list :algorithm algorithm
                        :solution-type requested-solution-type
                        :max-pairs-per-connector max-pairs-per-connector))
            (enum-actions-summary enum-actions)

            (setf *actions* enum-actions
                  *init-actions* nil
                  *problem-type* 'csp
                  *solution-type* solver-solution-type
                  *tree-or-graph* 'tree
                  *algorithm* algorithm
                  *depth-cutoff* (length enum-actions))

            (setf *start-state*
                  (make-problem-state
                   :name 'start :time 0 :value 0
                   :idb (make-hash-table :test 'eql)
                   :idb-hash nil))

            (setf *solutions* nil
                  *unique-solutions* nil)

            (ww-solve)

            ;; Truncate if user requested N (search continues for now, per your decision).
            (let* ((uniq *unique-solutions*)
                   (uniq2 (if (integerp requested-solution-type)
                              (last uniq requested-solution-type)
                              uniq))
                   (states (mapcar #'solution.goal uniq2)))
              (setf *enumerated-unique-solutions* uniq2
                    *enumerated-goal-states* states)
              states))

        ;; Restore globals.
        (setf *actions* saved-actions
              *init-actions* saved-init-actions
              *start-state* saved-start-state
              *problem-type* saved-problem-type
              *solution-type* saved-solution-type
              *depth-cutoff* saved-depth-cutoff
              *tree-or-graph* saved-tree-or-graph
              *algorithm* saved-algorithm
              *goal* saved-goal)
        (when saved-goal-form
          (setf (get 'goal-fn :form) saved-goal-form))
        (when saved-goal-fn-expr
          (setf (symbol-value 'goal-fn) saved-goal-fn-expr))
        (when saved-goal-fn-def
          (setf (symbol-function 'goal-fn) saved-goal-fn-def))))))


(defun enum-normalize-goal-spec (goal-spec)
  "Normalize GOAL-SPEC for enumeration.

Returns two values:
  1) a normalized goal-spec suitable for COERCE-GOAL
  2) a goal-form (list) suitable for goal-driven pruning (or NIL if unavailable)

If GOAL-SPEC is a \"partial goal state\" written as a list of literals like:
  ((p a) (q b c))
it is normalized to:
  (and (p a) (q b c))

If GOAL-SPEC is NIL, normalize to (and t) (no constraints).

Quantified / connective goal forms (e.g., (forall ...) (and ...) (or ...))
are passed through unchanged."
  (labels ((literal-list-p (x)
             (and (listp x)
                  (every (lambda (e)
                           (and (consp e)
                                (symbolp (car e))
                                (not (member (car e)
                                             '(and or not forall exists implies when if)
                                             :test #'eq))))
                         x))))
    (cond
      ;; Function goal or named function: keep as-is, but no reliable FORM for pruning.
      ((or (functionp goal-spec)
           (and (symbolp goal-spec) (fboundp goal-spec)))
       (values goal-spec nil))

      ;; NIL => unconstrained goal
      ((null goal-spec)
       (values (list 'and t) (list 'and t)))

      ;; Partial-goal-state shorthand: list of literals => (and ...)
      ((literal-list-p goal-spec)
       (let ((form (if (null goal-spec) (list 'and t) (cons 'and goal-spec))))
         (values form form)))

      ;; Regular goal form (including quantifiers): pass through
      ((consp goal-spec)
       (values goal-spec goal-spec))

      (t
       (error "ENUM-NORMALIZE-GOAL-SPEC: unsupported goal spec: ~S" goal-spec)))))


(defun fgs-build-report (goal-form settings base-relations goal-states
                         &key (actions *enumerator-actions*)
                              (unique-solutions *enumerated-unique-solutions*))
  "Build and return a FIND-GOAL-STATES report plist.

Fields include:
  :GOAL                normalized goal form used for installation/translation
  :SETTINGS            plist of user-visible settings (:solution-type, :algorithm, ...)
  :BASE-RELATIONS       base schema used for this run
  :ACTIONS             enum actions generated (for debugging/reporting)
  :GOAL-STATES          list of PROBLEM-STATE objects (consumed by ww-backwards via :goal-states)
  :GOAL-STATE-PROPS     propositional databases for each goal state
  :GOAL-BASE-PROPS      base-only propositional databases (filtered by BASE-RELATIONS)
  :SUMMARY              quick counts"
  (labels ((base-props-only (props)
             (remove-if-not (lambda (p)
                              (and (consp p)
                                   (member (car p) base-relations :test #'eq)))
                            props)))
    (let* ((goal-state-props
             (mapcar (lambda (st) (list-database (problem-state.idb st)))
                     goal-states))
           (goal-base-props
             (mapcar #'base-props-only goal-state-props))
           (summary (list :goals 1
                          :goal-states (length goal-states)
                          :unique-solutions (if unique-solutions
                                                (length unique-solutions)
                                                0)
                          :actions (if actions (length actions) 0))))
      (list
       :goal goal-form
       :settings settings
       :base-relations base-relations
       :actions actions
       :unique-solutions unique-solutions
       :goal-states goal-states
       :goal-state-props goal-state-props
       :goal-base-props goal-base-props
       :summary summary))))


(defun enumerated-goal-databases ()
  "Convenience: return a list of propositional databases for enumerated goal states."
  (mapcar (lambda (st) (list-database (problem-state.idb st)))
          *enumerated-goal-states*))


(defun enum-actions-summary (&optional (actions *enumerator-actions*))
  "Print a compact summary of ACTIONS (defaults to the last generated enum-actions)."
  (format t "~&~%;;;; ENUM-ACTIONS-SUMMARY ;;;;~%")
  ;; CHANGED: print the effective base schema for this run (supports temporary overrides).
  (format t "~&Base schema: ~S~%"
          (or (and (boundp '*enumerator-base-relations*)
                   *enumerator-base-relations*)
              (get-base-relations)))
  (when *enumerator-action-settings*
    (format t "~&Settings: ~S~%" *enumerator-action-settings*))
  (format t "~&#actions: ~D~%" (length actions))
  (dolist (a actions)
    (let* ((args (action.precondition-args a))
           (n (if (listp args) (length args) 0)))
      (format t "~&  ~A  (~D instantiations)" (action.name a) n)
      (when (and (listp args) (plusp n))
        (format t "  e.g. ~S" (first args)))
      (terpri)))
  actions)


;;;; ----------------------------------------------------------------------
;;;; Base-relation schema management
;;;; ----------------------------------------------------------------------

(defun install-base-relations (schema)
  "Install SCHEMA (a list of symbols) as the enumerator's base-relation schema.

SCHEMA is intentionally permissive (you may mix type names and predicate names).
The enumerator interprets this schema to generate CSP enum-actions."
  (check-type schema list)
  (format t "~&Installing base relations for enumerator...~%")
  (setf *enumerator-base-relations* schema)
  ;; Also store it under the current problem name when available.
  (when (and (boundp '*problem-name*) *problem-name*)
    (setf (get *problem-name* :enumerator-base-relations) schema))
  schema)


(defun get-base-relations (&optional (problem *problem-name*))
  "Retrieve the enumerator base-relation schema for PROBLEM (or current problem)."
  (or (get problem :enumerator-base-relations)
      *enumerator-base-relations*))


;;;; ----------------------------------------------------------------------
;;;; Goal-spec handling
;;;; ----------------------------------------------------------------------

(defun install-goal-from-form (goal-form)
  "Install GOAL-FORM as the current goal, compile GOAL-FN, and return #'GOAL-FN."
  ;; Uses Wouldwork's normal goal installation (translate -> goal-fn lambda-expr).
  (install-goal goal-form)
  ;; Compile with integer-substitution so GOAL-FN runs on integer databases.
  (compile 'goal-fn (subst-int-code (symbol-value 'goal-fn)))
  (symbol-function 'goal-fn))


(defun coerce-goal (goal-spec)
  "Return a goal function suitable for WW-SOLVE.

GOAL-SPEC may be:
  - a function object
  - a symbol naming a function (e.g., GOAL-FN)
  - a goal form (a list such as (and ...), (forall ...), ...)
  - a partial goal-state shorthand: ((p a) (q b c)) => (and (p a) (q b c))  ;; CHANGED
  - NIL => (and t)                                                        ;; CHANGED"
  (cond
    ;; A lambda/function object
    ((functionp goal-spec) goal-spec)

    ;; A function name
    ((and (symbolp goal-spec) (fboundp goal-spec))
     (symbol-function goal-spec))

    ;; CHANGED: Normalize list/NIL goal specs (supports partial goal-state shorthand).
    ((or (consp goal-spec) (null goal-spec))
     (multiple-value-bind (norm-goal-spec goal-form)
         (enum-normalize-goal-spec goal-spec)   ;; CHANGED (new helper you added)
       (declare (ignore goal-form))
       (cond
         ((functionp norm-goal-spec) norm-goal-spec)
         ((and (symbolp norm-goal-spec) (fboundp norm-goal-spec))
          (symbol-function norm-goal-spec))
         ((consp norm-goal-spec)
          (install-goal-from-form norm-goal-spec))
         (t
          (error "Unsupported normalized goal spec: ~S" norm-goal-spec)))))

    (t
     (error "Unsupported goal spec: ~S" goal-spec))))


;;;; ----------------------------------------------------------------------
;;;; State query helpers
;;;; ----------------------------------------------------------------------

(defun type-instances (type)
  "Return the installed instances for TYPE, or signal an error if unknown."
  (or (gethash type *types*)
      (error "Unknown type ~S (no instances installed)." type)))


(defun fluent-value (state fluentless-prop)
  "Return (values value present-p) for FLUENTLESS-PROP in STATE's idb."
  (let ((key (convert-to-integer fluentless-prop)))
    (gethash key (problem-state.idb state))))


(defun fluent-bound-p (state fluentless-prop)
  "True iff FLUENTLESS-PROP has a value bound in STATE's idb."
  (multiple-value-bind (v present-p) (fluent-value state fluentless-prop)
    (declare (ignore v))
    present-p))


(defun held-by-p (state agent cargo)
  "True iff (holds AGENT CARGO) is currently true in STATE."
  (multiple-value-bind (vals present-p)
      (fluent-value state (list 'holds agent))
    (and present-p (equal vals (list cargo)))))


(defun copy-state (state)
  "Return a fresh copy of STATE (essential for depth-first child idb storage)."
  (copy-problem-state state))


(defun make-update-from (state instantiations)
  "Wrap STATE's idb and INSTANTIATIONS into a single-element update list."
  (list (make-update :changes (problem-state.idb state)
                     :value 0.0
                     :instantiations instantiations
                     :followups nil
                     :sim-state nil)))


;;;; ----------------------------------------------------------------------
;;;; Schema + relation metadata helpers
;;;; ----------------------------------------------------------------------

(defun installed-type-p (sym)
  "True iff SYM names a type with instances in *TYPES*."
  (and (symbolp sym) (gethash sym *types*)))


(defun installed-relation-p (sym)
  "True iff SYM names a relation in *RELATIONS*."
  (and (symbolp sym) (gethash sym *relations*)))


(defun schema-types (schema)
  "Return the subset of SCHEMA symbols that name installed types."
  (remove-if-not #'installed-type-p schema))


(defun schema-relations (schema)
  "Return the subset of SCHEMA symbols that name installed relations."
  (remove-if-not #'installed-relation-p schema))


(defun schema-focus-instances (schema)
  "Return a stable, duplicate-free list of object instances mentioned by SCHEMA's types.

Ordering follows the type order in SCHEMA, and instance order within each type."
  (let ((seen (make-hash-table :test 'eq))
        (out nil))
    (dolist (ty (schema-types schema) (nreverse out))
      (dolist (x (gethash ty *types*))
        (unless (gethash x seen)
          (setf (gethash x seen) t)
          (push x out))))))


(defun maybe-type-instances (type)
  "Like TYPE-INSTANCES but returns NIL when TYPE is not installed."
  (and (symbolp type) (gethash type *types*)))


(defun expand-type-spec-instances* (type-spec)
  "Return the list of instances denoted by TYPE-SPEC (a type symbol or (EITHER ...)).

Unlike TYPE-INSTANCES, unknown type symbols yield NIL rather than an error.
This is important for domains that use non-enumerable types like RATIONAL or LIST."
  (cond
    ((null type-spec) nil)
    ((symbolp type-spec) (maybe-type-instances type-spec))
    ((and (consp type-spec) (eql (car type-spec) 'either))
     (remove-duplicates
      (mapcan #'maybe-type-instances (cdr type-spec))
      :test #'eq))
    (t nil)))


(defun relation-signature (rel)
  "Return REL's signature from *RELATIONS* (or NIL if not a list signature)."
  (let ((sig (gethash rel *relations*)))
    (and (listp sig) sig)))


(defun relation-fluent-indices (rel)
  "Return the fluent position indices for REL from *FLUENT-RELATION-INDICES*."
  (gethash rel *fluent-relation-indices*))


(defun relation-key-type-specs (rel)
  "Return the type specs (from signature) for REL's non-fluent positions."
  (let* ((sig (relation-signature rel))
         (fpos (relation-fluent-indices rel)))
    (when sig
      (loop for spec in sig
            for pos from 1
            unless (member pos fpos) collect spec))))


(defun relation-fluent-type-specs (rel)
  "Return the type specs (from signature) for REL's fluent positions."
  (let* ((sig (relation-signature rel))
         (fpos (relation-fluent-indices rel)))
    (when sig
      (loop for spec in sig
            for pos from 1
            when (member pos fpos) collect spec))))


(defun assemble-proposition (rel key-args fluent-args)
  "Assemble a full proposition for REL from KEY-ARGS and FLUENT-ARGS.

KEY-ARGS are in the order of REL's non-fluent positions (signature order with
fluent positions removed). FLUENT-ARGS are in the order of REL's fluent indices."
  (let* ((sig (relation-signature rel))
         (fpos (relation-fluent-indices rel))
         (k key-args)
         (v fluent-args))
    (unless sig
      (error "Cannot assemble proposition: no list signature for ~S" rel))
    (cons rel
          (loop for spec in sig
                for pos from 1
                collect (if (member pos fpos)
                            (prog1 (car v) (setf v (cdr v)))
                            (prog1 (car k) (setf k (cdr k))))))))


;;;; ----------------------------------------------------------------------
;;;; Goal analysis
;;;; ----------------------------------------------------------------------

(defun goal-contains-or-walk (f)
  "Recursive walker for GOAL-CONTAINS-OR-P.  Descends F skipping quoted lists."
  (cond
    ((atom f) nil)
    ((and (consp f) (eql (car f) 'quote)) nil)
    ((and (consp f) (eql (car f) 'or)) t)
    (t (or (goal-contains-or-walk (car f))
           (goal-contains-or-walk (cdr f))))))


(defun goal-contains-or-p (form)
  "True iff FORM contains an OR connective (outside of quoted lists)."
  (and (consp form) (goal-contains-or-walk form)))


(defun ground-term-p (x)
  "A conservative groundness test for goal literals."
  (or (numberp x)
      (stringp x)
      (characterp x)
      (and (symbolp x)
           (not (?varp x))
           (not ($varp x)))))


(defun collect-positive-ground-literals-walk (f)
  "Recursive walker for COLLECT-POSITIVE-GROUND-LITERALS.  Collects ground positive literals."
  (cond
    ((atom f) nil)
    ((and (consp f) (eql (car f) 'quote)) nil)
    ((and (consp f) (eql (car f) 'not)) nil)
    ((and (consp f) (member (car f) '(and or)))
     (mapcan #'collect-positive-ground-literals-walk (cdr f)))
    ((and (consp f) (symbolp (car f)))
     (let ((args (cdr f)))
       (if (every #'ground-term-p args)
           (list (copy-list f))
           ;; Still descend: the form might be a complex expression.
           (mapcan #'collect-positive-ground-literals-walk (cdr f)))))
    (t (nconc (collect-positive-ground-literals-walk (car f))
              (collect-positive-ground-literals-walk (cdr f))))))


(defun collect-positive-ground-literals (form)
  "Return a list of positive ground literals appearing in FORM.

This is conservative: it ignores negated literals and any literal whose args are
not all ground.  It also ignores content inside quoted lists."
  (and (consp form) (collect-positive-ground-literals-walk form)))


(defun goal-fixed-fluent-assignments (goal-form rel)
  "Return a hash-table mapping REL key-args -> fluent-args fixed by GOAL-FORM.

Only applies when GOAL-FORM contains no OR (to avoid unsound pruning)."
  (when (and (consp goal-form)
             (not (goal-contains-or-p goal-form)))
    (let* ((sig (relation-signature rel))
           (fpos (relation-fluent-indices rel))
           (fixed (make-hash-table :test #'equal))
           (conflict (make-hash-table :test #'equal)))
      (when (and sig fpos)
        (dolist (lit (collect-positive-ground-literals goal-form))
          (when (and (consp lit)
                     (eql (car lit) rel)
                     (= (length (cdr lit)) (length sig)))
            (let (kargs fargs)
              (loop for a in (cdr lit)
                    for pos from 1
                    do (if (member pos fpos)
                           (push a fargs)
                           (push a kargs)))
              (setf kargs (nreverse kargs)
                    fargs (nreverse fargs))
              (cond
                ((gethash kargs conflict) nil)
                ((gethash kargs fixed)
                 (unless (equal (gethash kargs fixed) fargs)
                   (remhash kargs fixed)
                   (setf (gethash kargs conflict) t)))
                (t
                 (setf (gethash kargs fixed) fargs)))))))
      fixed)))


(defun goal-fixed-loc-walk (f obj)
  "Recursive walker for GOAL-FIXED-LOC.  Searches F for a positive (LOC OBJ area) literal."
  (cond
    ((and (consp f) (eql (car f) 'loc) (eql (second f) obj))
     (third f))
    ((and (consp f) (member (car f) '(and or)))
     (or (goal-fixed-loc-walk (second f) obj)
         (goal-fixed-loc-walk (cons (car f) (cddr f)) obj)))
    ((and (consp f) (eql (car f) 'not))
     nil)
    ((consp f)
     (or (goal-fixed-loc-walk (car f) obj)
         (goal-fixed-loc-walk (cdr f) obj)))
    (t nil)))


(defun goal-fixed-loc (goal-form obj)
  "If GOAL-FORM contains a positive (loc OBJ <area>) literal, return that area."
  (when (consp goal-form)
    (goal-fixed-loc-walk goal-form obj)))


;;;; ----------------------------------------------------------------------
;;;; Combinatoric helpers
;;;; ----------------------------------------------------------------------

(defun cartesian-product (lists)
  "Cartesian product of LISTS, yielding a list of tuples (lists)."
  (cond
    ((null lists) (list nil))
    ((null (car lists)) nil)
    (t
     (let ((head (car lists))
           (tail (cartesian-product (cdr lists))))
       (mapcan (lambda (x)
                 (mapcar (lambda (rest) (cons x rest)) tail))
               head)))))


(defun k-subsets (items k)
  "All subsets of ITEMS of size exactly K (as lists)."
  (cond
    ((= k 0) (list nil))
    ((null items) nil)
    (t
     (let ((x (car items))
           (xs (cdr items)))
       (nconc
        (mapcar (lambda (rest) (cons x rest))
                (k-subsets xs (1- k)))
        (k-subsets xs k))))))


(defun subsets-up-to (items max-k)
  "All subsets of ITEMS of size 0..MAX-K."
  (loop for k from 0 to max-k append (k-subsets items k)))


;;;; ----------------------------------------------------------------------
;;;; Enum action construction
;;;; ----------------------------------------------------------------------

(defun install-enum-action (name pre-fn eff-fn precondition-args)
  "Create and return an ACTION struct installed under NAME."
  (let* ((pre-name (intern (format nil "~A-PRE" name) *package*))
         (eff-name (intern (format nil "~A-EFF" name) *package*)))
    (setf (symbol-function pre-name) pre-fn)
    (setf (symbol-function eff-name) eff-fn)
    (make-action
     :name name
     :pre-defun-name pre-name
     :eff-defun-name eff-name
     :duration 0
     :precondition-params nil
     :precondition-variables nil
     :precondition-types nil
     :precondition-type-inst nil
     :dynamic nil
     :precondition-args precondition-args
     :precondition-form nil
     :init nil
     :precondition-lambda nil
     :iprecondition-lambda nil
     :effect-variables nil
     :effect-adds nil
     :effect-lambda nil
     :ieffect-lambda nil)))


(defun enum-filter-focus (xs focus-objs focus-set)
  "Filter XS to retain only elements in FOCUS-SET, or return XS if FOCUS-OBJS is nil."
  (if (null focus-objs)
      xs
      (remove-if-not (lambda (x) (gethash x focus-set)) xs)))


(defun enum-key-tuples-for (rel fixed-table focus-objs focus-set)
  "Return key tuples (lists) to enumerate for REL.

Generates the full Cartesian product of per-position type domains (filtered by
FOCUS-SET when applicable).  Any extra keys present in FIXED-TABLE but missing
from the product are appended."
  (let* ((k-specs (relation-key-type-specs rel))
         (k-domains
           (mapcar (lambda (spec)
                     (let* ((full (expand-type-spec-instances* spec))
                            (filt (enum-filter-focus full focus-objs focus-set)))
                       (or filt full)))
                   k-specs))
         (keys (cartesian-product k-domains)))
    (when (and fixed-table (> (hash-table-count fixed-table) 0))
      (let ((extras
              (loop for k being the hash-keys of fixed-table
                    unless (member k keys :test #'equal)
                    collect k)))
        (when extras
          (setf keys (nconc keys extras)))))
    keys))


(defun enum-value-tuples-for (rel)
  "Return all fluent value tuples for REL, or NIL if non-enumerable."
  (let* ((v-specs (relation-fluent-type-specs rel))
         (v-domains (mapcar #'expand-type-spec-instances* v-specs)))
    (when (every #'identity v-domains)
      (cartesian-product v-domains))))


(defun enum-make-fluent-action (rel key-args allowed-value-tuples have-elevation
                                &key (hold-skip-agent nil)
                                     (hold-skip-object nil)
                                     (hold-skip-area nil))
  "Create a per-key enum action for REL.

If HOLD-SKIP-AGENT/OBJECT/AREA are provided and the object is currently held by
that agent, the action becomes a no-op update (but still triggers propagation),
accepting only HOLD-SKIP-AREA as the single admissible instantiation."
  (let* ((name (if key-args
                   (intern (format nil "ENUM-~A-~{~A~^-~}" rel key-args) *package*)
                   (intern (format nil "ENUM-~A" rel) *package*)))
         (fluentless-prop (cons rel key-args)))
    (install-enum-action
     name
     (lambda (state &rest vals)
       (let ((held? (and hold-skip-agent hold-skip-object
                         (eql rel 'loc)
                         (held-by-p state hold-skip-agent hold-skip-object))))
         (cond
           (held?
            (and hold-skip-area
                 (equal vals (list hold-skip-area))
                 vals))
           (t
            (and (not (fluent-bound-p state fluentless-prop))
                 (member vals allowed-value-tuples :test #'equal)
                 vals)))))
     (lambda (state &rest vals)
       (let ((s (copy-state state)))
         (let ((held? (and hold-skip-agent hold-skip-object
                           (eql rel 'loc)
                           (held-by-p s hold-skip-agent hold-skip-object))))
           (unless held?
             (let ((lit (assemble-proposition rel key-args vals)))
               (update (problem-state.idb s) lit))))
         ;; Corner-ish convenience: elevation defaults when loc is set.
         (when (and have-elevation (eql rel 'loc))
           (let ((obj (first key-args)))
             (when (and obj (not (fluent-bound-p s (list 'elevation obj))))
               (update (problem-state.idb s) (list 'elevation obj 0)))))
         (propagate-changes! s)
         (make-update-from s vals)))
     (mapcar (lambda (tuple) tuple) allowed-value-tuples))))


(defun enum-make-holds-action (agent cargo-domain fixed-table)
  "Create a HOLDS enum action for AGENT with :NONE as the no-hold choice.

If FIXED-TABLE provides a fixed value for this key, restrict to that value only."
  (let* ((key-args (list agent))
         (fixed (and fixed-table (gethash key-args fixed-table)))
         (choices (if fixed
                      (list fixed)
                      (cons (list :none)
                            (mapcar (lambda (c) (list c)) cargo-domain)))))
    (install-enum-action
     (intern (format nil "ENUM-HOLDS-~A" agent) *package*)
     (lambda (state choice)
       (and (not (fluent-bound-p state (list 'holds agent)))
            (member (list choice) choices :test #'equal)
            (list choice)))
     (lambda (state choice)
       (let ((s (copy-state state)))
         (unless (eql choice :none)
           (update (problem-state.idb s) (list 'holds agent choice)))
         (propagate-changes! s)
         (make-update-from s (list choice))))
     (mapcar (lambda (x) x) choices))))


(defun enum-connector-index (c connectors)
  "Return the position of connector C in the CONNECTORS list."
  (position c connectors))


(defun enum-allowed-termini-for (c connectors termini)
  "Return termini that C may pair with, excluding itself and lower-indexed connectors."
  (let ((i (enum-connector-index c connectors)))
    (remove-if
     (lambda (x)
       (or (eql x c)
           (and (member x connectors :test #'eq)
                (<= (enum-connector-index x connectors) i))))
     termini)))


(defun enum-paired-present-p (state a b)
  "True iff (PAIRED A B) is present in STATE's idb."
  (gethash (convert-to-integer (list 'paired a b))
           (problem-state.idb state)))


(defun enum-connector-partners (state conn termini)
  "Return a duplicate-free list of CONN's current partners (all termini)."
  (let (out)
    (dolist (term termini out)
      (when (and (not (eql term conn))
                 (or (enum-paired-present-p state conn term)
                     (enum-paired-present-p state term conn)))
        (pushnew term out :test #'eq)))))


(defun enum-global-degree-ok-p (state conn chosen-set connectors termini max-pairs-per-connector)
  "True iff adding CHOSEN-SET to CONN does not violate the global degree cap.

The cap applies to:
  1) CONN itself, counting all of its partners (termini), and
  2) any TARGET in CHOSEN-SET that is itself a CONNECTOR.

Existing symmetric duplicates are ignored (partner sets are de-duplicated)."
  (let* ((max max-pairs-per-connector)
         (existing (enum-connector-partners state conn termini))
         (new (set-difference chosen-set existing :test #'eq)))
    (and (<= (+ (length existing) (length new)) max)
         (every (lambda (tgt)
                  (or (not (member tgt connectors :test #'eq))
                      (let* ((tgt-existing (enum-connector-partners state tgt termini))
                             (adds-one (not (member conn tgt-existing :test #'eq))))
                        (<= (+ (length tgt-existing)
                               (if adds-one 1 0))
                            max))))
                new))))


(defun enum-make-paired-actions (connectors termini max-pairs-per-connector
                                 &key (have-holds nil)
                                      (primary-agent nil))
  "Create subset-enumeration actions for (PAIRED terminus terminus) with global degree cap.

For each connector, generates an action whose instantiations are all subsets (up to
MAX-PAIRS-PER-CONNECTOR) of the connector's allowed termini.  Symmetry breaking
excludes lower-indexed connectors from the pairing targets."
  (when (and connectors termini)
    ;; IMPORTANT: bind C freshly per loop iteration to avoid closure capture.
    (loop for c0 in connectors append
          (let ((c c0))
            (let* ((allowed (enum-allowed-termini-for c connectors termini))
                   (sets (subsets-up-to allowed max-pairs-per-connector))
                   (args (mapcar (lambda (set) (list set)) sets)))
              (list
               (install-enum-action
                (intern (format nil "ENUM-PAIR-~A" c) *package*)
                (lambda (state set)
                  (cond
                    ((and have-holds primary-agent
                          (held-by-p state primary-agent c))
                     (when (null set) (list set)))
                    (t
                     ;; Corner-ish dependency: only pair after LOC(c) is bound.
                     ;; Also enforce the GLOBAL degree cap here (early pruning).
                     (and (fluent-bound-p state (list 'loc c))
                          (member set sets :test #'equal)
                          (enum-global-degree-ok-p state c set connectors termini max-pairs-per-connector)
                          (list set)))))
                (lambda (state set)
                  (let ((s (copy-state state)))
                    (dolist (tgt set)
                      (update (problem-state.idb s) (list 'paired c tgt)))
                    (propagate-changes! s)
                    (make-update-from s (list set))))
                args)))))))


(defun enum-make-finalize-action ()
  "Create the terminal ENUM-FINALIZE action that triggers final propagation."
  (install-enum-action
   'enum-finalize
   (lambda (state) t)
   (lambda (state)
     (let ((s (copy-state state)))
       (propagate-changes! s)
       (make-update-from s nil)))
   (list nil)))


;;;; ----------------------------------------------------------------------
;;;; Enum action generator
;;;; ----------------------------------------------------------------------

(defun generate-cornerish-enum-actions (&key (goal-form nil)
                                             (max-pairs-per-connector 4))
  "Generate a CSP enum-action sequence driven by (DEFINE-BASE-RELATIONS ...).

The generator is relation-driven:
  - For each *fluent* relation listed in the base schema, create per-key enum-actions
    that bind its fluent positions to values drawn from the declared type domains.
  - Special-case support remains for:
      * HOLDS: enumerated when present in the base schema, with a :NONE choice.
      * PAIRED: subset enumeration (corner-ish) using MAX-PAIRS-PER-CONNECTOR.
      * ELEVATION: if present, defaults to (ELEVATION <obj> 0) when a LOC is set.

Goal-driven restriction:
  - If GOAL-FORM contains positive ground literals that *fix* fluent values for a
    particular key (and GOAL-FORM contains no OR), instantiation domains are
    restricted accordingly."
  (let* ((schema (get-base-relations))
         (focus-objs (schema-focus-instances schema))
         (focus-set (let ((h (make-hash-table :test 'eq)))
                      (dolist (o focus-objs h)
                        (setf (gethash o h) t))))
         (relations (remove-duplicates (schema-relations schema) :test #'eq))
         ;; Convenience: domain lookups (may be NIL if type absent).
         (areas (maybe-type-instances 'area))
         (skip-area (and areas (first areas)))
         (agents (maybe-type-instances 'agent))
         (primary-agent (and agents (first agents)))
         (connectors (maybe-type-instances 'connector))
         (termini (maybe-type-instances 'terminus))

         (goal-has-or (goal-contains-or-p goal-form))

         (have-loc (member 'loc relations))
         (have-holds (member 'holds relations))
         (have-elevation (member 'elevation relations))
         (have-paired (member 'paired relations))

         (fixed-maps (make-hash-table :test #'eq)))

    (dolist (r relations)
      (when (and (relation-signature r) (relation-fluent-indices r))
        (setf (gethash r fixed-maps)
              (goal-fixed-fluent-assignments goal-form r))))

    (let ((actions nil))

      ;; --- 1) LOC first for AGENT keys (helps pruning + makes HOLDS usable later).
      (when have-loc
        (let* ((fixed (gethash 'loc fixed-maps))
               (all-keys (enum-key-tuples-for 'loc fixed focus-objs focus-set))
               (agent-keys (and agents
                                (remove-if-not (lambda (k)
                                                 (member (first k) agents))
                                               all-keys)))
               (other-keys (if agent-keys
                               (set-difference all-keys agent-keys :test #'equal)
                               all-keys))
               (vtuples (or (enum-value-tuples-for 'loc)
                            (and areas (mapcar (lambda (a) (list a)) areas)))))

          (dolist (k agent-keys)
            (let* ((f (and fixed (gethash k fixed)))
                   (allowed (or (and f (list f)) vtuples)))
              (when allowed
                (push (enum-make-fluent-action 'loc k allowed have-elevation)
                      actions))))

          ;; --- 2) HOLDS immediately after LOC(agent), before LOC(connectors).
          (when (and have-holds primary-agent)
            (let* ((fixed-h (gethash 'holds fixed-maps))
                   (cargo-domain
                     (let* ((v-specs (relation-fluent-type-specs 'holds))
                            (dom (and v-specs
                                      (expand-type-spec-instances*
                                       (first v-specs)))))
                       (or (enum-filter-focus dom focus-objs focus-set) dom))))
              (when cargo-domain
                (push (enum-make-holds-action primary-agent cargo-domain fixed-h)
                      actions))))

          ;; --- 3) LOC for remaining keys (connectors, etc.).
          (dolist (k other-keys)
            (let* ((f (and fixed (gethash k fixed)))
                   (allowed (or (and f (list f)) vtuples)))
              (when allowed
                (push (enum-make-fluent-action
                       'loc k allowed have-elevation
                       :hold-skip-agent (and (not goal-has-or)
                                             have-holds primary-agent
                                             skip-area (null f))
                       :hold-skip-object (first k)
                       :hold-skip-area skip-area)
                      actions))))))

      ;; --- 4) Other fluent relations (data-driven).
      (dolist (r relations)
        (when (and (relation-signature r)
                   (relation-fluent-indices r)
                   (not (member r '(loc holds elevation) :test #'eq)))
          (let* ((fixed (gethash r fixed-maps))
                 (keys (enum-key-tuples-for r fixed focus-objs focus-set))
                 (vtuples (enum-value-tuples-for r)))
            (when vtuples
              (dolist (k keys)
                (let* ((f (and fixed (gethash k fixed)))
                       (allowed (or (and f (list f)) vtuples)))
                  (push (enum-make-fluent-action r k allowed have-elevation)
                        actions)))))))

      ;; --- 5) Pairing subsets (corner-ish), fail-first by instantiation count.
      (when have-paired
        (let* ((pas (enum-make-paired-actions connectors termini max-pairs-per-connector
                                              :have-holds have-holds
                                              :primary-agent primary-agent))
               (pas-sorted
                 (stable-sort (copy-list pas) #'<
                              :key (lambda (a)
                                     (length (action.precondition-args a))))))
          (dolist (pa pas-sorted)
            (push pa actions))))

      ;; --- 6) Finalize.
      (push (enum-make-finalize-action) actions)

      ;; Return in execution order.
      (nreverse actions))))


;;; Filename: ww-enumerator.lisp
;;;
;;; CSP-based enumeration support for goal-state generation.
;;;
;;; Provides:
;;;   1) DEFINE-BASE-RELATIONS — problem specs declare the "base" (non-derived)
;;;      relations/types that the enumerator branches on.
;;;   2) DEFINE-ENUM-RELATION — problem specs declare per-relation enumeration
;;;      metadata (pattern, allow-unassigned, early-keys, on-assign,
;;;      symmetric-batch, max-per-key) to drive generic action generation.
;;;   3) Auto-detection of symmetric (undirected) relations and interchangeable
;;;      object groups from relation metadata and static-relation signatures.
;;;   4) DEFINE-PREFILTER — problem-spec hook to prune base states before
;;;      propagation.
;;;   5) FIND-GOAL-STATES — user-facing entry point for goal-state enumeration.
;;;   6) GENERATE-ENUM-ACTIONS — data-driven generator that creates a CSP
;;;      enum-action sequence from declared base relations, auto-detected
;;;      symmetries, and prefilters.


(in-package :ww)


;;;; ----------------------------------------------------------------------
;;;; Parameters
;;;; ----------------------------------------------------------------------

(defparameter *enumerator-base-relations* nil
  "Holds the most recently installed base-relation schema (a list of symbols).
   Intended to be set by DEFINE-BASE-RELATIONS within a problem specification.")


(defparameter *enumerator-actions* nil
  "Most recently generated enum-actions (ACTION structs) for CSP enumeration.")


(defparameter *enumerator-action-settings* nil
  "Most recently used keyword settings for CSP action generation (for reporting).")


(defparameter *enumerated-unique-solutions* nil
  "After ENUMERATE-STATE, holds the list of unique SOLUTION structs (unique goal states).")


(defparameter *enumerated-goal-states* nil
  "After ENUMERATE-STATE, holds the list of PROBLEM-STATE objects (unique goal states).")


(defparameter *enumerator-detected-groups* nil
  "Auto-detected interchangeable object groups for the most recent enumeration.
   Each group is a list of objects that share identical static-relation signatures.")


(defparameter *enumerator-detected-symmetric-relations* nil
  "Auto-detected symmetric (undirected) relations for the most recent enumeration.
   A binary relation is symmetric when both argument positions have the same type
   and neither is fluent.")


(defparameter *enumerator-prefilter* nil
  "Prefilter function for enumeration, set via DEFINE-PREFILTER.
   When non-nil, applied at ENUM-FINALIZE to prune states before propagation.")


(defparameter *enum-relation-metadata* (make-hash-table :test 'eq)
  "Hash table mapping relation symbols to enum metadata plists.
   Set by DEFINE-ENUM-RELATION in problem specifications.
   Keys: relation symbols.  Values: plists with keys
   :PATTERN :ALLOW-UNASSIGNED :EARLY-KEYS :ON-ASSIGN :SYMMETRIC-BATCH :MAX-PER-KEY
   :KEY-TYPES :REQUIRES-FLUENT.")


;;;; ----------------------------------------------------------------------
;;;; Macros
;;;; ----------------------------------------------------------------------


(defmacro define-base-relations (schema)
  "Problem-spec macro: (define-base-relations ( ...symbols... ))"
  `(install-base-relations ',schema))


(defmacro define-prefilter (name lambda-list &body body)
  "Problem-spec macro: define a prefilter function for enumeration.
   The prefilter is applied to each base state before propagation.
   States for which the prefilter returns NIL are pruned.
   NAME: symbol naming the prefilter (for documentation)
   LAMBDA-LIST: must be (state) — the function receives a problem-state
   BODY: code that returns T to keep the state, NIL to prune it
   Example:
     (define-prefilter corner-paths (state)
       (and (prefilter-paired-reachable-p state 'transmitter1 'receiver2)
            (prefilter-paired-reachable-p state 'transmitter2 'receiver3)))"
  `(install-prefilter ',name (lambda ,lambda-list ,@body)))


(defmacro define-enum-relation (relation &rest keys)
  "Problem-spec macro: declare enumeration metadata for RELATION.
   Keys:
   :PATTERN          :FLUENT | :SUBSET | :DERIVED  (default :FLUENT)
   :ALLOW-UNASSIGNED T | (:TYPES type...)  — which keys may be unassigned
   :EARLY-KEYS       (:TYPES type...)  — enumerate these key-types first
   :ON-ASSIGN        ((rel val)...)  — default other relations when assigned
   :SYMMETRIC-BATCH  T | NIL  — use canonical multisets for interchangeable groups
   :MAX-PER-KEY      positive integer  — cardinality cap (for :SUBSET pattern)
   :KEY-TYPES        (:TYPES type...)  — restrict subset keys to these types
   :REQUIRES-FLUENT  symbol  — fluent that must be bound for non-empty subsets
   Example:
     (define-enum-relation loc
       :pattern :fluent
       :allow-unassigned (:types cargo)
       :early-keys (:types agent)
       :on-assign ((elevation 0))
       :symmetric-batch t)"
  `(install-enum-relation-meta ',relation ',keys))


;;;; ----------------------------------------------------------------------
;;;; Top-level entry points
;;;; ----------------------------------------------------------------------


(defmacro find-goal-states (goal-spec 
                            &key 
                            (algorithm nil algorithm-supplied-p)
                            (solution-type nil solution-type-supplied-p)
                            (exclude-relations nil exclude-relations-supplied-p)
                            (include-relations nil include-relations-supplied-p)
                            (prefilter :use-installed prefilter-supplied-p))
  "User-facing macro wrapper for FIND-GOAL-STATES-FN.
   Automatically quotes goal-spec, relation symbols/lists, and algorithm/solution-type symbols.
   GOAL-SPEC: goal form, partial goal-state shorthand, or symbol naming a goal function
   Keywords:
   :SOLUTION-TYPE  FIRST | EVERY | <integer N>
   :ALGORITHM      enumeration/search algorithm symbol (e.g., DEPTH-FIRST)
   :EXCLUDE-RELATIONS  symbol or list of symbols to remove from base schema
   :INCLUDE-RELATIONS  symbol or list of symbols to add to base schema
   :PREFILTER  function of (state) to prune base states before propagation.
               Default :USE-INSTALLED uses (get-prefilter); pass NIL to disable.
   Returns: T (the report plist is saved on (get 'find-goal-states :last-report))."
  (flet ((literal-symbol-p (form)
           (and (symbolp form) 
                (not (keywordp form))
                (not (null form))))
         (literal-list-p (form)
           (and (consp form)
                (not (eq (car form) 'quote)))))
    (labels ((maybe-quote (form)
               (cond
                 ((null form) nil)
                 ((and (consp form) (eq (car form) 'quote)) form)
                 ((literal-symbol-p form) `',form)
                 ((literal-list-p form) `',form)
                 (t form))))
      `(find-goal-states-fn 
        ,(maybe-quote goal-spec)
        ,@(if algorithm-supplied-p
              `(:algorithm ,(maybe-quote algorithm))
              '(:algorithm *algorithm*))
        ,@(if solution-type-supplied-p
              `(:solution-type ,(maybe-quote solution-type))
              '(:solution-type 'first))
        ,@(when exclude-relations-supplied-p
            `(:exclude-relations ,(maybe-quote exclude-relations)))
        ,@(when include-relations-supplied-p
            `(:include-relations ,(maybe-quote include-relations)))
        ,@(when prefilter-supplied-p
            `(:prefilter ,prefilter))))))


(defun find-goal-states-fn (goal-spec &key (algorithm *algorithm*) (solution-type 'first)
                                        exclude-relations include-relations
                                        (prefilter :use-installed))
  "Internal function for goal-state enumeration (called by FIND-GOAL-STATES macro).
   Internally uses :FINALIZE-ONLY propagation: base-relation assignments are
   enumerated without propagation, then each complete leaf state is propagated
   once and tested against the goal.
   GOAL-SPEC can be:
   - a goal form, including quantifiers like (forall ...), (and ...), etc.
   - a partial goal-state shorthand: ((p a) (q b)) meaning (and (p a) (q b))
   Keywords:
   :SOLUTION-TYPE  FIRST | EVERY | <integer N>
   :ALGORITHM      enumeration/search algorithm (e.g., DEPTH-FIRST)
   :EXCLUDE-RELATIONS  list (or single symbol) to remove from base schema for this run
   :INCLUDE-RELATIONS  list (or single symbol) to add to base schema for this run
   :PREFILTER  function of (state) to prune base states before propagation.
               Default :USE-INSTALLED uses (get-prefilter); pass NIL to disable.
   Returns: T (the report plist is saved on (get 'find-goal-states :last-report))."
  (unless (get-base-relations)
    (error "FIND-GOAL-STATES: no base relations declared.  ~
            Use (define-base-relations (...)) in the problem specification."))
  ;; Guard: *debug* and *probe* interfere with enumeration output.
  (when (or (> *debug* 0) *probe*)
    (format t "~&[find-goal-states] Please reset first by entering ")
    (when (> *debug* 0)
      (format t " (ww-set *debug* 0)~%"))
    (when *probe*
      (format t " (ww-set *probe* nil)~%"))
    (return-from find-goal-states-fn nil))
  (multiple-value-bind (norm-goal-spec goal-form)
      (enum-normalize-goal-spec goal-spec)
    (let* ((normalized-solution-type (fgs-normalize-solution-type solution-type))
           (run-base-relations (fgs-compute-run-base-relations exclude-relations include-relations)))
      ;; Resolve :use-installed prefilter default.
      (let ((effective-prefilter (if (eq prefilter :use-installed)
                                     (get-prefilter)
                                     prefilter)))
        ;; Dynamic binding provides temporary override for this run.
        (let ((*enumerator-base-relations* run-base-relations))
          (let ((states (enumerate-state norm-goal-spec
                                         :algorithm algorithm
                                         :solution-type normalized-solution-type
                                         :propagate :finalize-only
                                         :prefilter effective-prefilter)))
          (let ((report (fgs-build-report goal-form states)))
            (fgs-print-report report)
            (setf (get 'find-goal-states :last-report) report)
            t)))))))


(defun fgs-normalize-solution-type (x)
  "Validate and normalize SOLUTION-TYPE for find-goal-states UI.
   Accepts FIRST, EVERY, or a positive integer N. Invalid values default to FIRST."
  (cond
    ((eq x 'first) 'first)
    ((eq x 'every) 'every)
    ((integerp x)
     (if (plusp x)
         x
         (progn
           (format t "~&[find-goal-states] SOLUTION-TYPE integer must be > 0; got ~S. Using FIRST.~%" x)
           'first)))
    (t
     (format t "~&[find-goal-states] SOLUTION-TYPE must be FIRST, EVERY, or integer N; got ~S. Using FIRST.~%" x)
     'first)))


(defun fgs-compute-run-base-relations (exclude-relations include-relations)
  "Compute the effective base-relation schema for this run.
   Starts from current schema, removes EXCLUDE-RELATIONS, appends INCLUDE-RELATIONS."
  (let ((base (copy-list (get-base-relations))))
    (dolist (r (fgs-ensure-list exclude-relations))
      (setf base (remove r base :test #'eq)))
    (dolist (r (fgs-ensure-list include-relations))
      (unless (member r base :test #'eq)
        (setf base (append base (list r)))))
    base))


(defun fgs-ensure-list (x)
  "Coerce X to a list: NIL -> NIL, atom -> (atom), list -> list."
  (cond ((null x) nil)
        ((listp x) x)
        (t (list x))))


(defun enumerate-state (goal-spec &key (algorithm *algorithm*) (solution-type 'first)
                                       (default-max-per-key 4) (propagate t)
                                       (prefilter nil))
  "Enumerate compatible goal states via CSP-based variable assignment.
   GOAL-SPEC may be a goal form (including quantifiers) or a partial goal-state
   shorthand ((p ...) (q ...)).
   SOLUTION-TYPE may be any existing solver mode (FIRST, EVERY, MIN-LENGTH, etc.).
   If SOLUTION-TYPE is a positive integer N, return N goal states (truncated).
   PROPAGATE: T — propagate after every action (default).
              :FINALIZE-ONLY — skip intermediate propagation, propagate at finalize.
              NIL — no propagation (raw leaf collection).
   PREFILTER: when non-nil, prunes base states before propagation."
  (unless (get-base-relations)
    (error "ENUMERATE-STATE: no base relations declared.  ~
            Use (define-base-relations (...)) in the problem specification."))
  ;; only validate integer N; otherwise accept existing solution-type modes.
  (when (integerp solution-type)
    (unless (plusp solution-type)
      (error "ENUMERATE-STATE: integer SOLUTION-TYPE must be a positive N; got ~S"
             solution-type)))
  (enumerate-state-csp goal-spec
                       :algorithm algorithm
                       :solution-type solution-type
                       :default-max-per-key default-max-per-key
                       :propagate propagate
                       :prefilter prefilter))


(defun enumerate-state-csp (goal-spec &key (algorithm *algorithm*) (solution-type 'first)
                                           (default-max-per-key 4) (propagate t)
                                           (prefilter nil))
  "Enumerate compatible states via a CSP-style, generated enum-action sequence.
   SOLUTION-TYPE:
   - any existing solver mode (FIRST, EVERY, MIN-LENGTH, etc.)
   - or a positive integer N meaning: stop after recording N solutions.
   PROPAGATE: T — propagate after every action (default).
              :FINALIZE-ONLY — skip intermediate propagation, propagate at finalize.
              NIL — no propagation (raw leaf collection).
   PREFILTER: when non-nil, a function of one argument (state) applied at
   ENUM-FINALIZE to prune states before propagation."
  (multiple-value-bind (norm-goal-spec goal-form)
      (enum-normalize-goal-spec goal-spec)
    (let* ((requested-solution-type solution-type)
           (solver-solution-type requested-solution-type)
           (gf (coerce-goal norm-goal-spec))
           (enum-actions (generate-enum-actions
                          :goal-form goal-form
                          :default-max-per-key default-max-per-key
                          :propagate propagate
                          :prefilter prefilter))
           ;; When propagate=NIL, collect ALL finalize states (defer goal test).
           ;; When propagate=T, apply the real goal test at leaf nodes.
           (leaf-goal (if propagate
                          (lambda (state)
                            (and (eql (problem-state.name state) 'enum-finalize)
                                 (funcall gf state)))
                          (lambda (state)
                            (eql (problem-state.name state) 'enum-finalize))))
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
                        :default-max-per-key default-max-per-key
                        :propagate propagate
                        :prefilter (if prefilter t nil)))
            (enum-actions-summary enum-actions)
            (setf *actions* enum-actions
                  *init-actions* nil
                  *problem-type* 'csp
                  *solution-type* solver-solution-type
                  *tree-or-graph* 'tree  ;note that graph mode may miss some candidate states
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
            ;; Truncate if user requested N (kept for backward compatibility).
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


(defun goal-literal-list-p (x)
  "True iff X is a list of positive ground literals (partial goal-state shorthand).
   Each element must be a cons whose car is a symbol not in the set of
   goal connectives (AND, OR, NOT, FORALL, EXISTS, IMPLIES, WHEN, IF)."
  (and (listp x)
       (every (lambda (e)
                (and (consp e)
                     (symbolp (car e))
                     (not (member (car e)
                                  '(and or not forall exists implies when if)
                                  :test #'eq))))
              x)))


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
  (cond
    ;; Function object: no symbol plist available, no form for pruning.
    ((functionp goal-spec)
     (values goal-spec nil))
    ;; Named function symbol: use plist :form for pruning when available.
    ((and (symbolp goal-spec) (fboundp goal-spec))
     (values goal-spec (get goal-spec :form)))
    ;; NIL => unconstrained goal
    ((null goal-spec)
     (values (list 'and t) (list 'and t)))
    ;; Partial-goal-state shorthand: list of literals => (and ...)
    ((goal-literal-list-p goal-spec)
     (let ((form (cons 'and goal-spec)))  ; goal-spec guaranteed non-nil here
       (values form form)))
    ;; Regular goal form (including quantifiers): pass through
    ((consp goal-spec)
     (values goal-spec goal-spec))
    (t
     (error "ENUM-NORMALIZE-GOAL-SPEC: unsupported goal spec: ~S" goal-spec))))


(defun fgs-build-report (goal-form goal-states)
  "Build and return a FIND-GOAL-STATES report plist.

Fields:
  :GOAL             normalized goal form
  :GOAL-STATES      list of PROBLEM-STATE objects (for ww-backwards)
  :GOAL-STATE-PROPS propositional databases for each goal state
  :SUMMARY          counts plist (:GOAL-STATES n)"
  (let* ((goal-state-props
           (mapcar (lambda (st) (list-database (problem-state.idb st)))
                   goal-states))
         (summary (list :goal-states (length goal-states))))
    (list
     :goal goal-form
     :goal-states goal-states
     :goal-state-props goal-state-props
     :summary summary)))


(defun fgs-print-report (report)
  "Print FIND-GOAL-STATES report interactively.
Displays goal, summary, and first goal-state propositions.
Prompts with y-or-n-p to show additional states one at a time.
If the user answers NO, returns immediately."
  (let ((goal (getf report :goal))
        (summary (getf report :summary))
        (props-list (getf report :goal-state-props)))
    (format t "~&~%;;;; FIND-GOAL-STATES REPORT ;;;;~%")
    (format t "~&Goal: ~S~%" goal)
    (format t "~&Goal states found: ~D~%" (getf summary :goal-states))
    (when props-list
      (let ((n (length props-list)))
        (format t "~&~%Goal state 1 of ~D:~%" n)
        (fgs-print-props (first props-list))
        (loop for k from 2 to n
              for props in (rest props-list)
              do (unless (y-or-n-p "~&Show the next goal state of ~D?" n)
                   (return-from fgs-print-report report))
                 (format t "~&~%Goal state ~D of ~D:~%" k n)
                 (fgs-print-props props)))))
  report)


(defun fgs-print-props (props)
  "Print a list of propositions, one per line, indented."
  (dolist (p props)
    (format t "  ~S~%" p)))


(defun enumerated-goal-databases ()
  "Convenience: return a list of propositional databases for enumerated goal states."
  (mapcar (lambda (st) (list-database (problem-state.idb st)))
          *enumerated-goal-states*))


(defun maybe-propagate-changes! (state)
  "Invoke problem-specific propagation when available."
  (when (fboundp 'propagate-changes!)
    (funcall (symbol-function 'propagate-changes!) state)))


(defun propagate-and-filter-states (states goal-fn)
  "Propagate each state in STATES and return those satisfying GOAL-FN.
   STATES: list of PROBLEM-STATE objects (typically unpropagated leaf states).
   GOAL-FN: a function of one argument (state) returning T if goal is satisfied.
   Returns: list of propagated states that satisfy GOAL-FN.
   Side effect: modifies each state's idb via propagate-changes!."
  (let ((goal-states nil)
        (total (length states))
        (checked 0))
    (dolist (s states (nreverse goal-states))
      (incf checked)
      (maybe-propagate-changes! s)
      (when (funcall goal-fn s)
        (push s goal-states))
      (when (zerop (mod checked 10000))
        (format t "~&[propagate-and-filter] ~D/~D states processed, ~D goals found~%"
                checked total (length goal-states))))))


(defun enumerate-unpropagated-leaves (goal-spec &key (algorithm *algorithm*)
                                                     (default-max-per-key 4))
  "Enumerate all leaf states WITHOUT propagation; return the list of raw states.
   These states contain only base relation assignments (no derived relations).
   Use PROPAGATE-AND-FILTER-STATES to batch-propagate and filter afterward."
  (enumerate-state goal-spec
                   :algorithm algorithm
                   :solution-type 'every
                   :default-max-per-key default-max-per-key
                   :propagate nil))


;;;; ----------------------------------------------------------------------
;;;; Prefilter helpers
;;;; ----------------------------------------------------------------------


(defun enum-actions-summary (&optional (actions *enumerator-actions*))
  "Print a compact summary of ACTIONS (defaults to the last generated enum-actions)."
  (format t "~&~%;;;; ENUM-ACTIONS-SUMMARY ;;;;~%")
  (format t "~&Problem: ~S~%" *problem-name*)
  ;; print the effective base schema for this run (supports temporary overrides).
  (format t "~&Base schema: ~S~%"
          (or (and (boundp '*enumerator-base-relations*)
                   *enumerator-base-relations*)
              (get-base-relations)))
  (when *enumerator-detected-symmetric-relations*
    (format t "~&Symmetric relations: ~S~%" *enumerator-detected-symmetric-relations*))
  (when *enumerator-detected-groups*
    (format t "~&Interchangeable groups: ~S~%" *enumerator-detected-groups*))
  (when *enumerator-action-settings*
    (format t "~&Settings: ~S~%" *enumerator-action-settings*)
    (format t "~&Goal form: ~S~%" (get 'goal-fn :form)))
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
  "Retrieve the enumerator base-relation schema.
Checks dynamic binding first (for temporary overrides), then problem property."
  (or *enumerator-base-relations*
      (get problem :enumerator-base-relations)))


;;;; ----------------------------------------------------------------------
;;;; Enum relation metadata
;;;; ----------------------------------------------------------------------


(defun install-enum-relation-meta (relation keys)
  "Install enumeration metadata for RELATION.
   RELATION: a symbol naming the relation.
   KEYS: a property list (:PATTERN val :ALLOW-UNASSIGNED val ...).
   Valid keys:
   :PATTERN          :FLUENT | :SUBSET | :DERIVED  (default :FLUENT)
   :ALLOW-UNASSIGNED T | (:TYPES type...)  — which keys may be unassigned
   :EARLY-KEYS       (:TYPES type...)  — enumerate these key-types first
   :ON-ASSIGN        ((rel val)...)  — default other relations when assigned
   :SYMMETRIC-BATCH  T | NIL  — use canonical multisets for interchangeable groups
   :MAX-PER-KEY      positive integer  — cardinality cap (for :SUBSET pattern)
   :KEY-TYPES        (:TYPES type...)  — restrict subset keys to these types
   :REQUIRES-FLUENT  symbol  — fluent that must be bound for non-empty subsets"
  (check-type relation symbol)
  (let ((plist (enum-canonicalize-meta-keys keys)))
    (enum-validate-relation-meta relation plist)
    (setf (gethash relation *enum-relation-metadata*) plist)
    (when (and (boundp '*problem-name*) *problem-name*)
      (let ((tbl (or (get *problem-name* :enum-relation-metadata)
                     (let ((h (make-hash-table :test 'eq)))
                       (setf (get *problem-name* :enum-relation-metadata) h)
                       h))))
        (setf (gethash relation tbl) plist)))
    (format t "~&Installing enumerator metadata for ~S...~%" relation)
    relation))


(defun enum-canonicalize-meta-keys (keys)
  "Canonicalize KEYS plist: ensure :PATTERN defaults to :FLUENT when absent."
  (let ((plist (copy-list keys)))
    (unless (getf plist :pattern)
      (setf plist (list* :pattern :fluent plist)))
    plist))


(defun enum-validate-relation-meta (relation plist)
  "Validate enumeration metadata PLIST for RELATION.  Signals error on invalid entries."
  (let ((pattern (getf plist :pattern))
        (allow-unassigned (getf plist :allow-unassigned))
        (early-keys (getf plist :early-keys))
        (on-assign (getf plist :on-assign))
        (max-per-key (getf plist :max-per-key)))
    (unless (member pattern '(:fluent :subset :derived))
      (error "DEFINE-ENUM-RELATION ~S: :PATTERN must be :FLUENT, :SUBSET, or :DERIVED; got ~S"
             relation pattern))
    (when allow-unassigned
      (unless (or (eq allow-unassigned t)
                  (and (consp allow-unassigned)
                       (eql (car allow-unassigned) :types)
                       (every #'symbolp (cdr allow-unassigned))))
        (error "DEFINE-ENUM-RELATION ~S: :ALLOW-UNASSIGNED must be T or (:TYPES type...); got ~S"
               relation allow-unassigned)))
    (when early-keys
      (unless (and (consp early-keys)
                   (eql (car early-keys) :types)
                   (every #'symbolp (cdr early-keys)))
        (error "DEFINE-ENUM-RELATION ~S: :EARLY-KEYS must be (:TYPES type...); got ~S"
               relation early-keys)))
    (when on-assign
      (unless (and (listp on-assign)
                   (every (lambda (entry)
                            (and (consp entry)
                                 (symbolp (first entry))
                                 (= (length entry) 2)))
                          on-assign))
        (error "DEFINE-ENUM-RELATION ~S: :ON-ASSIGN must be ((rel val)...); got ~S"
               relation on-assign)))
    (when max-per-key
      (unless (and (integerp max-per-key) (plusp max-per-key))
        (error "DEFINE-ENUM-RELATION ~S: :MAX-PER-KEY must be a positive integer; got ~S"
               relation max-per-key)))
    (let ((key-types (getf plist :key-types)))
      (when key-types
        (unless (and (consp key-types)
                     (eql (car key-types) :types)
                     (every #'symbolp (cdr key-types)))
          (error "DEFINE-ENUM-RELATION ~S: :KEY-TYPES must be (:TYPES type...); got ~S"
                 relation key-types))))
    (let ((requires-fluent (getf plist :requires-fluent)))
      (when requires-fluent
        (unless (symbolp requires-fluent)
          (error "DEFINE-ENUM-RELATION ~S: :REQUIRES-FLUENT must be a symbol; got ~S"
                 relation requires-fluent))))))


(defun enum-relation-meta (rel)
  "Return the enum metadata plist for REL, or NIL if none declared."
  (or (gethash rel *enum-relation-metadata*)
      (let ((tbl (and (boundp '*problem-name*) *problem-name*
                      (get *problem-name* :enum-relation-metadata))))
        (and tbl (gethash rel tbl)))))


(defun enum-pattern (rel)
  "Return the enumeration pattern for REL.  Defaults to :FLUENT."
  (let ((meta (enum-relation-meta rel)))
    (if meta (getf meta :pattern) :fluent)))


(defun enum-allow-unassigned (rel)
  "Return the :ALLOW-UNASSIGNED spec for REL, or NIL."
  (getf (enum-relation-meta rel) :allow-unassigned))


(defun enum-early-keys (rel)
  "Return the :EARLY-KEYS spec for REL, or NIL."
  (getf (enum-relation-meta rel) :early-keys))


(defun enum-on-assign (rel)
  "Return the :ON-ASSIGN list for REL, or NIL."
  (getf (enum-relation-meta rel) :on-assign))


(defun enum-symmetric-batch-p (rel)
  "Return T if REL declares :SYMMETRIC-BATCH."
  (getf (enum-relation-meta rel) :symmetric-batch))


(defun enum-max-per-key (rel)
  "Return the :MAX-PER-KEY integer for REL, or NIL."
  (getf (enum-relation-meta rel) :max-per-key))


(defun enum-key-types (rel)
  "Return the :KEY-TYPES spec for REL, or NIL."
  (getf (enum-relation-meta rel) :key-types))


(defun enum-requires-fluent (rel)
  "Return the :REQUIRES-FLUENT symbol for REL, or NIL."
  (getf (enum-relation-meta rel) :requires-fluent))


(defun enum-key-allows-unassigned-p (rel key-tuple)
  "Return T if KEY-TUPLE for REL qualifies for an unassigned sentinel.
   Dispatches on the :ALLOW-UNASSIGNED spec:
   T — all keys qualify.
   (:TYPES type...) — qualify if any object in KEY-TUPLE is an instance of
   one of the listed types."
  (let ((spec (enum-allow-unassigned rel)))
    (cond
      ((null spec) nil)
      ((eq spec t) t)
      ((and (consp spec) (eql (car spec) :types))
       (let ((types (cdr spec)))
         (some (lambda (obj)
                 (some (lambda (ty) (member obj (maybe-type-instances ty) :test #'eq))
                       types))
               key-tuple))))))


(defun enum-key-matches-types-p (key-tuple type-list)
  "Return T if any object in KEY-TUPLE is an instance of one of the types in TYPE-LIST."
  (some (lambda (obj)
          (some (lambda (ty) (member obj (maybe-type-instances ty) :test #'eq))
                type-list))
        key-tuple))


(defun enum-compute-allowed-values (rel key fixed vtuples)
  "Compute allowed value tuples for REL at KEY.
   When FIXED provides a forced value for KEY, return only that value.
   Otherwise return VTUPLES, appending (:UNASSIGNED) when the key qualifies
   via the relation's :ALLOW-UNASSIGNED metadata."
  (let* ((f (and fixed (gethash key fixed)))
         (base (or (and f (list f)) vtuples)))
    (if (and (null f) (enum-key-allows-unassigned-p rel key))
        (append base (list '(:unassigned)))
        base)))


(defun clear-enum-relation-metadata ()
  "Clear all enum relation metadata."
  (clrhash *enum-relation-metadata*)
  (when (and (boundp '*problem-name*) *problem-name*)
    (setf (get *problem-name* :enum-relation-metadata) nil))
  nil)


;;;; ----------------------------------------------------------------------
;;;; Auto-detection of symmetric relations and interchangeable groups
;;;; ----------------------------------------------------------------------


(defun enum-detect-symmetric-relations (relations)
  "Auto-detect which RELATIONS are symmetric (undirected).
   A binary relation is symmetric when it has exactly 2 argument positions,
   neither is fluent, and both have the same type spec.
   Returns a list of relation symbols."
  (let ((symmetric nil))
    (dolist (rel relations symmetric)
      (let ((sig (relation-signature rel))
            (fpos (relation-fluent-indices rel)))
        (when (and sig
                   (= (length sig) 2)
                   (null fpos)
                   (equal (first sig) (second sig)))
          (push rel symmetric))))))


(defun enum-collect-symbols-in-form (form)
  "Collect all unique symbols appearing in FORM via iterative tree walk."
  (let ((symbols nil)
        (stack (list form)))
    (loop while stack do
      (let ((x (pop stack)))
        (cond
          ((symbolp x) (pushnew x symbols :test #'eq))
          ((consp x)
           (push (car x) stack)
           (push (cdr x) stack)))))
    symbols))


(defun enum-extract-goal-object-references (goal-form)
  "Extract object constants that appear explicitly in GOAL-FORM.
   Returns a list of symbols that are both in GOAL-FORM and in *types*."
  (when (null goal-form)
    (return-from enum-extract-goal-object-references nil))
  (let ((all-objects (collect-all-objects))
        (form-symbols (enum-collect-symbols-in-form goal-form)))
    (intersection form-symbols all-objects :test #'eq)))


(defun enum-detect-interchangeable-groups (schema goal-form)
  "Auto-detect groups of interchangeable objects for enumeration.
   Uses the same static-relation signature analysis as search-time symmetry
   detection (identify-candidate-types, compute-all-signatures, partition-by-signature).
   Objects explicitly named in GOAL-FORM are excluded from groups.
   Returns a list of groups, each a list of interchangeable objects."
  (let ((candidate-types (identify-candidate-types)))
    (when (null candidate-types)
      (return-from enum-detect-interchangeable-groups nil))
    (let* ((signatures (compute-all-signatures candidate-types))
           (groups (partition-by-signature candidate-types signatures))
           (goal-objects (enum-extract-goal-object-references goal-form)))
      (when goal-objects
        (setf groups
              (loop for group in groups
                    for unreferenced = (set-difference group goal-objects)
                    when (> (length unreferenced) 1)
                    collect unreferenced)))
      groups)))


;;;; ----------------------------------------------------------------------
;;;; Prefilter management
;;;; ----------------------------------------------------------------------


(defun install-prefilter (name fn)
  "Install FN as the enumeration prefilter under NAME.
   FN must be a function of one argument (state) returning T to keep, NIL to prune."
  (check-type name symbol)
  (check-type fn function)
  (format t "~&Installing enumeration prefilter for ~S...~%" name)
  (setf *enumerator-prefilter* fn)
  (when (and (boundp '*problem-name*) *problem-name*)
    (setf (get *problem-name* :enumerator-prefilter) fn)
    (setf (get *problem-name* :enumerator-prefilter-name) name))
  name)


(defun get-prefilter (&optional (problem *problem-name*))
  "Retrieve the enumeration prefilter function.
   Checks dynamic variable first, then problem property."
  (or *enumerator-prefilter*
      (get problem :enumerator-prefilter)))


(defun get-prefilter-name (&optional (problem *problem-name*))
  "Retrieve the name of the installed prefilter (for reporting)."
  (get problem :enumerator-prefilter-name))


(defun clear-prefilter ()
  "Clear the enumeration prefilter."
  (setf *enumerator-prefilter* nil)
  (when (and (boundp '*problem-name*) *problem-name*)
    (setf (get *problem-name* :enumerator-prefilter) nil)
    (setf (get *problem-name* :enumerator-prefilter-name) nil))
  nil)


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
   - a partial goal-state shorthand: ((p a) (q b c)) => (and (p a) (q b c))
   - NIL => (and t)"
  (cond
    ;; A lambda/function object
    ((functionp goal-spec) goal-spec)
    ;; A function name
    ((and (symbolp goal-spec) (fboundp goal-spec))
     (symbol-function goal-spec))
    ;; Normalize list/NIL goal specs (supports partial goal-state shorthand).
    ((or (consp goal-spec) (null goal-spec))
     (multiple-value-bind (norm-goal-spec goal-form)
         (enum-normalize-goal-spec goal-spec)
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


(defun fluent-value (state fluentless-prop)
  "Return (values value present-p) for FLUENTLESS-PROP in STATE's idb."
  (let ((key (convert-to-integer fluentless-prop)))
    (gethash key (problem-state.idb state))))


(defun fluent-bound-p (state fluentless-prop)
  "True iff FLUENTLESS-PROP has a value bound in STATE's idb."
  (multiple-value-bind (v present-p) (fluent-value state fluentless-prop)
    (declare (ignore v))
    present-p))


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
   Unknown type symbols yield NIL rather than an error, which is important for
   domains that use non-enumerable types like RATIONAL or LIST."
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


(defun multisets-with-repetition (items k)
  "Generate all multisets (sorted selections with repetition) of size K from ITEMS.
   ITEMS must be a sorted list. Returns a list of sorted K-tuples.
   Example: (multisets-with-repetition '(a b c) 2) => ((a a) (a b) (a c) (b b) (b c) (c c))
   For 4 areas and 3 connectors: 4 items, k=3 yields C(4+3-1,3) = C(6,3) = 20 multisets."
  (cond
    ((= k 0) (list nil))
    ((null items) nil)
    (t
     (let ((x (car items))
           (xs (cdr items)))
       ;; Include x in the multiset (can repeat x), plus multisets not using x
       (nconc
        (mapcar (lambda (rest) (cons x rest))
                (multisets-with-repetition items (1- k)))  ; x can repeat
        (multisets-with-repetition xs k))))))


(defun canonical-value-assignments (objects values)
  "Generate canonical (sorted) value assignments for symmetric OBJECTS over VALUES.
   Returns a list of assignment alists, each mapping object -> value.
   Canonical means the value sequence is non-decreasing by value index."
  (let* ((n (length objects))
         (multisets (multisets-with-repetition values n)))
    (mapcar (lambda (value-tuple)
              (mapcar #'cons objects value-tuple))
            multisets)))


(defun enum-assignment-respects-fixed-p (assignment fixed-table)
  "Return T if ASSIGNMENT respects all fixed constraints in FIXED-TABLE.
   ASSIGNMENT is an alist of (object . value) pairs.
   FIXED-TABLE maps (object) keys to (value) tuples."
  (every (lambda (pair)
           (let* ((obj (car pair))
                  (val (cdr pair))
                  (key (list obj))
                  (fixed (and fixed-table (gethash key fixed-table))))
             (or (null fixed)
                 (equal (list val) fixed))))
         assignment))


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


(defun enum-make-fluent-action (rel key-args allowed-value-tuples &key (propagate t))
  "Create a per-key enum action for REL.
   When the value tuple is (:UNASSIGNED), the object is left unassigned (no update).
   When REL declares :ON-ASSIGN metadata, default relations are applied on assignment."
  (let* ((name (if key-args
                   (intern (format nil "ENUM-~A-~{~A~^-~}" rel key-args) *package*)
                   (intern (format nil "ENUM-~A" rel) *package*)))
         (fluentless-prop (cons rel key-args))
         (on-assign (enum-on-assign rel)))
    (install-enum-action
     name
     (lambda (state &rest vals)
       (and (not (fluent-bound-p state fluentless-prop))
            (member vals allowed-value-tuples :test #'equal)
            vals))
     (lambda (state &rest vals)
       (let ((s (copy-state state)))
         (unless (eq (first vals) :unassigned)
           (let ((lit (assemble-proposition rel key-args vals)))
             (update (problem-state.idb s) lit))
           (when on-assign
             (let ((obj (first key-args)))
               (dolist (entry on-assign)
                 (let ((target-rel (first entry))
                       (default-val (second entry)))
                   (when (not (fluent-bound-p s (list target-rel obj)))
                       (update (problem-state.idb s)
                             (list target-rel obj default-val))))))))
         (when propagate
           (maybe-propagate-changes! s))
         (make-update-from s vals)))
     (mapcar (lambda (tuple) tuple) allowed-value-tuples))))


(defun enum-make-symmetric-batch-action (rel sym-type objects value-domain
                                         &key (propagate t) (fixed-maps nil))
  "Create a single enum action that assigns values to all symmetric OBJECTS at once.
   Uses canonical (sorted) multiset assignments to avoid enumerating symmetric permutations.
   REL is the relation being assigned.
   SYM-TYPE is the symmetry group type name (for action naming).
   OBJECTS is the list of symmetric objects.
   VALUE-DOMAIN is the list of possible values.
   When REL's :ALLOW-UNASSIGNED metadata applies, :UNASSIGNED is appended to the domain.
   FIXED-MAPS: when provided, filters assignments to respect goal-fixed constraints.
   Returns a single action with one instantiation per canonical assignment."
  (let* ((name (intern (format nil "ENUM-~A-~AS" rel sym-type) *package*))
         (allow-unassigned (some (lambda (obj)
                                   (enum-key-allows-unassigned-p rel (list obj)))
                                 objects))
         (effective-domain (if allow-unassigned
                               (append value-domain (list :unassigned))
                               value-domain))
         (all-assignments (canonical-value-assignments objects effective-domain))
         (fixed (and fixed-maps (gethash rel fixed-maps)))
         (assignments (if fixed
                          (remove-if-not
                           (lambda (asgn)
                             (enum-assignment-respects-fixed-p asgn fixed))
                           all-assignments)
                          all-assignments))
         (on-assign (enum-on-assign rel)))
    (install-enum-action
     name
     ;; Precondition: none of these objects have REL bound yet
     (lambda (state assignment)
       (and (every (lambda (pair)
                     (not (fluent-bound-p state (list rel (car pair)))))
                   assignment)
            (member assignment assignments :test #'equal)
            (list assignment)))
     ;; Effect: bind REL for each assigned object; skip :UNASSIGNED
     (lambda (state assignment)
       (let ((s (copy-state state)))
         (dolist (pair assignment)
           (let ((obj (car pair))
                 (val (cdr pair)))
             (unless (eq val :unassigned)
               (update (problem-state.idb s) (list rel obj val))
               (when on-assign
                 (dolist (entry on-assign)
                   (let ((target-rel (first entry))
                         (default-val (second entry)))
                     (when (not (fluent-bound-p s (list target-rel obj)))
                       (update (problem-state.idb s)
                               (list target-rel obj default-val)))))))))
         (when propagate
           (maybe-propagate-changes! s))
         (make-update-from s (list assignment))))
     (mapcar #'list assignments))))


(defun enum-key-index (key keys)
  "Return the position of KEY in the KEYS list."
  (position key keys))


(defun enum-allowed-partners-for (key keys partners &key symmetric-relation-p)
  "Return partners that KEY may pair with, excluding itself.
   When SYMMETRIC-RELATION-P is true, also exclude lower-indexed keys
   (since the undirected pair will be covered from the other direction)."
  (let ((i (enum-key-index key keys)))
    (remove-if
     (lambda (x)
       (or (eql x key)
           (and symmetric-relation-p
                (member x keys :test #'eq)
                (<= (enum-key-index x keys) i))))
     partners)))


(defun enum-subset-present-p (state rel a b)
  "True iff (REL A B) is present in STATE's idb."
  (gethash (convert-to-integer (list rel a b))
           (problem-state.idb state)))


(defun enum-key-partners (state rel key partners)
  "Return a duplicate-free list of KEY's current partners under REL."
  (let (out)
    (dolist (p partners out)
      (when (and (not (eql p key))
                 (or (enum-subset-present-p state rel key p)
                     (enum-subset-present-p state rel p key)))
        (pushnew p out :test #'eq)))))


(defun enum-subset-degree-ok-p (state rel key chosen-set keys partners max-per-key)
  "True iff adding CHOSEN-SET to KEY does not violate the global degree cap for REL.
   The cap applies to:
   1) KEY itself, counting all of its partners, and
   2) any TARGET in CHOSEN-SET that is itself a key.
   Existing symmetric duplicates are ignored (partner sets are de-duplicated)."
  (let* ((existing (enum-key-partners state rel key partners))
         (new (set-difference chosen-set existing :test #'eq)))
    (and (<= (+ (length existing) (length new)) max-per-key)
         (every (lambda (tgt)
                  (or (not (member tgt keys :test #'eq))
                      (let* ((tgt-existing (enum-key-partners state rel tgt partners))
                             (adds-one (not (member key tgt-existing :test #'eq))))
                        (<= (+ (length tgt-existing)
                               (if adds-one 1 0))
                            max-per-key))))
                new))))


(defun enum-make-subset-actions (rel keys partners max-per-key
                                 &key (propagate t) (symmetric-relation-p nil)
                                      (requires-fluent nil))
  "Create subset-enumeration actions for REL with global degree cap.
   For each key, generates an action whose instantiations are all subsets (up to
   MAX-PER-KEY) of the key's allowed partners.
   When SYMMETRIC-RELATION-P, excludes lower-indexed keys from pairing targets.
   When REQUIRES-FLUENT is non-nil, keys without that fluent bound are restricted
   to empty subsets."
  (when (and keys partners)
    ;; bind K freshly per loop iteration to avoid closure capture.
    (loop for k0 in keys append
          (let ((k k0))
            (let* ((allowed (enum-allowed-partners-for k keys partners
                                                      :symmetric-relation-p symmetric-relation-p))
                   (sets (subsets-up-to allowed max-per-key))
                   (args (mapcar (lambda (set) (list set)) sets)))
              (list
               (install-enum-action
                (intern (format nil "ENUM-~A-~A" rel k) *package*)
                (lambda (state set)
                  (cond
                    ((and requires-fluent
                          (not (fluent-bound-p state (list requires-fluent k))))
                     (when (null set) (list set)))
                    (t
                     ;; Key with fluent bound (or no fluent required): enumerate subsets.
                     (and (member set sets :test #'equal)
                          (enum-subset-degree-ok-p state rel k set keys partners max-per-key)
                          (list set)))))
                (lambda (state set)
                  (let ((s (copy-state state)))
                    (dolist (tgt set)
                      (update (problem-state.idb s) (list rel k tgt)))
                    (when propagate
                      (maybe-propagate-changes! s))
                    (make-update-from s (list set))))
                args)))))))


(defun enum-make-finalize-action (&key (propagate t) (prefilter nil))
  "Create the terminal ENUM-FINALIZE action that triggers final propagation.
   PREFILTER: when non-nil, a function of one argument (state) that must return
   true for the state to proceed. Applied BEFORE propagation for early pruning."
  (install-enum-action
   'enum-finalize
   ;; Precondition: apply prefilter if provided
   (if prefilter
       (lambda (state)
         (funcall prefilter state))
       (lambda (state) t))
   ;; Effect: propagate (if enabled) and finalize
   (lambda (state)
     (let ((s (copy-state state)))
       (when propagate
         (maybe-propagate-changes! s))
       (make-update-from s nil)))
   (list nil)))


;;;; ----------------------------------------------------------------------
;;;; Enum action generator
;;;; ----------------------------------------------------------------------


(defun enum-generate-fluent-actions (ctx propagate)
  "Generate enum actions for all :FLUENT-pattern relations using metadata.
   Returns two values: EARLY-ACTIONS and MAIN-ACTIONS.
   Each relation is processed using its enum metadata:
     :EARLY-KEYS — keys matching these types are enumerated first (early actions)
     :ALLOW-UNASSIGNED — qualifying keys get an :UNASSIGNED sentinel value
     :SYMMETRIC-BATCH — interchangeable groups use canonical multisets
     :DERIVED — relations with this pattern are skipped entirely"
  (let ((relations (getf ctx :relations))
        (focus-objs (getf ctx :focus-objs))
        (focus-set (getf ctx :focus-set))
        (fixed-maps (getf ctx :fixed-maps))
        (interchangeable-groups (getf ctx :interchangeable-groups))
        (sym-objects (getf ctx :sym-objects))
        (early-actions nil)
        (main-actions nil))
    (dolist (r relations)
      (when (and (eq (enum-pattern r) :fluent)
                 (relation-signature r)
                 (relation-fluent-indices r))
        (let* ((fixed (gethash r fixed-maps))
               (all-keys (enum-key-tuples-for r fixed focus-objs focus-set))
               (vtuples (enum-value-tuples-for r))
               (early-spec (enum-early-keys r))
               (early-types (and early-spec (cdr early-spec))))
          (when vtuples
            ;; Split keys into early and non-early.
            ;; Early keys exclude symmetric objects (handled in batch).
            (let ((e-keys nil) (o-keys nil))
              (if early-types
                  (dolist (k all-keys)
                    (if (and (enum-key-matches-types-p k early-types)
                             (not (some (lambda (obj)
                                          (member obj sym-objects :test #'eq))
                                        k)))
                        (push k e-keys)
                        (push k o-keys)))
                  (setf o-keys all-keys))
              (setf e-keys (nreverse e-keys)
                    o-keys (nreverse o-keys))
              ;; Early-key actions (no unassigned sentinel)
              (dolist (k e-keys)
                (let* ((f (and fixed (gethash k fixed)))
                       (allowed (or (and f (list f)) vtuples)))
                  (push (enum-make-fluent-action r k allowed :propagate propagate)
                        early-actions)))
              ;; Main-key actions: symmetric batch + individual
              (let ((batch-objects (make-hash-table :test 'eq)))
                ;; Symmetric batch for interchangeable groups (single-fluent only)
                (when (and (enum-symmetric-batch-p r)
                           (= (length (relation-fluent-indices r)) 1))
                  (dolist (group interchangeable-groups)
                    (let ((group-in-keys (remove-if-not
                                          (lambda (obj)
                                            (member (list obj) o-keys :test #'equal))
                                          group)))
                      (when (> (length group-in-keys) 1)
                        (let ((stype (or (find-group-type group) 'symmetric))
                              (value-domain (mapcar #'first vtuples)))
                          (push (enum-make-symmetric-batch-action
                                 r stype group-in-keys value-domain
                                 :propagate propagate
                                 :fixed-maps fixed-maps)
                                main-actions)
                          (dolist (obj group-in-keys)
                            (setf (gethash obj batch-objects) t)))))))
                ;; Individual key actions
                (dolist (k o-keys)
                  (unless (gethash (first k) batch-objects)
                    (let ((allowed (enum-compute-allowed-values r k fixed vtuples)))
                      (push (enum-make-fluent-action r k allowed :propagate propagate)
                            main-actions))))))))))
    (values (nreverse early-actions) (nreverse main-actions))))


(defun enum-compute-context (goal-form)
  "Compute context plist for enum action generation.
   Gathers schema metadata, type domains, goal analysis, and fixed-maps.
   Returns a plist with keys:
     :schema :focus-objs :focus-set :relations :goal-form
     :symmetric-relations :interchangeable-groups :sym-objects :fixed-maps"
  (let* ((schema (get-base-relations))
         (focus-objs (schema-focus-instances schema))
         (focus-set (let ((h (make-hash-table :test 'eq)))
                      (dolist (o focus-objs h)
                        (setf (gethash o h) t))))
         (relations (remove-duplicates (schema-relations schema) :test #'eq))
         (symmetric-relations (enum-detect-symmetric-relations relations))
         (interchangeable-groups (enum-detect-interchangeable-groups schema goal-form))
         (sym-objects (let ((objs nil))
                        (dolist (group interchangeable-groups objs)
                          (dolist (o group)
                            (pushnew o objs :test #'eq)))))
         (fixed-maps (make-hash-table :test #'eq)))
    (dolist (r relations)
      (when (and (relation-signature r) (relation-fluent-indices r))
        (setf (gethash r fixed-maps)
              (goal-fixed-fluent-assignments goal-form r))))
    (list :schema schema
          :focus-objs focus-objs
          :focus-set focus-set
          :relations relations
          :goal-form goal-form
          :symmetric-relations symmetric-relations
          :interchangeable-groups interchangeable-groups
          :sym-objects sym-objects
          :fixed-maps fixed-maps)))


(defun enum-generate-subset-actions (ctx default-max-per-key propagate)
  "Generate subset-enumeration actions for all :SUBSET-pattern relations.
   For each relation, derives keys and partners from the signature and metadata.
   Creates one action per key, sorted fail-first by instantiation count.
   When a relation is auto-detected as symmetric, applies index-based deduplication.
   Returns a list of actions in execution order."
  (let ((relations (getf ctx :relations))
        (symmetric-relations (getf ctx :symmetric-relations))
        (all-actions nil))
    (dolist (r relations)
      (when (eq (enum-pattern r) :subset)
        (let* ((sig (relation-signature r))
               (key-type-spec (first sig))
               (partner-type-spec (second sig))
               (all-key-instances (expand-type-spec-instances* key-type-spec))
               (key-types-spec (enum-key-types r))
               (keys (if key-types-spec
                         (let ((types (cdr key-types-spec)))
                           (remove-if-not
                            (lambda (obj)
                              (some (lambda (ty)
                                      (member obj (maybe-type-instances ty) :test #'eq))
                                    types))
                            all-key-instances))
                         all-key-instances))
               (partners (expand-type-spec-instances* partner-type-spec))
               (max-k (or (enum-max-per-key r) default-max-per-key))
               (requires-fluent (enum-requires-fluent r))
               (actions (enum-make-subset-actions
                         r keys partners max-k
                         :propagate propagate
                         :symmetric-relation-p (member r symmetric-relations)
                         :requires-fluent requires-fluent)))
          (setf all-actions (nconc all-actions
                                   (stable-sort (copy-list actions) #'<
                                                :key (lambda (a)
                                                       (length (action.precondition-args a)))))))))
    all-actions))


(defun generate-enum-actions (&key (goal-form nil) (default-max-per-key 4)
                                   (propagate t) (prefilter nil))
  "Generate a CSP enum-action sequence driven by declared base relations,
   enum-relation metadata, symmetry groups, and prefilter.
   Orchestrates metadata-driven generators and assembles final action list.
   Phases:
   1. Early fluent actions (:EARLY-KEYS from metadata, for pruning)
   2. Main fluent actions (:SYMMETRIC-BATCH + individual, with :UNASSIGNED)
   3. Subset enumeration for :SUBSET-pattern relations (fail-first ordering)
   4. ENUM-FINALIZE (triggers propagation, applies prefilter)
   PROPAGATE: T — propagate after every action (default).
              :FINALIZE-ONLY — skip intermediate propagation, propagate at finalize.
              NIL — no propagation anywhere (raw leaf collection).
   PREFILTER: when non-nil, a function applied at ENUM-FINALIZE to prune states
     before propagation."
  (let* ((intermediate-propagate (eq propagate t))
         (finalize-propagate (not (null propagate)))
         (ctx (enum-compute-context goal-form)))
    (setf *enumerator-detected-groups* (getf ctx :interchangeable-groups)
          *enumerator-detected-symmetric-relations* (getf ctx :symmetric-relations))
    (multiple-value-bind (early-actions main-actions)
        (enum-generate-fluent-actions ctx intermediate-propagate)
      (let* ((subset-actions (enum-generate-subset-actions
                              ctx default-max-per-key intermediate-propagate))
             (finalize-action (enum-make-finalize-action
                               :propagate finalize-propagate :prefilter prefilter)))
        ;; Assemble in execution order:
        ;; early-fluents -> main-fluents -> subset -> FINALIZE
        (nconc early-actions
               main-actions
               subset-actions
               (list finalize-action))))))

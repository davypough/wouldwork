;;; Filename: ww-enumerator.lisp
;;;
;;; CSP-based enumeration support for goal-state generation.
;;;
;;; Provides:
;;;   1) DEFINE-BASE-RELATIONS — optional problem-spec override for the
;;;      enumerator's base schema (relations/types to branch on).
;;;   2) DEFINE-BASE-RELATION — problem specs declare per-relation enumeration
;;;      metadata (pattern, allow-unassigned, early-keys, on-assign,
;;;      max-per, requires) to drive generic action generation.
;;;   3) Auto-detection of symmetric (undirected) relations and interchangeable
;;;      object groups from relation metadata and static-relation signatures.
;;;   4) DEFINE-PREFILTER — problem-spec hook to prune base states before
;;;      propagation.
;;;   5) FIND-GOAL-STATES — user-facing entry point for goal-state enumeration.
;;;   6) GENERATE-ENUM-ACTIONS — data-driven generator that creates a CSP
;;;      enum-action sequence from declared base relations, auto-detected
;;;      symmetries, prefilters, and branch-time feasibility pruning.
;;;   7) Branch-time feasibility pruning via :REQUIRES metadata,
;;;      which uses WouldWork DSL predicates over relation args to prune
;;;      infeasible partner candidates during CSP subset branching.


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


(defparameter *enumerator-ui-settings* nil
  "Optional caller-facing keyword settings for the current enumeration run.
   Used by ENUM-ACTIONS-SUMMARY to print user-level settings (e.g. FIND-GOAL-STATES
   keyword arguments) alongside internal enumerator settings.")


(defparameter *enumerated-unique-solutions* nil
  "After ENUMERATE-STATE, holds the list of unique SOLUTION structs (unique goal states).")


(defparameter *enumerated-goal-states* nil
  "After ENUMERATE-STATE, holds the list of PROBLEM-STATE objects (unique goal states).")


(defparameter *enumerated-goal-unpruned-canonical-counts* nil
  "Lazy cache: hash table of canonical base keys -> multiplicity from an unpruned goal run.")


(defparameter *enumerated-goal-unpruned-canonical-first-state* nil
  "Lazy cache: hash table of canonical base keys -> first matching state from unpruned goal run.")


(defparameter *enumerated-goal-unpruned-canonical-signature* nil
  "Run signature used to validate *ENUMERATED-GOAL-UNPRUNED-CANONICAL-* caches.")


(defparameter *backward-reachable-set* nil
  "After FIND-PREDECESSORS, a hash table mapping canonical base-prop key to
   (STATE . ACTION-PATH) where ACTION-PATH is a list of action forms ordered
   from the state toward the goal.  NIL when no backward set has been computed.")


(defparameter *enum-disable-csp-metadata-pruning* nil
  "When non-NIL, enum subset-action generation ignores :REQUIRES metadata.")


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


(defparameter *enumerator-base-filter-form* nil
  "Lambda form for a base filter defined via DEFINE-BASE-FILTER.
   Stored at install time, compiled later by COMPILE-ALL-FUNCTIONS.")


(defparameter *enumerator-base-filter-name* nil
  "Name of the base filter defined via DEFINE-BASE-FILTER.")


(defparameter *enum-relation-metadata* (make-hash-table :test 'eq)
  "Hash table mapping relation symbols to enum metadata plists.
   Set by DEFINE-BASE-RELATION in problem specifications.
   Keys: relation symbols.  Values: plists with keys
   :PATTERN :ALLOW-UNASSIGNED :EARLY-KEYS :ON-ASSIGN :MAX-PER-KEY
   :KEY-TYPES :REQUIRES-PREDICATE.")


(defparameter *enum-relation-order* nil
  "Declaration-order list of relations seen by DEFINE-BASE-RELATION.
   Used to infer base relations when DEFINE-BASE-RELATIONS is omitted.")


(defparameter *enum-residual-symmetry-enabled* t
  "Master switch for residual symmetry pruning in subset enumeration.
   Default T to enable pruning; set NIL to disable for verification.")


(defparameter *enum-csp-enforce-no-runtime-symmetry-pruning* t
  "When non-NIL, CSP enumeration binds *SYMMETRY-PRUNING* and
   *ENUM-RESIDUAL-SYMMETRY-ENABLED* to NIL for sound completeness.
   Canonical dedupe at full assignments then handles symmetry safely.")


(defparameter *enumerator-disabled-search-hook-symbols*
  '(min-steps-remaining? heuristic? prune-state? bounding-function?)
  "Search hook function symbols temporarily disabled during CSP enumeration.
   These hooks are designed for normal planning search states and can
  incorrectly prune or bias synthetic partial states used by the enumerator.")


(defstruct (fps-layer-stats (:constructor make-fps-layer-stats ()))
  "Mutable tracking counters and per-operator tables for a single backward
   predecessor layer expansion.  Threaded through helper functions to avoid
   a large flat let-binding in the expansion loop."
  (layer-table (make-hash-table :test 'equal))
  (raw-count 0 :type fixnum)
  (feasible-count 0 :type fixnum)
  (validated-count 0 :type fixnum)
  (collision-reachable-count 0 :type fixnum)
  (collision-layer-count 0 :type fixnum)
  (novel-validation-attempts 0 :type fixnum)
  (apply-success-count 0 :type fixnum)
  (apply-fail-count 0 :type fixnum)
  (key-mismatch-count 0 :type fixnum)
  (op-attempts (make-hash-table :test 'eq))
  (op-success (make-hash-table :test 'eq))
  (op-fail (make-hash-table :test 'eq))
  (op-sample-failure (make-hash-table :test 'eq))
  (op-sample-detail (make-hash-table :test 'eq))
  (op-raw (make-hash-table :test 'eq))
  (op-feasible (make-hash-table :test 'eq)))


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


(defmacro define-base-filter (name body)
  "Problem-spec macro: define a base-state filter using WouldWork's logic DSL.
   Like DEFINE-PREFILTER but the body uses the same syntax as query functions:
   EXISTS, FORALL, AND, OR, NOT, BIND, relation lookups, etc.
   NAME: symbol naming the filter (for documentation/debugging)
   BODY: a WouldWork expression returning non-NIL to keep the state, NIL to prune
   Example:
     (define-base-filter connectors-in-areas2&3
       (and (exists ((?c1 ?c2) connector)
              (and (loc ?c1 area2) (loc ?c2 area3)))))"
  `(install-base-filter ',name ',body))


(defun install-base-filter (name body)
  "Translate BODY using the WouldWork translator and store the resulting
   lambda form for deferred compilation by COMPILE-ALL-FUNCTIONS.
   The compiled function is later installed as the enumeration prefilter."
  (format t "~&Installing base filter ~S...~%" name)
  (let ((new-$vars (delete-duplicates
                     (get-all-nonspecial-vars #'$varp body))))
    (setf *enumerator-base-filter-name* name)
    (setf (symbol-value name)
      `(lambda (state)
         ,(format nil "~A base-filter" name)
         (declare (ignorable state))
         (block ,name
           (let (,@new-$vars)
             (declare (ignorable ,@new-$vars))
             ,(if (eql (car body) 'let)
                `(let ,(second body)
                   ,(third body)
                   ,(translate (fourth body) 'pre))
                (translate body 'pre))))))
    (fix-if-ignore '(state) (symbol-value name))
    (setf *enumerator-base-filter-form* (symbol-value name)))
  name)


(defmacro define-base-relation (relation-spec &rest keys)
  "Problem-spec macro: declare enumeration metadata for RELATION-SPEC.
   RELATION-SPEC can be:
     RELATION-SYMBOL
     (RELATION-SYMBOL ?arg1 ?arg2 ...)
   Keys:
   :PATTERN          :FLUENT | :SUBSET  (default inferred from relation)
   :ALLOW-UNASSIGNED T | (:TYPES type...)  — which keys may be unassigned
   :EARLY-KEYS       (:TYPES type...)  — enumerate these key-types first
   :ON-ASSIGN        ((rel val)...)  — default other relations when assigned
   :KEY-TYPES        (:TYPES type...)  — restrict subset keys to these types
   :MAX-UNASSIGNED   (:TYPES type... positive-integer)
                     cap how many keys of listed types may take (:UNASSIGNED)
   :MAX-PER          (?arg positive-integer) — subset cardinality cap for arg1
   :REQUIRES         WouldWork DSL form over relation args for subset partner feasibility
   Note: when *SYMMETRY-PRUNING* is T, interchangeable groups automatically use
   canonical multiset assignments to avoid enumerating symmetric permutations.
   Example:
     (define-base-relation (paired ?connector ?terminus)
       :max-per (?connector 3)
       :requires (bind (loc ?connector $any-area)))"
  `(install-enum-relation-meta ',relation-spec ',keys))


;;;; ----------------------------------------------------------------------
;;;; Top-level entry points
;;;; ----------------------------------------------------------------------


(defmacro find-goal-states (&rest raw-args)
  "User-facing macro wrapper for FIND-GOAL-STATES-FN.
   Automatically quotes goal-spec, relation symbols/lists, and algorithm/solution-type symbols.
   GOAL-SPEC: goal form, partial goal-state shorthand, or symbol naming a goal function.
              Optional; defaults to GOAL-FN.
   Keywords:
   :SOLUTION-TYPE  FIRST | EVERY | <integer N>
   :ALGORITHM      enumeration/search algorithm symbol (e.g., DEPTH-FIRST)
   :EXCLUDE-RELATIONS  symbol or list of symbols to remove from base schema
   :INCLUDE-RELATIONS  symbol or list of symbols to add to base schema
   :PREFILTER  function of (state) to prune base states before propagation.
               Default :USE-INSTALLED uses (get-prefilter); pass NIL to disable.
   :SORT<  comparison function of (state-a state-b) for sorting goal states.
           States for which (funcall sort< a b) is true sort first (best).
           Default NIL (no sorting).
   :SORT-KEY  function of (state) returning a numeric key for sorting.
              Lower keys sort first.  More efficient than :SORT< because each
              state's key is computed once (Schwartzian transform).
              Default NIL (no sorting).  Mutually exclusive with :SORT<.
   Returns: T (the report plist is saved on (get 'find-goal-states :last-report))."
  (let ((args (cond
                ((null raw-args) (list 'goal-fn))
                ((keywordp (car raw-args)) (cons 'goal-fn raw-args))
                (t raw-args))))
    (destructuring-bind (goal-spec
                         &key
                         (algorithm nil algorithm-supplied-p)
                         (solution-type nil solution-type-supplied-p)
                         (exclude-relations nil exclude-relations-supplied-p)
                         (include-relations nil include-relations-supplied-p)
                         (prefilter :use-installed prefilter-supplied-p)
                         (sort< nil sort<-supplied-p)
                         (sort-key nil sort-key-supplied-p))
        args
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
                  '(:solution-type 'every))
            ,@(when exclude-relations-supplied-p
                `(:exclude-relations ,(maybe-quote exclude-relations)))
            ,@(when include-relations-supplied-p
                `(:include-relations ,(maybe-quote include-relations)))
            ,@(when prefilter-supplied-p
                `(:prefilter ,prefilter))
            ,@(when sort<-supplied-p
                `(:sort< ,sort<))
            ,@(when sort-key-supplied-p
                `(:sort-key ,sort-key))))))))


(defmacro find-predecessors (&key
                               (action-families :auto action-families-supplied-p)
                               (direction 'forward direction-supplied-p))
  "User-facing macro wrapper for FIND-PREDECESSORS-FN.
   Automatically quotes literal symbols/lists for :ACTION-FAMILIES and :DIRECTION,
   so users can write e.g. (find-predecessors :direction forward
                                              :action-families (move pickup))."
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
      `(find-predecessors-fn
        ,@(if action-families-supplied-p
              `(:action-families ,(maybe-quote action-families))
              '(:action-families :auto))
        ,@(if direction-supplied-p
              `(:direction ,(maybe-quote direction))
              '(:direction 'forward))))))


(defmacro analyze-predecessor-prune (&key
                                       (predicate 'predecessor-state-feasible? predicate-supplied-p)
                                       (action-families :auto action-families-supplied-p)
                                       (sample-limit 10 sample-limit-supplied-p))
  "User-facing macro wrapper for ANALYZE-PREDECESSOR-PRUNE-FN.
   Estimates forward-candidate prune impact of PREDICATE."
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
      `(analyze-predecessor-prune-fn
        ,@(if predicate-supplied-p
              `(:predicate ,(maybe-quote predicate))
              '(:predicate 'predecessor-state-feasible?))
        ,@(if action-families-supplied-p
              `(:action-families ,(maybe-quote action-families))
              '(:action-families :auto))
        ,@(if sample-limit-supplied-p
              `(:sample-limit ,sample-limit)
              '(:sample-limit 10))))))


(defun find-goal-states-fn (&optional (goal-spec 'goal-fn)
                                      &key (algorithm *algorithm*) (solution-type 'every)
                                        exclude-relations include-relations
                                        (prefilter :use-installed)
                                        sort< sort-key)
  "Internal function for goal-state enumeration (called by FIND-GOAL-STATES macro).
   Internally uses :FINALIZE-ONLY propagation: base-relation assignments are
   enumerated without propagation, then each complete leaf state is propagated
   once and tested against the goal.
   GOAL-SPEC can be:
   - a goal form, including quantifiers like (forall ...), (and ...), etc.
   - a partial goal-state shorthand: ((p a) (q b)) meaning (and (p a) (q b))
   - omitted, in which case GOAL-FN is used
   Keywords:
   :SOLUTION-TYPE  FIRST | EVERY | <integer N>
   :ALGORITHM      enumeration/search algorithm (e.g., DEPTH-FIRST)
   :EXCLUDE-RELATIONS  list (or single symbol) to remove from base schema for this run
   :INCLUDE-RELATIONS  list (or single symbol) to add to base schema for this run
   :PREFILTER  function of (state) to prune base states before propagation.
               Default :USE-INSTALLED uses (get-prefilter); pass NIL to disable.
   :SORT<  comparison function of (state-a state-b) → generalized boolean.
           When non-nil, goal states are stable-sorted so that states for which
           (funcall sort< a b) is true appear first.  Default NIL (no sorting).
   :SORT-KEY  function of (state) → number.  Lower keys sort first.
              More efficient than :SORT< (Schwartzian transform: each key
              computed once).  Default NIL.  Mutually exclusive with :SORT<.
   Returns: T (the report plist is saved on (get 'find-goal-states :last-report))."
  (unless (get-base-relations)
    (error "FIND-GOAL-STATES: no base relations declared.  ~
            Declare relations with DEFINE-BASE-RELATION (or use DEFINE-BASE-RELATIONS)."))
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
           (run-base-relations (fgs-compute-run-base-relations exclude-relations include-relations))
           (goal-run-settings (list :algorithm algorithm
                                    :solution-type normalized-solution-type
                                    :exclude-relations exclude-relations
                                    :include-relations include-relations
                                    :prefilter prefilter
                                    :sort< sort<
                                    :sort-key sort-key)))
      ;; Invalidate lazy unpruned canonical cache for this new run context.
      (setf *enumerated-goal-unpruned-canonical-counts* nil
            *enumerated-goal-unpruned-canonical-first-state* nil
            *enumerated-goal-unpruned-canonical-signature* nil)
      ;; New goal enumeration run invalidates previously computed predecessor set.
      (setf *backward-reachable-set* nil
            (get 'find-predecessors :last-report) nil)
      ;; Resolve :use-installed prefilter default.
      (let* ((effective-prefilter (if (eq prefilter :use-installed)
                                      (get-prefilter)
                                      prefilter))
             ;; Action-invariant prefilter: prune states violating goal literals
             ;; on base relations that no action can modify.
             (modifiable-rels (actions-modifiable-base-relations *actions*))          ;; ADDED
             (invariants (enum-action-invariant-literals                              ;; ADDED
                          goal-form run-base-relations modifiable-rels))              ;; ADDED
             (invariant-prefilter (make-goal-invariant-prefilter                      ;; ADDED
                                   invariants 'find-goal-states))                    ;; ADDED
             (combined-prefilter (compose-prefilters                                  ;; ADDED
                                  effective-prefilter invariant-prefilter)))          ;; ADDED
        ;; Dynamic binding provides temporary override for this run.
        (let ((*enumerator-base-relations* run-base-relations))
          (let ((*enumerator-ui-settings* goal-run-settings))
            (let ((states (enumerate-state norm-goal-spec
                                           :algorithm algorithm
                                           :solution-type normalized-solution-type
                                           :propagate 'finalize-only
                                           :prefilter combined-prefilter
                                           :canonical-dedupe t)))
          (when (and sort< sort-key)
            (error "FIND-GOAL-STATES: :SORT< and :SORT-KEY are mutually exclusive."))
          (when sort<
            (setf states (stable-sort states sort<)))
          (when sort-key
            (let ((decorated (mapcar (lambda (st) (cons (funcall sort-key st) st)) states)))
              (setf states (mapcar #'cdr (stable-sort decorated #'< :key #'car)))))
          (let ((report (fgs-build-report goal-form states)))
            (fgs-print-report report)
            (setf (get 'find-goal-states :last-report) report)
            t))))))))


(defun enum-normalize-action-families (action-families)
  "Normalize ACTION-FAMILIES to plain symbols MOVE, PICKUP, CONNECT.
   Accepts either plain symbols or keywords; NIL/ALL means all families."
  (let* ((families0 (fgs-ensure-list action-families))
         (families (mapcar (lambda (fam)
                             (cond
                               ((or (eq fam 'move) (eq fam :move)) 'move)
                               ((or (eq fam 'pickup) (eq fam :pickup)) 'pickup)
                               ((or (eq fam 'connect) (eq fam :connect)) 'connect)
                               ((or (eq fam 'all) (eq fam :all)) 'all)
                               (t fam)))
                           families0)))
    (if (or (null families) (member 'all families))
        '(move pickup connect)
        families)))


(defun enum-action-in-family-p (action-name family)
  "True iff ACTION-NAME belongs to FAMILY (MOVE, PICKUP, CONNECT)."
  (case family
    (move (eql action-name 'move))
    (pickup (eql action-name 'pickup-connector))
    (connect (member action-name
                     '(connect-to-1-terminus
                       connect-to-2-terminus
                       connect-to-3-terminus)
                     :test #'eq))
    (otherwise nil)))


(defun enum-select-actions-by-family (actions action-families)
  "Select ACTION structs from ACTIONS whose names match ACTION-FAMILIES.
   ACTION-FAMILIES may include MOVE/PICKUP/CONNECT/ALL (or keyword variants)."
  (let* ((families (enum-normalize-action-families action-families)))
    (remove-if-not
     (lambda (a)
       (some (lambda (fam)
               (enum-action-in-family-p (action.name a) fam))
             families))
     actions)))


(defun enum-default-action-families-from-actions (actions)
  "Infer default predecessor action families from ACTIONS.
   Returns a list subset of (MOVE PICKUP CONNECT), preserving that order."
  (let ((families nil))
    (when (some (lambda (a) (enum-action-in-family-p (action.name a) 'move)) actions)
      (push 'move families))
    (when (some (lambda (a) (enum-action-in-family-p (action.name a) 'pickup)) actions)
      (push 'pickup families))
    (when (some (lambda (a) (enum-action-in-family-p (action.name a) 'connect)) actions)
      (push 'connect families))
    (nreverse families)))


(defun enum-action-instantiated-forms (action state)
  "Enumerate applicable ACTION forms for ACTION in STATE.
   Each result is a fully instantiated list: (action-name arg1 arg2 ...)."
  (let ((forms nil))
    (dolist (pre-args (get-precondition-args action state))
      (let ((pre-result (apply (action.pre-defun-name action) state pre-args)))
        (when pre-result
          (let ((updated-dbs
                  (if (eql pre-result t)
                      (funcall (action.eff-defun-name action) state)
                      (apply (action.eff-defun-name action) state pre-result))))
            (dolist (update updated-dbs)
              (let* ((inst (update.instantiations update))
                     (args inst))
                (push (cons (action.name action) args) forms)))))))
    (let ((uniq (remove-duplicates forms :test #'equal)))
      (sort uniq (lambda (x y)
                   (string< (prin1-to-string x)
                            (prin1-to-string y)))))))


(defun enum-applicable-action-forms (state actions)
  "Enumerate all applicable instantiated action forms for ACTIONS in STATE."
  (let ((forms nil))
    (dolist (a actions)
      (setf forms (nconc (enum-action-instantiated-forms a state) forms)))
    (let ((uniq (remove-duplicates forms :test #'equal)))
      (sort uniq (lambda (x y)
                   (string< (prin1-to-string x)
                            (prin1-to-string y)))))))


(defun enum-goal-reaching-last-actions (state goal-fn problem-actions selected-actions)
  "Return plists for one-step actions from STATE that satisfy GOAL-FN.
   Each plist has keys :ACTION and :GOAL-STATE."
  (let ((*actions* problem-actions)
        (hits nil))
    (dolist (action-form (enum-applicable-action-forms state selected-actions)
                         (nreverse hits))
      (multiple-value-bind (next-state success-p failure-reason)
          (apply-action-to-state action-form state nil nil)
        (declare (ignore failure-reason))
          (when (and success-p (funcall goal-fn next-state))
            (push (list :action action-form :goal-state next-state) hits))))))

(defun fps-state-enumeration-feasible-p (state)
  "Shared canonical state feasibility predicate for enumeration checks."
  (or (not (fboundp 'penultimate-state-feasible?))
      (funcall (symbol-function 'penultimate-state-feasible?) state)))


(defun fps-predecessor-state-feasible-p (state)
  "Feasibility predicate for predecessor enumeration.
   Uses PREDECESSOR-STATE-FEASIBLE? when provided; otherwise allows all states."
  (or (not (fboundp 'predecessor-state-feasible?))
      (funcall (symbol-function 'predecessor-state-feasible?) state)))


(defun fps-allowed-last-actions-from-state (state problem-actions selected-actions
                                            &key (direction :forward))
  "Shared last-action candidate API used by forward/backward predecessor flows."
  (ecase direction
    (:forward
     (let ((*actions* problem-actions))
       (enum-applicable-action-forms state selected-actions)))
    (:backward
     (let ((include-move (some (lambda (a)
                                 (enum-action-in-family-p (action.name a) 'move))
                               selected-actions))
           (include-pickup (some (lambda (a)
                                   (enum-action-in-family-p (action.name a) 'pickup))
                                 selected-actions))
           (include-connect (some (lambda (a)
                                   (enum-action-in-family-p (action.name a) 'connect))
                                 selected-actions)))
       (bw-candidate-last-actions state
                                  :include-move include-move
                                  :include-pickup include-pickup
                                  :include-connect include-connect)))))

(defun enum-state-base-props (state base-relations)
  "Return sorted base-relation propositions from STATE."
  (let ((rels (or base-relations (get-base-relations))))
    (sort
     (remove-if-not (lambda (p) (member (car p) rels :test #'eq))
                    (list-database (problem-state.idb state)))
     (lambda (x y)
       (string< (prin1-to-string x)
                (prin1-to-string y))))))


(defun fps-state-base-prop-key (state &optional base-relations)
  "Return canonical key for STATE from sorted base-relation propositions."
  (prin1-to-string (enum-state-base-props state base-relations)))


(defun fps-state-relation-signature-key (state relations)
  "Return canonical key for STATE projected to RELATIONS.
   When RELATIONS is NIL, returns NIL (no projection constraint)."
  (when relations
    (prin1-to-string (enum-state-base-props state relations))))


(defun fps-build-goal-signature-set (goal-states relations)
  "Build hash-set of canonical projected keys for GOAL-STATES over RELATIONS.
   Returns NIL when RELATIONS is NIL."
  (when relations
    (let ((set (make-hash-table :test 'equal)))
      (dolist (gs goal-states set)
        (setf (gethash (fps-state-relation-signature-key gs relations) set) t)))))


(defun fps-goal-signature-compatible-p (state relations goal-signature-set)
  "True iff STATE's RELATIONS projection is present in GOAL-SIGNATURE-SET.
   When RELATIONS is NIL, always true."
  (or (null relations)
      (and goal-signature-set
           (gethash (fps-state-relation-signature-key state relations)
                    goal-signature-set))))


(defun actions-modifiable-base-relations (actions)
  "Compute the set of base relation symbols modifiable by any action in ACTIONS.
   Intersects each action's EFFECT-ADDS with the current base-relation schema,
   then takes the union across all actions."
  (let ((base-rels (get-base-relations))
        (result nil))
    (dolist (a actions result)
      (dolist (rel (action.effect-adds a))
        (when (and (member rel base-rels :test #'eq)
                   (not (member rel result :test #'eq)))
          (push rel result))))))


(defun enum-proposition-holds-p (state proposition)
  "True iff PROPOSITION holds in STATE's idb."
  (let ((fluent-indices (get-prop-fluent-indices proposition)))
    (if fluent-indices
        (equal (fluent-value state (get-fluentless-prop proposition))
               (get-prop-fluents proposition))
        (gethash (convert-to-integer proposition)
                 (problem-state.idb state)))))


(defun enum-action-invariant-literals (goal-form base-relations modifiable-rels)
  "Return positive ground base-relation goal literals invariant under MODIFIABLE-RELS.
   A literal is invariant when its relation is a base relation NOT in MODIFIABLE-RELS."
  (let* ((literals (collect-positive-ground-literals goal-form))
         (base-literals (remove-if-not (lambda (lit)
                                         (member (car lit) base-relations :test #'eq))
                                       literals)))
    (remove-if (lambda (lit)
                 (member (car lit) modifiable-rels :test #'eq))
               base-literals)))


(defun make-goal-invariant-prefilter (invariants &optional (caller 'enumeration))
  "Build a prefilter enforcing INVARIANTS (list of ground literal lists).
   CALLER names the calling context (for log messages).
   Returns a function of (state) or NIL when INVARIANTS is empty."
  (when invariants
    (format t "~&[~(~A~)] Goal-invariant prefilter: ~S~%" caller invariants)
    (lambda (state)
      (every (lambda (lit) (enum-proposition-holds-p state lit))
             invariants))))


(defun compose-prefilters (pf1 pf2)
  "AND-compose two prefilter functions.  NIL means no filter."
  (cond
    ((and pf1 pf2) (lambda (state) (and (funcall pf1 state) (funcall pf2 state))))
    (pf1 pf1)
    (pf2 pf2)
    (t nil)))


(defun fps-normalize-predecessor-direction (direction)
  "Normalize DIRECTION to :BACKWARD or :FORWARD."
  (cond
    ((or (eq direction 'backward) (eq direction :backward)) :backward)
    ((or (eq direction 'forward) (eq direction :forward)) :forward)
    (t (error "FIND-PREDECESSORS: :DIRECTION must be BACKWARD or FORWARD; got ~S." direction))))


(defun fps-enumerate-all-base-configurations ()
  "Enumerate all feasible base configurations while preserving previously
   enumerated goal-state globals.  Disables symmetry pruning so that all
   concrete object namings are covered (required for forward-index key
   matching against non-canonical goal states).
   Intentionally does NOT apply installed goal/base prefilters; predecessor
   filtering is controlled separately (e.g., PREDECESSOR-STATE-FEASIBLE?)."
  (let ((saved-goal-states *enumerated-goal-states*)
        (saved-unique-solutions *enumerated-unique-solutions*)
        (*symmetry-pruning* nil))                                        ;; CHANGED
    (unwind-protect
        (enumerate-state (lambda (state) (declare (ignore state)) t)
                         :solution-type 'every
                         :skip-fixed-maps t
                         :propagate 'finalize-only
                         :prefilter nil)
      (setf *enumerated-goal-states* saved-goal-states
            *enumerated-unique-solutions* saved-unique-solutions))))


(defun fps-hash-table-alist-sorted (table)
  "Return TABLE as an alist sorted by key name."
  (let ((out nil))
    (maphash (lambda (k v) (push (cons k v) out)) table)
    (sort out (lambda (a b)
                (string< (symbol-name (car a))
                         (symbol-name (car b)))))))


(defun fps-expand-predecessor-layer-backward (frontier reachable base-relations
                                              problem-actions selected-actions)
  "Expand one predecessor layer using backward regression from FRONTIER.
   Returns (values layer-table raw-count feasible-count validated-count diagnostics)."
  (let ((stats (make-fps-layer-stats)))
    (iter (for target in frontier)
      (for target-key = (fps-state-base-prop-key target base-relations))
      (for candidates = (fps-allowed-last-actions-from-state
                         target problem-actions selected-actions
                         :direction :backward))
      (iter (for action-form in candidates)
        (iter (for pred0 in (bw-regress target action-form))
          (incf (fps-layer-stats-raw-count stats))
          (incf (gethash (car action-form) (fps-layer-stats-op-raw stats) 0))
          (let ((pred (copy-problem-state pred0)))
            (bw-normalize! pred)
            (when (fps-predecessor-state-feasible-p pred)
              (incf (fps-layer-stats-feasible-count stats))
              (incf (gethash (car action-form) (fps-layer-stats-op-feasible stats) 0))
              (let ((pred-key (fps-state-base-prop-key pred base-relations)))
                (cond
                  ((gethash pred-key reachable)
                   (incf (fps-layer-stats-collision-reachable-count stats)))
                  ((gethash pred-key (fps-layer-stats-layer-table stats))
                   (incf (fps-layer-stats-collision-layer-count stats)))
                  (t
                   (fps-validate-novel-predecessor
                    stats pred pred-key action-form target-key
                    base-relations reachable)))))))))
    (values (fps-layer-stats-layer-table stats)
            (fps-layer-stats-raw-count stats)
            (fps-layer-stats-feasible-count stats)
            (fps-layer-stats-validated-count stats)
            (fps-build-layer-diagnostics stats))))


(defun fps-validate-novel-predecessor (stats pred pred-key action-form target-key
                                       base-relations reachable)
  "Validate a novel predecessor candidate by forward action application.
   On success, checks key match and records the predecessor path in STATS.
   On failure, records diagnostic samples in STATS."
  (incf (fps-layer-stats-novel-validation-attempts stats))
  (incf (gethash (car action-form) (fps-layer-stats-op-attempts stats) 0))
  (multiple-value-bind (next-state success-p failure-reason)
      (apply-action-to-state action-form pred nil nil)
    (if success-p
        (progn
          (incf (fps-layer-stats-apply-success-count stats))
          (incf (gethash (car action-form) (fps-layer-stats-op-success stats) 0))
          (let ((next-key (fps-state-base-prop-key next-state base-relations)))
            (if (string= next-key target-key)
                (progn
                  (incf (fps-layer-stats-validated-count stats))
                  (let* ((target-entry (gethash target-key reachable))
                         (target-path (cdr target-entry))
                         (new-path (cons action-form target-path)))
                    (setf (gethash pred-key (fps-layer-stats-layer-table stats))
                          (cons pred new-path))))
                (incf (fps-layer-stats-key-mismatch-count stats)))))
        (progn
          (incf (fps-layer-stats-apply-fail-count stats))
          (fps-record-apply-failure stats action-form pred failure-reason
                                    pred-key target-key)))))


(defun fps-record-apply-failure (stats action-form pred failure-reason
                                 pred-key target-key)
  "Record a failed action application: per-operator failure count and first sample."
  (let ((op (car action-form)))
    (incf (gethash op (fps-layer-stats-op-fail stats) 0))
    (unless (gethash op (fps-layer-stats-op-sample-failure stats))
      (setf (gethash op (fps-layer-stats-op-sample-failure stats))
            failure-reason))
    (unless (gethash op (fps-layer-stats-op-sample-detail stats))
      (multiple-value-bind (agent-loc agent-loc-p)
          (if (and (second action-form) (symbolp (second action-form)))
              (fluent-value pred (list 'loc (second action-form)))
              (values nil nil))
        (setf (gethash op (fps-layer-stats-op-sample-detail stats))
              (list :action action-form
                    :failure failure-reason
                    :agent-loc (and agent-loc-p agent-loc)
                    :pred-key pred-key
                    :target-key target-key))))))


(defun fps-build-layer-diagnostics (stats)
  "Build the diagnostics plist from backward layer expansion statistics."
  (list :collision-reachable (fps-layer-stats-collision-reachable-count stats)
        :collision-layer (fps-layer-stats-collision-layer-count stats)
        :novel-validation-attempts (fps-layer-stats-novel-validation-attempts stats)
        :apply-success (fps-layer-stats-apply-success-count stats)
        :apply-fail (fps-layer-stats-apply-fail-count stats)
        :key-mismatch (fps-layer-stats-key-mismatch-count stats)
        :op-attempts (fps-hash-table-alist-sorted (fps-layer-stats-op-attempts stats))
        :op-success (fps-hash-table-alist-sorted (fps-layer-stats-op-success stats))
        :op-fail (fps-hash-table-alist-sorted (fps-layer-stats-op-fail stats))
        :op-sample-failure (fps-hash-table-alist-sorted (fps-layer-stats-op-sample-failure stats))
        :op-sample-detail (fps-hash-table-alist-sorted (fps-layer-stats-op-sample-detail stats))
        :op-raw (fps-hash-table-alist-sorted (fps-layer-stats-op-raw stats))
        :op-feasible (fps-hash-table-alist-sorted (fps-layer-stats-op-feasible stats))))


(defun fps-build-forward-predecessor-index (base-relations problem-actions selected-actions
                                            &key immutable-relations goal-signature-set)
  "Build inverse one-step transition index for forward predecessor expansion.
   Returns (values index state-count edge-count indexed-state-count
                   immutable-pruned-count feasible-pruned-count)."
  (let ((index (make-hash-table :test 'equal))
        (states (fps-enumerate-all-base-configurations))
        (state-count 0)
        (indexed-state-count 0)
        (immutable-pruned-count 0)
        (feasible-pruned-count 0)
        (edge-count 0))
    (let ((*actions* problem-actions))
      (dolist (pred states)
        (incf state-count)
        (cond
          ((not (fps-goal-signature-compatible-p pred immutable-relations goal-signature-set))
           (incf immutable-pruned-count))
          ((not (fps-predecessor-state-feasible-p pred))
           (incf feasible-pruned-count))
          (t
            (progn
              (incf indexed-state-count)
              (let ((pred-key (fps-state-base-prop-key pred base-relations)))
                (dolist (action-form (enum-applicable-action-forms pred selected-actions))
                  (multiple-value-bind (next-state success-p)
                      (apply-action-to-state action-form pred nil nil)
                    (when success-p
                      (let ((next-key (fps-state-base-prop-key next-state base-relations)))
                        (push (list pred-key pred action-form) (gethash next-key index))
                        (incf edge-count)))))))))))
    (values index state-count edge-count indexed-state-count
            immutable-pruned-count feasible-pruned-count)))


(defstruct (fps-forward-lazy-index (:constructor make-fps-forward-lazy-index ()))
  "Lazy forward predecessor cache keyed by successor base-prop key."
  (entries-by-successor-key (make-hash-table :test 'equal))
  (materialized-keys (make-hash-table :test 'equal))
  (candidate-predecessors nil)
  (selected-actions nil)
  (base-relations nil)
  (problem-actions nil)
  (edge-count 0 :type fixnum))


(defun fps-build-forward-predecessor-candidate-pool (base-relations
                                                     &key immutable-relations goal-signature-set)
  "Build forward predecessor candidate pool after immutable-signature filtering.
   Returns (values candidates state-count pool-count immutable-pruned)."
  (let ((states (fps-enumerate-all-base-configurations))
        (state-count 0)
        (immutable-pruned-count 0)
        (candidates nil))
    (dolist (pred states)
      (incf state-count)
      (cond
        ((not (fps-goal-signature-compatible-p pred immutable-relations goal-signature-set))
         (incf immutable-pruned-count))
        (t
         (push (cons (fps-state-base-prop-key pred base-relations) pred) candidates))))
    (let ((pool (nreverse candidates)))
      (values pool state-count (length pool) immutable-pruned-count))))


(defun fps-build-forward-predecessor-candidates (base-relations problem-actions
                                                 &key immutable-relations goal-signature-set)
  "Build filtered predecessor candidates for lazy forward expansion.
   Returns (values candidates state-count indexed-count immutable-pruned feasible-pruned)."
  (declare (ignore problem-actions))
  (multiple-value-bind (pool state-count pool-count immutable-pruned-count)
      (fps-build-forward-predecessor-candidate-pool
       base-relations
       :immutable-relations immutable-relations
       :goal-signature-set goal-signature-set)
    (declare (ignore pool-count))
    (let ((candidates nil)
          (feasible-pruned-count 0))
      (dolist (entry pool)
        (if (fps-predecessor-state-feasible-p (cdr entry))
            (push entry candidates)
            (incf feasible-pruned-count)))
      (let ((filtered (nreverse candidates)))
        (values filtered state-count (length filtered) immutable-pruned-count
                feasible-pruned-count)))))


(defun fps-resolve-prune-predicate (predicate)
  "Resolve PREDICATE to (values function display-name)."
  (cond
    ((functionp predicate)
     (values predicate '#:anonymous-function))
    ((and (symbolp predicate) (fboundp predicate))
     (values (symbol-function predicate) predicate))
    ((and (symbolp predicate) (not (fboundp predicate)))
     ;; Match FIND-PREDECESSORS semantics: missing predicate means no extra prune.
     (values (lambda (state) (declare (ignore state)) t) predicate))
    (t
     (error "ANALYZE-PREDECESSOR-PRUNE: :PREDICATE must be a function or a fbound symbol; got ~S."
            predicate))))


(defun analyze-predecessor-prune-fn (&key (predicate 'predecessor-state-feasible?)
                                          (action-families :auto)
                                          (sample-limit 10))
  "Analyze forward predecessor candidate pruning impact of PREDICATE.
   Stores report in (get 'analyze-predecessor-prune :last-report)."
  (unless (and (integerp sample-limit) (>= sample-limit 0))
    (error "ANALYZE-PREDECESSOR-PRUNE: :SAMPLE-LIMIT must be a non-negative integer; got ~S."
           sample-limit))
  (unless *enumerated-goal-states*
    (format t "~&[analyze-predecessor-prune] No goal states. Run (find-goal-states) first.~%")
    (return-from analyze-predecessor-prune-fn nil))
  (multiple-value-bind (predicate-fn predicate-name)
      (fps-resolve-prune-predicate predicate)
    (let* ((base-relations (get-base-relations))
           (problem-actions *actions*)
           (resolved-action-families (if (eq action-families :auto)
                                         (enum-default-action-families-from-actions problem-actions)
                                         action-families))
           (families (enum-normalize-action-families resolved-action-families))
           (selected-actions (enum-select-actions-by-family problem-actions families))
           (modifiable-rels (actions-modifiable-base-relations selected-actions))
           (immutable-rels (set-difference base-relations modifiable-rels :test #'eq))
           (goal-signature-set (fps-build-goal-signature-set *enumerated-goal-states*
                                                             immutable-rels)))
      (multiple-value-bind (pool state-count pool-count immutable-pruned-count)
          (fps-build-forward-predecessor-candidate-pool
           base-relations
           :immutable-relations immutable-rels
           :goal-signature-set goal-signature-set)
        (let ((kept-count 0)
              (rejected-count 0)
              (rejected-sample nil))
          (dolist (entry pool)
            (if (funcall predicate-fn (cdr entry))
                (incf kept-count)
                (progn
                  (incf rejected-count)
                  (when (< (length rejected-sample) sample-limit)
                    (push (car entry) rejected-sample)))))
          (let* ((report (list :predicate predicate-name
                               :action-families families
                               :selected-actions (mapcar #'action.name selected-actions)
                               :state-count state-count
                               :pool-count pool-count
                               :immutable-pruned immutable-pruned-count
                               :kept kept-count
                               :rejected rejected-count
                               :rejected-rate
                               (if (plusp pool-count)
                                   (/ (* 100.0 rejected-count) pool-count)
                                   0.0)
                               :rejected-sample-keys (nreverse rejected-sample))))
            (setf (get 'analyze-predecessor-prune :last-report) report)
            (format t "~&[analyze-predecessor-prune] Predicate ~S over forward pool: ~
                       states=~:D, pool=~:D, immutable-pruned=~:D, rejected=~:D (~,2F%%), kept=~:D.~%"
                    predicate-name state-count pool-count immutable-pruned-count
                    rejected-count (getf report :rejected-rate) kept-count)
            (when (getf report :rejected-sample-keys)
              (format t "~&[analyze-predecessor-prune] Sample rejected keys: ~S~%"
                      (getf report :rejected-sample-keys)))
            report))))))


(defun fps-forward-lazy-materialize-successor-keys (lazy-index target-keys)
  "Materialize predecessor entries in LAZY-INDEX for TARGET-KEYS not yet cached.
   Returns (values missing-key-count new-edge-count)."
  (let* ((materialized (fps-forward-lazy-index-materialized-keys lazy-index))
         (missing (remove-if (lambda (k) (gethash k materialized)) target-keys)))
    (if (null missing)
        (values 0 0)
        (let ((missing-set (make-hash-table :test 'equal))
              (new-edge-count 0)
              (entries (fps-forward-lazy-index-entries-by-successor-key lazy-index))
              (base-relations (fps-forward-lazy-index-base-relations lazy-index))
              (selected-actions (fps-forward-lazy-index-selected-actions lazy-index))
              (problem-actions (fps-forward-lazy-index-problem-actions lazy-index)))
          (dolist (k missing)
            (setf (gethash k missing-set) t))
          (let ((*actions* problem-actions))
            (dolist (candidate (fps-forward-lazy-index-candidate-predecessors lazy-index))
              (let ((pred-key (car candidate))
                    (pred (cdr candidate)))
                (dolist (action-form (enum-applicable-action-forms pred selected-actions))
                  (multiple-value-bind (next-state success-p)
                      (apply-action-to-state action-form pred nil nil)
                    (when success-p
                      (let ((next-key (fps-state-base-prop-key next-state base-relations)))
                        (when (gethash next-key missing-set)
                          (push (list pred-key pred action-form)
                                (gethash next-key entries))
                          (incf new-edge-count)))))))))
          (dolist (k missing)
            (setf (gethash k materialized) t))
          (incf (fps-forward-lazy-index-edge-count lazy-index) new-edge-count)
          (values (length missing) new-edge-count)))))


(defun fps-expand-predecessor-layer-forward-lazy (frontier-keys reachable lazy-index
                                                  direction layer)
  "Expand one predecessor layer from FRONTIER-KEYS using LAZY-INDEX cache."
  (multiple-value-bind (missing-count new-edge-count)
      (fps-forward-lazy-materialize-successor-keys lazy-index frontier-keys)
    (when (> missing-count 0)
      (format t "~&[find-predecessors ~S] Layer ~D lazy cache: materialized ~:D keys, +~:D edges (~:D total edges, ~:D successor keys cached).~%"
              direction layer missing-count new-edge-count
              (fps-forward-lazy-index-edge-count lazy-index)
              (hash-table-count (fps-forward-lazy-index-entries-by-successor-key lazy-index))))
    (fps-expand-predecessor-layer-forward-indexed
     frontier-keys reachable
     (fps-forward-lazy-index-entries-by-successor-key lazy-index))))


(defun fps-expand-predecessor-layer-forward-indexed (frontier-keys reachable forward-index)
  "Expand one predecessor layer from FRONTIER-KEYS using FORWARD-INDEX."
  (let ((layer-table (make-hash-table :test 'equal))
        (raw-count 0)
        (feasible-count 0)
        (validated-count 0))
    (dolist (target-key frontier-keys)
      (dolist (entry (gethash target-key forward-index))
        (let ((pred-key (first entry)))
          (unless (or (gethash pred-key reachable)
                      (gethash pred-key layer-table))
            (incf raw-count)
            (incf feasible-count)
            (incf validated-count)
            (let* ((pred (second entry))
                   (action-form (third entry))
                   (target-entry (gethash target-key reachable))
                   (new-path (cons action-form (cdr target-entry))))
              (setf (gethash pred-key layer-table) (cons pred new-path)))))))
    (values layer-table raw-count feasible-count validated-count)))


(defun fps-print-layer-diagnostics (direction layer diagnostics)
  "Print backward-layer diagnostic details when DIAGNOSTICS is non-nil
   and DIRECTION is :BACKWARD."
  (when (and (eq direction :backward) diagnostics)
    (format t "~&[find-predecessors ~S] Layer ~D diagnostics: ~
               collisions reachable=~:D, collisions layer=~:D, ~
               novel validations=~:D, apply success=~:D, apply fail=~:D, key mismatch=~:D.~%"
            direction layer
            (getf diagnostics :collision-reachable)
            (getf diagnostics :collision-layer)
            (getf diagnostics :novel-validation-attempts)
            (getf diagnostics :apply-success)
            (getf diagnostics :apply-fail)
            (getf diagnostics :key-mismatch))
    (format t "~&[find-predecessors ~S] Layer ~D diagnostics (ops): attempts=~S success=~S fail=~S.~%"
            direction layer
            (getf diagnostics :op-attempts)
            (getf diagnostics :op-success)
            (getf diagnostics :op-fail))
    (format t "~&[find-predecessors ~S] Layer ~D diagnostics (op coverage): raw=~S feasible=~S.~%"
            direction layer
            (getf diagnostics :op-raw)
            (getf diagnostics :op-feasible))
    (format t "~&[find-predecessors ~S] Layer ~D diagnostics (sample failures): ~S~%"
            direction layer
            (getf diagnostics :op-sample-failure))
    (format t "~&[find-predecessors ~S] Layer ~D diagnostics (sample details): ~S~%"
            direction layer
            (getf diagnostics :op-sample-detail))))


(defun fps-complete-layer (layer-table reachable start-time norm-direction layer
                           raw-count feasible-count validated-count diagnostics)
  "Merge LAYER-TABLE into REACHABLE, print layer summary and diagnostics,
   update *BACKWARD-REACHABLE-SET*.  Returns the count of newly added states."
  (let ((new-count 0))
    (maphash (lambda (key entry)
               (unless (gethash key reachable)
                 (setf (gethash key reachable) entry)
                 (incf new-count)))
             layer-table)
    (let ((elapsed (/ (- (get-internal-real-time) start-time)
                      internal-time-units-per-second)))
      (format t "~&[find-predecessors ~S] Layer ~D: ~:D raw, ~:D feasible, ~
                 ~:D validated, ~:D new unique (~:D total). ~,1Fs.~%"
              norm-direction layer raw-count feasible-count validated-count
              new-count (hash-table-count reachable) elapsed))
    (fps-print-layer-diagnostics norm-direction layer diagnostics)
    (setf *backward-reachable-set* reachable)
    new-count))


(defun find-predecessors-fn (&key (action-families :auto)
                                  (direction 'forward))
  "Interactive iterative predecessor enumeration from *ENUMERATED-GOAL-STATES*.
   DIRECTION BACKWARD regresses from the current frontier.
   DIRECTION FORWARD builds a one-step inverse transition index, then expands
   by reachable successor-key frontiers.
   Prompts (y-or-n-p) after each layer. Ctrl-C safely reverts to the last
   completed layer."
  (unless *enumerated-goal-states*
    (format t "~&[find-predecessors] No goal states. Run (find-goal-states) first.~%")
    (return-from find-predecessors-fn nil))
  (when (or (> *debug* 0) *probe*)
    (format t "~&[find-predecessors] Please reset *debug* and *probe* first.~%")
    (return-from find-predecessors-fn nil))
  (let* ((norm-direction (fps-normalize-predecessor-direction direction))
         (base-relations (get-base-relations))
         (problem-actions *actions*)
         (resolved-action-families (if (eq action-families :auto)
                                       (enum-default-action-families-from-actions problem-actions)
                                       action-families))
         (families (enum-normalize-action-families resolved-action-families))
         (selected-actions (enum-select-actions-by-family problem-actions families))
         (modifiable-rels (actions-modifiable-base-relations selected-actions))
         (immutable-rels (set-difference base-relations modifiable-rels :test #'eq))
         (goal-signature-set (fps-build-goal-signature-set *enumerated-goal-states*
                                                           immutable-rels))
         (reachable (ecase norm-direction
                      (:backward (make-hash-table :test 'equal))
                      (:forward
                       (if (and *backward-reachable-set*
                                (> (hash-table-count *backward-reachable-set*) 0))
                           *backward-reachable-set*
                           (make-hash-table :test 'equal)))))
         (frontier nil)
         (forward-lazy-index nil)
         (forward-frontier-keys nil)
         (layer 0))
    (ecase norm-direction
      (:backward
       ;; Seed layer 0: goal states with empty action paths
       (dolist (gs *enumerated-goal-states*)
         (let ((key (fps-state-base-prop-key gs base-relations)))
           (unless (gethash key reachable)
             (setf (gethash key reachable) (cons gs nil)))))
       (setf *backward-reachable-set* reachable)
       (format t "~&[find-predecessors ~S] Layer 0 (goals): ~:D unique states seeded.~%"
               norm-direction
               (hash-table-count reachable))
       ;; Initial frontier is all seeded goal states
       (setf frontier (loop for entry being the hash-values of reachable
                            collect (car entry))))
      (:forward
       (unless *enumerated-goal-states*
         (format t "~&[find-predecessors ~S] No goal states available. Run (find-goal-states) first.~%"
                 norm-direction)
         (return-from find-predecessors-fn nil))
       (when (zerop (hash-table-count reachable))
         (dolist (gs *enumerated-goal-states*)
           (let ((key (fps-state-base-prop-key gs base-relations)))
             (unless (gethash key reachable)
               (setf (gethash key reachable) (cons gs nil)))))
         (setf *backward-reachable-set* reachable)
         (format t "~&[find-predecessors ~S] Layer 0 (goals): ~:D unique states seeded.~%"
                 norm-direction (hash-table-count reachable)))
       (format t "~&[find-predecessors ~S] Starting from ~:D reachable states.~%"
               norm-direction (hash-table-count reachable))
       (format t "~&[find-predecessors ~S] Building forward predecessor index...~%"
               norm-direction)
       (if immutable-rels
           (format t "~&[find-predecessors ~S] Immutable-signature pruning enabled over relations ~S (goal signatures=~:D).~%"
                   norm-direction immutable-rels (hash-table-count goal-signature-set))
           (format t "~&[find-predecessors ~S] Immutable-signature pruning disabled (selected actions can modify all base relations).~%"
                   norm-direction))
       (multiple-value-bind (candidates state-count indexed-count immutable-pruned feasible-pruned)
           (fps-build-forward-predecessor-candidates base-relations problem-actions
                                                     :immutable-relations immutable-rels
                                                     :goal-signature-set goal-signature-set)
         (setf forward-lazy-index (make-fps-forward-lazy-index))
         (setf (fps-forward-lazy-index-candidate-predecessors forward-lazy-index) candidates
               (fps-forward-lazy-index-selected-actions forward-lazy-index) selected-actions
               (fps-forward-lazy-index-base-relations forward-lazy-index) base-relations
               (fps-forward-lazy-index-problem-actions forward-lazy-index) problem-actions)
         (format t "~&[find-predecessors ~S] Forward lazy cache initialized: ~:D states (~:D candidates, ~:D immutable-pruned, ~:D feasible-pruned).~%"
                 norm-direction state-count indexed-count immutable-pruned feasible-pruned))
       (setf forward-frontier-keys
             (loop for key being the hash-keys of reachable collect key))))
    ;; Iterative expansion
    (loop
      (incf layer)
      (ecase norm-direction
        (:backward
         (format t "~&[find-predecessors ~S] Computing layer ~D from ~:D frontier states...~%"
                 norm-direction layer (length frontier)))
        (:forward
         (format t "~&[find-predecessors ~S] Computing layer ~D from ~:D frontier keys...~%"
                 norm-direction layer (length forward-frontier-keys))))
      (let ((layer-table (make-hash-table :test 'equal))
            (raw-count 0)
            (feasible-count 0)
            (validated-count 0)
            (diagnostics nil)
            (start-time (get-internal-real-time))
            (interrupted nil))
        (handler-case
            (multiple-value-setq (layer-table raw-count feasible-count validated-count diagnostics)
              (ecase norm-direction
                (:backward
                 (fps-expand-predecessor-layer-backward frontier reachable base-relations
                                                        problem-actions selected-actions))
                (:forward
                 (let ((result
                         (multiple-value-list
                          (fps-expand-predecessor-layer-forward-lazy forward-frontier-keys
                                                                     reachable
                                                                     forward-lazy-index
                                                                     norm-direction
                                                                     layer))))
                   (values (first result)
                           (second result)
                           (third result)
                           (fourth result)
                           nil)))))
          (serious-condition (c)
            (setf interrupted t)
            (format t "~&[find-predecessors ~S] Layer ~D interrupted: ~A~%"
                    norm-direction layer c)
            (format t "~&[find-predecessors ~S] Keeping ~:D states from ~D completed layers.~%"
                    norm-direction
                    (hash-table-count reachable) (1- layer))))
        (when interrupted
          (return))
        ;; Layer completed — merge, report, check saturation
        (let ((new-count (fps-complete-layer layer-table reachable start-time
                                             norm-direction layer
                                             raw-count feasible-count validated-count
                                             diagnostics)))
          (when (zerop new-count)
            (format t "~&[find-predecessors ~S] Saturated - no new states at layer ~D.~%"
                    norm-direction layer)
            (return))
          (ecase norm-direction
            (:backward
             (setf frontier (loop for entry being the hash-values of layer-table
                                  collect (car entry))))
            (:forward
             (setf forward-frontier-keys
                   (loop for key being the hash-keys of layer-table collect key))))
          (unless (y-or-n-p "Continue to layer ~D? " (1+ layer))
            (return)))))
    (format t "~&[find-predecessors ~S] Done. *backward-reachable-set* contains ~:D states.~%"
            norm-direction
            (hash-table-count *backward-reachable-set*))
    t))


(defun fps-meeting-point-entry-for-state (state &optional
                                                (reachable-set *backward-reachable-set*)
                                                (base-relations (get-base-relations)))
  "Return the reachable-set entry (STATE . ACTION-PATH) for STATE, or NIL."
  (gethash (fps-state-base-prop-key state base-relations) reachable-set))


(defun install-meeting-point-goal (&key
                                     (reachable-set *backward-reachable-set*)
                                     (base-relations (get-base-relations))
                                     (min-backward-depth 0))
  "Install GOAL-FN as backward-reachable-set membership over base-prop keys."
  (unless (and reachable-set (> (hash-table-count reachable-set) 0))
    (error "INSTALL-MEETING-POINT-GOAL: *BACKWARD-REACHABLE-SET* is empty. Run FIND-PREDECESSORS first."))
  (unless (and (integerp min-backward-depth) (>= min-backward-depth 0))
    (error "INSTALL-MEETING-POINT-GOAL: :MIN-BACKWARD-DEPTH must be a non-negative integer; got ~S."
           min-backward-depth))
  (let ((captured-set reachable-set)
        (captured-rels base-relations)
        (captured-min-depth min-backward-depth))
    (setf (symbol-function 'goal-fn)
          (lambda (state)
            (let ((entry (fps-meeting-point-entry-for-state state captured-set captured-rels)))
              (and entry (>= (length (cdr entry)) captured-min-depth)))))
    (setf (get 'goal-fn :form)
          (list 'meeting-point-goal
                :target-count (hash-table-count captured-set)
                :min-backward-depth captured-min-depth
                :base-relations captured-rels))
    t))


(defun ww-solve-meeting-point (&key
                                 (depth-cutoff 12)
                                 (solution-type 'first)
                                 (min-backward-depth 0))
  "Run WW-SOLVE with GOAL-FN set to backward-reachable-set membership."
  (let ((saved-depth-cutoff *depth-cutoff*)
        (saved-solution-type *solution-type*)
        (saved-goal-form (get 'goal-fn :form))
        (saved-goal-fn-def (and (fboundp 'goal-fn) (symbol-function 'goal-fn))))
    (unwind-protect
        (progn
          (install-meeting-point-goal :min-backward-depth min-backward-depth)
          (setf *depth-cutoff* depth-cutoff
                *solution-type* solution-type)
          (ww-solve)
          *solutions*)
      (setf *depth-cutoff* saved-depth-cutoff
            *solution-type* saved-solution-type)
      (setf (get 'goal-fn :form) saved-goal-form)
      (when saved-goal-fn-def
        (setf (symbol-function 'goal-fn) saved-goal-fn-def)))))


(defun fgs-normalize-solution-type (x)
  "Validate and normalize SOLUTION-TYPE for find-goal-states UI.
   Accepts FIRST, EVERY, or a positive integer N. Invalid values default to EVERY."
  (cond
    ((eq x 'first) 'first)
    ((eq x 'every) 'every)
    ((integerp x)
     (if (plusp x)
         x
         (progn
           (format t "~&[find-goal-states] SOLUTION-TYPE integer must be > 0; got ~S. Using EVERY.~%" x)
           'every)))
    (t
     (format t "~&[find-goal-states] SOLUTION-TYPE must be FIRST, EVERY, or integer N; got ~S. Using EVERY.~%" x)
     'every)))


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
                                       (prefilter nil) (goal-form nil)
                                       (skip-fixed-maps nil)
                                       (canonical-dedupe nil))
  "Enumerate compatible goal states via CSP-based variable assignment.
   GOAL-SPEC may be a goal form (including quantifiers) or a partial goal-state
   shorthand ((p ...) (q ...)).
   SOLUTION-TYPE may be any existing solver mode (FIRST, EVERY, MIN-LENGTH, etc.).
   If SOLUTION-TYPE is a positive integer N, return N goal states (truncated).
   PROPAGATE: T — propagate after every action (default).
              :FINALIZE-ONLY — skip intermediate propagation, propagate at finalize.
              NIL — no propagation (raw leaf collection).
   PREFILTER: when non-nil, prunes base states before propagation.
   GOAL-FORM: when non-nil, overrides the goal form used for CSP action generation
              pruning (e.g., goal-fixed-fluent constraints).  The actual leaf acceptance
              test is still driven by GOAL-SPEC.
   SKIP-FIXED-MAPS: when T, disable goal-fixed-fluent pruning in CSP generation.
   CANONICAL-DEDUPE: when T, dedupe finalized states by canonical base key."
  (unless (get-base-relations)
    (error "ENUMERATE-STATE: no base relations declared.  ~
            Declare relations with DEFINE-BASE-RELATION (or use DEFINE-BASE-RELATIONS)."))
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
                       :prefilter prefilter
                       :goal-form goal-form
                       :skip-fixed-maps skip-fixed-maps
                       :canonical-dedupe canonical-dedupe))


(defun enumerate-state-csp (goal-spec &key (algorithm *algorithm*) (solution-type 'first)
                                           (default-max-per-key 4) (propagate t)
                                           (prefilter nil) (goal-form nil)
                                           (skip-fixed-maps nil)
                                           (canonical-dedupe nil))
  "Enumerate compatible states via a CSP-style, generated enum-action sequence.
   SOLUTION-TYPE:
   - any existing solver mode (FIRST, EVERY, MIN-LENGTH, etc.)
   - or a positive integer N meaning: stop after recording N solutions.
   PROPAGATE: T — propagate after every action (default).
              :FINALIZE-ONLY — skip intermediate propagation, propagate at finalize.
              NIL — no propagation (raw leaf collection).
   PREFILTER: when non-nil, a function of one argument (state) applied at
   ENUM-FINALIZE to prune states before propagation.
   GOAL-FORM: when non-nil, overrides the derived goal form for CSP action
              generation pruning (goal-fixed-fluent constraints).
   SKIP-FIXED-MAPS: when T, disable goal-fixed-fluent pruning in CSP generation.
   CANONICAL-DEDUPE: when T, dedupe finalized states by canonical base key."
  (multiple-value-bind (norm-goal-spec derived-goal-form)
      (enum-normalize-goal-spec goal-spec)
    (let* ((*symmetry-pruning* (if *enum-csp-enforce-no-runtime-symmetry-pruning*
                                   nil
                                   *symmetry-pruning*))
           (*enum-residual-symmetry-enabled*
            (if *enum-csp-enforce-no-runtime-symmetry-pruning*
                nil
                *enum-residual-symmetry-enabled*))
           (effective-goal-form (or goal-form derived-goal-form))
           (requested-solution-type solution-type)
           (solver-solution-type requested-solution-type)
           (gf (coerce-goal norm-goal-spec))
           (enum-actions (generate-enum-actions
                          :goal-form effective-goal-form
                          :default-max-per-key default-max-per-key
                          :propagate propagate
                          :prefilter prefilter
                          :skip-fixed-maps skip-fixed-maps))
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
           (saved-goal-fn-def (when (fboundp 'goal-fn) (symbol-function 'goal-fn)))
           (saved-disabled-hooks nil))
      ;; Validate integer N here too (defensive).
      (when (integerp requested-solution-type)
        (unless (plusp requested-solution-type)
          (error "ENUMERATE-STATE-CSP: integer SOLUTION-TYPE must be a positive N; got ~S"
                 requested-solution-type)))
      (unwind-protect
          (progn
            (setf (symbol-function 'goal-fn) leaf-goal)
            ;; Disable normal planner hooks that are unsafe for partial CSP states.
            (dolist (sym *enumerator-disabled-search-hook-symbols*)
              (when (fboundp sym)
                (push (cons sym (symbol-function sym)) saved-disabled-hooks)
                (fmakunbound sym)))

            (setf *enumerator-actions* enum-actions)
            (setf *enumerator-action-settings*
                  (list :keyword-settings *enumerator-ui-settings*
                        :internal-settings
                        (list :algorithm algorithm
                              :solution-type requested-solution-type
                              :default-max-per-key default-max-per-key
                              :propagate propagate
                              :canonical-dedupe canonical-dedupe
                              :symmetry-pruning *symmetry-pruning*
                              :residual-symmetry-pruning *enum-residual-symmetry-enabled*
                              :prefilter (if prefilter t nil))))
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
                              uniq)))
              (if canonical-dedupe
                  (multiple-value-bind (dedup-uniq dedup-states dropped)
                      (enum-dedupe-solutions-by-base-key uniq2)
                    (when (> dropped 0)
                      (format t "~&[enumerate-state-csp] canonical dedupe dropped ~D duplicate states.~%"
                              dropped))
                    (setf *enumerated-unique-solutions* dedup-uniq
                          *enumerated-goal-states* dedup-states)
                    dedup-states)
                  (let ((states (mapcar #'solution.goal uniq2)))
                    (setf *enumerated-unique-solutions* uniq2
                          *enumerated-goal-states* states)
                    states))))
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
          (setf (symbol-function 'goal-fn) saved-goal-fn-def))
        (dolist (entry saved-disabled-hooks)
          (setf (symbol-function (car entry)) (cdr entry)))))))


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


(defun enum-solution-state-key (state &optional (groups *enumerator-detected-groups*))
  "Return stable dedupe key for finalized STATE.
   Uses canonical base key when GROUPS are present, otherwise concrete base key."
  (if groups
      (prin1-to-string (enum-canonical-base-key state groups))
      (fps-state-base-prop-key state (get-base-relations))))


(defun enum-dedupe-solutions-by-base-key (solutions &optional (groups *enumerator-detected-groups*))
  "Stable dedupe of SOLUTIONS by full base-state key.
   Returns three values: deduped-solution-list, deduped-state-list, dropped-count."
  (let ((seen (make-hash-table :test #'equal))
        (dedup-solutions nil)
        (dedup-states nil)
        (dropped 0))
    (dolist (sol solutions)
      (let* ((st (solution.goal sol))
             (key (enum-solution-state-key st groups)))
        (if (gethash key seen)
            (incf dropped)
            (progn
              (setf (gethash key seen) t)
              (push sol dedup-solutions)
              (push st dedup-states)))))
    (values (nreverse dedup-solutions) (nreverse dedup-states) dropped)))


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


(defun enum-make-state-from-propositions (propositions)
  "Build a fresh PROBLEM-STATE from readable proposition list PROPOSITIONS."
  (let ((st (make-problem-state
             :idb (make-hash-table :test #'eql :synchronized (> *threads* 0))
             :hidb (make-hash-table :test #'eql :synchronized (> *threads* 0)))))
    (dolist (p propositions)
      (when (consp p)
        (add-proposition p (problem-state.idb st))))
    (setf (problem-state.idb-hash st) nil)
    st))


(defun enum-goal-compare-key-from-state (state &optional (groups *enumerator-detected-groups*))
  "Return comparison key for STATE.
   Uses canonical base key when interchangeable GROUPS are available;
   otherwise uses concrete base-proposition key."
  (if groups
      (prin1-to-string (enum-canonical-base-key state groups))
      (fps-state-base-prop-key state (get-base-relations))))


(defun enum-goal-unpruned-cache-signature (&optional goal-form
                                           (base-relations (get-base-relations))
                                           (groups *enumerator-detected-groups*))
  "Build signature for lazy unpruned canonical goal cache."
  (list :goal goal-form
        :base-relations base-relations
        :groups groups))


(defun enum-build-unpruned-goal-canonical-cache (&optional goal-form)
  "Build unpruned canonical-key cache for goal enumeration and store globals.
   Returns (values counts-table first-state-table signature)."
  (let* ((goal-form (or goal-form
                        (getf (get 'find-goal-states :last-report) :goal)
                        'goal-fn))
         (base-relations (get-base-relations))
         (groups *enumerator-detected-groups*)
         (signature (enum-goal-unpruned-cache-signature goal-form base-relations groups))
         (counts (make-hash-table :test 'equal))
         (first-state (make-hash-table :test 'equal))
         (saved-goal-states *enumerated-goal-states*)
         (saved-unique-solutions *enumerated-unique-solutions*)
         (saved-groups *enumerator-detected-groups*)
         (saved-symmetric-relations *enumerator-detected-symmetric-relations*))
    (multiple-value-bind (norm-goal-spec normalized-goal-form)
        (enum-normalize-goal-spec goal-form)
      (let* ((effective-prefilter (get-prefilter))
             (modifiable-rels (actions-modifiable-base-relations *actions*))
             (invariants (enum-action-invariant-literals
                          normalized-goal-form base-relations modifiable-rels))
             (invariant-prefilter (make-goal-invariant-prefilter
                                   invariants 'known-goal-state-enumerated-p))
             (combined-prefilter (compose-prefilters
                                  effective-prefilter invariant-prefilter)))
        (unwind-protect
            (let ((*symmetry-pruning* nil)
                  (*enumerator-base-relations* base-relations)
                  (*enumerator-ui-settings* nil))
              (dolist (st (enumerate-state norm-goal-spec
                                           :algorithm *algorithm*
                                           :solution-type 'every
                                           :propagate 'finalize-only
                                           :prefilter combined-prefilter
                                           :canonical-dedupe nil))
                (let ((key (enum-goal-compare-key-from-state st groups)))
                  (incf (gethash key counts 0))
                  (unless (gethash key first-state)
                    (setf (gethash key first-state) st))))
          (setf *enumerated-goal-states* saved-goal-states
                *enumerated-unique-solutions* saved-unique-solutions
                *enumerator-detected-groups* saved-groups
                *enumerator-detected-symmetric-relations* saved-symmetric-relations)))))
    (setf *enumerated-goal-unpruned-canonical-counts* counts
          *enumerated-goal-unpruned-canonical-first-state* first-state
          *enumerated-goal-unpruned-canonical-signature* signature)
    (values counts first-state signature)))


(defun enum-ensure-unpruned-goal-canonical-cache (&optional goal-form)
  "Ensure lazy unpruned canonical cache matches current goal/base/groups signature."
  (let ((signature (enum-goal-unpruned-cache-signature
                    (or goal-form
                        (getf (get 'find-goal-states :last-report) :goal)
                        'goal-fn))))
    (if (and *enumerated-goal-unpruned-canonical-counts*
             *enumerated-goal-unpruned-canonical-first-state*
             (equal signature *enumerated-goal-unpruned-canonical-signature*))
        (values *enumerated-goal-unpruned-canonical-counts*
                *enumerated-goal-unpruned-canonical-first-state*
                signature)
        (enum-build-unpruned-goal-canonical-cache goal-form))))


(defun known-goal-state-enumerated-p (&optional
                                      (known-goal-state
                                        (and (boundp '*known-goal-state*)
                                             *known-goal-state*))
                                      (enumerated-goal-states *enumerated-goal-states*))
  "Return five values:
   1) T iff KNOWN-GOAL-STATE appears in the canonical goal universe.
   2) The first matching PROBLEM-STATE, or NIL when not found.
   3) Number of matches found.
   4) List of all matching PROBLEM-STATE objects from the pruned enumeration set.
   5) Match source keyword: :PRUNED-CANONICAL, :UNPRUNED-CANONICAL-UNIVERSE, or :NONE."
  (let* ((known-state (enum-make-state-from-propositions known-goal-state))
         (known-key (enum-goal-compare-key-from-state known-state)))
    (let ((matches nil))
      (dolist (st enumerated-goal-states)
        (let ((st-key (enum-goal-compare-key-from-state st)))
          (when (string= known-key st-key)
            (push st matches))))
      (setf matches (nreverse matches))
      (if matches
          (values t
                  (first matches)
                  (length matches)
                  matches
                  :pruned-canonical)
          (if *enumerator-detected-groups*
              (multiple-value-bind (counts first-state-table)
                  (enum-ensure-unpruned-goal-canonical-cache)
                (let ((count (gethash known-key counts 0)))
                  (if (> count 0)
                      (values t
                              (gethash known-key first-state-table)
                              count
                              nil
                              :unpruned-canonical-universe)
                      (values nil nil 0 nil :none))))
              (values nil nil 0 nil :none))))))


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
    (let ((keyword-settings (getf *enumerator-action-settings* :keyword-settings))
          (internal-settings (getf *enumerator-action-settings* :internal-settings)))
      (cond
        (keyword-settings
         (format t "~&Settings: ~S~%" keyword-settings)
         (format t "~&Enumerator settings: ~S~%" internal-settings))
        (t
         (format t "~&Settings: ~S~%" *enumerator-action-settings*))))
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
      (get problem :enumerator-base-relations)
      *enum-relation-order*
      (get problem :enum-relation-order)))


;;;; ----------------------------------------------------------------------
;;;; Enum relation metadata
;;;; ----------------------------------------------------------------------


(defun install-enum-relation-meta (relation-spec keys)
  "Install enumeration metadata for RELATION-SPEC.
   RELATION-SPEC: RELATION symbol or (RELATION ?arg1 ?arg2 ...).
   KEYS: property list using DEFINE-BASE-RELATION keys."
  (multiple-value-bind (relation relation-vars)
      (enum-parse-relation-spec relation-spec)
    (unless (gethash relation *relations*)
      (error "DEFINE-BASE-RELATION ~S: relation is not installed in *RELATIONS*." relation))
    (let ((plist (enum-canonicalize-meta-keys relation relation-vars keys)))
      (enum-validate-relation-meta relation plist)
      (setf (gethash relation *enum-relation-metadata*) plist)
      (unless (member relation *enum-relation-order* :test #'eq)
        (setf *enum-relation-order* (append *enum-relation-order* (list relation))))
    (when (and (boundp '*problem-name*) *problem-name*)
      (let ((tbl (or (get *problem-name* :enum-relation-metadata)
                     (let ((h (make-hash-table :test 'eq)))
                       (setf (get *problem-name* :enum-relation-metadata) h)
                       h))))
        (setf (gethash relation tbl) plist))
      (let ((rels (get *problem-name* :enum-relation-order)))
        (unless (member relation rels :test #'eq)
          (setf (get *problem-name* :enum-relation-order)
                (append rels (list relation))))))
      (format t "~&Installing enumerator metadata for ~S...~%" relation)
      relation)))


(defun enum-parse-relation-spec (relation-spec)
  "Parse RELATION-SPEC and return (values relation relation-vars)."
  (cond
    ((symbolp relation-spec)
     (values relation-spec nil))
    ((consp relation-spec)
     (let ((relation (first relation-spec))
           (vars (rest relation-spec)))
       (unless (symbolp relation)
         (error "DEFINE-BASE-RELATION: relation head must start with a symbol: ~S"
                relation-spec))
       (values relation vars)))
    (t
     (error "DEFINE-BASE-RELATION: invalid relation spec: ~S" relation-spec))))


(defun enum-check-even-plist (keys)
  "Signal an error when KEYS is not an even-length plist."
  (unless (evenp (length keys))
    (error "DEFINE-BASE-RELATION: metadata keys must be an even-length plist: ~S" keys)))


(defun enum-build-requires-lambda (relation relation-vars form)
  "Build DSL FORM into a lambda form (state key partner) for RELATION.
   The resulting form is compiled later by COMPILE-ALL-FUNCTIONS."
  (let* ((new-$vars (delete-duplicates
                     (get-all-nonspecial-vars #'$varp form)))
         (v1 (first relation-vars))
         (v2 (second relation-vars))
         (lambda-form
          `(lambda (state key partner)
             (declare (ignorable state key partner))
             (block ,(intern (format nil "ENUM-~A-REQUIRES" relation) *package*)
               (let ((,v1 key)
                     (,v2 partner)
                     ,@new-$vars)
                 (declare (ignorable ,v1 ,v2 ,@new-$vars))
                 ,(translate form 'pre))))))
    (fix-if-ignore '(state key partner) lambda-form)
    lambda-form))


(defun enum-canonicalize-meta-keys (relation relation-vars keys)
  "Canonicalize and normalize KEYS plist for RELATION."
  (enum-check-even-plist keys)
  (let ((allowed-keys '(:pattern :allow-unassigned :early-keys :on-assign
                        :key-types :max-unassigned :max-per :requires)))
    (loop for tail on keys by #'cddr
          for k = (first tail) do
      (unless (member k allowed-keys :test #'eq)
        (error "DEFINE-BASE-RELATION ~S: unsupported metadata key ~S." relation k))))
  (when (or (getf keys :max-per-key)
            (getf keys :requires-fluent)
            (getf keys :partner-feasible))
    (error "DEFINE-BASE-RELATION ~S: old subset keys (:MAX-PER-KEY/:REQUIRES-FLUENT/:PARTNER-FEASIBLE) are no longer supported."
           relation))
  (let ((plist (copy-list keys)))
    (let ((max-per (getf plist :max-per)))
      (when max-per
        (unless (and relation-vars
                     (= (length relation-vars) 2))
          (error "DEFINE-BASE-RELATION ~S: :MAX-PER requires relation head vars, e.g. (~S ?arg1 ?arg2)."
                 relation relation))
        (unless (and (consp max-per)
                     (= (length max-per) 2)
                     (symbolp (first max-per))
                     (integerp (second max-per))
                     (plusp (second max-per)))
          (error "DEFINE-BASE-RELATION ~S: :MAX-PER must be (?arg positive-integer); got ~S"
                 relation max-per))
        (unless (eq (first max-per) (first relation-vars))
          (error "DEFINE-BASE-RELATION ~S: :MAX-PER arg must be the first relation arg (~S); got ~S"
                 relation (first relation-vars) (first max-per)))
        (remf plist :max-per)
        (setf (getf plist :max-per-key) (second max-per))))
    (let ((requires (getf plist :requires)))
      (when requires
        (unless (and relation-vars
                     (= (length relation-vars) 2))
          (error "DEFINE-BASE-RELATION ~S: :REQUIRES requires relation head vars, e.g. (~S ?arg1 ?arg2)."
                 relation relation))
        (remf plist :requires)
        (setf (getf plist :requires-lambda)
              (enum-build-requires-lambda relation relation-vars requires))))
    plist))


(defun enum-infer-pattern (relation)
  "Infer enumeration pattern for RELATION from installed relation metadata.
   Relations with fluent positions are :FLUENT; otherwise they are :SUBSET."
  (if (relation-fluent-indices relation) :fluent :subset))


(defun enum-validate-relation-meta (relation plist)
  "Validate enumeration metadata PLIST for RELATION.  Signals error on invalid entries."
  (let* ((explicit-pattern (getf plist :pattern))
        (pattern (or explicit-pattern (enum-infer-pattern relation)))
        (allow-unassigned (getf plist :allow-unassigned))
        (early-keys (getf plist :early-keys))
        (on-assign (getf plist :on-assign))
        (max-per-key (getf plist :max-per-key))
        (max-unassigned (getf plist :max-unassigned)))
    (when (and explicit-pattern
               (not (member explicit-pattern '(:fluent :subset))))
      (error "DEFINE-BASE-RELATION ~S: :PATTERN must be :FLUENT or :SUBSET; got ~S"
             relation explicit-pattern))
    (when allow-unassigned
      (unless (or (eq allow-unassigned t)
                  (and (consp allow-unassigned)
                       (eql (car allow-unassigned) :types)
                       (every #'symbolp (cdr allow-unassigned))))
        (error "DEFINE-BASE-RELATION ~S: :ALLOW-UNASSIGNED must be T or (:TYPES type...); got ~S"
               relation allow-unassigned)))
    (when early-keys
      (unless (and (consp early-keys)
                   (eql (car early-keys) :types)
                   (every #'symbolp (cdr early-keys)))
        (error "DEFINE-BASE-RELATION ~S: :EARLY-KEYS must be (:TYPES type...); got ~S"
               relation early-keys)))
    (when on-assign
      (unless (and (listp on-assign)
                   (every (lambda (entry)
                            (and (consp entry)
                                 (symbolp (first entry))
                                 (= (length entry) 2)))
                          on-assign))
        (error "DEFINE-BASE-RELATION ~S: :ON-ASSIGN must be ((rel val)...); got ~S"
               relation on-assign)))
    (when max-per-key
      (unless (and (integerp max-per-key) (plusp max-per-key))
        (error "DEFINE-BASE-RELATION ~S: :MAX-PER value must be a positive integer; got ~S"
               relation max-per-key)))
    (when max-unassigned
      (unless (eq pattern :fluent)
        (error "DEFINE-BASE-RELATION ~S: :MAX-UNASSIGNED only valid for :FLUENT pattern; got :PATTERN ~S"
               relation pattern))
      (unless (and (consp max-unassigned)
                   (eql (car max-unassigned) :types)
                   (>= (length max-unassigned) 3)
                   (every #'symbolp (butlast (cdr max-unassigned)))
                   (let ((n (car (last max-unassigned))))
                     (and (integerp n) (>= n 0))))
        (error "DEFINE-BASE-RELATION ~S: :MAX-UNASSIGNED must be (:TYPES type... non-negative-integer); got ~S"
               relation max-unassigned)))
    (let ((key-types (getf plist :key-types)))
      (when key-types
        (unless (and (consp key-types)
                     (eql (car key-types) :types)
                     (every #'symbolp (cdr key-types)))
          (error "DEFINE-BASE-RELATION ~S: :KEY-TYPES must be (:TYPES type...); got ~S"
                 relation key-types))))
    (let ((requires-lambda (getf plist :requires-lambda)))
      (when requires-lambda
        (unless (eq pattern :subset)
          (error "DEFINE-BASE-RELATION ~S: :REQUIRES only valid for :SUBSET pattern; got :PATTERN ~S"
                 relation pattern))
        (unless (and (consp requires-lambda)
                     (eq (car requires-lambda) 'lambda))
          (error "DEFINE-BASE-RELATION ~S: internal requires lambda is malformed; got ~S"
                 relation requires-lambda))))))


(defun enum-relation-meta (rel)
  "Return the enum metadata plist for REL, or NIL if none declared."
  (or (gethash rel *enum-relation-metadata*)
      (let ((tbl (and (boundp '*problem-name*) *problem-name*
                      (get *problem-name* :enum-relation-metadata))))
        (and tbl (gethash rel tbl)))))


(defun enum-pattern (rel)
  "Return the enumeration pattern for REL.
   Uses explicit :PATTERN metadata when provided; otherwise infers from REL."
  (let ((meta (enum-relation-meta rel)))
    (or (and meta (getf meta :pattern))
        (enum-infer-pattern rel))))


(defun enum-allow-unassigned (rel)
  "Return the :ALLOW-UNASSIGNED spec for REL, or NIL."
  (getf (enum-relation-meta rel) :allow-unassigned))


(defun enum-early-keys (rel)
  "Return the :EARLY-KEYS spec for REL, or NIL."
  (getf (enum-relation-meta rel) :early-keys))


(defun enum-on-assign (rel)
  "Return the :ON-ASSIGN list for REL, or NIL."
  (getf (enum-relation-meta rel) :on-assign))


(defun enum-max-per-key (rel)
  "Return the :MAX-PER-KEY integer for REL, or NIL."
  (getf (enum-relation-meta rel) :max-per-key))


(defun enum-max-unassigned (rel)
  "Return the :MAX-UNASSIGNED spec for REL, or NIL."
  (getf (enum-relation-meta rel) :max-unassigned))


(defun enum-key-types (rel)
  "Return the :KEY-TYPES spec for REL, or NIL."
  (getf (enum-relation-meta rel) :key-types))


(defun enum-requires-predicate (rel)
  "Return the compiled :REQUIRES predicate for REL, or NIL."
  (let ((meta (enum-relation-meta rel)))
    (or (getf meta :requires-predicate)
        (let ((lam (getf meta :requires-lambda)))
          (when lam
            ;; Fallback for callers that use enumeration without a prior
            ;; COMPILE-ALL-FUNCTIONS pass.
            (let ((fn (compile nil (subst-int-code lam))))
              (setf (getf meta :requires-predicate) fn)
              fn))))))


(defun compile-enum-relation-requires-predicates ()
  "Compile and install :REQUIRES lambdas for enum relation metadata."
  (labels ((compile-meta-table (table)
             (when table
               (maphash
                (lambda (rel meta)
                  (declare (ignore rel))
                  (let ((lam (and meta (getf meta :requires-lambda))))
                    (when lam
                      (setf (getf meta :requires-predicate)
                            (compile nil (subst-int-code lam))))))
                table))))
    (compile-meta-table *enum-relation-metadata*)
    (when (and (boundp '*problem-name*) *problem-name*)
      (compile-meta-table (get *problem-name* :enum-relation-metadata*)))))


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


(defun enum-object-matches-types-p (obj type-list)
  "Return T if OBJ is an instance of one of TYPE-LIST."
  (some (lambda (ty)
          (member obj (maybe-type-instances ty) :test #'eq))
        type-list))


(defun enum-assignment-unassigned-count (assignment type-list)
  "Count (:UNASSIGNED) choices in ASSIGNMENT for objects matching TYPE-LIST."
  (count-if (lambda (pair)
              (and (enum-object-matches-types-p (car pair) type-list)
                   (equal (cdr pair) '(:unassigned))))
            assignment))


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
  (setf *enum-relation-order* nil)
  (when (and (boundp '*problem-name*) *problem-name*)
    (setf (get *problem-name* :enum-relation-metadata) nil)
    (setf (get *problem-name* :enum-relation-order) nil))
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
      (apply #'append (mapcar #'maybe-type-instances (cdr type-spec)))
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


(defun canonical-value-assignments (objects value-tuples)
  "Generate canonical (sorted) assignments for symmetric OBJECTS over VALUE-TUPLES.
   Returns a list of assignment alists, each mapping object -> tuple.
   Canonical means the tuple sequence is non-decreasing by tuple index."
  (let* ((n (length objects))
         (multisets (multisets-with-repetition value-tuples n)))
    (mapcar (lambda (tuple-selection)
              (mapcar #'cons objects tuple-selection))
            multisets)))


(defun enum-assignment-respects-fixed-p (assignment fixed-table)
  "Return T if ASSIGNMENT respects all fixed constraints in FIXED-TABLE.
   ASSIGNMENT is an alist of (object . tuple) pairs.
   FIXED-TABLE maps (object) keys to fluent tuples."
  (every (lambda (pair)
           (let* ((obj (car pair))
                  (vals (cdr pair))
                  (key (list obj))
                  (fixed (and fixed-table (gethash key fixed-table))))
             (or (null fixed)
                 (equal vals fixed))))
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


(defun enum-make-fluent-action (rel key-args allowed-value-tuples
                                &key (propagate t))
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


(defun enum-make-symmetric-batch-action (rel sym-type objects value-tuples
                                         &key (propagate t) (fixed-maps nil)
                                         (max-unassigned-spec nil))
  "Create a single enum action that assigns values to all symmetric OBJECTS at once.
   Uses canonical (sorted) multiset assignments to avoid enumerating symmetric permutations.
   REL is the relation being assigned.
   SYM-TYPE is the symmetry group type name (for action naming).
   OBJECTS is the list of symmetric objects.
   VALUE-TUPLES is the list of possible fluent tuples.
   When REL's :ALLOW-UNASSIGNED metadata applies, (:UNASSIGNED) is appended to the domain.
   FIXED-MAPS: when provided, filters assignments to respect goal-fixed constraints.
   Returns a single action with one instantiation per canonical assignment."
  (let* ((name (intern (format nil "ENUM-~A-~AS" rel sym-type) *package*))
         (allow-unassigned (some (lambda (obj)
                                   (enum-key-allows-unassigned-p rel (list obj)))
                                 objects))
         (effective-domain (if allow-unassigned
                               (append value-tuples (list '(:unassigned)))
                               value-tuples))
         (all-assignments (canonical-value-assignments objects effective-domain))
         (max-unassigned-types (and max-unassigned-spec
                                    (butlast (cdr max-unassigned-spec))))
         (max-unassigned-cap (and max-unassigned-spec
                                  (car (last max-unassigned-spec))))
         (cap-filtered-assignments (if max-unassigned-spec
                                       (remove-if-not
                                        (lambda (asgn)
                                          (<= (enum-assignment-unassigned-count asgn max-unassigned-types)
                                              max-unassigned-cap))
                                        all-assignments)
                                       all-assignments))
         (fixed (and fixed-maps (gethash rel fixed-maps)))
         (assignments (if fixed
                          (remove-if-not
                           (lambda (asgn)
                             (enum-assignment-respects-fixed-p asgn fixed))
                           cap-filtered-assignments)
                          cap-filtered-assignments))
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
     ;; Effect: bind REL for each assigned object; skip (:UNASSIGNED)
     (lambda (state assignment)
       (let ((s (copy-state state)))
         (dolist (pair assignment)
           (let ((obj (car pair))
                 (vals (cdr pair)))
             (unless (equal vals '(:unassigned))
               (update (problem-state.idb s)
                       (assemble-proposition rel (list obj) vals))
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
  "True iff adding CHOSEN-SET to KEY does not violate KEY's per-key cap for REL.
   Existing symmetric duplicates are ignored (partner sets are de-duplicated).
   Note: this intentionally does NOT cap partner objects that appear in REL's
   second argument position."
  (let* ((existing (enum-key-partners state rel key partners))
         (new (set-difference chosen-set existing :test #'eq)))
    (declare (ignore keys))
    (<= (+ (length existing) (length new)) max-per-key)))


(defun enum-collect-fluent-relations (relations)
  "Return unary-key fluent relations from RELATIONS.
   A relation qualifies when:
   - its enum pattern is :FLUENT,
   - it has fluent indices, and
   - it has exactly one non-fluent (key) position."
  (remove-if-not (lambda (r)
                   (and (eq (enum-pattern r) :fluent)
                        (relation-fluent-indices r)
                        (= (length (relation-key-type-specs r)) 1)))
                 relations))


(defun enum-all-installed-relations ()
  "Return all installed relation symbols."
  (let (rels)
    (maphash (lambda (r sig)
               (declare (ignore sig))
               (push r rels))
             *relations*)
    rels))


(defun enum-object-matches-type-spec-p (obj type-spec)
  "Return T when OBJ is an instance of TYPE-SPEC (symbol or (either ...))."
  (member obj (expand-type-spec-instances* type-spec) :test #'eq))


(defun enum-group-predecessors-map (keys interchangeable-groups)
  "Build hash table mapping each key to its preceding same-group members.
   Only keys belonging to an interchangeable group get entries.
   Predecessors are in execution order (matching KEYS list order)."
  (let ((map (make-hash-table :test 'eq)))
    (dolist (group interchangeable-groups map)
      (let ((group-keys (remove-if-not (lambda (obj) (member obj group :test #'eq))
                                       keys)))
        (loop for tail on group-keys
              for k = (car tail)
              for preds = (ldiff group-keys tail)
              when preds
              do (setf (gethash k map) preds))))))


(defun enum-objects-residually-equivalent-p (state obj1 obj2 fluent-relations)
  "True iff OBJ1 and OBJ2 have identical fluent values across all FLUENT-RELATIONS.
   Both unbound counts as equivalent for a given relation.
   Relations where OBJ1/OBJ2 are not valid key instances are ignored."
  (every (lambda (rel)
           (let ((key-spec (first (relation-key-type-specs rel))))
             (if (and key-spec
                      (enum-object-matches-type-spec-p obj1 key-spec)
                      (enum-object-matches-type-spec-p obj2 key-spec))
                 (multiple-value-bind (v1 p1) (fluent-value state (list rel obj1))
                   (multiple-value-bind (v2 p2) (fluent-value state (list rel obj2))
                     (and (eq (not (null p1)) (not (null p2)))
                          (or (not p1) (equal v1 v2)))))
                 t)))
         fluent-relations))


(defun enum-canonical-predecessor (state key predecessors fluent-relations)
  "Find the latest member of PREDECESSORS residually equivalent to KEY in STATE.
   Returns the predecessor symbol, or NIL if none is equivalent."
  (loop for pred in (reverse predecessors)
        when (enum-objects-residually-equivalent-p state key pred fluent-relations)
        return pred))


(defun enum-subset-lex<= (set1 set2)
  "True iff sorted SET1 is lexicographically <= sorted SET2 under
   proposition-level ordering.  Empty sets are maximal (a connector with
   no partners contributes no propositions, sorting after all partners).
   On prefix match the longer set is smaller (more paired-props sort earlier)."
  (cond
    ((and (null set1) (null set2)) t)
    ((null set1) nil)
    ((null set2) t)
    (t (loop for a in set1
             for b in set2
             do (cond ((string< (symbol-name a) (symbol-name b)) (return t))
                      ((string> (symbol-name a) (symbol-name b)) (return nil)))
             finally (return (>= (length set1) (length set2)))))))


(defun enum-filter-group-members (partner-set keys)
  "Remove members of KEYS from PARTNER-SET to obtain the external-partner subset.
   When interchangeable group members appear in each other's partner sets,
   the raw lex comparison is not invariant under renaming.  Filtering to
   external partners restores a stable comparison."
  (remove-if (lambda (p) (member p keys :test #'eq)) partner-set))


(defun enum-residual-symmetry-ok-p (state rel key chosen-set
                                    predecessors-map fluent-relations
                                    keys partners)
  "True iff CHOSEN-SET respects residual symmetry ordering for KEY.
   If KEY has a residually-equivalent canonical predecessor, KEY's external
   partner set (group members removed) must be lexicographically >= the
   predecessor's external partner set."
  (or (null *enum-residual-symmetry-enabled*)
      (let ((preds (and predecessors-map (gethash key predecessors-map))))
        (or (null preds)
            (null fluent-relations)
            (let ((pred (enum-canonical-predecessor state key preds fluent-relations)))
              (or (null pred)
                  ;; Same-area connectors are not truly interchangeable due to
                  ;; connectable constraint + fixed activation order in propagation.
                  ;; Skip pruning when key and predecessor share the same loc.
                  (multiple-value-bind (pred-loc pred-bound-p)
                      (fluent-value state (list 'loc pred))
                    (multiple-value-bind (key-loc key-bound-p)
                        (fluent-value state (list 'loc key))
                      (and pred-bound-p key-bound-p (equal pred-loc key-loc))))
                  (let* ((pred-ext (sort (copy-list (enum-filter-group-members
                                                     (enum-key-partners state rel pred partners)
                                                     keys))
                                         #'string< :key #'symbol-name))
                         ;; KEY may already have partners introduced by earlier keys
                         ;; in symmetric subset relations. Compare against KEY's
                         ;; complete post-action external partner set.
                         (key-ext (sort
                                   (copy-list
                                    (enum-filter-group-members
                                     (union (enum-key-partners state rel key partners)
                                            chosen-set
                                            :test #'eq)
                                     keys))
                                   #'string< :key #'symbol-name))
                         ;; When predecessor has no external partners after filtering,
                         ;; internal partner structure (filtered out above) may still
                         ;; determine canonical orientation.  In that case, do not
                         ;; prune on external-lex order alone.
                         (ok (or (null pred-ext)
                                 (enum-subset-lex<= pred-ext key-ext))))
                    ok)))))))


(defun enum-all-permutations (lst)
  "Generate all permutations of LST."
  (if (null lst)
      (list nil)
      (mapcan (lambda (x)
                (mapcar (lambda (p) (cons x p))
                        (enum-all-permutations (remove x lst :count 1))))
              lst)))


(defun enum-substitute-in-form (form alist)
  "Recursively substitute symbols in FORM according to ALIST."
  (cond
    ((null form) nil)
    ((symbolp form) (or (cdr (assoc form alist :test #'eq)) form))
    ((consp form) (cons (enum-substitute-in-form (car form) alist)
                        (enum-substitute-in-form (cdr form) alist)))
    (t form)))


(defun enum-canonical-base-key (state groups)
  "Compute canonical base-proposition key for STATE under symmetry GROUPS.
   Uses only base relations (loc, paired) to avoid beam-name artifacts.
   Returns a sorted proposition list that is the lex minimum over all
   group permutations.  Handles multiple disjoint groups sequentially."
  (let* ((base-rels (get-base-relations))
         (props (remove-if-not
                 (lambda (p) (member (car p) base-rels :test #'eq))
                 (list-database (problem-state.idb state))))
         (current (sort (copy-list props) #'string< :key #'prin1-to-string)))
    (dolist (group groups current)
      (let ((best current))
        (dolist (perm (enum-all-permutations group))
          (let* ((alist (mapcar #'cons group perm))
                 (subst (mapcar (lambda (p) (enum-substitute-in-form p alist))
                                current))
                 (sorted (sort (copy-list subst) #'string< :key #'prin1-to-string)))
            (when (string< (prin1-to-string sorted) (prin1-to-string best))
              (setf best sorted))))
        (setf current best)))))


(defun enum-count-canonical-uniques (&optional (states *enumerated-goal-states*)
                                               (groups *enumerator-detected-groups*))
  "Count unique canonical base-proposition forms among STATES.
   Reports raw count, canonical unique count, and duplicate count."
  (let ((seen (make-hash-table :test #'equal))
        (unique-count 0)
        (total (length states)))
    (dolist (state states)
      (let ((key (prin1-to-string (enum-canonical-base-key state groups))))
        (unless (gethash key seen)
          (setf (gethash key seen) t)
          (incf unique-count))))
    (format t "~&[canonical-uniques] ~D total states, ~D canonical uniques, ~D duplicates~%"
            total unique-count (- total unique-count))
    unique-count))


(defun enum-connector-external-partners (state key group)
  "Return sorted external partners of KEY under PAIRED in STATE.
   External means not a member of GROUP (the interchangeable connector group).
   Used by diagnostic functions to reconstruct what residual symmetry would see."
  (let ((partners nil))
    (dolist (prop (list-database (problem-state.idb state)))
      (when (eq (car prop) 'paired)
        (cond ((eq (second prop) key)
               (pushnew (third prop) partners :test #'eq))
              ((eq (third prop) key)
               (pushnew (second prop) partners :test #'eq)))))
    (sort (remove-if (lambda (p) (member p group :test #'eq)) partners)
          #'string< :key #'symbol-name)))


(defun enum-simulate-would-prune-p (state keys group)
  "Simulate residual symmetry pruning on final STATE.
   Checks only loc equivalence (the only fluent assigned during base enumeration).
   Returns T if any key's subset assignment would have been rejected."
  (let ((k1 (first keys)) (k2 (second keys)) (k3 (third keys)))
    (multiple-value-bind (loc1 p1) (fluent-value state (list 'loc k1))
      (multiple-value-bind (loc2 p2) (fluent-value state (list 'loc k2))
        (multiple-value-bind (loc3 p3) (fluent-value state (list 'loc k3))
          (let ((ext1 (enum-connector-external-partners state k1 group))
                (ext2 (enum-connector-external-partners state k2 group))
                (ext3 (enum-connector-external-partners state k3 group)))
            ;; c2 vs c1: must check when same loc (both bound to same value,
            ;; or both unbound)
            (when (and (eq (not (null p1)) (not (null p2)))
                       (or (and (not p1) (not p2))
                           (and p1 p2 (equal loc1 loc2))))
              (unless (or (null ext1) (enum-subset-lex<= ext1 ext2))
                (return-from enum-simulate-would-prune-p
                  (values t :c2-vs-c1 ext1 ext2))))
            ;; c3: find latest equivalent predecessor
            (let ((pred (cond
                          ((and (eq (not (null p2)) (not (null p3)))
                                (or (and (not p2) (not p3))
                                    (and p2 p3 (equal loc2 loc3))))
                           k2)
                          ((and (eq (not (null p1)) (not (null p3)))
                                (or (and (not p1) (not p3))
                                    (and p1 p3 (equal loc1 loc3))))
                           k1)
                          (t nil))))
              (when pred
                (let ((ext-pred (if (eq pred k1) ext1 ext2)))
                  (unless (or (null ext-pred) (enum-subset-lex<= ext-pred ext3))
                    (return-from enum-simulate-would-prune-p
                      (values t :c3-vs pred ext-pred ext3)))))))))))
  nil)


(defun enum-diagnose-over-pruning (&optional (states *enumerated-goal-states*)
                                              (groups *enumerator-detected-groups*)
                                              (max-examples 10))
  "Find canonical keys that lose ALL representatives to residual symmetry pruning.
   Prints summary and detailed examples of over-pruned equivalence classes."
  (let ((keys '(connector1 connector2 connector3))
        (group (first groups))
        (canonical-table (make-hash-table :test #'equal))
        (kept-keys (make-hash-table :test #'equal))
        (over-pruned nil)
        (example-count 0))
    ;; Pass 1: group states by canonical key
    (dolist (state states)
      (let ((ckey (prin1-to-string (enum-canonical-base-key state groups))))
        (push state (gethash ckey canonical-table))))
    ;; Pass 2: for each state, check if it would survive pruning
    (dolist (state states)
      (unless (enum-simulate-would-prune-p state keys group)
        (let ((ckey (prin1-to-string (enum-canonical-base-key state groups))))
          (setf (gethash ckey kept-keys) t))))
    ;; Pass 3: find canonical keys with no survivor
    (maphash (lambda (ckey state-list)
               (unless (gethash ckey kept-keys)
                 (push (cons ckey state-list) over-pruned)))
             canonical-table)
    ;; Report
    (format t "~&[diagnose] Total canonical keys: ~D~%" (hash-table-count canonical-table))
    (format t "~&[diagnose] Canonical keys with at least one survivor: ~D~%"
            (hash-table-count kept-keys))
    (format t "~&[diagnose] Over-pruned canonical keys: ~D~%" (length over-pruned))
    ;; Detailed examples
    (dolist (entry over-pruned)
      (when (>= example-count max-examples) (return))
      (incf example-count)
      (format t "~&~%--- Over-pruned class ~D (size ~D) ---~%" example-count (length (cdr entry)))
      (dolist (state (cdr entry))
        (let ((base-rels (get-base-relations)))
          (format t "~&  State base props:~%")
          (dolist (p (sort (remove-if-not
                           (lambda (p) (member (car p) base-rels :test #'eq))
                           (list-database (problem-state.idb state)))
                          #'string< :key #'prin1-to-string))
            (format t "    ~S~%" p)))
        ;; Show simulation detail
        (multiple-value-bind (pruned-p reason a b)
            (enum-simulate-would-prune-p state keys group)
          (format t "  Simulated prune: ~A  reason=~S  ~S vs ~S~%"
                  pruned-p reason a b))))
    (length over-pruned)))


(defun enum-count-and-save-canonical-keys (filename &key (goal-spec 'goal-fn)
                                                          (pruned t)
                                                          (max-per-key 4))
  "Enumerate goal states, count raw and canonical uniques, save canonical keys to file.
   No states are retained in memory.  PRUNED controls *enum-residual-symmetry-enabled*.
   FILENAME: path to write canonical keys (one per line).
   Returns: (values raw-count canonical-count)."
  (let* ((real-goal-fn (coerce-goal goal-spec))
         (seen (make-hash-table :test #'equal))
         (raw 0)
         (counting-goal
          (lambda (state)
            (when (and (eql (problem-state.name state) 'enum-finalize)
                       (funcall real-goal-fn state))
              (incf raw)
              (let ((ckey (prin1-to-string
                           (enum-canonical-base-key state *enumerator-detected-groups*))))
                (setf (gethash ckey seen) t))
              nil))))
    (setf *enum-residual-symmetry-enabled* pruned)
    (format t "~&[count] Running with pruning=~A...~%" pruned)
    (enumerate-state counting-goal
                     :algorithm *algorithm*
                     :solution-type 'every
                     :default-max-per-key max-per-key
                     :propagate 'finalize-only
                     :prefilter (get-prefilter))
    (let ((canonical (hash-table-count seen)))
      (format t "~&[count] Raw: ~D  Canonical: ~D  Duplicates: ~D~%"
              raw canonical (- raw canonical))
      (with-open-file (out filename :direction :output :if-exists :supersede)
        (maphash (lambda (k v)
                   (declare (ignore v))
                   (write-line k out))
                 seen))
      (format t "~&[count] Saved ~D canonical keys to ~A~%" canonical filename)
      (values raw canonical))))


(defun enum-compare-canonical-key-files (file1 file2)
  "Compare two canonical-key files.  Reports missing and extra keys.
   FILE1 is the reference (unpruned), FILE2 is the test (pruned)."
  (let ((ref (make-hash-table :test #'equal))
        (test (make-hash-table :test #'equal)))
    (with-open-file (in file1 :direction :input)
      (loop for line = (read-line in nil nil)
            while line do (setf (gethash line ref) t)))
    (with-open-file (in file2 :direction :input)
      (loop for line = (read-line in nil nil)
            while line do (setf (gethash line test) t)))
    (let ((missing 0) (extra 0))
      (maphash (lambda (k v) (declare (ignore v))
                 (unless (gethash k test) (incf missing)))
               ref)
      (maphash (lambda (k v) (declare (ignore v))
                 (unless (gethash k ref) (incf extra)))
               test)
      (format t "~&[compare] Reference keys: ~D  Test keys: ~D~%"
              (hash-table-count ref) (hash-table-count test))
      (format t "~&[compare] Missing from test: ~D  Extra in test: ~D~%"
              missing extra)
      (format t "~&[compare] Sound: ~A  Complete: ~A~%"
              (zerop missing) (zerop extra))
      (values missing extra))))


(defun enum-make-subset-actions (rel keys partners max-per-key
                                 &key (propagate t) (symmetric-relation-p nil)
                                      (requires-predicate nil)
                                      (predecessors-map nil) (fluent-relations nil))
  "Create subset-enumeration actions for REL with per-key subset cap.
   For each key, generates an action whose instantiations are all subsets (up to
   MAX-PER-KEY) of the key's allowed partners.
   When SYMMETRIC-RELATION-P, excludes lower-indexed keys from pairing targets.
   When REQUIRES-PREDICATE is non-nil, each partner in a proposed subset must
   satisfy (funcall requires-predicate state key partner)."
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
                  (and (member set sets :test #'equal)
                       (or (null requires-predicate)
                           (every (lambda (p)
                                    (funcall requires-predicate state k p))
                                  set))
                       (enum-subset-degree-ok-p state rel k set keys partners max-per-key)
                       (enum-residual-symmetry-ok-p state rel k set
                                                    predecessors-map fluent-relations
                                                    keys partners)
                       (list set)))
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
         (when (funcall prefilter state) t))  ;; normalize to T for generate-children
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
     *SYMMETRY-PRUNING* — interchangeable groups use canonical multisets"
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
                ;; Symmetric batch for interchangeable groups.
                (when *symmetry-pruning*
                  (dolist (group interchangeable-groups)
                    (let ((group-in-keys (remove-if-not
                                          (lambda (obj)
                                            (member (list obj) o-keys :test #'equal))
                                          group)))
                      (when (> (length group-in-keys) 1)
                        (let ((stype (or (find-group-type group) 'symmetric))
                              (value-tuples vtuples))
                          (push (enum-make-symmetric-batch-action
                                 r stype group-in-keys value-tuples
                                 :propagate propagate
                                 :fixed-maps fixed-maps
                                 :max-unassigned-spec (enum-max-unassigned r))
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


(defun enum-compute-context (goal-form &key skip-fixed-maps)
  "Compute context plist for enum action generation.
   Gathers schema metadata, type domains, goal analysis, and fixed-maps.
   SKIP-FIXED-MAPS: when T, leave :FIXED-MAPS empty while still using GOAL-FORM
   for interchangeable-group/symmetry detection.
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
    (unless skip-fixed-maps
      (dolist (r relations)
        (when (and (relation-signature r) (relation-fluent-indices r))
          (setf (gethash r fixed-maps)
                (goal-fixed-fluent-assignments goal-form r)))))
    (list :schema schema
          :focus-objs focus-objs
          :focus-set focus-set
          :relations relations
          :goal-form goal-form
          :symmetric-relations symmetric-relations
          :interchangeable-groups interchangeable-groups
          :sym-objects sym-objects
          :fixed-maps fixed-maps)))


(defun enum-key-subset-count (key keys partners max-per-key
                              &key symmetric-relation-p)
  "Count the number of subset instantiations for KEY given current KEYS order.
   Used to sort keys fail-first before building predecessor maps."
  (let ((allowed (enum-allowed-partners-for key keys partners
                                            :symmetric-relation-p symmetric-relation-p)))
    (length (subsets-up-to allowed max-per-key))))


(defun enum-sort-keys-fail-first (keys partners max-per-key
                                  &key symmetric-relation-p)
  "Sort KEYS by ascending instantiation count (fail-first heuristic).
   Counts are estimated from the original KEYS order.  The resulting
   execution order is used for predecessor-map construction so that
   residual symmetry pruning is consistent with actual CSP execution."
  (let ((counts (mapcar (lambda (k)
                          (cons k (enum-key-subset-count
                                   k keys partners max-per-key
                                   :symmetric-relation-p symmetric-relation-p)))
                        keys)))
    (mapcar #'car (stable-sort counts #'< :key #'cdr))))


(defun enum-generate-subset-actions (ctx default-max-per-key propagate)
  "Generate subset-enumeration actions for all :SUBSET-pattern relations.
   For each relation, derives keys and partners from the signature and metadata.
   Keys are sorted fail-first before branch generation.
   Residual symmetry pruning metadata is only built when explicitly enabled.
   When a relation is auto-detected as symmetric, applies index-based deduplication.
   When :REQUIRES metadata is declared, builds a branch-time feasibility
   predicate and threads it into subset preconditions.
   Returns a list of actions in execution order (no post-hoc sort needed)."
  (let ((relations (getf ctx :relations))
        (symmetric-relations (getf ctx :symmetric-relations))
        (interchangeable-groups (getf ctx :interchangeable-groups))
        (residual-symmetry-enabled-p (not (null *enum-residual-symmetry-enabled*)))
        ;; Used only by optional residual symmetry checks.
        (fluent-relations (when *enum-residual-symmetry-enabled*
                            (enum-collect-fluent-relations
                             (enum-all-installed-relations))))
        (all-actions nil))
    (dolist (r relations)
      (when (eq (enum-pattern r) :subset)
        (let* ((sig (relation-signature r))
               (key-type-spec (first sig))
               (partner-type-spec (second sig))
               (all-key-instances (expand-type-spec-instances* key-type-spec))
               (key-types-spec (enum-key-types r))
               (unsorted-keys (if key-types-spec
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
               (sym-rel-p (member r symmetric-relations :test #'eq))
               (keys (enum-sort-keys-fail-first unsorted-keys partners max-k  ;; CHANGED: sort first
                                                :symmetric-relation-p sym-rel-p))
               (requires-predicate (unless *enum-disable-csp-metadata-pruning*
                                     (enum-requires-predicate r)))
               (preds-map (when residual-symmetry-enabled-p
                           (enum-group-predecessors-map keys interchangeable-groups)))
               (actions (enum-make-subset-actions
                         r keys partners max-k
                         :propagate propagate
                         :symmetric-relation-p sym-rel-p
                         :requires-predicate requires-predicate
                         :predecessors-map preds-map
                         :fluent-relations fluent-relations)))
          ;; Actions are already in execution order — no post-hoc sort.
          (format t "~&[subset-actions] rel=~S keys=~S~%" r keys)
          (when (and preds-map
                     (> (hash-table-count preds-map) 0))
            (format t "~&[subset-actions] predecessors-map:~%")
            (maphash (lambda (k v) (format t "  ~S -> ~S~%" k v)) preds-map))
          (setf all-actions (nconc all-actions actions)))))
    all-actions))


(defun generate-enum-actions (&key (goal-form nil) (default-max-per-key 4)
                                   (propagate t) (prefilter nil)
                                   (skip-fixed-maps nil))
  "Generate a CSP enum-action sequence driven by declared base relations,
   enum-relation metadata, symmetry groups, and prefilter.
   Orchestrates metadata-driven generators and assembles final action list.
   Phases:
   1. Early fluent actions (:EARLY-KEYS from metadata, for pruning)
   2. Main fluent actions (*SYMMETRY-PRUNING* batch + individual, with :UNASSIGNED)
   3. Subset enumeration for :SUBSET-pattern relations (fail-first ordering)
   4. ENUM-FINALIZE (triggers propagation, applies prefilter)
   PROPAGATE: T — propagate after every action (default).
              :FINALIZE-ONLY — skip intermediate propagation, propagate at finalize.
              NIL — no propagation anywhere (raw leaf collection).
   PREFILTER: when non-nil, a function applied at ENUM-FINALIZE to prune states
     before propagation.
   SKIP-FIXED-MAPS: when T, disable goal-fixed-fluent pruning while retaining
     goal-driven symmetry detection."
  (let* ((intermediate-propagate (eq propagate t))
         (finalize-propagate (not (null propagate)))
         (ctx (enum-compute-context goal-form :skip-fixed-maps skip-fixed-maps)))
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

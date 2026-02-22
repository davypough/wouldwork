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
;;;   6) FIND-PENULTIMATE-STATES — user-facing entry point for one-step
;;;      predecessor enumeration with candidate-goal transition summaries.
;;;   7) GENERATE-ENUM-ACTIONS — data-driven generator that creates a CSP
;;;      enum-action sequence from declared base relations, auto-detected
;;;      symmetries, prefilters, and branch-time feasibility pruning.
;;;   8) Branch-time feasibility pruning via :PARTNER-FEASIBLE metadata,
;;;      which references existing problem-spec query functions to prune
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


(defparameter *enumerated-penultimate-states* nil
  "After FIND-PENULTIMATE-STATES, holds PROBLEM-STATE objects accepted as penultimate states.")


(defparameter *enumerated-penultimate-results* nil
  "After FIND-PENULTIMATE-STATES, holds per-state reaching-action result plists.")

(defparameter *penultimate-equivalence-cache* (make-hash-table :test 'equal)
  "Cross-run cache keyed by penultimate run signature for forward/backward set comparison.")

(defparameter *enum-disable-csp-metadata-pruning* nil
  "When non-NIL, enum subset-action generation ignores :REQUIRES-FLUENT and
   :PARTNER-FEASIBLE metadata.")


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
   Set by DEFINE-ENUM-RELATION in problem specifications.
   Keys: relation symbols.  Values: plists with keys
   :PATTERN :ALLOW-UNASSIGNED :EARLY-KEYS :ON-ASSIGN :SYMMETRIC-BATCH :MAX-PER-KEY
   :KEY-TYPES :REQUIRES-FLUENT :PARTNER-FEASIBLE.")


(defparameter *enum-residual-symmetry-enabled* t
  "Master switch for residual symmetry pruning in subset enumeration.
   Default T to enable pruning; set NIL to disable for verification.")


(defparameter *enumerator-disabled-search-hook-symbols*
  '(min-steps-remaining? heuristic? prune-state? bounding-function?)
  "Search hook function symbols temporarily disabled during CSP enumeration.
   These hooks are designed for normal planning search states and can
   incorrectly prune or bias synthetic partial states used by the enumerator.")


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


(defmacro define-enum-relation (relation &rest keys)
  "Problem-spec macro: declare enumeration metadata for RELATION.
   Keys:
   :PATTERN          :FLUENT | :SUBSET  (default inferred from relation)
   :ALLOW-UNASSIGNED T | (:TYPES type...)  — which keys may be unassigned
   :EARLY-KEYS       (:TYPES type...)  — enumerate these key-types first
   :ON-ASSIGN        ((rel val)...)  — default other relations when assigned
   :SYMMETRIC-BATCH  T | NIL  — use canonical multisets for interchangeable groups
   :MAX-PER-KEY      positive integer  — cardinality cap (for :SUBSET pattern)
   :KEY-TYPES        (:TYPES type...)  — restrict subset keys to these types
   :REQUIRES-FLUENT  symbol  — fluent that must be bound for non-empty subsets
   :PARTNER-FEASIBLE  (:QUERY name :ARGS (arg-specs...))  — branch-time feasibility check
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
           (run-base-relations (fgs-compute-run-base-relations exclude-relations include-relations))
           (goal-run-settings (list :algorithm algorithm
                                    :solution-type normalized-solution-type
                                    :exclude-relations exclude-relations
                                    :include-relations include-relations
                                    :prefilter prefilter
                                    :sort< sort<
                                    :sort-key sort-key)))
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
                                           :prefilter combined-prefilter)))
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


(defmacro find-penultimate-states (&rest raw-args)
  "User-facing macro wrapper for penultimate-state enumeration.
   GOAL-SPEC: goal form, partial goal-state shorthand, or symbol naming a goal function.
              Optional; defaults to GOAL-FN.
   Keywords:
   :DIRECTION       FORWARD (default) or BACKWARD.
                    FORWARD enumerates all base-relation assignments via CSP, testing
                    each for one-step goal reachability.  May be intractable when the
                    goal heavily constrains the state (skip-fixed-maps disables pruning).
                    BACKWARD reverses from *ENUMERATED-GOAL-STATES* (requires prior
                    FIND-GOAL-STATES call), generating predecessor candidates via
                    regression and validating them with forward application.
   :SOLUTION-TYPE   FIRST | EVERY | <integer N>  (forward only)
   :ALGORITHM       enumeration/search algorithm symbol  (forward only)
   :EXCLUDE-RELATIONS  symbol or list of symbols to remove from base schema  (forward only)
   :INCLUDE-RELATIONS  symbol or list of symbols to add to base schema  (forward only)
   :PREFILTER       function of (state) to prune base states  (forward only)
   :ACTION-FAMILIES list drawn from (MOVE PICKUP CONNECT), default all three.
   :SORT<  comparison function of (state-a state-b) for sorting penultimate states.
           States for which (funcall sort< a b) is true sort first (best).
           Default NIL (no sorting).
   :SORT-KEY  function of (state) returning a numeric key for sorting.
              Lower keys sort first.  More efficient than :SORT< because each
              state's key is computed once (Schwartzian transform).
              Default NIL (no sorting).  Mutually exclusive with :SORT<.
   :PRINT-REPORT  when NIL, skip interactive report printing (default T).
   Returns: T (the report plist is saved on (get 'find-penultimate-states :last-report))."
  (let ((args (cond
                ((null raw-args) (list 'goal-fn))
                ((keywordp (car raw-args)) (cons 'goal-fn raw-args))
                (t raw-args))))
    (destructuring-bind (goal-spec
                         &key
                         (direction 'forward)
                         (algorithm nil algorithm-supplied-p)
                         (solution-type nil solution-type-supplied-p)
                         (exclude-relations nil exclude-relations-supplied-p)
                         (include-relations nil include-relations-supplied-p)
                         (prefilter nil prefilter-supplied-p)
                         (action-families nil action-families-supplied-p)
                         (sort< nil sort<-supplied-p)
                         (sort-key nil sort-key-supplied-p)
                         (print-report nil print-report-supplied-p))
        args
      (flet ((literal-symbol-p (form)
               (and (symbolp form)
                    (not (keywordp form))
                    (not (null form))))
             (literal-list-p (form)
               (and (consp form)
                    (not (eq (car form) 'quote))))
             (backward-direction-form-p (form)
               (let ((x (if (and (consp form) (eq (car form) 'quote))
                            (cadr form)
                            form)))
                 (or (eq x 'backward) (eq x :backward)))))
        (labels ((maybe-quote (form)
                   (cond
                     ((null form) nil)
                     ((and (consp form) (eq (car form) 'quote)) form)
                     ((literal-symbol-p form) `',form)
                     ((literal-list-p form) `',form)
                     (t form))))
          (if (backward-direction-form-p direction)
              (progn
                (when algorithm-supplied-p
                  (error "In FIND-PENULTIMATE-STATES, :ALGORITHM is only supported with :DIRECTION FORWARD. ~
                          Remove :ALGORITHM for BACKWARD mode."))
                `(find-penultimate-states-backward-fn
                  ,(maybe-quote goal-spec)
                  ,@(when action-families-supplied-p
                      `(:action-families ,(maybe-quote action-families)))
                  ,@(when sort<-supplied-p
                      `(:sort< ,sort<))
                  ,@(when sort-key-supplied-p
                      `(:sort-key ,sort-key))
                  ,@(when print-report-supplied-p
                      `(:print-report ,print-report))))
              `(find-penultimate-states-fn
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
                ,@(when action-families-supplied-p
                    `(:action-families ,(maybe-quote action-families)))
                ,@(when sort<-supplied-p
                    `(:sort< ,sort<))
                ,@(when sort-key-supplied-p
                    `(:sort-key ,sort-key))
                ,@(when print-report-supplied-p
                    `(:print-report ,print-report)))))))))


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
  "Shared canonical state feasibility predicate for penultimate enumeration."
  (or (not (fboundp 'penultimate-state-feasible?))
      (funcall (symbol-function 'penultimate-state-feasible?) state)))

(defun fps-allowed-last-actions-from-state (state problem-actions selected-actions
                                            &key (direction :forward))
  "Shared last-action candidate API used by forward/backward penultimate enumeration."
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

(defun fps-validate-penultimate-state (state goal-fn problem-actions selected-actions
                                       &key base-relations
                                            candidate-goal-key-table candidate-goal-match-required-p
                                            direction allowed-actions)
  "Shared penultimate validator used by both forward and backward modes.
   Returns a list of reaching hit plists (:ACTION ... :GOAL-STATE ...)."
  (when (fps-state-enumeration-feasible-p state)
    (let ((actions (or allowed-actions
                       (fps-allowed-last-actions-from-state state
                                                            problem-actions
                                                            selected-actions
                                                            :direction direction)))
          (hits nil))
      (let ((*actions* problem-actions))
        (dolist (action-form actions (nreverse hits))
          (multiple-value-bind (next-state success-p failure-reason)
              (apply-action-to-state action-form state nil nil)
            (declare (ignore failure-reason))
            (when (and success-p (funcall goal-fn next-state))
              (let ((goal-key-ok
                      (if candidate-goal-match-required-p
                          (and candidate-goal-key-table
                               (gethash (fps-state-base-prop-key next-state base-relations)
                                        candidate-goal-key-table))
                          t)))
                (when goal-key-ok
                  (push (list :action action-form :goal-state next-state) hits))))))))))

(defun fps-penultimate-run-signature (goal-form action-families base-relations)
  "Key used to compare forward/backward penultimate sets for the same settings."
  (list :goal goal-form
        :action-families (sort (copy-list (or action-families '(move pickup connect))) #'string<
                               :key #'symbol-name)
        :base-relations (sort (copy-list (or base-relations (get-base-relations))) #'string<
                              :key #'symbol-name)))

(defun fps-register-penultimate-set-and-compare (goal-form action-families
                                                 base-relations direction state-keys)
  "Update equivalence cache and return values:
   (values expected-p matched-p other-direction-keys)."
  (let* ((signature (fps-penultimate-run-signature goal-form action-families base-relations))
         (entry (or (gethash signature *penultimate-equivalence-cache*)
                    (let ((ht (make-hash-table :test #'eq)))
                      (setf (gethash signature *penultimate-equivalence-cache*) ht)
                      ht)))
         (other-direction (if (eq direction 'forward) 'backward 'forward))
         (other-keys (gethash other-direction entry))
         (matched (and other-keys
                       (null (set-exclusive-or state-keys other-keys :test #'equal)))))
    (setf (gethash direction entry) (copy-list state-keys))
    (values t (and other-keys matched) other-keys)))

(defun fps-forward-raw-candidates-from-goals (candidate-goal-states problem-actions selected-actions
                                              &key base-relations)
  "Generate forward raw penultimate candidates by regressing from candidate goals.
   Returns two values: list of unique predecessor states and raw predecessor count."
  (let ((table (make-hash-table :test 'equal))
        (raw-count 0))
    (dolist (gs candidate-goal-states)
      (dolist (action-form (fps-allowed-last-actions-from-state
                            gs problem-actions selected-actions :direction :backward))
        (dolist (pred0 (bw-regress gs action-form))
          (incf raw-count)
          (let ((pred (copy-problem-state pred0)))
            (bw-normalize! pred)
            (let ((key (fps-state-base-prop-key pred base-relations)))
              (unless (gethash key table)
                (setf (gethash key table) pred)))))))
    (values (loop for st being the hash-values of table collect st)
            raw-count)))

(defun penultimate-state-base-props (state base-relations)
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
  (prin1-to-string (penultimate-state-base-props state base-relations)))


(defun fps-build-report (goal-form penultimate-states state-hit-table
                       &key base-relations action-families settings candidate-goal-states
                            raw-candidates semantic-valid-penultimate-states
                            equivalence-expected-p forward-backward-set-match
                            forward-backward-other-count)
  "Build and return a FIND-PENULTIMATE-STATES report plist."
  (let* ((candidate-goal-states (or candidate-goal-states *enumerated-goal-states*))
         (candidate-goal-key-table
           (when candidate-goal-states
             (let ((ht (make-hash-table :test 'equal)))
               (dolist (gs candidate-goal-states ht)
                 (setf (gethash (fps-state-base-prop-key gs base-relations) ht) t)))))
         (candidate-reachable-goal-keys
           (when candidate-goal-key-table
             (make-hash-table :test 'equal)))
         (candidate-transition-details nil)
         (candidate-transitions 0)
         (results
           (mapcar
            (lambda (st)
              (let* ((hits (gethash st state-hit-table))
                     (reaching-actions
                       (mapcar
                        (lambda (hit)
                          (let ((goal-state (getf hit :goal-state)))
                            (list :action (getf hit :action)
                                  :goal-state goal-state
                                  :goal-props (list-database (problem-state.idb goal-state)))))
                        hits)))
                (when candidate-goal-key-table
                  (let ((candidate-reaches nil))
                    (dolist (ra reaching-actions)
                      (let* ((goal-state (getf ra :goal-state))
                             (goal-key (fps-state-base-prop-key goal-state base-relations)))
                        (when (gethash goal-key candidate-goal-key-table)
                          (push ra candidate-reaches)
                          (setf (gethash goal-key candidate-reachable-goal-keys) t)
                          (incf candidate-transitions))))
                    (when candidate-reaches
                      (push (list :penultimate-state st
                                  :penultimate-props
                                  (sort (copy-list (list-database (problem-state.idb st)))
                                        (lambda (x y) (string< (prin1-to-string x)
                                                               (prin1-to-string y))))
                                  :reaches (nreverse candidate-reaches))
                            candidate-transition-details))))
                (list :state st
                      :props (sort (copy-list (list-database (problem-state.idb st)))
                                   (lambda (x y) (string< (prin1-to-string x)
                                                          (prin1-to-string y))))
                      :reaching-actions reaching-actions)))
            penultimate-states))
         (candidate-transition-details
           (nreverse candidate-transition-details))
         (summary (list :raw-candidates raw-candidates
                        :semantic-valid-penultimate-states
                        semantic-valid-penultimate-states
                        :penultimate-states (length penultimate-states)
                        :candidate-goal-states
                        (when candidate-goal-key-table
                          (length candidate-goal-states))
                        :candidate-reachable-goal-states
                        (when candidate-goal-key-table
                          (hash-table-count candidate-reachable-goal-keys))
                        :candidate-unreachable-goal-states
                        (when candidate-goal-key-table
                          (- (length candidate-goal-states)
                             (hash-table-count candidate-reachable-goal-keys)))
                        :candidate-transitions
                        (when candidate-goal-key-table candidate-transitions)
                        :candidate-transition-penultimate-states
                        (when candidate-goal-key-table
                          (length candidate-transition-details))
                        :forward-backward-equivalence-expected
                        equivalence-expected-p
                        :forward-backward-set-match
                        (if (null forward-backward-set-match) :pending forward-backward-set-match)
                        :forward-backward-other-count
                        forward-backward-other-count)))
    (list
     :goal goal-form
     :settings settings
     :action-families action-families
     :penultimate-states penultimate-states
     :results results
     :candidate-transition-details candidate-transition-details
     :summary summary)))


(defun fps-print-report (report)
  "Print FIND-PENULTIMATE-STATES report interactively.
   Displays summary counts, then pages through individual transitions
   from feasible penultimate states to candidate goal states."
  (let ((goal (getf report :goal))
        (settings (getf report :settings))
        (direction (or (getf (getf report :settings) :direction) 'forward))
        (summary (getf report :summary))
        (candidate-transition-details (getf report :candidate-transition-details)))
    (format t "~&~%;;;; FIND-PENULTIMATE-STATES REPORT ;;;;~%")
    (format t "~&Goal: ~S~%" goal)
    (format t "~&Direction: ~S~%" direction)
    (format t "~&Settings: ~S~%" settings)
    (when (getf summary :raw-candidates)
      (format t "~&Raw candidates: ~D~%" (getf summary :raw-candidates)))
    (when (getf summary :semantic-valid-penultimate-states)
      (format t "~&Semantic-valid penultimate states: ~D~%"
              (getf summary :semantic-valid-penultimate-states)))
    (format t "~&Penultimate states found: ~D~%" (getf summary :penultimate-states))
    (when (getf summary :forward-backward-equivalence-expected)
      (format t "~&Forward/backward equivalence expected: T~%")
      (format t "~&Forward/backward sets matched (base-key): ~A~%"
              (getf summary :forward-backward-set-match))
      (when (getf summary :forward-backward-other-count)
        (format t "~&Other direction set size: ~D~%"
                (getf summary :forward-backward-other-count))))
    (when (getf summary :candidate-goal-states)
      (format t "~&Candidate goal states (from find-goal-states): ~D~%"
              (getf summary :candidate-goal-states))
      (format t "~&Candidate goals reached from feasible penultimate states: ~D~%"
              (getf summary :candidate-reachable-goal-states))
      (format t "~&Candidate-goal transitions found: ~D~%"
              (getf summary :candidate-transitions))
      (format t "~&Penultimate states with candidate-goal transitions: ~D~%"
              (getf summary :candidate-transition-penultimate-states)))
    (let ((transitions (stable-sort (fps-flatten-transitions candidate-transition-details)
                                     #'< :key (lambda (tr) (length (getf tr :penultimate-props))))))
      (when transitions
        (let ((n (length transitions)))
          (loop for idx from 1 to n
                for tr in transitions
                do (format t "~&~%Transition ~D of ~D:~%" idx n)
                   (format t "~&Penultimate state:~%")
                   (fgs-print-props (getf tr :penultimate-props))
                   (format t "~&Action: ~S~%" (getf tr :action))
                   (format t "~&Resulting goal state:~%")
                   (fgs-print-props (getf tr :goal-props))
                   (when (< idx n)
                     (unless (y-or-n-p "~&Show the next transition of ~D?" n)
                       (return))))))))
  report)


(defun fps-flatten-transitions (candidate-transition-details)
  "Flatten CANDIDATE-TRANSITION-DETAILS into a list of individual transition plists.
   Each plist has keys :PENULTIMATE-PROPS, :ACTION, :GOAL-PROPS."
  (let ((transitions nil))
    (dolist (item candidate-transition-details (nreverse transitions))
      (let ((penultimate-props (getf item :penultimate-props)))
        (dolist (reach (getf item :reaches))
          (push (list :penultimate-props penultimate-props
                      :action (getf reach :action)
                      :goal-props (getf reach :goal-props))
                transitions))))))


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


(defun find-penultimate-states-fn (&optional (goal-spec 'goal-fn)
                                             &key (algorithm *algorithm*) (solution-type 'every)
                                              exclude-relations include-relations
                                              (prefilter nil)
                                              (action-families '(move pickup connect))
                                              sort< sort-key
                                              (print-report t))
  "Enumerate penultimate states that can reach GOAL-SPEC in one action.
   Enumeration uses the same CSP base-state generation as FIND-GOAL-STATES.
   GOAL-SPEC is optional; defaults to GOAL-FN.
   PREFILTER defaults to NIL for completeness (especially connect-last cases).
   :SORT<  comparison function of (state-a state-b) → generalized boolean.
           When non-nil, penultimate states are stable-sorted so that states for which
           (funcall sort< a b) is true appear first.  Default NIL (no sorting).
   :SORT-KEY  function of (state) → number.  Lower keys sort first.
              More efficient than :SORT< (Schwartzian transform: each key
              computed once).  Default NIL.  Mutually exclusive with :SORT<."
  (unless (get-base-relations)
    (error "FIND-PENULTIMATE-STATES: no base relations declared.  ~
            Use (define-base-relations (...)) in the problem specification."))
  (when (or (> *debug* 0) *probe*)
    (format t "~&[find-penultimate-states] Please reset first by entering ")
    (when (> *debug* 0)
      (format t " (ww-set *debug* 0)~%"))
    (when *probe*
      (format t " (ww-set *probe* nil)~%"))
    (return-from find-penultimate-states-fn nil))
  (multiple-value-bind (norm-goal-spec goal-form)
      (enum-normalize-goal-spec goal-spec)
    (let* ((normalized-solution-type (fgs-normalize-solution-type solution-type))
           (run-base-relations (fgs-compute-run-base-relations exclude-relations include-relations))
           (penultimate-run-settings (list :direction 'forward
                                           :algorithm algorithm
                                           :solution-type normalized-solution-type
                                           :exclude-relations exclude-relations
                                           :include-relations include-relations
                                           :prefilter prefilter
                                           :action-families (or action-families '(move pickup connect))
                                           :sort< sort<
                                           :sort-key sort-key
                                           :print-report print-report))
           (problem-actions *actions*)
           (goal-fn (coerce-goal norm-goal-spec))
           (normalized-families (enum-normalize-action-families action-families))
           (selected-actions (enum-select-actions-by-family problem-actions normalized-families))
           (state-hit-table (make-hash-table :test 'eq))
           (effective-prefilter (if (or (eq prefilter 'use-installed)
                                        (eq prefilter :use-installed))
                                    (get-prefilter)
                                    prefilter))
           (modifiable-rels (actions-modifiable-base-relations selected-actions))     ;; CHANGED
           (invariants (enum-action-invariant-literals                                ;; CHANGED
                        goal-form run-base-relations modifiable-rels))                ;; CHANGED
           (invariant-prefilter (make-goal-invariant-prefilter                        ;; CHANGED
                                 invariants 'find-penultimate-states))               ;; CHANGED
           (combined-prefilter (compose-prefilters effective-prefilter invariant-prefilter))
           (candidate-goal-states-snapshot *enumerated-goal-states*)
           (candidate-goal-match-required-p (not (null candidate-goal-states-snapshot)))
           (candidate-goal-key-table
            (when candidate-goal-states-snapshot
              (let ((ht (make-hash-table :test 'equal)))
                (dolist (gs candidate-goal-states-snapshot ht)
                  (setf (gethash (fps-state-base-prop-key gs run-base-relations) ht) t))))))
      (when (null selected-actions)
        (format t "~&[find-penultimate-states] No matching actions for families ~S.~%"
                normalized-families))
      (let ((*enumerator-base-relations* run-base-relations))
        (let ((*enumerator-ui-settings* penultimate-run-settings))
          (let* ((*enum-disable-csp-metadata-pruning* t)
                 (raw-states nil)
                 (raw-candidate-count 0)
                 (semantic-states nil))
            (if candidate-goal-states-snapshot
                (multiple-value-setq (raw-states raw-candidate-count)
                  (fps-forward-raw-candidates-from-goals
                   candidate-goal-states-snapshot
                   problem-actions
                   selected-actions
                   :base-relations run-base-relations))
                (let ((enumerated (enumerate-state (lambda (state) (declare (ignore state)) t)
                                                   :algorithm algorithm
                                                   :solution-type normalized-solution-type
                                                   :propagate 'finalize-only
                                                   :prefilter combined-prefilter
                                                   :goal-form goal-form
                                                   :skip-fixed-maps t)))
                  (setf raw-states enumerated
                        raw-candidate-count (length enumerated))))
            (dolist (st raw-states)
              (let ((semantic-hits
                      (fps-validate-penultimate-state
                       st goal-fn problem-actions selected-actions
                       :base-relations run-base-relations
                       :candidate-goal-key-table candidate-goal-key-table
                       :candidate-goal-match-required-p candidate-goal-match-required-p
                       :direction :forward)))
                (when semantic-hits
                  (push st semantic-states)
                  (setf (gethash st state-hit-table) semantic-hits))))
            (setf semantic-states (nreverse semantic-states))
            (let ((states semantic-states))
          (when (and sort< sort-key)
            (error "FIND-PENULTIMATE-STATES: :SORT< and :SORT-KEY are mutually exclusive."))
          (when sort<
            (setf states (stable-sort states sort<)))
          (when sort-key
            (let ((decorated (mapcar (lambda (st) (cons (funcall sort-key st) st)) states)))
              (setf states (mapcar #'cdr (stable-sort decorated #'< :key #'car)))))
            (let* ((state-keys (mapcar (lambda (st) (fps-state-base-prop-key st run-base-relations))
                                       states))
                   (equivalence-expected-p nil)
                   (set-match nil)
                   (other-count nil))
              (multiple-value-setq (equivalence-expected-p set-match)
                (fps-register-penultimate-set-and-compare
                 goal-form normalized-families run-base-relations
                 'forward state-keys))
              (setf other-count
                    (let* ((signature (fps-penultimate-run-signature
                                       goal-form normalized-families run-base-relations))
                           (entry (gethash signature *penultimate-equivalence-cache*))
                           (other-keys (and entry (gethash 'backward entry))))
                      (and other-keys (length other-keys))))
              (let ((report (fps-build-report goal-form
                                              states
                                              state-hit-table
                                              :base-relations run-base-relations
                                              :action-families normalized-families
                                              :candidate-goal-states candidate-goal-states-snapshot
                                              :raw-candidates raw-candidate-count
                                              :semantic-valid-penultimate-states (length semantic-states)
                                              :equivalence-expected-p equivalence-expected-p
                                              :forward-backward-set-match set-match
                                              :forward-backward-other-count other-count
                                              :settings (list :direction 'forward
                                                              :algorithm algorithm
                                                              :solution-type normalized-solution-type
                                                              :exclude-relations exclude-relations
                                                              :include-relations include-relations
                                                              :prefilter prefilter
                                                              :action-families normalized-families
                                                              :sort< sort<
                                                              :sort-key sort-key
                                                              :print-report print-report))))
                (when print-report
                  (fps-print-report report))
                (setf *enumerated-penultimate-states* states
                      *enumerated-penultimate-results* (getf report :results))
                (setf (get 'find-penultimate-states :last-report) report)
                t)))))))))


(defun find-penultimate-states-backward-fn (&optional (goal-spec 'goal-fn)
                                            &key
                                            (action-families '(move pickup connect))
                                            sort< sort-key
                                            (print-report t))
  "Backward penultimate-state search: regress from *ENUMERATED-GOAL-STATES*.
   Uses shared feasibility/validation logic and the same FPS-PRINT-REPORT
   output format as forward mode."
  (unless *enumerated-goal-states*
    (format t "~&[find-penultimate-states :backward] No goal states available.  ~
               Run (find-goal-states ...) first.~%")
    (return-from find-penultimate-states-backward-fn nil))
  (when (or (> *debug* 0) *probe*)
    (format t "~&[find-penultimate-states :backward] Please reset first by entering ")
    (when (> *debug* 0)
      (format t " (ww-set *debug* 0)~%"))
    (when *probe*
      (format t " (ww-set *probe* nil)~%"))
    (return-from find-penultimate-states-backward-fn nil))
  (multiple-value-bind (norm-goal-spec goal-form)
      (enum-normalize-goal-spec goal-spec)
    (let* ((goal-fn (coerce-goal norm-goal-spec))
           (families (enum-normalize-action-families action-families))
           (base-relations (get-base-relations))
           (problem-actions *actions*)
           (selected-actions (enum-select-actions-by-family problem-actions families))
           (candidate-goal-key-table
            (let ((ht (make-hash-table :test 'equal)))
              (dolist (gs *enumerated-goal-states* ht)
                (setf (gethash (fps-state-base-prop-key gs base-relations) ht) t))))
           (penult-table (make-hash-table :test 'equal))
           (state-hit-table (make-hash-table :test 'eq))
           (total-raw-predecessors 0))
      (dolist (gs *enumerated-goal-states*)
        (let ((candidates (fps-allowed-last-actions-from-state
                           gs problem-actions selected-actions :direction :backward)))
          (dolist (action-form candidates)
            (dolist (pred0 (bw-regress gs action-form))
              (incf total-raw-predecessors)
              (let ((pred (copy-problem-state pred0)))
                (bw-normalize! pred)
                (let ((semantic-hits
                        (fps-validate-penultimate-state
                         pred goal-fn problem-actions selected-actions
                         :base-relations base-relations
                         :candidate-goal-key-table candidate-goal-key-table
                         :candidate-goal-match-required-p t
                         :direction :forward)))
                  (when semantic-hits
                    (let ((key (fps-state-base-prop-key pred base-relations)))
                      (unless (gethash key penult-table)
                        (setf (gethash key penult-table) pred))
                      (setf (gethash (gethash key penult-table) state-hit-table)
                            semantic-hits)))))))))
      (let* ((semantic-states (loop for st being the hash-values of penult-table collect st))
             (states semantic-states)
             (hits-table state-hit-table))
        (when (and sort< sort-key)
          (error "FIND-PENULTIMATE-STATES :BACKWARD: :SORT< and :SORT-KEY are mutually exclusive."))
        (when sort<
          (setf states (stable-sort states sort<)))
        (when sort-key
          (let ((decorated (mapcar (lambda (st) (cons (funcall sort-key st) st)) states)))
            (setf states (mapcar #'cdr (stable-sort decorated #'< :key #'car)))))
        (let* ((state-keys (mapcar (lambda (st) (fps-state-base-prop-key st base-relations))
                                   states))
               (equivalence-expected-p nil)
               (set-match nil)
               (other-count nil))
          (multiple-value-setq (equivalence-expected-p set-match)
            (fps-register-penultimate-set-and-compare
             goal-form families base-relations 'backward state-keys))
          (setf other-count
                (let* ((signature (fps-penultimate-run-signature
                                   goal-form families base-relations))
                       (entry (gethash signature *penultimate-equivalence-cache*))
                       (other-keys (and entry (gethash 'forward entry))))
                  (and other-keys (length other-keys))))
          (let ((report (fps-build-report goal-form
                                          states
                                          hits-table
                                          :base-relations base-relations
                                          :action-families families
                                          :raw-candidates total-raw-predecessors
                                          :semantic-valid-penultimate-states (length semantic-states)
                                          :equivalence-expected-p equivalence-expected-p
                                          :forward-backward-set-match set-match
                                          :forward-backward-other-count other-count
                                          :settings (list :direction 'backward
                                                          :algorithm nil
                                                          :solution-type nil
                                                          :exclude-relations nil
                                                          :include-relations nil
                                                          :prefilter nil
                                                          :action-families families
                                                          :sort< sort<
                                                          :sort-key sort-key
                                                          :print-report print-report))))
            (when print-report
              (fps-print-report report))
            (setf *enumerated-penultimate-states* states
                  *enumerated-penultimate-results* (getf report :results))
            (setf (get 'find-penultimate-states :last-report) report)
            t))))))


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
                                       (skip-fixed-maps nil))
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
   SKIP-FIXED-MAPS: when T, disable goal-fixed-fluent pruning in CSP generation."
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
                       :prefilter prefilter
                       :goal-form goal-form
                       :skip-fixed-maps skip-fixed-maps))


(defun enumerate-state-csp (goal-spec &key (algorithm *algorithm*) (solution-type 'first)
                                           (default-max-per-key 4) (propagate t)
                                           (prefilter nil) (goal-form nil)
                                           (skip-fixed-maps nil))
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
   SKIP-FIXED-MAPS: when T, disable goal-fixed-fluent pruning in CSP generation."
  (multiple-value-bind (norm-goal-spec derived-goal-form)
      (enum-normalize-goal-spec goal-spec)
    (let* ((effective-goal-form (or goal-form derived-goal-form))
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
      (get problem :enumerator-base-relations)))


;;;; ----------------------------------------------------------------------
;;;; Enum relation metadata
;;;; ----------------------------------------------------------------------


(defun install-enum-relation-meta (relation keys)
  "Install enumeration metadata for RELATION.
   RELATION: a symbol naming the relation.
   KEYS: a property list (:PATTERN val :ALLOW-UNASSIGNED val ...).
   Valid keys:
   :PATTERN          :FLUENT | :SUBSET  (default inferred from relation)
   :ALLOW-UNASSIGNED T | (:TYPES type...)  — which keys may be unassigned
   :EARLY-KEYS       (:TYPES type...)  — enumerate these key-types first
   :ON-ASSIGN        ((rel val)...)  — default other relations when assigned
   :SYMMETRIC-BATCH  T | NIL  — use canonical multisets for interchangeable groups
   :MAX-PER-KEY      positive integer  — cardinality cap (for :SUBSET pattern)
   :KEY-TYPES        (:TYPES type...)  — restrict subset keys to these types
   :REQUIRES-FLUENT  symbol  — fluent that must be bound for non-empty subsets
   :PARTNER-FEASIBLE  (:QUERY name :ARGS (arg-specs...))  — branch-time feasibility check"
  (check-type relation symbol)
  (unless (gethash relation *relations*)
    (error "DEFINE-ENUM-RELATION ~S: relation is not installed in *RELATIONS*." relation))
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
  "Canonicalize KEYS plist."
  (copy-list keys))


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
        (max-per-key (getf plist :max-per-key)))
    (when (and explicit-pattern
               (not (member explicit-pattern '(:fluent :subset))))
      (error "DEFINE-ENUM-RELATION ~S: :PATTERN must be :FLUENT or :SUBSET; got ~S"
             relation explicit-pattern))
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
                 relation requires-fluent))))
    (let ((pf (getf plist :partner-feasible)))
      (when pf
        (unless (eq pattern :subset)
          (error "DEFINE-ENUM-RELATION ~S: :PARTNER-FEASIBLE only valid for :SUBSET pattern; got :PATTERN ~S"
                 relation pattern))
        (unless (and (consp pf) (getf pf :query) (getf pf :args))
          (error "DEFINE-ENUM-RELATION ~S: :PARTNER-FEASIBLE must be (:QUERY name :ARGS (...))"
                 relation))
        (unless (symbolp (getf pf :query))
          (error "DEFINE-ENUM-RELATION ~S: :PARTNER-FEASIBLE :QUERY must be a symbol; got ~S"
                 relation (getf pf :query)))
        (dolist (arg-spec (getf pf :args))
          (unless (or (eq arg-spec :partner)
                      (eq arg-spec :key)
                      (and (consp arg-spec)
                           (eq (first arg-spec) :key-fluent)
                           (symbolp (second arg-spec))
                           (= (length arg-spec) 2)))
            (error "DEFINE-ENUM-RELATION ~S: :PARTNER-FEASIBLE arg-spec must be :PARTNER, :KEY, or (:KEY-FLUENT sym); got ~S"
                   relation arg-spec)))))))


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


(defun enum-partner-feasible (rel)
  "Return the :PARTNER-FEASIBLE spec for REL, or NIL."
  (getf (enum-relation-meta rel) :partner-feasible))


(defun enum-build-partner-feasible-fn (rel)
  "Build a feasibility predicate from REL's :PARTNER-FEASIBLE metadata.
   Returns a function (lambda (state key partner) ...) that returns T when
   the partner is feasible for the key, or NIL if no spec is declared.
   The query function must already be compiled (fboundp)."
  (let ((spec (enum-partner-feasible rel)))
    (when spec
      (let* ((query-name (getf spec :query))
             (arg-specs (getf spec :args))
             (query-fn (symbol-function query-name)))
        (format t "~&[partner-feasible] rel=~S query=~S args=~S~%"
                rel query-name arg-specs)
        (let ((resolvers
                (mapcar (lambda (arg-spec)
                          (cond
                            ((eq arg-spec :partner)
                             (lambda (state key partner)
                               (declare (ignore state key))
                               partner))
                            ((eq arg-spec :key)
                             (lambda (state key partner)
                               (declare (ignore state partner))
                               key))
                            ((and (consp arg-spec)
                                  (eq (first arg-spec) :key-fluent))
                             (let ((fluent-rel (second arg-spec)))
                               (lambda (state key partner)
                                 (declare (ignore partner))
                                 (first (fluent-value state (list fluent-rel key))))))
                            (t (error "Unknown arg-spec: ~S" arg-spec))))
                        arg-specs)))
          (compile nil
                   (lambda (state key partner)
                     (apply query-fn state
                            (mapcar (lambda (r) (funcall r state key partner))
                                    resolvers)))))))))


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


(defun enum-make-symmetric-batch-action (rel sym-type objects value-tuples
                                         &key (propagate t) (fixed-maps nil))
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
                                      (requires-fluent nil)
                                      (predecessors-map nil) (fluent-relations nil)
                                      (partner-feasible-fn nil))
  "Create subset-enumeration actions for REL with global degree cap.
   For each key, generates an action whose instantiations are all subsets (up to
   MAX-PER-KEY) of the key's allowed partners.
   When SYMMETRIC-RELATION-P, excludes lower-indexed keys from pairing targets.
   When REQUIRES-FLUENT is non-nil, keys without that fluent bound are restricted
   to empty subsets.
   When PARTNER-FEASIBLE-FN is non-nil, each partner in a proposed subset must
   satisfy (funcall partner-feasible-fn state key partner) for the subset to be
   accepted.  Prunes the DFS tree by rejecting infeasible subsets at branch time."
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
                          (or (null partner-feasible-fn)
                              (every (lambda (p)
                                       (funcall partner-feasible-fn state k p))
                                     set))
                          (or (null requires-fluent)
                              (every (lambda (tgt)
                                       (or (not (member tgt keys :test #'eq))
                                           (fluent-bound-p state (list requires-fluent tgt))))
                                     set))
                          (enum-subset-degree-ok-p state rel k set keys partners max-per-key)
                          (enum-residual-symmetry-ok-p state rel k set
                                                       predecessors-map fluent-relations
                                                       keys partners)
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
     :SYMMETRIC-BATCH — interchangeable groups use canonical multisets"
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
                (when (enum-symmetric-batch-p r)
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


(defun enum-compute-context (goal-form &key skip-fixed-maps)
  "Compute context plist for enum action generation.
   Gathers schema metadata, type domains, goal analysis, and fixed-maps.
   SKIP-FIXED-MAPS: when T, leave :FIXED-MAPS empty (penultimate enumeration
   needs interchangeable-group detection from GOAL-FORM but must not fix
   base-relation assignments, since penultimate states don't satisfy the goal).
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
   Keys are sorted fail-first BEFORE building the predecessor map, ensuring
   residual symmetry pruning is consistent with actual CSP execution order.
   When a relation is auto-detected as symmetric, applies index-based deduplication.
   When :PARTNER-FEASIBLE metadata is declared, builds a branch-time feasibility
   predicate and threads it into subset preconditions.
   Returns a list of actions in execution order (no post-hoc sort needed)."
  (let ((relations (getf ctx :relations))
        (symmetric-relations (getf ctx :symmetric-relations))
        (interchangeable-groups (getf ctx :interchangeable-groups))     ;residual symmetry
        ;; Use all installed unary fluent relations for residual-equivalence checks,
        ;; not only base-schema relations.
        (fluent-relations (enum-collect-fluent-relations
                           (enum-all-installed-relations)))  ;residual symmetry
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
               (requires-fluent (unless *enum-disable-csp-metadata-pruning*
                                  (enum-requires-fluent r)))
               (pf-fn (unless *enum-disable-csp-metadata-pruning*
                        (enum-build-partner-feasible-fn r)))
               (preds-map (enum-group-predecessors-map keys interchangeable-groups))  ;residual symmetry
               (actions (enum-make-subset-actions
                         r keys partners max-k
                         :propagate propagate
                         :symmetric-relation-p sym-rel-p
                         :requires-fluent requires-fluent
                         :predecessors-map preds-map               ;residual symmetry
                         :fluent-relations fluent-relations         ;residual symmetry
                         :partner-feasible-fn pf-fn)))
          ;; Actions are already in execution order — no post-hoc sort.
          (format t "~&[subset-actions] rel=~S keys=~S~%" r keys)
          (when (> (hash-table-count preds-map) 0)
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
   2. Main fluent actions (:SYMMETRIC-BATCH + individual, with :UNASSIGNED)
   3. Subset enumeration for :SUBSET-pattern relations (fail-first ordering)
   4. ENUM-FINALIZE (triggers propagation, applies prefilter)
   PROPAGATE: T — propagate after every action (default).
              :FINALIZE-ONLY — skip intermediate propagation, propagate at finalize.
              NIL — no propagation anywhere (raw leaf collection).
   PREFILTER: when non-nil, a function applied at ENUM-FINALIZE to prune states
     before propagation.
   SKIP-FIXED-MAPS: when T, disable goal-fixed-fluent pruning (for penultimate
     enumeration where the goal form drives symmetry detection only)."
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

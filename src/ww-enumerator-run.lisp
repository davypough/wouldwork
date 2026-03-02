;;; Filename: ww-enumerator-run.lisp

(in-package :ww)

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
               Default :USE-INSTALLED uses the installed base filter; pass NIL to disable.
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


(defun find-goal-states-fn (&optional (goal-spec 'goal-fn)
                                      &key (algorithm *algorithm*) (solution-type 'every)
                                        exclude-relations include-relations
                                        (prefilter :use-installed)
                                        sort< sort-key)
  "Top-level API wrapper. Full implementation is defined below in
   %FIND-GOAL-STATES-FN to keep call flow top-down in this file."
  (%find-goal-states-fn goal-spec
                        :algorithm algorithm
                        :solution-type solution-type
                        :exclude-relations exclude-relations
                        :include-relations include-relations
                        :prefilter prefilter
                        :sort< sort<
                        :sort-key sort-key))


(defun find-predecessors-fn (&key (action-families :auto)
                                  (direction 'forward))
  "Top-level API wrapper. Full implementation is defined below in
   %FIND-PREDECESSORS-FN to keep call flow top-down in this file."
  (%find-predecessors-fn :action-families action-families
                         :direction direction))


(defun enumerate-state (goal-spec &key (algorithm *algorithm*) (solution-type 'first)
                                       (default-max-per-key 4) (propagate t)
                                       (prefilter nil) (goal-form nil)
                                       (skip-fixed-maps nil)
                                       (canonical-dedupe nil))
  "Top-level API wrapper. Full implementation is defined below in
   %ENUMERATE-STATE to keep call flow top-down in this file."
  (%enumerate-state goal-spec
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
  "Top-level API wrapper. Full implementation is defined below in
   %ENUMERATE-STATE-CSP to keep call flow top-down in this file."
  (%enumerate-state-csp goal-spec
                        :algorithm algorithm
                        :solution-type solution-type
                        :default-max-per-key default-max-per-key
                        :propagate propagate
                        :prefilter prefilter
                        :goal-form goal-form
                        :skip-fixed-maps skip-fixed-maps
                        :canonical-dedupe canonical-dedupe))


(defun %find-goal-states-fn (&optional (goal-spec 'goal-fn)
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
               Default :USE-INSTALLED uses the installed base filter; pass NIL to disable.
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
    (return-from %find-goal-states-fn nil))
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
                                      *enumerator-prefilter*
                                      prefilter))
             ;; Action-invariant prefilter: prune states violating goal literals
             ;; on base relations that no action can modify.
             (modifiable-rels (actions-modifiable-base-relations *actions*))
             (invariants (enum-action-invariant-literals
                          goal-form run-base-relations modifiable-rels))
             (invariant-prefilter (make-goal-invariant-prefilter
                                   invariants 'find-goal-states))
             (feasibility-prefilter (when (fboundp 'state-feasible?)
                                      #'fps-state-feasible-p))
             (combined-prefilter (compose-prefilters
                                  (compose-prefilters
                                   effective-prefilter invariant-prefilter)
                                  feasibility-prefilter)))
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


(defun fps-state-feasible-p (state)
  "Unified state feasibility predicate for enumeration and predecessor expansion.
   When the user defines STATE-FEASIBLE? in the problem spec, calls it to test
   whether STATE is physically reachable (e.g., agent accessibility).
   Returns T when no hook is defined or when the hook returns non-NIL."
  (or (not (fboundp 'state-feasible?))
      (funcall (symbol-function 'state-feasible?) state)))


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


(defun fps-idb-base-prop-key (idb base-relations)
  "Return canonical base-prop key computed directly from IDB hash-table.
   Avoids constructing a full problem-state for successor key checks."
  (let ((rels (or base-relations (get-base-relations))))
    (prin1-to-string
     (sort (remove-if-not (lambda (p) (member (car p) rels :test #'eq))
                          (list-database idb))
           (lambda (x y)
             (string< (prin1-to-string x)
                      (prin1-to-string y)))))))


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


(defun fps-compute-base-relation-domains (base-relations)
  "Precompute domain information for each relation in BASE-RELATIONS.
   Returns an alist keyed by relation symbol.  Each entry is a plist:
     :PATTERN           :FLUENT or :SUBSET
     :KEY-OBJECTS        instances of the key (non-fluent) type(s)
     :VALUES             (fluent only) instances of the fluent value type
     :ALLOW-UNASSIGNED   types whose key instances may have no assignment
     :PARTNERS           (subset only) all partner instances (before self-exclusion)
     :MAX-PER-KEY        (subset only) cardinality cap from metadata"
  (mapcar
   (lambda (rel)
     (let* ((pattern (enum-pattern rel))
            (key-specs (relation-key-type-specs rel))
            (key-objects (loop for s in key-specs
                               append (copy-list (expand-type-spec-instances* s))))
            (allow-raw (enum-allow-unassigned rel))
            (allow-types (cond ((eq allow-raw t)
                                (loop for s in key-specs
                                      append (copy-list (expand-type-spec-instances* s))))
                               ((and (consp allow-raw) (eql (car allow-raw) :types))
                                (loop for s in (cdr allow-raw)
                                      append (copy-list (expand-type-spec-instances* s))))
                               (t nil))))
       (cons rel
             (ecase pattern
               (:fluent
                (let* ((val-specs (relation-fluent-type-specs rel))
                       (values (loop for s in val-specs
                                     append (copy-list (expand-type-spec-instances* s)))))
                  (list :pattern :fluent
                        :key-objects key-objects
                        :values values
                        :allow-unassigned allow-types)))
               (:subset
                (let* ((sig (relation-signature rel))
                       (partners (loop for s in sig
                                       append (copy-list (expand-type-spec-instances* s))))
                       (max-k (enum-max-per-key rel)))
                  (list :pattern :subset
                        :key-objects key-objects
                        :partners (remove-duplicates partners :test #'eq)
                        :max-per-key (or max-k 3))))))))
   base-relations))


(defun fps-object-prop-combos (object modified-rels domains)
  "Compute the cartesian product of base-proposition choices for OBJECT across
   MODIFIED-RELS.  Returns a list of combos; each combo is a list of propositions
   (NIL entries represent 'unassigned').  Returns NIL if OBJECT has no choices."
  (let ((per-rel-choices nil))
    (dolist (rel modified-rels)
      (let* ((domain (cdr (assoc rel domains :test #'eq)))
             (pattern (getf domain :pattern))
             (key-objects (getf domain :key-objects)))
        (when (member object key-objects :test #'eq)
          (let ((choices nil))
            (ecase pattern
              (:fluent
               (let ((values (getf domain :values))
                     (allow-unassigned (getf domain :allow-unassigned)))
                 (dolist (val values)
                   (push (list (list rel object val)) choices))
                 (when (member object allow-unassigned :test #'eq)
                   (push nil choices))))
              (:subset
               (let* ((all-partners (getf domain :partners))
                      (partners (remove object all-partners :test #'eq))
                      (subsets (bw--pairing-subsets partners)))
                 (dolist (subset subsets)
                   (push (mapcar (lambda (partner) (list rel object partner))
                                 subset)
                         choices)))))
            (when choices
              (push (nreverse choices) per-rel-choices))))))
    (unless per-rel-choices
      (return-from fps-object-prop-combos nil))
    (reduce (lambda (acc rel-choices)
              (let ((result nil))
                (dolist (existing acc)
                  (dolist (choice rel-choices)
                    (push (append existing choice) result)))
                (nreverse result)))
            (nreverse per-rel-choices)
            :initial-value '(nil))))


(defun fps-flatten-type-inst (type-inst)
  "Flatten a precondition-type-inst into a list of unique object symbols.
   Handles nested (HEADER ...) sublists and skips header keywords."
  (let ((result nil))
    (dolist (item type-inst result)
      (cond ((member item *parameter-headers*) nil)
            ((and (listp item) (member (car item) *parameter-headers*))
             (dolist (obj (fps-flatten-type-inst item))
               (pushnew obj result :test #'eq)))
            ((listp item)
             (dolist (obj item)
               (when (symbolp obj)
                 (pushnew obj result :test #'eq))))))))


(defun fps-action-object-set (action)
  "Return a flat list of all object instances from ACTION's precondition-type-inst.
   Used to filter key-objects to only those the action can operate on.
   Returns NIL for dynamic actions (query-based types), meaning no filtering."
  (when (action.dynamic action)
    (return-from fps-action-object-set nil))
  (fps-flatten-type-inst (action.precondition-type-inst action)))


(defun fps-forward-key-objects-for-action (modified-rels domains action-objects)
  "Collect candidate key-objects for MODIFIED-RELS, filtered by ACTION-OBJECTS."
  (let ((key-objects nil))
    (dolist (rel modified-rels)
      (let ((domain (cdr (assoc rel domains :test #'eq))))
        (dolist (obj (getf domain :key-objects))
          (pushnew obj key-objects :test #'eq))))
    (if action-objects
        (intersection key-objects action-objects :test #'eq)
        key-objects)))


(defun fps-forward-validate-candidate (candidate pred-key frontier-key action
                                       reachable base-relations layer-table stats)
  "Validate CANDIDATE as a novel predecessor and store it when it reaches FRONTIER-KEY."
  (handler-case
      (block validate
        (incf (fps-layer-stats-novel-validation-attempts stats))
        (dolist (pre-args (get-precondition-args action candidate))
          (let ((pre-result (apply (action.pre-defun-name action) candidate pre-args)))
            (when pre-result
              (let ((updated-dbs
                      (if (eql pre-result t)
                          (funcall (action.eff-defun-name action) candidate)
                          (apply (action.eff-defun-name action) candidate pre-result))))
                (dolist (update updated-dbs)
                  (let ((changes (update.changes update)))
                    (when (hash-table-p changes)
                      (incf (fps-layer-stats-apply-success-count stats))
                      (let ((next-key (fps-idb-base-prop-key changes base-relations)))
                        (when (string= next-key frontier-key)
                          (let* ((inst (update.instantiations update))
                                 (action-form (cons (action.name action) inst))
                                 (target-entry (gethash frontier-key reachable))
                                 (target-path (cdr target-entry))
                                 (new-path (cons action-form target-path))
                                 (stored (copy-problem-state candidate)))
                            (incf (fps-layer-stats-validated-count stats))
                            (setf (gethash pred-key layer-table)
                                  (cons stored new-path))
                            (return-from validate t)))))))))))
        nil)
    (error ()
      (incf (fps-layer-stats-apply-fail-count stats))
      nil)))


(defun fps-forward-process-object-combos (frontier-state frontier-key object
                                          modified-rels action reachable
                                          base-relations domains layer-table stats)
  "Process all predecessor combinations for one OBJECT and update STATS/LAYER-TABLE."
  (let ((combos (fps-object-prop-combos object modified-rels domains)))
    (when combos
      (let ((candidate (copy-problem-state frontier-state)))
        (bw--delete-props-matching!
         candidate (lambda (p) (and (member (car p) modified-rels :test #'eq)
                                    (eql (second p) object))))
        (dolist (combo combos)
          (let* ((props (remove nil combo))
                 (idb (problem-state.idb candidate)))
            (dolist (prop props)
              (add-proposition prop idb))
            (let ((pred-key (fps-state-base-prop-key candidate base-relations)))
              (incf (fps-layer-stats-raw-count stats))
              (cond
                ((string= pred-key frontier-key) nil)
                ((gethash pred-key reachable)
                 (incf (fps-layer-stats-collision-reachable-count stats)))
                ((gethash pred-key layer-table)
                 (incf (fps-layer-stats-collision-layer-count stats)))
                (t
                 (incf (fps-layer-stats-feasible-count stats))
                 (bw-normalize! candidate)
                 (fps-forward-validate-candidate
                  candidate pred-key frontier-key action reachable
                  base-relations layer-table stats))))
            (dolist (prop props)
              (delete-proposition prop idb))))))))


(defun fps-expand-one-state-forward (frontier-state frontier-key action
                                     reachable base-relations
                                     domains problem-actions action-objects stats)
  "Expand predecessors of FRONTIER-STATE via ACTION using on-demand variation.
   Copies frontier state ONCE per key-object, then mutates in place for each
   candidate combination.  Validates via single-pass precondition+effect.
   ACTION-OBJECTS is a precomputed list of objects the action can bind (or NIL
   to skip filtering)."
  (let* ((modified-rels (intersection (action.effect-adds action)
                                      base-relations :test #'eq)))
    (unless modified-rels
      (return-from fps-expand-one-state-forward nil))
    (let ((key-objects (fps-forward-key-objects-for-action
                        modified-rels domains action-objects)))
      (let ((*actions* problem-actions)
            (layer-table (fps-layer-stats-layer-table stats)))
        (dolist (object key-objects)
          (fps-forward-process-object-combos
           frontier-state frontier-key object modified-rels action reachable
           base-relations domains layer-table stats))))))


(defun fps-build-predecessor-enum-actions (action base-relations)
  "Build CSP enum-actions for the base relations modified by ACTION.
   Returns (values enum-actions modified-rels) or NIL if ACTION modifies no base rels.
   Binds *enumerator-base-relations* to only the modified relations, disables
   symmetry pruning, and strips the finalize action from the generated sequence."
  (let ((modified-rels (intersection (action.effect-adds action)
                                     base-relations :test #'eq)))
    (when modified-rels
      (let* ((*enumerator-base-relations* modified-rels)
             (*symmetry-pruning* nil)
             (*enum-residual-symmetry-enabled* nil)
             (enum-actions (generate-enum-actions :goal-form nil
                                                  :propagate nil
                                                  :prefilter nil
                                                  :skip-fixed-maps t)))
        (values (butlast enum-actions) modified-rels)))))


(defun fps-csp-search (state enum-actions callback)
  "Lightweight recursive DFS through CSP ENUM-ACTIONS starting from STATE.
   At each leaf (empty ENUM-ACTIONS), calls (FUNCALL CALLBACK STATE).
   Each level applies one enum-action: iterates precondition-args, applies
   precondition+effect, and recurses into each resulting child state."
  (if (null enum-actions)
      (funcall callback state)
      (let ((action (first enum-actions))
            (remaining (rest enum-actions)))
        (dolist (pre-args (action.precondition-args action))
          (let ((pre-result (apply (action.pre-defun-name action)
                                   state pre-args)))
            (when pre-result
              (let ((updates (if (eql pre-result t)
                                 (funcall (action.eff-defun-name action) state)
                                 (apply (action.eff-defun-name action)
                                        state pre-result))))
                (dolist (update updates)
                  (let ((changes (update.changes update)))
                    (when (hash-table-p changes)
                      (let ((child (make-problem-state
                                    :idb changes
                                    :hidb (problem-state.hidb state))))
                        (fps-csp-search child remaining callback))))))))))))


(defun fps-partition-enum-actions-by-key (enum-actions modified-rels)
  "Partition ENUM-ACTIONS into an alist keyed by key-object symbol.
   Enum-action names follow ENUM-<REL>-<KEY>.  For each enum-action, try each
   relation in MODIFIED-RELS as prefix ENUM-<rel>-; the remainder after the
   prefix is the key-object symbol.  Groups are returned in order of first
   appearance of each key-object.
   Returns: ((KEY1 ea1 ea2 ...) (KEY2 ea3 ea4 ...) ...)"
  (let ((result nil))
    (dolist (ea enum-actions)
      (let* ((name-str (symbol-name (action.name ea)))
             (key-sym nil))
        (dolist (rel modified-rels)
          (let* ((prefix (concatenate 'string "ENUM-" (symbol-name rel) "-"))
                 (prefix-len (length prefix)))
            (when (and (> (length name-str) prefix-len)
                       (string= name-str prefix :end1 prefix-len))
              (setf key-sym (intern (subseq name-str prefix-len) *package*))
              (return))))
        (when key-sym
          (let ((entry (assoc key-sym result :test #'eq)))
            (if entry
                (nconc entry (list ea))
                (push (list key-sym ea) result))))))
    (nreverse result)))


(defun fps-build-per-object-csp-start-state (frontier-state key-object modified-rels)
  "Copy FRONTIER-STATE and delete only KEY-OBJECT's propositions in MODIFIED-RELS.
   All other objects' propositions remain intact, providing context for :REQUIRES
   constraint pruning during CSP enumeration."
  (let ((copy (copy-problem-state frontier-state)))
    (bw--delete-props-matching!
     copy (lambda (p) (and (member (car p) modified-rels :test #'eq)
                           (eql (second p) key-object))))
    copy))


(defun fps-forward-per-object-store-validated-predecessor (leaf pred-key frontier-key action
                                                     upd reachable layer-table stats)
  "Store validated predecessor LEAF and its action/path metadata for per-object CSP flow."
  (let* ((inst (update.instantiations upd))
         (action-form (cons (action.name action) inst))
         (target-entry (gethash frontier-key reachable))
         (target-path (cdr target-entry))
         (new-path (cons action-form target-path))
         (stored (copy-problem-state leaf)))
    (incf (fps-layer-stats-validated-count stats))
    (setf (gethash pred-key layer-table)
          (cons stored new-path))))


(defun fps-forward-per-object-process-update (upd leaf pred-key frontier-key action
                                            reachable base-relations layer-table stats)
  "Process one update from per-object forward validation. Returns T when frontier is reached."
  (let ((changes (update.changes upd)))
    (when (hash-table-p changes)
      (incf (fps-layer-stats-apply-success-count stats))
      (incf (gethash (action.name action) (fps-layer-stats-op-success stats) 0))
      (let ((next-key (fps-idb-base-prop-key changes base-relations)))
        (if (string= next-key frontier-key)
            (progn
              (fps-forward-per-object-store-validated-predecessor
               leaf pred-key frontier-key action upd reachable layer-table stats)
              t)
            (progn
              (incf (fps-layer-stats-key-mismatch-count stats))
              nil))))))


(defun fps-forward-per-object-validate-leaf (leaf pred-key frontier-key action
                                       reachable base-relations layer-table stats)
  "Validate one novel per-object CSP leaf and store it when ACTION reaches FRONTIER-KEY."
  (handler-case
      (progn
        (bw-normalize! leaf)
        (when (fps-state-feasible-p leaf)
          (block validate
            (incf (fps-layer-stats-novel-validation-attempts stats))
            (incf (gethash (action.name action) (fps-layer-stats-op-attempts stats) 0))
            (dolist (pre-args (get-precondition-args action leaf))
              (let ((pre-result (apply (action.pre-defun-name action) leaf pre-args)))
                (when pre-result
                  (let ((updated-dbs
                          (if (eql pre-result t)
                              (funcall (action.eff-defun-name action) leaf)
                              (apply (action.eff-defun-name action) leaf pre-result))))
                    (dolist (upd updated-dbs)
                      (when (fps-forward-per-object-process-update
                             upd leaf pred-key frontier-key action reachable
                             base-relations layer-table stats)
                        (return-from validate t)))))))))
        nil)
    (error ()
      (incf (fps-layer-stats-apply-fail-count stats))
      (incf (gethash (action.name action) (fps-layer-stats-op-fail stats) 0))
      nil)))


(defun fps-forward-per-object-handle-leaf (leaf frontier-key action reachable
                                     base-relations layer-table stats)
  "Handle one per-object CSP leaf: dedupe checks plus validation for novel candidates."
  (let ((pred-key (fps-state-base-prop-key leaf base-relations)))
    (incf (fps-layer-stats-raw-count stats))
    (incf (gethash (action.name action) (fps-layer-stats-op-raw stats) 0))
    (cond
      ((string= pred-key frontier-key) nil)
      ((gethash pred-key reachable)
       (incf (fps-layer-stats-collision-reachable-count stats)))
      ((gethash pred-key layer-table)
       (incf (fps-layer-stats-collision-layer-count stats)))
      (t
       (incf (fps-layer-stats-feasible-count stats))
       (incf (gethash (action.name action) (fps-layer-stats-op-feasible stats) 0))
       (fps-forward-per-object-validate-leaf
        leaf pred-key frontier-key action reachable
        base-relations layer-table stats)))))


(defun fps-expand-one-state-forward-per-object-csp (frontier-state frontier-key action
                                           reachable base-relations
                                           problem-actions key-object
                                           object-enum-actions modified-rels
                                           stats)
  "Per-object CSP expansion of predecessors for FRONTIER-STATE via ACTION.
   Builds a start state with only KEY-OBJECT's MODIFIED-RELS propositions
   removed, runs CSP DFS through OBJECT-ENUM-ACTIONS, and validates each
   leaf by forward action application."
  (let ((csp-start (fps-build-per-object-csp-start-state
                    frontier-state key-object modified-rels))
        (layer-table (fps-layer-stats-layer-table stats))
        (*actions* problem-actions))
    (fps-csp-search
     csp-start object-enum-actions
     (lambda (leaf)
       (fps-forward-per-object-handle-leaf
        leaf frontier-key action reachable
        base-relations layer-table stats)))))


(defun fps-expand-predecessor-layer-forward-per-object-csp (frontier reachable
                                                   base-relations
                                                   problem-actions
                                                   selected-actions)
  "Expand one predecessor layer using per-object CSP forward candidate generation.
   Phase 1: For each selected action, build CSP enum-actions, partition by
   key-object, and filter by action's type instances.
   Phase 2: For each frontier state x action x key-object, run per-object CSP
   DFS + forward validation.
   Returns (values layer-table raw-count feasible-count validated-count diagnostics)."
  (let ((stats (make-fps-layer-stats))
        (frontier-count (length frontier))
        (frontier-idx 0)
        (last-report-time (get-internal-real-time))
        (action-info nil)
        (productive-count 0)          ;; ADDED: frontier productivity tracking
        (first-productive-idx 0)      ;; ADDED
        (last-productive-idx 0))
    ;; Phase 1: precompute per-action, per-key-object enum-action groups.
    (setf *enum-diag-printed* nil)
    (dolist (action selected-actions)
      (multiple-value-bind (enum-actions modified-rels)
          (fps-build-predecessor-enum-actions action base-relations)
        (when enum-actions
          (let* ((key-groups (fps-partition-enum-actions-by-key
                              enum-actions modified-rels))
                 (action-objects (fps-action-object-set action))
                 (filtered-groups (if action-objects
                                      (remove-if-not
                                       (lambda (g)
                                         (member (car g) action-objects :test #'eq))
                                       key-groups)
                                      key-groups)))
            (when filtered-groups
              (push (list action modified-rels filtered-groups) action-info))))))
    (setf action-info (nreverse action-info))
    (format t "~&  [forward] ~D/~D actions modify base relations. ~
               Processing ~:D frontier states...~%"
            (length action-info) (length selected-actions) frontier-count)
    (dolist (info action-info)
      (let ((a (first info))
            (groups (third info)))
        (format t "~&  [forward]   ~S: ~D key-objects ~S~%"
                (action.name a) (length groups)
                (mapcar (lambda (g) (list (car g) (1- (length g))))
                        groups))))
    (finish-output)
    ;; Phase 2: expand each frontier state.
    (dolist (target frontier)
      (incf frontier-idx)
      (when (= frontier-idx 1)
        (format t "~&  [forward] Starting frontier state 1/~:D...~%" frontier-count)
        (finish-output))
      (let ((target-key (fps-state-base-prop-key target base-relations))
            (pre-validated (fps-layer-stats-validated-count stats)))
        (dolist (info action-info)
          (let ((action (first info))
                (modified-rels (second info))
                (key-groups (third info)))
            (dolist (group key-groups)
              (let ((key-object (car group))
                    (object-enum-actions (cdr group)))
                (fps-expand-one-state-forward-per-object-csp
                 target target-key action reachable base-relations
                 problem-actions key-object object-enum-actions
                 modified-rels stats)))))
        (when (> (fps-layer-stats-validated-count stats) pre-validated)
          (incf productive-count)
          (when (zerop first-productive-idx)
            (setf first-productive-idx frontier-idx))
          (setf last-productive-idx frontier-idx)))
      (let ((now (get-internal-real-time)))
        (when (or (= frontier-idx 1)
                  (>= (- now last-report-time)
                       (* 30 internal-time-units-per-second)))
          (format t "~&  [forward] ~:D/~:D frontier states (~,1F%), ~
                     raw=~:D, feasible=~:D, validated=~:D, found=~:D~%"
                  frontier-idx frontier-count
                  (* 100.0 (/ frontier-idx frontier-count))
                  (fps-layer-stats-raw-count stats)
                  (fps-layer-stats-feasible-count stats)
                  (fps-layer-stats-validated-count stats)
                  (hash-table-count (fps-layer-stats-layer-table stats)))
          (finish-output)
          (setf last-report-time now))))
    (let ((diag (fps-build-layer-diagnostics stats)))  ;; ADDED: append frontier productivity
      (setf (getf diag :productive-frontier-count) productive-count
            (getf diag :first-productive-frontier) first-productive-idx
            (getf diag :last-productive-frontier) last-productive-idx
            (getf diag :frontier-count) frontier-count)
      (values (fps-layer-stats-layer-table stats)
              (fps-layer-stats-raw-count stats)
              (fps-layer-stats-feasible-count stats)
              (fps-layer-stats-validated-count stats)
              diag))))


(defun fps-expand-predecessor-layer-forward-ondemand (frontier reachable
                                                       base-relations
                                                       problem-actions
                                                       selected-actions)
  "Expand one predecessor layer using on-demand forward candidate generation.
   For each frontier state and each selected action, generates candidate
   predecessors by varying only the base propositions the action can modify,
   then validates each by forward application.
   Returns (values layer-table raw-count feasible-count validated-count diagnostics)."
  (let ((stats (make-fps-layer-stats))
        (domains (fps-compute-base-relation-domains base-relations))
        (frontier-count (length frontier))
        (frontier-idx 0)
        (last-report-time (get-internal-real-time))
        ;; Precompute per-action object sets for key-object filtering.
        (action-object-sets (mapcar (lambda (a) (cons a (fps-action-object-set a)))
                                    selected-actions)))
    (format t "~&  [forward-ondemand] Domains computed. Processing ~:D frontier states x ~:D actions...~%"
            frontier-count (length selected-actions))
    (finish-output)
    (dolist (target frontier)
      (incf frontier-idx)
      (when (= frontier-idx 1)
        (format t "~&  [forward-ondemand] Starting frontier state 1/~:D...~%" frontier-count)
        (finish-output))
      (let ((target-key (fps-state-base-prop-key target base-relations)))
        (dolist (action-entry action-object-sets)
          (fps-expand-one-state-forward target target-key (car action-entry)
                                        reachable base-relations domains
                                        problem-actions (cdr action-entry) stats)))
      (let ((now (get-internal-real-time)))
        (when (or (= frontier-idx 1)
                  (>= (- now last-report-time)
                       (* 5 internal-time-units-per-second)))
          (format t "~&  [forward-ondemand] ~:D/~:D frontier states (~,1F%%), ~
                     raw=~:D, feasible=~:D, validated=~:D, found=~:D~%"
                  frontier-idx frontier-count
                  (* 100.0 (/ frontier-idx frontier-count))
                  (fps-layer-stats-raw-count stats)
                  (fps-layer-stats-feasible-count stats)
                  (fps-layer-stats-validated-count stats)
                  (hash-table-count (fps-layer-stats-layer-table stats)))
          (finish-output)
          (setf last-report-time now))))
    (values (fps-layer-stats-layer-table stats)
            (fps-layer-stats-raw-count stats)
            (fps-layer-stats-feasible-count stats)
            (fps-layer-stats-validated-count stats)
            (fps-build-layer-diagnostics stats))))


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
            (when (fps-state-feasible-p pred)
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


(defun fps-print-layer-diagnostics (direction layer diagnostics)
  "Print layer diagnostic details when DIAGNOSTICS is non-nil."
  (when diagnostics
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
            (getf diagnostics :op-sample-detail))
    (when (getf diagnostics :productive-frontier-count)  ;; ADDED: frontier productivity
      (format t "~&[find-predecessors ~S] Layer ~D diagnostics (frontier productivity): ~
                 ~:D of ~:D frontier states produced validations (first idx=~:D, last idx=~:D).~%"
              direction layer
              (getf diagnostics :productive-frontier-count)
              (getf diagnostics :frontier-count)
              (getf diagnostics :first-productive-frontier)
              (getf diagnostics :last-productive-frontier)))))


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


(defun %find-predecessors-fn (&key (action-families :auto)
                                   (direction 'forward))
  "Interactive iterative predecessor enumeration from *ENUMERATED-GOAL-STATES*.
   DIRECTION BACKWARD regresses from the current frontier.
   DIRECTION FORWARD generates predecessor candidates on demand by varying
   base propositions each action can modify, then validates by forward application.
   When no *BACKWARD-REACHABLE-SET* exists, :FORWARD auto-seeds from goal states.
   Prompts (y-or-n-p) after each layer. Ctrl-C safely reverts to the last
   completed layer."
  (unless *enumerated-goal-states*
    (format t "~&[find-predecessors] No goal states. Run (find-goal-states) first.~%")
    (return-from %find-predecessors-fn nil))
  (when (or (> *debug* 0) *probe*)
    (format t "~&[find-predecessors] Please reset *debug* and *probe* first.~%")
    (return-from %find-predecessors-fn nil))
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
                       (or *backward-reachable-set*
                           ;; Auto-seed from enumerated goal states
                           (let ((ht (make-hash-table :test 'equal)))
                             (dolist (gs *enumerated-goal-states*)
                               (let ((key (fps-state-base-prop-key gs base-relations)))
                                 (unless (gethash key ht)
                                   (setf (gethash key ht) (cons gs nil)))))
                             (setf *backward-reachable-set* ht)
                             ht)))))
         (frontier nil)
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
               (hash-table-count reachable)))
      (:forward
       (format t "~&[find-predecessors ~S] Starting from reachable set: ~:D states.~%"
               norm-direction
               (hash-table-count reachable))))
    ;; Seed frontier from reachable set (both directions use same pattern)
    (setf frontier (loop for entry being the hash-values of reachable
                         collect (car entry)))
    ;; Iterative expansion
    (loop
      (incf layer)
      (format t "~&[find-predecessors ~S] Computing layer ~D from ~:D frontier states...~%"
              norm-direction layer (length frontier))
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
                 (fps-expand-predecessor-layer-forward-per-object-csp frontier reachable base-relations
                                                              problem-actions selected-actions))))
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
          (setf frontier (loop for entry being the hash-values of layer-table
                               collect (car entry)))
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


(defun %enumerate-state (goal-spec &key (algorithm *algorithm*) (solution-type 'first)
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


(defun %enumerate-state-csp (goal-spec &key (algorithm *algorithm*) (solution-type 'first)
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
      (let* ((effective-prefilter *enumerator-prefilter*)
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
                                        (let ((sym '*known-goal-state*))
                                          (and (boundp sym)
                                               (symbol-value sym))))
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

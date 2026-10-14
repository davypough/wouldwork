;;; Filename: ww-enumerator-run.lisp

(in-package :ww)

(defparameter *find-predecessors-print-diagnostics* nil
  "When non-NIL, FIND-PREDECESSORS prints per-layer diagnostics details.
By default diagnostics are suppressed and only layer summary lines are printed.")

(defparameter *solve-meeting-point-print-diagnostics* nil
  "When non-NIL, SOLVE-MEETING-POINT prints per-solution expansion diagnostics.")

(defparameter *solve-meeting-point-last-diagnostics* nil
  "Diagnostics plist from the last SOLVE-MEETING-POINT run.")

(defparameter *backward-reachable-max-witnesses-per-key* 8
  "Maximum witness count stored per backward-reachable key.
Set to NIL for no cap.")

(defparameter *fps-worker-inherited-specials*
  '(*package* *standard-output* *print-pretty* *print-case* *print-circle* *print-escape*
    *print-readably* *print-base* *print-radix* *print-length* *print-level* *print-lines*
    *print-right-margin* *print-array* *print-gensym* *read-default-float-format*)
  "Specials that parallel FIND-PREDECESSORS workers rebind to the caller's values.
New threads see only global values, and predecessor keys are printed strings, so a
worker must print exactly as the caller does for its keys to match the reachable set.")

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
                               (actions :all actions-supplied-p)
                               (direction 'forward direction-supplied-p)
                               (layers 1)
                               (save t))
  "User-facing macro wrapper for FIND-PREDECESSORS-FN.
   Automatically quotes literal symbols/lists for :ACTIONS and :DIRECTION,
   so users can write e.g. (find-predecessors :direction forward
                                              :actions (move pickup-connector)).
   :ACTIONS defaults to every action in the problem (see ENUM-SELECT-ACTIONS).
   :LAYERS is the number of new layers to compute before returning (default 1).
   :SAVE (default T) calls SAVE-PREDECESSORS after each layer that adds states."
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
        ,@(if actions-supplied-p
              `(:actions ,(maybe-quote actions))
              '(:actions :all))
        ,@(if direction-supplied-p
              `(:direction ,(maybe-quote direction))
              '(:direction 'forward))
        :layers ,layers
        :save ,save))))


(defun find-goal-states-fn (goal-spec &key (algorithm *algorithm*) (solution-type 'every)
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


(defun find-predecessors-fn (&key (actions :all)
                                  (direction 'forward)
                                  (layers 1)
                                  (save t))
  "Top-level API wrapper. Full implementation is defined below in
   %FIND-PREDECESSORS-FN to keep call flow top-down in this file."
  (%find-predecessors-fn :actions actions
                         :direction direction
                         :layers layers
                         :save save))


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


(defun %find-goal-states-fn (goal-spec &key (algorithm *algorithm*) (solution-type 'every)
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


(defun enum-select-actions (actions names)
  "Select ACTION structs from ACTIONS named by NAMES.
   NAMES :ALL (or NIL, or a list containing ALL) selects every action.  Otherwise
   each name is an action name or an older corner-model alias: PICKUP means
   PICKUP-CONNECTOR, CONNECT means CONNECT-TO-1/2/3-TERMINUS.  Keywords are
   accepted for any name.  A name matching no action in ACTIONS is an error."
  (let ((name-list (fgs-ensure-list names))
        (wanted nil))
    (when (or (null name-list)
              (some (lambda (name) (string= name "ALL")) name-list))
      (return-from enum-select-actions (copy-list actions)))
    (dolist (name name-list)
      (let* ((sym (intern (symbol-name name) *package*))
             (expanded (case sym
                         (pickup '(pickup-connector))
                         (connect '(connect-to-1-terminus
                                    connect-to-2-terminus
                                    connect-to-3-terminus))
                         (otherwise (list sym)))))
        (unless (some (lambda (n) (find n actions :key #'action.name :test #'eq))
                      expanded)
          (error "FIND-PREDECESSORS: no action named ~S in this problem." name))
        (setf wanted (union wanted expanded :test #'eq))))
    (remove-if-not (lambda (a) (member (action.name a) wanted :test #'eq))
                   actions)))


(defun enum-action-in-family-p (action-name family)
  "True iff ACTION-NAME belongs to FAMILY (MOVE, PICKUP, CONNECT).
   Used only by the :BACKWARD direction, whose regressions exist only for the
   older corner model's actions."
  (case family
    (move (eql action-name 'move))
    (pickup (eql action-name 'pickup-connector))
    (connect (member action-name
                     '(connect-to-1-terminus
                       connect-to-2-terminus
                       connect-to-3-terminus)
                     :test #'eq))
    (otherwise nil)))


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


(defun fps-state-feasible-p (state)
  "Unified state feasibility predicate for enumeration and predecessor expansion.
   When the user defines STATE-FEASIBLE? in the problem spec, calls it to test
   whether STATE is physically reachable (e.g., agent accessibility).
   Returns T when no hook is defined or when the hook returns non-NIL."
  (and (not (state-is-inconsistent state))
       (or (not (fboundp 'state-feasible?))
           (funcall (symbol-function 'state-feasible?) state))))


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
     (enum-base-props-of (list-database (problem-state.idb state)) rels)
     (lambda (x y)
       (string< (prin1-to-string x)
                (prin1-to-string y))))))


(defun fps-state-base-prop-key (state &optional base-relations)
  "Return canonical key for STATE from sorted base-relation propositions."
  (prin1-to-string (enum-state-base-props state base-relations)))


(defun fps-state-full-prop-key (state)
  "Return canonical key over all propositions in STATE."
  (prin1-to-string
   (sort (list-database (problem-state.idb state))
         (lambda (x y)
           (string< (prin1-to-string x)
                    (prin1-to-string y))))))


(defun fps-make-witness-entry (state path)
  "Build a reachable-set entry containing one witness (STATE . PATH)."
  (list (cons state path)))


(defun fps-normalize-witness-entry (entry)
  "Normalize ENTRY to witness-list format: ((STATE . PATH) ...).
   Supports legacy single-witness format (STATE . PATH)."
  (cond
    ((null entry) nil)
    ((and (consp entry) (typep (car entry) 'problem-state))
     (list (cons (car entry) (cdr entry))))
    (t entry)))


(defun fps-entry-primary-witness (entry)
  "Return the primary witness pair (STATE . PATH) from ENTRY."
  (first (fps-normalize-witness-entry entry)))


(defun fps-entry-primary-state (entry)
  "Return the primary witness state from ENTRY."
  (car (fps-entry-primary-witness entry)))


(defun fps-entry-primary-path (entry)
  "Return the primary witness path from ENTRY."
  (cdr (fps-entry-primary-witness entry)))


(defun fps-entry-max-depth (entry)
  "Return maximum stored backward depth across ENTRY witnesses."
  (let ((norm (fps-normalize-witness-entry entry)))
    (if (null norm)
      0
      (reduce #'max norm :key (lambda (w) (length (cdr w))) :initial-value 0))))


(defun fps-entry-add-witness (entry state path)
  "Add witness (STATE . PATH) to ENTRY and apply witness cap.
   Dedupes by exact (state-full-key, path) pair."
  (let* ((existing (fps-normalize-witness-entry entry))
         (state-full-key (fps-state-full-prop-key state))
         (already (find-if (lambda (w)
                             (and (equal path (cdr w))
                                  (string= state-full-key
                                           (fps-state-full-prop-key (car w)))))
                           existing))
         (updated (if already existing (cons (cons state path) existing)))
         (cap *backward-reachable-max-witnesses-per-key*))
    (if (and cap (integerp cap) (> (length updated) cap))
        (subseq updated 0 cap)
        updated)))


(defun fps-merge-witness-entries (entry-a entry-b)
  "Merge ENTRY-B witnesses into ENTRY-A and return merged entry."
  (let ((merged (fps-normalize-witness-entry entry-a)))
    (dolist (w (fps-normalize-witness-entry entry-b) merged)
      (setf merged (fps-entry-add-witness merged (car w) (cdr w))))))


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


(defun fps-base-prop-set (state base-relations)
  "Return an EQUAL hash table holding STATE's base-relation propositions
   (bijective relations under their own name, see ENUM-BASE-PROPS-OF)."
  (let ((set (make-hash-table :test 'equal)))
    (dolist (p (enum-base-props-of (list-database (problem-state.idb state)) base-relations) set)
      (setf (gethash p set) t))))


(defun fps-idb-base-props-match-p (idb base-relations frontier-props)
  "True iff IDB's base-relation propositions (see ENUM-BASE-PROPS-OF) are exactly
   the set FRONTIER-PROPS (built by FPS-BASE-PROP-SET).  Stops at the first
   proposition not in the set, so a mismatch costs no key building."
  (let ((count 0))
    (dolist (p (enum-base-props-of (list-database idb) base-relations)
               (= count (hash-table-count frontier-props)))
      (unless (gethash p frontier-props)
        (return nil))
      (incf count))))


(defun fps-idb-full-prop-key (idb)
  "Return canonical full-prop key computed directly from IDB hash-table."
  (prin1-to-string
   (sort (list-database idb)
         (lambda (x y)
           (string< (prin1-to-string x)
                    (prin1-to-string y))))))


(defun fps-path-replayable-from-state-p (state path)
  "Return T iff PATH can be replayed forward from STATE without failure."
  (let ((trial-state (copy-problem-state state))
        (ok t))
    (dolist (action-form path ok)
      (when ok
        (multiple-value-bind (next-state success-p failure-reason)
            (apply-action-to-state action-form trial-state nil nil)
          (declare (ignore failure-reason))
          (if (and success-p (not (state-is-inconsistent next-state)))
              (setf trial-state next-state)
              (setf ok nil)))))))


(defun fps-entry-path-for-successor (entry &key successor-state successor-idb)
  "Return matching witness pair (STATE . PATH) from ENTRY.
   SUCCESSOR-STATE or SUCCESSOR-IDB must be provided.
   Falls back to first witness whose PATH replay succeeds from SUCCESSOR-STATE.
   Note: PATH may be NIL (valid for layer-0 goal witnesses)."
  (let* ((norm (fps-normalize-witness-entry entry))
         (successor-full-key (cond
                               (successor-state
                                (fps-state-full-prop-key successor-state))
                               (successor-idb
                                (fps-idb-full-prop-key successor-idb))
                               (t nil))))
    (when norm
      (let ((match (and successor-full-key
                        (find successor-full-key norm
                              :test #'string=
                              :key (lambda (w)
                                     (fps-state-full-prop-key (car w)))))))
        (or match
            (and successor-state
                 (loop for w in norm
                       when (fps-path-replayable-from-state-p successor-state (cdr w))
                         do (return w))))))))


(defun fps-select-target-witness-for-successor (entry successor-state)
  "Pick a witness from ENTRY for SUCCESSOR-STATE.
   Prefers exact/replayable match; falls back to ENTRY primary witness
   to preserve base-reachability completeness."
  (or (fps-entry-path-for-successor entry :successor-state successor-state)
      (fps-entry-primary-witness entry)))


(defun fps-target-witness-paths-for-successor (entry successor-state)
  "Return PATH list to propagate from ENTRY for SUCCESSOR-STATE.
   Preference:
   1) all replayable witness paths from SUCCESSOR-STATE
   2) all witness paths in ENTRY (completeness fallback)."
  (let* ((norm (fps-normalize-witness-entry entry))
         (replayable (and successor-state
                          (remove-if-not
                           (lambda (w)
                             (fps-path-replayable-from-state-p successor-state (cdr w)))
                           norm)))
         (chosen (if (and replayable (not (null replayable)))
                     replayable
                     norm)))
    (mapcar #'cdr chosen)))


(defun fps-state-relation-signature-key (state relations)
  "Return canonical key for STATE projected to RELATIONS.
   When RELATIONS is NIL, returns NIL (no projection constraint)."
  (when relations
    (prin1-to-string (enum-state-base-props state relations))))


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
   appearance of each key-object.  An enum-action named exactly ENUM-<REL> has no
   key object (eg a bijective relation enumerated as one assignment).
   Returns two values: ((KEY1 ea1 ea2 ...) (KEY2 ea3 ea4 ...) ...) and the
   unkeyed enum-actions as ((REL . ea) ...)."
  (let ((result nil)
        (unkeyed nil))
    (dolist (ea enum-actions)
      (let* ((name-str (symbol-name (action.name ea)))
             (key-sym nil))
        (dolist (rel modified-rels)
          (let* ((prefix (concatenate 'string "ENUM-" (symbol-name rel) "-"))
                 (prefix-len (length prefix)))
            (when (string= name-str prefix :end2 (1- prefix-len))
              (push (cons rel ea) unkeyed)
              (return))
            (when (and (> (length name-str) prefix-len)
                       (string= name-str prefix :end1 prefix-len))
              (setf key-sym (intern (subseq name-str prefix-len) *package*))
              (return))))
        (when key-sym
          (let ((entry (assoc key-sym result :test #'eq)))
            (if entry
                (nconc entry (list ea))
                (push (list key-sym ea) result))))))
    (values (nreverse result) (nreverse unkeyed))))


(defun fps-build-object-sets (key-groups unkeyed modified-rels)
  "Build the object sets whose base facts one predecessor guess re-enumerates.
   Each set is (MEMBERS CLEAR-SPEC ENUM-ACTIONS), where CLEAR-SPEC is a list of
   (OBJECT . RELATIONS) naming the facts cleared before enumeration.
   Every key object in KEY-GROUPS forms a single set over the keyed relations.
   Every value (HOLDER HELD) of an UNKEYED relation (eg bijective HOLDING) forms a
   pair set: HELD is re-enumerated in all MODIFIED-RELS, HOLDER only in that
   relation, whose enumeration is restricted to the pair's own link or none."
  (let ((keyed-rels (set-difference modified-rels (mapcar #'car unkeyed) :test #'eq))
        (sets nil))
    (dolist (group key-groups)
      (push (list (list (car group))
                  (list (cons (car group) keyed-rels))
                  (cdr group))
            sets))
    (dolist (entry unkeyed)
      (dolist (tuple (action.precondition-args (cdr entry)))
        (unless (eq (first tuple) :unassigned)
          (let ((holder (first tuple))
                (held (second tuple))
                (restricted (copy-action (cdr entry))))
            (setf (action.precondition-args restricted) (list tuple '(:unassigned)))
            (push (list (list holder held)
                        (list (list holder (car entry)) (cons held modified-rels))
                        (append (cdr (assoc held key-groups :test #'eq))
                                (list restricted)))
                  sets)))))
    (nreverse sets)))


(defun fps-build-per-object-csp-start-state (frontier-state clear-spec)
  "Copy FRONTIER-STATE and delete the facts named by CLEAR-SPEC, a list of
   (OBJECT . RELATIONS).  A fact is deleted when its relation is listed for an
   object that is its key (first argument), or, for a bijective relation, any of
   its arguments.  Bijective facts are deleted through the relation's own name, so
   both index facts go.  All other facts remain, providing context for :REQUIRES
   constraint pruning during CSP enumeration."
  (let* ((copy (copy-problem-state frontier-state))
         (idb (problem-state.idb copy))
         (doomed nil))
    (dolist (p (list-database idb))
      (let ((rel (or (car (gethash (car p) *bijective-canonical*)) (car p))))
        (when (some (lambda (entry)
                      (and (member rel (cdr entry) :test #'eq)
                           (if (gethash rel *bijective-relations*)
                               (member (car entry) (cdr p) :test #'eq)
                               (eql (second p) (car entry)))))
                    clear-spec)
          (pushnew (cons rel (cdr p)) doomed :test #'equal))))
    (dolist (p doomed)
      (delete-proposition p idb))
    (invalidate-problem-state-hash copy)
    copy))


(defun fps-forward-per-object-store-validated-predecessor (leaf action upd next-state
                                                           reachable layer-table stats)
  "Store validated predecessor LEAF and its action/path metadata for per-object CSP flow.
   The reachable set and layer table are keyed by canonical key (connector-renamed
   twins share one entry).  NEXT-STATE has the frontier state's base facts, so its
   canonical key finds the frontier's entry; successor-aware witness selection
   keeps the paths that replay from NEXT-STATE."
  (let* ((inst (update.instantiations upd))
         (action-form (cons (action.name action) inst))
         (target-entry (gethash (enum-solution-state-key next-state) reachable))
         (target-paths (and target-entry
                            (fps-target-witness-paths-for-successor target-entry
                                                                    next-state))))
    (when target-paths
      (let* ((pred-key (enum-solution-state-key leaf))
             (stored (copy-problem-state leaf))
             (existing (gethash pred-key layer-table)))
        (when (gethash pred-key reachable)
          (incf (fps-layer-stats-collision-reachable-count stats)))
        (when existing
          (incf (fps-layer-stats-collision-layer-count stats)))
        (incf (fps-layer-stats-validated-count stats))
        (dolist (target-path target-paths)
          (setf existing
                (fps-entry-add-witness existing stored (cons action-form target-path))))
        (setf (gethash pred-key layer-table) existing)
        t))))


(defun fps-forward-per-object-process-update (upd leaf frontier-props action reachable
                                              base-relations layer-table stats)
  "Process one update from per-object forward validation. Returns T when frontier is reached."
  (if (update-is-inconsistent upd)
      nil
      (let ((changes (update.changes upd)))
    (when (hash-table-p changes)
      (incf (fps-layer-stats-apply-success-count stats))
      (incf (gethash (action.name action) (fps-layer-stats-op-success stats) 0))
      (if (fps-idb-base-props-match-p changes base-relations frontier-props)
          (let ((next-state (make-problem-state :idb changes
                                                :hidb (problem-state.hidb leaf))))
            (if (fps-forward-per-object-store-validated-predecessor
                 leaf action upd next-state reachable layer-table stats)
                t
                nil))
          (progn
            (incf (fps-layer-stats-key-mismatch-count stats))
            nil))))))


(defun fps-forward-per-object-validate-leaf (leaf frontier-props action reachable
                                             base-relations layer-table stats)
  "Validate one novel per-object CSP leaf and store it when ACTION reaches the frontier state.
   Errors are not caught: a failing candidate stops the layer where it fails."
  (bw-normalize! leaf)
  (when (fps-state-feasible-p leaf)
    (maybe-propagate-changes! leaf)
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
                       upd leaf frontier-props action reachable
                       base-relations layer-table stats)
                  (return-from validate t))))))))))


(defun fps-forward-per-object-handle-leaf (leaf leaf-key frontier-key frontier-props action
                                     reachable base-relations layer-table stats seen)
  "Handle one per-object CSP leaf: skip a leaf already handled for this frontier
   state and ACTION (object sets overlap), skip a leaf equal to the frontier state,
   then validate.  Collisions are counted on validated predecessors (canonical keys)."
  (let ((seen-key (concatenate 'string (symbol-name (action.name action)) " " leaf-key)))
    (unless (gethash seen-key seen)
      (setf (gethash seen-key seen) t)
      (let ((pred-key (fps-state-base-prop-key leaf base-relations)))
        (incf (fps-layer-stats-raw-count stats))
        (incf (gethash (action.name action) (fps-layer-stats-op-raw stats) 0))
        (cond
          ((string= pred-key frontier-key) nil)
          (t
           (incf (fps-layer-stats-feasible-count stats))
           (incf (gethash (action.name action) (fps-layer-stats-op-feasible stats) 0))
           ;; Even on collisions, validate to accumulate additional witnesses.
           (fps-forward-per-object-validate-leaf
            leaf frontier-props action reachable base-relations layer-table stats)))))))


(defun fps-expand-one-state-forward-per-object-csp (frontier-state frontier-key frontier-props
                                           actions reachable base-relations
                                           problem-actions object-set
                                           stats seen)
  "Per-object-set CSP expansion of predecessors for FRONTIER-STATE via ACTIONS.
   OBJECT-SET is (MEMBERS CLEAR-SPEC ENUM-ACTIONS).  Builds a start state with the
   CLEAR-SPEC facts removed, runs CSP DFS through ENUM-ACTIONS, and validates
   each leaf by forward application of every action in ACTIONS.  SEEN records
   the leaves already handled for FRONTIER-STATE."
  (let ((csp-start (fps-build-per-object-csp-start-state
                    frontier-state (second object-set)))
        (layer-table (fps-layer-stats-layer-table stats))
        (*actions* problem-actions))
    (fps-csp-search
     csp-start (third object-set)
     (lambda (leaf)
       (let ((leaf-key (fps-state-full-prop-key leaf)))
         (dolist (action actions)
           (fps-forward-per-object-handle-leaf
            leaf leaf-key frontier-key frontier-props action reachable
            base-relations layer-table stats seen)))))))


(defun fps-expand-predecessor-layer-forward-per-object-csp (frontier reachable
                                                   base-relations
                                                   problem-actions
                                                   selected-actions)
  "Expand one predecessor layer using per-object CSP forward candidate generation.
   Phase 1: Group selected actions by modified base-relations, build CSP
   enum-actions once per group, build object sets (single key objects, plus
   holder-held pairs of unkeyed bijective relations), and filter by union of
   action type instances.
   Phase 2: For each frontier state x group x object set, run the CSP DFS once
   and validate each leaf against all applicable actions in the group.
   Returns (values layer-table raw-count feasible-count validated-count diagnostics)."
  (let ((stats (make-fps-layer-stats))
        (frontier-count (length frontier))
        (rels-groups nil))
    ;; Phase 1: group actions by modified base-rels, build CSP enum-actions per group.
    (setf *enum-diag-printed* nil)
    (let ((rels-table (make-hash-table :test #'equal)))
      ;; 1a. Group actions by sorted modified base-rels.
      (dolist (action selected-actions)
        (let ((modified-rels (intersection (action.effect-adds action)
                                           base-relations :test #'eq)))
          (when modified-rels
            (let ((sorted-rels (sort (copy-list modified-rels) #'string<
                                     :key #'symbol-name))
                  (obj-set (fps-action-object-set action)))
              (push (cons action obj-set)
                    (gethash sorted-rels rels-table))))))
      ;; 1b. For each group, generate CSP enum-actions once, build object sets, filter.
      (maphash
       (lambda (sorted-rels action-entries)
         (setf action-entries (nreverse action-entries))
         (let* ((representative (car (first action-entries)))
                (enum-actions (fps-build-predecessor-enum-actions
                               representative base-relations)))
           (when enum-actions
             (multiple-value-bind (key-groups unkeyed)
                 (fps-partition-enum-actions-by-key enum-actions sorted-rels)
               (let* ((any-dynamic (some (lambda (ae) (null (cdr ae)))
                                         action-entries))
                      (union-objects
                        (unless any-dynamic
                          (let ((result nil))
                            (dolist (ae action-entries result)
                              (dolist (obj (cdr ae))
                                (pushnew obj result :test #'eq))))))
                      (object-sets (fps-build-object-sets key-groups unkeyed sorted-rels))
                      (filtered-sets
                        (if union-objects
                            (remove-if-not
                             (lambda (os)
                               (some (lambda (m) (member m union-objects :test #'eq))
                                     (first os)))
                             object-sets)
                            object-sets)))
                 (when filtered-sets
                   (push (list sorted-rels filtered-sets action-entries)
                         rels-groups)))))))
       rels-table))
    (setf rels-groups (sort rels-groups #'<
                            :key (lambda (rg) (length (first rg)))))
    ;; Report grouping results.
    (let ((total-actions (loop for rg in rels-groups
                               sum (length (third rg)))))
      (format t "~&  [forward] ~D CSP group~:P from ~D/~D actions. ~
                 Processing ~:D frontier states...~%"
              (length rels-groups) total-actions (length selected-actions)
              frontier-count))
    (dolist (rg rels-groups)
      (let ((modified-rels (first rg))
            (object-sets (second rg))
            (action-entries (third rg)))
        (format t "~&  [forward]   ~S: ~D action~:P ~S, ~D object set~:P ~S~%"
                modified-rels
                (length action-entries)
                (mapcar (lambda (ae) (action.name (car ae))) action-entries)
                (length object-sets)
                (mapcar (lambda (os) (list (first os) (length (third os))))
                        object-sets))))
    (finish-output)
    ;; Phase 2: expand each frontier state, serially or on worker threads.
    (multiple-value-bind (productive-count first-productive-idx last-productive-idx)
        (if (plusp *threads*)
            (fps-expand-frontier-parallel frontier rels-groups reachable base-relations
                                          problem-actions stats)
            (fps-expand-frontier-serial frontier rels-groups reachable base-relations
                                        problem-actions stats))
      (let ((diag (fps-build-layer-diagnostics stats)))
        (setf (getf diag :productive-frontier-count) productive-count
              (getf diag :first-productive-frontier) first-productive-idx
              (getf diag :last-productive-frontier) last-productive-idx
              (getf diag :frontier-count) frontier-count)
        (values (fps-layer-stats-layer-table stats)
                (fps-layer-stats-raw-count stats)
                (fps-layer-stats-feasible-count stats)
                (fps-layer-stats-validated-count stats)
                diag)))))


(defun fps-expand-frontier-serial (frontier rels-groups reachable base-relations
                                   problem-actions stats)
  "Expand FRONTIER states in order into STATS, reporting progress every 30 seconds.
   Returns (values productive-count first-productive-idx last-productive-idx)."
  (let ((frontier-count (length frontier))
        (frontier-idx 0)
        (last-report-time (get-internal-real-time))
        (productive-count 0)
        (first-productive-idx 0)
        (last-productive-idx 0))
    (dolist (target frontier)
      (incf frontier-idx)
      (when (= frontier-idx 1)
        (format t "~&  [forward] Starting frontier state 1/~:D...~%" frontier-count)
        (finish-output))
      (when (fps-expand-frontier-state target rels-groups reachable base-relations
                                       problem-actions stats)
        (incf productive-count)
        (when (zerop first-productive-idx)
          (setf first-productive-idx frontier-idx))
        (setf last-productive-idx frontier-idx))
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
    (values productive-count first-productive-idx last-productive-idx)))


(defun fps-expand-frontier-parallel (frontier rels-groups reachable base-relations
                                     problem-actions stats)
  "Expand FRONTIER states on *THREADS* workers, then merge their results into STATS.
   Each task is (INDEX . STATE).  Each worker owns its stats and layer table and only
   reads REACHABLE.  Workers start with the caller's values of
   *FPS-WORKER-INHERITED-SPECIALS*.  A worker error stops the run and is resignaled.
   Returns (values productive-count first-productive-idx last-productive-idx)."
  (let* ((queue (make-new-task-queue))
         (results (make-array *threads* :initial-element nil))
         (job (list :rels-groups rels-groups
                    :reachable reachable
                    :base-relations base-relations
                    :problem-actions problem-actions
                    :results results
                    :special-values (mapcar #'symbol-value *fps-worker-inherited-specials*)
                    :frontier-count (length frontier)
                    :progress (list 0 (get-internal-real-time)
                                    (sb-thread:make-mutex :name "fps-frontier-progress"))))
         (productive-count 0)
         (first-productive-idx 0)
         (last-productive-idx 0))
    (format t "~&  [forward] Expanding ~:D frontier states on ~D threads...~%"
            (length frontier) *threads*)
    (finish-output)
    (tq-push-many queue (loop for target in frontier
                              for idx from 1
                              collect (cons idx target)))
    (tq-signal-done queue)
    (reset-parallel-control-flags)
    (call-with-parallel-search-lifetime
     (lambda ()
       (run-parallel-worker-group
        queue *threads*
        :function (lambda (worker-id worker-queue)
                    (fps-forward-per-object-worker worker-id worker-queue job)))))
    (loop for result across results
          do (fps-merge-layer-stats stats (first result))
             (incf productive-count (second result))
             (when (and (plusp (third result))
                        (or (zerop first-productive-idx)
                            (< (third result) first-productive-idx)))
               (setf first-productive-idx (third result)))
             (setf last-productive-idx (max last-productive-idx (fourth result))))
    (values productive-count first-productive-idx last-productive-idx)))


(defun fps-forward-per-object-worker (worker-id queue job)
  "Worker loop for FPS-EXPAND-FRONTIER-PARALLEL: pop (INDEX . STATE) tasks from QUEUE
   and expand each into this worker's own stats.  Saves
   (STATS PRODUCTIVE-COUNT FIRST-PRODUCTIVE-IDX LAST-PRODUCTIVE-IDX) in JOB's results."
  (progv *fps-worker-inherited-specials* (getf job :special-values)
    (let ((stats (make-fps-layer-stats))
          (productive-count 0)
          (first-productive-idx 0)
          (last-productive-idx 0))
      (tq-register-worker queue)
      (loop for task = (tq-pop-blocking queue)
            while task
            do (when (fps-expand-frontier-state (cdr task)
                                                (getf job :rels-groups)
                                                (getf job :reachable)
                                                (getf job :base-relations)
                                                (getf job :problem-actions)
                                                stats)
                 (incf productive-count)
                 (when (or (zerop first-productive-idx)
                           (< (car task) first-productive-idx))
                   (setf first-productive-idx (car task)))
                 (setf last-productive-idx (max last-productive-idx (car task))))
               (fps-report-parallel-frontier-progress job))
      (setf (aref (getf job :results) worker-id)
            (list stats productive-count first-productive-idx last-productive-idx)))))


(defun fps-report-parallel-frontier-progress (job)
  "Count one finished frontier state for JOB; print the total at most every 30 seconds."
  (let ((progress (getf job :progress))
        (frontier-count (getf job :frontier-count)))
    (sb-thread:with-mutex ((third progress))
      (incf (first progress))
      (let ((now (get-internal-real-time)))
        (when (>= (- now (second progress)) (* 30 internal-time-units-per-second))
          (format t "~&  [forward] ~:D/~:D frontier states (~,1F%)~%"
                  (first progress) frontier-count
                  (* 100.0 (/ (first progress) frontier-count)))
          (finish-output)
          (setf (second progress) now))))))


(defun fps-merge-layer-stats (into from)
  "Add FROM's counters, per-action tables and layer table into INTO.  Per-action
   counts are summed, sample tables keep INTO's entry, and layer-table entries for
   the same predecessor key have their witnesses merged."
  (incf (fps-layer-stats-raw-count into) (fps-layer-stats-raw-count from))
  (incf (fps-layer-stats-feasible-count into) (fps-layer-stats-feasible-count from))
  (incf (fps-layer-stats-validated-count into) (fps-layer-stats-validated-count from))
  (incf (fps-layer-stats-collision-reachable-count into)
        (fps-layer-stats-collision-reachable-count from))
  (incf (fps-layer-stats-collision-layer-count into)
        (fps-layer-stats-collision-layer-count from))
  (incf (fps-layer-stats-novel-validation-attempts into)
        (fps-layer-stats-novel-validation-attempts from))
  (incf (fps-layer-stats-apply-success-count into) (fps-layer-stats-apply-success-count from))
  (incf (fps-layer-stats-apply-fail-count into) (fps-layer-stats-apply-fail-count from))
  (incf (fps-layer-stats-key-mismatch-count into) (fps-layer-stats-key-mismatch-count from))
  (dolist (accessor (list #'fps-layer-stats-op-attempts #'fps-layer-stats-op-success
                          #'fps-layer-stats-op-fail #'fps-layer-stats-op-raw
                          #'fps-layer-stats-op-feasible))
    (let ((into-table (funcall accessor into)))
      (maphash (lambda (key count)
                 (incf (gethash key into-table 0) count))
               (funcall accessor from))))
  (dolist (accessor (list #'fps-layer-stats-op-sample-failure
                          #'fps-layer-stats-op-sample-detail))
    (let ((into-table (funcall accessor into)))
      (maphash (lambda (key sample)
                 (unless (nth-value 1 (gethash key into-table))
                   (setf (gethash key into-table) sample)))
               (funcall accessor from))))
  (let ((into-table (fps-layer-stats-layer-table into)))
    (maphash (lambda (key entry)
               (setf (gethash key into-table)
                     (fps-merge-witness-entries (gethash key into-table) entry)))
             (fps-layer-stats-layer-table from)))
  into)


(defun fps-expand-frontier-state (target rels-groups reachable base-relations
                                  problem-actions stats)
  "Expand one frontier state TARGET over every CSP group and object set into STATS.
   An action applies to an object set when any member is in the action's object set
   (or the action has none).  Returns T when TARGET produced a validated predecessor."
  (let ((target-key (fps-state-base-prop-key target base-relations))
        (target-props (fps-base-prop-set target base-relations))
        (pre-validated (fps-layer-stats-validated-count stats))
        (seen (make-hash-table :test #'equal)))
    (dolist (rg rels-groups)
      (let ((object-sets (second rg))
            (action-entries (third rg)))
        (dolist (object-set object-sets)
          (let* ((members (first object-set))
                 (applicable-actions
                   (loop for (action . obj-set) in action-entries
                         when (or (null obj-set)
                                  (some (lambda (m) (member m obj-set :test #'eq))
                                        members))
                         collect action)))
            (when applicable-actions
              (fps-expand-one-state-forward-per-object-csp
               target target-key target-props applicable-actions reachable base-relations
               problem-actions object-set stats seen))))))
    (> (fps-layer-stats-validated-count stats) pre-validated)))


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
              (let ((pred-key (enum-solution-state-key pred)))
                (when (gethash pred-key reachable)
                  (incf (fps-layer-stats-collision-reachable-count stats)))
                (when (gethash pred-key (fps-layer-stats-layer-table stats))
                  (incf (fps-layer-stats-collision-layer-count stats)))
                ;; Even on collisions, validate to accumulate additional witnesses.
                (fps-validate-novel-predecessor
                 stats pred pred-key action-form target-key
                 base-relations reachable)))))))
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
    (if (and success-p (not (state-is-inconsistent next-state)))
        (progn
          (incf (fps-layer-stats-apply-success-count stats))
          (incf (gethash (car action-form) (fps-layer-stats-op-success stats) 0))
          (let ((next-key (fps-state-base-prop-key next-state base-relations)))
            (if (string= next-key target-key)
                (progn
                  (let* ((target-entry (gethash (enum-solution-state-key next-state) reachable))
                         (target-paths (and target-entry
                                            (fps-target-witness-paths-for-successor
                                             target-entry next-state))))
                    (if target-paths
                        (let ((existing (gethash pred-key (fps-layer-stats-layer-table stats))))
                          (incf (fps-layer-stats-validated-count stats))
                          (dolist (target-path target-paths)
                            (setf existing
                                  (fps-entry-add-witness existing pred
                                                         (cons action-form target-path))))
                          (setf (gethash pred-key (fps-layer-stats-layer-table stats))
                                existing))
                        nil))
                (incf (fps-layer-stats-key-mismatch-count stats))))))
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
  (when (and *find-predecessors-print-diagnostics* diagnostics)
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
    (when (getf diagnostics :productive-frontier-count)
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
   update *BACKWARD-REACHABLE-SET*.  Returns the count of newly added states.
   Only keys not yet reachable are added; an existing entry keeps the witnesses of
   the layer that first reached it, so every stored witness depth is the state's
   shortest backward distance."
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


(defun %find-predecessors-fn (&key (actions :all)
                                   (direction 'forward)
                                   (layers 1)
                                   (save t))
  "Iterative predecessor enumeration from *ENUMERATED-GOAL-STATES*.
   DIRECTION BACKWARD regresses from the current frontier.
   DIRECTION FORWARD generates predecessors via per-object CSP enumeration
   over action-modified base relations, then validates each candidate by
   forward application.
   ACTIONS selects the candidate last actions (see ENUM-SELECT-ACTIONS).
   When no *BACKWARD-REACHABLE-SET* exists, :FORWARD auto-seeds from goal states;
   otherwise it resumes from the entries at the deepest stored depth.
   Computes LAYERS new layers, then returns (earlier on saturation).
   When SAVE is true, each layer that adds states is saved with SAVE-PREDECESSORS.
   Ctrl-C safely reverts to the last completed layer."
  (unless *enumerated-goal-states*
    (format t "~&[find-predecessors] No goal states. Run (find-goal-states) first.~%")
    (return-from %find-predecessors-fn nil))
  (when (or (> *debug* 0) *probe*)
    (format t "~&[find-predecessors] Please reset *debug* and *probe* first.~%")
    (return-from %find-predecessors-fn nil))
  (let* ((norm-direction (fps-normalize-predecessor-direction direction))
         (base-relations (get-base-relations))
         (problem-actions *actions*)
         (selected-actions (enum-select-actions problem-actions actions))
         (reachable (ecase norm-direction
                      (:backward (make-hash-table :test 'equal))
                      (:forward
                       (or *backward-reachable-set*
                           ;; Auto-seed from enumerated goal states
                           (let ((ht (make-hash-table :test 'equal)))
                             (dolist (gs *enumerated-goal-states*)
                               (let ((key (enum-solution-state-key gs)))
                                 (let ((existing (gethash key ht)))
                                   (setf (gethash key ht)
                                         (if existing
                                             (fps-entry-add-witness existing gs nil)
                                             (fps-make-witness-entry gs nil))))))
                             (setf *backward-reachable-set* ht)
                             ht)))))
         (frontier nil)
         (layer 0)
         (completed-layers 0))
    (when (eq norm-direction :backward)
      ;; Seed layer 0: goal states with empty action paths
      (dolist (gs *enumerated-goal-states*)
        (let ((key (enum-solution-state-key gs)))
          (let ((existing (gethash key reachable)))
            (setf (gethash key reachable)
                  (if existing
                      (fps-entry-add-witness existing gs nil)
                      (fps-make-witness-entry gs nil))))))
      (setf *backward-reachable-set* reachable)
      (format t "~&[find-predecessors ~S] Layer 0 (goals): ~:D unique states seeded.~%"
              norm-direction
              (hash-table-count reachable)))
    ;; Resume from the deepest stored layer (depth 0 for a fresh set)
    (setf layer (loop for entry being the hash-values of reachable
                      maximize (fps-entry-max-depth entry)))
    (setf completed-layers layer)
    (setf frontier (loop for entry being the hash-values of reachable
                         when (= (fps-entry-max-depth entry) layer)
                           collect (fps-entry-primary-state entry)))
    (when (eq norm-direction :forward)
      (format t "~&[find-predecessors ~S] Starting from reachable set: ~:D states, depth ~D, ~:D frontier states.~%"
              norm-direction
              (hash-table-count reachable) layer (length frontier)))
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
        ;; Layer completed — next frontier is the states not already reachable
        ;; (collect before the merge), then merge, report, check saturation
        (setf frontier (loop for key being the hash-keys of layer-table
                               using (hash-value entry)
                             unless (gethash key reachable)
                               collect (fps-entry-primary-state entry)))
        (let ((new-count (fps-complete-layer layer-table reachable start-time
                                             norm-direction layer
                                             raw-count feasible-count validated-count
                                             diagnostics)))
          (setf completed-layers layer)
          (when (zerop new-count)
            (format t "~&[find-predecessors ~S] Saturated - no new states at layer ~D.~%"
                    norm-direction layer)
            (return))
          (when save
            (save-predecessors))
          (when (zerop (decf layers))
            (return)))))
    (format t "~&[find-predecessors ~S] Done. *backward-reachable-set* contains ~:D states.~%"
            norm-direction
            (hash-table-count *backward-reachable-set*))
    (setf (get 'find-predecessors :last-report)
          (list :direction norm-direction
                :completed-layers completed-layers
                :reachable-count (hash-table-count *backward-reachable-set*)))
    t))


(defun save-predecessors (&optional path)
  "Save *BACKWARD-REACHABLE-SET* and *ENUMERATED-GOAL-STATES* to PATH as readable data.
   PATH defaults to <project root>/<problem-name>-predecessors-<layers>.lisp, where
   <layers> is the deepest stored depth.  The file is written to a temporary sibling
   and renamed over PATH, so an interrupted save leaves any earlier file intact.
   Problems with happenings are not supported (the hidb is not saved)."
  (when *happening-names*
    (error "SAVE-PREDECESSORS: problems with happenings are not supported."))
  (let* ((reachable *backward-reachable-set*)
         (layers (loop for entry being the hash-values of reachable
                       maximize (fps-entry-max-depth entry)))
         (target (if path
                     (merge-pathnames (pathname path) *default-pathname-defaults*)
                     (merge-pathnames (format nil "~(~A~)-predecessors-~D.lisp" *problem-name* layers)
                                      (asdf:system-source-directory :wouldwork))))
         (start-time (get-internal-real-time)))
    (write-predecessors-file target layers)
    (format t "~&[save-predecessors] Saved ~:D entries (~D layers) and ~:D goal states to ~A (~:D bytes, ~,1Fs).~%"
            (hash-table-count reachable)
            layers
            (length *enumerated-goal-states*)
            target
            (with-open-file (s target :element-type '(unsigned-byte 8))
              (file-length s))
            (/ (- (get-internal-real-time) start-time)
               internal-time-units-per-second))
    target))


(defun write-predecessors-file (target layers)
  "Atomically replace TARGET with the backward reachable set as readable data."
  (let ((temporary (subgoal-progress-temporary-pathname target))
        (completed nil))
    (unwind-protect
        (progn
          (with-open-file (stream temporary :direction :output :if-exists :error
                                            :if-does-not-exist :create)
            (write-predecessors-data stream layers))
          (uiop:rename-file-overwriting-target temporary target)
          (setf completed t))
      (unless completed
        (uiop:delete-file-if-exists temporary)))))


(defun write-predecessors-data (stream layers)
  "Write one form per line: the header plist, one fact list per goal state, then one
   (KEY ((FACTS . PATH) ...)) form per reachable entry.  FACTS is the LIST-DATABASE of
   the witness idb; PATH is its action forms toward the goal."
  (let ((reachable *backward-reachable-set*))
    (format stream ";;; Wouldwork backward reachable set (find-predecessors)~%")
    (format stream ";;; Forms: header plist, goal-state fact lists, then (key ((facts . path) ...)) entries.~%")
    (format stream ";;; Restore with LOAD-PREDECESSORS after staging the same problem.~2%")
    (with-standard-io-syntax
      (let ((*package* (find-package :ww))
            (*print-pretty* nil)
            (*print-readably* nil))
        (prin1 (list :format-version 1
                     :problem *problem-name*
                     :saved (multiple-value-bind (sec min hour day month year)
                                (decode-universal-time (get-universal-time))
                              (format nil "~D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
                                      year month day hour min sec))
                     :goal (getf (get 'find-goal-states :last-report) :goal)
                     :base-relations (get-base-relations)
                     :groups *enumerator-detected-groups*
                     :symmetric-relations *enumerator-detected-symmetric-relations*
                     :completed-layers layers
                     :entry-count (hash-table-count reachable)
                     :goal-state-count (length *enumerated-goal-states*))
               stream)
        (terpri stream)
        (dolist (gs *enumerated-goal-states*)
          (prin1 (list-database (problem-state.idb gs)) stream)
          (terpri stream))
        (maphash (lambda (key entry)
                   (prin1 (list key
                                (mapcar (lambda (w)
                                          (cons (list-database (problem-state.idb (car w)))
                                                (cdr w)))
                                        entry))
                          stream)
                   (terpri stream))
                 reachable)))))


(defun load-predecessors (&key path verify)
  "Restore *BACKWARD-REACHABLE-SET* and *ENUMERATED-GOAL-STATES* saved by SAVE-PREDECESSORS
   into a freshly staged problem.  PATH defaults to the highest-numbered
   <problem-name>-predecessors-<layers>.lisp in the project root.  The header must match the
   staged problem, and the interchangeable groups, recomputed from the goal, must match the
   saved ones.  Keys are recomputed under the caller's print settings, as FIND-PREDECESSORS
   made them.  When VERIFY is true, every witness path is replayed and must reach a state
   satisfying GOAL-FN.  Afterwards (find-predecessors) continues from the deepest layer."
  (let ((source (if path
                    (merge-pathnames (pathname path) *default-pathname-defaults*)
                    (first (sort (directory (merge-pathnames
                                             (format nil "~(~A~)-predecessors-*.lisp" *problem-name*)
                                             (asdf:system-source-directory :wouldwork)))
                                 #'>
                                 :key (lambda (p)
                                        (let ((name (pathname-name p)))
                                          (parse-integer name :start (1+ (position #\- name :from-end t)))))))))
        (start-time (get-internal-real-time)))
    (unless source
      (error "LOAD-PREDECESSORS: no ~(~A~)-predecessors-*.lisp file in the project root."
             *problem-name*))
    (multiple-value-bind (header goal-fact-lists entry-forms)
        (read-predecessors-file source)
      (let ((goal (getf header :goal))
            (layers (getf header :completed-layers)))
        (unless (eql (getf header :format-version) 1)
          (error "LOAD-PREDECESSORS: unsupported format version ~S." (getf header :format-version)))
        (unless (eq (getf header :problem) *problem-name*)
          (error "LOAD-PREDECESSORS: file is for problem ~S, staged problem is ~S."
                 (getf header :problem) *problem-name*))
        (unless (equal goal (get 'goal-fn :form))
          (error "LOAD-PREDECESSORS: saved goal ~S differs from the staged goal ~S."
                 goal (get 'goal-fn :form)))
        (unless (equal (getf header :base-relations) (get-base-relations))
          (error "LOAD-PREDECESSORS: saved base relations ~S differ from the staged ~S."
                 (getf header :base-relations) (get-base-relations)))
        (let ((ctx (enum-compute-context goal :skip-fixed-maps t)))
          (unless (and (= (length (getf ctx :interchangeable-groups)) (length (getf header :groups)))
                       (every #'alexandria:set-equal (getf ctx :interchangeable-groups) (getf header :groups)))
            (error "LOAD-PREDECESSORS: recomputed groups ~S differ from the saved ~S."
                   (getf ctx :interchangeable-groups) (getf header :groups)))
          (setf *enumerator-detected-groups* (getf header :groups)
                *enumerator-detected-symmetric-relations* (getf ctx :symmetric-relations)))
        (let ((goal-states (mapcar #'enum-make-state-from-propositions goal-fact-lists))
              (reachable (rebuild-predecessor-entries entry-forms verify)))
          (unless (= (loop for entry being the hash-values of reachable
                           maximize (fps-entry-max-depth entry))
                     layers)
            (error "LOAD-PREDECESSORS: deepest stored depth differs from the header's ~D layers."
                   layers))
          (setf *enumerated-goal-states* goal-states
                *backward-reachable-set* reachable
                (get 'find-goal-states :last-report) (fgs-build-report goal goal-states)
                (get 'find-predecessors :last-report) (list :direction :forward
                                                            :completed-layers layers
                                                            :reachable-count (hash-table-count reachable)))
          (format t "~&[load-predecessors] Loaded ~:D entries (~D layers) and ~:D goal states from ~A~:[~;, verified~] (~,1Fs).~%"
                  (hash-table-count reachable)
                  layers
                  (length goal-states)
                  source
                  verify
                  (/ (- (get-internal-real-time) start-time)
                     internal-time-units-per-second))
          (loop for depth from 0 to layers
                do (format t "~&[load-predecessors]   depth ~D: ~:D entries~%"
                           depth
                           (loop for entry being the hash-values of reachable
                                 count (= (fps-entry-max-depth entry) depth))))
          source)))))


(defun read-predecessors-file (source)
  "Read SOURCE with reader evaluation disabled.  Returns three values: the header plist,
   the goal-state fact lists, and the (KEY ((FACTS . PATH) ...)) entry forms."
  (with-open-file (stream source :direction :input)
    (with-standard-io-syntax
      (let* ((*package* (find-package :ww))
             (*read-eval* nil)
             (eof (gensym "EOF"))
             (header (read stream))
             (goal-fact-lists (loop repeat (getf header :goal-state-count)
                                    collect (read stream)))
             (entry-forms (loop for form = (read stream nil eof)
                                until (eq form eof)
                                collect form)))
        (unless (= (length entry-forms) (getf header :entry-count))
          (error "LOAD-PREDECESSORS: ~A has ~:D entries; its header says ~:D."
                 source (length entry-forms) (getf header :entry-count)))
        (values header goal-fact-lists entry-forms)))))


(defun rebuild-predecessor-entries (entry-forms verify)
  "Return an EQUAL reachable table built from ENTRY-FORMS, each (KEY ((FACTS . PATH) ...)).
   Each witness state is rebuilt from FACTS and must reproduce KEY and FACTS' count.
   When VERIFY is true, each PATH must replay from its state to a state satisfying GOAL-FN."
  (let ((reachable (make-hash-table :test 'equal)))
    (dolist (form entry-forms reachable)
      (destructuring-bind (key witnesses) form
        (setf (gethash key reachable)
              (mapcar (lambda (witness)
                        (let ((state (enum-make-state-from-propositions (car witness))))
                          (unless (string= (enum-solution-state-key state) key)
                            (error "LOAD-PREDECESSORS: rebuilt key differs from stored key ~S." key))
                          (unless (= (hash-table-count (problem-state.idb state)) (length (car witness)))
                            (error "LOAD-PREDECESSORS: rebuilt idb has ~D entries, ~D facts stored, for key ~S."
                                   (hash-table-count (problem-state.idb state)) (length (car witness)) key))
                          (when verify
                            (let ((validation (validate-action-sequence state (cdr witness)
                                                                        :goal-test (symbol-function 'goal-fn))))
                              (unless (and (action-sequence-validation-success-p validation)
                                           (action-sequence-validation-goal-satisfied-p validation))
                                (error "LOAD-PREDECESSORS: path ~S does not reach the goal from its state (key ~S)."
                                       (cdr witness) key))))
                          (cons state (cdr witness))))
                      witnesses))))))


(defun fps-meeting-point-entry-for-state (state &optional
                                                (reachable-set *backward-reachable-set*)
                                                (base-relations (get-base-relations)))
  "Return the reachable-set witness entry list for STATE, or NIL.
   The reachable set is keyed by canonical key, so a connector-renamed twin matches."
  (declare (ignore base-relations))
  (fps-normalize-witness-entry
   (gethash (enum-solution-state-key state) reachable-set)))


(defun fps-default-meeting-point-min-depth ()
  "Default meeting-point depth from the last completed FIND-PREDECESSORS run."
  (let ((report (get 'find-predecessors :last-report)))
    (if (and (listp report)
             (integerp (getf report :completed-layers))
             (>= (getf report :completed-layers) 0))
        (getf report :completed-layers)
        0)))


(defun fps-meeting-state-bridgeable-p (state reachable-set base-relations original-goal-fn
                                      &optional bridge-cache)
  "True when STATE can be concretely extended to ORIGINAL-GOAL-FN via REACHABLE-SET."
  (let* ((cache-key (fps-state-full-prop-key state)))
    (when (and bridge-cache (gethash cache-key bridge-cache))
      (return-from fps-meeting-state-bridgeable-p
        (gethash cache-key bridge-cache)))
    (let* ((entry (fps-meeting-point-entry-for-state state reachable-set base-relations))
           (start-depth (and entry (fps-entry-max-depth entry)))
           (result nil))
      (when (and entry start-depth)
        ;; First try guided bridge reconstruction.
        (multiple-value-bind (moves final-state nodes-expanded found-p)
            (fps-guided-suffix-to-goal state start-depth reachable-set base-relations original-goal-fn)
          (declare (ignore moves final-state nodes-expanded))
          (setf result found-p))
        ;; If guided search fails, try each stored witness replay.
        (unless result
          (dolist (witness entry)
            (unless result
              (let ((trial-state (copy-problem-state state))
                    (trial-ok t))
                (dolist (action-form (cdr witness))
                  (when trial-ok
                    (multiple-value-bind (next-state success-p failure-reason)
                        (apply-action-to-state action-form trial-state nil nil)
                      (declare (ignore failure-reason))
                      (if success-p
                          (setf trial-state next-state)
                          (setf trial-ok nil)))))
                (when (and trial-ok (funcall original-goal-fn trial-state))
                  (setf result t)))))))
      (when bridge-cache
        (setf (gethash cache-key bridge-cache) result))
      result)))


(defun install-meeting-point (&key
                                (reachable-set *backward-reachable-set*)
                                (base-relations (get-base-relations))
                                (min-backward-depth nil)
                                (original-goal-fn nil))
  "Install GOAL-FN as backward-reachable-set membership over base-prop keys.
   MIN-BACKWARD-DEPTH defaults to the last completed FIND-PREDECESSORS layer."
  (let ((resolved-min-depth (if (null min-backward-depth)
                                (fps-default-meeting-point-min-depth)
                                min-backward-depth)))
    (unless (and reachable-set (> (hash-table-count reachable-set) 0))
      (error "INSTALL-MEETING-POINT: *BACKWARD-REACHABLE-SET* is empty. Run FIND-PREDECESSORS first."))
    (unless (and (integerp resolved-min-depth) (>= resolved-min-depth 0))
      (error "INSTALL-MEETING-POINT: :MIN-BACKWARD-DEPTH must be a non-negative integer; got ~S."
             resolved-min-depth))
    (let ((captured-set reachable-set)
          (captured-rels base-relations)
          (captured-min-depth resolved-min-depth)
          (captured-original-goal-fn original-goal-fn)
          (bridge-cache (make-hash-table :test 'equal)))
      (setf (symbol-function 'goal-fn)
            (lambda (state)
              (let ((entry (fps-meeting-point-entry-for-state state captured-set captured-rels)))
                (and entry
                     (>= (fps-entry-max-depth entry) captured-min-depth)
                     (or (null captured-original-goal-fn)
                         (fps-meeting-state-bridgeable-p state
                                                         captured-set
                                                         captured-rels
                                                         captured-original-goal-fn
                                                         bridge-cache))))))
      (setf (get 'goal-fn :form)
            (list 'meeting-point-goal
                  :target-count (hash-table-count captured-set)
                  :min-backward-depth captured-min-depth
                  :requires-bridgeability (if captured-original-goal-fn t nil)
                  :base-relations captured-rels))
      t)))


(defun install-meeting-point-goal (&key
                                     (reachable-set *backward-reachable-set*)
                                     (base-relations (get-base-relations))
                                     (min-backward-depth nil)
                                     (original-goal-fn nil))
  "Backward-compatible wrapper for INSTALL-MEETING-POINT."
  (install-meeting-point :reachable-set reachable-set
                         :base-relations base-relations
                         :min-backward-depth min-backward-depth
                         :original-goal-fn original-goal-fn))


(defun fps-meeting-entry-depth (state reachable-set base-relations)
  "Return backward depth (suffix length) of STATE in REACHABLE-SET, else NIL."
  (let ((e (fps-meeting-point-entry-for-state state reachable-set base-relations)))
    (and e (fps-entry-max-depth e))))


(defun fps-guided-suffix-to-goal (meeting-state start-depth reachable-set base-relations goal-fn)
  "Try to find a concrete suffix from MEETING-STATE to an original goal.
   Returns (values suffix-moves final-state nodes-expanded found-p)."
  (let ((stack (list (list (copy-problem-state meeting-state) start-depth nil)))
        (visited (make-hash-table :test 'equal))
        (nodes-expanded 0))
    (loop while stack
          do (let* ((frame (pop stack))
                    (state (first frame))
                    (depth (second frame))
                    (path (third frame))
                    (state-key (fps-state-base-prop-key state base-relations))
                    (visit-key (cons state-key depth)))
               (unless (gethash visit-key visited)
                 (setf (gethash visit-key visited) t)
                 (incf nodes-expanded)
                 (if (zerop depth)
                     (when (funcall goal-fn state)
                       (return (values path state nodes-expanded t)))
                     (dolist (action-form (enum-applicable-action-forms state *actions*))
                       (multiple-value-bind (next-state success-p failure-reason)
                           (apply-action-to-state action-form state nil nil)
                         (declare (ignore failure-reason))
                         (when (and success-p (not (state-is-inconsistent next-state)))
                           (let ((next-depth (fps-meeting-entry-depth next-state reachable-set base-relations)))
                             (when (and next-depth (< next-depth depth))
                               (push (list next-state
                                           next-depth
                                           (append path (list (record-move next-state))))
                                     stack)))))))))
          finally (return (values nil nil nodes-expanded nil)))))


(defun fps-extend-meeting-solution-to-goal (solution &optional
                                                     (reachable-set *backward-reachable-set*)
                                                     (base-relations (get-base-relations))
                                                     (goal-fn nil))
  "Convert one meeting-point SOLUTION into a full start-to-goal solution."
  (let* ((meeting-state (solution.goal solution))
         (meeting-key (fps-state-base-prop-key meeting-state base-relations))
         (entry (fps-normalize-witness-entry
                 (fps-meeting-point-entry-for-state meeting-state reachable-set base-relations)))
         (start-depth nil)
         (guided-found nil)
         (guided-nodes-expanded 0)
         (used-fallback-replay nil)
         (current (copy-problem-state meeting-state))
         (suffix-moves nil))
    (when *solve-meeting-point-print-diagnostics*
      (let* ((witness-state (car (first entry)))
             (meeting-non-base (set-difference 
                                 (list-database (problem-state.idb meeting-state))
                                 (enum-state-base-props meeting-state base-relations)
                                 :test #'equal))
             (witness-non-base (set-difference
                                 (list-database (problem-state.idb witness-state))
                                 (enum-state-base-props witness-state base-relations)
                                 :test #'equal)))
        (format t "~&[diag] Meeting state non-base props: ~S~%" meeting-non-base)
        (format t "~&[diag] Witness state non-base props: ~S~%" witness-non-base)))
    (unless entry
      (format t "~&[solve-meeting-point] Warning: meeting state not found in reachable-set: ~S~%" meeting-key)
      (return-from fps-extend-meeting-solution-to-goal (values nil nil)))
    (setf start-depth (fps-entry-max-depth entry))
    (when goal-fn
      (multiple-value-bind (guided-moves guided-final nodes-expanded found-p)
          (fps-guided-suffix-to-goal current start-depth reachable-set base-relations goal-fn)
        (setf guided-nodes-expanded nodes-expanded
              guided-found found-p)
        (when found-p
          (setf suffix-moves guided-moves
                current guided-final))))
    (unless guided-found
      (setf used-fallback-replay t)
      (let ((replay-success nil)
            (last-failure nil))
        (dolist (witness entry)
          (unless replay-success
            (let ((trial-state (copy-problem-state meeting-state))
                  (trial-moves nil)
                  (trial-ok t))
              (dolist (action-form (cdr witness))
                (when trial-ok
                  (multiple-value-bind (next-state success-p failure-reason)
                      (apply-action-to-state action-form trial-state nil nil)
                    (if success-p
                        (progn
                          (push (record-move next-state) trial-moves)
                          (setf trial-state next-state))
                        (progn
                          (setf trial-ok nil
                                last-failure failure-reason))))))
              (when trial-ok
                (setf replay-success t
                      current trial-state
                      suffix-moves (nreverse trial-moves))))))
        (unless replay-success
          (format t "~&[solve-meeting-point] Warning: failed replaying all ~D witness suffix(es) from meeting state (~A).~%"
                 (length entry) last-failure)
          (return-from fps-extend-meeting-solution-to-goal (values nil nil)))))
    (let* ((full-path (append (solution.path solution) suffix-moves))
           (final-goal-satisfied (and goal-fn (funcall goal-fn current)))
           (diag (list :meeting-key meeting-key
                       :witness-count (length entry)
                       :start-backward-depth start-depth
                       :guided-search-attempted (not (null goal-fn))
                       :guided-search-found guided-found
                       :guided-nodes-expanded guided-nodes-expanded
                       :fallback-replay-used used-fallback-replay
                       :final-goal-satisfied final-goal-satisfied
                       :final-key (fps-state-base-prop-key current base-relations))))
      (values
       (make-solution
        :depth (length full-path)
       :time (problem-state.time current)
       :value (problem-state.value current)
       :path full-path
       :goal current)
       diag))))


(defun fps-rebuild-unique-solutions (solutions)
  "Rebuild *unique-solution-states* semantics from SOLUTIONS using current SOLUTION-BETTER-P."
  (let ((unique nil))
    (dolist (sol solutions)
      (let* ((goal-idb (problem-state.idb (solution.goal sol)))
             (existing (find goal-idb unique
                             :key (lambda (s) (problem-state.idb (solution.goal s)))
                             :test #'equalp)))
        (cond
          ((null existing)
           (push sol unique))
          ((solution-better-p sol existing)
           (setf unique (substitute sol existing unique))))))
    (nreverse unique)))


(defun fps-print-solve-meeting-point-diagnostics (diagnostics)
  "Print SOLVE-MEETING-POINT expansion diagnostics."
  (when *solve-meeting-point-print-diagnostics*
    (let* ((total (length diagnostics))
           (guided-found (count-if (lambda (d) (getf d :guided-search-found)) diagnostics))
           (fallback-used (count-if (lambda (d) (getf d :fallback-replay-used)) diagnostics))
           (goal-ok (count-if (lambda (d) (getf d :final-goal-satisfied)) diagnostics)))
      (format t "~&[solve-meeting-point diagnostics] expanded=~:D guided-found=~:D fallback-used=~:D goal-satisfied=~:D~%"
              total guided-found fallback-used goal-ok)
      (loop for d in diagnostics
            for i from 1
            do (format t "~&[solve-meeting-point diagnostics] #~D depth=~D guided=~A fallback=~A goal=~A key=~S~%"
                       i
                       (or (getf d :start-backward-depth) 0)
                       (if (getf d :guided-search-found) 'yes 'no)
                       (if (getf d :fallback-replay-used) 'yes 'no)
                       (if (getf d :final-goal-satisfied) 'yes 'no)
                       (getf d :meeting-key))))))


(defmacro solve-meeting-point (&rest raw-args)
  "User-facing macro wrapper for SOLVE-MEETING-POINT-FN.
   Auto-quotes literal symbol values for :SOLUTION-TYPE so calls like
   (solve-meeting-point :depth-cutoff 10 :solution-type min-length) work."
  (destructuring-bind (&key
                       (depth-cutoff 12 depth-cutoff-supplied-p)
                       (solution-type 'first solution-type-supplied-p)
                       (min-backward-depth nil min-backward-depth-supplied-p))
      raw-args
    (flet ((literal-symbol-p (form)
             (and (symbolp form)
                  (not (keywordp form))
                  (not (null form)))))
      (labels ((maybe-quote-symbol (form)
                 (cond
                   ((and (consp form) (eq (car form) 'quote)) form)
                   ((literal-symbol-p form) `',form)
                   (t form))))
        `(solve-meeting-point-fn
          ,@(if depth-cutoff-supplied-p
                `(:depth-cutoff ,depth-cutoff)
                '(:depth-cutoff 12))
          ,@(if solution-type-supplied-p
                `(:solution-type ,(maybe-quote-symbol solution-type))
                '(:solution-type 'first))
          ,@(if min-backward-depth-supplied-p
                `(:min-backward-depth ,min-backward-depth)
                '(:min-backward-depth nil)))))))


(defun solve-meeting-point-fn (&key
                                 (depth-cutoff 12)
                                 (solution-type 'first)
                                 (min-backward-depth nil))
  "Run WW-SOLVE to a meeting point, then expand to full start-to-goal solutions."
  (let ((saved-depth-cutoff *depth-cutoff*)
        (saved-solution-type *solution-type*)
        (saved-goal-form (get 'goal-fn :form))
        (saved-goal-fn-def (and (fboundp 'goal-fn) (symbol-function 'goal-fn)))
        (requested-solution-type solution-type))
    (unwind-protect
        (progn
          (install-meeting-point :min-backward-depth min-backward-depth
                                 :original-goal-fn nil)
          (setf *depth-cutoff* depth-cutoff
                *solution-type* 'every)
          (ww-solve)
          (setf *solution-type* requested-solution-type)
          (let ((expanded-solutions nil)
                (expansion-diagnostics nil)
                (extension-failures 0)
                (goal-hit-found nil))
            (dolist (sol *solution-paths*)
              (multiple-value-bind (expanded-sol diag)
                  (fps-extend-meeting-solution-to-goal sol
                                                       *backward-reachable-set*
                                                       (get-base-relations)
                                                       saved-goal-fn-def)
                (if expanded-sol
                    (progn
                      (push expanded-sol expanded-solutions)
                      (push diag expansion-diagnostics)
                      (when (and (eq requested-solution-type 'first)
                                 (or (null saved-goal-fn-def)
                                     (getf diag :final-goal-satisfied)))
                        (setf goal-hit-found t)))
                    (incf extension-failures)))
              (when goal-hit-found
                (return)))
            (when (> extension-failures 0)
              (format t "~&[solve-meeting-point] ~:D meeting-point solution(s) could not be extended (skipped).~%"
                      extension-failures))
            (setf expanded-solutions (nreverse expanded-solutions)
                  expansion-diagnostics (nreverse expansion-diagnostics))
            (setf *solve-meeting-point-last-diagnostics*
                  (list :expanded-count (length expanded-solutions)
                        :details expansion-diagnostics))
            (when *solve-meeting-point-print-diagnostics*
              (let* ((total (length expansion-diagnostics))
                     (guided-found (count-if (lambda (d) (getf d :guided-search-found))
                                             expansion-diagnostics))
                     (fallback-used (count-if (lambda (d) (getf d :fallback-replay-used))
                                              expansion-diagnostics))
                     (goal-ok (count-if (lambda (d) (getf d :final-goal-satisfied))
                                        expansion-diagnostics)))
                (format t "~&[solve-meeting-point diagnostics] expanded=~:D guided-found=~:D fallback-used=~:D goal-satisfied=~:D~%"
                        total guided-found fallback-used goal-ok)
                (loop for d in expansion-diagnostics
                      for i from 1
                      do (format t "~&[solve-meeting-point diagnostics] #~D depth=~D guided=~A fallback=~A goal=~A key=~S~%"
                                 i
                                 (or (getf d :start-backward-depth) 0)
                                 (if (getf d :guided-search-found) 'yes 'no)
                                 (if (getf d :fallback-replay-used) 'yes 'no)
                                 (if (getf d :final-goal-satisfied) 'yes 'no)
                                 (getf d :meeting-key)))))
            (let ((discarded-count 0)
                  (filtered-solutions nil))
              ;; Keep only expanded paths that satisfy the original (pre-meeting-point) goal.
              (setf filtered-solutions
                    (if saved-goal-fn-def
                        (remove-if-not
                         (lambda (sol)
                           (let ((ok (funcall saved-goal-fn-def (solution.goal sol))))
                             (unless ok
                               (incf discarded-count))
                             ok))
                         expanded-solutions)
                        expanded-solutions))
              ;; Apply requested SOLUTION-TYPE on expanded full-goal solutions.
              (setf *solution-paths*
                    (case requested-solution-type
                      (first
                       (if filtered-solutions
                           (list (first filtered-solutions))
                           nil))
                      (every
                       filtered-solutions)
                      (min-length
                       (if filtered-solutions
                           (list (reduce (lambda (a b)
                                           (if (< (solution.depth a) (solution.depth b)) a b))
                                         filtered-solutions))
                           nil))
                      (min-time
                       (if filtered-solutions
                           (list (reduce (lambda (a b)
                                           (if (< (solution.time a) (solution.time b)) a b))
                                         filtered-solutions))
                           nil))
                      (min-value
                       (if filtered-solutions
                           (list (reduce (lambda (a b)
                                           (if (< (solution.value a) (solution.value b)) a b))
                                         filtered-solutions))
                           nil))
                      (max-value
                       (if filtered-solutions
                           (list (reduce (lambda (a b)
                                           (if (> (solution.value a) (solution.value b)) a b))
                                         filtered-solutions))
                           nil))
                      (otherwise
                       filtered-solutions)))
              (when expanded-solutions
                (format t "~&[solve-meeting-point] Expanded ~:D meeting-point solution(s) to full path(s).~%"
                        (length expanded-solutions)))
              (when (and saved-goal-fn-def (> discarded-count 0))
                (format t "~&[solve-meeting-point] Discarded ~:D expanded solution(s) that do not satisfy the original goal.~%"
                        discarded-count))))
          (let ((unique nil))
            (dolist (sol *solution-paths*)
              (let* ((goal-idb (problem-state.idb (solution.goal sol)))
                     (existing (find goal-idb unique
                                     :key (lambda (s) (problem-state.idb (solution.goal s)))
                                     :test #'equalp)))
                (cond
                  ((null existing)
                   (push sol unique))
                  ((solution-better-p sol existing)
                   (setf unique (substitute sol existing unique))))))
            (setf *unique-solution-states* (nreverse unique)))
          (when *solution-paths*
            (format t "~&[solve-meeting-point] Expanded ~:D meeting-point solution(s) to full goal path(s).~%"
                    (length *solution-paths*))
            (format t "~&[solve-meeting-point] Example full solution path:~%")
            (when (fboundp 'printout-solution)
              (printout-solution (first *solution-paths*))))
          *solution-paths*)
      (setf *depth-cutoff* saved-depth-cutoff
            *solution-type* saved-solution-type)
      (setf (get 'goal-fn :form) saved-goal-form)
      (when saved-goal-fn-def
        (setf (symbol-function 'goal-fn) saved-goal-fn-def)))))


(defun ww-solve-meeting-point (&key
                                 (depth-cutoff 12)
                                 (solution-type 'first)
                                 (min-backward-depth nil))
  "Backward-compatible wrapper for SOLVE-MEETING-POINT."
  (solve-meeting-point-fn :depth-cutoff depth-cutoff
                          :solution-type solution-type
                          :min-backward-depth min-backward-depth))


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
            (setf *solution-paths* nil
                  *unique-solution-states* nil)
            (let ((*enum-csp-search-active* t))
              (ww-solve))
            ;; Truncate if user requested N (kept for backward compatibility).
            (let* ((uniq *unique-solution-states*)
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
    (invalidate-problem-state-hash st)
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
      (when (and (not (state-is-inconsistent s))
                 (funcall goal-fn s))
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

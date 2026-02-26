;;; Filename: ww-csp-ir.lisp
;;;
;;; Backend-neutral CSP IR scaffold for goal-enumerator migration.


(in-package :ww)


(defstruct (enum-csp-ir (:constructor make-enum-csp-ir
                                      (&key spec-name base-relations
                                            constraints prefilter domain-hints
                                            constraint-filter-name)))
  "Normalized, backend-neutral representation of a goal-enumerator CSP."
  spec-name
  base-relations
  constraints
  prefilter
  domain-hints
  constraint-filter-name)


(defparameter *active-enum-csp-ir* nil
  "Holds the currently-installed enum-csp-ir object, if any.")


(defstruct (enum-csp-next-plan (:constructor make-enum-csp-next-plan
                                             (&key spec-name base-relations
                                                   relation-plans leaf-upper-bound)))
  "Deterministic next-backend plan summary compiled from enum-csp-ir."
  spec-name
  base-relations
  relation-plans
  leaf-upper-bound)


(defvar *enum-csp-backend* :legacy
  "Selected enum-csp backend adapter.
   Supported values: :LEGACY and :NEXT-BACKEND.")


(defparameter *active-enum-csp-next-plan* nil
  "Most recent installed next-backend plan summary.")


(defparameter *enum-csp-backend-last-result* nil
  "Deterministic metadata plist produced by the most recent IR adapter call.")


(defparameter *enum-csp-next-backend-invocations* 0
  "Number of times the next-backend adapter stub has been invoked.")


(defvar *enum-csp-next-backend-run-fluent-pilot* nil
  "When non-NIL, next-backend adapter also runs fluent-phase pilot execution.")


(defvar *enum-csp-shadow-compare-enabled* nil
  "When non-NIL, legacy installs also compute backend parity metadata.")


(defvar *enum-csp-shadow-compare-strict* nil
  "When non-NIL with shadow compare enabled, backend parity mismatches signal an error.")


(defparameter *enum-csp-shadow-compare-last* nil
  "Most recent backend parity comparison plist, or NIL.")


(defvar *enum-csp-next-pilot-bound-check-enabled* nil
  "When non-NIL, next-backend adapter computes phase-2 pilot bound-check summary.")


(defvar *enum-csp-next-pilot-bound-check-strict* nil
  "When non-NIL with next-pilot bound-check enabled, bound-check mismatches signal an error.")


(defparameter *enum-csp-next-pilot-bound-check-last* nil
  "Most recent next-backend pilot bound-check summary plist, or NIL.")


(defun build-enum-csp-ir (&key spec-name base-relations constraints prefilter
                               domain-hints constraint-filter-name)
  "Build an enum-csp-ir object from interpreted goal-enumerator data."
  (make-enum-csp-ir
   :spec-name spec-name
   :base-relations base-relations
   :constraints constraints
   :prefilter prefilter
   :domain-hints domain-hints
   :constraint-filter-name constraint-filter-name))


(defun enum-csp-ir-from-config (config)
  "Build an enum-csp-ir from CONFIG plist keys.
   Expected keys: :SPEC-NAME :BASE-RELATIONS :CONSTRAINT-FORM :PREFILTER :DOMAIN-HINTS
   Optional key: :CONSTRAINT-FILTER-NAME."
  (build-enum-csp-ir
   :spec-name (getf config :spec-name)
   :base-relations (getf config :base-relations)
   :constraints (getf config :constraint-form)
   :prefilter (getf config :prefilter)
   :domain-hints (getf config :domain-hints)
   :constraint-filter-name (getf config :constraint-filter-name)))


(defun install-enum-csp-ir (ir)
  "Install IR as the active backend-neutral CSP representation."
  (check-type ir enum-csp-ir)
  (setf *active-enum-csp-ir* ir)
  ir)


(defun clear-enum-csp-ir ()
  "Clear active enum-csp-ir."
  (setf *active-enum-csp-ir* nil
        *active-enum-csp-next-plan* nil
        *enum-csp-backend-last-result* nil
        *enum-csp-shadow-compare-last* nil
        *enum-csp-next-pilot-bound-check-last* nil)
  nil)


(defun current-enum-csp-ir ()
  "Return the active enum-csp-ir, or NIL."
  *active-enum-csp-ir*)


(defun install-enum-csp-next-plan (plan)
  "Install next-backend PLAN summary."
  (check-type plan enum-csp-next-plan)
  (setf *active-enum-csp-next-plan* plan)
  plan)


(defun current-enum-csp-next-plan ()
  "Return installed next-backend plan summary, or NIL."
  *active-enum-csp-next-plan*)


(defun enum-csp-supported-backends ()
  "Return supported internal backend adapter keys."
  '(:legacy :next-backend))


(defun enum-csp-cutover-modes ()
  "Return supported enum-csp cutover mode keys."
  '(:legacy-safe :shadow :next-pilot-strict))


(defun enum-csp-cutover-mode-state ()
  "Return current cutover-relevant runtime settings."
  (list :backend *enum-csp-backend*
        :shadow-compare-enabled *enum-csp-shadow-compare-enabled*
        :shadow-compare-strict *enum-csp-shadow-compare-strict*
        :run-fluent-pilot *enum-csp-next-backend-run-fluent-pilot*
        :pilot-bound-check-enabled *enum-csp-next-pilot-bound-check-enabled*
        :pilot-bound-check-strict *enum-csp-next-pilot-bound-check-strict*))


(defun set-enum-csp-cutover-mode (mode)
  "Set coordinated enum-csp cutover MODE and return resulting settings."
  (unless (member mode (enum-csp-cutover-modes) :test #'eq)
    (error "Unsupported enum-csp cutover mode ~S. Supported: ~S"
           mode
           (enum-csp-cutover-modes)))
  (case mode
    (:legacy-safe
     (set-enum-csp-backend :legacy)
     (setf *enum-csp-shadow-compare-enabled* nil
           *enum-csp-shadow-compare-strict* nil
           *enum-csp-next-backend-run-fluent-pilot* nil
           *enum-csp-next-pilot-bound-check-enabled* nil
           *enum-csp-next-pilot-bound-check-strict* nil))
    (:shadow
     (set-enum-csp-backend :legacy)
     (setf *enum-csp-shadow-compare-enabled* t
           *enum-csp-shadow-compare-strict* nil
           *enum-csp-next-backend-run-fluent-pilot* nil
           *enum-csp-next-pilot-bound-check-enabled* nil
           *enum-csp-next-pilot-bound-check-strict* nil))
    (:next-pilot-strict
     (set-enum-csp-backend :next-backend)
     (setf *enum-csp-shadow-compare-enabled* nil
           *enum-csp-shadow-compare-strict* nil
           *enum-csp-next-backend-run-fluent-pilot* t
           *enum-csp-next-pilot-bound-check-enabled* t
           *enum-csp-next-pilot-bound-check-strict* t)))
  (enum-csp-cutover-mode-state))


(defun set-enum-csp-backend (backend)
  "Set the active internal backend adapter key."
  (unless (member backend (enum-csp-supported-backends) :test #'eq)
    (error "Unsupported enum-csp backend ~S. Supported: ~S"
           backend
           (enum-csp-supported-backends)))
  (setf *enum-csp-backend* backend)
  backend)


(defun enum-csp-hint-keys-for-rel (hints rel)
  "Return sorted hint key list for REL from HINTS hash table."
  (let ((plist (and hints (gethash rel hints))))
    (when plist
      (sort
       (loop for (k v) on plist by #'cddr
             when v collect k)
       #'string<
       :key #'symbol-name))))


(defun enum-csp-hint-key-summary (hints)
  "Return deterministic alist of relation -> hint keys."
  (let ((rels nil))
    (when hints
      (maphash (lambda (rel plist)
                 (declare (ignore plist))
                 (push rel rels))
               hints))
    (setf rels (sort rels #'string< :key #'symbol-name))
    (let ((out nil))
      (dolist (rel rels (nreverse out))
        (push (cons rel (or (enum-csp-hint-keys-for-rel hints rel) nil)) out)))))


(defun enum-csp-ir-shape-errors (ir)
  "Return list of IR shape validation error strings."
  (let ((errors nil)
        (base-relations (enum-csp-ir-base-relations ir))
        (constraints (enum-csp-ir-constraints ir))
        (prefilter (enum-csp-ir-prefilter ir))
        (domain-hints (enum-csp-ir-domain-hints ir)))
    (unless (and (listp base-relations)
                 base-relations
                 (every #'symbolp base-relations))
      (push "base-relations must be a non-empty list of symbols" errors))
    (when (and constraints (atom constraints))
      (push "constraints must be NIL or a list form" errors))
    (when (and prefilter
               (not (symbolp prefilter))
               (not (functionp prefilter)))
      (push "prefilter must be NIL, symbol, or function" errors))
    (when (and domain-hints (not (hash-table-p domain-hints)))
      (push "domain-hints must be NIL or a hash table" errors))
    (nreverse errors)))


(defun enum-csp-ir-metadata (&optional (ir *active-enum-csp-ir*) &key backend)
  "Return deterministic metadata summary for IR and BACKEND."
  (check-type ir enum-csp-ir)
  (let* ((base-relations (copy-list (enum-csp-ir-base-relations ir)))
         (constraints (enum-csp-ir-constraints ir))
         (prefilter (enum-csp-ir-prefilter ir))
         (hint-summary (enum-csp-hint-key-summary (enum-csp-ir-domain-hints ir)))
         (shape-errors (enum-csp-ir-shape-errors ir))
         (prefilter-kind (cond
                           ((null prefilter) :none)
                           ((functionp prefilter) :function)
                           ((symbolp prefilter) :symbol)
                           (t :invalid))))
    (list :backend backend
          :spec-name (enum-csp-ir-spec-name ir)
          :base-relations base-relations
          :base-relation-count (length base-relations)
          :constraint-present (not (null constraints))
          :constraint-form-kind (if constraints :list :none)
          :prefilter-kind prefilter-kind
          :hint-keys hint-summary
          :shape-valid-p (null shape-errors)
          :shape-errors shape-errors)))


(defun cspir-sort-items-by-string (items)
  "Sort ITEMS by prin1 string."
  (sort (copy-list items)
        #'string<
        :key #'prin1-to-string))


(defun cspir-ir-domain-hint (ir rel key)
  "Return IR domain hint value for REL KEY, or NIL."
  (let ((plist (and (enum-csp-ir-domain-hints ir)
                    (gethash rel (enum-csp-ir-domain-hints ir)))))
    (and plist (getf plist key))))


(defun cspir-relation-symmetric-p (rel)
  "Return T when REL is detected as symmetric by current relation metadata."
  (member rel (enum-detect-symmetric-relations (list rel)) :test #'eq))


(defun cspir-subset-keys-for-rel (rel)
  "Return deterministic subset keys for REL after :KEY-TYPES filtering."
  (let* ((sig (relation-signature rel))
         (key-type-spec (and sig (first sig)))
         (all-key-instances (expand-type-spec-instances* key-type-spec))
         (key-types-spec (enum-key-types rel)))
    (if key-types-spec
        (let ((types (cdr key-types-spec)))
          (remove-if-not
           (lambda (obj)
             (some (lambda (ty)
                     (member obj (maybe-type-instances ty) :test #'eq))
                   types))
           all-key-instances))
        all-key-instances)))


(defun cspir-subset-partners-for-rel (ir rel)
  "Return deterministic subset partners for REL after hint filtering."
  (let* ((sig (relation-signature rel))
         (partner-type-spec (and sig (second sig)))
         (partners0 (expand-type-spec-instances* partner-type-spec))
         (hint-partners (cspir-ir-domain-hint ir rel :partners)))
    (cspir-sort-items-by-string
     (if hint-partners
         (remove-if-not (lambda (p)
                          (member p hint-partners :test #'eq))
                        partners0)
         partners0))))


(defun cspir-fluent-relation-plan (ir rel)
  "Build next-backend plan entry for fluent REL."
  (let* ((keys (cspir-sort-items-by-string (enum-key-tuples-for rel nil nil nil)))
         (vtuples0 (or (enum-value-tuples-for rel) nil))
         (hint-vtuples (cspir-ir-domain-hint ir rel :value-tuples))
         (filtered-vtuples (if (and vtuples0 hint-vtuples)
                               (remove-if-not (lambda (vt)
                                                (member vt hint-vtuples :test #'equal))
                                              vtuples0)
                               vtuples0))
         (vtuples (cspir-sort-items-by-string filtered-vtuples))
         (choice-counts
          (mapcar (lambda (k)
                    (+ (length vtuples)
                       (if (enum-key-allows-unassigned-p rel k) 1 0)))
                  keys))
         (leaf-upper-bound
          (if choice-counts
              (reduce #'* choice-counts :initial-value 1)
              0)))
    (list :relation rel
          :pattern :fluent
          :key-count (length keys)
          :value-tuple-count (length vtuples)
          :unassigned-key-count (count-if (lambda (k)
                                            (enum-key-allows-unassigned-p rel k))
                                          keys)
          :symmetric-batch-p (not (null (enum-symmetric-batch-p rel)))
          :estimated-leaf-upper-bound leaf-upper-bound)))


(defun cspir-subset-relation-plan (ir rel)
  "Build next-backend plan entry for subset REL."
  (let* ((keys (cspir-sort-items-by-string (cspir-subset-keys-for-rel rel)))
         (partners (cspir-subset-partners-for-rel ir rel))
         (max-k (or (enum-max-per-key rel) 4))
         (sym-rel-p (cspir-relation-symmetric-p rel))
         (per-key-subset-counts
          (mapcar (lambda (k)
                    (let* ((allowed (enum-allowed-partners-for
                                     k keys partners :symmetric-relation-p sym-rel-p))
                           (sets (subsets-up-to allowed max-k)))
                      (cons k (length sets))))
                  keys))
         (leaf-upper-bound
          (if per-key-subset-counts
              (reduce #'* (mapcar #'cdr per-key-subset-counts) :initial-value 1)
              0)))
    (list :relation rel
          :pattern :subset
          :key-count (length keys)
          :partner-count (length partners)
          :max-per-key max-k
          :requires-fluent (enum-requires-fluent rel)
          :symmetric-relation-p sym-rel-p
          :estimated-leaf-upper-bound leaf-upper-bound
          :estimated-choice-points
          (reduce #'+ (mapcar #'cdr per-key-subset-counts) :initial-value 0))))


(defun cspir-relation-plan (ir rel)
  "Build next-backend plan entry for REL."
  (let ((pattern (enum-pattern rel)))
    (case pattern
      (:fluent
       (cspir-fluent-relation-plan ir rel))
      (:subset
       (cspir-subset-relation-plan ir rel))
      (otherwise
       (list :relation rel :pattern pattern :unsupported t)))))


(defun build-enum-csp-next-plan (&optional (ir *active-enum-csp-ir*))
  "Compile IR into deterministic next-backend plan summary."
  (check-type ir enum-csp-ir)
  (let* ((rels (copy-list (enum-csp-ir-base-relations ir)))
         (ordered-rels (sort rels #'string< :key #'symbol-name))
         (relation-plans (mapcar (lambda (rel)
                                   (cspir-relation-plan ir rel))
                                 ordered-rels))
         (leaf-upper-bound
          (if relation-plans
              (reduce #'* (mapcar (lambda (rp)
                                    (or (getf rp :estimated-leaf-upper-bound) 1))
                                  relation-plans)
                      :initial-value 1)
              0)))
    (make-enum-csp-next-plan
     :spec-name (enum-csp-ir-spec-name ir)
     :base-relations ordered-rels
     :relation-plans relation-plans
     :leaf-upper-bound leaf-upper-bound)))


(defun cspir-relation-plans-for-pattern (relation-plans pattern)
  "Return RELATION-PLANS entries with PATTERN."
  (remove-if-not (lambda (rp)
                   (eq (getf rp :pattern) pattern))
                 relation-plans))


(defun cspir-relation-plans-upper-bound (relation-plans)
  "Multiply :ESTIMATED-LEAF-UPPER-BOUND across RELATION-PLANS."
  (if relation-plans
      (reduce #'* (mapcar (lambda (rp)
                            (or (getf rp :estimated-leaf-upper-bound) 1))
                          relation-plans)
              :initial-value 1)
      1))


(defun cspir-next-plan-phase-summary (plan)
  "Return deterministic phase summary plist list for PLAN."
  (check-type plan enum-csp-next-plan)
  (let* ((relation-plans (enum-csp-next-plan-relation-plans plan))
         (fluent-plans (cspir-relation-plans-for-pattern relation-plans :fluent))
         (subset-plans (cspir-relation-plans-for-pattern relation-plans :subset)))
    (list (list :phase :fluent
                :relation-count (length fluent-plans)
                :estimated-leaf-upper-bound
                (cspir-relation-plans-upper-bound fluent-plans))
          (list :phase :subset
                :relation-count (length subset-plans)
                :estimated-leaf-upper-bound
                (cspir-relation-plans-upper-bound subset-plans))
          (list :phase :finalize
                :relation-count 0
                :estimated-leaf-upper-bound 1))))


(defun enum-csp-next-plan-summary (&optional (plan *active-enum-csp-next-plan*))
  "Return deterministic plist summary for PLAN."
  (check-type plan enum-csp-next-plan)
  (let ((phases (cspir-next-plan-phase-summary plan)))
    (list :spec-name (enum-csp-next-plan-spec-name plan)
        :base-relations (copy-list (enum-csp-next-plan-base-relations plan))
        :relation-plan-count (length (enum-csp-next-plan-relation-plans plan))
        :leaf-upper-bound (enum-csp-next-plan-leaf-upper-bound plan)
        :relation-plans (copy-tree (enum-csp-next-plan-relation-plans plan))
        :phase-order '(:fluent :subset :finalize)
        :phases phases)))


(defun cspir-make-enum-start-state ()
  "Create the empty start state used by CSP enumeration."
  (make-problem-state
   :name 'start :time 0 :value 0
   :idb (make-hash-table :test 'eql)
   :idb-hash nil))


(defun cspir-apply-update-hash-to-state (state update action)
  "Apply UPDATE (hash-table changes) to STATE and return new state."
  (let ((new-state (copy-problem-state state)))
    (setf (problem-state.idb new-state) (copy-idb (update.changes update)))
    (setf (problem-state.idb-hash new-state) nil)
    (setf (problem-state.name new-state) (action.name action))
    (incf (problem-state.time new-state) (action.duration action))
    (setf (problem-state.value new-state) (update.value update))
    new-state))


(defun cspir-action-successors (state action)
  "Return successor states for ACTION from STATE."
  (let ((out nil))
    (dolist (pre-args (get-precondition-args action state) (nreverse out))
      (let ((pre-result (apply (action.pre-defun-name action) state pre-args)))
        (when pre-result
          (let ((updates (if (eql pre-result t)
                             (funcall (action.eff-defun-name action) state)
                             (apply (action.eff-defun-name action) state pre-result))))
            (dolist (update updates)
              (let ((changes (update.changes update)))
                (when (hash-table-p changes)
                  (push (cspir-apply-update-hash-to-state state update action) out))))))))))


(defun cspir-expand-frontier-with-action (frontier action)
  "Expand FRONTIER by applying ACTION once to each state."
  (let ((next nil))
    (dolist (state frontier (nreverse next))
      (setf next (nconc (nreverse (cspir-action-successors state action))
                        next)))))


(defun cspir-fluent-actions-from-ir (ir)
  "Build fluent-phase actions from IR using existing enumerator generators."
  (let* ((ctx (enum-compute-context (enum-csp-ir-constraints ir)))
         (early nil)
         (main nil))
    (multiple-value-setq (early main)
      (enum-generate-fluent-actions ctx nil))
    (append early main)))


(defun cspir-execute-fluent-pilot-internal (ir)
  "Run fluent pilot for IR and return (values summary frontier)."
  (let* ((actions (cspir-fluent-actions-from-ir ir))
         (frontier (list (cspir-make-enum-start-state)))
         (steps nil))
    (dolist (action actions)
      (setf frontier (cspir-expand-frontier-with-action frontier action))
      (push (list :action (action.name action)
                  :frontier-size (length frontier))
            steps))
    (values
     (list :spec-name (enum-csp-ir-spec-name ir)
           :phase :fluent-pilot
           :action-count (length actions)
           :final-frontier-size (length frontier)
           :steps (nreverse steps))
     frontier)))


(defun execute-enum-csp-next-fluent-pilot (&optional (ir *active-enum-csp-ir*)
                                                     &key (stream nil))
  "Execute next-backend fluent phase only and return deterministic stats."
  (check-type ir enum-csp-ir)
  (multiple-value-bind (summary frontier)
      (cspir-execute-fluent-pilot-internal ir)
    (declare (ignore frontier))
    (when stream
      (format stream "~&[enum-csp-ir] fluent pilot ~S: ~D actions, final frontier ~D~%"
              (getf summary :spec-name)
              (getf summary :action-count)
              (getf summary :final-frontier-size)))
    summary))


(defun cspir-subset-relations-from-ir (ir)
  "Return deterministic subset relation order for IR."
  (let ((rels (remove-if-not (lambda (rel)
                               (eq (enum-pattern rel) :subset))
                             (copy-list (enum-csp-ir-base-relations ir)))))
    (sort rels #'string< :key #'symbol-name)))


(defun cspir-subset-pilot-interchangeable-groups (ir)
  "Return interchangeable object groups for subset pilot ordering/pruning."
  (enum-detect-interchangeable-groups
   (enum-csp-ir-base-relations ir)
   (enum-csp-ir-constraints ir)))


(defun cspir-subset-partners-domain-counts (ir rel)
  "Return domain size counts for REL partners (raw and hint-filtered)."
  (let* ((sig (relation-signature rel))
         (partner-type-spec (and sig (second sig)))
         (partners0 (expand-type-spec-instances* partner-type-spec))
         (hint-partners (cspir-ir-domain-hint ir rel :partners))
         (partners1 (if hint-partners
                        (remove-if-not (lambda (p)
                                         (member p hint-partners :test #'eq))
                                       partners0)
                        partners0)))
    (values (length partners0) (length partners1))))


(defun cspir-subset-pilot-partner-feasible-fn (ir rel)
  "Return combined partner feasibility predicate for subset pilot REL."
  (let* ((base-fn (enum-build-partner-feasible-fn rel))
         (hint-fn (cspir-ir-domain-hint ir rel :partner-feasible-fn)))
    (cond
      ((and base-fn hint-fn)
       (lambda (state key partner)
         (and (funcall base-fn state key partner)
              (enum-call-hint :partner rel hint-fn state key partner))))
      (base-fn base-fn)
      (hint-fn
       (lambda (state key partner)
         (enum-call-hint :partner rel hint-fn state key partner)))
      (t nil))))


(defun cspir-subset-pilot-relation-context (ir rel interchangeable-groups fluent-relations)
  "Build execution context plist for subset pilot REL."
  (let* ((unsorted-keys (cspir-subset-keys-for-rel rel))
         (partners (cspir-subset-partners-for-rel ir rel))
         (max-k (or (enum-max-per-key rel) 4))
         (sym-rel-p (cspir-relation-symmetric-p rel))
         (keys (enum-sort-keys-fail-first unsorted-keys partners max-k
                                          :symmetric-relation-p sym-rel-p))
         (predecessors-map (enum-group-predecessors-map keys interchangeable-groups)))
    (list :relation rel
          :keys keys
          :partners partners
          :max-per-key max-k
          :symmetric-relation-p sym-rel-p
          :requires-fluent (enum-requires-fluent rel)
          :partner-feasible-fn (cspir-subset-pilot-partner-feasible-fn ir rel)
          :predecessors-map predecessors-map
          :fluent-relations fluent-relations)))


(defun cspir-subset-pilot-rejection-reason (state rel key set rel-context)
  "Return rejection reason keyword for subset candidate SET, or NIL on accept."
  (let ((requires-fluent (getf rel-context :requires-fluent)))
    (cond
      ((and requires-fluent
            (not (fluent-bound-p state (list requires-fluent key))))
       (and set :requires-fluent))
      ((and (getf rel-context :partner-feasible-fn)
            (not (every (lambda (p)
                          (funcall (getf rel-context :partner-feasible-fn)
                                   state key p))
                        set)))
       :partner-feasible-fn)
      ((and requires-fluent
            (not (every (lambda (tgt)
                          (or (not (member tgt (getf rel-context :keys) :test #'eq))
                              (fluent-bound-p state (list requires-fluent tgt))))
                        set)))
       :requires-fluent)
      ((not (enum-subset-degree-ok-p state rel key set
                                     (getf rel-context :keys)
                                     (getf rel-context :partners)
                                     (getf rel-context :max-per-key)))
       :degree)
      ((not (enum-residual-symmetry-ok-p state rel key set
                                         (getf rel-context :predecessors-map)
                                         (getf rel-context :fluent-relations)
                                         (getf rel-context :keys)
                                         (getf rel-context :partners)))
       :residual-symmetry)
      (t nil))))


(defun cspir-apply-subset-set-to-state (state rel key set)
  "Return successor state after adding (REL KEY PARTNER) tuples from SET."
  (let ((new-state (copy-problem-state state)))
    (setf (problem-state.name new-state) (intern (format nil "ENUM-~A-~A" rel key) *package*))
    (dolist (partner set)
      (update (problem-state.idb new-state) (list rel key partner)))
    (setf (problem-state.idb-hash new-state) nil)
    new-state))


(defun cspir-expand-frontier-with-subset-key (frontier rel key candidate-sets rel-context)
  "Expand FRONTIER for one subset key and return (values next key-summary)."
  (let ((next nil)
        (accepted 0)
        (pruned-requires 0)
        (pruned-partner-fn 0)
        (branch-sum 0)
        (branch-min nil)
        (branch-max 0))
    (dolist (state frontier)
      (let ((branches-for-state 0))
        (dolist (set candidate-sets)
          (let ((reason (cspir-subset-pilot-rejection-reason state rel key set rel-context)))
            (if reason
                (case reason
                  (:requires-fluent (incf pruned-requires))
                  (:partner-feasible-fn (incf pruned-partner-fn))
                  (otherwise nil))
                (progn
                  (incf accepted)
                  (incf branches-for-state)
                  (push (cspir-apply-subset-set-to-state state rel key set) next)))))
        (incf branch-sum branches-for-state)
        (setf branch-max (max branch-max branches-for-state))
        (setf branch-min (if branch-min
                             (min branch-min branches-for-state)
                             branches-for-state))))
    (let ((state-count (length frontier)))
      (values
       (nreverse next)
       (list :key key
             :candidate-subset-count (length candidate-sets)
             :frontier-size-in state-count
             :frontier-size-out (length next)
             :accepted-branches accepted
             :pruned-branches
             (list :partner-feasible-fn pruned-partner-fn
                   :requires-fluent pruned-requires)
             :branching-min (or branch-min 0)
             :branching-max branch-max
             :branching-avg (if (> state-count 0)
                                (/ branch-sum state-count)
                                0))))))


(defun cspir-execute-subset-pilot-internal (ir fluent-frontier)
  "Run subset pilot from FLUENT-FRONTIER and return (values summary frontier)."
  (let* ((relations (cspir-subset-relations-from-ir ir))
         (interchangeable-groups (cspir-subset-pilot-interchangeable-groups ir))
         (fluent-relations (enum-collect-fluent-relations
                            (enum-all-installed-relations)))
         (frontier fluent-frontier)
         (relation-stats nil)
         (total-partners-pruned 0)
         (total-pruned-requires 0)
         (total-pruned-partner-fn 0))
    (dolist (rel relations)
      (multiple-value-bind (partners-in partners-out)
          (cspir-subset-partners-domain-counts ir rel)
        (let* ((rel-context (cspir-subset-pilot-relation-context
                             ir rel interchangeable-groups fluent-relations))
               (frontier-in (length frontier))
               (key-stats nil)
               (rel-pruned-requires 0)
               (rel-pruned-partner-fn 0))
          (incf total-partners-pruned (- partners-in partners-out))
          (dolist (key (getf rel-context :keys))
            (let* ((allowed (enum-allowed-partners-for
                             key
                             (getf rel-context :keys)
                             (getf rel-context :partners)
                             :symmetric-relation-p
                             (getf rel-context :symmetric-relation-p)))
                   (candidate-sets (subsets-up-to allowed (getf rel-context :max-per-key))))
              (multiple-value-bind (next key-summary)
                  (cspir-expand-frontier-with-subset-key
                   frontier rel key candidate-sets rel-context)
                (setf frontier next)
                (incf rel-pruned-requires
                      (getf (getf key-summary :pruned-branches) :requires-fluent))
                (incf rel-pruned-partner-fn
                      (getf (getf key-summary :pruned-branches) :partner-feasible-fn))
                (push key-summary key-stats))))
          (incf total-pruned-requires rel-pruned-requires)
          (incf total-pruned-partner-fn rel-pruned-partner-fn)
          (push (list :relation rel
                      :key-count (length (getf rel-context :keys))
                      :frontier-size-in frontier-in
                      :frontier-size-out (length frontier)
                      :partners-domain-size-in partners-in
                      :partners-domain-size-out partners-out
                      :pruning-counts
                      (list :partners (- partners-in partners-out)
                            :partner-feasible-fn rel-pruned-partner-fn
                            :requires-fluent rel-pruned-requires)
                      :key-stats (nreverse key-stats))
                relation-stats))))
    (values
     (list :spec-name (enum-csp-ir-spec-name ir)
           :phase :subset-pilot
           :fluent-frontier-size-in (length fluent-frontier)
           :subset-frontier-size-out (length frontier)
           :relation-count (length relations)
           :pruning-counts
           (list :partners total-partners-pruned
                 :partner-feasible-fn total-pruned-partner-fn
                 :requires-fluent total-pruned-requires)
           :relation-stats (nreverse relation-stats))
     frontier)))


(defun execute-enum-csp-next-subset-pilot (&optional (ir *active-enum-csp-ir*)
                                                     &key (stream nil))
  "Execute subset pilot on top of fluent pilot frontier and return stats."
  (check-type ir enum-csp-ir)
  (multiple-value-bind (fluent-summary fluent-frontier)
      (cspir-execute-fluent-pilot-internal ir)
    (declare (ignore fluent-summary))
    (multiple-value-bind (subset-summary subset-frontier)
        (cspir-execute-subset-pilot-internal ir fluent-frontier)
      (declare (ignore subset-frontier))
      (when stream
        (format stream "~&[enum-csp-ir] subset pilot ~S: fluent in ~D, subset out ~D~%"
                (getf subset-summary :spec-name)
                (getf subset-summary :fluent-frontier-size-in)
                (getf subset-summary :subset-frontier-size-out)))
      subset-summary)))


(defun cspir-execute-next-backend-phase2-pilot (ir)
  "Run phase-1 fluent + phase-2 subset pilots and return both summaries."
  (multiple-value-bind (fluent-summary fluent-frontier)
      (cspir-execute-fluent-pilot-internal ir)
    (multiple-value-bind (subset-summary subset-frontier)
        (cspir-execute-subset-pilot-internal ir fluent-frontier)
      (declare (ignore subset-frontier))
      (values fluent-summary subset-summary))))


(defun cspir-resolve-finalize-prefilter (ir)
  "Return two values: resolved prefilter function for IR and status keyword."
  (let ((prefilter (enum-csp-ir-prefilter ir)))
    (cond
      ((null prefilter) (values nil :none))
      ((functionp prefilter) (values prefilter :function))
      ((and (symbolp prefilter) (fboundp prefilter))
       (values (symbol-function prefilter) :symbol))
      ((symbolp prefilter) (values nil :symbol-unbound))
      (t (values nil :invalid)))))


(defun cspir-copy-state-for-finalize (state)
  "Return a fresh problem-state copy for finalize-pilot processing."
  (if (fboundp 'copy-state)
      (copy-state state)
      (copy-problem-state state)))


(defun cspir-resolve-finalize-goal-fn ()
  "Return currently installed goal function for finalize checks, or NIL."
  (and (fboundp 'goal-fn)
       (symbol-function 'goal-fn)))


(defun cspir-build-constraint-filter-fn (ir)
  "Build a runtime constraint filter function from IR constraints, or NIL."
  (let ((constraints (enum-csp-ir-constraints ir)))
    (when constraints
      (let* ((new-$vars (delete-duplicates
                         (get-all-nonspecial-vars #'$varp constraints)))
             (lambda-expr
               `(lambda (state)
                  (declare (ignorable state))
                  (let (,@new-$vars)
                    (declare (ignorable ,@new-$vars))
                    ,(if (eql (car constraints) 'let)
                         `(let ,(second constraints)
                            ,(third constraints)
                            ,(translate (fourth constraints) 'pre))
                         (translate constraints 'pre))))))
        (fix-if-ignore '(state) lambda-expr)
        (compile nil
                 (if (fboundp 'subst-int-code)
                     (subst-int-code lambda-expr)
                     lambda-expr))))))


(defun cspir-execute-finalize-pilot-internal (ir subset-frontier &key goal-fn)
  "Run finalize pilot from SUBSET-FRONTIER and return (values summary frontier)."
  (multiple-value-bind (prefilter-fn prefilter-status)
      (cspir-resolve-finalize-prefilter ir)
    (let* ((constraint-fn (cspir-build-constraint-filter-fn ir))
           (propagation-available-p (fboundp 'maybe-propagate-changes!))
           (goal-fn (or goal-fn (cspir-resolve-finalize-goal-fn)))
           (prefilter-checked 0)
           (prefilter-skipped 0)
           (prefilter-passed 0)
           (prefilter-pruned 0)
           (constraint-checked 0)
           (constraint-skipped 0)
           (constraint-passed 0)
           (constraint-pruned 0)
           (propagation-attempted 0)
           (propagation-performed 0)
           (propagation-skipped 0)
           (propagation-pruned 0)
           (goal-checked 0)
           (goal-skipped 0)
           (goal-passed 0)
           (goal-pruned 0)
           (accepted-by-goal 0)
           (accepted-without-goal 0)
           (accepted-frontier nil))
      (dolist (state subset-frontier)
        (let ((working (cspir-copy-state-for-finalize state))
              (keep-state t))
          (cond
            (prefilter-fn
             (incf prefilter-checked)
             (if (funcall prefilter-fn working)
                 (incf prefilter-passed)
                 (progn
                   (setf keep-state nil)
                   (incf prefilter-pruned))))
            (t
             (incf prefilter-skipped)
             (incf prefilter-passed)))
          (when keep-state
            (if constraint-fn
                (progn
                  (incf constraint-checked)
                  (if (funcall constraint-fn working)
                      (incf constraint-passed)
                      (progn
                        (setf keep-state nil)
                        (incf constraint-pruned))))
                (progn
                  (incf constraint-skipped)
                  (incf constraint-passed))))
          (when keep-state
            (incf propagation-attempted)
            (if propagation-available-p
                (handler-case
                    (progn
                      (incf propagation-performed)
                      (funcall (symbol-function 'maybe-propagate-changes!) working))
                  (error ()
                    (setf keep-state nil)
                    (incf propagation-pruned)))
                (incf propagation-skipped))
            (when keep-state
              (if goal-fn
                  (progn
                    (incf goal-checked)
                    (if (funcall goal-fn working)
                        (progn
                          (incf goal-passed)
                          (incf accepted-by-goal))
                        (progn
                          (setf keep-state nil)
                          (incf goal-pruned))))
                  (progn
                    (incf goal-skipped)
                    (incf accepted-without-goal)))))
          (when keep-state
            (push working accepted-frontier))))
      (let ((accepted (length accepted-frontier)))
        (values
         (list :spec-name (enum-csp-ir-spec-name ir)
               :phase :finalize-pilot
               :subset-frontier-size-in (length subset-frontier)
               :finalize-frontier-size-out accepted
               :accepted-final-size accepted
               :prefilter-status prefilter-status
               :prefilter-counts
               (list :checked prefilter-checked
                     :passed prefilter-passed
                     :pruned prefilter-pruned
                     :skipped prefilter-skipped)
               :constraint-counts
               (list :checked constraint-checked
                     :passed constraint-passed
                     :pruned constraint-pruned
                     :skipped constraint-skipped)
               :propagation-counts
               (list :attempted propagation-attempted
                     :performed propagation-performed
                     :skipped propagation-skipped)
               :goal-check-counts
               (list :available-p (not (null goal-fn))
                     :checked goal-checked
                     :passed goal-passed
                     :pruned goal-pruned
                     :skipped goal-skipped)
               :prune-reason-counts
               (list :prefilter prefilter-pruned
                     :constraints constraint-pruned
                     :propagation propagation-pruned
                     :goal goal-pruned)
               :accept-reason-counts
               (list :goal accepted-by-goal
                     :no-goal-check accepted-without-goal))
         (nreverse accepted-frontier))))))


(defun execute-enum-csp-next-finalize-pilot (&optional (ir *active-enum-csp-ir*)
                                                       &key (stream nil))
  "Execute finalize pilot on top of subset pilot frontier and return stats."
  (check-type ir enum-csp-ir)
  (multiple-value-bind (fluent-summary fluent-frontier)
      (cspir-execute-fluent-pilot-internal ir)
    (declare (ignore fluent-summary))
    (multiple-value-bind (subset-summary subset-frontier)
        (cspir-execute-subset-pilot-internal ir fluent-frontier)
      (declare (ignore subset-summary))
      (multiple-value-bind (finalize-summary finalize-frontier)
          (cspir-execute-finalize-pilot-internal ir subset-frontier)
        (declare (ignore finalize-frontier))
        (when stream
          (format stream "~&[enum-csp-ir] finalize pilot ~S: subset in ~D, finalize out ~D~%"
                  (getf finalize-summary :spec-name)
                  (getf finalize-summary :subset-frontier-size-in)
                  (getf finalize-summary :finalize-frontier-size-out)))
        finalize-summary))))


(defun cspir-execute-next-backend-phase3-pilot (ir)
  "Run phase-1..3 pilots and return fluent/subset/finalize summaries."
  (multiple-value-bind (fluent-summary fluent-frontier)
      (cspir-execute-fluent-pilot-internal ir)
    (multiple-value-bind (subset-summary subset-frontier)
        (cspir-execute-subset-pilot-internal ir fluent-frontier)
      (multiple-value-bind (finalize-summary finalize-frontier)
          (cspir-execute-finalize-pilot-internal ir subset-frontier)
        (declare (ignore finalize-frontier))
        (values fluent-summary subset-summary finalize-summary)))))


(defun cspir-execute-next-backend-phase3-pilot-with-frontier (ir &key goal-fn)
  "Run phase-1..3 pilots and return fluent/subset/finalize summaries plus frontier."
  (multiple-value-bind (fluent-summary fluent-frontier)
      (cspir-execute-fluent-pilot-internal ir)
    (multiple-value-bind (subset-summary subset-frontier)
        (cspir-execute-subset-pilot-internal ir fluent-frontier)
      (multiple-value-bind (finalize-summary finalize-frontier)
          (cspir-execute-finalize-pilot-internal ir subset-frontier :goal-fn goal-fn)
        (values fluent-summary subset-summary finalize-summary finalize-frontier)))))


(defun enum-csp-next-find-goal-states-fn (&optional (goal-spec 'goal-fn)
                                                 &key (algorithm *algorithm*) (solution-type 'every)
                                                   exclude-relations include-relations
                                                   (prefilter :use-installed)
                                                   sort< sort-key
                                                   (ir *active-enum-csp-ir*))
  "Next-backend path for FIND-GOAL-STATES-FN.
   Uses phase-3 finalize pilot frontier as candidate goal states, then applies
   UI-compatible sorting/reporting behavior."
  (declare (ignore algorithm))
  (check-type ir enum-csp-ir)
  (when (and exclude-relations include-relations)
    (error "FIND-GOAL-STATES (next-backend): simultaneous :EXCLUDE-RELATIONS and :INCLUDE-RELATIONS are unsupported."))
  (when (or exclude-relations include-relations)
    (error "FIND-GOAL-STATES (next-backend): :EXCLUDE-RELATIONS/:INCLUDE-RELATIONS are not yet supported in next mode."))
  (when (and sort< sort-key)
    (error "FIND-GOAL-STATES: :SORT< and :SORT-KEY are mutually exclusive."))
  (multiple-value-bind (norm-goal-spec goal-form)
      (enum-normalize-goal-spec goal-spec)
    (let* ((normalized-solution-type (if (fboundp 'fgs-normalize-solution-type)
                                         (fgs-normalize-solution-type solution-type)
                                         solution-type))
           (goal-fn (coerce-goal norm-goal-spec))
           (ir-run (if (eq prefilter :use-installed)
                       ir
                       (build-enum-csp-ir
                        :spec-name (enum-csp-ir-spec-name ir)
                        :base-relations (copy-list (enum-csp-ir-base-relations ir))
                        :constraints (enum-csp-ir-constraints ir)
                        :prefilter prefilter
                        :domain-hints (enum-csp-ir-domain-hints ir)
                        :constraint-filter-name (enum-csp-ir-constraint-filter-name ir))))
           (states nil))
      (multiple-value-bind (fluent-summary subset-summary finalize-summary finalize-frontier)
          (cspir-execute-next-backend-phase3-pilot-with-frontier ir-run :goal-fn goal-fn)
        (declare (ignore fluent-summary subset-summary finalize-summary))
        (setf states finalize-frontier))
      (when sort<
        (setf states (stable-sort states sort<)))
      (when sort-key
        (let ((decorated (mapcar (lambda (st) (cons (funcall sort-key st) st)) states)))
          (setf states (mapcar #'cdr (stable-sort decorated #'< :key #'car)))))
      (setf states
            (cond
              ((eq normalized-solution-type 'first)
               (if states (list (first states)) nil))
              ((integerp normalized-solution-type)
               (last states normalized-solution-type))
              (t
               states)))
      (when (boundp '*enumerated-goal-states*)
        (setf *enumerated-goal-states* states))
      (let ((report (fgs-build-report goal-form states)))
        (fgs-print-report report)
        (when (fboundp 'enum-print-hint-diagnostics)
          (enum-print-hint-diagnostics))
        (setf (get 'find-goal-states :last-report) report)
        t))))


(defun cspir-state-base-prop-key (state base-relations)
  "Return canonical base-proposition key string for STATE over BASE-RELATIONS."
  (if (fboundp 'fps-state-base-prop-key)
      (fps-state-base-prop-key state base-relations)
      (let* ((props (remove-if-not
                     (lambda (p)
                       (member (car p) base-relations :test #'eq))
                     (list-database (problem-state.idb state))))
             (sorted (sort props (lambda (x y)
                                   (string< (prin1-to-string x)
                                            (prin1-to-string y))))))
        (prin1-to-string sorted))))


(defun cspir-state-key-signature (states base-relations)
  "Return deterministic signature plist for STATES projected to BASE-RELATIONS."
  (let ((keys nil)
        (seen (make-hash-table :test 'equal)))
    (dolist (state states)
      (let ((k (cspir-state-base-prop-key state base-relations)))
        (unless (gethash k seen)
          (setf (gethash k seen) t)
          (push k keys))))
    (let* ((sorted-keys (sort keys #'string<))
           (count (length sorted-keys))
           (first-key (and sorted-keys (first sorted-keys)))
           (last-key (and sorted-keys (car (last sorted-keys)))))
      (list :count count
            :first-key first-key
            :last-key last-key
            :signature (prin1-to-string sorted-keys)))))


(defun cspir-comparison-differences (legacy-sig next-sig)
  "Return deterministic mismatch entries between LEGACY-SIG and NEXT-SIG."
  (let ((diffs nil))
    (dolist (k '(:count :first-key :last-key :signature))
      (let ((lv (getf legacy-sig k))
            (nv (getf next-sig k)))
        (unless (equal lv nv)
          (push (list k lv nv) diffs))))
    (nreverse diffs)))


(defun compare-enum-csp-next-finalize-with-find-goal-states
    (&optional (ir *active-enum-csp-ir*)
               &key (goal-spec 'goal-fn) (stream nil))
  "Compare next-backend finalize pilot accepted states with legacy FIND-GOAL-STATES.
   Returns deterministic plist with counts/signatures and mismatch details."
  (check-type ir enum-csp-ir)
  (let ((base-relations (copy-list (enum-csp-ir-base-relations ir))))
    (multiple-value-bind (fluent-summary subset-summary finalize-summary finalize-frontier)
        (cspir-execute-next-backend-phase3-pilot-with-frontier ir)
      (declare (ignore fluent-summary subset-summary))
      (let* ((*standard-output* (make-broadcast-stream))
             (*error-output* (make-broadcast-stream))
             (*query-io* (make-two-way-stream
                          (make-string-input-stream "n\n")
                          (make-string-output-stream))))
        (find-goal-states-fn goal-spec :solution-type 'every :backend :legacy)
        (let* ((report (get 'find-goal-states :last-report))
               (legacy-states (or (getf report :goal-states) nil))
               (legacy-sig (cspir-state-key-signature legacy-states base-relations))
               (next-sig (cspir-state-key-signature finalize-frontier base-relations))
               (diffs (cspir-comparison-differences legacy-sig next-sig))
               (comparison
                (list :spec-name (enum-csp-ir-spec-name ir)
                      :goal-spec goal-spec
                      :base-relations base-relations
                      :legacy-goal-signature legacy-sig
                      :next-finalize-signature next-sig
                      :finalize-pilot-summary finalize-summary
                      :match-p (null diffs)
                      :differences diffs)))
          (when stream
            (format stream "~&[enum-csp-ir] finalize-vs-find-goal-states for ~S: ~:[MISMATCH~;MATCH~]~%"
                    (getf comparison :spec-name)
                    (getf comparison :match-p))
            (format stream "  legacy-count=~D next-count=~D~%"
                    (getf legacy-sig :count)
                    (getf next-sig :count))
            (when diffs
              (dolist (d diffs)
                (format stream "  ~S~%" d))))
          comparison)))))


(defun report-enum-csp-next-finalize-find-goal-states-mismatches
    (&optional (ir *active-enum-csp-ir*)
               &key (goal-spec 'goal-fn) (comparison nil) (stream *standard-output*))
  "Print only finalize-vs-find-goal-states mismatches; return mismatch list.
   Emits no output when comparison matches."
  (let* ((comparison (or comparison
                         (compare-enum-csp-next-finalize-with-find-goal-states
                          ir :goal-spec goal-spec :stream nil)))
         (diffs (getf comparison :differences)))
    (when (and stream diffs)
      (format stream "~&[enum-csp-ir] finalize-vs-find-goal-states mismatches~%")
      (dolist (d diffs)
        (format stream "  ~S~%" d)))
    diffs))


(defun run-enum-csp-e2e-finalize-parity
    (&optional (ir *active-enum-csp-ir*)
               &key (goal-spec 'goal-fn)
                    (strict nil)
                    (stream *standard-output*))
  "Run end-to-end legacy find-goal-states vs next finalize parity check.
   Returns a deterministic summary plist.  When STRICT is non-NIL, mismatches signal an error."
  (let* ((comparison (compare-enum-csp-next-finalize-with-find-goal-states
                      ir :goal-spec goal-spec :stream nil))
         (legacy-sig (getf comparison :legacy-goal-signature))
         (next-sig (getf comparison :next-finalize-signature))
         (diffs (or (getf comparison :differences) nil))
         (summary (list :spec-name (getf comparison :spec-name)
                        :goal-spec (getf comparison :goal-spec)
                        :legacy-goal-count (getf legacy-sig :count)
                        :next-finalize-count (getf next-sig :count)
                        :match-p (getf comparison :match-p)
                        :difference-count (length diffs)
                        :difference-keys (mapcar #'first diffs)
                        :differences diffs
                        :comparison comparison)))
    (when stream
      (format stream "~&[enum-csp-ir] e2e finalize parity for ~S: ~:[MISMATCH~;MATCH~]~%"
              (getf summary :spec-name)
              (getf summary :match-p))
      (format stream "  legacy-goals=~D next-finalize=~D diff-count=~D keys=~S~%"
              (getf summary :legacy-goal-count)
              (getf summary :next-finalize-count)
              (getf summary :difference-count)
              (getf summary :difference-keys)))
    (when (and strict (not (getf summary :match-p)))
      (error "[enum-csp-ir] strict e2e finalize parity mismatch: ~S"
             (getf summary :differences)))
    summary))


(defun cspir-choose-comb (n k)
  "Return binomial coefficient C(N,K) for non-negative integers."
  (cond
    ((or (< k 0) (> k n)) 0)
    ((or (= k 0) (= k n)) 1)
    (t
     (let ((kk (min k (- n k)))
           (acc 1))
       (loop for i from 1 to kk do
         (setf acc (/ (* acc (+ (- n kk) i)) i)))
       acc))))


(defun cspir-subset-count-upper-bound (partner-count max-k)
  "Return upper bound on subset choices with PARTNER-COUNT and MAX-K."
  (loop for k from 0 to (min partner-count max-k)
        sum (cspir-choose-comb partner-count k)))


(defun enum-csp-next-pilot-bound-check (&optional (result *enum-csp-backend-last-result*))
  "Validate phase-2/3 pilot summaries in RESULT against next-plan bounds."
  (let* ((plan-summary (getf result :next-plan-summary))
         (fluent-summary (getf result :fluent-pilot-summary))
         (subset-summary (getf result :subset-pilot-summary))
         (finalize-summary (getf result :finalize-pilot-summary))
         (issues nil))
    (cond
      ((null plan-summary)
       (list :ok-p nil :issues (list (list :missing :next-plan-summary))))
      ((null subset-summary)
       (list :ok-p nil :issues (list (list :missing :subset-pilot-summary))))
      (t
       (let* ((phases (getf plan-summary :phases))
              (subset-phase (find :subset phases :key (lambda (p) (getf p :phase)) :test #'eq))
              (finalize-phase (find :finalize phases :key (lambda (p) (getf p :phase)) :test #'eq))
              (subset-bound (and subset-phase (getf subset-phase :estimated-leaf-upper-bound)))
              (finalize-bound (and finalize-phase (getf finalize-phase :estimated-leaf-upper-bound)))
              (fluent-in (getf subset-summary :fluent-frontier-size-in))
              (subset-out (getf subset-summary :subset-frontier-size-out))
              (phase-bound (and (integerp subset-bound)
                                (integerp fluent-in)
                                (* subset-bound fluent-in))))
         (when (and fluent-summary
                    (not (= fluent-in (getf fluent-summary :final-frontier-size))))
           (push (list :fluent-frontier-size-mismatch
                       fluent-in
                       (getf fluent-summary :final-frontier-size))
                 issues))
         (unless (and (integerp phase-bound)
                      (<= subset-out phase-bound))
           (push (list :subset-frontier-out-of-bound subset-out phase-bound) issues))
         (dolist (rel-stat (getf subset-summary :relation-stats))
           (let ((rel-plan (find (getf rel-stat :relation)
                                 (getf plan-summary :relation-plans)
                                 :key (lambda (rp) (getf rp :relation))
                                 :test #'eq)))
             (cond
               ((null rel-plan)
                (push (list :missing-relation-plan (getf rel-stat :relation)) issues))
               ((not (eq (getf rel-plan :pattern) :subset))
                (push (list :relation-plan-not-subset (getf rel-stat :relation)) issues))
               (t
                (when (/= (getf rel-stat :key-count)
                          (getf rel-plan :key-count))
                  (push (list :key-count-mismatch
                              (getf rel-stat :relation)
                              (getf rel-stat :key-count)
                              (getf rel-plan :key-count))
                        issues))
                (when (> (getf rel-stat :partners-domain-size-out)
                         (getf rel-plan :partner-count))
                  (push (list :partner-domain-exceeds-plan
                              (getf rel-stat :relation)
                              (getf rel-stat :partners-domain-size-out)
                              (getf rel-plan :partner-count))
                        issues))
                 (let* ((partner-cap (getf rel-stat :partners-domain-size-out))
                        (max-k (getf rel-plan :max-per-key))
                        (choice-cap (cspir-subset-count-upper-bound partner-cap max-k)))
                  (dolist (key-stat (getf rel-stat :key-stats))
                    (when (> (getf key-stat :candidate-subset-count) choice-cap)
                      (push (list :candidate-subset-count-exceeds-cap
                                  (getf rel-stat :relation)
                                  (getf key-stat :key)
                                  (getf key-stat :candidate-subset-count)
                                  choice-cap)
                            issues))))))))
         (if (null finalize-summary)
             (push (list :missing :finalize-pilot-summary) issues)
             (let* ((finalize-in (getf finalize-summary :subset-frontier-size-in))
                    (finalize-out (getf finalize-summary :finalize-frontier-size-out))
                    (finalize-phase-bound (and (integerp finalize-bound)
                                               (integerp finalize-in)
                                               (* finalize-bound finalize-in)))
                    (prune-counts (getf finalize-summary :prune-reason-counts))
                    (goal-counts (getf finalize-summary :goal-check-counts))
                    (accept-counts (getf finalize-summary :accept-reason-counts))
                    (prefilter-counts (getf finalize-summary :prefilter-counts))
                    (constraint-counts (getf finalize-summary :constraint-counts))
                    (total-pruned (+ (or (getf prune-counts :prefilter) 0)
                                     (or (getf prune-counts :constraints) 0)
                                     (or (getf prune-counts :propagation) 0)
                                     (or (getf prune-counts :goal) 0))))
               (when (not (= finalize-in subset-out))
                 (push (list :finalize-frontier-in-mismatch
                             finalize-in
                             subset-out)
                       issues))
               (unless (and (integerp finalize-phase-bound)
                            (<= finalize-out finalize-phase-bound))
                 (push (list :finalize-frontier-out-of-bound finalize-out finalize-phase-bound)
                       issues))
               (when (/= (+ finalize-out total-pruned) finalize-in)
                 (push (list :finalize-accounting-mismatch
                             :in finalize-in
                             :out finalize-out
                             :pruned total-pruned)
                       issues))
               (let ((goal-available (getf goal-counts :available-p)))
                 (if goal-available
                     (when (/= (or (getf goal-counts :passed) 0)
                               (or (getf accept-counts :goal) 0))
                       (push (list :finalize-goal-accept-mismatch
                                   (getf goal-counts :passed)
                                   (getf accept-counts :goal))
                             issues))
                     (when (/= finalize-out (or (getf accept-counts :no-goal-check) 0))
                       (push (list :finalize-no-goal-accept-mismatch
                                   finalize-out
                                   (getf accept-counts :no-goal-check))
                             issues))))
               (when (/= (or (getf prefilter-counts :passed) 0)
                         (+ (or (getf constraint-counts :pruned) 0)
                            (or (getf constraint-counts :passed) 0)))
                 (push (list :finalize-prefilter-constraint-mismatch
                             (getf prefilter-counts :passed)
                             constraint-counts)
                       issues))
               (when (/= (or (getf constraint-counts :passed) 0)
                         (or (getf (getf finalize-summary :propagation-counts) :attempted) 0))
                 (push (list :finalize-constraint-propagation-mismatch
                             (getf constraint-counts :passed)
                             (getf (getf finalize-summary :propagation-counts) :attempted))
                       issues))))
         (list :ok-p (null issues)
               :issues (nreverse issues)))))))


(defun cspir-run-next-pilot-bound-check-if-enabled (result)
  "Optionally compute and store next pilot bound-check summary for RESULT."
  (let ((check (and *enum-csp-next-pilot-bound-check-enabled*
                    (enum-csp-next-pilot-bound-check result))))
    (setf *enum-csp-next-pilot-bound-check-last* check)
    (when (and check
               *enum-csp-next-pilot-bound-check-strict*
               (not (getf check :ok-p)))
      (error "[enum-csp-ir] strict next pilot bound-check mismatch: ~S"
             (getf check :issues)))
    check))


(defun report-enum-csp-next-pilot-bound-check (&optional (result *enum-csp-backend-last-result*)
                                                         &key (stream *standard-output*))
  "Print only next-pilot bound-check issues; return issue list.
   Emits no output when check passes."
  (let* ((check (enum-csp-next-pilot-bound-check result))
         (issues (getf check :issues)))
    (setf *enum-csp-next-pilot-bound-check-last* check)
    (when (and stream issues)
      (format stream "~&[enum-csp-ir] next pilot bound-check mismatches~%")
      (dolist (issue issues)
        (format stream "  ~S~%" issue)))
    issues))


(defun enum-csp-next-finalize-bound-check-issues (&optional (result *enum-csp-backend-last-result*))
  "Return finalize-focused issue subset from next-pilot bound-check RESULT."
  (let* ((check (enum-csp-next-pilot-bound-check result))
         (issues (getf check :issues)))
    (remove-if-not (lambda (issue)
                     (let ((kind (first issue)))
                       (or (eq kind :missing)
                           (search "FINALIZE" (string-upcase (symbol-name kind))))))
                   issues)))


(defun report-enum-csp-next-finalize-mismatches (&optional (result *enum-csp-backend-last-result*)
                                                           &key (stream *standard-output*))
  "Print only finalize-related next-pilot bound-check mismatches; return issue list.
   Emits no output when finalize checks pass."
  (let ((issues (enum-csp-next-finalize-bound-check-issues result)))
    (when (and stream issues)
      (format stream "~&[enum-csp-ir] next pilot finalize mismatches~%")
      (dolist (issue issues)
        (format stream "  ~S~%" issue)))
    issues))


(defun cspir-plist-keys (plist)
  "Return deterministic key list for PLIST."
  (loop for tail on plist by #'cddr
        collect (first tail)))


(defun enum-csp-migration-health-snapshot (&key (recompute-bound-check nil))
  "Return consolidated enum-csp migration health snapshot plist."
  (let* ((backend *enum-csp-backend*)
         (last-result *enum-csp-backend-last-result*)
         (last-shadow *enum-csp-shadow-compare-last*)
         (shadow-diffs (and last-shadow (getf last-shadow :differences)))
         (bound-check
          (cond
            (recompute-bound-check
             (and last-result (enum-csp-next-pilot-bound-check last-result)))
            (t *enum-csp-next-pilot-bound-check-last*)))
         (bound-issues (and bound-check (getf bound-check :issues)))
         (plan-summary (and last-result (getf last-result :next-plan-summary)))
         (fluent-summary (and last-result (getf last-result :fluent-pilot-summary)))
         (subset-summary (and last-result (getf last-result :subset-pilot-summary)))
         (finalize-summary (and last-result (getf last-result :finalize-pilot-summary))))
    (list :backend backend
          :spec-name (and last-result (getf last-result :spec-name))
          :invocations *enum-csp-next-backend-invocations*
          :flags
          (list :shadow-compare-enabled *enum-csp-shadow-compare-enabled*
                :shadow-compare-strict *enum-csp-shadow-compare-strict*
                :run-fluent-pilot *enum-csp-next-backend-run-fluent-pilot*
                :pilot-bound-check-enabled *enum-csp-next-pilot-bound-check-enabled*
                :pilot-bound-check-strict *enum-csp-next-pilot-bound-check-strict*)
          :shadow-parity
          (and last-shadow
               (list :available-p t
                     :match-p (getf last-shadow :match-p)
                     :difference-keys (mapcar #'first shadow-diffs)
                     :difference-count (length shadow-diffs)))
          :next-plan
          (and plan-summary
               (list :relation-plan-count (getf plan-summary :relation-plan-count)
                     :phase-order (getf plan-summary :phase-order)
                     :leaf-upper-bound (getf plan-summary :leaf-upper-bound)))
          :fluent-pilot
          (and fluent-summary
               (list :action-count (getf fluent-summary :action-count)
                     :final-frontier-size (getf fluent-summary :final-frontier-size)))
          :subset-pilot
          (and subset-summary
               (list :fluent-frontier-size-in (getf subset-summary :fluent-frontier-size-in)
                     :subset-frontier-size-out (getf subset-summary :subset-frontier-size-out)
                     :relation-count (getf subset-summary :relation-count)
                     :pruning-counts (getf subset-summary :pruning-counts)))
          :finalize-pilot
          (and finalize-summary
               (list :subset-frontier-size-in (getf finalize-summary :subset-frontier-size-in)
                     :finalize-frontier-size-out (getf finalize-summary :finalize-frontier-size-out)
                     :prefilter-status (getf finalize-summary :prefilter-status)
                     :prune-reason-counts (getf finalize-summary :prune-reason-counts)
                     :accept-reason-counts (getf finalize-summary :accept-reason-counts)))
          :pilot-bound-check
          (and bound-check
               (list :available-p t
                     :ok-p (getf bound-check :ok-p)
                     :issue-count (length bound-issues)
                     :issue-kinds (remove-duplicates
                                   (mapcar #'first bound-issues)
                                   :test #'eq)))
          :last-result-keys (and last-result (cspir-plist-keys last-result)))))


(defun report-enum-csp-migration-health (&key (stream *standard-output*)
                                              (recompute-bound-check nil))
  "Print and return a one-screen enum-csp migration health snapshot."
  (let* ((snapshot (enum-csp-migration-health-snapshot
                    :recompute-bound-check recompute-bound-check))
         (shadow (getf snapshot :shadow-parity))
         (plan (getf snapshot :next-plan))
         (fluent (getf snapshot :fluent-pilot))
         (subset (getf snapshot :subset-pilot))
         (finalize (getf snapshot :finalize-pilot))
         (bound (getf snapshot :pilot-bound-check))
         (flags (getf snapshot :flags)))
    (when stream
      (format stream "~&[enum-csp-ir] migration health for ~S~%"
              (getf snapshot :spec-name))
      (format stream "  backend=~S invocations=~D~%"
              (getf snapshot :backend)
              (getf snapshot :invocations))
      (format stream "  flags: shadow=~S strict=~S pilot=~S bound-check=~S bound-strict=~S~%"
              (getf flags :shadow-compare-enabled)
              (getf flags :shadow-compare-strict)
              (getf flags :run-fluent-pilot)
              (getf flags :pilot-bound-check-enabled)
              (getf flags :pilot-bound-check-strict))
      (when shadow
        (format stream "  shadow-parity: match=~S diff-count=~D keys=~S~%"
                (getf shadow :match-p)
                (getf shadow :difference-count)
                (getf shadow :difference-keys)))
      (when plan
        (format stream "  plan: relations=~D phases=~S leaf-upper-bound=~D~%"
                (getf plan :relation-plan-count)
                (getf plan :phase-order)
                (getf plan :leaf-upper-bound)))
      (when fluent
        (format stream "  fluent-pilot: actions=~D frontier=~D~%"
                (getf fluent :action-count)
                (getf fluent :final-frontier-size)))
      (when subset
        (format stream "  subset-pilot: in=~D out=~D relations=~D pruning=~S~%"
                (getf subset :fluent-frontier-size-in)
                (getf subset :subset-frontier-size-out)
                (getf subset :relation-count)
                (getf subset :pruning-counts)))
      (when finalize
        (format stream "  finalize-pilot: in=~D out=~D prefilter=~S prune=~S accept=~S~%"
                (getf finalize :subset-frontier-size-in)
                (getf finalize :finalize-frontier-size-out)
                (getf finalize :prefilter-status)
                (getf finalize :prune-reason-counts)
                (getf finalize :accept-reason-counts)))
      (when bound
        (format stream "  bound-check: ok=~S issues=~D kinds=~S~%"
                (getf bound :ok-p)
                (getf bound :issue-count)
                (getf bound :issue-kinds)))
      (format stream "  last-result-keys: ~S~%"
              (getf snapshot :last-result-keys)))
    snapshot))


(defun enum-csp-parity-signature (metadata)
  "Return stable parity signature string derived from METADATA."
  (prin1-to-string
   (list :base-relations (getf metadata :base-relations)
         :constraint-present (getf metadata :constraint-present)
         :hint-keys (getf metadata :hint-keys)
         :shape-valid-p (getf metadata :shape-valid-p))))


(defun cspir-ground-constant-p (x)
  "Return T when X is a ground constant suitable for domain hints."
  (or (numberp x)
      (stringp x)
      (characterp x)
      (and (symbolp x)
           (not (?varp x))
           (not ($varp x))
           (not (keywordp x))
           (not (null x)))))


(defun cspir-collect-positive-literals (form)
  "Collect positive literal forms from FORM.
   Conservative: ignores NOT forms; OR branches are unioned."
  (cond
    ((atom form) nil)
    ((and (consp form) (eq (car form) 'quote)) nil)
    ((and (consp form) (eq (car form) 'not)) nil)
    ((and (consp form) (member (car form) '(and or) :test #'eq))
     (mapcan #'cspir-collect-positive-literals (cdr form)))
    ((and (consp form) (member (car form) '(exists forall) :test #'eq))
     (mapcan #'cspir-collect-positive-literals (cddr form)))
    ((and (consp form) (symbolp (car form)))
     (list form))
    (t
     (nconc (cspir-collect-positive-literals (car form))
            (cspir-collect-positive-literals (cdr form))))))


(defun cspir-collect-unassigned-loc-vars (form)
  "Collect vars appearing in (not (bind (loc ?var $x))) patterns."
  (cond
    ((atom form) nil)
    ((and (consp form)
          (eq (car form) 'not)
          (consp (second form))
          (eq (car (second form)) 'bind)
          (consp (second (second form)))
          (eq (car (second (second form))) 'loc))
     (let ((k (second (second (second form)))))
       (if (and (symbolp k) (?varp k))
           (list k)
           nil)))
    ((consp form)
     (remove-duplicates
      (nconc (cspir-collect-unassigned-loc-vars (car form))
             (cspir-collect-unassigned-loc-vars (cdr form)))
      :test #'eq))
    (t nil)))


(defun cspir-hint-push (table rel key value &key (test #'equal))
  "Push VALUE into REL's KEY list in TABLE if not already present."
  (let ((plist (or (gethash rel table) nil)))
    (unless (member value (getf plist key) :test test)
      (setf (getf plist key) (append (getf plist key) (list value))))
    (setf (gethash rel table) plist)))


(defun cspir-hint-put (table rel key value)
  "Set REL's KEY hint VALUE in TABLE."
  (let ((plist (or (gethash rel table) nil)))
    (setf (getf plist key) value)
    (setf (gethash rel table) plist)))


(defun cspir-subset-area-partner-map (rel literals)
  "Derive area->partners map for subset REL from LITERALS."
  (let ((var->area (make-hash-table :test 'eq))
        (var->partners (make-hash-table :test 'eq))
        (area->partners (make-hash-table :test 'eq)))
    (dolist (lit literals)
      (when (and (consp lit) (eq (car lit) 'loc) (= (length lit) 3))
        (let ((k (second lit))
              (area (third lit)))
          (when (and (symbolp k) (?varp k) (cspir-ground-constant-p area))
            (setf (gethash k var->area) area))))
      (when (and (consp lit) (eq (car lit) rel) (= (length lit) 3))
        (let ((k (second lit))
              (partner (third lit)))
          (when (and (symbolp k) (?varp k) (cspir-ground-constant-p partner))
            (let ((old (gethash k var->partners)))
              (setf (gethash k var->partners)
                    (if (member partner old :test #'eq)
                        old
                        (append old (list partner)))))))))
    (maphash (lambda (k area)
               (let ((partners (gethash k var->partners)))
                 (when partners
                   (let ((old (gethash area area->partners)))
                     (setf (gethash area area->partners)
                           (remove-duplicates (append old partners) :test #'eq))))))
             var->area)
    (when (> (hash-table-count area->partners) 0)
      area->partners)))


(defun cspir-make-area-partner-feasible-fn (area->partners)
  "Build location-aware partner feasibility predicate from AREA->PARTNERS map."
  (when area->partners
    (lambda (state key partner)
      (multiple-value-bind (loc bound-p) (fluent-value state (list 'loc key))
        (if (not bound-p)
            t
            (let ((allowed (gethash loc area->partners)))
              (if allowed
                  (member partner allowed :test #'eq)
                  t)))))))


(defun cspir-distinct-pair-p (lit)
  "True iff LIT is a (different ?x ?y) form over variables."
  (and (consp lit)
       (eq (car lit) 'different)
       (= (length lit) 3)
       (symbolp (second lit))
       (symbolp (third lit))
       (?varp (second lit))
       (?varp (third lit))))


(defun cspir-vars-fully-distinct-p (vars literals)
  "Return T iff every pair in VARS is constrained by DIFFERENT."
  (let ((pairs (make-hash-table :test 'equal)))
    (dolist (lit literals)
      (when (cspir-distinct-pair-p lit)
        (let ((a (second lit))
              (b (third lit)))
          (setf (gethash (if (string< (symbol-name a) (symbol-name b))
                             (list a b)
                             (list b a))
                         pairs)
                t))))
    (loop for i from 0 below (length vars) always
      (loop for j from (1+ i) below (length vars) always
        (let* ((a (nth i vars))
               (b (nth j vars))
               (key (if (string< (symbol-name a) (symbol-name b))
                        (list a b)
                        (list b a))))
          (gethash key pairs))))))


(defun cspir-loc-role-allowed-map (literals)
  "Build role-var -> allowed-locations from positive LOC literals with variable keys."
  (let ((m (make-hash-table :test 'eq)))
    (dolist (lit literals m)
      (when (and (consp lit) (eq (car lit) 'loc) (= (length lit) 3))
        (let ((k (second lit))
              (v (third lit)))
          (when (and (symbolp k) (?varp k) (cspir-ground-constant-p v))
            (let ((old (gethash k m)))
              (setf (gethash k m)
                    (if (member v old :test #'eq)
                        old
                        (append old (list v)))))))))))


(defun cspir-paired-role-vars (literals)
  "Return vars seen as first arg in positive PAIRED literals."
  (let ((out nil))
    (dolist (lit literals (nreverse out))
      (when (and (consp lit) (eq (car lit) 'paired) (= (length lit) 3))
        (let ((k (second lit)))
          (when (and (symbolp k) (?varp k))
            (pushnew k out :test #'eq)))))))


(defun cspir-loc-role-vars (literals)
  "Return vars seen as key arg in positive LOC literals."
  (let ((out nil))
    (dolist (lit literals (nreverse out))
      (when (and (consp lit) (eq (car lit) 'loc) (= (length lit) 3))
        (let ((k (second lit)))
          (when (and (symbolp k) (?varp k))
            (pushnew k out :test #'eq)))))))


(defun cspir-build-loc-count-feasible-fn (literals unassigned-vars)
  "Derive a connector LOC feasibility predicate from role constraints."
  (let* ((loc-vars (cspir-loc-role-vars literals))
         (paired-vars (cspir-paired-role-vars literals))
         (role-vars (if loc-vars loc-vars paired-vars))
         (loc-map (cspir-loc-role-allowed-map literals))
         (vars (remove-if-not (lambda (v) (gethash v loc-map)) role-vars))
         (connectors (or (maybe-type-instances 'connector) nil)))
    (when (and vars
               connectors
               (= (length vars) (length connectors))
               (cspir-vars-fully-distinct-p vars literals))
      (let ((min-counts (make-hash-table :test 'eq))
            (max-counts (make-hash-table :test 'eq)))
        (dolist (v vars)
          (let ((allowed (gethash v loc-map)))
            (when (member v unassigned-vars :test #'eq)
              (setf allowed (if (member :unassigned allowed :test #'eq)
                                allowed
                                (append allowed (list :unassigned)))))
            (when (= (length allowed) 1)
              (let ((a (first allowed)))
                (setf (gethash a min-counts) (1+ (or (gethash a min-counts) 0)))))
            (dolist (a allowed)
              (setf (gethash a max-counts) (1+ (or (gethash a max-counts) 0))))))
        (lambda (state key-args vals)
          (let ((key (first key-args))
                (candidate (first vals)))
            (if (not (member key connectors :test #'eq))
                t
                (let ((counts (make-hash-table :test 'eq))
                      (bound-after 0))
                  (dolist (c connectors)
                    (cond
                      ((eq c key)
                       (unless (eq candidate :unassigned)
                         (incf bound-after)
                         (setf (gethash candidate counts)
                               (1+ (or (gethash candidate counts) 0)))))
                      (t
                       (multiple-value-bind (lv lp) (fluent-value state (list 'loc c))
                         (when lp
                           (incf bound-after)
                           (setf (gethash lv counts)
                                 (1+ (or (gethash lv counts) 0))))))))
                  (let ((remaining (- (length connectors) bound-after)))
                    (and
                     (loop for area being the hash-keys of counts
                           for cnt = (gethash area counts)
                           always (<= cnt (or (gethash area max-counts) 0)))
                     (let ((deficit 0))
                       (maphash (lambda (area mn)
                                  (incf deficit (max 0 (- mn (or (gethash area counts) 0)))))
                                min-counts)
                       (<= deficit remaining))))))))))))


(defun cspir-build-loc-batch-feasible-fn (literals unassigned-vars)
  "Derive batch feasibility predicate for symmetric LOC assignment."
  (let* ((loc-vars (cspir-loc-role-vars literals))
         (paired-vars (cspir-paired-role-vars literals))
         (role-vars (if loc-vars loc-vars paired-vars))
         (loc-map (cspir-loc-role-allowed-map literals))
         (vars (remove-if-not (lambda (v) (gethash v loc-map)) role-vars))
         (connectors (or (maybe-type-instances 'connector) nil)))
    (when (and vars
               connectors
               (= (length vars) (length connectors))
               (cspir-vars-fully-distinct-p vars literals))
      (let ((min-counts (make-hash-table :test 'eq))
            (max-counts (make-hash-table :test 'eq)))
        (dolist (v vars)
          (let ((allowed (copy-list (gethash v loc-map))))
            (when (member v unassigned-vars :test #'eq)
              (unless (member :unassigned allowed :test #'eq)
                (push :unassigned allowed)))
            (when (= (length allowed) 1)
              (let ((a (first allowed)))
                (setf (gethash a min-counts) (1+ (or (gethash a min-counts) 0)))))
            (dolist (a allowed)
              (setf (gethash a max-counts) (1+ (or (gethash a max-counts) 0))))))
        (lambda (state assignment)
          (declare (ignore state))
          (let ((counts (make-hash-table :test 'eq)))
            (dolist (pair assignment)
              (let ((vals (cdr pair)))
                (let ((v (if (consp vals) (first vals) vals)))
                  (setf (gethash v counts) (1+ (or (gethash v counts) 0))))))
            (and
             (loop for area being the hash-keys of counts
                   for cnt = (gethash area counts)
                   always (<= cnt (or (gethash area max-counts) 0)))
             (loop for area being the hash-keys of min-counts
                   for mn = (gethash area min-counts)
                   always (>= (or (gethash area counts) 0) mn)))))))))


(defun derive-enum-csp-domain-hints (constraints)
  "Derive domain hints from CONSTRAINTS using only guaranteed-sound inference.
   Current policy is intentionally strict: no automatic hints are inferred.
   Potentially unsound pruning should be expressed explicitly via
   DEFINE-ENUM-RELATION metadata (for example :PARTNER-FEASIBLE)."
  (declare (ignore constraints))
  (make-hash-table :test 'eq))


(defun apply-enum-csp-ir-to-legacy (&optional (ir *active-enum-csp-ir*))
  "Apply IR to the current legacy ww-enumerator runtime configuration.
   This adapter preserves the current backend while migration proceeds."
  (check-type ir enum-csp-ir)
  (let ((rels (enum-csp-ir-base-relations ir))
        (constraints (enum-csp-ir-constraints ir))
        (prefilter (enum-csp-ir-prefilter ir))
        (domain-hints (enum-csp-ir-domain-hints ir))
        (filter-name (or (enum-csp-ir-constraint-filter-name ir)
                         (and (enum-csp-ir-spec-name ir)
                              (intern (format nil "~A-CONSTRAINTS"
                                              (enum-csp-ir-spec-name ir))
                                      *package*)))))
    (install-base-relations rels)
    (setf *enumerator-domain-hints*
          (or domain-hints (make-hash-table :test 'eq)))
    (when constraints
      (install-base-filter filter-name constraints))
    (cond
      ((null prefilter) nil)
      ((functionp prefilter)
       (install-prefilter (or (enum-csp-ir-spec-name ir) 'goal-enumerator) prefilter))
      ((and (symbolp prefilter) (fboundp prefilter))
       (install-prefilter prefilter (symbol-function prefilter)))
      ((symbolp prefilter)
       (format t "~&[enum-csp-ir] prefilter symbol ~S not fbound yet; leaving unchanged.~%"
               prefilter)))
    (setf *enum-csp-backend-last-result*
          (enum-csp-ir-metadata ir :backend :legacy))
    ir))


(defun apply-enum-csp-ir-to-next-backend (&optional (ir *active-enum-csp-ir*))
  "Next-backend adapter stub: validate IR and return deterministic metadata.
   This does not execute full enumeration."
  (check-type ir enum-csp-ir)
  (let* ((metadata (enum-csp-ir-metadata ir :backend :next-backend-stub))
         (plan (install-enum-csp-next-plan
                (build-enum-csp-next-plan ir)))
         (fluent-pilot-summary nil)
         (subset-pilot-summary nil)
         (finalize-pilot-summary nil))
    (when *enum-csp-next-backend-run-fluent-pilot*
      (multiple-value-setq (fluent-pilot-summary subset-pilot-summary finalize-pilot-summary)
        (cspir-execute-next-backend-phase3-pilot ir)))
    (let* ((result (append metadata
                           (list :next-plan-summary
                                 (enum-csp-next-plan-summary plan)
                                 :fluent-pilot-summary fluent-pilot-summary
                                 :subset-pilot-summary subset-pilot-summary
                                 :finalize-pilot-summary finalize-pilot-summary)))
           (bound-check (cspir-run-next-pilot-bound-check-if-enabled result)))
      (when bound-check
        (setf result (append result (list :next-pilot-bound-check bound-check))))
      (incf *enum-csp-next-backend-invocations*)
      (unless (getf metadata :shape-valid-p)
        (error "[enum-csp-ir] next-backend IR shape invalid: ~S"
               (getf metadata :shape-errors)))
      (format t "~&[enum-csp-ir] next-backend stub accepted ~S with ~D base relations.~%"
              (getf metadata :spec-name)
              (getf metadata :base-relation-count))
      (setf *enum-csp-backend-last-result* result)
      result)))


(defun apply-enum-csp-ir (&optional (ir *active-enum-csp-ir*)
                                    (backend *enum-csp-backend*))
  "Apply IR through selected backend adapter."
  (check-type ir enum-csp-ir)
  (case backend
    (:legacy
     (apply-enum-csp-ir-to-legacy ir))
    (:next-backend
     (apply-enum-csp-ir-to-next-backend ir))
    (otherwise
     (error "Unsupported enum-csp backend ~S. Supported: ~S"
            backend
            (enum-csp-supported-backends)))))


(defun report-enum-csp-backend-comparison (&optional (ir *active-enum-csp-ir*)
                                                     &key (stream *standard-output*))
  "Print and return side-by-side metadata for legacy and next-backend adapters."
  (check-type ir enum-csp-ir)
  (let* ((legacy (enum-csp-ir-metadata ir :backend :legacy))
         (next (enum-csp-ir-metadata ir :backend :next-backend-stub))
         (comparison (list :legacy legacy :next-backend next)))
    (when stream
      (format stream "~&[enum-csp-ir] backend comparison for ~S~%"
              (enum-csp-ir-spec-name ir))
      (format stream "  legacy: rels=~S constraint=~S hints=~S shape-valid=~S~%"
              (getf legacy :base-relations)
              (getf legacy :constraint-present)
              (getf legacy :hint-keys)
              (getf legacy :shape-valid-p))
      (format stream "  next-backend: rels=~S constraint=~S hints=~S shape-valid=~S~%"
              (getf next :base-relations)
              (getf next :constraint-present)
              (getf next :hint-keys)
              (getf next :shape-valid-p)))
    comparison))


(defun enum-csp-backend-parity-differences (legacy next)
  "Return a list of backend parity differences between LEGACY and NEXT metadata."
  (let ((differences nil))
    (unless (equal (getf legacy :base-relations)
                   (getf next :base-relations))
      (push (list :base-relations
                  (getf legacy :base-relations)
                  (getf next :base-relations))
            differences))
    (unless (eql (getf legacy :constraint-present)
                 (getf next :constraint-present))
      (push (list :constraint-present
                  (getf legacy :constraint-present)
                  (getf next :constraint-present))
            differences))
    (unless (equal (getf legacy :hint-keys)
                   (getf next :hint-keys))
      (push (list :hint-keys
                  (getf legacy :hint-keys)
                  (getf next :hint-keys))
            differences))
    (nreverse differences)))


(defun compare-enum-csp-backends (&optional (ir *active-enum-csp-ir*)
                                            &key (stream nil))
  "Return and optionally print backend parity summary for IR."
  (check-type ir enum-csp-ir)
  (let* ((legacy (enum-csp-ir-metadata ir :backend :legacy))
         (next (enum-csp-ir-metadata ir :backend :next-backend-stub))
         (legacy-signature (enum-csp-parity-signature legacy))
         (next-signature (enum-csp-parity-signature next))
         (differences (enum-csp-backend-parity-differences legacy next))
         (summary (list :match-p (null differences)
                        :legacy-signature legacy-signature
                        :next-backend-signature next-signature
                        :differences differences
                        :legacy legacy
                        :next-backend next)))
    (setf *enum-csp-shadow-compare-last* summary)
    (when stream
      (format stream "~&[enum-csp-ir] backend parity for ~S: ~:[MISMATCH~;MATCH~]~%"
              (enum-csp-ir-spec-name ir)
              (null differences))
      (when differences
        (format stream "  differences: ~S~%" differences)))
    summary))


(defun report-enum-csp-backend-mismatches (&optional (ir *active-enum-csp-ir*)
                                                     &key (stream *standard-output*))
  "Print only backend parity mismatches; return mismatch list.
   Emits no output when parity matches."
  (check-type ir enum-csp-ir)
  (let* ((summary (compare-enum-csp-backends ir))
         (differences (getf summary :differences)))
    (when (and stream differences)
      (format stream "~&[enum-csp-ir] backend mismatches for ~S~%"
              (enum-csp-ir-spec-name ir))
      (dolist (d differences)
        (format stream "  ~S~%" d)))
    differences))

;;; Filename: ww-advisor.lisp

;;; Strategy advisor for heuristic-void subgoals.
;;; Runs a named strategy as a sequence of phase goals, each solved in turn,
;;; with the heuristic disabled throughout.  Builds on the same continuation
;;; machinery as solve-subgoal in ww-goal-chaining.lisp.


(in-package :ww)


(defstruct strategy
  "A named capability the planner may use when normal search stalls.
   PARAMETERS is a list of ?-prefixed logical variables occurring in
   APPLICABILITY and PHASES; the matcher grounds them against the
   session model before execution.  APPLICABILITY is a list of tagged
   clauses (type-guard, known-true, known-false, parametric,
   establishable) that both constrain and drive enumeration of
   parameter bindings.  PHASES is an ordered list of goal-forms over
   PARAMETERS.  Each phase is solved in turn via the existing
   continuation mechanism; the solution state of phase i becomes the
   start state of phase i+1.  NOTES is a free-form description shown
   when the user asks to list strategies.  A strategy with empty
   PARAMETERS is a ground strategy and is executed without binding
   enumeration."
  name
  parameters
  applicability
  phases
  notes)


(defvar *strategies* (make-hash-table :test 'eq)
  "Registry of named strategies, keyed by strategy name.")


(defmacro solve-via-strategy (&optional (goal-form nil goal-form-supplied-p))
  "Invoke a registered strategy from the REPL.
   If GOAL-FORM is supplied, it is installed as the overall goal first
   (analogous to solve-subgoal).  If omitted, the currently installed goal
   is kept unchanged.
   The heuristic is disabled for the duration of the strategy's phases
   and restored on completion or abort."
  (if goal-form-supplied-p
    `(run-advisor ',goal-form t)
    `(run-advisor nil nil)))


(defun run-advisor (goal-form goal-form-supplied-p)
  "Top-level entry point for solve-via-strategy.  If GOAL-FORM-SUPPLIED-P,
   install GOAL-FORM as the overall goal and ask the user whether to
   proceed with strategy investigation; cancellation returns nil.
   Otherwise pick a strategy, disable the heuristic, match
   applicability, and attempt each binding's phase sequence under
   unwind-protect until one succeeds or all bindings are exhausted.
   The heuristic is restored on completion or abort."
  (when goal-form-supplied-p
    (install-or-continue-overall-goal goal-form))
  (let ((strategy (select-strategy)))
    (unless strategy
      (format t "~%No strategy selected; aborting.~%")
      (return-from run-advisor nil))
    (format t "~%Running strategy ~A (~D phase~:P)...~%"
            (strategy-name strategy) (length (strategy-phases strategy)))
    (let ((saved-heuristic (and (fboundp 'heuristic?) (symbol-function 'heuristic?)))
          (saved-start-state (copy-problem-state *start-state*))
          (saved-solutions (and (boundp '*solutions*) *solutions*)))
      (when saved-heuristic
        (fmakunbound 'heuristic?)
        (format t "~&Heuristic temporarily disabled for strategic search.~%"))
      (unwind-protect
          (try-strategy-bindings strategy saved-start-state saved-solutions)
        (when saved-heuristic
          (setf (symbol-function 'heuristic?) saved-heuristic)
          (format t "~&Heuristic restored.~%")))
      t)))


(defun install-or-continue-overall-goal (goal-form)
  "Install GOAL-FORM as the overall goal.
   If a prior solution exists, continue from it (mirroring solve-subgoal);
   otherwise install the goal on the current start state."
  (cond ((and (boundp '*solutions*) *solutions*)
         (continue-from-solution goal-form))
        (t
         (terpri)
         (install-goal goal-form)
         (when (boundp 'goal-fn)
           (compile 'goal-fn (subst-int-code (symbol-value 'goal-fn)))))))


(defun try-strategy-bindings (strategy saved-start-state saved-solutions)
  "Enumerate admissible bindings for STRATEGY and attempt each in turn.
   Before each binding's attempt, restore *start-state* and *solutions*
   from the saved pre-advisor snapshots so bindings start from the
   same initial conditions.  Returns the successful binding (projected)
   on success; nil if no binding's phase sequence completes."
  (let ((bindings (match-applicability strategy *start-state*)))
    (cond ((null bindings)
           (format t "~%No admissible bindings for strategy ~A; aborting.~%"
                   (strategy-name strategy))
           nil)
          (t
           (format t "~%Matcher produced ~D binding~:P to try.~%" (length bindings))
           (iter (for raw-binding in bindings)
                 (for attempt from 1)
                 (for projected = (project-binding raw-binding (strategy-parameters strategy)))
                 (format t "~2%--- Binding attempt ~D of ~D: ~S ---~%"
                         attempt (length bindings) projected)
                 (restore-advisor-state saved-start-state saved-solutions)
                 (when (execute-strategy-phases strategy (instantiate-phases strategy projected))
                   (format t "~2%Strategy ~A succeeded with binding ~S.~%"
                           (strategy-name strategy) projected)
                   (return-from try-strategy-bindings projected)))
           (format t "~2%All bindings exhausted; strategy ~A failed.~%"
                   (strategy-name strategy))
           nil))))


(defun restore-advisor-state (saved-start-state saved-solutions)
  "Restore *start-state* and *solutions* from snapshots taken before
   binding enumeration.  Called before each binding attempt so every
   binding starts from the same pre-advisor initial conditions."
  (setf *start-state* (copy-problem-state saved-start-state))
  (setf *solutions* (copy-list saved-solutions)))


(defun execute-strategy-phases (strategy instantiated-phases)
  "Iterate through INSTANTIATED-PHASES, running each as a separate
   solve.  Between phases, the solution's final state becomes the new
   start state via the standard continuation mechanism.  The last
   phase's final state is the strategy's end state, from which normal
   search can resume.  Returns t on success, nil on failure of any
   phase."
  (let ((total (length instantiated-phases)))
    (iter (for phase-goal in instantiated-phases)
          (for phase-number from 1)
          (format t "~2%===== Strategy ~A phase ~D of ~D =====~%"
                  (strategy-name strategy) phase-number total)
          (format t "Phase goal: ~S~%" phase-goal)
          (if (= phase-number 1)
            (progn (terpri)
                   (install-goal phase-goal)
                   (when (boundp 'goal-fn)
                     (compile 'goal-fn (subst-int-code (symbol-value 'goal-fn)))))
            (continue-from-solution phase-goal))
          (ww-solve)
          (unless (and (boundp '*solutions*) *solutions*)
            (format t "~%Strategy aborted: phase ~D produced no solution.~%" phase-number)
            (return-from execute-strategy-phases nil)))
    (format t "~2%===== Strategy ~A completed all ~D phase~:P =====~%"
            (strategy-name strategy) total)
    t))


(defun select-strategy ()
  "Return a strategy from the registry.
   If exactly one is registered, return it.  If more than one, list them
   and prompt the user to pick by number.  If none, return nil."
  (let ((names nil))
    (maphash (lambda (k v) (declare (ignore v)) (push k names)) *strategies*)
    (cond ((null names)
           (format t "~%No strategies are currently registered.~%")
           nil)
          ((= (length names) 1)
           (gethash (first names) *strategies*))
          (t
           (format t "~%Registered strategies:~%")
           (iter (for name in names)
                 (for i from 1)
                 (for strat = (gethash name *strategies*))
                 (format t "  ~D. ~A  -- ~A~%"
                         i name (or (strategy-notes strat) "(no description)")))
           (format t "Select a strategy by number: ")
           (finish-output)
           (let ((choice (read)))
             (cond ((and (integerp choice) (<= 1 choice (length names)))
                    (gethash (nth (1- choice) names) *strategies*))
                   (t
                    (format t "~%Invalid selection.~%")
                    nil)))))))


(defun match-applicability (strategy state)
  "Return the list of admissible bindings for STRATEGY's parameters
   given the facts in STATE.  Each binding is an alist mapping
   parameter symbols to ground values.  Returns nil if no binding
   satisfies all applicability clauses.  If STRATEGY has no
   applicability clauses, returns a singleton list containing the empty
   binding."
  (declare (type problem-state state))
  (let ((bindings (list nil)))
    (iter (for clause in (strategy-applicability strategy))
          (setf bindings (apply-clause clause bindings state))
          (when (null bindings)
            (return-from match-applicability nil)))
    bindings))


(defun apply-clause (clause bindings state)
  "Dispatch CLAUSE by its tag and fold it over BINDINGS, returning the
   new binding list.  A clause has the form (TAG EXPR) where TAG names
   the applicability category and EXPR is the relation, type, or
   predicate expression."
  (case (first clause)
    (type-guard  (apply-type-guard  (second clause) bindings))
    (known-true  (apply-known-true  (second clause) bindings state))
    (known-false (apply-known-false (second clause) bindings state))
    (parametric  (apply-parametric  (second clause) bindings))
    (establishable
     (warn "Establishable clauses are not yet supported; dropping all candidates for ~S." clause)
     nil)
    (t (error "Unknown applicability clause tag ~S in ~S." (first clause) clause))))


(defun apply-type-guard (expr bindings)
  "Expression has form (TYPE-NAME ARG).  If ARG is a ?-variable
   unbound in a binding, branch into one extended binding per object
   in the type's object list.  If ARG is bound (via the binding) or
   ground, keep the binding iff the resolved value is in the object
   list."
  (let* ((type-name (first expr))
         (arg (second expr))
         (objects (gethash type-name *types*))
         (new-bindings nil))
    (iter (for b in bindings)
          (cond ((and (?varp arg) (not (assoc arg b)))
                 (iter (for obj in objects)
                       (push (cons (cons arg obj) b) new-bindings)))
                ((member (resolve-var arg b) objects)
                 (push b new-bindings))))
    (nreverse new-bindings)))


(defun apply-known-true (expr bindings state)
  "Expression has form (REL . PATTERN-ARGS).  Enumerate all facts of
   relation REL (dynamic from STATE, static from *static-db*) and, for
   each current binding, collect the extended bindings produced by
   unifying each fact's arguments against PATTERN-ARGS.  Zero matches
   drops a candidate; multiple matches branch it."
  (let* ((rel (first expr))
         (pattern-args (rest expr))
         (facts (all-facts-of-relation rel state))
         (new-bindings nil))
    (iter (for b in bindings)
          (iter (for fact in facts)
                (multiple-value-bind (extended ok)
                    (unify-args pattern-args (rest fact) b)
                  (when ok
                    (push extended new-bindings)))))
    (nreverse new-bindings)))


(defun apply-known-false (expr bindings state)
  "Expression has form (REL . PATTERN-ARGS).  Drop any candidate
   binding under which PATTERN-ARGS unifies with some fact of relation
   REL.  A candidate survives only when no fact matches the pattern
   under the candidate's current bindings."
  (let* ((rel (first expr))
         (pattern-args (rest expr))
         (facts (all-facts-of-relation rel state))
         (new-bindings nil))
    (iter (for b in bindings)
          (unless (iter (for fact in facts)
                        (thereis (nth-value 1 (unify-args pattern-args (rest fact) b))))
            (push b new-bindings)))
    (nreverse new-bindings)))


(defun apply-parametric (expr bindings)
  "Expression has form (PREDICATE . ARGS).  Substitute ?-variables in
   ARGS using each binding, then call PREDICATE by funcall; keep the
   binding iff the call returns non-nil.  PREDICATE names an ordinary
   Lisp function (eg different, eql, or a user-defined predicate)."
  (let ((predicate (first expr))
        (raw-args (rest expr))
        (new-bindings nil))
    (iter (for b in bindings)
          (when (apply (symbol-function predicate)
                       (mapcar (lambda (a) (resolve-var a b)) raw-args))
            (push b new-bindings)))
    (nreverse new-bindings)))


(defun unify-args (pattern fact binding)
  "Position-wise unify PATTERN against FACT under BINDING.  Returns
   two values: the extended binding and a success flag.  On mismatch
   the flag is nil.  A ?-variable in PATTERN either extends the
   binding or must already agree with its current binding; any other
   item in PATTERN must eql the fact's slot."
  (iter (for p in pattern)
        (for f in fact)
        (cond ((?varp p)
               (let ((existing (assoc p binding)))
                 (cond ((null existing)
                        (setf binding (cons (cons p f) binding)))
                       ((not (eql (cdr existing) f))
                        (return-from unify-args (values nil nil))))))
              ((not (eql p f))
               (return-from unify-args (values nil nil)))))
  (values binding t))


(defun all-facts-of-relation (rel state)
  "Return the list of propositions for relation REL.  Dynamic
   relations are reconstructed from STATE's integer-encoded idb via
   list-database; static relations are read from *static-db* via
   list-static-db.  Errors when REL is not a known relation."
  (declare (type problem-state state))
  (cond ((gethash rel *relations*)
         (iter (for fact in (list-database (problem-state.idb state)))
               (when (eq (first fact) rel)
                 (collect fact))))
        ((gethash rel *static-relations*)
         (iter (for fact in (list-static-db))
               (when (eq (first fact) rel)
                 (collect fact))))
        (t (error "Unknown relation ~S in an applicability clause." rel))))


(defun list-static-db ()
  "Return all facts in *static-db* as a list of full propositions.
   The static db is equal-keyed.  For non-fluent relations the key is
   the full proposition and the value is t.  For fluent-bearing static
   relations the key is the fluentless proposition and the value is
   the list of fluent values, reinserted here at the positions named
   in *fluent-relation-indices*."
  (iter (for (key val) in-hashtable *static-db*)
        (cond ((eql val t)
               (collect (copy-list key)))
              ((listp val)
               (let ((proposition (copy-list key)))
                 (iter (for idx in (get-prop-fluent-indices key))
                       (for v in val)
                       (setf proposition (ut::ninsert-list v idx proposition)))
                 (collect proposition))))))


(defun resolve-var (item binding)
  "If ITEM is a ?-variable bound in BINDING, return the bound value.
   Otherwise return ITEM unchanged.  Ground items pass through."
  (let ((pair (and (?varp item) (assoc item binding))))
    (if pair (cdr pair) item)))


(defun project-binding (binding parameters)
  "Return BINDING restricted to just the ?-variables listed in
   PARAMETERS, preserving their order as declared.  Pairs for any
   variables not in PARAMETERS are dropped; variables in PARAMETERS
   not bound by the matcher are omitted silently (which can happen
   only if a parameter is declared but never constrained, a
   strategy-authoring error that will surface when phase substitution
   leaves the variable unsubstituted)."
  (iter (for p in parameters)
        (for pair = (assoc p binding))
        (when pair (collect pair))))


(defun instantiate-phases (strategy binding)
  "Return STRATEGY's phase goal-forms with BINDING substituted.
   BINDING is a projected alist of (?-var . value) pairs suitable for
   sublis.  A ground strategy (empty binding) returns the phases
   unchanged."
  (sublis binding (strategy-phases strategy)))


(defmacro define-strategy (name &key parameters applicability phases notes)
  "Register a named strategy.  PARAMETERS is a list of ?-prefixed logical
   variables used in PHASES and APPLICABILITY.  APPLICABILITY is a list
   of tagged clauses (type-guard, known-true, known-false, parametric,
   establishable) over PARAMETERS.  PHASES is a list of goal-forms,
   solved in order; variables in phases are substituted at run time
   from the bindings produced by the matcher.  NOTES is an optional
   description string.  A strategy with no PARAMETERS is a ground
   strategy and is executed without binding enumeration."
  `(setf (gethash ',name *strategies*)
         (make-strategy :name ',name
                        :parameters ',parameters
                        :applicability ',applicability
                        :phases ',phases
                        :notes ,notes)))


(defun list-strategies ()
  "Print the names and notes of all currently registered strategies."
  (cond ((zerop (hash-table-count *strategies*))
         (format t "~%No strategies are currently registered.~%"))
        (t
         (format t "~%Registered strategies:~%")
         (maphash (lambda (name strat)
                    (format t "  ~A -- ~A~%"
                            name (or (strategy-notes strat) "(no description)")))
                  *strategies*))))


;;; ============================================================
;;; Strategy definitions
;;; ============================================================


;; Parameterized chain-then-flip-color strategy.  Generalizes the
;; smallspace5 pattern described in advisor/library/talos-strategies.md
;; to any Talos-family problem where:
;;   - two transmitters of different hues exist (?source-a and ?source-b),
;;   - two connectors exist (?connector-a and ?connector-b),
;;   - a blocking gate (?gate) sits between ?head-area and ?goal-area,
;;     opened by a receiver (?receiver-b) of the phase-2 hue,
;;   - a same-hue receiver (?receiver-a) is visible from ?end-area and
;;     provides useful intermediate activation under the phase-1 hue,
;;   - ?head-area has LOS to both transmitters, ?end-area has LOS to
;;     both receivers, and ?end-area is visible to ?head-area.
;;
;; Phase 1: place ?connector-a in ?head-area paired with ?source-a,
;; with ?connector-a taking color ?hue-a.  The color conjunct prevents
;; BFS from establishing a conflicting-hue co-source, because
;; hue-conflict blocks the derived color.  Agent ends in ?head-area.
;;
;; Phase 2: place ?connector-b in ?end-area paired with ?connector-a,
;; ?receiver-a, and ?receiver-b, with ?connector-b also taking color
;; ?hue-a.  ?Connector-a is the phase-1 hue power source; ?connector-b
;; acts as the chain end-connector, lighting ?receiver-a and arming
;; ?receiver-b per the connector-color-switch pattern.  Agent ends in
;; ?end-area.
;;
;; Phase 3: flip the chain source by repairing ?connector-a with
;; ?source-b and ?connector-b, forcing both connectors to ?hue-b via
;; the color conjuncts.  ?Receiver-b activates and ?gate opens (both
;; derived), and the agent moves into ?goal-area.
;;
;; Notes on the current applicability:
;;   - The (chroma ?source-X ?hue-X) clauses effectively narrow
;;     ?source-a and ?source-b to transmitters, since chroma is
;;     asserted only on fixtures (connectors carry color, not
;;     chroma).  Generalizing to powered-connector sources would
;;     require an additional color-or-chroma applicability pattern.
;;   - LOS clauses use los0 only (unconditional).  los1-mediated LOS
;;     (open gate required) is a future extension.
;;   - The matcher produces multiple admissible bindings in
;;     problems with multiple gate-controlled paths.  The advisor's
;;     enumerate-and-try loop walks them in order until one succeeds.

(define-strategy chain-then-flip-color
  :parameters (?agent ?connector-a ?connector-b ?source-a ?source-b
               ?hue-a ?hue-b ?receiver-a ?receiver-b
               ?gate ?head-area ?end-area ?goal-area)
  :applicability
    ((type-guard (agent ?agent))
     (type-guard (connector ?connector-a))
     (type-guard (connector ?connector-b))
     (type-guard (source ?source-a))
     (type-guard (source ?source-b))
     (type-guard (receiver ?receiver-a))
     (type-guard (receiver ?receiver-b))
     (known-true (chroma ?source-a ?hue-a))
     (known-true (chroma ?source-b ?hue-b))
     (known-true (chroma ?receiver-a ?hue-a))
     (known-true (chroma ?receiver-b ?hue-b))
     (known-true (controls ?receiver-b ?gate))
     (known-true (accessible1 ?head-area ?gate ?goal-area))
     (known-true (loc ?connector-a ?head-area))
     (known-true (loc ?connector-b ?end-area))
     (known-true (los0 ?head-area ?source-a))
     (known-true (los0 ?head-area ?source-b))
     (known-true (los0 ?end-area ?receiver-a))
     (known-true (los0 ?end-area ?receiver-b))
     (known-true (visible0 ?end-area ?head-area))
     (parametric (different ?connector-a ?connector-b))
     (parametric (different ?source-a ?source-b))
     (parametric (different ?hue-a ?hue-b))
     (parametric (different ?receiver-a ?receiver-b))
     (parametric (different ?head-area ?end-area))
     (parametric (different ?head-area ?goal-area))
     (parametric (different ?end-area ?goal-area)))
  :phases ((and (loc ?connector-a ?head-area)
                (paired ?connector-a ?source-a)
                (color ?connector-a ?hue-a)
                (loc ?agent ?head-area))
           (and (loc ?connector-b ?end-area)
                (paired ?connector-b ?connector-a)
                (paired ?connector-b ?receiver-a)
                (paired ?connector-b ?receiver-b)
                (color ?connector-b ?hue-a)
                (loc ?agent ?end-area))
           (and (paired ?connector-a ?source-b)
                (paired ?connector-a ?connector-b)
                (color ?connector-a ?hue-b)
                (color ?connector-b ?hue-b)
                (loc ?agent ?goal-area)))
  :notes "Build a chain under one hue via ?connector-a and ?connector-b lighting ?receiver-a; then re-source ?connector-a from ?source-b to flip the chain's hue and activate ?receiver-b, opening ?gate onto ?goal-area.")

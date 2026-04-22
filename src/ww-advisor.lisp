;;; Filename: ww-advisor.lisp

;;; Strategy advisor for heuristic-void subgoals.
;;; Runs a named strategy as a sequence of phase goals, each solved in turn,
;;; with the heuristic disabled throughout.  Builds on the same continuation
;;; machinery as ww-continue in ww-goal-chaining.lisp.


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


(defmacro ww-advise (&optional (goal-form nil goal-form-supplied-p))
  "Invoke a registered strategy from the REPL.
   If GOAL-FORM is supplied, it is installed as the overall goal first
   (analogous to ww-continue).  If omitted, the currently installed goal
   is kept unchanged.
   The heuristic is disabled for the duration of the strategy's phases
   and restored on completion or abort."
  (if goal-form-supplied-p
    `(run-advisor ',goal-form t)
    `(run-advisor nil nil)))


(defun run-advisor (goal-form goal-form-supplied-p)
  "Top-level entry point for ww-advise.  Picks a strategy, optionally
   installs an overall goal, disables the heuristic, and runs the
   strategy's phases end-to-end under unwind-protect."
  (when goal-form-supplied-p
    (install-or-continue-overall-goal goal-form))
  (let ((strategy (select-strategy)))
    (unless strategy
      (format t "~%No strategy selected; aborting.~%")
      (return-from run-advisor nil))
    (format t "~%Running strategy ~A (~D phase~:P)...~%"
            (strategy-name strategy) (length (strategy-phases strategy)))
    (let ((saved-heuristic (and (fboundp 'heuristic?) (symbol-function 'heuristic?))))
      (when saved-heuristic
        (fmakunbound 'heuristic?)
        (format t "~&Heuristic temporarily disabled for strategic search.~%"))
      (unwind-protect
          (execute-strategy-phases strategy)
        (when saved-heuristic
          (setf (symbol-function 'heuristic?) saved-heuristic)
          (format t "~&Heuristic restored.~%"))))))


(defun install-or-continue-overall-goal (goal-form)
  "Install GOAL-FORM as the overall goal.
   If a prior solution exists, continue from it (mirroring ww-continue);
   otherwise install the goal on the current start state."
  (cond ((and (boundp '*solutions*) *solutions*)
         (continue-from-solution goal-form))
        (t
         (terpri)
         (install-goal goal-form)
         (when (boundp 'goal-fn)
           (compile 'goal-fn (subst-int-code (symbol-value 'goal-fn)))))))


(defun execute-strategy-phases (strategy)
  "Iterate through STRATEGY's phases, running each as a separate solve.
   Between phases, the solution's final state becomes the new start state
   via the standard continuation mechanism.  The last phase's final state
   is the strategy's end state, from which normal search can resume."
  (let ((total (length (strategy-phases strategy))))
    (iter (for phase-goal in (strategy-phases strategy))
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


;; First draft strategy, specific to problem-smallspace5.lisp.
;; Implements the chain-build-then-flip pattern described in
;; advisor/library/talos-strategies.md for the goal of moving agent1
;; into area3 (which requires opening gate2).
;;
;; Phase 1: place connector2 in area2 paired with transmitter1, with
;; connector2 colored blue.  The color conjunct prevents BFS from
;; establishing a conflicting-hue co-source (e.g. also pairing with
;; transmitter2), because hue-conflict blocks the derived color.
;; Agent ends in area2.
;;
;; Phase 2: place connector1 in area1 paired with connector2,
;; receiver1, and receiver2, with connector1 colored blue.  Connector2
;; is the blue power source; connector1 acts as the chain end-connector,
;; lighting receiver1 and arming receiver2 per the connector-color-switch
;; pattern.  The color conjunct again rules out conflicting co-sources.
;; Agent ends in area1.
;;
;; Phase 3: flip the chain source by repairing connector2 with
;; transmitter2 and connector1, forcing both connectors to red via
;; the color conjuncts.  Receiver2 activates and gate2 opens (both
;; derived), and the agent moves into area3.  Agent ends in area3.

(define-strategy chain-build-then-flip-to-reach-area3
  :notes "Smallspace5 to reach area3: build blue chain via connector2 (area2) and connector1 (area1), then flip connector2 source to transmitter2 to re-hue red, opening gate2."
  :phases ((and (loc connector2 area2)
                (paired connector2 transmitter1)
                (color connector2 blue)
                (loc agent1 area2))
           (and (loc connector1 area1)
                (paired connector1 connector2)
                (paired connector1 receiver1)
                (paired connector1 receiver2)
                (color connector1 blue)
                (loc agent1 area1))
           (and (paired connector2 transmitter2)
                (paired connector2 connector1)
                (color connector2 red)
                (color connector1 red)
                (loc agent1 area3))))

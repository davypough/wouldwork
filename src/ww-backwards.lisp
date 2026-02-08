;;; Filename: ww-backwards.lisp
;;;
;;; Experimental backward-regression helpers.
;;;
;;; Goal: Provide a *testable* "one step backwards" mechanism:
;;;   Given a target state S' and an action instance A, generate candidate
;;;   predecessor states S such that applying A in S can reproduce S' (up to
;;;   a configurable projection / normalization).
;;;
;;; Scope (development / problem-corner):
;;;   - MOVE
;;;   - PICKUP-CONNECTOR
;;;   - CONNECT-TO-{1,2,3}-TERMINUS
;;;
;;; Notes:
;;;   * We avoid comparing BEAM identities (BEAM1/BEAM2/...) by offering a
;;;     normalizer that strips beam-segment/current-beams (and other derived
;;;     clutter) and then recomputes derived consequences via a user-supplied
;;;     normalizer (default: PROPAGATE-CHANGES! when present).
;;;   * Not integrated with the planner/searcher yet; meant for REPL-driven
;;;     regression/validation experiments.
;;;
;;; Suggested ASDF integration (in wouldwork.asd):
;;;   add (:file "ww-backwards") after (:file "ww-solution-validation")


(in-package :ww)


;;;; ==================== Configuration ====================


(defparameter *bw-normalize-strip-relations*
  ;; Relations to drop before recomputing derived consequences.
  ;; For problem-corner, these eliminate history-dependent beam naming.
  '(beam-segment current-beams active open color)
  "Relations removed by BW-NORMALIZE! prior to recomputing derived consequences.")


(defparameter *bw-default-normalizer*
  ;; A SYMBOL naming a function of one arg (STATE) that recomputes derived closure.
  ;; For problem-corner, PROPAGATE-CHANGES! is the canonical choice.
  'propagate-changes!
  "Default normalizer function symbol used by BW-NORMALIZE!.")


(defparameter *bw-default-projection-relations*
  ;; Relations included in BW-PROJECT / BW-EQUIVALENT-P by default.
  ;; You can tune this per domain.
  '(loc elevation on holds paired supports active open color)
  "Relations included in BW-PROJECT / BW-EQUIVALENT-P by default.")


(defparameter *bw-base-fact-relations*
  ;; Base facts only (decision-layer), excluding derived facts (active, open, color).
  ;; Used for footprint-aware matching where derived facts may differ due to
  ;; intentionally omitted base facts in the regressed predecessor.
  '(loc elevation on holds paired supports)
  "Base-fact relations for footprint-aware predecessor matching.")


;;;; ==================== Small state utilities ====================


(defun bw--all-props (state)
  "Return a readable list of propositions in STATE."
  (list-database (problem-state.idb state)))


(defun bw--delete-props-by-relation! (state relation)
  "Delete *all* propositions whose car is RELATION."
  (let ((idb (problem-state.idb state)))
    (dolist (prop (bw--all-props state))
      (when (eql (car prop) relation)
        (delete-proposition prop idb))))
  (setf (problem-state.idb-hash state) nil)
  state)


(defun bw--delete-props-matching! (state predicate)
  "Delete all propositions for which (PREDICATE prop) is true."
  (let ((idb (problem-state.idb state)))
    (dolist (prop (bw--all-props state))
      (when (funcall predicate prop)
        (delete-proposition prop idb))))
  (setf (problem-state.idb-hash state) nil)
  state)


(defun bw--set-current-beams-empty! (state)
  "Ensure (CURRENT-BEAMS ()) exists, and no other CURRENT-BEAMS proposition exists."
  (bw--delete-props-by-relation! state 'current-beams)
  (add-proposition '(current-beams ()) (problem-state.idb state))
  (setf (problem-state.idb-hash state) nil)
  state)


(defun bw--get-binary-fluent (state relation subject)
  "Return the 3rd element of (RELATION SUBJECT <value>) if present, else NIL."
  (let ((prop (find-if (lambda (p)
                         (and (eql (car p) relation)
                              (eql (second p) subject)))
                       (bw--all-props state))))
    (when prop (third prop))))


(defun bw--maybe-propagate-changes! (state)
  "Invoke problem-specific propagation when available."
  (when (fboundp 'propagate-changes!)
    (funcall (symbol-function 'propagate-changes!) state)))


;;;; ==================== Building / projecting states ====================


(defun bw-make-state-from-propositions (propositions &key
                                                     (relations nil)
                                                     (time 0.0)
                                                     (value 0.0))
  "Build a fresh PROBLEM-STATE from a list of readable PROPOSITIONS.

If RELATIONS is :ALL or NIL, keep all propositions.
Otherwise keep only propositions whose relation is in RELATIONS.

Intended for REPL experiments (e.g., pasting a validator printout)."
  (let ((st (make-problem-state :time time
                               :value value
                               :idb (make-hash-table :test #'eql :synchronized (> *threads* 0))
                               :hidb (make-hash-table :test #'eql :synchronized (> *threads* 0)))))
    (dolist (p propositions)
      (when (and (consp p)
                 (or (null relations)
                     (eql relations :all)
                     (member (car p) relations)))
        (add-proposition p (problem-state.idb st))))
    (setf (problem-state.idb-hash st) nil)
    st))


(defun bw-project (state &key
                         (relations *bw-default-projection-relations*)
                         ignore-predicate)
  "Return a canonicalized set (as a sorted list) of propositions from STATE,
restricted to RELATIONS.

If IGNORE-PREDICATE is non-nil, any proposition for which it returns true is
excluded from the projection (useful for footprint-aware comparisons)."
  (let* ((props (remove-if-not (lambda (p) (member (car p) relations))
                               (bw--all-props state)))
         (props (if ignore-predicate
                    (remove-if ignore-predicate props)
                    props))
         ;; Canonicalize paired symmetry by ordering its two args.
         (canon (mapcar (lambda (p)
                          (if (and (eql (car p) 'paired)
                                   (symbolp (second p))
                                   (symbolp (third p)))
                              (let ((a (second p)) (b (third p)))
                                (if (string< (symbol-name a) (symbol-name b))
                                    p
                                    (list 'paired b a)))
                              p))
                        props)))
    (sort (remove-duplicates canon :test #'equal)
          (lambda (x y)
            (string< (prin1-to-string x) (prin1-to-string y))))))


(defun bw-equivalent-p (state-a state-b &key
                                (relations *bw-default-projection-relations*)
                                ignore-predicate)
  "True iff STATE-A and STATE-B match on BW-PROJECT with RELATIONS.

If IGNORE-PREDICATE is supplied, it is applied to both states' projections."
  (equal (bw-project state-a :relations relations :ignore-predicate ignore-predicate)
         (bw-project state-b :relations relations :ignore-predicate ignore-predicate)))


(defun bw-milestone-props (state &key
                                 (relations *bw-base-fact-relations*)
                                 ignore-predicate)
  "Return a canonicalized, sorted list of milestone propositions derived from STATE.

This is a thin wrapper around BW-PROJECT, defaulting to *BW-BASE-FACT-RELATIONS*.
Use IGNORE-PREDICATE to filter out propositions (PROP -> boolean) before projection."
  (bw-project state :relations relations :ignore-predicate ignore-predicate))


(defun bw-milestone-goal-form (state &key
                                     (relations *bw-base-fact-relations*)
                                     ignore-predicate
                                     (operator 'and))
  "Return a goal form suitable for INSTALL-GOAL / COERCE-GOAL, derived from STATE.

The returned form is (OPERATOR <milestone-prop>...).

Important: Wouldwork's translator rejects empty connectives like (AND) or (OR).
If projection yields no milestone propositions, this returns (AND T) for OPERATOR=AND,
(OR NIL) for OPERATOR=OR, and (OPERATOR T) otherwise."
  (let ((props (bw-milestone-props state
                                   :relations relations
                                   :ignore-predicate ignore-predicate)))
    (cond
      (props (cons operator props))
      ((eql operator 'and) (list 'and t))
      ((eql operator 'or)  (list 'or nil))
      (t (list operator t)))))


;;;; ==================== Normalization ====================


(defun bw-normalize! (state &key
                            (strip-relations *bw-normalize-strip-relations*)
                            (normalizer *bw-default-normalizer*))
  "Strip history-sensitive derived relations, reset CURRENT-BEAMS, and
recompute derived closure by calling NORMALIZER (a symbol naming a function).

Returns STATE (mutated)."
  (dolist (rel strip-relations)
    (bw--delete-props-by-relation! state rel))
  ;; problem-corner expects CURRENT-BEAMS to exist.
  (bw--set-current-beams-empty! state)
  ;; Recompute closure, if available.
  (when (and normalizer (symbolp normalizer) (fboundp normalizer))
    (let (forward-list inverse-list)
      (declare (special forward-list inverse-list))
      (funcall (symbol-function normalizer) state)))
  (setf (problem-state.idb-hash state) nil)
  state)


;;;; ==================== One-step regression ====================


(defun bw-regress (target-state action-form)
  "Dispatch to action-family regressors.
Returns a list of predecessor candidate states (each is a PROBLEM-STATE)."
  (check-type action-form cons)
  (let ((op (first action-form)))
    (case op
      (move
       (destructuring-bind (move agent from to) action-form
         (declare (ignore move))
         (bw-regress-move target-state agent from to)))
      (pickup-connector
       (destructuring-bind (pickup agent connector area) action-form
         (declare (ignore pickup))
         (bw-regress-pickup-connector target-state agent connector area)))
      (connect-to-1-terminus (bw-regress-connect-to-n-terminus target-state action-form 1))
      (connect-to-2-terminus (bw-regress-connect-to-n-terminus target-state action-form 2))
      (connect-to-3-terminus (bw-regress-connect-to-n-terminus target-state action-form 3))
      (otherwise nil))))


(defun bw-regress-move (target-state agent from to)
  "Regress a MOVE step.
Action instance: (MOVE agent from to)."
  (declare (ignore to))
  (let ((pred (copy-problem-state target-state))
        (idb nil))
    (setf idb (problem-state.idb pred))
    ;; Ensure agent was at FROM before the move.
    (bw--delete-props-matching!
     pred (lambda (p) (and (eql (car p) 'loc)
                           (eql (second p) agent))))
    (add-proposition (list 'loc agent from) idb)
    (setf (problem-state.idb-hash pred) nil)
    (list pred)))


(defun bw-regress-pickup-connector (target-state agent connector area)
  "Regress a PICKUP-CONNECTOR step.
Action instance: (PICKUP-CONNECTOR agent connector area).

Because pickup deletes (LOC connector area) and (ELEVATION connector e), but the
target state no longer contains ELEVATION for the connector, we generate a small
set of candidates for the connector's prior elevation (typically {e-agent-1,
 e-agent, e-agent+1} clipped at 0)."
  (let* ((e-agent (or (bw--get-binary-fluent target-state 'elevation agent) 0))
         (candidates (remove-duplicates
                      (remove-if (lambda (x) (and (numberp x) (< x 0)))
                                 (list (- e-agent 1) e-agent (+ e-agent 1)))
                      :test #'equal)))
    (loop for e-conn in candidates
          collect
          (let* ((pred (copy-problem-state target-state))
                 (idb (problem-state.idb pred)))
            ;; Agent was not holding the connector before pickup.
            (bw--delete-props-matching!
             pred (lambda (p) (and (eql (car p) 'holds)
                                   (eql (second p) agent))))
            ;; Connector existed in AREA before pickup.
            (bw--delete-props-matching!
             pred (lambda (p) (and (eql (car p) 'loc)
                                   (eql (second p) connector))))
            (add-proposition (list 'loc connector area) idb)
            ;; Connector had some elevation before pickup.
            (bw--delete-props-matching!
             pred (lambda (p) (and (eql (car p) 'elevation)
                                   (eql (second p) connector))))
            (add-proposition (list 'elevation connector e-conn) idb)
            ;; NOTE: we intentionally do not reconstruct (PAIRED ...) facts involving
            ;; CONNECTOR here; see BW-IGNORE-PREDICATE-FOR-ACTION and BW-DIFF.
            (setf (problem-state.idb-hash pred) nil)
            pred))))


(defun bw-regress-connect-to-n-terminus (target-state action-form n)
  "Regress CONNECT-TO-N-TERMINUS (N in {1,2,3}).

We treat this family as: 'place a held connector and create pairings'.

Predecessor sketch:
  - (HOLDS agent cargo)
  - cargo has no LOC/ELEVATION/ON/SUPPORTS in predecessor
  - all (PAIRED cargo x) and (PAIRED x cargo) removed in predecessor
Other facts are preserved from TARGET-STATE."
  (declare (type fixnum n))
  (let* ((op (first action-form))
         (expected-len (+ 5 n))
         (len (length action-form)))
    (unless (= len expected-len)
      (warn "BW-REGRESS: ~A expects ~D args, got ~D: ~S"
            op (1- expected-len) (1- len) action-form)
      (return-from bw-regress-connect-to-n-terminus nil))
    (let* ((agent (second action-form))
           (cargo (third action-form))
           (pred (copy-problem-state target-state))
           (idb (problem-state.idb pred)))
      ;; Agent must have been holding cargo before connect.
      (bw--delete-props-matching!
       pred (lambda (p) (and (eql (car p) 'holds)
                             (eql (second p) agent))))
      (add-proposition (list 'holds agent cargo) idb)
      ;; Cargo is no longer placed (remove loc/elevation/on/supports as applicable).
      (bw--delete-props-matching!
       pred (lambda (p) (and (member (car p) '(loc elevation on supports))
                             (eql (second p) cargo))))
      ;; Remove any pairings involving cargo (both directions).
      (bw--delete-props-matching!
       pred (lambda (p)
              (and (eql (car p) 'paired)
                   (or (eql (second p) cargo)
                       (eql (third p) cargo)))))
      (setf (problem-state.idb-hash pred) nil)
      (list pred))))


;;;; ==================== Regression + forward validation ====================


(defun bw-regress+validate-subsuming (target-state action-form &key
                                                  (normalizer *bw-default-normalizer*)
                                                  (strip-relations *bw-normalize-strip-relations*)
                                                  (projection-relations *bw-default-projection-relations*)
                                                  (required-props nil)
                                                  (max-results nil)
                                                  (verbose t))
  "Like BW-REGRESS+VALIDATE, but accepts NEXT states that *subsumes* REQUIRED-PROPS
(on PROJECTION-RELATIONS after normalization), rather than requiring equivalence.

If REQUIRED-PROPS is NIL, uses (BW-GOAL-REQUIRED-PROPS normalized-target)."
  (let ((target (copy-problem-state target-state))
        (ignore-pred (bw-ignore-predicate-for-action action-form)))
    ;; Normalize target for canonical comparison
    (bw-normalize! target :strip-relations strip-relations :normalizer normalizer)

    ;; Compute required footprint (canonicalized via BW-PROJECT)
    (let* ((req-props (or required-props (bw-goal-required-props target)))
           (req-state (bw-make-state-from-propositions req-props :relations :all))
           (required (bw-project req-state
                                 :relations projection-relations
                                 :ignore-predicate ignore-pred)))
      (loop with results = nil
            for pred0 in (bw-regress target action-form)
            for pred = (copy-problem-state pred0) do
              (bw-normalize! pred :strip-relations strip-relations :normalizer normalizer)
              (multiple-value-bind (next-state ok failure-reason)
                  (apply-action-to-state action-form pred nil nil)
                (cond
                  ((not ok)
                   (when verbose
                     (format t "~&BW: candidate rejected (precondition failed): ~S~%  ~A~%"
                             action-form failure-reason)))
                  (t
                   (let* ((next (copy-problem-state next-state)))
                     (bw-normalize! next :strip-relations strip-relations :normalizer normalizer)
                     (let ((next-proj (bw-project next
                                                  :relations projection-relations
                                                  :ignore-predicate ignore-pred)))
                       (when (subsetp required next-proj :test #'equal)
                         (push pred results)
                         (when (and max-results (>= (length results) max-results))
                           (return (nreverse results)))))))))
            finally (return (nreverse results))))))


(defun bw-regress+validate (target-state action-form &key
                                         (normalizer *bw-default-normalizer*)
                                         (strip-relations *bw-normalize-strip-relations*)
                                         (projection-relations *bw-default-projection-relations*)
                                         (max-results nil)
                                         (verbose t))
  "Generate predecessor candidates by BW-REGRESS, normalize them, and keep those
whose forward application of ACTION-FORM reproduces TARGET-STATE (up to
BW-EQUIVALENT-P on PROJECTION-RELATIONS after normalization).

Uses footprint-aware comparison to ignore PAIRED facts for the cargo involved
in PICKUP/CONNECT actions, since these facts are destroyed/created by those actions.

Returns a list of validated predecessor states." 
  (let ((target (copy-problem-state target-state))
        (ignore-pred (bw-ignore-predicate-for-action action-form)))
    ;; Normalize target for canonical comparison
    (bw-normalize! target :strip-relations strip-relations :normalizer normalizer)
    (loop with results = nil
          for pred0 in (bw-regress target action-form)
          for pred = (copy-problem-state pred0) do
            (bw-normalize! pred :strip-relations strip-relations :normalizer normalizer)
            (multiple-value-bind (next-state ok failure-reason)
                (apply-action-to-state action-form pred nil nil)
              (cond
                ((not ok)
                 (when verbose
                   (format t "~&BW: candidate rejected (precondition failed): ~S~%  ~A~%"
                           action-form failure-reason)))
                (t
                 (let ((next (copy-problem-state next-state)))
                   (bw-normalize! next :strip-relations strip-relations :normalizer normalizer)
                   (when (bw-equivalent-p next target 
                                          :relations projection-relations
                                          :ignore-predicate ignore-pred)
                     (push pred results)
                     (when (and max-results (>= (length results) max-results))
                       (return (nreverse results))))))))
          finally (return (nreverse results)))))


(defun bw-regressed-predecessor-report (candidate-goal-states candidate-actions &key
                                                             (validator #'bw-regress+validate) ; CHANGED
                                                             (normalizer *bw-default-normalizer*)
                                                             (strip-relations *bw-normalize-strip-relations*)
                                                             (projection-relations *bw-default-projection-relations*)
                                                             (milestone-relations *bw-base-fact-relations*)
                                                             (milestone-format :props)
                                                             (max-results nil)
                                                             (verbose nil))
  "Phase-3 driver: regress+validate each (goal, action) for one or more candidate goal states.

Returns a list of per-goal report plists:
  (:GOAL-INDEX i :GOAL-STATE <state> :ACTION-REPORTS (<plist> ...))"
  (let ((goal-states (bw-coerce-goal-states candidate-goal-states)))
    (loop for goal-state in goal-states
          for goal-index from 0
          collect
          (let ((action-reports
                  (loop for action in candidate-actions
                        collect
                        (let* ((validated
                                 (funcall validator goal-state action             ; CHANGED
                                          :normalizer normalizer
                                          :strip-relations strip-relations
                                          :projection-relations projection-relations
                                          :max-results max-results
                                          :verbose verbose))
                               (milestones
                                 (ecase milestone-format
                                   (:props
                                    (mapcar (lambda (pred)
                                              (bw-milestone-props pred
                                                                  :relations milestone-relations))
                                            validated))
                                   (:goal-form
                                    (mapcar (lambda (pred)
                                              (bw-milestone-goal-form pred
                                                                      :relations milestone-relations))
                                            validated)))))
                          (list :action action
                                :validated-predecessors validated
                                :milestones milestones
                                :validated-count (length validated))))))
            (list :goal-index goal-index
                  :goal-state goal-state
                  :action-reports action-reports)))))


(defun bw-regressed-predecessor-report/enumerated (candidate-actions &rest keys
                                                   &key &allow-other-keys)
  "Convenience wrapper: runs BW-REGRESSED-PREDECESSOR-REPORT using *ENUMERATED-GOAL-STATES*."
  (unless (and (boundp '*enumerated-goal-states*) *enumerated-goal-states*)
    (warn "BW-REGRESSED-PREDECESSOR-REPORT/ENUMERATED: *ENUMERATED-GOAL-STATES* is empty or unbound."))
  (apply #'bw-regressed-predecessor-report
         (append (list *enumerated-goal-states* candidate-actions) keys)))


(defun bw-coerce-goal-states (candidate-goal-states)
  "Coerce CANDIDATE-GOAL-STATES into a list of PROBLEM-STATE objects.

Accepted inputs:
  - NIL                               => NIL (no goal states)
  - a PROBLEM-STATE                   => (list <that state>)
  - a list of PROBLEM-STATEs          => that list
  - a proposition list                => (list (bw-make-state-from-propositions ...))
  - a list of proposition lists       => (mapcar bw-make-state-from-propositions ...)
  - a report plist with :GOAL-STATES  => coerces (GETF <report> :GOAL-STATES)  ;; NEW

Note: NIL by itself is treated as \"no goal states\". To represent a single empty
proposition list as a goal-state, pass (list nil)."
  (cond
    ;; NIL means: no goal states (important for enumerator bridges)
    ((null candidate-goal-states)
     nil)

    ;; NEW: Allow passing a FIND-GOAL-STATES report plist directly.
    ;; We detect this by the presence of the :GOAL-STATES key.
    ((and (consp candidate-goal-states)
          (keywordp (first candidate-goal-states))
          (member :goal-states candidate-goal-states))                 ;; NEW
     (bw-coerce-goal-states (getf candidate-goal-states :goal-states))) ;; NEW

    ;; Single PROBLEM-STATE
    ((typep candidate-goal-states 'problem-state)
     (list candidate-goal-states))

    ;; List of PROBLEM-STATEs
    ((and (listp candidate-goal-states)
          (every (lambda (x) (typep x 'problem-state)) candidate-goal-states))
     candidate-goal-states)

    ;; Single proposition list (must be non-empty and start with a proposition)
    ((and (consp candidate-goal-states)
          (consp (first candidate-goal-states))
          (symbolp (caar candidate-goal-states)))
     (list (bw-make-state-from-propositions candidate-goal-states)))

    ;; List of proposition lists (each element is a prop-list; prop-list may be NIL)
    ((and (listp candidate-goal-states)
          (every (lambda (pl)
                   (and (listp pl)
                        (every (lambda (p)
                                 (and (consp p) (symbolp (car p))))
                               pl)))
                 candidate-goal-states))
     (mapcar #'bw-make-state-from-propositions candidate-goal-states))

    (t
     (error "BW-COERCE-GOAL-STATES: unsupported goal-states input: ~S"
            candidate-goal-states))))


(defun bw-milestone->props (milestone &key (format :auto))
  "Normalize MILESTONE into a proposition list suitable for hashing/comparison.

FORMAT:
  :props     => MILESTONE is already a proposition list
  :goal-form => MILESTONE is (OP <prop>...) (e.g., (AND ...))
  :auto      => try to detect (OP ...) vs props list

Special cases:
  (AND T) => NIL (empty milestone)
  (OR NIL) => NIL (empty milestone)"
  (cond
    ((null milestone) nil)

    ((eq format :props)
     milestone)

    ((or (eq format :goal-form)
         (and (eq format :auto)
              (consp milestone)
              (symbolp (first milestone))
              ;; goal-form is (OP <...>); props-list begins with a proposition like (LOC ...)
              (not (and (consp (first (rest milestone)))
                        (symbolp (car (first (rest milestone))))))))
     (let ((op (first milestone))
           (args (rest milestone)))
       (cond
         ((and (eql op 'and) (equal args (list t))) nil)
         ((and (eql op 'or)  (equal args (list nil))) nil)
         (t args))))

    (t
     ;; Assume it is already a prop list
     milestone)))


(defun bw-rank-milestones (report &key
                                  (milestone-format :auto)
                                  (max-examples 3)
                                  (include-empty nil))
  "Return ranked milestone summaries derived from REPORT.

Each summary is a plist:
  (:MILESTONE-PROPS <props> :COUNT n :CLAUSES k :EXAMPLES (<plist> ...))

Ranking: higher COUNT first; ties broken by fewer CLAUSES."
  (let ((ht (make-hash-table :test #'equal)))
    ;; Accumulate
    (loop for goal-report in report do
          (let ((goal-index (getf goal-report :goal-index))
                (action-reports (getf goal-report :action-reports)))
            (loop for ar in action-reports do
                  (let ((action (getf ar :action))
                        (milestones (getf ar :milestones)))
                    (loop for m in milestones
                          for pred-index from 0 do
                          (let* ((props (bw-milestone->props m :format milestone-format)))
                            (when (or include-empty props)
                              (let ((entry (gethash props ht)))
                                (if entry
                                    (progn
                                      (setf (getf entry :count) (1+ (getf entry :count)))
                                      (when (< (length props) (getf entry :clauses))
                                        (setf (getf entry :clauses) (length props)))
                                      (let ((ex (getf entry :examples)))
                                        (when (< (length ex) max-examples)
                                          (setf (getf entry :examples)
                                                (append ex (list (list :goal-index goal-index
                                                                       :action action
                                                                       :predecessor-index pred-index)))))))
                                  (setf (gethash props ht)
                                        (list :milestone-props props
                                              :count 1
                                              :clauses (length props)
                                              :examples (list (list :goal-index goal-index
                                                                    :action action
                                                                    :predecessor-index pred-index)))))))))))))

    ;; Extract + sort
    (let ((summaries nil))
      (maphash (lambda (_k v) (declare (ignore _k)) (push v summaries)) ht)
      (stable-sort summaries
                   (lambda (a b)
                     (let ((ca (getf a :count))
                           (cb (getf b :count)))
                       (or (> ca cb)
                           (and (= ca cb)
                                (< (getf a :clauses) (getf b :clauses))))))))))


(defun bw-top-milestones (report &key
                                 (n 10)
                                 (return :props) ; :props | :summaries
                                 (milestone-format :auto)
                                 (max-examples 3)
                                 (include-empty nil))
  "Return the top-N milestones from REPORT.

RETURN:
  :props     => list of milestone prop-lists
  :summaries => list of summary plists (see BW-RANK-MILESTONES)"
  (let* ((ranked (bw-rank-milestones report
                                     :milestone-format milestone-format
                                     :max-examples max-examples
                                     :include-empty include-empty))
         (top (subseq ranked 0 (min n (length ranked)))))
    (ecase return
      (:props (mapcar (lambda (s) (getf s :milestone-props)) top))
      (:summaries top))))


(defun bw-report-summary (report)
  "Summarize a regressed-predecessor REPORT.

Returns a plist:
  (:GOALS n :ACTION-REPORTS n :VALIDATED-TOTAL n :MILESTONES-TOTAL n)"
  (let ((goals 0)
        (action-reports 0)
        (validated-total 0)
        (milestones-total 0))
    (loop for goal-report in report do
          (incf goals)
          (loop for ar in (getf goal-report :action-reports) do
                (incf action-reports)
                (incf validated-total (or (getf ar :validated-count) 0))
                (incf milestones-total (length (or (getf ar :milestones) nil)))))
    (list :goals goals
          :action-reports action-reports
          :validated-total validated-total
          :milestones-total milestones-total)))


(defun bw-report-action-totals (report)
  "Aggregate validated/milestone counts per action across the whole REPORT.

Returns a list of plists sorted by descending :VALIDATED-COUNT, then descending
:MILESTONE-COUNT:
  (:ACTION <form> :VALIDATED-COUNT n :MILESTONE-COUNT m)"
  (let ((ht (make-hash-table :test #'equal)))
    (loop for goal-report in report do
          (loop for ar in (getf goal-report :action-reports) do
                (let* ((action (getf ar :action))
                       (vc (or (getf ar :validated-count) 0))
                       (mc (length (or (getf ar :milestones) nil)))
                       (entry (gethash action ht)))
                  (if entry
                      (progn
                        (incf (getf entry :validated-count) vc)
                        (incf (getf entry :milestone-count) mc))
                    (setf (gethash action ht)
                          (list :action action
                                :validated-count vc
                                :milestone-count mc))))))

    (let ((totals nil))
      (maphash (lambda (_k v) (declare (ignore _k)) (push v totals)) ht)
      (stable-sort totals
                   (lambda (a b)
                     (let ((va (getf a :validated-count))
                           (vb (getf b :validated-count)))
                       (or (> va vb)
                           (and (= va vb)
                                (> (getf a :milestone-count)
                                   (getf b :milestone-count))))))))))


(defun bw-report-top-actions (report &key (n 10))
  "Return the top-N actions by total validated-count across REPORT."
  (let* ((totals (bw-report-action-totals report))
         (top (subseq totals 0 (min n (length totals)))))
    (mapcar (lambda (x) (getf x :action)) top)))


(defun bw-milestone-props->goal-form (props &key (operator 'and))
  "Convert a milestone proposition list PROPS into a translator-safe goal form.

Never returns (AND) or (OR) with no arguments:
  NIL + AND => (AND T)
  NIL + OR  => (OR NIL)"
  (cond
    (props (cons operator props))
    ((eql operator 'and) (list 'and t))
    ((eql operator 'or)  (list 'or nil))
    (t (list operator t))))


(defun bw-top-milestone-goals (report &key
                                      (n 10)
                                      (operator 'and)
                                      (milestone-format :auto)
                                      (max-examples 3)
                                      (include-empty nil))
  "Return top-N milestone goal-forms from REPORT."
  (mapcar (lambda (props)
            (bw-milestone-props->goal-form props :operator operator))
          (bw-top-milestones report
                             :n n
                             :return :props
                             :milestone-format milestone-format
                             :max-examples max-examples
                             :include-empty include-empty)))


;;;; ==================== Footprint-aware equivalence helpers ====================


(defun bw-ignore-predicate-for-action (action-form)
  "Return a predicate (PROP -> boolean) that marks propositions we should
ignore when comparing a regressed predecessor to the *actual* predecessor.

This exists because our regression is intentionally conservative: for some
actions we *don't* reconstruct all pre-state facts that the action later
overwrites (e.g., PICKUP-CONNECTOR destroys PAIRed facts involving the picked
up connector, but we don't try to guess all of them during regression).

For trace-checking we therefore ignore those overwritten facts when deciding
whether a regressed predecessor 'matches' the previous trace state."
  (check-type action-form cons)
  (case (first action-form)
    (pickup-connector
     (let ((connector (third action-form)))
       (lambda (p)
         (and (consp p)
              (eql (car p) 'paired)
              (or (eql (second p) connector)
                  (eql (third p) connector))))))
    ((connect-to-1-terminus connect-to-2-terminus connect-to-3-terminus)
     (let ((cargo (third action-form)))
       (lambda (p)
         (and (consp p)
              (eql (car p) 'paired)
              (or (eql (second p) cargo)
                  (eql (third p) cargo))))))
    (t nil)))


(defun bw--normalized-copy (state &key
                                  (strip-relations *bw-normalize-strip-relations*)
                                  (normalizer *bw-default-normalizer*))
  "Copy STATE and run BW-NORMALIZE! on the copy."
  (let ((st (copy-problem-state state)))
    (bw-normalize! st :strip-relations strip-relations :normalizer normalizer)
    st))


(defun bw-matches-previous-p (candidate-predecessors previous-state action-form &key
                                                     (projection-relations *bw-base-fact-relations*))
  "True iff any predecessor candidate matches PREVIOUS-STATE on base facts,
ignoring action-overwritten facts.

Note: This compares RAW states without normalization, using only base-fact
relations. Derived facts (active, open, color) are excluded because the
regressed predecessor intentionally omits some base facts (e.g., paired facts
for pickup), which would cause derived facts to differ after normalization."
  (when (and previous-state candidate-predecessors)
    (let ((ignore (bw-ignore-predicate-for-action action-form)))
      (some (lambda (pred)
              (bw-equivalent-p pred previous-state
                               :relations projection-relations
                               :ignore-predicate ignore))
            candidate-predecessors))))


(defun bw-goal-required-props (state)
  "Extract a minimal 'goal footprint' from STATE:
   - all (ACTIVE x)
   - (LOC agent* area) only for agent subjects."
  (let ((req nil))
    (dolist (p (bw--all-props state))
      (case (car p)
        (active (push p req))
        (loc (when (bw--agent-symbol-p (second p))
               (push p req)))))
    (sort (remove-duplicates req :test #'equal)
          (lambda (x y)
            (string< (prin1-to-string x) (prin1-to-string y))))))


;;;; ==================== Trace-driven regression tests ====================


(defun bw-trace-step->triple (step)
  "Return (values ACTION PREVIOUS TARGET) from STEP.

Supported STEP shapes:
  1) plist: (:action <form> :prev <state> :target <state>)
     also accepts :previous/:pre and :next/:post for TARGET.
  2) triple list: (<form> <previous-state> <target-state>)"
  (cond
    ;; plist
    ((and (consp step) (keywordp (car step)))
     (values (or (getf step :action) (getf step :op))
             (or (getf step :prev) (getf step :previous) (getf step :pre))
             (or (getf step :target) (getf step :next) (getf step :post))))
    ;; (action prev target)
    ((and (consp step) (consp (first step)))
     (values (first step) (second step) (third step)))
    (t
     (values nil nil nil))))


(defun bw-backward-check-trace (trace &key
                                      (normalizer *bw-default-normalizer*)
                                      (strip-relations *bw-normalize-strip-relations*)
                                      (projection-relations *bw-default-projection-relations*)
                                      (verbose t))
  "For each step in TRACE, regress from TARGET using ACTION, forward-validate
the candidates, and report whether *any* validated predecessor matches the
provided PREVIOUS state (footprint-aware).

Returns a list of plists:
  (:ACTION <form> :VALIDATED-COUNT <n> :MATCHED-PREVIOUS-P <bool>)"
  (loop for step in trace
        collect
        (multiple-value-bind (action previous target) (bw-trace-step->triple step)
          (when (and verbose action)
            (format t "~&BW TRACE: ~S~%" action))
          (let* ((validated (and action target
                                 (bw-regress+validate target action
                                                    :normalizer normalizer
                                                    :strip-relations strip-relations
                                                    :projection-relations projection-relations
                                                    :verbose nil)))
                 (matched (bw-matches-previous-p validated previous action)))
            (when verbose
              (format t "  validated predecessors: ~D~%" (length validated))
              (format t "  matches previous: ~S~%" matched))
            (list :action action
                  :validated-count (length validated)
                  :matched-previous-p matched)))))


;;;; ==================== Problem-corner trace data ====================
;;;
;;; Complete solution trace from problem-corner.lisp (15 actions).
;;; State indexing: S0 = start state, Sn = state after action n.
;;; Action n transitions S(n-1) -> Sn.


(defparameter *corner-actions*
  '((pickup-connector agent1 connector1 area1)                                      ; 1
    (connect-to-2-terminus agent1 connector1 transmitter1 receiver1 area1 ground)   ; 2
    (move agent1 area1 area2)                                                       ; 3
    (pickup-connector agent1 connector2 area2)                                      ; 4
    (move agent1 area2 area4)                                                       ; 5
    (connect-to-1-terminus agent1 connector2 transmitter1 area4 ground)             ; 6
    (move agent1 area4 area1)                                                       ; 7
    (pickup-connector agent1 connector1 area1)                                      ; 8
    (move agent1 area1 area2)                                                       ; 9
    (connect-to-3-terminus agent1 connector1 transmitter2 receiver1 receiver3 area2 ground) ; 10
    (move agent1 area2 area3)                                                       ; 11
    (pickup-connector agent1 connector3 area3)                                      ; 12
    (connect-to-3-terminus agent1 connector3 transmitter1 connector1 receiver2 area3 ground) ; 13
    (move agent1 area3 area4)                                                       ; 14
    (pickup-connector agent1 connector2 area4))                                     ; 15
  "The 15-action optimal solution for problem-corner.")


(defparameter *corner-state-props*
  ;; Vector indexed 0..15. Entry i holds propositions for state Si.
  (vector
   ;; S0: Start state
   '((current-beams nil)
     (elevation agent1 0)
     (elevation connector1 0)
     (elevation connector2 0)
     (elevation connector3 0)
     (loc agent1 area1)
     (loc connector1 area1)
     (loc connector2 area2)
     (loc connector3 area3))

   ;; S1: After action 1 (pickup-connector agent1 connector1 area1)
   '((current-beams nil)
     (elevation agent1 0)
     (elevation connector2 0)
     (elevation connector3 0)
     (holds agent1 connector1)
     (loc agent1 area1)
     (loc connector2 area2)
     (loc connector3 area3))

   ;; S2: After action 2 (connect-to-2-terminus agent1 connector1 transmitter1 receiver1 area1 ground)
   '((active receiver1)
     (beam-segment beam1 transmitter1 connector1 9 1)
     (beam-segment beam2 connector1 receiver1 81/10 1)
     (color connector1 red)
     (current-beams (beam2 beam1))
     (elevation agent1 0)
     (elevation connector2 0)
     (elevation connector3 0)
     (elevation connector1 0)
     (loc agent1 area1)
     (loc connector2 area2)
     (loc connector3 area3)
     (loc connector1 area1)
     (open gate1)
     (paired receiver1 connector1)
     (paired connector1 receiver1)
     (paired transmitter1 connector1)
     (paired connector1 transmitter1))

   ;; S3: After action 3 (move agent1 area1 area2)
   '((active receiver1)
     (beam-segment beam1 transmitter1 connector1 9 1)
     (beam-segment beam2 connector1 receiver1 81/10 1)
     (color connector1 red)
     (current-beams (beam2 beam1))
     (elevation agent1 0)
     (elevation connector2 0)
     (elevation connector3 0)
     (elevation connector1 0)
     (loc agent1 area2)
     (loc connector2 area2)
     (loc connector3 area3)
     (loc connector1 area1)
     (open gate1)
     (paired receiver1 connector1)
     (paired connector1 receiver1)
     (paired transmitter1 connector1)
     (paired connector1 transmitter1))

   ;; S4: After action 4 (pickup-connector agent1 connector2 area2)
   '((active receiver1)
     (beam-segment beam1 transmitter1 connector1 9 1)
     (beam-segment beam2 connector1 receiver1 81/10 1)
     (color connector1 red)
     (current-beams (beam2 beam1))
     (elevation agent1 0)
     (elevation connector3 0)
     (elevation connector1 0)
     (holds agent1 connector2)
     (loc agent1 area2)
     (loc connector3 area3)
     (loc connector1 area1)
     (open gate1)
     (paired receiver1 connector1)
     (paired connector1 receiver1)
     (paired transmitter1 connector1)
     (paired connector1 transmitter1))

   ;; S5: After action 5 (move agent1 area2 area4)
   '((active receiver1)
     (beam-segment beam1 transmitter1 connector1 9 1)
     (beam-segment beam2 connector1 receiver1 81/10 1)
     (color connector1 red)
     (current-beams (beam2 beam1))
     (elevation agent1 0)
     (elevation connector3 0)
     (elevation connector1 0)
     (holds agent1 connector2)
     (loc agent1 area4)
     (loc connector3 area3)
     (loc connector1 area1)
     (open gate1)
     (paired receiver1 connector1)
     (paired connector1 receiver1)
     (paired transmitter1 connector1)
     (paired connector1 transmitter1))

   ;; S6: After action 6 (connect-to-1-terminus agent1 connector2 transmitter1 area4 ground)
   '((active receiver1)
     (beam-segment beam1 transmitter1 connector1 9 1)
     (beam-segment beam2 connector1 receiver1 81/10 1)
     (beam-segment beam3 transmitter1 connector2 7 8)
     (color connector1 red)
     (color connector2 red)
     (current-beams (beam3 beam2 beam1))
     (elevation agent1 0)
     (elevation connector3 0)
     (elevation connector1 0)
     (elevation connector2 0)
     (loc agent1 area4)
     (loc connector3 area3)
     (loc connector1 area1)
     (loc connector2 area4)
     (open gate1)
     (paired receiver1 connector1)
     (paired connector1 receiver1)
     (paired transmitter1 connector1)
     (paired connector1 transmitter1)
     (paired transmitter1 connector2)
     (paired connector2 transmitter1))

   ;; S7: After action 7 (move agent1 area4 area1)
   '((active receiver1)
     (beam-segment beam1 transmitter1 connector1 9 1)
     (beam-segment beam2 connector1 receiver1 81/10 1)
     (beam-segment beam3 transmitter1 connector2 7 8)
     (color connector1 red)
     (color connector2 red)
     (current-beams (beam3 beam2 beam1))
     (elevation agent1 0)
     (elevation connector3 0)
     (elevation connector1 0)
     (elevation connector2 0)
     (loc agent1 area1)
     (loc connector3 area3)
     (loc connector1 area1)
     (loc connector2 area4)
     (open gate1)
     (paired receiver1 connector1)
     (paired connector1 receiver1)
     (paired transmitter1 connector1)
     (paired connector1 transmitter1)
     (paired transmitter1 connector2)
     (paired connector2 transmitter1))

   ;; S8: After action 8 (pickup-connector agent1 connector1 area1)
   '((beam-segment beam3 transmitter1 connector2 8 241/40)
     (current-beams (beam3 nil nil))
     (elevation agent1 0)
     (elevation connector3 0)
     (elevation connector2 0)
     (holds agent1 connector1)
     (loc agent1 area1)
     (loc connector3 area3)
     (loc connector2 area4)
     (paired transmitter1 connector2)
     (paired connector2 transmitter1))

   ;; S9: After action 9 (move agent1 area1 area2)
   '((beam-segment beam3 transmitter1 connector2 8 241/40)
     (current-beams (beam3 nil nil))
     (elevation agent1 0)
     (elevation connector3 0)
     (elevation connector2 0)
     (holds agent1 connector1)
     (loc agent1 area2)
     (loc connector3 area3)
     (loc connector2 area4)
     (paired transmitter1 connector2)
     (paired connector2 transmitter1))

   ;; S10: After action 10 (connect-to-3-terminus agent1 connector1 transmitter2 receiver1 receiver3 area2 ground)
   '((beam-segment beam3 transmitter1 connector2 29/3 41/15)
     (beam-segment beam4 transmitter2 connector1 29/3 41/15)
     (current-beams (beam4 beam3 nil nil))
     (elevation agent1 0)
     (elevation connector3 0)
     (elevation connector2 0)
     (elevation connector1 0)
     (loc agent1 area2)
     (loc connector3 area3)
     (loc connector2 area4)
     (loc connector1 area2)
     (paired transmitter1 connector2)
     (paired connector2 transmitter1)
     (paired receiver3 connector1)
     (paired connector1 receiver3)
     (paired receiver1 connector1)
     (paired connector1 receiver1)
     (paired transmitter2 connector1)
     (paired connector1 transmitter2))

   ;; S11: After action 11 (move agent1 area2 area3)
   '((beam-segment beam3 transmitter1 connector2 29/3 41/15)
     (beam-segment beam4 transmitter2 connector1 29/3 41/15)
     (current-beams (beam4 beam3 nil nil))
     (elevation agent1 0)
     (elevation connector3 0)
     (elevation connector2 0)
     (elevation connector1 0)
     (loc agent1 area3)
     (loc connector3 area3)
     (loc connector2 area4)
     (loc connector1 area2)
     (paired transmitter1 connector2)
     (paired connector2 transmitter1)
     (paired receiver3 connector1)
     (paired connector1 receiver3)
     (paired receiver1 connector1)
     (paired connector1 receiver1)
     (paired transmitter2 connector1)
     (paired connector1 transmitter2))

   ;; S12: After action 12 (pickup-connector agent1 connector3 area3)
   '((beam-segment beam3 transmitter1 connector2 29/3 41/15)
     (beam-segment beam4 transmitter2 connector1 29/3 41/15)
     (current-beams (beam4 beam3 nil nil))
     (elevation agent1 0)
     (elevation connector2 0)
     (elevation connector1 0)
     (holds agent1 connector3)
     (loc agent1 area3)
     (loc connector2 area4)
     (loc connector1 area2)
     (paired transmitter1 connector2)
     (paired connector2 transmitter1)
     (paired receiver3 connector1)
     (paired connector1 receiver3)
     (paired receiver1 connector1)
     (paired connector1 receiver1)
     (paired transmitter2 connector1)
     (paired connector1 transmitter2))

   ;; S13: After action 13 (connect-to-3-terminus agent1 connector3 transmitter1 connector1 receiver2 area3 ground)
   '((active receiver2)
     (active receiver1)
     (beam-segment beam3 transmitter1 connector2 29/3 41/15)
     (beam-segment beam4 transmitter2 connector1 29/3 41/15)
     (beam-segment beam5 transmitter1 connector3 10 9)
     (beam-segment beam6 connector3 receiver2 7 109/10)
     (beam-segment beam7 connector3 connector1 9 8)
     (beam-segment beam8 connector1 receiver1 81/10 1)
     (beam-segment beam9 connector1 receiver3 1 109/10)
     (color connector3 red)
     (color connector1 red)
     (current-beams (beam9 beam8 beam7 beam6 beam5 beam4 beam3 nil nil))
     (elevation agent1 0)
     (elevation connector2 0)
     (elevation connector1 0)
     (elevation connector3 0)
     (loc agent1 area3)
     (loc connector2 area4)
     (loc connector1 area2)
     (loc connector3 area3)
     (open gate1)
     (paired transmitter1 connector2)
     (paired connector2 transmitter1)
     (paired receiver3 connector1)
     (paired connector1 receiver3)
     (paired receiver1 connector1)
     (paired connector1 receiver1)
     (paired transmitter2 connector1)
     (paired connector1 transmitter2)
     (paired connector1 connector3)
     (paired connector3 connector1)
     (paired receiver2 connector3)
     (paired connector3 receiver2)
     (paired transmitter1 connector3)
     (paired connector3 transmitter1))

   ;; S14: After action 14 (move agent1 area3 area4)
   '((active receiver2)
     (active receiver1)
     (beam-segment beam3 transmitter1 connector2 29/3 41/15)
     (beam-segment beam4 transmitter2 connector1 29/3 41/15)
     (beam-segment beam5 transmitter1 connector3 10 9)
     (beam-segment beam6 connector3 receiver2 7 109/10)
     (beam-segment beam7 connector3 connector1 9 8)
     (beam-segment beam8 connector1 receiver1 81/10 1)
     (beam-segment beam9 connector1 receiver3 1 109/10)
     (color connector3 red)
     (color connector1 red)
     (current-beams (beam9 beam8 beam7 beam6 beam5 beam4 beam3 nil nil))
     (elevation agent1 0)
     (elevation connector2 0)
     (elevation connector1 0)
     (elevation connector3 0)
     (loc agent1 area4)
     (loc connector2 area4)
     (loc connector1 area2)
     (loc connector3 area3)
     (open gate1)
     (paired transmitter1 connector2)
     (paired connector2 transmitter1)
     (paired receiver3 connector1)
     (paired connector1 receiver3)
     (paired receiver1 connector1)
     (paired connector1 receiver1)
     (paired transmitter2 connector1)
     (paired connector1 transmitter2)
     (paired connector1 connector3)
     (paired connector3 connector1)
     (paired receiver2 connector3)
     (paired connector3 receiver2)
     (paired transmitter1 connector3)
     (paired connector3 transmitter1))

   ;; S15: After action 15 (pickup-connector agent1 connector2 area4) - FINAL
   '((active receiver2)
     (active receiver3)
     (beam-segment beam4 transmitter2 connector1 9 8)
     (beam-segment beam5 transmitter1 connector3 10 9)
     (beam-segment beam6 connector3 receiver2 7 109/10)
     (beam-segment beam7 connector3 connector1 9 8)
     (beam-segment beam8 connector1 receiver1 81/10 1)
     (beam-segment beam9 connector1 receiver3 1 109/10)
     (beam-segment beam10 connector1 connector3 10 9)
     (color connector3 red)
     (color connector1 blue)
     (current-beams (beam10 beam9 beam8 beam7 beam6 beam5 beam4 nil nil nil))
     (elevation agent1 0)
     (elevation connector1 0)
     (elevation connector3 0)
     (holds agent1 connector2)
     (loc agent1 area4)
     (loc connector1 area2)
     (loc connector3 area3)
     (paired receiver3 connector1)
     (paired connector1 receiver3)
     (paired receiver1 connector1)
     (paired connector1 receiver1)
     (paired transmitter2 connector1)
     (paired connector1 transmitter2)
     (paired connector1 connector3)
     (paired connector3 connector1)
     (paired receiver2 connector3)
     (paired connector3 receiver2)
     (paired transmitter1 connector3)
     (paired connector3 transmitter1)))
  "Vector of proposition lists for states S0..S15 of the problem-corner solution.")


;;;; ==================== REPL test helpers ====================


(defun bw-corner-state (n)
  "Return a fresh PROBLEM-STATE for state Sn (0 <= n <= 15)."
  (check-type n (integer 0 15))
  (bw-make-state-from-propositions (aref *corner-state-props* n)
                                   :relations nil
                                   :time (float n)))


(defun bw-corner-action (n)
  "Return the Nth action form (1 <= n <= 15)."
  (check-type n (integer 1 15))
  (nth (1- n) *corner-actions*))


(defun bw-corner-build-trace (start-action end-action)
  "Build a trace list for actions START-ACTION through END-ACTION (inclusive).

Each trace step is a plist with :action, :prev, :target.
Action n uses state S(n-1) as :prev and Sn as :target."
  (check-type start-action (integer 1 15))
  (check-type end-action (integer 1 15))
  (assert (<= start-action end-action))
  (loop for n from start-action to end-action
        collect (list :action (bw-corner-action n)
                      :prev (bw-corner-state (1- n))
                      :target (bw-corner-state n))))


(defun bw-test-corner-range (start-action end-action &key (verbose t))
  "Test backward regression on actions START-ACTION through END-ACTION.

Example usage:
  (bw-test-corner-range 14 15)   ; test actions 14 and 15
  (bw-test-corner-range 12 15)   ; test actions 12 through 15
  (bw-test-corner-range 1 15)    ; test entire solution trace

Returns a list of result plists, one per action."
  (check-type start-action (integer 1 15))
  (check-type end-action (integer 1 15))
  (assert (<= start-action end-action))
  (when verbose
    (format t "~&=== Testing backward regression: actions ~D-~D ===~%" 
            start-action end-action))
  (let* ((trace (bw-corner-build-trace start-action end-action))
         (results (bw-backward-check-trace trace :verbose verbose)))
    (when verbose
      (format t "~&=== Summary ===~%")
      (loop for r in results
            for n from start-action
            do (format t "  Action ~2D: ~A predecessors, matched=~A~%"
                       n
                       (getf r :validated-count)
                       (getf r :matched-previous-p))))
    results))


(defun bw-test-corner-single (action-num &key (verbose t))
  "Test backward regression on a single action.

Example: (bw-test-corner-single 15)"
  (bw-test-corner-range action-num action-num :verbose verbose))


;;;; ==================== Goal State Enumeration (problem-corner) ====================
;;;
;;; For backward search, we need complete goal state(s) to regress from.
;;; This section provides functions to enumerate valid goal states by:
;;;   1. Analyzing goal condition to identify derived-fact requirements
;;;   2. Building activation templates (what base facts produce derived goals)
;;;   3. Enumerating instantiations of templates
;;;   4. Validating via propagate-changes! and goal predicate check
;;;
;;; For problem-corner, the goal is:
;;;   (and (active receiver2)    ; derived, requires RED beam chain from transmitter1
;;;        (active receiver3)    ; derived, requires BLUE beam chain from transmitter2
;;;        (loc agent1 area4))   ; base fact
;;;
;;; Activation template analysis:
;;;   - receiver2 (chroma RED) needs: transmitter1 -> ?C_red -> receiver2
;;;   - receiver3 (chroma BLUE) needs: transmitter2 -> ?C_blue -> receiver3
;;;   - ?C_red must be in area with los0 to transmitter1 AND receiver2: {area2, area3}
;;;   - ?C_blue must be in area with los0 to transmitter2 AND receiver3: {area2, area3}
;;;   - ?C_red ≠ ?C_blue (connector can't be two colors)
;;;   - Third connector: held by agent or placed somewhere
;;;
;;; Enumeration: 2 areas × 2 areas × 5 dispositions = 20 candidate configurations


(defparameter *corner-static-facts*
  '(;; Coordinates
    (coords area1 9 1 0)
    (coords area2 9 8 0)
    (coords area3 10 9 0)
    (coords area4 7 8 0)
    (coords transmitter1 11 1/10 1)
    (coords transmitter2 10 1/10 1)
    (coords receiver1 81/10 1 1)
    (coords receiver2 7 109/10 1)
    (coords receiver3 1 109/10 1)
    ;; Controls
    (controls receiver1 gate1)
    ;; Geometry segments
    (wall-segments ((wall1 8 7 8 8) (wall2 8 0 8 3)))
    (gate-segments ((gate1 8 3 8 7) (gate2 2 11 6 11)))
    (window-segments ((window1 8 8 8 11)))
    ;; Chroma (transmitter/receiver colors)
    (chroma transmitter1 red)
    (chroma transmitter2 blue)
    (chroma receiver1 red)
    (chroma receiver2 red)
    (chroma receiver3 blue)
    ;; Line-of-sight (area to fixture)
    (los0 area1 transmitter1)
    (los0 area1 transmitter2)
    (los0 area1 receiver1)
    (los0 area2 transmitter1)
    (los0 area2 transmitter2)
    (los0 area2 receiver1)
    (los0 area2 receiver2)
    (los0 area2 receiver3)
    (los0 area3 transmitter1)
    (los0 area3 transmitter2)
    (los0 area3 receiver1)
    (los0 area3 receiver2)
    (los0 area3 receiver3)
    (los1 area4 gate1 transmitter1)
    (los1 area4 gate1 transmitter2)
    (los0 area4 receiver2)
    (los0 area4 receiver3)
    ;; Visibility (area to area)
    (visible0 area1 area2)
    (visible0 area1 area3)
    (visible1 area1 gate1 area4)
    (visible0 area2 area3)
    (visible0 area2 area4)
    (visible0 area3 area4)
    ;; Accessibility (area to area)
    (accessible0 area1 area2)
    (accessible0 area1 area3)
    (accessible1 area1 gate1 area4)
    (accessible0 area2 area3)
    (accessible1 area2 gate1 area4)
    (accessible1 area3 gate1 area4))
  "Static facts from problem-corner's define-init, required for action preconditions.")


(defun bw-corner-goal-satisfied-p (state)
  "Check if STATE satisfies the problem-corner goal condition."
  (let ((props (bw--all-props state)))
    (and (member '(active receiver2) props :test #'equal)
         (member '(active receiver3) props :test #'equal)
         (member '(loc agent1 area4) props :test #'equal))))


(defun bw-corner-build-goal-candidate (c-red-area c-blue-area c-other-disposition)
  "Build a goal state candidate for problem-corner with MINIMAL base facts.

C-RED-AREA: area for the connector relaying RED to receiver2 (area2 or area3)
C-BLUE-AREA: area for the connector relaying BLUE to receiver3 (area2 or area3)
C-OTHER-DISPOSITION: :held or an area symbol for the third connector

Uses canonical connector assignment:
  connector1 = C_red (relays RED: transmitter1 -> receiver2)
  connector2 = C_blue (relays BLUE: transmitter2 -> receiver3)
  connector3 = C_other (held or placed)

Returns a PROBLEM-STATE with minimal base facts, before propagation."
  (let ((base-facts
          (append
           ;; Fixed by goal
           '((loc agent1 area4)
             (elevation agent1 0))
           
           ;; C_red = connector1 relays RED: transmitter1 -> connector1 -> receiver2
           `((loc connector1 ,c-red-area)
             (elevation connector1 0)
             (paired transmitter1 connector1)
             (paired connector1 transmitter1)
             (paired receiver2 connector1)
             (paired connector1 receiver2))
           
           ;; C_blue = connector2 relays BLUE: transmitter2 -> connector2 -> receiver3
           `((loc connector2 ,c-blue-area)
             (elevation connector2 0)
             (paired transmitter2 connector2)
             (paired connector2 transmitter2)
             (paired receiver3 connector2)
             (paired connector2 receiver3))
           
           ;; C_other = connector3
           (if (eql c-other-disposition :held)
               '((holds agent1 connector3))
               `((loc connector3 ,c-other-disposition)
                 (elevation connector3 0)))
           
           ;; Required for propagation
           '((current-beams ()))
           
           ;; Static facts required for action preconditions
           *corner-static-facts*)))
    
    (bw-make-state-from-propositions base-facts :relations nil :time 0.0)))


(defun bw-corner-enumerate-goal-candidates ()
  "Enumerate all 20 candidate goal configurations for problem-corner.

Returns a list of plists:
  (:id N :c-red-area AREA :c-blue-area AREA :c-other DISP :state STATE)

Configuration numbering:
  1-5:   c-red=area2, c-blue=area2, c-other varies
  6-10:  c-red=area2, c-blue=area3, c-other varies
  11-15: c-red=area3, c-blue=area2, c-other varies  (config 11 matches S15 pattern)
  16-20: c-red=area3, c-blue=area3, c-other varies"
  (let ((results nil)
        (id 0))
    (dolist (c-red-area '(area2 area3))
      (dolist (c-blue-area '(area2 area3))
        (dolist (c-other '(:held area1 area2 area3 area4))
          (incf id)
          (push (list :id id
                      :c-red-area c-red-area
                      :c-blue-area c-blue-area
                      :c-other c-other
                      :state (bw-corner-build-goal-candidate c-red-area c-blue-area c-other))
                results))))
    (nreverse results)))


(defun bw-corner-test-goal-candidates (&key (verbose t))
  "Test all 20 goal candidate configurations for problem-corner.

For each candidate:
  1. Copy the state
  2. Run propagate-changes! to compute derived facts
  3. Check if goal is satisfied
  4. Report results

Returns list of valid configuration plists (with propagated :state).

Example usage:
  (bw-corner-test-goal-candidates)           ; see all results
  (bw-corner-test-goal-candidates :verbose nil)  ; just get valid configs"
  (let ((candidates (bw-corner-enumerate-goal-candidates))
        (valid nil))
    (when verbose
      (format t "~&=== Testing ~D goal candidate configurations ===~%" (length candidates))
      (format t "~&    (Minimal base facts + propagation → check goal)~%~%"))
    
    (dolist (cand candidates)
      (let* ((id (getf cand :id))
             (state (copy-problem-state (getf cand :state))))
        ;; Run propagation to compute derived facts (active, open, color, beam-segment)
        (let (forward-list inverse-list)
          (declare (special forward-list inverse-list))
          (bw--maybe-propagate-changes! state))
        ;; Check goal satisfaction
        (let ((satisfied (bw-corner-goal-satisfied-p state)))
          (when verbose
            (format t "Config ~2D: red=~A blue=~A other=~6A → ~A~%"
                    id
                    (getf cand :c-red-area)
                    (getf cand :c-blue-area)
                    (getf cand :c-other)
                    (if satisfied "VALID" "---")))
          (when satisfied
            (push (list :id id
                        :c-red-area (getf cand :c-red-area)
                        :c-blue-area (getf cand :c-blue-area)
                        :c-other (getf cand :c-other)
                        :state state)  ; propagated state
                  valid)))))
    
    (when verbose
      (format t "~&~%=== ~D valid goal state~:P found ===~%" (length valid)))
    
    (nreverse valid)))


(defun bw-corner-show-goal-state (config &key (relations *bw-default-projection-relations*))
  "Display the key propositions of a goal state configuration.

CONFIG is a plist from bw-corner-test-goal-candidates.

Example:
  (let ((valid (bw-corner-test-goal-candidates :verbose nil)))
    (bw-corner-show-goal-state (first valid)))"
  (let ((state (getf config :state)))
    (format t "~&=== Config ~D: red=~A blue=~A other=~A ===~%"
            (getf config :id)
            (getf config :c-red-area)
            (getf config :c-blue-area)
            (getf config :c-other))
    (format t "~&Propositions (~A):~%" relations)
    (dolist (prop (bw-project state :relations relations))
      (format t "  ~S~%" prop))))


(defun bw-corner-valid-goal-states ()
  "Return list of valid goal states for problem-corner (states only, not full configs).

This is the entry point for backward search initialization.

Example:
  (let ((goal-states (bw-corner-valid-goal-states)))
    (format t \"Found ~D goal states to regress from.\" (length goal-states)))"
  (mapcar (lambda (config) (getf config :state))
          (bw-corner-test-goal-candidates :verbose nil)))


(defun bw--collect-connectors (state)
  "Collect connector symbols appearing anywhere in STATE props."
  (let ((cs nil))
    (dolist (p (bw--all-props state))
      (dolist (x (rest p))
        (when (bw--connector-symbol-p x)
          (push x cs))))
    (sort (remove-duplicates cs :test #'eq)
          #'string< :key #'symbol-name)))


(defun bw-candidate-last-actions-for-goal (target-state &key
                                                       (include-move t)
                                                       (include-pickup t)
                                                       (include-connect nil)
                                                       (n-values '(1 2 3))
                                                       (place 'ground))
  "Generate plausible last-action candidates for a *goal-ish* TARGET-STATE.

Key behavior: uses *TYPES* to source areas/connectors/agents when TARGET-STATE
doesn't mention enough objects (common with enumerated or projected states).

MOVE candidates: (MOVE ag from goal-area) for all FROM areas != goal-area.
PICKUP candidates: (PICKUP-CONNECTOR ag conn goal-area) for all connectors.
CONNECT candidates (optional): uses BW-CANDIDATE-CONNECT-ACTIONS."
  (let* ((agents (bw--union-symbol-lists
                  (bw--collect-agents target-state)
                  (bw--type-instances 'real-agent)
                  (bw--type-instances 'agent)))
         (areas (bw--union-symbol-lists
                 (bw--collect-areas target-state)
                 (bw--type-instances 'area)))
         (connectors (bw--union-symbol-lists
                      (bw--collect-connectors target-state)
                      (bw--type-instances 'connector)))
         (actions nil))
    (dolist (ag agents)
      (let ((goal-area (bw--loc-of target-state ag)))
        (when goal-area
          (when include-move
            (dolist (from areas)
              (when (and from (not (eql from goal-area)))
                (push (list 'move ag from goal-area) actions))))
          (when include-pickup
            (dolist (c connectors)
              (push (list 'pickup-connector ag c goal-area) actions))))))
    (when include-connect
      (setf actions
            (nconc actions
                   (bw-candidate-connect-actions target-state
                                                :agents agents
                                                :n-values n-values
                                                :place place))))
    (let ((uniq (remove-duplicates actions :test #'equal)))
      (sort uniq (lambda (x y)
                   (string< (prin1-to-string x) (prin1-to-string y)))))))


;;;; ==================== Backward Search Loop (problem-corner) ====================
;;;
;;; DFS backward search from goal states to start state.
;;;
;;; Algorithm:
;;;   1. Initialize stack with (goal-state, empty-path) for each valid goal state
;;;   2. Pop (state, path) from stack
;;;   3. If state matches *start-state* on base facts → SUCCESS
;;;   4. If depth > cutoff → skip
;;;   5. Generate candidate last-actions that could have produced state
;;;   6. For each action, regress+validate → predecessor(s)
;;;   7. Push (predecessor, action :: path) onto stack
;;;   8. If stack empty → FAILURE

;;; ---------- Action candidate generation ----------


(defun bw-corner-candidate-actions (state)
  "Generate candidate actions that could have produced STATE.

Returns a list of action forms. Each is a fully instantiated action
that might have led to the current state.

For problem-corner, we check:
  - MOVE: if agent is at area X, could have moved from adjacent area Y
  - PICKUP-CONNECTOR: if agent holds connector, could have picked it up
  - CONNECT-TO-N-TERMINUS: if connector is placed with pairings, could have been connected"
  (let ((props (bw--all-props state))
        (candidates nil))
    
    ;; --- MOVE candidates ---
    ;; If agent is at ?to, could have moved from any accessible ?from
    ;; Filter by actual accessibility: accessible0 (direct) or accessible1 with gate open
    (let ((agent-loc (find-if (lambda (p) (and (eql (car p) 'loc)
                                                (eql (second p) 'agent1)))
                              props)))
      (when agent-loc
        (let ((to-area (third agent-loc)))
          ;; Generate MOVE from each possible source area, checking accessibility
          (dolist (from-area '(area1 area2 area3 area4))
            (unless (eql from-area to-area)
              ;; Check if move is accessible
              (let ((accessible-p nil))
                ;; Check accessible0 (direct, no gate)
                (when (or (find-if (lambda (p) (and (eql (car p) 'accessible0)
                                                    (eql (second p) from-area)
                                                    (eql (third p) to-area)))
                                   props)
                          (find-if (lambda (p) (and (eql (car p) 'accessible0)
                                                    (eql (second p) to-area)
                                                    (eql (third p) from-area)))
                                   props))
                  (setf accessible-p t))
                ;; Check accessible1 (gate-controlled) - need gate open
                (unless accessible-p
                  (dolist (p props)
                    (when (and (eql (car p) 'accessible1)
                               (or (and (eql (second p) from-area)
                                        (eql (fourth p) to-area))
                                   (and (eql (second p) to-area)
                                        (eql (fourth p) from-area))))
                      ;; Found accessible1 - check if gate is open
                      (let ((gate (third p)))
                        (when (find-if (lambda (q) (and (eql (car q) 'open)
                                                        (eql (second q) gate)))
                                       props)
                          (setf accessible-p t))))))
                ;; Only generate candidate if accessible
                (when accessible-p
                  (push (list 'move 'agent1 from-area to-area) candidates))))))))
    
    ;; --- PICKUP-CONNECTOR candidates ---
    ;; If agent holds connector C, could have picked it up from agent's current area
    (let ((holds-prop (find-if (lambda (p) (and (eql (car p) 'holds)
                                                 (eql (second p) 'agent1)))
                               props))
          (agent-loc (find-if (lambda (p) (and (eql (car p) 'loc)
                                                (eql (second p) 'agent1)))
                              props)))
      (when (and holds-prop agent-loc)
        (let ((connector (third holds-prop))
              (area (third agent-loc)))
          (push (list 'pickup-connector 'agent1 connector area) candidates))))
    
    ;; --- CONNECT-TO-N-TERMINUS candidates ---
    ;; Two cases:
    ;; 1. Connector has existing pairings -> use those to determine termini
    ;; 2. Connector has NO pairings -> enumerate possible terminus combinations
    (let ((agent-area (let ((aloc (find-if (lambda (p) (and (eql (car p) 'loc)
                                                            (eql (second p) 'agent1)))
                                           props)))
                       (when aloc (third aloc)))))
      (dolist (connector '(connector1 connector2 connector3))
        (let ((loc-prop (find-if (lambda (p) (and (eql (car p) 'loc)
                                                   (eql (second p) connector)))
                                 props)))
          ;; Only consider connector if it's at agent's current location
          (when (and loc-prop agent-area (eql (third loc-prop) agent-area))
            (let ((area (third loc-prop))
                  (termini nil))
              ;; Collect all termini this connector is paired with
              (dolist (p props)
                (when (and (eql (car p) 'paired)
                           (eql (second p) connector)
                           (not (eql (third p) connector)))  ; not self-paired
                  (push (third p) termini)))
              
              (if termini
                  ;; Case 1: Has pairings - generate connect action based on them
                  (let ((n (length termini)))
                    (when (and (>= n 1) (<= n 3))
                      (let ((place 'ground))  ; simplified: always ground for now
                        (case n
                          (1 (push (list 'connect-to-1-terminus 'agent1 connector
                                         (first termini) area place)
                                   candidates))
                          (2 (push (list 'connect-to-2-terminus 'agent1 connector
                                         (first termini) (second termini) area place)
                                   candidates))
                          (3 (push (list 'connect-to-3-terminus 'agent1 connector
                                         (first termini) (second termini) (third termini)
                                         area place)
                                   candidates))))))
                  
                  ;; Case 2: No pairings - enumerate possible terminus combinations
                  ;; Build list of available termini (transmitters, receivers, other placed connectors)
                  (let ((available-termini nil))
                    ;; Add transmitters and receivers
                    (dolist (term '(transmitter1 transmitter2 receiver1 receiver2 receiver3))
                      (push term available-termini))
                    ;; Add other placed connectors
                    (dolist (other-conn '(connector1 connector2 connector3))
                      (unless (eql other-conn connector)
                        (when (find-if (lambda (p) (and (eql (car p) 'loc)
                                                        (eql (second p) other-conn)))
                                       props)
                          (push other-conn available-termini))))
                    ;; Generate 1-terminus combinations
                    (dolist (t1 available-termini)
                      (push (list 'connect-to-1-terminus 'agent1 connector t1 area 'ground)
                            candidates))
                    ;; Generate 2-terminus combinations
                    (loop for rest1 on available-termini do
                      (let ((t1 (car rest1)))
                        (dolist (t2 (cdr rest1))
                          (push (list 'connect-to-2-terminus 'agent1 connector t1 t2 area 'ground)
                                candidates))))
                    ;; Generate 3-terminus combinations
                    (loop for rest1 on available-termini do
                      (let ((t1 (car rest1)))
                        (loop for rest2 on (cdr rest1) do
                          (let ((t2 (car rest2)))
                            (dolist (t3 (cdr rest2))
                              (push (list 'connect-to-3-terminus 'agent1 connector t1 t2 t3 area 'ground)
                                    candidates)))))))))))))
    
    (nreverse candidates)))


;;; ---------- Start state matching ----------


(defun bw-corner-matches-start-p (state)
  "Check if STATE matches the problem-corner start state on base facts."
  (let ((start-props '((loc agent1 area1)
                       (loc connector1 area1)
                       (loc connector2 area2)
                       (loc connector3 area3)
                       (elevation agent1 0)
                       (elevation connector1 0)
                       (elevation connector2 0)
                       (elevation connector3 0))))
    ;; Build a minimal start state for comparison
    (let ((start (bw-make-state-from-propositions start-props :relations nil)))
      (bw-equivalent-p state start
                       :relations *bw-base-fact-relations*
                       :ignore-predicate nil))))


;;; ---------- Main search loop ----------


(defun bw-corner-backward-search (&key (depth-cutoff 20) (verbose t) (max-states 100000))
  "Run backward DFS search from goal states to start state for problem-corner.

Returns (values SUCCESS-P PATH STATS) where:
  SUCCESS-P: T if solution found, NIL otherwise
  PATH: List of actions from start to goal (reversed from search order)
  STATS: Plist of search statistics

Keyword arguments:
  DEPTH-CUTOFF: Maximum regression depth (default 20)
  VERBOSE: Print progress messages (default T)
  MAX-STATES: Maximum states to explore before giving up (default 100000)

Example:
  (bw-corner-backward-search :depth-cutoff 15 :verbose t)"
  
  (let ((goal-states (bw-corner-valid-goal-states))
        (stack nil)  ; Each entry: (state path depth)
        (states-explored 0)
        (states-generated 0)
        (max-depth-seen 0)
        (start-time (get-internal-real-time)))
    
    (when verbose
      (format t "~&=== Backward Search (problem-corner) ===~%")
      (format t "Goal states: ~D~%" (length goal-states))
      (format t "Depth cutoff: ~D~%" depth-cutoff)
      (format t "~%"))
    
    ;; Initialize stack with goal states
    (dolist (gs goal-states)
      (push (list gs nil 0) stack)
      (incf states-generated))
    
    ;; Main search loop
    (loop
      ;; Check termination conditions
      (when (null stack)
        (when verbose
          (format t "~&Search exhausted. No solution found.~%"))
        (return (values nil nil (list :success nil
                                      :states-explored states-explored
                                      :states-generated states-generated
                                      :max-depth max-depth-seen
                                      :time-ms (round (* 1000 (/ (- (get-internal-real-time) start-time)
                                                                  internal-time-units-per-second)))))))
      
      (when (> states-explored max-states)
        (when verbose
          (format t "~&State limit (~D) reached. Stopping.~%" max-states))
        (return (values nil nil (list :success nil
                                      :states-explored states-explored
                                      :states-generated states-generated
                                      :max-depth max-depth-seen
                                      :time-ms (round (* 1000 (/ (- (get-internal-real-time) start-time)
                                                                  internal-time-units-per-second)))))))
      
      ;; Pop next state
      (destructuring-bind (state path depth) (pop stack)
        (incf states-explored)
        (setf max-depth-seen (max max-depth-seen depth))
        
        ;; Progress reporting
        (when (and verbose (zerop (mod states-explored 1000)))
          (format t "  Explored: ~D, Stack: ~D, Depth: ~D~%"
                  states-explored (length stack) depth))
        
        ;; Check if we reached start state
        (when (bw-corner-matches-start-p state)
          (let ((solution-path (reverse path)))
            (when verbose
              (format t "~&=== SOLUTION FOUND ===~%")
              (format t "Path length: ~D~%" (length solution-path))
              (format t "States explored: ~D~%" states-explored)
              (format t "Actions:~%")
              (loop for action in solution-path
                    for i from 1
                    do (format t "  ~2D. ~S~%" i action)))
            (return (values t solution-path
                            (list :success t
                                  :path-length (length solution-path)
                                  :states-explored states-explored
                                  :states-generated states-generated
                                  :max-depth max-depth-seen
                                  :time-ms (round (* 1000 (/ (- (get-internal-real-time) start-time)
                                                              internal-time-units-per-second))))))))
        
        ;; Skip if depth exceeded, otherwise generate successors
        (when (< depth depth-cutoff)
          ;; Generate candidate actions and regress
          (let ((candidate-actions (bw-corner-candidate-actions state)))
            (dolist (action candidate-actions)
              ;; Regress + validate
              (let ((predecessors (bw-regress+validate state action :verbose nil)))
                (dolist (pred predecessors)
                  (incf states-generated)
                  ;; Push predecessor with extended path
                  (push (list pred (cons action path) (1+ depth)) stack))))))))))


(defun bw-corner-backward-search-brief (&key (depth-cutoff 20))
  "Run backward search with minimal output. Returns (values success-p path)."
  (multiple-value-bind (success path stats)
      (bw-corner-backward-search :depth-cutoff depth-cutoff :verbose nil)
    (declare (ignore stats))
    (values success path)))


(defun bw-diagnose-connect-validation ()
  "Diagnose why CONNECT candidates fail validation."
  (let* ((gs (first (bw-corner-valid-goal-states)))
         (candidates (bw-corner-candidate-actions gs))
         (action (first candidates)))  ; PICKUP
    
    ;; Get a predecessor state (after PICKUP regression)
    (let* ((preds (bw-regress+validate gs action :verbose nil))
           (pred (first preds)))
      
      (format t "~&=== State after PICKUP regression ===~%")
      (format t "Agent at: ~S~%" 
              (find-if (lambda (p) (and (eql (car p) 'loc) (eql (second p) 'agent1)))
                       (bw--all-props pred)))
      (format t "Connector3 at: ~S~%"
              (find-if (lambda (p) (and (eql (car p) 'loc) (eql (second p) 'connector3)))
                       (bw--all-props pred)))
      (format t "Holds: ~S~%"
              (find-if (lambda (p) (eql (car p) 'holds)) (bw--all-props pred)))
      
      ;; Now test a CONNECT candidate through bw-regress+validate
      (let ((connect-action '(connect-to-1-terminus agent1 connector3 receiver2 area4 ground)))
        (format t "~%=== Testing CONNECT via bw-regress+validate ===~%")
        (format t "Action: ~S~%" connect-action)
        
        ;; First just do regression
        (let ((regressed (bw-regress pred connect-action)))
          (format t "~%Raw regression produced ~D candidate(s)~%" (length regressed))
          (when regressed
            (let ((reg-pred (first regressed)))
              (format t "Regressed predecessor:~%")
              (format t "  Agent at: ~S~%"
                      (find-if (lambda (p) (and (eql (car p) 'loc) (eql (second p) 'agent1)))
                               (bw--all-props reg-pred)))
              (format t "  Holds: ~S~%"
                      (find-if (lambda (p) (eql (car p) 'holds)) (bw--all-props reg-pred)))
              (format t "  Connector3 loc: ~S~%"
                      (find-if (lambda (p) (and (eql (car p) 'loc) (eql (second p) 'connector3)))
                               (bw--all-props reg-pred)))
              
              ;; Normalize and try forward application
              (let ((norm-pred (copy-problem-state reg-pred)))
                (bw-normalize! norm-pred)
                (format t "~%After normalization:~%")
                (format t "  Holds: ~S~%"
                        (find-if (lambda (p) (eql (car p) 'holds)) (bw--all-props norm-pred)))
                (format t "  Connector3 loc: ~S~%"
                        (find-if (lambda (p) (and (eql (car p) 'loc) (eql (second p) 'connector3)))
                                 (bw--all-props norm-pred)))
                
                                (format t "~%Attempting forward application...~%")
                (multiple-value-bind (next-state ok reason)
                    (apply-action-to-state connect-action norm-pred nil nil)
                  (format t "  Success: ~S~%" ok)
                  (format t "  Reason: ~S~%" reason)
                  
                  (when ok
                    ;; Normalize next-state and compare to target (pred)
                    (let ((norm-next (copy-problem-state next-state)))
                      (bw-normalize! norm-next)
                      (let ((norm-target (copy-problem-state pred)))
                        (bw-normalize! norm-target)
                        
                        (format t "~%=== Equivalence check ===~%")
                        (let ((next-proj (bw-project norm-next))
                              (target-proj (bw-project norm-target)))
                          (format t "Next state projection (~D props):~%" (length next-proj))
                          (format t "Target projection (~D props):~%" (length target-proj))
                          
                          ;; Show differences
                          (let ((in-next-only (set-difference next-proj target-proj :test #'equal))
                                (in-target-only (set-difference target-proj next-proj :test #'equal)))
                            (format t "~%In next but not target (~D):~%" (length in-next-only))
                            (dolist (p in-next-only)
                              (format t "  ~S~%" p))
                            (format t "~%In target but not next (~D):~%" (length in-target-only))
                            (dolist (p in-target-only)
                              (format t "  ~S~%" p)))
                          
                          (format t "~%Equivalent: ~S~%" (equal next-proj target-proj)))))))))))
        
        ;; Full validation
        (format t "~%=== Full bw-regress+validate ===~%")
        (let ((validated (bw-regress+validate pred connect-action :verbose t)))
          (format t "Validated predecessors: ~D~%" (length validated)))))))


(defun bw-diagnose-depth-1 ()
  "Diagnose what happens after first regression step."
  (let* ((gs (first (bw-corner-valid-goal-states)))
         (candidates (bw-corner-candidate-actions gs)))
    
    (format t "~&=== Depth 0: Goal State ===~%")
    (format t "Agent loc: ~S~%" 
            (find-if (lambda (p) (and (eql (car p) 'loc) (eql (second p) 'agent1)))
                     (bw--all-props gs)))
    (format t "Holds: ~S~%"
            (find-if (lambda (p) (eql (car p) 'holds)) (bw--all-props gs)))
    (format t "Gate1 open: ~S~%"
            (find-if (lambda (p) (equal p '(open gate1))) (bw--all-props gs)))
    (format t "Candidates: ~S~%" candidates)
    
    ;; Get validated predecessors from first candidate
    (let ((action (first candidates)))
      (format t "~%=== Regressing: ~S ===~%" action)
      (let ((preds (bw-regress+validate gs action :verbose nil)))
        (format t "Validated predecessors: ~D~%" (length preds))
        
        ;; For each predecessor, show state and candidates
        (loop for pred in preds
              for i from 1 do
          (format t "~%--- Predecessor ~D ---~%" i)
          (format t "Agent loc: ~S~%"
                  (find-if (lambda (p) (and (eql (car p) 'loc) (eql (second p) 'agent1)))
                           (bw--all-props pred)))
          (format t "Holds: ~S~%"
                  (find-if (lambda (p) (eql (car p) 'holds)) (bw--all-props pred)))
          (format t "Gate1 open: ~S~%"
                  (find-if (lambda (p) (equal p '(open gate1))) (bw--all-props pred)))
          (format t "Receiver1 active: ~S~%"
                  (find-if (lambda (p) (equal p '(active receiver1))) (bw--all-props pred)))
          
          ;; Show connector locations
          (format t "Connector locs:~%")
          (dolist (c '(connector1 connector2 connector3))
            (let ((loc (find-if (lambda (p) (and (eql (car p) 'loc) (eql (second p) c)))
                                (bw--all-props pred))))
              (when loc (format t "  ~S~%" loc))))
          
          ;; Show candidates from this predecessor
          (let ((pred-candidates (bw-corner-candidate-actions pred)))
            (format t "Candidates from this predecessor: ~D~%" (length pred-candidates))
            (dolist (c pred-candidates)
              (format t "  ~S~%" c))))))))


(defun bw-diagnose-move-regression ()
  "Diagnose why MOVE regressions fail.

Traces through the regression and forward validation step by step."
  (let* ((gs (first (bw-corner-valid-goal-states))))
    
    (format t "~&=== Diagnosing MOVE regression ===~%")
    
    ;; Show agent location in goal state
    (format t "~%Goal state agent loc: ~S~%"
            (find-if (lambda (p) (and (eql (car p) 'loc) (eql (second p) 'agent1)))
                     (bw--all-props gs)))
    
    ;; Check gate1 status in goal state
    (format t "~%Gate/receiver status in goal state:~%")
    (let ((open-gate1 (find-if (lambda (p) (equal p '(open gate1))) (bw--all-props gs)))
          (active-r1 (find-if (lambda (p) (equal p '(active receiver1))) (bw--all-props gs))))
      (format t "  (open gate1): ~S~%" open-gate1)
      (format t "  (active receiver1): ~S~%" active-r1))
    
    ;; Show actual candidates generated
    (format t "~%=== Candidate actions generated by bw-corner-candidate-actions ===~%")
    (let ((candidates (bw-corner-candidate-actions gs)))
      (if candidates
          (dolist (c candidates)
            (format t "  ~S~%" c))
          (format t "  NONE~%"))
      
      ;; Check for MOVE candidates specifically
      (let ((move-candidates (remove-if-not (lambda (c) (eql (car c) 'move)) candidates)))
        (format t "~%MOVE candidates: ~D~%" (length move-candidates))
        (dolist (m move-candidates)
          (format t "  ~S~%" m)))
      
      ;; Test each candidate
      (format t "~%=== Testing each candidate ===~%")
      (dolist (action candidates)
        (format t "~%--- Action: ~S ---~%" action)
        (let ((validated (bw-regress+validate gs action :verbose t)))
          (format t "  => ~D validated predecessor~:P~%" (length validated)))))))


;;; ---------- Diagnostics ----------


(defun bw-diagnose-goal-state (&optional (config-index 0))
  "Diagnose why backward search fails from a goal state.

CONFIG-INDEX: 0-4 for the 5 valid goal configurations.

Shows:
  1. Goal state base facts
  2. Candidate actions generated
  3. For each candidate, regression + validation results"
  (let* ((configs (bw-corner-test-goal-candidates :verbose nil))
         (config (nth config-index configs))
         (gs (getf config :state)))
    
    (format t "~&=== Diagnosing Goal Config ~D ===~%" (getf config :id))
    (format t "  c-red @ ~A, c-blue @ ~A, c-other = ~A~%"
            (getf config :c-red-area)
            (getf config :c-blue-area)
            (getf config :c-other))
    
    ;; Show base facts
    (format t "~%Base facts in goal state:~%")
    (dolist (p (bw-project gs :relations *bw-base-fact-relations*))
      (format t "  ~S~%" p))
    
    ;; Generate candidates
    (let ((candidates (bw-corner-candidate-actions gs)))
      (format t "~%Candidate actions (~D):~%" (length candidates))
      (dolist (a candidates)
        (format t "  ~S~%" a))
      
      ;; Test each candidate with verbose output
      (format t "~%Validation results:~%")
      (dolist (action candidates)
        (format t "~%--- Action: ~S ---~%" action)
        (let ((validated (bw-regress+validate gs action :verbose t)))
          (format t "  => ~D validated predecessor~:P~%" (length validated)))))))


(defun bw-diagnose-from-actual-s15 ()
  "Diagnose backward search from actual solution state S15.

This uses the known-good final state from the solution trace
rather than the enumerated minimal goal state."
  (let ((s15 (bw-corner-state 15)))
    (format t "~&=== Diagnosing from actual S15 ===~%")
    
    ;; Show base facts
    (format t "~%Base facts in S15:~%")
    (dolist (p (bw-project s15 :relations *bw-base-fact-relations*))
      (format t "  ~S~%" p))
    
    ;; Generate candidates
    (let ((candidates (bw-corner-candidate-actions s15)))
      (format t "~%Candidate actions (~D):~%" (length candidates))
      (dolist (a candidates)
        (format t "  ~S~%" a))
      
      ;; Test each candidate with verbose output
      (format t "~%Validation results:~%")
      (dolist (action candidates)
        (format t "~%--- Action: ~S ---~%" action)
        (let ((validated (bw-regress+validate s15 action :verbose t)))
          (format t "  => ~D validated predecessor~:P~%" (length validated)))))))


;;;;;;;


(defun bw--symbol-prefix-p (sym prefix)
  (and (symbolp sym)
       (let ((name (symbol-name sym)))
         (and (<= (length prefix) (length name))
              (string= prefix name :end2 (length prefix))))))


(defun bw--agent-symbol-p (sym)
  (bw--symbol-prefix-p sym "AGENT"))


(defun bw--area-symbol-p (sym)
  (bw--symbol-prefix-p sym "AREA"))


(defun bw--connector-symbol-p (sym)
  (bw--symbol-prefix-p sym "CONNECTOR"))


(defun bw--collect-areas (state)
  "Collect area symbols seen in (LOC _ area) and (COORDS area ...), if present."
  (let ((areas nil))
    (dolist (p (bw--all-props state))
      (case (car p)
        (loc
         (let ((a (third p)))
           (when (bw--area-symbol-p a) (push a areas))))
        (coords
         (let ((a (second p)))
           (when (bw--area-symbol-p a) (push a areas))))))
    (sort (remove-duplicates areas :test #'eq)
          #'string< :key #'symbol-name)))


(defun bw--collect-agents (state)
  "Collect agent symbols seen as the subject of (LOC agent area)."
  (let ((agents nil))
    (dolist (p (bw--all-props state))
      (when (eql (car p) 'loc)
        (let ((obj (second p)))
          (when (bw--agent-symbol-p obj)
            (push obj agents)))))
    (sort (remove-duplicates agents :test #'eq)
          #'string< :key #'symbol-name)))


(defun bw--holds-pairs (state)
  "Return list of (agent . cargo) pairs from (HOLDS agent cargo)."
  (let ((pairs nil))
    (dolist (p (bw--all-props state))
      (when (eql (car p) 'holds)
        (push (cons (second p) (third p)) pairs)))
    pairs))


(defun bw--paired-partners-of (state cargo)
  "Return unique partners T such that (PAIRED cargo T) or (PAIRED T cargo)."
  (let ((partners nil))
    (dolist (p (bw--all-props state))
      (when (eql (car p) 'paired)
        (let ((a (second p)) (b (third p)))
          (cond ((eql a cargo) (push b partners))
                ((eql b cargo) (push a partners))))))
    (sort (remove-duplicates partners :test #'eq)
          #'string< :key #'symbol-name)))


(defun bw--has-any-prop-with-subject-p (state relation subject)
  "True iff STATE contains (RELATION SUBJECT ...)."
  (some (lambda (p)
          (and (eql (car p) relation)
               (eql (second p) subject)))
        (bw--all-props state)))


(defun bw--loc-of (state obj)
  (bw--get-binary-fluent state 'loc obj))


(defun bw--elevation-of (state obj)
  (bw--get-binary-fluent state 'elevation obj))


(defun bw--combinations-k (lst k)
  "Return all k-combinations of LST, preserving order."
  (cond
    ((= k 0) (list nil))
    ((null lst) nil)
    (t
     (append
      (mapcar (lambda (rest) (cons (car lst) rest))
              (bw--combinations-k (cdr lst) (1- k)))
      (bw--combinations-k (cdr lst) k)))))


;;;;;;;;;; Candidate generators per action family


(defun bw-candidate-move-actions (target-state &key agents areas)
  "Generate MOVE last-action hypotheses that end at the agent's current LOC."
  (let* ((agents (or agents (bw--collect-agents target-state)))
         (areas  (or areas  (bw--collect-areas target-state)))
         (actions nil))
    (dolist (ag agents)
      (let ((to (bw--loc-of target-state ag)))
        (when to
          (dolist (from areas)
            (when (and from (not (eql from to)))
              (push (list 'move ag from to) actions))))))
    (nreverse actions)))


(defun bw-candidate-pickup-actions (target-state &key holds-pairs)
  "Generate PICKUP-CONNECTOR last-action hypotheses from (HOLDS agent cargo).

We only propose pickup when:
  - cargo looks like a CONNECTOR,
  - target contains (HOLDS agent cargo),
  - target does NOT contain any (LOC cargo ...) (because pickup deletes it),
  - target does NOT contain any (ELEVATION cargo ...) (because pickup deletes it).
Area argument is set to the agent's current LOC."
  (let ((pairs (or holds-pairs (bw--holds-pairs target-state)))
        (actions nil))
    (dolist (pr pairs)
      (let* ((ag (car pr))
             (cargo (cdr pr)))
        (when (and (bw--connector-symbol-p cargo)
                   (not (bw--has-any-prop-with-subject-p target-state 'loc cargo))
                   (not (bw--has-any-prop-with-subject-p target-state 'elevation cargo)))
          (let ((area (bw--loc-of target-state ag)))
            (when area
              (push (list 'pickup-connector ag cargo area) actions))))))
    (nreverse actions)))


(defun bw--connect-op-for-n (n)
  (case n
    (1 'connect-to-1-terminus)
    (2 'connect-to-2-terminus)
    (3 'connect-to-3-terminus)
    (otherwise (error "BW--CONNECT-OP-FOR-N: unsupported n=~S" n))))


(defun bw-candidate-connect-actions (target-state &key
                                                 agents
                                                 (n-values '(1 2 3))
                                                 (place 'ground))
  "Generate CONNECT-TO-N-TERMINUS last-action hypotheses.

We propose connect only when, in TARGET-STATE:
  - agent AG has LOC = AREA
  - some connector C has (LOC C AREA)
  - (ELEVATION C 0) holds (connect asserts elevation 0)
  - terminus candidates come from PAIRED partners of C
We also avoid terminus choices that are connectors located in the same AREA (precondition)."
  (let* ((agents (or agents (bw--collect-agents target-state)))
         (actions nil))
    (dolist (ag agents)
      (let ((area (bw--loc-of target-state ag)))
        (when area
          ;; Consider each connector located at the agent's area
          (dolist (p (bw--all-props target-state))
            (when (and (eql (car p) 'loc)
                       (bw--connector-symbol-p (second p))
                       (eql (third p) area))
              (let* ((cargo (second p))
                     (elev (bw--elevation-of target-state cargo)))
                (when (eql elev 0)
                  (let* ((partners (bw--paired-partners-of target-state cargo))
                         ;; Filter out cargo itself and any connector terminus in same area
                         (partners
                           (remove-if (lambda (term)  ;; CHANGED: was (lambda (t) ...)
                                        (or (eql term cargo)
                                            (and (bw--connector-symbol-p term)
                                                 (eql (bw--loc-of target-state term) area))))
                                      partners)))
                    (dolist (n n-values)
                      (when (and (<= 1 n) (<= n 3) (<= n (length partners)))
                        (dolist (combo (bw--combinations-k partners n))
                          (push (append (list (bw--connect-op-for-n n) ag cargo)
                                        combo
                                        (list area place))
                                actions))))))))))))
    ;; Dedup deterministically
    (let ((uniq (remove-duplicates actions :test #'equal)))
      (sort uniq (lambda (x y)
                   (string< (prin1-to-string x) (prin1-to-string y)))))))


(defun bw-candidate-last-actions (target-state &key
                                              (include-move t)
                                              (include-pickup t)
                                              (include-connect t)
                                              (n-values '(1 2 3))
                                              (place 'ground))
  "Construct a bounded, plausible candidate last-action set for TARGET-STATE."
  (let* ((agents (bw--collect-agents target-state))
         (areas  (bw--collect-areas target-state))
         (holds  (bw--holds-pairs target-state))
         (actions nil))
    (when include-move
      (setf actions (nconc actions (bw-candidate-move-actions target-state
                                                             :agents agents
                                                             :areas areas))))
    (when include-pickup
      (setf actions (nconc actions (bw-candidate-pickup-actions target-state
                                                               :holds-pairs holds))))
    (when include-connect
      (setf actions (nconc actions (bw-candidate-connect-actions target-state
                                                                :agents agents
                                                                :n-values n-values
                                                                :place place))))
    (let ((uniq (remove-duplicates actions :test #'equal)))
      (sort uniq (lambda (x y)
                   (string< (prin1-to-string x) (prin1-to-string y)))))))


(defun bw--type-instances (type)
  "Return instances for TYPE from *TYPES*, or NIL if unavailable."
  (when (and (boundp '*types*) (hash-table-p *types*))
    (let ((xs (gethash type *types*)))
      (and xs (copy-list xs)))))


(defun bw--union-symbol-lists (&rest lists)
  "Union of LISTS (symbols), stable-ish and duplicate-free."
  (let ((out nil))
    (dolist (lst lists)
      (dolist (x lst)
        (when (symbolp x)
          (pushnew x out :test #'eq))))
    (sort out #'string< :key #'symbol-name)))

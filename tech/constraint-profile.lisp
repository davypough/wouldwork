;;; Filename: constraint-profile.lisp

;;; Static constraint profile extractors for the constraint-led analysis method
;;; (doc/solving-advisor.md).  The profile is a pure
;;; function of a staged problem and optional supplied state: it reads engine databases and
;;; reports the invariant structure a hand analysis would otherwise rederive one session
;;; at a time.
;;;
;;; THIS FILE IS A LOADABLE DIAGNOSTIC.  It is never named in an (include-tech ...)
;;; directive and is not an ASDF component, so it cannot break a search and can be
;;; rewritten freely.  Load it by hand after staging:
;;;
;;;   (progn (ql:quickload :wouldwork) (in-package :ww))
;;;   (stage <problem>)
;;;   (load (merge-pathnames "tech/constraint-profile.lisp"
;;;                          (asdf:system-source-directory :wouldwork)))
;;;   (report-static-constraint-profile)
;;;
;;; RO takes an explicit scenario and is called on its own:
;;;
;;;   (report-role-obligations <scenario plist>)
;;;
;;; FH, the "from here" report, reads one state rather than the staged problem and is not
;;; part of the profile:
;;;
;;;   (report-from-here [<checkpoint> or <action-list>])
;;;
;;; CP, the cycle-plan check, reads a stage plan stated as data and is not part of the profile:
;;;
;;;   (report-cycle-plan-check <plan plist>)
;;;
;;; T33 accepts a complete reference-state scenario; see specification 6.1:
;;;   (report-relay-view-scenario scenario)
;;; EQ reads one settled state's removable fans and mounts; see specification 8.12:
;;;   (report-equipment-scenario (list :state <state> :provenance "<text>"))
;;; SW compares the services of two settled states; see specification 8.13:
;;;   (report-service-transition (list :before <state> :before-provenance "<text>"
;;;                                    :state <state> :provenance "<text>"))
;;; RC, NH and the whole-profile reporter take the same optional scenario. The writer
;;; takes it after the pathname. No scenario means explicit UNRESOLVED view results.
;;;
;;; CONSEQUENT RESTRICTION.  A merely LOADed file gets no tech splice, so everything here
;;; is plain Common Lisp in the :WW package -- ordinary DEFUN and DEFPARAMETER, no
;;; define-query, define-types, define-dynamic-relations or any other DSL defining form.
;;; Reading the staged databases and calling what staging installed is unrestricted.
;;;
;;; DEFINITION ORDER.  Callees precede callers, which is the reverse of this project's usual
;;; high-level-first order.  This file is LOADed by hand and reloaded after every edit, so a
;;; forward reference costs a STYLE-WARNING on each load, and a screenful of them hides the one
;;; warning that matters.  The extractors still sit in contiguous blocks -- S0, then S1, then S2
;;; -- each ending with its own reporter, and the two entry points end the file.
;;;
;;; DOMAIN GENERALITY IS A HARD RULE (C3).  No problem object name appears anywhere in
;;; this file.  S1 names CONTROLS, S3 names its traversal inputs, and S4 names the
;;; placement, pressure, switch and reach interfaces documented beside its code.  RO
;;; names CONTROLS, ON and HAS-POSITION, and takes every problem-specific term --
;;; devices, bodies, reasons -- from the caller's scenario, which is data.
;;;
;;; STATUS: S0, S1 and S2, each carrying the fix its first score earned -- G1's device state
;;; axiom block on S1, G5's per-row provenance on S0, and G6's consumer classification on S2,
;;; which prints each bound under the predicate that reads it rather than under the pool --
;;; and S3, the gate-labelled region quotient, scored PARTIAL on its first run (7.19) and
;;; carrying the spine/composed classification its second score earned (7.20), since the
;;; rows the coordinate derivation supplies are a transitive closure and not an adjacency
;;; list.  S0, the type extent census, is not in section 3 of crelay-topo's
;;; Constraint-Prediction-Register.txt (archive removed 2026-10-02; in git history):
;;; it closes G2 of doc/constraint-led-solving/Schema-Gaps.txt and runs
;;; as step 0, since every later extractor needs the emptiness facts before it may read a
;;; control aggregate as an axiom.  S4 adds the qualified cut-keeper table under the
;;; approved interpretation in register 7.23.  RO, the role-obligation analysis, adds
;;; conditional allocation for one caller-stated segment: it is deliberately NOT S5-S7,
;;; does not amend their sealed specifications, and is not run by the whole-profile
;;; reporter, because a generated file must not carry a segment nobody stated.
;;; S5-S7, T6, RC, MC (mechanic coverage, T19), CC (coupling census, T20) and NH (necessity
;;; hints, T21) followed, then FH (from here, T22) and CP (cycle-plan check, T24), which are
;;; not run by the whole-profile reporter.  SD (services and setup dependencies, T43) closes
;;; the profile; its SW transition check is not run by it.  The component index is in
;;; doc/constraint-led-solving/Extractor-Specifications.md.

(in-package :ww)


;; One mutual recursion lives below, and no ordering removes it: the switch walk descends into
;; a query a restricting term calls, and the descent resumes the walk inside that body (G7).
;; Declaimed so a hand LOAD stays free of style warnings, which is the whole reason this file
;; is ordered callees-first.
(declaim (ftype function census-switch-terms))


(defun census-type-provenance (type)
  "Where TYPE came from, since *TYPES* is not the set of types the specification declares.
   AUTHORED: a real DEFINE-TYPES installed it, which is the only writer of
   *TYPE-SIGNATURES*.  SYNTHESIZED: the engine interned it while translating an inline
   (either ...) parameter specification -- DISSECT-PRE-PARAMS names such a type by sorting
   its components and joining them with +, and registers its components but no signature.
   OPTIONAL: declared by DEFINE-OPTIONAL-TYPES and never populated, so it has neither.
   G5: an extractor citing the declared types must know which rows are the engine's own
   bookkeeping, and a hand derivation from the sources will never reproduce the count."
  (cond ((nth-value 1 (gethash type *type-signatures*)) "authored")
        ((nth-value 1 (gethash type *type-components*)) "synthesized")
        (t "optional")))


(defun census-type-instances (type)
  "TYPE's instances, with the empty-alias sentinel normalized away.  INSTALL-TYPES stores
   an EITHER alias whose components are all empty as (NIL) to distinguish it from a leaf
   type declared with no instances; both mean the extent is empty and the census reads
   them alike."
  (let ((instances (gethash type *types*)))
    (if (equal instances '(nil))
      nil
      instances)))


(defun census-type-flag (type)
  "EMPTY, SINGLETON, or nothing.  The two cardinalities that carry grade-1 consequences."
  (let ((size (length (census-type-instances type))))
    (cond ((zerop size) "EMPTY")
          ((= size 1) "SINGLETON")
          (t nil))))


(defun census-type-names ()
  "Every declared type, sorted by name."
  (sort (loop for type being the hash-keys of *types* collect type)
        #'string< :key #'symbol-name))


(defun report-type-extent-table ()
  "Every key of *TYPES* with its cardinality, its composite components when it was declared
   as an EITHER alias, and its flag.  An alias whose components are all empty is installed
   as the one-element list (NIL) rather than NIL, so cardinality is read through
   CENSUS-TYPE-INSTANCES, which normalizes that sentinel; counting the raw list would
   report such a type as a singleton and silently license every predicate over it."
  (let ((types (census-type-names)))
    (format t "~%  type extents (~D types)~%" (length types))
    (dolist (type types)
      (format t "    ~(~A~)  ~D  ~A~@[  ~A~]~@[  components ~(~{~A~^ ~}~)~]~%"
              type
              (length (census-type-instances type))
              (census-type-provenance type)
              (census-type-flag type)
              (gethash type *type-components*)))))


(defun report-empty-and-singleton-types ()
  "The census's payload, separated out because these are the entries later extractors
   cite: an empty type collapses every predicate over it to a constant with no induction,
   and a singleton makes its type predicate a name."
  (let ((empties (remove-if-not (lambda (type) (null (census-type-instances type)))
                                (census-type-names)))
        (singletons (remove-if-not (lambda (type) (= 1 (length (census-type-instances type))))
                                   (census-type-names))))
    (format t "~%  empty types (~D)~%" (length empties))
    (dolist (type empties)
      (format t "    ~(~A~)  ~A~@[  alias over ~(~{~A~^ ~}~)~]~%"
              type (census-type-provenance type) (gethash type *type-components*)))
    (format t "~%  singleton types (~D)~%" (length singletons))
    (dolist (type singletons)
      (format t "    ~(~A~) == ~(~A~)  ~A~%"
              type (first (census-type-instances type)) (census-type-provenance type)))))


(defun census-spec-type-names (spec)
  "The declared types an argument or parameter specification admits, flattening EITHER."
  (cond ((and (symbolp spec) (nth-value 1 (gethash spec *types*)))
         (list spec))
        ((and (consp spec) (eq (first spec) 'either))
         (loop for subspec in (rest spec) append (census-spec-type-names subspec)))
        (t nil)))


(defun census-spec-empty-p (spec)
  "True when SPEC names at least one declared type and every type it names is empty, so no
   binding of that position exists."
  (let ((types (census-spec-type-names spec)))
    (and types
         (every (lambda (type) (null (census-type-instances type))) types))))


(defun census-relation-entries (table kind)
  "Scans TABLE -- *RELATIONS* or *STATIC-RELATIONS* -- for relations with an uninstantiable
   argument position, returning (name kind position signature empty-types) for each.  The
   unary relation implied by every type name, and the two indices a bijective declaration
   installs beside its canonical name, are skipped: neither is a declared relation."
  (let ((entries nil))
    (loop for name being the hash-keys of table using (hash-value signature)
          do (unless (or (nth-value 1 (gethash name *types*))
                         (gethash name *bijective-canonical*)
                         (not (listp signature)))
               (loop for spec in signature
                     for position from 1
                     do (when (census-spec-empty-p spec)
                          (push (list name kind position signature
                                      (census-spec-type-names spec))
                                entries)))))
    (sort entries #'string< :key (lambda (entry) (symbol-name (first entry))))))


(defun report-uninstantiable-relations ()
  "Every declared relation with an argument position whose type specification admits no
   object.  Such a relation holds of nothing in any state: identically false, grade 1.  A
   fluent position naming no declared type -- a number, a list, a mode -- is not a type
   quantification and is passed over."
  (let ((entries (append (census-relation-entries *relations* "dynamic")
                         (census-relation-entries *static-relations* "static"))))
    (format t "~%  relations over an empty type (~D relation~:P, ~D position~:P)~%"
            (length (remove-duplicates (mapcar #'first entries))) (length entries))
    (dolist (entry entries)
      (format t "    ~(~A~)  ~A  position ~D of ~(~A~)  empty type~P ~(~A~)~%"
              (first entry) (second entry) (third entry) (fourth entry)
              (length (fifth entry)) (fifth entry)))))


(defun census-constant-sites (form)
  "Every quantifier in FORM whose domain the engine's own STATIC-EMPTY-QUANTIFIER-TRUTH
   decides statically, as (form truth).  The translator computes this to skip emitting the
   unreachable body; the census reports it, because the same fact is a grade-1 constraint
   and is invisible in the output otherwise."
  (let ((sites nil))
    (when (consp form)
      (let ((truth (static-empty-quantifier-truth form)))
        (when (member truth '(:true :false))
          (push (list form truth) sites)))
      (dolist (subform form)
        (setf sites (nconc sites (census-constant-sites subform)))))
    sites))


(defun census-fold (form visited)
  "FORM's truth value under the empty extents alone: :TRUE, :FALSE, or :UNKNOWN.  Three
   leaf rules -- a quantifier over an empty domain, a type test of an empty type, and a
   call to a query whose own body folds -- and the ordinary propagation through AND, OR
   and NOT.  VISITED carries the queries already entered, so a recursive query terminates.
   Anything else is :UNKNOWN, so a reported constant is one the extents force and not one
   the fold guessed."
  (unless (consp form)
    (return-from census-fold :unknown))
  (let ((quantified (static-empty-quantifier-truth form)))
    (when (member quantified '(:true :false))
      (return-from census-fold quantified)))
  (when (and (= (length form) 2)
             (symbolp (first form))
             (nth-value 1 (gethash (first form) *types*))
             (null (census-type-instances (first form))))
    (return-from census-fold :false))
  (case (first form)
    (and (let ((values (mapcar (lambda (term) (census-fold term visited)) (rest form))))
           (cond ((member :false values) :false)
                 ((every (lambda (value) (eq value :true)) values) :true)
                 (t :unknown))))
    (or (let ((values (mapcar (lambda (term) (census-fold term visited)) (rest form))))
          (cond ((member :true values) :true)
                ((and values (every (lambda (value) (eq value :false)) values)) :false)
                (t :unknown))))
    (not (case (census-fold (second form) visited)
           (:true :false)
           (:false :true)
           (t :unknown)))
    (t (if (and (symbolp (first form))
                (member (first form) *query-names*)
                (not (member (first form) visited)))
         (census-fold (get (first form) :raw-body) (cons (first form) visited))
         :unknown))))


(defun report-constant-predicates ()
  "Every query and update holding a quantifier over an empty domain, and every query whose
   whole body therefore folds to a constant.  This is the census's second payload: it is
   what lets a later extractor state, rather than assume, that an axiom's override
   disjunct is dead and its remaining aggregate decides the device."
  (let ((named (append (copy-list *query-names*) (copy-list *update-names*)))
        (sites nil)
        (constants nil))
    (dolist (name (sort named #'string< :key #'symbol-name))
      (dolist (site (census-constant-sites (get name :raw-body)))
        (push (cons name site) sites))
      (let ((value (census-fold (get name :raw-body) nil)))
        (unless (eq value :unknown)
          (push (cons name value) constants))))
    (setf sites (nreverse sites))
    (setf constants (nreverse constants))
    (format t "~%  constant quantifier sites (~D)~%" (length sites))
    (dolist (site sites)
      (format t "    ~(~A~)  ~(~A ~A~)  ->  ~(~A~)~%"
              (first site) (first (second site)) (second (second site)) (third site)))
    (format t "~%  constant predicates (~D)~%" (length constants))
    (dolist (constant constants)
      (format t "    ~(~A~) == ~(~A~)~%" (car constant) (cdr constant)))))


(defun report-type-extent-census ()
  "S0, grade 1.  The type extent census: every declared type with its cardinality, the
   empty and singleton types called out, the relations an empty type makes identically
   false, and the query and update sites an empty type makes constant.  Grade 1 because a
   declared type's extent is fixed by the specification: no action creates or destroys an
   object, so a predicate quantified over an empty type is constant in every syntactically
   well-formed state, reachable or not.  Step 0 of the profile: the emptiness facts every
   later extractor needs before it may read an aggregate as an axiom."
  (format t "~2%S0  TYPE EXTENT CENSUS  [grade 1]~%")
  (format t "~A~%" (make-string 62 :initial-element #\-))
  (report-type-extent-table)
  (report-empty-and-singleton-types)
  (report-uninstantiable-relations)
  (report-constant-predicates)
  (values))


(defun control-facts ()
  "Every (controls <clauses> <device> <mode>) proposition in the static database, sorted
   by device name.  LIST-STATIC-DB reinserts the fluent values at their declared indices,
   so each proposition comes back in declared argument order."
  (sort (loop for fact in (list-static-db)
              when (and (consp fact) (eq (first fact) 'controls))
                collect (copy-list fact))
        #'string<
        :key (lambda (fact) (symbol-name (third fact)))))


(defun axiom-reading-text (reduction)
  "What the reduction licenses, in words, since the keyword alone invites misreading."
  (case reduction
    (:aggregate "state == aggregate")
    (:negated-aggregate "state == not aggregate")
    (:true "state constant TRUE -- the aggregate does not decide it")
    (:false "state constant FALSE -- the aggregate does not decide it")
    (t "state == override OR aggregate -- OVERRIDE LIVE, every claim below is conditional on it")))


(defun report-device-state-axioms (axioms)
  "G1's fix.  S1's control table is an aggregate over CONTROLS, and the aggregate is not
   the device's state: the update that asserts the state relation combines it with an
   override -- a gate ORs jamming in, a gears drive ANDs not-jammed in -- and the two
   readings coincide only where the override is dead.  This block reads each such update,
   reduces its condition with the aggregate held opaque, and says which reading the
   extents license.  Without it the table and the pairs are claims about CONTROL-ON
   wearing the name of the device state."
  (format t "~%  device state axioms (~D)~%" (length axioms))
  (dolist (axiom axioms)
    (format t "    ~(~A~) asserted by ~(~A~)~%" (first axiom) (second axiom))
    (format t "      condition  ~(~A~)~%" (third axiom))
    (format t "      aggregate  ~(~A~)~%" (fourth axiom))
    (format t "      reading    ~A~@[  premise: ~(~{~A~^, ~}~) empty~]~%"
            (axiom-reading-text (fifth axiom)) (sixth axiom))))


(defun form-calls-query-p (form name)
  "True when FORM calls the query NAME anywhere."
  (and (consp form)
       (or (eq (first form) name)
           (some (lambda (subform) (form-calls-query-p subform name)) form))))


(defun form-reads-relation-p (form relation)
  "True when RELATION appears in head position anywhere in FORM."
  (and (consp form)
       (or (eq (first form) relation)
           (some (lambda (subform) (form-reads-relation-p subform relation)) form))))


(defun census-query-reads-p (name relation visited)
  "True when the query NAME reads RELATION, following the queries it calls.  S1 ran this
   walk with CONTROLS hard-coded and S2 needs it over a placement relation, so it takes the
   relation instead of naming one.  VISITED carries the queries already entered, so a
   recursive query terminates."
  (unless (member name visited)
    (let ((body (get name :raw-body))
          (seen (cons name visited)))
      (or (form-reads-relation-p body relation)
          (loop for called in *query-names*
                thereis (and (form-calls-query-p body called)
                             (census-query-reads-p called relation seen)))))))


(defun control-aggregate-call (form visited)
  "The call inside FORM to a query that reads CONTROLS, directly or through the queries it
   calls.  That call IS the control aggregate; everything else in the condition is the
   override S1 was blind to."
  (when (consp form)
    (when (and (symbolp (first form))
               (member (first form) *query-names*)
               (census-query-reads-p (first form) 'controls visited))
      (return-from control-aggregate-call form))
    (dolist (subform form)
      (let ((found (control-aggregate-call subform visited)))
        (when found
          (return-from control-aggregate-call found)))))
  nil)


(defun reduce-conjunction (readings)
  "A conjunction is FALSE if any term is, and otherwise is whatever single term survives
   after the constantly-true ones are dropped."
  (if (member :false readings)
    :false
    (let ((survivors (remove :true readings)))
      (cond ((null survivors) :true)
            ((and (null (cdr survivors)) (member (first survivors) '(:aggregate :negated-aggregate)))
             (first survivors))
            (t :unknown)))))


(defun reduce-disjunction (readings)
  "A disjunction is TRUE if any term is, and otherwise is whatever single term survives
   after the constantly-false ones are dropped."
  (if (member :true readings)
    :true
    (let ((survivors (remove :false readings)))
      (cond ((null survivors) :false)
            ((and (null (cdr survivors)) (member (first survivors) '(:aggregate :negated-aggregate)))
             (first survivors))
            (t :unknown)))))


(defun census-reduce-condition (form aggregate)
  "FORM's reading with AGGREGATE held opaque and every other term folded by the extents:
   :AGGREGATE when the condition reduces to the aggregate alone -- the override is dead
   and the device state is what S1's table says -- :NEGATED-AGGREGATE, :TRUE, :FALSE, or
   :UNKNOWN when some term survives and the aggregate does not decide the state."
  (when (equal form aggregate)
    (return-from census-reduce-condition :aggregate))
  (unless (consp form)
    (return-from census-reduce-condition :unknown))
  (case (first form)
    (and (reduce-conjunction
           (mapcar (lambda (term) (census-reduce-condition term aggregate)) (rest form))))
    (or (reduce-disjunction
          (mapcar (lambda (term) (census-reduce-condition term aggregate)) (rest form))))
    (not (case (census-reduce-condition (second form) aggregate)
           (:aggregate :negated-aggregate)
           (:negated-aggregate :aggregate)
           (:true :false)
           (:false :true)
           (t :unknown)))
    (t (census-fold form nil))))


(defun census-form-empty-types (form aggregate visited)
  "The empty types FORM names outside AGGREGATE, following the queries it calls: the
   premise the reading rests on, so it is stated rather than assumed.  A problem
   populating any of them gets a different axiom and a different table."
  (unless (and (consp form) (not (equal form aggregate)))
    (return-from census-form-empty-types nil))
  (let ((types nil))
    (dolist (subform form)
      (cond ((and (symbolp subform)
                  (nth-value 1 (gethash subform *types*))
                  (null (census-type-instances subform)))
             (pushnew subform types))
            ((and (symbolp subform)
                  (member subform *query-names*)
                  (not (member subform visited)))
             (dolist (type (census-form-empty-types (get subform :raw-body)
                                                    aggregate
                                                    (cons subform visited)))
               (pushnew type types)))
            (t (dolist (type (census-form-empty-types subform aggregate visited))
                 (pushnew type types)))))
    (sort types #'string< :key #'symbol-name)))


(defun type-spec-extent (spec)
  "The constants a relation argument's type specification admits.  A fluent position names
   no type and admits nothing."
  (cond ((and (symbolp spec) (nth-value 1 (gethash spec *types*)))
         (gethash spec *types*))
        ((and (consp spec) (eq (first spec) 'either))
         (loop for subspec in (rest spec) append (type-spec-extent subspec)))
        (t nil)))


(defun relation-keys-a-device-p (relation devices)
  "True when some argument position of RELATION admits a controlled device."
  (loop for spec in (gethash relation *relations*)
        thereis (intersection devices (type-spec-extent spec))))


(defun update-state-axioms (form devices)
  "Walks FORM for the IF forms that assert a device's state relation under a condition
   mentioning the control aggregate, returning (relation condition aggregate reduction
   premise) for each."
  (let ((axioms nil))
    (when (consp form)
      (when (and (eq (first form) 'if) (consp (third form)))
        (let ((relation (first (third form)))
              (aggregate (control-aggregate-call (second form) nil)))
          (when (and aggregate
                     (gethash relation *derived-relations*)
                     (relation-keys-a-device-p relation devices))
            (push (list relation
                        (second form)
                        aggregate
                        (census-reduce-condition (second form) aggregate)
                        (census-form-empty-types (second form) aggregate nil))
                  axioms))))
      (dolist (subform form)
        (setf axioms (nconc axioms (update-state-axioms subform devices)))))
    axioms))


(defun device-state-axioms (facts)
  "Every (relation update condition aggregate reduction premise) an update supplies for a
   controlled device's state.  An axiom is an IF whose test calls a query that reads
   CONTROLS and whose consequent asserts a derived relation admitting a controlled device.
   Updates and their raw bodies are what staging installed; nothing here is named."
  (let ((devices (mapcar #'third facts))
        (axioms nil))
    (dolist (update *update-names*)
      (dolist (axiom (update-state-axioms (get update :raw-body) devices))
        (push (cons (first axiom) (cons update (rest axiom))) axioms)))
    (sort (nreverse axioms) #'string< :key (lambda (axiom) (symbol-name (first axiom))))))


(defun control-boolean-form (clauses mode)
  "The device's Boolean function.  An empty clause list is FALSE and a list holding one
   empty clause is TRUE; the substrate treats the two as distinct and so does this."
  (let ((dnf (cond ((null clauses) 'false)
                   ((and (null (cdr clauses)) (null (first clauses))) 'true)
                   ((null (cdr clauses))
                    (if (null (cdr (first clauses)))
                      (first (first clauses))
                      (cons 'and (first clauses))))
                   (t (cons 'or (loop for clause in clauses
                                      collect (if (null (cdr clause))
                                                (first clause)
                                                (cons 'and clause))))))))
    (if (eq mode 'inverted)
      (list 'not dnf)
      dnf)))


(defun report-control-table (facts)
  "Step 1.  Each device as a Boolean function of its primitive controllers: the DNF
   aggregate over its clauses, negated under INVERTED."
  (format t "~%  control table (~D entries)~%" (length facts))
  (dolist (fact facts)
    (format t "    ~(~A~) == ~(~A~)~%"
            (third fact)
            (control-boolean-form (second fact) (fourth fact)))))


(defun uncovered-control-devices (axioms)
  "The controlled devices no reported axiom's relation admits.  The premise licenses the
   state reading only for the devices actually covered, so the gap is printed rather than
   left to the reader."
  (let ((devices (mapcar #'third (control-facts))))
    (remove-if (lambda (device)
                 (loop for axiom in axioms
                       thereis (relation-keys-a-device-p (first axiom) (list device))))
               devices)))


(defun report-pair-qualification (axioms)
  "Whether the pairs above are claims about device state or only about the aggregate.  A
   pair follows from a shared clause set, which decides the AGGREGATE; it decides the
   STATE only where every override is dead.  G1: on a jammer-bearing problem the same
   clause sets yield the same pairs and the pairs are false."
  (let ((live (remove-if (lambda (axiom) (eq (fifth axiom) :aggregate)) axioms))
        (premise (remove-duplicates (loop for axiom in axioms append (copy-list (sixth axiom))))))
    (format t "~%  pair qualification~%")
    (cond ((null axioms)
           (format t "    no device state axiom was found, so the pairs are claims about ~
                      the control aggregate and NOT about device state.~%"))
          (live
           (format t "    CONDITIONAL.  ~D of ~D state axioms keep a live override ~(~{~A~^, ~}~); ~
                      the pairs hold only where those overrides are inactive.~%"
                   (length live) (length axioms) (mapcar #'first live)))
          (t
           (format t "    UNCONDITIONAL, on a stated premise.  All ~D device state axioms ~
                      reduce to their control aggregate because ~(~{~A~^, ~}~) ~
                      ~:[are~;is~] empty, so the table and the pairs are claims about ~
                      device state.  Populate ~:[any of those types~;that type~] and both ~
                      become false.~%"
                   (length axioms) (sort premise #'string< :key #'symbol-name)
                   (null (cdr premise)) (null (cdr premise)))))
    (dolist (device (uncovered-control-devices axioms))
      (format t "    NOT COVERED: ~(~A~) is controlled but no update asserts a state ~
                 relation for it under the aggregate; its row is aggregate only.~%"
              device))))


(defun clause-set-key (clauses)
  "A canonical reading of a DNF clause list: every clause name-sorted and duplicate-free,
   the clause list likewise, so two devices wired to the same controllers compare EQUAL
   however their init facts happened to be written."
  (sort (remove-duplicates
          (loop for clause in clauses
                collect (sort (remove-duplicates (copy-list clause))
                              #'string< :key #'symbol-name))
          :test #'equal)
        #'string<
        :key (lambda (clause) (format nil "~{~A~^,~}" clause))))


(defun report-control-pairs (facts axioms)
  "Step 2.  Devices wired to an identical clause set.  Same mode gives an EQUIVALENCE
   pair -- the two are in the same state in every well-formed state.  Opposite mode gives
   an EXCLUSION pair -- never both active, and in fact exactly one of them always is."
  (let ((exclusions nil)
        (equivalences nil))
    (loop for (fact . rest) on facts
          do (dolist (other rest)
               (when (equal (clause-set-key (second fact))
                            (clause-set-key (second other)))
                 (if (eq (fourth fact) (fourth other))
                   (push (list (third fact) (third other) (second fact)) equivalences)
                   (push (list (third fact) (third other) (second fact)) exclusions)))))
    (setf exclusions (nreverse exclusions))
    (setf equivalences (nreverse equivalences))
    (format t "~%  exclusion pairs (~D)~%" (length exclusions))
    (dolist (pair exclusions)
      (format t "    {~(~A~), ~(~A~)}  on ~(~A~)~%" (first pair) (second pair) (third pair)))
    (format t "~%  equivalence pairs (~D)~%" (length equivalences))
    (dolist (pair equivalences)
      (format t "    {~(~A~), ~(~A~)}  on ~(~A~)~%" (first pair) (second pair) (third pair)))
    (report-pair-qualification axioms)))


(defun control-device-depth (device facts tiers path)
  "DEVICE's depth: one plus the deepest contribution of its primitives.  Returns the depth
   and, when the recursion re-enters a device already on PATH, that cycle."
  (if (member device path)
    (values 0 (reverse (cons device path)))
    (let ((fact (find device facts :key #'third))
          (deepest 0)
          (cycle nil))
      (dolist (clause (second fact))
        (dolist (primitive clause)
          (cond ((find primitive facts :key #'third)
                 (multiple-value-bind (sub-depth sub-cycle)
                     (control-device-depth primitive facts tiers (cons device path))
                   (when sub-cycle
                     (setf cycle sub-cycle))
                   (setf deepest (max deepest sub-depth))))
                ((eq (cdr (assoc primitive tiers)) :device-mediated)
                 (setf deepest (max deepest 1))))))
      (values (1+ deepest) cycle))))


(defun conjunction-status-pairs (conjuncts)
  "For every variable the conjunction tests with both a type predicate and a dynamic
   relation, the pair (type . relation)."
  (let ((typed nil)
        (related nil)
        (pairs nil))
    (dolist (conjunct conjuncts)
      (when (and (consp conjunct)
                 (symbolp (first conjunct))
                 (= (length conjunct) 2)
                 (symbolp (second conjunct)))
        (cond ((nth-value 1 (gethash (first conjunct) *types*))
               (push (cons (second conjunct) (first conjunct)) typed))
              ((nth-value 1 (gethash (first conjunct) *relations*))
               (push (cons (second conjunct) (first conjunct)) related)))))
    (dolist (type-entry typed)
      (dolist (relation-entry related)
        (when (eq (car type-entry) (car relation-entry))
          (push (cons (cdr type-entry) (cdr relation-entry)) pairs))))
    (nreverse pairs)))


(defun collect-controller-status-pairs (form visited)
  "Walks FORM and the body of every query it calls, collecting the status pairs of each
   conjunction that tests one variable's type and that same variable's dynamic state."
  (let ((pairs nil))
    (when (consp form)
      (when (eq (first form) 'and)
        (setf pairs (nconc pairs (conjunction-status-pairs (rest form)))))
      (when (and (symbolp (first form))
                 (member (first form) *query-names*)
                 (not (gethash (first form) visited)))
        (setf (gethash (first form) visited) t)
        (setf pairs (nconc pairs (collect-controller-status-pairs
                                   (get (first form) :raw-body) visited))))
      (dolist (subform form)
        (setf pairs (nconc pairs (collect-controller-status-pairs subform visited)))))
    pairs))


(defun control-status-relation-map ()
  "Pairs (type . relation) taken from the queries that evaluate CONTROLS: wherever such a
   query tests a controller's type and, in the same conjunction, a dynamic relation of
   that same variable, that relation is the controller kind's energizing state.  Derived
   this way rather than named, so the extractor carries no substrate vocabulary and no
   problem vocabulary."
  (let ((pairs nil)
        (visited (make-hash-table :test #'eq)))
    (dolist (name *query-names*)
      (when (form-reads-relation-p (get name :raw-body) 'controls)
        (setf pairs (nconc pairs (collect-controller-status-pairs
                                   (get name :raw-body) visited)))))
    (remove-duplicates pairs :test #'equal :from-end t)))


(defun object-type-names (object)
  "Every declared type whose extent contains OBJECT."
  (loop for type being the hash-keys of *types* using (hash-value constants)
        when (member object constants)
          collect type))


(defun control-status-relation (primitive)
  "The dynamic relation saying whether PRIMITIVE is energized, found by matching the
   primitive's declared types against the (type . relation) pairs harvested from the
   substrate's own controller evaluator."
  (let ((types (object-type-names primitive)))
    (cdr (find-if (lambda (pair) (member (first pair) types))
                  (control-status-relation-map)))))


(defun report-control-dag-notes (tiers)
  "The two-tier rule's own limit, stated in the output rather than left to the reader.
   S1 records that a device-mediated primitive adds a level; WHICH device supplies that
   level is a question about geometry, not about the control algebra, and is deferred.  A
   relation-level reading would answer it by naming every device of the mediating kind,
   which merges the genuine dependency into a spurious cycle."
  (format t "~%  notes~%")
  (let ((mediated (remove-if-not (lambda (entry) (eq (cdr entry) :device-mediated)) tiers))
        (unidentified (remove-if-not (lambda (entry) (eq (cdr entry) :unknown)) tiers)))
    (when (null mediated)
      (format t "    every primitive is ground: the DAG is flat and S1 resolves it fully.~%"))
    (dolist (entry mediated)
      (format t "    ~(~A~) is device-mediated through its status relation ~(~A~), which ~
                 an update derives from another device's derived output.  The extra level ~
                 is recorded; the identity of the supplying device is not decidable from ~
                 the control algebra and is deferred to the sightline extractor.~%"
              (car entry) (control-status-relation (car entry))))
    (dolist (entry unidentified)
      (format t "    ~(~A~) has no identifiable status relation; its tier is undetermined ~
                 and its device counts only its resolvable primitives.~%"
              (car entry)))))


(defun control-primitives (facts)
  "Every distinct primitive controller named in any clause, sorted by name."
  (sort (remove-duplicates
          (loop for fact in facts
                append (loop for clause in (second fact)
                             append (copy-list clause))))
        #'string< :key #'symbol-name))


(defun control-update-mediates-p (update relation devices)
  "True when UPDATE writes RELATION and reads at least one OTHER derived relation keyed by
   a controlled device."
  (multiple-value-bind (reads writes) (propagation-relation-sets update)
    (and (gethash relation writes)
         (loop for read being the hash-keys of reads
               thereis (and (not (eq read relation))
                            (gethash read *derived-relations*)
                            (relation-keys-a-device-p read devices))))))


(defun control-relation-device-mediated-p (relation devices)
  "True when the update maintaining RELATION reads some controlled device's derived
   output.  RELATION is then not a ground fact about the world but a consequence of what
   other devices are doing, which puts the primitive it energizes one level up."
  (and (gethash relation *derived-relations*)
       (loop for update in *update-names*
             thereis (and (propagation-candidate-p update)
                          (control-update-mediates-p update relation devices)))))


(defun control-primitive-tier (primitive devices)
  "GROUND, DEVICE-MEDIATED, or UNKNOWN, by what maintains the primitive's status relation."
  (let ((relation (control-status-relation primitive)))
    (cond ((null relation) :unknown)
          ((control-relation-device-mediated-p relation devices) :device-mediated)
          (t :ground))))


(defun report-control-dag (facts)
  "Step 3.  The dependency DAG under the TWO-TIER rule.  A device depends on its primitive
   controllers.  A primitive is GROUND when the relation that energizes it is not derived
   from any device's output -- a plate follows occupancy, a switch follows its own toggle
   -- and DEVICE-MEDIATED when it is, as a receiver following a beam that other devices
   gate.  A ground primitive contributes nothing to depth, a device-mediated one
   contributes a level, and a primitive that is itself a controlled device recurses."
  (let* ((devices (mapcar #'third facts))
         (tiers (loop for primitive in (control-primitives facts)
                      collect (cons primitive (control-primitive-tier primitive devices))))
         (cycles nil))
    (format t "~%  primitive controllers (~D)~%" (length tiers))
    (dolist (entry tiers)
      (format t "    ~(~A~)  ~(~A~)  status relation ~(~A~)~%"
              (car entry) (cdr entry)
              (or (control-status-relation (car entry)) "unidentified")))
    (format t "~%  device depth~%")
    (dolist (device devices)
      (multiple-value-bind (depth cycle) (control-device-depth device facts tiers nil)
        (when cycle
          (pushnew cycle cycles :test #'equal))
        (format t "    ~(~A~)  depth ~D~@[  on cycle ~A~]~%" device depth cycle)))
    (format t "~%  cycles: ~A~%" (if cycles (length cycles) "none"))
    (dolist (cycle cycles)
      (format t "    ~(~{~A~^ -> ~}~)~%" cycle))
    (report-control-dag-notes tiers)))


(defun report-control-algebra ()
  "S1, grade 1.  The control algebra: one Boolean function per controlled device, the
   exclusion and equivalence pairs that follow from devices sharing a clause set, and the
   device dependency DAG with its depths and cycles.  Grade 1 because every claim here
   follows from the axioms defining the derived device states plus static instance facts,
   so it holds in every syntactically well-formed state, reachable or not."
  (let* ((facts (control-facts))
         (axioms (device-state-axioms facts)))
    (format t "~2%S1  CONTROL ALGEBRA  [grade 1]~%")
    (format t "~A~%" (make-string 62 :initial-element #\-))
    (if (null facts)
      (format t "  no CONTROLS facts in the static database.~%")
      (progn (report-control-table facts)
             (report-device-state-axioms axioms)
             (report-control-pairs facts axioms)
             (report-control-dag facts)))
    (values)))


(defun census-key-text (entry)
  "ENTRY's key positions, or the reason it has none.  A bijective declaration keys both
   directions at once; a relation whose every position is fluent keys nothing at all and is
   a global fluent.  Both print an empty key list and they are not the same fact, which is
   what the first version of this line got wrong."
  (cond ((fifth entry) (format nil "~A" (fifth entry)))
        ((gethash (first entry) *bijective-relations*)
         "none -- bijective, functional both ways")
        (t "none -- keyless global fluent")))


(defun report-functional-relations (entries)
  "Step 1.  Every declared relation carrying a fluent position, with its key and its value
   positions.  Such a relation is a partial function from its key tuple to its value tuple,
   and that is a property of the DECLARATION rather than of any state: no action can make
   one key carry two values, because the engine stores the value at the key.  Every counting
   argument below rests on this table and on nothing else."
  (format t "~%  functional relations (~D: ~D dynamic, ~D static)~%"
          (length entries)
          (count "dynamic" entries :key #'second :test #'string=)
          (count "static" entries :key #'second :test #'string=))
  (format t "    ~D bijective index relation~:P excluded, as S0 excludes them~%"
          (hash-table-count *bijective-canonical*))
  (dolist (entry entries)
    (format t "    ~(~A~)  ~A  key ~A  value ~A  signature ~(~A~)~%"
            (first entry) (second entry) (census-key-text entry)
            (fourth entry) (third entry))))


(defun census-functional-entries (table kind)
  "Scans TABLE -- *RELATIONS* or *STATIC-RELATIONS* -- for relations the engine keys."
  (let ((entries nil))
    (loop for name being the hash-keys of table using (hash-value signature)
          do (let ((fluents (gethash name *fluent-relation-indices*)))
               (when (and fluents
                          (listp signature)
                          (not (nth-value 1 (gethash name *types*)))
                          (not (gethash name *bijective-canonical*)))
                 (push (list name kind signature fluents
                             (loop for position from 1 to (length signature)
                                   unless (member position fluents)
                                     collect position))
                       entries))))
    entries))


(defun functional-relation-entries ()
  "Every declared relation with at least one fluent position, as
   (name kind signature value-positions key-positions).  The fluent positions are the value
   and the rest are the key.  A bijective declaration keys both directions, so its canonical
   name has every position fluent and an empty key; that is reported as it stands rather than
   forced into a key.  The unary relation implied by every type name, and the two indices a
   bijective declaration installs beside its canonical name, are skipped exactly as S0 skips
   them, so the two extractors do not disagree about what a declared relation is."
  (sort (append (census-functional-entries *relations* "dynamic")
                (census-functional-entries *static-relations* "static"))
        #'string< :key (lambda (entry) (symbol-name (first entry)))))


(defun placement-relations (entries)
  "Steps 2 and 3's input: the functional relations shaped like an occupancy -- exactly one
   key position, exactly one value position, and both positions naming declared types.  Such
   a relation assigns each member of its key pool at most one member of its value pool, which
   is the only premise the cardinality bound needs.  Found BY SHAPE and not by name: the
   support occupancy of a physical substrate is one instance and the extractor carries no
   vocabulary that says so.  Returned as (name kind key-position value-position key-spec
   value-spec)."
  (let ((placements nil))
    (dolist (entry entries (nreverse placements))
      (when (and (= 1 (length (fourth entry))) (= 1 (length (fifth entry))))
        (let ((key-spec (nth (1- (first (fifth entry))) (third entry)))
              (value-spec (nth (1- (first (fourth entry))) (third entry))))
          (when (and (census-spec-type-names key-spec) (census-spec-type-names value-spec))
            (push (list (first entry) (second entry)
                        (first (fifth entry)) (first (fourth entry))
                        key-spec value-spec)
                  placements)))))))


(defun census-layer-class (object pairs)
  "LIVE when OBJECT has a counterpart, GHOST when it is one, UNPAIRED when no layer-pair
   relation mentions it.  An unpaired object is one object rather than a copy of anything, so
   it is present in every layer's view and is counted in every layer's bound."
  (cond ((assoc object pairs) "live")
        ((rassoc object pairs) "ghost")
        (t "unpaired")))


(defun census-spec-extent (spec)
  "The constants SPEC admits, taken through CENSUS-TYPE-INSTANCES so an EITHER alias whose
   components are all empty reads as empty rather than as the one-element list holding its
   sentinel.  S1's TYPE-SPEC-EXTENT reads *TYPES* raw and is left alone: S2 must not move an
   S1 number."
  (remove-duplicates
    (loop for type in (census-spec-type-names spec)
          append (copy-list (census-type-instances type)))))


(defparameter *census-printed-pools* nil
  "The pool specifications already listed in full by the running S2 report.  A pool shared
   by several placement relations is counted every time and listed once.")


(defun report-pool (label spec pairs)
  "SPEC's extent, its per-layer counts, and its members each tagged with its layer."
  (let* ((members (sort (copy-list (census-spec-extent spec)) #'string< :key #'symbol-name))
         (classes (mapcar (lambda (object) (census-layer-class object pairs)) members)))
    (format t "        ~A pool ~(~A~) (~D): ~D live, ~D ghost, ~D unpaired~%"
            label spec (length members)
            (count "live" classes :test #'string=)
            (count "ghost" classes :test #'string=)
            (count "unpaired" classes :test #'string=))
    (cond ((member spec *census-printed-pools* :test #'equal)
           (format t "              members listed above~%"))
          (members
           (push spec *census-printed-pools*)
           (format t "              ~(~{~A~^ ~}~)~%"
                   (mapcar (lambda (object class) (format nil "~A[~A]" object class))
                           members classes))))))


(defun report-occupancy-pools (placements pairs)
  "Steps 2, 3 and the vocabulary step 5 needs.  For each placement relation, the pool its key
   ranges over and the pool its value ranges over -- the declared union intersected with the
   extents S0 computed -- each split by interaction layer and each member tagged.  Objects
   lying in BOTH pools are called out, since an occupant that is itself a support is what
   makes a stack possible and what makes the bound a bound on supports rather than on
   objects."
  (setf *census-printed-pools* nil)
  (format t "~%  occupancy pools (~D placement relation~:P; ~D layer pair~:P)~%"
          (length placements) (length pairs))
  (dolist (pair pairs)
    (format t "    layer pair: ~(~A~) -> ~(~A~)~%" (car pair) (cdr pair)))
  (dolist (placement placements)
    (format t "    ~(~A~)  ~A  ~(~A~) at ~D  ->  ~(~A~) at ~D~%"
            (first placement) (second placement)
            (fifth placement) (third placement)
            (sixth placement) (fourth placement))
    (report-pool "key  " (fifth placement) pairs)
    (report-pool "value" (sixth placement) pairs)
    (let ((both (intersection (census-spec-extent (fifth placement))
                              (census-spec-extent (sixth placement)))))
      (when both
        (format t "        in both pools (~D): ~(~{~A~^ ~}~)~%"
                (length both)
                (sort (copy-list both) #'string< :key #'symbol-name))))))


(defun layer-pair-relations ()
  "Every bijective relation whose two argument positions admit exactly the same constants.
   Such a relation pairs each member of one copy of the world with its counterpart in
   another, so it is what splits a pool into interaction layers.  A bijective relation
   between DIFFERENT pools -- a holder and its cargo, say -- pairs nothing and is not one of
   these.  Found by shape, so the extractor names no substrate relation."
  (loop for name being the hash-keys of *bijective-relations*
        for signature = (or (gethash name *relations*) (gethash name *static-relations*))
        when (and (listp signature)
                  (= 2 (length signature))
                  (census-spec-type-names (first signature))
                  (null (set-exclusive-or (census-spec-extent (first signature))
                                          (census-spec-extent (second signature)))))
          collect name))


(defun layer-pairs ()
  "Every (live . ghost) pair a layer-pair relation states, read from the static database.
   Read from the RELATION and never from the naming convention that generated it: a problem
   is free to name its copies as it likes, and on one that names them otherwise a convention
   reader would silently report a single layer.  Facts stored under a bijective relation's
   index names are resolved back to their canonical relation and deduplicated, since
   LIST-STATIC-DB returns both indices in declared argument order."
  (let ((relations (layer-pair-relations))
        (pairs nil))
    (dolist (fact (list-static-db) (nreverse pairs))
      (when (and (consp fact)
                 (= 3 (length fact))
                 (member (or (car (gethash (first fact) *bijective-canonical*)) (first fact))
                         relations))
        (pushnew (cons (second fact) (third fact)) pairs :test #'equal)))))


(defun placement-assignments (placement)
  "The (key . value) pairs PLACEMENT's relation holds in *START-STATE*, read at the relation's
   declared key and value positions rather than at positions 1 and 2."
  (loop for fact in (database *start-state*)
        when (and (consp fact) (eq (first fact) (first placement)))
          collect (cons (nth (third placement) fact) (nth (fourth placement) fact))))


(defun placement-test-query (form relation)
  "The query FORM calls that reads RELATION, directly or through the queries that query
   calls, or NIL when it calls none."
  (when (consp form)
    (when (and (symbolp (first form))
               (member (first form) *query-names*)
               (census-query-reads-p (first form) relation nil))
      (return-from placement-test-query (first form)))
    (dolist (subform form)
      (let ((found (placement-test-query subform relation)))
        (when found
          (return-from placement-test-query found)))))
  nil)


(defun census-asserted-relations (form)
  "Every declared dynamic relation appearing in head position anywhere in FORM.  The unary
   relation implied by a type name is skipped, as everywhere else in this file."
  (let ((relations nil))
    (when (consp form)
      (when (and (symbolp (first form))
                 (nth-value 1 (gethash (first form) *relations*))
                 (not (nth-value 1 (gethash (first form) *types*))))
        (pushnew (first form) relations))
      (dolist (subform form)
        (dolist (relation (census-asserted-relations subform))
          (pushnew relation relations))))
    relations))


(defun update-placement-consumers (form relation)
  "Walks FORM for IF forms whose test calls a query reading RELATION, returning
   (asserted-relation query) for every relation either branch touches.  Both branches count:
   a relation denied when the test fails is governed by that test as surely as one asserted
   when it holds."
  (let ((consumers nil))
    (when (consp form)
      (when (eq (first form) 'if)
        (let ((query (placement-test-query (second form) relation)))
          (when query
            (dolist (asserted (census-asserted-relations (cddr form)))
              (push (list asserted query) consumers)))))
      (dolist (subform form)
        (setf consumers (nconc consumers (update-placement-consumers subform relation)))))
    consumers))


(defun placement-consumers (placement)
  "Every (relation update query derived-p) an update asserts under an IF whose test calls a
   query reading PLACEMENT's relation.  Every relation a branch touches is reported, derived
   or not: restricting to derived relations is the S1-shaped reading and it would drop a
   toggle latch that flips on the occupancy edge, which is a consumer by any honest account."
  (let ((consumers nil))
    (dolist (update *update-names*)
      (dolist (consumer (update-placement-consumers (get update :raw-body) (first placement)))
        (pushnew (list (first consumer) update (second consumer)
                       (and (gethash (first consumer) *derived-relations*) t))
                 consumers :test #'equal)))
    (sort consumers #'string< :key (lambda (consumer) (symbol-name (first consumer))))))


(defun census-form-mentions-p (form symbol)
  "True when SYMBOL occurs anywhere in FORM, in any position."
  (if (consp form)
    (some (lambda (subform) (census-form-mentions-p subform symbol)) form)
    (eq form symbol)))


(defun placement-sibling-sites (form relation key-position)
  "Every (variable terms) in FORM where a term (RELATION ...) carries a ?-variable at
   KEY-POSITION, with TERMS the other members of the enclosing AND or OR that mention that
   same variable.  A read with no Boolean around it restricts the key by nothing and is
   reported as a site with no terms: that is the layer-blind case, and it is the one whose
   silence G6 was written to break."
  (let ((sites nil))
    (when (consp form)
      (dolist (subform form)
        (when (and (consp subform)
                   (eq (first subform) relation)
                   (?varp (nth key-position subform)))
          (push (list (nth key-position subform)
                      (when (member (first form) '(and or))
                        (remove-if-not (lambda (term)
                                         (and (not (eq term subform))
                                              (census-form-mentions-p term
                                                                      (nth key-position subform))))
                                       (rest form))))
                sites)))
      (dolist (subform form)
        (setf sites (nconc sites (placement-sibling-sites subform relation key-position)))))
    sites))


(defun placement-consumer-sites (query placement visited)
  "Every place QUERY's own body, or a body it calls, reads PLACEMENT's relation directly, as
   (query variable terms).  A consumer may reach the relation at several sites with different
   restrictions -- the beam queries reach it through the elevation of whatever a body rests
   on -- so each site is reported rather than the first one found."
  (unless (member query visited)
    (let ((sites nil)
          (seen (cons query visited)))
      (dolist (site (placement-sibling-sites (get query :raw-body)
                                             (first placement) (third placement)))
        (push (cons query site) sites))
      (dolist (called *query-names*)
        (when (and (form-calls-query-p (get query :raw-body) called)
                   (census-query-reads-p called (first placement) nil))
          (setf sites (nconc sites (placement-consumer-sites called placement seen)))))
      sites)))


(defun census-term-reads-layer-p (form pair-relations)
  "True when FORM reads a layer-pair relation, directly or through the queries it calls."
  (loop for relation in pair-relations
          thereis (or (form-reads-relation-p form relation)
                      (loop for called in *query-names*
                            thereis (and (form-calls-query-p form called)
                                         (census-query-reads-p called relation nil))))))


(defun census-call-parameter (term variable)
  "The parameter VARIABLE is bound to in the query TERM calls, by position against the flat
   parameter list the engine stores at :RAW-ARGS."
  (let ((position (position variable (rest term))))
    (when position
      (nth position (get (first term) :raw-args)))))


(defun census-callee-switch-terms (term variable visited)
  "The switches inside the query TERM calls, searched against the parameter that the tracked
   VARIABLE is passed as.  Each is returned tagged with the callee, so a reader sees how far
   the switch sits from the test it governs."
  (let* ((query (first term))
         (parameter (census-call-parameter term variable)))
    (when parameter
      (loop for switch in (census-switch-terms (list (get query :raw-body))
                                               parameter
                                               (cons query visited))
            collect (cons (car switch) (or (cdr switch) query))))))


(defun census-switch-terms (terms variable visited)
  "Every (term . query) inside TERMS that does not mention VARIABLE: the switch a
   state-selected restriction turns on, with the query it was found in or NIL when it was
   local.  A Boolean or an IF is descended into rather than rejected, because the switch sits
   BESIDE the class test inside a branch rather than around the whole of it.  G7: a term that
   CALLS A QUERY is descended into as well, against the parameter the tracked variable maps
   to, because the class test and its switch can both live inside the callee -- which is where
   the first version of this search printed FIXED over a restriction that varies.  The
   engine's :RAW-ARGS holds the flat parameter list and says in its own comment that it is
   kept for an interprocedural walk, so the mapping costs nothing."
  (let ((switches nil))
    (dolist (term terms (nreverse switches))
      (when (consp term)
        (cond ((member (first term) '(and or not if))
               (dolist (found (census-switch-terms (rest term) variable visited))
                 (pushnew found switches :test #'equal)))
              ((and (symbolp (first term))
                    (member (first term) *query-names*)
                    (not (member (first term) visited))
                    (census-form-mentions-p term variable))
               (dolist (found (census-callee-switch-terms term variable visited))
                 (pushnew found switches :test #'equal)))
              ((not (census-form-mentions-p term variable))
               (pushnew (cons term nil) switches :test #'equal)))))))


(defun placement-site-restriction (site pair-relations)
  "SITE's restriction on its key variable, as (kind switch-terms).  :NONE when nothing stands
   beside the read.  :LAYER-STATIC when the terms beside it test the key against a layer-pair
   relation and nothing else varies.  :LAYER-STATE-SELECTED when the same Boolean also holds
   terms that do not mention the key -- those terms are the switch, and they are named, because
   a class chosen at run time is not a property of the partition.  :OTHER when the terms
   restrict the key by something that is not a layer.  Both kinds carry their terms in the
   same (term . query) shape, the query being NIL where there is none, so one printer serves
   both: two shapes behind one accessor is what garbled the :OTHER line once."
  (let ((terms (second site)))
    (cond ((null terms) (list :none nil))
          ((notany (lambda (term) (census-term-reads-layer-p term pair-relations)) terms)
           (list :other (mapcar (lambda (term) (cons term nil)) terms)))
          (t (let ((switches (census-switch-terms terms (first site) nil)))
               (if switches
                 (list :layer-state-selected switches)
                 (list :layer-static nil)))))))


(defun census-restriction-text (kind)
  "A restriction kind in words, since the kind is the whole finding."
  (case kind
    (:none "NONE -- layer-blind, it reads every occupant")
    (:layer-static "LAYER, no switch found -- see the note on what the walk covers")
    (:layer-state-selected "LAYER, STATE-SELECTED")
    (t "OTHER -- restricted by something that is not a layer")))


(defun census-consumer-witnesses (restrictions live ghost total)
  "How many occupants a consumer can actually witness with, given its sites' restrictions.
   The WEAKEST site decides (A14): a consumer that is layer-blind anywhere reads the whole
   pool somewhere.  A state-selected class cannot be named statically, so both classes are
   reported and the larger bounds it."
  (let ((kinds (mapcar #'first restrictions)))
    (cond ((null kinds) "none -- the relation is never read")
          ((member :none kinds) (format nil "~D -- the whole pool" total))
          ((member :other kinds) "not determined -- see the restricting terms above")
          ((member :layer-state-selected kinds)
           (if (= live ghost)
             (format nil "~D, either class -- the class is chosen at run time" live)
             (format nil "~D or ~D, whichever class is chosen at run time" live ghost)))
          ((= live ghost) (format nil "~D, one class, and the walk found no switch" live))
          (t (format nil "~D or ~D, one class, and the walk found no switch" live ghost)))))


(defun census-switch-text (switches)
  "Restricting terms, each with the query it was found in or NIL.  A term found locally
   prints alone; one found inside a callee prints with that callee's name, because the
   distance between a test and the thing that moves it is the finding G7 was opened for."
  (when switches
    (format nil "~{~A~^, ~}"
            (mapcar (lambda (switch)
                      (if (cdr switch)
                        (format nil "~(~A~) in ~(~A~)" (car switch) (cdr switch))
                        (format nil "~(~A~)" (car switch))))
                    switches))))


(defun report-placement-consumers (placement live ghost total)
  "G6's fix.  Every relation an update asserts under a test that reads PLACEMENT's relation,
   with the sites at which the read happens, each site's restriction on the key variable, and
   the witness count that restriction leaves.  A layer partition of bodies is not a layer
   partition of effects: one consumer here reads the whole pool and another has its class
   chosen at run time, and printing a per-layer number without its consumer invites the
   reading of the first as though it were the second."
  (let ((consumers (placement-consumers placement))
        (pair-relations (layer-pair-relations)))
    (format t "      consumers (~D relation~:P): the bound each one actually reads~%"
            (length consumers))
    (dolist (consumer consumers)
      (let* ((sites (remove-duplicates (placement-consumer-sites (third consumer) placement nil)
                                       :test #'equal :from-end t))
             (restrictions (mapcar (lambda (site)
                                     (placement-site-restriction (rest site) pair-relations))
                                   sites)))
        (format t "        ~(~A~)~A  asserted by ~(~A~)  via ~(~A~)~%"
                (first consumer)
                (if (fourth consumer) " [derived]" " [not derived]")
                (second consumer) (third consumer))
        (loop for site in sites
              for restriction in restrictions
              do (format t "          site in ~(~A~): ~A~@[ by ~(~A~)~]~%"
                         (first site)
                         (census-restriction-text (first restriction))
                         (census-switch-text (second restriction))))
        (format t "          witnesses it reads: ~A~%"
                (census-consumer-witnesses restrictions live ghost total))))
    (when (null consumers)
      (format t "        none: no update asserts anything under a test reading this relation~%"))
    (format t "      NOTE: assertion side only.  A query used in an action's precondition is not listed here.~%")
    (format t "      NOTE: the switch walk descends AND, OR, NOT, IF and the queries those call.  A switch inside any other form is not found, so \"no switch found\" is weaker than \"no switch\".~%")))


(defun report-placement-bound (placement pairs)
  "One placement's bound, the bodies behind it, the bound each CONSUMER actually reads, and
   -- reported apart, and labelled -- what *START-STATE* happens to hold.  The state reading
   is kept apart deliberately: a count taken from one state is a reading of that state, and
   printing it beside an invariant is the grade 3 for grade 2 confusion this method exists
   to avoid.  |unavailable| is left as a parameter because being carried, or not yet
   existing, is a state fact no static reading decides.
   G6: the layer split below is a split of BODIES.  It becomes a claim about EFFECTS only
   under a consumer, and the consumers here do not agree with one another, so the per-layer
   number is printed under each of them and never on its own."
  (let* ((keys (census-spec-extent (fifth placement)))
         (classes (mapcar (lambda (object) (census-layer-class object pairs)) keys))
         (live (+ (count "live" classes :test #'string=)
                  (count "unpaired" classes :test #'string=)))
         (ghost (+ (count "ghost" classes :test #'string=)
                   (count "unpaired" classes :test #'string=)))
         (assignments (placement-assignments placement)))
    (format t "    ~(~A~): ~(~A~) -> ~(~A~)~%"
            (first placement) (fifth placement) (sixth placement))
    (format t "      |occupied ~(~A~)| <= ~D - |unavailable ~(~A~)|~%"
            (sixth placement) (length keys) (fifth placement))
    (format t "      injective: the relation is keyed by its ~(~A~) argument, so two distinct occupied values need two distinct witnesses~%"
            (fifth placement))
    (format t "      BODIES by layer, not yet a claim about any predicate: live + unpaired ~D, ghost + unpaired ~D, total ~D~%"
            live ghost (length keys))
    (report-placement-consumers placement live ghost (length keys))
    (format t "      *start-state* reading, NOT the bound: ~D of ~D keys assigned, ~D distinct value~:P occupied~%"
            (length assignments) (length keys)
            (length (remove-duplicates (mapcar #'cdr assignments))))))


(defun report-cardinality-bounds (placements pairs)
  "Steps 4 and 5, and the reason the extractor exists.  The bound is emitted only for DYNAMIC
   placements: a static relation's extension cannot change, so its count is a reading and not
   a bound."
  (format t "~%  cardinality bounds  [grade 2: the keying is structural, so no action moves it]~%")
  (dolist (placement placements)
    (when (string= (second placement) "dynamic")
      (report-placement-bound placement pairs))))


(defun report-functional-relation-census ()
  "S2, grade 1 -> 2.  The functional-relation census: every declared relation the engine
   keys, the pools a placement relation maps between, their split by interaction layer, and
   the cardinality bound injectivity of the witness map gives.  Grade 1 in its first half --
   a relation's keying and a type's extent are fixed by the specification -- and grade 2 in
   the bound, which needs the induction that no action can change either.  S2 INHERITS S0:
   the extents, the empty and singleton lists and the provenance split are already computed,
   and the pools here are those extents intersected with what a placement relation admits."
  (let* ((functionals (functional-relation-entries))
         (placements (placement-relations functionals))
         (pairs (layer-pairs))
         (*print-pretty* nil))
    (format t "~2%S2  FUNCTIONAL-RELATION CENSUS  [grade 1 -> 2]~%")
    (format t "~A~%" (make-string 62 :initial-element #\-))
    (report-functional-relations functionals)
    (report-occupancy-pools placements pairs)
    (report-cardinality-bounds placements pairs)
    (values)))


;;; T6 -- Mechanized budget arithmetic
;;; Consumes S1 and S2 to emit the impossibility constraints AM1–AM3.
;;; AM1: A tight, disjoint support budget leaves no occupant outside a plate.
;;; AM2: A goal actor that leaves the occupant pool spends one support body.
;;; AM3: The remaining live and full-pool budgets bound the support cost that
;;;      must close in each segment.
;;; No problem object names anywhere (C1). Named substrate interfaces: ON,
;;; PRESSURE-PLATE, HAS-LOCATION, GOAL-FN (established as precedent in S4).


(defun budget-arithmetic-control-classes (facts)
  "S1-equivalent control facts grouped by mode and canonical clauses, in first-seen order."
  (let ((classes nil))
    (dolist (fact facts (mapcar #'cdr (nreverse classes)))
      (let* ((key (list (fourth fact) (clause-set-key (second fact))))
             (class (assoc key classes :test #'equal)))
        (if class
          (push fact (cdr class))
          (push (list key fact) classes))))))


(defun budget-arithmetic-body-cost-devices (facts)
  "Every S1 control class whose clause set names a pressure-plate. Returns
   (devices cost-function), DEVICES name-sorted, where cost-function is (PLATES...) with one
   plate per clause or (PLATES... :disjoint) when plates appear in different
   clauses, meaning independent supports. Classes with no body-cost are omitted."
  (let ((plates (census-type-instances 'pressure-plate))
        (devices nil))
    (dolist (class (budget-arithmetic-control-classes facts) (nreverse devices))
      (let ((members (sort (remove-duplicates (mapcar #'third class)) #'string< :key #'symbol-name))
            (clauses (second (first class))))
        (when clauses
          (let ((plate-set nil))
            (dolist (clause clauses)
              (dolist (primitive clause)
                (when (member primitive plates)
                  (pushnew primitive plate-set))))
            (when plate-set
              (let ((disjoint-p (loop for clause in clauses
                                       count (intersection clause plate-set) into matches
                                       finally (return (< matches (length clauses))))))
                (push (list members (if disjoint-p
                                     (append (sort (copy-list plate-set) #'string< :key #'symbol-name)
                                             '(:disjoint))
                                     (sort (copy-list plate-set) #'string< :key #'symbol-name)))
                      devices)))))))))


(defun budget-arithmetic-gate-costs (device-costs)
  "One existing T6 support cost per class in DEVICE-COSTS; returns (devices cost)."
  (mapcar (lambda (entry)
            (list (first entry) (length (second entry))))
          device-costs))


(defun budget-arithmetic-total-cost (gate-costs)
  "Sum of all gate costs."
  (let ((total 0))
    (dolist (entry gate-costs total)
      (incf total (second entry)))))


(defun budget-arithmetic-segment-occupancy ()
  "The occupant pool by segment: outside a cycle (live only) vs. inside a cycle
   (live + ghost).  Returns (outside-count inside-count) or NIL."
  (let* ((functionals (functional-relation-entries))
         (placements (placement-relations functionals))
         (on-placement (find 'on placements :key #'first)))
    (when (and on-placement (string= (second on-placement) "dynamic"))
      (let* ((keys (census-spec-extent (fifth on-placement)))
             (pairs (layer-pairs))
             (live-count (+ (count "live" (mapcar (lambda (obj) (census-layer-class obj pairs)) keys)
                                    :test #'string=)
                            (count "unpaired" (mapcar (lambda (obj) (census-layer-class obj pairs)) keys)
                                   :test #'string=))))
        (list live-count (length keys))))))


(defun budget-arithmetic-find-relation-call (form relation)
  "The first call to RELATION in FORM, or NIL."
  (cond ((atom form) nil)
        ((eq (first form) relation) form)
        (t (loop for item in form
                 for call = (budget-arithmetic-find-relation-call item relation)
                 when call return call))))


(defun budget-arithmetic-goal-actor-leaves-pool-p ()
  "Whether the goal moves an ON-pool body to a location without a pressure plate."
  (let* ((functionals (functional-relation-entries))
         (placements (placement-relations functionals))
         (on-placement (find 'on placements :key #'first))
         (goal-call (budget-arithmetic-find-relation-call (get 'goal-fn :form)
                                                           'has-location))
         (plates (census-type-instances 'pressure-plate)))
    (when (and on-placement goal-call)
      (let ((actor (second goal-call))
            (location (third goal-call))
            (pool (census-spec-extent (fifth on-placement))))
        (and (member actor pool)
             (not (find location
                        (list-static-db)
                        :test (lambda (place fact)
                                (and (eq (first fact) 'has-position)
                                     (member (second fact) plates)
                                     (eq (third fact) place))))))))))


(defun budget-arithmetic-disjoint-supports-p (device-costs)
  "Whether no pressure plate supplies two non-equivalent body-cost classes."
  (let ((supports (loop for entry in device-costs append (second entry))))
    (= (length supports) (length (remove-duplicates supports)))))


(defun budget-arithmetic-constraints (gate-costs occupancy goal-actor-leaves-pool-p
                                      supports-disjoint-p)
  "Derives AM1–AM3 from gate costs and occupancy.  Returns a list of constraint
   descriptions as strings."
  (let ((total (budget-arithmetic-total-cost gate-costs))
        (outside (first occupancy))
        (inside (second occupancy))
        (constraints nil))
    (when (and (integerp total) (integerp outside) (integerp inside)
               supports-disjoint-p)
      (when (= total inside)
        (push (format nil "AM1 [grade 1 -> 2; S1 controls, S2 ON pool]: Budget is tight.  ~
                           ~D ~A demand ~D total plate keepers; the full ~
                           occupant pool is ~D.  All can be open only if every body is on a plate."
                      (length gate-costs)
                      (if (some (lambda (entry) (cdr (first entry))) gate-costs)
                        "independent body-cost demands" "body-cost devices")
                      total inside)
              constraints))
      (when goal-actor-leaves-pool-p
        (push (format nil "AM2 [grade 1 -> 2; S1 controls, S2 ON pool, goal form]: ~
                           The goal actor occupies 1 of ~D bodies in the full occupant pool ~
                           and its destination has no pressure plate."
                      inside)
              constraints)
        (let ((outside-available (- outside 1)))
          (push (format nil "AM3a [grade 1 -> 2; S1 controls, S2 live ON pool]: ~
                             Outside a cycle, ~D bodies remain for supports; demand is ~D, ~
                             so at least ~D support cost must close."
                        outside-available total
                        (max 0 (- total outside-available)))
                constraints))
        (let ((inside-available (- inside 1)))
          (push (format nil "AM3b [grade 1 -> 2; S1 controls, S2 full ON pool]: ~
                             Inside a cycle, ~D bodies remain for supports; demand is ~D, ~
                             so at least ~D support cost must close."
                        inside-available total
                        (max 0 (- total inside-available)))
                constraints))))
    (nreverse constraints)))


(defun report-budget-arithmetic-classes (gate-costs)
  "Name shared demands without adding output for singleton-only budgets."
  (dolist (entry gate-costs)
    (when (cdr (first entry))
      (format t "  {~(~{~A~^, ~}~)}: one shared control demand of ~D plate keepers; ~
                 S1 mode/override qualifications apply.~%" (first entry) (second entry)))))


(defun report-budget-arithmetic ()
  "T6, grade 1->2.  The mechanized budget arithmetic: gate costs from S1 control
   algebra joined with occupant pool from S2 functional-relation census.  Derives
   impossibility constraints on simultaneous gate-open assignments.  Grade 1 from
   control facts and type extents; grade 2 from S2 structural injectivity."
  (let* ((facts (control-facts))
         (device-costs (budget-arithmetic-body-cost-devices facts))
         (gate-costs (budget-arithmetic-gate-costs device-costs))
         (occupancy (budget-arithmetic-segment-occupancy))
         (goal-actor-leaves-pool-p (budget-arithmetic-goal-actor-leaves-pool-p))
         (supports-disjoint-p (budget-arithmetic-disjoint-supports-p device-costs)))
    (format t "~2%T6  MECHANIZED BUDGET ARITHMETIC  [grade 1 -> 2]~%")
    (format t "~A~%" (make-string 62 :initial-element #\-))
    (report-budget-arithmetic-classes gate-costs)
    (cond ((null gate-costs)
           (format t "  no body-cost devices found.~%"))
          ((null occupancy)
           (format t "  occupant pool not determined; cannot compute constraints.~%"))
          ((not supports-disjoint-p)
           (format t "  body-cost support sets overlap; no summed-cost constraint is sound.~%"))
          (t (let ((constraints (budget-arithmetic-constraints gate-costs occupancy
                                                               goal-actor-leaves-pool-p
                                                               supports-disjoint-p)))
               (dolist (constraint constraints)
                 (format t "~%  ~A~%" constraint))
               (if (> (budget-arithmetic-total-cost gate-costs)
                      (- (first occupancy) (if goal-actor-leaves-pool-p 1 0)))
                 (format t "~%  NOTE: these are impossibility constraints on state assignments, ~
                            not on action sequences.  They refute the fully-open assignment without ~
                            searching.~%")
                 (format t "~%  NOTE: these are control-demand budgets, not action-sequence proofs. ~
                            No shortage follows from this pooled count.~%")))))
    (values)))


;;; T7 -- S5 height and reach lattice


(defun height-lattice-overrides ()
  "Every authored (HAS-HEIGHT object height) override, sorted by object."
  (sort (loop for fact in (list-static-db)
              when (and (consp fact) (eq (first fact) 'has-height))
                collect (list (second fact) (third fact)))
        #'string< :key (lambda (entry) (symbol-name (first entry)))))


(defun height-lattice-type-rows ()
  "The vertical defaults and authored override count for each vertical type."
  (let ((overrides (height-lattice-overrides)))
    (mapcar (lambda (entry)
              (let ((type (first entry)))
                (list type
                      (second entry)
                      (third entry)
                      (fourth entry)
                      (count-if (lambda (override)
                                  (member (first override)
                                          (census-type-instances type)))
                                overrides))))
            *vertical-type-constants*)))


(defun height-lattice-location-levels (state)
  "Each declared location and its staged floor elevation."
  (mapcar (lambda (location)
            (list location
                  (funcall (symbol-function 'location-elevation) state location)))
          (census-type-instances 'location)))


(defun height-lattice-support-tops (state)
  "Each declared support and its staged top elevation."
  (mapcar (lambda (support)
            (list support
                  (funcall (symbol-function 'top) state support)))
          (census-type-instances 'support)))


(defun height-lattice-type-top (type base)
  "The top of TYPE when it rests at BASE under the vertical constants."
  (let ((entry (find type *vertical-type-constants* :key #'first)))
    (if (eq (third entry) :vertical)
      (+ base (second entry))
      base)))


(defun height-lattice-placement-supports (state)
  "Potential support tops admitted by the placement substrate.
 Grounded trays are inert, so only held trays enter the enumeration."
  (let ((supports (list (list 'ground 0))))
    (dolist (type '(box fan))
      (dolist (object (census-type-instances type))
        (push (list object
                    (if (vertical-axis-p object)
                      (funcall (symbol-function 'object-height) state object)
                      0))
              supports)))
    (when (and (census-type-instances 'agent)
               (census-type-instances 'tray))
      (dolist (agent (census-type-instances 'agent))
        (dolist (tray (census-type-instances 'tray))
          (push (list tray
                      (funcall (symbol-function 'object-height) state agent))
                supports))))
    (sort (remove-duplicates supports :test #'equal)
          #'string< :key (lambda (entry) (symbol-name (first entry))))))


(defun height-lattice-achievable-tops (state)
  "Every carried object's structural top values after a legal placement.
 This enumerates placement forms, not locations that a plan can reach."
  (let ((supports (height-lattice-placement-supports state)))
    (mapcar (lambda (object)
              (let ((height (funcall (symbol-function 'object-height) state object)))
                (list object
                      (sort (remove-duplicates
                             (mapcar (lambda (support)
                                       (+ (second support) height))
                                     supports))
                            #'<))))
            (census-type-instances 'cargo))))


(defun height-lattice-placement-matrix (state)
  "Every staged agent-base/candidate-support-top pair and its reach result."
  (let ((rows nil))
    (dolist (agent (census-type-instances 'agent) (nreverse rows))
      (let ((base (funcall (symbol-function 'base) state agent)))
        (dolist (support (height-lattice-placement-supports state))
          (push (list agent
                      base
                      (first support)
                      (second support)
                      (funcall (symbol-function 'within-agent-placement-reach)
                               state agent (second support)))
                rows))))))


(defun height-lattice-ground-unreachable-supports (state)
  "Candidate support tops no ground-level agent can reach for placement."
  (remove-if (lambda (support)
               (<= (second support) *vertical-reach-limit*))
             (height-lattice-placement-supports state)))


(defun report-height-and-reach-lattice ()
  "S5, grade 2.  Reports vertical defaults, staged support tops, placement
 reach, and support tops unreachable from a ground-level placement."
  (let* ((state *start-state*)
         (type-rows (height-lattice-type-rows))
         (overrides (height-lattice-overrides))
         (levels (height-lattice-location-levels state))
         (tops (height-lattice-achievable-tops state))
         (matrix (height-lattice-placement-matrix state))
         (unreachable (height-lattice-ground-unreachable-supports state)))
    (format t "~2%S5  HEIGHT AND REACH LATTICE  [grade 2]~%")
    (format t "~A~%" (make-string 62 :initial-element #\-))
    (format t "  type heights (~D types; ~D authored override~:P)~%"
            (length type-rows) (length overrides))
    (dolist (row type-rows)
      (format t "    ~(~A~)  height ~A  axis ~A  base ~A~@[  overrides ~D~]~%"
              (first row) (second row) (third row) (fourth row)
              (unless (zerop (fifth row)) (fifth row))))
    (format t "~%  location levels~%")
    (dolist (level levels)
      (format t "    ~(~A~)  ~A~%" (first level) (second level)))
    (format t "~%  achievable carried-object tops~%")
    (dolist (entry tops)
      (format t "    ~(~A~)  ~{~A~^, ~}~%" (first entry) (second entry)))
    (format t "~%  placement legality matrix (agent base -> support top)~%")
    (dolist (row matrix)
      (format t "    ~(~A~) ~A -> ~(~A~) ~A  ~:[NO~;YES~]~%"
              (first row) (second row) (third row) (fourth row) (fifth row)))
    (format t "~%  unreachable from ground (placement reach limit ~A)~%"
            *vertical-reach-limit*)
    (if unreachable
      (dolist (support unreachable)
        (format t "    ~(~A~)  top ~A~%" (first support) (second support)))
      (format t "    none~%"))
    (values)))


;;; T8 -- S6 beam sightline table


(defun sightline-gate-subsets (gates)
  "Every subset of GATES, including the empty subset."
  (if (null gates)
    (list nil)
    (let ((subsets (sightline-gate-subsets (rest gates))))
      (append subsets
              (mapcar (lambda (subset)
                        (cons (first gates) subset))
                      subsets)))))


(defun sightline-fixed-endpoints ()
  "The fixed apparatus endpoint types accepted by BEAM-VISIBLE."
  (append (census-type-instances 'transmitter)
          (census-type-instances 'receiver)
          (census-type-instances 'floor-repeater)
          (census-type-instances 'wall-repeater)))


(defun sightline-connector-tops (state)
  "S5's distinct structural connector top elevations."
  (sort (remove-duplicates
         (loop for entry in (height-lattice-achievable-tops state)
               when (member (first entry) (census-type-instances 'connector))
                 append (second entry)))
        #'<))


(defun sightline-state-with-open-gates (gates open-gates)
  "A start-state copy with exactly OPEN-GATES asserted as open.
 This deliberately bypasses propagation: S6 varies only the gate bits."
  (let ((state (copy-problem-state *start-state*)))
    (dolist (gate gates)
      (delete-proposition (list 'open gate) (problem-state.idb state)))
    (dolist (gate open-gates)
      (add-proposition (list 'open gate) (problem-state.idb state)))
    (invalidate-problem-state-hash state)
    state))


(defun sightline-visible-records (gates)
  "Every visible S6 row over all direct gate subsets."
  (let ((records nil)
        (subsets (sightline-gate-subsets gates)))
    (dolist (open-gates subsets records)
      (let ((state (sightline-state-with-open-gates gates open-gates)))
        (dolist (location (census-type-instances 'location))
          (dolist (top (sightline-connector-tops state))
            (dolist (endpoint (sightline-fixed-endpoints))
              (when (funcall (symbol-function 'beam-visible)
                             state location top endpoint
                             (funcall (symbol-function 'top) state endpoint))
                (push (list location top endpoint open-gates) records))))))))
    )


(defun sightline-visible-subsets (location top endpoint records)
  "The gate subsets under which LOCATION/TOP can see ENDPOINT."
  (loop for record in records
        when (and (eq location (first record))
                  (= top (second record))
                  (eq endpoint (third record)))
          collect (fourth record)))


(defun sightline-row-status (visible-subsets subset-count)
  "ALWAYS, NEVER, or CONDITIONAL for one collapsed S6 row."
  (cond ((null visible-subsets) :never)
        ((= (length visible-subsets) subset-count) :always)
        (t :conditional)))


(defun sightline-required-open-gates (visible-subsets)
  "Gates present in every visible subset for one conditional S6 row."
  (if visible-subsets
    (reduce (lambda (left right)
              (intersection left right))
            (rest visible-subsets)
            :initial-value (first visible-subsets))))


(defun sightline-location-occluder-records (state)
  "Every location occluder that blocks one S6 beam at its interpolated height."
  (let ((records nil))
    (dolist (location (census-type-instances 'location))
      (dolist (top (sightline-connector-tops state))
        (dolist (endpoint (sightline-fixed-endpoints))
          (let ((far-elevation (funcall (symbol-function 'top) state endpoint)))
            (dolist (fact (list-static-db))
              (when (and (eq (first fact) 'los-via)
                         (eq (second fact) location)
                         (eq (fourth fact) endpoint))
                (dolist (occluder (third fact))
                  (when (and (member occluder (census-type-instances 'location))
                             (funcall (symbol-function 'los-location-occluded)
                                      state nil occluder location top endpoint far-elevation))
                    (push (list location top endpoint occluder) records)))))))))
    (nreverse records)))


(defun report-sightline-row (location top endpoint visible-subsets subset-count)
  "Print one collapsed S6 visibility row."
  (let ((status (sightline-row-status visible-subsets subset-count)))
    (format t "    ~(~A~) @ ~A -> ~(~A~)  ~A"
            location top endpoint status)
    (when (eq status :conditional)
      (format t "  requires open ~{~(~A~)~^, ~}"
              (sightline-required-open-gates visible-subsets)))
    (terpri)))


(defun report-beam-sightline-table ()
  "S6, grade 2.  Evaluates BEAM-VISIBLE for every S5 connector top, location,
 fixed endpoint, and direct gate subset without propagation or search."
  (let* ((gates (census-type-instances 'gate))
         (subsets (sightline-gate-subsets gates))
         (records (sightline-visible-records gates))
         (state *start-state*))
    (format t "~2%S6  BEAM SIGHTLINE TABLE  [grade 2]~%")
    (format t "~A~%" (make-string 62 :initial-element #\-))
    (format t "  direct gate subsets: ~D; no propagation applied~%" (length subsets))
    (format t "~%  visibility rows~%")
    (dolist (location (census-type-instances 'location))
      (dolist (top (sightline-connector-tops state))
        (dolist (endpoint (sightline-fixed-endpoints))
          (report-sightline-row
           location top endpoint
           (sightline-visible-subsets location top endpoint records)
           (length subsets)))))
    (format t "~%  location-occluder kill list~%")
    (let ((occluders (sightline-location-occluder-records state)))
      (if occluders
        (dolist (record occluders)
          (format t "    ~(~A~) @ ~A -> ~(~A~) blocked at ~(~A~)~%"
                  (first record) (second record) (third record) (fourth record)))
        (format t "    none~%")))
    (values)))


;;; T9 -- S7 landmark graph and orderings


(defun landmark-goal-conjuncts (form)
  "The explicit positive conjuncts in FORM, or FORM itself when it is atomic."
  (if (and (consp form) (eq (first form) 'and))
    (rest form)
    (list form)))


(defun landmark-control-fact (device)
  "The S1 control fact for DEVICE, if the explicit goal names one."
  (find device (control-facts) :key #'third))


(defun landmark-primitive (primitive)
  "A delete-relaxed landmark for one S1 control primitive."
  (cond ((member primitive (census-type-instances 'pressure-plate))
         (format nil "some occupant on ~(~A~)" primitive))
        ((member primitive (census-type-instances 'toggle-plate))
         (format nil "toggle ~(~A~)" primitive))
        ((member primitive (census-type-instances 'switch))
         (format nil "toggle ~(~A~) from a reachable location" primitive))
        (t (format nil "establish ~(~A~)" primitive))))


(defun landmark-device-expansion (device)
  "The relaxed S1 primitive requirements for DEVICE."
  (let ((fact (landmark-control-fact device)))
    (when fact
      (mapcar (lambda (clause)
                (mapcar #'landmark-primitive clause))
              (second fact)))))


(defun landmark-goal-device (conjunct)
  "The device named by a unary explicit goal condition, or NIL."
  (when (and (consp conjunct) (= (length conjunct) 2))
    (let ((candidate (second conjunct)))
      (when (landmark-control-fact candidate)
        candidate))))


(defun report-landmark-graph-and-orderings ()
  "S7, grade 4.  Backward-chains explicit device goals through S1 controls
 under the delete relaxation; it does not infer movement routes or role overlap."
  (let ((goal (get 'goal-fn :form))
        (devices nil))
    (format t "~2%S7  LANDMARK GRAPH AND ORDERINGS  [grade 4]~%")
    (format t "~A~%" (make-string 62 :initial-element #\-))
    (format t "  relaxation: delete relaxation; achieved landmarks persist.~%")
    (format t "  no simultaneous-role, keeper-return, segment, or route claim is emitted.~%")
    (format t "~%  explicit goal landmarks~%")
    (dolist (conjunct (landmark-goal-conjuncts goal))
      (let ((device (landmark-goal-device conjunct)))
        (if device
          (progn
            (pushnew device devices)
            (format t "    ~(~S~) requires device ~(~A~)~%" conjunct device))
          (format t "    ~(~S~)  movement/query landmark; S1 expansion unavailable.~%"
                  conjunct))))
    (format t "~%  S1 controller expansions~%")
    (if devices
      (dolist (device (nreverse devices))
        (format t "    ~(~A~)~%" device)
        (dolist (clause (landmark-device-expansion device))
          (format t "      AND ~{~A~^; ~}~%" clause)))
      (format t "    none: the explicit goal has no controlled-device condition.~%"))
    (format t "~%  greedy-necessary orderings~%")
    (if devices
      (dolist (device devices)
        (format t "    establish controller primitives for ~(~A~) before ~(~A~).~%"
                device device))
      (format t "    none: route/order extraction needs a separately stated movement relaxation.~%"))
    (values)))


(defparameter *traversal-symmetric-relation* 'traverse-via
  "The symmetric traversal relation, NAMED because the sealed spec names it as S3's input,
   exactly as S1 names CONTROLS as its own (A17).  Finding it by shape would also admit
   REACH-VIA, which -traversal.lisp excludes in as many words: reaching across a barrier
   authorizes manipulation, not movement, so a shape rule would fold a relation that moves
   nobody into the relation that moves everybody.  This is the only substrate vocabulary S3
   carries; no problem object name appears anywhere in it (C3).")


(defparameter *traversal-directed-relation* 'traverse-via>
  "The directed traversal relation, source first.  Its arcs are never contracted (A19): a
   one-way door-free arc does not make its endpoints mutually reachable, and a region built
   as though it did would be unsound for the stranding arguments S4 will ask of it.")


(defun traversal-endpoint-type ()
  "The declared type a traversal arc's endpoints range over: the type standing at TWO
   positions of the relation's signature.  -traversal.lisp says the engine mirrors a
   relation whose argument types repeat, and that the repeated type here is the location
   type -- so the REPETITION is the endpoint marker, and the extractor reads it off the
   signature rather than being told a name (A18).  The fluent family position occurs once
   and is passed over."
  (let ((signature (gethash *traversal-symmetric-relation* *static-relations*)))
    (find-if (lambda (spec)
               (and (census-spec-type-names spec)
                    (= 2 (count spec signature :test #'equal))))
             signature)))


(defun traversal-signature-layout ()
  "Where each part of a traversal proposition sits, as (family-position source-position
   destination-position), all 1-based into the signature and therefore directly usable as
   NTH into a proposition, whose first element is the relation name.  Computed rather than
   assumed: the endpoint type is the repeated one and the family is the relation's single
   fluent."
  (let* ((signature (gethash *traversal-symmetric-relation* *static-relations*))
         (endpoint-type (traversal-endpoint-type))
         (fluents (gethash *traversal-symmetric-relation* *fluent-relation-indices*))
         (endpoints nil))
    (loop for spec in signature
          for position from 1
          when (equal spec endpoint-type)
            do (push position endpoints))
    (setf endpoints (nreverse endpoints))
    (list (first fluents) (first endpoints) (second endpoints))))


(defun traversal-arc-door-family (clauses)
  "CLAUSES, each sorted by name, as one kind's door family: NIL when one is empty;
   otherwise the distinct clauses no other clause is a proper subset of, shortest first."
  (unless (member nil clauses)
    (let ((distinct (remove-duplicates clauses :test #'equal)))
      (sort (remove-if (lambda (clause)
                         (some (lambda (other)
                                 (and (not (equal other clause)) (subsetp other clause)))
                               distinct))
                       distinct)
            #'traversal-clause-precedes-p))))


(defun traversal-arc-kind-families (source family destination)
  "FAMILY split by clause kind, as (kind door-family) entries in
   *TRAVERSAL-KIND-PREFERENCE* order.  A clause's kind is the engine's segment kind at the
   staged start, so a bare-level walk across a level difference reads as a jump here
   exactly as the engine reads it.  Its doors are its MEANS: the clause without its static
   separators (staircase, edge, floor drive), which have no state and are never doors.  A
   kind's door family is NIL, the direct case, when any of its clauses has no means left."
  (let ((groups nil))
    (dolist (clause (or family (list nil)))
      (let ((kind (funcall (symbol-function 'traversal-clause-segment-kind)
                           *start-state* source destination clause))
            (means (second (funcall (symbol-function 'traversal-clause-profile) clause))))
        (push (sort (copy-list means) #'string< :key #'symbol-name) (getf groups kind))))
    (loop for kind in *traversal-kind-preference*
          for clauses = (getf groups kind)
          when clauses
            collect (list kind (traversal-arc-door-family clauses)))))


(defun traversal-arc-facts ()
  "Every traversal edge in the static database, normalized to (relation kind source family
   destination) whatever order the signature declares.  One fact gives one arc per kind
   its clauses infer (TRAVERSAL-ARC-KIND-FAMILIES), so a pair crossable by stairs or by a
   jump yields a STAIRS arc and a JUMP arc, as the mode facts it replaced did.  Authored
   and derived edges arrive identically: -walkability-coordinates derives the walk-kind
   facts from raw segment geometry during initialization and asserts them as ordinary
   propositions of these two relations, so the extractor reads two relations and never the
   geometry behind them.  A symmetric arc has its endpoints put in name order, so the two
   spellings of one undirected crossing collapse to one entry whether or not the engine
   stored a mirror; a directed arc keeps the order it was asserted in."
  (let ((layout (traversal-signature-layout))
        (arcs nil))
    (dolist (fact (list-static-db))
      (when (and (consp fact)
                 (member (first fact) (list *traversal-symmetric-relation*
                                            *traversal-directed-relation*)))
        (let ((source (nth (second layout) fact))
              (destination (nth (third layout) fact)))
          (when (and (eq (first fact) *traversal-symmetric-relation*)
                     (string> (symbol-name source) (symbol-name destination)))
            (rotatef source destination))
          (dolist (entry (traversal-arc-kind-families source (nth (first layout) fact)
                                                      destination))
            (pushnew (list (first fact) (first entry) source (second entry) destination)
                     arcs :test #'equal)))))
    (sort arcs #'string<
          :key (lambda (arc) (format nil "~A|~A|~A|~A"
                                     (third arc) (fifth arc) (second arc) (first arc))))))


(defun traversal-endpoints ()
  "Every member of the endpoint type, sorted by name.  Arcs supply the edges and the type
   supplies the vertices, so an endpoint no arc mentions still gets a block of its own and
   is visible as isolated rather than silently absent."
  (sort (copy-list (census-spec-extent (traversal-endpoint-type)))
        #'string< :key #'symbol-name))


(defun region-find (node parents)
  "NODE's block representative, with the path compressed behind it."
  (let ((parent (gethash node parents node)))
    (if (eq parent node)
      node
      (let ((root (region-find parent parents)))
        (setf (gethash node parents) root)
        root))))


(defun region-union (left right parents)
  "Merge the blocks holding LEFT and RIGHT."
  (let ((left-root (region-find left parents))
        (right-root (region-find right parents)))
    (unless (eq left-root right-root)
      (setf (gethash left-root parents) right-root))))


(defun contract-free-arcs (arcs)
  "Step 2.  Union the endpoints of every SYMMETRIC arc whose clause family is empty -- the
   direct, unguarded case -- and of no other arc.  A19: an empty DIRECTED arc is left
   uncontracted and survives into the quotient carrying the empty family, because reaching
   a region for free is not the same as belonging to it."
  (let ((parents (make-hash-table :test #'eq)))
    (dolist (arc arcs parents)
      (when (and (eq (first arc) *traversal-symmetric-relation*)
                 (null (fourth arc)))
        (region-union (third arc) (fifth arc) parents)))))


(defun region-blocks (endpoints parents)
  "The contraction's blocks as (name members), members sorted by name and blocks ordered by
   their first member, so a region's name is a deterministic function of the specification
   and the generated profile diffs cleanly across runs."
  (let ((groups (make-hash-table :test #'eq))
        (blocks nil))
    (dolist (endpoint endpoints)
      (push endpoint (gethash (region-find endpoint parents) groups)))
    (loop for members being the hash-values of groups
          do (push (sort (copy-list members) #'string< :key #'symbol-name) blocks))
    (setf blocks (sort blocks #'string<
                       :key (lambda (members) (symbol-name (first members)))))
    (loop for members in blocks
          for index from 1
          collect (list (format nil "R~D" index) members))))


(defun region-name-table (blocks)
  "Endpoint -> region name, so an arc's two ends are placed without rescanning the blocks."
  (let ((table (make-hash-table :test #'eq)))
    (dolist (block blocks table)
      (dolist (member (second block))
        (setf (gethash member table) (first block))))))


(defun quotient-arc-rows (arcs names)
  "Step 3.  One row per (from, to, kind, family, directedness), carrying the count of
   location arcs standing behind it (A20).  Printing every location arc would bury the
   quotient in the thing it abstracts; printing one row without the count would hide that a
   region pair is joined by several independent doorways, which is the first thing S4 must
   know before calling any crossing obligatory.  An arc whose ends fall in one block is
   internal to that region and contributes no row."
  (let ((rows (make-hash-table :test #'equal)))
    (dolist (arc arcs)
      (let ((from (gethash (third arc) names))
            (to (gethash (fifth arc) names))
            (symmetric (eq (first arc) *traversal-symmetric-relation*)))
        (unless (string= from to)
          (let ((key (if (and symmetric (string> from to))
                       (list to from (second arc) (fourth arc) :both)
                       (list from to (second arc) (fourth arc)
                             (if symmetric :both :forward)))))
            (incf (gethash key rows 0))))))
    (sort (loop for key being the hash-keys of rows using (hash-value count)
                collect (append key (list count)))
          #'string<
          :key (lambda (row) (format nil "~A|~A|~A|~A|~A"
                                     (first row) (second row) (third row)
                                     (fifth row) (fourth row))))))


(defun report-traversal-arcs (arcs)
  "Step 1.  What was read, by relation and by kind, before any contraction.  Printed first
   so a reader can tell an empty quotient caused by an empty input from one caused by total
   contraction."
  (format t "~%  traversal arcs read (~D)~%" (length arcs))
  (format t "    ~D symmetric (~(~A~)), ~D directed (~(~A~))~%"
          (count *traversal-symmetric-relation* arcs :key #'first)
          *traversal-symmetric-relation*
          (count *traversal-directed-relation* arcs :key #'first)
          *traversal-directed-relation*)
  (dolist (kind (sort (remove-duplicates (mapcar #'second arcs))
                      #'string< :key #'symbol-name))
    (let ((of-kind (remove-if-not (lambda (arc) (eq kind (second arc))) arcs)))
      (format t "    kind ~(~A~): ~D arc~:P, ~D with an empty family~%"
              kind (length of-kind) (count-if #'null of-kind :key #'fourth)))))


(defun report-region-blocks (blocks arcs endpoints)
  "Step 3's first half.  The contraction's blocks, with the rule that produced them stated
   in the output as step 2 requires, and with the qualification that makes the blocks
   readable: this is a DOOR-COST quotient and not a reachability quotient."
  (format t "~%  contraction rule: two endpoints share a region when an arc of ~(~A~) ~
             joins them with an EMPTY door family.  Static separators -- staircases, edges, ~
             floor drives -- are not doors.  Arcs of ~(~A~) are never contracted, ~
             whatever their family.~%"
          *traversal-symmetric-relation* *traversal-directed-relation*)
  (format t "  NOTE: a region is a set of endpoints NO DOOR separates.  Each kind carries ~
             its own predicate -- a jump's reach limit, a ladder's position -- which this ~
             extractor does not evaluate, having no state to evaluate it in.  Two ~
             endpoints in one region therefore need not be mutually reachable.~%")
  (format t "~%  regions (~D over ~D endpoint~:P of type ~(~A~))~%"
          (length blocks) (length endpoints) (traversal-endpoint-type))
  (dolist (block blocks)
    (format t "    ~A  (~D): ~(~{~A~^ ~}~)~%"
            (first block) (length (second block)) (second block)))
  (let* ((touched (append (mapcar #'third arcs) (mapcar #'fifth arcs)))
         (isolated (remove-if (lambda (endpoint) (member endpoint touched)) endpoints)))
    (when isolated
      (format t "    on no arc at all (~D): ~(~{~A~^ ~}~)~%" (length isolated) isolated))))


(defun quotient-row-doors (row)
  "Every door ROW's clause family names, sorted, so two rows' door sets compare by EQUAL."
  (sort (remove-duplicates (loop for clause in (fourth row) append (copy-list clause)))
        #'string< :key #'symbol-name))


(defun quotient-reachable-door-sets (start rows limit)
  "Every (region . door-set) reachable from START over ROWS whose doors all lie inside
   LIMIT, the set accumulating along the path.  A bidirectional row is walked either way
   and a directed row forward only (A26).  Bounded by LIMIT, so the state space is the
   regions crossed with the subsets of one family and the walk terminates.
   THE COPY-LIST IS LOAD-BEARING.  UNION may share structure with either argument, and
   DOORS is the cdr of a key already in SEEN; sorting that result destructively rewrites
   the stored key after it was hashed, GETHASH then misses states that are present, and
   the frontier never drains.  This hung the first time it ran."
  (let ((seen (make-hash-table :test #'equal))
        (frontier (list (cons start nil))))
    (setf (gethash (cons start nil) seen) t)
    (loop while frontier
          do (let* ((current (pop frontier))
                    (region (car current))
                    (doors (cdr current)))
               (dolist (row rows)
                 (let ((next (cond ((string= (first row) region) (second row))
                                   ((and (eq (fifth row) :both)
                                         (string= (second row) region))
                                    (first row))))
                       (row-doors (quotient-row-doors row)))
                   (when (and next (subsetp row-doors limit))
                     (let ((entry (cons next
                                        (sort (copy-list (union doors row-doors))
                                              #'string< :key #'symbol-name))))
                       (unless (gethash entry seen)
                         (setf (gethash entry seen) t)
                         (push entry frontier))))))))
    seen))


(defun quotient-row-composition (row rows)
  "SPINE, COMPOSED or NON-MINIMAL for ROW against the other rows.  COMPOSED when some path
   of other rows joins its endpoints with a door union EQUAL to its own family; NON-MINIMAL
   when a path arrives with a STRICT SUBSET of that family, which would mean the coordinate
   derivation emitted a family a cheaper route already beats (A25).  SPINE otherwise: the
   row has no replacement in this graph. Reduction applies this test sequentially."
  (let* ((limit (quotient-row-doors row))
         (others (remove row rows :test #'equal))
         (seen (quotient-reachable-door-sets (first row) others limit))
         (exact nil)
         (cheaper nil)
         (cheaper-p nil))
    (loop for entry being the hash-keys of seen
          do (when (string= (car entry) (second row))
               (if (null (set-exclusive-or limit (cdr entry)))
                 (setf exact t)
                 (setf cheaper (cdr entry) cheaper-p t))))
    (cond (cheaper-p (list :non-minimal cheaper))
          (exact (list :composed nil))
          (t (list :spine nil)))))


(defun keeper-row-avoids-p (row device)
  "Traversal NIL means direct; otherwise one DNF alternative avoiding DEVICE suffices."
  (or (null device) (null (fourth row))
      (some (lambda (clause) (not (member device clause))) (fourth row))))


(defun keeper-row-next (row region)
  "Directedness is preserved, including for door-free and climb-kind edges."
  (cond ((equal region (first row)) (second row))
        ((and (eq (fifth row) :both) (equal region (second row))) (first row))))


(defun keeper-reachable (start rows device)
  "Forward graph reachability with DEVICE forbidden and all other conditions relaxed."
  (let ((seen (make-hash-table :test #'equal))
        (frontier (list start)))
    (setf (gethash start seen) t)
    (loop while frontier
          do (let ((current (pop frontier)))
               (dolist (row rows)
                 (let ((next (keeper-row-next row current)))
                   (when (and next (keeper-row-avoids-p row device)
                              (not (gethash next seen)))
                     (setf (gethash next seen) t)
                     (push next frontier))))))
    (sort (loop for region being the hash-keys of seen collect region) #'string<)))


(defun quotient-reachability-mismatch (rows reduced regions devices)
  "First full/reduced reachability disagreement, or NIL. Include uncontrolled row doors."
  (dolist (device (cons nil (remove-duplicates
                             (append devices (mapcan #'quotient-row-doors rows)))))
    (dolist (region regions)
      (unless (equal (keeper-reachable region rows device)
                     (keeper-reachable region reduced device))
        (return-from quotient-reachability-mismatch
          (list :reachability-mismatch region device)))))
  nil)


(defun quotient-row-replaceable-p (row rows)
  "A same-cost or cheaper replacement must exist in every direction ROW permits."
  (and (not (eq :spine (first (quotient-row-composition row rows))))
       (or (not (eq (fifth row) :both))
           (let ((reverse-row (copy-list row)))
             (rotatef (first reverse-row) (second reverse-row))
             (not (eq :spine
                      (first (quotient-row-composition
                               reverse-row (remove row rows :test #'equal)))))))))


(defun quotient-alternative-families-p (rows)
  "Whether any row has more than one alternative door clause."
  (some (lambda (row) (> (length (fourth row)) 1)) rows))


(defun quotient-reduced-rows (rows)
  "Deterministic representative graph; sequential deletion never removes its own witnesses.
   Alternative families remain intact for clause-aware S4 analysis on the full quotient."
  (let* ((ordered (sort (copy-list rows) #'string<
                        :key (lambda (row)
                               (with-standard-io-syntax (prin1-to-string row)))))
         (retained (copy-list ordered))
         (regions (remove-duplicates (append (mapcar #'first rows) (mapcar #'second rows))
                                     :test #'equal)))
    (unless (quotient-alternative-families-p rows)
      (dolist (row ordered)
        (when (quotient-row-replaceable-p row retained)
          (let ((candidate (remove row retained :test #'equal)))
            (unless (quotient-reachability-mismatch rows candidate regions nil)
              (setf retained candidate))))))
    retained))


(defun quotient-classified-rows (rows)
  "S3 classifications against the same final retained graph S4 uses."
  (let ((retained (quotient-reduced-rows rows)))
    (mapcar (lambda (row)
              (cons row (if (member row retained :test #'equal)
                          (list :spine nil)
                          (quotient-row-composition row retained))))
            rows)))


(defun report-quotient-arcs (rows)
  "Step 3's second half.  Every crossing between two regions, with its clause family, its
   kind, its directedness, how many location arcs stand behind it, and whether it is part
   of the adjacency SPINE or a COMPOSITION of spine rows.
   THE DISTINCTION IS THE POINT.  The coordinate derivation emits a minimal door-set for
   every LOCATION PAIR, so these rows are a transitive closure and not an adjacency list:
   many are compositions. The spine is a deterministic reachability-preserving representative,
   not a unique physical doorway decomposition. S4 uses the same retained rows."
  (let ((classified (quotient-classified-rows rows)))
    (format t "~%  region crossings (~D rows: ~D spine, ~D composed)~%"
            (length classified)
            (count :spine classified :key #'second)
            (count :composed classified :key #'second))
    (format t "    NOTE: these rows are the transitive CLOSURE, one minimal door-set per ~
               location pair. The spine preserves reachability; it is not a physical doorway count.~%")
    (dolist (entry classified)
      (let ((row (first entry)))
        (format t "    ~A ~A ~A  kind ~(~A~)  family ~(~A~)  ~D location arc~:P  ~A~@[ via ~(~A~)~]~%"
                (first row)
                (if (eq (fifth row) :both) "<->" "-->")
                (second row)
                (third row)
                (if (fourth row) (fourth row) "() direct")
                (sixth row)
                (case (second entry)
                  (:spine "SPINE")
                  (:composed "composed")
                  (t "NON-MINIMAL -- a cheaper route exists; see the derivation"))
                (third entry))))
    (when (null rows)
      (format t "    none: every arc is internal to a region~%"))
    (format t "~%  adjacency spine (~D)~%" (count :spine classified :key #'second))
    (dolist (entry classified)
      (when (eq (second entry) :spine)
        (let ((row (first entry)))
          (format t "    ~A ~A ~A  kind ~(~A~)  family ~(~A~)~%"
                  (first row)
                  (if (eq (fifth row) :both) "<->" "-->")
                  (second row) (third row)
                  (if (fourth row) (fourth row) "() direct")))))))


(defun report-traversal-door-coverage (arcs)
  "Step 4.  Which controlled devices label an arc and which label none.  The sealed step
   calls a device on no arc an authoring signal; that reading is NOT adopted unqualified,
   because a device can be entirely live and simply not gate a passage.  What the
   computation supports is the narrower claim, and the narrower claim is what prints."
  (let* ((doors (sort (remove-duplicates
                        (loop for arc in arcs
                              append (loop for clause in (fourth arc)
                                           append (copy-list clause))))
                      #'string< :key #'symbol-name))
         (devices (mapcar #'third (control-facts)))
         (unlabelled (sort (copy-list (set-difference devices doors))
                           #'string< :key #'symbol-name))
         (uncontrolled (sort (copy-list (set-difference doors devices))
                             #'string< :key #'symbol-name)))
    (format t "~%  doors on arcs (~D)~%    ~(~{~A~^ ~}~)~%" (length doors) doors)
    (format t "~%  controlled devices labelling NO traversal arc (~D of ~D)~%"
            (length unlabelled) (length devices))
    (dolist (device unlabelled)
      (format t "    ~(~A~)~%" device))
    (when (null unlabelled)
      (format t "    none: every controlled device gates some crossing~%"))
    (format t "    READING: such a device is movement-irrelevant, meaning that no traversal ~
               clause names it.  It is NOT a claim that the device is inert -- a device may ~
               act on objects rather than on passage -- and it is NOT a claim about ~
               reaching, which a separate relation carries and this extractor does not ~
               read.~%")
    (format t "~%  doors that are NOT controlled devices (~D)~%    ~(~{~A~^ ~}~)~%"
            (length uncontrolled) uncontrolled)
    (format t "    these are obstacles with no CONTROLS entry, so no plate or switch opens ~
               them and S1's algebra says nothing about them.~%")))


(defun report-region-quotient ()
  "S3, grade 2.  The gate-labelled region quotient: endpoints contracted across door-free
   symmetric arcs, the surviving crossings with their clause families and directedness, and
   the controlled devices that label no crossing at all.  Grade 2 rather than grade 1
   because the arcs are a static reading while the claim a reader wants from them -- that
   passing between two regions requires one of these families -- is an invariant over
   reachable states, resting on the induction that no action asserts a traversal fact.
   WHAT THIS DOES NOT SUPPLY: which SIDE of a crossing a door's controller lies on, and
   therefore whether crossing strands a body.  That is S4, and the quotient's whole value
   to the abstract model depends on it."
  (let* ((*print-pretty* nil)
         (arcs (traversal-arc-facts)))
    (format t "~2%S3  GATE-LABELLED REGION QUOTIENT  [grade 2]~%")
    (format t "~A~%" (make-string 62 :initial-element #\-))
    (if (null arcs)
      (format t "  no traversal facts in the static database.~%")
      (let* ((endpoints (traversal-endpoints))
             (parents (contract-free-arcs arcs))
             (blocks (region-blocks endpoints parents))
             (names (region-name-table blocks)))
        (report-traversal-arcs arcs)
        (report-region-blocks blocks arcs endpoints)
        (report-quotient-arcs (quotient-arc-rows arcs names))
        (report-traversal-door-coverage arcs)))
    (values)))


(defun keeper-sorted-set (items)
  "A fresh, deterministic symbol set.  Never sort a possibly shared set operation."
  (sort (copy-list (remove-duplicates items)) #'string< :key #'symbol-name))


(defun keeper-pressure-clauses (fact pressure-plates)
  "Positive weight requirements per NORMAL control alternative, never for inversion.
   NIL means no positive alternative (or inverted); (NIL) means a zero-weight alternative."
  (when (eq (fourth fact) 'normal)
    (mapcar (lambda (clause)
              (keeper-sorted-set (intersection clause pressure-plates)))
            (second fact))))


(defun keeper-mandatory-plates (clauses)
  "Plates occurring in EVERY positive alternative; alternatives are not conjoined."
  (when clauses
    (keeper-sorted-set (reduce (lambda (left right) (intersection left right)) clauses))))


(defun keeper-spine (rows regions devices)
  "Return the shared S3 graph and NIL, or NIL and a reachability failure reason.
   Alternative families use the unreduced quotient; singleton families use the reduction."
  (let* ((spine (quotient-reduced-rows rows))
         (reason (quotient-reachability-mismatch rows spine regions devices)))
    (if reason (values nil reason) (values spine nil))))


(defun keeper-fact-value (relation object facts)
  "The functional binary interfaces named by S4: object first, location second."
  (third (find-if (lambda (fact)
                   (and (eq (first fact) relation) (eq (second fact) object))) facts)))


(defun keeper-goal-destinations (form)
  "Return positive ground location conjuncts and unsupported forms separately.
   Never harvest a destination from under OR, NOT, a quantifier, or an opaque query."
  (cond ((and (consp form) (eq (first form) 'and))
         (let ((destinations nil) (unresolved nil))
           (dolist (term (rest form))
             (multiple-value-bind (found missed) (keeper-goal-destinations term)
               (setf destinations (append destinations found)
                     unresolved (append unresolved missed))))
           (values (remove-duplicates destinations :test #'equal) unresolved)))
        ((and (consp form) (= (length form) 3) (eq (first form) 'has-location)
              (every (lambda (item) (and (symbolp item) item (not (?varp item))))
                     (rest form)))
         (values (list form) nil))
        (t (values nil (list form)))))


(defun keeper-controller-kind (controller)
  "These substrate kinds have different persistence semantics in -controls and plate."
  (cond ((member controller (census-type-instances 'pressure-plate)) :pressure)
        ((member controller (census-type-instances 'toggle-plate)) :latch)
        ((member controller (census-type-instances 'switch)) :switch)
        ((member controller (census-type-instances 'receiver)) :receiver)
        (t :unresolved)))


(defun keeper-reach-sites (controller facts names)
  "Manipulation candidates (location region barriers); directed reach is reacher -> target.
   These are not legal toggle witnesses: no vertical reach or actor state is evaluated."
  (let ((sites nil))
    (dolist (fact facts)
      (let ((location nil))
        (cond ((and (member (first fact) '(reach-via reach-via>))
                    (eq (fourth fact) controller))
               (setf location (second fact)))
              ((and (eq (first fact) 'reach-via) (eq (second fact) controller))
               (setf location (fourth fact))))
        (when (and location (gethash location names))
          (pushnew (list location (gethash location names) (third fact)) sites :test #'equal))))
    (sort sites #'string< :key (lambda (site) (symbol-name (first site))))))


(defun report-keeper-controller (controller facts names)
  (let* ((position (keeper-fact-value 'has-position controller facts))
         (kind (keeper-controller-kind controller)))
    (format t "      controller ~(~A~): ~A; position ~(~A~); region ~A~%"
            controller kind (or position :unresolved)
            (or (gethash position names) :unresolved))
    (when (eq kind :switch)
      (format t "        manipulation reach candidates (location region required-open barriers): ~(~S~)~%"
              (keeper-reach-sites controller facts names)))
    (when (member kind '(:latch :switch))
      (format t "        persistent state; no continuous weight requirement inferred.~%"))
    (when (eq kind :receiver)
      (format t "        beam dependency unresolved; requires sightline analysis.~%"))))


(defun report-keeper-axioms (device axioms)
  "Carry S1's proof qualification on each device row, including any live override."
  (let ((matching (remove-if-not
                   (lambda (axiom) (relation-keys-a-device-p (first axiom) (list device))) axioms)))
    (if matching
      (dolist (axiom matching)
        (format t "      state ~(~A~): ~A; empty-type premises ~(~S~)~%"
                (first axiom) (axiom-reading-text (fifth axiom)) (sixth axiom)))
      (format t "      device-state correspondence UNRESOLVED; aggregate requirements only.~%"))))


(defun keeper-plate-side (region approach departure)
  "Accessibility in two separate forward closures, never an undirected partition."
  (cond ((null region) :unlocated)
        ((member region approach :test #'equal)
         (if (member region departure :test #'equal) :both :approach-only))
        ((member region departure :test #'equal) :departure-only)
        (t :neither)))


(defun report-keeper-direction (source destination device plates spine facts names)
  (let ((approach (keeper-reachable source spine device))
        (departure (keeper-reachable destination spine device)))
    (format t "        ~A -> ~A, device absent: source reaches ~S; destination reaches ~S~%"
            source destination approach departure)
    (format t "          graph cut in this direction: ~:[YES~;NO, bypass exists~]~%"
            (member destination approach :test #'equal))
    (dolist (plate plates)
      (let* ((position (keeper-fact-value 'has-position plate facts))
             (region (gethash position names))
             (side (keeper-plate-side region approach departure)))
        (format t "          mandatory pressure ~(~A~) at ~A: ~A~%" plate (or region :unresolved) side)
        (when (eq side :approach-only)
          (format t "            KEEPER-OBLIGATED candidate while device state requires this aggregate;~%")
          (format t "            permanent stranding UNRESOLVED (movement and lifecycle coverage).~%"))))))


(defun report-keeper-device (fact spine reason facts names axioms pressure-plates)
  (let* ((device (third fact))
         (clauses (keeper-pressure-clauses fact pressure-plates))
         (mandatory (keeper-mandatory-plates clauses))
         (crossings (remove-if-not (lambda (row) (member device (quotient-row-doors row))) spine)))
    (format t "~%    ~(~A~) == ~(~A~)~%" device (control-boolean-form (second fact) (fourth fact)))
    (report-keeper-axioms device axioms)
    (dolist (controller (keeper-sorted-set (apply #'append (second fact))))
      (report-keeper-controller controller facts names))
    (format t "      positive pressure alternatives ~(~S~); individually mandatory ~(~S~)~%"
            clauses mandatory)
    (cond ((eq (fourth fact) 'inverted)
           (format t "      inverted aggregate: no positive keeper demand inferred.~%"))
          ((null (second fact))
           (format t "      normal empty DNF: aggregate cannot activate; device overrides remain separate.~%"))
          (t (let ((minimum (reduce #'min clauses :key #'length)))
               (format t "      conditional shortage: available eligible ON witnesses < ~D implies~%" minimum)
               (format t "        this normal aggregate cannot activate (minimum simultaneous plate demand).~%"))))
    (cond (reason (format t "      directional analysis UNRESOLVED: ~S~%" reason))
          ((null crossings)
           (format t "      no movement-spine occurrence; nonmovement function UNRESOLVED, not inert.~%"))
          (t (dolist (row crossings)
               (format t "      spine kind ~(~A~), family ~(~S~)~%" (third row) (fourth row))
               (report-keeper-direction (first row) (second row) device mandatory spine facts names)
               (when (eq (fifth row) :both)
                 (report-keeper-direction (second row) (first row) device mandatory spine facts names)))))))


(defun keeper-required-devices (source destination spine devices)
  "Necessary labels in the relaxed graph only.  NIL,NIL means baseline disconnected."
  (when (member destination (keeper-reachable source spine nil) :test #'equal)
    (values (remove-if (lambda (device)
                         (member destination (keeper-reachable source spine device) :test #'equal))
                       devices)
            t)))


(defun report-keeper-destinations (goal initial names spine reason devices)
  (multiple-value-bind (destinations unresolved) (keeper-goal-destinations goal)
    (format t "~%  explicit goal destinations (~D); goal form ~(~S~)~%" (length destinations) goal)
    (dolist (term unresolved)
      (format t "    UNRESOLVED goal form ~(~S~); no destination extracted.~%" term))
    (dolist (destination destinations)
      (let* ((object (second destination))
             (source (keeper-fact-value 'has-location object initial))
             (from (gethash source names))
             (to (gethash (third destination) names)))
        (format t "    ~(~A~): ~(~A~) (~A) -> ~(~A~) (~A)~%"
                object source from (third destination) to)
        (if (or reason (null from) (null to))
          (format t "      UNRESOLVED: graph unavailable or endpoint not located.~%")
          (multiple-value-bind (required connected) (keeper-required-devices from to spine devices)
            (if connected
              (format t "      GRAPH-REQUIRED candidates ~(~S~); concrete necessity UNRESOLVED.~%" required)
              (format t "      baseline graph disconnected; no device-cut conclusion.~%"))))))
    (dolist (fact initial)
      (when (and (eq (first fact) 'has-location)
                 (not (find (second fact) destinations :key #'second)))
        (format t "    ~(~A~): initially ~(~A~); no explicit destination, crossings UNRESOLVED.~%"
                (second fact) (third fact))))))


(defun report-keeper-supply ()
  "S2's ON pool and consumer bounds, reused without equating pool size to availability."
  (let ((placement (find 'on (placement-relations (functional-relation-entries)) :key #'first)))
    (format t "~%  keeper supply from S2~%")
    (if (and placement (string= (second placement) "dynamic")
             (= (third placement) 1) (= (fourth placement) 2))
      (report-placement-bound placement (layer-pairs))
      (format t "    UNRESOLVED: ON is not the expected functional occupant -> support interface.~%"))
    (format t "    Available eligible witnesses are a parameter for each view and segment.~%")
    (format t "    Total sequential crossers are NOT simultaneous body demand.~%")))


(defun report-cut-keeper-table ()
  "S4.  Control requirements and conditional cardinality consequences, plus directed
   graph-cut candidates.  No concrete stranding proof or implicit cargo destination.
   Uses substrate interfaces, never problem object names; does not run a search."
  (let* ((*print-pretty* nil)
         (facts (list-static-db))
         (controls (control-facts))
         (devices (mapcar #'third controls))
         (arcs (traversal-arc-facts))
         (blocks (region-blocks (traversal-endpoints) (contract-free-arcs arcs)))
         (names (region-name-table blocks))
         (rows (quotient-arc-rows arcs names))
         (axioms (device-state-axioms controls))
         (pressure-plates (census-type-instances 'pressure-plate)))
    (format t "~2%S4  CUT-KEEPER TABLE  [conditional bounds; graph candidates]~%")
    (format t "~A~%" (make-string 62 :initial-element #\-))
    (format t "  SCOPE: graph reachability relaxes all other doors, elevation and cargo conditions.~%")
    (format t "  It omits non-traversal relocation, support transitions and recorder lifecycle.~%")
    (format t "  Graph cuts and approach-only regions therefore do NOT prove concrete stranding.~%")
    (format t "  Pressure counts require ON's functional key and the relevant view's occupancy semantics.~%")
    (format t "  Device-state conclusions additionally require the printed S1 axiom premises.~%")
    (multiple-value-bind (spine reason) (keeper-spine rows (mapcar #'first blocks) devices)
      (format t "~%  controlled devices (~D); spine rows (~D); spine check ~S~%"
              (length controls) (length spine) (or reason :verified-against-quotient))
      (when (quotient-alternative-families-p rows)
        (format t "  alternative door families: using the unreduced quotient; ~
                   any clause avoiding the forbidden device permits a row.~%"))
      (dolist (fact controls)
        (report-keeper-device fact spine reason facts names axioms pressure-plates))
      (report-keeper-destinations (get 'goal-fn :form) (database *start-state*)
                                  names spine reason devices))
    (report-keeper-supply)
    (format t "~%  UNCONDITIONAL STRANDING: unresolved; no verdict emitted.~%")
    (values)))


;;; T16 -- RC relay chain table (G17)
;;;
;;; Not S6: S6's specification is sealed and is not amended.  RC answers what S6 does not
;;; ask -- which relay placements see each other, which chains carry a source's beam to a
;;; receiver, and what each chain commits in bodies, supports and device states.  It is
;;; defined here, after S4, because it reuses S4's pressure-clause helpers; the profile
;;; prints it after S6.  Named substrate interfaces: the relay types CONNECTOR,
;;; TRANSMITTER, RECEIVER, FLOOR-REPEATER and WALL-REPEATER, the relations HAS-POSITION,
;;; HAS-CHROMA and COUPLED, and the BEAM-VISIBLE query, as beam-relay.lisp consumes them.
;;; A connector-to-connector hop is tested oriented target-first, as
;;; PAIRED-RELAY-VISIBLE-FOR-OBJECT orients it when the lit relay is the source.


(defun relay-chain-plate-locations ()
  "Each pressure plate paired with its HAS-POSITION location."
  (loop for fact in (list-static-db)
        when (and (consp fact)
                  (eq (first fact) 'has-position)
                  (member (second fact) (census-type-instances 'pressure-plate)))
          collect (cons (second fact) (third fact))))


(defun relay-chain-support-bodies (support)
  "Bodies a placement support commits under a connector: none for the ground, the tray and
 the agent holding it for a held tray, and the support itself otherwise."
  (cond ((eq support 'ground) 0)
        ((member support (census-type-instances 'tray)) 2)
        (t 1)))


(defun relay-chain-stations (state)
  "Every location with every connector top achievable there: the location's level plus an
 S5 placement support's top plus the connector's height.  Each station is
 (location top supports plates), where SUPPORTS give that top and PLATES sit there.  A
 problem with no connector has no stations: a station is a place to set one."
  (when (census-type-instances 'connector)
    (let ((supports (height-lattice-placement-supports state))
          (height (funcall (symbol-function 'object-height)
                           state (first (census-type-instances 'connector))))
          (plates (relay-chain-plate-locations))
          (stations nil))
      (dolist (location (census-type-instances 'location) (nreverse stations))
        (let ((level (funcall (symbol-function 'location-elevation) state location))
              (tops nil))
          (dolist (support supports)
            (let* ((top (+ level (second support) height))
                   (entry (assoc top tops)))
              (if entry
                (push (first support) (rest entry))
                (push (list top (first support)) tops))))
          (dolist (entry (sort tops #'< :key #'first))
            (push (list location
                        (first entry)
                        (reverse (rest entry))
                        (loop for (plate . place) in plates
                              when (eq place location) collect plate))
                  stations)))))))


(defun relay-chain-repeaters ()
  "The fixed relays."
  (append (census-type-instances 'floor-repeater)
          (census-type-instances 'wall-repeater)))


(defun relay-chain-chroma (object)
  "OBJECT's authored HAS-CHROMA hue, or NIL."
  (third (find-if (lambda (fact)
                    (and (consp fact) (eq (first fact) 'has-chroma) (eq (second fact) object)))
                  (list-static-db))))


(defun relay-chain-coupling-count ()
  "Authored fixed couplings, which RC reports but does not enumerate."
  (count-if (lambda (fact) (and (consp fact) (eq (first fact) 'coupled)))
            (list-static-db)))


(defun relay-chain-gate-states (gates)
  "The start-state copy with every gate open, and for each gate the copy with only that
 gate closed.  Like S6, no propagation is applied."
  (values (sightline-state-with-open-gates gates gates)
          (mapcar (lambda (gate)
                    (cons gate (sightline-state-with-open-gates gates (remove gate gates))))
                  gates)))


(defun relay-chain-hop (open-state closed-states near near-top far far-top)
  "NIL when the hop is invisible with every gate open; otherwise (T . gates), the gates
 whose closing alone blocks it.  Exact when a hop is blocked iff some gate it crosses is
 closed -- the monotone, conjunctive reading the acceptance check tests against S6."
  (when (funcall (symbol-function 'beam-visible) open-state near near-top far far-top)
    (cons t (loop for (gate . state) in closed-states
                  unless (funcall (symbol-function 'beam-visible)
                                  state near near-top far far-top)
                    collect gate))))


(defun relay-chain-endpoint-links (stations open-state closed-states)
  "Every visible station-to-fixed-endpoint hop as (station endpoint gates)."
  (let ((links nil))
    (dolist (station stations (nreverse links))
      (dolist (endpoint (sightline-fixed-endpoints))
        (let ((hop (relay-chain-hop open-state closed-states
                                    (first station) (second station)
                                    endpoint
                                    (funcall (symbol-function 'top) open-state endpoint))))
          (when hop
            (push (list station endpoint (rest hop)) links)))))))


(defun relay-chain-station-links (stations open-state closed-states)
  "Every visible directed hop from a lit source station to a target station at another
 location, as (source target gates), tested target-first."
  (let ((links nil))
    (dolist (source stations (nreverse links))
      (dolist (target stations)
        (unless (eq (first source) (first target))
          (let ((hop (relay-chain-hop open-state closed-states
                                      (first target) (second target)
                                      (first source) (second source))))
            (when hop
              (push (list source target (rest hop)) links))))))))


(defun relay-chain-extend (node hops used-locations used-repeaters budget receiver
                           endpoint-links station-links)
  "Every completion of a partial chain whose last relay is NODE (a station or a repeater)
 that ends at RECEIVER.  HOPS is the reversed hop list; BUDGET counts connectors left.
 One connector per location, each repeater once, and a receiver fed only by a connector."
  (let ((chains nil))
    (if (symbolp node)
      (dolist (link endpoint-links)
        (when (and (eq (second link) node)
                   (plusp budget)
                   (not (member (first (first link)) used-locations)))
          (setf chains
                (append chains
                        (relay-chain-extend (first link)
                                            (cons (list node (first link) (third link)) hops)
                                            (cons (first (first link)) used-locations)
                                            used-repeaters (1- budget) receiver
                                            endpoint-links station-links)))))
      (progn
        (dolist (link endpoint-links)
          (when (eq (first link) node)
            (cond ((eq (second link) receiver)
                   (push (reverse (cons (list node receiver (third link)) hops)) chains))
                  ((and (member (second link) (relay-chain-repeaters))
                        (not (member (second link) used-repeaters)))
                   (setf chains
                         (append chains
                                 (relay-chain-extend (second link)
                                                     (cons (list node (second link) (third link))
                                                           hops)
                                                     used-locations
                                                     (cons (second link) used-repeaters)
                                                     budget receiver
                                                     endpoint-links station-links)))))))
        (dolist (link station-links)
          (when (and (eq (first link) node)
                     (plusp budget)
                     (not (member (first (second link)) used-locations)))
            (setf chains
                  (append chains
                          (relay-chain-extend (second link)
                                              (cons (list node (second link) (third link)) hops)
                                              (cons (first (second link)) used-locations)
                                              used-repeaters (1- budget) receiver
                                              endpoint-links station-links)))))))
    chains))


(defun relay-chain-enumerate (receiver endpoint-links station-links)
  "Every chain from a hue-matching transmitter to RECEIVER, each a list of hops
 (from to gates), using at most as many connector stations as there are connectors."
  (let ((chains nil)
        (budget (length (census-type-instances 'connector))))
    (dolist (transmitter (census-type-instances 'transmitter) chains)
      (when (eql (relay-chain-chroma transmitter) (relay-chain-chroma receiver))
        (dolist (link endpoint-links)
          (when (and (eq (second link) transmitter) (plusp budget))
            (setf chains
                  (append chains
                          (relay-chain-extend (first link)
                                              (list (list transmitter (first link) (third link)))
                                              (list (first (first link)))
                                              nil (1- budget) receiver
                                              endpoint-links station-links)))))))))


(defun relay-chain-exclusion-pairs (controls)
  "S1's exclusion pairs: devices on an identical clause set with opposite polarity."
  (loop for (fact . rest) on controls
        append (loop for other in rest
                     when (and (equal (clause-set-key (second fact))
                                      (clause-set-key (second other)))
                               (not (eq (fourth fact) (fourth other))))
                       collect (list (third fact) (third other)))))


(defun relay-chain-receiver-devices (receiver controls)
  "Devices whose control clauses name RECEIVER."
  (loop for fact in controls
        when (some (lambda (clause) (member receiver clause)) (second fact))
          collect (third fact)))


(defun relay-chain-assign-risers (stations)
  "One support per station, fewest bodies first, never reusing a support object and never
 holding more trays than there are agents.  Returns the supports, or :INFEASIBLE."
  (let ((used nil)
        (holders 0)
        (chosen nil))
    (dolist (station stations (nreverse chosen))
      (let ((pick (find-if (lambda (support)
                             (or (eq support 'ground)
                                 (and (not (member support used))
                                      (or (/= (relay-chain-support-bodies support) 2)
                                          (< holders (length (census-type-instances 'agent)))))))
                           (sort (copy-list (third station)) #'<
                                 :key #'relay-chain-support-bodies))))
        (unless pick
          (return-from relay-chain-assign-risers :infeasible))
        (unless (eq pick 'ground)
          (push pick used))
        (when (= (relay-chain-support-bodies pick) 2)
          (incf holders))
        (push pick chosen)))))


(defun wall-stream-strikes-p (base top stream)
  "Wall-blower's body interval: exclusive base, inclusive top. Fans are excluded by callers."
  (and (< base stream) (<= stream top)))


(defun wall-stream-drives ()
  "Fixed wall blowers and removable wall drives, in deterministic name order."
  (keeper-sorted-set (append (census-type-instances 'wall-blower)
                             (census-type-instances 'wall-gears))))


(defun relay-state-equals-aggregate-p (relation device axioms)
  "An S1 axiom admits DEVICE and proves this physical state equals its aggregate."
  (and (relation-keys-a-device-p relation (list device))
       (some (lambda (axiom)
               (and (eq (first axiom) relation) (eq (fifth axiom) :aggregate))) axioms)))


(defun relay-wall-required-gates (drive gates controls axioms)
  "Required physical gates that force a fixed wall drive active, under S1's proven premises."
  (let ((control (find drive controls :key #'third)))
    (when (and control (member drive (census-type-instances 'wall-blower))
               (relay-state-equals-aggregate-p 'turning drive axioms))
      (remove-if-not
        (lambda (gate)
          (let ((other (find gate controls :key #'third)))
            (and other (eq (fourth control) (fourth other))
                 (equal (clause-set-key (second control)) (clause-set-key (second other)))
                 (relay-state-equals-aggregate-p 'open gate axioms))))
        gates))))


(defun relay-wall-exposures (stations gates controls)
  "Conditional station contacts, not a stability simulation or a body/view assignment."
  (let* ((state *start-state*)
         (static (list-static-db))
         (height (when stations
                   (funcall (symbol-function 'object-height) state
                            (first (census-type-instances 'connector)))))
         (axioms (when (wall-stream-drives) (device-state-axioms controls)))
         (rows nil))
    (dolist (station stations (nreverse rows))
      (dolist (drive (wall-stream-drives))
        (when (eq (first station) (keeper-fact-value 'has-position drive static))
          (let* ((base (- (second station) height))
                 (stream (funcall (symbol-function 'blower-elevation) state drive))
                 (struck (wall-stream-strikes-p base (second station) stream)))
            (push (list :station station :drive drive :base base :top (second station)
                        :stream stream :struck struck
                        :destination (keeper-fact-value 'aimed-at drive static)
                        :live-conflict-gates (when struck
                                              (relay-wall-required-gates drive gates controls axioms)))
                  rows)))))))


(defun relay-wall-exposure-text (row)
  "Contact and same-view gate conflict, without certifying a ghost or support placement."
  (format nil "~(~A~) at ~(~A~): base ~A, top ~A, stream ~A; ~A; destination ~(~A~)~A"
          (getf row :drive) (first (getf row :station))
          (getf row :base) (getf row :top) (getf row :stream)
          (if (getf row :struck)
            "SWEPT if a fan is present and turns in the occupant's view"
            "connector body UNSWEPT; support motion still unresolved")
          (getf row :destination)
          (if (getf row :live-conflict-gates)
            (format nil "; LIVE STATION CONFLICT while physical ~(~{~A~^, ~}~) open (S1 state/aggregate equivalence); ghost fan state independent"
                    (getf row :live-conflict-gates))
            "; required fan activity not established")))


(defun relay-chain-qualification-notes (chain)
  "Qualifications travel with RC chains into NH and other diagnostic consumers."
  (cons "GEOMETRIC CANDIDATE, physical sightlines only; recording sightlines, body/view assignment and occupancy stability UNRESOLVED"
        (mapcar #'relay-wall-exposure-text (getf chain :wall-exposures))))


(defun relay-chain-evaluate (hops exclusions receiver-devices controls pressure-plates)
  "One chain's gate set, class and body commitments as a plist."
  (let* ((stations (remove-if-not #'consp (mapcar #'second hops)))
         (gates (keeper-sorted-set (loop for hop in hops append (copy-list (third hop)))))
         (risers (relay-chain-assign-risers stations))
         (plated (count-if #'fourth stations))
         (bodies (unless (eq risers :infeasible)
                   (+ (length stations)
                      (reduce #'+ (mapcar #'relay-chain-support-bodies risers)))))
         (needed (keeper-sorted-set
                  (loop for gate in gates
                        append (let ((fact (find gate controls :key #'third)))
                                 (when fact
                                   (copy-list (keeper-mandatory-plates
                                               (keeper-pressure-clauses fact pressure-plates))))))))
         (class (cond ((some (lambda (pair) (subsetp pair gates)) exclusions) :excluded)
                      ((eq risers :infeasible) :infeasible)
                      ((intersection gates receiver-devices) :latch)
                      (t :bootstrap))))
    (list :hops hops :stations stations :gates gates :risers risers :class class
          :bodies bodies :plated plated :off-plate (when bodies (- bodies plated))
          :wall-exposures (relay-wall-exposures stations gates controls)
          :plates-needed needed
          :self-kept (keeper-sorted-set
                      (intersection needed (loop for station in stations
                                                 append (copy-list (fourth station))))))))


(defun relay-chain-node-text (node)
  "A station as location@top; any other node by name."
  (if (consp node)
    (format nil "~(~A~)@~A" (first node) (second node))
    (format nil "~(~A~)" node)))


(defun report-relay-chain-stations (stations)
  "One line per location: its achievable connector tops, their supports, and its plates."
  (format t "~%  stations (~D): location, then top (supports) per achievable connector top~%"
          (length stations))
  (dolist (location (census-type-instances 'location))
    (let ((rows (remove-if-not (lambda (station) (eq (first station) location)) stations)))
      (format t "    ~(~A~)~{  ~A~}~@[  plate ~(~{~A~^, ~}~)~]~%"
              location
              (mapcar (lambda (station)
                        (format nil "~A (~(~{~A~^ ~}~))" (second station) (third station)))
                      rows)
              (fourth (first rows))))))


(defun relay-chain-top-count (location links)
  "How many station tops LOCATION has, read from the stations the hop list names."
  (length (remove-duplicates
           (loop for link in links
                 append (loop for station in (list (first link) (second link))
                              when (eq (first station) location)
                                collect (second station))))))


(defun report-relay-chain-links (links)
  "Station-to-station hops grouped by ordered location pair and gate requirement."
  (let ((groups nil))
    (dolist (link links)
      (let* ((key (list (first (first link)) (first (second link)) (third link)))
             (entry (assoc key groups :test #'equal)))
        (if entry
          (push (list (second (first link)) (second (second link))) (rest entry))
          (push (list key (list (second (first link)) (second (second link)))) groups))))
    (format t "~%  station-to-station hops (~D visible, ~D groups): source -> target, ~
               source>target tops, gates required~%"
            (length links) (length groups))
    (dolist (entry (reverse groups))
      (destructuring-bind (source target gates) (first entry)
        (format t "    ~(~A~) -> ~(~A~)  ~:[~{~A~^ ~}~;all tops~*~]  ~
                   ~:[ALWAYS~;requires open ~(~{~A~^, ~}~)~]~%"
                source target
                (= (length (rest entry))
                   (* (relay-chain-top-count source links) (relay-chain-top-count target links)))
                (mapcar (lambda (pair) (format nil "~A>~A" (first pair) (second pair)))
                        (reverse (rest entry)))
                gates gates)))))


(defun report-relay-chain (index chain exclusions)
  "One chain: its path and class; an excluded chain names its exclusion pair, any other
 chain adds its gates and body commitments."
  (format t "    ~D  ~A~{ -> ~A~}  ~A"
          index
          (relay-chain-node-text (first (first (getf chain :hops))))
          (mapcar (lambda (hop) (relay-chain-node-text (second hop))) (getf chain :hops))
          (getf chain :class))
  (when (eq (getf chain :class) :excluded)
    (format t " on {~(~{~A~^, ~}~)}~%"
            (find-if (lambda (pair) (subsetp pair (getf chain :gates))) exclusions))
    (return-from report-relay-chain))
  (terpri)
  (format t "        gates ~(~{~A~^, ~}~)~:[ (none)~;~]; plates for them ~(~{~A~^, ~}~)~:[ (none)~;~]~
             ~@[, self-kept ~(~{~A~^, ~}~)~]~%"
          (getf chain :gates) (getf chain :gates)
          (getf chain :plates-needed) (getf chain :plates-needed)
          (getf chain :self-kept))
  (if (eq (getf chain :risers) :infeasible)
    (format t "        risers INFEASIBLE from the support pool~%")
    (format t "        connectors ~D; risers ~(~{~A~^, ~}~); bodies ~D, on plates ~D, off plates ~D~%"
            (length (getf chain :stations)) (getf chain :risers)
            (getf chain :bodies) (getf chain :plated) (getf chain :off-plate)))
  (dolist (note (relay-chain-qualification-notes chain))
    (format t "        ~A~%" note)))


(defun report-relay-chain-summary (receiver chains)
  "The bootstrap chains' common gates and least off-plate commitments, overall and per
 number of connectors used."
  (let ((bootstrap (remove-if-not (lambda (chain) (eq (getf chain :class) :bootstrap)) chains)))
    (format t "~%  summary for ~(~A~)~%" receiver)
    (format t "    chains ~D: bootstrap ~D, latch ~D, excluded ~D, infeasible ~D~%"
            (length chains) (length bootstrap)
            (count :latch chains :key (lambda (chain) (getf chain :class)))
            (count :excluded chains :key (lambda (chain) (getf chain :class)))
            (count :infeasible chains :key (lambda (chain) (getf chain :class))))
    (if (null bootstrap)
      (format t "    no geometric bootstrap candidate in RC scope; receiver activation outside this scope UNRESOLVED~%")
      (progn
        (format t "    gates open in every bootstrap chain: ~(~{~A~^, ~}~)~:[ none~;~]~%"
                (keeper-sorted-set (reduce #'intersection
                                           (mapcar (lambda (chain) (getf chain :gates)) bootstrap)))
                (reduce #'intersection (mapcar (lambda (chain) (getf chain :gates)) bootstrap)))
        (format t "    least off-plate bodies over bootstrap chains: ~D~%"
                (reduce #'min (mapcar (lambda (chain) (getf chain :off-plate)) bootstrap)))
        (dolist (count (sort (remove-duplicates
                              (mapcar (lambda (chain) (length (getf chain :stations))) bootstrap))
                             #'<))
          (let ((group (remove-if-not (lambda (chain) (= (length (getf chain :stations)) count))
                                      bootstrap)))
            (format t "    with ~D connector~:P: ~D chain~:P, least off-plate bodies ~D, ~
                       receiver-end stations ~{~A~^, ~}~%"
                    count (length group)
                    (reduce #'min (mapcar (lambda (chain) (getf chain :off-plate)) group))
                    (remove-duplicates
                     (mapcar (lambda (chain)
                               (relay-chain-node-text
                                (first (car (last (getf chain :hops))))))
                             group)
                     :test #'string=))))))))


;;;; T33 -- SUPPLIED RELAY VIEW SCENARIOS ;;;;
;;; Specification 6.1. Queries only, on private copies; no settling or search.

(defun relay-view-call (name state &rest arguments)
  (apply (symbol-function name) state arguments))

(defun relay-view-input-reason (scenario)
  "Missing data is unavailable analysis, never a failed sightline."
  (cond
    ((null scenario) "no explicit scenario supplied")
    ((not (typep (getf scenario :state) 'problem-state)) "missing reference problem-state")
    ((not (eq t (getf scenario :complete-state))) "complete-state assertion missing")
    ((not (member (getf scenario :phase) '(:ordinary :open :closed :shadow-only)))
     "explicit recorder phase missing or unsupported")
    ((not (and (stringp (getf scenario :provenance))
               (plusp (length (getf scenario :provenance))))) "state provenance missing")
    ((or (eq :missing (getf scenario :hops :missing))
         (eq :missing (getf scenario :chains :missing))) "explicit hops/chains keys missing")
    ((not (or (getf scenario :hops) (getf scenario :chains))) "no supplied hop or chain")))

(defun relay-view-mappings ()
  "Canonicalize the bijective storage indexes, which preserve argument order."
  (remove-duplicates
   (loop for fact in (list-static-db)
         when (eq 'recording-copy>
                  (or (car (gethash (first fact) *bijective-canonical*)) (first fact)))
           collect (list 'recording-copy> (second fact) (third fact)))
   :test #'equal))

(defun relay-view-context-reason (scenario view mappings)
  (let* ((facts (database (getf scenario :state)))
         (open (member '(recording-in-progress) facts :test #'equal))
         (closed (member '(recorder-cycle-closed) facts :test #'equal))
         (phase (getf scenario :phase)))
    (cond
      ((not (case phase
              (:ordinary (null mappings))
              (:open (and mappings open (not closed)))
              (:closed (and mappings closed (not open)))
              (:shadow-only (and mappings (not open) (not closed)))))
       "declared phase disagrees with reference state/mapping")
      ((and (eq view :recording) (or (null mappings) (eq phase :closed)))
       "recording context unavailable in this phase")
      ((not (member "visibility" *spliced-tech-names* :test #'string=))
       "visibility technology unavailable (neutral hook is not an analysis)")
      ((and (getf scenario :chains)
            (not (member "beam-relay" *spliced-tech-names* :test #'string=)))
       "relay technology unavailable"))))

(defun relay-view-gate-reason (premises)
  (let ((seen nil))
    (dolist (premise premises)
      (destructuring-bind (view gate value) premise
        (cond
          ((not (and (member view '(:physical :recording))
                     (member gate (census-type-instances 'gate))
                     (member value '(t nil))))
           (return-from relay-view-gate-reason "invalid gate premise"))
          ((member (list view gate) seen :test #'equal)
           (return-from relay-view-gate-reason "duplicate/conflicting gate premise"))
          ((and (eq view :recording) (not (gethash 'recording-open *relations*)))
           (return-from relay-view-gate-reason "recording gate context unavailable")))
        (push (list view gate) seen)))))

(defun relay-view-state (scenario)
  (let ((state (copy-problem-state (getf scenario :state))))
    (dolist (premise (getf scenario :gate-premises))
      (destructuring-bind (view gate value) premise
        (let ((fact (list (if (eq view :physical) 'open 'recording-open) gate)))
          (if value
            (add-proposition fact (problem-state.idb state))
            (delete-proposition fact (problem-state.idb state))))))
    (invalidate-problem-state-hash state)
    state))

(defun relay-view-hop-blockers (state selector near near-top far far-top)
  "Explain failed engine visibility using its own per-barrier and per-location queries."
  (let ((fact (find-if (lambda (row) (and (eq (first row) 'los-via)
                                         (eq (second row) near) (eq (fourth row) far)))
                       (list-static-db))))
    (unless fact (return-from relay-view-hop-blockers (list "no LOS entry")))
    (let* ((crossings (relay-view-call 'los-barrier-crossings state near far))
           (barriers
             (unless (eq crossings :unrecorded)
               (loop for crossing in crossings
                     unless (relay-view-call 'barrier-crossing-clear-for-object
                                             state selector crossing near-top far-top)
                       collect (format nil "barrier ~S" (subseq crossing 0 2)))))
           (occluders
             (loop for object in (third fact)
                   when (if (member object (census-type-instances 'gate))
                          (and (eq crossings :unrecorded)
                               (not (relay-view-call 'gate-open-for-object state selector object)))
                          (relay-view-call 'los-location-occluded state selector object
                                           near near-top far far-top))
                     collect (format nil "occluder ~S" object))))
      (append barriers occluders))))

(defun relay-view-hop-result (state selector hop)
  (destructuring-bind (near near-top far far-top) hop
    (if (not (and (member near (census-type-instances 'location))
                  (member far (append (census-type-instances 'location)
                                      (sightline-fixed-endpoints)))
                  (realp near-top) (realp far-top)))
      (list :input hop :status :invalid :reason "invalid geometric endpoint or height")
      (let ((clear (relay-view-call 'beam-visible-for-object
                                    state selector near near-top far far-top)))
        (list :input hop :status (if clear :clear :blocked)
              :reason (if clear "engine sightline clear"
                          (format nil "engine sightline blocked: ~{~A~^; ~}"
                                  (relay-view-hop-blockers state selector near near-top far far-top))))))))

(defun relay-view-pairing-reason (state connector mappings)
  "Outgoing pairings are owned selections; incoming links do not use that capacity."
  (let* ((facts (database state))
         (pairs (remove-if-not (lambda (fact)
                                (and (eq (first fact) 'paired)
                                     (eq (second fact) connector))) facts))
         (ghost (find connector mappings :key #'third))
         (live (find connector mappings :key #'second)))
    (cond
      ((> (length pairs) *max-connector-pairings*) "outgoing pairing capacity exceeded")
      ((some (lambda (pair) (eq (third pair) connector)) pairs) "connector paired to itself")
      ((some (lambda (pair)
               (let ((target (third pair)))
                 (and (member target (census-type-instances 'connector))
                      (or (and ghost (not (find target mappings :key #'third)))
                          (and live (find target mappings :key #'third)
                               (not (member '(recording-in-progress) facts :test #'equal)))))))
             pairs)
       "pairing violates recording-side dependency policy"))))

(defun relay-view-chain-input-reason (state chain mappings)
  (cond
    ((not (and (>= (length chain) 3)
               (member (first chain) (census-type-instances 'transmitter))
               (member (car (last chain)) (census-type-instances 'receiver))
               (every (lambda (object) (member object (census-type-instances 'relay)))
                      (butlast (rest chain))))) "chain requires source, relays and receiver")
    ((/= (length chain) (length (remove-duplicates chain))) "chain reuses an identity")
    (t (loop for object in (butlast (rest chain))
             thereis (and (member object (census-type-instances 'connector))
                          (relay-view-pairing-reason state object mappings))))))

(defun relay-view-relay-absent-reason (state selector relay)
  (cond
    ((and selector (not (relay-view-call 'recording-shadow-object-present state relay)))
     "relay absent from recording view")
    ((and (member relay (census-type-instances 'connector))
          (not (relay-view-call 'relay-anchor state relay))) "connector has no beam location")))

(defun relay-view-final-link (state selector source receiver active)
  "Check this particular final relay, not merely some relay reaching the receiver."
  (let ((anchor (relay-view-call 'relay-anchor state source)))
    (and
      (if (member source (census-type-instances 'connector))
        (and (member (list 'paired source receiver) (database state) :test #'equal)
             (relay-view-call 'beam-visible-for-object state selector anchor
                              (relay-view-call 'top state source) receiver
                              (relay-view-call 'top state receiver)))
        (and (member (list 'coupled source receiver) (list-static-db) :test #'equal)
             (relay-view-call 'fixed-beam-corridor-clear-for-object state selector source receiver)))
      (or selector (not (relay-view-call 'beam-cut-in state anchor receiver active))))))

(defun relay-view-link-result (state selector source target active)
  (let ((clear
          (if (member target (census-type-instances 'receiver))
            (relay-view-final-link state selector source target active)
            (relay-view-call 'relay-link-clear-for-object state selector source
                             (if (member source (census-type-instances 'transmitter))
                               source (relay-view-call 'relay-anchor state source))
                             target (relay-view-call 'relay-anchor state target) active))))
    (list :input (list source target) :status (if clear :clear :blocked)
          :reason (if clear "engine relay link clear"
                      "required pairing/coupling, sightline or beam-cut check failed"))))

(defun relay-view-install-lighting (state lighting)
  "Physical receiver query reads COLOR; install only computed colors on the private copy."
  (dolist (fact (database state))
    (when (eq (first fact) 'color) (delete-proposition fact (problem-state.idb state))))
  (dolist (record lighting)
    (add-proposition (list 'color (first record) (second record)) (problem-state.idb state)))
  (invalidate-problem-state-hash state))

(defun relay-view-chain-result (state selector chain mappings)
  (let ((invalid (relay-view-chain-input-reason state chain mappings)))
    (when invalid
      (return-from relay-view-chain-result (list :input chain :status :invalid :reason invalid))))
  (let* ((relays (butlast (rest chain)))
         (absent (loop for relay in relays
                       for reason = (relay-view-relay-absent-reason state selector relay)
                       when reason collect (list relay reason))))
    (when absent
      (return-from relay-view-chain-result
        (list :input chain :status :blocked :reason (format nil "~S" absent))))
    (let* ((active (unless selector (relay-view-call 'current-crossing-set state)))
           (links (loop for (source target) on chain while target
                        collect (relay-view-link-result state selector source target active)))
           (lighting (relay-view-call 'compute-relay-lighting-for-object state selector active))
           (hue (relay-chain-chroma (first chain)))
           (receiver (car (last chain)))
           (unlit (remove-if (lambda (relay) (eql hue (second (assoc relay lighting)))) relays)))
      (unless selector (relay-view-install-lighting state lighting))
      (let* ((reaches (if selector
                       (relay-view-call 'recording-shadow-relay-beam-reaches-receiver
                                        state selector lighting receiver)
                       (relay-view-call 'relay-beam-reaches-receiver state receiver)))
             (reason (cond
                       ((some (lambda (row) (eq :blocked (getf row :status))) links)
                        "required chain link failed; see link rows")
                       ((not (and hue (eql hue (relay-chain-chroma receiver))))
                        "source/receiver hue mismatch or missing chroma")
                       (unlit (format nil "relays not lit with source hue: ~S" unlit))
                       ((not reaches) "engine receiver evaluation failed")
                       (t "all supplied links and engine lighting/receiver checks pass"))))
        (list :input chain :status (if (and reaches (not unlit)
                                           hue (eql hue (relay-chain-chroma receiver))
                                           (every (lambda (row) (eq :clear (getf row :status))) links))
                                      :clear :blocked)
              :reason reason :links links :lighting lighting :receiver-reached (not (null reaches)))))))

(defun relay-view-one-result (scenario view)
  (let ((missing (relay-view-input-reason scenario)))
    (when missing
      (return-from relay-view-one-result (list :view view :status :unresolved :reason missing))))
  (let* ((mappings (relay-view-mappings))
         (reason (or (relay-view-context-reason scenario view mappings)
                     (relay-view-gate-reason (getf scenario :gate-premises)))))
    (when reason
      (return-from relay-view-one-result
        (list :view view :phase (getf scenario :phase) :status :unresolved :reason reason)))
    (let* ((state (relay-view-state scenario))
           (selector (when (eq view :recording)
                       (relay-view-call 'recording-shadow-view-object state)))
           (hops (mapcar (lambda (hop) (relay-view-hop-result state selector hop))
                         (getf scenario :hops)))
           (chains (mapcar (lambda (chain) (relay-view-chain-result state selector chain mappings))
                           (getf scenario :chains)))
           (statuses (mapcar (lambda (row) (getf row :status)) (append hops chains))))
      (list :view view :phase (getf scenario :phase)
            :status (cond ((member :invalid statuses) :invalid)
                          ((member :blocked statuses) :blocked) (t :clear))
            :reason "supplied tests only; stability, reachability and replay validation UNRESOLVED"
            :hops hops :chains chains))))

(defun relay-view-results (scenario)
  "Two independent views of one explicit scenario; never inferred from geometric candidates."
  (mapcar (lambda (view)
            (append (relay-view-one-result scenario view)
                    (list :provenance (getf scenario :provenance)
                          :gate-premises (copy-tree (getf scenario :gate-premises))
                          :phase-premise (getf scenario :phase)
                          :complete-state-premise (getf scenario :complete-state))))
          '(:physical :recording)))

(defun report-relay-view-scenario (scenario)
  (let ((*print-pretty* nil))
    (format t "~%  SUPPLIED RELAY SCENARIO (T33): CLEAR is conditional, not stable/reachable/validated.~%")
    (when scenario
      (format t "    provenance: ~A; phase: ~S~%" (getf scenario :provenance) (getf scenario :phase))
      (format t "    gate premises: ~S; forced bits do not establish controller consistency.~%"
              (getf scenario :gate-premises))
      (when (typep (getf scenario :state) 'problem-state)
        (format t "    reference facts (all other gates/placements/pairings inherited): ~S~%"
                (database (getf scenario :state)))
        (format t "    live/ghost mapping: ~S~%" (relay-view-mappings))))
    (dolist (result (relay-view-results scenario))
      (format t "    ~A ~A: ~A~%" (getf result :view) (getf result :status) (getf result :reason))
      (dolist (kind '(:hops :chains))
        (dolist (row (getf result kind))
          (format t "      ~A ~S ~A: ~A~%" kind (getf row :input) (getf row :status) (getf row :reason))
          (dolist (link (getf row :links))
            (format t "        ~S ~A: ~A~%" (getf link :input) (getf link :status) (getf link :reason)))))))
  (values))

(defun fixed-beam-record (coupling facts state)
  "One fixed corridor's authored/recorded dependencies, not a relay-chain realization."
  (let* ((source (second coupling))
         (sink (third coupling))
         (authored (find-if (lambda (fact)
                             (and (eq (first fact) 'beam-via) (eq (second fact) source)
                                  (eq (fourth fact) sink))) facts))
         (recorded (find-if (lambda (fact)
                             (and (eq (first fact) 'los-barrier-crossings>)
                                  (eq (second fact) source) (eq (fourth fact) sink))) facts))
         (obstacles (third authored))
         (crossings (third recorded)))
    (list :source source :sink sink :authored-p (not (null authored))
          :obstacles obstacles :crossings (if recorded crossings :unrecorded)
          :gates (keeper-sorted-set
                   (append (intersection obstacles (census-type-instances 'gate))
                           (loop for crossing in crossings
                                 when (eq (first crossing) :gate) collect (second crossing))))
          :locations (keeper-sorted-set (intersection obstacles (census-type-instances 'location)))
          :source-hue (relay-chain-chroma source) :sink-hue (relay-chain-chroma sink)
          :corridor-clear (and authored
                               (funcall (symbol-function 'fixed-beam-corridor-clear)
                                        state source sink)))))


(defun fixed-beam-records ()
  "Fixed links only when BEAM-DIRECT is actually spliced; no simulated changes."
  (when (member "beam-direct" *spliced-tech-names* :test #'string=)
    (let ((facts (list-static-db)) (state (copy-problem-state *start-state*)))
      (loop for coupling in (sort (remove-if-not (lambda (fact) (eq (first fact) 'coupled)) facts)
                                 #'string< :key #'prin1-to-string)
            collect (fixed-beam-record coupling facts state)))))


(defun report-fixed-beam-corridors ()
  "MC/RC share fixed-link data; gate candidates are qualified by height and view."
  (let ((records (fixed-beam-records)) (*print-pretty* nil))
    (when records
      (format t "~%  fixed beam corridors (~D), staged physical view~%" (length records))
      (dolist (row records)
        (format t "    ~(~A~) -> ~(~A~): BEAM-VIA ~:[MISSING~;present~]; authored ~(~S~); recorded ~(~S~)~%"
                (getf row :source) (getf row :sink) (getf row :authored-p)
                (getf row :obstacles) (getf row :crossings))
        (format t "      gate candidates ~(~S~); occupancy locations ~(~S~); chromas ~(~A~)/~(~A~); corridor clear now ~S~%"
                (getf row :gates) (getf row :locations) (getf row :source-hue)
                (getf row :sink-hue) (getf row :corridor-clear)))
      (format t "    Recorded barriers use finite spans and interpolated beam height; authored gates without geometry require open. Locations block only when an occupant spans that height.~%")
      (format t "    Corridor clearance is not receiver activation: matching chromas, BEAM-CUT and upstream repeater lighting also matter. Other beam routes, recording views, stability and reachability are not established.~%"))))


(defun report-relay-chain-table (&optional scenario)
  "RC, grade 2.  Stations from S5's placement supports at every location level; hops by
 BEAM-VISIBLE on start-state copies with gate bits forced, no propagation or search; every
 simple source-to-receiver chain within the connector pool, classified and costed."
  (let* ((*print-pretty* nil)
         (state *start-state*)
         (gates (census-type-instances 'gate))
         (controls (control-facts))
         (exclusions (relay-chain-exclusion-pairs controls))
         (pressure-plates (census-type-instances 'pressure-plate))
         (stations (relay-chain-stations state)))
    (format t "~2%RC  RELAY CHAIN TABLE  [grade 2]~%")
    (report-relay-view-scenario scenario)
    (format t "~A~%" (make-string 62 :initial-element #\-))
    (format t "  GEOMETRIC ENUMERATION SCOPE: physical view only; supplied scenario views are separate.  Hops are~%")
    (format t "  tested on start-state copies with gate OPEN bits forced and no propagation.  A~%")
    (format t "  hop's required gates are those whose closing alone blocks it (monotone reading).~%")
    (format t "  Stations are placements, not reachability claims.  Bodies count the connector and~%")
    (format t "  its riser (ground 0, held tray 2, other support 1); a station with a plate is~%")
    (format t "  counted as keeping it, so off-plate counts are least values within this enumeration.~%")
    (format t "  GEOMETRIC CANDIDATES: stable simultaneous occupancy and support motion UNRESOLVED.~%")
    (format t "  BOOTSTRAP/LATCH are geometric classes, not validated realizations. Connector height~%")
    (format t "  is that used by station enumeration; differing connector heights are not enumerated.~%")
    (format t "  connectors ~D; fixed couplings ~D (~A); exclusion pairs ~(~{~A~^, ~}~)~%"
            (length (census-type-instances 'connector)) (relay-chain-coupling-count)
            (if (member "beam-direct" *spliced-tech-names* :test #'string=)
              "corridors reported below; mixed chains not enumerated" "not enumerated")
            (mapcar (lambda (pair) (format nil "{~A, ~A}" (first pair) (second pair))) exclusions))
    (report-fixed-beam-corridors)
    (report-relay-chain-stations stations)
    (multiple-value-bind (open-state closed-states) (relay-chain-gate-states gates)
      (let ((endpoint-links (relay-chain-endpoint-links stations open-state closed-states))
            (station-links (relay-chain-station-links stations open-state closed-states)))
        (format t "~%  station-to-endpoint hops (~D visible)~%" (length endpoint-links))
        (dolist (link endpoint-links)
          (format t "    ~A -> ~(~A~)  ~:[ALWAYS~;requires open ~(~{~A~^, ~}~)~]~%"
                  (relay-chain-node-text (first link)) (second link)
                  (third link) (third link)))
        (report-relay-chain-links station-links)
        (dolist (receiver (census-type-instances 'receiver))
          (let* ((devices (relay-chain-receiver-devices receiver controls))
                 (chains (mapcar (lambda (hops)
                                   (relay-chain-evaluate hops exclusions devices
                                                         controls pressure-plates))
                                 (relay-chain-enumerate receiver endpoint-links station-links)))
                 (index 0))
            (format t "~%  chains to ~(~A~) (~D); LATCH needs a device this receiver controls~%"
                    receiver (length chains))
            (dolist (chain (stable-sort (stable-sort (copy-list chains) #'<
                                                     :key (lambda (chain)
                                                            (or (getf chain :off-plate) 99)))
                                        #'<
                                        :key (lambda (chain)
                                               (position (getf chain :class)
                                                         '(:bootstrap :latch :excluded
                                                           :infeasible)))))
              (report-relay-chain (incf index) chain exclusions))
            (report-relay-chain-summary receiver chains)))))
    (values)))


(defun role-class-universe ()
  "The classes a recorder-style layer partition can distinguish."
  '("live" "ghost" "unpaired"))


(defun role-class-complement (classes)
  "The layer classes excluded by CLASSES."
  (set-difference (role-class-universe) classes :test #'string=))


(defun role-query-call-p (form)
  "Whether FORM invokes one of the staged problem's queries."
  (and (consp form)
       (symbolp (first form))
       (member (first form) *query-names*)))


(defun role-raw-argument-map (call)
  "Map a query CALL's raw parameters to its actual arguments."
  (let ((parameters (get (first call) :raw-args))
        (arguments (rest call)))
    (when (= (length parameters) (length arguments))
      (mapcar #'cons parameters arguments))))


(defun role-substitute-parameters (form bindings)
  "Replace query parameters in FORM with the actual arguments of its caller."
  (cond ((symbolp form)
         (or (cdr (assoc form bindings)) form))
        ((consp form)
         (mapcar (lambda (part) (role-substitute-parameters part bindings)) form))
        (t form)))


(defun role-layer-test-classes (form pair-relations visited)
  "The class a layer-pair test in FORM selects, or NIL when it cannot be resolved.
The first argument of a pair is live and the second is ghost.  A test that
does not bind exactly one variable to one pair position remains unresolved."
  (when (consp form)
    (let ((relation (first form)))
      (when (member relation pair-relations)
        (let ((variables (remove-if-not #'?varp (rest form))))
          (when (= (length variables) 1)
            (let ((position (position (first variables) (rest form))))
              (return-from role-layer-test-classes
                (case position
                  (0 '("live"))
                  (1 '("ghost"))
                  (t nil)))))))
      (when (and (role-query-call-p form)
                 (not (member relation visited)))
        (let ((bindings (role-raw-argument-map form)))
          (when bindings
            (let ((classes
                    (role-layer-test-classes
                      (role-substitute-parameters (get relation :raw-body) bindings)
                      pair-relations (cons relation visited))))
              (when classes
                (return-from role-layer-test-classes classes))))))
      (dolist (part form)
        (let ((classes (role-layer-test-classes part pair-relations visited)))
          (when classes
            (return-from role-layer-test-classes classes)))))))


(defun role-governed-relation-sites
    (form relation conditions query visited)
  "Read sites for RELATION, carrying the class tests that govern each one.
IF branches contribute their test with branch polarity.  AND siblings
contribute positive conditions; OR deliberately contributes none."
  (cond ((atom form) nil)
        ((eq (first form) relation)
         (list (list query conditions)))
        ((eq (first form) 'if)
         (append
           (role-governed-relation-sites (second form) relation conditions query visited)
           (role-governed-relation-sites
             (third form) relation
             (cons (cons (second form) t) conditions) query visited)
           (role-governed-relation-sites
             (fourth form) relation
             (cons (cons (second form) nil) conditions) query visited)))
        ((eq (first form) 'and)
         (loop for conjunct in (rest form)
               append (role-governed-relation-sites
                        conjunct relation
                        (append
                          (loop for sibling in (rest form)
                                unless (eq sibling conjunct)
                                  collect (cons sibling t))
                          conditions)
                        query visited)))
        ((eq (first form) 'or)
         (loop for disjunct in (rest form)
               append (role-governed-relation-sites
                        disjunct relation conditions query visited)))
        ((eq (first form) 'not)
         (role-governed-relation-sites
           (second form) relation
           (mapcar (lambda (condition)
                     (cons (car condition) (not (cdr condition))))
                   conditions)
           query visited))
        ((and (role-query-call-p form)
              (not (member (first form) visited)))
         (let ((bindings (role-raw-argument-map form)))
           (if bindings
             (role-governed-relation-sites
               (role-substitute-parameters (get (first form) :raw-body) bindings)
               relation conditions (first form) (cons (first form) visited))
             nil)))
        (t (loop for part in form
                 append (role-governed-relation-sites
                          part relation conditions query visited)))))


(defun role-query-call-governance (form query pair-relations governed)
  "Whether each call to QUERY in FORM occurs under a class-governed branch."
  (cond ((atom form) nil)
        ((eq (first form) query) (list governed))
        ((eq (first form) 'if)
         (let ((class-governed
                 (role-layer-test-classes (second form) pair-relations nil)))
           (append
             (role-query-call-governance (second form) query pair-relations governed)
             (role-query-call-governance (third form) query pair-relations
                                         (or governed class-governed))
             (role-query-call-governance (fourth form) query pair-relations
                                         (or governed class-governed)))))
        (t (loop for part in form
                 append (role-query-call-governance
                          part query pair-relations governed)))))


(defun role-private-view-reader-p (query)
  "Whether every call of QUERY occurs beneath a class-governed view dispatch."
  (let ((calls
          (loop for caller in *query-names*
                unless (eq caller query)
                  append (role-query-call-governance
                           (get caller :raw-body) query
                           (layer-pair-relations) nil))))
    (and calls (every #'identity calls))))


(defun role-relation-read-sites (relation)
  "Every query read site for RELATION, with its governing conditions.
Helpers reached only through class-governed view dispatches are not also
counted as layer-blind roots."
  (loop for query in *query-names*
        unless (role-private-view-reader-p query)
        append (role-governed-relation-sites
                 (get query :raw-body) relation nil query (list query))))


(defun role-site-admission (site pair-relations)
  "SITE's admitted classes, whether it has a class test, and unresolved-test count."
  (let ((admitted (role-class-universe))
        (governed nil)
        (unresolved 0))
    (dolist (condition (second site))
      (let ((classes (role-layer-test-classes (car condition) pair-relations nil)))
        (if classes
          (progn
            (setf governed t)
            (setf admitted
                  (intersection admitted
                                (if (cdr condition)
                                  classes
                                  (role-class-complement classes))
                                :test #'string=)))
          (incf unresolved))))
    (values admitted governed unresolved)))


(declaim (ftype function role-view-classes))


(defun role-axiom-index (relation view)
  "Classify RELATION's consumers against VIEW without changing any allocation."
  (let ((sites (role-relation-read-sites relation))
        (pair-relations (layer-pair-relations))
        (view-classes (role-view-classes view))
        (in-view nil)
        (out-of-view nil)
        (blind 0)
        (unresolved 0)
        (witness nil))
    (dolist (site sites)
      (multiple-value-bind (admitted governed missed)
          (role-site-admission site pair-relations)
        (incf unresolved missed)
        (if governed
          (cond ((and admitted
                      (subsetp admitted view-classes :test #'string=))
                 (setf in-view t witness (list site admitted)))
                ((null (intersection admitted view-classes :test #'string=))
                 (setf out-of-view t)
                 (unless witness
                   (setf witness (list site admitted)))))
          (incf blind))))
    (list (cond (in-view :in-view)
                (out-of-view :out-of-view)
                (t :unindexed))
          witness blind unresolved (length sites))))


(defun role-index-status-text (status)
  "The reporter's stable vocabulary for a computed axiom index."
  (case status
    (:in-view "IN-VIEW")
    (:out-of-view "OUT-OF-VIEW")
    (t "UNINDEXED")))


(defun report-role-axiom-index (axiom view)
  "Append the G14 provenance for one inherited S1 axiom."
  (let* ((index (role-axiom-index (first axiom) view))
         (status (first index))
         (witness (second index))
         (blind (third index))
         (unresolved (fourth index))
         (site (first witness))
         (classes (second witness)))
    (format t "        view index: ~A for ~(~S~)"
            (role-index-status-text status) view)
    (when classes
      (format t "; classes admitted ~(~S~)" classes))
    (when site
      (format t "; reader ~(~A~)" (first site)))
    (format t "~%")
    (format t "        view-blind read sites: ~D~@[; ~D governing test~:P unresolved~]~%"
            blind unresolved)
    (when (eq status :in-view)
      (format t "        NOTE: IN-VIEW is existential: another consumer may read this relation outside the stated view.~%"))
    (when (eq status :out-of-view)
      (format t "        RETAINED: this inherited premise is outside the stated view and is printed for provenance.~%"))))


(defun report-role-axioms (device axioms scenario)
  "RO-local S1 axiom reporter.  It preserves the shared data lines verbatim."
  (let ((matching (remove-if-not
                   (lambda (axiom) (relation-keys-a-device-p (first axiom) (list device)))
                   axioms)))
    (if matching
      (dolist (axiom matching)
        (format t "      state ~(~A~): ~A; empty-type premises ~(~S~)~%"
                (first axiom) (axiom-reading-text (fifth axiom)) (sixth axiom))
        (report-role-axiom-index axiom (getf scenario :view)))
      (format t "      device-state correspondence UNRESOLVED; aggregate requirements only.~%"))
    (when matching
      (let ((indexes (mapcar (lambda (axiom)
                               (role-axiom-index (first axiom) (getf scenario :view)))
                             matching)))
        (format t "      axiom view summary: ~D in-view, ~D out-of-view, ~D unindexed; stated view ~(~S~)~%"
                (count :in-view indexes :key #'first)
                (count :out-of-view indexes :key #'first)
                (count :unindexed indexes :key #'first)
                (getf scenario :view))
        (format t "      NOTE: indexes classify consumer reads, not the bodies whose occupancy derives device state.~%")))))


(defun role-subsets (items)
  "Every subset of ITEMS.  A demanded support set here is single digits wide, so enumerating
   its subsets costs less than a dedicated deficiency search and makes the violator exactly
   reportable -- which is the whole difference between a shortage and a count."
  (if (null items)
    (list nil)
    (let ((rest (role-subsets (rest items))))
      (append rest (mapcar (lambda (subset) (cons (first items) subset)) rest)))))


(defun role-neighbourhood (supports eligibility)
  "The witnesses SUPPORTS can draw on between them, counted once each."
  (remove-duplicates (loop for support in supports
                           append (copy-list (rest (assoc support eligibility))))))


(defun role-hall-violator (supports eligibility)
  "The smallest nonempty support subset whose combined eligibility is smaller than itself.
   This is the REASON a shortage exists.  Two supports restricted to one witness are short
   however large the rest of the pool is, and no total count detects that."
  (let ((violators (remove-if-not
                     (lambda (subset)
                       (and subset (< (length (role-neighbourhood subset eligibility))
                                      (length subset))))
                     (role-subsets supports))))
    (first (sort violators #'< :key #'length))))


(defun role-augment (support eligibility assignment visited)
  "One augmenting path.  ASSIGNMENT is an alist (witness . support); VISITED marks the
   witnesses this search has already tried, so a displaced holder is re-sought at most once.
   Returns an extended assignment, or NIL when SUPPORT cannot be served."
  (dolist (witness (rest (assoc support eligibility)))
    (unless (gethash witness visited)
      (setf (gethash witness visited) t)
      (let ((holder (assoc witness assignment)))
        (if holder
          (let ((extended (role-augment (rest holder) eligibility
                                        (remove holder assignment :count 1) visited)))
            (when extended
              (return-from role-augment (acons witness support extended))))
          (return-from role-augment (acons witness support assignment)))))))


(defun role-matching (supports eligibility)
  "A maximum matching, as an alist (witness . support).  A matching is the right question
   because ON is keyed by its occupant: two simultaneously occupied supports need two
   distinct bodies, which is S2's injectivity and not an assumption RO adds."
  (let ((assignment nil))
    (dolist (support supports assignment)
      (let ((extended (role-augment support eligibility assignment
                                    (make-hash-table :test #'eq))))
        (when extended
          (setf assignment extended))))))


(defun role-perfect-p (supports eligibility)
  "Whether every demanded support can hold its own distinct witness at the same time."
  (= (length supports) (length (role-matching supports eligibility))))


(defun role-eligibility-without-witness (eligibility witness)
  "ELIGIBILITY with WITNESS struck out everywhere, for the forced-membership test."
  (mapcar (lambda (entry) (cons (first entry) (remove witness (rest entry)))) eligibility))


(defun role-eligibility-without-edge (eligibility support witness)
  "ELIGIBILITY with one pairing struck out, for the forced-pairing test."
  (mapcar (lambda (entry)
            (if (eq (first entry) support)
              (cons support (remove witness (rest entry)))
              entry))
          eligibility))


(defun role-forced-witnesses (supports eligibility)
  "A witness is forced when no admissible assignment avoids it.  Called only where a perfect
   matching exists: an empty assignment set forces everything vacuously, and reporting that
   as necessity is the failure this test is shaped to avoid."
  (remove-if (lambda (witness)
               (role-perfect-p supports (role-eligibility-without-witness eligibility witness)))
             (role-neighbourhood supports eligibility)))


(defun role-forced-pairings (supports eligibility)
  "The (support witness) pairs every admissible assignment uses.  Forced membership does NOT
   imply a forced pairing; saturating a pool of exactly three forces all three bodies and
   fixes none of them to a particular support."
  (let ((pairings nil))
    (dolist (support supports (nreverse pairings))
      (dolist (witness (rest (assoc support eligibility)))
        (unless (role-perfect-p supports
                                (role-eligibility-without-edge eligibility support witness))
          (push (list support witness) pairings))))))


(defun role-view-classes (view)
  "The layer classes a view admits.  An unpaired object is not a copy of anything, so it is
   present in every view, exactly as S2's bound counts it in every layer."
  (case view
    (:physical (list "live" "unpaired"))
    (:recording (list "ghost" "unpaired"))))


(defun role-view-pool (view pairs)
  "ON's occupant pool restricted to the classes VIEW admits.  ON is a declared substrate
   interface here as it is in S4; the pool itself comes through S2's shape test, so no
   problem vocabulary enters.  This is a PRESENCE pool and never an availability set."
  (let ((placement (find 'on (placement-relations (functional-relation-entries)) :key #'first)))
    (remove-if-not (lambda (object)
                     (member (census-layer-class object pairs) (role-view-classes view)
                             :test #'string=))
                   (census-spec-extent (fifth placement)))))


(defun role-removal-reason (object scenario)
  "Why OBJECT is not an available witness in this segment, or NIL.  Every removal prints its
   reason: an unexplained exclusion is a premise smuggled into an allocation."
  (let ((excluded (assoc object (getf scenario :excluded)))
        (committed (assoc object (getf scenario :committed))))
    (cond (excluded (format nil "excluded -- ~A" (second excluded)))
          (committed (format nil "committed -- ~A" (second committed))))))


(defun role-availability-known-p (scenario)
  "A stated availability set is a cons.  :UNKNOWN, and an absent key alike, are unknown: a
   declared type extent states presence, and presence is not availability."
  (consp (getf scenario :available-witnesses)))


(defun role-available-witnesses (scenario pairs)
  "The caller's availability set, intersected with the view's presence pool, minus removals."
  (keeper-sorted-set
    (remove-if (lambda (object) (role-removal-reason object scenario))
               (intersection (getf scenario :available-witnesses)
                             (role-view-pool (getf scenario :view) pairs)))))


(defun role-eligibility (supports witnesses)
  "Per-support eligible witnesses.  This version restricts by availability only; any further
   per-support restriction -- reach, elevation, occupancy history -- is UNRESOLVED and says
   so at the claim site.  The matcher itself takes differing sets, which the acceptance
   cases exercise, so the limitation is in the input and not in the reasoning."
  (mapcar (lambda (support) (cons support (copy-list witnesses))) supports))


(defun report-role-segment (scenario witnesses known)
  (format t "~%  segment: view ~(~S~); cycle ~(~S~); ghosts ~(~S~)~%"
          (getf scenario :view) (getf scenario :cycle) (getf scenario :ghosts))
  (format t "    provenance: ~A~%" (or (getf scenario :provenance) "NONE STATED"))
  (if known
    (format t "    stated available witnesses (~D): ~(~S~)~%" (length witnesses) witnesses)
    (format t "    availability UNKNOWN; obligations report demand only.~%"))
  (dolist (entry (append (getf scenario :excluded) (getf scenario :committed)))
    (format t "    removed ~(~A~): ~A~%"
            (first entry) (role-removal-reason (first entry) scenario))))


(defun report-role-destinations (supports facts)
  (format t "        conditional occupancy destinations (~D):~%" (length supports))
  (dolist (support supports)
    (format t "          ~(~A~) at ~(~A~)~%"
            support (or (keeper-fact-value 'has-position support facts) :unresolved)))
  (format t "          CONDITIONAL ONLY: a destination becomes a transport obligation after its~%")
  (format t "          necessity, initial position, object-presence history and segment are established.~%"))


(defun report-role-matching (supports eligibility)
  "The allocation verdict.  A shortage prints its violator and NO forced membership."
  (if (role-perfect-p supports eligibility)
    (let ((forced (keeper-sorted-set (role-forced-witnesses supports eligibility)))
          (pairings (role-forced-pairings supports eligibility)))
      (format t "        matching: PERFECT; an injective simultaneous assignment exists~%")
      (format t "          forced members (~D): ~(~S~)~%" (length forced) forced)
      (format t "          forced pairings (~D): ~(~S~)~%" (length pairings) pairings))
    (progn
      (format t "        matching: SHORTAGE; no injective assignment exists~%")
      (format t "          Hall violator ~(~S~)~%" (role-hall-violator supports eligibility))
      (format t "          no forced membership inferred from an empty assignment set.~%"))))


(defun report-role-alternative (index supports eligibility known facts scenario)
  (format t "      alternative ~D: supports ~(~S~); minimum simultaneous demand ~D~%"
          index supports (length supports))
  (if known
    (progn
      (dolist (support supports)
        (format t "        eligible for ~(~A~) (~D): ~(~S~)~%"
                support (length (rest (assoc support eligibility)))
                (rest (assoc support eligibility))))
      (format t "        per-support restriction beyond availability UNRESOLVED (reach, elevation, history).~%")
      (report-role-matching supports eligibility)
      (report-role-destinations supports facts)
      (format t "        grade 1; status CONDITIONAL~%"))
    (format t "        eligibility symbolic; status UNRESOLVED; no matching attempted.~%"))
  (format t "        sources: CONTROLS clauses and polarity; ON's functional keying; HAS-POSITION;~%")
  (format t "          stated segment ~A~%" (or (getf scenario :provenance) "NONE STATED"))
  (format t "        unresolved premises: necessity of this segment; ghost absence; agent occupancy;~%")
  (format t "          replacement witnesses; recorder transitions~%"))


(defun report-role-device (fact condition witnesses known facts axioms pressure-plates scenario)
  (let ((clauses (keeper-pressure-clauses fact pressure-plates)))
    (format t "~%    ~(~A~) == ~(~A~)~%"
            (third fact) (control-boolean-form (second fact) (fourth fact)))
    (format t "      requested ~(~S~); provenance ~A~%" (second condition) (third condition))
    (report-role-axioms (third fact) axioms scenario)
    (if (null clauses)
      (format t "      no positive pressure demand: inverted aggregate, or no plate witness in any alternative.~%")
      (loop for clause in clauses
            for index from 1
            do (if clause
                 (report-role-alternative index clause (role-eligibility clause witnesses)
                                          known facts scenario)
                 (format t "      alternative ~D: zero support demand; no witness required.~%" index))))))


(defun report-role-obligations (scenario)
  "RO.  Conditional role-obligation analysis for ONE EXPLICIT segment the caller states.
   It allocates witnesses to the supports a requested NORMAL control aggregate demands, by
   injective matching, and separates forced membership from a forced body-to-plate pairing.
   It emits no transport obligation, no stranding verdict, and no goal it was not given.
   Nothing here infers a segment and no default availability exists: SCENARIO is data, and
   an absent key leaves its obligation unresolved rather than taking a convenient reading.
   Uses substrate interfaces only, never problem object names; does not run a search."
  (let* ((*print-pretty* nil)
         (facts (list-static-db))
         (controls (control-facts))
         (axioms (device-state-axioms controls))
         (pressure-plates (census-type-instances 'pressure-plate))
         (pairs (layer-pairs))
         (known (role-availability-known-p scenario))
         (witnesses (when known (role-available-witnesses scenario pairs)))
         (requested (remove-if-not (lambda (entry) (eq (second entry) :active))
                                   (getf scenario :device-conditions))))
    (format t "~2%RO  ROLE OBLIGATIONS  [conditional allocation for one stated segment]~%")
    (format t "~A~%" (make-string 62 :initial-element #\-))
    (format t "  SCOPE: allocation only.  No action search, no plan witness, no transport claim.~%")
    (format t "  A conditional obligation may hold without its condition being reachable or necessary.~%")
    (format t "  Presence is not availability; sequential crossers are not simultaneous demand.~%")
    (report-role-segment scenario witnesses known)
    (format t "~%  requested active device conditions (~D)~%" (length requested))
    (dolist (condition requested)
      (let ((fact (find (first condition) controls :key #'third)))
        (if fact
          (report-role-device fact condition witnesses known facts axioms pressure-plates scenario)
          (format t "~%    ~(~A~): UNRESOLVED; no control aggregate is declared for it.~%"
                  (first condition)))))
    (format t "~%  no transport obligation and no stranding verdict is emitted by this component.~%")
    (values)))


;;;; BX -- BEAM CROSSINGS AND CUT ORDER (T40) ;;;;
;;;
;;; Specification: doc/constraint-led-solving/Extractor-Specifications.md section 8.10.  The
;;; static rows read the engine's own crossing data: the published pool, each directed
;;; beam's stored crossing order and each gate's split of that order.  They name possible
;;; crossings only.  The supplied-state scenario evaluates one settled state with the
;;; engine's own liveness, reaching and fixed-point queries; it is not in the profile.
;;;
;;; SUBSTRATE VOCABULARY (C3).  The relations CURRENT-BEAM-CROSSINGS, CROSSINGS-ALONG-BEAM>,
;;; BEAM-CROSSINGS-BEFORE-GATE>, LOS-VIA, OPEN and CROSSING-ACTIVE, the coordinate relations
;;; APPARATUS-COORDS> and LOCATION-COORDS>, and the queries named below are beam-crossing's
;;; interface.  No problem object name appears.


(defun crossing-static-facts (relation)
  "Every static fact of RELATION, in a deterministic order."
  (sort (remove-if-not (lambda (fact) (eq (first fact) relation)) (list-static-db))
        #'string< :key #'prin1-to-string))


(defun crossing-pool ()
  "The published crossing pool and whether it is published at all."
  (let ((fact (first (crossing-static-facts 'current-beam-crossings))))
    (values (second fact) (not (null fact)))))


(defun crossing-beam-rows ()
  "Every stored directed beam as (FROM IDS TO), sorted by its endpoints."
  (sort (mapcar #'rest (crossing-static-facts 'crossings-along-beam>))
        #'string< :key (lambda (row) (prin1-to-string (list (first row) (third row))))))


(defun crossing-beam-text (from to rows)
  "A beam's name: A<->B when both directions are stored, else FROM->TO."
  (if (find-if (lambda (row) (and (eq (first row) to) (eq (third row) from))) rows)
    (let ((ends (keeper-sorted-set (list from to))))
      (format nil "~(~A<->~A~)" (first ends) (second ends)))
    (format nil "~(~A->~A~)" from to)))


(defun crossing-other-beam (crossing from to rows)
  "The stored beam through CROSSING other than FROM-TO and its reverse, as (FROM TO)."
  (let ((row (find-if (lambda (row)
                        (and (member crossing (second row))
                             (not (and (eq (first row) from) (eq (third row) to)))
                             (not (and (eq (first row) to) (eq (third row) from)))))
                      rows)))
    (when row
      (list (first row) (third row)))))


(defun crossing-gate-splits (from to)
  "Each gate's BEAM-CROSSINGS-BEFORE-GATE> list for FROM-TO, as (GATE . BEFORE), by gate."
  (sort (loop for fact in (crossing-static-facts 'beam-crossings-before-gate>)
              when (and (eq (second fact) from) (eq (fifth fact) to))
                collect (cons (fourth fact) (third fact)))
        #'string< :key (lambda (split) (symbol-name (car split)))))


(defun crossing-los-gates (from to)
  "The gates LOS-VIA names between FROM and TO, in either stored order."
  (let ((fact (find-if (lambda (fact)
                         (or (and (eq (second fact) from) (eq (fourth fact) to))
                             (and (eq (second fact) to) (eq (fourth fact) from))))
                       (crossing-static-facts 'los-via))))
    (keeper-sorted-set (intersection (third fact) (census-type-instances 'gate)))))


(defun crossing-sequence-text (from ids to rows)
  "FROM->TO, then each crossing with its other beam, with |gate| inserted after the
   crossings its split list holds.  Split lists are prefixes of the stored order (engine
   init check), so a gate's place is the length of its list."
  (let ((splits (crossing-gate-splits from to))
        (parts nil))
    (loop for index from 0 to (length ids)
          do (dolist (split splits)
               (when (= index (length (cdr split)))
                 (push (format nil "|~(~A~)|" (car split)) parts)))
             (when (< index (length ids))
               (let* ((crossing (nth index ids))
                      (other (crossing-other-beam crossing from to rows)))
                 (push (format nil "~(~A~) (x ~A)" crossing
                               (if other
                                 (crossing-beam-text (first other) (second other) rows)
                                 "no other stored beam"))
                       parts))))
    (format nil "~(~A->~A~): ~{~A~^, ~}" from to (reverse parts))))


(defun crossing-unsplit-gates (from to)
  "LOS-VIA gates of FROM-TO without a split fact for this direction."
  (set-difference (crossing-los-gates from to) (mapcar #'car (crossing-gate-splits from to))))


(defun crossing-points (pool)
  "Alist CROSSING -> (X Y) from the engine's own records, or NIL without coordinates."
  (when (or (crossing-static-facts 'apparatus-coords>) (crossing-static-facts 'location-coords>))
    (let* ((state *start-state*)
           (positions (funcall (symbol-function 'beam-coordinates-endpoint-positions) state))
           (beams (funcall (symbol-function 'beam-coordinates-potential-beams) state)))
      (loop for (crossing beam parameter) in (beam-coordinates-crossing-records beams positions pool)
            collect (let ((start (beam-coordinates-position (first beam) positions))
                          (end (beam-coordinates-position (second beam) positions)))
                      (list crossing
                            (+ (first start) (* parameter (- (first end) (first start))))
                            (+ (second start) (* parameter (- (second end) (second start))))))))))


(defun crossing-canonical-rows (rows)
  "ROWS with one direction per location-to-location beam: the one stored FROM-first by
   name, as the engine's own canonical beam list keeps it."
  (remove-if (lambda (row)
               (and (find-if (lambda (other) (and (eq (first other) (third row))
                                                  (eq (third other) (first row))))
                             rows)
                    (string< (symbol-name (third row)) (symbol-name (first row)))))
             rows))


(defun crossing-index-rows (pool rows)
  "Per crossing: the canonical stored beams through it, each with (POSITION LENGTH)."
  (loop for crossing in pool
        collect (list crossing
                      (loop for (from ids to) in (crossing-canonical-rows rows)
                            when (member crossing ids)
                              collect (list from to (1+ (position crossing ids)) (length ids))))))


(defun report-beam-crossing-index (pool rows)
  "The crossing index, with engine-derived points when coordinates exist."
  (let ((points (crossing-points pool)))
    (format t "    crossing index (~D)~:[; no coordinates, no points~;~]:~%" (length pool) points)
    (dolist (entry (crossing-index-rows pool rows))
      (let ((point (rest (assoc (first entry) points))))
        (format t "      ~(~A~)~@[ at ~A~]: ~{~A~^ x ~}~%" (first entry)
                (when point
                  (format nil "(~,2F,~,2F)" (float (first point)) (float (second point))))
                (loop for (from to at of) in (second entry)
                      collect (format nil "~A ~D/~D" (crossing-beam-text from to rows) at of)))))))


(defun report-beam-crossing-instances ()
  "MC instance rows for beam-crossing: pool, directed orders with gate splits, index."
  (multiple-value-bind (pool published) (crossing-pool)
    (let ((rows (crossing-beam-rows)))
      (cond ((not published)
             (format t "    No CURRENT-BEAM-CROSSINGS pool is published~:[~;; ~D stored beam lists are therefore inert (every crossing loop is empty)~].~%"
                     rows (length rows)))
            (t
             (format t "    pool: ~D crossings; ~D stored directed beams with crossings~%"
                     (length pool) (length rows))
             (format t "    directed beams, crossings in order from the source, |gate| at its split (a<->b: both directions stored, positions counted from a):~%")
             (dolist (row rows)
               (destructuring-bind (from ids to) row
                 (format t "      ~A~%" (crossing-sequence-text from ids to rows))
                 (let ((unsplit (crossing-unsplit-gates from to)))
                   (when unsplit
                     (format t "        unsplit LOS gates ~(~S~): no split fact, so gate state never stops this beam short of a crossing~%"
                             unsplit)))))
             (when (member "-beam-crossing-coordinates" *spliced-tech-names* :test #'string=)
               (format t "    potential beams with no crossing: ~D~%"
                       (- (length (funcall (symbol-function 'beam-coordinates-potential-beams) *start-state*))
                          (length (crossing-canonical-rows rows)))))
             (report-beam-crossing-index pool rows)))
      (format t "    These are possible crossings: none is claimed active, no row claims its two beams can be live together, and nothing is claimed reachable. Evaluated cuts need a settled state (REPORT-BEAM-CROSSING-SCENARIO).~%"))))


(defun crossing-state-facts (state)
  "STATE's dynamic facts, decoded."
  (list-database (problem-state.idb state)))


(defun crossing-settled-p (state)
  "Whether a private copy of STATE is unchanged by the engine's own propagation."
  (let ((copy (%copy-problem-state state t))
        (before (crossing-state-facts state)))
    (let ((*applying-init-action* nil))
      (relay-view-call 'propagate-changes! copy))
    (let ((after (crossing-state-facts copy)))
      (and (not (state-is-inconsistent copy))
           (null (set-difference before after :test #'equal))
           (null (set-difference after before :test #'equal))))))


(defun crossing-scenario-reason (scenario)
  "Why SCENARIO cannot be evaluated, or NIL.  Missing data is never a crossing verdict."
  (let ((state (getf scenario :state)))
    (cond ((not (member "beam-crossing" *spliced-tech-names* :test #'string=))
           "beam-crossing technology not spliced")
          ((not (nth-value 1 (crossing-pool))) "no crossing pool published")
          ((not (typep state 'problem-state)) "missing problem-state")
          ((not (and (stringp (getf scenario :provenance))
                     (plusp (length (getf scenario :provenance)))))
           "state provenance missing")
          ((state-is-inconsistent state) "state marked inconsistent")
          ((not (member 'propagate-changes! *update-names*)) "propagation unavailable")
          ((not (crossing-settled-p state))
           "state is not a propagation fixed point; supply a replayed state or settle it first (section 6.2)"))))


(defun crossing-live-direction (state from to lighting)
  "The live orientation of the beam FROM-TO as (SOURCE DESTINATION), in the engine's order
   of trial, or NIL."
  (cond ((relay-view-call 'beam-live-for-cutting state from to lighting) (list from to))
        ((relay-view-call 'beam-live-for-cutting state to from lighting) (list to from))))


(defun crossing-unreached-reasons (crossing ids splits active open-gates)
  "The source rules that stop a beam short of CROSSING: an earlier active crossing on IDS,
   and each closed gate whose split list omits CROSSING."
  (let ((earlier (find-if (lambda (other) (member other active))
                          (subseq ids 0 (or (position crossing ids) 0)))))
    (append (when earlier (list (format nil "BEYOND CUT at ~(~A~)" earlier)))
            (loop for (gate . before) in splits
                  when (and (not (member gate open-gates)) (not (member crossing before)))
                    collect (format nil "BEYOND CLOSED ~(~A~)" gate)))))


(defun crossing-other-status (state crossing from to active lighting rows open-gates)
  "Why the other beam through CROSSING does not make it active: NOT LIVE, or NOT REACHING
   with its reasons."
  (let ((other (crossing-other-beam crossing from to rows)))
    (if (null other)
      "UNEXPLAINED (no other stored beam)"
      (let ((direction (crossing-live-direction state (first other) (second other) lighting))
            (text (crossing-beam-text (first other) (second other) rows)))
        (cond ((null direction) (format nil "other beam ~A NOT LIVE" text))
              ((relay-view-call 'beam-reaches-crossing state (first other) (second other)
                                crossing active lighting)
               (format nil "UNEXPLAINED (other beam ~A also reaches)" text))
              (t (let ((row (find-if (lambda (row) (and (eq (first row) (first direction))
                                                        (eq (third row) (second direction))))
                                     rows)))
                   (format nil "other beam ~A NOT REACHING (~{~A~^; ~})" text
                           (or (and row (crossing-unreached-reasons
                                          crossing (second row)
                                          (crossing-gate-splits (first direction) (second direction))
                                          active open-gates))
                               (list "UNEXPLAINED"))))))))))


(defun crossing-label (state crossing from ids to active lighting rows open-gates)
  "One crossing's label on the live beam FROM-TO, decided by the engine's reaching query."
  (cond ((not (relay-view-call 'beam-reaches-crossing state from to crossing active lighting))
         (format nil "~{~A~^; ~}"
                 (or (crossing-unreached-reasons crossing ids (crossing-gate-splits from to)
                                                 active open-gates)
                     (list "NOT REACHED: UNEXPLAINED"))))
        ((member crossing active) "ACTIVE")
        (t (format nil "REACHED, INACTIVE: ~A"
                   (crossing-other-status state crossing from to active lighting rows open-gates)))))


(defun crossing-live-beam-rows (state active lighting rows open-gates)
  "Each beam live for cutting, once, in the orientation the engine evaluates: its first
   live trial from the canonical endpoints, so a location-to-location beam live both ways
   is read from its name-first endpoint.  Crossings are labelled in that source order."
  (loop for (from nil to) in (crossing-canonical-rows rows)
        for direction = (crossing-live-direction state from to lighting)
        for row = (when direction
                    (find-if (lambda (row) (and (eq (first row) (first direction))
                                                (eq (third row) (second direction))))
                             rows))
        when row
          collect (destructuring-bind (source ids destination) row
                    (list :from source :to destination
                          :cut (relay-view-call 'beam-cut state source destination)
                          :crossings (loop for crossing in ids
                                           collect (list crossing
                                                         (crossing-label state crossing source ids destination
                                                                         active lighting rows open-gates)))))))


(defun crossing-active-rows (state active rows)
  "Each active crossing with its two beams, by the engine's endpoint query."
  (loop for crossing in (keeper-sorted-set active)
        collect (multiple-value-bind (from1 to1 from2 to2)
                    (relay-view-call 'beam-crossing-endpoints state crossing)
                  (list crossing (crossing-beam-text from1 to1 rows) (crossing-beam-text from2 to2 rows)))))


(defun beam-crossing-scenario-result (scenario)
  "BX: one settled state's crossings, by the engine's own queries.  Returns a plist; the
   caller's state is never changed."
  (let ((reason (crossing-scenario-reason scenario)))
    (if reason
      (list :status :unresolved :reason reason)
      (let* ((state (getf scenario :state))
             (facts (crossing-state-facts state))
             (gates (census-type-instances 'gate))
             (open-gates (remove-if-not (lambda (gate) (member (list 'open gate) facts :test #'equal))
                                        gates))
             (active (relay-view-call 'current-crossing-set state))
             (lighting (relay-view-call 'compute-relay-lighting state active))
             (rows (crossing-beam-rows)))
        (list :status :evaluated :provenance (getf scenario :provenance)
              :open-gates (keeper-sorted-set open-gates)
              :closed-gates (keeper-sorted-set (set-difference gates open-gates))
              :active (keeper-sorted-set active)
              :fixed-point (not (null (relay-view-call 'same-crossing-set state
                                                          (relay-view-call 'compute-active-beam-crossings state active)
                                                          active)))
              :active-rows (crossing-active-rows state active rows)
              :beams (crossing-live-beam-rows state active lighting rows open-gates))))))


(defun report-beam-crossing-scenario (scenario)
  "BX report: one settled state's gate states, active crossings and live-beam cut order.
   Not a reachability, stability or composition result."
  (let ((result (beam-crossing-scenario-result scenario))
        (*print-pretty* nil))
    (format t "~%BX  BEAM CROSSINGS IN ONE SUPPLIED STATE  [supplied state]~%")
    (format t "~A~%" (make-string 62 :initial-element #\-))
    (if (eq (getf result :status) :unresolved)
      (format t "  UNRESOLVED: ~A~%" (getf result :reason))
      (progn
        (format t "  provenance: ~A~%" (getf result :provenance))
        (format t "  gates read from the state (not premises): open ~(~S~); closed ~(~S~)~%"
                (getf result :open-gates) (getf result :closed-gates))
        (format t "  active crossings ~(~S~); engine fixed point ~:[FAILS~;holds~]~%"
                (getf result :active) (getf result :fixed-point))
        (dolist (row (getf result :active-rows))
          (format t "    ~(~A~): ~A x ~A~%" (first row) (second row) (third row)))
        (format t "  beams live for cutting (~D), crossings in order from the source:~%"
                (length (getf result :beams)))
        (dolist (beam (getf result :beams))
          (format t "    ~(~A->~A~)~:[~; CUT~]~%" (getf beam :from) (getf beam :to) (getf beam :cut))
          (dolist (entry (getf beam :crossings))
            (format t "      ~(~A~): ~A~%" (first entry) (second entry))))
        (format t "  For this state only: not reachability, not stability beyond the fixed-point check, and no claim that separately evaluated beams compose.~%")))
    (values)))


;;;; RL -- COMPETING COLORS AND CONNECTOR LINKS (T41) ;;;;
;;;
;;; Specification: doc/constraint-led-solving/Extractor-Specifications.md section 8.11.  The
;;; static rows state beam-relay's pools, start links and the hues each RC station can
;;; see directly.  The supplied-state scenario replays one settled state's lighting layer
;;; by layer through the engine's own link query and checks the replay against the
;;; engine's lighting, colors and receiver status; it is not in the profile.
;;;
;;; SUBSTRATE VOCABULARY (C3).  The types RELAY, CONNECTOR, TRANSMITTER, RECEIVER and
;;; REPEATER, the relations PAIRED, COUPLED, HAS-CHROMA, COLOR, ACTIVE, RECORDING-ACTIVE and
;;; RECORDING-IN-PROGRESS, the setting *MAX-CONNECTOR-PAIRINGS*, and the queries named
;;; below are beam-relay's and the recorder's interface.  No problem object name appears.


(defun relay-light-pairings (facts)
  "Every stored PAIRED fact in FACTS as (OWNER TERMINUS), in a deterministic order."
  (sort (loop for fact in facts
              when (eq (first fact) 'paired)
                collect (list (second fact) (third fact)))
        #'string< :key #'prin1-to-string))


(defun relay-light-couplings ()
  "Every authored COUPLED fact as (SOURCE SINK), in a deterministic order."
  (sort (mapcar #'rest (crossing-static-facts 'coupled)) #'string< :key #'prin1-to-string))


(defun relay-light-hue-groups (objects)
  "OBJECTS grouped by authored hue, as ((HUE OBJECT ...) ...), hues by name."
  (let ((groups nil))
    (dolist (object objects)
      (let* ((hue (relay-chain-chroma object))
             (group (assoc hue groups)))
        (if group
          (push object (rest group))
          (push (list hue object) groups))))
    (sort (mapcar (lambda (group) (cons (first group) (keeper-sorted-set (rest group)))) groups)
          #'string< :key (lambda (group) (prin1-to-string (first group))))))


(defun report-relay-light-pools ()
  "Capacity, and the transmitter, receiver, connector and repeater pools."
  (format t "    capacity: ~D outgoing pairings per connector (*max-connector-pairings*); incoming links unlimited~%"
          *max-connector-pairings*)
  (format t "    transmitters by hue: ~:[none~;~:*~{~A~^; ~}~]~%"
          (mapcar (lambda (group) (format nil "~(~A~) ~(~{~A~^, ~}~)" (first group) (rest group)))
                  (relay-light-hue-groups (census-type-instances 'transmitter))))
  (format t "    receivers by hue: ~:[none~;~:*~{~A~^; ~}~]~%"
          (mapcar (lambda (group) (format nil "~(~A~) ~(~{~A~^, ~}~)" (first group) (rest group)))
                  (relay-light-hue-groups (census-type-instances 'receiver))))
  (format t "    connectors (~D): ~(~{~A~^, ~}~)~%"
          (length (census-type-instances 'connector)) (census-type-instances 'connector))
  (let ((repeaters (relay-chain-repeaters))
        (couplings (relay-light-couplings)))
    (format t "    repeaters (~D)~:[~;:~]~%" (length repeaters) repeaters)
    (dolist (repeater repeaters)
      (format t "      ~(~A~): coupled from ~:[none~;~:*~(~{~A~^, ~}~)~], coupled to ~:[none~;~:*~(~{~A~^, ~}~)~]~%" repeater
              (mapcar #'first (remove-if-not (lambda (pair) (eq (second pair) repeater)) couplings))
              (mapcar #'second (remove-if-not (lambda (pair) (eq (first pair) repeater)) couplings))))))


(defun report-relay-light-start-links ()
  "Each connector's outgoing pairings against capacity and its incoming links, at the start."
  (let ((pairings (relay-light-pairings (crossing-state-facts *start-state*))))
    (format t "    start-state links (~D stored pairings)~:[: none~;~]~%" (length pairings) pairings)
    (when pairings
      (dolist (connector (census-type-instances 'connector))
        (let ((outgoing (mapcar #'second (remove-if-not (lambda (pair) (eq (first pair) connector)) pairings)))
              (incoming (mapcar #'first (remove-if-not (lambda (pair) (eq (second pair) connector)) pairings))))
          (when (or outgoing incoming)
            (format t "      ~(~A~): outgoing ~D/~D ~(~S~); incoming ~(~S~)~%" connector
                    (length outgoing) *max-connector-pairings* outgoing incoming)))))))


(defun relay-light-station-feeds (station endpoint-links)
  "The transmitters visible from STATION, as (HUE (TRANSMITTER GATES) ...) by hue."
  (let ((groups nil))
    (dolist (link endpoint-links)
      (when (and (equal (first link) station)
                 (member (second link) (census-type-instances 'transmitter)))
        (let* ((hue (relay-chain-chroma (second link)))
               (group (assoc hue groups)))
          (if group
            (setf (rest group) (append (rest group) (list (list (second link) (third link)))))
            (setf groups (append groups (list (list hue (list (second link) (third link))))))))))
    (sort groups #'string< :key (lambda (group) (prin1-to-string (first group))))))


(defun report-relay-light-competition ()
  "Per RC station, the transmitters visible from it grouped by hue; two or more hues is
   COMPETING HUES.  Geometry only: RC's start-state copies with gates forced, no crossings."
  (let* ((stations (relay-chain-stations *start-state*))
         (gates (census-type-instances 'gate))
         (rows nil))
    (multiple-value-bind (open-state closed-states) (relay-chain-gate-states gates)
      (let ((endpoint-links (relay-chain-endpoint-links stations open-state closed-states)))
        (dolist (station stations)
          (let ((groups (relay-light-station-feeds station endpoint-links)))
            (when groups
              (push (list station groups) rows))))))
    (setf rows (nreverse rows))
    (format t "    direct feeds by RC station (~D with a visible transmitter; ~D COMPETING HUES), each transmitter with the gates it requires open:~%"
            (length rows) (count-if (lambda (row) (rest (second row))) rows))
    (dolist (row rows)
      (format t "      ~A: ~{~A~^; ~}~:[~;  COMPETING HUES~]~%"
              (relay-chain-node-text (first row))
              (mapcar (lambda (group)
                        (format nil "~(~A~) ~{~A~^, ~}" (first group)
                                (mapcar (lambda (feed)
                                          (format nil "~(~A~)~:[ ALWAYS~; requires open ~(~{~A~^, ~}~)~]"
                                                  (first feed) (second feed) (second feed)))
                                        (rest group))))
                      (second row))
              (rest (second row))))
    (format t "    Pairing one connector to two hues settles it in layer 1 with a CONFLICT (dark, feeds nothing). A direct transmitter link reaches a connector in layer 1, ahead of any relayed hue, so a relayed hue lights it only while that direct link is unpaired, blocked or cut. Separately possible per-hue routes are not claimed to compose; a joint arrangement needs REPORT-RELAY-LIGHTING-SCENARIO.~%")))


(defun report-beam-relay-instances ()
  "MC instance rows for beam-relay: pools, start-state links, and direct-feed competition."
  (report-relay-light-pools)
  (report-relay-light-start-links)
  (report-relay-light-competition))


(defun relay-light-view-selector (state view)
  "The engine's view object: NIL for the physical view, the recording view object else."
  (when (eq view :recording)
    (relay-view-call 'recording-shadow-view-object state)))


(defun relay-light-scenario-reason (scenario)
  "Why SCENARIO cannot be evaluated, or NIL.  Missing data is never a lighting verdict."
  (let ((state (getf scenario :state))
        (view (getf scenario :view :physical)))
    (cond ((not (member "beam-relay" *spliced-tech-names* :test #'string=))
           "beam-relay technology not spliced")
          ((not (typep state 'problem-state)) "missing problem-state")
          ((not (and (stringp (getf scenario :provenance))
                     (plusp (length (getf scenario :provenance)))))
           "state provenance missing")
          ((not (member view '(:physical :recording))) "view must be :physical or :recording")
          ((and (eq view :recording)
                (not (member "recorder" *spliced-tech-names* :test #'string=)))
           "recording view unavailable: recorder technology not spliced")
          ((and (eq view :recording)
                (not (member '(recording-in-progress) (crossing-state-facts state) :test #'equal)))
           "recording view unavailable: no open recording cycle in the state")
          ((state-is-inconsistent state) "state marked inconsistent")
          ((not (member 'propagate-changes! *update-names*)) "propagation unavailable")
          ((not (crossing-settled-p state))
           "state is not a propagation fixed point; supply a replayed state or settle it first (section 6.2)"))))


(defun relay-light-anchor (state selector relay)
  "RELAY's beam anchor in the view, or NIL when it has no location or is absent there."
  (when (or (null selector) (relay-view-call 'recording-shadow-object-present state relay))
    (relay-view-call 'relay-anchor state relay)))


(defun relay-light-initial-frontier ()
  "Layer 0: every transmitter with an authored hue, as (SOURCE ANCHOR HUE LAYER)."
  (loop for transmitter in (census-type-instances 'transmitter)
        for hue = (relay-chain-chroma transmitter)
        when hue
          collect (list transmitter transmitter hue 0)))


(defun relay-light-arrivals (state selector target anchor frontier active)
  "The (SOURCE HUE) pairs of FRONTIER whose link to TARGET the engine finds clear."
  (loop for (source source-anchor hue) in frontier
        when (relay-view-call 'relay-link-clear-for-object state selector
                              source source-anchor target anchor active)
          collect (list source hue)))


(defun relay-light-settle (state selector target anchor frontier active lit-locations layer)
  "TARGET's record for this layer, or NIL when nothing reaches it: (RELAY LAYER ARRIVALS
   VERDICT HUE).  The engine's order of tests: two hues, then an already lit location."
  (let ((arrivals (relay-light-arrivals state selector target anchor frontier active)))
    (when arrivals
      (let ((hues (remove-duplicates (mapcar #'second arrivals))))
        (cond ((rest hues) (list target layer arrivals :conflict nil))
              ((and (member target (census-type-instances 'connector))
                    (member anchor lit-locations))
               (list target layer arrivals :location-lit nil))
              (t (list target layer arrivals :lit (first hues))))))))


(defun relay-light-layers (state selector active)
  "The engine's breadth-first lighting, replayed layer by layer through its link query:
   one record per relay, (RELAY LAYER ARRIVALS VERDICT HUE), unreached and absent relays
   with layer NIL.  Relays are tried in the engine's type order, so a same-location tie
   within a layer resolves as the engine resolves it."
  (let ((frontier (relay-light-initial-frontier))
        (records nil)
        (lit-locations nil))
    (loop for layer from 1 to 99
          while frontier
          do (let ((next nil))
               (dolist (target (census-type-instances 'relay))
                 (unless (assoc target records)
                   (let ((anchor (relay-light-anchor state selector target)))
                     (when anchor
                       (let ((record (relay-light-settle state selector target anchor frontier
                                                         active lit-locations layer)))
                         (when record
                           (push record records)
                           (when (eq (fourth record) :lit)
                             (when (member target (census-type-instances 'connector))
                               (push anchor lit-locations))
                             (push (list target anchor (fifth record) layer) next))))))))
               (setf frontier next)))
    (dolist (relay (census-type-instances 'relay))
      (unless (assoc relay records)
        (push (list relay nil nil
                    (if (relay-light-anchor state selector relay) :unreached :absent) nil)
              records)))
    (sort records #'string< :key (lambda (record) (symbol-name (first record))))))


(defun relay-light-link-facts (state)
  "Every stored link that can carry a beam into a relay: PAIRED facts of STATE and authored
   COUPLED facts ending at a repeater, as (KIND OWNER OTHER)."
  (append (mapcar (lambda (pair) (cons :paired pair)) (relay-light-pairings (crossing-state-facts state)))
          (loop for pair in (relay-light-couplings)
                when (member (second pair) (relay-chain-repeaters))
                  collect (cons :coupled pair))))


(defun relay-light-source-hue (source records)
  "SOURCE's hue and layer as a lit source: a transmitter's authored hue in layer 0, else a
   lit relay's record."
  (if (member source (census-type-instances 'transmitter))
    (values (relay-chain-chroma source) 0)
    (let ((record (assoc source records)))
      (when (eq (fourth record) :lit)
        (values (fifth record) (second record))))))


(defun relay-light-direction (state selector kind source target records active)
  "One beam direction SOURCE -> TARGET over a stored link, TARGET a relay: sightline, cut
   and outcome, with the engine's own clear-link reading."
  (let ((source-anchor (if (member source (census-type-instances 'transmitter))
                         source
                         (relay-light-anchor state selector source)))
        (target-anchor (relay-light-anchor state selector target)))
    (if (not (and source-anchor target-anchor))
      (list :from source :to target :outcome "ENDPOINT ABSENT")
      (let* ((sight (not (null (if (eq kind :coupled)
                                 (relay-view-call 'fixed-beam-corridor-clear-for-object
                                                  state selector source target)
                                 (relay-view-call 'paired-relay-visible-for-object state selector
                                                  source source-anchor target target-anchor)))))
             (cut (not (null (relay-view-call 'beam-cut-in state source-anchor target-anchor active))))
             (clear (not (null (relay-view-call 'relay-link-clear-for-object state selector source
                                                source-anchor target target-anchor active))))
             (record (assoc target records)))
        (multiple-value-bind (hue layer) (relay-light-source-hue source records)
          (list :from source :to target :sight sight :cut cut :engine-clear clear
                :outcome (cond ((not clear) "NOT CLEAR")
                               ((null hue) "SOURCE DARK")
                               ((and (eql (second record) (1+ layer))
                                     (member (list source hue) (third record) :test #'equal))
                                (format nil "DELIVERED ~(~A~)" hue))
                               ((and (second record) (<= (second record) layer))
                                (format nil "IGNORED ~(~A~) (target settled in layer ~D)"
                                        hue (second record)))
                               (t "UNEXPLAINED"))))))))


(defun relay-light-source-p (object)
  "Whether OBJECT can send a beam: a transmitter or a relay."
  (or (member object (census-type-instances 'transmitter))
      (member object (census-type-instances 'relay))))


(defun relay-light-links (state selector records active)
  "Every stored link into a relay, with each beam direction it can carry: a pairing either
   way, a coupling only from its source."
  (loop for (kind owner other) in (relay-light-link-facts state)
        for directions = (append (when (and (member other (census-type-instances 'relay))
                                            (relay-light-source-p owner))
                                   (list (relay-light-direction state selector kind owner other
                                                                records active)))
                                 (when (and (eq kind :paired)
                                            (member owner (census-type-instances 'relay))
                                            (relay-light-source-p other))
                                   (list (relay-light-direction state selector kind other owner
                                                                records active))))
        when directions
          collect (list :kind kind :owner owner :other other :directions directions)))


(defun relay-light-capacity (state)
  "Per connector: (CONNECTOR OUTGOING INCOMING OVER-CAPACITY-P), counts of stored links."
  (let ((pairings (relay-light-pairings (crossing-state-facts state))))
    (loop for connector in (census-type-instances 'connector)
          for outgoing = (count connector pairings :key #'first)
          collect (list connector outgoing (count connector pairings :key #'second)
                        (> outgoing *max-connector-pairings*)))))


(defun relay-light-feeder (state selector receiver relay records)
  "One relay whose own link names RECEIVER: its hue, sightline, cut and verdict."
  (let* ((record (assoc relay records))
         (hue (when (eq (fourth record) :lit) (fifth record)))
         (anchor (relay-light-anchor state selector relay))
         (connector (member relay (census-type-instances 'connector)))
         (sight (and anchor
                     (not (null (if connector
                                  (relay-view-call 'beam-visible-for-object state selector anchor
                                                   (relay-view-call 'top state relay) receiver
                                                   (relay-view-call 'top state receiver))
                                  (relay-view-call 'fixed-beam-corridor-clear-for-object
                                                   state selector relay receiver))))))
         (cut (and anchor (null selector)
                   (not (null (relay-view-call 'beam-cut state anchor receiver))))))
    (list :relay relay :hue hue :sight sight :cut cut
          :verdict (cond ((null hue) "DARK")
                         ((not (eql hue (relay-chain-chroma receiver))) "WRONG HUE")
                         ((not sight) "BLOCKED")
                         (cut "CUT")
                         (t "DELIVERS")))))


(defun relay-light-receivers (state selector records lighting)
  "Per receiver: required hue, stored status, feeding relays, and the engine's readings."
  (let ((facts (crossing-state-facts state))
        (status (if selector 'recording-active 'active)))
    (loop for receiver in (census-type-instances 'receiver)
          for feeders = (loop for relay in (census-type-instances 'relay)
                              when (if (member relay (census-type-instances 'connector))
                                     (member (list 'paired relay receiver) facts :test #'equal)
                                     (member (list relay receiver) (relay-light-couplings) :test #'equal))
                                collect (relay-light-feeder state selector receiver relay records))
          collect (list :receiver receiver :hue (relay-chain-chroma receiver)
                        :stored (not (null (member (list status receiver) facts :test #'equal)))
                        :feeders feeders
                        :direct (not (null (if selector
                                             (relay-view-call 'recording-shadow-direct-beam-reaches-receiver
                                                              state selector receiver)
                                             (relay-view-call 'direct-beam-reaches-receiver state receiver))))
                        :engine-relay (not (null (if selector
                                                   (relay-view-call 'recording-shadow-relay-beam-reaches-receiver
                                                                    state selector lighting receiver)
                                                   (relay-view-call 'relay-beam-reaches-receiver state receiver))))))))


(defun relay-light-lit-set (records)
  "The replayed lit records as (RELAY HUE LAYER), for comparison with the engine."
  (loop for record in records
        when (eq (fourth record) :lit)
          collect (list (first record) (fifth record) (second record))))


(defun relay-light-same-set-p (list1 list2)
  "Whether LIST1 and LIST2 hold the same elements under EQUAL."
  (and (null (set-difference list1 list2 :test #'equal))
       (null (set-difference list2 list1 :test #'equal))))


(defun relay-light-chain-results (scenario)
  "T33's view results for the supplied chains, in the same state and the stated phase."
  (when (getf scenario :chains)
    (relay-view-results (list :state (getf scenario :state) :complete-state t
                              :phase (getf scenario :phase) :provenance (getf scenario :provenance)
                              :hops nil :chains (getf scenario :chains) :gate-premises nil))))


(defun relay-lighting-scenario-result (scenario)
  "RL: one settled state's relay lighting in one view, replayed layer by layer with the
   engine's own queries and checked against the engine.  Returns a plist; the caller's
   state is never changed."
  (let ((reason (relay-light-scenario-reason scenario)))
    (if reason
      (list :status :unresolved :reason reason)
      (let* ((state (getf scenario :state))
             (view (getf scenario :view :physical))
             (selector (relay-light-view-selector state view))
             (active (unless selector (relay-view-call 'current-crossing-set state)))
             (lighting (relay-view-call 'compute-relay-lighting-for-object state selector active))
             (records (relay-light-layers state selector active))
             (receivers (relay-light-receivers state selector records lighting))
             (colors (loop for fact in (crossing-state-facts state)
                           when (eq (first fact) 'color) collect (rest fact))))
        (list :status :evaluated :view view :provenance (getf scenario :provenance)
              :active (keeper-sorted-set active)
              :relays records
              :links (relay-light-links state selector records active)
              :capacity (relay-light-capacity state)
              :receivers receivers
              :lighting-agreement (relay-light-same-set-p (relay-light-lit-set records) lighting)
              :color-agreement (or selector
                                   (relay-light-same-set-p
                                    colors (mapcar (lambda (lit) (list (first lit) (second lit)))
                                                   (relay-light-lit-set records))))
              :receiver-agreement
              (every (lambda (entry)
                       (eq (getf entry :stored)
                           (or (getf entry :direct) (getf entry :engine-relay))))
                     receivers)
              :chains (relay-light-chain-results scenario))))))


(defun relay-light-verdict-text (record)
  "One relay's verdict as printed."
  (ecase (fourth record)
    (:lit (format nil "LIT ~(~A~)" (fifth record)))
    (:conflict (format nil "CONFLICT ~(~S~), dark"
                       (remove-duplicates (mapcar #'second (third record)))))
    (:location-lit "LOCATION ALREADY LIT, dark")
    (:unreached "UNREACHED")
    (:absent "ABSENT (no location in this view)")))


(defun report-relay-light-relays (result)
  "Each relay with its layer, arrivals, verdict, and every stored link touching it."
  (format t "  relays by propagation layer (layers are propagation order, not time):~%")
  (dolist (record (sort (copy-list (getf result :relays)) #'<
                        :key (lambda (record) (or (second record) 100))))
    (format t "    ~(~A~)~@[ layer ~D~]: ~A~@[; arrivals ~A~]~%" (first record) (second record)
            (relay-light-verdict-text record)
            (when (third record)
              (format nil "~(~{~A~^, ~}~)"
                      (mapcar (lambda (arrival) (format nil "~A ~A" (first arrival) (second arrival)))
                              (third record)))))
    (dolist (link (getf result :links))
      (when (or (eq (getf link :owner) (first record)) (eq (getf link :other) (first record)))
        (format t "      ~A ~(~A~) ~(~A~) -> ~(~A~):~{ ~A~^;~}~%"
                (if (eq (getf link :kind) :coupled) "coupled" "paired")
                (if (eq (getf link :owner) (first record)) "owns" "incoming from")
                (getf link :owner) (getf link :other)
                (mapcar (lambda (direction)
                          (if (eq (getf direction :sight :absent) :absent)
                            (format nil "~(~A->~A~) ~A" (getf direction :from) (getf direction :to)
                                    (getf direction :outcome))
                            (format nil "~(~A->~A~) sight ~:[BLOCKED~;CLEAR~]~:[~; CUT~] ~A"
                                    (getf direction :from) (getf direction :to)
                                    (getf direction :sight) (getf direction :cut)
                                    (getf direction :outcome))))
                        (getf link :directions)))))))


(defun report-relay-light-receivers (result)
  "Each receiver: its hue, stored status, feeders and the engine's readings."
  (format t "  receivers (~(~A~) view status fact):~%" (getf result :view))
  (dolist (entry (getf result :receivers))
    (format t "    ~(~A~) needs ~(~A~): ~:[inactive~;ACTIVE~]; engine relay ~:[no~;yes~], direct ~:[no~;yes~]~%"
            (getf entry :receiver) (getf entry :hue) (getf entry :stored)
            (getf entry :engine-relay) (getf entry :direct))
    (if (getf entry :feeders)
      (dolist (feeder (getf entry :feeders))
        (format t "      from ~(~A~)~@[ ~(~A~)~]: sight ~:[BLOCKED~;CLEAR~]~:[~; CUT~] ~A~%"
                (getf feeder :relay) (getf feeder :hue) (getf feeder :sight) (getf feeder :cut)
                (getf feeder :verdict)))
      (format t "      no relay link names it~%"))))


(defun report-relay-light-chains (result)
  "T33's chain verdicts for the supplied chains, both views."
  (dolist (view-result (getf result :chains))
    (format t "  T33 chains, ~(~A~) view: ~A -- ~A~%" (getf view-result :view)
            (getf view-result :status) (getf view-result :reason))
    (dolist (row (getf view-result :chains))
      (format t "    ~(~S~) ~A: ~A~%" (getf row :input) (getf row :status) (getf row :reason)))))


(defun report-relay-lighting-scenario (scenario)
  "RL report: one settled state's relay layers, links, capacity and receivers in one view.
   Not a reachability, stability or composition result."
  (let ((result (relay-lighting-scenario-result scenario))
        (*print-pretty* nil))
    (format t "~%RL  RELAY LIGHTING IN ONE SUPPLIED STATE  [supplied state]~%")
    (format t "~A~%" (make-string 62 :initial-element #\-))
    (if (eq (getf result :status) :unresolved)
      (format t "  UNRESOLVED: ~A~%" (getf result :reason))
      (progn
        (format t "  provenance: ~A~%" (getf result :provenance))
        (format t "  view ~(~A~); active crossings ~(~S~)~%" (getf result :view) (getf result :active))
        (format t "  engine agreement: lighting ~:[FAILS~;holds~], colors ~:[FAILS~;holds~], receivers ~:[FAILS~;hold~]~%"
                (getf result :lighting-agreement) (getf result :color-agreement)
                (getf result :receiver-agreement))
        (report-relay-light-relays result)
        (format t "  connector capacity (outgoing/capacity, incoming):~%")
        (dolist (entry (getf result :capacity))
          (format t "    ~(~A~): ~D/~D, incoming ~D~:[~;  OVER CAPACITY~]~%" (first entry) (second entry)
                  *max-connector-pairings* (third entry) (fourth entry)))
        (report-relay-light-receivers result)
        (report-relay-light-chains result)
        (format t "  For this state only: not reachability, not stability beyond the fixed-point check, and no claim that separately evaluated colors compose.~%")))
    (values)))


;;;; MC -- MECHANIC COVERAGE (T19, I5) ;;;;
;;;
;;; Specification: doc/constraint-led-solving/Extractor-Specifications.md section 8.  The Phase 0
;;; coverage gate: which public technologies the staged problem splices, and which of them
;;; carry a declared static contract.  The whole-profile reporter prints it first, but it
;;; sits here, after every block it reads, because this file is ordered callees-first.
;;;
;;; SUBSTRATE VOCABULARY (C3).  Technology names, the types FLOOR-BLOWER, LADDER, RECORDER,
;;; WALL and VAULTABLE-OBJECT, the traversal kinds JUMP and STAIRS, the relations HAS-POSITION, AIMED-AT
;;; and CONTROLS, and the settings *VERTICAL-REACH-LIMIT* and *MAX-RECORDER-CYCLES* are named:
;;; each is a tech/ interface, and a contract is by definition a statement about one
;;; technology.  No problem object name appears; every instance comes from the staged
;;; databases.  T28 (section 8.7) added the jump and recorder contracts and the step entry.


(defparameter *mechanic-contracts*
  '(("beam-crossing" :contract "also BX scenario" report-beam-crossing-instances
     "the derived CROSSING-ACTIVE of each crossing in the published CURRENT-BEAM-CROSSINGS pool, recomputed in every propagation before relay and receiver status"
     "nothing; an active crossing cuts every beam through it, and a cut link neither lights its relay nor activates its receiver"
     "both beams live for cutting and each reaching the crossing: no earlier active crossing on it from its live source, and no closed gate whose BEAM-CROSSINGS-BEFORE-GATE> list for that direction omits it. Live: transmitter to a location whose connector is paired with it, lit or not; a lit connector to a paired receiver or repeater, or to a location whose connector is paired with it either way; a fixed coupled beam from a transmitter with a clear corridor, or from a lit repeater. Liveness ignores current visibility, bodies, walls and receiver hue; only gates split a crossing order, and fixed coupled beams have no split. Fixed point; a two-set oscillation is resolved by the validated union, else nearest-to-source arbitration, else the state is inconsistent. Proper 2D intersections, no height filter")
    ("beam-direct" :contract "also RC NH H3" report-fixed-beam-corridors
     "receiver activation from a chroma-matching direct coupling with a clear corridor and no BEAM-CUT; repeaters additionally depend on upstream lighting"
     "nothing; fixed oriented links are COUPLED pairs, independent of connector placements"
     "BEAM-VIA present; recorded walls/edges/boundaries and closed gates clear at interpolated endpoint heights; unrecorded authored gates open; authored location occupants not spanning beam height. Views may differ; other beam mechanisms can activate the receiver")
    ("beam-relay" :contract "also S6 RC; RL scenario" report-beam-relay-instances
     "the derived COLOR of every relay (connector or repeater), recomputed in each propagation after the crossing set, and each receiver's ACTIVE fact (RECORDING-ACTIVE in the recording view)"
     "connectors only: PICKUP-CONNECTOR lifts one and deletes every PAIRED fact it owns and every one naming it; PICKUP-CONNECTOR-RETAINING-PAIRINGS keeps them, but a held connector has no location and neither receives nor sends a beam; PUT-CONNECTOR places without pairing; CONNECT-CONNECTOR places the held connector at a location the agent reaches and stores 1 to *max-connector-pairings* outgoing PAIRED facts, to termini structurally visible from any location the agent can walk to, never to a connector at the placement location"
     "lighting runs in propagation layers from every transmitter (layer 0). A relay settles in the first layer in which any clear link from a lit source reaches it: one hue lights it; two or more hues in that layer leave it dark, and a dark relay feeds nothing; a hue arriving in a later layer is ignored. A connector is also dark when a connector at its location is already lit. Clear link: a stored PAIRED fact in either direction (COUPLED for fixed apparatus), a live sightline in the reading view, and no cut by an active crossing (physical view only). Pairings persist while their beam is blocked or cut; only pickup clears them. Capacity counts a connector's outgoing pairings; incoming links are unlimited. A receiver is reached only by a relay of its hue whose own outgoing pairing (or coupling) names it, visible and uncut; beam-direct may reach it independently. CONNECT-CONNECTOR needs no lit connector of the same recorder layer at the placement location. Layers are propagation order, not travel time or action order")
    ("box" :extractors "S2 S5")
    ("elevation" :infrastructure "authors levels; read by S5")
    ("floor-blower" :contract "also S1" report-floor-blower-instances
     "its CONTROLS entry; turning == the control aggregate while no jammer jams it (S1 device state axiom), read in each object's own view"
     "every non-fan occupant resting ON the blower, with its stack, to the AIMED-AT destination while the blower turns in that occupant's view; a fan resting on it is toppled at the source"
     "an occupant ON the blower (an agent mounts it by step); when no floor drive aimed at the destination turns in its view, an occupant there not ON a support falls back to the blower's location -- keep the lift by leaving through an exit arc, or standing on a support there, before the stream stops")
    ("floor-gears" :contract "also S1 CC; EQ scenario" report-floor-gears-instances
     "the gears' CONTROLS aggregate (uncontrolled default on) unless jammed; TURNING is that state. A stream exists only while a fan is mounted and the gears turn: turning gears with no fan lift nothing, and a fan on stopped gears is inert"
     "a non-fan occupant resting ON the mounted fan, with its stack, to AIMED-AT; when no floor drive aimed there has a fan and turns, an occupant there not ON a support drops back to the gears' location. A loose fan resting on a mounted fan is toppled to the ground at the source"
     "removal by PICKUP-FAN: empty hands, REACHABLE, vertical reach to the fan, and a clear top (an occupied fan cannot be lifted). MOUNT-FAN: holding the fan, manipulation allowed, the gears' location REACHABLE, no fan already mounted there, vertical reach to the working height; turning gears accept it. A floor-mounted fan is a flush steppable support at the gears' location; a fan on the ground or a box is not steppable. Boarding is a STEP from the ground there onto the clear fan, support use allowed. One fan occupies one mount at a time; any gears type accepts it. Recorder problems reject floor gears")
    ("gate" :extractors "S1 S3 S4")
    ("jammer" :contract "also S1" report-jammer-instances
     "a placed jam overrides a gate open, a blower drive stopped or a gun safe; picking up the jammer removes its jam and support"
     "carried cargo; PUT-JAMMER places inertly, JAM-TARGET places and jams; placement on a pressure plate can also supply weight"
     "holding a jammer, reachable placement, no directional JAM-DISALLOWED> exclusion, legal placement/support/view and target sightline from the placed jammer's top. Ground/plate/box sightline rows are candidates only; gate bits are hypotheses")
    ("jump" :contract "also S3 S5" report-jump-instances
     "nothing; a jump has no CONTROLS entry and no state"
     "an agent across a jump-kind clause -- one naming an edge, a wall or a floor drive, or in a bare-level problem naming none across a level difference -- (symmetric, or one way when directed), landing on the floor or on a box or held-tray top at the far end; locally, onto a box or held-tray top at its own location, and down from one"
     "the landing at most *vertical-reach-limit* above the launch elevation (the floor, or the top of the support the agent stands on); edges and floor drives are static and always passable; every other clause member a gate, screen or wall, and each one not passable (a closed gate, a non-passable screen, every wall) with its top at most that limit above the launch; a safe destination.  Downward and level landings are unrestricted; a grounded tray is no landing, nor the agent's own held tray")
    ("ladder" :contract "also S3" report-ladder-instances
     "nothing; a ladder has no CONTROLS entry and no state"
     "an agent across a climb-kind clause (one naming a ladder) from its fact's source to its destination, one way (traverse-via>); a supported agent at the source lands on the ground at the destination"
     "a ladder named in the arc's clause positioned at its source (ladder-init-check), every other means in the clause clear, and a safe destination")
    ("stairs" :contract "also S3" report-stairs-instances
     "nothing; a staircase has no state"
     "an agent across a stairs-kind clause (one naming a staircase), in its fact's permitted direction, through a MOVE stairs segment"
     "all means of one alternative clause (the clause without its staircase) passable for the mover and a safe destination; no elevation-difference or elevation-equality limit. Empty hands only when a clause's means require them")
    ("plate" :extractors "S1 S2 T6")
    ("reachability" :infrastructure "reach relations; read by S4")
    ("recorder" :contract "also S2 RO CP" report-recorder-instances
     "the recording session, not a device (no CONTROLS entry): START-RECORDER opens a cycle -- a live agent at a recorder's position, empty-handed, no ghost left from a closed cycle, within *max-recorder-cycles*; STOP-RECORDER (by a ghost agent) or CANCEL-PLAYBACK (by a live agent) closes it"
     "at START-RECORDER each live mobile object's ghost appears where the live one is, with its holding, ON and pairing state; while the cycle is open live and ghost bodies both act, each manipulating only its own side's objects; closing removes every ghost and every fact naming one, and rebuilds the recording view from live state"
     "STOP: every ghost agent at a recorder's position and empty-handed, and no HOLDING or ON between a live and a ghost object; CANCEL: the live agent at a recorder's position and empty-handed, ghost dependencies discarded.  Devices and plates are read in each object's own view: physical counts every body present, ghosts included; recording counts ghost occupants only.  A live body may stand on a ghost-held tray; a ghost never uses a live support.  A closed cycle must leave persistent progress.  Initialization rejects beam crossings, floor gears, angled blowers, threats, receiver-controlled blower drives and movable wall-fan copies")
    ("step" :extractors "S2 T6; boarding a fixed floor blower in the floor-blower contract, a gears-mounted fan in the floor-gears contract")
    ("switch" :extractors "S1 S4")
    ("topo-lower-bound" :infrastructure "search pruning bound")
    ("tray" :extractors "S2 S5")
    ("visibility" :infrastructure "line of sight; read by S6")
    ("walkability" :infrastructure "derives walk-kind facts; read by S3")
    ("wall-blower" :contract "also S1 S3 RC CC" report-wall-blower-instances
     "its CONTROLS aggregate (uncontrolled default on), unless jammed; a fan must be present. Live objects read TURNING, ghosts read RECORDING-TURNING; the two views need not agree"
     "horizontal sweep from HAS-POSITION to AIMED-AT when base < stream <= top; detach from support, relocate the occupant and its stack, with held cargo following its agent; land on a flush-floor support or ground. Pairing and jamming facts persist, effects recomputed at the destination"
     "own-view fan activity and body contact with the stream; fans are never swept. Wall-mounted fans have no HAS-LOCATION and are not standing supports. Walk-kind clauses naming the drive require it inactive in the actor's view. Directly unswept bodies may still move with swept supports; transport cycles must converge"))
  "The contract registry, one entry per public technology name, section 8.3 of the
   specification.  An entry is (NAME KIND NOTE) for KIND :EXTRACTORS, whose NOTE names the
   components that already carry the technology's static consequences, and for
   :INFRASTRUCTURE, whose NOTE says what the technology supplies instead of a constraint.
   A :CONTRACT entry adds its instance reporter and its three contract texts, (NAME
   :CONTRACT NOTE REPORTER CONTROLS MOVES REQUIRES).  The texts were written by hand from
   the technology's source and are quoted, not derived.  A technology with no entry is
   UNCOVERED; that absence is the finding, so nothing here is a placeholder.")


(defun mechanic-public-techs ()
  "The public technologies the staged problem spliced, sorted by name: every name in
   *SPLICED-TECH-NAMES* that does not begin with a dash.  Spliced rather than included,
   because a public technology nested by another public one still brings its mechanics;
   dash files are covered through the public technology that nests them."
  (sort (remove-if (lambda (name) (char= (char name 0) #\-))
                   (remove-duplicates (copy-list *spliced-tech-names*) :test #'string=))
        #'string<))


(defun mechanic-verdict-text (entry)
  "The verdict column for one registry ENTRY, or UNCOVERED when there is none."
  (if entry
    (ecase (second entry)
      (:contract (format nil "COVERED    contract (~A)" (third entry)))
      (:extractors (format nil "COVERED    extractors ~A" (third entry)))
      (:infrastructure (format nil "COVERED    infrastructure -- ~A" (third entry))))
    "UNCOVERED"))


(defun report-mechanic-verdicts (techs)
  "One verdict row per public technology, then the UNCOVERED list on its own line so the
   coverage gate can be read without scanning the table."
  (let ((uncovered (remove-if (lambda (tech)
                                (find tech *mechanic-contracts* :key #'first :test #'string=))
                              techs)))
    (format t "~%  public technologies spliced (~D): ~D covered, ~D UNCOVERED~%"
            (length techs) (- (length techs) (length uncovered)) (length uncovered))
    (dolist (tech techs)
      (format t "    ~18A~A~%" tech
              (mechanic-verdict-text (find tech *mechanic-contracts*
                                           :key #'first :test #'string=))))
    (format t "~%  UNCOVERED (~D):~{ ~A~}~%" (length uncovered) uncovered)
    (format t "    READING: UNCOVERED means no static contract is declared for that technology.  ~
               The coverage gate (Problem-Solving Guide, Phase 0 step 2) requires a hand ~
               contract in the Briefing, or a component, before going on.  COVERED by ~
               extractors or infrastructure means the named components carry its static ~
               consequences; it is not a contract.~%")))


(defun mechanic-exit-text (location arc)
  "One exit ARC from LOCATION as its far endpoint, then its clause family when it has one,
   then a directed marker.  A symmetric arc is stored with its endpoints in name order
   (TRAVERSAL-ARC-FACTS), so the far endpoint is whichever end is not LOCATION."
  (format nil "~(~A~)~@[ ~(~S~)~]~:[~; (directed)~]"
          (if (eq (third arc) location) (fifth arc) (third arc))
          (fourth arc)
          (eq (first arc) *traversal-directed-relation*)))


(defun report-mechanic-exits (location arcs)
  "Every traversal arc that leaves LOCATION, grouped by kind: a symmetric arc with LOCATION
   at either end, or a directed arc with LOCATION as its source.  These are S3's input arcs,
   read before any contraction; the kind predicates -- jump reach and clearance -- are not
   evaluated, so an arc listed here is a candidate exit, not a legal move."
  (let ((exits (remove-if-not (lambda (arc)
                                (or (eq (third arc) location)
                                    (and (eq (first arc) *traversal-symmetric-relation*)
                                         (eq (fifth arc) location))))
                              arcs)))
    (format t "      exits from ~(~A~) (~D), by kind; each kind's own predicate is not evaluated~%"
            location (length exits))
    (dolist (kind (sort (remove-duplicates (mapcar #'second exits))
                        #'string< :key #'symbol-name))
      (let ((texts (sort (loop for arc in exits
                               when (eq (second arc) kind)
                                 collect (mechanic-exit-text location arc))
                         #'string<)))
        (format t "        ~(~A~) (~D): ~{~A~^, ~}~%" kind (length texts) texts)))))


(defun report-floor-blower-row (blower facts arcs controls)
  "BLOWER's source, destination, control and the exits at its destination.  The exits are
   what the drop rule makes matter: a lifted occupant keeps its height only by leaving the
   destination, or by standing on a support there, before the stream stops."
  (let ((destination (keeper-fact-value 'aimed-at blower facts))
        (control (find blower controls :key #'third)))
    (format t "    ~(~A~)~%" blower)
    (format t "      source       ~(~A~)~%" (keeper-fact-value 'has-position blower facts))
    (format t "      destination  ~(~A~)~%" destination)
    (if control
      (format t "      control      ~(~S~) ~(~A~)~%" (second control) (fourth control))
      (format t "      control      no CONTROLS entry~%"))
    (report-mechanic-exits destination arcs)))


(defun report-floor-blower-instances ()
  "The floor-blower contract's instance rows, one per blower, sorted by name."
  (let ((facts (list-static-db))
        (arcs (traversal-arc-facts))
        (controls (control-facts)))
    (dolist (blower (sort (copy-list (census-type-instances 'floor-blower))
                          #'string< :key #'symbol-name))
      (report-floor-blower-row blower facts arcs controls))))


(defun report-wall-blower-instances ()
  "Wall-drive endpoints, stream dimensions, mounting kind and control wiring."
  (let ((static (list-static-db))
        (controls (control-facts)))
    (dolist (drive (wall-stream-drives))
      (let ((control (find drive controls :key #'third)))
        (format t "    ~(~A~)  ~A~%" drive
                (if (member drive (census-type-instances 'wall-blower))
                  "fixed complete fixture" "wall gears; removable fan required"))
        (format t "      horizontal   ~(~A~) -> ~(~A~)~%"
                (keeper-fact-value 'has-position drive static)
                (keeper-fact-value 'aimed-at drive static))
        (format t "      stream       elevation ~A; width ~A; base < stream <= top~%"
                (funcall (symbol-function 'blower-elevation) *start-state* drive)
                (or (keeper-fact-value 'stream-width drive static) 3))
        (if control
          (format t "      control      ~(~S~) ~(~A~), separately in each environmental view~%"
                  (second control) (fourth control))
          (format t "      control      uncontrolled default on, unless jammed~%"))))))


(defun report-ladder-row (ladder facts arcs)
  "LADDER's position and every traversal arc whose clause family names it, with whether the
   ladder stands at that arc's source -- the condition LADDER-INIT-CHECK enforces, printed
   so a reader need not take the init check on trust."
  (let ((position (keeper-fact-value 'has-position ladder facts))
        (named (remove-if-not (lambda (arc)
                                (some (lambda (clause) (member ladder clause)) (fourth arc)))
                              arcs)))
    (format t "    ~(~A~)  at ~(~A~)~%" ladder position)
    (if named
      (dolist (arc named)
        (format t "      ~(~A~)  ~(~A~) ~:[--~;-->~] ~(~A~)  family ~(~S~)  ladder at source: ~:[NO~;yes~]~%"
                (second arc) (third arc) (eq (first arc) *traversal-directed-relation*)
                (fifth arc) (fourth arc) (eq position (third arc))))
      (format t "      no traversal arc names it~%"))))


(defun report-ladder-instances ()
  "The ladder contract's instance rows, one per ladder, sorted by name."
  (let ((facts (list-static-db))
        (arcs (traversal-arc-facts)))
    (dolist (ladder (sort (copy-list (census-type-instances 'ladder))
                          #'string< :key #'symbol-name))
      (report-ladder-row ladder facts arcs))))


(defun jump-reading-raise (vaulted source-level target-level state)
  "The raise above the source floor a jump needs when the VAULTED members must be cleared:
   the launch is the greater of the landing level and every vaulted member's top, less the
   reach limit, and the raise is how far that launch sits above the floor, never below zero.
   The same bound serves the landing and the clearance, as in JUMP-ELEVATION-REACHABLE and
   JUMP-PATH-CLEAR."
  (let ((highest (reduce #'max (mapcar (lambda (object)
                                         (funcall (symbol-function 'top) state object))
                                       vaulted)
                         :initial-value target-level)))
    (max 0 (- highest *vertical-reach-limit* source-level))))


(defun report-jump-reading (label vaulted source-level target-level state)
  "One reading line: LABEL, then \"from the floor\" or the raise the jump needs."
  (let ((raise (jump-reading-raise vaulted source-level target-level state)))
    (format t "      ~A  ~:[raise ~A above the floor~;from the floor~]~%"
            label (zerop raise) raise)))


(defun report-jump-clause (clause source-level target-level state)
  "The readings of one CLAUSE.  A member that is not a gate, screen or wall makes the clause
   no jump at all.  Gates and screens are vaulted only when not passable, which is a state
   reading, so a clause naming one prints an open reading (walls vaulted) and a closed one
   (every member vaulted); walls are always vaulted."
  (let* ((walls (census-type-instances 'wall))
         (foreign (remove-if (lambda (object)
                               (member object (census-type-instances 'vaultable-object)))
                             clause))
         (passable (remove-if (lambda (object) (member object walls)) clause))
         (fixed (remove-if-not (lambda (object) (member object walls)) clause)))
    (cond (foreign
           (format t "      ~(~{~A~^ ~}~) not a gate, screen or wall: no jump across this clause~%"
                   foreign))
          (passable
           (report-jump-reading (format nil "~(~{~A~^ ~}~) open" passable)
                                fixed source-level target-level state)
           (report-jump-reading (format nil "~(~{~A~^ ~}~) closed" passable)
                                clause source-level target-level state))
          (fixed
           (report-jump-reading (format nil "walls ~(~{~A~^ ~}~)" fixed)
                                fixed source-level target-level state))
          (t
           (report-jump-reading "no feature" nil source-level target-level state)))))


(defun report-jump-direction (source destination family state)
  "One jump direction, SOURCE to DESTINATION, with the floor levels and, for each clause of
   FAMILY (an empty family is one empty clause), its readings."
  (let ((source-level (funcall (symbol-function 'location-elevation) state source))
        (target-level (funcall (symbol-function 'location-elevation) state destination)))
    (dolist (clause (or family (list nil)))
      (format t "    ~(~A~) -> ~(~A~)  clause (~(~{~A~^ ~}~))  level ~A -> ~A~%"
              source destination clause source-level target-level)
      (report-jump-clause clause source-level target-level state))))


(defun report-jump-instances ()
  "The jump contract's instance rows: the reach limit, then every traversal arc of kind
   JUMP in S3's arc order, a symmetric arc in its stored direction and then the reverse.
   Levels and tops are read from the staged start; they are static."
  (let ((state *start-state*)
        (arcs (remove-if-not (lambda (arc) (eq (second arc) 'jump)) (traversal-arc-facts))))
    (format t "    reach limit ~A (*vertical-reach-limit*); a raise is the least launch elevation above the source floor, reached by standing on a support (S5 tops)~%"
            *vertical-reach-limit*)
    (unless arcs
      (format t "    no jump arc~%"))
    (dolist (arc arcs)
      (report-jump-direction (third arc) (fifth arc) (fourth arc) state)
      (when (eq (first arc) *traversal-symmetric-relation*)
        (report-jump-direction (fifth arc) (third arc) (fourth arc) state)))))


(defun report-recorder-instances ()
  "The recorder contract's instance rows: the cycles allowed, the live -> ghost pairs S2
   reads by shape (by live name), and each recorder with its position.  No cycle count is
   stated: that would be analysis, not a contract."
  (let ((facts (list-static-db))
        (pairs (sort (copy-list (layer-pairs)) #'string<
                     :key (lambda (pair) (symbol-name (car pair))))))
    (format t "    cycles allowed  ~(~A~) (*max-recorder-cycles*)~%"
            (or *max-recorder-cycles* "unlimited"))
    (format t "    live -> ghost (~D):~{ ~(~A~) -> ~(~A~)~^,~}~%"
            (length pairs) (loop for pair in pairs append (list (car pair) (cdr pair))))
    (dolist (recorder (sort (copy-list (census-type-instances 'recorder))
                            #'string< :key #'symbol-name))
      (format t "    ~(~A~)  at ~(~A~)~%" recorder
              (keeper-fact-value 'has-position recorder facts)))))


(defun jammer-survey-sites ()
  "Ground everywhere, authored plate sites, and staged box tops; not moved support layouts."
  (let ((sites (loop for location in (census-type-instances 'location)
                     collect (list location 'ground)))
        (static (list-static-db)) (dynamic (database *start-state*)))
    (dolist (plate (census-type-instances 'plate))
      (let ((location (keeper-fact-value 'has-position plate static)))
        (when location (push (list location plate) sites))))
    (dolist (box (census-type-instances 'box))
      (let ((location (keeper-fact-value 'has-location box dynamic)))
        (when location (push (list location box) sites))))
    (sort sites #'string< :key #'prin1-to-string)))


(defun jammer-site-visible-p (state site jammer target)
  "Physical sightline only; no placement/action legality is asserted."
  (funcall (symbol-function 'jammer-target-visible-from-placement)
           state nil (first site) (second site) jammer target))


(defun jammer-sightline-rows ()
  "Visible surveyed sites and gates whose single closure blocks them."
  (multiple-value-bind (open-state closed-states)
      (relay-chain-gate-states (census-type-instances 'gate))
    (let ((sites (jammer-survey-sites)))
      (loop for jammer in (keeper-sorted-set (census-type-instances 'jammer))
            append (loop for target in (keeper-sorted-set (census-type-instances 'target))
                         collect (list :jammer jammer :target target
                                       :sites (loop for site in sites
                                                    when (jammer-site-visible-p open-state site jammer target)
                                                      collect (append site
                                                                (list (loop for (gate . state) in closed-states
                                                                            unless (jammer-site-visible-p state site jammer target)
                                                                              collect gate))))))))))


(defun report-jammer-instances ()
  "Sightline candidates under stated gate premises, and authored directional exclusions."
  (format t "    Survey: ground, fixed plates, staged box tops; physical view, all gates forced open without propagation. No moved/stacked supports, trays or fans surveyed.~%")
  (dolist (row (jammer-sightline-rows))
    (format t "    ~(~A~) -> ~(~A~): visible sites (location support required-open gates) ~(~S~)~%"
            (getf row :jammer) (getf row :target) (getf row :sites)))
  (format t "    JAM-DISALLOWED> (agent-location placement-location target):~%")
  (dolist (fact (sort (remove-if-not (lambda (fact) (eq (first fact) 'jam-disallowed>))
                                    (list-static-db)) #'string< :key #'prin1-to-string))
    (format t "      ~(~S~)~%" (rest fact)))
  (format t "    A sightline is not an applicable action: exclusions, reach, support use, occupancy, availability and actual view remain to be checked.~%"))


(defun report-stairs-instances ()
  "Stairs-kind arcs, retaining alternative clauses and direction.  A family lists only
   the doors beside the staircase; NIL means the staircase alone."
  (dolist (arc (traversal-arc-facts))
    (when (eq (second arc) 'stairs)
      (format t "    ~(~A~) ~:[<->~;->~] ~(~A~); family ~(~S~)~%"
              (third arc) (eq (first arc) *traversal-directed-relation*)
              (fifth arc) (fourth arc)))))


(defun equipment-state-facts (state)
  "STATE's propositions, with a bijective relation's index names read back as the relation
   itself, as FROM-HERE-FACTS reads them, so a held fan's HOLDING fact is present."
  (remove-duplicates
    (mapcar (lambda (fact)
              (let ((canonical (car (gethash (first fact) *bijective-canonical*))))
                (if canonical
                  (cons canonical (rest fact))
                  fact)))
            (database state))
    :test #'equal))


(defun equipment-gears ()
  "Every gears instance -- floor, wall and angled mounts for a removable fan -- in name order."
  (keeper-sorted-set (append (census-type-instances 'floor-gears)
                             (census-type-instances 'wall-gears)
                             (census-type-instances 'angled-gears))))


(defun equipment-gears-kind (gears)
  "GEARS's mounting kind: FLOOR, WALL or ANGLED."
  (cond ((member gears (census-type-instances 'floor-gears)) 'floor)
        ((member gears (census-type-instances 'wall-gears)) 'wall)
        (t 'angled)))


(defun equipment-mounted-fan (gears facts)
  "The fan FACTS mount on GEARS, or NIL."
  (second (find-if (lambda (fact) (and (eq (first fact) 'mounted-on) (eq (third fact) gears)))
                   facts)))


(defun equipment-fan-place (fan facts)
  "FAN's place in the dynamic FACTS: (:MOUNTED gears location), a wall-hung fan having no
   location; (:HELD agent); (:RESTING support-or-ground location); or (:ABSENT)."
  (let ((gears (keeper-fact-value 'mounted-on fan facts))
        (holder (second (find-if (lambda (fact) (and (eq (first fact) 'holding) (eq (third fact) fan)))
                                 facts)))
        (location (keeper-fact-value 'has-location fan facts)))
    (cond (gears (list :mounted gears location))
          (holder (list :held holder))
          (location (list :resting (or (keeper-fact-value 'on fan facts) 'ground) location))
          (t (list :absent)))))


(defun equipment-place-text (place)
  "One fan PLACE as printed."
  (ecase (first place)
    (:mounted (if (third place)
                (format nil "MOUNTED on ~(~A~) at ~(~A~)" (second place) (third place))
                (format nil "MOUNTED on ~(~A~), WALL-HUNG (no location)" (second place))))
    (:held (format nil "HELD by ~(~A~)" (second place)))
    (:resting (format nil "RESTING on ~(~A~) at ~(~A~)" (second place) (third place)))
    (:absent "ABSENT")))


(defun equipment-gated-arcs (drive arcs)
  "The traversal ARCS with a clause naming DRIVE: passable only while DRIVE has no active
   stream in the mover's view (STREAM-OBSTACLE-CLEAR)."
  (remove-if-not (lambda (arc) (some (lambda (clause) (member drive clause)) (fourth arc)))
                 arcs))


(defun equipment-arc-text (arc)
  "One traversal ARC as kind and endpoints with direction; S3 prints its clauses."
  (format nil "~(~A~) ~(~A~)~:[<->~;->~]~(~A~)"
          (second arc) (third arc) (eq (first arc) *traversal-directed-relation*) (fifth arc)))


(defun equipment-reach-sites (position state)
  "Every location from which the engine's REACHABLE reaches POSITION in STATE."
  (remove-if-not (lambda (location)
                   (funcall (symbol-function 'reachable) state position location))
                 (keeper-sorted-set (census-type-instances 'location))))


(defun equipment-site-text (site height state)
  "One reach SITE and whether HEIGHT lies within the reach limit of its floor."
  (let ((floor (funcall (symbol-function 'location-elevation) state site)))
    (format nil "~(~A~) (floor ~A: ~:[beyond vertical reach from the floor~;within vertical reach~])"
            site floor (<= (abs (- height floor)) *vertical-reach-limit*))))


(defun equipment-start-text (fan turning)
  "A mount's start occupancy and turning, and whether that gives a stream."
  (cond ((and fan turning) (format nil "~(~A~) mounted, turning: EFFECTIVE STREAM" fan))
        (fan (format nil "~(~A~) mounted, stopped: no stream" fan))
        (turning "VACANT, TURNING, NO FAN: no stream")
        (t "VACANT, stopped: no stream")))


(defun report-equipment-fans (fans mounts dynamic)
  "Each removable fan's start place, the compatible mounts, and the one-mount-at-a-time
   limit on how many mounts can have a stream together."
  (format t "    fans (~D); every gears instance accepts any fan (MOUNT-FAN), one mount per fan at a time~%"
          (length fans))
  (dolist (fan fans)
    (format t "      ~(~A~)  start ~A; compatible mounts ~(~{~A~^, ~}~)~%"
            fan (equipment-place-text (equipment-fan-place fan dynamic)) mounts))
  (format t "      ~D fan~:P for ~D mount~:P: at most ~D mount~:P can have a stream at once; a fan moved to one mount leaves its old mount without a stream~%"
          (length fans) (length mounts) (min (length fans) (length mounts))))


(defun report-equipment-removal (gears arcs)
  "What removing a mounted fan does at GEARS: the stream stops though the gears keep
   turning, and the arcs its stream gates become passable without it."
  (let ((gated (equipment-gated-arcs gears arcs)))
    (format t "      removal (PICKUP-FAN, empty hands, reach to the fan): the stream stops; the gears keep turning; a jam of ~(~A~) is redundant while no fan is mounted~%"
            gears)
    (format t "      arcs gated by its stream (~D; clauses in S3), passable while it has no fan or does not turn in the mover's view: ~:[none~;~:*~{~A~^, ~}~]~%"
            (length gated) (mapcar #'equipment-arc-text gated))))


(defun report-equipment-installation (position destination arcs state)
  "What installing a fan on floor gears at POSITION supplies: a steppable support, the boarding
   conditions, the lift and the landing requirements with the destination's exits."
  (format t "      installation: the fan becomes a flush steppable support at ~(~A~); a fan lying on the ground or a box is not steppable~%"
          position)
  (format t "      boarding: STEP from the ground at ~(~A~) onto the fan, top clear, support use allowed; while the gears turn, a non-fan occupant ON it is launched with its stack to ~(~A~) (level ~A)~%"
          position destination (funcall (symbol-function 'location-elevation) state destination))
  (format t "      landing: kept only while some floor drive aimed at ~(~A~) has a fan and turns; otherwise an occupant not ON a support drops back to ~(~A~) -- stand on a support there or leave by an exit first~%"
          destination position)
  (report-mechanic-exits destination arcs))


(defun report-equipment-mount (gears state static dynamic arcs controls)
  "One mount GEARS: endpoints, working height, control, start occupancy, reach sites, and
   the removal and installation consequences its kind supports."
  (let* ((kind (equipment-gears-kind gears))
         (position (keeper-fact-value 'has-position gears static))
         (destination (keeper-fact-value 'aimed-at gears static))
         (height (funcall (symbol-function 'blower-elevation) state gears))
         (control (find gears controls :key #'third)))
    (format t "    ~(~A~)  ~(~A~) gears at ~(~A~) -> ~(~A~) (level ~A); working height ~A~%"
            gears kind position destination
            (funcall (symbol-function 'location-elevation) state destination) height)
    (if control
      (format t "      control      ~(~S~) ~(~A~), unless jammed~%" (second control) (fourth control))
      (format t "      control      uncontrolled default on, unless jammed~%"))
    (format t "      start        ~A~%"
            (equipment-start-text (equipment-mounted-fan gears dynamic)
                                  (member (list 'turning gears) dynamic :test #'equal)))
    (format t "      mount from   ~{~A~^, ~} (engine REACHABLE, start state; floor level only)~%"
            (mapcar (lambda (site) (equipment-site-text site height state))
                    (equipment-reach-sites position state)))
    (report-equipment-removal gears arcs)
    (case kind
      (floor (report-equipment-installation position destination arcs state))
      (wall (format t "      stream physics: the wall-blower contract~%"))
      (angled (format t "      angled launch: no contract here~%")))))


(defun report-floor-gears-instances ()
  "The floor-gears contract's instance rows: every removable fan and every mount, floor,
   wall or angled, with prerequisites and consequences.  Compatibility only."
  (let* ((state *start-state*)
         (static (list-static-db))
         (dynamic (equipment-state-facts state))
         (arcs (traversal-arc-facts))
         (controls (control-facts))
         (mounts (equipment-gears)))
    (report-equipment-fans (keeper-sorted-set (census-type-instances 'fan)) mounts dynamic)
    (format t "    mounts (~D)~%" (length mounts))
    (dolist (gears mounts)
      (report-equipment-mount gears state static dynamic arcs controls))
    (format t "    These rows state compatibility and prerequisites only: not that a fan can be carried between mounts, a mount reached, or a lift used.  A settled state's equipment needs REPORT-EQUIPMENT-SCENARIO.~%")))


(defun report-mechanic-contract (entry)
  "One :CONTRACT registry ENTRY: its three quoted texts, then its instance rows."
  (destructuring-bind (tech kind note reporter controls moves requires) entry
    (declare (ignore kind note))
    (format t "~%  contract ~A~%" tech)
    (format t "    controls  ~A~%" controls)
    (format t "    moves     ~A~%" moves)
    (format t "    requires  ~A~%" requires)
    (funcall reporter)))


(defun report-mechanic-coverage ()
  "MC, grade 1.  The mechanic coverage gate: every public technology the staged problem
   spliced, COVERED by a contract, by named extractors or as infrastructure, or UNCOVERED;
   then each spliced contract with its instance rows.  Grade 1 because the rows are direct
   readings of static facts; the contract texts are quoted from the registry, and a
   contract is a statement of what the tech/ code does, not a check of it."
  (let ((techs (mechanic-public-techs)))
    (format t "~2%MC  MECHANIC COVERAGE  [grade 1]~%")
    (format t "~A~%" (make-string 62 :initial-element #\-))
    (report-mechanic-verdicts techs)
    (dolist (tech techs)
      (let ((entry (find tech *mechanic-contracts* :key #'first :test #'string=)))
        (when (and entry (eq (second entry) :contract))
          (report-mechanic-contract entry))))
    (values)))


;;;; CC -- COUPLING CENSUS (T20, I3) ;;;;
;;;
;;; Specification: doc/constraint-led-solving/Extractor-Specifications.md section 9.  Which
;;; controls or devices change two subsystems at once: a role table over every controlled
;;; device and primitive controller, fan-out primitives (K1), multi-role objects (K2), the
;;; gates that can cut a beam-driven controller's last hop (K3), and lift-barrier couplings
;;; flagged with the G15 checklist's question.  It reads S1, S3 and RC, and prints last.
;;;
;;; SUBSTRATE VOCABULARY (C3).  The relation AIMED-AT, the types SUPPORT and GATE, and the
;;; role and subsystem words below are named; each is a tech/ interface or this section's
;;; own vocabulary.  No problem object name appears; every instance comes from the staged
;;; databases.


(defparameter *coupling-role-subsystems*
  '((barrier . route) (occluder . beam) (lift . lift)
    (horizontal-transport . transport) (transport . transport)
    (support-controller . occupancy) (beam-driven . beam))
  "Each role in the order it prints, paired with the subsystem it belongs to, section
   9.2 of the specification.")


(defparameter *coupling-subsystem-order* '(route beam lift transport occupancy)
  "The order subsystems print in.")


(defun coupling-subsystems (roles)
  "The subsystems ROLES belong to, once each, in print order."
  (remove-if-not (lambda (subsystem)
                   (some (lambda (role)
                           (eq subsystem (cdr (assoc role *coupling-role-subsystems*))))
                         roles))
                 *coupling-subsystem-order*))


(defun coupling-word-text (words)
  "WORDS in lower case separated by spaces, or none."
  (if words
    (format nil "~(~{~A~^ ~}~)" words)
    "none"))


(defun coupling-hop-rows ()
  "One (target . required-open-gates) entry per visible RC hop, evaluated exactly as RC
   evaluates them: every station-to-fixed-endpoint hop, whose target is the endpoint, and
   every station-to-station hop, whose target is the target station's location.  A hop's
   required gates are those whose closing alone blocks it."
  (let ((stations (relay-chain-stations *start-state*)))
    (multiple-value-bind (open-state closed-states)
        (relay-chain-gate-states (census-type-instances 'gate))
      (append (loop for link in (relay-chain-endpoint-links stations open-state closed-states)
                    collect (cons (second link) (third link)))
              (loop for link in (relay-chain-station-links stations open-state closed-states)
                    collect (cons (first (second link)) (third link)))))))


(defun coupling-barriers (arcs)
  "Every object named in some clause of a traversal arc's family."
  (remove-duplicates (loop for arc in arcs
                           append (loop for clause in (fourth arc)
                                        append (copy-list clause)))))


(defun coupling-occluders (rows)
  "Every gate in the required-open set of some RC hop."
  (remove-duplicates (loop for row in rows
                           append (copy-list (cdr row)))))


(defun coupling-domain (facts)
  "Every controlled device and every primitive controller, sorted by name."
  (sort (remove-duplicates (append (mapcar #'third facts) (control-primitives facts)))
        #'string< :key #'symbol-name))


(defun coupling-motion-role (aimed-p floor-p wall-p)
  "AIMED-AT alone supplies transport, not the floor-blower lift/drop contract."
  (when aimed-p
    (cond (floor-p 'lift) (wall-p 'horizontal-transport) (t 'transport))))


(defun coupling-role-table (facts arcs rows)
  "One (object kind roles) entry per domain object.  KIND is a list of DEVICE and
   PRIMITIVE; ROLES follow the print order of *COUPLING-ROLE-SUBSYSTEMS*."
  (let* ((devices (mapcar #'third facts))
         (primitives (control-primitives facts))
         (barriers (coupling-barriers arcs))
         (occluders (coupling-occluders rows))
         (static (list-static-db))
         (supports (census-type-instances 'support)))
    (loop for object in (coupling-domain facts)
          collect (list object
                        (append (when (member object devices) (list 'device))
                                (when (member object primitives) (list 'primitive)))
                        (append (when (member object barriers) (list 'barrier))
                                (when (member object occluders) (list 'occluder))
                                (let ((role (coupling-motion-role
                                              (keeper-fact-value 'aimed-at object static)
                                              (member object (append (census-type-instances 'floor-blower)
                                                                     (census-type-instances 'floor-gears)))
                                              (member object (wall-stream-drives)))))
                                  (when role (list role)))
                                (when (and (member object primitives)
                                           (member object supports))
                                  (list 'support-controller))
                                (when (and (member object primitives)
                                           (eq (control-primitive-tier object devices)
                                               :device-mediated))
                                  (list 'beam-driven)))))))


(defun coupling-roles (object table)
  "OBJECT's roles from the role TABLE."
  (third (find object table :key #'first)))


(defun coupling-driven-facts (primitive facts)
  "The CONTROLS facts whose clauses name PRIMITIVE, in FACTS order."
  (remove-if-not (lambda (fact)
                   (some (lambda (clause) (member primitive clause)) (second fact)))
                 facts))


(defun coupling-fan-out (facts)
  "One (primitive . driven-facts) entry per primitive named by two or more devices."
  (loop for primitive in (control-primitives facts)
        for driven = (coupling-driven-facts primitive facts)
        when (cdr driven)
          collect (cons primitive driven)))


(defun coupling-pair-relation (fact other)
  "S1's relation between two devices: EXCLUSION or EQUIVALENCE on an identical clause
   set, by mode; otherwise DISTINCT CLAUSES."
  (cond ((not (equal (clause-set-key (second fact)) (clause-set-key (second other))))
         "DISTINCT CLAUSES")
        ((eq (fourth fact) (fourth other)) "EQUIVALENCE")
        (t "EXCLUSION")))


(defun coupling-exit-arcs (location arcs)
  "Every traversal arc leaving LOCATION, by MC's exit rule: a symmetric arc with LOCATION
   at either end, or a directed arc with LOCATION as its source."
  (remove-if-not (lambda (arc)
                   (or (eq (third arc) location)
                       (and (eq (first arc) *traversal-symmetric-relation*)
                            (eq (fifth arc) location))))
                 arcs))


(defun coupling-lift-barriers (fan-out arcs table)
  "One (primitive lift-fact destination barrier-fact exits) entry per K1 primitive, lift
   it drives, and other driven device that is a barrier on an exit arc from the lift's
   destination."
  (let ((static (list-static-db))
        (entries nil))
    (dolist (entry fan-out)
      (dolist (lift (cdr entry))
        (when (member 'lift (coupling-roles (third lift) table))
          (let ((destination (keeper-fact-value 'aimed-at (third lift) static)))
            (dolist (barrier (cdr entry))
              (let ((exits (remove-if-not
                             (lambda (arc)
                               (some (lambda (clause) (member (third barrier) clause))
                                     (fourth arc)))
                             (coupling-exit-arcs destination arcs))))
                (when (and exits (not (eq barrier lift)))
                  (push (list (car entry) lift destination barrier exits) entries))))))))
    (nreverse entries)))


(defun report-coupling-role-table (table)
  "The role table, one row per domain object."
  (format t "~%  role table (~D objects)~%" (length table))
  (dolist (row table)
    (format t "    ~(~A~)  ~A  ~A  subsystems ~A~%"
            (first row) (coupling-word-text (second row)) (coupling-word-text (third row))
            (coupling-word-text (coupling-subsystems (third row))))))


(defun report-coupling-fan-out (fan-out table)
  "K1: each fan-out primitive, its subsystems, its driven devices and their pairs."
  (format t "~%  K1 fan-out (~D)~%" (length fan-out))
  (unless fan-out
    (format t "    none~%"))
  (dolist (entry fan-out)
    (format t "    ~(~A~)  subsystems ~A~%"
            (car entry)
            (coupling-word-text
              (coupling-subsystems
                (append (coupling-roles (car entry) table)
                        (loop for fact in (cdr entry)
                              append (copy-list (coupling-roles (third fact) table)))))))
    (dolist (fact (cdr entry))
      (format t "      ~(~A~) == ~(~A~)  ~A~%"
              (third fact) (control-boolean-form (second fact) (fourth fact))
              (coupling-word-text (coupling-roles (third fact) table))))
    (loop for (fact . rest) on (cdr entry)
          do (dolist (other rest)
               (format t "      {~(~A~), ~(~A~)}  ~A~%"
                       (third fact) (third other) (coupling-pair-relation fact other))))))


(defun report-coupling-multi-role (table)
  "K2: every domain object whose own roles span two or more subsystems."
  (let ((rows (remove-if-not (lambda (row) (cdr (coupling-subsystems (third row)))) table)))
    (format t "~%  K2 multi-role (~D)~%" (length rows))
    (unless rows
      (format t "    none~%"))
    (dolist (row rows)
      (format t "    ~(~A~)  ~A~%"
              (first row) (coupling-word-text (coupling-subsystems (third row)))))))


(defun report-coupling-last-hop-gate (gate primitive facts fan-out)
  "One last-hop GATE: its controllers, SELF when PRIMITIVE is among them, K1 when a
   controller is a fan-out primitive."
  (let* ((fact (find gate facts :key #'third))
         (controllers (when fact (control-primitives (list fact))))
         (marks (append (when (member primitive controllers) (list "SELF"))
                        (when (some (lambda (controller) (assoc controller fan-out))
                                    controllers)
                          (list "K1")))))
    (if controllers
      (format t "        ~(~A~)  controllers ~(~{~A~^ ~}~)~{  ~A~}~%" gate controllers marks)
      (format t "        ~(~A~)  uncontrolled~%" gate))))


(defun report-coupling-beam-feedback (facts rows table fan-out)
  "K3: for each beam-driven primitive, the devices it drives and the gates required open
   by the RC hops ending at it."
  (let ((driven (remove-if-not (lambda (row) (member 'beam-driven (third row))) table)))
    (format t "~%  K3 beam feedback (~D)~%" (length driven))
    (unless driven
      (format t "    none~%"))
    (dolist (row driven)
      (let* ((primitive (first row))
             (gates (sort (remove-duplicates
                            (loop for entry in rows
                                  when (eq (car entry) primitive)
                                    append (copy-list (cdr entry))))
                          #'string< :key #'symbol-name)))
        (format t "    ~(~A~)  drives ~(~{~A~^ ~}~)~%"
                primitive (mapcar #'third (coupling-driven-facts primitive facts)))
        (if (loop for entry in rows thereis (eq (car entry) primitive))
          (progn (format t "      last-hop gates (~D)~%" (length gates))
                 (dolist (gate gates)
                   (report-coupling-last-hop-gate gate primitive facts fan-out)))
          (format t "      no RC hops~%"))))))


(defun report-coupling-lift-barrier (entry)
  "One G15 lift-barrier row: the lift, its destination, the barrier, its exit arcs, the S1
   relation and its label, and the checklist question for FLAG and CHECK."
  (destructuring-bind (primitive lift destination barrier exits) entry
    (let ((relation (coupling-pair-relation lift barrier)))
      (format t "    ~(~A~)  lift ~(~A~) -> ~(~A~)  barrier ~(~A~)~%"
              primitive (third lift) destination (third barrier))
      (dolist (arc exits)
        (format t "      exit ~(~A~) ~A~%" (second arc) (mechanic-exit-text destination arc)))
      (cond ((string= relation "EXCLUSION")
             (format t "      EXCLUSION  G15 FLAG: ~(~A~) is active only while ~(~A~) is inactive~%"
                     (third barrier) (third lift)))
            ((string= relation "EQUIVALENCE")
             (format t "      EQUIVALENCE  compatible: ~(~A~) is active exactly while ~(~A~) is active~%"
                     (third barrier) (third lift)))
            (t
             (format t "      DISTINCT CLAUSES  G15 CHECK: the relation of ~(~A~) and ~(~A~) depends on their other literals~%"
                     (third barrier) (third lift))))
      (unless (string= relation "EQUIVALENCE")
        (format t "      QUESTION (checklist 2.2): in the successor after the toggle, is the launch support at ~(~A~) still present?~%"
                destination)))))


(defun report-coupling-rows (facts arcs rows)
  "Every CC row for the control FACTS, traversal ARCS and RC hop ROWS given.  The
   entry point supplies the staged ones; a check may bind altered FACTS in a LET."
  (let* ((table (coupling-role-table facts arcs rows))
         (fan-out (coupling-fan-out facts))
         (lift-barriers (coupling-lift-barriers fan-out arcs table)))
    (report-coupling-role-table table)
    (report-coupling-fan-out fan-out table)
    (report-coupling-multi-role table)
    (report-coupling-beam-feedback facts rows table fan-out)
    (format t "~%  G15 lift-barrier couplings (~D)~%" (length lift-barriers))
    (unless lift-barriers
      (format t "    none~%"))
    (dolist (entry lift-barriers)
      (report-coupling-lift-barrier entry))))


(defun report-coupling-census ()
  "CC, grade 1 (occluder role grade 2).  The coupling census over the staged problem."
  (format t "~2%CC  COUPLING CENSUS  [grade 1; occluder role grade 2]~%")
  (format t "~A~%" (make-string 62 :initial-element #\-))
  (format t "  subsystems:~{ ~A~^,~}~%"
          (loop for subsystem in *coupling-subsystem-order*
                collect (format nil "~(~A~) (~(~{~A~^, ~}~))"
                                subsystem
                                (loop for (role . owner) in *coupling-role-subsystems*
                                      when (eq owner subsystem)
                                        collect role))))
  (report-coupling-rows (control-facts) (traversal-arc-facts) (coupling-hop-rows))
  (values))


;;;; EQ -- REMOVABLE EQUIPMENT IN ONE SUPPLIED STATE (T42) ;;;;
;;;
;;; Specification: doc/constraint-led-solving/Extractor-Specifications.md section 8.12.  One
;;; settled state's fans, mounts, boarding, mounting, removal and lifts, read with the
;;; engine's own queries and checked against its successor generator; optionally the
;;; differences from an earlier settled state.  Not part of the profile.  The caller's states
;;; are never changed.  MC's floor-gears block supplies the shared helpers.


(defun equipment-call (name state &rest arguments)
  "Call the engine query NAME on STATE."
  (apply (symbol-function name) state arguments))


(defun equipment-state-reason (state provenance label)
  "Why STATE, labelled LABEL, cannot be evaluated, or NIL."
  (cond ((not (typep state 'problem-state)) (format nil "~A: missing problem-state" label))
        ((not (and (stringp provenance) (plusp (length provenance))))
         (format nil "~A: state provenance missing" label))
        ((state-is-inconsistent state) (format nil "~A: state marked inconsistent" label))
        ((not (crossing-settled-p state))
         (format nil "~A: state is not a propagation fixed point; supply a replayed state or settle it first (section 6.2)"
                 label))))


(defun equipment-scenario-reason (scenario)
  "Why SCENARIO cannot be evaluated, or NIL.  Missing data is never an equipment verdict."
  (cond ((null (census-type-instances 'fan)) "no fan in the problem")
        ((null (equipment-gears)) "no gears in the problem")
        ((member "recorder" *spliced-tech-names* :test #'string=)
         "recorder technology spliced: live and recording views are not evaluated")
        ((not (member 'propagate-changes! *update-names*)) "propagation unavailable")
        (t (or (equipment-state-reason (getf scenario :state) (getf scenario :provenance) "state")
               (when (getf scenario :before)
                 (equipment-state-reason (getf scenario :before)
                                         (getf scenario :before-provenance) "before state"))))))


(defun equipment-agents-holding (facts)
  "Each (agent object) holding pair in FACTS."
  (loop for fact in facts
        when (eq (first fact) 'holding)
          collect (rest fact)))


(defun equipment-drive-status (fan turning)
  "A mount's status from its FAN and TURNING."
  (cond ((and fan turning) :effective-stream)
        (turning :turning-no-fan)
        (fan :fan-mounted-stopped)
        (t :vacant-stopped)))


(defun equipment-drive-records (facts)
  "One plist per gears: fan, turning, jammers and status."
  (loop for gears in (equipment-gears)
        collect (let ((fan (equipment-mounted-fan gears facts))
                      (turning (not (null (member (list 'turning gears) facts :test #'equal)))))
                  (list :gears gears :kind (equipment-gears-kind gears) :fan fan :turning turning
                        :jammers (keeper-sorted-set
                                  (loop for fact in facts
                                        when (and (eq (first fact) 'jamming) (eq (third fact) gears))
                                          collect (second fact)))
                        :status (equipment-drive-status fan turning)))))


(defun equipment-fan-records (state facts)
  "One plist per fan: place, blowing, and whether the engine finds it steppable."
  (loop for fan in (keeper-sorted-set (census-type-instances 'fan))
        collect (let ((place (equipment-fan-place fan facts)))
                  (list :fan fan :place place
                        :blowing (not (null (member (list 'blowing fan) facts :test #'equal)))
                        :steppable (let ((location (keeper-fact-value 'has-location fan facts)))
                                     (and location
                                          (equipment-call 'steppable-fixture-at state fan location)
                                          t))))))


(defun equipment-step-offered-p (state agent fan)
  "Whether the engine's step provider offers AGENT a step onto FAN from its configuration."
  (let ((configuration (equipment-call 'agent-configuration state agent)))
    (some (lambda (transition) (eql (second (fourth transition)) fan))
          (equipment-call 'step-configuration-transitions state agent configuration))))


(defun equipment-boarding-verdict (state facts agent fan location)
  "BOARDABLE, or the first failing boarding condition for AGENT onto FAN at LOCATION."
  (cond ((not (eq (keeper-fact-value 'has-location agent facts) location)) "AGENT ELSEWHERE")
        ((keeper-fact-value 'on agent facts) "AGENT NOT ON GROUND")
        ((not (equipment-call 'cleartop state fan agent)) "TOP OCCUPIED")
        ((not (equipment-call 'support-use-allowed state agent fan)) "SUPPORT USE NOT ALLOWED")
        (t "BOARDABLE")))


(defun equipment-boarding-records (state facts fans)
  "Per floor-mounted fan and agent, the boarding verdict and the engine's step offer; per
   resting fan at an agent's location, NOT STEPPABLE."
  (loop for record in fans
        for place = (getf record :place)
        append (loop for agent in (keeper-sorted-set (census-type-instances 'agent))
                     for location = (third place)
                     when (and location
                               (or (eq (first place) :mounted)
                                   (eq (keeper-fact-value 'has-location agent facts) location)))
                       collect (list :fan (getf record :fan) :agent agent
                                     :verdict (if (getf record :steppable)
                                                (equipment-boarding-verdict state facts agent
                                                                            (getf record :fan) location)
                                                "NOT STEPPABLE (inert)")
                                     :engine (not (null (equipment-step-offered-p state agent
                                                                                   (getf record :fan))))))))


(defun equipment-mount-failures (state facts agent fan gears)
  "Every failing MOUNT-FAN condition for AGENT holding FAN onto GEARS."
  (let ((location (keeper-fact-value 'has-location agent facts))
        (position (keeper-fact-value 'has-position gears (list-static-db)))
        (occupant (equipment-mounted-fan gears facts)))
    (append (unless (equipment-call 'object-manipulation-allowed state agent fan)
              (list "MANIPULATION NOT ALLOWED"))
            (unless (equipment-call 'reachable state position location) (list "OUT OF REACH"))
            (when occupant (list (format nil "OCCUPIED by ~(~A~)" occupant)))
            (unless (equipment-call 'within-agent-vertical-reach state agent
                                    (equipment-call 'blower-elevation state gears))
              (list "BEYOND VERTICAL REACH")))))


(defun equipment-mount-records (state facts)
  "Per agent holding a fan, per gears: MOUNTABLE or its failures."
  (loop for (agent object) in (equipment-agents-holding facts)
        when (member object (census-type-instances 'fan))
          append (loop for gears in (equipment-gears)
                       collect (list :agent agent :fan object :gears gears
                                     :failures (equipment-mount-failures state facts agent object gears)))))


(defun equipment-pickup-failures (state facts agent fan place)
  "Every failing PICKUP-FAN condition for AGENT and FAN at PLACE, by the action's branch:
   a fan with a location, or a wall-hung fan reached at its gears' position and height."
  (let* ((location (keeper-fact-value 'has-location agent facts))
         (hung (and (eq (first place) :mounted) (null (third place))))
         (target (if hung
                   (keeper-fact-value 'has-position (second place) (list-static-db))
                   (third place)))
         (height (if hung
                   (equipment-call 'blower-elevation state (second place))
                   (equipment-call 'base state fan))))
    (append (unless (equipment-call 'object-manipulation-allowed state agent fan)
              (list "MANIPULATION NOT ALLOWED"))
            (when (keeper-fact-value 'holding agent facts) (list "HANDS FULL"))
            (unless (equipment-call 'reachable state target location) (list "OUT OF REACH"))
            (unless (equipment-call 'within-agent-vertical-reach state agent height)
              (list "BEYOND VERTICAL REACH"))
            (unless (or hung (equipment-call 'cleartop state fan fan)) (list "TOP OCCUPIED")))))


(defun equipment-pickup-records (state facts fans)
  "Per fan not held and agent: PICKUP POSSIBLE or its failures."
  (loop for record in fans
        for place = (getf record :place)
        unless (member (first place) '(:held :absent))
          append (loop for agent in (keeper-sorted-set (census-type-instances 'agent))
                       collect (list :agent agent :fan (getf record :fan)
                                     :failures (equipment-pickup-failures state facts agent
                                                                          (getf record :fan) place)))))


(defun equipment-lifted-occupants (state facts destination)
  "Each non-fan occupant at DESTINATION not ON a support, with the floor drives aimed at
   DESTINATION that are active in its view."
  (loop for occupant in (keeper-sorted-set (census-type-instances 'support-occupant))
        when (and (not (member occupant (census-type-instances 'fan)))
                  (eq (keeper-fact-value 'has-location occupant facts) destination)
                  (null (keeper-fact-value 'on occupant facts)))
          collect (list occupant
                        (loop for drive in (keeper-sorted-set
                                            (append (census-type-instances 'floor-gears)
                                                    (census-type-instances 'floor-blower)))
                              when (and (eq (keeper-fact-value 'aimed-at drive (list-static-db)) destination)
                                        (equipment-call 'blower-active-for-object state occupant drive))
                                collect drive))))


(defun equipment-lift-records (state facts)
  "Per floor gears: destination, occupants held aloft there and the exits."
  (let ((static (list-static-db))
        (arcs (traversal-arc-facts)))
    (loop for gears in (keeper-sorted-set (census-type-instances 'floor-gears))
          collect (let ((destination (keeper-fact-value 'aimed-at gears static)))
                    (list :gears gears :destination destination
                          :source (keeper-fact-value 'has-position gears static)
                          :occupants (equipment-lifted-occupants state facts destination)
                          :exits (mapcar (lambda (arc) (mechanic-exit-text destination arc))
                                         (coupling-exit-arcs destination arcs)))))))


(defun equipment-engine-children (state)
  "The MOUNT-FAN (agent fan gears) and PICKUP-FAN (agent fan) instantiations the engine's
   successor generator accepts in STATE, as (name . arguments), with symmetry pruning off.
   The generator also records bound locations after the parameters; they are dropped."
  (let ((*algorithm* 'depth-first)
        (*symmetry-pruning* nil))
    (loop for child in (generate-children (make-node :state state :depth 0))
          for name = (problem-state.name child)
          when (member name '(mount-fan pickup-fan))
            collect (cons name (subseq (problem-state.instantiations child)
                                       0 (if (eq name 'mount-fan) 3 2))))))


(defun equipment-same-set-p (list1 list2)
  "Whether LIST1 and LIST2 hold the same elements under EQUAL."
  (and (null (set-difference list1 list2 :test #'equal))
       (null (set-difference list2 list1 :test #'equal))))


(defun equipment-agreement (state mounts pickups boardings)
  "Engine agreement for mounting, removal and boarding."
  (let ((children (equipment-engine-children state)))
    (list :mount (equipment-same-set-p
                  (loop for record in mounts
                        unless (getf record :failures)
                          collect (list 'mount-fan (getf record :agent) (getf record :fan) (getf record :gears)))
                  (remove 'pickup-fan children :key #'first))
          :pickup (equipment-same-set-p
                   (loop for record in pickups
                         unless (getf record :failures)
                           collect (list 'pickup-fan (getf record :agent) (getf record :fan)))
                   (remove 'mount-fan children :key #'first))
          :boarding (every (lambda (record)
                             (eq (getf record :engine) (string= (getf record :verdict) "BOARDABLE")))
                           boardings))))


(defun equipment-state-result (state)
  "The per-state part of an EQ result."
  (let* ((facts (equipment-state-facts state))
         (fans (equipment-fan-records state facts))
         (mounts (equipment-mount-records state facts))
         (pickups (equipment-pickup-records state facts fans))
         (boardings (equipment-boarding-records state facts fans)))
    (list :fans fans
          :drives (equipment-drive-records facts)
          :boarding boardings
          :mounting mounts
          :removal pickups
          :lifts (equipment-lift-records state facts)
          :agreement (equipment-agreement state mounts pickups boardings))))


(defun equipment-location-map (facts)
  "Each mobile object's location in FACTS, as (object . location)."
  (loop for object in (keeper-sorted-set (census-type-instances 'mobile-object))
        collect (cons object (keeper-fact-value 'has-location object facts))))


(defun equipment-transition (before after before-facts after-facts)
  "Differences from the BEFORE result to the AFTER result, and in mobile-object locations."
  (list :fans (loop for old in (getf before :fans)
                    for new = (find (getf old :fan) (getf after :fans) :key (lambda (r) (getf r :fan)))
                    unless (equal (getf old :place) (getf new :place))
                      collect (list (getf old :fan) (getf old :place) (getf new :place)))
        :drives (loop for old in (getf before :drives)
                      for new = (find (getf old :gears) (getf after :drives) :key (lambda (r) (getf r :gears)))
                      unless (eq (getf old :status) (getf new :status))
                        collect (list (getf old :gears) (getf old :status) (getf new :status)
                                      (cond ((eq (getf new :status) :effective-stream) "STREAM GAINED")
                                            ((eq (getf old :status) :effective-stream) "STREAM LOST")
                                            (t "NO STREAM EITHER WAY"))))
        :moved (loop for (object . old) in (equipment-location-map before-facts)
                     for new = (cdr (assoc object (equipment-location-map after-facts)))
                     unless (eq old new)
                       collect (list object old new))
        :lifts (loop for old in (getf before :lifts)
                     for new = (find (getf old :gears) (getf after :lifts) :key (lambda (r) (getf r :gears)))
                     for old-set = (mapcar #'first (getf old :occupants))
                     for new-set = (mapcar #'first (getf new :occupants))
                     collect (list (getf old :gears) (getf old :destination)
                                   (keeper-sorted-set (set-difference new-set old-set))
                                   (keeper-sorted-set (set-difference old-set new-set))))))


(defun equipment-scenario-result (scenario)
  "EQ: one settled state's removable equipment, and optionally its differences from an
   earlier settled state.  Returns a plist; the caller's states are never changed."
  (let ((reason (equipment-scenario-reason scenario)))
    (if reason
      (list :status :unresolved :reason reason)
      (let* ((state (getf scenario :state))
             (result (equipment-state-result state))
             (before (getf scenario :before)))
        (append (list :status :evaluated :provenance (getf scenario :provenance))
                result
                (when before
                  (list :before-provenance (getf scenario :before-provenance)
                        :transition (equipment-transition (equipment-state-result before) result
                                                          (equipment-state-facts before)
                                                          (equipment-state-facts state)))))))))


(defun equipment-failures-text (failures)
  "Failing conditions, or the positive verdict's absence of them."
  (format nil "~{~A~^; ~}" failures))


(defun equipment-status-text (status)
  "A mount status keyword as printed."
  (substitute #\Space #\- (symbol-name status)))


(defun report-equipment-state (result)
  "The per-state part of an EQ report."
  (format t "  fans:~%")
  (dolist (record (getf result :fans))
    (format t "    ~(~A~)  ~A~:[~;; BLOWING~]; ~:[NOT STEPPABLE~;STEPPABLE~]~%"
            (getf record :fan) (equipment-place-text (getf record :place))
            (getf record :blowing) (getf record :steppable)))
  (format t "  mounts:~%")
  (dolist (record (getf result :drives))
    (format t "    ~(~A~)  ~(~A~) gears; fan ~(~A~); ~:[stopped~;turning~]~@[; jammed by ~(~{~A~^, ~}~)~]: ~A~%"
            (getf record :gears) (getf record :kind) (or (getf record :fan) "VACANT")
            (getf record :turning) (getf record :jammers)
            (equipment-status-text (getf record :status))))
  (format t "  boarding:~:[ none~;~]~%" (getf result :boarding))
  (dolist (record (getf result :boarding))
    (format t "    ~(~A~) onto ~(~A~): ~A; engine step ~:[not offered~;offered~]~%"
            (getf record :agent) (getf record :fan) (getf record :verdict) (getf record :engine)))
  (format t "  mounting:~:[ no agent holds a fan~;~]~%" (getf result :mounting))
  (dolist (record (getf result :mounting))
    (format t "    ~(~A~) ~(~A~) on ~(~A~): ~:[MOUNTABLE~;~:*~A~]~%"
            (getf record :agent) (getf record :fan) (getf record :gears)
            (when (getf record :failures) (equipment-failures-text (getf record :failures)))))
  (format t "  removal:~:[ no fan to pick up~;~]~%" (getf result :removal))
  (dolist (record (getf result :removal))
    (format t "    ~(~A~) picks up ~(~A~): ~:[PICKUP POSSIBLE~;~:*~A~]~%"
            (getf record :agent) (getf record :fan)
            (when (getf record :failures) (equipment-failures-text (getf record :failures)))))
  (format t "  lifts:~%")
  (dolist (record (getf result :lifts))
    (format t "    ~(~A~) -> ~(~A~): ~:[no occupant aloft~;~:*~{~A~^; ~}~]~%"
            (getf record :gears) (getf record :destination)
            (mapcar (lambda (entry)
                      (format nil "~(~A~) SUSTAINED by ~(~{~A~^, ~}~); it drops to ~(~A~) if they stop or lose their fan, unless it first stands ON a support there or leaves"
                              (first entry) (second entry) (getf record :source)))
                    (getf record :occupants)))
    (format t "      exits from ~(~A~) (kind predicates not evaluated): ~{~A~^, ~}~%"
            (getf record :destination) (getf record :exits)))
  (let ((agreement (getf result :agreement)))
    (format t "  engine agreement: mounting ~:[DISAGREES~;agrees~]; removal ~:[DISAGREES~;agrees~]; boarding ~:[DISAGREES~;agrees~]~%"
            (getf agreement :mount) (getf agreement :pickup) (getf agreement :boarding))))


(defun report-equipment-transition (transition)
  "The differences from the before state."
  (format t "  transition from the before state (two engine states compared; no action named or implied):~%")
  (dolist (entry (getf transition :fans))
    (format t "    fan ~(~A~): ~A -> ~A~%" (first entry)
            (equipment-place-text (second entry)) (equipment-place-text (third entry))))
  (dolist (entry (getf transition :drives))
    (format t "    mount ~(~A~): ~A -> ~A: ~A~%" (first entry)
            (equipment-status-text (second entry)) (equipment-status-text (third entry)) (fourth entry)))
  (dolist (entry (getf transition :moved))
    (format t "    moved ~(~A~): ~(~A~) -> ~(~A~)~%" (first entry) (or (second entry) "none") (or (third entry) "none")))
  (dolist (entry (getf transition :lifts))
    (when (or (third entry) (fourth entry))
      (format t "    lift ~(~A~) -> ~(~A~):~@[ aloft gained ~(~{~A~^, ~}~)~]~@[ aloft lost ~(~{~A~^, ~}~)~]~%"
              (first entry) (second entry) (third entry) (fourth entry)))))


(defun report-equipment-scenario (scenario)
  "EQ report: one settled state's removable equipment, and optionally its differences from
   an earlier settled state.  Not reachability, a transport plan or stability."
  (let ((result (equipment-scenario-result scenario))
        (*print-pretty* nil))
    (format t "~%EQ  REMOVABLE EQUIPMENT IN ONE SUPPLIED STATE  [supplied state]~%")
    (format t "~A~%" (make-string 62 :initial-element #\-))
    (if (eq (getf result :status) :unresolved)
      (format t "  UNRESOLVED: ~A~%" (getf result :reason))
      (progn
        (format t "  provenance: ~A~%" (getf result :provenance))
        (report-equipment-state result)
        (when (getf result :transition)
          (format t "  before provenance: ~A~%" (getf result :before-provenance))
          (report-equipment-transition (getf result :transition)))
        (format t "  For this state (or pair) only: not reachability, not a transport plan, and not stability beyond the fixed-point check.~%")))
    (values)))


;;;; NH -- NECESSITY HINTS (T21, I6) ;;;;
;;;
;;; Specification: doc/constraint-led-solving/Extractor-Specifications.md section 10.  Each static
;;; limit another section already states, restated as a candidate plan element for the
;;; Briefing: H1 body budget (T6), H2 keepers left behind (S4), H3 beam-held devices and
;;; candidate beams (S1, RC), H4 controllers off the goal route (S4, S3), H5 lift landings
;;; (CC, MC), H6 devices active at the start (S1), H7 placement limits (S5).  NH derives no
;;; new constraint, and it prints last because it reads every section above it.
;;;
;;; SUBSTRATE VOCABULARY (C3).  The relations HAS-LOCATION, HAS-POSITION and ON, the control
;;; modes NORMAL and INVERTED, S5's GROUND support marker, the types PRESSURE-PLATE, RECEIVER
;;; and GATE, and the helpers of the sections it reads are named; each is a tech/ interface
;;; or this file's own.  No problem object name appears; every instance comes from the
;;; staged databases.


(defparameter *hint-family-titles*
  '("body budget" "keepers left behind" "beam-held devices" "controllers off the goal route"
    "lift landings" "active at the start" "placement limits")
  "The seven NH family titles, in print order, section 10.3 of the specification.")


(defun hint-goal-routes (names spine devices)
  "One (object from to devices) entry per positive ground HAS-LOCATION goal conjunct whose
   start and goal regions S3 locates and S4's relaxed graph joins.  DEVICES are S4's
   GRAPH-REQUIRED candidates between the two regions: the goal route."
  (let ((initial (database *start-state*))
        (routes nil))
    (dolist (destination (keeper-goal-destinations (get 'goal-fn :form)) (nreverse routes))
      (let* ((object (second destination))
             (from (gethash (keeper-fact-value 'has-location object initial) names))
             (to (gethash (third destination) names)))
        (when (and from to)
          (multiple-value-bind (required connected)
              (keeper-required-devices from to spine devices)
            (when connected
              (push (list object from to required) routes))))))))


(defun hint-route-context (controls)
  "S3/S4 graph context, retaining S4 failure separately from a valid empty spine."
  (let* ((arcs (traversal-arc-facts))
         (blocks (region-blocks (traversal-endpoints) (contract-free-arcs arcs)))
         (names (region-name-table blocks))
         (devices (mapcar #'third controls)))
    (multiple-value-bind (spine reason)
        (keeper-spine (quotient-arc-rows arcs names) (mapcar #'first blocks) devices)
      (list :arcs arcs :blocks blocks :names names :devices devices :spine spine
            :spine-reason reason :routes (unless reason (hint-goal-routes names spine devices))))))


(defun hint-route-devices (context)
  "Every device on some goal route of CONTEXT."
  (remove-duplicates (loop for route in (getf context :routes)
                           append (copy-list (fourth route)))))


(defun hint-budget-rows (controls occupancy actor)
  "H1.  OCCUPANCY is T6's (live all) ON pool count; ACTOR is the goal actor when T6's AM2
   holds, else NIL.  Empty when T6's own preconditions fail."
  (let ((device-costs (budget-arithmetic-body-cost-devices controls))
        (pressure-plates (census-type-instances 'pressure-plate))
        (grade "grade 1 -> 2; T6 S2")
        (hints nil))
    (when (and device-costs occupancy (budget-arithmetic-disjoint-supports-p device-costs))
      (let* ((total (budget-arithmetic-total-cost (budget-arithmetic-gate-costs device-costs)))
             (live (first occupancy))
             (all (second occupancy))
             (ghosts (- all live))
             (outside (- live (if actor 1 0)))
             (inside (- all (if actor 1 0)))
             (less (if actor (format nil ", less the goal actor ~(~A~)" actor) ""))
             (besides (if actor (format nil " besides the goal actor ~(~A~)" actor) "")))
        (when (> total outside)
          (push (list :family 1 :label "NECESSARY" :grade grade :source (list :outside total)
                      :limit (format nil "plate demand ~D (~D ~A) exceeds the ~D bod~:@P ~
                                          outside a cycle (~D live~A)"
                                     total (length device-costs)
                                     (format nil "~A~P"
                                             (if (some (lambda (entry) (cdr (first entry))) device-costs)
                                               "demand" "device")
                                             (length device-costs))
                                     outside live less)
                      :hints (list (if (plusp ghosts)
                                     (format nil "a state holding more than ~D plate~:P needs ~
                                                  ghost bodies (~D in the ON pool)"
                                             outside ghosts)
                                     (format nil "at most ~D held plate~:P at once: the other ~
                                                  demands are met in sequence"
                                             outside))))
                hints))
        (when (and (plusp ghosts) (> total inside))
          (push (list :family 1 :label "NECESSARY" :grade grade :source (list :inside total)
                      :limit (format nil "plate demand ~D exceeds the ~D bod~:@P inside a cycle ~
                                          (~D in the ON pool~A)"
                                     total inside all less)
                      :hints (list (format nil "at least ~D plate~:P ~:[is~;are~] free at every ~
                                                moment: the plate demands are met in sequence, ~
                                                never all at once"
                                           (- total inside) (> (- total inside) 1))))
                hints))
        (dolist (fact controls)
          (let ((clauses (keeper-pressure-clauses fact pressure-plates)))
            (when (and clauses (notany #'null clauses))
              (let* ((plates (first (stable-sort (copy-list clauses) #'< :key #'length)))
                     (demand (length plates))
                     (device (third fact)))
                (when (>= demand outside)
                  (push (list :family 1 :label "NECESSARY" :grade grade
                              :source (list :device device)
                              :limit (format nil "~(~A~) needs ~D plate~:P at once (~(~{~A~^, ~}~)); ~
                                                  ~D bod~:@P outside a cycle~A"
                                             device demand plates outside besides)
                              :hints (list (cond ((and (= demand outside) (plusp ghosts))
                                                  (format nil "opening ~(~A~) outside a cycle takes ~
                                                               every one of them; any other plate ~
                                                               held at the same time needs a ghost"
                                                          device))
                                                 ((= demand outside)
                                                  (format nil "opening ~(~A~) takes every one of ~
                                                               them; no other plate can be held at ~
                                                               the same time"
                                                          device))
                                                 ((<= demand inside)
                                                  (format nil "opening ~(~A~) needs at least ~D ~
                                                               ghost bod~:@P"
                                                          device (- demand outside)))
                                                 (t (format nil "~(~A~) cannot be opened: its ~
                                                                 demand exceeds every body available"
                                                            device)))))
                        hints))))))))
    (nreverse hints)))


(defun hint-keeper-row (fact source destination row context route-devices)
  "One H2 hint for crossing FACT's device from region SOURCE to region DESTINATION over
   spine ROW, or NIL when none of its mandatory plates is APPROACH-ONLY that way.  A plate
   APPROACH-ONLY one way is DEPARTURE-ONLY the other way by S4's two closures, so the
   return note needs only a bidirectional row. No obligation is inferred when another
   clause of this row avoids the device."
  (when (keeper-row-avoids-p row (third fact))
    (return-from hint-keeper-row nil))
  (let* ((device (third fact))
         (spine (getf context :spine))
         (names (getf context :names))
         (blocks (getf context :blocks))
         (static (list-static-db))
         (approach (keeper-reachable source spine device))
         (departure (keeper-reachable destination spine device))
         (kept (remove-if-not
                 (lambda (plate)
                   (eq :approach-only
                       (keeper-plate-side (gethash (keeper-fact-value 'has-position plate static) names)
                                          approach departure)))
                 (keeper-mandatory-plates
                   (keeper-pressure-clauses fact (census-type-instances 'pressure-plate))))))
    (when kept
      (list :family 2 :label "NECESSARY" :grade "grade 2, graph candidate; S4 S1"
            :source (list device source destination)
            :limit (format nil "crossing ~(~A~) from ~A {~(~{~A~^ ~}~)} to ~A {~(~{~A~^ ~}~)} needs ~
                                ~(~{~A~^, ~}~) held; ~:[it lies~;they lie~] on the ~A side only ~
                                (approach-only)"
                           device source (second (find source blocks :key #'first :test #'string=))
                           destination (second (find destination blocks :key #'first :test #'string=))
                           kept (cdr kept) source)
            :hints (list (format nil "before crossing ~(~A~) into ~A, leave ~D bod~:@P other than ~
                                      the crosser on ~(~{~A~^, ~}~) (~A)"
                                 device destination (length kept) kept
                                 (if (member device route-devices) "goal route" "off the goal route")))
            :notes (when (eq (fifth row) :both)
                     (list (format nil "coming back from ~A to ~A through ~(~A~) needs the same ~
                                        plates held: the bodies stay until the crosser returns"
                                   destination source device)))))))


(defun hint-keeper-rows (controls context)
  "H2.  One hint per S4 spine crossing direction with an APPROACH-ONLY mandatory plate, in
   S4's order: devices by name, spine rows in order, forward before reverse."
  (let ((route-devices (hint-route-devices context))
        (hints nil))
    (dolist (fact controls (nreverse hints))
      (dolist (row (remove-if-not (lambda (row) (member (third fact) (quotient-row-doors row)))
                                  (getf context :spine)))
        (let ((forward (hint-keeper-row fact (first row) (second row) row context route-devices))
              (backward (when (eq (fifth row) :both)
                          (hint-keeper-row fact (second row) (first row) row context route-devices))))
          (when forward
            (push forward hints))
          (when backward
            (push backward hints)))))))


(defun hint-relay-chains (receivers controls)
  "RC's evaluated chains, one (receiver . chains) entry per RECEIVER, computed as
   REPORT-RELAY-CHAIN-TABLE computes them."
  (let ((exclusions (relay-chain-exclusion-pairs controls))
        (pressure-plates (census-type-instances 'pressure-plate))
        (stations (relay-chain-stations *start-state*)))
    (multiple-value-bind (open-state closed-states)
        (relay-chain-gate-states (census-type-instances 'gate))
      (let ((endpoint-links (relay-chain-endpoint-links stations open-state closed-states))
            (station-links (relay-chain-station-links stations open-state closed-states)))
        (loop for receiver in receivers
              collect (cons receiver
                            (mapcar (lambda (hops)
                                      (relay-chain-evaluate hops exclusions
                                                            (relay-chain-receiver-devices receiver controls)
                                                            controls pressure-plates))
                                    (relay-chain-enumerate receiver endpoint-links station-links))))))))


(defun hint-chain-end (chain)
  "The location of CHAIN's last connector, the station feeding the receiver."
  (first (first (car (last (getf chain :hops))))))


(defun hint-chain-text (chain)
  "CHAIN's path as RC prints it, then its risers, bodies, off-plate count and self-kept plates."
  (format nil "~A~{ -> ~A~}  risers ~(~{~A~^, ~}~); bodies ~D, off plates ~D~
               ~@[, self-kept ~(~{~A~^, ~}~)~]"
          (relay-chain-node-text (first (first (getf chain :hops))))
          (mapcar (lambda (hop) (relay-chain-node-text (second hop))) (getf chain :hops))
          (getf chain :risers) (getf chain :bodies) (getf chain :off-plate)
          (getf chain :self-kept)))


(defun hint-beam-necessity (fact receiver chains controls)
  "S1's necessary receiver condition, with RC bounds conditional on geometric scope."
  (let* ((device (third fact))
         (pressure-plates (census-type-instances 'pressure-plate))
         (usable (remove-if-not (lambda (chain) (member (getf chain :class) '(:bootstrap :latch)))
                                chains))
         (bootstrap (remove-if-not (lambda (chain) (eq (getf chain :class) :bootstrap)) usable))
         (common (when usable
                   (keeper-sorted-set
                     (set-difference (reduce #'intersection
                                             (mapcar (lambda (chain) (getf chain :gates)) usable))
                                     (relay-chain-receiver-devices receiver controls)))))
         (gate-plates (loop for gate in common
                            collect (let ((gate-fact (find gate controls :key #'third)))
                                      (when gate-fact
                                        (keeper-mandatory-plates
                                          (keeper-pressure-clauses gate-fact pressure-plates))))))
         (plates (keeper-sorted-set (loop for entry in gate-plates append (copy-list entry))))
         (least (when bootstrap
                  (reduce #'min bootstrap :key (lambda (chain) (getf chain :off-plate)))))
         (parts (append (when plates
                          (list (if (cdr plates)
                                  (format nil "bodies stand on ~(~{~A~^, ~}~)" plates)
                                  (format nil "a body stands on ~(~A~)" (first plates)))))
                        (when least
                          (list (format nil "at least ~D bod~:@P ~:[is~;are~] off plates for the beam"
                                        least (/= least 1)))))))
    (list :family 3 :label "NECESSARY" :grade "grade 2; S1 RC" :source (list device)
          :limit (format nil "~(~A~) == ~(~A~) holds only while ~(~A~) is ~(~A~); within RC's enumerated physical geometric candidates, every chain ~
                              to ~(~A~) (~D bootstrap, ~D latch) needs ~:[no common gate~;~:*~{~A~^, ~}~]"
                         device (control-boolean-form (second fact) (fourth fact))
                         receiver (control-status-relation receiver) receiver
                         (length bootstrap) (- (length usable) (length bootstrap))
                         (loop for gate in common
                               for entry in gate-plates
                               collect (format nil "~(~A~) open~@[ (~(~{~A~^, ~}~))~]" gate entry)))
          :hints (list (if parts
                         (format nil "within those geometric candidates for ~(~A~), ~{~A~^, and ~}" device parts)
                         (format nil "while ~(~A~) must hold, RC implies nothing further" device)))
          :notes (list "S1's receiver condition is necessary; RC gate/body bounds are conditional on its physical geometric enumeration, not all recording-view beams"
                       "Recording sightlines, body/view assignment, forced transport and occupancy stability UNRESOLVED; geometric chains are not validated realizations"))))


(defun hint-beam-candidates (receiver chains names)
  "H3's CANDIDATE hints for RECEIVER: per receiver-end location, in name order, the bootstrap
   chains ending there with the least off-plate bodies, in path-text order."
  (let* ((bootstrap (remove-if-not (lambda (chain) (eq (getf chain :class) :bootstrap)) chains))
         (locations (sort (remove-duplicates (mapcar #'hint-chain-end bootstrap))
                          #'string< :key #'symbol-name))
         (hints nil))
    (dolist (location locations (nreverse hints))
      (let* ((group (remove-if-not (lambda (chain) (eq location (hint-chain-end chain))) bootstrap))
             (least (reduce #'min group :key (lambda (chain) (getf chain :off-plate)))))
        (push (list :family 3 :label "CANDIDATE" :grade "grade 2; RC" :source (list receiver location)
                    :limit (format nil "physical geometric candidates to ~(~A~) with last connector at ~(~A~) (~A): ~
                                        least off-plate bodies ~D"
                                   receiver location (gethash location names) least)
                    :hints (sort (loop for chain in group
                                       when (= least (getf chain :off-plate))
                                         collect (hint-chain-text chain))
                                 #'string<)
                    :notes (remove-duplicates
                             (loop for chain in group
                                   when (= least (getf chain :off-plate))
                                     append (relay-chain-qualification-notes chain))
                             :test #'string=))
              hints)))))


(defun hint-fixed-beam-rows ()
  "H3 fixed receiver corridors are conditional candidates, not required relay chains."
  (loop for row in (fixed-beam-records)
        when (member (getf row :sink) (census-type-instances 'receiver))
          collect (list :family 3 :label "CANDIDATE" :grade "grade 1; fixed corridor, staged physical view"
                        :source (list :fixed (getf row :source) (getf row :sink))
                        :limit (format nil "fixed ~(~A~) -> ~(~A~): gate candidates ~(~S~); occupancy locations ~(~S~)"
                                       (getf row :source) (getf row :sink) (getf row :gates) (getf row :locations))
                        :hints (list "check the fixed corridor's recorded barriers and authored obstacles in RC before assigning connector roles")
                        :notes (list "Gates block only at the beam's height; location occupants must span it. Matching chromas, BEAM-CUT and upstream lighting also matter; other routes may activate the receiver. No reachability, recording-view or stability claim."))))


(defun hint-beam-rows (controls names)
  "H3.  For each receiver of S1's device-mediated tier: a NECESSARY hint per normal device
   whose every clause names it, then its CANDIDATE beams."
  (let* ((devices (mapcar #'third controls))
         (receivers (remove-if-not
                      (lambda (primitive)
                        (and (member primitive (census-type-instances 'receiver))
                             (eq :device-mediated (control-primitive-tier primitive devices))))
                      (control-primitives controls)))
         (hints nil))
    (dolist (entry (when receivers (hint-relay-chains receivers controls))
                   (append (nreverse hints) (hint-fixed-beam-rows)))
      (dolist (fact controls)
        (when (and (eq (fourth fact) 'normal)
                   (second fact)
                   (every (lambda (clause) (member (car entry) clause)) (second fact)))
          (push (hint-beam-necessity fact (car entry) (cdr entry) controls) hints)))
      (dolist (hint (hint-beam-candidates (car entry) (cdr entry) names))
        (push hint hints)))))


(defun hint-device-text (device controls)
  "DEVICE with its mandatory plates in parentheses, when it has any."
  (let ((fact (find device controls :key #'third)))
    (format nil "~(~A~)~@[ (~(~{~A~^, ~}~))~]"
            device
            (when fact
              (keeper-mandatory-plates
                (keeper-pressure-clauses fact (census-type-instances 'pressure-plate)))))))


(defun hint-controller-sites (controller static names)
  "Where CONTROLLER is operated, each as (location region barriers): a switch's S4 reach
   sites, or a latch's own position."
  (if (eq (keeper-controller-kind controller) :switch)
    (keeper-reach-sites controller static names)
    (let ((position (keeper-fact-value 'has-position controller static)))
      (when (gethash position names)
        (list (list position (gethash position names) nil))))))


(defun hint-site-extras (site route context)
  "The devices reaching SITE needs beyond ROUTE's own: S4's relaxed required devices from
   the route's start region to the site's region, plus the site's barriers.  :UNREACHABLE
   when the relaxed graph does not join them."
  (multiple-value-bind (required connected)
      (keeper-required-devices (second route) (second site)
                               (getf context :spine) (getf context :devices))
    (if connected
      (keeper-sorted-set (set-difference (union (copy-list required) (copy-list (third site)))
                                         (fourth route)))
      :unreachable)))


(defun hint-controller-row (controller route controls context)
  "H4's hint for CONTROLLER of a device on ROUTE, or NIL when some site of it needs no
   device beyond the route's own."
  (let* ((sites (hint-controller-sites controller (list-static-db) (getf context :names)))
         (extras (mapcar (lambda (site) (hint-site-extras site route context)) sites))
         (driven (remove-if-not (lambda (fact) (member (third fact) (fourth route)))
                                (coupling-driven-facts controller controls)))
         (common (unless (or (null extras) (member :unreachable extras))
                   (keeper-sorted-set (reduce #'intersection extras)))))
    (when (and sites (every #'identity extras))
      (list :family 4 :label (if common "NECESSARY" "CANDIDATE")
            :grade "grade 2, graph candidate; S4 S3 S1" :source (list controller)
            :limit (format nil "~(~A~) drives ~(~{~A~^, ~}~) (goal route); it is operated ~
                                ~:[from~;only from~] ~{~A~^, ~}"
                           controller (mapcar #'third driven) (null (cdr sites))
                           (mapcar (lambda (site) (format nil "~(~A~) (~A)" (first site) (second site)))
                                   sites))
            :hints (if common
                     (list (format nil "an excursion off the goal route: reaching ~A from ~A also ~
                                        needs ~{~A~^, ~}"
                                   (if (cdr sites) "any of its sites" (second (first sites)))
                                   (second route)
                                   (mapcar (lambda (device) (hint-device-text device controls))
                                           common)))
                     (loop for site in sites
                           for extra in extras
                           collect (if (eq extra :unreachable)
                                     (format nil "from ~(~A~) (~A): not joined to ~A in the relaxed graph"
                                             (first site) (second site) (second route))
                                     (format nil "from ~(~A~) (~A): reaching it from ~A also needs ~
                                                  ~{~A~^, ~}"
                                             (first site) (second site) (second route)
                                             (mapcar (lambda (device) (hint-device-text device controls))
                                                     extra)))))
            :notes (loop for (fact . rest) on driven
                         append (loop for other in rest
                                      when (string= "EXCLUSION" (coupling-pair-relation fact other))
                                        collect (format nil "{~(~A~), ~(~A~)} EXCLUSION on ~(~A~): never ~
                                                             active together; the route crosses them ~
                                                             under different settings of ~(~A~)"
                                                        (third fact) (third other) controller controller)))))))


(defun hint-controller-rows (controls context)
  "H4.  For each goal route, each switch or latch controller of a device on it, by name."
  (let ((hints nil))
    (dolist (route (getf context :routes) (nreverse hints))
      (dolist (controller (control-primitives
                            (remove-if-not (lambda (fact) (member (third fact) (fourth route)))
                                           controls)))
        (when (member (keeper-controller-kind controller) '(:switch :latch))
          (let ((hint (hint-controller-row controller route controls context)))
            (when hint
              (push hint hints))))))))


(defun hint-exit-text (location arcs)
  "LOCATION's exits among ARCS, grouped by kind as MC prints them, groups joined by '; '."
  (format nil "~{~A~^; ~}"
          (loop for kind in (sort (remove-duplicates (mapcar #'second arcs)) #'string<
                                  :key #'symbol-name)
                collect (let ((texts (sort (loop for arc in arcs
                                                 when (eq (second arc) kind)
                                                   collect (mechanic-exit-text location arc))
                                           #'string<)))
                          (format nil "~(~A~) (~D): ~{~A~^, ~}" kind (length texts) texts)))))


(defun hint-lift-rows (controls arcs)
  "H5.  One CANDIDATE hint per CC G15 lift-barrier row that is not EQUIVALENCE."
  (let* ((static (list-static-db))
         (table (coupling-role-table controls arcs nil))
         (supports (remove 'ground (mapcar #'first (height-lattice-placement-supports *start-state*))))
         (hints nil))
    (dolist (entry (coupling-lift-barriers (coupling-fan-out controls) arcs table) (nreverse hints))
      (destructuring-bind (primitive lift destination barrier exits) entry
        (let ((relation (coupling-pair-relation lift barrier))
              (others (remove-if (lambda (arc)
                                   (some (lambda (clause) (member (third barrier) clause)) (fourth arc)))
                                 (coupling-exit-arcs destination arcs))))
          (unless (string= relation "EQUIVALENCE")
            (push (list :family 5 :label "CANDIDATE" :grade "grade 1; CC MC S5"
                        :source (list primitive (third lift) (third barrier))
                        :limit (format nil "~(~A~) drives lift ~(~A~) (~(~A~) -> ~(~A~)) and ~(~A~) on an ~
                                            exit from ~(~A~): ~:[their relation depends on the other ~
                                            literals (G15 CHECK)~;~(~A~) is active only while ~(~A~) ~
                                            is inactive (G15 FLAG)~]"
                                       primitive (third lift)
                                       (keeper-fact-value 'has-position (third lift) static)
                                       destination (third barrier) destination
                                       (string= relation "EXCLUSION") (third barrier) (third lift))
                        :hints (list (format nil "keep the lift through the toggle: a support at ~(~A~) ~
                                                  (~(~{~A~^, ~}~)), then leave through ~(~A~): ~A"
                                             destination supports (third barrier)
                                             (hint-exit-text destination exits))
                                     (if others
                                       (format nil "or leave ~(~A~) while ~(~A~) runs, by an exit not ~
                                                    through ~(~A~): ~A"
                                               destination (third lift) (third barrier)
                                               (hint-exit-text destination others))
                                       (format nil "no exit from ~(~A~) avoids ~(~A~): the lift is kept ~
                                                    only by a support there"
                                               destination (third barrier))))
                        :notes (list "each kind's own rule is not evaluated: a listed exit is a candidate, not a legal move"))
                  hints)))))))


(defun hint-primitive-active-p (primitive database)
  "Whether PRIMITIVE is in its S1 status relation in DATABASE, a proposition list."
  (let ((relation (control-status-relation primitive)))
    (and relation (member (list relation primitive) database :test #'equal) t)))


(defun hint-aggregate-holds-p (fact database)
  "Whether FACT's control aggregate holds in DATABASE: some clause with every primitive
   active, negated for an inverted device."
  (let ((normal (some (lambda (clause)
                        (every (lambda (primitive) (hint-primitive-active-p primitive database))
                               clause))
                      (second fact))))
    (if (eq (fourth fact) 'inverted)
      (not normal)
      (and normal t))))


(defun hint-start-row (fact database route-devices)
  "H6's hint for FACT's device, whose aggregate holds in the start DATABASE."
  (let* ((device (third fact))
         (primitives (control-primitives (list fact)))
         (holders (loop for primitive in primitives
                        when (and (member primitive (census-type-instances 'pressure-plate))
                                  (hint-primitive-active-p primitive database))
                          collect (cons primitive
                                        (keeper-sorted-set
                                          (loop for proposition in database
                                                when (and (eq (first proposition) 'on)
                                                          (eq (third proposition) primitive))
                                                  collect (second proposition))))))
         (marker (if (member device route-devices) "goal route" "off the goal route")))
    (list :family 6 :label "CANDIDATE" :grade "grade 1; S1, start state" :source (list device)
          :limit (format nil "~(~A~) == ~(~A~) holds at the start: ~{~A~^, ~}"
                         device (control-boolean-form (second fact) (fourth fact))
                         (mapcar (lambda (primitive)
                                   (format nil "~(~A~) is ~:[not ~;~]~(~A~)~@[, held by ~(~{~A~^, ~}~)~]"
                                           primitive (hint-primitive-active-p primitive database)
                                           (control-status-relation primitive)
                                           (cdr (assoc primitive holders))))
                                 primitives))
          :hints (list (if holders
                         (format nil "~(~A~) is free while ~{~A~^ and ~} (~A)"
                                 device
                                 (mapcar (lambda (entry)
                                           (format nil "~(~{~A~^, ~}~) ~:[stays~;stay~] on ~(~A~)"
                                                   (cdr entry) (cddr entry) (car entry)))
                                         holders)
                                 marker)
                         (format nil "~(~A~) is free until ~(~{~A~^, ~}~) ~:[changes~;change~] (~A)"
                                 device primitives (cdr primitives) marker))))))


(defun hint-start-rows (controls context)
  "H6.  One hint per controlled device, by name, whose aggregate holds in the start state."
  (let ((database (database *start-state*))
        (route-devices (hint-route-devices context)))
    (loop for fact in controls
          when (hint-aggregate-holds-p fact database)
            collect (hint-start-row fact database route-devices))))


(defun hint-placement-rows (state)
  "H7.  One CANDIDATE hint per support top S5 lists as unreachable from the ground."
  (let* ((unreachable (height-lattice-ground-unreachable-supports state))
         (levels (height-lattice-location-levels state))
         (hints nil))
    (dolist (top (sort (remove-duplicates (mapcar #'second unreachable)) #'<) (nreverse hints))
      (let ((supports (loop for support in unreachable
                            when (= (second support) top)
                              collect (first support)))
            (raised (sort (remove-if-not (lambda (level)
                                           (and (plusp (second level))
                                                (<= (- top (second level)) *vertical-reach-limit*)))
                                         levels)
                          #'string< :key (lambda (level) (symbol-name (first level))))))
        (push (list :family 7 :label "CANDIDATE" :grade "grade 2; S5" :source (list top)
                    :limit (format nil "~(~{~A~^, ~}~) at top ~A take~:[s~;~] no placement from a ~
                                        grounded agent (placement reach limit ~A)"
                                   supports top (cdr supports) *vertical-reach-limit*)
                    :hints (list (format nil "place onto ~:[it~;them~] from a raised location: ~
                                              ~:[none~;~:*~{~A~^, ~}~]"
                                         (cdr supports)
                                         (mapcar (lambda (level)
                                                   (format nil "~(~A~) (level ~A)" (first level) (second level)))
                                                 raised)))
                    :notes (list "placement reach from that location to the support, and a base raised by standing on a support, are not evaluated"))
              hints)))))


(defun necessity-hint-families (controls context)
  "Every NH family's hints, as seven hint lists in family order.  A hint is a plist with
   :FAMILY, :LABEL, :GRADE, :SOURCE (the source row it restates), :LIMIT, :HINTS and :NOTES."
  (let ((actor (when (budget-arithmetic-goal-actor-leaves-pool-p)
                 (second (budget-arithmetic-find-relation-call (get 'goal-fn :form) 'has-location)))))
    (list (hint-budget-rows controls (budget-arithmetic-segment-occupancy) actor)
          (hint-keeper-rows controls context)
          (hint-beam-rows controls (getf context :names))
          (hint-controller-rows controls context)
          (hint-lift-rows controls (getf context :arcs))
          (hint-start-rows controls context)
          (hint-placement-rows *start-state*))))


(defun report-hint (hint index)
  "One hint: its number, label and grade, then its limit, hint and note lines."
  (format t "    H~D.~D  ~A  [~A]~%" (getf hint :family) index (getf hint :label) (getf hint :grade))
  (format t "      limit  ~A~%" (getf hint :limit))
  (dolist (text (getf hint :hints))
    (format t "      hint   ~A~%" text))
  (dolist (text (getf hint :notes))
    (format t "      note   ~A~%" text)))


(defun report-necessity-hint-families (families context)
  "Report valid empty families as NONE; H2/H4 failures as UNAVAILABLE with their S4 reason."
  (let ((all (apply #'append families)))
    (format t "~%  hints (~D): ~D NECESSARY, ~D CANDIDATE~%"
            (length all)
            (count "NECESSARY" all :key (lambda (hint) (getf hint :label)) :test #'string=)
            (count "CANDIDATE" all :key (lambda (hint) (getf hint :label)) :test #'string=))
    (loop for hints in families
          for title in *hint-family-titles*
          for family from 1
          do (format t "~%  H~D ~A (~D)~%" family title (length hints))
             (cond ((and (member family '(2 4)) (getf context :spine-reason))
                    (format t "    UNAVAILABLE: S4 ~S~%" (getf context :spine-reason)))
                   ((null hints) (format t "    none~%")))
             (loop for hint in hints
                   for index from 1
                   do (report-hint hint index)))))


(defun report-necessity-hints (&optional scenario)
  "NH, grade per hint.  Every static limit the sections above state, restated as a
   candidate plan element for the Briefing."
  (let* ((*print-pretty* nil)
         (controls (control-facts)))
    (format t "~2%NH  NECESSITY HINTS  [grade per hint]~%")
    (report-relay-view-scenario scenario)
    (format t "~A~%" (make-string 62 :initial-element #\-))
    (format t "  READING: a hint restates static limits as a candidate plan element for the ~
               Briefing (Problem-Solving Guide, Phase 1 step 3).  NECESSARY: every plan meets ~
               it, under the grade shown.  CANDIDATE: one way to meet a limit; others may ~
               exist.  A hint is not a plan, and a graph candidate is not a proof.~%")
    (let ((context (hint-route-context controls)))
      (report-necessity-hint-families (necessity-hint-families controls context) context))
    (values)))


;;;; SD -- SERVICES AND SETUP DEPENDENCIES (T43) ;;;;
;;;
;;; Specification: doc/constraint-led-solving/Extractor-Specifications.md section 8.13.  A service
;;; is a condition a crossing or the goal needs: a gate open, a drive named by a traversal
;;; clause not blowing, a receiver active.  Each has providers (S1 CONTROL options, MC jam
;;; sites, gears left without a fan, RC chains and fixed corridors to a receiver), each a
;;; conjunction of premises.  A monotone AND-OR closure then classifies every option, naming
;;; a provider whose installation needs the very service it will provide.  Transit, return
;;; and final requirements are read from S3's rows and the goal.  Grade 2, physical view.
;;;
;;; SUBSTRATE VOCABULARY (C3).  The types GATE, JAMMER, RECEIVER, PRESSURE-PLATE, CONNECTOR,
;;; FAN, AGENT and MOBILE-OBJECT and the gears and fixed-blower leaf types; the relations
;;; OPEN, TURNING, JAMMING, PAIRED, MOUNTED-ON, HAS-LOCATION and JAM-DISALLOWED>; the queries
;;; OBSTACLE-CLEAR, ALL-CLEAR, BLOWER-PRESENT, REACHABLE and MOBILITY-RESULTS-IN-STATE; and the
;;; helpers of the sections read.  No problem object is named.


(defparameter *service-drive-types*
  '(floor-gears wall-gears angled-gears floor-blower wall-blower angled-blower)
  "The drive leaf types OBSTACLE-CLEAR defers to STREAM-OBSTACLE-CLEAR (-passability.lisp):
   passable while not active.  Active is present and TURNING (UPDATE-BLOWER-STATUS!).")


(defparameter *service-gears-types* '(floor-gears wall-gears angled-gears)
  "Drives present only while a fan is mounted (BLOWER-PRESENT); the rest are fixed blowers.")


(defparameter *service-negation-limit* 16
  "The most CONTROL options a negated aggregate is expanded into before it is left opaque.")


(defun service-drive-p (object)
  "Whether OBJECT is a drive: a gears or fixed-blower leaf instance."
  (some (lambda (type) (member object (census-type-instances type))) *service-drive-types*))


(defun service-gears-p (object)
  "Whether OBJECT is gears, present only with a fan mounted."
  (some (lambda (type) (member object (census-type-instances type))) *service-gears-types*))


(defun service-arc-drives (arcs)
  "Every drive named by some traversal clause of ARCS, in name order."
  (keeper-sorted-set (loop for arc in arcs
                           append (loop for clause in (fourth arc)
                                        append (remove-if-not #'service-drive-p clause)))))


(defun service-passage-objects (arcs)
  "Every gate, then every drive a traversal clause names: the passage services."
  (append (keeper-sorted-set (census-type-instances 'gate)) (service-arc-drives arcs)))


(defun service-node-text (node)
  "A service node in words."
  (case (first node)
    (:pass (format nil "~(~A~) ~:[clear~;open~]" (second node)
                   (member (second node) (census-type-instances 'gate))))
    (:active (format nil "~(~A~) active" (second node)))
    (t (format nil "~(~S~)" node))))


(defun service-literal-premise (primitive polarity)
  "A control literal as (premises . leaves): a receiver ACTIVE is a service node; every other
   literal is a leaf, available in the relaxation."
  (let ((relation (control-status-relation primitive)))
    (cond ((and polarity (member primitive (census-type-instances 'receiver)))
           (cons (list (list :active primitive)) nil))
          ((member primitive (census-type-instances 'receiver))
           (cons nil (list (format nil "~(~A~) inactive" primitive))))
          ((member primitive (census-type-instances 'pressure-plate))
           (cons nil (list (if polarity
                             (format nil "a body on ~(~A~) (T6)" primitive)
                             (format nil "~(~A~) clear" primitive)))))
          (t (cons nil (list (format nil "~(~A~) ~:[not ~;~]~(~A~)"
                                     primitive polarity (or relation "energized"))))))))


(defun service-clause-choices (clauses)
  "Every way to break every clause, one primitive each, as literal lists; NIL when the
   product exceeds *SERVICE-NEGATION-LIMIT*, as :OPAQUE."
  (let ((choices (list nil)))
    (dolist (clause clauses choices)
      (setf choices (loop for choice in choices
                          append (loop for primitive in clause
                                       collect (cons (cons primitive nil) choice))))
      (when (> (length choices) *service-negation-limit*)
        (return :opaque)))))


(defun service-control-literals (fact want)
  "The literal lists under which FACT's aggregate equals WANT.  An INVERTED device needs
   its DNF to take the opposite value."
  (let ((dnf-value (if (eq (fourth fact) 'inverted) (not want) want)))
    (if dnf-value
      (loop for clause in (second fact)
            collect (mapcar (lambda (primitive) (cons primitive t)) clause))
      (service-clause-choices (second fact)))))


(defun service-control-options (object controls)
  "OBJECT's CONTROL options: the S1 aggregate true for a gate, false for a drive."
  (let ((fact (find object controls :key #'third)))
    (when fact
      (let ((literals (service-control-literals fact (not (service-drive-p object)))))
        (if (eq literals :opaque)
          (list (list :kind :control :opaque t :premises (list :opaque) :leaves nil
                      :literals nil))
          (loop for literal-list in (remove-duplicates literals :test #'equal)
                collect (let ((parts (mapcar (lambda (literal)
                                               (service-literal-premise (car literal) (cdr literal)))
                                             literal-list)))
                          (list :kind :control :literals literal-list
                                :premises (remove-duplicates (loop for part in parts append (car part))
                                                             :test #'equal)
                                :leaves (loop for part in parts append (cdr part))))))))))


(defun service-disallowed-sources (location target)
  "Agent locations JAM-DISALLOWED> forbids for placing at LOCATION to jam TARGET."
  (keeper-sorted-set (loop for fact in (list-static-db)
                           when (and (eq (first fact) 'jam-disallowed>)
                                     (eq (third fact) location) (eq (fourth fact) target))
                             collect (second fact))))


(defun service-override-options (object jam-rows)
  "OBJECT's OVERRIDE options: one per MC jam site seeing it, over every jammer."
  (let ((sites nil))
    (dolist (row jam-rows)
      (when (eq (getf row :target) object)
        (dolist (site (getf row :sites))
          (let ((entry (assoc site sites :test #'equal)))
            (if entry
              (pushnew (getf row :jammer) (cdr entry))
              (push (list site (getf row :jammer)) sites))))))
    (loop for (site . jammers) in (sort sites #'string< :key (lambda (entry) (prin1-to-string (car entry))))
          collect (list :kind :override :site (list (first site) (second site))
                        :jammers (keeper-sorted-set jammers)
                        :premises (mapcar (lambda (gate) (list :pass gate)) (third site))
                        :leaves nil
                        :disallowed (service-disallowed-sources (first site) object)))))


(defun service-equipment-options (object)
  "Gears with no fan mounted are never active (BLOWER-PRESENT)."
  (when (service-gears-p object)
    (list (list :kind :equipment :premises nil :leaves (list "no fan mounted (T42 removal)")))))


(defun service-chain-options (receiver chains)
  "RC chains to RECEIVER, BOOTSTRAP or LATCH, grouped by class and gate set; and the counts
   of EXCLUDED and INFEASIBLE chains, which are not options."
  (let ((groups nil))
    (dolist (chain chains)
      (when (member (getf chain :class) '(:bootstrap :latch))
        (let* ((key (list (getf chain :class) (getf chain :gates)))
               (entry (assoc key groups :test #'equal)))
          (if entry
            (incf (cdr entry))
            (push (cons key 1) groups)))))
    (values (loop for ((class gates) . count) in (sort groups #'string< :key (lambda (entry) (prin1-to-string (car entry))))
                  collect (list :kind :chain :rc-class class :count count :receiver receiver
                                :premises (mapcar (lambda (gate) (list :pass gate)) gates)
                                :leaves nil))
            (count-if (lambda (chain) (member (getf chain :class) '(:excluded :infeasible))) chains))))


(defun service-corridor-options (receiver corridors)
  "Fixed corridors to RECEIVER with matching hues: premises their gates, leaves their
   occupancy locations clear."
  (loop for row in corridors
        when (and (eq (getf row :sink) receiver) (eq (getf row :source-hue) (getf row :sink-hue)))
          collect (list :kind :corridor :source (getf row :source) :receiver receiver
                        :premises (mapcar (lambda (gate) (list :pass gate)) (getf row :gates))
                        :leaves (mapcar (lambda (location) (format nil "~(~A~) clear" location))
                                        (getf row :locations))
                        :locations (getf row :locations))))


(defun service-goal-finals (form)
  "Positive goal conjuncts of one argument naming a receiver or a controlled device."
  (loop for conjunct in (landmark-goal-conjuncts form)
        when (and (consp conjunct) (= (length conjunct) 2) (symbolp (second conjunct))
                  (or (member (second conjunct) (census-type-instances 'receiver))
                      (landmark-control-fact (second conjunct))))
          collect conjunct))


(defun service-receivers (controls finals)
  "Receivers named by a control clause or by a FINAL goal conjunct."
  (keeper-sorted-set (append (intersection (control-primitives controls)
                                           (census-type-instances 'receiver))
                             (loop for conjunct in finals
                                   when (member (second conjunct) (census-type-instances 'receiver))
                                     collect (second conjunct)))))


(defun service-table (arcs controls finals)
  "The service graph: one (node . options) entry per passage object and receiver, and the
   per-receiver counts of chains that are not options."
  (let ((jam-rows (when (census-type-instances 'jammer) (jammer-sightline-rows)))
        (receivers (service-receivers controls finals))
        (corridors (fixed-beam-records))
        (table nil) (unused nil))
    (dolist (object (service-passage-objects arcs))
      (push (cons (list :pass object)
                  (append (service-control-options object controls)
                          (service-override-options object jam-rows)
                          (service-equipment-options object)))
            table))
    (loop for (receiver . chains) in (when receivers (hint-relay-chains receivers controls))
          do (multiple-value-bind (options others) (service-chain-options receiver chains)
               (push (cons receiver others) unused)
               (push (cons (list :active receiver)
                           (append options (service-corridor-options receiver corridors)))
                     table)))
    (values (nreverse table) (nreverse unused))))


(defun service-option-available-p (option available)
  "Whether every premise of OPTION is in AVAILABLE; :OPAQUE never is."
  (every (lambda (premise) (member premise available :test #'equal)) (getf option :premises)))


(defun service-closure (table off on standing)
  "The least fixed point of available nodes over TABLE, with node OFF forced unavailable
   and node ON forced available (either may be NIL).  STANDING: only non-OVERRIDE options
   make a node available, so no further jammer is spent on a premise."
  (let ((available (when on (list on)))
        (changed t))
    (loop while changed
          do (setf changed nil)
             (dolist (entry table)
               (unless (or (member (car entry) available :test #'equal)
                           (equal (car entry) off))
                 (when (some (lambda (option)
                               (and (not (and standing (eq (getf option :kind) :override)))
                                    (service-option-available-p option available)))
                             (cdr entry))
                   (push (car entry) available)
                   (setf changed t)))))
    available))


(defun service-dependency-path (option node table minus plus standing)
  "A shortest premise path from OPTION back to NODE through options available only with NODE
   provided, as a node list ending in NODE, or NIL.  STANDING as in SERVICE-CLOSURE."
  (let ((parents (make-hash-table :test #'equal))
        (frontier nil))
    (dolist (premise (getf option :premises))
      (unless (member premise minus :test #'equal)
        (unless (nth-value 1 (gethash premise parents))
          (setf (gethash premise parents) nil)
          (setf frontier (append frontier (list premise))))))
    (loop while frontier
          do (let ((current (pop frontier)))
               (when (equal current node)
                 (return-from service-dependency-path
                   (let ((path nil))
                     (loop for step = current then (gethash step parents)
                           while step
                           do (push step path))
                     path)))
               (dolist (next-option (cdr (assoc current table :test #'equal)))
                 (when (and (not (and standing (eq (getf next-option :kind) :override)))
                            (service-option-available-p next-option plus))
                   (dolist (premise (getf next-option :premises))
                     (unless (or (member premise minus :test #'equal)
                                 (nth-value 1 (gethash premise parents)))
                       (setf (gethash premise parents) current)
                       (setf frontier (append frontier (list premise)))))))))
    nil))


(defun service-option-class (option minus plus)
  "OPTION's class against the closures without (MINUS) and with (PLUS) its own service."
  (cond ((null (getf option :premises)) :direct)
        ((service-option-available-p option minus) :supported)
        ((service-option-available-p option plus) :needs-first)
        (t :unsupported)))


(defun service-verdict (classes)
  "A node's verdict from its options' CLASSES: available when some option is DIRECT or
   SUPPORTED against the closure without the node itself."
  (cond ((intersection classes '(:direct :supported)) :supported)
        ((member :needs-first classes) :setup-question)
        (t :no-provider)))


(defun service-classify (table)
  "Per node: verdict, and each option with its class and, for NEEDS FIRST, its path; the same
   again when premises may use only standing providers (:STANDING-CLASS, :STANDING-PATH).
   Returns (node verdict classified-options standing-verdict) entries in TABLE order."
  (loop for (node . options) in table
        collect (let* ((minus (service-closure table node nil nil))
                       (plus (service-closure table nil node nil))
                       (standing-minus (service-closure table node nil t))
                       (standing-plus (service-closure table nil node t))
                       (classified
                         (loop for option in options
                               collect (let ((class (service-option-class option minus plus))
                                             (standing (service-option-class option standing-minus standing-plus)))
                                         (append (list :class class
                                                       :path (when (eq class :needs-first)
                                                               (service-dependency-path option node table minus plus nil))
                                                       :standing-class standing
                                                       :standing-path (when (eq standing :needs-first)
                                                                        (service-dependency-path option node table
                                                                                                 standing-minus standing-plus t)))
                                                 option)))))
                  (list node
                        (service-verdict (mapcar (lambda (option) (getf option :class)) classified))
                        classified
                        (service-verdict (mapcar (lambda (option) (getf option :standing-class)) classified))))))


(defun service-door-add (region doors table)
  "Record DOORS as a way into REGION unless a subset is already recorded; drop supersets.
   True when recorded."
  (let ((known (gethash region table)))
    (unless (some (lambda (old) (subsetp old doors)) known)
      (setf (gethash region table)
            (cons doors (remove-if (lambda (old) (subsetp doors old)) known)))
      t)))


(defun service-door-sets (start rows)
  "Region -> minimal door sets from START over ROWS: one clause per row crossed, directed
   rows forward only, an empty family free.  A clause-aware walk; sets only grow, so it ends."
  (let ((table (make-hash-table :test #'equal))
        (frontier nil))
    (service-door-add start nil table)
    (push (cons start nil) frontier)
    (loop while frontier
          do (let* ((current (pop frontier))
                    (region (car current))
                    (doors (cdr current)))
               (when (member doors (gethash region table) :test #'equal)
                 (dolist (row rows)
                   (let ((next (keeper-row-next row region)))
                     (when next
                       (dolist (clause (or (fourth row) (list nil)))
                         (let ((union (keeper-sorted-set (append doors clause))))
                           (when (service-door-add next union table)
                             (push (cons next union) frontier))))))))))
    table))


(defun service-sorted-sets (sets)
  "Door sets in size, then name, order."
  (sort (copy-list sets)
        (lambda (left right)
          (or (< (length left) (length right))
              (and (= (length left) (length right))
                   (string< (prin1-to-string left) (prin1-to-string right)))))))


(defun service-sets-text (sets)
  "Door sets as parenthesized lists, an empty set as (), no sets as none."
  (if sets
    (format nil "~{~A~^ ~}" (mapcar (lambda (set) (format nil "(~(~{~A~^ ~}~))" set)) sets))
    "none"))


(defun service-goal-actor ()
  "(object start-location goal-location) for the first positive HAS-LOCATION goal conjunct,
   or NIL."
  (let ((destination (first (keeper-goal-destinations (get 'goal-fn :form)))))
    (when destination
      (list (second destination)
            (keeper-fact-value 'has-location (second destination) (database *start-state*))
            (third destination)))))


(defun service-phases (arcs names finals)
  "The goal actor's regions, transit and return sets, access table, and FINAL conjuncts."
  (let* ((rows (quotient-arc-rows arcs names))
         (actor (service-goal-actor))
         (from (when actor (gethash (second actor) names)))
         (to (when actor (gethash (third actor) names)))
         (forward (when from (service-door-sets from rows)))
         (backward (when to (service-door-sets to rows))))
    (list :actor actor :from from :to to :finals finals
          :transit (when forward (service-sorted-sets (gethash to forward)))
          :return (when backward (service-sorted-sets (gethash from backward)))
          :access forward)))


(defun service-necessary-doors (sets)
  "Doors in every set of SETS."
  (when sets
    (keeper-sorted-set (reduce #'intersection sets))))


(defun service-route-role (object phases)
  "OBJECT's role words: TRANSIT and RETURN, each NECESSARY or ALTERNATIVE, and TEMPORARY."
  (let* ((transit (getf phases :transit))
         (back (getf phases :return))
         (in-transit (some (lambda (set) (member object set)) transit))
         (in-return (some (lambda (set) (member object set)) back))
         (final (find object (getf phases :finals) :key #'second)))
    (append (when in-transit
              (list (if (member object (service-necessary-doors transit))
                      "TRANSIT necessary" "TRANSIT alternative")))
            (when in-return
              (list (if (member object (service-necessary-doors back))
                      "RETURN necessary" "RETURN alternative")))
            (when final (list "FINAL"))
            (when (and in-transit (not final)) (list "TEMPORARY")))))


(defun service-start-status (node actor)
  "NODE at the start: the actor's OBSTACLE-CLEAR for a passage, ACTIVE for a receiver."
  (if (eq (first node) :pass)
    (if actor
      (if (equipment-call 'obstacle-clear *start-state* actor (second node)) "PASSABLE" "BLOCKED")
      "UNRESOLVED (no agent)")
    (if (member (list (control-status-relation (second node)) (second node))
                (database *start-state*) :test #'equal)
      "ACTIVE" "INACTIVE")))


(defun service-opposed-controls (table)
  "(primitive positive-objects negative-objects) for every primitive whose literals appear
   with both polarities across different services' CONTROL options."
  (let ((uses nil))
    (loop for (node . options) in table
          do (dolist (option options)
               (when (eq (getf option :kind) :control)
                 (dolist (literal (getf option :literals))
                   (let ((entry (or (assoc (car literal) uses)
                                    (car (push (list (car literal) nil nil) uses)))))
                     (if (cdr literal)
                       (pushnew (second node) (second entry))
                       (pushnew (second node) (third entry))))))))
    (sort (loop for (primitive positive negative) in uses
                when (and positive negative)
                  collect (list primitive (keeper-sorted-set positive) (keeper-sorted-set negative)))
          #'string< :key (lambda (entry) (symbol-name (first entry))))))


(defun service-corridor-flag (option object table)
  "For an OVERRIDE site of OBJECT standing on an occupancy location of a fixed corridor whose
   premises include OBJECT, that corridor's text."
  (let ((location (first (getf option :site))))
    (loop for (node . options) in table
          append (loop for other in options
                       when (and (eq (getf other :kind) :corridor)
                                 (member location (getf other :locations))
                                 (member (list :pass object) (getf other :premises) :test #'equal))
                         collect (format nil "OCCUPIES ~(~A~) on ~(~A~) -> ~(~A~)'s fixed corridor, which needs ~(~A~)"
                                         location (getf other :source) (second node) object)))))


(defun service-premise-text (option)
  "OPTION's premises and leaves in words."
  (let ((parts (append (mapcar #'service-node-text (remove :opaque (getf option :premises)))
                       (getf option :leaves))))
    (cond ((getf option :opaque) "opaque negated aggregate")
          (parts (format nil "~{~A~^; ~}" parts))
          (t "no premise"))))


(defun service-class-name (class path node)
  "A class and, for NEEDS FIRST, its path, in words."
  (case class
    (:direct "DIRECT")
    (:supported "SUPPORTED")
    (:needs-first (format nil "NEEDS ~A FIRST~@[ (path ~{~A~^ -> ~})~]"
                          (service-node-text node) (mapcar #'service-node-text path)))
    (t "UNSUPPORTED IN SCOPE")))


(defun service-class-text (option node)
  "An option's class in words, and its dependency through standing providers when that alone
   needs NODE first: the dependency another jam would otherwise hide."
  (let ((class (service-class-name (getf option :class) (getf option :path) node)))
    (if (and (eq (getf option :standing-class) :needs-first)
             (not (eq (getf option :class) :needs-first)))
      (format nil "~A; through standing providers ~A" class
              (service-class-name :needs-first (getf option :standing-path) node))
      class)))


(defun report-service-override-groups (options node table)
  "OVERRIDE options grouped by class, premises and path, sites listed with their jammers,
   exclusions and corridor flags."
  (let ((groups nil))
    (dolist (option options)
      (let* ((key (list (getf option :class) (getf option :premises) (getf option :path)
                         (eq (getf option :standing-class) :needs-first) (getf option :standing-path)))
             (entry (assoc key groups :test #'equal)))
        (if entry
          (push option (cdr entry))
          (push (list key option) groups))))
    (dolist (group (nreverse groups))
      (let ((first-option (second group)))
        (format t "      OVERRIDE ~A: ~A~%" (service-class-text first-option node)
                (service-premise-text first-option))
        (dolist (option (reverse (cdr group)))
          (format t "        jam at ~(~A~) on ~(~A~) (~(~{~A~^, ~}~))~@[; JAM-DISALLOWED> from ~(~{~A~^, ~}~)~]~{; ~A~}~%"
                  (first (getf option :site)) (second (getf option :site)) (getf option :jammers)
                  (getf option :disallowed)
                  (service-corridor-flag option (second node) table)))))))


(defun report-service-option (option node)
  "One non-OVERRIDE option."
  (case (getf option :kind)
    (:control (format t "      CONTROL ~A: ~A~%" (service-class-text option node) (service-premise-text option)))
    (:equipment (format t "      EQUIPMENT ~A: ~A~%" (service-class-text option node) (service-premise-text option)))
    (:chain (format t "      CHAINS ~A: ~D RC chain~:P (~(~A~)); ~A~%" (service-class-text option node)
                    (getf option :count) (getf option :rc-class) (service-premise-text option)))
    (:corridor (format t "      CORRIDOR ~A: fixed ~(~A~) -> ~(~A~); ~A~%" (service-class-text option node)
                       (getf option :source) (second node) (service-premise-text option)))))


(defun service-verdict-text (verdict)
  "A service verdict in words."
  (case verdict
    (:supported "SUPPORTED")
    (:setup-question "SETUP QUESTION")
    (t "NO PROVIDER IN SCOPE")))


(defun report-service-row (entry controls phases actor table unused)
  "One service: its head line, then its options."
  (let* ((node (first entry))
         (object (second node))
         (fact (find object controls :key #'third))
         (options (third entry)))
    (format t "    ~A~:[~;  == ~:*~(~A~)~]  start ~A  verdict ~A~@[  route ~{~A~^, ~}~]~%"
            (service-node-text node)
            (when (and (eq (first node) :pass) fact) (control-boolean-form (second fact) (fourth fact)))
            (service-start-status node actor)
            (if (eq (fourth entry) :setup-question)
              (format nil "~A (through standing providers SETUP QUESTION)" (service-verdict-text (second entry)))
              (service-verdict-text (second entry)))
            (if (eq (first node) :pass)
              (service-route-role object phases)
              (when (find object (getf phases :finals) :key #'second) (list "FINAL"))))
    (when (and (eq (first node) :pass) (null fact))
      (format t "      no controller: ~:[override or equipment only~;uncontrolled drive turns unless jammed~]~%"
              (service-drive-p object)))
    (when (and (eq (first node) :active) (plusp (or (cdr (assoc object unused)) 0)))
      (format t "      (~D RC chain~:P EXCLUDED or INFEASIBLE, not options)~%" (cdr (assoc object unused))))
    (when (null options)
      (format t "      no option in scope~%"))
    (dolist (option (remove :override options :key (lambda (option) (getf option :kind))))
      (report-service-option option node))
    (report-service-override-groups (remove :override options :key (lambda (option) (getf option :kind))
                                            :test-not #'eq)
                                    node table)))


(defun report-service-phases (phases names)
  "The goal actor's transit, return, FINAL and TEMPORARY requirements."
  (let ((actor (getf phases :actor)))
    (if (null actor)
      (format t "~%  goal actor: none (no positive HAS-LOCATION goal conjunct); transit and return UNRESOLVED~%")
      (progn
        (format t "~%  goal actor ~(~A~): start ~(~A~) (~A) -> goal ~(~A~) (~A)~%"
                (first actor) (second actor) (getf phases :from) (third actor) (getf phases :to))
        (format t "    transit door sets (~A -> ~A, ~D): ~A~%" (getf phases :from) (getf phases :to)
                (length (getf phases :transit)) (service-sets-text (getf phases :transit)))
        (format t "      necessary: ~:[none~;~:*~(~{~A~^ ~}~)~]~%" (service-necessary-doors (getf phases :transit)))
        (format t "    return door sets (~A -> ~A, ~D): ~A~%" (getf phases :to) (getf phases :from)
                (length (getf phases :return)) (service-sets-text (getf phases :return)))
        (format t "      necessary: ~:[none~;~:*~(~{~A~^ ~}~)~]~%" (service-necessary-doors (getf phases :return)))))
    (format t "    final services: ~:[none (the goal names no receiver or controlled device)~;~:*~(~{~S~^ ~}~)~]~%"
            (getf phases :finals))
    (format t "    temporary services (in some transit set, not final): ~:[none~;~:*~{~A~^, ~}~]~%"
            (mapcar (lambda (door)
                      (format nil "~(~A~)~@[ via ~(~{~A~^ ~}~)~]" door
                              (let ((fact (landmark-control-fact door)))
                                (when fact (control-primitives (list fact))))))
                    (keeper-sorted-set
                      (loop for set in (getf phases :transit)
                            append (remove-if-not (lambda (door)
                                                    (and (or (member door (census-type-instances 'gate))
                                                             (service-drive-p door))
                                                         (not (find door (getf phases :finals) :key #'second))))
                                                  set)))))
    (format t "      READING: TEMPORARY means needed while crossing and expendable afterward, unless a return or a later crossing needs it again.~%")
    (when (getf phases :access)
      (format t "~%  access from ~A (minimal door sets to each region; kind predicates not evaluated)~%"
              (getf phases :from))
      (dolist (region (sort (remove-duplicates (loop for region being the hash-values of names collect region)
                                               :test #'string=)
                            (lambda (left right)
                              (< (parse-integer left :start 1) (parse-integer right :start 1)))))
        (format t "    ~A  ~A~%" region
                (if (gethash region (getf phases :access))
                  (service-sets-text (service-sorted-sets (gethash region (getf phases :access))))
                  "not reached in the relaxed graph"))))
    (format t "~%  retrieval (start places of jammers, connectors and fans)~%")
    (let ((objects (keeper-sorted-set (append (census-type-instances 'jammer)
                                              (census-type-instances 'connector)
                                              (census-type-instances 'fan))))
          (facts (equipment-state-facts *start-state*)))
      (if (null objects)
        (format t "    none~%")
        (dolist (object objects)
          (let ((location (keeper-fact-value 'has-location object facts)))
            (format t "    ~(~A~)  ~:[~(~A~)~;~(~A~) (~A)~]~%" object location
                    (or location (equipment-place-text (equipment-fan-place object facts)))
                    (when location (gethash location names)))))))))


(defun report-service-dependencies ()
  "SD, grade 2.  Services, their providers and premises, the setup dependencies between
   them, and transit, return and final requirements."
  (let* ((*print-pretty* nil)
         (controls (control-facts))
         (arcs (traversal-arc-facts))
         (names (region-name-table (region-blocks (traversal-endpoints) (contract-free-arcs arcs))))
         (finals (service-goal-finals (get 'goal-fn :form)))
         (phases (service-phases arcs names finals))
         (actor (or (first (getf phases :actor))
                    (first (keeper-sorted-set (census-type-instances 'agent))))))
    (format t "~2%SD  SERVICES AND SETUP DEPENDENCIES  [grade 2]~%")
    (format t "~A~%" (make-string 62 :initial-element #\-))
    (format t "  SCOPE: a monotone premise closure over passage services (every gate; each drive a traversal clause names) and receiver literals.  Providers: S1 CONTROL options, MC jam sites (OVERRIDE), gears with no fan (EQUIPMENT), RC chains and fixed corridors to a receiver.  No ordering, simultaneity, bodies, reach, occupancy or view is checked.~%")
    (format t "  CLASSES: DIRECT no premise; SUPPORTED premises available without this service; NEEDS <service> FIRST: installing this provider needs the service it will provide (a setup dependency); UNSUPPORTED IN SCOPE.  A service with only NEEDS-FIRST options is a SETUP QUESTION, not an impossibility.  \"Through standing providers\" repeats the test with premises supplied only by CONTROL, chains, corridors and equipment, never another jam, and is printed when that exposes a setup dependency a further jammer would hide.~%")
    (when (member "recorder" *spliced-tech-names* :test #'string=)
      (format t "  RECORDER SPLICED: physical view only; recording-view providers are not read.~%"))
    (report-service-phases phases names)
    (multiple-value-bind (table unused) (service-table arcs controls finals)
      (let ((classified (service-classify table)))
        (format t "~%  services (~D): ~D SUPPORTED, ~D SETUP QUESTION, ~D NO PROVIDER IN SCOPE~%"
                (length classified)
                (count :supported classified :key #'second)
                (count :setup-question classified :key #'second)
                (count :no-provider classified :key #'second))
        (dolist (entry classified)
          (report-service-row entry controls phases actor table unused))
        (format t "~%  opposed controls (a primitive needed in both states by different services)~%")
        (let ((opposed (service-opposed-controls table)))
          (if (null opposed)
            (format t "    none~%")
            (dolist (entry opposed)
              (format t "    ~(~A~): on for ~(~{~A~^, ~}~); off for ~(~{~A~^, ~}~)~%"
                      (first entry) (second entry) (third entry)))))
        (format t "~%  setup dependencies (options whose premises lead back to their own service)~%")
        (let ((rows (loop for entry in classified
                          for options = (remove-if-not (lambda (option)
                                                         (or (eq (getf option :class) :needs-first)
                                                             (eq (getf option :standing-class) :needs-first)))
                                                       (third entry))
                          when options
                            collect (list (first entry) (length options)
                                          (count :needs-first options :key (lambda (option) (getf option :class)))))))
          (if (null rows)
            (format t "    none~%")
            (dolist (row rows)
              (format t "    ~A: ~D option~:P, ~D even with further jams~%"
                      (service-node-text (first row)) (second row) (third row)))))
        (format t "    setup questions: ~:[none~;~:*~{~A~^, ~}~]~%"
                (loop for entry in classified
                      when (or (eq (second entry) :setup-question) (eq (fourth entry) :setup-question))
                        collect (service-node-text (first entry))))))
    (format t "~%")
    (format t "  NOT CLAIMED: an order, simultaneous availability, a body or reach allocation, occupancy, view or a realized setup.  A premise-free provider still needs placing, and a NEEDS-FIRST provider needs another provider in force while it is installed (a handover).  Access is to a site's own region; placement or pickup from another location within reach is not modelled.  For concrete consequences use REPORT-SERVICE-TRANSITION on two settled states.~%")
    (values)))


;;;; SW -- SERVICE TRANSITION BETWEEN TWO SUPPLIED STATES (T43) ;;;;
;;;
;;; Specification: doc/constraint-led-solving/Extractor-Specifications.md section 8.13.  What changes
;;; between two settled states: each passage service kept, kept by an alternative provider,
;;; lost or gained; the supplies withdrawn and added and the devices they drive; the arcs,
;;; mobility and retrieval an agent loses or gains; and stated transit, return and final
;;; requirements.  Not part of the profile.  The caller's states are never changed.


(defun service-transition-agent (scenario)
  "The named agent, or the sole agent, or NIL."
  (let ((agents (census-type-instances 'agent)))
    (or (getf scenario :agent)
        (when (null (cdr agents)) (first agents)))))


(defun service-transition-reason (scenario)
  "Why SCENARIO cannot be evaluated, or NIL.  Missing data is never a service verdict."
  (let ((agent (service-transition-agent scenario))
        (locations (census-type-instances 'location)))
    (cond ((member "recorder" *spliced-tech-names* :test #'string=)
           "recorder technology spliced: live and recording views are not evaluated (T45)")
          ((not (member 'propagate-changes! *update-names*)) "propagation unavailable")
          ((equipment-state-reason (getf scenario :before) (getf scenario :before-provenance) "before state"))
          ((equipment-state-reason (getf scenario :state) (getf scenario :provenance) "state"))
          ((null agent) "several agents: name one with :AGENT")
          ((not (member agent (census-type-instances 'agent))) (format nil "~(~A~) is not an agent" agent))
          ((notevery (lambda (location) (member location locations))
                     (append (getf scenario :transit) (getf scenario :return)))
           "a :TRANSIT or :RETURN entry is not a location")
          ((notevery #'consp (getf scenario :final)) "a :FINAL entry is not a proposition"))))


(defun service-aggregate-value (object controls facts)
  "OBJECT's S1 aggregate in FACTS; an uncontrolled device takes its default (NIL for a gate,
   T for a drive: UPDATE-GATE-STATUS!, UPDATE-BLOWER-STATUS!)."
  (let ((fact (find object controls :key #'third)))
    (if fact
      (hint-aggregate-holds-p fact facts)
      (service-drive-p object))))


(defun service-providers-in-force (state object controls facts)
  "The providers making OBJECT passable in STATE: :CONTROL, (:JAM jammer), :NO-FAN."
  (let ((aggregate (service-aggregate-value object controls facts)))
    (append (when (if (service-drive-p object) (not aggregate) aggregate) (list :control))
            (loop for fact in facts
                  when (and (eq (first fact) 'jamming) (eq (third fact) object))
                    collect (list :jam (second fact)))
            (when (and (service-gears-p object)
                       (not (equipment-call 'blower-present state object)))
              (list :no-fan)))))


(defun service-state-services (state agent arcs controls facts)
  "One plist per passage service in STATE: passable, bit, providers and engine agreement."
  (loop for object in (service-passage-objects arcs)
        collect (let ((passable (and (equipment-call 'obstacle-clear state agent object) t))
                      (providers (service-providers-in-force state object controls facts)))
                  (list :object object :passable passable :providers providers
                        :bit (if (service-drive-p object)
                               (if (member (list 'turning object) facts :test #'equal) "turning" "stopped")
                               (if (member (list 'open object) facts :test #'equal) "open" "closed"))
                        :agrees (eq passable (not (null providers)))))))


(defun service-arc-passable-p (state agent arc)
  "Whether some clause of ARC's family is ALL-CLEAR for AGENT in STATE."
  (or (null (fourth arc))
      (some (lambda (clause) (equipment-call 'all-clear state agent clause)) (fourth arc))))


(defun service-state-mobility (state agent facts)
  "The locations AGENT's mobility closure reaches from its own location in STATE."
  (let ((location (keeper-fact-value 'has-location agent facts)))
    (when location
      (keeper-sorted-set (mapcar #'first (funcall (symbol-function 'mobility-results-in-state) state agent location))))))


(defun service-state-retrieval (state facts mobility)
  "(object location retrievable) for every unheld mobile object other than an agent."
  (loop for object in (keeper-sorted-set (census-type-instances 'mobile-object))
        for location = (keeper-fact-value 'has-location object facts)
        when (and location (not (member object (census-type-instances 'agent))))
          collect (list object location
                        (and (some (lambda (from) (equipment-call 'reachable state location from))
                                   mobility)
                             t))))


(defun service-state-record (state agent arcs controls)
  "Everything SW reads from one state."
  (let* ((facts (equipment-state-facts state))
         (mobility (service-state-mobility state agent facts)))
    (list :facts facts
          :location (keeper-fact-value 'has-location agent facts)
          :services (service-state-services state agent arcs controls facts)
          :arcs (loop for arc in arcs
                      when (fourth arc)
                        collect (cons arc (service-arc-passable-p state agent arc)))
          :mobility mobility
          :retrieval (service-state-retrieval state facts mobility))))


(defun service-change (old new)
  "One passage service's transition from OLD to NEW records."
  (let ((before (getf old :providers))
        (after (getf new :providers)))
    (cond ((and (getf old :passable) (getf new :passable))
           (if (equal before after)
             (list :object (getf old :object) :change :kept :providers after)
             (list :object (getf old :object) :change :kept-by-alternative
                   :lost (set-difference before after :test #'equal) :providers after
                   :override (every (lambda (provider) (and (consp provider) (eq (first provider) :jam)))
                                    after))))
          ((getf old :passable)
           (list :object (getf old :object) :change :lost :lost before))
          ((getf new :passable)
           (list :object (getf old :object) :change :gained :providers after))
          (t (list :object (getf old :object) :change :blocked)))))


(defun service-fact-changes (relation before after)
  "(removed added) facts of RELATION between two fact lists."
  (let ((old (remove-if-not (lambda (fact) (eq (first fact) relation)) before))
        (new (remove-if-not (lambda (fact) (eq (first fact) relation)) after)))
    (list (sort (set-difference old new :test #'equal) #'string< :key #'prin1-to-string)
          (sort (set-difference new old :test #'equal) #'string< :key #'prin1-to-string))))


(defun service-primitive-changes (controls before after)
  "(primitive from to devices) for every S1 primitive and receiver whose status changed."
  (loop for primitive in (keeper-sorted-set (append (control-primitives controls)
                                                    (census-type-instances 'receiver)))
        for relation = (control-status-relation primitive)
        for old = (and relation (member (list relation primitive) before :test #'equal) t)
        for new = (and relation (member (list relation primitive) after :test #'equal) t)
        unless (eq old new)
          collect (list primitive old new
                        (mapcar #'third (coupling-driven-facts primitive controls)))))


(defun service-requirements (scenario after-state agent record)
  "Each stated requirement on the second state, as (kind item met)."
  (append (loop for target in (getf scenario :transit)
                collect (list :transit target (and (member target (getf record :mobility)) t)))
          (loop for target in (getf scenario :return)
                collect (list :return target
                              (and (getf record :location)
                                   (assoc (getf record :location)
                                          (funcall (symbol-function 'mobility-results-in-state) after-state agent target))
                                   t)))
          (loop for fact in (getf scenario :final)
                collect (list :final fact (and (or (member fact (getf record :facts) :test #'equal)
                                                   (member fact (list-static-db) :test #'equal))
                                               t)))))


(defun service-transition-result (scenario)
  "SW: what changes between two settled states for one agent.  Returns a plist; the
   caller's states are never changed."
  (let ((reason (service-transition-reason scenario)))
    (if reason
      (list :status :unresolved :reason reason)
      (let* ((agent (service-transition-agent scenario))
             (controls (control-facts))
             (arcs (traversal-arc-facts))
             (old (service-state-record (getf scenario :before) agent arcs controls))
             (new (service-state-record (getf scenario :state) agent arcs controls)))
        (list :status :evaluated :agent agent
              :before-provenance (getf scenario :before-provenance) :provenance (getf scenario :provenance)
              :locations (list (getf old :location) (getf new :location))
              :services (mapcar #'service-change (getf old :services) (getf new :services))
              :agreement (list (every (lambda (record) (getf record :agrees)) (getf old :services))
                               (every (lambda (record) (getf record :agrees)) (getf new :services)))
              :disagreements (loop for record in (append (getf old :services) (getf new :services))
                                   unless (getf record :agrees) collect (getf record :object))
              :bits (mapcar (lambda (old-record new-record)
                              (list (getf old-record :object) (getf old-record :bit) (getf new-record :bit)))
                            (getf old :services) (getf new :services))
              :primitives (service-primitive-changes controls (getf old :facts) (getf new :facts))
              :jams (service-fact-changes 'jamming (getf old :facts) (getf new :facts))
              :pairings (service-fact-changes 'paired (getf old :facts) (getf new :facts))
              :mounts (service-fact-changes 'mounted-on (getf old :facts) (getf new :facts))
              :arcs-lost (loop for (arc . passable) in (getf old :arcs)
                               when (and passable (not (cdr (assoc arc (getf new :arcs) :test #'equal))))
                                 collect arc)
              :arcs-gained (loop for (arc . passable) in (getf new :arcs)
                                 when (and passable (not (cdr (assoc arc (getf old :arcs) :test #'equal))))
                                   collect arc)
              :mobility-lost (keeper-sorted-set (set-difference (getf old :mobility) (getf new :mobility)))
              :mobility-gained (keeper-sorted-set (set-difference (getf new :mobility) (getf old :mobility)))
              :mobility (getf new :mobility)
              :retrieval-lost (loop for entry in (getf new :retrieval)
                                    when (and (not (third entry))
                                              (third (assoc (first entry) (getf old :retrieval))))
                                      collect entry)
              :retrieval-gained (loop for entry in (getf new :retrieval)
                                      when (and (third entry)
                                                (not (third (assoc (first entry) (getf old :retrieval)))))
                                        collect entry)
              :unretrievable (remove-if #'third (getf new :retrieval))
              :requirements (service-requirements scenario (getf scenario :state) agent new))))))


(defun service-provider-text (providers)
  "Providers in words."
  (format nil "~:[none~;~:*~{~A~^, ~}~]"
          (mapcar (lambda (provider)
                    (cond ((eq provider :control) "CONTROL")
                          ((eq provider :no-fan) "NO FAN")
                          (t (format nil "JAM by ~(~A~)" (second provider)))))
                  providers)))


(defun service-arc-text (arc)
  "A traversal arc as source, direction and destination with its family."
  (format nil "~(~A~) ~:[<->~;->~] ~(~A~) ~(~S~)" (third arc) (eq (first arc) *traversal-directed-relation*)
          (fifth arc) (fourth arc)))


(defun report-service-changes (result)
  "The per-service part of an SW report."
  (format t "  passage services (~D)~%" (length (getf result :services)))
  (dolist (change (getf result :services))
    (let ((bits (find (getf change :object) (getf result :bits) :key #'first)))
      (case (getf change :change)
        (:kept (format t "    ~(~A~)  KEPT: ~A~%" (getf change :object) (service-provider-text (getf change :providers))))
        (:kept-by-alternative
         (format t "    ~(~A~)  KEPT BY ALTERNATIVE~:[~; (OVERRIDE)~]: lost ~A; remaining ~A~%"
                 (getf change :object) (getf change :override)
                 (service-provider-text (getf change :lost)) (service-provider-text (getf change :providers))))
        (:lost (format t "    ~(~A~)  LOST (~A -> ~A): providers lost ~A~%" (getf change :object)
                       (second bits) (third bits) (service-provider-text (getf change :lost))))
        (:gained (format t "    ~(~A~)  GAINED (~A -> ~A) by ~A~%" (getf change :object)
                         (second bits) (third bits) (service-provider-text (getf change :providers))))
        (t (format t "    ~(~A~)  blocked both (~A)~%" (getf change :object) (third bits)))))))


(defun report-service-transition (scenario)
  "Print SW for SCENARIO and return its result."
  (let ((*print-pretty* nil)
        (result (service-transition-result scenario)))
    (format t "~%SW  SERVICE TRANSITION  [supplied settled states]~%")
    (if (eq (getf result :status) :unresolved)
      (format t "  UNRESOLVED: ~A~%" (getf result :reason))
      (progn
        (format t "  before: ~A~%  after:  ~A~%  agent ~(~A~) at ~(~A~) -> ~(~A~)~%"
                (getf result :before-provenance) (getf result :provenance) (getf result :agent)
                (first (getf result :locations)) (second (getf result :locations)))
        (format t "  engine agreement (provider reading = OBSTACLE-CLEAR): before ~:[DISAGREES~;agrees~], after ~:[DISAGREES~;agrees~]~@[ on ~(~{~A~^, ~}~)~]~%"
                (first (getf result :agreement)) (second (getf result :agreement)) (getf result :disagreements))
        (report-service-changes result)
        (format t "  supplies withdrawn or established~%")
        (if (getf result :primitives)
          (dolist (entry (getf result :primitives))
            (format t "    ~(~A~) ~:[off~;on~] -> ~:[off~;on~]~:[; drives no device~;; affected devices ~:*~(~{~A~^, ~}~)~]~%"
                    (first entry) (second entry) (third entry) (fourth entry)))
          (format t "    no primitive status changed~%"))
        (loop for (label key) in '(("jams" :jams) ("pairings" :pairings) ("fan mounts" :mounts))
              do (let ((changes (getf result key)))
                   (when (or (first changes) (second changes))
                     (format t "    ~A removed ~:[none~;~:*~(~{~S~^ ~}~)~]; added ~:[none~;~:*~(~{~S~^ ~}~)~]~%"
                             label (first changes) (second changes)))))
        (format t "  arcs for ~(~A~): ~D lost, ~D gained~%" (getf result :agent)
                (length (getf result :arcs-lost)) (length (getf result :arcs-gained)))
        (dolist (arc (getf result :arcs-lost))
          (format t "    LOST   ~A~%" (service-arc-text arc)))
        (dolist (arc (getf result :arcs-gained))
          (format t "    GAINED ~A~%" (service-arc-text arc)))
        (format t "  mobility from each state's own location: lost ~:[none~;~:*~(~{~A~^ ~}~)~]; gained ~:[none~;~:*~(~{~A~^ ~}~)~]~%"
                (getf result :mobility-lost) (getf result :mobility-gained))
        (format t "  retrieval: lost ~:[none~;~:*~(~{~A~^ ~}~)~]; gained ~:[none~;~:*~(~{~A~^ ~}~)~]; not retrievable now ~:[none~;~:*~(~{~A~^, ~}~)~]~%"
                (mapcar #'first (getf result :retrieval-lost)) (mapcar #'first (getf result :retrieval-gained))
                (mapcar (lambda (entry) (format nil "~A at ~A" (first entry) (second entry)))
                        (getf result :unretrievable)))
        (when (getf result :requirements)
          (format t "  requirements on the second state~%")
          (dolist (entry (getf result :requirements))
            (format t "    ~A ~(~S~): ~:[NOT MET~;MET~]~@[~A~]~%" (first entry) (second entry) (third entry)
                    (when (eq (first entry) :return)
                      "  (mobility from the target in this state; the agent is not relocated)"))))
        (format t "  NOT CLAIMED: which action caused a change, that the second state follows from the first, a realizable ordering, a safe transfer, or future reachability.  Mobility is one engine closure in each state.~%")))
    result))


;;;; FH -- FROM HERE (T22, I1) ;;;;
;;;
;;; Specification: doc/constraint-led-solving/Extractor-Specifications.md section 11.  The Phase 3
;;; step 7 report at one state: F0 the state itself; F1 each agent's MOVE successors, and
;;; which primitive controllers a one-step successor, or the agent's own next action after
;;; one of its moves, changes; F2 where held cargo can be set down; F3 the live relay state,
;;; then RC's usable chains grouped by the gates closed now.  Every reading is of the given
;;; state, through the engine's own queries and GENERATE-CHILDREN; no search runs and nothing
;;; is propagated by hand.  FH is NOT part of the profile: REPORT-STATIC-CONSTRAINT-PROFILE
;;; does not call it, and the profile file does not change.  Call it after staging as
;;;
;;;   (report-from-here)                 ; the staged start
;;;   (report-from-here <checkpoint>)    ; a search checkpoint's endpoint
;;;   (report-from-here <action-list>)   ; a prefix replayed from the staged start
;;;
;;; SUBSTRATE VOCABULARY (C3).  The relations HAS-LOCATION, ON, HOLDING, PAIRED, COLOR and
;;; OPEN, the queries AGENT-CONFIGURATION, BASE, TOP, REACHABLE and PLACEMENT-OPTIONS, the
;;; action MOVE and its route argument, the types AGENT, LOCATION, RECEIVER, CONNECTOR,
;;; FLOOR-REPEATER and WALL-REPEATER, the placement marker GROUND, and the helpers of the
;;; sections it reads are named; each is a tech/ or engine interface or this file's own.  No
;;; problem object name appears; every instance comes from the staged databases.


(defun from-here-facts (state)
  "STATE's propositions, with a bijective relation's two index names read back as the
   relation itself, so (HOLDING1 a c) and (HOLDING2 a c) come back as one (HOLDING a c)."
  (remove-duplicates
    (mapcar (lambda (fact)
              (let ((canonical (car (gethash (first fact) *bijective-canonical*))))
                (if canonical
                  (cons canonical (rest fact))
                  fact)))
            (database state))
    :test #'equal))


(defun from-here-primitive-text (primitive facts)
  "PRIMITIVE by name, then its S1 status relation when it holds, with the occupants resting
   on it (a depressed plate's weights), or - when it does not."
  (let ((occupants (keeper-sorted-set (loop for fact in facts
                                            when (and (eq (first fact) 'on)
                                                      (eq (third fact) primitive))
                                              collect (second fact)))))
    (if (hint-primitive-active-p primitive facts)
      (format nil "~(~A  ~A~)~@[ (~(~{~A~^, ~}~))~]"
              primitive (control-status-relation primitive) occupants)
      (format nil "~(~A~)  -" primitive))))


(defun from-here-agent-line (state agent facts)
  "F0's line for AGENT: its configuration, base and holding, or NOT PRESENT."
  (if (keeper-fact-value 'has-location agent facts)
    (format nil "      ~(~A  ~{~A on ~A~}, base ~A; holds ~A~)"
            agent
            (funcall (symbol-function 'agent-configuration) state agent)
            (funcall (symbol-function 'base) state agent)
            (or (third (find-if (lambda (fact)
                                  (and (eq (first fact) 'holding) (eq (second fact) agent)))
                                facts))
                "nothing"))
    (format nil "      ~(~A~)  not present" agent)))


(defun from-here-object-line (state object facts)
  "F0's line for a located non-agent OBJECT: where it is, what it rests on or who holds it,
   and its top."
  (let ((holder (second (find-if (lambda (fact)
                                   (and (eq (first fact) 'holding) (eq (third fact) object)))
                                 facts))))
    (format nil "      ~(~A  ~A ~:[on ~A~;held by ~A~], top ~A~)"
            object
            (keeper-fact-value 'has-location object facts)
            holder
            (or holder (keeper-fact-value 'on object facts) 'ground)
            (funcall (symbol-function 'top) state object))))


(defun from-here-state-section (state facts context)
  "F0: agents, located objects, controlled devices with every S1 device state relation that
   holds for them, and primitive controllers."
  (let ((agents (getf context :agents))
        (objects (sort (loop for fact in facts
                             when (and (eq (first fact) 'has-location)
                                       (not (member (second fact) (getf context :agents))))
                               collect (second fact))
                       #'string< :key #'symbol-name)))
    (append (list "  F0 state [grade 1]"
                  (format nil "    agents (~D)" (length agents)))
            (mapcar (lambda (agent) (from-here-agent-line state agent facts)) agents)
            (list (format nil "    objects (~D)" (length objects)))
            (mapcar (lambda (object) (from-here-object-line state object facts)) objects)
            (list (format nil "    devices (~D)" (length (getf context :devices))))
            (mapcar (lambda (device)
                      (format nil "      ~(~A  ~:[-~;~:*~{~A~^, ~}~]~)"
                              device
                              (remove-if-not (lambda (relation)
                                               (member (list relation device) facts :test #'equal))
                                             (getf context :relations))))
                    (getf context :devices))
            (list (format nil "    primitives (~D)" (length (getf context :primitives))))
            (mapcar (lambda (primitive)
                      (format nil "      ~A" (from-here-primitive-text primitive facts)))
                    (getf context :primitives)))))


(defun from-here-children (state)
  "STATE's one-step successors as GENERATE-CHILDREN produces them in a depth-first search,
   with symmetry pruning off so that no equivalent instantiation is hidden."
  (let ((*algorithm* 'depth-first)
        (*symmetry-pruning* nil))
    (generate-children (make-node :state state :depth 0))))


(defun from-here-expansion (state agents)
  "STATE's successors, and each agent's MOVE successors expanded once more through that
   agent's own actions.  A plist: :CHILDREN, one (agent action facts) per successor, the
   agent being the first of AGENTS among the action's arguments; :MOVES, one (agent
   configuration route-labels facts next-facts child) per MOVE successor, sorted by
   configuration, NEXT-FACTS holding the facts of the agent's own successors from there."
  (let ((children nil)
        (moves nil))
    (dolist (child (from-here-children state))
      (let ((agent (find-if (lambda (item) (member item agents))
                            (problem-state.instantiations child))))
        (push (list agent (problem-state.name child) (from-here-facts child)) children)
        (when (and agent (eq (problem-state.name child) 'move))
          (push (list agent
                      (funcall (symbol-function 'agent-configuration) child agent)
                      (remove-duplicates (mapcar #'first (second (problem-state.instantiations child))))
                      (from-here-facts child)
                      (loop for next in (from-here-children child)
                            when (eq agent (find-if (lambda (item) (member item agents))
                                                    (problem-state.instantiations next)))
                              collect (from-here-facts next))
                      child)
                moves))))
    (list :children (nreverse children)
          :moves (sort moves #'string<
                       :key (lambda (move) (format nil "~(~{~A on ~A~}~)" (second move)))))))


(defun from-here-move-lines (state facts context expansion)
  "F1's moves: per present agent, its configuration and MOVE successors, each with its
   route's segment labels."
  (loop for agent in (getf context :agents)
        when (keeper-fact-value 'has-location agent facts)
          append (let ((moves (remove-if-not (lambda (move) (eq (first move) agent))
                                             (getf expansion :moves))))
                   (cons (format nil "      ~(~A from ~{~A on ~A~}~) (~D)"
                                 agent
                                 (funcall (symbol-function 'agent-configuration) state agent)
                                 (length moves))
                         (mapcar (lambda (move)
                                   (format nil "        ~(~{~A on ~A~}  (~{~A~^, ~})~)"
                                           (second move) (third move)))
                                 moves)))))


(defun from-here-now-text (primitive facts expansion)
  "The (agent action) pairs whose one-step successor changes PRIMITIVE's status, or NIL."
  (let ((before (hint-primitive-active-p primitive facts))
        (pairs nil))
    (dolist (child (getf expansion :children))
      (unless (eq before (hint-primitive-active-p primitive (third child)))
        (pushnew (format nil "~(~A (~A)~)" (or (first child) "none") (second child))
                 pairs :test #'string=)))
    (when pairs
      (format nil "        now: ~{~A~^, ~}" (sort pairs #'string<)))))


(defun from-here-after-text (primitive context expansion)
  "Per agent, the destinations of its moves from which its own next action changes
   PRIMITIVE's status, or NIL when there are none."
  (let ((parts nil))
    (dolist (agent (getf context :agents))
      (let ((via (loop for move in (getf expansion :moves)
                       when (and (eq (first move) agent)
                                 (some (lambda (next)
                                         (not (eq (hint-primitive-active-p primitive (fourth move))
                                                  (hint-primitive-active-p primitive next))))
                                       (fifth move)))
                         collect (format nil "~(~{~A on ~A~}~)" (second move)))))
        (when via
          (push (format nil "~(~A~) via ~{~A~^, ~}" agent via) parts))))
    (when parts
      (format nil "        after one move: ~{~A~^; ~}" (nreverse parts)))))


(defun from-here-site-lines (primitive facts context)
  "One line per site of PRIMITIVE and per present agent: the devices S4's relaxed graph
   requires from the agent's region to the site's, with the site's barriers, each read
   open or closed now."
  (let ((names (getf context :names)))
    (loop for site in (hint-controller-sites primitive (getf context :static) names)
          append (loop for agent in (getf context :agents)
                       for location = (keeper-fact-value 'has-location agent facts)
                       when location
                         collect (multiple-value-bind (required connected)
                                     (when (getf context :spine)
                                       (keeper-required-devices (gethash location names) (second site)
                                                                (getf context :spine)
                                                                (getf context :devices)))
                                   (let ((needed (keeper-sorted-set
                                                   (union (copy-list required) (copy-list (third site))))))
                                     (format nil "        site ~(~A~) (~A): from ~(~A~) in ~A ~A"
                                             (first site) (second site) agent (gethash location names)
                                             (cond ((null (getf context :spine)) "S4 spine unavailable")
                                                   ((not connected) "not joined in the relaxed graph")
                                                   ((null needed) "needs no device")
                                                   (t (format nil "needs ~{~A~^, ~}"
                                                              (mapcar (lambda (device)
                                                                        (format nil "~(~A~) (~:[closed~;open~])"
                                                                                device
                                                                                (member (list 'open device) facts
                                                                                        :test #'equal)))
                                                                      needed)))))))))))


(defun from-here-controller-lines (primitive facts context expansion)
  "F1's rows for one primitive controller: its status, then NOW and AFTER ONE MOVE, or NOT
   WITHIN ONE MOVE with its sites."
  (let ((now (from-here-now-text primitive facts expansion))
        (after (from-here-after-text primitive context expansion)))
    (append (list (format nil "      ~A" (from-here-primitive-text primitive facts)))
            (when now (list now))
            (when after (list after))
            (unless (or now after)
              (if (eq (keeper-controller-kind primitive) :receiver)
                (list "        not within one move; beam-driven: see F3")
                (cons "        not within one move" (from-here-site-lines primitive facts context)))))))


(defun from-here-reach-section (state facts context expansion)
  "F1: every present agent's moves, then every primitive controller."
  (append (list "  F1 reachable and controllers [grade 1; sites grade 2]" "    moves")
          (from-here-move-lines state facts context expansion)
          (list (format nil "    controllers (~D)" (length (getf context :primitives))))
          (loop for primitive in (getf context :primitives)
                append (from-here-controller-lines primitive facts context expansion))))


(defun from-here-placement-text (state agent held here)
  "Every location REACHABLE from HERE in STATE, with the places PLACEMENT-OPTIONS offers
   AGENT there for HELD, ground first; NIL when there are none."
  (let ((targets (loop for location in (sort (copy-list (census-type-instances 'location))
                                             #'string< :key #'symbol-name)
                       for places = (when (funcall (symbol-function 'reachable) state location here)
                                      (funcall (symbol-function 'placement-options)
                                               state agent location held))
                       when places
                         collect (format nil "~(~A (~{~A~^, ~})~)"
                                         location
                                         (append (when (member 'ground places) (list 'ground))
                                                 (sort (remove 'ground (copy-list places))
                                                       #'string< :key #'symbol-name))))))
    (when targets
      (format nil "~{~A~^; ~}" targets))))


(defun from-here-placement-lines (state facts context expansion)
  "F2: per present agent, what it holds, then the placements from where it stands and from
   each of its MOVE successors' configurations."
  (loop for agent in (getf context :agents)
        for held = (third (find-if (lambda (fact)
                                     (and (eq (first fact) 'holding) (eq (second fact) agent)))
                                   facts))
        when (keeper-fact-value 'has-location agent facts)
          append (if held
                   (cons (format nil "    ~(~A  holds ~A~)" agent held)
                         (append (let ((text (from-here-placement-text
                                               state agent held
                                               (keeper-fact-value 'has-location agent facts))))
                                   (when text
                                     (list (format nil "      from ~(~{~A on ~A~}~): ~A"
                                                   (funcall (symbol-function 'agent-configuration)
                                                            state agent)
                                                   text))))
                                 (loop for move in (getf expansion :moves)
                                       for text = (when (eq (first move) agent)
                                                    (from-here-placement-text (sixth move) agent held
                                                                              (first (second move))))
                                       when text
                                         collect (format nil "      after a move to ~(~{~A on ~A~}~): ~A"
                                                         (second move) text))))
                   (list (format nil "    ~(~A~)  holds nothing" agent)))))


(defun from-here-relay-line (state relay facts)
  "F3's line for RELAY: where it stands and its top, who holds it, or FIXED; its links
   (PAIRED in either direction); its COLOR, or UNLIT.  A connector with neither a location
   nor a holder is NOT PRESENT."
  (let ((location (keeper-fact-value 'has-location relay facts))
        (holder (second (find-if (lambda (fact)
                                   (and (eq (first fact) 'holding) (eq (third fact) relay)))
                                 facts)))
        (links (keeper-sorted-set (loop for fact in facts
                                        when (and (eq (first fact) 'paired) (eq (second fact) relay))
                                          collect (third fact)
                                        when (and (eq (first fact) 'paired) (eq (third fact) relay))
                                          collect (second fact)))))
    (if (and (null location) (null holder) (member relay (census-type-instances 'connector)))
      (format nil "      ~(~A~)  not present" relay)
      (format nil "      ~(~A  ~A; links ~:[none~;~:*~{~A~^, ~}~]; ~:[unlit~;~:*~A~]~)"
              relay
              (cond (location (format nil "~A on ~A, top ~A"
                                      location
                                      (or (keeper-fact-value 'on relay facts) 'ground)
                                      (funcall (symbol-function 'top) state relay)))
                    (holder (format nil "held by ~A" holder))
                    (t "fixed"))
              links
              (keeper-fact-value 'color relay facts)))))


(defun from-here-gate-text (gate facts controls)
  "A closed GATE with its S1 form and the state of each of its primitives now."
  (let ((fact (find gate controls :key #'third)))
    (if fact
      (format nil "~(~A == ~A: ~{~A~^, ~}~)"
              gate
              (control-boolean-form (second fact) (fourth fact))
              (mapcar (lambda (primitive)
                        (format nil "~A ~:[not ~;~]~A"
                                primitive
                                (hint-primitive-active-p primitive facts)
                                (control-status-relation primitive)))
                      (control-primitives (list fact))))
      (format nil "~(~A~)" gate))))


(defun from-here-group-line (closed chains facts controls)
  "One candidate group: the gates CLOSED now that its CHAINS need, with their S1 forms, the
   chain count by class, and the least connectors and off-plate bodies."
  (format nil "        needs ~(~{~A~^, ~}~) (~{~A~^; ~}): ~D chain~:P (~{~A~^, ~}), ~
               least connectors ~D, least off-plate bodies ~D"
          closed
          (mapcar (lambda (gate) (from-here-gate-text gate facts controls)) closed)
          (length chains)
          (loop for class in '(:bootstrap :latch)
                for count = (count class chains :key (lambda (chain) (getf chain :class)))
                when (plusp count)
                  collect (format nil "~D ~(~A~)" count class))
          (reduce #'min chains :key (lambda (chain) (length (getf chain :stations))))
          (reduce #'min chains :key (lambda (chain) (getf chain :off-plate)))))


(defun from-here-candidate-lines (receiver chains facts controls)
  "RC's usable chains to an inactive RECEIVER, grouped by the gates they need that are
   closed now: the OPEN NOW group chain by chain, every other group on one line, groups
   ordered by their number of closed gates, then by name."
  (let ((usable (remove-if-not (lambda (chain) (member (getf chain :class) '(:bootstrap :latch)))
                               chains))
        (groups nil))
    (dolist (chain usable)
      (let* ((closed (keeper-sorted-set
                       (remove-if (lambda (gate) (member (list 'open gate) facts :test #'equal))
                                  (getf chain :gates))))
             (entry (assoc closed groups :test #'equal)))
        (if entry
          (push chain (rest entry))
          (push (list closed chain) groups))))
    (setf groups (sort groups (lambda (left right)
                                (if (= (length (first left)) (length (first right)))
                                  (string< (format nil "~{~A ~}" (first left))
                                           (format nil "~{~A ~}" (first right)))
                                  (< (length (first left)) (length (first right)))))))
    (append (list (format nil "      ~(~A~)  usable chains ~D: bootstrap ~D, latch ~D"
                          receiver (length usable)
                          (count :bootstrap usable :key (lambda (chain) (getf chain :class)))
                          (count :latch usable :key (lambda (chain) (getf chain :class)))))
            (if (assoc nil groups)
              (cons (format nil "        open now (~D):" (length (rest (assoc nil groups))))
                    (sort (mapcar (lambda (chain) (format nil "          ~A" (hint-chain-text chain)))
                                  (rest (assoc nil groups)))
                          #'string<))
              (list "        open now: none"))
            (loop for (closed . members) in groups
                  when closed
                    collect (from-here-group-line closed members facts controls)))))


(defun from-here-beam-section (state facts context)
  "F3: receivers, relays, then candidate chains for every receiver not active now."
  (let ((receivers (sort (copy-list (census-type-instances 'receiver)) #'string< :key #'symbol-name))
        (relays (sort (append (copy-list (census-type-instances 'connector))
                              (copy-list (census-type-instances 'floor-repeater))
                              (copy-list (census-type-instances 'wall-repeater)))
                      #'string< :key #'symbol-name)))
    (append (list "  F3 beams [grade 1; candidates grade 2]"
                  (format nil "    receivers (~D)" (length receivers)))
            (mapcar (lambda (receiver) (format nil "      ~A" (from-here-primitive-text receiver facts)))
                    receivers)
            (list (format nil "    relays (~D)" (length relays)))
            (mapcar (lambda (relay) (from-here-relay-line state relay facts)) relays)
            (list "    candidates")
            (loop for receiver in receivers
                  append (if (hint-primitive-active-p receiver facts)
                           (list (format nil "      ~(~A  ~A now; candidates not listed~)"
                                         receiver (control-status-relation receiver)))
                           (from-here-candidate-lines receiver
                                                      (rest (assoc receiver (getf context :chains)))
                                                      facts (getf context :controls)))))))


(defun from-here-context ()
  "What every FH section reads from the staged problem, computed once as a plist: S1's
   facts, devices, primitives and device state relations, the agents, the static database,
   S3's regions and S4's spine, and RC's chains per receiver."
  (let* ((controls (control-facts))
         (route (hint-route-context controls))
         (receivers (census-type-instances 'receiver)))
    (list :controls controls
          :agents (sort (copy-list (census-type-instances 'agent)) #'string< :key #'symbol-name)
          :devices (mapcar #'third controls)
          :primitives (control-primitives controls)
          :relations (sort (remove-duplicates (mapcar #'first (device-state-axioms controls)))
                           #'string< :key #'symbol-name)
          :static (list-static-db)
          :names (getf route :names)
          :spine (getf route :spine)
          :chains (when receivers (hint-relay-chains receivers controls)))))


(defun from-here-sections (state context)
  "FH's four sections for STATE, each a list of lines."
  (let ((facts (from-here-facts state))
        (expansion (from-here-expansion state (getf context :agents))))
    (list (from-here-state-section state facts context)
          (from-here-reach-section state facts context expansion)
          (cons "  F2 placements [grade 1]" (from-here-placement-lines state facts context expansion))
          (from-here-beam-section state facts context))))


(defun from-here-source-state (source)
  "The state SOURCE names, and its source text: the staged start for NIL, a search
   checkpoint's endpoint, or an action list replayed from the staged start."
  (cond ((null source)
         (values (copy-problem-state *start-state*) "staged start (0 actions)"))
        ((search-checkpoint-p source)
         (values (search-checkpoint-state source)
                 (format nil "checkpoint endpoint (~D actions)"
                         (length (goal-chain-cumulative-path
                                   (goal-chain-session-phases (search-checkpoint-session source)))))))
        (t (let ((validation (validate-action-sequence (copy-problem-state *start-state*) source)))
             (unless (action-sequence-validation-success-p validation)
               (error "Prefix replay failed at action ~S: ~S~%REASON: ~A"
                      (action-sequence-validation-failure-index validation)
                      (action-sequence-validation-failure-action validation)
                      (action-sequence-validation-failure-reason validation)))
             (values (action-sequence-validation-final-state validation)
                     (format nil "action prefix (~D actions)"
                             (action-sequence-validation-action-count validation)))))))


(defun report-from-here (&optional source)
  "FH, grade 1, with grade 2 where marked.  The Phase 3 step 7 report at SOURCE's state (see
   FROM-HERE-SOURCE-STATE): F0 state, F1 moves and controllers, F2 placements, F3 beams.  A
   line not in the same section of the staged start's report is marked *."
  (let ((*print-pretty* nil)
        (context (from-here-context)))
    (multiple-value-bind (state text) (from-here-source-state source)
      (let ((sections (from-here-sections state context))
            (baseline (when source
                        (from-here-sections (copy-problem-state *start-state*) context))))
        (format t "~2%FH  FROM HERE  [grade 1; grade 2 where marked]~%")
        (format t "~A~%" (make-string 62 :initial-element #\-))
        (format t "  READING: every row reads this state, or the engine's one-step successors of ~
                   it (GENERATE-CHILDREN); no search runs.  Site lines and candidate chains are ~
                   S4 and RC readings in the physical view [grade 2].  A line ending in * does ~
                   not occur in the same section for the staged start.~%")
        (format t "  source: ~A~%" text)
        (loop for section in sections
              for index from 0
              do (terpri)
                 (dolist (line section)
                   (format t "~A~:[ *~;~]~%"
                           line
                           (or (null baseline)
                               (member line (nth index baseline) :test #'string=)))))
        (values)))))


;;;; CP -- CYCLE-PLAN CHECK (T24, I2) ;;;;
;;;
;;; Specification: doc/constraint-led-solving/Extractor-Specifications.md section 13.  Checks a
;;; stage plan the user states, as data, against the plate, body and view budgets before any
;;; action is written: B0 view, B1 plate budget (RO's matching), B2 control conflict (S1), B3
;;; beam (RC), B4 lift landing (CC's G15 rows).  No search runs and no state is evaluated.  CP
;;; is NOT part of the profile: REPORT-STATIC-CONSTRAINT-PROFILE does not call it, and the
;;; profile file does not change.  Call it after staging as
;;;
;;;   (report-cycle-plan-check <plan plist>)
;;;
;;; SUBSTRATE VOCABULARY (C3).  The relation ON, the type PRESSURE-PLATE, the control mode
;;; INVERTED, and the helpers of the sections it reads are named; each is a tech/ interface or
;;; this file's own.  No problem object name appears; every
;;; device, body, plate and segment comes from the staged databases or from the plan.
;;;
;;; T44 (section 13.8) adds a stage's :RESERVATIONS: roles a caller assigns to bodies over a
;;; phase range.  B5 checks each body's roles for compatibility, the supports, holders and
;;; gears they share, the eligible pool and releases; B1 then matches with per-plate
;;; eligibility.  The role rules read ON, HOLDING, JAMMING, MOUNTED-ON and HAS-LOCATION, the
;;; types they key, and MC's jammer sightline survey.  No reservation is ever inferred.


(defparameter *cycle-plan-label-order* '("PASS" "CONDITIONAL" "CONFLICT")
  "Labels from best to worst; a segment, stage or plan takes the worst of its parts.")


(defparameter *cycle-plan-family-grades*
  '((0 . "grade 1; S2") (1 . "grade 1 -> 2; S1 S2 RO") (2 . "grade 1; S1")
    (3 . "grade 2; S1 RC") (4 . "grade 1; CC")
    (5 . "grade 1 -> 2; S2 roles, functional keys, MC jammer survey"))
  "Each family's grade and sources, section 13.4 of the specification.")


(defun cycle-plan-worst-label (labels)
  "The worst of LABELS, PASS for none."
  (or (find-if (lambda (label) (member label labels :test #'string=))
               (reverse *cycle-plan-label-order*))
      "PASS"))


(defun cycle-plan-context ()
  "The static readings every segment shares, taken once: S1's control facts, exclusion pairs
   and primitive tiers, S2's layer pairs and ON pools, CC's G15 lift-barrier rows other than
   EQUIVALENCE, an empty cache for RC's chains, filled per receiver on first use, the static
   database, and a cache for MC's jammer survey (T44)."
  (let* ((controls (control-facts))
         (devices (mapcar #'third controls))
         (arcs (traversal-arc-facts))
         (placement (find 'on (placement-relations (functional-relation-entries)) :key #'first)))
    (list :controls controls
          :pressure-plates (census-type-instances 'pressure-plate)
          :exclusions (relay-chain-exclusion-pairs controls)
          :tiers (loop for primitive in (control-primitives controls)
                       collect (cons primitive (control-primitive-tier primitive devices)))
          :pairs (layer-pairs)
          :occupants (census-spec-extent (fifth placement))
          :supports (census-spec-extent (sixth placement))
          :lift-barriers (remove-if (lambda (entry)
                                      (string= "EQUIVALENCE"
                                               (coupling-pair-relation (second entry) (fourth entry))))
                                    (coupling-lift-barriers (coupling-fan-out controls) arcs
                                                            (coupling-role-table controls arcs nil)))
          :chains (make-hash-table :test #'eq)
          :static (list-static-db)
          :cache (make-hash-table :test #'eq))))


(defun cycle-plan-receiver-chains (receiver context)
  "RC's evaluated chains to RECEIVER, computed as NH computes them, once per report."
  (multiple-value-bind (chains found) (gethash receiver (getf context :chains))
    (if found
      chains
      (setf (gethash receiver (getf context :chains))
            (rest (first (hint-relay-chains (list receiver) (getf context :controls))))))))


(defun cycle-plan-primitive-kind (primitive context)
  ":PLATE for a pressure plate, :BEAM for a device-mediated primitive (S1 tier), else :SWITCH."
  (cond ((member primitive (getf context :pressure-plates)) :plate)
        ((eq :device-mediated (rest (assoc primitive (getf context :tiers)))) :beam)
        (t :switch)))


(defun cycle-plan-device-literals (device context)
  "DEVICE's literals as a list of (primitive polarity), polarity :ON or :OFF, when its S1 form
   is one conjunction of literals: a normal device with one clause, or an inverted device
   whose every clause is one primitive.  :ALTERNATIVES for any other form, which is not
   combined; :UNCONTROLLED when no CONTROLS fact names DEVICE."
  (let ((fact (find device (getf context :controls) :key #'third)))
    (cond ((null fact) :uncontrolled)
          ((eq (fourth fact) 'inverted)
           (if (every (lambda (clause) (= 1 (length clause))) (second fact))
             (mapcar (lambda (clause) (list (first clause) :off)) (second fact))
             :alternatives))
          ((= 1 (length (second fact)))
           (mapcar (lambda (primitive) (list primitive :on)) (first (second fact))))
          (t :alternatives))))


(defun cycle-plan-eligible-p (body segment context)
  "Whether BODY counts on a plate in SEGMENT's view (section 13.3): in the physical view every
   body present, ghosts only when present; in the recording view ghosts only, in an open cycle."
  (let ((class (census-layer-class body (getf context :pairs))))
    (if (eq (getf segment :view) :recording)
      (and (string= class "ghost") (eq (getf segment :cycle) :open))
      (or (string/= class "ghost") (eq (getf segment :ghosts) :present)))))


(defun cycle-plan-availability-known-p (segment)
  "Whether SEGMENT states its available witnesses as a list; () states that none are free.
   :UNKNOWN, and an absent key, are unknown, as in RO."
  (multiple-value-bind (indicator value) (get-properties segment '(:available-witnesses))
    (and indicator (listp value))))


(defun cycle-plan-plate-holders (plate segment active)
  "The bodies SEGMENT's :HELD puts on PLATE and those ACTIVE reservations give (:WEIGHT PLATE),
   in name order.  ACTIVE is a list of (reservation . tag)."
  (keeper-sorted-set (append (loop for entry in (getf segment :held)
                                   when (eq (second entry) plate)
                                     collect (first entry))
                             (loop for (reservation) in active
                                   when (equal (getf reservation :role) (list :weight plate))
                                     collect (getf reservation :body)))))


(defun cycle-plan-view-rows (segment context)
  "B0.  One CONFLICT row per inconsistency between SEGMENT's bodies and its view, or one PASS."
  (let* ((held (keeper-sorted-set (mapcar #'first (getf segment :held))))
         (off-plate (keeper-sorted-set (getf segment :off-plate)))
         (witnesses (when (consp (getf segment :available-witnesses))
                      (keeper-sorted-set (getf segment :available-witnesses))))
         (named (keeper-sorted-set (append held off-plate witnesses)))
         (pairs (getf context :pairs))
         (texts nil))
    (dolist (body named)
      (unless (member body (getf context :occupants))
        (push (format nil "~(~A~) is not in the ON pool" body) texts))
      (when (and (string= "ghost" (census-layer-class body pairs))
                 (eq (getf segment :ghosts) :absent))
        (push (format nil "~(~A~) is a ghost body but ghosts are absent" body) texts)))
    (when (and (eq (getf segment :cycle) :none) (eq (getf segment :ghosts) :present))
      (push "ghosts are present but no cycle is open" texts))
    (when (and (eq (getf segment :cycle) :none) (eq (getf segment :view) :recording))
      (push "the recording view needs an open cycle" texts))
    (when (eq (getf segment :view) :recording)
      (dolist (body (keeper-sorted-set (append held witnesses)))
        (unless (string= "ghost" (census-layer-class body pairs))
          (push (format nil "~(~A~) holds or witnesses in the recording view but is not a ghost"
                        body)
                texts))))
    (dolist (body named)
      (when (< 1 (count-if (lambda (group) (member body group)) (list held off-plate witnesses)))
        (push (format nil "~(~A~) is in two of held, off-plate and available" body) texts)))
    (if texts
      (mapcar (lambda (text) (list 0 "CONFLICT" text nil)) (nreverse texts))
      (list (list 0 "PASS" "bodies consistent with the view" nil)))))


(defun cycle-plan-gate-text (gate context)
  "GATE, then the plates every positive alternative of its S1 form needs, as NH prints them."
  (let ((fact (find gate (getf context :controls) :key #'third)))
    (format nil "~(~A~)~@[ (~(~{~A~^, ~}~))~]"
            gate
            (when fact
              (keeper-mandatory-plates
                (keeper-pressure-clauses fact (getf context :pressure-plates)))))))


(defun cycle-plan-beam-row (device primitive polarity segment context)
  "B3 for one device-mediated literal of DEVICE.  Returns the row and the gates RC's usable
   chains to PRIMITIVE all need, less the devices PRIMITIVE drives."
  (let ((note (list (list "note" "chains are not matched to bodies; a stacked riser is not in RC's stations"))))
    (cond ((eq polarity :off)
           (values (list 3 "CONDITIONAL"
                         (format nil "~(~A~) demands ~(~A~) inactive: not evaluated" device primitive)
                         nil)
                   nil))
          ((eq (getf segment :view) :recording)
           (values (list 3 "CONDITIONAL"
                         (format nil "~(~A~) needs ~(~A~): RC is physical-view only" device primitive)
                         nil)
                   nil))
          (t
           (let* ((chains (cycle-plan-receiver-chains primitive context))
                  (usable (remove-if-not (lambda (chain)
                                           (member (getf chain :class) '(:bootstrap :latch)))
                                         chains))
                  (bootstrap (remove-if-not (lambda (chain) (eq (getf chain :class) :bootstrap))
                                            usable))
                  (common (when usable
                            (keeper-sorted-set
                              (set-difference
                                (reduce #'intersection
                                        (mapcar (lambda (chain) (getf chain :gates)) usable))
                                (relay-chain-receiver-devices primitive (getf context :controls))))))
                  (text (format nil "~(~A~) needs ~(~A~): RC's usable chains ~:[share no gate~;all need ~:*~{~A~^, ~}~]"
                                device primitive
                                (mapcar (lambda (gate) (cycle-plan-gate-text gate context)) common)))
                  (off-plate (length (remove-duplicates (getf segment :off-plate)))))
             (cond ((null usable)
                    (values (list 3 "CONDITIONAL"
                                  (format nil "~(~A~) needs ~(~A~): no RC chains" device primitive)
                                  nil)
                            nil))
                   ((null bootstrap)
                    (values (list 3 "CONDITIONAL" (format nil "~A; no bootstrap chain" text) note)
                            common))
                   (t
                    (let ((least (reduce #'min bootstrap :key (lambda (chain) (getf chain :off-plate)))))
                      (values (list 3 (if (< off-plate least) "CONFLICT" "PASS")
                                    (format nil "~A; off-plate bodies ~D, least ~D" text off-plate least)
                                    note)
                              common)))))))))


(defun cycle-plan-beam-rows (segment context)
  "B3 rows for SEGMENT, and the (gate device) pairs they add to the requirement, in order."
  (let ((rows nil)
        (added nil))
    (dolist (device (getf segment :require))
      (let ((literals (cycle-plan-device-literals device context)))
        (when (consp literals)
          (dolist (literal literals)
            (when (eq :beam (cycle-plan-primitive-kind (first literal) context))
              (multiple-value-bind (row gates)
                  (cycle-plan-beam-row device (first literal) (second literal) segment context)
                (push row rows)
                (dolist (gate gates)
                  (unless (find gate added :key #'first)
                    (push (list gate device) added)))))))))
    (values (nreverse rows) (nreverse added))))


(defun cycle-plan-demands (segment added context)
  "Every plate and switch literal of SEGMENT's :REQUIRE devices and of the ADDED gates, as
   (primitive polarity device beam-device), in requirement order; and a CONDITIONAL B1 row
   for each device whose form is not one conjunction or is not controlled."
  (let ((demands nil)
        (rows nil))
    (dolist (entry (append (mapcar (lambda (device) (list device nil)) (getf segment :require))
                           added))
      (let ((literals (cycle-plan-device-literals (first entry) context))
            (label (format nil "~(~A~)~@[ (beam, ~(~A~))~]" (first entry) (second entry))))
        (cond ((eq literals :alternatives)
               (push (list 1 "CONDITIONAL" (format nil "~A: alternatives not combined" label) nil)
                     rows))
              ((eq literals :uncontrolled)
               (push (list 1 "CONDITIONAL" (format nil "~A: no control aggregate declared" label) nil)
                     rows))
              (t
               (dolist (literal literals)
                 (unless (eq :beam (cycle-plan-primitive-kind (first literal) context))
                   (push (list (first literal) (second literal) (first entry) (second entry))
                         demands)))))))
    (values (nreverse demands) (nreverse rows))))


(defun cycle-plan-free-witnesses (segment active context)
  "SEGMENT's stated available witnesses eligible in its view, less its held and off-plate
   bodies and the bodies ACTIVE reserves, in name order."
  (keeper-sorted-set
    (remove-if (lambda (body)
                 (or (not (cycle-plan-eligible-p body segment context))
                     (member body (mapcar #'first (getf segment :held)))
                     (member body (getf segment :off-plate))
                     (find body active :key (lambda (entry) (getf (first entry) :body)))))
               (getf segment :available-witnesses))))


(defun cycle-plan-reservation-spans (stage)
  "Each of STAGE's reservations with the indices of the first and last segment it covers, as
   (reservation from through); by default the whole stage.  An unknown role, a phase naming no
   segment of STAGE, or a phase ending before it begins signals an error (section 13.8)."
  (let ((ids (mapcar (lambda (segment) (getf segment :id)) (getf stage :segments))))
    (loop for reservation in (getf stage :reservations)
          for from = (if (getf reservation :from)
                       (position (getf reservation :from) ids :test #'string=)
                       0)
          for through = (if (getf reservation :through)
                          (position (getf reservation :through) ids :test #'string=)
                          (1- (length ids)))
          do (unless (member (first (getf reservation :role)) '(:weight :jam :place :hold :mount :support))
               (error "Unknown role in reservation ~S" reservation))
             (unless (and from through (<= from through))
               (error "Reservation phase names no segment of stage ~A, or ends before it begins: ~S"
                      (getf stage :id) reservation))
          collect (list reservation from through))))


(defun cycle-plan-phase-reservations (spans ids index)
  "The reservations of SPANS covering segment INDEX, and those that ended at the segment
   before it, each as (reservation . tag), the tag naming its purpose and phase range by the
   segment IDS.  Returns (values active released)."
  (let ((active nil)
        (released nil))
    (dolist (span spans)
      (destructuring-bind (reservation from through) span
        (let ((entry (cons reservation
                           (format nil "~@[~A; ~]~A..~A"
                                   (getf reservation :purpose) (nth from ids) (nth through ids)))))
          (when (<= from index through)
            (push entry active))
          (when (= through (1- index))
            (push entry released)))))
    (values (nreverse active) (nreverse released))))


(defun cycle-plan-role-text (role)
  "ROLE in words."
  (ecase (first role)
    (:weight (format nil "weight ~(~A~)" (second role)))
    (:jam (format nil "jam ~(~A~)~@[ at ~(~A~)~]" (second role) (third role)))
    (:place (format nil "place at ~(~A~)" (second role)))
    (:hold (format nil "held by ~(~A~)" (second role)))
    (:mount (format nil "mounted on ~(~A~)" (second role)))
    (:support (format nil "supports ~(~A~)" (second role)))
    (:off-plate "off plates")))


(defun cycle-plan-role-location (role context)
  "The location ROLE fixes: a weighted plate's or floor/angled gears' HAS-POSITION, a jam's or
   placement's stated location, :NONE for wall gears (MOUNT-FAN gives a wall-mounted fan no
   HAS-LOCATION), else NIL."
  (case (first role)
    (:weight (keeper-fact-value 'has-position (second role) (getf context :static)))
    (:jam (third role))
    (:place (second role))
    (:mount (if (member (second role) (census-type-instances 'wall-gears))
              :none
              (keeper-fact-value 'has-position (second role) (getf context :static))))))


(defun cycle-plan-role-type-reason (body role context)
  "C7: why BODY's type, or the type of ROLE's argument, does not admit ROLE, or NIL."
  (let ((argument (second role)))
    (case (first role)
      (:weight (cond ((not (member body (getf context :occupants))) "not in the ON occupant pool")
                     ((not (member argument (getf context :pressure-plates)))
                      (format nil "~(~A~) is not a pressure plate" argument))))
      (:jam (cond ((not (member body (census-type-instances 'jammer))) "not a jammer")
                  ((not (member argument (census-type-instances 'target)))
                   (format nil "~(~A~) is not a jam target" argument))
                  ((and (third role) (not (member (third role) (census-type-instances 'location))))
                   (format nil "~(~A~) is not a location" (third role)))))
      (:place (cond ((not (member body (census-type-instances 'mobile-object))) "not a mobile object")
                    ((not (member argument (census-type-instances 'location)))
                     (format nil "~(~A~) is not a location" argument))))
      (:hold (cond ((not (member body (census-type-instances 'cargo))) "not cargo")
                   ((not (member argument (census-type-instances 'agent)))
                    (format nil "~(~A~) is not an agent" argument))))
      (:mount (cond ((not (member body (census-type-instances 'fan))) "not a fan")
                    ((not (member argument (census-type-instances 'gears)))
                     (format nil "~(~A~) is not gears" argument))))
      (:support (cond ((not (member body (getf context :supports))) "not in the support pool")
                      ((not (member argument (getf context :occupants)))
                       (format nil "~(~A~) is not in the ON occupant pool" argument)))))))


(defun cycle-plan-body-conflicts (body roles context)
  "C1-C7 for BODY's ROLES in one segment (section 13.8), as texts in rule order; NIL when the
   roles are compatible."
  (let* ((kinds (mapcar #'first roles))
         (tray (member body (census-type-instances 'tray)))
         (locations (remove-duplicates
                      (remove nil (mapcar (lambda (role) (cycle-plan-role-location role context)) roles))))
         (texts nil))
    (when (and (member :hold kinds)
               (or (intersection kinds '(:weight :jam :mount))
                   (and (member :place kinds) (not tray))))
      (push "C1 held and stationary at once" texts))
    (when (< 1 (length locations))
      (push (format nil "C2 locations differ: ~(~{~A~^, ~}~)" locations) texts))
    (dolist (kind '(:weight :jam :hold :mount))
      (let ((values (remove-duplicates (loop for role in roles
                                             when (eq (first role) kind)
                                               collect (second role)))))
        (when (< 1 (length values))
          (push (format nil "C3 ~(~A~) names ~(~{~A~^, ~}~); its relation holds one" kind values) texts))))
    (when (and (member :mount kinds) (member :weight kinds))
      (push "C4 a mounted fan rests on no support" texts))
    (when (member :support kinds)
      (cond ((and (member :hold kinds) (not tray))
             (push "C5 a held body other than a tray supports nothing" texts))
            ((and tray (not (member :hold kinds)))
             (push "C5 a tray supports only while held" texts))
            ((find-if (lambda (role)
                        (and (eq (first role) :mount)
                             (member (second role) (census-type-instances 'wall-gears))))
                      roles)
             (push "C5 a wall-mounted fan supports nothing" texts))))
    (when (and (member :off-plate kinds) (member :weight kinds))
      (push "C6 committed off plates and weighting a plate" texts))
    (dolist (role roles)
      (let ((reason (cycle-plan-role-type-reason body role context)))
        (when reason
          (push (format nil "C7 ~A: ~A" (cycle-plan-role-text role) reason) texts))))
    (nreverse texts)))


(defun cycle-plan-jam-sites (jammer target context)
  "MC's surveyed sites from which JAMMER sees TARGET, as (location support gates), GATES being
   those whose single closure blocks the sightline.  The survey runs once per report."
  (let* ((cache (getf context :cache))
         (rows (or (gethash :jam-rows cache)
                   (setf (gethash :jam-rows cache) (jammer-sightline-rows)))))
    (getf (find-if (lambda (row) (and (eq (getf row :jammer) jammer) (eq (getf row :target) target)))
                   rows)
          :sites)))


(defun cycle-plan-jam-results (body roles segment context)
  "The sightline of each jam role of BODY whose location ROLES fix, as (label text).  A plate
   site is surveyed exactly; at another location a visible surveyed site gives PASS and none
   CONDITIONAL; the recording view is CONDITIONAL, since the survey is physical."
  (let ((plate (second (find :weight roles :key #'first)))
        (location (find-if (lambda (value) (and value (not (eq value :none))))
                           (mapcar (lambda (role) (cycle-plan-role-location role context)) roles))))
    (loop for role in roles
          when (and (eq (first role) :jam)
                    location
                    (member body (census-type-instances 'jammer))
                    (member (second role) (census-type-instances 'target)))
            collect (let* ((target (second role))
                           (sites (remove-if-not (lambda (site)
                                                   (and (eq (first site) location)
                                                        (or (null plate) (eq (second site) plate))))
                                                 (cycle-plan-jam-sites body target context))))
                      (cond ((eq (getf segment :view) :recording)
                             (list "CONDITIONAL"
                                   (format nil "jam ~(~A~) from ~(~A~): MC's sightline survey is physical"
                                           target location)))
                            (sites
                             (list "PASS"
                                   (format nil "jam ~(~A~) sighted from ~(~A~)~{~A~}" target location
                                           (mapcar (lambda (site)
                                                     (format nil " on ~(~A~)~:[~; if ~(~{~A~^, ~}~) open~]"
                                                             (second site) (third site) (third site)))
                                                   sites))))
                            (plate
                             (list "CONFLICT"
                                   (format nil "jam ~(~A~): no sightline from ~(~A~) on ~(~A~) in MC's survey"
                                           target location plate)))
                            (t
                             (list "CONDITIONAL"
                                   (format nil "jam ~(~A~): no surveyed site at ~(~A~) sees it; moved supports are not surveyed"
                                           target location))))))))


(defun cycle-plan-plate-eligibility (body roles plate segment context)
  "Whether reserved BODY, committed to ROLES, may also weight PLATE (section 13.8): eligible in
   the view, the roles with (:WEIGHT PLATE) added compatible, and no jam sightline refuted from
   PLATE.  Returns (values eligible premises), the premises being the jam results' texts."
  (let ((extended (cons (list :weight plate) roles)))
    (if (or (not (cycle-plan-eligible-p body segment context))
            (cycle-plan-body-conflicts body extended context))
      (values nil nil)
      (let ((results (cycle-plan-jam-results body extended segment context)))
        (if (find "CONFLICT" results :key #'first :test #'string=)
          (values nil nil)
          (values t (mapcar #'second results)))))))


(defun cycle-plan-commitments (segment active)
  "Per body, its commitments in SEGMENT as (body (role tag) ...), in body name order: ACTIVE
   reservations' roles, :HELD entries as (:WEIGHT plate) tagged held, and :OFF-PLATE bodies as
   (:OFF-PLATE) tagged off-plate."
  (let ((table nil))
    (dolist (entry (append (mapcar (lambda (entry)
                                     (list (getf (first entry) :body) (getf (first entry) :role) (rest entry)))
                                   active)
                           (mapcar (lambda (held) (list (first held) (list :weight (second held)) "held"))
                                   (getf segment :held))
                           (mapcar (lambda (body) (list body (list :off-plate) "off-plate"))
                                   (getf segment :off-plate))))
      (let ((row (assoc (first entry) table)))
        (if row
          (setf (rest row) (append (rest row) (list (rest entry))))
          (push (list (first entry) (rest entry)) table))))
    (sort table #'string< :key (lambda (row) (symbol-name (first row))))))


(defun cycle-plan-plate-rows (segment demands active context)
  "B1 rows for the required plates, in plate name order: a pinned plate by its holders, the
   rest matched injectively with RO's allocation to the free witnesses and to the reserved
   bodies whose roles admit that plate (section 13.8); then a CONFLICT for each plate
   demanded empty that is held or also required."
  (let* ((required (remove-if-not (lambda (demand)
                                    (and (eq :plate (cycle-plan-primitive-kind (first demand) context))
                                         (eq :on (second demand))))
                                  demands))
         (plates (keeper-sorted-set (mapcar #'first required)))
         (unpinned (remove-if (lambda (plate) (cycle-plan-plate-holders plate segment active)) plates))
         (known (cycle-plan-availability-known-p segment))
         (free (when known (cycle-plan-free-witnesses segment active context)))
         (commitments (cycle-plan-commitments segment active))
         (reserved (keeper-sorted-set (mapcar (lambda (entry) (getf (first entry) :body)) active)))
         (eligibility (mapcar (lambda (plate)
                                (cons plate
                                      (append (copy-list free)
                                              (loop for body in reserved
                                                    when (cycle-plan-plate-eligibility
                                                           body (mapcar #'first (rest (assoc body commitments)))
                                                           plate segment context)
                                                      collect body))))
                              unpinned))
         (perfect (and known (role-perfect-p unpinned eligibility)))
         (violator (when (and known (not perfect)) (role-hall-violator unpinned eligibility)))
         (rows nil))
    (dolist (plate plates)
      (let* ((demand (find plate required :key #'first))
             (label (format nil "~(~A~) for ~(~A~)~@[ (beam, ~(~A~))~]"
                            plate (third demand) (fourth demand)))
             (holders (cycle-plan-plate-holders plate segment active))
             (ineligible (remove-if (lambda (body) (cycle-plan-eligible-p body segment context))
                                    holders)))
        (push (cond ((and holders ineligible)
                     (list 1 "CONFLICT"
                           (format nil "~A: held by ~(~{~A~^, ~}~); not eligible in the ~(~A~) view: ~(~{~A~^, ~}~)"
                                   label holders (getf segment :view) ineligible)
                           nil))
                    (holders
                     (list 1 "PASS" (format nil "~A: held by ~(~{~A~^, ~}~)" label holders) nil))
                    ((not known)
                     (list 1 "CONDITIONAL" (format nil "~A: matched; availability unknown" label) nil))
                    (perfect
                     (list 1 "PASS" (format nil "~A: matched" label) nil))
                    (t
                     (let ((witnesses (keeper-sorted-set (role-neighbourhood violator eligibility))))
                       (list 1 "CONFLICT"
                             (format nil "~A: matched; shortage, violator ~(~{~A~^, ~}~) against ~D witness~:[es~;~]~@[ (~(~{~A~^, ~}~))~]~:[~;; for the supplied reservations, refutes this allocation only~]"
                                     label (keeper-sorted-set violator) (length witnesses)
                                     (= 1 (length witnesses)) witnesses active)
                             nil))))
              rows)))
    (dolist (demand demands)
      (when (and (eq :plate (cycle-plan-primitive-kind (first demand) context))
                 (eq :off (second demand))
                 (or (cycle-plan-plate-holders (first demand) segment active)
                     (member (first demand) plates)))
        (push (list 1 "CONFLICT"
                    (format nil "~(~A~) demanded empty by ~(~A~)~@[: held by ~(~{~A~^, ~}~)~]~:[~;; also required~]"
                            (first demand) (third demand)
                            (cycle-plan-plate-holders (first demand) segment active)
                            (member (first demand) plates))
                    nil)
              rows)))
    (nreverse rows)))


(defun cycle-plan-control-rows (demands context)
  "B2.  A CONFLICT for each primitive demanded both ways, naming the two devices and S1's
   exclusion pair when there is one; else one PASS row."
  (let ((rows nil))
    (dolist (primitive (keeper-sorted-set (mapcar #'first demands)))
      (let ((on (find-if (lambda (demand) (and (eq (first demand) primitive) (eq (second demand) :on)))
                         demands))
            (off (find-if (lambda (demand) (and (eq (first demand) primitive) (eq (second demand) :off)))
                          demands))
            (plate (eq :plate (cycle-plan-primitive-kind primitive context))))
        (when (and on off)
          (let ((pair (find-if (lambda (pair)
                                 (null (set-exclusive-or pair (list (third on) (third off)))))
                               (getf context :exclusions))))
            (push (list 2 "CONFLICT"
                        (format nil "~(~A~) demanded ~A by ~(~A~) and ~A by ~(~A~)~@[; S1 EXCLUSION {~(~{~A~^, ~}~)}~]"
                                primitive (if plate "held" "on") (third on)
                                (if plate "empty" "off") (third off) pair)
                        nil)
                  rows)))))
    (if rows
      (nreverse rows)
      (list (list 2 "PASS" "no primitive demanded both ways" nil)))))


(defun cycle-plan-landing-rows (previous segment context)
  "B4.  For each CC G15 row whose lift PREVIOUS requires and whose barrier SEGMENT requires,
   the toggle between them: CONDITIONAL with checklist 2.2's question when SEGMENT states no
   :LANDING, CONFLICT when the landing is not in S2's support pool, else PASS."
  (let ((rows nil))
    (when previous
      (dolist (entry (getf context :lift-barriers))
        (destructuring-bind (primitive lift destination barrier exits) entry
          (declare (ignore exits))
          (when (and (member (third lift) (getf previous :require))
                     (member (third barrier) (getf segment :require)))
            (let ((head (if (string= "EXCLUSION" (coupling-pair-relation lift barrier))
                          (format nil "after ~A: ~(~A~) stops ~(~A~) (lift to ~(~A~)) and opens ~(~A~) (G15 FLAG)"
                                  (getf previous :id) primitive (third lift) destination (third barrier))
                          (format nil "after ~A: ~(~A~) drives ~(~A~) (lift to ~(~A~)) and ~(~A~) (G15 CHECK)"
                                  (getf previous :id) primitive (third lift) destination (third barrier))))
                  (landing (getf segment :landing)))
              (push (cond ((null landing)
                           (list 4 "CONDITIONAL" (format nil "~A; no landing stated" head)
                                 (list (list "question"
                                             (format nil "in the successor after the toggle, is the launch support at ~(~A~) still present?"
                                                     destination)))))
                          ((not (member landing (getf context :supports)))
                           (list 4 "CONFLICT" (format nil "~A; landing ~(~A~) is not a support" head landing)
                                 nil))
                          (t
                           (list 4 "PASS" (format nil "~A; landing ~(~A~) is a support" head landing)
                                 (list (list "note" "where the landing stands is not evaluated")))))
                    rows)))))
      (nreverse rows))))


(defun cycle-plan-capacity-rows (commitments reserved segment context)
  "B5's K1-K4 over every commitment of SEGMENT: an occupant on two supports or two contending
   occupants on one (contention per SUPPORT-OCCUPANCY-CONFLICT-P, by S2's layer classes), an
   agent holding two bodies, gears with two fans, and a RESERVED ghost while ghosts are absent.
   One CONFLICT row per violation, else one PASS."
  (let ((on nil)
        (holds nil)
        (mounts nil)
        (texts nil)
        (pairs (getf context :pairs)))
    (dolist (row commitments)
      (dolist (entry (rest row))
        (let ((role (first entry)))
          (case (first role)
            (:weight (pushnew (list (first row) (second role)) on :test #'equal))
            (:support (pushnew (list (second role) (first row)) on :test #'equal))
            (:hold (pushnew (list (second role) (first row)) holds :test #'equal))
            (:mount (pushnew (list (second role) (first row)) mounts :test #'equal))))))
    (dolist (occupant (keeper-sorted-set (mapcar #'first on)))
      (let ((supports (keeper-sorted-set (loop for (body support) in on
                                               when (eq body occupant) collect support))))
        (when (< 1 (length supports))
          (push (format nil "K1 ~(~A~) is on ~(~{~A~^, ~}~); ON holds one support" occupant supports)
                texts))))
    (dolist (support (keeper-sorted-set (mapcar #'second on)))
      (let ((occupants (keeper-sorted-set (loop for (body place) in on
                                                when (eq place support) collect body))))
        (loop for (occupant . others) on occupants
              do (dolist (other others)
                   (unless (member (list (census-layer-class occupant pairs) (census-layer-class other pairs))
                                   '(("live" "ghost") ("ghost" "live")) :test #'equal)
                     (push (format nil "K1 ~(~A~) and ~(~A~) contend for ~(~A~)" occupant other support)
                           texts))))))
    (dolist (agent (keeper-sorted-set (mapcar #'first holds)))
      (let ((bodies (keeper-sorted-set (loop for (holder body) in holds
                                             when (eq holder agent) collect body))))
        (when (< 1 (length bodies))
          (push (format nil "K2 ~(~A~) holds ~(~{~A~^, ~}~); HOLDING is bijective" agent bodies) texts))))
    (dolist (gears (keeper-sorted-set (mapcar #'first mounts)))
      (let ((fans (keeper-sorted-set (loop for (place fan) in mounts
                                           when (eq place gears) collect fan))))
        (when (< 1 (length fans))
          (push (format nil "K3 ~(~A~) carries ~(~{~A~^, ~}~); MOUNT-FAN needs vacant gears" gears fans)
                texts))))
    (when (eq (getf segment :ghosts) :absent)
      (dolist (body reserved)
        (when (string= "ghost" (census-layer-class body pairs))
          (push (format nil "K4 ~(~A~) is reserved but ghosts are absent" body) texts))))
    (if texts
      (mapcar (lambda (text) (list 5 "CONFLICT" text nil)) (nreverse texts))
      (list (list 5 "PASS" "supports, holders and gears not overcommitted" nil)))))


(defun cycle-plan-body-row (body entries segment context)
  "B5's row for reserved BODY, whose commitments are ENTRIES, each (role tag): its roles, SHARED
   when of two or more kinds, and each C1-C7 and jam sightline result; the worst label."
  (let* ((roles (mapcar #'first entries))
         (kinds (remove-duplicates (remove :off-plate (mapcar #'first roles))))
         (conflicts (cycle-plan-body-conflicts body roles context))
         (jams (cycle-plan-jam-results body roles segment context)))
    (list 5
          (cycle-plan-worst-label (append (when conflicts (list "CONFLICT")) (mapcar #'first jams)))
          (format nil "~(~A~): ~{~A~^; ~}~:[~; -- SHARED~]~{; ~A~}~{; ~A~}"
                  body
                  (mapcar (lambda (entry) (format nil "~A [~A]" (cycle-plan-role-text (first entry)) (second entry)))
                          entries)
                  (< 1 (length kinds)) conflicts (mapcar #'second jams))
          (when (find :jam roles :key #'first)
            (list (list "note" "JAM-DISALLOWED> depends on the agent's location; not evaluated"))))))


(defun cycle-plan-pool-rows (segment demands active commitments reserved context)
  "B5's pool rows: per reserved body, the unpinned required plates it is eligible for with
   their jam premises, or reserved off plates; then the free witnesses."
  (let* ((plates (keeper-sorted-set
                   (loop for demand in demands
                         when (and (eq :plate (cycle-plan-primitive-kind (first demand) context))
                                   (eq :on (second demand)))
                           collect (first demand))))
         (unpinned (remove-if (lambda (plate) (cycle-plan-plate-holders plate segment active)) plates)))
    (if (null unpinned)
      (list (list 5 "PASS" "pool: no unpinned plate required" nil))
      (append
        (loop for body in reserved
              collect (let ((texts (loop for plate in unpinned
                                         append (multiple-value-bind (eligible premises)
                                                    (cycle-plan-plate-eligibility
                                                      body (mapcar #'first (rest (assoc body commitments)))
                                                      plate segment context)
                                                  (when eligible
                                                    (list (format nil "~(~A~)~@[ (~{~A~^; ~})~]" plate premises)))))))
                        (list 5 "PASS"
                              (if texts
                                (format nil "pool: ~(~A~) eligible for ~{~A~^, ~}" body texts)
                                (format nil "pool: ~(~A~) reserved off plates" body))
                              nil)))
        (list (list 5 "PASS"
                    (if (cycle-plan-availability-known-p segment)
                      (format nil "pool: free witnesses ~:[none~;~:*~(~{~A~^, ~}~)~]"
                              (cycle-plan-free-witnesses segment active context))
                      "pool: free witnesses unknown")
                    nil))))))


(defun cycle-plan-reservation-rows (segment previous demands active released context)
  "B5 for SEGMENT (section 13.8): a row per reserved body, the capacity rows, the pool rows,
   then a row per reservation RELEASED after PREVIOUS.  A release is not simultaneous
   availability: the body counts here only as SEGMENT states it."
  (let* ((commitments (cycle-plan-commitments segment active))
         (reserved (keeper-sorted-set (mapcar (lambda (entry) (getf (first entry) :body)) active))))
    (append
      (loop for body in reserved
            collect (cycle-plan-body-row body (rest (assoc body commitments)) segment context))
      (cycle-plan-capacity-rows commitments reserved segment context)
      (cycle-plan-pool-rows segment demands active commitments reserved context)
      (loop for (reservation . tag) in released
            for body = (getf reservation :body)
            collect (list 5 "PASS"
                          (format nil "released after ~A: ~(~A~) ~A [~A]; ~:[not stated available here~;~:*~A here~]"
                                  (getf previous :id) body (cycle-plan-role-text (getf reservation :role)) tag
                                  (cond ((member body reserved) "reserved again")
                                        ((and (listp (getf segment :available-witnesses))
                                              (member body (getf segment :available-witnesses)))
                                         "listed available")))
                          nil)))))


(defun cycle-plan-segment-rows (segment previous active released context)
  "Every row for SEGMENT, in family order.  B3 runs first, since the gates it adds join B1 and
   B2; PREVIOUS is the segment before it in the same stage, or NIL.  ACTIVE and RELEASED are
   the stage's reservations covering SEGMENT and ended at PREVIOUS; B5 prints only when there
   is one."
  (multiple-value-bind (beam-rows added) (cycle-plan-beam-rows segment context)
    (multiple-value-bind (demands conditional-rows) (cycle-plan-demands segment added context)
      (let ((plate-rows (append (cycle-plan-plate-rows segment demands active context) conditional-rows)))
        (append (cycle-plan-view-rows segment context)
                (or plate-rows (list (list 1 "PASS" "no plate required" nil)))
                (cycle-plan-control-rows demands context)
                beam-rows
                (cycle-plan-landing-rows previous segment context)
                (when (or active released)
                  (cycle-plan-reservation-rows segment previous demands active released context)))))))


(defun cycle-plan-results (plan context)
  "Per stage, (stage . ((segment . rows) ...)), in plan order."
  (loop for stage in (getf plan :stages)
        collect (cons stage
                      (let ((spans (cycle-plan-reservation-spans stage))
                            (ids (mapcar (lambda (segment) (getf segment :id)) (getf stage :segments))))
                        (loop for segment in (getf stage :segments)
                              for index from 0
                              for previous = nil then current
                              for current = segment
                              collect (multiple-value-bind (active released)
                                          (cycle-plan-phase-reservations spans ids index)
                                        (cons segment
                                              (cycle-plan-segment-rows segment previous active released
                                                                       context))))))))


(defun report-cycle-plan-row (row)
  "One row, then its notes."
  (format t "      B~D  ~A  [~A]  ~A~%"
          (first row) (second row) (rest (assoc (first row) *cycle-plan-family-grades*)) (third row))
  (dolist (note (fourth row))
    (format t "        ~A  ~A~%" (first note) (second note))))


(defun report-cycle-plan-stage (stage entries)
  "A stage line, then each segment line and its rows."
  (format t "~%  stage ~A  ~A  ~A~%"
          (getf stage :id)
          (cycle-plan-worst-label (loop for entry in entries append (mapcar #'second (rest entry))))
          (getf stage :intent))
  (dolist (entry entries)
    (let ((segment (first entry)))
      (format t "    segment ~A  view ~(~A~)  cycle ~(~A~)  ghosts ~(~A~)  ~A~%"
              (getf segment :id) (getf segment :view) (getf segment :cycle) (getf segment :ghosts)
              (cycle-plan-worst-label (mapcar #'second (rest entry))))
      (dolist (row (rest entry))
        (report-cycle-plan-row row)))))


(defun report-cycle-plan-check (plan)
  "CP, grade per row.  Checks PLAN, a stage plan stated as data (section 13.2), against the
   plate, body and view budgets: B0 view, B1 plate budget, B2 control conflict, B3 beam, B4
   lift landing.  A PASS is not a plan witness; a CONFLICT rejects the stated budget only."
  (let* ((*print-pretty* nil)
         (context (cycle-plan-context))
         (results (cycle-plan-results plan context))
         (segment-labels (loop for (nil . entries) in results
                               append (mapcar (lambda (entry)
                                                (cycle-plan-worst-label (mapcar #'second (rest entry))))
                                              entries))))
    (format t "~2%CP  CYCLE-PLAN CHECK  [grade per row]~%")
    (format t "~A~%" (make-string 62 :initial-element #\-))
    (format t "  READING: checks a stated stage plan against plate, body and view budgets.  ~
               PASS says no budget forbids the segment; it is not a plan witness.  ~
               CONFLICT rejects the stated budget, not the stage's idea.  ~
               Per-plate eligibility beyond the view (reach, elevation, history) is not evaluated.~%")
    (format t "~%  plan ~A~%" (getf plan :name))
    (format t "    provenance ~A~%" (getf plan :provenance))
    (format t "    stages ~D~%" (length (getf plan :stages)))
    (format t "  segments (~D): ~D PASS, ~D CONDITIONAL, ~D CONFLICT~%"
            (length segment-labels)
            (count "PASS" segment-labels :test #'string=)
            (count "CONDITIONAL" segment-labels :test #'string=)
            (count "CONFLICT" segment-labels :test #'string=))
    (dolist (result results)
      (report-cycle-plan-stage (first result) (rest result)))
    (format t "~%  plan ~A~%" (cycle-plan-worst-label segment-labels))
    (values)))


(defun report-static-constraint-profile (&optional scenario)
  "Runs every extractor written so far and prints the whole static profile.  Callers who
   want the file write it with WRITE-STATIC-CONSTRAINT-PROFILE; this reporter only prints,
   so each extractor can also be run and scored on its own."
  (format t "~2%STATIC CONSTRAINT PROFILE -- problem ~(~A~)~%" *problem-name*)
  (format t "generated by tech/constraint-profile.lisp -- NEVER HAND-EDITED~%")
  (report-mechanic-coverage)
  (report-type-extent-census)
  (report-control-algebra)
  (report-functional-relation-census)
  (report-budget-arithmetic)
  (report-height-and-reach-lattice)
  (report-beam-sightline-table)
  (report-relay-chain-table scenario)
  (report-landmark-graph-and-orderings)
  (report-region-quotient)
  (report-cut-keeper-table)
  (report-coupling-census)
  (report-necessity-hints scenario)
  (report-service-dependencies)
  (format t "RO requires an explicit segment input and is not generated; call~%")
  (format t "REPORT-ROLE-OBLIGATIONS with a stated scenario.~%")
  (values))


(defun write-static-constraint-profile (pathname &optional scenario)
  "Writes the profile to PATHNAME, superseding whatever is there.  The caller names the
   file because the profile is domain-general while its home is the problem's own
   documentation folder.
   WRITTEN THROUGH A TEMPORARY AND RENAMED ONLY ON SUCCESS.  :SUPERSEDE truncates the
   target before the generator has produced a byte, so an extractor that errors, or fails
   to terminate, leaves no profile at all.  That happened on 7.20's first attempt, and the
   only surviving copy was a backup taken by hand for an unrelated diff.  M2 says the
   profile is regenerable and must never be hand-edited; REGENERABLE IS NOT THE SAME AS
   RECOVERABLE, and this rename is what makes the two agree."
  (let* ((pathname (merge-pathnames pathname))
         (temporary (make-pathname :type "tmp" :defaults pathname)))
    (with-open-file (stream temporary :direction :output
                                      :if-exists :supersede
                                      :if-does-not-exist :create)
      (let ((*standard-output* stream))
        (report-static-constraint-profile scenario)))
    (when (probe-file pathname)
      (delete-file pathname))
    (rename-file temporary pathname))
  pathname)

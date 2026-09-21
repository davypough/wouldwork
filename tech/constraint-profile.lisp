;;; Filename: constraint-profile.lisp

;;; Static constraint profile extractors for the constraint-led analysis method
;;; (doc/problems/<problem>/Constraint-Continuation-Prompt.txt).  The profile is a pure
;;; function of a staged problem: it reads the databases the engine has already built and
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
;;; list.  S0, the type extent census, is not in section 3 of the problem's
;;; Constraint-Prediction-Register.txt: it closes G2 of Constraint-Schema-Gaps.txt and runs
;;; as step 0, since every later extractor needs the emptiness facts before it may read a
;;; control aggregate as an axiom.  S4 adds the qualified cut-keeper table under the
;;; approved interpretation in register 7.23.  RO, the role-obligation analysis, adds
;;; conditional allocation for one caller-stated segment: it is deliberately NOT S5-S7,
;;; does not amend their sealed specifications, and is not run by the whole-profile
;;; reporter, because a generated file must not carry a segment nobody stated.
;;; S5-S7 are not yet written.

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
   signature rather than being told a name (A18).  The mode position and the fluent family
   position each occur once and are passed over."
  (let ((signature (gethash *traversal-symmetric-relation* *static-relations*)))
    (find-if (lambda (spec)
               (and (census-spec-type-names spec)
                    (= 2 (count spec signature :test #'equal))))
             signature)))


(defun traversal-signature-layout ()
  "Where each part of a traversal proposition sits, as (mode-position family-position
   source-position destination-position), all 1-based into the signature and therefore
   directly usable as NTH into a proposition, whose first element is the relation name.
   Computed rather than assumed: the endpoint type is the repeated one, the family is the
   relation's single fluent, and the mode is whatever declared type is left."
  (let* ((signature (gethash *traversal-symmetric-relation* *static-relations*))
         (endpoint-type (traversal-endpoint-type))
         (fluents (gethash *traversal-symmetric-relation* *fluent-relation-indices*))
         (endpoints nil)
         (mode-position nil))
    (loop for spec in signature
          for position from 1
          do (cond ((equal spec endpoint-type) (push position endpoints))
                   ((member position fluents))
                   ((census-spec-type-names spec) (setf mode-position position))))
    (setf endpoints (nreverse endpoints))
    (list mode-position (first fluents) (first endpoints) (second endpoints))))


(defun traversal-arc-facts ()
  "Every traversal edge in the static database, normalized to (relation mode source family
   destination) whatever order the signature declares.  Authored and derived edges arrive
   identically: -walkability-coordinates derives the walking edges from raw segment
   geometry during initialization and asserts them as ordinary propositions of these two
   relations, so the extractor reads two relations and never the geometry behind them.
   A symmetric arc has its endpoints put in name order, so the two spellings of one
   undirected crossing collapse to one entry whether or not the engine stored a mirror; a
   directed arc keeps the order it was asserted in."
  (let ((layout (traversal-signature-layout))
        (arcs nil))
    (dolist (fact (list-static-db))
      (when (and (consp fact)
                 (member (first fact) (list *traversal-symmetric-relation*
                                            *traversal-directed-relation*)))
        (let ((source (nth (third layout) fact))
              (destination (nth (fourth layout) fact)))
          (when (and (eq (first fact) *traversal-symmetric-relation*)
                     (string> (symbol-name source) (symbol-name destination)))
            (rotatef source destination))
          (pushnew (list (first fact) (nth (first layout) fact) source
                         (nth (second layout) fact) destination)
                   arcs :test #'equal))))
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
  "Step 3.  One row per (from, to, mode, family, directedness), carrying the count of
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
  "Step 1.  What was read, by relation and by mode, before any contraction.  Printed first
   so a reader can tell an empty quotient caused by an empty input from one caused by total
   contraction."
  (format t "~%  traversal arcs read (~D)~%" (length arcs))
  (format t "    ~D symmetric (~(~A~)), ~D directed (~(~A~))~%"
          (count *traversal-symmetric-relation* arcs :key #'first)
          *traversal-symmetric-relation*
          (count *traversal-directed-relation* arcs :key #'first)
          *traversal-directed-relation*)
  (dolist (mode (sort (remove-duplicates (mapcar #'second arcs))
                      #'string< :key #'symbol-name))
    (let ((of-mode (remove-if-not (lambda (arc) (eq mode (second arc))) arcs)))
      (format t "    mode ~(~A~): ~D arc~:P, ~D with an empty family~%"
              mode (length of-mode) (count-if #'null of-mode :key #'fourth)))))


(defun report-region-blocks (blocks arcs endpoints)
  "Step 3's first half.  The contraction's blocks, with the rule that produced them stated
   in the output as step 2 requires, and with the qualification that makes the blocks
   readable: this is a DOOR-COST quotient and not a reachability quotient."
  (format t "~%  contraction rule: two endpoints share a region when an arc of ~(~A~) ~
             joins them with an EMPTY clause family.  Arcs of ~(~A~) are never contracted, ~
             whatever their family.~%"
          *traversal-symmetric-relation* *traversal-directed-relation*)
  (format t "  NOTE: a region is a set of endpoints NO DOOR separates.  Each mode carries ~
             its own predicate -- an elevation equality, a jump rule -- which this ~
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
   row is an irreducible crossing and belongs to the adjacency spine S4 iterates."
  (let* ((limit (quotient-row-doors row))
         (others (remove row rows :test #'equal))
         (seen (quotient-reachable-door-sets (first row) others limit))
         (exact nil)
         (cheaper nil))
    (loop for entry being the hash-keys of seen
          do (when (string= (car entry) (second row))
               (if (null (set-exclusive-or limit (cdr entry)))
                 (setf exact t)
                 (setf cheaper (cdr entry)))))
    (cond (cheaper (list :non-minimal cheaper))
          (exact (list :composed nil))
          (t (list :spine nil)))))


(defun report-quotient-arcs (rows)
  "Step 3's second half.  Every crossing between two regions, with its clause family, its
   mode, its directedness, how many location arcs stand behind it, and whether it is part
   of the adjacency SPINE or a COMPOSITION of spine rows.
   THE DISTINCTION IS THE POINT.  The coordinate derivation emits a minimal door-set for
   every LOCATION PAIR, so these rows are a transitive closure and not an adjacency list:
   most of them are compositions, and a reader counting them as doorways counts the same
   door many times.  The spine is printed again on its own below, because that is what a
   keeper or stranding analysis has to iterate."
  (let ((classified (loop for row in rows
                          collect (cons row (quotient-row-composition row rows)))))
    (format t "~%  region crossings (~D rows: ~D spine, ~D composed)~%"
            (length classified)
            (count :spine classified :key #'second)
            (count :composed classified :key #'second))
    (format t "    NOTE: these rows are the transitive CLOSURE, one minimal door-set per ~
               location pair, not an adjacency list.  Count doorways from the spine.~%")
    (dolist (entry classified)
      (let ((row (first entry)))
        (format t "    ~A ~A ~A  mode ~(~A~)  family ~(~A~)  ~D location arc~:P  ~A~@[ via ~(~A~)~]~%"
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
          (format t "    ~A ~A ~A  mode ~(~A~)  family ~(~A~)~%"
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


(defun keeper-row-avoids-p (row device)
  "Traversal NIL means direct; otherwise one DNF alternative avoiding DEVICE suffices."
  (or (null device) (null (fourth row))
      (some (lambda (clause) (not (member device clause))) (fourth row))))


(defun keeper-row-next (row region)
  "Directedness is preserved, including for door-free and climbing edges."
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


(defun keeper-spine (rows regions devices)
  "Return S3's spine and NIL, or NIL and a reason its use cannot support S4's analysis.
   S3's composition test flattens families, so multi-clause input is explicitly unresolved.
   Compare reachability with the closure to catch simultaneous removal of redundant rows."
  (when (some (lambda (row) (> (length (fourth row)) 1)) rows)
    (return-from keeper-spine (values nil :alternative-families)))
  (let ((spine (remove-if-not
                (lambda (row) (eq :spine (first (quotient-row-composition row rows)))) rows)))
    (dolist (device (cons nil devices))
      (dolist (region regions)
        (unless (equal (keeper-reachable region rows device)
                       (keeper-reachable region spine device))
          (return-from keeper-spine
            (values nil (list :reachability-mismatch region device))))))
    (values spine nil)))


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
               (format t "      spine mode ~(~A~), family ~(~S~)~%" (third row) (fourth row))
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
      (dolist (fact controls)
        (report-keeper-device fact spine reason facts names axioms pressure-plates))
      (report-keeper-destinations (get 'goal-fn :form) (database *start-state*)
                                  names spine reason devices))
    (report-keeper-supply)
    (format t "~%  UNCONDITIONAL STRANDING: unresolved; no verdict emitted.~%")
    (values)))


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
    (report-keeper-axioms (third fact) axioms)
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


(defun report-static-constraint-profile ()
  "Runs every extractor written so far and prints the whole static profile.  Callers who
   want the file write it with WRITE-STATIC-CONSTRAINT-PROFILE; this reporter only prints,
   so each extractor can also be run and scored on its own."
  (format t "~2%STATIC CONSTRAINT PROFILE -- problem ~(~A~)~%" *problem-name*)
  (format t "generated by tech/constraint-profile.lisp -- NEVER HAND-EDITED~%")
  (report-type-extent-census)
  (report-control-algebra)
  (report-functional-relation-census)
  (report-region-quotient)
  (report-cut-keeper-table)
  (format t "~2%S5-S7 not yet written.~%")
  (format t "RO requires an explicit segment input and is not generated; call~%")
  (format t "REPORT-ROLE-OBLIGATIONS with a stated scenario.~%")
  (values))


(defun write-static-constraint-profile (pathname)
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
        (report-static-constraint-profile)))
    (when (probe-file pathname)
      (delete-file pathname))
    (rename-file temporary pathname))
  pathname)

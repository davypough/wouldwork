;;; Filename: ww-init-validator.lisp

;;; Engine support for validation over raw DEFINE-INIT entries.


(in-package :ww)


(define-condition init-check-failure (simple-error)
  ((check
     :initarg :check
     :reader init-check-failure-check)
   (literal
     :initarg :literal
     :initform nil
     :reader init-check-failure-literal))
  (:report
    (lambda (condition stream)
      (format stream "~%Initialization check ~S failed."
              (init-check-failure-check condition))
      (when (init-check-failure-literal condition)
        (format stream "~%Literal: ~S"
                (init-check-failure-literal condition)))
      (format stream "~%")
      (apply #'format
             stream
             (simple-condition-format-control condition)
             (simple-condition-format-arguments condition)))))


(defvar *current-init-check* nil
  "Initialization check currently being executed.")


(defun validate-init-literals (literals &key (checks *init-checks*))
  "Check engine invariants, then run staged technology checks over raw literals."
  (check-init-general-consistency literals)
  (run-init-checks literals checks))


(defun register-init-check (check &optional consumed-types)
  "Register CHECK and the object types consumed only through its raw list payloads."
  (register-problem-function check)
  (when (member check *init-checks*)
    (error "Initialization check is registered more than once: ~S" check))
  (setf (get check :init-check-consumed-types) consumed-types)
  (setf *init-checks*
        (append *init-checks* (list check)))
  check)


(defun register-init-literal-generator (generator)
  "Register GENERATOR to derive additional DEFINE-INIT literals from the problem's declared
   types.  A technology whose identity mapping is fully determined by the problem's own type
   declarations can derive that mapping here instead of requiring every problem to restate
   it.  Generators run before CHECK-PROPOSITION and before any registered check, so derived
   tuples reach relation indexes and initialization checks exactly as authored ones do."
  (register-problem-function generator)
  (when (member generator *init-literal-generators*)
    (error "Initialization literal generator is registered more than once: ~S" generator))
  (setf *init-literal-generators*
        (append *init-literal-generators* (list generator)))
  generator)


(defun generate-init-literals (literals)
  "Return LITERALS extended with whatever the registered generators derive, in registration
   order.  Each generator sees every literal derived before it, so one generator can decline
   to duplicate a tuple another already supplied."
  (dolist (generator *init-literal-generators* literals)
    (setf literals (append literals (funcall generator literals)))))


(defun register-init-literal-defaults (relation &rest defaults)
  "Declare DEFAULTS for RELATION's trailing arguments, letting a DEFINE-INIT literal omit
   them.  DEFAULTS are given in argument order and cover the last (LENGTH DEFAULTS)
   positions of RELATION's signature; a literal may stop anywhere inside that suffix but
   nowhere before it.  Padding runs ahead of CHECK-PROPOSITION and of every registered
   check, so an omitted argument is type-checked, validated, and stored exactly as a
   written one.  The technology owning RELATION registers its defaults beside the
   relation's own declaration; the engine attaches no meaning to any particular value."
  (let ((signature (init-relation-signature relation)))
    (unless (consp signature)
      (error "Init-literal defaults must name a declared relation with typed arguments: ~S"
             relation))
    (unless (<= (length defaults) (length signature))
      (error "Relation ~A takes ~D argument~:P, but ~D default~:P were registered for it."
             relation (length signature) (length defaults)))
    (when (nth-value 1 (gethash relation *init-literal-defaults*))
      (error "Init-literal defaults are registered more than once for relation ~A."
             relation))
    (setf (gethash relation *init-literal-defaults*) defaults)
    relation))


(defun pad-init-literal (literal)
  "Return LITERAL with any omitted trailing arguments supplied from its relation's
   registered defaults."
  (if (and (consp literal)
           (eql (car literal) 'not))
    (list 'not (pad-init-proposition (second literal)))
    (pad-init-proposition literal)))


(defun pad-init-proposition (proposition)
  "Return PROPOSITION extended with the registered defaults for whichever trailing
   arguments it omits.  A relation with no registered defaults is returned unchanged, so
   the fluentless lookup keys CHECK-PROPOSITION tolerates elsewhere are unaffected."
  (let ((defaults (gethash (car proposition) *init-literal-defaults*)))
    (when (null defaults)
      (return-from pad-init-proposition proposition))
    (let* ((arity (length (init-relation-signature (car proposition))))
           (missing (- arity (length (cdr proposition)))))
      (unless (<= 0 missing (length defaults))
        (error "~%The DEFINE-INIT literal ~S supplies ~D of ~A's ~D arguments.~%~
                Only its last ~D argument~:P may be omitted."
               proposition (length (cdr proposition)) (car proposition)
               arity (length defaults)))
      (append proposition (last defaults missing)))))


(defun run-init-checks (literals &optional (checks *init-checks*))
  "Run registered initialization checks in declaration order."
  (dolist (check checks)
    (let ((*current-init-check* check))
      (funcall check literals))))


(defun fail-init-check (literal control &rest arguments)
  "Signal a named authoring failure from the current initialization check."
  (error 'init-check-failure
         :check *current-init-check*
         :literal literal
         :format-control control
         :format-arguments arguments))


(defun check-init-general-consistency (literals)
  "Check initialization invariants owned by the planning engine."
  (check-init-duplicate-fluent-keys literals)
  (check-init-no-derived-facts literals))


(defun init-literal-proposition (literal)
  "Return the proposition inside a positive or negative initialization literal."
  (if (and (consp literal)
           (eql (car literal) 'not))
    (second literal)
    literal))


(defun init-literals-with-relation (relation literals)
  "Return positive and negative LITERALS whose proposition names RELATION."
  (remove-if-not (lambda (literal)
                   (let ((proposition (init-literal-proposition literal)))
                     (and (consp proposition)
                          (eql (car proposition) relation))))
                 literals))


(defun positive-init-literal-p (literal)
  "Return true unless LITERAL is an explicit negation."
  (not (and (consp literal)
            (eql (car literal) 'not))))


(defun positive-init-literals-with-relation (relation literals)
  "Return the positive LITERALS whose proposition names RELATION."
  (remove-if-not #'positive-init-literal-p
                 (init-literals-with-relation relation literals)))


(defun check-init-duplicate-fluent-keys (literals)
  "Reject repeated fluent storage keys before installation can overwrite one.  A
   bijective relation stores two independent indices, one per argument position --
   the canonical relation alone has no non-fluent position left to discriminate on,
   so each index is checked separately against its own key space."
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (literal literals)
      (let* ((proposition (init-literal-proposition literal))
             (index-names (gethash (car proposition) *bijective-relations*)))
        (if index-names
          (dolist (index-name index-names)
            (check-init-fluent-key literal (cons index-name (cdr proposition)) seen))
          (check-init-fluent-key literal proposition seen))))))


(defun check-init-fluent-key (literal proposition seen)
  "Record PROPOSITION's fluent storage key for LITERAL in SEEN, or signal a collision
   with a prior literal already occupying that key."
  (let ((fluent-indices (get-prop-fluent-indices proposition)))
    (when fluent-indices
      (let ((key (get-fluentless-prop proposition fluent-indices)))
        (ut::if-it (gethash key seen)
          (error "~%Duplicate DEFINE-INIT fluent key.~%~
                  First literal:  ~S~%~
                  Second literal: ~S~%~
                  Storage key:    ~S~%~
                  Only one fluent value can be stored for this key."
                 ut::it literal key)
          (setf (gethash key seen) literal))))))


(defun check-init-no-derived-facts (literals)
  "Reject facts whose relations are computed during initialization."
  (dolist (literal literals)
    (let* ((proposition (init-literal-proposition literal))
           (relation (car proposition)))
      (when (gethash relation *derived-relations*)
        (error "~%DEFINE-INIT contains a derived fact.~%~
                Literal: ~S~%~
                Relation ~S is derived during initialization; remove it from DEFINE-INIT."
               literal relation)))))


(defun init-literal-map (relation literals key-index value-index)
  "Build a raw initialization map for two fields of RELATION propositions."
  (let ((map (make-hash-table :test #'equal)))
    (dolist (literal (init-literals-with-relation relation literals))
      (let ((proposition (init-literal-proposition literal)))
        (setf (gethash (nth key-index proposition) map)
              (nth value-index proposition))))
    map))


(defun init-relation-signature (relation)
  "Return RELATION's installed dynamic or static signature."
  (or (gethash relation *relations*)
      (gethash relation *static-relations*)))


(defun init-relation-argument-type (relation index)
  "Return the one-based argument type at INDEX in RELATION's signature."
  (let ((signature (init-relation-signature relation)))
    (when (and (listp signature)
               (<= 1 index (length signature)))
      (nth (1- index) signature))))


(defun init-relation-fluent-indices (relation)
  "Return the installed fluent argument indices for RELATION."
  (gethash relation *fluent-relation-indices*))


(defun init-type-instances (type)
  "Return the installed instances of TYPE."
  (remove nil (gethash type *types*)))


(defun init-type-components (type)
  "Return the declared component types of a composite TYPE."
  (gethash type *type-components*))


(defun init-type-member-p (object type)
  "Return true when OBJECT is an installed instance of TYPE."
  (member object (init-type-instances type)))


(defun init-member-of-any-type-p (object types)
  "Return true when OBJECT is an installed instance of any member of TYPES."
  (some (lambda (type)
          (init-type-member-p object type))
        types))


(defun init-type-spec-member-p (object type-spec)
  "Return true when OBJECT belongs to an atomic or (EITHER ...) TYPE-SPEC."
  (cond ((symbolp type-spec)
         (init-type-member-p object type-spec))
        ((and (consp type-spec)
              (eql (car type-spec) 'either))
         (init-member-of-any-type-p object (cdr type-spec)))))


(defun init-type-spec-includes-type-p (type-spec type)
  "Return true when TYPE-SPEC directly names TYPE."
  (cond ((eql type-spec type) t)
        ((and (consp type-spec)
              (eql (car type-spec) 'either))
         (member type (cdr type-spec)))))


(defun init-check-list-items-have-types (literal items types)
  "Reject any member of ITEMS that belongs to none of TYPES."
  (dolist (item items)
    (unless (init-member-of-any-type-p item types)
      (fail-init-check literal
        "Invalid item ~S in list; expected an instance of one of ~S."
        item types))))


(defun check-init-list-relation-items-have-types
    (literals relation types)
  "Check the list-valued second argument of every raw RELATION literal."
  (dolist (literal (init-literals-with-relation relation literals))
    (init-check-list-items-have-types
      literal
      (third (init-literal-proposition literal))
      types)))


(defun init-check-dnf-list-items-have-types (literal clauses types)
  "Reject malformed DNF CLAUSES or members that belong to none of TYPES."
  (unless (listp clauses)
    (fail-init-check literal "Expected a list of alternative clauses, got ~S." clauses))
  (dolist (clause clauses)
    (unless (listp clause)
      (fail-init-check literal "Expected a list clause, got ~S." clause))
    (init-check-list-items-have-types literal clause types)))

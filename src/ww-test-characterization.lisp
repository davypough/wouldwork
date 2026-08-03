;;; Filename: ww-test-characterization.lisp

;;; Named, attributed claims for technology characterization problems.


(in-package :ww)


(define-condition test-claim-failure (simple-error)
  ((claim
     :initarg :claim
     :reader test-claim-failure-claim)
   (clause
     :initarg :clause
     :reader test-claim-failure-clause))
  (:report
    (lambda (condition stream)
      (format stream "~%Test claim ~S failed.~%Clause: ~S~%"
              (test-claim-failure-claim condition)
              (test-claim-failure-clause condition))
      (apply #'format
             stream
             (simple-condition-format-control condition)
             (simple-condition-format-arguments condition)))))


(defstruct test-mutation
  name
  target
  note
  install-thunk)


(defvar *current-test-claim* nil
  "Characterization claim currently being evaluated.")


(defvar *current-test-clause* nil
  "Clause currently being evaluated within *CURRENT-TEST-CLAIM*.")


(defvar *requested-test-mutation* nil
  "Mutation selected by the test harness for the current staging operation.")


(defvar *test-mutation-applied* nil
  "Name of the requested mutation installed while the problem was loaded.")


(defun fail-test-claim (control &rest arguments)
  "Signal an attributed failure from the current characterization clause."
  (error 'test-claim-failure
         :claim *current-test-claim*
         :clause *current-test-clause*
         :format-control control
         :format-arguments arguments))


(defun register-test-claim (claim)
  "Register a problem-owned characterization CLAIM in declaration order."
  (register-problem-function claim)
  (when (member claim *test-claims* :test #'eq)
    (error "Test claim is registered more than once: ~S" claim))
  (setf *test-claims* (append *test-claims* (list claim)))
  claim)


(defun register-test-mutation (mutation)
  "Register MUTATION and install it when selected by the test harness."
  (let ((name (test-mutation-name mutation)))
    (when (find name *test-mutations* :key #'test-mutation-name :test #'eq)
      (error "Test mutation is registered more than once: ~S" name))
    (setf *test-mutations* (append *test-mutations* (list mutation)))
    (when (eq name *requested-test-mutation*)
      (when *test-mutation-applied*
        (error "More than one test mutation was applied: ~S and ~S"
               *test-mutation-applied* name))
      (funcall (test-mutation-install-thunk mutation))
      (setf *test-mutation-applied* name)))
  mutation)


(defun run-test-claims (&optional (claims *test-claims*))
  "Run registered characterization claims in declaration order."
  (dolist (claim claims)
    (let ((*current-test-claim* claim)
          (*current-test-clause* nil))
      (funcall claim)))
  t)


(defmacro define-test-helper (name lambda-list &body body)
  "Define a test-owned Lisp helper removed when another problem is staged."
  `(define-problem-helper ,name ,lambda-list
     ,@body))


(defmacro define-test-claim (name &body clauses)
  "Define a named characterization whose individual CLAUSES must return true."
  `(progn
     (defun ,name ()
       ,@(mapcar
           (lambda (clause)
             `(let ((*current-test-clause* ',clause))
                (unless ,clause
                  (fail-test-claim "Expression returned NIL."))))
           clauses)
       t)
     (register-test-claim ',name)))


(defmacro define-query-mutation (name target args body note)
  "Declare a deliberately broken query definition owned by this test problem."
  `(register-test-mutation
     (make-test-mutation
       :name ',name
       :target ',target
       :note ,note
       :install-thunk
         (lambda ()
           (install-query ',target ',args ',body)))))


(defmacro define-update-mutation (name target args body note)
  "Declare a deliberately broken update definition owned by this test problem."
  `(register-test-mutation
     (make-test-mutation
       :name ',name
       :target ',target
       :note ,note
       :install-thunk
         (lambda ()
           (install-update ',target ',args ',body)))))


(defmacro define-action-precondition-mutation
    (name action precondition note)
  "Declare a deliberately broken action precondition owned by this test problem."
  `(register-test-mutation
     (make-test-mutation
       :name ',name
       :target ',action
       :note ,note
       :install-thunk
         (lambda ()
           (install-action-precondition-mutation ',action ',precondition)))))


(defun install-action-precondition-mutation (action-name new-precondition)
  "Replace ACTION-NAME's precondition lambda before staged compilation."
  (let* ((action (or (find action-name *actions* :key #'action.name)
                     (error "Cannot mutate undefined action: ~S" action-name)))
         (pre-params (action.precondition-params action)))
    (multiple-value-bind (pre-param-?vars pre-param-types)
        (dissect-pre-params pre-params)
      (let* ((flat-pre-param-?vars (alexandria:flatten pre-param-?vars))
             (*var-type-env*
               (append
                 (mapcar #'cons
                         flat-pre-param-?vars
                         (flatten-param-types pre-param-types))
                 *var-type-env*))
             (pre-$vars
               (remove-if-not #'$varp (action.precondition-variables action)))
             (pre-special-$vars
               (get-special-vars (action.precondition-form action)))
             (eff-args
               (append flat-pre-param-?vars pre-$vars pre-special-$vars)))
        (setf (action.precondition-lambda action)
              (build-precondition-lambda
                action-name
                new-precondition
                pre-param-?vars
                pre-$vars
                eff-args)))))
  action-name)


(defun expect-type-instances (type expected)
  "Require TYPE to have exactly the EXPECTED installed instances."
  (multiple-value-bind (actual presentp)
      (gethash type *types*)
    (unless presentp
      (fail-test-claim "Type ~S is not declared." type))
    (unless (same-members-p actual expected)
      (fail-test-claim "Type ~S has instances ~S; expected ~S."
                       type actual expected)))
  t)


(defun expect-type-components (type expected)
  "Require composite TYPE to have exactly EXPECTED component types."
  (multiple-value-bind (actual presentp)
      (gethash type *type-components*)
    (unless presentp
      (fail-test-claim "Composite type ~S is not declared." type))
    (unless (same-members-p actual expected)
      (fail-test-claim "Composite type ~S has components ~S; expected ~S."
                       type actual expected)))
  t)


(defun expect-type-absent (type)
  "Require TYPE not to be installed."
  (when (nth-value 1 (gethash type *types*))
    (fail-test-claim "Type ~S is unexpectedly declared as ~S."
                     type (gethash type *types*)))
  t)


(defun expect-empty-type (type)
  "Require TYPE to be declared with no non-NIL instances."
  (multiple-value-bind (actual presentp)
      (gethash type *types*)
    (unless presentp
      (fail-test-claim "Empty type ~S is not declared." type))
    (when (remove nil actual)
      (fail-test-claim "Type ~S has instances ~S; expected none." type actual)))
  t)


(defun expect-type-instance (type instance)
  "Require TYPE to contain INSTANCE."
  (unless (member instance (gethash type *types*) :test #'equal)
    (fail-test-claim "Type ~S does not contain instance ~S; installed instances are ~S."
                     type instance (gethash type *types*)))
  t)


(defun expect-type-component (type component)
  "Require composite TYPE to contain COMPONENT."
  (unless (member component (gethash type *type-components*) :test #'eq)
    (fail-test-claim "Composite type ~S does not contain ~S; components are ~S."
                     type component (gethash type *type-components*)))
  t)


(defun expect-types (expected)
  "Require the complete installed type roster to equal EXPECTED."
  (let ((actual
          (loop for type being the hash-keys of *types*
                collect type)))
    (unless (same-members-p actual expected)
      (fail-test-claim "Installed types are ~S; expected ~S." actual expected)))
  t)


(defun expect-types-disjoint (left-type right-type)
  "Require LEFT-TYPE and RIGHT-TYPE to have no installed instance in common."
  (let ((overlap
          (intersection (gethash left-type *types*)
                        (gethash right-type *types*)
                        :test #'equal)))
    (when overlap
      (fail-test-claim "Types ~S and ~S overlap at ~S."
                       left-type right-type overlap)))
  t)


(defun relation-table-for-kind (kind)
  "Return the installed relation table selected by KIND."
  (ecase kind
    (:dynamic *relations*)
    (:static *static-relations*)))


(defun other-relation-table-for-kind (kind)
  "Return the relation table opposite KIND."
  (ecase kind
    (:dynamic *static-relations*)
    (:static *relations*)))


(defun expect-relation-schema
    (relation kind signature &key (fluent-indices nil fluent-indices-p))
  "Require RELATION to have KIND, SIGNATURE, and optionally FLUENT-INDICES."
  (let* ((table (relation-table-for-kind kind))
         (other-table (other-relation-table-for-kind kind))
         (actual-signature (gethash relation table))
         (presentp (nth-value 1 (gethash relation table)))
         (wrong-kind-p (nth-value 1 (gethash relation other-table))))
    (unless presentp
      (fail-test-claim "Relation ~S is not registered as ~S." relation kind))
    (when wrong-kind-p
      (fail-test-claim "Relation ~S is also registered with the opposite kind."
                       relation))
    (unless (equal actual-signature signature)
      (fail-test-claim "Relation ~S has signature ~S; expected ~S."
                       relation actual-signature signature))
    (when (and fluent-indices-p
               (not (equal (gethash relation *fluent-relation-indices*)
                           fluent-indices)))
      (fail-test-claim "Relation ~S has fluent indices ~S; expected ~S."
                       relation
                       (gethash relation *fluent-relation-indices*)
                       fluent-indices)))
  t)


(defun expect-relation-kind (relation kind)
  "Require RELATION to be registered only with KIND."
  (let ((table (relation-table-for-kind kind))
        (other-table (other-relation-table-for-kind kind)))
    (unless (nth-value 1 (gethash relation table))
      (fail-test-claim "Relation ~S is not registered as ~S." relation kind))
    (when (nth-value 1 (gethash relation other-table))
      (fail-test-claim "Relation ~S is also registered with the opposite kind."
                       relation)))
  t)


(defun expect-relation-absent (relation &optional kind)
  "Require RELATION to be absent from KIND, or from both kinds when KIND is NIL."
  (when (and (member kind '(nil :dynamic))
             (nth-value 1 (gethash relation *relations*)))
    (fail-test-claim "Dynamic relation ~S is unexpectedly registered." relation))
  (when (and (member kind '(nil :static))
             (nth-value 1 (gethash relation *static-relations*)))
    (fail-test-claim "Static relation ~S is unexpectedly registered." relation))
  t)


(defun relation-names (kind)
  "Return the installed relation names for KIND."
  (loop for relation being the hash-keys of (relation-table-for-kind kind)
        collect relation))


(defun same-members-p (left right)
  "Return true when LEFT and RIGHT contain the same elements with equal multiplicity."
  (and (= (length left) (length right))
       (null (set-exclusive-or left right :test #'equal))))


(defun expect-relations (kind expected)
  "Require KIND's complete installed relation roster to equal EXPECTED."
  (let ((actual (relation-names kind)))
    (unless (same-members-p actual expected)
      (fail-test-claim "~S relations are ~S; expected ~S."
                       kind actual expected)))
  t)


(defun registration-names (kind)
  "Return the names registered in the selected extension category."
  (ecase kind
    (:query *query-names*)
    (:update *update-names*)
    (:action (mapcar #'action.name *actions*))
    (:init-action (mapcar #'action.name *init-actions*))
    (:solution-printer *solution-report-printers*)
    (:solution-validator *solution-validators*)))


(defun expect-registered (kind name)
  "Require NAME to be registered in extension category KIND."
  (unless (member name (registration-names kind) :test #'eq)
    (fail-test-claim "~S ~S is not registered." kind name))
  t)


(defun expect-not-registered (kind name)
  "Require NAME not to be registered in extension category KIND."
  (when (member name (registration-names kind) :test #'eq)
    (fail-test-claim "~S ~S is unexpectedly registered." kind name))
  t)


(defun expect-registrations (kind expected)
  "Require KIND's complete registration roster to equal EXPECTED in order."
  (let ((actual (registration-names kind)))
    (unless (equal actual expected)
      (fail-test-claim "~S registrations are ~S; expected ~S."
                       kind actual expected)))
  t)


(defun expect-condition
    (operation expected-type &key containing (check nil checkp))
  "Require OPERATION to signal EXPECTED-TYPE with optional text and init-check identity."
  (let ((condition
          (handler-case
              (progn
                (funcall operation)
                nil)
            (error (signaled-condition)
              signaled-condition))))
    (unless condition
      (fail-test-claim "Expected a condition of type ~S, but none was signaled."
                       expected-type))
    (unless (typep condition expected-type)
      (fail-test-claim "Signaled ~S; expected a condition of type ~S."
                       condition expected-type))
    (when (and containing
               (not (search containing (princ-to-string condition))))
      (fail-test-claim "Condition ~S does not contain ~S."
                       condition containing))
    (when (and checkp
               (not (and (typep condition 'init-check-failure)
                         (eql (init-check-failure-check condition) check))))
      (fail-test-claim "Condition ~S was not attributed to init check ~S."
                       condition check)))
  t)

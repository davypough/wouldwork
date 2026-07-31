;;; Filename: problem-query-update-parameter-tests.lisp

;;; Focused tests for the DEFINE-QUERY/DEFINE-UPDATE parameter-list language:
;;;
;;;   - every formal parameter is a ?variable
;;;   - each parameter may independently carry a Wouldwork object type
;;;   - the returned type list stays positionally aligned, using NIL for an untyped slot
;;;   - action-domain headers and query-valued domains do not belong in function signatures
;;;   - $variables remain runtime/local variables, not query/update formal parameters
;;;   - a nonempty typed update accepts, executes, and records a correctly typed call
;;;   - the same update rejects a literal belonging to a different object type
;;;
;;; Run from the Wouldwork REPL with:
;;;
;;;   (stage query-update-parameter-tests)
;;;   (solve)
;;;
(in-package :ww)


(ww-set *problem-name* query-update-parameter-tests)

(ww-set *problem-type* planning)

(ww-set *solution-type* first)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 1)


;;;; TYPES ;;;;


(define-types
  query-parameter-test-object-a (query-parameter-test-a1)
  query-parameter-test-object-b (query-parameter-test-b1))


(define-optional-types query-parameter-test-empty)


;;;; RELATIONS ;;;;


(define-dynamic-relations
  (query-parameter-tests-ready)
  (query-parameter-tests-passed)
  (query-parameter-object-update-ran query-parameter-test-object-a))


(define-static-relations
  (query-parameter-test-pair query-parameter-test-object-a query-parameter-test-object-b)
  (query-parameter-test-empty-relation query-parameter-test-empty))


;;;; PARAMETER-LIST TESTS ;;;;


(defun query-parameter-parse-values (parameters)
  "Return DISSECT-QUERY-PARAMS' two values as a list for convenient comparison."
  (multiple-value-list (dissect-query-params parameters)))


(defun query-parameter-check-equal (description expected actual)
  "Signal a focused test failure unless EXPECTED and ACTUAL are EQUAL."
  (unless (equal expected actual)
    (error "~A~%Expected: ~S~%Actual:   ~S" description expected actual))
  t)


(defun query-parameter-check-error (description thunk)
  "Signal a focused test failure unless THUNK signals an error."
  (let ((condition
          (handler-case
              (progn (funcall thunk) nil)
            (error (error-condition) error-condition))))
    (unless condition
      (error "~A~%Expected an error, but none was signaled." description))
    t))


(defun run-query-parameter-success-tests ()
  "Exercise every accepted query/update signature shape."
  (query-parameter-check-equal
    "An empty signature has no variables or type slots."
    '(nil nil)
    (query-parameter-parse-values nil))
  (query-parameter-check-equal
    "A bare signature receives one NIL type slot per parameter."
    '((?first ?second) (nil nil))
    (query-parameter-parse-values '(?first ?second)))
  (query-parameter-check-equal
    "A fully typed signature preserves its positional object types."
    '((?first ?second)
      (query-parameter-test-object-a query-parameter-test-object-b))
    (query-parameter-parse-values
      '(?first query-parameter-test-object-a
        ?second query-parameter-test-object-b)))
  (query-parameter-check-equal
    "An untyped parameter may precede a typed parameter."
    '((?value ?object)
      (nil query-parameter-test-object-a))
    (query-parameter-parse-values
      '(?value ?object query-parameter-test-object-a)))
  (query-parameter-check-equal
    "An untyped parameter may occur between typed parameters."
    '((?first ?value ?second)
      (query-parameter-test-object-a nil query-parameter-test-object-b))
    (query-parameter-parse-values
      '(?first query-parameter-test-object-a
        ?value
        ?second query-parameter-test-object-b)))
  (query-parameter-check-equal
    "An untyped parameter may follow a typed parameter."
    '((?object ?value)
      (query-parameter-test-object-a nil))
    (query-parameter-parse-values
      '(?object query-parameter-test-object-a ?value)))
  (query-parameter-check-equal
    "An inline EITHER remains the declared type of one parameter."
    '((?object)
      ((either query-parameter-test-object-a query-parameter-test-object-b)))
    (query-parameter-parse-values
      '(?object
        (either query-parameter-test-object-a query-parameter-test-object-b))))
  (query-parameter-check-equal
    "A known empty optional object type remains a valid annotation."
    '((?object)
      (query-parameter-test-empty))
    (query-parameter-parse-values
      '(?object query-parameter-test-empty)))
  (query-parameter-check-equal
    "An installed null query retains an empty object type and an untyped value slot."
    '(query-parameter-test-empty nil)
    (get 'query-parameter-null-hook :param-types))
  (query-parameter-check-equal
    "An installed null update retains an empty object type and an untyped value slot."
    '(query-parameter-test-empty nil)
    (get 'query-parameter-null-update! :param-types))
  (check-query/update-call '(query-parameter-boolean-hook nil))
  (check-query/update-call '(query-parameter-boolean-hook t))
  (check-query/update-call '(query-parameter-lisp-value-hook :test-value))
  (check-query/update-call
    '(query-parameter-object-hook query-parameter-test-a1))
  (check-query/update-call
    '(query-parameter-either-hook query-parameter-test-a1))
  (check-query/update-call
    '(query-parameter-either-hook query-parameter-test-b1))
  (check-query/update-call
    '(query-parameter-object-hook
       (identity query-parameter-test-a1)))
  (check-query/update-call
    '(query-parameter-object-update! query-parameter-test-a1))
  t)


(defun run-query-parameter-rejection-tests ()
  "Exercise forms that do not belong in query/update signatures."
  (query-parameter-check-error
    "A $variable is not a query/update formal parameter."
    (lambda () (dissect-query-params '($value))))
  (query-parameter-check-error
    "Action-domain headers do not belong in query/update signatures."
    (lambda ()
      (dissect-query-params
        '(standard ?object query-parameter-test-object-a))))
  (pushnew 'query-parameter-test-domain *query-names*)
  (unwind-protect
      (query-parameter-check-error
        "A query-valued action domain does not belong in a function signature."
        (lambda ()
          (dissect-query-params
            '(?object (query-parameter-test-domain)))))
    (setf *query-names*
          (remove 'query-parameter-test-domain *query-names*)))
  (query-parameter-check-error
    "A misspelled or unknown object type is rejected at its parameter."
    (lambda ()
      (dissect-query-params
        '(?object query-parameter-test-unknown))))
  (query-parameter-check-error
    "Every signature item must begin a ?parameter or describe the preceding one."
    (lambda ()
      (dissect-query-params
        '(query-parameter-test-object-a))))
  (query-parameter-check-error
    "A literal planning object must belong to the parameter's declared type."
    (lambda ()
      (check-query/update-call
        '(query-parameter-object-hook query-parameter-test-b1))))
  (query-parameter-check-error
    "Quoting a literal does not bypass its declared object type."
    (lambda ()
      (check-query/update-call
        '(query-parameter-object-hook
           'query-parameter-test-b1))))
  (query-parameter-check-error
    "A typed update rejects a literal belonging to a different object type."
    (lambda ()
      (check-query/update-call
        '(query-parameter-object-update! query-parameter-test-b1))))
  (query-parameter-check-error
    "An empty optional type has no literal object, including NIL."
    (lambda ()
      (check-query/update-call
        '(query-parameter-null-hook nil :test-value))))
  t)


;;;; EMPTY-TYPE INSTALLATION TESTS ;;;;


(define-query query-parameter-null-hook
    (?object query-parameter-test-empty ?value)
  ;; The hook is installed even though QUERY-PARAMETER-TEST-EMPTY has no objects.
  (do ?object ?value nil))


(define-update query-parameter-null-update!
    (?object query-parameter-test-empty ?value)
  ;; Likewise for an update; only enumeration over the empty type would skip a call.
  (do ?object ?value nil))


(define-update query-parameter-object-update!
    (?object query-parameter-test-object-a)
  ;; The action below executes this valid nonempty typed call; the goal observes its write.
  (query-parameter-object-update-ran ?object))


(define-query query-parameter-boolean-hook (?value)
  ;; NIL and T are legitimate untyped Lisp values in a query/update call.
  (do ?value t))


(define-query query-parameter-lisp-value-hook (?value)
  ;; Keywords are also legitimate untyped Lisp values in a query/update call.
  (do ?value t))


(define-query query-parameter-object-hook
    (?object query-parameter-test-object-a)
  (do ?object t))


(define-query query-parameter-either-hook
    (?object (either query-parameter-test-object-a
                     query-parameter-test-object-b))
  (do ?object t))


(defun run-query-update-parameter-tests ()
  "Run the focused specification tests for query/update parameter lists."
  (run-query-parameter-success-tests)
  (run-query-parameter-rejection-tests)
  (format t "~&Query/update parameter tests passed.~%")
  t)


(run-query-update-parameter-tests)


;;;; ACTION ;;;;


(define-action complete-query-parameter-tests
  1
  ()
  (query-parameter-tests-ready)
  ()
  (assert (query-parameter-object-update! query-parameter-test-a1)
          (query-parameter-tests-passed)
          (not (query-parameter-tests-ready))))


;;;; INITIALIZATION ;;;;


(define-init
  (query-parameter-tests-ready)
  (query-parameter-test-pair query-parameter-test-a1 query-parameter-test-b1))


;;;; GOAL ;;;;


(define-goal
  (and (query-parameter-tests-passed)
       (query-parameter-object-update-ran query-parameter-test-a1)))

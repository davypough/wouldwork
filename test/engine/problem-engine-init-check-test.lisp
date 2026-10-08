;;; Initialization-check registration and valid-world characterization.

(in-package :ww)


(ww-set *problem-name* engine-init-check-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 0)


(define-types
  init-check-token
    (allowed rejected-by-both rejected-by-second coding-error))


(define-static-relations
  (init-check-marker init-check-token))


(define-init-check-helper init-check-marker-value (literal)
  (second (init-literal-proposition literal)))


(define-init-check first-init-check (literals)
  (:consumes init-check-token)
  (dolist
      (literal
        (positive-init-literals-with-relation
          'init-check-marker literals))
    (when (eql (init-check-marker-value literal)
               'rejected-by-both)
      (fail-init-check literal "Rejected by the first check."))))


(define-init-check second-init-check (literals)
  (dolist
      (literal
        (positive-init-literals-with-relation
          'init-check-marker literals))
    (when (member
            (second (init-literal-proposition literal))
            '(rejected-by-both rejected-by-second))
      (fail-init-check literal "Rejected by the second check."))))


(define-init-check erroneous-init-check (literals)
  (dolist
      (literal
        (positive-init-literals-with-relation
          'init-check-marker literals))
    (when (eql
            (second (init-literal-proposition literal))
            'coding-error)
      (error "Deliberate initialization-check coding error."))))


(define-init
  (init-check-marker allowed))


(define-goal
  (init-check-marker allowed))

;;; Derived-relation declaration and valid authored-state characterization.

(in-package :ww)


(ww-set *problem-name* engine-derived-relation-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 0)


(define-types
  derived-relation-token (authored computed))


(define-dynamic-relations
  (authored-marker derived-relation-token)
  (computed-marker derived-relation-token))


(define-static-relations
  (static-marker derived-relation-token))


(define-derived-relations
  computed-marker)


(define-init
  (authored-marker authored))


(define-goal
  (authored-marker authored))

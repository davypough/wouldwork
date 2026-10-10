;;; Symmetric self-pair rejection and relation-annotation characterization.

(in-package :ww)


(ww-set *problem-name* engine-symmetric-self-pair-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 0)


(define-types
  pair-gate (gate1)
  pair-area (area1 area2))


(define-dynamic-relations
  (pair-at pair-area))


(define-static-relations
  (pair-separates pair-gate pair-area pair-area)
  (pair-route> pair-area pair-area)
  (pair-span pair-area $fixnum $fixnum))


(define-init
  (pair-at area1)
  (pair-separates gate1 area1 area2)
  (pair-route> area1 area1)
  (pair-span area1 2 2))


(define-goal
  (pair-at area1))

;;; Filename: problem-symtest.lisp

;;; Test problem for symmetry pruning.
;;; Four blocks A, B, C, D all on table T.
;;; Goal: any block on any other block (doesn't reference specific blocks).
;;; Expected: All 4 blocks should be symmetric and have identical
;;; initial signatures since they all satisfy (on _ T).

(in-package :ww)

(ww-set *problem-name* symtest)
(ww-set *problem-type* planning)
(ww-set *solution-type* every)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 4)

(define-types
  block (A B C D)
  table (T)
  support (either block table))

(define-dynamic-relations
  (on block $support))

(define-query cleartop? (?block)
  (not (exists (?b block)
         (on ?b ?block))))

(define-action put
    1
  (standard ?block block ?target support)
  (and (cleartop? ?block)
       (bind (on ?block $block-support))
       (different ?target $block-support)
       (or (and (block ?target) (cleartop? ?target))
           (table ?target)))
  (?block ?target)
  (assert (on ?block ?target)))

(define-init
  (on A T)
  (on B T)
  (on C T)
  (on D T))

;; Goal: just need any two-block stack - no specific blocks named
(define-goal
  (exists (?b1 block)
    (exists (?b2 block)
      (on ?b1 ?b2))))
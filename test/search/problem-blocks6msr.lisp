;;; Filename: problem-blocks6msr.lisp


;;; Engine test problem: a six-block world with a min-steps-remaining? lower bound.
;;; Exercises min-steps-remaining? pruning in both serial and parallel search
;;; (no dynamic objects, so it runs under *threads* > 0).
;;; Expected: an 8-step minimum-length solution, with a nonzero
;;; "Min-steps-remaining pruned" count in the summary.


(in-package :ww)  ;required


(ww-set *problem-name* blocks6msr)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 9)


(define-types
    block (A B C D E F)
    table (T)
    support (either block table))


(define-dynamic-relations
    (on block support))


(define-query cleartop? (?block)
  (not (exists (?b block)
         (on ?b ?block))))


(define-query min-steps-remaining? ()
  ;Admissible: each put changes the support of one block, so it can fix at most one goal fact
  (+ (if (on A B) 0 1) (if (on B C) 0 1) (if (on C D) 0 1)
     (if (on D E) 0 1) (if (on E F) 0 1) (if (on F T) 0 1)))


(define-action put
    1
  (standard ?block block (?block-support ?target) support)
  (and (cleartop? ?block)
       (on ?block ?block-support)
       (or (and (block ?target) (cleartop? ?target))
           (table ?target)))
  (?block ?target)
  (assert (on ?block ?target)
          (not (on ?block ?block-support))))


(define-init
  (on F A)
  (on A T)
  (on B C)
  (on C T)
  (on D E)
  (on E T))


(define-goal
  (and (on F T) (on E F) (on D E) (on C D) (on B C) (on A B)))

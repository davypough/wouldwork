;;; Filename: problem-blocks3.lisp


;;; Problem specification for a blocks world problem:
;;; stack blocks named A, B, and C on a table named T.


(in-package :ww)  ;required


(ww-set *problem-name* blocks3)

(ww-set *problem-type* planning)

(ww-set *solution-type* every)

(ww-set *tree-or-graph* tree)  ;note: preferrable to use blocks3a if graph search wanted


(define-types
    block (A B C)
    table (T)
    target (either block table))


(define-dynamic-relations
    (on block target))


(define-static-relations
    (height target $real))


(define-query cleartop? (?block)
  (not (exists (standard ?b block)
         (on ?b ?block))))


(define-action put
    1
  (standard ?block block (?block-support ?target) target)
  (and (cleartop? ?block)
       (on ?block ?block-support)
       (cleartop? ?target)
       (different ?block ?target))
  (?block ?target)
  (assert (on ?block ?target)
          (not (on ?block ?block-support))))


(define-init
  (on A T)
  (on B T)
  (on C T))


(define-goal
  (or (and (on C T) (on B C) (on A B))
      (and (on A T) (on B A) (on C B))))

;;; Filename: problem-blocks3a.lisp


;;; Problem specification for a blocks world problem:
;;; stack blocks named A, B, and C on a table named T.
;;; Keep problem-state.idb keys fixed.


(in-package :ww)  ;required


(ww-set *problem-name* blocks3a)

(ww-set *problem-type* planning)

(ww-set *solution-type* every)

(ww-set *tree-or-graph* graph)


(define-types
    block (A B C)
    table (T)
    target (either block table))


(define-dynamic-relations
    (on block $symbol))  ;eg, (on A T)


(define-static-relations
    (height target $real))


(define-query cleartop? ($block)
  (not (exists (?b block)
         (on ?b $block))))
  


(define-action put
    1
  (standard ?block block ?target target)
  (and (cleartop? ?block)
       (cleartop? ?target)
       (different ?block ?target))
  (?block ?target)
  (assert (bind (on ?block $block-support))
          (not (on ?block $block-support))
          (on ?block ?target)))


(define-init
  (on A T)
  (on B T)
  (on C T))


(define-goal
  (or (and (on C T) (on B C) (on A B))  ;A -> B -> C -> T
      (and (on A T) (on B A) (on C B))))  ;C -> B -> A -> T

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
  support (either block table))


(define-dynamic-relations
  (on block $support))  ;eg, (on A T), where $support is a fluent


(define-query cleartop? (?block)
  (not (exists (?b block)
         (on ?b ?block))))
  

(define-action put
    1
  (standard ?block block ?target support)
  (and (cleartop? ?block)                 ;?block must have a clear top
       (bind (on ?block $block-support))  ;get the $block-support under ?block
       (and (block ?target)               ;if target is a block (not the table)
            (cleartop? ?target)))         ;it must have a clear top, otherwise can move to table
  (?block ?target)                        ;the action description will be (put ?block ?target)
  (assert (on ?block ?target)))           ;fluent status of ?block updated


(define-init
  ;(on A T)  ;note: all possible (on block $support) relations must be initially
  (on B T)  ;instantiated for greatest efficiency
  (on C T))


(define-goal
  (or (and (on C T) (on B C) (on A B))    ;A -> B -> C -> T
      (and (on A T) (on B A) (on C B))))  ;C -> B -> A -> T

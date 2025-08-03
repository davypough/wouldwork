;;; Filename: problem-blocks3a.lisp


;;; Problem specification for a blocks world problem:
;;; stack blocks named A, B, and C on a table named T.
;;; Uses fluent variables

(in-package :ww)  ;required


(ww-set *problem-name* blocks3a)

(ww-set *problem-type* planning)  ;use planning vs constraint satisfaction search strategy

(ww-set *solution-type* every)  ;find every possible solution

(ww-set *tree-or-graph* graph)  ;eliminate repeated states


(define-types
  block (A B C)
  table (T)
  support (either block table))  ;a block or table can be a support (for a block)


(define-dynamic-relations
  (on block $support))  ;a block can be on a fluent support, allows direct lookup with bind


(define-query cleartop? (?block)
  (not (exists (?b block)  ;?block has cleartop if there is no block on it
         (on ?b ?block))))
  

(define-action put
    1
  (standard ?block block ?target support)  ;standard (optional) means ?block /= ?target 
  (and (cleartop? ?block)                  ;?block must have a clear top
       (bind (on ?block $block-support))   ;get the $block-support under ?block
       (different ?target $block-support)  ;prevents a no-change action
       (or (and (block ?target) (cleartop? ?target))  ;there is no block on the ?target block
           (table ?target)))                          ;or the ?target is the table
  (?block ?target)                         ;the action description will be (put ?block ?target)
  (assert (on ?block ?target)))            ;fluent status of ?block updated


(define-init
  (on A T)  ;note: all possible (on block $support) relations must be initially
  (on B T)  ;      instantiated for greatest efficiency
  (on C T))


(define-goal
  (or (and (on C T) (on B C) (on A B))    ;A -> B -> C -> T
      (and (on A T) (on B A) (on C B))))  ;C -> B -> A -> T

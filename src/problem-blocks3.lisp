;;; Filename: problem-blocks3.lisp


;;; Problem specification for a blocks world problem:
;;; stack blocks named A, B, and C on a table named T.


(in-package :ww)  ;required


(ww-set *problem-name* blocks3)

(ww-set *problem-type* planning)  ;use planning (vs constraint satisfaction) search strategy

(ww-set *solution-type* every)  ;find every possible solution

(ww-set *tree-or-graph* tree)

(ww-set *depth-cutoff* 3)


(define-types
    block (A B C)
    table (T)
    support (either block table))  ;a block or table can be a support (for a block)


(define-dynamic-relations
    (on block support))  ;a block can be on a support


(define-query cleartop? (?block)
  (not (exists (?b block)  ;?block has cleartop if there is no block on it
         (on ?b ?block))))


(define-action put
    1
  (standard ?block block (?block-support ?target) support)  ;standard (optional) means ?block /= ?support /= ?target 
  (and (cleartop? ?block)          ;there is no other block on ?block
       (on ?block ?block-support)  ;?block is on some ?support
       (or (and (block ?target) (cleartop? ?target))  ;there is no block on the ?target block
           (table ?target)))                          ;or the ?target is the table
  (?block ?target)                 ;the action description will be (put ?block ?target)
  (assert (on ?block ?target)      ;new assertion added to state
          (not (on ?block ?block-support))))  ;previous assertion removed from state


(define-init
  (on A T)
  (on B T)
  (on C T))


(define-goal
  (or (and (on C T) (on B C) (on A B))    ;A -> B -> C -> T
      (and (on A T) (on B A) (on C B))))  ;C -> B -> A -> T

;;; Controlled ternary tree for DFS/backtracking comparisons.
(in-package :ww)

(defvar *assignment-tree-depth* 8)
(defvar *assignment-tree-facts* 40)

(ww-set *problem-name* assignment-tree)
(ww-set *problem-type* csp)
(ww-set *tree-or-graph* tree)
(ww-set *solution-type* count)
(ww-set *threads* 0)

(assert (typep *assignment-tree-depth* '(integer 1 8)))
(assert (typep *assignment-tree-facts* '(integer 10 1000)))

(define-types
  slot-number (1 2 3 4 5 6 7 8)
  digit (0 1 2)
  payload-key (compute (loop for i from 1 to (- *assignment-tree-facts* 9)
                             collect i)))

(define-dynamic-relations
  (next-slot $fixnum)
  (assigned slot-number $fixnum)
  (payload payload-key $fixnum))

(define-action assign-digit
  1
  (?digit digit)
  (and (bind (next-slot $slot)) (<= $slot *assignment-tree-depth*))
  (?digit)
  (assert (assigned $slot ?digit)
          (next-slot (1+ $slot))))

;; Eight initialized assignment fluents keep the fact count constant at all depths.
;; Payload is dynamic so state copying includes it, although actions leave it alone.
(install-init
 (append '((next-slot 1))
         (loop for i from 1 to 8 collect (list 'assigned i -1))
         (loop for i from 1 to (- *assignment-tree-facts* 9)
               collect (list 'payload i i))))

(define-goal (next-slot (1+ *assignment-tree-depth*)))

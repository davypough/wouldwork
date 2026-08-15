;;; Filename: problem-engine-state-copy-test.lisp

;;; Characterization of problem-state database copying.  Fluent value lists are immutable
;;; database values and may be shared, while every copied state owns an independent hash
;;; table whose additions, replacements, and removals cannot affect its parent.

(in-package :ww)


(ww-set *problem-name* engine-state-copy-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 0)


(define-types
  token (token1)
  location (site1 site2))


(define-dynamic-relations
  (copy-slot token $location)
  (copy-flag token))


(define-init
  (copy-slot token1 site1)
  (copy-flag token1))


(define-test-claim state-idb-copy-contract
  (let* ((parent *start-state*)
         (child (copy-problem-state parent))
         (parent-idb (problem-state.idb parent))
         (child-idb (problem-state.idb child))
         (slot-key (convert-to-integer-memoized '(copy-slot token1)))
         (flag-key (convert-to-integer-memoized '(copy-flag token1)))
         (parent-slot (gethash slot-key parent-idb))
         (child-slot (gethash slot-key child-idb)))
    (and
      (not (eq parent-idb child-idb))
      ;; COPY-IDB deliberately shares immutable fluent values.
      (eq parent-slot child-slot)
      (progn
        (add-proposition '(copy-slot token1 site2) child-idb)
        (and (equal (gethash slot-key parent-idb) '(site1))
             (equal (gethash slot-key child-idb) '(site2))))
      (progn
        (delete-proposition '(copy-flag token1) child-idb)
        (and (gethash flag-key parent-idb)
             (not (nth-value 1 (gethash flag-key child-idb))))))))


(define-goal
  (always-true))

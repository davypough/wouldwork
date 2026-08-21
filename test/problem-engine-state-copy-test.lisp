;;; Filename: problem-engine-state-copy-test.lisp

;;; Characterization of problem-state database copying.  Fluent value lists are immutable
;;; database values and may be shared.  IDB is independently mutable in every copy; HIDB is
;;; independent when happenings exist and otherwise shared because generated code cannot
;;; reach it.

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


(define-test-claim effect-state-copy-contract
  (let* ((parent (copy-problem-state *start-state*))
         (parent-idb (problem-state.idb parent))
         (parent-hidb (problem-state.hidb parent))
         (happening-key 987654321))
    (setf (gethash happening-key parent-hidb) t)
    (and
      ;; Without happenings, neither ordinary nor generated effect code can reference
      ;; HIDB.  Both kinds of copy own a new IDB but share the unread HIDB.
      (null *happening-names*)
      (let ((ordinary-state (copy-problem-state parent)))
        (and (not (eq parent-idb (problem-state.idb ordinary-state)))
             (eq parent-hidb (problem-state.hidb ordinary-state))))
      (let ((effect-state (copy-problem-state-for-effect parent nil)))
        (and (not (eq parent-idb (problem-state.idb effect-state)))
             (eq parent-hidb (problem-state.hidb effect-state))))
      ;; A problem with happenings retains independent tables for ordinary copies.
      (let ((original-happening-names *happening-names*))
        (unwind-protect
          (progn
            (setf *happening-names* '(copy-test-happening))
            (let* ((ordinary-state (copy-problem-state parent))
                   (ordinary-hidb (problem-state.hidb ordinary-state)))
              (and (not (eq parent-idb (problem-state.idb ordinary-state)))
                   (not (eq parent-hidb ordinary-hidb))
                   (progn
                     (remhash happening-key ordinary-hidb)
                     (and (gethash happening-key parent-hidb)
                          (not (nth-value 1
                                 (gethash happening-key ordinary-hidb))))))))
          (setf *happening-names* original-happening-names)))
      ;; Generated effect copies use the same isolation rule when happenings exist.
      (let ((effect-state (copy-problem-state-for-effect parent t)))
        (let ((effect-hidb (problem-state.hidb effect-state)))
          (and (not (eq parent-idb (problem-state.idb effect-state)))
               (not (eq parent-hidb effect-hidb))
               (progn
                 (remhash happening-key effect-hidb)
                 (and (gethash happening-key parent-hidb)
                      (not (nth-value 1 (gethash happening-key effect-hidb)))))))))))


(define-goal
  (always-true))

;;; Filename: problem-recorder-symmetry-test.lisp

;;; Focused characterization of recorder-aware symmetry.  The two connector copy pairs
;;; form one interchangeable two-column family.  A live-only exchange is forbidden, while
;;; exchanging the complete live/ghost rows is both a static automorphism and a canonical
;;; duplicate.  Expected minimum path length: zero.

(in-package :ww)


(ww-set *problem-name* recorder-symmetry-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 1)
(ww-set *symmetry-pruning* t)

(setf *expected-min-length* 0)


(define-types
  agent (live-agent ghost-agent)
  connector (live-connector-1 live-connector-2 ghost-connector-1 ghost-connector-2)
  recorder (recorder1)
  location (site-1 site-2))


;; Core identity is sufficient here; the test owns its one generic searched action.
(include-tech -recorder-core)


(define-dynamic-relations
  (symmetry-link connector $connector))


(define-action relocate-symmetry-connector
    1
  (standard ?connector connector ?location location)
  (bind (has-location ?connector $old-location))
  (?connector ?location)
  (assert (has-location ?connector ?location)))


(define-init
  (recording-copy> live-agent ghost-agent)
  (recording-copy> live-connector-1 ghost-connector-1)
  (recording-copy> live-connector-2 ghost-connector-2)
  (has-position recorder1 site-1)
  (has-location live-agent site-1)
  (has-location ghost-agent site-1)
  (has-location live-connector-1 site-1)
  (has-location ghost-connector-1 site-1)
  (has-location live-connector-2 site-2)
  (has-location ghost-connector-2 site-2))


(define-test-helper recorder-symmetry-state
    (live-1-site ghost-1-site live-2-site ghost-2-site)
  "Return a copy of the start state with the four connector locations replaced."
  (let* ((state (copy-problem-state *start-state*))
         (idb (problem-state.idb state))
         (objects
           '(live-connector-1 ghost-connector-1
             live-connector-2 ghost-connector-2)))
    (dolist (proposition (list-database idb))
      (when (and (eq (first proposition) 'has-location)
                 (member (second proposition) objects :test #'eq))
        (delete-proposition proposition idb)))
    (loop for object in objects
          for location in (list live-1-site ghost-1-site live-2-site ghost-2-site)
          do (add-proposition `(has-location ,object ,location) idb))
    (invalidate-problem-state-hash state)
    state))


(define-test-claim recorder-symmetry-family-contract
  (let* ((membership
           (gethash 'live-connector-1 *object-to-symmetry-membership*))
         (rows
           (and membership
                (symmetry-family.rows
                  (symmetry-membership.family membership)))))
    (and (= (length rows) 2)
         (alexandria:set-equal
           rows
           '((live-connector-1 ghost-connector-1)
             (live-connector-2 ghost-connector-2))
           :test #'equal)))
  ;; This is the ordinary one-column exchange that recorder identity invalidates.
  (not (static-transposition-preserves-p
         '(live-connector-1) '(live-connector-2)))
  ;; The complete pair exchange preserves direction and both semantic columns.
  (static-transposition-preserves-p
    '(live-connector-1 ghost-connector-1)
    '(live-connector-2 ghost-connector-2)))


(define-test-claim recorder-symmetry-canonicalization-contract
  (let ((base
          (recorder-symmetry-state 'site-1 'site-1 'site-2 'site-2))
        (live-only-swap
          (recorder-symmetry-state 'site-2 'site-1 'site-1 'site-2))
        (paired-swap
          (recorder-symmetry-state 'site-2 'site-2 'site-1 'site-1)))
    (and (equal
           (build-canonical-idb-form (problem-state.idb base))
           (build-canonical-idb-form (problem-state.idb paired-swap)))
         (not (equal
                (build-canonical-idb-form (problem-state.idb base))
                (build-canonical-idb-form (problem-state.idb live-only-swap)))))))


(define-test-claim recorder-symmetry-split-hash-contract
  (let ((base
          (recorder-symmetry-state 'site-1 'site-1 'site-2 'site-2))
        (live-only-swap
          (recorder-symmetry-state 'site-2 'site-1 'site-1 'site-2))
        (paired-swap
          (recorder-symmetry-state 'site-2 'site-2 'site-1 'site-1)))
    (and (= (ensure-idb-hash base)
            (ensure-idb-hash paired-swap))
         (canonical-state-equal-p base paired-swap)
         (not (canonical-state-equal-p base live-only-swap)))))


(define-test-claim recorder-symmetry-local-state-contract
  (let ((undistinguished
          (recorder-symmetry-state 'site-1 'site-1 'site-1 'site-1)))
    (and (objects-equivalent-in-state-p
           'live-connector-1 'live-connector-2 undistinguished)
         (progn
           ;; The second endpoint is stored in a fluent value.  It must constrain the
           ;; transposition just as strongly as an object in the proposition key.
           (add-proposition
             '(symmetry-link live-connector-1 ghost-connector-1)
             (problem-state.idb undistinguished))
           (not (objects-equivalent-in-state-p
                  'live-connector-1 'live-connector-2 undistinguished))))))


(define-test-claim recorder-symmetry-goal-refresh-contract
  (unwind-protect
      (progn
        (install-compiled-goal
          '(has-location live-connector-1 site-1))
        (and (null *symmetry-families*)
             (progn
               (install-compiled-goal '(always-true))
               (not (null *symmetry-families*)))))
    (install-compiled-goal '(always-true))))


(define-goal
  (always-true))

;;; Filename: problem-engine-symmetry-test.lisp

;;; Ordinary one-column symmetry characterization.  This is the non-recorder counterpart
;;; to problem-recorder-symmetry-test: exchanging TOKEN-1 and TOKEN-2 is a safe static
;;; automorphism and produces the same canonical state.  Expected minimum path length: zero.

(in-package :ww)


(ww-set *problem-name* engine-symmetry-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 1)
(ww-set *symmetry-pruning* t)

(setf *expected-min-length* 0)


(define-types
  token (token-1 token-2)
  coded-token (coded-token-1 coded-token-2)
  location (site-1 site-2))


(define-dynamic-relations
  (token-at token $location)
  (token-link token $token)
  (coded-token-at coded-token $location))


(define-query coded-token-priority (?token coded-token)
  ;; Deliberately identity-sensitive: used to prove that one unsafe action invalidates
  ;; this family even though another action below uses the same type generically.
  (gethash ?token *types*))


(define-action relocate-token
    1
  (standard ?token token ?location location)
  (bind (token-at ?token $old-location))
  (?token ?location)
  (assert (token-at ?token ?location)))


(define-action relocate-coded-token
    1
  (standard ?token coded-token ?location location)
  (bind (coded-token-at ?token $old-location))
  (?token ?location)
  (assert (coded-token-at ?token ?location)))


(define-action inspect-coded-token
    1
  (standard ?token coded-token)
  (and (coded-token-priority ?token)
       (bind (coded-token-at ?token $location)))
  (?token)
  (assert (coded-token-at ?token $location)))


(define-init
  (token-at token-1 site-1)
  (token-at token-2 site-2)
  (coded-token-at coded-token-1 site-1)
  (coded-token-at coded-token-2 site-2))


(define-test-helper engine-symmetry-state (token-1-site token-2-site)
  "Return a copy of the start state with both token locations replaced."
  (let* ((state (copy-problem-state *start-state*))
         (idb (problem-state.idb state)))
    (dolist (proposition (list-database idb))
      (when (eq (first proposition) 'token-at)
        (delete-proposition proposition idb)))
    (add-proposition `(token-at token-1 ,token-1-site) idb)
    (add-proposition `(token-at token-2 ,token-2-site) idb)
    (setf (problem-state.idb-hash state) nil)
    state))


(define-test-claim ordinary-symmetry-contract
  ;; CODED-TOKEN has one generic action and one identity-sensitive action.  It must not
  ;; survive merely because the generic action would have made the family operative.
  (null (gethash 'coded-token-1 *object-to-symmetry-membership*))
  (static-transposition-preserves-p '(token-1) '(token-2))
  (let ((base (engine-symmetry-state 'site-1 'site-2))
        (swapped (engine-symmetry-state 'site-2 'site-1)))
    (equal
      (build-canonical-idb-form (problem-state.idb base))
      (build-canonical-idb-form (problem-state.idb swapped))))
  (let ((undistinguished (engine-symmetry-state 'site-1 'site-1)))
    (and (objects-equivalent-in-state-p
           'token-1 'token-2 undistinguished)
         (progn
           (add-proposition '(token-link token-1 token-1)
                            (problem-state.idb undistinguished))
           (not (objects-equivalent-in-state-p
                  'token-1 'token-2 undistinguished))))))


(define-goal
  (exists (?token token)
    (token-at ?token site-1)))

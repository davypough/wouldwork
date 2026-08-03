;;; Characterization-claim registration, assertion, and valid-world fixture.


(in-package :ww)


(ww-set *problem-name* engine-test-claim-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 1)
(setf *expected-min-length* 0)


(define-types
  claim-token (claim-token-1))


(define-static-relations
  (claim-marker claim-token))


(define-init
  (claim-marker claim-token-1))


(define-test-helper engine-test-claim-helper (value)
  (* value 2))


(define-test-claim engine-test-claim-contract
  (expect-type-instances 'claim-token '(claim-token-1))
  (expect-relation-schema 'claim-marker :static '(claim-token))
  (expect-registrations :action '())
  (= (engine-test-claim-helper 3) 6)
  (zerop *debug*))


(define-goal
  (claim-marker claim-token-1))

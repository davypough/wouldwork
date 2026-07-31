;;; Dedicated regression coverage for the neutral -threat configuration.
;;;
;;; The problem defines no gun instances, so the composite THREAT type is
;;; empty.  It verifies that SAFE remains true at every location, that ordinary
;;; propagation accepts located agents through the threat-safety backstop, and
;;; that an unlocated agent remains unlocated.  It also characterizes the
;;; empty-type and relation metadata installed by -threat while asserting that
;;; the optional gun control/jamming machinery remains absent.
;;;
;;; The initial and final dynamic state contains exactly the two HAS-LOCATION
;;; facts below.  No planning actions are needed, so the expected minimum path
;;; length is 0.

(in-package :ww)

(ww-set *problem-name* threat-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 1)
(setf *expected-min-length* 0)

(define-types
  agent (first-agent second-agent unlocated-agent)
  location (first-site second-site empty-site))

(include-tech -location)
(include-tech -threat)

(define-init
  (has-location first-agent first-site)
  (has-location second-agent second-site))

(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!)))

(setf
  (symbol-function 'threat-neutral-schema-valid-p)
  (lambda ()
    (multiple-value-bind (threatens-signature threatens-static-p)
        (gethash 'threatens *static-relations*)
      (multiple-value-bind (lethal-signature lethal-dynamic-p)
          (gethash 'lethal *relations*)
        (and
          (equal (gethash 'threat *type-components*) '(gun))
          (equal (gethash 'threat *types*) '(nil))
          (null (gethash 'gun *types*))
          threatens-static-p
          (equal threatens-signature '(threat list))
          (equal (gethash 'threatens *fluent-relation-indices*) '(2))
          lethal-dynamic-p
          (equal lethal-signature '(threat))
          (not (nth-value 1 (gethash 'lethal *static-relations*)))
          (not (nth-value 1 (gethash 'controls *static-relations*)))
          (not (nth-value 1 (gethash 'jamming *relations*))))))))

(setf
  (symbol-function 'threat-state-consistent-p)
  (lambda (state)
    (not (state-is-inconsistent state))))

(define-query threat-scenarios-valid ()
  (and
    (has-location first-agent first-site)
    (has-location second-agent second-site)
    (not (has-location first-agent second-site))
    (not (has-location second-agent first-site))
    (not (bind (has-location unlocated-agent $unexpected-location)))
    (safe first-site)
    (safe second-site)
    (safe empty-site)
    (not (exists (?candidate threat)
           (lethal ?candidate)))
    (not (exists (?candidate threat)
           (bind (threatens ?candidate
                            $unexpected-threatened-locations))))
    (threat-state-consistent-p state)
    (threat-neutral-schema-valid-p)))

(define-goal
  (threat-scenarios-valid))

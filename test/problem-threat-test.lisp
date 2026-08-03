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

(define-test-claim threat-neutral-schema
  (expect-type-components 'threat '(gun))
  (expect-empty-type 'threat)
  (expect-empty-type 'gun)
  (expect-relation-schema
    'threatens :static '(threat list) :fluent-indices '(2))
  (expect-relation-schema 'lethal :dynamic '(threat))
  (expect-relation-absent 'controls :static)
  (expect-relation-absent 'jamming :dynamic)
  (not (state-is-inconsistent *start-state*)))

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
                            $unexpected-threatened-locations))))))

(define-goal
  (threat-scenarios-valid))

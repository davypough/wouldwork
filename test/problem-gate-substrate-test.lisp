;;; Dedicated zero-action regression for the shared -gate substrate.
;;;
;;; AUTHORED-OPEN-GATE verifies that the substrate installs OPEN as an ordinary
;;; dynamic relation whose authored state is preserved.  DEFAULT-CLOSED-GATE
;;; verifies the complementary absence default.  The goal also confirms that
;;; the public gate control/update layer, initialization, and actions remain
;;; absent when only the substrate is included.
;;;
;;; The initial and final dynamic states contain exactly
;;; (OPEN AUTHORED-OPEN-GATE).  The expected minimum path length is 0.

(in-package :ww)

(ww-set *problem-name* gate-substrate-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 1)
(setf *expected-min-length* 0)


;;;; TYPES ;;;;


(define-types
  gate (authored-open-gate default-closed-gate))


;;;; TECHNOLOGY INCLUDE ;;;;


(include-tech -gate)


;;;; INITIALIZATION ;;;;


(define-init
  (open authored-open-gate))


;;;; CHARACTERIZATION CLAIM ;;;;


(define-test-claim gate-substrate-schema
  (expect-type-instances
    'gate
    '(authored-open-gate default-closed-gate))
  (expect-type-absent 'jammer)
  (expect-relation-schema 'open :dynamic '(gate))
  (expect-relations :dynamic '(inconsistent-state open))
  (expect-relations :static '(gate always-true))
  (expect-relation-absent 'controls)
  (expect-not-registered :update 'update-gate-status!)
  (expect-registrations :init-action '())
  (expect-registrations :action '())
  (equal (database *start-state*) '((open authored-open-gate)))
  (not (state-is-inconsistent *start-state*)))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query gate-substrate-type-valid
    (?object gate)
  (do
    ?object
    t))


(define-query gate-substrate-scenarios-valid ()
  (and
    ;; Both constants inhabit the optional type supplied by the substrate.
    (gate-substrate-type-valid authored-open-gate)
    (gate-substrate-type-valid default-closed-gate)

    ;; Authored dynamic membership is preserved; absence remains closed.
    (open authored-open-gate)
    (not (open default-closed-gate))))


(define-goal
  (gate-substrate-scenarios-valid))

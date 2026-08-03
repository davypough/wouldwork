;;; Filename: problem-reachability-test.lisp

;;; Zero-action characterization test for reachability.lisp.  Independent location pairs
;;; exercise:
;;;
;;;   1. Identity reachability for an isolated location.
;;;   2. A direct edge with no barriers, in both directions.
;;;   3. Direct edges guarded by one and two open gates.
;;;   4. A closed-gate edge and a mixed open/closed edge, both blocked in both directions.
;;;   5. Two adjacent direct edges whose endpoints remain mutually unreachable, proving
;;;      REACHABLE does not compute transitive closure.
;;;   6. Two disconnected locations that remain mutually unreachable.
;;;   7. REACHABLE-CLEAR directly recognizing open gates and rejecting a closed gate.
;;;
;;; OPEN-GATE1 and OPEN-GATE2 are seeded as dependency fixtures; CLOSED-GATE is deliberately
;;; absent from OPEN.  There are no actions or derived-state updates, so the initial state
;;; must satisfy every positive and negative assertion directly.  The expected minimum path
;;; length is therefore zero.


(in-package :ww)


(ww-set *problem-name* reachability-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 0)


;;;; TYPES ;;;;


(define-types
  location (isolated
            clear-left clear-right
            open-left open-right
            all-open-left all-open-right
            closed-left closed-right
            mixed-left mixed-right
            chain-left chain-middle chain-right
            disconnected-left disconnected-right)
  gate (open-gate1 open-gate2 closed-gate))


;;;; TECHNOLOGY INCLUDE ;;;;


(include-tech reachability)


;;;; INITIALIZATION ;;;;


(define-init
  ;; OPEN is dynamic state supplied by the gate substrate.  Seeding it here isolates the
  ;; reachability query from the independent gate-control lifecycle.
  (open open-gate1)
  (open open-gate2)

  ;; Positive direct-edge cases.
  (reach-via clear-left () clear-right)
  (reach-via open-left (open-gate1) open-right)
  (reach-via all-open-left (open-gate1 open-gate2) all-open-right)

  ;; Negative barrier cases.
  (reach-via closed-left (closed-gate) closed-right)
  (reach-via mixed-left (open-gate1 closed-gate) mixed-right)

  ;; Direct adjacency is deliberately not transitive.
  (reach-via chain-left () chain-middle)
  (reach-via chain-middle () chain-right))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query reachability-scenarios-valid ()
  (and
    ;; Authored gate-state fixtures and the public barrier predicate.
    (open open-gate1)
    (open open-gate2)
    (not (open closed-gate))
    (reachable-clear open-gate1)
    (reachable-clear open-gate2)
    (not (reachable-clear closed-gate))

    ;; Identity and empty-barrier boundary.
    (reachable isolated isolated)
    (reachable clear-left clear-right)
    (reachable clear-right clear-left)

    ;; Every barrier open: one-gate and multi-gate positive cases.
    (reachable open-left open-right)
    (reachable open-right open-left)
    (reachable all-open-left all-open-right)
    (reachable all-open-right all-open-left)

    ;; A closed barrier blocks the complete edge in either direction.
    (not (reachable closed-left closed-right))
    (not (reachable closed-right closed-left))
    (not (reachable mixed-left mixed-right))
    (not (reachable mixed-right mixed-left))

    ;; Direct adjacency succeeds, but reachability has no transitive closure.
    (reachable chain-left chain-middle)
    (reachable chain-middle chain-left)
    (reachable chain-middle chain-right)
    (reachable chain-right chain-middle)
    (not (reachable chain-left chain-right))
    (not (reachable chain-right chain-left))

    ;; No authored edge means no reach between distinct locations.
    (not (reachable disconnected-left disconnected-right))
    (not (reachable disconnected-right disconnected-left))))


(define-goal
  (reachability-scenarios-valid))


;;;; MUTATION CHARACTERIZATION ;;;;


(define-query-mutation reachable-clear-ignores-gate-state reachable-clear
  (?barrier gate)
  (gate ?barrier)
  "Drops REACHABLE-CLEAR's open-state check.  The closed-gate reachability probes
   must then make this characterization fail.")

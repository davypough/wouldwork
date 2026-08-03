;;; Filename: problem-plate-test.lisp

;;; Dedicated zero-action regression for public plate behavior.  Seven independent
;;; fixtures characterize the complete derived invariant:
;;;
;;;   DEPRESSED(?plate) iff some SUPPORT-OCCUPANT rests ON ?plate.
;;;
;;; Five plates begin occupied by the five support-occupant leaf types: agent, box,
;;; jammer, connector, and fan.  Their DEPRESSED facts are omitted so initialization
;;; must derive them.  The test initialization action injects a stale DEPRESSED fact for
;;; a sixth, clear plate immediately before propagation, which must retract it.  A seventh
;;; plate begins clear and undepressed, checking the already-consistent negative case.
;;;
;;; The goal directly verifies every ON fact, exact single-support ownership, CLEARTOP,
;;; and DEPRESSED result after the ordinary propagation init action.  No manipulation
;;; action is involved because normalization of the derived plate state is the behavior
;;; under test.  Expected minimum path length: zero.

(in-package :ww)


(ww-set *problem-name* plate-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 0)


;;;; TYPES ;;;;


(define-types
  agent (agent-occupant)
  pressure-plate (agent-plate box-plate jammer-plate connector-plate fan-plate
                  stale-clear-plate ordinary-clear-plate)
  box (box-occupant)
  jammer (jammer-occupant)
  connector (connector-occupant)
  fan (fan-occupant))


;;;; TECHNOLOGY INCLUDE ;;;;


(include-tech plate)


;;;; INITIALIZATION ;;;;


(define-init
  ;; Positive matrix: depression is deliberately absent before propagation.
  (on agent-occupant agent-plate)
  (on box-occupant box-plate)
  (on jammer-occupant jammer-plate)
  (on connector-occupant connector-plate)
  (on fan-occupant fan-plate))


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert
    ;; Negative/retraction matrix: both plates begin clear.  Inject stale internal
    ;; state for the first immediately before the behavior under test.
    (depressed stale-clear-plate)
    (propagate-changes!)))


;;;; CHARACTERIZATION HELPERS ;;;;


(define-query occupies-only
    (?occupant support-occupant ?intended plate)
  (and (on ?occupant ?intended)
       (not (exists (?other support)
              (and (different ?other ?intended)
                   (on ?occupant ?other))))))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query plate-scenarios-valid ()
  (and
    ;; Every support-occupant leaf independently depresses its plate.
    (occupies-only agent-occupant agent-plate)
    (not (cleartop agent-plate))
    (depressed agent-plate)

    (occupies-only box-occupant box-plate)
    (not (cleartop box-plate))
    (depressed box-plate)

    (occupies-only jammer-occupant jammer-plate)
    (not (cleartop jammer-plate))
    (depressed jammer-plate)

    (occupies-only connector-occupant connector-plate)
    (not (cleartop connector-plate))
    (depressed connector-plate)

    (occupies-only fan-occupant fan-plate)
    (not (cleartop fan-plate))
    (depressed fan-plate)

    ;; Zero occupants is the exact negative boundary.  Propagation must retract
    ;; the authored stale fact and leave the already-consistent clear plate alone.
    (cleartop stale-clear-plate)
    (not (depressed stale-clear-plate))
    (cleartop ordinary-clear-plate)
    (not (depressed ordinary-clear-plate))))


(define-goal
  (plate-scenarios-valid))

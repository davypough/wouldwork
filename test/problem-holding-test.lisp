;;; Filename: problem-holding-test.lisp

;;; Dedicated regression for the shared -holding role.  Two independent scenarios
;;; characterize the CARGO union and the functional (HOLDING agent $cargo) relation:
;;;
;;;   1. A static matrix binds one box, jammer, connector, and fan to four agents,
;;;      while a fifth agent remains exactly empty-handed.
;;;   2. A four-action lifecycle replaces one agent's box with a jammer, then a
;;;      connector, then a fan, before releasing the fan.  Each replacement asserts
;;;      only the new fluent value; the following action explicitly requires every
;;;      superseded HOLDING fact to be absent.
;;;
;;; The characterization goal verifies exact direct and bound lookup for every cargo
;;; leaf, the empty-hand boundary, lifecycle completion, and all important absent
;;; facts.  No public manipulation behavior is duplicated.
;;; Expected minimum path length: four.

(in-package :ww)


(ww-set *problem-name* holding-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 4)


;;;; TYPES ;;;;


(define-types
  agent (lifecycle-agent box-holder jammer-holder connector-holder fan-holder
         empty-agent)
  box (lifecycle-box held-box)
  jammer (lifecycle-jammer held-jammer)
  connector (lifecycle-connector held-connector)
  fan (lifecycle-fan held-fan)
  holding-phase
    (holding-jammer holding-connector holding-fan lifecycle-complete))


;;;; TECHNOLOGY INCLUDE ;;;;


(include-tech -holding)


;;;; TEST LIFECYCLE STATE ;;;;


(define-dynamic-relations
  (holding-lifecycle-phase holding-phase))


;;;; ACTIONS ;;;;


(define-action replace-box-with-jammer
  1
  ()
  (and
    (holding lifecycle-agent lifecycle-box)
    (not (exists (?phase holding-phase)
           (holding-lifecycle-phase ?phase))))
  ("replace the held box with the jammer")
  (assert
    (holding lifecycle-agent lifecycle-jammer)
    (holding-lifecycle-phase holding-jammer)))


(define-action replace-jammer-with-connector
  1
  ()
  (and
    (holding-lifecycle-phase holding-jammer)
    (holding lifecycle-agent lifecycle-jammer)
    (not (holding lifecycle-agent lifecycle-box)))
  ("replace the held jammer with the connector")
  (assert
    (not (holding-lifecycle-phase holding-jammer))
    (holding lifecycle-agent lifecycle-connector)
    (holding-lifecycle-phase holding-connector)))


(define-action replace-connector-with-fan
  1
  ()
  (and
    (holding-lifecycle-phase holding-connector)
    (holding lifecycle-agent lifecycle-connector)
    (not (holding lifecycle-agent lifecycle-box))
    (not (holding lifecycle-agent lifecycle-jammer)))
  ("replace the held connector with the fan")
  (assert
    (not (holding-lifecycle-phase holding-connector))
    (holding lifecycle-agent lifecycle-fan)
    (holding-lifecycle-phase holding-fan)))


(define-action release-lifecycle-fan
  1
  ()
  (and
    (holding-lifecycle-phase holding-fan)
    (holding lifecycle-agent lifecycle-fan)
    (not (holding lifecycle-agent lifecycle-box))
    (not (holding lifecycle-agent lifecycle-jammer))
    (not (holding lifecycle-agent lifecycle-connector)))
  ("release the lifecycle fan")
  (assert
    (not (holding-lifecycle-phase holding-fan))
    (not (holding lifecycle-agent lifecycle-fan))
    (holding-lifecycle-phase lifecycle-complete)))


;;;; INITIALIZATION ;;;;


(define-init
  ;; Direct cargo-leaf matrix.
  (holding box-holder held-box)
  (holding jammer-holder held-jammer)
  (holding connector-holder held-connector)
  (holding fan-holder held-fan)

  ;; EMPTY-AGENT deliberately has no HOLDING fact.

  ;; The lifecycle begins with the first of four successive cargo values.
  (holding lifecycle-agent lifecycle-box))


;;;; CHARACTERIZATION HELPERS ;;;;


(define-query holds-exactly (?agent agent ?expected cargo)
  (and
    (holding ?agent ?expected)
    (do (bind (holding ?agent $bound-cargo))
        (eql $bound-cargo ?expected))
    (not (exists (?other cargo)
           (and (different ?other ?expected)
                (holding ?agent ?other))))))


(define-query empty-handed (?agent agent)
  (not (bind (holding ?agent $any-cargo))))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query holding-scenarios-valid ()
  (and
    ;; Every cargo leaf supports direct lookup and fluent binding, with no
    ;; competing value for the same keyed agent.
    (holds-exactly box-holder held-box)
    (holds-exactly jammer-holder held-jammer)
    (holds-exactly connector-holder held-connector)
    (holds-exactly fan-holder held-fan)

    ;; Absence is the exact empty-hand boundary.
    (empty-handed empty-agent)

    ;; The replacement-and-release lifecycle completed with no stale cargo or
    ;; intermediate phase left behind.
    (holding-lifecycle-phase lifecycle-complete)
    (not (holding-lifecycle-phase holding-jammer))
    (not (holding-lifecycle-phase holding-connector))
    (not (holding-lifecycle-phase holding-fan))
    (empty-handed lifecycle-agent)
    (not (holding lifecycle-agent lifecycle-box))
    (not (holding lifecycle-agent lifecycle-jammer))
    (not (holding lifecycle-agent lifecycle-connector))
    (not (holding lifecycle-agent lifecycle-fan))))


(define-goal
  (holding-scenarios-valid))

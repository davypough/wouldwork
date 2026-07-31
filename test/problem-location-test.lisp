;;; Filename: problem-location-test.lisp

;;; Dedicated regression for the shared -location role.  Two independent scenarios
;;; characterize the MOBILE-OBJECT union and functional
;;; (HAS-LOCATION mobile-object $location) relation:
;;;
;;;   1. A static matrix binds one agent, box, jammer, connector, and fan to five
;;;      exact locations.
;;;   2. A two-action lifecycle moves a second object of every mobile kind from one
;;;      common source to one common destination, then removes every destination
;;;      fact.  The move asserts only the new fluent values; the removal action
;;;      explicitly requires every superseded source fact to be absent.
;;;
;;; The characterization goal verifies direct and bound lookup, absence of competing
;;; locations, complete replacement for every mobile leaf, complete retraction, and
;;; lifecycle completion.  Expected minimum path length: two.

(in-package :ww)


(ww-set *problem-name* location-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 2)


;;;; TYPES ;;;;


(define-types
  agent (static-agent lifecycle-agent)
  box (static-box lifecycle-box)
  jammer (static-jammer lifecycle-jammer)
  connector (static-connector lifecycle-connector)
  fan (static-fan lifecycle-fan)
  location (static-agent-site static-box-site static-jammer-site
            static-connector-site static-fan-site
            lifecycle-source lifecycle-destination)
  location-phase (moved-to-destination lifecycle-complete))


;;;; TECHNOLOGY INCLUDE ;;;;


(include-tech -location)


;;;; TEST LIFECYCLE STATE ;;;;


(define-dynamic-relations
  (location-lifecycle-phase location-phase))


;;;; ACTIONS ;;;;


(define-action move-all-mobile-objects
  1
  ()
  (and
    (has-location lifecycle-agent lifecycle-source)
    (has-location lifecycle-box lifecycle-source)
    (has-location lifecycle-jammer lifecycle-source)
    (has-location lifecycle-connector lifecycle-source)
    (has-location lifecycle-fan lifecycle-source)
    (not (exists (?phase location-phase)
           (location-lifecycle-phase ?phase))))
  ("move every lifecycle object to the destination")
  (assert
    (has-location lifecycle-agent lifecycle-destination)
    (has-location lifecycle-box lifecycle-destination)
    (has-location lifecycle-jammer lifecycle-destination)
    (has-location lifecycle-connector lifecycle-destination)
    (has-location lifecycle-fan lifecycle-destination)
    (location-lifecycle-phase moved-to-destination)))


(define-action remove-all-mobile-locations
  1
  ()
  (and
    (location-lifecycle-phase moved-to-destination)
    (has-location lifecycle-agent lifecycle-destination)
    (has-location lifecycle-box lifecycle-destination)
    (has-location lifecycle-jammer lifecycle-destination)
    (has-location lifecycle-connector lifecycle-destination)
    (has-location lifecycle-fan lifecycle-destination)
    (not (has-location lifecycle-agent lifecycle-source))
    (not (has-location lifecycle-box lifecycle-source))
    (not (has-location lifecycle-jammer lifecycle-source))
    (not (has-location lifecycle-connector lifecycle-source))
    (not (has-location lifecycle-fan lifecycle-source)))
  ("remove every lifecycle location")
  (assert
    (not (location-lifecycle-phase moved-to-destination))
    (not (has-location lifecycle-agent lifecycle-destination))
    (not (has-location lifecycle-box lifecycle-destination))
    (not (has-location lifecycle-jammer lifecycle-destination))
    (not (has-location lifecycle-connector lifecycle-destination))
    (not (has-location lifecycle-fan lifecycle-destination))
    (location-lifecycle-phase lifecycle-complete)))


;;;; INITIALIZATION ;;;;


(define-init
  ;; Direct mobile-object leaf matrix.
  (has-location static-agent static-agent-site)
  (has-location static-box static-box-site)
  (has-location static-jammer static-jammer-site)
  (has-location static-connector static-connector-site)
  (has-location static-fan static-fan-site)

  ;; Every lifecycle object legitimately shares the same source.
  (has-location lifecycle-agent lifecycle-source)
  (has-location lifecycle-box lifecycle-source)
  (has-location lifecycle-jammer lifecycle-source)
  (has-location lifecycle-connector lifecycle-source)
  (has-location lifecycle-fan lifecycle-source))


;;;; CHARACTERIZATION HELPERS ;;;;


(define-query located-exactly
    (?object mobile-object ?expected-location location)
  (and
    (has-location ?object ?expected-location)
    (do (bind (has-location ?object $bound-location))
        (eql $bound-location ?expected-location))
    (not (exists (?other-location location)
           (and (different ?other-location ?expected-location)
                (has-location ?object ?other-location))))))


(define-query unlocated (?object mobile-object)
  (not (bind (has-location ?object $any-location))))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query location-scenarios-valid ()
  (and
    ;; Every mobile-object leaf supports direct lookup and fluent binding, with
    ;; no competing location for the same keyed object.
    (located-exactly static-agent static-agent-site)
    (located-exactly static-box static-box-site)
    (located-exactly static-jammer static-jammer-site)
    (located-exactly static-connector static-connector-site)
    (located-exactly static-fan static-fan-site)

    ;; Replacement and removal completed for every mobile leaf.
    (location-lifecycle-phase lifecycle-complete)
    (not (location-lifecycle-phase moved-to-destination))
    (unlocated lifecycle-agent)
    (unlocated lifecycle-box)
    (unlocated lifecycle-jammer)
    (unlocated lifecycle-connector)
    (unlocated lifecycle-fan)

    ;; Explicitly exclude both authored lifecycle locations so the absence
    ;; cannot pass through an unintended value.
    (not (has-location lifecycle-agent lifecycle-source))
    (not (has-location lifecycle-agent lifecycle-destination))
    (not (has-location lifecycle-box lifecycle-source))
    (not (has-location lifecycle-box lifecycle-destination))
    (not (has-location lifecycle-jammer lifecycle-source))
    (not (has-location lifecycle-jammer lifecycle-destination))
    (not (has-location lifecycle-connector lifecycle-source))
    (not (has-location lifecycle-connector lifecycle-destination))
    (not (has-location lifecycle-fan lifecycle-source))
    (not (has-location lifecycle-fan lifecycle-destination))))


(define-goal
  (location-scenarios-valid))

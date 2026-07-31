;;; Filename: problem-beam-occlusion-test.lisp

;;; Dedicated regression for the shared -beam-occlusion role.  Independent
;;; fixtures characterize every BEAM-BLOCKER leaf and both live queries:
;;;
;;;   1. A raised agent exercises inclusive lower/upper span boundaries.
;;;   2. Two ground boxes exercise default height and existential location
;;;      occlusion when only the taller blocker spans the requested elevation.
;;;   3. A connector on a box exercises recursively supported elevation.
;;;   4. An explicitly empty location remains clear.
;;;   5. A jammer moves from default ground to elevation seven, proving that
;;;      occlusion follows live HAS-LOCATION state and recomputes its base.
;;;
;;; The lifecycle action is gated by the jammer's initial occlusion.  The final
;;; characterization requires the origin clear and the destination occluded at
;;; both inclusive boundaries, so unrelated behavior cannot solve the test.
;;; Expected minimum path length: one.

(in-package :ww)


(ww-set *problem-name* beam-occlusion-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 1)


;;;; TYPES ;;;;


(define-types
  agent (raised-agent)
  location (raised-agent-site
            mixed-box-site
            connector-stack-site
            empty-site
            lifecycle-origin
            lifecycle-destination)
  box (default-box tall-box connector-support-box)
  jammer (lifecycle-jammer)
  connector (supported-connector)
  plate (unused-plate)
  fan (unused-fan))


;;;; TECHNOLOGY INCLUDE ;;;;


(include-tech -beam-occlusion)


;;;; INITIALIZATION ;;;;


(define-init
  ;; Explicit nonzero base and height: the agent spans [3,5].
  (has-elevation raised-agent-site 3)
  (has-location raised-agent raised-agent-site)
  (has-height raised-agent 2)

  ;; DEFAULT-BOX spans [0,1].  TALL-BOX shares the location and spans [0,3],
  ;; so only the latter witnesses location occlusion at elevation two.
  (has-location default-box mixed-box-site)
  (has-location tall-box mixed-box-site)
  (has-height tall-box 3)

  ;; The support box spans [0,2].  The connector stands on its top and, with
  ;; height three, spans [2,5].
  (has-location connector-support-box connector-stack-site)
  (has-height connector-support-box 2)
  (has-location supported-connector connector-stack-site)
  (has-height supported-connector 3)
  (on supported-connector connector-support-box)

  ;; The jammer begins on ordinary ground with height one.  Its destination's
  ;; floor is elevation seven.
  (has-location lifecycle-jammer lifecycle-origin)
  (has-height lifecycle-jammer 1)
  (has-elevation lifecycle-destination 7))


;;;; LIFECYCLE TRIGGER ;;;;


(define-action move-lifecycle-jammer
  1
  ()
  (and
    (has-location lifecycle-jammer lifecycle-origin)
    (beam-blocker-occludes-location lifecycle-origin 0)
    (beam-blocker-occludes-location lifecycle-origin 1)
    (not (beam-blocker-occludes-location lifecycle-destination 7)))
  ("> lifecycle jammer moves to the raised destination")
  (assert
    (has-location lifecycle-jammer lifecycle-destination)))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query beam-occlusion-only-on
    (?occupant support-occupant ?expected-support support)
  (and
    (on ?occupant ?expected-support)
    (not (exists (?other-support support)
           (and
             (different ?other-support ?expected-support)
             (on ?occupant ?other-support))))))


(define-query stable-beam-occlusion-scenarios-valid ()
  (and
    ;; Raised agent: both boundaries are inclusive; adjacent values are clear.
    (= (occupant-elevation raised-agent) 3)
    (= (declared-height raised-agent) 2)
    (beam-blocker-spans-elevation raised-agent 3)
    (beam-blocker-spans-elevation raised-agent 5)
    (not (beam-blocker-spans-elevation raised-agent 2))
    (not (beam-blocker-spans-elevation raised-agent 6))
    (beam-blocker-occludes-location raised-agent-site 3)
    (beam-blocker-occludes-location raised-agent-site 5)
    (not (beam-blocker-occludes-location raised-agent-site 2))
    (not (beam-blocker-occludes-location raised-agent-site 6))

    ;; Default height is one.  At elevation two only TALL-BOX spans, which is
    ;; sufficient for the existential location query.
    (not (bind (has-height default-box $default-box-height)))
    (= (occupant-elevation default-box) 0)
    (= (declared-height default-box) 1)
    (beam-blocker-spans-elevation default-box 0)
    (beam-blocker-spans-elevation default-box 1)
    (not (beam-blocker-spans-elevation default-box -1))
    (not (beam-blocker-spans-elevation default-box 2))

    (= (occupant-elevation tall-box) 0)
    (= (declared-height tall-box) 3)
    (beam-blocker-spans-elevation tall-box 2)
    (beam-blocker-occludes-location mixed-box-site 2)
    (beam-blocker-occludes-location mixed-box-site 3)
    (not (beam-blocker-occludes-location mixed-box-site -1))
    (not (beam-blocker-occludes-location mixed-box-site 4))

    ;; ON takes precedence over the connector's coincident location fact.
    (beam-occlusion-only-on supported-connector connector-support-box)
    (= (support-top-elevation connector-support-box) 2)
    (= (occupant-elevation supported-connector) 2)
    (= (declared-height supported-connector) 3)
    (beam-blocker-spans-elevation supported-connector 2)
    (beam-blocker-spans-elevation supported-connector 5)
    (not (beam-blocker-spans-elevation supported-connector 1))
    (not (beam-blocker-spans-elevation supported-connector 6))

    ;; No blocker is located here at any elevation.
    (not (exists (?blocker beam-blocker)
           (has-location ?blocker empty-site)))
    (not (beam-blocker-occludes-location empty-site 0))
    (not (beam-blocker-occludes-location empty-site 4))))


(define-query lifecycle-beam-occlusion-scenario-valid ()
  (and
    ;; HAS-LOCATION is functional: the action replaces, rather than retains,
    ;; the initial origin.
    (has-location lifecycle-jammer lifecycle-destination)
    (not (has-location lifecycle-jammer lifecycle-origin))
    (not (exists (?support support)
           (on lifecycle-jammer ?support)))

    ;; The same unit-height jammer now spans [7,8].
    (= (occupant-elevation lifecycle-jammer) 7)
    (= (declared-height lifecycle-jammer) 1)
    (beam-blocker-spans-elevation lifecycle-jammer 7)
    (beam-blocker-spans-elevation lifecycle-jammer 8)
    (not (beam-blocker-spans-elevation lifecycle-jammer 6))
    (not (beam-blocker-spans-elevation lifecycle-jammer 9))

    ;; Occlusion follows the move immediately.
    (not (beam-blocker-occludes-location lifecycle-origin 0))
    (not (beam-blocker-occludes-location lifecycle-origin 1))
    (beam-blocker-occludes-location lifecycle-destination 7)
    (beam-blocker-occludes-location lifecycle-destination 8)
    (not (beam-blocker-occludes-location lifecycle-destination 6))
    (not (beam-blocker-occludes-location lifecycle-destination 9))))


(define-query beam-occlusion-scenarios-valid ()
  (and
    (stable-beam-occlusion-scenarios-valid)
    (lifecycle-beam-occlusion-scenario-valid)))


(define-goal
  (beam-occlusion-scenarios-valid))

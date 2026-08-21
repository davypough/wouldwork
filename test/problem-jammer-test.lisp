;;; Filename: problem-jammer-test.lisp

;;; Dedicated jammer regression for the target and support branches not directly
;;; covered by the gun tests.  Four isolated lanes require:
;;;
;;;   1. JAM-TARGET against a gate through LOS-VIA, choosing a plate
;;;      placement so the retained jam both depresses the plate and opens the gate.
;;;   2. JAM-TARGET against wall gears from a distinct visible vantage, resolving
;;;      sight through the gears' HAS-POSITION location and stopping the gears.
;;;   3. JAM-TARGET against floor gears at their exact HAS-POSITION location,
;;;      succeeding without any authored LOS fact and stopping the gears.
;;;   4. PICKUP-JAMMER remotely retrieving a jammer that rests on a plate and
;;;      suppresses wall gears.  Pickup must clear location, support, and jamming;
;;;      propagation must clear the plate and restart the fixed wall blower.
;;;
;;; The first three agents begin holding their dedicated jammers so each positive
;;; targeting branch requires exactly one action.  The lifecycle agent begins
;;; empty-handed and requires exactly one pickup.  The four lanes have disconnected
;;; manipulation topology and exact target assertions, so no action can satisfy
;;; another lane.  Expected minimum path length: four actions, in any order.

(in-package :ww)


(ww-set *problem-name* jammer-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 4)

(setf *expected-min-length* 4)


;;;; TYPES ;;;;


(define-types
  agent (gate-agent wall-agent floor-agent lifecycle-agent disallowed-agent)
  location (gate-site
            wall-vantage
            wall-gears-site
            floor-site
            lifecycle-agent-site
            lifecycle-gears-site
            disallowed-site)
  jammer (gate-jammer wall-jammer floor-jammer lifecycle-jammer disallowed-jammer)
  gate (gate-target)
  pressure-plate (gate-plate lifecycle-plate)
  wall-gears (wall-target)
  wall-blower (lifecycle-target)
  floor-gears (floor-target disallowed-gears)
  )


;;;; TECHNOLOGY INCLUDES ;;;;


(include-tech plate)
(include-tech jammer)
(include-tech gate)
(include-tech -gears-fan)
(include-tech reachability)
(include-tech visibility)


;;;; INITIALIZATION ;;;;


(define-init
  ;; Gate target: the only useful successor places GATE-JAMMER on GATE-PLATE.
  (has-location gate-agent gate-site)
  (holding gate-agent gate-jammer)
  (has-position gate-plate gate-site)
  (has-elevation gate-target 2)
  (has-height gate-target 6)
  (los-via gate-site () gate-target)

  ;; Wall gears target: the jammer remains at the distinct vantage and sees the
  ;; gears through their fixed-position location.
  (has-location wall-agent wall-vantage)
  (holding wall-agent wall-jammer)
  (has-position wall-target wall-gears-site)
  (los-via wall-vantage () wall-gears-site)

  ;; Floor gears target: exact placement/fixture location equality is sufficient;
  ;; no LOS fact is authored for this lane.
  (has-location floor-agent floor-site)
  (holding floor-agent floor-jammer)
  (has-position floor-target floor-site)

  ;; Pickup lifecycle: the jammer starts on a plate, actively suppressing the
  ;; uncontrolled fixed wall blower.  The agent can reach across one
  ;; authored empty-barrier edge but never moves.
  (has-location lifecycle-agent lifecycle-agent-site)
  (has-location lifecycle-jammer lifecycle-gears-site)
  (has-position lifecycle-plate lifecycle-gears-site)
  (on lifecycle-jammer lifecycle-plate)
  (jamming lifecycle-jammer lifecycle-target)
  (has-position lifecycle-target lifecycle-gears-site)
  (reach-via lifecycle-agent-site () lifecycle-gears-site)

  ;; Disallowed-pairing negative probe: geometrically and visually legal (placement
  ;; exactly at the gears' own has-position location), but an authored JAM-DISALLOWED>
  ;; fact blocks this exact agent/placement/target triple.
  (has-location disallowed-agent disallowed-site)
  (holding disallowed-agent disallowed-jammer)
  (has-position disallowed-gears disallowed-site)
  (jam-disallowed> disallowed-site disallowed-site disallowed-gears))


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!)))


;;;; CHARACTERIZATION HELPERS ;;;;


(define-query jammer-jams-only
    (?jammer jammer ?intended target)
  (and (jamming ?jammer ?intended)
       (not (exists (?other target)
              (and (different ?other ?intended)
                   (jamming ?jammer ?other))))))


(define-test-helper jammer-action-applicable-p (state action-name args)
  "Whether the installed jammer action accepts ARGS in STATE."
  (let ((action (find action-name *actions* :key #'action.name)))
    (and (member args (get-precondition-args action state) :test #'equal)
         (apply (action.pre-defun-name action) state args))))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query jammer-scenarios-valid ()
  (and
    ;; Gate branch and support placement: the plate successor is required, not
    ;; the simultaneously available ground-placement successor.
    (has-location gate-agent gate-site)
    (not (holding gate-agent gate-jammer))
    (has-location gate-jammer gate-site)
    (on gate-jammer gate-plate)
    (not (cleartop gate-plate))
    (depressed gate-plate)
    (jammer-jams-only gate-jammer gate-target)
    (open gate-target)
    (= (base gate-target) 2)
    (= (top gate-target) 8)
    (= (jammer-target-elevation gate-target) 5)

    ;; Wall-gears branch: visible from a distinct placement location, then placed
    ;; on bare ground and propagated to the stopped state.
    (has-location wall-agent wall-vantage)
    (not (holding wall-agent wall-jammer))
    (has-location wall-jammer wall-vantage)
    (not (exists (?support support)
           (on wall-jammer ?support)))
    (visible wall-vantage wall-gears-site)
    (= (jammer-target-elevation wall-target)
       (blower-elevation wall-target))
    (jammer-jams-only wall-jammer wall-target)
    (not (turning wall-target))

    ;; Floor-gears exact-location branch succeeds despite having no sightline.
    (has-location floor-agent floor-site)
    (not (holding floor-agent floor-jammer))
    (has-location floor-jammer floor-site)
    (not (exists (?support support)
           (on floor-jammer ?support)))
    (not (potentially-visible floor-site floor-site))
    (= (jammer-target-elevation floor-target)
       (blower-elevation floor-target))
    (jammer-jams-only floor-jammer floor-target)
    (not (turning floor-target))

    ;; Pickup clears all three jammer-owned state facts and normal propagation
    ;; restores the target and support-derived state.
    (has-location lifecycle-agent lifecycle-agent-site)
    (holding lifecycle-agent lifecycle-jammer)
    (not (exists (?location location)
           (has-location lifecycle-jammer ?location)))
    (not (exists (?support support)
           (on lifecycle-jammer ?support)))
    (not (exists (?target target)
           (jamming lifecycle-jammer ?target)))
    (cleartop lifecycle-plate)
    (not (depressed lifecycle-plate))
    (turning lifecycle-target)
    (blowing lifecycle-target)

    ;; Important cross-lane absences.
    (not (jamming gate-jammer wall-target))
    (not (jamming wall-jammer floor-target))
    (not (jamming floor-jammer gate-target))

    ;; An otherwise legal target/placement pairing is blocked by an authored
    ;; JAM-DISALLOWED> fact naming this exact agent-location/placement/target triple.
    (has-location disallowed-agent disallowed-site)
    (holding disallowed-agent disallowed-jammer)
    (has-position disallowed-gears disallowed-site)
    (jam-disallowed> disallowed-site disallowed-site disallowed-gears)
    (not (jammer-action-applicable-p
           state 'jam-target '(disallowed-agent disallowed-gears disallowed-site)))))


(define-goal
  (jammer-scenarios-valid))


;;;; MUTATION CHARACTERIZATION ;;;;


(define-action-precondition-mutation jam-target-ignores-disallowed jam-target
  (and (bind (holding ?agent $any-jammer))
       (jammer $any-jammer)
       (bind (has-location ?agent $a-location))
       (reachable ?location $a-location)
       (assign $places
               (placement-options ?agent ?location $any-jammer))
       (assign $visible-places
               (jammer-visible-placement-options
                 ?agent ?location $any-jammer ?target $places))
       (not (null $visible-places)))
  "Drops JAM-TARGET's authored disallowance guard.  The disallowed pairing probe
   must then make this characterization fail.")


(define-update-mutation gate-update-ignores-jamming update-gate-status!
  ()
  (doall (?gate gate)
    (do (assign $control-on nil)
        (if (bind (controls $clauses ?gate $mode))
          (do (assign $any-clause-on
                (ww-loop for $clause in $clauses
                         thereis (ww-loop for $c in $clause
                                          always (energized $c))))
              (if (eql $mode 'normal)
                (assign $control-on $any-clause-on)
                (if (eql $mode 'inverted)
                  (assign $control-on (not $any-clause-on))))))
        (if $control-on
          (open ?gate)
          (not (open ?gate)))))
  "Drops UPDATE-GATE-STATUS!'s jamming override.  GATE-TARGET must then remain
   closed and make this characterization fail.")

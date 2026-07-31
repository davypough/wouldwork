;;; Filename: problem-placement-test.lisp

;;; Dedicated regression for the shared -placement role.  Three isolated scenarios
;;; exercise:
;;;
;;;   1. PUT-BOX placing a held box on a clear floor-mounted fan exactly two elevation
;;;      units below its height-two agent.  The simultaneous ground successor cannot
;;;      satisfy the goal.
;;;   2. PLACEMENT-OPTIONS returning exactly ground, a clear plate, a clear box, and a
;;;      clear floor-mounted fan while excluding occupied supports, a loose fan, a
;;;      wall-mounted fan, and the candidate object itself.
;;;   3. Symmetric vertical reach: ground and a plate are offered exactly two units
;;;      above and below a height-two agent, but no option is offered three units away.
;;;
;;; Only the first scenario changes state.  Its goal verifies PLACE-HELD-OBJECT! releases
;;; the hold, establishes the location and fan support, and leaves no competing support
;;; fact.  The other agents retain their held probes, making their characterization
;;; fixtures unavailable as alternate solutions.  Expected minimum path length: one
;;; action, PUT-BOX on LIFECYCLE-FAN.

(in-package :ww)


(ww-set *problem-name* placement-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 1)


;;;; TYPES ;;;;


(define-types
  agent (lifecycle-agent matrix-agent boundary-agent
         occupied-plate-rider occupied-box-rider occupied-fan-rider)
  location (lifecycle-origin lifecycle-target matrix-site control-site
            boundary-origin upper-boundary lower-boundary
            too-high-boundary too-low-boundary)
  plate (control-plate clear-matrix-plate occupied-matrix-plate
         upper-boundary-plate lower-boundary-plate
         too-high-plate too-low-plate)
  box (lifecycle-box matrix-probe-box boundary-probe-box
       clear-support-box occupied-support-box)
  floor-gears (lifecycle-gears clear-fan-gears occupied-fan-gears)
  wall-gears (wall-fan-gears)
  fan (lifecycle-fan clear-floor-fan occupied-floor-fan loose-fan wall-fan)
  mode (normal))


;;;; TECHNOLOGY INCLUDES ;;;;


(include-tech plate)
(include-tech box)
(include-tech gears-fan)
(include-tech reachability)


;;;; INITIALIZATION ;;;;


(define-init
  ;; Lifecycle: the fan top at elevation 0 is exactly at the lower reach boundary
  ;; of the height-two agent standing at elevation 2.
  (has-location lifecycle-agent lifecycle-origin)
  (has-height lifecycle-agent 2)
  (has-elevation lifecycle-origin 2)
  (holding lifecycle-agent lifecycle-box)
  (has-position lifecycle-gears lifecycle-target)
  (has-location lifecycle-fan lifecycle-target)
  (mounted-on lifecycle-fan lifecycle-gears)
  (reach-via lifecycle-origin () lifecycle-target)

  ;; Complete support matrix.  Rider agents occupy the negative fixtures without
  ;; becoming additional box candidates themselves.
  (has-location matrix-agent matrix-site)
  (has-height matrix-agent 2)
  (holding matrix-agent matrix-probe-box)

  (has-position clear-matrix-plate matrix-site)
  (has-position occupied-matrix-plate matrix-site)
  (has-location occupied-plate-rider matrix-site)
  (on occupied-plate-rider occupied-matrix-plate)

  (has-location clear-support-box matrix-site)
  (has-height clear-support-box 2)
  (has-location occupied-support-box matrix-site)
  (has-location occupied-box-rider matrix-site)
  (on occupied-box-rider occupied-support-box)

  (has-position clear-fan-gears matrix-site)
  (has-location clear-floor-fan matrix-site)
  (mounted-on clear-floor-fan clear-fan-gears)

  (has-position occupied-fan-gears matrix-site)
  (has-location occupied-floor-fan matrix-site)
  (mounted-on occupied-floor-fan occupied-fan-gears)
  (has-location occupied-fan-rider matrix-site)
  (on occupied-fan-rider occupied-floor-fan)

  (has-location loose-fan matrix-site)
  (has-position wall-fan-gears matrix-site)
  (mounted-on wall-fan wall-fan-gears)

  ;; Symmetric inclusive boundaries around elevation 5.  Each destination has a
  ;; plate so both the ground and plate branches cross the same inequality.
  (has-location boundary-agent boundary-origin)
  (has-height boundary-agent 2)
  (has-elevation boundary-origin 5)
  (holding boundary-agent boundary-probe-box)

  (has-elevation upper-boundary 7)
  (has-position upper-boundary-plate upper-boundary)
  (has-elevation lower-boundary 3)
  (has-position lower-boundary-plate lower-boundary)
  (has-elevation too-high-boundary 8)
  (has-position too-high-plate too-high-boundary)
  (has-elevation too-low-boundary 2)
  (has-position too-low-plate too-low-boundary)

  ;; Keep every mounted fan stopped so the placement fixtures remain ordinary,
  ;; stable supports throughout the one-action search.
  (has-position control-plate control-site)
  (controls ((control-plate)) lifecycle-gears normal)
  (controls ((control-plate)) clear-fan-gears normal)
  (controls ((control-plate)) occupied-fan-gears normal)
  (controls ((control-plate)) wall-fan-gears normal)
  (aimed-at> lifecycle-gears control-site)
  (aimed-at> clear-fan-gears control-site)
  (aimed-at> occupied-fan-gears control-site)
  (aimed-at> wall-fan-gears control-site))


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!)))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query placement-scenarios-valid ()
  (and
    ;; The only required action chooses the fan successor instead of ground.
    (has-location lifecycle-agent lifecycle-origin)
    (not (holding lifecycle-agent lifecycle-box))
    (has-location lifecycle-box lifecycle-target)
    (on lifecycle-box lifecycle-fan)
    (not (exists (?support support)
           (and (not (eql ?support 'lifecycle-fan))
                (on lifecycle-box ?support))))
    (mounted-on lifecycle-fan lifecycle-gears)
    (has-location lifecycle-fan lifecycle-target)
    (not (cleartop lifecycle-fan))
    (= (occupant-elevation lifecycle-agent) 2)
    (= (support-top-elevation lifecycle-fan) 0)
    (within-agent-vertical-reach lifecycle-agent 0)

    ;; The matrix probe remains held while its direct query returns exactly one
    ;; candidate of each supported kind.
    (holding matrix-agent matrix-probe-box)
    (not (exists (?location location)
           (has-location matrix-probe-box ?location)))
    (do (assign $places
                (placement-options matrix-agent matrix-site matrix-probe-box))
        (and (= (length $places) 4)
             (member 'ground $places)
             (member 'clear-matrix-plate $places)
             (member 'clear-support-box $places)
             (member 'clear-floor-fan $places)
             (not (member 'occupied-matrix-plate $places))
             (not (member 'occupied-support-box $places))
             (not (member 'occupied-floor-fan $places))
             (not (member 'loose-fan $places))
             (not (member 'wall-fan $places))))

    ;; Each movable support is excluded when passed as the hypothetical object
    ;; being placed, while the other three legal choices remain available.
    (do (assign $without-box
                (placement-options matrix-agent matrix-site clear-support-box))
        (and (= (length $without-box) 3)
             (member 'ground $without-box)
             (member 'clear-matrix-plate $without-box)
             (member 'clear-floor-fan $without-box)
             (not (member 'clear-support-box $without-box))))
    (do (assign $without-fan
                (placement-options matrix-agent matrix-site clear-floor-fan))
        (and (= (length $without-fan) 3)
             (member 'ground $without-fan)
             (member 'clear-matrix-plate $without-fan)
             (member 'clear-support-box $without-fan)
             (not (member 'clear-floor-fan $without-fan))))

    ;; The negative support facts are explicit, so a missing option cannot pass
    ;; merely because its fixture was initialized incorrectly.
    (cleartop clear-matrix-plate)
    (not (cleartop occupied-matrix-plate))
    (cleartop clear-support-box)
    (not (cleartop occupied-support-box))
    (mounted-on clear-floor-fan clear-fan-gears)
    (cleartop clear-floor-fan)
    (mounted-on occupied-floor-fan occupied-fan-gears)
    (not (cleartop occupied-floor-fan))
    (not (exists (?gears gears)
           (mounted-on loose-fan ?gears)))
    (has-location loose-fan matrix-site)
    (mounted-on wall-fan wall-fan-gears)
    (not (exists (?location location)
           (has-location wall-fan ?location)))

    ;; Absolute reach is inclusive in both directions and rejects one unit beyond.
    (holding boundary-agent boundary-probe-box)
    (= (occupant-elevation boundary-agent) 5)
    (within-agent-vertical-reach boundary-agent 7)
    (within-agent-vertical-reach boundary-agent 3)
    (not (within-agent-vertical-reach boundary-agent 8))
    (not (within-agent-vertical-reach boundary-agent 2))
    (do (assign $upper
                (placement-options
                  boundary-agent upper-boundary boundary-probe-box))
        (and (= (length $upper) 2)
             (member 'ground $upper)
             (member 'upper-boundary-plate $upper)))
    (do (assign $lower
                (placement-options
                  boundary-agent lower-boundary boundary-probe-box))
        (and (= (length $lower) 2)
             (member 'ground $lower)
             (member 'lower-boundary-plate $lower)))
    (not (placement-options
           boundary-agent too-high-boundary boundary-probe-box))
    (not (placement-options
           boundary-agent too-low-boundary boundary-probe-box))

    ;; The shared clear control remains inert, keeping every fan fixture stopped.
    (cleartop control-plate)
    (not (depressed control-plate))
    (not (turning lifecycle-gears))
    (not (blowing lifecycle-fan))
    (not (turning clear-fan-gears))
    (not (blowing clear-floor-fan))
    (not (turning occupied-fan-gears))
    (not (blowing occupied-floor-fan))
    (not (turning wall-fan-gears))
    (not (blowing wall-fan))))


(define-goal
  (placement-scenarios-valid))

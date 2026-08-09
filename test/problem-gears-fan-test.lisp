;;; Filename: problem-gears-fan-test.lisp

;;; Dedicated regression for the fan mounting/manipulation actions shared by floor,
;;; wall, and angled blowers.  Five isolated lanes require:
;;;
;;;   1. PUT-FAN to place a held fan on a plate.
;;;   2. PICKUP-FAN's ordinary located-fan branch to remove a clear floor-mounted fan.
;;;   3. PICKUP-FAN's wall-mounted branch at the inclusive vertical-reach boundary.
;;;   4. MOUNT-FAN's flush branch to attach a held fan to angled gears.
;;;   5. MOUNT-FAN's wall branch at the inclusive vertical-reach boundary.
;;;
;;; The goal also probes the installed action preconditions directly.  A welded fan, an
;;; occupied fan, occupied gears, wall gears one unit beyond reach, and gears at a
;;; disconnected location must remain unusable.  Every lane has its own agent and isolated
;;; topology, so each positive result requires one distinct unit-cost action and no agent
;;; can satisfy another lane's goal.  Expected minimum path length: five actions, in any
;;; order.
;;;
;;; The characterization assertions distinguish mounting from support placement, verify
;;; floor/angled versus wall location ownership, and check that propagation clears BLOWING
;;; after pickup and derives it after mounting.


(in-package :ww)


(ww-set *problem-name* gears-fan-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 5)

(setf *expected-min-length* 5)


;;;; TYPES ;;;;


(define-types
  agent (placing-agent floor-pickup-agent wall-pickup-agent
         angled-mounting-agent wall-mounting-agent)
  location (placing-site floor-pickup-site
            wall-pickup-site wall-pickup-fixture-site
            angled-mounting-site
            wall-mounting-site wall-mounting-fixture-site
            remote-site unused-destination)
  pressure-plate (placement-plate off-plate)
  box (occupant-box)
  floor-gears (floor-pickup-gears welded-gears occupied-fan-gears
                occupied-gears remote-gears)
  wall-gears (wall-pickup-gears wall-mounting-gears high-wall-gears)
  angled-gears (angled-mounting-gears)
  fan (fan-to-place floor-pickup-fan wall-pickup-fan
       angled-mounting-fan wall-mounting-fan
       welded-fan occupied-fan gear-occupant-fan))


;;;; TECHNOLOGY INCLUDES ;;;;


(include-tech plate)
(include-tech floor-blower)
(include-tech wall-blower)
(include-tech angled-blower)
(include-tech reachability)


;;;; INITIALIZATION ;;;;


(define-init
  ;; The five agents never move.  Only the two wall-manipulation lanes have authored
  ;; reach edges, each joining its agent site to an otherwise empty fixture site.
  (has-location placing-agent placing-site)
  (has-location floor-pickup-agent floor-pickup-site)
  (has-location wall-pickup-agent wall-pickup-site)
  (has-location angled-mounting-agent angled-mounting-site)
  (has-location wall-mounting-agent wall-mounting-site)
  (reach-via wall-pickup-site () wall-pickup-fixture-site)
  (reach-via wall-mounting-site () wall-mounting-fixture-site)

  ;; PUT-FAN lane: the plate is clear initially and is the required final placement.
  (holding placing-agent fan-to-place)
  (has-position placement-plate placing-site)

  ;; Every gears set has a harmless fixed stream destination, satisfying the blower
  ;; topology without placing any occupant in an air stream.
  (aimed-at floor-pickup-gears unused-destination)
  (aimed-at welded-gears unused-destination)
  (aimed-at occupied-fan-gears unused-destination)
  (aimed-at occupied-gears unused-destination)
  (aimed-at remote-gears unused-destination)
  (aimed-at wall-pickup-gears unused-destination)
  (aimed-at wall-mounting-gears unused-destination)
  (aimed-at high-wall-gears unused-destination)
  (aimed-at angled-mounting-gears unused-destination)

  ;; Located pickup lane: a floor-mounted fan is both located and attached.
  (has-position floor-pickup-gears floor-pickup-site)
  (has-location floor-pickup-fan floor-pickup-site)
  (mounted-on floor-pickup-fan floor-pickup-gears)

  ;; Wall pickup lane: the fan hangs without a location.  Elevation 2 is exactly the
  ;; default-height agent's inclusive reach from the elevation-0 floor.
  (has-position wall-pickup-gears wall-pickup-fixture-site)
  (has-elevation wall-pickup-gears 2)
  (mounted-on wall-pickup-fan wall-pickup-gears)

  ;; Flush angled mounting lane.
  (holding angled-mounting-agent angled-mounting-fan)
  (has-position angled-mounting-gears angled-mounting-site)

  ;; Wall mounting lane, also at the inclusive reach boundary.
  (holding wall-mounting-agent wall-mounting-fan)
  (has-position wall-mounting-gears wall-mounting-fixture-site)
  (has-elevation wall-mounting-gears 2)

  ;; Negative pickup fixtures at PLACING-SITE.  WELDED-FAN is otherwise clear and
  ;; reachable; OCCUPIED-FAN is otherwise removable but carries OCCUPANT-BOX.
  (has-position welded-gears placing-site)
  (has-location welded-fan placing-site)
  (mounted-on welded-fan welded-gears)
  (welded welded-fan welded-gears)

  (has-position occupied-fan-gears placing-site)
  (has-location occupied-fan placing-site)
  (mounted-on occupied-fan occupied-fan-gears)
  (has-location occupant-box placing-site)
  (on occupant-box occupied-fan)
  (has-position off-plate remote-site)
  (controls ((off-plate)) occupied-fan-gears normal)

  ;; Negative mounting fixtures for FLOOR-PICKUP-AGENT after it holds its fan.
  ;; OCCUPIED-GEARS fails only vacancy; HIGH-WALL-GEARS is one unit beyond the exact
  ;; positive boundary; REMOTE-GEARS fails identity-only reachability.
  (has-position occupied-gears floor-pickup-site)
  (has-location gear-occupant-fan floor-pickup-site)
  (mounted-on gear-occupant-fan occupied-gears)

  (has-position high-wall-gears floor-pickup-site)
  (has-elevation high-wall-gears 3)

  (has-position remote-gears remote-site))


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!)))


;;;; ACTION-PRECONDITION CHARACTERIZATION ;;;;


(define-test-helper fan-action-applicable-p (state action-name args)
  "Whether the installed fan action accepts ARGS in STATE."
  (let ((action (find action-name *actions* :key #'action.name)))
    (and (member args (get-precondition-args action state) :test #'equal)
         (apply (action.pre-defun-name action) state args))))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query gears-fan-scenarios-valid ()
  (and
    ;; PUT-FAN releases the hold, establishes an ordinary location/support placement,
    ;; depresses the plate through propagation, and never creates a gears attachment.
    (has-location placing-agent placing-site)
    (not (holding placing-agent fan-to-place))
    (has-location fan-to-place placing-site)
    (on fan-to-place placement-plate)
    (depressed placement-plate)
    (not (exists (?gears gears)
           (mounted-on fan-to-place ?gears)))
    (not (blowing fan-to-place))

    ;; The located pickup branch removes both the floor location and attachment, then
    ;; propagation retracts the stale blowing state.
    (has-location floor-pickup-agent floor-pickup-site)
    (holding floor-pickup-agent floor-pickup-fan)
    (not (exists (?location location)
           (has-location floor-pickup-fan ?location)))
    (not (exists (?gears gears)
           (mounted-on floor-pickup-fan ?gears)))
    (not (blowing floor-pickup-fan))
    (turning floor-pickup-gears)

    ;; The wall pickup branch succeeds exactly two elevation units above the agent and
    ;; likewise clears mounting/blowing without inventing a location.
    (= (gears-elevation wall-pickup-gears) 2)
    (within-agent-vertical-reach wall-pickup-agent 2)
    (has-location wall-pickup-agent wall-pickup-site)
    (holding wall-pickup-agent wall-pickup-fan)
    (not (exists (?location location)
           (has-location wall-pickup-fan ?location)))
    (not (exists (?gears gears)
           (mounted-on wall-pickup-fan ?gears)))
    (not (blowing wall-pickup-fan))
    (turning wall-pickup-gears)

    ;; Angled gears use the flush floor-mounting effect: the fan gains the gears'
    ;; location, attaches, and blows on the already-turning uncontrolled gears.
    (has-location angled-mounting-agent angled-mounting-site)
    (not (holding angled-mounting-agent angled-mounting-fan))
    (mounted-on angled-mounting-fan angled-mounting-gears)
    (has-location angled-mounting-fan angled-mounting-site)
    (not (exists (?support support)
           (on angled-mounting-fan ?support)))
    (turning angled-mounting-gears)
    (blowing angled-mounting-fan)

    ;; Wall mounting has the same inclusive elevation-2 boundary but deliberately adds
    ;; no location, keeping the fan unavailable to support placement.
    (= (gears-elevation wall-mounting-gears) 2)
    (within-agent-vertical-reach wall-mounting-agent 2)
    (has-location wall-mounting-agent wall-mounting-site)
    (not (holding wall-mounting-agent wall-mounting-fan))
    (mounted-on wall-mounting-fan wall-mounting-gears)
    (not (exists (?location location)
           (has-location wall-mounting-fan ?location)))
    (not (exists (?support support)
           (on wall-mounting-fan ?support)))
    (turning wall-mounting-gears)
    (blowing wall-mounting-fan)

    ;; Welding is the only failed condition for this otherwise clear, reachable,
    ;; floor-mounted fan.
    (mounted-on welded-fan welded-gears)
    (welded welded-fan welded-gears)
    (cleartop welded-fan)
    (not (fan-action-applicable-p
           state 'pickup-fan '(placing-agent welded-fan)))

    ;; Occupancy independently blocks pickup.
    (mounted-on occupied-fan occupied-fan-gears)
    (on occupant-box occupied-fan)
    (not (cleartop occupied-fan))
    (not (depressed off-plate))
    (not (turning occupied-fan-gears))
    (not (blowing occupied-fan))
    (not (fan-action-applicable-p
           state 'pickup-fan '(placing-agent occupied-fan)))

    ;; The held floor pickup fan cannot be remounted onto occupied gears.
    (mounted-on gear-occupant-fan occupied-gears)
    (not (fan-action-applicable-p
           state 'mount-fan
           '(floor-pickup-agent floor-pickup-fan occupied-gears)))

    ;; Elevation 3 is exactly one unit beyond a default-height agent's reach.
    (= (gears-elevation high-wall-gears) 3)
    (not (within-agent-vertical-reach floor-pickup-agent 3))
    (not (fan-action-applicable-p
           state 'mount-fan
           '(floor-pickup-agent floor-pickup-fan high-wall-gears)))

    ;; The remote gears are vertically usable but disconnected.
    (= (gears-elevation remote-gears) 0)
    (not (reachable remote-site floor-pickup-site))
    (not (fan-action-applicable-p
           state 'mount-fan
           '(floor-pickup-agent floor-pickup-fan remote-gears)))))


(define-goal
  (gears-fan-scenarios-valid))


;;;; MUTATION CHARACTERIZATION ;;;;


(define-action-precondition-mutation pickup-fan-allows-welded pickup-fan
  (and (bind (has-location ?agent $a-location))
       (or (and (bind (has-location ?fan $fan-location))
                (cleartop ?fan)
                (pickup-clear ?agent $a-location ?fan $fan-location))
           (and (bind (mounted-on ?fan $w-gears))
                (wall-gears $w-gears)
                (not (bind (holding ?agent $any-held)))
                (bind (has-position $w-gears $fan-location))
                (reachable $fan-location $a-location)
                (within-agent-vertical-reach
                  ?agent
                  (gears-elevation $w-gears)))))
  "Drops PICKUP-FAN's not-welded guard.  The welded-fan probe must then make
   this characterization fail.")


(define-action-precondition-mutation mount-fan-allows-occupied-gears mount-fan
  (and (holding ?agent ?fan)
       (bind (has-location ?agent $a-location))
       (bind (has-position ?gears $g-location))
       (reachable $g-location $a-location)
       (within-agent-vertical-reach ?agent (gears-elevation ?gears)))
  "Drops MOUNT-FAN's vacancy guard.  The occupied-gears probe must then make
   this characterization fail.")

;;; Filename: problem-pickup-test.lisp

;;; Dedicated zero-action regression for the shared -pickup role.  Independent
;;; characterization fixtures exercise all three PICKUP-CLEAR gates:
;;;
;;;   1. Empty-handed identity pickup and authored-edge pickup for box, jammer,
;;;      and connector cargo.
;;;   2. Inclusive vertical reach exactly two elevation units above and below a
;;;      height-two agent, with rejection one unit beyond either boundary.
;;;   3. A vertically valid but disconnected connector, rejected only by reachability.
;;;   4. A colocated, vertically valid connector rejected only because its agent
;;;      already holds another object.
;;;   5. An occupied box accepted by PICKUP-CLEAR, because box-top clearance is the
;;;      public PICKUP-BOX action's additional policy rather than part of this role.
;;;
;;; Every query call uses the object's authored HAS-LOCATION fact and the agent's
;;; authored location.  The goal directly characterizes the unchanged initial state;
;;; no pickup action is involved because query eligibility itself is under test.
;;; Expected minimum path length: zero.

(in-package :ww)


(ww-set *problem-name* pickup-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 1)


;;;; TYPES ;;;;


(define-types
  agent (boundary-agent holding-agent occupancy-agent box-rider)
  location (boundary-origin upper-site lower-site same-level-site
            too-high-site too-low-site disconnected-site
            holding-site occupied-site)
  box (identity-box upper-box too-high-box held-box occupied-box)
  jammer (lower-jammer too-low-jammer)
  connector (same-level-connector disconnected-connector available-connector))


;;;; TECHNOLOGY INCLUDES ;;;;


(include-tech -pickup)
(include-tech reachability)


;;;; INITIALIZATION ;;;;


(define-init
  ;; Boundary matrix: the agent stands at elevation 5 with an inclusive reach of 2.
  (has-location boundary-agent boundary-origin)
  (has-height boundary-agent 2)
  (has-elevation boundary-origin 5)

  (has-location identity-box boundary-origin)

  (has-location upper-box upper-site)
  (has-elevation upper-site 7)
  (reach-via boundary-origin () upper-site)

  (has-location lower-jammer lower-site)
  (has-elevation lower-site 3)
  (reach-via boundary-origin () lower-site)

  (has-location same-level-connector same-level-site)
  (has-elevation same-level-site 5)
  (reach-via boundary-origin () same-level-site)

  ;; Both locations are reachable, so only vertical distance rejects these objects.
  (has-location too-high-box too-high-site)
  (has-elevation too-high-site 8)
  (reach-via boundary-origin () too-high-site)

  (has-location too-low-jammer too-low-site)
  (has-elevation too-low-site 2)
  (reach-via boundary-origin () too-low-site)

  ;; Same elevation as the boundary agent but deliberately disconnected.
  (has-location disconnected-connector disconnected-site)
  (has-elevation disconnected-site 5)

  ;; Full-hand negative: every other condition succeeds by identity.
  (has-location holding-agent holding-site)
  (holding holding-agent held-box)
  (has-location available-connector holding-site)

  ;; Occupied-box policy boundary.  The shared role ignores CLEARTOP; PICKUP-BOX
  ;; adds that public action-specific precondition.
  (has-location occupancy-agent occupied-site)
  (has-location box-rider occupied-site)
  (has-location occupied-box occupied-site)
  (on box-rider occupied-box))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query pickup-scenarios-valid ()
  (and
    ;; Positive cargo matrix: identity, upper boundary, lower boundary, and an
    ;; authored same-level reach edge.
    (not (exists (?cargo cargo)
           (holding boundary-agent ?cargo)))
    (= (occupant-elevation boundary-agent) 5)
    (= (declared-height boundary-agent) 2)

    (= (occupant-elevation identity-box) 5)
    (reachable boundary-origin boundary-origin)
    (pickup-clear
      boundary-agent boundary-origin identity-box boundary-origin)

    (= (occupant-elevation upper-box) 7)
    (reachable upper-site boundary-origin)
    (within-agent-vertical-reach boundary-agent 7)
    (pickup-clear boundary-agent boundary-origin upper-box upper-site)

    (= (occupant-elevation lower-jammer) 3)
    (reachable lower-site boundary-origin)
    (within-agent-vertical-reach boundary-agent 3)
    (pickup-clear boundary-agent boundary-origin lower-jammer lower-site)

    (= (occupant-elevation same-level-connector) 5)
    (reachable same-level-site boundary-origin)
    (pickup-clear
      boundary-agent boundary-origin same-level-connector same-level-site)

    ;; One unit beyond either inclusive boundary fails only vertical reach.
    (= (occupant-elevation too-high-box) 8)
    (reachable too-high-site boundary-origin)
    (not (within-agent-vertical-reach boundary-agent 8))
    (not (pickup-clear
           boundary-agent boundary-origin too-high-box too-high-site))

    (= (occupant-elevation too-low-jammer) 2)
    (reachable too-low-site boundary-origin)
    (not (within-agent-vertical-reach boundary-agent 2))
    (not (pickup-clear
           boundary-agent boundary-origin too-low-jammer too-low-site))

    ;; Reachability is independently necessary even at the agent's own elevation.
    (= (occupant-elevation disconnected-connector) 5)
    (within-agent-vertical-reach boundary-agent 5)
    (not (reachable disconnected-site boundary-origin))
    (not (pickup-clear
           boundary-agent boundary-origin
           disconnected-connector disconnected-site))

    ;; Holding any cargo independently rejects an otherwise valid identity pickup.
    (holding holding-agent held-box)
    (has-location available-connector holding-site)
    (= (occupant-elevation holding-agent)
       (occupant-elevation available-connector))
    (reachable holding-site holding-site)
    (within-agent-vertical-reach
      holding-agent (occupant-elevation available-connector))
    (not (pickup-clear
           holding-agent holding-site available-connector holding-site))

    ;; Support occupancy is deliberately outside the shared role.
    (not (exists (?cargo cargo)
           (holding occupancy-agent ?cargo)))
    (on box-rider occupied-box)
    (not (cleartop occupied-box))
    (reachable occupied-site occupied-site)
    (within-agent-vertical-reach
      occupancy-agent (occupant-elevation occupied-box))
    (pickup-clear
      occupancy-agent occupied-site occupied-box occupied-site)))


(define-goal
  (pickup-scenarios-valid))

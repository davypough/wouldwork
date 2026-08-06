;;; Dedicated zero-action regression for -walkability topology and neutral -mobility.
;;;
;;; An authored WALK-VIA edge verifies symmetric relation installation and
;;; preservation of its DNF obstacle clauses.  An authored WALK-VIA> edge
;;; verifies the corresponding directional boundary.  Despite both topology
;;; facts, neutral MOBILITY-RESULTS returns exactly the requested starting location;
;;; only public walkability registers a provider and brings in the MOVE action.
;;;
;;; The initial and final dynamic states are empty.  The characterization query
;;; is already true after staging, so the expected minimum path length is 0.

(in-package :ww)

(ww-set *problem-name* walkability-substrate-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 1)
(setf *expected-min-length* 0)


;;;; TYPES ;;;;


(define-types
  agent (first-agent second-agent)
  location (origin symmetric-neighbor directional-neighbor isolated-site)
  gate (door-a door-b door-c))


;;;; TECHNOLOGY INCLUDE ;;;;


(include-tech -walkability)
(include-tech -mobility)


;;;; STATIC TOPOLOGY ;;;;


(define-init
  (walk-via
    origin
    ((door-a) (door-b door-c))
    symmetric-neighbor)
  (walk-via>
    origin
    ()
    directional-neighbor))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-test-claim walkability-substrate-schema
  (expect-registrations :action '())
  (null (database *start-state*))
  (not (state-is-inconsistent *start-state*)))


(define-query substrate-walk-via-family-is
    (?from location ?to location ?expected)
  (do
    (bind (walk-via ?from $actual ?to))
    (equal $actual ?expected)))


(define-query substrate-walk-via>-family-is
    (?from location ?to location ?expected)
  (do
    (bind (walk-via> ?from $actual ?to))
    (equal $actual ?expected)))


(define-query walkability-substrate-scenarios-valid ()
  (and
    ;; WALK-VIA is symmetric and retains its opaque DNF clause value.
    (substrate-walk-via-family-is
      origin
      symmetric-neighbor
      '((door-a) (door-b door-c)))
    (substrate-walk-via-family-is
      symmetric-neighbor
      origin
      '((door-a) (door-b door-c)))

    ;; WALK-VIA> retains the direct empty clause value but never reverses.
    (substrate-walk-via>-family-is
      origin
      directional-neighbor
      nil)
    (not
      (bind
        (walk-via>
          directional-neighbor
          $unexpected-directional-family
          origin)))

    ;; Authored topology cannot expand mobility without a registered provider.
    (equal
      (mobility-locations first-agent origin)
      '(origin))
    (traversable first-agent origin origin)
    (not (traversable first-agent origin symmetric-neighbor))
    (not (traversable first-agent origin directional-neighbor))

    ;; The default is independent of the agent and works at an isolated start.
    (equal
      (mobility-locations second-agent origin)
      '(origin))
    (equal
      (mobility-locations second-agent isolated-site)
      '(isolated-site))
    (traversable second-agent isolated-site isolated-site)
    (not (traversable second-agent isolated-site origin))))


(define-goal
  (walkability-substrate-scenarios-valid))

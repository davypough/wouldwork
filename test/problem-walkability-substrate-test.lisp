;;; Dedicated zero-action regression for the shared -walkability substrate.
;;;
;;; An authored WALK-VIA edge verifies symmetric relation installation and
;;; preservation of its DNF obstacle clauses.  An authored WALK-VIA> edge
;;; verifies the corresponding directional boundary.  Despite both topology
;;; facts, the substrate's neutral WALKABLE-LOCATIONS implementation must return
;;; exactly the requested starting location; only public walkability may replace
;;; that identity default with a walking closure and install the WALK action.
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


(setf
  (symbol-function 'walkability-substrate-state-valid-p)
  (lambda (state)
    (and
      (null *actions*)
      (null (database state))
      (not (state-is-inconsistent state)))))


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

    ;; Authored topology cannot expand the substrate's identity default.
    (equal
      (walkable-locations first-agent origin)
      '(origin))
    (walkable first-agent origin origin)
    (not (walkable first-agent origin symmetric-neighbor))
    (not (walkable first-agent origin directional-neighbor))

    ;; The default is independent of the agent and works at an isolated start.
    (equal
      (walkable-locations second-agent origin)
      '(origin))
    (equal
      (walkable-locations second-agent isolated-site)
      '(isolated-site))
    (walkable second-agent isolated-site isolated-site)
    (not (walkable second-agent isolated-site origin))

    ;; The role technology has no actions or dynamic state of its own.
    (walkability-substrate-state-valid-p state)))


(define-goal
  (walkability-substrate-scenarios-valid))

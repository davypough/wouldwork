;;; Filename: problem-corner-topo-advisor-cap3.lisp

;;; Cap-3 copy of problem-corner-topo-advisor.lisp, kept to check the enumerator against
;;; the known cap-3 results (1,351 goal states).

;;; The Corner (Talos Principle), topological model.  A partition at x=8 divides the room;
;;; its only walkable opening is gate1, which is open while receiver1 is lit red.  Agent1
;;; starts on the east side with three connectors and must end on the west side, at
;;; location4, with receiver2 lit red and receiver3 lit blue.  Beams that cross cut each
;;; other, and bodies standing in a beam's path block it.
;;;
;;; Design notes:
;;;   - Only the given geometry is authored: segment endpoints, standing locations, and
;;;     apparatus coordinates.  Walkability, sightlines and beam crossings are derived from
;;;     them at initialization, so the search state holds only where things are, what the
;;;     agent holds, and which connectors are paired.
;;;   - All mechanics come from the shared Talos technologies; this file adds no actions.
;;;   - The connectors are interchangeable, so arrangements differing only in connector
;;;     names are treated as one state.
;;;   - Meet in the middle: the file also declares which facts make up a state (where things
;;;     are, what is held, which connectors are paired).  The goal states can then be
;;;     generated directly and worked backward from, so the search from the start only has
;;;     to reach one of them.  The rules and the goal are unchanged.
;;;   - The declared facts include a held connector's pending selections, and states that
;;;     break the rules on those facts (pairing limit, held-or-placed) are screened out before
;;;     they are worked backward from.  The screen enforces rules only; it guesses nothing
;;;     about the answer.


(in-package :ww)


(ww-set *problem-name* corner-topo-advisor-cap3)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 5)
(ww-set *symmetry-pruning* t)
(ww-set *max-connector-pairings* 3)


(define-types
  agent       (agent1)
  gate        (gate1)
  wall        (wall1 wall2)
  window      (window1)
  location    (location1 location2 location3 location4)
  connector   (connector1 connector2 connector3)
  transmitter (transmitter1 transmitter2)
  receiver    (receiver1 receiver2 receiver3)
  hue         (blue red))


(include-tech gate)
(include-tech beam-relay)
(include-tech beam-crossing)
(include-tech walkability)
(include-tech visibility)


(define-init
  ;; Starting positions
  (has-location agent1 location1)
  (has-location connector1 location1)
  (has-location connector2 location2)
  (has-location connector3 location3)
  (controls ((receiver1)) gate1 normal)
  (has-chroma transmitter1 red)
  (has-chroma transmitter2 blue)
  (has-chroma receiver1 red)
  (has-chroma receiver2 red)
  (has-chroma receiver3 blue)
  ;; Room outline and the x=8 partition, bottom to top
  (boundary-wall ((0 0) (12 0) (12 11) (0 11) (0 0)))
  (wall-segment> wall2 8 0 8 3)
  (gate-segment> gate1 8 3 8 11/2)
  (wall-segment> wall1 8 11/2 8 8)
  (window-segment> window1 8 8 8 11)
  (location-coords> location1 9 1)
  (location-coords> location2 9 8)
  (location-coords> location3 10 9)
  (location-coords> location4 6 8)
  (apparatus-coords> transmitter1 11 1/10)
  (apparatus-coords> transmitter2 10 1/10)
  (apparatus-coords> receiver1 81/10 1)
  (apparatus-coords> receiver2 7 109/10)
  (apparatus-coords> receiver3 1 109/10))


;; Set the starting beam colors, receiver states and gate state from the initial layout.
(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!)))


(define-goal
  (and (has-location agent1 location4)
       (active receiver2)
       (active receiver3)))


;;;; ENUMERATOR DECLARATIONS ;;;;
;;;; Used only by find-goal-states, find-predecessors and solve-meeting-point.


;; Where the agent and each connector are.  A connector with no location is being held,
;; and at most one can be.
(define-base-relation has-location
  :allow-unassigned (:types connector)
  :max-unassigned (:types connector 1))


;; What the agent holds: nothing, or one connector.
(define-base-relation holding
  :allow-unassigned t)


;; Each connector's pairings, up to the pairing limit.  A held connector may keep its
;; pairings (retaining pickup).
(define-base-relation (paired ?connector ?terminus)
  :max-per (?connector 3))


;; Termini selected for the held connector, not yet paired (they pair at put-down).
;; Only the held connector can have selections, so other connectors get none.  Kept
;; pairings and selections share the connector's limit of 3.
(define-base-relation (selected ?connector ?terminus)
  :max-per (?connector 3)
  :shares-max-per (paired)
  :requires (holding agent1 ?connector))


;; Checked once where things are and what is held are chosen, before any pairings are
;; enumerated: each connector is either at a location or held, never both.  It is the
;; first part of state-feasible?, so it rejects nothing that check would accept.
(define-query fluents-feasible? ()
  (forall (?c connector)
    (or (and (bind (has-location ?c $loc))
             (not (holding agent1 ?c)))
        (and (not (bind (has-location ?c $loc)))
             (holding agent1 ?c)))))


;; Consistency only, not a guess at the answer.  Applied to goal states and to every
;; predecessor.  Each connector is either at a location or held, never both; it has at
;; most *max-connector-pairings* pairings and selections, counted on either side; only the
;; held connector has selections; a terminus is not both paired and selected; and no
;; connector pairs with itself or is paired in both directions.
(define-query state-feasible? ()
  (forall (?c connector)
    (and (or (and (bind (has-location ?c $loc))
                  (not (holding agent1 ?c)))
             (and (not (bind (has-location ?c $loc)))
                  (holding agent1 ?c)))
         (<= (connector-link-count ?c) *max-connector-pairings*)
         (forall (?t terminus)
           (and (not (and (paired ?c ?t) (selected ?c ?t)))
                (or (not (selected ?c ?t))
                    (holding agent1 ?c))
                (or (not (connector ?t))
                    (and (or (different ?c ?t)
                             (not (or (paired ?c ?t) (selected ?c ?t))))
                         (not (and (paired ?c ?t) (paired ?t ?c)))
                         (not (and (selected ?c ?t) (paired ?t ?c))))))))))


;; Goal states only, and necessary rather than a guess: receiver2 (red) and receiver3
;; (blue) light only from a placed connector paired with them in structural sight, and red
;; and blue enter the connectors only from transmitter1 and transmitter2 the same way.
;; Screens base states before the costly lighting step.
(define-goal-filter goal-needs-sighted-links
  (and (exists (?c connector)
         (and (paired ?c receiver2)
              (bind (has-location ?c $loc))
              (potentially-visible $loc (top ?c) receiver2 (top receiver2))))
       (exists (?c connector)
         (and (paired ?c receiver3)
              (bind (has-location ?c $loc))
              (potentially-visible $loc (top ?c) receiver3 (top receiver3))))
       (exists (?c connector)
         (and (paired ?c transmitter1)
              (bind (has-location ?c $loc))
              (potentially-visible $loc (top ?c) transmitter1 (top transmitter1))))
       (exists (?c connector)
         (and (paired ?c transmitter2)
              (bind (has-location ?c $loc))
              (potentially-visible $loc (top ?c) transmitter2 (top transmitter2))))))

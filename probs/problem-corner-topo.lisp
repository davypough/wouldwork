;;; Filename: problem-corner-topo.lisp

;;; Companion to problem-corner.lisp: same puzzle and geometry, but nothing here is
;;; hand-authored topology anymore.  tech/-beam-los-coordinates.lisp's DERIVE-LOS-FROM-
;;; SEGMENTS derives LOS-VIA; tech/-beam-crossing-
;;; coordinates.lisp's ESTABLISH-BEAM-COORDINATES and DERIVE-BEAM-CROSSINGS-BEFORE-GATE mint the
;;; crossing pool and derive CROSSINGS-ALONG-BEAM>/BEAM-CROSSINGS-BEFORE-GATE>; and
;;; tech/-walkability-coordinates.lisp's DERIVE-WALKING-FROM-SEGMENTS derives walking
;;; traverse-via facts -- all from the raw segment geometry below.  This is the fuller
;;; topology-diagram geometry, not the reduced internal-segment model in
;;; problem-corner.lisp; the two files describe the same puzzle but are not
;;; coordinate-identical.  The two beam derivations read the authored
;;; WALL-SEGMENT>/GATE-SEGMENT> facts and BOUNDARY-WALL (this problem has no EDGE-SEGMENT>
;;; facts); WINDOW-SEGMENT> is consulted only by the walking derivation, whose
;;; side-of-partition-line classification reflects that walking connectivity is a
;;; zone-adjacency question, not a sightline one (see that file's header for why).
;;;
;;; location4 is deliberately moved to (6,8) here (problem-corner.lisp keeps it at (7,8));
;;; this is an intentional test-fixture divergence, not a correction.  The wall1/gate1 split
;;; point is placed at y=11/2 so all three gate1-conditioned beams (location1->location4,
;;; transmitter2->location4, transmitter1->location4) cross x=8 comfortably below the split,
;;; while the only other affected beam (location1->receiver2) crosses x=8 above it.  That
;;; sightline, and location2/location4 through wall1's top corner (8,8), retain structural
;;; crossing records but remain blocked at their ordinary low elevations.


(in-package :ww)


(ww-set *problem-name* corner-topo)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 19)  ;old 15-step plan = 19 steps with select-termini + put-connector

(ww-set *symmetry-pruning* t)

(ww-set *max-connector-pairings* 4)  ;max pairings per connector at any time, either direction (beam-relay)


;;;; TYPES ;;;;
;;;; Leaf object types the problem instantiates live here, ahead of the technology
;;;; includes -- and must stay ahead of them.  INSTALL-QUERY translates a DEFINE-QUERY body
;;;; the moment the form is evaluated, and TRANSLATE-DOALL turns a DOALL over a bare type
;;;; name into a literal domain at that point, so a type declared below the includes would
;;;; reach every tech query as an empty domain.  That failure is silent, not an error.
;;;; Every composite type that a consuming tech file needs (mobile-object, cargo,
;;;; support-occupant, support, target, fixed-position-object, los-endpoint) is declared
;;;; identically inside that tech file instead, so no tech file depends on a type
;;;; declaration living in the problem.  terminus is owned by beam-relay.
;;;; crossing is deliberately absent: the pool is minted at init time by
;;;; -beam-crossing-coordinates.lisp's ESTABLISH-BEAM-COORDINATES, one crossing per computed
;;;; intersection, and published as CURRENT-BEAM-CROSSINGS for beam-crossing.lisp's
;;;; GET-CURRENT-BEAM-CROSSINGS to iterate.


(define-types
  agent       (agent1)
  gate        (gate1)  ;only the dynamic gate (controlled by receiver1); corner's walls/window are static occlusion baked into the los facts
  wall        (wall1 wall2)
  window      (window1)
  location    (location1 location2 location3 location4)  ;corner area1..area4
  connector   (connector1 connector2 connector3)
  transmitter (transmitter1 transmitter2)
  receiver    (receiver1 receiver2 receiver3)
  hue         (blue red)
)


;;;; TECHNOLOGY INCLUDES ;;;;
;;;; corner-topo needs beam relaying through movable connectors (beam-relay) with
;;;; crossing-based beam cutting (beam-crossing), plus the walking/sightline background
;;;; (walkability, visibility).  Beam-relay's nested -reachability substrate supplies
;;;; identity reach, so pickup/connect require the agent's own location exactly as in
;;;; corner-topo.  Including the full reachability technology would override that default
;;;; and add reach-via edges, but this problem has none.  beam-crossing nests in
;;;; -beam-crossing-coordinates automatically, which (together with -location-coordinates, nested
;;;; there too) is what turns the position facts below into CROSSINGS-ALONG-BEAM>.


(include-tech gate)                  ;controls; energized; update-gate-status!
(include-tech beam-relay)            ;paired; selected; color; pickup/select-termini/put connector actions
(include-tech beam-crossing)         ;crossing-active; beam-crossing>; crossings-along-beam>; apparatus-coords>
(include-tech walkability)          ;walking mode; mobility-results; traversable; one-step-walkable; move
(include-tech visibility)            ;los-via; visible; visible-clear
(include-tech topo-lower-bound)      ;admissible finite-resource bound (search pruning)


;;;; INITIALIZATION ;;;;


(define-init
  ;; Dynamic state
  (has-location agent1 location1)
  (has-location connector1 location1)
  (has-location connector2 location2)
  (has-location connector3 location3)

  ;; Gate control (DNF): receiver1 active -> gate1 open
  (controls ((receiver1)) gate1 normal)

  ;; Fixed hues
  (has-chroma transmitter1 red)
  (has-chroma transmitter2 blue)
  (has-chroma receiver1 red)
  (has-chroma receiver2 red)
  (has-chroma receiver3 blue)

  ;; Boundary wall.  The repeated final point explicitly closes the polygon.  tech/-beam-los-coordinates.lisp's
  ;; DERIVE-LOS-FROM-SEGMENTS retains each polygon crossing, so ordinary low sight outside
  ;; this silhouette is blocked exactly like an internal wall segment.  The walking
  ;; derivation also uses the polygon as its outer solid boundary.  Because this rectangle
  ;; is convex and every authored location lies inside it, it introduces no internal
  ;; walking partition; likewise, a straight sightline between two interior points cannot
  ;; cross it.
  (boundary-wall
    ((0 0) (12 0) (12 11) (0 11) (0 0)))


  ;; Raw wall/gate/window segment geometry -- drives DERIVE-LOS-FROM-SEGMENTS (tech/-beam-los-
  ;; coordinates.lisp) and DERIVE-BEAM-CROSSINGS-BEFORE-GATE (tech/-beam-crossing-
  ;; coordinates.lisp) below.  wall1/gate1 split at
  ;; y=11/2 keeps all three gate1-conditioned beams below the split and the location1-
  ;; >receiver2 wall crossing above it, so neither segment is fragmented.
  (wall-segment> wall1 8 11/2 8 8)
  (wall-segment> wall2 8 0 8 3)
  (gate-segment> gate1 8 3 8 11/2)
  (window-segment> window1 8 8 8 11)

  ;; Exact 2D endpoint coordinates, split between LOCATION-COORDS> (locations; shared
  ;; with walkability.lisp's own coordinate needs -- see tech/-location-coordinates.lisp)
  ;; and APPARATUS-COORDS> (transmitter/receiver only).  tech/-beam-los-coordinates.lisp's
  ;; DERIVE-LOS-FROM-SEGMENTS uses these together with the wall/gate segments above to
  ;; derive LOS-VIA, and ESTABLISH-BEAM-COORDINATES uses them again
  ;; for CROSSINGS-ALONG-BEAM>, before search begins; the coordinates themselves drive
  ;; nothing thereafter.  location4 is at (6,8) here -- a deliberate divergence from
  ;; problem-corner.lisp's (7,8); see file header.
  (location-coords> location1 9 1)
  (location-coords> location2 9 8)
  (location-coords> location3 10 9)
  (location-coords> location4 6 8)
  (apparatus-coords> transmitter1 11 1/10)
  (apparatus-coords> transmitter2 10 1/10)
  (apparatus-coords> receiver1 81/10 1)
  (apparatus-coords> receiver2 7 109/10)
  (apparatus-coords> receiver3 1 109/10)
)


(define-init-action initialize-derived-state
  ;; Derive the full derived layer (open, crossing-active, color, active) after
  ;; -beam-crossing-coordinates.lisp's establish-beam-coordinates has minted the crossing
  ;; pool and established the static crossing topology from the coordinates above.
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!)))


;;;; GOAL ;;;;


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
  :max-per (?connector 4))


;; Termini selected for the held connector, not yet paired (they pair at put-down).
;; Only the held connector can have selections, so other connectors get none.  Kept
;; pairings and selections share the connector's limit of 4.
(define-base-relation (selected ?connector ?terminus)
  :max-per (?connector 4)
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

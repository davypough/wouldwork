;;; Filename: problem-corner-topo.lisp

;;; Companion to problem-corner.lisp: same puzzle and geometry, but nothing here is
;;; hand-authored topology anymore -- tech/-beam-coordinates.lisp's DERIVE-LOS-FROM-
;;; SEGMENTS and DERIVE-CROSSINGS-BEFORE-GATE derive LOS-TO-TRANSCEIVER/LOS-TO-LOCATION/
;;; CROSSINGS-BEFORE-GATE>, and tech/-accessibility-coordinates.lisp's DERIVE-WALK-VIA-
;;; FROM-SEGMENTS derives WALK-VIA, all directly from the raw WALL-SEGMENTS/GATE-SEGMENTS/
;;; WINDOW-SEGMENTS geometry below (same shape as problem-corner.lisp's).  WALK-VIA's
;;; derivation uses side-of-partition-line classification, not beam intersection --
;;; walking connectivity is a zone-adjacency question, not a sightline one (see that
;;; file's header for why).
;;;
;;; location4 is deliberately moved to (6,8) here (problem-corner.lisp and
;;; problem-corner-topo.lisp keep it at (7,8)); this is an intentional test-fixture
;;; divergence, not a correction.  The wall1/gate1 split point is placed at y=11/2 so all
;;; three gate1-conditioned beams (location1->location4, transmitter2->location4,
;;; transmitter1->location4) cross x=8 comfortably below the split, while the only other
;;; affected beam (location1->receiver2) crosses x=8 above it -- hence derivation
;;; correctly excludes location1->receiver2 as wall-blocked.  location2/location4 is
;;; wall-blocked too: their sightline runs exactly along wall1's top corner (8,8) --
;;; solid wall, not the window above it -- which -beam-coordinates.lisp's wall-corner
;;; convention now excludes (see that file's header).  Verified against problem-
;;; corner-topo.lisp's hand-authored fact set: identical 17 LOS-TO-TRANSCEIVER + 5 LOS-TO-
;;; LOCATION pairs, identical 26-crossing count (location2/location4 crossed no other
;;; beam either way), same solutions reachable.


(in-package :ww)


(ww-set *problem-name* corner-topo)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 15)

(ww-set *symmetry-pruning* t)

(ww-set *progress-reporting-interval* 1000000)


(defparameter *max-pairings* 3)  ;max termini a connector may pair in one connect (beam-relay's connect-connector)


;;;; TYPES ;;;;
;;;; Leaf object types the problem instantiates live here, ahead of the technology
;;;; includes.  Every composite type that a consuming tech file needs (mobile-object,
;;;; cargo, support-occupant, support, target, fixed-position-object) is declared identically
;;;; inside that tech file instead, so no tech file depends on a type declaration living
;;;; in the problem.  beam-endpoint is the remaining exception: it is a corner-topo-specific
;;;; composite that no tech file declares, so it must live here.  terminus is owned by
;;;; beam-relay.
;;;; crossing must still be declared in full here (its instance count can't itself be
;;;; derived -- see -beam-coordinates.lisp's header), even though its content
;;;; (crossings-along-beam>) is now fully computed from the coordinates below.


(define-types
  agent       (agent1)
  gate        (gate1)  ;only the dynamic gate (controlled by receiver1); corner's walls/window are static occlusion baked into the los facts
  wall        (wall1 wall2 wall3)
  window      (window1)
  location    (location1 location2 location3 location4)  ;corner area1..area4
  connector   (connector1 connector2 connector3)
  transmitter (transmitter1 transmitter2)
  receiver    (receiver1 receiver2 receiver3)
  hue         (blue red)
  mode        (normal inverted toggle)  ;controller mode; corner uses only normal
  crossing    (crossing1 crossing2 crossing3 crossing4 crossing5 crossing6 crossing7 crossing8 crossing9 crossing10
               crossing11 crossing12 crossing13 crossing14 crossing15 crossing16 crossing17 crossing18 crossing19 crossing20
               crossing21 crossing22 crossing23 crossing24 crossing25 crossing26)  ;pool assigned from computed geometry during initialization
  beam-endpoint (either transmitter receiver location)  ;a fixture, or a connector's location; not supplied by any tech file
)


;;;; TECHNOLOGY INCLUDES ;;;;
;;;; corner-topo+ needs beam relaying through movable connectors (beam-relay) with
;;;; crossing-based beam cutting (beam-crossing), plus the walking/sightline background
;;;; (accessibility, visibility).  Beam-relay's nested -reachability substrate supplies
;;;; identity reach, so pickup/connect require the agent's own location exactly as in
;;;; corner-topo.  Including the full reachability technology would override that default
;;;; and add reach-via edges, but this problem has none.  beam-crossing nests in
;;;; -beam-coordinates automatically, which (together with -location-coordinates, nested
;;;; there too) is what turns the position facts below into CROSSINGS-ALONG-BEAM>.


(include-tech gate)                  ;controls; energized; update-gate-status!
(include-tech beam-relay)            ;paired; color; pickup/put/connect connector actions
(include-tech beam-crossing)         ;crossing-active; beam-crossing>; crossings-along-beam>; transceiver-position>
(include-tech accessibility)         ;walk-via; accessible; one-step-accessible; move
(include-tech visibility)            ;los-to-transceiver; los-to-location; visible; visible-clear


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

  ;; Boundary wall.  The final point connects back to the first.  tech/-beam-coordinates.lisp's
  ;; DERIVE-LOS-FROM-SEGMENTS folds each polygon edge into its wall list, so a sightline that
  ;; would have to cut outside this silhouette is blocked exactly like a wall-segment.  Not
  ;; currently consulted by accessibility's own coordinate derivation (walk-via).  This
  ;; rectangle fully encloses the map and is convex, so it's functionally inert here (a
  ;; straight line between two interior points of a convex boundary wall can never cross it).
  (boundary-wall
    ((0 0) (12 0) (12 11) (0 11)))


  ;; Raw wall/gate/window segment geometry -- drives DERIVE-LOS-FROM-SEGMENTS/DERIVE-
  ;; CROSSINGS-BEFORE-GATE below (see tech/-beam-coordinates.lisp).  wall1/gate1 split at
  ;; y=11/2 keeps all three gate1-conditioned beams below the split and the location1-
  ;; >receiver2 crossing (correctly wall-blocked) above it, so neither segment is
  ;; fragmented.
  (wall-segments ((wall1 8 11/2 8 8) (wall2 8 0 8 3) (wall3 11 10 16 10)))
  (gate-segments ((gate1 8 3 8 11/2)))
  (window-segments ((window1 8 8 8 11)))

  ;; Exact 2D endpoint coordinates, split between LOCATION-POSITION> (locations; shared
  ;; with accessibility-tech's own coordinate needs -- see tech/-location-coordinates.lisp)
  ;; and TRANSCEIVER-POSITION> (transmitter/receiver only).  tech/-beam-coordinates.lisp's
  ;; DERIVE-LOS-FROM-SEGMENTS uses these together with the wall/gate segments above to
  ;; derive LOS-TO-TRANSCEIVER/LOS-TO-LOCATION, and ESTABLISH-BEAM-COORDINATES uses them again
  ;; for CROSSINGS-ALONG-BEAM>, before search begins; the coordinates themselves drive
  ;; nothing thereafter.  location4 is at (6,8) here -- a deliberate divergence from
  ;; problem-corner.lisp/problem-corner-topo.lisp's (7,8); see file header.
  (location-position> location1 9 1)
  (location-position> location2 9 8)
  (location-position> location3 10 9)
  (location-position> location4 6 8)
  (transceiver-position> transmitter1 11 1/10)
  (transceiver-position> transmitter2 10 1/10)
  (transceiver-position> receiver1 81/10 1)
  (transceiver-position> receiver2 7 109/10)
  (transceiver-position> receiver3 1 109/10)
)


(define-init-action initialize-derived-state
  ;; Derive the full derived layer (open, crossing-active, color, active) after
  ;; -beam-coordinates.lisp's establish-beam-coordinates has established the static
  ;; crossing topology from the coordinates above.
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

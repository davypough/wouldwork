;;; Filename: problem-corner-topo-advisor.lisp

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


(in-package :ww)


(ww-set *problem-name* corner-topo-advisor)
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

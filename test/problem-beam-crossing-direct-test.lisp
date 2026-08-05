;;; Filename: problem-beam-crossing-direct-test.lisp

;;; Beam-direct + beam-crossing regression for the one beam family the coordinate
;;; derivation used to miss entirely: a direct transmitter -> receiver beam.  The LOS-
;;; derived beam families all have a location endpoint, so fixture-to-fixture beams must
;;; instead enter the crossing geometry through their authored COUPLED facts.
;;;
;;; Geometry: two direct beams on the diagonals of a 10x10 square.  transmitter1 (0,0) ->
;;; receiver1 (10,10) and transmitter2 (10,0) -> receiver2 (0,10) cross at exactly (5,5),
;;; a proper interior intersection of both segments.  A third direct beam, transmitter3
;;; (10,10) -> receiver3 (20,10), only touches the first beam at its endpoint.  Proper-
;;; intersection semantics must therefore mint exactly one crossing, not two.
;;;
;;; All three corridors are empty.  The one crossing must activate and cut both diagonal
;;; beams, leaving receiver1 and receiver2 inactive.  The endpoint-touching control beam
;;; must remain uncut and activate receiver3.  The goal checks the crossing pool, active
;;; set, cuts, and all three receiver outcomes directly.
;;;
;;; No WALL-SEGMENT> facts are authored, so DERIVE-LOS-FROM-SEGMENTS stays inert and contributes
;;; no location beams; every beam here comes from COUPLED.
;;;
;;; Expected minimum solution: 0 actions (the derived start state satisfies the goal).

(in-package :ww)


(ww-set *problem-name* beam-crossing-direct-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 0)


;;;; TYPES ;;;;
;;;; Ahead of the technology includes, as always -- a DOALL over a bare type name is
;;;; resolved into a literal domain when the tech file's DEFINE-QUERY is translated, which
;;;; happens the moment the include is evaluated.  crossing is absent by design: the pool
;;;; is minted during init by ESTABLISH-BEAM-COORDINATES.


(define-types
  agent       (agent1)
  location    (loc1)
  transmitter (transmitter1 transmitter2 transmitter3)
  receiver    (receiver1 receiver2 receiver3)
  gate        (unused-gate)
  hue         (blue green red)
)


;;;; TECHNOLOGY INCLUDES ;;;;
;;;; beam-direct supplies COUPLED/BEAM-VIA and the direct arrival and cut-liveness hooks;
;;;; beam-crossing supplies the crossing machinery and, through its nested
;;;; -beam-crossing-coordinates, the derivation under test.
;;;;
;;;; visibility is not optional here even though this problem has no sightlines to speak
;;;; of.  beam-crossing nests -beam-crossing-coordinates, which nests -beam-los-coordinates,
;;;; whose DERIVE-LOS-FROM-SEGMENTS references LOS-TO-APPARATUS/LOS-TO-LOCATION -- and
;;;; visibility is what declares those relations.  Omitting it does not merely lose a
;;;; capability; the tech file fails to translate, with an error naming LOS-TO-APPARATUS
;;;; and no hint that the missing piece is an include.  Every beam-crossing problem in the
;;;; repository includes visibility for this reason.  It stays inert here regardless, since
;;;; DERIVE-LOS-FROM-SEGMENTS is guarded on WALL-SEGMENT>, which this problem never asserts.


(include-tech beam-direct)
(include-tech beam-crossing)
(include-tech visibility)


;;;; INITIALIZATION ;;;;


(define-init
  (has-location agent1 loc1)

  ;; Hues match within each coupling.  Distinct hues keep the three receiver outcomes
  ;; independent of any unintended source.
  (has-chroma transmitter1 red)
  (has-chroma receiver1 red)
  (has-chroma transmitter2 blue)
  (has-chroma receiver2 blue)
  (has-chroma transmitter3 green)
  (has-chroma receiver3 green)

  ;; Empty BEAM-VIA corridors make every direct beam live for cutting and isolate the
  ;; crossing geometry.
  (coupled transmitter1 receiver1)
  (coupled transmitter2 receiver2)
  (coupled transmitter3 receiver3)
  (beam-via transmitter1 () receiver1)
  (beam-via transmitter2 () receiver2)
  (beam-via transmitter3 () receiver3)

  ;; The first two beams properly intersect at (5,5).
  (apparatus-coords> transmitter1 0 0)
  (apparatus-coords> receiver1 10 10)
  (apparatus-coords> transmitter2 10 0)
  (apparatus-coords> receiver2 0 10)

  ;; The control beam touches the first beam only at (10,10), an endpoint of each segment.
  (apparatus-coords> transmitter3 10 10)
  (apparatus-coords> receiver3 20 10)

  ;; The required location sits off every beam.
  ;; BEAM-COORDINATES-ENDPOINT-POSITIONS requires a position for every location regardless
  ;; of whether any beam reaches it.
  (location-coords> loc1 2 9)
)


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!))
)


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query beam-crossing-direct-scenario-valid ()
  (do (assign $crossings (get-current-beam-crossings))
      (assign $active-crossings (current-crossing-set))
      (and (= (length $crossings) 1)
           (same-crossing-set $crossings $active-crossings)
           (beam-cut transmitter1 receiver1)
           (beam-cut transmitter2 receiver2)
           (not (beam-cut transmitter3 receiver3))
           (not (active receiver1))
           (not (active receiver2))
           (active receiver3))))


(define-goal
  (beam-crossing-direct-scenario-valid))

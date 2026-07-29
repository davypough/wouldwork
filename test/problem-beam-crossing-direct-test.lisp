;;; Filename: problem-beam-crossing-direct-test.lisp

;;; Minimal beam-direct + beam-crossing exercise, covering the one beam family the
;;; coordinate derivation used to miss entirely: a direct transmitter -> receiver beam.
;;; The three families -beam-crossing-coordinates.lisp enumerates from the derived LOS
;;; relations all have a location endpoint, so a fixture-to-fixture beam had no LOS fact
;;; to be found through and never reached the crossing geometry.  Direct beams are now
;;; picked up from the authored COUPLED facts instead.
;;;
;;; Geometry: two direct beams on the diagonals of a 10x10 square.  transmitter1 (0,0) ->
;;; receiver1 (10,10) and transmitter2 (10,0) -> receiver2 (0,10) cross at exactly (5,5),
;;; a proper interior intersection of both segments, so the derivation should mint exactly
;;; one crossing.  That is the assertion this problem exists to make -- see the REPL check
;;; below.  No WALL-SEGMENTS are authored, so DERIVE-LOS-FROM-SEGMENTS stays inert and
;;; contributes no location beams; every beam here comes from COUPLED.
;;;
;;; Steady state: both corridors are empty, so both beams are live for cutting, so
;;; crossing1 activates and cuts both.  Neither receiver ends up active.  That is stable
;;; rather than oscillating because DIRECT-BEAM-LIVE-FOR-CUTTING depends only on the
;;; BEAM-VIA corridor clearing, never on whether the beam is itself cut -- so the fixpoint
;;; in UPDATE-CROSSING-STATUS! settles on the first pass.  The goal is deliberately
;;; independent of the receivers.
;;;
;;; Expected minimum solution (1 step): walk agent1 loc1 -> loc2.
;;;
;;; REPL check, after loading:
;;;   (length (get-current-crossings *start-state*))  =>  1

(in-package :ww)


(ww-set *problem-name* beam-crossing-direct-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 3)


;;;; TYPES ;;;;
;;;; Ahead of the technology includes, as always -- a DOALL over a bare type name is
;;;; resolved into a literal domain when the tech file's DEFINE-QUERY is translated, which
;;;; happens the moment the include is evaluated.  crossing is absent by design: the pool
;;;; is minted during init by ESTABLISH-BEAM-COORDINATES.


(define-types
  agent       (agent1)
  location    (loc1 loc2)
  transmitter (transmitter1 transmitter2)
  receiver    (receiver1 receiver2)
  hue         (blue red)
)


;;;; TECHNOLOGY INCLUDES ;;;;
;;;; beam-direct supplies COUPLED/BEAM-VIA and the direct arrival and cut-liveness hooks;
;;;; beam-crossing supplies the crossing machinery and, through its nested
;;;; -beam-crossing-coordinates, the derivation under test.  walkability is here only to
;;;; give the agent a WALK action so the problem has something to search.
;;;;
;;;; visibility is not optional here even though this problem has no sightlines to speak
;;;; of.  beam-crossing nests -beam-crossing-coordinates, which nests -beam-los-coordinates,
;;;; whose DERIVE-LOS-FROM-SEGMENTS references LOS-TO-APPARATUS/LOS-TO-LOCATION -- and
;;;; visibility is what declares those relations.  Omitting it does not merely lose a
;;;; capability; the tech file fails to translate, with an error naming LOS-TO-APPARATUS
;;;; and no hint that the missing piece is an include.  Every beam-crossing problem in the
;;;; repository includes visibility for this reason.  It stays inert here regardless, since
;;;; DERIVE-LOS-FROM-SEGMENTS is guarded on WALL-SEGMENTS, which this problem never asserts.


(include-tech beam-direct)
(include-tech beam-crossing)
(include-tech walkability)
(include-tech visibility)


;;;; INITIALIZATION ;;;;


(define-init
  (has-location agent1 loc1)

  ;; Hues must match for DIRECT-BEAM-REACHES-RECEIVER; the two beams use different hues so
  ;; neither transmitter can satisfy the other's receiver.
  (has-chroma transmitter1 red)
  (has-chroma receiver1 red)
  (has-chroma transmitter2 blue)
  (has-chroma receiver2 blue)

  ;; The two direct beams.  Empty BEAM-VIA corridors: no gates, no intervening locations,
  ;; so both are unconditionally live for cutting and the test isolates the geometry.
  (coupled transmitter1 receiver1)
  (coupled transmitter2 receiver2)
  (beam-via transmitter1 () receiver1)
  (beam-via transmitter2 () receiver2)

  ;; Diagonal endpoints; the two segments properly intersect at (5,5).
  (apparatus-coords> transmitter1 0 0)
  (apparatus-coords> receiver1 10 10)
  (apparatus-coords> transmitter2 10 0)
  (apparatus-coords> receiver2 0 10)

  ;; Locations sit off both diagonals so they cannot be mistaken for beam endpoints.
  ;; BEAM-COORDINATES-ENDPOINT-POSITIONS requires a position for every location regardless
  ;; of whether any beam reaches it.
  (location-coords> loc1 2 9)
  (location-coords> loc2 8 9)

  (walk-via loc1 () loc2)
)


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!))
)


;;;; GOAL ;;;;


(define-goal
  (has-location agent1 loc2)
)

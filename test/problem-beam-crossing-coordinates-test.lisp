;;; Filename: problem-beam-crossing-coordinates-test.lisp

;;; Coordinate-derived beam-crossing topology regression.  One horizontal
;;; location-to-location beam is crossed properly by three vertical beams at
;;; x=2, x=5, and x=8.  The derived crossing pool and every per-beam list must
;;; preserve that geometric order, including the exact reverse list for the
;;; horizontal beam's opposite direction.
;;;
;;; Two closed gates cross the horizontal beam between crossings: GATE1 at x=4
;;; and GATE2 at x=7.  Their independently derived BEAM-CROSSINGS-BEFORE-GATE> facts
;;; must split the sequence at different positions.  SPLIT-MARKER is also in the
;;; authored LOS occluder list, but is a location rather than a gate and must not
;;; create any gate-split record.
;;;
;;; A final vertical control beam begins at a distinct location with the same
;;; coordinates as the horizontal beam's right endpoint.  The two segments
;;; therefore touch only at their coordinate endpoints and must not mint a
;;; fourth crossing or receive a CROSSINGS-ALONG-BEAM> fact.
;;;
;;; No direct- or relay-beam technology is included, so normal propagation must
;;; leave the complete derived topology inactive and every beam uncut.  The goal
;;; is a zero-action characterization query: initial and final states are
;;; identical, and the expected minimum path length is 0.

(in-package :ww)


(ww-set *problem-name* beam-crossing-coordinates-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 0)


;;;; TYPES ;;;;


(define-types
  agent (idle-agent)
  location (main-left
            main-right
            vertical1-bottom
            vertical1-top
            vertical2-bottom
            vertical2-top
            vertical3-bottom
            vertical3-top
            endpoint-bottom
            endpoint-top
            split-marker)
  gate (gate1 gate2))


;;;; TECHNOLOGY INCLUDES ;;;;


(include-tech beam-crossing)
(include-tech visibility)


;;;; INITIALIZATION ;;;;


(define-init
  (has-location idle-agent split-marker)

  ;; Hand-authored LOS isolates crossing-coordinate derivation from the raw
  ;; wall-to-LOS derivation covered by problem-beam-los-coordinates-test.
  (los-to-location main-left (gate1 split-marker gate2) main-right)
  (los-to-location vertical1-bottom () vertical1-top)
  (los-to-location vertical2-bottom () vertical2-top)
  (los-to-location vertical3-bottom () vertical3-top)
  (los-to-location endpoint-bottom () endpoint-top)

  ;; Gate1 lies between CROSSING1/CROSSING2; gate2 lies between
  ;; CROSSING2/CROSSING3.
  (gate-segment> gate1 4 -1 4 1)
  (gate-segment> gate2 7 -1 7 1)

  ;; Main beam and its three proper crossings.
  (location-coords> main-left 0 0)
  (location-coords> main-right 10 0)
  (location-coords> vertical1-bottom 2 -2)
  (location-coords> vertical1-top 2 2)
  (location-coords> vertical2-bottom 5 -2)
  (location-coords> vertical2-top 5 2)
  (location-coords> vertical3-bottom 8 -2)
  (location-coords> vertical3-top 8 2)

  ;; Coordinate-only endpoint contact with MAIN-RIGHT.
  (location-coords> endpoint-bottom 10 0)
  (location-coords> endpoint-top 10 4)

  ;; Non-gate LOS occluder, deliberately away from every beam.
  (location-coords> split-marker 5 4))


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!)))


;;;; CHARACTERIZATION HELPERS ;;;;


(define-query crossings-along-present
    (?from los-endpoint ?to los-endpoint)
  (bind (crossings-along-beam> ?from $crossings ?to)))


(define-query gate-split-present
    (?from los-endpoint ?to los-endpoint)
  (exists (?gate gate)
    (bind (beam-crossings-before-gate> ?from $crossings ?gate ?to))))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query beam-crossing-coordinates-scenarios-valid ()
  (do (assign $pool (get-current-beam-crossings))
      (and
        ;; The pool is a deterministic output of the three proper intersections.
        (equal $pool '(crossing1 crossing2 crossing3))

        ;; Crossing order follows each beam from its named first endpoint.
        (bind (crossings-along-beam>
                main-left $main-forward main-right))
        (equal $main-forward '(crossing1 crossing2 crossing3))
        (bind (crossings-along-beam>
                main-right $main-reverse main-left))
        (equal $main-reverse '(crossing3 crossing2 crossing1))

        (bind (crossings-along-beam>
                vertical1-bottom $vertical1-forward vertical1-top))
        (equal $vertical1-forward '(crossing1))
        (bind (crossings-along-beam>
                vertical1-top $vertical1-reverse vertical1-bottom))
        (equal $vertical1-reverse '(crossing1))

        (bind (crossings-along-beam>
                vertical2-bottom $vertical2-forward vertical2-top))
        (equal $vertical2-forward '(crossing2))
        (bind (crossings-along-beam>
                vertical2-top $vertical2-reverse vertical2-bottom))
        (equal $vertical2-reverse '(crossing2))

        (bind (crossings-along-beam>
                vertical3-bottom $vertical3-forward vertical3-top))
        (equal $vertical3-forward '(crossing3))
        (bind (crossings-along-beam>
                vertical3-top $vertical3-reverse vertical3-bottom))
        (equal $vertical3-reverse '(crossing3))

        ;; Endpoint-only contact is not a proper beam crossing.
        (not (crossings-along-present endpoint-bottom endpoint-top))
        (not (crossings-along-present endpoint-top endpoint-bottom))

        ;; Each gate independently splits the main crossing sequence.
        (bind (beam-crossings-before-gate>
                main-left $gate1-forward gate1 main-right))
        (equal $gate1-forward '(crossing1))
        (bind (beam-crossings-before-gate>
                main-right $gate1-reverse gate1 main-left))
        (equal $gate1-reverse '(crossing3 crossing2))

        (bind (beam-crossings-before-gate>
                main-left $gate2-forward gate2 main-right))
        (equal $gate2-forward '(crossing1 crossing2))
        (bind (beam-crossings-before-gate>
                main-right $gate2-reverse gate2 main-left))
        (equal $gate2-reverse '(crossing3))

        ;; Empty-occluder and endpoint-control beams acquire no gate splits.
        (not (gate-split-present vertical1-bottom vertical1-top))
        (not (gate-split-present vertical2-bottom vertical2-top))
        (not (gate-split-present vertical3-bottom vertical3-top))
        (not (gate-split-present endpoint-bottom endpoint-top))

        ;; With no direct/relay liveness provider, propagation activates nothing.
        (null (current-crossing-set))
        (not (beam-cut main-left main-right))
        (not (beam-cut main-right main-left))
        (not (beam-cut vertical1-bottom vertical1-top))
        (not (beam-cut vertical2-bottom vertical2-top))
        (not (beam-cut vertical3-bottom vertical3-top))

        ;; Zero-action state and important dynamic absences.
        (has-location idle-agent split-marker)
        (not (open gate1))
        (not (open gate2)))))


(define-goal
  (beam-crossing-coordinates-scenarios-valid))

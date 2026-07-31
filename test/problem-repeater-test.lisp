;;; Filename: problem-repeater-test.lisp

;;; Combined stageable test for repeater.lisp.  Independent beam networks exercise:
;;;
;;;   1. A floor repeater whose beam anchor is base elevation + declared height.
;;;   2. A wall repeater whose beam anchor is its mounting elevation, independent of height.
;;;   3. A connector paired with a fixed repeater, followed by a directional coupling.
;;;   4. Two differently colored sources reaching one repeater in the same propagation
;;;      layer, leaving that repeater unlit just as a connector would be.
;;;   5. A normal beam blocker in a coupled BEAM-VIA corridor, preventing propagation.
;;;   6. Two transmitter-to-repeater links crossing and entering normal crossing
;;;      arbitration.
;;;
;;; All six scenarios coexist safely because they use disjoint endpoints.  The goal is a
;;; zero-action characterization query over the state produced by the ordinary derived-state
;;; initialization action.  This makes one staging/solve run check both mounting forms and
;;; the major relay behaviors without turning the test into a planning puzzle.


(in-package :ww)


(ww-set *problem-name* repeater-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 1)


(defparameter *max-pairings* 2)


;;;; TYPES ;;;;


(define-types
  agent (agent1)
  location (idle paired-location blocked-location)
  transmitter (floor-source wall-source paired-source conflict-red-source
               conflict-blue-source blocked-source crossing-source1 crossing-source2)
  receiver (floor-receiver wall-receiver paired-receiver conflict-receiver
            blocked-receiver crossing-receiver1 crossing-receiver2)
  connector (connector1)
  floor-repeater (floor-repeater1 blocked-repeater)
  wall-repeater (wall-repeater1 paired-repeater conflict-repeater
                 crossing-repeater1 crossing-repeater2)
  gate (unused-gate)
  box (beam-blocker1)
  hue (red blue green amber violet cyan yellow))


;;;; TECHNOLOGY INCLUDES ;;;;


(include-tech repeater)
(include-tech beam-crossing)


;;;; INITIALIZATION ;;;;


(define-init
  (has-location agent1 idle)

  ;; Floor-mounted: base 3 plus height 2 gives anchor elevation 5.
  (has-elevation floor-repeater1 3)
  (has-height floor-repeater1 2)
  (has-elevation floor-source 5)
  (has-elevation floor-receiver 5)
  (has-chroma floor-source amber)
  (has-chroma floor-receiver amber)
  (coupled floor-source floor-repeater1)
  (beam-via floor-source () floor-repeater1)
  (coupled floor-repeater1 floor-receiver)
  (beam-via floor-repeater1 () floor-receiver)

  ;; Wall-mounted: omitted elevation defaults to 1.  Height 4 is horizontal projection,
  ;; so it does not raise the anchor.
  (has-height wall-repeater1 4)
  (has-chroma wall-source violet)
  (has-chroma wall-receiver violet)
  (coupled wall-source wall-repeater1)
  (beam-via wall-source () wall-repeater1)
  (coupled wall-repeater1 wall-receiver)
  (beam-via wall-repeater1 () wall-receiver)

  ;; Mixed movable/fixed relay chain.
  (has-location connector1 paired-location)
  (has-chroma paired-source green)
  (has-chroma paired-receiver green)
  (paired connector1 paired-source)
  (paired connector1 paired-repeater)
  (los-to-apparatus paired-location () paired-source)
  (los-to-apparatus paired-location () paired-repeater)
  (coupled paired-repeater paired-receiver)
  (beam-via paired-repeater () paired-receiver)

  ;; Conflicting colors leave the repeater unlit and cannot activate its receiver.
  (has-chroma conflict-red-source red)
  (has-chroma conflict-blue-source blue)
  (has-chroma conflict-receiver red)
  (coupled conflict-red-source conflict-repeater)
  (beam-via conflict-red-source () conflict-repeater)
  (coupled conflict-blue-source conflict-repeater)
  (beam-via conflict-blue-source () conflict-repeater)
  (coupled conflict-repeater conflict-receiver)
  (beam-via conflict-repeater () conflict-receiver)

  ;; A unit-height box spans the coupled beam's elevation 1 and blocks the first hop.
  (has-chroma blocked-source blue)
  (has-chroma blocked-receiver blue)
  (has-location beam-blocker1 blocked-location)
  (coupled blocked-source blocked-repeater)
  (beam-via blocked-source (blocked-location) blocked-repeater)
  (coupled blocked-repeater blocked-receiver)
  (beam-via blocked-repeater () blocked-receiver)

  ;; Two first-hop fixed links cross properly.  The single active crossing cuts both
  ;; incoming beams, leaving both repeaters and their receivers dark.
  (has-chroma crossing-source1 cyan)
  (has-chroma crossing-receiver1 cyan)
  (has-chroma crossing-source2 yellow)
  (has-chroma crossing-receiver2 yellow)
  (coupled crossing-source1 crossing-repeater1)
  (beam-via crossing-source1 () crossing-repeater1)
  (coupled crossing-repeater1 crossing-receiver1)
  (beam-via crossing-repeater1 () crossing-receiver1)
  (coupled crossing-source2 crossing-repeater2)
  (beam-via crossing-source2 () crossing-repeater2)
  (coupled crossing-repeater2 crossing-receiver2)
  (beam-via crossing-repeater2 () crossing-receiver2)

  ;; APPARATUS-COORDS> names every apparatus functional point.  The horizontal bands keep
  ;; the independent scenarios from crossing accidentally; only the two diagonal first
  ;; hops at y=60..70 cross.  Repeaters have no HAS-LOCATION, including floor repeaters.
  (apparatus-coords> floor-source 0 0)
  (apparatus-coords> floor-repeater1 10 0)
  (apparatus-coords> floor-receiver 20 0)
  (apparatus-coords> wall-source 0 10)
  (apparatus-coords> wall-repeater1 10 10)
  (apparatus-coords> wall-receiver 20 10)
  (apparatus-coords> paired-source 0 20)
  (apparatus-coords> paired-repeater 20 20)
  (apparatus-coords> paired-receiver 30 20)
  (apparatus-coords> conflict-red-source 0 28)
  (apparatus-coords> conflict-blue-source 0 32)
  (apparatus-coords> conflict-repeater 10 30)
  (apparatus-coords> conflict-receiver 20 30)
  (apparatus-coords> blocked-source 0 40)
  (apparatus-coords> blocked-repeater 10 40)
  (apparatus-coords> blocked-receiver 20 40)
  (apparatus-coords> crossing-source1 0 60)
  (apparatus-coords> crossing-repeater1 10 70)
  (apparatus-coords> crossing-receiver1 20 70)
  (apparatus-coords> crossing-source2 10 60)
  (apparatus-coords> crossing-repeater2 0 70)
  (apparatus-coords> crossing-receiver2 -10 70)

  ;; The crossing-coordinate substrate requires coordinates for every declared location.
  (location-coords> idle -20 -20)
  (location-coords> paired-location 10 20)
  (location-coords> blocked-location 5 40))


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!)))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(defun repeaters-are-fixed-p ()
  (null (intersection (gethash 'repeater *types*)
                      (gethash 'mobile-object *types*))))


(define-query repeater-crossing-scenario-valid ()
  (and (= (length (get-current-crossings)) 1)
       (= (length (current-crossing-set)) 1)
       (beam-cut crossing-source1 crossing-repeater1)
       (beam-cut crossing-source2 crossing-repeater2)
       (not (beam-cut crossing-repeater1 crossing-receiver1))
       (not (beam-cut crossing-repeater2 crossing-receiver2))
       (not (exists (?h hue)
              (color crossing-repeater1 ?h)))
       (not (exists (?h hue)
              (color crossing-repeater2 ?h)))
       (not (active crossing-receiver1))
       (not (active crossing-receiver2))))


(define-query repeater-scenarios-valid ()
  (and (= (repeater-anchor-elevation floor-repeater1) 5)
       (= (repeater-anchor-elevation wall-repeater1) 1)
       (repeaters-are-fixed-p)
       (color floor-repeater1 amber)
       (active floor-receiver)
       (color wall-repeater1 violet)
       (active wall-receiver)
       (color connector1 green)
       (color paired-repeater green)
       (active paired-receiver)
       (not (exists (?h hue)
              (color conflict-repeater ?h)))
       (not (active conflict-receiver))
       (not (exists (?h hue)
              (color blocked-repeater ?h)))
       (not (active blocked-receiver))
       (repeater-crossing-scenario-valid)))


(define-goal
  (repeater-scenarios-valid))

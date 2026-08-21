;;; Filename: problem-stream-passability-test.lisp
;;;
;;; Dedicated regression coverage for coordinate-derived wall-stream passability.
;;;
;;; Two disconnected horizontal lanes each contain a wall-gears stream.  A solid
;;; cap continues each stream's upper side curtain exactly to the outer boundary,
;;; so the upper side zone can reach the destination only by crossing the stream.
;;; Coordinate derivation must therefore emit an unguarded directional ride into
;;; the destination and a gears-gated reverse route.  Routes crossing two curtains
;;; of one band must still name that gears only once.
;;;
;;; The lower fan is uncontrolled and remains blowing, characterizing the blocked
;;; ordinary crossings and the still-available inbound rides.  The upper fan is
;;; controlled by the plate under TEST-AGENT.  The required dismount transition clears
;;; that plate, stops the still-mounted fan, and makes every upper gears-gated route
;;; passable without changing any object's location.
;;;
;;; LOWER-GEARS uses the default stream width 3.  UPPER-GEARS overrides it with
;;; exact width 4, putting the tested side curtains at y=9/2 and y=11.
;;;
;;; Expected minimum path length: 1.

(in-package :ww)

(ww-set *problem-name* stream-passability-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 1)


;;;; TYPES ;;;;


(define-types
  agent (test-agent)
  location (lower-swept lower-destination lower-side
            upper-swept upper-destination upper-side)
  pressure-plate (upper-control-plate)
  wall-blower (lower-gears upper-gears)
  wall (backstop lane-separator lower-ride-cap upper-ride-cap))


;;;; TECHNOLOGY INCLUDES ;;;;


(include-tech plate)
(include-tech wall-blower)
(include-tech step)
(include-tech walkability)


;;;; INITIALIZATION ;;;;


(define-init
  ;; TEST-AGENT initially holds the upper control on.  Dismounting is the only
  ;; required state transition and leaves the agent at this same location.
  (has-location test-agent upper-side)
  (has-position upper-control-plate upper-side)
  (on test-agent upper-control-plate)

  ;; Both wall fans are permanent attachments.  LOWER-GEARS is uncontrolled;
  ;; UPPER-GEARS turns only while the occupied plate is energized.
  (has-position lower-gears lower-swept)
  (has-position upper-gears upper-swept)
  (controls ((upper-control-plate)) upper-gears normal)

  (aimed-at lower-gears lower-destination)
  (aimed-at upper-gears upper-destination)

  ;; LOWER-GEARS defaults to width 3.  UPPER-GEARS exercises the override.
  (stream-width upper-gears 4)

  ;; The backstop is the nearest vertical solid behind both rightward streams.
  ;; The lane separator joins it to the right boundary.  Each ride cap joins
  ;; the tested upper side curtain's front endpoint to the right boundary.
  (boundary-wall
    ((0 0) (14 0) (14 12) (0 12) (0 0)))

  (wall-segment> backstop 2 0 2 12)
  (wall-segment> lower-ride-cap 9 9/2 14 9/2)
  (wall-segment> lane-separator 2 6 14 6)
  (wall-segment> upper-ride-cap 9 11 14 11)

  ;; Stream center lines run from (3,3) to (9,3), and from (3,9) to
  ;; (9,9).  LOWER-SIDE and UPPER-SIDE lie beyond the capped upper
  ;; side curtain of their respective stream bands.
  (location-coords> lower-swept 3 3)
  (location-coords> lower-destination 9 3)
  (location-coords> lower-side 5 5)
  (location-coords> upper-swept 3 9)
  (location-coords> upper-destination 9 9)
  (location-coords> upper-side 5 23/2))


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!)))


;;;; CHARACTERIZATION HELPERS ;;;;


(define-query stream-symmetric-family-is
    (?from location ?to location ?expected)
  (do (bind (traverse-via walking ?from $actual ?to))
      (equal $actual ?expected)))


(define-query stream-directional-family-is
    (?from location ?to location ?expected)
  (do (bind (traverse-via> walking ?from $actual ?to))
      (equal $actual ?expected)))


(define-query stream-spec-is (?expected)
  (member ?expected
          (walkability-coordinates-stream-specs)
          :test #'equal))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query stream-passability-scenarios-valid ()
  (and
    ;; Dismounting changes support/control state, not location.
    (has-location test-agent upper-side)
    (not (exists (?support support)
           (on test-agent ?support)))
    (cleartop upper-control-plate)
    (not (depressed upper-control-plate))

    ;; The permanently-live lower stream remains the negative reference.
    (turning lower-gears)
    (blowing lower-gears)
    (not (stream-obstacle-clear test-agent lower-gears))
    (not (obstacle-clear test-agent lower-gears))

    ;; The upper attachment persists, but loss of plate control stops the stream
    ;; and immediately makes its gears obstacle clear.
    (not (turning upper-gears))
    (not (blowing upper-gears))
    (stream-obstacle-clear test-agent upper-gears)
    (obstacle-clear test-agent upper-gears)

    ;; Stream-spec gathering uses the lower default and upper exact override.
    (not (bind (stream-width lower-gears $lower-width)))
    (stream-width upper-gears 4)
    (stream-spec-is
      '(lower-gears lower-swept lower-destination 3 3 9 3 3))
    (stream-spec-is
      '(upper-gears upper-swept upper-destination 3 9 9 9 4))

    ;; Side and swept-location rides into each destination are unconditional.
    ;; Their reverse directions cross the band and require its gears once.
    (stream-directional-family-is
      lower-side lower-destination nil)
    (stream-directional-family-is
      lower-destination lower-side '((lower-gears)))
    (stream-directional-family-is
      lower-swept lower-destination nil)
    (stream-directional-family-is
      lower-destination lower-swept '((lower-gears)))

    (stream-directional-family-is
      upper-side upper-destination nil)
    (stream-directional-family-is
      upper-destination upper-side '((upper-gears)))
    (stream-directional-family-is
      upper-swept upper-destination nil)
    (stream-directional-family-is
      upper-destination upper-swept '((upper-gears)))

    ;; Neither endpoint is a ride destination here, so crossing the side
    ;; curtain remains an ordinary symmetric gears-gated edge.
    (stream-symmetric-family-is
      lower-side lower-swept '((lower-gears)))
    (stream-symmetric-family-is
      upper-side upper-swept '((upper-gears)))

    ;; While LOWER-FAN blows, inbound rides remain usable but every ordinary or
    ;; reverse crossing is blocked.  The exact closures expose any leak.
    (one-step-walkable
      test-agent lower-side lower-destination)
    (one-step-walkable
      test-agent lower-swept lower-destination)
    (not (one-step-walkable
           test-agent lower-destination lower-side))
    (not (one-step-walkable
           test-agent lower-destination lower-swept))
    (not (one-step-walkable
           test-agent lower-side lower-swept))
    (= (length
         (mobility-locations test-agent lower-side))
       2)
    (member 'lower-side
            (mobility-locations test-agent lower-side))
    (member 'lower-destination
            (mobility-locations test-agent lower-side))
    (not (member 'lower-swept
                 (mobility-locations test-agent lower-side)))
    (= (length
         (mobility-locations test-agent lower-destination))
       1)

    ;; Once UPPER-FAN stops, the same raw gears clauses pass in both
    ;; directions and all three upper locations form one walkable closure.
    (one-step-walkable
      test-agent upper-side upper-destination)
    (one-step-walkable
      test-agent upper-destination upper-side)
    (one-step-walkable
      test-agent upper-swept upper-destination)
    (one-step-walkable
      test-agent upper-destination upper-swept)
    (one-step-walkable
      test-agent upper-side upper-swept)
    (= (length
         (mobility-locations test-agent upper-side))
       3)
    (member 'upper-side
            (mobility-locations test-agent upper-side))
    (member 'upper-swept
            (mobility-locations test-agent upper-side))
    (member 'upper-destination
            (mobility-locations test-agent upper-side))

    ;; The solid separator keeps both stream scenarios independent.
    (not (traversable test-agent lower-side upper-side))
    (not (bind (traverse-via walking lower-side
                         $cross-lane-symmetric-family
                         upper-side)))
    (not (bind (traverse-via> walking lower-side
                          $cross-lane-directional-family
                          upper-side)))
    (not (bind (traverse-via> walking upper-side
                          $reverse-cross-lane-family
                          lower-side)))))


(define-goal
  (stream-passability-scenarios-valid))

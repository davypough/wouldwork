;;; Filename: problem-floor-gears-test.lisp

;;; Removable-fan floor-gears regression.  Four independent networks exercise:
;;;
;;;   1. Uncontrolled gears1 launches box1 from fan1 to explicitly elevated loft1 while
;;;      box2 rides along still stacked; the unsupported stack remains hovering because
;;;      fan1 keeps blowing toward that destination.
;;;   2. Uncontrolled gears2 blows through fan2, but loose fan3 resting on fan2 is too
;;;      flat to launch: it is toppled onto base2's ground instead.
;;;   3. Plate-controlled gears3 stays off, so box3 at loft3 falls to base3 even though
;;;      fan4 remains mounted.
;;;   4. Uncontrolled gears4 turns, but with no mounted fan its box4 likewise falls from
;;;      loft4 to base4.
;;;   5. Initialization requires a source and destination for every floor drive, accepts
;;;      distinct destinations, and rejects two drives sharing one because hover state
;;;      otherwise has no unique drop-back source.  It also accepts a geometry-known
;;;      vertical rise and rejects horizontal displacement or a non-rising destination.
;;;
;;; The zero-action goal characterizes the derived start state after ordinary propagation,
;;; covering sustained hover, stack transport, fan immunity, power-off drop, fan-removal
;;; drop, and an explicit destination elevation in one staging/solve run.


(in-package :ww)


(ww-set *problem-name* floor-gears-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 0)


;;;; TYPES ;;;;


(define-types
  agent (agent1)
  location (idle base1 loft1 base2 loft2 base3 loft3 base4 loft4)
  pressure-plate (plate1)
  box (box1 box2 box3 box4)
  floor-gears (gears1 gears2 gears3 gears4)
  fan (fan1 fan2 fan3 fan4)
)


;;;; TECHNOLOGY INCLUDES ;;;;


(include-tech plate)
(include-tech floor-gears)


;;;; INITIALIZATION ;;;;


(define-init
  (has-location agent1 idle)

  ;; Active launch with a stacked rider.  loft1's explicit elevation overrides the
  ;; floor-blowing default of 10.
  (has-location box1 base1)
  (has-location box2 base1)
  (has-location fan1 base1)
  (has-position gears1 base1)
  (mounted-on fan1 gears1)
  (on box1 fan1)
  (on box2 box1)
  (has-elevation loft1 7)
  (aimed-at gears1 loft1)

  ;; Fan immunity.  fan2 blows, but fan3 is only toppled off its top at base2.
  (has-location fan2 base2)
  (has-location fan3 base2)
  (has-position gears2 base2)
  (mounted-on fan2 gears2)
  (on fan3 fan2)
  (has-elevation loft2 8)
  (aimed-at gears2 loft2)

  ;; Power-off drop.  Nothing rests on plate1, so normal control leaves gears3 stopped
  ;; and the unsupported box at loft3 falls back to the gears' location.
  (has-location box3 loft3)
  (has-location fan4 base3)
  (has-position plate1 base3)
  (has-position gears3 base3)
  (mounted-on fan4 gears3)
  (has-elevation loft3 9)
  (controls ((plate1)) gears3 normal)
  (aimed-at gears3 loft3)

  ;; Fan-removal drop.  gears4 is uncontrolled and therefore turns, but no fan is
  ;; mounted on it, so nothing sustains box4 at loft4.
  (has-location box4 loft4)
  (has-position gears4 base4)
  (has-elevation loft4 11)
  (aimed-at gears4 loft4)
)


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!))
)


;;;; INITIALIZATION VALIDATION ;;;;


(define-test-helper floor-gears-complete-endpoints ()
  '((has-position gears1 base1)
    (aimed-at gears1 loft1)
    (has-position gears2 base2)
    (aimed-at gears2 loft2)
    (has-position gears3 base3)
    (aimed-at gears3 loft3)
    (has-position gears4 base4)
    (aimed-at gears4 loft4)))


(define-test-claim floor-gears-endpoint-validation
  (null
    (validate-init-literals
      (floor-gears-complete-endpoints)
      :checks '(gears-fan-init-check)))
  (expect-condition
    (lambda ()
      (validate-init-literals
        (remove '(has-position gears1 base1)
                (floor-gears-complete-endpoints)
                :test #'equal)
        :checks '(gears-fan-init-check)))
    'init-check-failure
    :containing "has no HAS-POSITION source"
    :check 'gears-fan-init-check)
  (expect-condition
    (lambda ()
      (validate-init-literals
        (remove '(aimed-at gears1 loft1)
                (floor-gears-complete-endpoints)
                :test #'equal)
        :checks '(gears-fan-init-check)))
    'init-check-failure
    :containing "has no AIMED-AT destination"
    :check 'gears-fan-init-check)
  (expect-condition
    (lambda ()
      (validate-init-literals
        (substitute '(aimed-at gears2 loft1)
                    '(aimed-at gears2 loft2)
                    (floor-gears-complete-endpoints)
                    :test #'equal)
        :checks '(floor-blowing-init-check)))
    'init-check-failure
    :containing "must not share an AIMED-AT destination"
    :check 'floor-blowing-init-check)

  (null
    (validate-init-literals
      (append
        (floor-gears-complete-endpoints)
        '((location-coords> base1 2 3 0)
          (location-coords> loft1 2 3 7)))
      :checks '(floor-blowing-init-check)))
  (expect-condition
    (lambda ()
      (validate-init-literals
        (append
          (floor-gears-complete-endpoints)
          '((location-coords> base1 2 3 0)
            (location-coords> loft1 4 3 7)))
        :checks '(floor-blowing-init-check)))
    'init-check-failure
    :containing "must be vertical"
    :check 'floor-blowing-init-check)

  (null
    (validate-init-literals
      (append
        (floor-gears-complete-endpoints)
        '((has-elevation base1 2)
          (has-elevation loft1 3)))
      :checks '(floor-blowing-init-check)))
  (expect-condition
    (lambda ()
      (validate-init-literals
        (append
          (floor-gears-complete-endpoints)
          '((has-elevation base1 2)
            (has-elevation loft1 2)))
        :checks '(floor-blowing-init-check)))
    'init-check-failure
    :containing "must be above its source"
    :check 'floor-blowing-init-check))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query floor-gears-scenarios-valid ()
  (and
    ;; Sustained, explicitly elevated stack hover.
    (= (location-elevation loft1) 7)
    (turning gears1)
    (blowing fan1)
    (mounted-on fan1 gears1)
    (has-location fan1 base1)
    (not (has-location box1 base1))
    (has-location box1 loft1)
    (has-location box2 loft1)
    (not (exists (?support support)
           (on box1 ?support)))
    (on box2 box1)

    ;; Loose fan immunity.
    (turning gears2)
    (blowing fan2)
    (not (blowing fan3))
    (has-location fan3 base2)
    (not (has-location fan3 loft2))
    (not (on fan3 fan2))

    ;; Power-off drop.
    (not (depressed plate1))
    (not (turning gears3))
    (not (blowing fan4))
    (mounted-on fan4 gears3)
    (not (has-location box3 loft3))
    (has-location box3 base3)

    ;; Missing-fan drop despite turning gears.
    (turning gears4)
    (not (exists (?fan fan)
           (mounted-on ?fan gears4)))
    (not (has-location box4 loft4))
    (has-location box4 base4)))


(define-goal
  (floor-gears-scenarios-valid))

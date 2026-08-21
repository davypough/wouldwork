;;; Filename: problem-floor-blower-test.lisp

;;; Fixed floor-blower regression.  Three independent units exercise the shared
;;; floor-directed physics without a removable mounted fan:
;;;
;;;   1. An uncontrolled fixed blower launches BOX1 to LOFT1 and carries BOX2 with it
;;;      still stacked.  The stream remains active, sustaining the unsupported stack.
;;;   2. A loose fan resting on another uncontrolled fixed blower is too flat to launch;
;;;      it is toppled onto the source location's ground instead.
;;;   3. A plate-controlled fixed blower starts off, so an unsupported box at its
;;;      destination drops back to the blower's source.
;;;
;;; The zero-action goal also characterizes fixed-unit identity: each floor-blower is its
;;; own drive, needs no mounted fan to be present, and exposes the same flush support
;;; surface from which occupants launch.


(in-package :ww)


(ww-set *problem-name* floor-blower-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 0)


;;;; TYPES ;;;;


(define-types
  agent (agent1)
  location (idle base1 loft1 base2 loft2 base3 loft3)
  pressure-plate (off-plate)
  box (box1 box2 box3)
  fan (loose-fan)
  floor-blower (active-blower fan-blower off-blower))


;;;; TECHNOLOGY INCLUDES ;;;;


(include-tech plate)
(include-tech floor-blower)


;;;; INITIALIZATION ;;;;


(define-init
  (has-location agent1 idle)

  ;; Direct fixed-unit launch with a stacked rider.
  (has-position active-blower base1)
  (aimed-at active-blower loft1)
  (has-elevation loft1 7)
  (has-location box1 base1)
  (has-location box2 base1)
  (on box1 active-blower)
  (on box2 box1)

  ;; Loose-fan immunity on a second active fixed unit.
  (has-position fan-blower base2)
  (aimed-at fan-blower loft2)
  (has-elevation loft2 8)
  (has-location loose-fan base2)
  (on loose-fan fan-blower)

  ;; Controlled-off drop from the third unit's destination.
  (has-position off-plate base3)
  (has-position off-blower base3)
  (aimed-at off-blower loft3)
  (has-elevation loft3 9)
  (controls ((off-plate)) off-blower normal)
  (has-location box3 loft3))


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!)))


;;;; CHARACTERIZATION ;;;;


(define-test-claim fixed-floor-blower-type-contract
  (expect-type-instances
    'floor-blower '(active-blower fan-blower off-blower))
  (expect-type-instances
    'blower '(active-blower fan-blower off-blower)))


(define-query floor-blower-scenarios-valid ()
  (and
    ;; A fixed unit is its own always-present drive and needs no MOUNTED-ON fact.
    (eql (blower-drive active-blower) 'active-blower)
    (blower-present active-blower)
    (turning active-blower)
    (blowing active-blower)

    ;; The launched stack remains unsupported at the live stream's destination.
    (has-location box1 loft1)
    (has-location box2 loft1)
    (not (has-location box1 base1))
    (not (exists (?support support)
           (on box1 ?support)))
    (on box2 box1)
    (cleartop active-blower)

    ;; A loose fan is toppled but not launched or made into a blowing source.
    (turning fan-blower)
    (blowing fan-blower)
    (has-location loose-fan base2)
    (not (has-location loose-fan loft2))
    (not (on loose-fan fan-blower))
    (not (blowing loose-fan))
    (cleartop fan-blower)

    ;; The controlled-off unit cannot sustain its destination, so BOX3 falls home.
    (not (depressed off-plate))
    (not (turning off-blower))
    (not (blowing off-blower))
    (not (has-location box3 loft3))
    (has-location box3 base3)))


(define-goal
  (floor-blower-scenarios-valid))

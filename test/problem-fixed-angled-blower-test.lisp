;;; Filename: problem-fixed-angled-blower-test.lisp

;;; Fixed angled-blower regression.  Four independent units exercise the angled physics
;;; without removable mounted fans:
;;;
;;;   1. A stacked pair arcs onto a clear box whose explicitly raised top is accepted.
;;;   2. A box arcs to an elevated destination with no support and lands on bare ground.
;;;   3. A loose fan resting on a fixed blower is toppled in place rather than launched.
;;;   4. Clear plate1 drives a fixed blower through inverted control, launching its box
;;;      during initialization.  The one required configuration transition mounts plate1,
;;;      switching the blower off; the delivered box must remain at its destination.
;;;
;;; The goal also characterizes fixed-unit identity: each angled-blower is its own
;;; always-present drive and directly exposes the flush support surface from which
;;; occupants launch.  Expected minimum solution: one step onto PLATE1.


(in-package :ww)


(ww-set *problem-name* fixed-angled-blower-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 2)

(setf *expected-min-length* 1)


;;;; TYPES ;;;;


(define-types
  agent (agent1)
  location
    (control source1 raised1 source2 bare2 source3 ignored3 source4 delivered4)
  pressure-plate (plate1)
  box (box1 box2 landing-box box3 box4)
  fan (loose-fan)
  angled-blower (support-blower bare-blower fan-blower controlled-blower))


;;;; TECHNOLOGY INCLUDES ;;;;


(include-tech plate)
(include-tech angled-blower)
(include-tech step)


;;;; INITIALIZATION ;;;;


(define-init
  (has-location agent1 control)

  ;; Raised-support landing with a stacked rider.  RAISED1 is level 4 and LANDING-BOX
  ;; is height 3, so BOX1 lands on its top at elevation 7.
  (has-position support-blower source1)
  (aimed-at support-blower raised1)
  (has-elevation raised1 4)
  (has-location box1 source1)
  (has-location box2 source1)
  (has-location landing-box raised1)
  (has-height landing-box 3)
  (on box1 support-blower)
  (on box2 box1)

  ;; Bare-ground landing at an explicitly elevated destination.
  (has-position bare-blower source2)
  (aimed-at bare-blower bare2)
  (has-elevation bare2 6)
  (has-location box3 source2)
  (on box3 bare-blower)

  ;; Loose-fan immunity on a third uncontrolled fixed unit.
  (has-position fan-blower source3)
  (aimed-at fan-blower ignored3)
  (has-elevation ignored3 8)
  (has-location loose-fan source3)
  (on loose-fan fan-blower)

  ;; One-shot persistence after inverted control turns the fourth unit off.
  (has-position plate1 control)
  (has-position controlled-blower source4)
  (aimed-at controlled-blower delivered4)
  (has-elevation delivered4 9)
  (controls ((plate1)) controlled-blower inverted)
  (has-location box4 source4)
  (on box4 controlled-blower))


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!)))


;;;; CHARACTERIZATION ;;;;


(define-test-claim fixed-angled-blower-type-contract
  (expect-type-instances
    'angled-blower
    '(support-blower bare-blower fan-blower controlled-blower))
  (expect-type-instances
    'blower
    '(support-blower bare-blower fan-blower controlled-blower)))


(define-query fixed-angled-blower-scenarios-valid ()
  (and
    ;; Fixed-unit identity and raised-support stack landing.
    (eql (blower-drive support-blower) 'support-blower)
    (blower-present support-blower)
    (turning support-blower)
    (blowing support-blower)
    (= (top landing-box) 7)
    (has-location box1 raised1)
    (has-location box2 raised1)
    (not (has-location box1 source1))
    (on box1 landing-box)
    (on box2 box1)
    (cleartop support-blower)

    ;; Bare-ground fallback.
    (turning bare-blower)
    (blowing bare-blower)
    (has-location box3 bare2)
    (not (has-location box3 source2))
    (not (exists (?support support)
           (on box3 ?support)))
    (cleartop bare-blower)

    ;; Loose fan toppling.
    (turning fan-blower)
    (blowing fan-blower)
    (has-location loose-fan source3)
    (not (has-location loose-fan ignored3))
    (not (on loose-fan fan-blower))
    (not (blowing loose-fan))
    (cleartop fan-blower)

    ;; One-shot delivery persists after the controlling transition stops the blower.
    (on agent1 plate1)
    (depressed plate1)
    (not (turning controlled-blower))
    (not (blowing controlled-blower))
    (has-location box4 delivered4)
    (not (has-location box4 source4))
    (not (exists (?support support)
           (on box4 ?support)))))


(define-goal
  (fixed-angled-blower-scenarios-valid))

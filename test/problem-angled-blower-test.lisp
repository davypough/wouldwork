;;; Filename: problem-angled-blower-test.lisp

;;; Combined angled-blower regression.  Five isolated networks exercise:
;;;
;;;   1. A stacked pair arcs onto a clear box whose explicitly raised top is accepted.
;;;   2. A box arcs to an explicitly elevated destination with no support and lands on
;;;      bare ground.
;;;   3. A box lands on a second blowing fan and is launched onward through a two-fan
;;;      chain.
;;;   4. A loose fan resting on a blowing fan is toppled in place rather than launched.
;;;   5. Clear plate1 drives gears6 through inverted control, so box6 is delivered during
;;;      initialization.  The one required action, step-on plate1, depresses the plate
;;;      and switches gears6 and fan7 off; box6 must remain at delivered5.
;;;
;;; The goal's characterization query verifies all positive outcomes and the absence of
;;; stale source/intermediate supports and locations.  Expected minimum solution: one
;;; step, (step-on agent1 plate1).

(in-package :ww)

(ww-set *problem-name* angled-blower-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 2)

(setf *expected-min-length* 1)


;;;; TYPES ;;;;


(define-types
  agent (agent1)
  location
    (control5 source1 raised1 source2 bare2 source3 relay3 final3
     source4 ignored4 source5 delivered5)
  plate (plate1)
  box (box1 box2 box3 box4 box5 box6)
  angled-gears (gears1 gears2 gears3 gears4 gears5 gears6)
  fan (fan1 fan2 fan3 fan4 fan5 fan6 fan7)
  mode (inverted)
)


;;;; TECHNOLOGY INCLUDES ;;;;


(include-tech plate)
(include-tech angled-blower)
(include-tech step)


;;;; INITIALIZATION ;;;;


(define-init
  (has-location agent1 control5)

  ;; Raised-support landing with a stacked rider.  raised1's floor is level 4 and box3
  ;; is height 3, so box1 lands at the explicitly non-flush top elevation 7.
  (has-location fan1 source1)
  (has-location box1 source1)
  (has-location box2 source1)
  (has-location box3 raised1)
  (has-position gears1 source1)
  (mounted-on fan1 gears1)
  (on box1 fan1)
  (on box2 box1)
  (has-elevation raised1 4)
  (has-height box3 3)
  (aimed-at> gears1 raised1)

  ;; Bare-ground landing at an explicitly elevated destination.
  (has-location fan2 source2)
  (has-location box4 source2)
  (has-position gears2 source2)
  (mounted-on fan2 gears2)
  (on box4 fan2)
  (has-elevation bare2 6)
  (aimed-at> gears2 bare2)

  ;; Two-fan chain.  box5 first lands on fan4 at relay3, then fan4 launches it to
  ;; final3 during the same propagation fixpoint.
  (has-location fan3 source3)
  (has-location box5 source3)
  (has-position gears3 source3)
  (mounted-on fan3 gears3)
  (on box5 fan3)
  (has-elevation relay3 2)
  (aimed-at> gears3 relay3)

  (has-location fan4 relay3)
  (has-position gears4 relay3)
  (mounted-on fan4 gears4)
  (has-elevation final3 5)
  (aimed-at> gears4 final3)

  ;; Fan immunity.  fan6 is loose cargo resting on fan5, so it is toppled onto source4
  ;; rather than launched to ignored4.
  (has-location fan5 source4)
  (has-location fan6 source4)
  (has-position gears5 source4)
  (mounted-on fan5 gears5)
  (on fan6 fan5)
  (has-elevation ignored4 8)
  (aimed-at> gears5 ignored4)

  ;; Persistence after power loss.  With plate1 initially clear, inverted control turns
  ;; gears6 on and initialization launches box6.  Stepping onto plate1 turns them off.
  (has-position plate1 control5)
  (has-location fan7 source5)
  (has-location box6 source5)
  (has-position gears6 source5)
  (mounted-on fan7 gears6)
  (on box6 fan7)
  (controls ((plate1)) gears6 inverted)
  (has-elevation delivered5 9)
  (aimed-at> gears6 delivered5)
)


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!))
)


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query angled-blower-scenarios-valid ()
  (and
    ;; Raised support and stack transport.
    (= (location-elevation raised1) 4)
    (= (support-top-elevation box3) 7)
    (turning gears1)
    (blowing fan1)
    (has-location fan1 source1)
    (has-location box1 raised1)
    (has-location box2 raised1)
    (not (has-location box1 source1))
    (not (on box1 fan1))
    (on box1 box3)
    (on box2 box1)

    ;; Bare-ground landing.
    (= (location-elevation bare2) 6)
    (turning gears2)
    (blowing fan2)
    (has-location box4 bare2)
    (not (has-location box4 source2))
    (not (on box4 fan2))
    (not (exists (?support support)
           (on box4 ?support)))

    ;; Chained launch through fan4.
    (= (location-elevation relay3) 2)
    (= (location-elevation final3) 5)
    (turning gears3)
    (turning gears4)
    (blowing fan3)
    (blowing fan4)
    (has-location fan4 relay3)
    (has-location box5 final3)
    (not (has-location box5 source3))
    (not (has-location box5 relay3))
    (not (on box5 fan3))
    (not (on box5 fan4))
    (not (exists (?support support)
           (on box5 ?support)))

    ;; Loose fan immunity.
    (turning gears5)
    (blowing fan5)
    (not (blowing fan6))
    (has-location fan6 source4)
    (not (has-location fan6 ignored4))
    (not (on fan6 fan5))
    (not (exists (?support support)
           (on fan6 ?support)))

    ;; One-shot delivery persists after inverted control switches the blower off.
    (on agent1 plate1)
    (depressed plate1)
    (not (turning gears6))
    (not (blowing fan7))
    (mounted-on fan7 gears6)
    (has-location fan7 source5)
    (has-location box6 delivered5)
    (not (has-location box6 source5))
    (not (on box6 fan7))
    (not (exists (?support support)
           (on box6 ?support)))))


(define-goal
  (angled-blower-scenarios-valid))

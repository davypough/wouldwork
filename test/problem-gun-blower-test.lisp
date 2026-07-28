;;; Filename: problem-gun-blower-test.lisp

;;; Exercises -threat's enforce-threat-safety! backstop, not accessibility's precondition
;;; check: gears1 is uncontrolled (always turning, -gears-fan's own default), so fan1 is
;;; already blowing once mounted -- stepping onto it launches the agent to loft with no
;;; move/jump/ladder precondition involved at all.  gun1 is a point fixture (positioned via
;;; LOS, not has-position) and threatens loft.  While gun1 is armed, the step-on child state
;;; that would land the agent at loft is generated and then dropped as inconsistent-state,
;;; so step-on becomes a dead end from the unjammed state -- there is no successor at all
;;; through it, not a refusal to launch.  Jamming a gun is a line-of-sight check only
;;; (jam-target's gun branch reads visible/los-to-apparatus, exactly like a gate), so
;;; visibility is included and the sightline from lower1 to gun1 is hand-authored directly
;;; -- (los-to-apparatus lower1 () gun1) -- rather than derived from wall-segments.
;;; Expected minimum solution (3 steps): pickup-jammer jammer1, jam-target gun1 (at lower1,
;;; via the hand-authored sightline), step-on fan1.


(in-package :ww)


(ww-set *problem-name* gun-blower-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 8)


;;;; TYPES ;;;;


(define-types
  agent (agent1)
  location (lower1 loft)
  jammer (jammer1)
  gun (gun1)
  floor-gears (gears1)
  fan (fan1)
)


;;;; TECHNOLOGY INCLUDES ;;;;


(include-tech gun)
(include-tech jammer)
(include-tech floor-blower)
(include-tech step)
(include-tech visibility)


;;;; INITIALIZATION ;;;;


(define-init
  ;; Movable objects
  (has-location agent1 lower1)
  (has-location jammer1 lower1)
  (has-location fan1 lower1)

  ;; Fixed-position objects; gears1 sits at lower1.
  (has-position gears1 lower1)

  ;; gun1's sightline from lower1, hand-authored directly: an empty occluder list is a
  ;; direct, always-clear line.
  (los-to-apparatus lower1 () gun1)

  ;; The fan starts mounted on the gears (an attachment, not an (on ...) support fact).
  ;; gears1 is uncontrolled, so it is turning from t=0 -- the fan blows as soon as
  ;; something rests on it, with no plate/receiver wiring needed.
  (mounted-on fan1 gears1)

  ;; gun1's kill zone; loft declares no elevation, so it floats at floor-blower's default
  ;; hover elevation of 10.
  (threatens gun1 (loft))

  ;; Air-stream destination.
  (aimed-at> gears1 loft)
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
  (has-location agent1 loft)
)

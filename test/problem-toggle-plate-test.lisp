(in-package :ww)

;;; Characterizes the intended toggle-plate lifecycle.  Three actions are
;;; STEP-ON, STEP-OFF, STEP-ON: the first press energizes both
;;; controlled devices, stepping off preserves the latched state, and the
;;; second press de-energizes both devices while depressing the plate again.
;;; Independently, a second plate initializes beneath one weight with its latch
;;; explicitly on.  Initial propagation establishes depression without changing
;;; that stored state; STACK-SECOND-WEIGHT adds another occupant while the plate
;;; remains depressed and must not count as a new press.  Two more fixtures prove
;;; that initial occupancy does not turn an unlatched plate on, and that a clear
;;; plate may begin explicitly latched.  The four required actions may occur in
;;; any order.

(ww-set *problem-name* toggle-plate-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 4)

(setf *expected-min-length* 4)

(define-types
  agent (agent1)
  location (site unused)
  toggle-plate (plate1 stacked-plate occupied-unlatched-plate clear-latched-plate)
  box (weight-a weight-b weight-c)
  test-phase (before-stack after-stack)
  gate (gate1)
  floor-gears (gears1))

(include-tech plate)
(include-tech step)
(include-tech gate)
(include-tech -gears-fan)

(define-dynamic-relations
  (current-phase test-phase))

(define-init
  (has-location agent1 site)
  (has-position plate1 site)
  (has-position stacked-plate site)
  (has-position occupied-unlatched-plate site)
  (has-position clear-latched-plate site)
  (has-position gears1 site)
  (aimed-at gears1 unused)
  (has-location weight-a site)
  (has-location weight-b site)
  (has-location weight-c site)
  (on weight-a stacked-plate)
  (on weight-c occupied-unlatched-plate)
  (latched stacked-plate)
  (latched clear-latched-plate)
  (current-phase before-stack)
  (controls ((plate1)) gate1 normal)
  (controls ((plate1)) gears1 normal))

(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!)))

(define-action stack-second-weight
  1
  ()
  (and (current-phase before-stack)
       (on weight-a stacked-plate)
       (not (on weight-b stacked-plate))
       (depressed stacked-plate)
       (latched stacked-plate))
  ("> second weight is stacked on the already-depressed toggle plate")
  (assert (on weight-b stacked-plate)
          (not (current-phase before-stack))
          (current-phase after-stack)
          (finally (propagate-changes!))))

(define-goal
  (and (on agent1 plate1)
       (depressed plate1)
       (not (latched plate1))
       (not (open gate1))
       (not (turning gears1))
       (current-phase after-stack)
       (on weight-a stacked-plate)
       (on weight-b stacked-plate)
       (depressed stacked-plate)
       (latched stacked-plate)
       (depressed occupied-unlatched-plate)
       (not (latched occupied-unlatched-plate))
       (not (depressed clear-latched-plate))
       (latched clear-latched-plate)))

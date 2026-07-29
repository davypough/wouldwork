;;; Filename: problem-floor-blower-test.lisp

;;; Minimal floor-blower (gears + fan) exercise.  lower1 holds the agent, box1, and the
;;; plate-controlled gears1 with fan1 pre-mounted; lower2 holds the control plate and a
;;; spare box2 (an alternative, longer way to depress the plate).  loft declares no
;;; has-elevation fact, so it floats at floor-blower's default landing elevation of 10.
;;; Expected minimum solution (4 steps): pickup-box box1, put-box box1 on fan1 (the gears
;;; are not yet turning, so it rests there), walk to lower2, step-on plate1 -- the agent's
;;; weight depresses the plate, setting the gears turning; the mounted fan blows and
;;; launches box1 to loft.


(in-package :ww)


(ww-set *problem-name* floor-blower-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 8)


;;;; TYPES ;;;;


(define-types
  agent (agent1)
  location (lower1 lower2 loft)
  plate (plate1)
  box (box1 box2)
  floor-gears (gears1)
  fan (fan1)
  mode (normal inverted)
)


;;;; TECHNOLOGY INCLUDES ;;;;


(include-tech plate)
(include-tech floor-blower)
(include-tech box)
(include-tech step)
(include-tech walkability)


;;;; INITIALIZATION ;;;;


(define-init
  ;; Movable objects
  (has-location agent1 lower1)
  (has-location box1 lower1)
  (has-location box2 lower2)
  (has-location fan1 lower1)

  ;; Fixed-position objects
  (has-position plate1 lower2)
  (has-position gears1 lower1)

  ;; The fan starts mounted on the gears (an attachment, not an (on ...) support fact).
  (mounted-on fan1 gears1)

  ;; Walking topology; lower1 and lower2 default to ground elevation 0.
  ;; loft declares no elevation, so it takes floor-blower's default of 10.
  (walk-via lower1 () lower2)

  ;; Gears control and air-stream destination
  (controls ((plate1)) gears1 normal)
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
  (has-location box1 loft)
)

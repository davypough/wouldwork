;;; Filename: problem-wall-blower-test.lisp

;;; Minimal wall-blower (wall-gears + fan) exercise.  near holds the agent and the
;;; control plate; mid is the location faced by wgears1 (mounted on mid's wall at the
;;; default stream elevation 1) and holds box1; far is the air stream's aimed-at>
;;; destination, an ordinary ground location.  fan1 starts mounted on wgears1, hanging
;;; with no has-location.  Expected minimum solution (1 step): step-on plate1 -- the
;;; agent's weight depresses the plate, setting the gears turning; the mounted fan blows
;;; and sweeps box1 from mid to far.  A second, longer route (walking to mid) would
;;; instead sweep the agent itself to far, which does not achieve the goal.


(in-package :ww)


(ww-set *problem-name* wall-blower-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 6)


;;;; TYPES ;;;;


(define-types
  agent (agent1)
  location (near mid far)
  plate (plate1)
  box (box1)
  wall-gears (wgears1)
  fan (fan1)
  mode (normal inverted)
)


;;;; TECHNOLOGY INCLUDES ;;;;


(include-tech plate)
(include-tech wall-blower)
(include-tech box)
(include-tech step)
(include-tech accessibility)


;;;; INITIALIZATION ;;;;


(define-init
  ;; Movable objects; fan1 is wall-mounted, so it has no has-location.
  (has-location agent1 near)
  (has-location box1 mid)

  ;; Fixed-position objects; wgears1 hangs on mid's wall, facing (sweeping) mid.
  (has-position plate1 near)
  (has-position wgears1 mid)

  ;; The fan starts mounted on the wall gears (an attachment, not an (on ...) fact).
  (mounted-on fan1 wgears1)

  ;; Walking topology; all locations are ordinary ground (elevation 0), and wgears1
  ;; declares no has-elevation, so its stream works at the default elevation 1.
  (walk-via near () mid)
  (walk-via mid () far)

  ;; Gears control and air-stream destination
  (controls ((plate1)) wgears1 normal)
  (aimed-at> wgears1 far)
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
  (has-location box1 far)
)

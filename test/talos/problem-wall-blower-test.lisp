;;; Filename: problem-wall-blower-test.lisp

;;; Wall-blower (wall-gears + fan) regression.  near holds the agent and control plate;
;;; mid is the location faced by wgears1 at its default stream elevation 1 and holds
;;; box1; far is the air stream's destination and holds plate2, whose top is flush with
;;; far's elevation-0 floor.  fan1 starts mounted on wgears1, hanging with no location.
;;;
;;; Expected minimum solution (1 step): mount agent1 on plate1.  The agent's weight
;;; depresses plate1, setting the gears turning and the mounted fan blowing.  The stream
;;; meets the unit-height box at its inclusive upper boundary, sweeps it from mid to far,
;;; and lands it on plate2 rather than bare ground.  The goal checks that whole lifecycle:
;;; both plate states, turning/blowing, wall mounting, the fan's absent location, the
;;; box's relocation and support, and the agent's unchanged location.


(in-package :ww)


(ww-set *problem-name* wall-blower-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 1)


;;;; TYPES ;;;;


(define-types
  agent (agent1)
  location (near mid far)
  pressure-plate (plate1 plate2)
  box (box1)
  wall-gears (wgears1)
  fan (fan1)
)


;;;; TECHNOLOGY INCLUDES ;;;;


(include-tech plate)
(include-tech wall-blower)
(include-tech step)


;;;; INITIALIZATION ;;;;


(define-init
  ;; Movable objects; fan1 is wall-mounted, so it has no has-location.
  (has-location agent1 near)
  (has-location box1 mid)

  ;; Fixed-position objects; wgears1 hangs on mid's wall, facing (sweeping) mid.
  ;; plate2 sits at far, uncontrolling, solely as a landing support for box1.
  (has-position plate1 near)
  (has-position plate2 far)
  (has-position wgears1 mid)

  ;; The fan starts mounted on the wall gears (an attachment, not an (on ...) fact).
  (mounted-on fan1 wgears1)

  ;; All locations are ordinary ground (elevation 0), and wgears1 declares no
  ;; has-elevation, so its stream works at the default elevation 1.
  ;; Gears control and air-stream destination:
  (controls ((plate1)) wgears1 normal)
  (aimed-at wgears1 far)
)


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!))
)


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query wall-blower-scenario-valid ()
  (and (has-location agent1 near)
       (on agent1 plate1)
       (depressed plate1)
       (turning wgears1)
       (blowing fan1)
       (mounted-on fan1 wgears1)
       (not (exists (?location location)
              (has-location fan1 ?location)))
       (not (has-location box1 mid))
       (has-location box1 far)
       (on box1 plate2)
       (depressed plate2)))


(define-goal
  (wall-blower-scenario-valid))

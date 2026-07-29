;;; Filename: problem-angled-blower-test.lisp

;;; Minimal angled-blower (angled-gears + fan) exercise.  near holds the agent and the
;;; control plate; pad holds agears1 with fan1 pre-mounted (angled-mounted, so fan1 has a
;;; has-location like a floor fan) and box1, which starts resting directly on fan1; far is
;;; the arc's aimed-at> destination and already holds box2, a clear box sitting there.
;;; Deliberately, there is no walk-via between pad and far at all -- standing in for a
;;; wall or fence foot traffic cannot cross -- so reaching far by walking is not merely
;;; longer, it is impossible; only the arc gets there.  Expected minimum solution (1
;;; step): step-on plate1 -- the agent's weight depresses the plate, setting agears1
;;; turning; the mounted fan arcs box1 from pad to far, landing it on box2 (box2's top
;;; sits above far's floor elevation, which an angled-blower arc accepts and a wall-blower
;;; sweep would not) rather than on bare ground.

(in-package :ww)

(ww-set *problem-name* angled-blower-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 6)


;;;; TYPES ;;;;


(define-types
  agent (agent1)
  location (near pad far)
  plate (plate1)
  box (box1 box2)
  angled-gears (agears1)
  fan (fan1)
  mode (normal inverted)
)


;;;; TECHNOLOGY INCLUDES ;;;;


(include-tech plate)
(include-tech angled-blower)
(include-tech box)
(include-tech step)
(include-tech walkability)


;;;; INITIALIZATION ;;;;


(define-init
  ;; Movable objects; fan1 is angled-mounted at pad, so it has a has-location like a
  ;; floor fan, and box1 starts resting directly on it.  box2 sits clear at far, a
  ;; pre-existing landing support.
  (has-location agent1 near)
  (has-location fan1 pad)
  (has-location box1 pad)
  (on box1 fan1)
  (has-location box2 far)

  ;; Fixed-position objects; agears1 sits flush at pad.
  (has-position plate1 near)
  (has-position agears1 pad)

  ;; The fan starts mounted on the angled gears (an attachment, not an (on ...) fact).
  (mounted-on fan1 agears1)

  ;; Walking topology; near connects to pad, but pad has no walk-via to far at all,
  ;; standing in for a wall/fence that blocks foot traffic but not the arc.
  (walk-via near () pad)

  ;; Gears control and arc destination.
  (controls ((plate1)) agears1 normal)
  (aimed-at> agears1 far)
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
  (and (has-location box1 far)
       (on box1 box2))
)

;;;; Filename: problem-windtunnel-topo.lisp

;;; Talos Principle problem in Purgatory workshop


(in-package :ww)


(ww-set *problem-name* windtunnel-topo)

(ww-set *problem-type* planning)

(ww-set *solution-type* first)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 16)

(ww-set *progress-reporting-interval* 3000000)


(defparameter *max-pairings* 2)


(define-types
  agent           (agent1 agent1*)
  recorder        (recorder1)
  gate            (gate1 gate2)
  wall            (wall1 wall2)
  connector       (connector1 connector1*)
  floor-repeater  (repeater1)
  transmitter     (transmitter1)
  receiver        (receiver1)
  toggle-plate    (plate1)
  wall-blower     (wgears1)
  hue             (blue)  ;the hue of a transmitter, receiver, repeater, or active connector
  location        (location1 location2 location3 location4 location5 location6))


;;;; TECHNOLOGY INCLUDES ;;;;


(include-tech plate)
(include-tech step)  ;stepping on plate1 toggles its controlled devices; stepping off does nothing
(include-tech wall-blower)
(include-tech gate)
(include-tech beam-relay)
(include-tech visibility)
(include-tech walkability)
(include-tech recorder)
(include-tech -terrain-consistency)  ;holds the authored levels against the derived zones


;;;; INITIALIZATION ;;;;


(define-init
  ;; Dynamic state (agent-manipulable or derived).  The asterisk names in DEFINE-TYPES above
  ;; are the recording copies: the recorder derives each RECORDING-COPY> pair from them, so
  ;; no mapping is declared here.  Ghosts have no initial location either -- START-RECORDER
  ;; forks each one from its live counterpart's current state when the search finds it, per
  ;; rule 5, and a ghost does not exist beforehand.
  (has-location agent1 location1)
  (has-location connector1 location1)

  ;; Static spatial configuration
  (location-coords> location1  4 20)
  (location-coords> location2 11 14)
  (location-coords> location3 229/10 8)
  (location-coords> location4 20 3)
  (location-coords> location5 14 3)
  (location-coords> location6 9 8)

  ;; Fixed-position objects
  (has-position wgears1 location3)
  (has-position plate1 location2)
  (has-position recorder1 location1)

  ;; Static fixture configuration
  (apparatus-coords> transmitter1 189/10 19)
  (apparatus-coords> receiver1 21 1/10)
  (apparatus-coords> repeater1 3 8)
  (controls ((plate1)) gate1 normal)
  (controls ((plate1)) wgears1 normal)
  (controls ((receiver1)) gate2 normal)
  (aimed-at wgears1 location6)
  (gate-segment> gate1 12 21 12 17)
  (gate-segment> gate2 17 6 17 0)
  (wall-segment> wall1 12 6 19 6)
  (wall-segment> wall2 12 0 12 6)
  (has-chroma transmitter1 blue)
  (has-chroma receiver1 blue)
  (stream-width wgears1 4)

  (boundary-wall
    ((0 21) (19 21) (19 17) (12 17) (12 10) (23 10) (23 0) (0 0) (0 21)
    ))
)


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!)))


;;;; GOAL ;;;;


(define-goal
  (and (has-location agent1 location5)
       ;(ghost-stops-recorder)
  )
)

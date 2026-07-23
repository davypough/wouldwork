;;; Filename: problem-wall-blower-test.lisp

;;; Claustrophobia problem from Subscription -- Oti's Trials

(in-package :ww)


(ww-set *problem-name* phobia)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 15)


(defparameter *max-pairings* 2)  ;rename to max-connector-pairings for clarity


;;;; TYPES ;;;;


(define-types
  agent (agent1)
  jammer (jammer1)
  connector (connector1 connector2)
  gate (gate1 gate2)
  transmitter (transmitter1)
  receiver (receiver1 receiver2)
  location (compute (loop for i from 1 to 11
                          collect (intern (format nil "LOCATION~D" i))))
  wall-gears (wgears1 wgears2 wgears3 wgears4)
  floor-gears (fgears1)
  fan (fan1 fan2 fan3 fan4)
  wall (wall1 wall2 wall3 wall4 wall5)
  hue (red)
  mode (normal inverted)
)


;;;; TECHNOLOGY INCLUDES ;;;;


(include-tech floor-blower)
(include-tech wall-blower)
(include-tech jammer)
(include-tech gate)
(include-tech beam-relay)
(include-tech visibility)
(include-tech accessibility)


;;;; MASTER PROPAGATION DRIVER ;;;;


(define-update propagate-changes! ()
  (let ((*detect-propagated-changes* t))
    (ww-loop for $iteration from 1 to 5
             do (if (not (propagate-consequences!))
                  (return t))
             finally (inconsistent-state)
                     (return nil)))
)


(define-update propagate-consequences! ()
  (let ((*propagated-state-changed* nil))
    (update-connector-status!)
    (update-receiver-status!)
    (update-gate-status!)
    (update-gears-status!)  ;derives turning/blowing state
    (update-wall-blower-status!)  ;wall-mounting consequences: sweep the faced location
    (update-floor-blower-status!)
    *propagated-state-changed*)
)


;;;; INITIALIZATION ;;;;


(define-init
  ;; Movable objects
  (has-location agent1 location4)
  (has-location jammer1 location1)
  (has-location connector1 location6)
  (has-location connector2 location3)
  (has-location fan1 location2)
  (has-location fan2 location5)
  (has-location fan3 location7)
  (has-location fan4 location9)

  ;; Fixed-position objects
  (has-position fgears1 location10)
  (has-position wgears1 location2)
  (has-position wgears2 location5)
  (has-position wgears3 location7)
  (has-position wgears4 location9)

  ;; Object coordinates
  (location-position> location1 12 125/10)
  (location-position> location2 61/10 125/10)
  (location-position> location3 4 16)
  (location-position> location4 18 115/10)
  (location-position> location5 239/10 115/10)
  (location-position> location6 28 15)
  (location-position> location7 349/10 15)
  (location-position> location8 30 115/10)
  (location-position> location9 241/10 115/10)
  (location-position> location10 241/10 6)
  (location-position> location11 241/10 6)
  (has-elevation location11 10)  ;do I need to specify this since it is the default
  (transceiver-position> transmitter1 7 169/10)
  (transceiver-position> receiver1 11/10 9)
  (transceiver-position> receiver2 23 209/10)

  ;; Object properties
  (has-chroma transmitter1 red)
  (has-chroma receiver1 red)
  (has-chroma receiver2 red)
  (mounted-on fan1 wgears1)
  (mounted-on fan2 wgears2)
  (mounted-on fan3 wgears3)
  (mounted-on fan4 wgears4)
  (welded fan2 wgears2)
  (welded fan3 wgears3)

  ;; Air stream blowing destination
  (aimed-at> wgears1 location1)
  (aimed-at> wgears2 location4)
  (aimed-at> wgears3 location6)
  (aimed-at> wgears4 location8)
  (aimed-at> fgears1 location11)

  ;; Controllers
  (controls ((receiver1)) gate1 normal)
  (controls ((receiver2)) wgears2 inverted)
  (controls ((receiver2)) wgears3 normal)

  ;; Boundary wall
  (boundary-wall
    ((1 17) (8 17) (8 14) (11 14) (11 21) (18 21) (18 17) (22 17) (22 21) (28 21) (28 17) (35 17)
     (35 11) (33 11) (33 1)
     (22 1) (22 10) (18 10) (18 4) (12 4) (12 11) (9 11) (9 7) (1 7)
    ))

  ;; Segments
  (wall-segments
    ((wall1 6 11 6 17)
     (wall2 22 13 22 17)
     (wall3 24 13 30 13)
     (wall4 24 10 24 13)
     (wall5 26 11 33 11)
    ))

  (gate-segments
    ((gate1 1 12 6 12)
     (gate2 1 14 6 14)
    ))
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
  (and (exists (?f fan)
         (and (mounted-on ?f fgears1)
              (blowing ?f)))
       (has-location agent1 location10)
  ))

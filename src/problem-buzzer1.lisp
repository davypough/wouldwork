;;;; Filename: problem-buzzer1.lisp

;;;; Modeling a buzzer object in Talos Principle.
;;;; A buzzer moves along a path defined by define-patroller.
;;;; Enhanced for 3D environments with stacking support.
;;;;
;;;; Height Rules:
;;;;   - Ground level: height = 0
;;;;   - On a box at ground level: height = 1
;;;;   - On a box on another support: height = 2
;;;;
;;;; Force Field Rules:
;;;;   - Agent cannot jump directly onto buzzer (force field)
;;;;   - Agent must be at height >= 1 to place box on buzzer
;;;;   - Agent CAN jump onto a box that is on the buzzer


(in-package :ww)


(ww-set *problem-name* buzzer1)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* tree)

(ww-set *depth-cutoff* 10)

(ww-set *progress-reporting-interval* 10000)


(define-types
  agent           (agent1)
  buzzer          (buzzer1)
  box             (box1 box2)
  transmitter     (transmitter1 transmitter2)
  area            (area1 area2 area3 area4 area5)
  cargo           (either box)
  fixture         (either transmitter)
  support         (either box buzzer)
)


(define-dynamic-relations
  (loc (either agent buzzer cargo) $area)      ;always ground area
  (on1 (either $agent $cargo) box)              ;stacking on a box
  (on2 (either agent cargo) $box)
  (supports $buzzer box)                       ;inverse of on, box placed on buzzer (force field distinction)
  (height (either agent cargo) $fixnum)        ;0 = ground, 1 = on support, 2 = stacked
  (holds agent $cargo)
)


(define-static-relations
  (fix-coords (either area fixture) $fixnum $fixnum $fixnum)
  (accessible0 area area)
)


(define-patroller buzzer1
  path (area1 area2 area3 area4 area5)
  mode :reverse
  rebound (exists (?c cargo)
            (and (bind (loc buzzer1 $area))
                 (loc ?c $area)))
)


;;;; HELPER QUERIES ;;;;


(define-query cleartop (?box)
  ;; True if nothing is on top of this box
  (not (exists (?x (either agent cargo))
         (on ?x ?box))))


(define-query accessible (?agent ?area1 ?area2)
  ;; Ground-level horizontal accessibility
  (do ?agent  ;consume for signature consistency
      (accessible0 ?area1 ?area2)))


(define-query get-object-on-box (?box)
  ;; Returns the agent or cargo resting on this box, or nil if none
  (exists (?object (either agent cargo))
    (and (on ?object ?box)
         ?object)))


(define-query get-box-below (?box)
  ;; Returns the box that ?box is resting on, or nil if none
  (exists (?lower-box box)
    (and (on ?box ?lower-box)
         ?lower-box)))


;;;; PROPAGATION ;;;;


(define-update propagate-changes! ()
  ;; Iterate until no more changes (max 10 to prevent infinite loop)
  (ww-loop for $iteration from 1 to 10
           do (if (not (propagate-consequences!))
                (return))
           finally (inconsistent-state)))


(define-update propagate-consequences! ()
  ;; Returns t if any change occurred
  (or (update-buzzer-cargo!)
      (drop-unsupported!)))


(define-update update-buzzer-cargo! ()
  ;; When buzzer moves, update loc of boxes on it and anything on those boxes
  ;; Returns t if any change occurred
  (ww-loop with $changed = nil
           for ?box in (box)
           do (when (and (bind (supports buzzer1 ?box))
                         (bind (loc buzzer1 $bz-area))
                         (bind (loc ?box $box-area))
                         (different $bz-area $box-area))
                ;; Box on buzzer has stale loc - update it
                (dodb (loc ?box $bz-area))
                (setq $changed t)
                ;; Also update anything on this box
                (ww-loop for ?x in (either agent cargo)
                         do (when (and (bind (on ?x ?box))
                                       (bind (loc ?x $x-area))
                                       (different $x-area $bz-area))
                              (dodb (loc ?x $bz-area))
                              (setq $changed t))))
           finally (return $changed)))


(define-update drop-unsupported! ()
  ;; Drop cargo/agent to ground if their supporting box was picked up
  ;; Returns t if any change occurred
  (ww-loop with $changed = nil
           for ?x in (either agent cargo)
           do (when (and (bind (on ?x $box))
                         (not (bind (loc $box $any))))  ;box is held, has no loc
                ;; Support removed - drop to ground
                (dodb (not (on ?x $box))
                      (height ?x 0))
                (setq $changed t))
           finally (return $changed)))


(define-update collapse-cargo-above-box! (?box)
  ;; Collapses all the cargo items above a box by one level
  (if (bind (on $cargo ?box))
    (do (bind (height $cargo $h-cargo))
        (height $cargo (1- $h-cargo))
        (if (box $cargo)
          (collapse-cargo-above-box! $cargo)))))


;;;; ACTIONS - GROUND MOVEMENT ;;;;


(define-action move
    1
  (?agent agent ?area2 area)
  (and (bind (height ?agent $h))
       (= $h 0)  ;must be on ground
       (bind (loc ?agent $area1))
       (different $area1 ?area2)
       (accessible ?agent $area1 ?area2))
  (?agent $area1 ?area2)
  (assert (loc ?agent ?area2)
          (propagate-changes!)))


;;;; ACTIONS - JUMPING ;;;;


(define-action jump-onto-box
  ;; Agent can jump up or down to any reachable box
  1
  (?agent agent ?box box)
  (and (cleartop ?box)
       (bind (loc ?agent $area))
       (loc ?box $area)  ;agent and box must be in same area
       (bind (height ?agent $h-agent))
       (bind (height ?box $h-box))
       (setq $h-delta (- $h-box $h-agent))
       (< $h-delta 1))  ;agent can only jump up 1 level max
                        ;but can jump across or down to any lower level box
  (?agent ?box $h-box $area)
  (assert (if (bind (on ?agent $old-box))
            (not (on ?agent $old-box)))
          (on ?agent ?box)
          (height ?agent (1+ $h-box))
          (propagate-changes!)))


(define-action jump-to-ground
  ;; Agent can jump from any height > 0 to the ground
  1
  (?agent agent)
  (and (bind (height ?agent $h-agent))
       (> $h-agent 0)
       (bind (loc ?agent $area)))
  (?agent $area)
  (assert (height ?agent 0)
          (if (bind (on ?agent $box))
            (not (on ?agent $box)))
          (propagate-changes!)))


;;;; ACTIONS - BOX PICKUP/DROP ;;;;


(define-action pickup-box
  ;; Agent picks up a box; anything on box falls one level
    1
  (?agent agent ?box box)
  (and (not (bind (holds ?agent $any-cargo)))
       (not (on ?agent ?box))
       (bind (loc ?agent $area))
       (loc ?box $area)
       (bind (height ?agent $h-agent))
       (bind (height ?box $h-box))
       (<= (abs (- $h-box $h-agent)) 1))  ;within reach (+1,0,-1)
  (?agent ?box $area)
  (assert (holds ?agent ?box)
          (not (loc ?box $area))
          (cond ((bind (supports $buzzer ?box))
                   (not (supports $buzzer ?box))
                   (bind (on $cargo ?box))  ;can't do this binding
                   (if (box $cargo)  ;if cargo is not a box, then just drops to ground
                     (supports $buzzer $cargo))
                ((bind (on ?box $lower-box))
                   (not (on ?box $lower-box))))
          (collapse-cargo-above-box! ?box)  ;stack above box shifts down one level after box removed
          (propagate-changes!)))


(define-action drop-cargo-on-ground
  ;; Agent drops held cargo at current location to the ground
    1
  (?agent agent)
  (and (bind (holds ?agent $cargo))
       (bind (loc ?agent $area)))
  (?agent $cargo $area)
  (assert (not (holds ?agent $cargo))
          (loc $cargo $area)
          (height $cargo 0)
          (propagate-changes!)))


;;;; ACTIONS - PLACING CARGO ON SUPPORTS ;;;;


(define-action put-cargo-on-box
  ;; Agent can place a cargo object onto a reachable box
    1
  (?agent agent ?box box)
  ;; Agent places held cargo onto a box
  (and (cleartop ?box)
       (bind (holds ?agent $cargo))
       (bind (loc ?agent $area))
       (loc ?box $area)
       (bind (height ?agent $h-agent))
       (bind (height ?box $h-box))
       (setq $h-delta (- $h-box $h-agent))
       (< (- $h-box $h-agent) 1))  ;within reach +1 up or any level down
  (?agent $cargo ?box $area)
  (assert (not (holds ?agent $cargo))
          (loc $cargo $area)
          (on $cargo ?box)
          (height $cargo (1+ $h-box))
          (propagate-changes!)))


(define-action put-box-on-buzzer
  ;; Agent places held box onto buzzer (requires height >= 1, force field)
    1
  (?agent agent ?buzzer buzzer)
  (and (bind (holds ?agent $box))
       (bind (height ?agent $h-agent))
       (>= $h-agent 1)  ;must be elevated
       (bind (loc ?agent $area))
       (loc ?buzzer $area))  ;buzzer is always at ground level
  (?agent $box ?buzzer $area)
  (assert (not (holds ?agent $box))
          (loc $box $area)
          (supports ?buzzer $box)
          (height $box 1)  ;on buzzer = height 1
          (propagate-changes!)))


;;;; INITIALIZATION ;;;;


(define-init
  ;; Agent state
  (loc agent1 area3)
  (height agent1 0)
  
  ;; Buzzer state
  (loc buzzer1 area1)
  
  ;; Box states
  (loc box1 area3)
  (height box1 0)
  (loc box2 area3)
  (on box2 box1)      ;box2 is stacked on box1
  (height box2 1)

  ;; Static spatial configuration
  (fix-coords area1 20 12 0)
  (fix-coords area2 14 12 0)
  (fix-coords area3 10 12 0)
  (fix-coords area4 7 12 0)
  (fix-coords area5 2 12 0)
  (fix-coords transmitter1 7 0 1)
  (fix-coords transmitter2 15 0 1)

  ;; Ground-level accessibility
  (accessible0 area1 area2)
  (accessible0 area2 area3)
  (accessible0 area3 area4)
  (accessible0 area4 area5)
)


;;;; GOAL ;;;;


(define-goal
  (loc buzzer1 area5)
)
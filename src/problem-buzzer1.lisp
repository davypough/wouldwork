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

(ww-set *depth-cutoff* 15)

(ww-set *progress-reporting-interval* 100000)


(define-types
  agent           (agent1)
  buzzer          (buzzer1)
  mine            ()
  box             (box1 box2)
  transmitter     (transmitter1 transmitter2)
  area            (area1 area2 area3 area4 area5)
  cargo           (either box)
  fixture         (either transmitter)
  support         (either box buzzer)
)


(define-dynamic-relations
  (loc (either agent buzzer mine cargo) $area)  ;always ground area
  (on (either $agent $cargo) $box :bijective)  ;stacking on a box, bijective allows binding either argument
  (supports (either $buzzer $mine) $box :bijective)  ;inverse of on, only box object can placed on buzzer
  (height (either agent cargo) $fixnum)  ;0 = on ground, 1 = on support, 2+ = stacked
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
  kill (and (bind (loc buzzer1 $area))  ;prune (disallow) state if true
            (loc agent1 $area))
)


;;;; HELPER QUERIES ;;;;


(define-query cleartop (?bearer)
  ;; True if nothing is on top of a box, buzzer, or mine.
  (cond ((box ?bearer) (not (bind (on $anything ?bearer))))  ;anything = agent or cargo
        ((buzzer ?bearer) (not (bind (supports ?bearer $any-box))))
        ((mine ?bearer) (not (bind (supports ?bearer $any-box))))))


(define-query accessible (?agent ?area1 ?area2)
  ;; Ground-level horizontal accessibility
  (do ?agent  ;consume for signature consistency
      (accessible0 ?area1 ?area2)))


(define-query safe (?area)
  (not (exists (?threat (either buzzer mine))
         (loc ?threat ?area))))


;;;; PROPAGATION ;;;;


(define-update propagate-changes! ()
  ;; Iterate until no more changes (max 10 to prevent infinite loop)
  (ww-loop for $iteration from 1 to 10
           do (if (not (propagate-consequences!))
                (return))
           finally (inconsistent-state)))


(define-update propagate-consequences! ()
  ;; All functions should execute in order and return t if any change occurred.
  (some #'identity
        (mapcar (lambda (fn) (funcall fn state))
                (list #'relocate-objects-above-rovers!))))


(define-update relocate-stacked-object! (?object ?area)
  (do (loc ?object ?area)
      (if (bind (on $higher-object ?object))
        (relocate-stacked-object! $higher-object ?area))))
  

(define-update relocate-objects-above-rovers! ()
  ;; When a buzzer or mine moves, we need to relocate any objects stacked on it.
  (doall (?rover (either buzzer mine))
    (if (bind (supports ?rover $box))
      (do (bind (loc ?rover $buzzer-area))
          (relocate-stacked-object! $box $buzzer-area)))))


(define-update collapse-cargo-above-box! (?box)
  ;; Collapses all the cargo items above a box by one level
  (if (bind (on $cargo ?box))
    (do (bind (height $cargo $h-cargo))
        (height $cargo (1- $h-cargo))
        (if (box $cargo)
          (collapse-cargo-above-box! $cargo)))))


;;;; ACTIONS ;;;;


(define-action put-cargo-on-place
  ;; Agent can place a cargo object on a box, buzzer, mine or the ground.
  1
  (?agent agent)
  (and (bind (holds ?agent $cargo))
       (bind (loc ?agent $area)))
  (?agent $cargo $place $area)
  (do  ;; Can put cargo on a box
      (doall (?box box)
        (if (and (loc ?box $area)
                 (cleartop ?box)
                 (bind (height ?box $h-box))
                 (bind (height ?agent $h-agent))
                 (setq $h-delta (- $h-box $h-agent))
                 (< $h-delta 1))  ;within reach +1 up or any level down
          (assert (not (holds ?agent $cargo))
                  (loc $cargo $area)
                  (on $cargo ?box)
                  (height $cargo (1+ $h-box))
                  (setq $place ?box))))
      ;; an put cargo = box on a buzzer or mine
      (doall (?rover (either buzzer mine))
        (if (and (box $cargo)
                 (loc ?rover $area)
                 (cleartop ?rover)
                 (bind (height ?agent $h-agent))
                 (>= $h-agent 1))  ;must be above buzzer
          (assert (not (holds ?agent $cargo))
                  (loc $cargo $area)
                  (supports ?rover $cargo)
                  (height $cargo 1))  ;box on buzzer = height 1
                  (setq $place ?rover)))
      ;; Can put cargo on the ground
      (assert (not (holds ?agent $cargo))
              (loc $cargo $area)
              (height $cargo 0)
              (setq $place 'ground))
      (finally (propagate-changes!))))


(define-action pickup-cargo
  ;; Agent picks up cargo 
    1
  (?agent agent)
  (and (not (bind (holds ?agent $any-cargo)))
       (bind (loc ?agent $area))
       (bind (height ?agent $h-agent)))
  (?agent $cargo $area)
  (do ;; if cargo is a box, anything on box falls one level
      (doall (?box box)
        (if (and (not (on ?agent ?box))
                 (loc ?box $area)
                 (bind (height ?box $h-box))
                 (<= (abs (- $h-box $h-agent)) 1))  ;within reach (+1,0,-1)
          (assert (setq $cargo ?box)
                  (holds ?agent ?box)
                  (not (loc ?box $area))
                  ;; First update heights while on relationships still exist
                  (collapse-cargo-above-box! ?box)
                  ;; Now handle removal of relationships and establishing new ones
                  (cond ((bind (supports $rover ?box))
                           ;; Box was on buzzer or mine - remove supports, transfer cargo to buzzer
                           (not (supports $rover ?box))
                           (if (bind (on $cargo ?box))
                             (do (not (on $cargo ?box))
                                 (if (box $cargo)
                                   (supports $rover $cargo)
                                   (height $cargo 0)))))  ;any cargo not a box falls to ground
                        ((bind (on ?box $under-box))
                           ;; Box was on another box - remove on, transfer cargo to under-box
                           (not (on ?box $under-box))
                           (if (bind (on $cargo ?box))
                             (do (not (on $cargo ?box))
                                 (on $cargo $under-box))))
                        (t
                           ;; Box was on ground - just remove on relationship for cargo
                           (if (bind (on $cargo ?box))
                             (not (on $cargo ?box))))))))
      (finally (propagate-changes!))))


(define-action jump-to-place
  ;; Agent can jump up or down to any reachable box or the ground.
  1
  (?agent agent)
  (and (bind (loc ?agent $area))
       (bind (height ?agent $h-agent)))
  (?agent $place $area)
  (do ;; Can jump to a reachable box
      (doall (?box box)
        (if (and (cleartop ?box)
                 (loc ?box $area)  ;agent and box must be in same area
                 (bind (height ?agent $h-agent))
                 (bind (height ?box $h-box))
                 (setq $h-delta (- $h-box $h-agent))
                 (< $h-delta 1))  ;agent can only reach box at same level or below;
          (assert (if (bind (on ?agent $old-box))
                    (not (on ?agent $old-box)))
                  (on ?agent ?box)
                  (height ?agent (1+ $h-box))  ;standing on a box sets agent height to box_height + 1
                  (setq $place ?box))))
      ;; Can jump to the ground
      (if (> $h-agent 0)
        (assert (height ?agent 0)
                (if (bind (on ?agent $box))
                  (not (on ?agent $box)))
                (setq $place 'ground)))
      (finally (propagate-changes!))))



(define-action move
    1
  (?agent agent)
  (and (bind (height ?agent $h-agent))
       (= $h-agent 0))  ;must be on ground
  (?agent $area1 ?area2)
  (do (doall (?area2 area)
        (if (and (bind (loc ?agent $area1))
                 (different ?area2 $area1)
                 (accessible ?agent $area1 ?area2)
                 (safe ?area2))
          (assert (loc ?agent ?area2))))
      (finally (propagate-changes!))))


(define-action wait
    0  ;always 0, wait for next exogenous event
  (?agent agent ?area area)
  (loc ?agent ?area)
  ()
  (assert (waiting)))


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

#|
Notes:
 


|#
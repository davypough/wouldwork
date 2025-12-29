;;;; Filename: problem-buzzer1.lisp

;;;; Modeling a buzzer object in Talos Principle.
;;;; A buzzer is an object that moves along a path defined by define-buzzer.


(in-package :ww)


(ww-set *problem-name* buzzer1)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* tree)

(ww-set *depth-cutoff* 6)

(ww-set *progress-reporting-interval* 10000)


(define-types
  agent           (agent1)
  buzzer          (buzzer1)
  box             (box1)
  area            (area1 area2 area3 area4 area5)
  cargo           (either box)
)


(define-dynamic-relations
  (loc (either agent buzzer cargo) $area)
)


(define-static-relations
  (coords area $fixnum $fixnum)
  (accessible0 area area)
)


(define-patroller buzzer1
  path (area1 area2 area3 area4 area5)
  mode :reverse
  rebound (exists (?c cargo)
            (and (bind (loc buzzer1 $area))
                 (loc ?c $area)))
)


(define-query accessible (?agent ?area1 ?area2)
  (do ?agent
      (or (accessible0 ?area1 ?area2))))


(define-update propagate-changes! ()
  (ww-loop for $iteration from 1 to 10
           do (if (not (propagate-consequences!))
                (return))  ;convergence achieved
           finally (inconsistent-state)))  ;no convergence, mark state inconsistent for pruning


(define-update propagate-consequences! ()
  ;; All functions must execute in order; returns t if any change occurred
  nil)


(define-action move
    1
  (?agent agent ?area2 area)
  (and (bind (loc ?agent $area1))
       (different $area1 ?area2)
       (accessible ?agent $area1 ?area2))
  (?agent $area1 ?area2)
  (assert (loc ?agent ?area2)
          (propagate-changes!)))


(define-init
  ;; Dynamic state
  (loc agent1 area3)
  (loc buzzer1 area1)
  (loc box1 area4)
  ;; Static spatial configuration
  (coords area1 20 12)
  (coords area2 14 12)
  (coords area3 10 12)
  (coords area4 7 12)
  (coords area5 2 12)
  (accessible0 area1 area2)  ;chain moves between areas to lower search branching
  (accessible0 area2 area3)
  (accessible0 area3 area4)
  (accessible0 area4 area5)
)


(define-goal
  (loc buzzer1 area5))
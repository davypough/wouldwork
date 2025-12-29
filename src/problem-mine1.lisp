;;; Filename: problem-mine1.lisp


;;; Problem specification for getting by an automated 
;;; sentry mine by jamming it.


(in-package :ww)  ;required

(ww-set *problem-name* mine1)

(ww-set *problem-type* planning)

(ww-set *tree-or-graph* tree)

(ww-set *solution-type* first)

(ww-set *depth-cutoff* 15)


(define-types
  agent     (agent1)
  box       (box1)
  jammer    (jammer1)
  gun       (gun1)
  mine      (mine1)  
  switch    (switch1)
  area      (area1 area2 area3 area4 area5 area6 area7 area8)
  cargo     (either jammer box)
  threat    (either gun mine)
  target    (either threat))


(define-dynamic-relations
  (holding agent cargo)
  (loc (either agent cargo threat target switch) $area)
  (red switch)
  (green switch)
  (jamming jammer target))


(define-static-relations
  (adjacent area area)
  (los area target)    ;line-of-sight exists
  (visible area area)  ;area is visible from another area
  (controls switch gun)
  (watches gun area))


(define-query free? (?agent) 
  (not (exists (?c cargo) 
         (holding ?agent ?c))))

  
(define-query passable? (?area1 ?area2)
  (adjacent ?area1 ?area2))


(define-query active? (?threat)
  (not (or (exists (?j jammer)
             (jamming ?j ?threat))
           (forall (?s switch)
             (and (controls ?s ?threat)
                  (green ?s))))))


(define-query safe? (?area)
  (and (not (exists (?g gun)
              (and (watches ?g ?area)
                   (active? ?g))))
       (not (exists (?m mine)
              (and (loc ?m ?area)
                   (active? ?m))))))


(define-patroller mine1
  path (area5 area6 area7)
  mode :reverse
  rebound (exists (?c cargo)
            (and (bind (loc mine1 $area))
                 (loc ?c $area)))
  interrupt (exists (?j jammer)
               (jamming ?j mine1))
  kill (and (not (exists (?j jammer)
                   (jamming ?j mine1)))
            (bind (loc mine1 $area))
            (loc agent1 $area)))


(define-action jam
    1
  (?target target ?area2 area ?jammer jammer ?area1 area)
  (and (holding agent1 ?jammer)
       (loc agent1 ?area1)
       (loc ?target ?area2)
       (visible ?area1 ?area2))
  (?target ?jammer ?area1)
  (assert (not (holding agent1 ?jammer))
          (loc ?jammer ?area1)
          (jamming ?jammer ?target)))


(define-action throw
    1
  (?switch switch ?area area)
  (and (free? agent1)
       (loc agent1 ?area)
       (loc ?switch ?area))
  (?switch)
  (assert (if (red ?switch)
            (do (not (red ?switch))
                (green ?switch))
            (do (not (green ?switch))
                (red ?switch)))))


(define-action pickup
    1
  (?cargo cargo ?area area)
  (and (loc agent1 ?area)
       (loc ?cargo ?area)
       (free? agent1))
  (?cargo ?area)
  (assert (not (loc ?cargo ?area))
          (holding agent1 ?cargo)
          (exists (?t target)
            (if (and (jammer ?cargo)
                     (jamming ?cargo ?t))
              (not (jamming ?cargo ?t))))))


(define-action drop
    1
  (?cargo cargo ?area area)
  (and (loc agent1 ?area)
       (holding agent1 ?cargo))
  (?cargo ?area)
  (assert (not (holding agent1 ?cargo))
          (loc ?cargo ?area)))
       

(define-action move
    1
  ((?area1 ?area2) area)
  (and (loc agent1 ?area1)
       (passable? ?area1 ?area2)
       (safe? ?area2))
  (?area1 ?area2)
  (assert (not (loc agent1 ?area1))
          (loc agent1 ?area2)))


(define-action wait
    0  ;always 0, wait for next exogenous event
  (?area area)
  (loc agent1 ?area)
  ()
  (assert (waiting)))


(define-init
  ;dynamic
  (loc agent1 area1)
  (loc jammer1 area1)
  (loc gun1 area2)
  (loc switch1 area3)
  (loc box1 area4)
  (red switch1)
  (loc mine1 area6)  ;area5)
  ;static
  (always-true)
  (watches gun1 area2)
  (controls switch1 gun1)
  (los area1 gun1)
  (los area2 gun1)
  (los area3 gun1)
  (los area4 gun1)
  (visible area5 area6)
  (visible area5 area7)
  (visible area5 area8)
  (visible area6 area7)
  (visible area6 area8)
  (visible area7 area8)
  (adjacent area1 area2)
  (adjacent area2 area3)
  (adjacent area2 area4)
  (adjacent area4 area5)
  (adjacent area5 area6)
  (adjacent area6 area7)
  (adjacent area7 area8))


(define-init-action derived-visibility
    0
    ((?area1 ?area2) area)
    (adjacent ?area1 ?area2)
    ()
    (assert (visible ?area1 ?area2)))


(define-goal
  (loc agent1 area8))

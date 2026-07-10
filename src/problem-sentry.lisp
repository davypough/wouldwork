;;; Filename: problem-sentry.lisp


;;; Problem specification for getting by an automated 
;;; sentry by jamming it.


(in-package :ww)  ;required

(ww-set *problem-name* sentry)

(ww-set *problem-type* planning)

(ww-set *tree-or-graph* tree)

(ww-set *depth-cutoff* 16)


(define-types
  myself    (me)
  box       (box1)
  jammer    (jammer1)
  gun       (gun1)
  sentry    (sentry1)  
  switch    (switch1)
  area      (area1 area2 area3 area4 area5 area6 area7
             area8)
  cargo     (either jammer box)
  threat    (either gun sentry)
  target    (either threat))


(define-dynamic-relations
  (holding myself cargo)
  (loc (either myself cargo target switch) $area)
  (red switch)
  (green switch)
  (jamming jammer target))


(define-static-relations
  (adjacent area area)
  (visible area area)  ;area is visible from another area
  (controls switch gun)
  (watches gun area))


(define-query free? (?myself) 
  (not (exists (?c cargo) 
               (holding ?myself ?c))))

  
(define-query passable? (?area1 ?area2)
  (adjacent ?area1 ?area2))


(define-query active? (?threat)
  (not (or (exists (?j jammer)
             (jamming ?j ?threat))
           (forall (?s switch)
             (and (controls ?s ?threat)
                  (green ?s))))))

(define-query safe? (?area)
  (not (exists (?g gun)
         (and (watches ?g ?area)
              (active? ?g)))))


(define-happening sentry1
  :inits ((loc sentry1 area6))  ;what's true at t=0
  :events  ;events happening at t>0
  ((1 (loc sentry1 area7))
   (2 (loc sentry1 area6))
   (3 (loc sentry1 area5))
   (4 (loc sentry1 area6)))
  :repeat t
  :interrupt (exists (?j jammer)
               (jamming ?j sentry1)))


(define-constraint
  ;Constraints only needed for happening events that can 
  ;kill or delay an action. Global constraints included 
  ;here. Return t if constraint satisfied, nil if 
  ;violated.
  (not (exists (?s sentry ?a area)
         (and (loc me ?a)
              (loc ?s ?a)
              (active? ?s)))))


(define-action jam
    1
  (?target target ?area2 area ?jammer jammer ?area1 area)
  (and (holding me ?jammer)
       (loc me ?area1)
       (loc ?target ?area2)
       (visible ?area1 ?area2))
  (?target ?jammer ?area1)
  (assert (not (holding me ?jammer))
          (loc ?jammer ?area1)
          (jamming ?jammer ?target)))







(define-action throw
    1
  (?switch switch ?area area)
  (and (free? me)
       (loc me ?area)
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
  (and (loc me ?area)
       (loc ?cargo ?area)
       (free? me))
  (?cargo ?area)
  (assert (not (loc ?cargo ?area))
          (holding me ?cargo)
          (exists (?t target)
            (if (and (jammer ?cargo)
                     (jamming ?cargo ?t))
              (not (jamming ?cargo ?t))))))


(define-action drop
    1
  (?cargo cargo ?area area)
  (and (loc me ?area)
       (holding me ?cargo))
  (?cargo ?area)
  (assert (not (holding me ?cargo))
          (loc ?cargo ?area)))
       









(define-action move
    1
  ((?area1 ?area2) area)
  (and (loc me ?area1)
       (passable? ?area1 ?area2)
       (safe? ?area2))
  (?area1 ?area2)
  (assert (loc me ?area2)))


(define-action wait
    0  ;always 0, wait for next exogenous event
  (?area area)
  (loc me ?area)
  ()
  (assert (waiting)))


(define-init
  ;dynamic
  (loc me area1)
  (loc jammer1 area1)
  (loc gun1 area2)
  (loc switch1 area3)
  (loc box1 area4)
  (red switch1)
  ;static
  (always-true)
  (watches gun1 area2)
  (controls switch1 gun1)
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
  (loc me area8))

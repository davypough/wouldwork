;;; Filename: problem-sentry.lisp


;;; Problem specification for getting by an automated 
;;; sentry by jamming it.


(in-package :ww)  ;required

(ww-set *problem-name* sentry)

(ww-set *problem-type* planning)

(ww-set *tree-or-graph* tree)

(ww-set *solution-type* min-length)

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
  threat    (either gun sentry))


(define-dynamic-relations
  (holding myself cargo)
  (loc (either myself cargo threat switch) $area)
  (red switch)
  (green switch)
  (jamming jammer threat))


(define-static-relations
  (adjacent area area)
  (visible area area)  ;area is visible from another area
  (controls switch gun)
  (watches gun area))


(define-query free? (?myself) 
  (not (exists (?c cargo) 
               (holding ?myself ?c))))

  
(define-query active? (?threat)
  (not (or (exists (?j jammer)
             (jamming ?j ?threat))
           (exists (?s switch)
             (and (controls ?s ?threat)
                  (green ?s))))))

(define-query safe? (?area)
  (not (exists (?g gun)
         (and (watches ?g ?area)
              (active? ?g)))))


(define-query min-steps-remaining? ()
  ;; Lower bound on the moves still needed: the adjacency distance from me to area8,
  ;; plus a jam of the sentry unless it is jammed or I am in area7 or area8,
  ;; plus a pickup of the jammer if that jam is needed and I am not holding it.
  ;; Sound: each move covers one adjacency, and the active sentry cannot be passed
  ;; (sharing or swapping areas is forbidden).  The agent can never be beyond an active
  ;; sentry: the jammer stays behind the sentry it jams, and picking it up puts the
  ;; agent behind the sentry again.  Distances are specific to this map.
  (do (bind (loc me $area))
      (+ (case $area
           (area8 0) (area7 1) (area6 2) (area5 3)
           (area4 4) (area2 5) (area1 6) (area3 6))
         (if (or (member $area '(area7 area8))
                 (exists (?j jammer) (jamming ?j sentry1)))
           0
           (if (exists (?j jammer) (holding me ?j))
             1
             2)))))


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
  (?target threat ?area2 area ?jammer jammer ?area1 area)
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
          (doall (?t threat)
            (not (jamming ?cargo ?t)))))


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
       (adjacent ?area1 ?area2)
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

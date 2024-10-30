;;;; Filename: problem-smallspace-macro.lisp


;;; Problem specification (in Talos Principle)
;;; for the small space problem in Road to Gehenna sigil 
;;; dome. First leg to area8.
;;; Double-move macro action added to problem-smallspace.lisp


(in-package :ww)  ;required

(ww-set *problem-name* smallspace-macro)

(ww-set *depth-cutoff* 19)

(ww-set *solution-type* min-length)


(define-types
  myself      (me)
  gate        (gate1 gate2)
  barrier     (nil)
  jammer      (nil)
  connector   (connector1 connector2)
  box         (nil)
  fan         (nil)
  gears       (nil)
  ladder      (nil)
  rostrum     (nil)
  hue         (blue red)
  transmitter (transmitter1 transmitter2)
  receiver    (receiver1 receiver2)
  area        (area1 area2 area3 area4 area5 area6 area7
               area8)
  cargo       (either connector)
  target      (either gate gears)
  divider     (either gate barrier)
  terminus    (either transmitter receiver connector)
  fixture     (either transmitter receiver)
  station     (either fixture gate)
  support     (either box rostrum))


(define-dynamic-relations
  (holding myself $cargo)
  (free myself)
  (loc (either myself cargo) $area)
  (on (either myself cargo) $support)
  (attached fan gears)
  (jamming jammer $target)
  (connecting terminus terminus)
  (active (either connector receiver gate))
  (color terminus $hue))


(define-static-relations
  ;agent can always move unimpeded between adjacent areas
  (adjacent area area)  
  (locale fixture area)
  (separates divider area area)
  (climbable> ladder area area)
  (height support $real)
  (controls receiver $gate)
  ;clear los from an area to a gate/fixture
  (los0 area (either gate fixture))  
  (los1 area divider (either gate fixture))
  (los2 area divider divider (either gate fixture))
  ;could see a mobile object in an area from a given area
  (visible0 area area)  
  (visible1 area divider area)
  (visible2 area divider divider area))


(define-complementary-relations  
  (holding myself $cargo) -> (not (free myself)))


;;;; QUERY FUNCTIONS ;;;;


(define-query same-color? (?terminus1 ?terminus2)
  (and (bind (color ?terminus1 $hue1))
       (bind (color ?terminus2 $hue2))
       (eql $hue1 $hue2)))


(define-query source? (?terminus)
  (or (transmitter ?terminus)
      (and (connector ?terminus)
           (active ?terminus))))
















(define-query los-thru-2-dividers? (?area ?station)
  (exists ((?d1 ?d2) divider)
    (and (los2 ?area ?d1 ?d2 ?station)
         (or (and (barrier ?d1)
                  (barrier ?d2))
             (and (barrier ?d1)
                  (gate ?d2)
                  (not (active ?d2)))
             (and (barrier ?d2)
                  (gate ?d1)
                  (not (active ?d1)))
             (and (gate ?d1)
                  (not (active ?d1))
                  (gate ?d2)
                  (not (active ?d2)))))))


(define-query los-thru-1-divider? (?area ?station)
  (exists (?d divider)
    (and (los1 ?area ?d ?station)
         (or (barrier ?d)
             (and (gate ?d)
                  (not (active ?d)))))))


(define-query los? (?area ?station)
  (or (los0 ?area ?station)
      (los-thru-1-divider? ?area ?station)
      (los-thru-2-dividers? ?area ?station)))


(define-query visible-thru-2-dividers? (?area1 ?area2)
  (exists ((?d1 ?d2) divider)
    (and (visible2 ?area1 ?d1 ?d2 ?area2)
         (or (and (barrier ?d1)
                  (barrier ?d2))
             (and (barrier ?d1)
                  (gate ?d2)
                  (not (active ?d2)))
             (and (barrier ?d2)
                  (gate ?d1)
                  (not (active ?d1)))
             (and (gate ?d1)
                  (not (active ?d1))
                  (gate ?d2)
                  (not (active ?d2)))))))

(define-query visible-thru-1-divider? (?area1 ?area2)
  (exists (?d divider)
    (and (visible1 ?area1 ?d ?area2)
         (or (barrier ?d)
             (and (gate ?d)
                  (not (active ?d)))))))


(define-query visible? (?area1 ?area2)
  (or (visible0 ?area1 ?area2)
      (visible-thru-1-divider? ?area1 ?area2)
      (visible-thru-2-dividers? ?area1 ?area2)))


(define-query connectable? (?area ?terminus)
  (or (los? ?area ?terminus)  ;from connector in area to terminus
      (and (connector ?terminus)
           (exists (?a area)
             (and (loc ?terminus ?a)
                  (visible? ?area ?a))))))


(define-query passable? (?area1 ?area2)
  (or (adjacent ?area1 ?area2)
      (exists (?b (either barrier ladder))
        (and (separates ?b ?area1 ?area2)
             (free me)))  ;must drop cargo first
      (exists (?g gate)
        (and (separates ?g ?area1 ?area2)
             (not (active ?g))))))


(define-query sourced? (?conn-or-rcvr $hue $visits)
  (do (push ?conn-or-rcvr $visits)
      (or (exists (?t transmitter)
            (and (connecting ?t ?conn-or-rcvr)
                 (bind (color ?t $hue1))
                 (eql $hue1 $hue)))
          (exists (?c connector)
            (and (connecting ?c ?conn-or-rcvr)  
                 (active ?c)
                 (bind (color ?c $hue1))
                 (eql $hue1 $hue)
                 (not (member ?c $visits))
                 (sourced? ?c $hue $visits))))))

;;;; UPDATE FUNCTIONS ;;;;


(define-update activate-connector! (?connector ?hue)
  (if (not (active ?connector))
    (commit (active ?connector)
            (color ?connector ?hue))))


(define-update deactivate-connector! (?connector)
  (if (and (active ?connector)
           (bind (color ?connector $hue)))
    (commit (not (active ?connector))
            (not (color ?connector $hue)))))


(define-update activate-receiver! (?receiver)
  (if (not (active ?receiver))
    (do (assert (active ?receiver))
        (doall (?g gate)  
          (if (and (controls ?receiver ?g) (active ?g))
            (assert (not (active ?g))))))))


(define-update deactivate-receiver! (?receiver)
  (if (active ?receiver)
    (do (assert (not (active ?receiver)))
        (doall (?g gate)
          (if (controls ?receiver ?g)
            (assert (active ?g)))))))


(define-update disconnect-connector! (?connector)
  (doall (?t terminus)
    (if (connecting ?connector ?t)
      (commit (not (connecting ?connector ?t))))))


(define-update disengage-jammer! (?jammer ?target)
  (assert (not (jamming ?jammer ?target))
          (if (not (exists (?j jammer)
                     (and (different ?j ?jammer)
                          (jamming ?j ?target))))
            (assert (active ?target)))))



(define-update chain-activate! (?connector)
  (if (and (active ?connector)
           (bind (color ?connector $hue)))
    (doall (?cr (either connector receiver))
      (if (connecting ?connector ?cr)
        (if (connector ?cr)
          (if (not (active ?cr))
            (do (activate-connector! ?cr $hue)
                (chain-activate! ?cr)))
          (if (receiver ?cr)
            (if (and (not (active ?cr))
                     (same-color? ?cr ?connector))
              (activate-receiver! ?cr))))))))


(define-update activate-connector-if! (?connector)
  (if (exists (?t transmitter)
        (and (connecting ?t ?connector)
             (bind (color ?t $hue))))
    (if (not (exists ((?t1 ?t2) transmitter)
               (and (connecting ?t1 ?connector)
                    (connecting ?t2 ?connector)
                    (bind (color ?t1 $hue1))
                    (bind (color ?t2 $hue2))
                    (not (eql $hue1 $hue2)))))
      (activate-connector! ?connector $hue))
    (if (exists (?c connector)
          (and (connecting ?c ?connector)
               (active ?c)
               (bind (color ?c $hue))))
      (if (not (exists ((?c1 ?c2) connector)
                 (and (connecting ?c1 ?connector)
                      (connecting ?c2 ?connector)
                      (active ?c1)
                      (active ?c2)
                      (bind (color ?c1 $hue1))
                      (bind (color ?c2 $hue2))
                      (not (eql $hue1 $hue2)))))
        (activate-connector! ?connector $hue)))))








(define-update deactivate-any-orphans! ()
  (do (doall (?c connector)
        (if (and (active ?c)
                 (bind (color ?c $hue))
                 (not (sourced? ?c $hue nil)))
          (deactivate-connector! ?c)))
      (doall (?r receiver)
        (if (and (active ?r)
                 (bind (color ?r $hue))
                 (not (sourced? ?r $hue nil)))
          (deactivate-receiver! ?r)))))


;;;; ACTIONS ;;;;


(define-action connect-to-1-terminus
    2
  (?terminus terminus)
  (and (bind (holding me $cargo))
       (connector $cargo)
       (bind (loc me $area))
       (connectable? $area ?terminus))
  ($cargo fluent ?terminus terminus ($area $hue) fluent)
  (do (assert (not (holding me $cargo))
              (loc $cargo $area)
              (connecting $cargo ?terminus))
      (if (and (source? ?terminus)
               (bind (color ?terminus $hue)))
        (assert (active $cargo)
                (color $cargo $hue)))))
















(define-action connect-to-2-terminus
    3
  (combinations (?terminus1 ?terminus2) terminus)
  (and (bind (holding me $cargo))
       (connector $cargo)
       (bind (loc me $area))
       (connectable? $area ?terminus1)
       (connectable? $area ?terminus2))
  ($cargo fluent (?terminus1 ?terminus2) terminus
   $area fluent)
  (do (assert (not (holding me $cargo))
              (loc $cargo $area)
              (connecting $cargo ?terminus1)
              (connecting $cargo ?terminus2))
      (next (activate-connector-if! $cargo))
      (finally (chain-activate! $cargo))))


(define-action connect-to-3-terminus
    4
  (combinations (?terminus1 ?terminus2 ?terminus3) terminus)
  (and (bind (holding me $cargo))
       (connector $cargo)
       (bind (loc me $area))
       (connectable? $area ?terminus1)
       (connectable? $area ?terminus2)
       (connectable? $area ?terminus3))
  ($cargo fluent
   (?terminus1 ?terminus2 ?terminus3) terminus
   $area fluent)
  (do (assert (not (holding me $cargo))
              (loc $cargo $area)
              (connecting $cargo ?terminus1)
              (connecting $cargo ?terminus2)
              (connecting $cargo ?terminus3))
      (next (activate-connector-if! $cargo))
      (finally (chain-activate! $cargo))))










(define-action jam
    2
  (?target target)
  (and (bind (holding me $cargo))
       (jammer $cargo)
       (bind (loc me $area))
       (los? $area ?target))
  (?target target $cargo fluent $area fluent)
  (assert (not (holding me $cargo))
          (loc $cargo $area)
          (jamming $cargo ?target)
          (not (active ?target))))


(define-action pickup-jammer
    1
  (?jammer jammer)
  (and (free me)
       (bind (loc me $area))
       (loc ?jammer $area))
  (?jammer jammer ($area $target) fluent)
  (do (assert (holding me ?jammer)
              (not (loc ?jammer $area)))
      (if (bind (jamming ?jammer $target))
        (disengage-jammer! ?jammer $target))))


(define-action pickup-connector
    1
  (?connector connector)
  (and (free me)
       (bind (loc me $area))
       (loc ?connector $area))
  (?connector connector $area fluent)
  (do (assert (holding me ?connector)
              (not (loc ?connector $area)))
      (next (deactivate-connector! ?connector))
      (next (disconnect-connector! ?connector))
      (finally (deactivate-any-orphans!))))








(define-action drop-cargo
    1
  ()
  (and (bind (loc me $area))
       (bind (holding me $cargo)))
  ($cargo fluent $area fluent)
  (assert (not (holding me $cargo))
          (loc $cargo $area)))


(define-action move
    1
  (?area2 area)
  (and (bind (loc me $area1))
       (different $area1 ?area2)
       (passable? $area1 ?area2))
  ($area1 fluent ?area2 area)
  (assert (loc me ?area2)))


(define-action double-move
    2
  ((?area2 ?area3) area)
  (and (bind (loc me $area1))
       (different $area1 ?area2)
       (different $area1 ?area3)
       (passable? $area1 ?area2)
       (passable? ?area2 ?area3))
  ($area1 fluent (?area2 ?area3) area)
  (assert (loc me ?area3)))


;;;; INITIALIZATION ;;;;


(define-init
  ;dynamic
  (loc me area5)
  (loc connector1 area5)
  (loc connector2 area7)
  (free me)
  (active gate1)
  (active gate2)
  ;static
  (adjacent area1 area2)
  (adjacent area2 area3)
  (adjacent area3 area4)
  (adjacent area4 area5)
  (adjacent area6 area7)
  (locale transmitter1 area4)
  (locale transmitter2 area7)
  (locale receiver1 area4)
  (locale receiver2 area8)
  (color transmitter1 blue)
  (color transmitter2 red)
  (color receiver1 blue)
  (color receiver2 red)
  (controls receiver1 gate1)
  (controls receiver2 gate2)
  (separates gate1 area4 area7)
  (separates gate2 area7 area8)

  ;los is from an area to a fixed station
  (los0 area2 transmitter1)
  (los0 area3 transmitter1)
  (los0 area3 receiver1)
  (los0 area5 transmitter1)
  (los0 area5 receiver1)
  (los0 area5 receiver2)
  (los0 area6 transmitter1)
  (los0 area6 transmitter2)
  (los0 area7 transmitter2)
  (los0 area8 transmitter1)
  (los1 area7 gate1 transmitter1)
  (los1 area7 gate2 receiver2)
  (los1 area8 gate2 transmitter2)
  (los2 area3 gate1 gate2 receiver2)
  (los2 area4 gate1 gate2 receiver2)

  ;visibility is from an area to an area 
  ;potentially containing a movable target or terminus
  (visible0 area1 area3)
  (visible0 area1 area4)
  (visible0 area1 area5)
  (visible0 area2 area4)
  (visible0 area2 area5)
  (visible0 area2 area6)
  (visible0 area3 area5)
  (visible0 area3 area6)
  (visible0 area3 area7)
  (visible0 area3 area8)
  (visible0 area4 area6)
  (visible0 area4 area8)
  (visible0 area5 area6)
  (visible0 area5 area8)
  (visible1 area1 gate1 area7) 
  (visible1 area3 gate1 area7) 
  (visible1 area2 gate1 area7) 
  (visible1 area4 gate1 area7) 
  (visible1 area4 gate1 area6) 
  (visible1 area5 gate1 area7) 
  (visible1 area6 gate2 area8)
  (visible1 area7 gate2 area8)
  

  (visible2 area2 gate1 gate2 area8)
  (visible2 area3 gate1 gate2 area8)
  (visible2 area4 gate1 gate2 area8)
)

;;;; INITIALIZATION ACTIONS ;;;;

;init-actions save listing systematic facts

 (define-init-action init-los0  
   ;los exists to any station within its local area
    0
  (?station station (?area1 ?area2) area)
  (or (locale ?station ?area1)             ;for fixtures
      (separates ?station ?area1 ?area2))  ;for gates
  ()
  (assert (los0 ?area1 ?station)))


 (define-init-action init-visible0-locally  
   ;any object is visible from its own local area
    0
  (?area area)
  (always-true)
  ()
  (assert (visible0 ?area ?area)))


 (define-init-action init-visible0-via-adjacency  
   ;any object is visible from an adjacent area
    0
  ((?area1 ?area2) area)
  (adjacent ?area1 ?area2)
  ()
  (assert (visible0 ?area1 ?area2)))


 (define-init-action init-visible1-thru-divider  
   ;any object is visible thru a divider
    0
  (?divider divider (?area1 ?area2) area)
  (separates ?divider ?area1 ?area2)
  ()
  (assert (visible1 ?area1 ?divider ?area2)))



;;;; GOAL ;;;;


(define-goal  ;always put this last
  (loc me area8))

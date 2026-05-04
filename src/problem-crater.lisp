;;; Filename: problem-crater.lisp

;;; Problem specification (in Talos Principle)
;;; for the (second) Nexus-2 crater problem in Road to Gehenna.
;;; Uses early problem specs; updated for later Talos problems.


(in-package :ww)  ;required

(ww-set *problem-name* crater)

(ww-set *problem-type* planning)

(ww-set *depth-cutoff* 9)

(ww-set *solution-type* first)  ;min-length)


(define-types
  myself      (me)
  gate        (gate4 gate5 gate6)
  barrier     (barrier3 barrier4)
  jammer      (jammer1)
  connector   (connector2 connector3 connector4 connector5 connector6)
  box         (box1 box2 box3 box4 box5)
  fan         (fan1 fan2 fan3 fan4)
  gears       (gears2 gears3)
  ladder      (ladder3 ladder4)
  rostrum     (rostrum2)
  hue         (blue red)
  transmitter (transmitter2)
  receiver    (receiver7 receiver8 receiver9 receiver10 receiver11 receiver12
               receiver13 receiver14)
  area        (area1 area2 area8 area9 area10 area11 area12)
  divider     (either gate barrier)
  cargo       (either jammer connector box fan)
  target      (either gate gears)
  terminus    (either transmitter receiver connector)
  fixture     (either transmitter receiver gears ladder rostrum)
  station     (either fixture gate)
  support     (either box rostrum))


(define-dynamic-relations
  (holding myself $cargo)
  (free myself)
  (loc (either myself cargo) $area)
  (on (either myself cargo) $support)
  (attached fan gears)
  (jamming jammer $target)
  (connecting connector terminus)
  (active (either connector gate receiver gears))
  (height support $real)
  (color terminus $hue))


(define-static-relations
  (adjacent area area)  ;agent can always move unimpeded between areas
  (locale fixture area)
  (separates divider area area)
  (climbable> ladder area area)
  (height support $real)
  (controls receiver target) ; (either gate gears))
  (controls2 receiver receiver gate)  ;gate controlled by two receivers together
  (los0 area (either gate fixture))  ;clear los from an area to a gate/fixture
  (los1 area divider station)         ;(either gate fixture))
  (los2 area divider divider station)  ; (either gate fixture))
  (visible0 area area)  ;could see a potential mobile object in an area from a given area
  (visible1 area divider area)
  (visible2 area divider divider area))


(define-complementary-relations  
  (holding myself $cargo) -> (not (free myself)))


;;;;;;;;;;;;;;;;;;;;;;;; QUERY FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-query elevation? (?support)
  (do (bind (height ?support $h))
      (bind (on ?support $s))
      (or (and (support $s)
               (+ $h (elevation? state $s)))
          $h)))


(define-query same-color? (?terminus1 ?terminus2)
  (and (bind (color ?terminus1 $hue1))
       (bind (color ?terminus2 $hue2))
       (eql $hue1 $hue2)))


(define-query source? (?terminus)
  (or (transmitter ?terminus)
      (and (connector ?terminus)
           (active ?terminus))))


(define-query los-thru-1-divider? (?area ?station)
  (exists (?d divider)
    (and (los1 ?area ?d ?station)
         (or (barrier ?d)
             (and (gate ?d)
                  (not (active ?d)))))))


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


(define-query los? (?area ?station)
  (or (los0 ?area ?station)
      (los-thru-1-divider? ?area ?station)
      (los-thru-2-dividers? ?area ?station)))


(define-query visible-thru-1-divider? (?area1 ?area2)
  (exists (?d divider)
    (and (visible1 ?area1 ?d ?area2)
         (or (barrier ?d)
             (and (gate ?d)
                  (not (active ?d)))))))


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


(define-query sourced? (?conn-or-rcvr $visits)
  (do (push ?conn-or-rcvr $visits)   ;(ut::prt $visits) (break)
      (or (exists (?t transmitter)
            (connecting ?t ?conn-or-rcvr))
          (exists (?c connector)
            (and (connecting ?c ?conn-or-rcvr)  
                 (active ?c)
                 (not (member ?c $visits))
                 (sourced? ?c $visits))))))


;;;;;;;;;;;;;;;;;;;;;;;; UPDATE FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;


(define-update activate-connector! (?connector ?hue)
  (do (active ?connector)
      (color ?connector ?hue)))


(define-update deactivate-connector! (?connector ?hue)
  (do (not (active ?connector))
      (not (color ?connector ?hue))))


(define-update activate-receiver! (?receiver)
  (do (active ?receiver)
      (doall (?g gate)
        (if (and (controls ?receiver ?g)
                 (active ?g))
          (not (active ?g))))))


(define-update deactivate-receiver! (?receiver)
  (do (not (active ?receiver))
      (doall (?g gate)
        (if (controls ?receiver ?g)
          (active ?g)))))


(define-update disconnect-connector! (?connector)
  (doall (?t terminus)
    (if (connecting ?connector ?t)
      (not (connecting ?connector ?t)))))


(define-update chain-activate! (?connector ?hue)
  (do (activate-connector! ?connector ?hue)
      (doall (?r receiver)
        (if (and (connecting ?connector ?r)
                 (not (active ?r))
                 (bind (color ?r $rhue))
                 (eql $rhue ?hue))
          (activate-receiver! ?r)))
      (doall (?c connector)
        (if (and (different ?c ?connector)
                 (connecting ?connector ?c)
                 (not (active ?c)))
          (chain-activate! ?c ?hue)))))


(define-update chain-deactivate! (?connector ?hue)
  (do (deactivate-connector! ?connector ?hue)
      (doall (?r receiver)
        (if (and (connecting ?connector ?r)
                 (not (exists (?c connector)
                        (and (connecting ?c ?r)
                             (bind (color ?c $hue1)
                             (eql $hue1 ?hue))))))
          (deactivate-receiver! ?r)))
      (doall (?c connector)
        (if (and (different ?c ?connector)
                 (connecting ?connector ?c))
          (do (not (connecting ?connector ?c))
              (chain-deactivate! ?c ?hue))))
      (doall (?t transmitter ?c connector)  ;reactivate connectors with a transmitter source
        (if (and (not (eql ?c ?connector))
                 (connecting ?t ?c)
                 (not (active ?c))
                 (bind (color ?t $thue)))
          (chain-activate! ?c $thue)))))


(define-update disengage-jammer! (?jammer ?target)
  (do (not (jamming ?jammer ?target))
      (if (not (exists (?j jammer)
                 (and (different ?j ?jammer)
                      (jamming ?j ?target))))
        (active ?target))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-action connect-to-1-terminus
    1.1
  (?terminus terminus)
  (and (bind (holding me $cargo))
       (connector $cargo)
       (bind (loc me $area))
       (connectable? $area ?terminus))
  ($cargo ?terminus $area $hue)
  (assert (not (holding me $cargo))
          (loc $cargo $area)
          (connecting $cargo ?terminus)
          (if (and (source? ?terminus)
                   (bind (color ?terminus $hue)))
            (activate-connector! $cargo $hue))))


(define-action connect-to-2-terminus
    2.2
  (combination (?terminus1 ?terminus2) terminus)
  (and (bind (holding me $cargo))
       (connector $cargo)
       (bind (loc me $area))
       (connectable? $area ?terminus1)
       (connectable? $area ?terminus2))
  ($cargo ?terminus1 ?terminus2 $area)
  (assert (not (holding me $cargo))
          (loc $cargo $area)
          (connecting $cargo ?terminus1)
          (connecting $cargo ?terminus2)
          (bind (color ?terminus1 $hue1))
          (bind (color ?terminus2 $hue2))
          (if (or $hue1 $hue2)    ;at least one active
            (if (eql $hue1 $hue2)  ;both active and the same color
              (setq $hue $hue1)
              (if (not (and $hue1 $hue2))  ;both are not active (with different colors)
                (setq $hue (or $hue1 $hue2)))))
          (if $hue
            (chain-activate! $cargo $hue))))


(define-action connect-to-3-terminus
    3.3
  (combination (?terminus1 ?terminus2 ?terminus3) terminus)
  (and (bind (holding me $cargo))
       (connector $cargo)
       (bind (loc me $area))
       (connectable? $area ?terminus1)
       (connectable? $area ?terminus2)
       (connectable? $area ?terminus3))
  ($cargo ?terminus1 ?terminus2 ?terminus3 $area)
  (assert (not (holding me $cargo))
          (loc $cargo $area)
          (connecting $cargo ?terminus1)
          (connecting $cargo ?terminus2)
          (connecting $cargo ?terminus3)
          (bind (color ?terminus1 $hue1))
          (bind (color ?terminus2 $hue2))
          (bind (color ?terminus3 $hue3))
          (if (or $hue1 $hue2 $hue3)    ;at least one active
            (if (eql* $hue1 $hue2 $hue3)  ;exactly three active and the same color
              (setq $hue $hue1)
              (if (or (eql $hue1 $hue2)  ;exactly two active and the same color
                      (eql $hue1 $hue3))
                (setq $hue $hue1)
                (if (eql $hue2 $hue3)
                  (setq $hue $hue2)
                  (if (not (and $hue1 $hue2 $hue3))  ;all are not active (with different colors)
                    (setq $hue (or $hue1 $hue2 $hue3)))))))
          (if $hue
            (chain-activate! $cargo $hue))))


(define-action jam
    1.4
  (?target target)
  (and (bind (holding me $cargo))
       (jammer $cargo)
       (bind (loc me $area))
       (los? $area ?target))
  (?target $cargo $area)
  (assert (not (holding me $cargo))
          (loc $cargo $area)
          (jamming $cargo ?target)
          (not (active ?target))))


(define-action pickup-jammer
    1
  (?jammer jammer)
  (and (not (bind (holding me $cargo)))
       (bind (loc me $area))
       (loc ?jammer $area))
  (?jammer $area $target)
  (assert (holding me ?jammer)
          (not (loc ?jammer $area))
          (if (bind (jamming ?jammer $target))
            (disengage-jammer! ?jammer $target))))


(define-action pickup-connector
    1
  (?connector connector)
  (and (free me)
       (bind (loc me $area))
       (loc ?connector $area))
  (?connector $area)
  (assert (holding me ?connector)
          (not (loc ?connector $area))
          (if (bind (color ?connector $hue))
            (chain-deactivate! ?connector $hue))
          (doall (?t terminus)
            (if (connecting ?connector ?t)
              (not (connecting ?connector ?t))))))


(define-action drop-cargo
  .8
  ()
  (and (bind (loc me $area))
       (bind (holding me $cargo)))
  ($cargo $area)
  (assert (not (holding me $cargo))
          (loc $cargo $area)))


(define-action move
    1
  (?area2 area)
  (and (bind (loc me $area1))
       (different $area1 ?area2)
       (passable? $area1 ?area2))
  ($area1 ?area2)
  (assert (loc me ?area2)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         

(define-init
  ;dynamic
  (loc me area1)
  (free me)
  (loc jammer1 area1)
  (loc connector2 area2) (loc connector3 area8) (loc connector4 area10)
  (loc connector5 area11) (loc connector6 area11)
  (loc box1 area2) (loc box3 area1) (loc box4 area1) (loc box5 area1)
  (on box3 box4)
  (loc fan2 area12) (loc fan3 area2) (loc fan1 area1)
  (attached fan3 gears3)
  (on connector2 box1)
  (active gate4) (active gate5) (active gate6)
  
  ;static
  (adjacent area8 area9)
  (height box2 1) (height box3 1) (height box4 1) (height box5 1)
  (height rostrum2 0.5)
  (locale transmitter2 area8)
  (locale rostrum2 area10)
  (locale ladder3 area10) (locale ladder4 area10)
  (locale gears2 area10) (locale gears3 area2)
  (locale receiver7 area9) (locale receiver8 area10) (locale receiver9 area10) 
  (locale receiver10 area10) (locale receiver11 area11)  (locale receiver12 area11)
  (locale receiver13 area11) (locale receiver14 area2)
  (color transmitter2 red)
  (color receiver7 blue) (color receiver8 blue) (color receiver9 blue) 
  (color receiver10 red) (color receiver11 red) (color receiver12 blue) 
  (color receiver13 red) (color receiver14 red)
  (controls receiver7 gate4) (controls receiver8 gate4) (controls receiver9 gears2) 
  (controls receiver10 gate5) (controls receiver11 gate5) (controls receiver12 gate6) 
  (controls receiver13 gate6) (controls receiver14 gears3)
  (separates barrier3 area1 area8) (separates barrier4 area1 area11)
  (separates gate4 area9 area10) (separates gate5 area10 area11) (separates gate6 area11 area12)
  (climbable> ladder3 area10 area8) (climbable> ladder4 area10 area11)

;los is from an area to a fixed station
  (los0 area1 transmitter2) (los0 area2 transmitter2)
  (los0 area1 receiver7) (los0 area8 receiver7)
  (los0 area1 gate4) (los0 area8 gate4) (los1 area11 gate5 gate4)    
  (los1 area11 gate5 receiver8)
  (los1 area9 gate4 receiver9) (los1 area11 gate5 receiver9)
  (los1 area9 gate4 receiver10)
  (los0 area1 gate5) (los1 area9 gate4 gate5) (los1 area12 gate6 gate5)
  (los0 area1 receiver11) (los1 area12 gate6 receiver11)
  (los2 area9 gate4 gate5 receiver12) (los1 area10 gate5 receiver12) (los1 area12 gate6 receiver12)
  (los2 area9 gate4 gate5 receiver13) (los1 area10 gate5 receiver13) (los1 area12 gate6 receiver13)
  (los1 area10 gate5 gate6)
  (los0 area1 receiver14) (los1 area10 gate5 receiver14) (los0 area11 receiver14)

;visibility is from an area to an area potentially containing a movable target or terminus
  (visible0 area1 area9) (visible0 area8 area2)
  (visible1 area1 gate4 area10) 
  (visible1 area8 gate4 area10) 
  (visible2 area9 gate4 gate5 area11)
  (visible1 area10 gate5 area1) (visible2 area10 gate5 gate6 area12) 
)


;init-actions save listing systematic facts

(define-init-action init-los0  ;los exists to any station within its local area
    0
  (?station station (?area1 ?area2) area)
  (or (locale ?station ?area1)             ;for fixtures
      (separates ?station ?area1 ?area2))  ;for gates
  ()
  (assert (los0 ?area1 ?station)))


(define-init-action init-visible0-locally  ;any object is visible from its own local area
    0
  (?area area)
  (always-true)
  ()
  (assert (visible0 ?area ?area)))


(define-init-action init-visible0-via-adjacency  ;any object is visible from an adjacent area
    0
  ((?area1 ?area2) area)
  (adjacent ?area1 ?area2)
  ()
  (assert (visible0 ?area1 ?area2)))


(define-init-action init-visible1-thru-divider
    0
  (?divider divider (?area1 ?area2) area)
  (separates ?divider ?area1 ?area2)
  ()
  (assert (visible1 ?area1 ?divider ?area2)))


(define-goal  ;always put this last
;check that all conditions in the final state are met
    (and (not (bind (holding me $cargo)))
         (loc me area10)
         (loc jammer1 area1)
         (jamming jammer1 gate4)
         (not (active gate4))
         (loc connector3 area8)
         (connecting connector3 transmitter2)
         (color connector3 red)
         (active connector3)
         (loc connector4 area10)
         (connecting connector4 connector3)
         (connecting connector4 receiver10)
         (color connector4 red)
         (active connector4)))

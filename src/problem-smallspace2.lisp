;;;; Filename: problem-smallspace2.lisp

;;; Problem specification (in Talos Principle)
;;; for the small space problem in Road to Gehenna sigil dome.
;;; Second Leg.


(in-package :ww)  ;required

(ww-set *problem-name* smallspace2)

(ww-set *problem-type* planning)

(ww-set *solution-type* first)

(ww-set *depth-cutoff* 15)


(define-types
  me          (me1)
  gate        (gate1 gate2 gate3 gate4)
  barrier     (nil)  ;cannot move cargo thru a barrier
  connector   (connector1 connector2 connector3)
  transmitter (transmitter1 transmitter2)
  receiver    (receiver1 receiver2 receiver3 receiver4)
  hue         (blue red)
  area        (area1 area2 area3 area4 area5 area6 area7 area8 area9 area10)
  cargo       (either connector)
  divider     (either barrier gate)
  terminus    (either transmitter receiver connector)
  fixture     (either transmitter receiver)
  station     (either fixture gate))


(define-dynamic-relations
  (holds me $cargo)
  (loc (either me cargo) $area)
  (connects terminus terminus)
  (active (either connector receiver gate))
  (color terminus $hue))


(define-static-relations
  (adjacent area area)  ;agent can always move unimpeded between areas
  (locale fixture area)
  (separates1 barrier area area)
  (separates2 gate area area)
  (controls receiver gate)
  ;(controls2 receiver receiver $gate)  ;gate controlled by two receivers together
  (los0 area (either gate fixture))  ;clear los from an area to a gate/fixture
  (los1 area divider (either gate fixture))
  (los2 area divider divider (either gate fixture))
  (visible0 area area)  ;could see a potential mobile object in an area from a given area
  (visible1 area divider area)
  (visible2 area divider divider area))


;;;;;;;;;;;;;;;;;;;;;;;; QUERY FUNCTIONS ;;;;;


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
                  (active ?d1)
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
                  (active ?d1)
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
      (exists (?b barrier)
        (and (separates1 ?b ?area1 ?area2)
             (not (bind (holds me1 $cargo)))))  ;must drop cargo first
      (exists (?g gate)
        (and (separates2 ?g ?area1 ?area2)
             (not (active ?g))))))


;;;;;;;;;;;;;;;;;;;;;;;; UPDATE FUNCTIONS ;;;;;


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


(define-update chain-activate! (?connector ?hue)
  (do (activate-connector! ?connector ?hue)
      (doall (?r receiver)
        (if (and (connects ?connector ?r)
                 (not (active ?r))
                 (bind (color ?r $rhue))
                 (eql $rhue ?hue))
          (activate-receiver! ?r)))
      (doall (?c connector)
        (if (and (different ?c ?connector)
                 (connects ?connector ?c)
                 (not (active ?c)))
          (chain-activate! ?c ?hue)))))


(define-update chain-deactivate! (?connector ?hue)
  (do 
    ;; Step 1: Deactivate this connector
    (deactivate-connector! ?connector ?hue)
    ;; Step 2: Deactivate receivers that lost power
    (doall (?r receiver)
      (if (and (connects ?connector ?r)
               (not (exists (?c connector)
                      (and (different ?c ?connector)
                           (connects ?c ?r)
                           (active ?c)
                           (bind (color ?c $c-hue))
                           (eql $c-hue ?hue)))))
        (deactivate-receiver! ?r)))
    ;; Step 3: For each connected connector, check ALL possible power paths
    ;; Including direct transmission paths and paths through other active connectors
    (doall (?c connector)
      (if (and (connects ?connector ?c)
               (different ?c ?connector))  ;; Prevent direct self-reference
        ;; Start a careful power source check
        (if (not (or 
                  ;; Check direct transmitter connection
                  (exists (?t transmitter)
                     (and (connects ?c ?t)
                          (bind (color ?t $t-hue))
                          (eql $t-hue ?hue)))
                  ;; Check connection to another active connector (not the one being picked up)
                  (exists (?other-connector connector)
                     (and (different ?other-connector ?connector)
                          (different ?other-connector ?c)
                          (connects ?c ?other-connector)
                          (active ?other-connector)
                          (bind (color ?other-connector $other-hue))
                          (eql $other-hue ?hue)))))
            ;; No alternative power source found, recursively deactivate
            (do
              ;; Break the connection before recursion to avoid infinite recursion
              (not (connects ?connector ?c))
              (chain-deactivate! ?c ?hue)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;; ACTIONS ;;;;;


(define-action connect-to-1-terminus
    2
  (?terminus terminus)
  (and (bind (holds me1 $cargo))
       (connector $cargo)
       (bind (loc me1 $area))
       (connectable? $area ?terminus))
  ($cargo ?terminus $area $hue)
  (assert (not (holds me1 $cargo))
          (loc $cargo $area)
          (connects $cargo ?terminus)
          (if (and (source? ?terminus)
                   (bind (color ?terminus $hue)))
            (activate-connector! $cargo $hue))))


(define-action connect-to-2-terminus
    3
  (combination (?terminus1 ?terminus2) terminus)
  (and (bind (holds me1 $cargo))
       (connector $cargo)
       (bind (loc me1 $area))
       (connectable? $area ?terminus1)
       (connectable? $area ?terminus2))
  ($cargo ?terminus1 ?terminus2 $area $hue)
  (assert (not (holds me1 $cargo))
          (loc $cargo $area)
          (connects $cargo ?terminus1)
          (connects $cargo ?terminus2)
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
    4
  (combination (?terminus1 ?terminus2 ?terminus3) terminus)
  (and (bind (holds me1 $cargo))
       (connector $cargo)
       (bind (loc me1 $area))
       (connectable? $area ?terminus1)
       (connectable? $area ?terminus2)
       (connectable? $area ?terminus3))
  ($cargo ?terminus1 ?terminus2 ?terminus3 $area $hue)
  (assert (not (holds me1 $cargo))
          (loc $cargo $area)
          (connects $cargo ?terminus1)
          (connects $cargo ?terminus2)
          (connects $cargo ?terminus3)
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


(define-action pickup-connector
    1
  (?connector connector)
  (and (not (bind (holds me1 $cargo)))
       (bind (loc me1 $area))
       (loc ?connector $area))
  (?connector $area)
  (assert (holds me1 ?connector)
          (not (loc ?connector $area))
          (if (bind (color ?connector $hue))
            (chain-deactivate! ?connector $hue))
          ;; Finally disconnect this picked up connector from everything
          (doall (?t terminus)
            (if (connects ?connector ?t)
              (not (connects ?connector ?t))))))


(define-action drop
    1
  ()
  (and (bind (loc me1 $area))  ;me1 is always located somewhere
       (bind (holds me1 $cargo)))  ;if not holding, then bind statement returns nil, otherwise binds $cargo
  ($cargo $area)
  (assert (not (holds me1 $cargo))
          (loc $cargo $area)))


(define-action move
    1
  (?area2 area)
  (and (bind (loc me1 $area1))
       (different $area1 ?area2)
       (passable? $area1 ?area2))
  ($area1 ?area2)
  (assert (loc me1 ?area2)))


;;;;;;;;;;;;;;;;;;;;;;; INITIALIZATION ;;;;;


(define-init
  ;dynamic (starting from final position in problem-smallspace.lisp)
  (ACTIVE CONNECTOR1) (ACTIVE CONNECTOR2)
  (ACTIVE GATE1) (ACTIVE GATE3) (ACTIVE GATE4) (ACTIVE RECEIVER2)
  (COLOR CONNECTOR1 RED) (COLOR CONNECTOR2 RED) 
  (connects CONNECTOR1 CONNECTOR2) (CONNECTS CONNECTOR2 CONNECTOR1) 
  (connects CONNECTOR1 RECEIVER1) (CONNECTS RECEIVER1 CONNECTOR1)
  (connects CONNECTOR1 RECEIVER2) (CONNECTS RECEIVER2 CONNECTOR1)
  (connects CONNECTOR2 TRANSMITTER2) (CONNECTS TRANSMITTER2 CONNECTOR2)
  (LOC ME1 AREA8) (LOC CONNECTOR1 AREA5) (LOC CONNECTOR2 AREA6) (LOC CONNECTOR3 AREA8)
  ;static
  (adjacent area1 area2)
  (adjacent area2 area3)
  (adjacent area3 area4)
  (adjacent area4 area5)
  (adjacent area6 area7)
  (locale transmitter1 area4)
  (locale transmitter2 area6)
  (locale receiver1 area4)
  (locale receiver2 area8)
  (locale receiver3 area9)
  (locale receiver4 area10)
  (color transmitter1 blue)
  (color transmitter2 red)
  (color receiver1 blue)
  (color receiver2 red)
  (color receiver3 blue)
  (color receiver4 red)
  (controls receiver1 gate1)
  (controls receiver2 gate2)
  (controls receiver3 gate3)
  (controls receiver4 gate4)
  (separates2 gate1 area4 area7)
  (separates2 gate2 area7 area8)
  (separates2 gate3 area8 area9)
  (separates2 gate4 area9 area10)

;los is from an area to a fixed station
  ;(los0 area2 transmitter1)
  (los0 area3 transmitter1)
  (los0 area3 receiver1)
  (los0 area3 receiver3)
  (los0 area4 receiver3)
  (los0 area5 transmitter1)
  (los0 area5 receiver1)
  (los0 area5 receiver2)
  (los0 area5 receiver3)
  (los0 area5 receiver4)
  (los0 area6 transmitter1)
  ;(los0 area6 transmitter2)
  (los0 area7 transmitter2)
  (los0 area8 transmitter1)
  (los0 area8 receiver4)
  (los0 area9 transmitter1)
  (los0 area10 receiver2)
  (los1 area7 gate1 transmitter1)
  (los1 area7 gate2 receiver2)
  (los1 area8 gate2 transmitter2)
  (los1 area8 gate3 receiver3)
  (los1 area9 gate4 receiver4)
  (los2 area3 gate1 gate2 receiver2)
  (los2 area4 gate1 gate2 receiver2)

;visibility is from an area to an area potentially containing a movable target or terminus
  (visible0 area1 area3)
  (visible0 area1 area4)
  ;(visible0 area1 area5)
  (visible0 area2 area4)
  (visible0 area2 area5)
  (visible0 area2 area6)
  (visible0 area3 area5)
  (visible0 area3 area6)
  (visible0 area3 area7)
  (visible0 area3 area8)
  (visible0 area3 area9)
  (visible0 area3 area10)
  (visible0 area4 area6)
  (visible0 area4 area8)
  (visible0 area4 area9)
  (visible0 area4 area10)
  (visible0 area5 area6)
  (visible0 area5 area8)
  (visible0 area5 area9)
  (visible0 area5 area10)
  (visible0 area6 area10)
  (visible0 area8 area9)
  (visible0 area8 area10)
  (visible1 area1 gate1 area7) 
  (visible1 area2 gate1 area7) 
  (visible1 area3 gate1 area7) 
  ;(visible1 area4 gate1 area7) 
  (visible1 area4 gate1 area6) 
  (visible1 area5 gate1 area6) 
  (visible1 area5 gate1 area7) 
  (visible1 area5 gate3 area8)
  (visible1 area5 gate4 area10)
  (visible1 area6 gate2 area8)
  ;(visible1 area7 gate2 area8)
  (visible1 area7 gate1 area10)
  (visible1 area8 gate3 area9)
  (visible1 area8 gate3 area10)
  (visible1 area9 gate4 area10)
  (visible2 area2 gate1 gate2 area8)
  (visible2 area3 gate1 gate2 area8)
  (visible2 area4 gate1 gate2 area8)
  (visible2 area7 gate2 gate3 area9)
  (visible2 area8 gate3 gate4 area10)
)

;;;;;;;;;;;;;;;;;;; INITIALIZATION ACTIONS ;;;;


;init-actions save listing systematic facts

 (define-init-action init-los0  
   ;los exists to any station within its local area
    0
  (?station station (?area1 ?area2) area)
  (or (locale ?station ?area1)             ;for fixtures
      (separates2 ?station ?area1 ?area2))  ;for gates
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
  (or (and (barrier ?divider)
           (separates1 ?divider ?area1 ?area2))
      (and (gate ?divider)
           (separates2 ?divider ?area1 ?area2)))
  ()
  (assert (visible1 ?area1 ?divider ?area2)))


;;;;;;;;;;;;;;;;;;; GOAL ;;;;

(define-goal  ;always put this last
  (loc me1 area9))


;;;;;;;; Invariant Checks ;;;;;;;;;;;;;;;

#+ignore (define-invariant active-connector-hue ()
  ;Connectors are active if and only if they have a hue
  (doall (?c connector)
    (equivalent (active ?c)
                (bind (color ?c $hue)))))


#+ignore (define-invariant holds-cargo-location ()
  ;Cargo cannot be both held and have a location simultaneously
  (not (and (bind (holds me1 $cargo))
            (bind (loc $cargo $area)))))


(define-invariant receiver-activation ()
  ;A receiver is active if and only if there exists at least one
  ;connected, active connector of the same color.
  (doall (?r receiver)
    (if (bind (color ?r $rhue))
      (equivalent (active ?r)
                  (exists (?c connector)
                    (and (connects ?c ?r)
                         (active ?c)
                         (bind (color ?c $chue))
                         (eql $chue $rhue)))))))


(define-invariant receiver-gate-control ()
  ;A receiver is active if and only if all gates it controls are inactive
  (doall (?r receiver)
    (if (exists (?g gate)
          (controls ?r ?g))
      (equivalent (active ?r)
                  (forall (?g gate)
                    (if (controls ?r ?g)
                      (not (active ?g))))))))


(define-invariant colored-connector-connection ()
  ;Any colored connector must have a valid source with matching color,
  ;either a transmitter or another connector
  (doall (?c connector)
    (if (bind (color ?c $hue))
        (or (exists (?t transmitter)
              (and (connects ?c ?t)
                   (bind (color ?t $t-hue))
                   (eql $t-hue $hue)))
            (exists (?other connector)
              (and (different ?other ?c)
                   (connects ?c ?other)    ; Connected to it
                   (bind (color ?other $other-hue))
                   (eql $other-hue $hue)))))))


(define-invariant connector-self-connection ()
  ;No connector is connected to itself
  (doall (?c connector)
    (not (connects ?c ?c))))


(define-invariant me1-has-location ()
  ;Me1 is always located in some area
  (bind (loc me1 $area)))

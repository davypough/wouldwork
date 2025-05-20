;;; Filename: problem-smallspace.lisp


;;; Problem specification (in Talos Principle)
;;; for the small space problem in Road to Gehenna sigil 
;;; dome. First leg to area8.
;;; Uses mixed fluent & non-fluent relations


(in-package :ww)  ;required

(ww-set *problem-name* smallspace)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 20)


(define-types
  me          (me1)
  gate        (gate1 gate2)
  barrier     (nil)  ;cannot move cargo thru a barrier
  jammer      (nil)
  gun         (nil)
  connector   (connector1 connector2)
  plate       (nil)
  box         (nil)
  fan         (nil)
  gears       (nil)
  switch      (nil)
  ladder      (nil)
  rostrum     (nil)
  transmitter (transmitter1 transmitter2)
  receiver    (receiver1 receiver2)
  hue         (blue red)  ;the color of a transmitter, receiver, or active connector
  area        (area1 area2 area3 area4 area5 area6 area7 area8)
  cargo       (either connector jammer box fan)  ;what an agent (me) can pickup & carry
  target      (either gate gears gun)  ;what a jammer can jam
  divider     (either barrier gate)
  terminus    (either transmitter receiver connector)  ;what a connector can connect to
  fixture     (either transmitter receiver gears ladder rostrum)
  station     (either fixture gate)  ;useful for los determinations
  support     (either box rostrum))


(define-dynamic-relations  ;relations with fluents can be bound in rules--eg (bind (holds me1 $any-cargo))
  (holds me $cargo)  ;fluent because we need to sometimes lookup what is currently being held
  ;(free me)
  (loc (either me cargo) $area)
  ;(on (either me cargo) $support)
  ;(attached fan gears)
  ;(jams jammer $target)
  (connects terminus terminus)
  (active (either connector receiver gate switch gun gears))
  (color terminus $hue))


(define-static-relations
  (adjacent area area)  ;agent can always move to adjacent area unimpeded  
  (locale fixture area)  ;locale is a fixed location, loc is dynamic
  (separates1 barrier area area)
  (separates2 gate area area)
  ;(separates divider area area)
  ;(climbable> ladder area area)
  ;(height support $real)
  (controls receiver gate)
  ;clear los from an area to a gate/fixture
  (los0 area (either gate fixture))  
  (los1 area divider (either gate fixture))
  (los2 area divider divider (either gate fixture))
  ;could see a mobile object in an area from a given area
  (visible0 area area)  
  (visible1 area divider area)
  (visible2 area divider divider area))


;(define-complementary-relations  
;  (holds me $cargo) -> (not (free me)))


;;;; QUERY FUNCTIONS ;;;;


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


;;;; UPDATE FUNCTIONS ;;;;


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


;;;; ACTIONS ;;;;


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


;;;; INITIALIZATION ;;;;


(define-init
  ;dynamic
  (loc me1 area5)
  (loc connector1 area5)
  (loc connector2 area7)
  ;(free me1)
  (active gate1)
  (active gate2)
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
  (color transmitter1 blue)
  (color transmitter2 red)
  (color receiver1 blue)
  (color receiver2 red)
  (controls receiver1 gate1)
  (controls receiver2 gate2)
  (separates2 gate1 area4 area7)
  (separates2 gate2 area7 area8)

  ;los is from an area to a fixed station
  ;(los0 area2 transmitter1)
  (los0 area3 transmitter1)
  (los0 area3 receiver1)
  (los0 area5 transmitter1)
  (los0 area5 receiver1)
  (los0 area5 receiver2)
  (los0 area6 transmitter1)
  ;(los0 area6 transmitter2)
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
  ;(visible0 area1 area5)
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
  (visible1 area2 gate1 area7) 
  (visible1 area3 gate1 area7) 
  ;(visible1 area4 gate1 area7) 
  (visible1 area4 gate1 area6) 
  (visible1 area5 gate1 area6) 
  (visible1 area5 gate1 area7) 
  (visible1 area6 gate2 area8)
  ;(visible1 area7 gate2 area8)
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


;;;; GOAL ;;;;

(define-goal  ;always put this last
  (loc me1 area8))


;;;;;;;; Invariant Checks for Debugging ;;;;;;;;;;;;;;;

#+ignore (define-invariant active-connector-hue ()
  ;Connectors are active if and only if they have a hue
  (doall (?c connector)
    (equivalent (active ?c)
                (bind (color ?c $hue)))))


#+ignore (define-invariant holds-cargo-location ()
  ;Cargo cannot be both held and have a location simultaneously
  (not (and (bind (holds me1 $cargo))
            (bind (loc $cargo $area)))))


#+ignore (define-invariant receiver-activation ()
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


#+ignore (define-invariant receiver-gate-control ()
  ;A receiver is active if and only if all gates it controls are inactive
  (doall (?r receiver)
    (if (exists (?g gate)
          (controls ?r ?g))
      (equivalent (active ?r)
                  (forall (?g gate)
                    (if (controls ?r ?g)
                      (not (active ?g))))))))


#+ignore (define-invariant colored-connector-connection ()
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


#+ignore (define-invariant connector-self-connection ()
  ;No connector is connected to itself
  (doall (?c connector)
    (not (connects ?c ?c))))


#+ignore (define-invariant me1-has-location ()
  ;Me1 is always located in some area
  (bind (loc me1 $area)))


#+ignore (define-invariant connector-transmitter-source ()
  ;Every active connector ultimately traces to transmitter sources without cycles
  (let (($valid-source t)) ; Assume valid until proven otherwise
    ;; For each active connector, verify it has a valid transmitter source
    (doall (?c connector)
      (if (active ?c)
        (do (setq $visited nil)           ; Track visited connectors
            (setq $source-found nil)      ; Flag if transmitter found
            (setq $stack nil)             ; DFS stack
            (setq $current ?c)            ; Current connector being examined
            (setq $color nil)             ; Color we're tracing
            ;; Get the color of this connector
            (bind (color ?c $col))
            (setq $color $col)
            ;; Initialize stack with current connector
            (push $current $stack)
            ;; Perform depth-first search to find transmitter or detect cycle
            (ww-loop while $stack do
              ;; Pop current connector from stack
              (setq $current (pop $stack))
              ;; Only process this connector if we haven't seen it before
              (unless (member $current $visited)
                ;; Mark as visited
                (push $current $visited)
                ;; Check if current is directly connected to a transmitter of same color
                (if (exists (?t transmitter)
                      (and (connects $current ?t)
                           (bind (color ?t $t-color))
                           (eql $t-color $color)))
                  (setq $source-found t)
                  ;; Otherwise, add connected connectors of same color to stack
                  (doall (?other connector)
                    (if (and (different ?other $current)
                             (connects $current ?other)
                             (active ?other)
                             (bind (color ?other $other-color))
                             (eql $other-color $color))
                      (push ?other $stack))))))
            ;; If we've explored all paths and never found a transmitter source,
            ;; this connector has no valid source (either disconnected or in a cycle)
            (if (not $source-found)
              (setq $valid-source nil)))))
    ;; Return result
    $valid-source))


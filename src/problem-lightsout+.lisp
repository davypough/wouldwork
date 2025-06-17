;;; Filename: problem-lightsout+.lisp


;;; Problem specification (in Talos Principle)
;;; for the Lights Out Star problem in In The Beginning 
;;; Uses mixed fluent & non-fluent relations


(in-package :ww)  ;required

(ww-set *problem-name* lightsout+)

(ww-set *problem-type* planning)

(ww-set *solution-type* first)  ;min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 8)


(define-types
  me          (me1)
  gate        (gate1 gate2 gate3)
  ;barrier     (nil)
  connector   (connector1 connector2 connector3)
  ;jammer      (nil)
  plate       (plate1)
  ladder      (ladder1 ladder2)
  transmitter (transmitter1 transmitter2)
  receiver    (receiver1 receiver2 receiver3)
  hue         (blue red)  ;the color of a transmitter, receiver, or active connector
  area        (area1 area2 area3)
  cargo       (either connector)  ;what an agent (me) can pickup & carry
  ;target      (either gate)  ;what a jammer can jam
  terminus    (either transmitter receiver connector)  ;what a connector can connect to
  fixture     (either transmitter receiver plate ladder)  ;has a permanent locale in an area
  station     (either transmitter receiver gate)  ;useful for los determinations
)


(define-dynamic-relations  ;relations with fluents can be bound in rules--eg (bind (holds me1 $any-cargo))
  (holds me $cargo)  ;fluent because we need to sometimes lookup what is currently being held
  (loc (either me cargo) $area)
  (on (either me cargo) $plate)
  (connected terminus terminus)
  #+ignore (jams jammer $target)
  (active (either receiver plate gate))
  (color connector $hue)
)


(define-static-relations
  (locale fixture $area)  ;locale is a fixed location, loc is dynamic
  (separates gate area area)
  (climbable> ladder area area)
  (controls (either receiver plate) gate)
  (chroma (either transmitter receiver) $hue)
  ;potential los from an area to a transmitter or receiver
  (los0 area (either transmitter receiver))
  (los1 area $gate (either transmitter receiver))
  (los2 area $gate $gate (either transmitter receiver))
  (visible0 area area)  ;visibility between areas is for connecting connectors
  (visible1 area $gate area)
  (visible2 area $gate $gate area)
)


;;;; QUERY FUNCTIONS ;;;;


(define-query hue-if-source? (?terminus)
  ;Gets the hue of ?terminus if it is a source, else nil
  (or (and (transmitter ?terminus)
           (bind (chroma ?terminus $hue))
           $hue)
      (and (connector ?terminus)
           (bind (color ?terminus $hue))
           $hue)))


(define-query all-controllers-active? (?gate)
  ;Also returns true if there are no controllers for ?gate
  (forall (?controller (either receiver plate))
    (if (controls ?controller ?gate)
      (active ?controller))))


(define-query open-los? (?area ?station)
  (or (los0 ?area ?station)
      (and (bind (los1 ?area $gate ?station))
           (not (active $gate)))
      (and (bind (los2 ?area $gate1 $gate2 ?station))
           (not (active $gate1))
           (not (active $gate2)))))


(define-query los? (?area ?station)
  ;There is a los, but not necessarily open
  (or (los0 ?area ?station)
      (bind (los1 ?area $gate ?station))
      (bind (los2 ?area $gate1 $gate2 ?station))))


(define-query visible? (?area1 ?area2)
  (or (visible0 ?area1 ?area2)  ;directly visible
      (and (visible1 ?area1 $gate ?area2)
           (not (active $gate)))
      (and (visible2 ?area1 $gate1 $gate2 ?area2)
         (not (active $gate1))
         (not (active $gate2)))))


(define-query connectable? (?area ?terminus)
  (or (los? ?area ?terminus)  ;from connector in area to terminus
      (and (connector ?terminus)
           (exists (?a area)
             (and (or (loc ?terminus ?a)
                      (locale ?terminus ?a))
                  (visible? ?area ?a))))))


(define-query passable? (?area1 ?area2)
  (and (not (bind (on me1 $plate)))  ;must step off plate first
       (or #+ignore (exists (?b barrier)
             (and (separates ?b ?area1 ?area2)
                  (not (bind (holds me1 $cargo)))))  ;must drop cargo first
           (exists (?g gate)
             (and (separates ?g ?area1 ?area2)
                  (not (active ?g)))))))


;;;; UPDATE FUNCTIONS ;;;;


(define-update activate-receiver! (?receiver)
  (do (active ?receiver)
      (doall (?g gate)
        (if (and (controls ?receiver ?g)
                 #+ignore (not (exists (?j jammer)
                        (jams ?j ?g)))
                 (all-controllers-active? ?g))
          (not (active ?g))))))


(define-update deactivate-receiver! (?receiver)
  (do (not (active ?receiver))
      (doall (?g gate)
        (if (and (controls ?receiver ?g)
                 #+ignore (not (exists (?j jammer)
                        (jams ?j ?g))))
          (active ?g)))))


(define-update activate-plate! (?plate)
  (do (active ?plate)
      (doall (?g gate)
        (if (and (controls ?plate ?g)
                 #+ignore (not (exists (?j jammer)
                        (jams ?j ?g)))
                 (all-controllers-active? ?g))
          (not (active ?g))))))


(define-update deactivate-plate! (?plate)
  (do (not (active ?plate))
      (doall (?g gate)
        (if (and (controls ?plate ?g)
                 #+ignore (not (exists (?j jammer)
                        (jams ?j ?g))))
          (active ?g)))))


(define-update chain-activate! (?connector ?hue ?area)
  (do 
    ;; Step 1: Color the connector being activated
    (color ?connector ?hue)
    ;; Step 2: Activate connected receivers FIRST (priority endpoints)
    ;; These are terminal devices that won't cause further cascading,
    ;; so activating them first prevents gate state changes from 
    ;; interfering with their activation
    (doall (?r receiver)
      (if (and (connected ?connector ?r)
               (not (active ?r))
               (chroma ?r ?hue)
               (open-los? ?area ?r))
        (activate-receiver! ?r)))
    ;; Step 3: Now propagate to connected connectors (cascading propagation)
    ;; Process these after receivers to ensure receivers get their chance
    ;; to activate before any gate states change due to further cascading
    (doall (?c connector)
      (if (and (different ?c ?connector)
               (connected ?connector ?c)
               (not (bind (color ?c $c-hue)))
               (bind (loc ?c $c-area))
               (visible? ?area $c-area))
        ;; Recursive call continues the cascading with same ordering
        (chain-activate! ?c ?hue $c-area)))))


(define-update connect-1-and-color! (?cargo ?terminus)
  (do (not (holds me1 ?cargo))
      (connected ?cargo ?terminus)
      (setq $hue (hue-if-source? ?terminus))
      (if $hue
        (color ?cargo $hue))))


(define-update connect-2-and-color! (?cargo ?terminus1 ?terminus2)
  (do 
    ;; Step 1: Release cargo and establish location
    (not (holds me1 ?cargo))
    (bind (loc me1 $area))
    ;; Step 2: Initialize color tracking variables  
    (setq $cargo-hue nil)       ;; The color this connector will have
    (setq $color-conflict nil)  ;; Flag for conflicting source colors
    ;; Step 3: Connect to transmitters first (highest priority for power source)
    ;; Check terminus1 for transmitter
    (if (transmitter ?terminus1)
      (do (connected ?cargo ?terminus1)
          (setq $hue1 (hue-if-source? ?terminus1))
          (if $hue1
            (do (setq $cargo-hue $hue1)
                (color ?cargo $cargo-hue)))))
    ;; Check terminus2 for transmitter
    (if (and (transmitter ?terminus2) (not $cargo-hue))
      (do (connected ?cargo ?terminus2)
          (setq $hue2 (hue-if-source? ?terminus2))
          (if $hue2
            (do (setq $cargo-hue $hue2)
                (color ?cargo $cargo-hue))))
      ;; If cargo already has color from terminus1 transmitter, check for conflict
      (if (and (transmitter ?terminus2) $cargo-hue)
        (do (connected ?cargo ?terminus2)
            (setq $hue2 (hue-if-source? ?terminus2))
            (if (and $hue2 (not (eql $cargo-hue $hue2)))
              (setq $color-conflict t)))))
    ;; Step 4: Connect to active connectors (secondary power source)
    ;; Check terminus1 for active connector
    (if (and (connector ?terminus1) (not $color-conflict) (bind (color ?terminus1 $conn1-hue)))
      (do (connected ?cargo ?terminus1)
          (if (not $cargo-hue)
            (do (setq $cargo-hue $conn1-hue)
                (color ?cargo $cargo-hue))
            (if (not (eql $cargo-hue $conn1-hue))
              (setq $color-conflict t)))))
    ;; Check terminus2 for active connector
    (if (and (connector ?terminus2) (not $color-conflict) (bind (color ?terminus2 $conn2-hue)))
      (do (connected ?cargo ?terminus2)
          (if (not $cargo-hue)
            (do (setq $cargo-hue $conn2-hue)
                (color ?cargo $cargo-hue))
            (if (not (eql $cargo-hue $conn2-hue))
              (setq $color-conflict t)))))
    ;; Step 5: Connect to receivers last and activate if powered
    ;; Check terminus1 for receiver
    (if (receiver ?terminus1)
      (do (connected ?cargo ?terminus1)
          ;; Activate receiver if connector is properly colored and path is open
          (if (and $cargo-hue 
                   (not $color-conflict)
                   (chroma ?terminus1 $cargo-hue)
                   (not (active ?terminus1))
                   (open-los? $area ?terminus1))
            (activate-receiver! ?terminus1))))
    ;; Check terminus2 for receiver
    (if (receiver ?terminus2)
      (do (connected ?cargo ?terminus2)
          (if (and $cargo-hue 
                   (not $color-conflict)
                   (chroma ?terminus2 $cargo-hue)
                   (not (active ?terminus2))
                   (open-los? $area ?terminus2))
            (activate-receiver! ?terminus2))))
    ;; Step 6: Handle cascading activations with ordered approach
    (if (and $cargo-hue (not $color-conflict))
      (chain-activate! ?cargo $cargo-hue $area))))


(define-update connect-3-and-color! (?cargo ?terminus1 ?terminus2 ?terminus3)
  (do 
    ;; Step 1: Release cargo and establish location
    (not (holds me1 ?cargo))
    (bind (loc me1 $area))
    ;; Step 2: Initialize color tracking
    (setq $cargo-hue nil)
    (setq $color-conflict nil)
    ;; Step 3: Connect to transmitters and handle coloring
    ;; Process terminus1 if transmitter
    (if (transmitter ?terminus1)
      (do (connected ?cargo ?terminus1)
          (setq $hue1 (hue-if-source? ?terminus1))
          (if $hue1
            (setq $cargo-hue $hue1))))
    ;; Process terminus2 if transmitter  
    (if (transmitter ?terminus2)
      (do (connected ?cargo ?terminus2)
          (setq $hue2 (hue-if-source? ?terminus2))
          (if $hue2
            (if (not $cargo-hue)
              (setq $cargo-hue $hue2)
              ;; Check for conflict with previously set color
              (if (not (eql $cargo-hue $hue2))
                (setq $color-conflict t))))))
    ;; Process terminus3 if transmitter
    (if (and (transmitter ?terminus3) (not $color-conflict))
      (do (connected ?cargo ?terminus3)
          (setq $hue3 (hue-if-source? ?terminus3))
          (if $hue3
            (if (not $cargo-hue)
              (setq $cargo-hue $hue3)
              (if (not (eql $cargo-hue $hue3))
                (setq $color-conflict t))))))
    ;; Step 4: Connect to active connectors (secondary power source)
    ;; Process terminus1 if active connector
    (if (and (connector ?terminus1) (bind (color ?terminus1 $conn1-hue)) (not $color-conflict))
      (do (connected ?cargo ?terminus1)
          (if (not $cargo-hue)
            (setq $cargo-hue $conn1-hue)
            (if (not (eql $cargo-hue $conn1-hue))
              (setq $color-conflict t)))))
    ;; Process terminus2 if active connector
    (if (and (connector ?terminus2) (bind (color ?terminus2 $conn2-hue)) (not $color-conflict))
      (do (connected ?cargo ?terminus2)
          (if (not $cargo-hue)
            (setq $cargo-hue $conn2-hue)
            (if (not (eql $cargo-hue $conn2-hue))
              (setq $color-conflict t)))))
    ;; Process terminus3 if active connector
    (if (and (connector ?terminus3) (bind (color ?terminus3 $conn3-hue)) (not $color-conflict))
      (do (connected ?cargo ?terminus3)
          (if (not $cargo-hue)
            (setq $cargo-hue $conn3-hue)
            (if (not (eql $cargo-hue $conn3-hue))
              (setq $color-conflict t)))))
    ;; Color the connector if we have a valid hue and no conflicts
    (if (and $cargo-hue (not $color-conflict))
      (color ?cargo $cargo-hue))
    ;; Step 5: Connect to receivers and activate if powered
    ;; Process terminus1 if receiver
    (if (receiver ?terminus1)
      (do (connected ?cargo ?terminus1)
          (if (and $cargo-hue 
                   (not $color-conflict)
                   (chroma ?terminus1 $cargo-hue)
                   (not (active ?terminus1))
                   (open-los? $area ?terminus1))
            (activate-receiver! ?terminus1))))
    ;; Process terminus2 if receiver
    (if (receiver ?terminus2)
      (do (connected ?cargo ?terminus2)
          (if (and $cargo-hue 
                   (not $color-conflict)
                   (chroma ?terminus2 $cargo-hue)
                   (not (active ?terminus2))
                   (open-los? $area ?terminus2))
            (activate-receiver! ?terminus2))))
    ;; Process terminus3 if receiver
    (if (receiver ?terminus3)
      (do (connected ?cargo ?terminus3)
          (if (and $cargo-hue 
                   (not $color-conflict)
                   (chroma ?terminus3 $cargo-hue)
                   (not (active ?terminus3))
                   (open-los? $area ?terminus3))
            (activate-receiver! ?terminus3))))
    ;; Step 6: Handle cascading activations
    (if (and $cargo-hue (not $color-conflict))
      (chain-activate! ?cargo $cargo-hue $area))))


(define-update chain-deactivate! (?connector ?hue)
  (do 
    ;; Step 1: Handle connected connectors FIRST (cascading deactivation)
    ;; Process these before receivers to prevent gate state interference
    ;; from affecting legitimate power path calculations
    (doall (?c connector)
      (if (and (connected ?connector ?c)
               (different ?c ?connector)
               (bind (color ?c $c-hue))
               (eql $c-hue ?hue))  ;; Only deactivate if same color
        ;; Check if this connector has alternative power sources
        (if (not (or 
                  ;; Check direct transmitter connection
                  (exists (?t transmitter)
                     (and (connected ?c ?t)
                          (chroma ?t ?hue)))
                  ;; Check connection to another active connector (not the one being deactivated)
                  (exists (?other-connector connector)
                     (and (different ?other-connector ?connector)
                          (different ?other-connector ?c)
                          (connected ?c ?other-connector)
                          (color ?other-connector ?hue)))))
            ;; No alternative power source found, recursively deactivate
            ;; Break connection before recursion to prevent infinite loops
            (do (not (connected ?connector ?c))
                (chain-deactivate! ?c ?hue)))))
    ;; Step 2: Handle receivers SECOND (after connector network has stabilized)
    ;; This prevents premature gate state changes from interfering with 
    ;; the connector deactivation cascade above
    (doall (?r receiver)
      (if (and (connected ?connector ?r)
               (active ?r)
               (chroma ?r ?hue))
        ;; Check if receiver still has power from other sources
        (if (not (exists (?c connector)
                    (and (different ?c ?connector)
                         (connected ?c ?r)
                         (color ?c ?hue))))
            ;; No alternative power source, deactivate receiver
            (deactivate-receiver! ?r))))
    ;; Step 3: Deactivate this connector LAST (after handling all dependencies)
    ;; This ensures all cascading effects are processed before removing
    ;; this connector's power contribution
    (not (color ?connector ?hue))))


;;;; ACTIONS ;;;;


#+ignore (define-action connect-to-1-terminus
    1
  (?terminus terminus)
  (and (bind (holds me1 $cargo))
       (connector $cargo)
       (bind (loc me1 $area))
       (connectable? $area ?terminus))
  ($cargo ?terminus $plate)
  (do 
    ;; Branch 1: Place connector without using any plate
    (assert (connect-1-and-color! $cargo ?terminus)
            (loc $cargo $area))
    ;; Branch 2: Place connector on each available plate (creates separate branches)
    (assert (doall (?plate plate)
              (if (and (locale ?plate $area)
                       (not (exists (?c cargo)
                              (on ?c ?plate))))
                ;; Reordering: activate plate BEFORE connection logic
                (do (loc $cargo $area)
                    (setq $plate ?plate)
                    (on $cargo ?plate)
                    (activate-plate! ?plate)
                    ;; NOW perform connection with gates in correct state
                    (connect-1-and-color! $cargo ?terminus)))))))


(define-action connect-to-2-terminus
    1
  (combination (?terminus1 ?terminus2) terminus)
  (and (bind (holds me1 $cargo))
       (connector $cargo)
       (bind (loc me1 $area))
       (connectable? $area ?terminus1)
       (connectable? $area ?terminus2))
  ($cargo ?terminus1 ?terminus2 $plate)
  (do ;; Branch 1: Place connector without using any plate
      (assert (connect-2-and-color! $cargo ?terminus1 ?terminus2)
              (loc $cargo $area))
      ;; Branch 2: Place connector on each available plate (creates separate branches)
      (assert (doall (?plate plate)
                (if (and (locale ?plate $area)
                         (not (exists (?c cargo)
                                (on ?c ?plate))))
                  ;; Critical reordering: activate plate BEFORE connection logic
                  (do (loc $cargo $area)           ;; Place cargo in area first
                      (setq $plate ?plate)         ;; Bind the plate variable
                      (on $cargo ?plate)           ;; Place cargo on the plate
                      (activate-plate! ?plate)     ;; Activate plate (opening/closing gates)
                      ;; NOW perform connections with gates in correct state
                      (connect-2-and-color! $cargo ?terminus1 ?terminus2)))))))
    

(define-action connect-to-3-terminus
    1
  (combination (?terminus1 ?terminus2 ?terminus3) terminus)
  (and (bind (holds me1 $cargo))
       (connector $cargo)
       (bind (loc me1 $area))
       (connectable? $area ?terminus1)
       (connectable? $area ?terminus2)
       (connectable? $area ?terminus3))
  ($cargo ?terminus1 ?terminus2 ?terminus3 $plate)
  (do ;; Branch 1: Place connector without using any plate
      (assert (connect-3-and-color! $cargo ?terminus1 ?terminus2 ?terminus3)
              (loc $cargo $area))
      ;; Branch 2: Separately, place connector on each available plate
      (assert (doall (?plate plate)
                (if (and (locale ?plate $area)
                         (not (exists (?c cargo)
                                (on ?c ?plate))))
                  ;; Critical ordering: activate plate BEFORE connection logic
                (do (loc $cargo $area)
                    (setq $plate ?plate)         ; Bind the plate variable  
                    (on $cargo ?plate)
                    (activate-plate! ?plate)
                    (connect-3-and-color! $cargo ?terminus1 ?terminus2 ?terminus3)))))))


(define-action pickup-connector
    1
  (?connector connector)
  (and (not (bind (holds me1 $cargo)))
       (bind (loc me1 $area))
       (loc ?connector $area))
  (?connector $area)
  (assert (holds me1 ?connector)
          (not (loc ?connector $area))
          ;; Handle plate deactivation FIRST if connector was on a plate
          (if (bind (on ?connector $plate))
            (do (not (on ?connector $plate))
                (deactivate-plate! $plate)))
          ;; Then handle power cascade deactivation
          (if (bind (color ?connector $hue))
            (chain-deactivate! ?connector $hue))
          ;; Finally disconnect from all terminus
          (doall (?t terminus)
            (if (connected ?connector ?t)
              (not (connected ?connector ?t))))))


#+ignore (define-action drop-cargo
    1
  ()
  (and (bind (loc me1 $area))  ;me1 is always located somewhere
       (bind (holds me1 $cargo)))  ;if not holding, then bind statement returns nil, otherwise binds $cargo
  ($cargo $area)
  (assert (not (holds me1 $cargo))
          (loc $cargo $area)))


#+ignore (define-action step-on-plate
    1
  (?plate plate)
  (and (not (bind (on me1 $plate)))
       (bind (loc me1 $area))
       (locale ?plate $area))
  (?plate)
  (assert (on me1 ?plate)
          (activate-plate! ?plate)))


#+ignore (define-action step-off-plate
    1
  ()
  (and (bind (on me1 $plate)))
  ($plate)
  (assert (not (on me1 $plate))
          (deactivate-plate! $plate)))


#+ignore (define-action put-on-plate
    1
  (?plate plate)
  (and (not (exists (?c cargo)
              (on ?c ?plate)))
       (bind (holds me1 $cargo))
       (bind (loc me1 $area))
       (locale ?plate $area))
  ($cargo ?plate)
  (assert (on $cargo ?plate)
          (not (holds me1 $cargo))
          (loc $cargo $area)
          (activate-plate! ?plate)))


#+ignore (define-action take-off-plate
    1
  (?cargo cargo ?plate plate)
  (and (on ?cargo ?plate)
       (not (bind (holds me1 $cargo)))
       (bind (loc me1 $area))
       (locale ?plate $area))
  (?cargo ?plate)
  (assert (not (on ?cargo ?plate))
          (holds me1 ?cargo)
          ;; Deactivate plate FIRST (consistent with pickup-connector)
          (deactivate-plate! ?plate)
          ;; Then handle connector power cascade deactivation
          (if (and (connector ?cargo)
                   (bind (color ?cargo $hue)))
            (chain-deactivate! ?cargo $hue))))


#+ignore (define-action jam-target
    1
  (?target target)
  (and (bind (holds me1 $jammer))
       (bind (loc me1 $area))
       (open-los? $area ?target))
  ($jammer ?target)
  (assert (jams $jammer ?target)
          (if (and (gate ?target)
                   (active ?target))
            (not (active ?target)))))


#+ignore (define-action pickup-jammer
    1
  (?jammer jammer)
  (and (not (bind (holds me1 $cargo)))
       (bind (loc me1 $area))
       (loc ?jammer $area))
  (?jammer $area)
  (assert (holds me1 ?jammer)
          (not (loc ?jammer $area))
          (if (bind (jams ?jammer $target))
            (do (not (jams ?jammer $target))
                (if (not (or (exists (?j jammer)
                               (and (different ?j ?jammer)
                                    (jams ?j $target)))
                             (and (gate $target)
                                  (all-controllers-active? $target))))
                  (active $target))))))
            

(define-action move-between-areas
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
  (loc me1 area1)
  (loc connector1 area1)
  (loc connector2 area1)
  (loc connector3 area1)
  (active gate1)
  (active gate2)
  (active gate3)

  ;static
  (locale transmitter1 area1)
  (locale transmitter2 area2)
  (locale receiver1 area1)
  (locale receiver2 area2)
  (locale receiver3 area3)
  (locale plate1 area1)
  (locale ladder1 area2)
  (locale ladder2 area3)
  (chroma transmitter1 red)
  (chroma transmitter2 blue)
  (chroma receiver1 blue)
  (chroma receiver2 blue)
  (chroma receiver3 red)
  (controls receiver1 gate1)
  (controls plate1 gate1)
  (controls receiver2 gate2)
  (controls receiver3 gate3)
  (separates gate1 area1 area2)
  (separates gate2 area2 area3)

  ;los is from an area to a fixed station
  (los0 area1 transmitter1)
  (los0 area1 transmitter2)
  (los0 area1 receiver1)
  (los0 area1 receiver3)  ;thru window
  (los1 area1 gate1 receiver2)
  (los0 area2 receiver2)
  (los1 area2 gate1 transmitter1)
  (los1 area2 gate1 transmitter2)
  (los0 area3 receiver3)
  (los0 area3 transmitter1)  ;thru window
  (los0 area3 transmitter2)  ;thru window
  (los2 area3 gate1 gate2 transmitter1)
  (los2 area3 gate1 gate2 transmitter2)
  
  ;visibility is a los from an area to an area 
  ;potentially containing a movable target or terminus
  (visible0 area1 area1)  ;same area
  (visible1 area1 gate1 area2)
  (visible0 area1 area3)  ;thru window
  (visible2 area1 gate1 gate2 area3)
  (visible1 area1 gate1 area2)
  (visible1 area1 gate2 area2)  ;thru window
  (visible2 area1 gate1 gate2 area3)
  (visible0 area2 area2)
  (visible1 area2 gate2 area3)
  (visible0 area3 area3)
)


;;;; GOAL ;;;;

(define-goal  ;always put this last
  (and (loc me1 area3)
       ;(loc connector3 area1)
       ;(connected connector3 transmitter2)
       ;(connected connector3 connector1)
       ;(color connector3 blue)
       (loc connector2 area3)
       (connected connector2 connector1)
       (connected connector2 transmitter1)
       (color connector1 blue)
))


;;;;;;;; Invariant Checks for Debugging ;;;;;;;;;;;;;;;

#+ignore (define-invariant holds-cargo-location ()
  ;Cargo cannot be both held and have a location simultaneously
  (not (and (bind (holds me1 $cargo))
            (bind (loc $cargo $area)))))


#+ignore (define-invariant receiver-activation ()
  ;A receiver is active if and only if there exists at least one
  ;connected, active connector of the same color.
  (doall (?r receiver)
    (if (bind (chroma ?r $r-hue))
      (equivalent (active ?r)
                  (exists (?c connector)
                    (and (connected ?c ?r)
                         (color ?c $r-hue)))))))


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
              (and (connected ?c ?t)
                   (chroma ?t $hue)))
            (exists (?other connector)
              (and (different ?other ?c)
                   (connected ?c ?other)    ; Connected to it
                   (color ?other $hue)))))))


#+ignore (define-invariant connector-self-connection ()
  ;No connector is connected to itself
  (doall (?c connector)
    (not (connected ?c ?c))))


#+ignore (define-invariant me1-has-location ()
  ;Me1 is always located in some area
  (bind (loc me1 $area)))


#+ignore (define-invariant connector-transmitter-source ()
  ;Every active connector ultimately traces to transmitter sources without cycles
  (let (($valid-source t)) ; Assume valid until proven otherwise
    ;; For each active connector, verify it has a valid transmitter source
    (doall (?c connector)
      (if (bind (color ?c $col))
        (do (setq $visited nil)           ; Track visited connectors
            (setq $source-found nil)      ; Flag if transmitter found
            (setq $stack nil)             ; DFS stack
            (setq $current ?c)            ; Current connector being examined
            (setq $color nil)             ; Color we're tracing
            ;; Get the color of this connector
            ;(bind (color ?c $col))
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
                      (and (connected $current ?t)
                           (chroma ?t $color)))
                  (setq $source-found t)
                  ;; Otherwise, add connected connectors of same color to stack
                  (doall (?other connector)
                    (if (and (different ?other $current)
                             (connected $current ?other)
                             (color ?other $color))
                      (push ?other $stack))))))
            ;; If we've explored all paths and never found a transmitter source,
            ;; this connector has no valid source (either disconnected or in a cycle)
            (if (not $source-found)
              (setq $valid-source nil)))))
    ;; Return result
    $valid-source))

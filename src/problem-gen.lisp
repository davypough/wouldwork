;;; Filename: problem-gen.lisp

;;; This is a generalization of functions for handling
;;; both lightsout+ and smallspace,
;;; using lightsout+ as the basis.


(in-package :ww)  ;required

(ww-set *problem-name* gen)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 20)


(define-types  ;types for smallspace
  me          (me1)
  gate        (gate1 gate2)
  plate       (nil)
  connector   (connector1 connector2)
  transmitter (transmitter1 transmitter2)
  receiver    (receiver1 receiver2)
  hue         (blue red)  ;the color of a transmitter, receiver, or active connector
  area        (area4 area5 area6 area7 area8)
  cargo       (either connector)  ;what an agent (me) can pickup & carry
  terminus    (either transmitter receiver connector)  ;what a connector can connect to
  fixture     (either transmitter receiver plate)  ;has a permanent locale in an area
  station     (either transmitter receiver gate)  ;useful for los determinations
)


(define-dynamic-relations  ;relations with fluents can be bound in rules--eg (bind (holds me1 $any-cargo))
  (holds me $cargo)  ;fluent because we need to sometimes lookup what is currently being held
  (loc (either me cargo) $area)
  (on (either me cargo) $plate)
  (connected terminus terminus)
  (active (either receiver plate gate))  ;a connector is implicitly active if colored
  (color connector $hue)
)


(define-static-relations
  (locale fixture $area)  ;locale is a fixed location, loc is dynamic
  (adjacent area area)
  (separates gate area area)
  (controls (either receiver plate) gate)
  (chroma (either transmitter receiver) $hue)
  ;potential los from an area to a transmitter or receiver
  (los0 area (either transmitter receiver))
  (los1 area $gate (either transmitter receiver))  ;line of sight from area thru gate to trans or rcvr
  (los2 area $gate $gate (either transmitter receiver))
  (visible0 area area)  ;visibility between areas is for connecting connectors
  (visible1 area $gate area)
  (visible2 area $gate $gate area)
)


;;;; HEURISTIC ;;;;


(defun area-distance-penalty (from-area to-area)
  ;; Simple area distance calculation based on problem topology
  ;; area1 -> area2 -> area3 (linear arrangement)
  (cond
    ((eql from-area to-area) 0)
    ((or (and (eql from-area 'area1) (eql to-area 'area2))
         (and (eql from-area 'area2) (eql to-area 'area1))) 1)
    ((or (and (eql from-area 'area2) (eql to-area 'area3))
         (and (eql from-area 'area3) (eql to-area 'area2))) 1)
    ((or (and (eql from-area 'area1) (eql to-area 'area3))
         (and (eql from-area 'area3) (eql to-area 'area1))) 2)
    (t 3))) ;; fallback for unexpected cases


#+ignore (define-query heuristic? ()
  ;; Goal-based heuristic: count unsatisfied goal conditions and add area distance penalties
  ;; Lower values indicate more promising states
  (do (setq $h-value 0)
      ;; Primary component: Unsatisfied goal conditions (weighted by importance)
      (if (not (loc me1 area2))
        (setq $h-value (+ $h-value 10)))
      (if (not (loc connector2 area3))
        (setq $h-value (+ $h-value 8)))
      (if (not (connected connector2 connector1))
        (setq $h-value (+ $h-value 6)))
      (if (not (connected connector2 transmitter1))
        (setq $h-value (+ $h-value 6)))
      (if (not (color connector1 blue))
        (setq $h-value (+ $h-value 4)))
      ;; Secondary component: Area distance penalties for misplaced objects
      (bind (loc me1 $me-area))
      (bind (loc connector2 $conn2-area))
      ;; Add distance penalty for me1 not in target area
      (if (not (eql $me-area 'area2))
        (setq $h-value (+ $h-value (area-distance-penalty $me-area 'area2))))
      ;; Add distance penalty for connector2 not in target area  
      (if (not (eql $conn2-area 'area3))
        (setq $h-value (+ $h-value (* 2 (area-distance-penalty $conn2-area 'area3)))))
      ;; Return the computed heuristic value
      $h-value))


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
       (or (adjacent ?area1 ?area2)
           (exists (?g gate)
             (and (separates ?g ?area1 ?area2)
                  (not (active ?g)))))))


(define-query valid-transmitter-hue? (?transmitter ?test-hue)
  ;; Returns compatible hue if valid, nil if conflict detected
  (do (bind (chroma ?transmitter $hue))
      (if (not ?test-hue)
        $hue                                ; First hue established  
        (if (eql ?test-hue $hue)
          ?test-hue                         ; Hues compatible
          nil))))                           ; Conflict detected


(define-query valid-connector-hue? (?connector ?test-hue)
  ;; Returns compatible hue if valid, nil if conflict detected
  (if (bind (color ?connector $hue))
    (if (not ?test-hue)
      $hue                                ; First hue established
      (if (eql ?test-hue $hue)
        ?test-hue                         ; Hues compatible
        nil))                             ; Conflict detected
    ?test-hue))                           ; No connector color, preserve test hue


(define-query valid-receiver-hue? (?receiver ?test-hue)
  ;; Returns test-hue if receiver can accept it, nil otherwise
  (if (and (chroma ?receiver ?test-hue)
           (not (active ?receiver)))
    ?test-hue
    nil))


(define-query compatible-transmitters? (?termini-list)
  ;; Returns hue from transmitters, nil if conflict or no transmitters
  (do (setq $compatible-hue nil)
      (ww-loop for $terminus in ?termini-list do
        (if (transmitter $terminus)
          (do (setq $candidate-hue (valid-transmitter-hue? $terminus $compatible-hue))
              (if $candidate-hue
                (setq $compatible-hue $candidate-hue)
                (return nil)))))  ;; Immediate return on conflict
      $compatible-hue))


(define-query compatible-connectors? (?termini-list ?transmitter-hue)
  ;; Returns compatible hue after processing connectors, nil if conflict  
  (do (setq $compatible-hue ?transmitter-hue)
      (ww-loop for $terminus in ?termini-list do
        (if (connector $terminus)
          (do (setq $candidate-hue (valid-connector-hue? $terminus $compatible-hue))
              (if $candidate-hue
                (setq $compatible-hue $candidate-hue)
                (return nil)))))  ;; Immediate return on conflict
      $compatible-hue))


;;;; UPDATE FUNCTIONS ;;;;


(define-update activate-receiver! (?receiver)
  (do (active ?receiver)
      (doall (?g gate)
        (if (and (controls ?receiver ?g)
                 #+ignore (not (exists (?j jammer)
                        (jams ?j ?g)))
                 (all-controllers-active? ?g))
          (not (active ?g))))))


(define-update activate-compatible-receivers! (?termini-list ?hue ?area)
  ;; Activates all receivers in ?termini-list that are compatible with ?hue
  (if ?hue
    (ww-loop for $terminus in ?termini-list do
      (if (and (receiver $terminus)
               (valid-receiver-hue? $terminus ?hue)
               (open-los? ?area $terminus))
        (activate-receiver! $terminus)))))


(define-update deactivate-receiver! (?receiver)
  (do (not (active ?receiver))
      (doall (?g gate)
        (if (controls ?receiver ?g)
          (active ?g)))))


(define-update activate-plate! (?plate)
  (do (active ?plate)
      (doall (?g gate)
        (if (and (controls ?plate ?g)
                 (all-controllers-active? ?g))
          (not (active ?g))))))


(define-update deactivate-plate! (?plate)
  (do (not (active ?plate))
      (doall (?g gate)
        (if (controls ?plate ?g)
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


(define-update propagate-connect-to-1-terminus! (?cargo ?terminus)
  ; Propagates a source to a connector (?cargo)
  (do (setq $hue (hue-if-source? ?terminus))
      (if $hue
        (color ?cargo $hue))))


(define-update propagate-connect-to-2-terminus! (?cargo ?terminus1 ?terminus2 ?area)
  ; Propagates sources to a connector (?cargo)
  (do 
    ;; Create terminus list for uniform processing
    (setq $termini-list (list ?terminus1 ?terminus2))
    ;; Process transmitters with built-in conflict detection
    (setq $transmitter-hue (compatible-transmitters? $termini-list))
    ;; Process connectors, maintaining transmitter hue or detecting conflicts
    (setq $final-hue (compatible-connectors? $termini-list $transmitter-hue))
    ;; Color cargo if we have a valid hue (no conflicts detected)
    (if $final-hue
        (color ?cargo $final-hue))
    ;; Activate all compatible receivers uniformly
    (activate-compatible-receivers! $termini-list $final-hue ?area)
    ;; Perform cascading activation if successful
    (if $final-hue
        (chain-activate! ?cargo $final-hue ?area))))


(define-update propagate-connect-to-3-terminus! (?cargo ?terminus1 ?terminus2 ?terminus3 ?area)
  ; Propagates source terminus to a connector (?cargo)
  (do 
    ;; Create terminus list for uniform processing
    (setq $termini-list (list ?terminus1 ?terminus2 ?terminus3))
    ;; Process transmitters with built-in conflict detection
    (setq $transmitter-hue (compatible-transmitters? $termini-list))
    ;; Process connectors, maintaining transmitter hue or detecting conflicts
    (setq $final-hue (compatible-connectors? $termini-list $transmitter-hue))
    ;; Color cargo if we have a valid hue (no conflicts detected)
    (if $final-hue
        (color ?cargo $final-hue))
    ;; Activate all compatible receivers uniformly
    (activate-compatible-receivers! $termini-list $final-hue ?area)
    ;; Perform cascading activation if successful
    (if $final-hue
        (chain-activate! ?cargo $final-hue ?area))))


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


(define-action connect-to-1-terminus
    1
  (?terminus terminus)
  (and (bind (holds me1 $cargo))
       (connector $cargo)
       (bind (loc me1 $area))
       (connectable? $area ?terminus))
  ($cargo ?terminus $plate)
  (do ;; Place connector on the ground; immediate changes first, then propagate those changes
      (assert (not (holds me1 $cargo))
              (loc $cargo $area)
              (connected $cargo ?terminus)
              (propagate-connect-to-1-terminus! $cargo ?terminus))
      ;; Place connector on each available plate
      (doall (?plate plate)
        (if (and (locale ?plate $area)
                 (not (exists (?c cargo)
                        (on ?c ?plate))))
          (assert (not (holds me1 $cargo))
                  (loc $cargo $area)
                  (connected $cargo ?terminus)
                  (on $cargo ?plate)
                  (setq $plate ?plate)
                  (activate-plate! ?plate)
                  (propagate-connect-to-1-terminus! $cargo ?terminus))))))


(define-action connect-to-2-terminus
    1
  (combination (?terminus1 ?terminus2) terminus)
  (and (bind (holds me1 $cargo))
       (connector $cargo)
       (bind (loc me1 $area))
       (connectable? $area ?terminus1)
       (connectable? $area ?terminus2))
  ($cargo ?terminus1 ?terminus2 $plate)
  (do ;; Place connector on the ground
      (assert (not (holds me1 $cargo))
              (loc $cargo $area)
              (connected $cargo ?terminus1)
              (connected $cargo ?terminus2)
              (propagate-connect-to-2-terminus! $cargo ?terminus1 ?terminus2 $area))
      ;; Place connector on each available plate
      (doall (?plate plate)
        (if (and (locale ?plate $area)
                 (not (exists (?c cargo)
                        (on ?c ?plate))))
          (assert (not (holds me1 $cargo))
                  (loc $cargo $area)
                  (connected $cargo ?terminus1)
                  (connected $cargo ?terminus2)
                  (on $cargo ?plate)
                  (setq $plate ?plate)
                  (activate-plate! ?plate)
                  (propagate-connect-to-2-terminus! $cargo ?terminus1 ?terminus2 $area))))))
    

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
  (do ;; Place connector on the ground
      (assert (not (holds me1 $cargo))
              (loc $cargo $area)
              (connected $cargo ?terminus1)
              (connected $cargo ?terminus2)
              (connected $cargo ?terminus3)
              (propagate-connect-to-3-terminus! $cargo ?terminus1 ?terminus2 ?terminus3 $area))
      ;; Place connector on each available plate
      (doall (?plate plate)
        (if (and (locale ?plate $area)
                 (not (exists (?c cargo)
                        (on ?c ?plate))))
          (assert (not (holds me1 $cargo))
                  (loc $cargo $area)
                  (connected $cargo ?terminus1)
                  (connected $cargo ?terminus2)
                  (connected $cargo ?terminus3)
                  (on $cargo ?plate)
                  (setq $plate ?plate)
                  (activate-plate! ?plate)
                  (propagate-connect-to-3-terminus! $cargo ?terminus1 ?terminus2 ?terminus3 $area))))))


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


(define-action drop-cargo
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


(define-init  ;init for smallspace
  ;dynamic
  (loc me1 area5)
  (loc connector1 area5)
  (loc connector2 area7)
  (active gate1)
  (active gate2)
  ;static
  (adjacent area4 area5)
  (adjacent area6 area7)
  (locale transmitter1 area4)
  (locale transmitter2 area6)
  (locale receiver1 area4)
  (locale receiver2 area8)
  (chroma transmitter1 blue)
  (chroma transmitter2 red)
  (chroma receiver1 blue)
  (chroma receiver2 red)
  (controls receiver1 gate1)
  (controls receiver2 gate2)
  (separates gate1 area4 area7)
  (separates gate2 area7 area8)

  ;los is from an area to a fixed station
  (los0 area4 transmitter1)
  (los0 area5 transmitter1)
  (los0 area6 transmitter1)
  (los0 area8 transmitter1)
  (los1 area7 gate1 transmitter1)
  (los0 area4 receiver1)
  (los0 area5 receiver1)
  (los0 area5 receiver2)
  (los0 area8 receiver2)
  (los1 area7 gate2 receiver2)
  (los0 area6 transmitter2)
  (los0 area7 transmitter2)
  (los1 area8 gate2 transmitter2)
  (los2 area4 gate1 gate2 receiver2)

  ;visibility is from an area to an area 
  ;potentially containing a movable target or terminus
  (visible0 area4 area6)
  (visible0 area4 area8)
  (visible0 area5 area6)
  (visible0 area5 area8)
  (visible1 area4 gate1 area6) 
  (visible1 area4 gate1 area7) 
  (visible1 area5 gate1 area6) 
  (visible1 area5 gate1 area7)
  (visible1 area6 gate2 area8)
  (visible1 area7 gate2 area8)
  (visible2 area4 gate1 gate2 area8)
)


#+ignore (define-init  ;init for lightsout+
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


(define-goal  ;goal for smallspace
  (loc me1 area8))


#+ignore (define-goal  ;goal for lightsout+
  (and (loc connector1 area1)
       (on connector1 plate1)
       (connected connector1 transmitter2)
       (connected connector1 receiver1)
       (connected connector1 receiver2)
       (loc connector2 area3)
       (connected connector2 transmitter2)
       (connected connector2 connector1)
       (loc connector3 area1)
       (connected connector3 transmitter2)
       (connected connector3 receiver1)
       (loc me1 area1)
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

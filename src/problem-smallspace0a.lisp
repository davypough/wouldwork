;;; Filename: problem-smallspace0a.lisp


;;; Problem specification (in Talos Principle)
;;; for the small space problem in Road to Gehenna sigil 
;;; dome. First leg to area8.
;;; Uses mixed fluent & non-fluent relations
;;; Reduces number of areas to 3, adds windows


(in-package :ww)  ;required

(ww-set *problem-name* smallspace0a)

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
  area        (area1 area2 area3)
  cargo       (either connector jammer box fan)  ;what an agent (me) can pickup & carry
  target      (either gate gears gun)  ;what a jammer can jam
  divider     (either barrier gate)  ;can impede movement
  terminus    (either transmitter receiver connector)  ;what a connector can connect to
  fixture     (either transmitter receiver gears ladder rostrum)
  station     (either fixture gate)  ;useful for los determinations
  support     (either box rostrum)
)


(define-dynamic-relations  ;relations with fluents can be bound in rules--eg (bind (holds me1 $any-cargo))
  (holds me $cargo)  ;fluent because we need to sometimes lookup what is currently being held
  (loc (either me cargo) $area)
  ;(on (either me cargo) $support)
  ;(attached fan gears)
  ;(jams jammer $target)
  (connected terminus terminus)
  (active (either receiver gate switch gun gears))
  (color connector $hue)
)


(define-static-relations
  (locale fixture area)  ;locale is a fixed location, loc is dynamic
  (separates divider area area)
  ;(climbable> ladder area area)
  ;(height support $real)
  (controls receiver gate)
  (chroma (either transmitter receiver) $hue)
  ;clear los from an area to a gate/fixture
  (los0 area (either gate fixture))
  (los1 area gate (either gate fixture))
  (los2 area gate gate (either gate fixture))
  (visible0 area area)  
  (visible1 area gate area)
  (visible2 area gate gate area)
)


;;;; QUERY FUNCTIONS ;;;;


(define-query hue-if-source? (?terminus)
  ;gets the hue of ?terminus if it is a source, else nil
  (or (and (transmitter ?terminus)
           (bind (chroma ?terminus $hue))
           $hue)
      (and (connector ?terminus)
           (bind (color ?terminus $hue))
           $hue)))


(define-query open-los-thru-1-gate? (?area ?station)
  (exists (?g gate)
    (and (los1 ?area ?g ?station)
         (not (active ?g)))))


(define-query open-los-thru-2-gates? (?area ?station)
  (exists ((?g1 ?g2) gate)
    (and (los2 ?area ?g1 ?g2 ?station)
         (not (active ?g1))
         (not (active ?g2)))))


(define-query open-los? (?area ?station)
  (or (los0 ?area ?station)
      (open-los-thru-1-gate? ?area ?station)
      (open-los-thru-2-gates? ?area ?station)))


(define-query visible-thru-1-gate? (?area1 ?area2)
  (exists (?g gate)
    (and (visible1 ?area1 ?g ?area2)
         (not (active ?g)))))


(define-query visible-thru-2-gates? (?area1 ?area2)
  (exists ((?g1 ?g2) gate)
    (and (visible2 ?area1 ?g1 ?g2 ?area2)
         (not (active ?g1))
         (not (active ?g2)))))


(define-query visible? (?area1 ?area2)
  (or (visible0 ?area1 ?area2)  ;directly visible
      (visible-thru-1-gate? ?area1 ?area2)
      (visible-thru-2-gates? ?area1 ?area2)))


(define-query connectable? (?area ?terminus)
  (or (open-los? ?area ?terminus)  ;from connector in area to terminus
      (and (connector ?terminus)
           (exists (?a area)
             (and (loc ?terminus ?a)
                  (visible? ?area ?a))))))


(define-query passable? (?area1 ?area2)
  (or (exists (?b barrier)
        (and (separates ?b ?area1 ?area2)
             (not (bind (holds me1 $cargo)))))  ;must drop cargo first
      (exists (?g gate)
        (and (separates ?g ?area1 ?area2)
             (not (active ?g))))))


;;;; UPDATE FUNCTIONS ;;;;


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
  (do (color ?connector ?hue)
      (doall (?r receiver)
        (if (and (connected ?connector ?r)
                 (not (active ?r))
                 (bind (chroma ?r $r-hue))
                 (eql $r-hue ?hue))
          (activate-receiver! ?r)))
      (doall (?c connector)
        (if (and (different ?c ?connector)
                 (connected ?connector ?c)
                 (not (bind (color ?c $c-hue))))
          (chain-activate! ?c ?hue)))))


(define-update chain-deactivate! (?connector ?hue)
  (do 
    ;; Step 1: Deactivate this connector
    (not (color ?connector ?hue))
    ;; Step 2: Deactivate receivers that lost power
    (doall (?r receiver)
      (if (and (connected ?connector ?r)
               (not (exists (?c connector)
                      (and (different ?c ?connector)
                           (connected ?c ?r)
                           (bind (color ?c $c-hue))
                           (eql $c-hue ?hue)))))
        (deactivate-receiver! ?r)))
    ;; Step 3: For each connected connector, check ALL possible power paths
    ;; Including direct transmission paths and paths through other active connectors
    (doall (?c connector)
      (if (and (connected ?connector ?c)
               (different ?c ?connector))  ;; Prevent direct self-reference
        ;; Start a careful power source check
        (if (not (or 
                  ;; Check direct transmitter connection
                  (exists (?t transmitter)
                     (and (connected ?c ?t)
                          (bind (chroma ?t $t-hue))
                          (eql $t-hue ?hue)))
                  ;; Check connection to another active connector (not the one being picked up)
                  (exists (?other-connector connector)
                     (and (different ?other-connector ?connector)
                          (different ?other-connector ?c)
                          (connected ?c ?other-connector)
                          (bind (color ?other-connector $other-hue))
                          (eql $other-hue ?hue)))))
            ;; No alternative power source found, recursively deactivate
            (do
              ;; Break the connection before recursion to avoid infinite recursion
              (not (connected ?connector ?c))
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
          (connected $cargo ?terminus)
          (setq $hue (hue-if-source? ?terminus))
          (if $hue
            (color $cargo $hue))))


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
          (connected $cargo ?terminus1)
          (connected $cargo ?terminus2)
          (setq $hue1 (hue-if-source? ?terminus1))
          (setq $hue2 (hue-if-source? ?terminus2))
          (unless (and $hue1 $hue2 (not (eql $hue1 $hue2)))
            (setq $hue (or $hue1 $hue2)))
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
          (connected $cargo ?terminus1)
          (connected $cargo ?terminus2)
          (connected $cargo ?terminus3)
          (setq $hue1 (hue-if-source? ?terminus1))
          (setq $hue2 (hue-if-source? ?terminus2))
          (setq $hue3 (hue-if-source? ?terminus3))
          (unless (or (and $hue1 $hue2 (not (eql $hue1 $hue2)))  ; hue1 vs hue2 conflict
                      (and $hue1 $hue3 (not (eql $hue1 $hue3)))  ; hue1 vs hue3 conflict  
                      (and $hue2 $hue3 (not (eql $hue2 $hue3))))  ; hue2 vs hue3 conflict
            (setq $hue (or $hue1 $hue2 $hue3)))
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
            (if (connected ?connector ?t)
              (not (connected ?connector ?t))))))


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
  (loc me1 area1)
  (loc connector1 area1)
  (loc connector2 area2)
  (active gate1)
  (active gate2)

  ;static
  (locale transmitter1 area1)
  (locale transmitter2 area2)
  (locale receiver1 area1)
  (locale receiver2 area3)
  (chroma transmitter1 blue)
  (chroma transmitter2 red)
  (chroma receiver1 blue)
  (chroma receiver2 red)
  (controls receiver1 gate1)
  (controls receiver2 gate2)
  (separates gate1 area1 area2)
  (separates gate2 area2 area3)

  ;los is from an area to a fixed station
  (los0 area1 transmitter1)
  (los0 area2 transmitter1)  ;thru window
  (los0 area3 transmitter1)
  (los0 area2 transmitter2)
  (los0 area1 receiver1)
  (los0 area3 receiver2)
  (los0 area1 receiver2)
  (los1 area2 gate1 transmitter1)
  (los1 area2 gate2 receiver2)
  (los1 area3 gate2 transmitter2)
  (los2 area1 gate1 gate2 receiver2)

  ;visibility is a los from an area to an area 
  ;potentially containing a movable target or terminus
  (visible0 area1 area1)  ;same area
  (visible0 area1 area2)  ;thru window
  (visible0 area1 area3)  ;thru window
  (visible1 area1 gate1 area2)  ;but already have thru window, needed?
  (visible1 area1 gate2 area2)
  (visible2 area1 gate1 gate2 area3)
  (visible0 area2 area2)
  (visible1 area2 gate2 area3)
  (visible0 area3 area3)
)


;;;; GOAL ;;;;

(define-goal  ;always put this last
  (loc me1 area3))


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
                         (bind (color ?c $c-hue))
                         (eql $c-hue $r-hue)))))))


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
                   (bind (chroma ?t $t-hue))
                   (eql $t-hue $hue)))
            (exists (?other connector)
              (and (different ?other ?c)
                   (connected ?c ?other)    ; Connected to it
                   (bind (color ?other $other-hue))
                   (eql $other-hue $hue)))))))


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
                           (bind (chroma ?t $t-color))
                           (eql $t-color $color)))
                  (setq $source-found t)
                  ;; Otherwise, add connected connectors of same color to stack
                  (doall (?other connector)
                    (if (and (different ?other $current)
                             (connected $current ?other)
                             (bind (color ?other $other-color))
                             (eql $other-color $color))
                      (push ?other $stack))))))
            ;; If we've explored all paths and never found a transmitter source,
            ;; this connector has no valid source (either disconnected or in a cycle)
            (if (not $source-found)
              (setq $valid-source nil)))))
    ;; Return result
    $valid-source))

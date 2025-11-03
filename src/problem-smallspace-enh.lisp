;;; Filename: problem-smallspace-enh.lisp


;;; Enhanced topological representation for connectivity.
;;; Connectivity potential to areas (with visibility) and fixtures (with los).
;;; First leg to area8.


(in-package :ww)  ;required

(ww-set *problem-name* smallspace-enh)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 15)


(define-types
  agent       (agent1)
  gate        (gate1 gate2)
  connector   (connector1 connector2)
  transmitter (transmitter1 transmitter2)
  receiver    (receiver1 receiver2)
  hue         (blue red nil)  ;the color of a transmitter, receiver, or connector
  area        (area1 area2 area3)
  cargo       (either connector)  ;what an agent can pickup & carry
  terminus    (either transmitter receiver connector)  ;what a connector can connect to
  fixture     (either transmitter receiver)
)


(define-dynamic-relations  ;relations with fluents can be bound in rules--eg (bind (holds agent1 $any-cargo))
  (holds agent $cargo)  ;fluent because we need to sometimes lookup what is currently being held
  (loc (either agent cargo) $area)
  (connected terminus terminus)
  (active receiver)
  (open gate)
  (color connector $hue)
)


(define-static-relations
  (controls receiver gate)
  (chroma terminus $hue)
  ;clear los from an area to a fixture
  (los0 area fixture)  
  (los1 area gate fixture)
  (los2 area gate gate fixture)
  ;could see from an area to another area
  (visible0 area area)  
  (visible1 area gate area)
  (visible2 area gate gate area)
  ;could move from an area to another area
  (accessible0 area area)
  (accessible1 area gate area)
  (accessible2 area gate gate area)
)

;;;; QUERY FUNCTIONS ;;;;


(define-query get-hue-if-source (?terminus)
  ;gets the hue of ?terminus if it is a source, else nil
  (or (and (transmitter ?terminus)
           (bind (chroma ?terminus $hue))
           $hue)
      (and (connector ?terminus)
           (bind (color ?terminus $hue))
           $hue))
)


(define-query los (?area ?fixture)
  (or (los0 ?area ?fixture)
      (exists (?g gate)
        (and (los1 ?area ?g ?fixture)
             (open ?g)))
      (exists ((?g1 ?g2) gate)
        (and (los2 ?area ?g1 ?g2 ?fixture)
             (open ?g1)
             (open ?g2))))
)


(define-query visible (?area1 ?area2)
  (or (visible0 ?area1 ?area2)
      (exists (?g gate)
        (and (visible1 ?area1 ?g ?area2)
             (open ?g)))
      (exists ((?g1 ?g2) gate)
        (and (visible2 ?area1 ?g1 ?g2 ?area2)
             (open ?g1)
             (open ?g2))))
)
      

(define-query accessible (?area1 ?area2)
  (or (accessible0 ?area1 ?area2)
      (exists (?g gate)
        (and (accessible1 ?area1 ?g ?area2)
             (open ?g)))
      (exists ((?g1 ?g2) gate)
        (and (accessible2 ?area1 ?g1 ?g2 ?area2)
             (open ?g1)
             (open ?g2))))
)


(define-query connectable (?area ?terminus)
  (or (and (fixture ?terminus)
           (los ?area ?terminus))
      (and (connector ?terminus)
           (bind (loc ?terminus $area))
           (visible ?area $area)))
)


;;;; UPDATE FUNCTIONS ;;;;


(define-update activate-receiver! (?receiver)
  (do (active ?receiver)
      (doall (?g gate)
        (if (controls ?receiver ?g)
          (open ?g))))
)


(define-update deactivate-receiver! (?receiver)
  (do (not (active ?receiver))
      (doall (?g gate)
        (if (controls ?receiver ?g)
          (not (open ?g)))))
)


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
                 (color ?c nil))
          (chain-activate! ?c ?hue))))
)



(define-update chain-deactivate! (?connector ?hue)
  (do ;; Step 1: Deactivate this connector
      (color ?connector nil)
      ;; Step 2: Deactivate receivers that lost power
      (doall (?r receiver)
        (if (and (connected ?connector ?r)
                 (bind (chroma ?r $r-hue))
                 (eql $r-hue ?hue)
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
                 (different ?c ?connector)  ;; Prevent direct self-reference
                 (bind (color ?c $c-hue))
                 (eql $c-hue ?hue))
          ;; Start a careful power source check
          (if (not (or ;; Check direct transmitter connection
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
            (do ;; Break the connection before recursion to avoid infinite recursion
                (not (connected ?connector ?c))
                (chain-deactivate! ?c ?hue))))))
)


;;;; ACTIONS ;;;;


(define-action connect-to-1-terminus
  1
  (?terminus terminus)
  (and (bind (holds agent1 $cargo))
       (connector $cargo)
       (bind (loc agent1 $area))
       (connectable $area ?terminus))
  ($cargo ?terminus $area $hue)
  (assert (not (holds agent1 $cargo))
          (loc $cargo $area)
          (connected $cargo ?terminus)
          (setq $hue (get-hue-if-source ?terminus))
          (if $hue
            (color $cargo $hue)))
)


(define-action connect-to-2-terminus
  1
  (combination (?terminus1 ?terminus2) terminus)
  (and (bind (holds agent1 $cargo))
       (connector $cargo)
       (bind (loc agent1 $area))
       (connectable $area ?terminus1)
       (connectable $area ?terminus2))
  ($cargo ?terminus1 ?terminus2 $area $hue)
  (assert (not (holds agent1 $cargo))
          (loc $cargo $area)
          (connected $cargo ?terminus1)
          (connected $cargo ?terminus2)
          (setq $hue1 (get-hue-if-source ?terminus1))
          (setq $hue2 (get-hue-if-source ?terminus2))
          (unless (and $hue1 $hue2 (not (eq $hue1 $hue2)))
            (setq $hue (or $hue1 $hue2)))
          (if $hue
            (chain-activate! $cargo $hue)))
)


(define-action connect-to-3-terminus
  1
  (combination (?terminus1 ?terminus2 ?terminus3) terminus)
  (and (bind (holds agent1 $cargo))
       (connector $cargo)
       (bind (loc agent1 $area))
       (connectable $area ?terminus1)
       (connectable $area ?terminus2)
       (connectable $area ?terminus3))
  ($cargo ?terminus1 ?terminus2 ?terminus3 $area $hue)
  (assert (not (holds agent1 $cargo))
          (loc $cargo $area)
          (connected $cargo ?terminus1)
          (connected $cargo ?terminus2)
          (connected $cargo ?terminus3)
          (setq $hue1 (get-hue-if-source ?terminus1))
          (setq $hue2 (get-hue-if-source ?terminus2))
          (setq $hue3 (get-hue-if-source ?terminus3))
          (unless (or (and $hue1 $hue2 (not (eql $hue1 $hue2)))  ; hue1 vs hue2 conflict
                      (and $hue1 $hue3 (not (eql $hue1 $hue3)))  ; hue1 vs hue3 conflict  
                      (and $hue2 $hue3 (not (eql $hue2 $hue3))))  ; hue2 vs hue3 conflict
            (setq $hue (or $hue1 $hue2 $hue3)))
          (if $hue
            (chain-activate! $cargo $hue)))
)


(define-action pickup-connector
  1
  (?connector connector)
  (and (not (bind (holds agent1 $cargo)))
       (bind (loc agent1 $area))
       (loc ?connector $area))
  (?connector $area)
  (assert (holds agent1 ?connector)
          (not (loc ?connector $area))
          (bind (color ?connector $hue))
          (if $hue
            (chain-deactivate! ?connector $hue))
          ;; Finally disconnect this picked up connector from everything
          (doall (?t terminus)
            (if (connected ?connector ?t)
              (not (connected ?connector ?t)))))
)


(define-action drop
    1
  ()
  (and (bind (loc agent1 $area))  ;agent1 is always located somewhere
       (bind (holds agent1 $cargo)))  ;if not holding, then bind statement returns nil, otherwise binds $cargo
  ($cargo $area)
  (assert (not (holds agent1 $cargo))
          (loc $cargo $area))
)


(define-action move
  1
  (?area2 area)
  (and (bind (loc agent1 $area1))
       (different $area1 ?area2)
       (accessible $area1 ?area2))
  ($area1 ?area2)
  (assert (loc agent1 ?area2))
)


;;;; INITIALIZATION ;;;;


(define-init
  ;dynamic
  (loc agent1 area1)
  (loc connector1 area1)
  (loc connector2 area2)
  (color connector1 nil)
  (color connector2 nil)
  ;static
  (accessible1 area1 gate1 area2)
  (accessible2 area1 gate1 gate2 area3)
  (accessible1 area2 gate2 area3) 
  (chroma transmitter1 blue)
  (chroma transmitter2 red)
  (chroma receiver1 blue)
  (chroma receiver2 red)
  (controls receiver1 gate1)
  (controls receiver2 gate2)
  ;los is from an area to a fixture
  (los0 area1 transmitter1)
  (los0 area1 receiver1)
  (los0 area1 receiver2)
  (los0 area2 transmitter2)
  (los0 area2 transmitter1)
  (los1 area2 gate2 receiver2)
  (los0 area3 receiver2)
  (los0 area3 transmitter1)
  (los1 area3 gate2 transmitter2)
  ;visibility is from an area to an area 
  (visible0 area1 area2)
  (visible0 area1 area3)
  (visible1 area2 gate2 area3)
)


;;;; GOAL ;;;;

(define-goal  ;always put this last
  (loc agent1 area3)
)


;;;;;;;; Invariant Checks for Debugging ;;;;;;;;;;;;;;;

#+ignore (define-invariant holds-cargo-location ()
  ;Cargo cannot be both held and have a location simultaneously
  (not (and (bind (holds agent1 $cargo))
            (bind (loc $cargo $area))))
)


#+ignore (define-invariant receiver-activation ()
  ;A receiver is active if and only if there exists at least one
  ;connected, active connector of the same color.
  (doall (?r receiver)
    (if (bind (color ?r $rhue))
      (equivalent (active ?r)
                  (exists (?c connector)
                    (and (connected ?c ?r)
                         (active ?c)
                         (bind (color ?c $chue))
                         (eql $chue $rhue))))))
)


#+ignore (define-invariant receiver-gate-control ()
  ;A receiver is active if and only if all gates it controls are inactive
  (doall (?r receiver)
    (if (exists (?g gate)
          (controls ?r ?g))
      (equivalent (active ?r)
                  (forall (?g gate)
                    (if (controls ?r ?g)
                      (not (active ?g)))))))
)


#+ignore (define-invariant colored-connector-connection ()
  ;Any colored connector must have a valid source with matching color,
  ;either a transmitter or another connector
  (doall (?c connector)
    (if (bind (color ?c $hue))
        (or (exists (?t transmitter)
              (and (connected ?c ?t)
                   (bind (color ?t $t-hue))
                   (eql $t-hue $hue)))
            (exists (?other connector)
              (and (different ?other ?c)
                   (connected ?c ?other)    ; Connected to it
                   (bind (color ?other $other-hue))
                   (eql $other-hue $hue))))))
)


#+ignore (define-invariant connector-self-connection ()
  ;No connector is connected to itself
  (doall (?c connector)
    (not (connected ?c ?c)))
)


#+ignore (define-invariant agent1-has-location ()
  ;agent1 is always located in some area
  (bind (loc agent1 $area))
)


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
                      (and (connected $current ?t)
                           (bind (color ?t $t-color))
                           (eql $t-color $color)))
                  (setq $source-found t)
                  ;; Otherwise, add connected connectors of same color to stack
                  (doall (?other connector)
                    (if (and (different ?other $current)
                             (connected $current ?other)
                             (active ?other)
                             (bind (color ?other $other-color))
                             (eql $other-color $color))
                      (push ?other $stack))))))
            ;; If we've explored all paths and never found a transmitter source,
            ;; this connector has no valid source (either disconnected or in a cycle)
            (if (not $source-found)
              (setq $valid-source nil)))))
    ;; Return result
    $valid-source)
)


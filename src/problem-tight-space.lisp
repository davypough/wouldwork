;;; Filename: problem-tight-space.lisp


;;; Problem specification (in Talos Principle)
;;; for the tight space problem in The Lost Prisoner.
;;; Only topology (relations) of layout is needed (not actual coordinates)
;;; if there are no connectors. But this problem has connectors.


(in-package :ww)  ;required

(ww-set *problem-name* tight-space)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 20)


(define-types
  me          (me1)  ;the name of the agent
  gate        (gate1)
  connector   (connector1 connector2 connector3)
  transmitter (transmitter1 transmitter2)
  receiver    (receiver1 receiver2 receiver3)
  hue         (blue red)  ;the color of a transmitter, receiver, or active connector
  area        (area1 area2 area3 area4 area5)
  gate-status (open closed)
  receiver-status (active inactive)
  beam-status (active inactive)
  cargo       (either connector)  ;what an agent (me1) can pickup & carry
  terminus    (either transmitter receiver connector)  ;what a connector can connect to
  fixture     (either transmitter receiver)
)


(define-dynamic-relations  ;relations with fluents can be bound in rules--eg (bind (holds me1 $any-cargo))
  (holds me $cargo)  ;fluent because we need to sometimes lookup what is currently being held
  (loc (either me cargo) $area)  ;a location in an area
  (pos (either me cargo) $fixnum $fixnum)  ;a position in grid coordinates (x,y)
  (paired terminus terminus $beam-status)  ;potential beam between terminus if not blocked
  (gate-status gate $gate-status)
  (receiver-status receiver $receiver-status)
  (color connector $hue)
  (beam-segment terminus terminus $fixnum $fixnum)  ;ie, (beam-segment source target endpoint-x endpoint-y)
)


(define-static-relations
  (adjacent area area)   ;agent can always move to adjacent area unimpeded
  (centroid area $fixnum $fixnum)  ;approximate center of an area
  (controls receiver gate)
  (gate-separates gate area area)
  (fixpoint fixture $fixnum $fixnum)  ;coordinates of a transmitter or receiver
  (gate-segment gate $fixnum $fixnum $fixnum $fixnum)
  (chroma fixture $hue)
  (los0 area fixture)  ;direct line of sight to a fixture
  (los1 area gate fixture)  ;line of sight thru a gate to a fixture
  (visible0 area area)  ;visibility between areas to a connector
  (visible1 area gate area)  ;visibility thru a gate
)


;;;; QUERY FUNCTIONS ;;;;


(define-query get-source-hue? (?terminus)
  ;Returns color or chroma if source, otherwise nil
  (do (or (and (transmitter ?terminus)
               (bind (chroma ?terminus $hue)))
          (and (connector ?terminus)
               (bind (color ?terminus $hue))))
      $hue))


(define-query los-thru-1-gate? (?area ?fixture)
  (exists (?g gate)
    (and (los1 ?area ?g ?fixture)
         (gate-status ?g open))))


(define-query los? (?area ?fixture)
  (or (los0 ?area ?fixture)
      (los-thru-1-gate? ?area ?fixture)))


(define-query visible-thru-1-gate? (?area1 ?area2)
  (exists (?g gate)
    (and (visible1 ?area1 ?g ?area2)
         (gate-status ?g open))))


(define-query visible? (?area1 ?area2)
  (or (visible0 ?area1 ?area2)
      (visible-thru-1-gate? ?area1 ?area2)))


(define-query connector-has-valid-line-of-sight? (?connector ?hue)
  (or 
    ;; Check direct transmitter connection with current line-of-sight
    (exists (?t transmitter)
       (and (paired ?connector ?t)
            (bind (chroma ?t $t-hue))
            (eql $t-hue ?hue)
            (bind (loc ?connector $c-area))
            (los? $c-area ?t)))
    ;; Check if connected to another active connector that has line-of-sight to transmitter
    (exists (?other-connector connector)
       (and (different ?other-connector ?connector)
            (paired ?connector ?other-connector)
            (bind (color ?other-connector $other-hue))
            (eql $other-hue ?hue)
            ;; Verify the other connector has direct transmitter access
            (exists (?t transmitter)
              (and (paired ?other-connector ?t)
                   (bind (chroma ?t $t2-hue))
                   (eql $t2-hue ?hue)
                   (bind (loc ?other-connector $other-area))
                   (los? $other-area ?t)))))))


(define-query connectable? (?area ?terminus)
  (or (los? ?area ?terminus)  ;from connector in area to terminus
      (and (connector ?terminus)
           (exists (?a area)
             (and (loc ?terminus ?a)
                  (visible? ?area ?a))))))


(define-query get-coordinates? (?object)
  ; Coordinate lookup for fixtures (fixpoint) and movable cargo (pos)
  (or (and (bind (fixpoint ?object $x $y))
           (values $x $y))
      (and (bind (pos ?object $x $y))
           (values $x $y))))


(define-query can-establish-beam? (?source ?target)
  ; Check if beam from source to target is possible
  ; Returns target coords if unblocked, intersection coords if blocked, nil if impossible
  (and (mvsetq ($sx $sy) (get-coordinates? ?source))
       (mvsetq ($tx $ty) (get-coordinates? ?target))
       (find-first-intersection $sx $sy $tx $ty)))


(define-query passable? (?area1 ?area2)
  (or (adjacent ?area1 ?area2)
      (exists (?g gate)
        (and (gate-separates ?g ?area1 ?area2)
             (gate-status ?g open)))))


;;;; UPDATE FUNCTIONS ;;;;


(define-update activate-connector! (?connector ?hue)
  (color ?connector ?hue))


(define-update deactivate-connector! (?connector ?hue)
  (not (color ?connector ?hue)))


(define-update deactivate-receiver! (?receiver)
  (do (receiver-status ?receiver inactive)
      (doall (?g gate)
        (if (controls ?receiver ?g)
          (gate-status ?g closed)))))


(define-update create-beam-segment! (?source ?target)
  (do (mvsetq ($sx $sy) (get-coordinates? ?source))
      (mvsetq ($tx $ty) (get-coordinates? ?target))
      (mvsetq ($ix $iy) (find-first-intersection $sx $sy $tx $ty))
      (beam-segment ?source ?target $ix $iy)))


(define-update chain-activate! (?terminus ?hue)
  (do
    ;; Step 1: Activate the terminus based on its type
    (if (connector ?terminus)
      (activate-connector! ?terminus ?hue)
      (if (receiver ?terminus)
        (do (receiver-status ?terminus active)
            (doall (?g gate)
              (if (controls ?terminus ?g)
                (gate-status ?g open))))))
    ;; Step 2: Handle cascading effects based on terminus type
    (if (connector ?terminus)
      (do
        ;; Connector activation: activate connected receivers of matching color
        (doall (?r receiver)
          (if (and (paired ?terminus ?r)
                   (receiver-status ?r inactive)
                   (bind (chroma ?r $rhue))
                   (eql $rhue ?hue))
            (chain-activate! ?r ?hue)))
        ;; Connector activation: activate connected connectors
        (doall (?c connector)
          (if (and (different ?c ?terminus)
                   (paired ?terminus ?c)
                   (not (bind (color ?c $hue))))
            (chain-activate! ?c ?hue))))
      ;; Handle receiver activation branch
      (if (receiver ?terminus)
          ;; Receiver activation: check for newly accessible connectors
          (doall (?c connector)
            (if (not (bind (color ?c $hue)))
              (doall (?t transmitter)
                (if (and (paired ?c ?t)
                         (bind (chroma ?t $t-hue))
                         (eql $t-hue ?hue)
                         ;(bind (loc ?c $c-area))
                         ;(los? $c-area ?t))
                         (can-establish-beam? ?t ?c))
                  (do (create-beam-segment! ?t ?c)
                      (chain-activate! ?c $t-hue))))))))))


(define-update chain-deactivate! (?connector ?hue)
  (do 
    ;; Step 1: Deactivate this connector
    (deactivate-connector! ?connector ?hue)
    ;; Remove beam segment originating from this connector
;    (if (bind (beam-segment ?connector $x $y $ex $ey))
;      (not (beam-segment ?connector $x $y $ex $ey)))
    ;; Step 2: Deactivate receivers that lost power
    (doall (?r receiver)
      (if (and (paired ?connector ?r)
               (not (exists (?c connector)
                      (and (different ?c ?connector)
                           (paired ?c ?r)
                           (bind (color ?c $c-hue))
                           (eql $c-hue ?hue)))))
        (deactivate-receiver! ?r)))
    ;; Step 3: Connector revalidation
    (doall (?c connector)
      (if (and (bind (color ?c $c-hue))
               (eql $c-hue ?hue))
        ;; Check if this connector still has valid line-of-sight to power sources
        (if (not (connector-has-valid-line-of-sight? ?c ?hue))
          (chain-deactivate! ?c ?hue))))))


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
          (paired ?terminus $cargo inactive)
          (setq $hue (get-source-hue? ?terminus))
          (if $hue  ;ie, if ?terminus is a source
            (if (setq $beam-coords (can-establish-beam? ?terminus me1))
              (do (paired ?terminus $cargo active)
                  (if (equal $beam-coords (get-coordinates? me1))
                    (activate-connector! $cargo $hue)))))))


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
          ;; Always establish physical connections
          (paired $cargo ?terminus1)
          (paired $cargo ?terminus2)
          ;; Extract source colors
          (setq $hue1 (get-source-hue? ?terminus1))
          (setq $hue2 (get-source-hue? ?terminus2))
          ;; Determine activation color based on source consensus
          (if (and $hue1 $hue2)  ; both sources active
            (if (eql $hue1 $hue2)  ; same color
              (setq $hue $hue1))  ; activate with consensus color
            (if (or $hue1 $hue2)  ; exactly one source active
              (setq $hue (or $hue1 $hue2))))  ; activate with available color
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
          ;; Always establish physical connections
          (paired $cargo ?terminus1)
          (paired $cargo ?terminus2)
          (paired $cargo ?terminus3)
          ;; Extract source colors systematically
          (setq $hue1 (get-source-hue? ?terminus1))
          (setq $hue2 (get-source-hue? ?terminus2))
          (setq $hue3 (get-source-hue? ?terminus3))
          ;; Systematic consensus determination
          (if (and $hue1 $hue2 $hue3)  ; all three sources active
            (if (and (eql $hue1 $hue2) (eql $hue2 $hue3))  ; universal consensus
              (setq $hue $hue1))
            (if (and $hue1 $hue2)  ; exactly two sources: 1 and 2
              (if (eql $hue1 $hue2)  ; consensus between active pair
                (setq $hue $hue1))
              (if (and $hue1 $hue3)  ; exactly two sources: 1 and 3
                (if (eql $hue1 $hue3)  ; consensus between active pair
                  (setq $hue $hue1))
                (if (and $hue2 $hue3)  ; exactly two sources: 2 and 3
                  (if (eql $hue2 $hue3)  ; consensus between active pair
                    (setq $hue $hue2))
                  ;; exactly one source active
                  (setq $hue (or $hue1 $hue2 $hue3))))))
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
            (if (bind (paired ?connector ?t $beam-status))
              (not (paired ?connector ?t $beam-status))))))


(define-action drop
    1
  ()
  (and (bind (holds me1 $cargo))  ;if not holding anything, then bind statement returns nil
       (bind (loc me1 $area)))  ;me1 is always located somewhere
  ($cargo $area)
  (assert (not (holds me1 $cargo))
          (loc $cargo $area)
          (pos $cargo (get-coordinates? me1))))


(define-action move
    1
  (?area2 area)
  (and (bind (loc me1 $area1))
       (different $area1 ?area2)
       (passable? $area1 ?area2))
  ($area1 ?area2)
  (assert (loc me1 ?area2)
          (bind (centroid ?area2 $x $y))
          (pos me1 $x $y)))


;;;; INITIALIZATION ;;;;


(define-init
  ;dynamic
  (loc me1 area4)
  (loc connector1 area4)
  (loc connector2 area4)
  (loc connector3 area4)
  (pos me1 10 10)
  (pos connector1 11 11)
  (pos connector2 12 12)
  (pos connector3 13 13)
  (gate-status gate1 closed)
  (paired transmitter1 receiver3)
  (paired transmitter2 receiver1)
  ;static
  (adjacent area1 area3)
  (adjacent area1 area4)
  (adjacent area1 area5)
  (adjacent area3 area4)
  (adjacent area3 area5)
  (adjacent area4 area5)
  (centroid area1 27 16)
  (centroid area2 33 13)
  (centroid area3 27 10)
  (centroid area4 15 12)
  (centroid area5 10 20)
  (fixpoint transmitter1 32 13)
  (fixpoint transmitter2 25  0)
  (fixpoint receiver1 25 26)
  (fixpoint receiver2  0  7)
  (fixpoint receiver3  0 18)
  (gate-segment gate1 31 15 31 11)
  (chroma transmitter1 blue)  ;chroma indicates a static color, color is dynamic
  (chroma transmitter2 red)
  (chroma receiver1 red)
  (chroma receiver2 red)
  (chroma receiver3 blue)
  (controls receiver1 gate1)
  (los1 area1 gate1 transmitter1)  ;line of sight from an area thru a gate to transmitter1
  (los0 area1 transmitter2)  ;direct line of sight from an area to transmitter2
  (los0 area1 receiver1)
  (los0 area1 receiver2)
  (los0 area1 receiver3)
  (los1 area2 gate1 receiver2)
  (los1 area3 gate1 transmitter1)
  (los0 area3 transmitter2)
  (los0 area3 receiver1)
  (los0 area3 receiver2)
  (los0 area3 receiver3)
  (los1 area4 gate1 transmitter1)
  (los0 area4 transmitter2)
  (los0 area4 receiver1)
  (los0 area4 receiver2)
  (los0 area4 receiver3)
  (los1 area5 gate1 transmitter1)
  (los0 area5 transmitter2)
  (los0 area5 receiver1)
  (los0 area5 receiver2)
  (los0 area5 receiver3)
  (visible1 area1 gate1 area2)  ;visibility from area1 thru gate1 to area2
  (visible0 area1 area3)  ;direct visibility from area1 to area3
  (visible0 area1 area4)
  (visible0 area1 area5)
  (visible1 area2 gate1 area3)
  (visible1 area2 gate1 area4)
  (visible1 area2 gate1 area5)
  (visible0 area3 area4)
  (visible0 area3 area5)
  (visible0 area4 area5)
)


;;;; GOAL ;;;;

(define-goal  ;always put this last
  (and (receiver-status receiver2 active)
       (receiver-status receiver3 active)))

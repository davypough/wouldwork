;;; Filename: problem-tight-space.lisp


;;; Problem specification in the Talos Principle game
;;; for the tight space problem in The Lost Prisoner.


(in-package :ww)  ;required

(ww-set *problem-name* tight-space)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 7)


(define-types
  agent       (me)  ;the name of the agent performing actions
  gate        (gate1)
  wall        (wall1)
  connector   (connector1 connector2 connector3)
  transmitter (transmitter1 transmitter2)
  receiver    (receiver1 receiver2 receiver3)
  hue         (blue red)  ;the color of a transmitter, receiver, or active connector
  beam        (beam1 beam2)
  area        (area1 area2 area3 area4 area5 area6)  ;vantage points
  gate-status (open closed)
  receiver-status (active inactive)
  cargo       (either connector)  ;what an agent (me) can pickup & carry
  terminus    (either transmitter receiver connector)  ;what a connector can connect to
  source      (either transmitter connector)  ;beam source
  target      (either connector receiver)  ;beam target
  fixture     (either transmitter receiver)
)


(define-dynamic-relations  ;relations with fluents can be bound in rules--eg (bind (holds me $any-cargo))
  (holds agent $cargo)  ;fluent because we need to sometimes lookup what is currently being held
  (loc (either agent cargo) $area)  ;a location in an area
  (paired terminus terminus)  ;potential beam between two terminus
  (gate-status gate $gate-status)
  (receiver-status receiver $receiver-status)
  (color connector $hue)
  (beam-segment beam $source $target $rational $rational)  ;endpoint-x endpoint-y
  (current-beams $list)
)


(define-static-relations
  (vantage area $fixnum $fixnum)  ;the (x,y) position of an area
  (adjacent area area)   ;agent can always move to adjacent area unimpeded
  (controls receiver gate)
  (gate-separates gate area area)
  (fixpoint fixture $fixnum $fixnum)  ;coordinates of a transmitter or receiver
  (gate-segment gate $fixnum $fixnum $fixnum $fixnum)
  (wall-segment wall $fixnum $fixnum $fixnum $fixnum)
  (chroma fixture $hue)
  (los0 area fixture)  ;direct line of sight to a fixture
  (los1 area gate fixture)  ;line of sight thru a gate to a fixture
  (visible0 area area)  ;visibility between areas to a connector
  (visible1 area gate area)  ;visibility thru a gate
)


;;;; QUERY FUNCTIONS ;;;;


(define-query get-current-beams? ()
  (do (bind (current-beams $beams))
      $beams))


(define-query get-hue-if-source? (?terminus)
  ;Returns color or chroma if source, otherwise nil
  (do (or (and (transmitter ?terminus)
               (bind (chroma ?terminus $hue)))
          (and (connector ?terminus)
               (bind (color ?terminus $hue))))
      $hue))


(define-query get-coordinates? (?object)
  (if (and (bind (loc ?object $area))
           (bind (vantage $area $x $y)))
    (values $x $y)
    (if (bind (fixpoint ?object $x $y))
      (values $x $y)
      (if (bind (vantage ?object $x $y))
        (values $x $y)
        (values nil nil)))))


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


(define-query resolve-consensus-hue? ($hue-list)
  ;; Returns consensus hue from list of available hues, nil if no consensus
  (do (setq $unique-hues (remove-duplicates (remove nil $hue-list)))
      (if (= (length $unique-hues) 1)
        (first $unique-hues)  ; Single unique hue (consensus achieved)
        nil)))               ; Multiple different hues or no hues (no consensus)

(define-query beam-segment-interference? (?source-x ?source-y ?end-x ?end-y ?cross-x1 ?cross-y1 ?cross-x2 ?cross-y2)
  ;; Determines if a cross segment (beam, gate, wall, etc) interferes with the main beam-segment
  ;; Returns the interference endpoint, or nil
  ;; Step 1: Calculate direction vectors and displacement
  (do (setq $dx1 (- ?end-x ?source-x))    ; Beam direction x
      (setq $dy1 (- ?end-y ?source-y))    ; Beam direction y
      (setq $dx2 (- ?cross-x2 ?cross-x1))  ; Obstacle direction x  
      (setq $dy2 (- ?cross-y2 ?cross-y1))  ; Obstacle direction y
      (setq $dx3 (- ?cross-x1 ?source-x))   ; Displacement x
      (setq $dy3 (- ?cross-y1 ?source-y))   ; Displacement y
      ;; Step 2: Calculate determinant for parallel line detection
      (setq $det (- (* $dy1 $dx2) (* $dx1 $dy2)))
      ;; Step 3: Handle parallel lines case
      (if (< (abs $det) 1e-10)  ; Numerical tolerance for parallel detection
        (values nil nil nil)     ; No intersection for parallel lines
        ;; Step 4: Solve for intersection parameters using Cramer's rule
        (do (setq $t (/ (- (* $dy3 $dx2) (* $dx3 $dy2)) $det))
            (setq $s (/ (- (* $dx1 $dy3) (* $dy1 $dx3)) $det))
            ;; Step 5: Validate intersection lies within both segments
            ;; Exclude continuation points at beam start (t=0)
            (if (and (> $t 1e-10) (< $t 1.0)       ; New beam: exclude both endpoints
                     (> $s 0.0) (< $s 1.0))         ; Existing beam: exclude both endpoints
              ;; Step 6: Calculate and return intersection coordinates
              (do (setq $int-x (+ ?source-x (* $t $dx1)))
                  (setq $int-y (+ ?source-y (* $t $dy1)))
                  (values $t $int-x $int-y))      ; Return parameter and coordinates
              ;; No valid intersection within segments
              (values nil nil nil))))))


(define-query beam-segment-occlusion? (?source-x ?source-y ?end-x ?end-y ?px ?py)
  ;; Determines if an object in an area at (?px,?py) occludes a beam-segment with tolerance < 1.0
  ;; Step 1: Calculate beam direction and length
  (do (setq $dx (- ?end-x ?source-x))
      (setq $dy (- ?end-y ?source-y))
      (setq $length-squared (+ (* $dx $dx) (* $dy $dy)))
      ;; Step 2: Handle degenerate case (zero-length beam)
      (if (< $length-squared 1e-10)
        ;; Check if point coincides with source/target
        (if (and (< (abs (- ?px ?source-x)) 1e-6)
                 (< (abs (- ?py ?source-y)) 1e-6))
          (values 0.0 ?source-x ?source-y)      ; Point at source
          (values nil nil nil))     ; Point not at degenerate beam
        ;; Step 3: Calculate projection parameter
        (do (setq $vx (- ?px ?source-x))   ; Vector from source to point
            (setq $vy (- ?py ?source-y))
            (setq $dot-product (+ (* $vx $dx) (* $vy $dy)))
            (setq $t (/ $dot-product $length-squared))
            ;; Step 4: Validate parameter within segment bounds
            (if (and (> $t 0.0) (< $t 1.0))
              ;; Step 5: Calculate closest point on beam to given point
              (do (setq $closest-x (+ ?source-x (* $t $dx)))
                  (setq $closest-y (+ ?source-y (* $t $dy)))
                  ;; Step 6: Calculate distance from point to beam
                  (setq $dist-x (- ?px $closest-x))
                  (setq $dist-y (- ?py $closest-y))
                  (setq $distance (sqrt (+ (* $dist-x $dist-x) 
                                          (* $dist-y $dist-y))))
                  ;; Step 7: Check if point is within intersection tolerance
                  (if (< $distance 1.0)  ; Tolerance for blocking the beam
                    (values $t $closest-x $closest-y)  ; Point blocks beam
                    (values nil nil nil)))             ; Point too far from beam
              ;; Projection falls outside segment bounds
              (values nil nil nil))))))


(define-query find-first-intersection? (?source-x ?source-y ?end-x ?end-y)
  (do
    ;; Initialize closest intersection tracking
    (setq $closest-t 1.0)  ; Default to target if no intersections
    (setq $result-x ?end-x)   
    (setq $result-y ?end-y)
    ;; Check for interference
    (doall (?obj (either gate wall beam))
      (if (and ;; Type-specific coordinate binding and conditions
               (or (and (gate ?obj) 
                        (gate-status ?obj closed)
                        (bind (gate-segment ?obj $x1 $y1 $x2 $y2)))
                   (and (wall ?obj) 
                        (bind (wall-segment ?obj $x1 $y1 $x2 $y2)))
                   (and (beam ?obj) 
                        (bind (beam-segment ?obj $src $tgt $x2 $y2))
                        (mvsetq ($x1 $y1) (get-coordinates? $src))))
               ;; Endpoint exclusion logic for all segment types
               (not (or (and (= $x1 ?source-x) (= $y1 ?source-y))
                        (and (= $x2 ?source-x) (= $y2 ?source-y))
                        (and (= $x1 ?end-x) (= $y1 ?end-y))
                        (and (= $x2 ?end-x) (= $y2 ?end-y)))))
        ;; Common interference detection and update logic
        (do (mvsetq ($int-t $int-x $int-y)
              (beam-segment-interference? ?source-x ?source-y ?end-x ?end-y $x1 $y1 $x2 $y2))
            (if (and $int-t (< $int-t $closest-t))
              (do (setq $closest-t $int-t)
                  (setq $result-x $int-x)
                  (setq $result-y $int-y))))))
    ;; Check for occlusion 
    (doall (?obj connector)
      (if (and (bind (loc ?obj $area))  ;exclude held connector
               (bind (vantage $area $ox $oy))
               (not (or (and (= $ox ?source-x) (= $oy ?source-y))
                        (and (= $ox ?end-x) (= $oy ?end-y)))))
        (do (mvsetq ($int-t $int-x $int-y)
              (beam-segment-occlusion? ?source-x ?source-y ?end-x ?end-y $ox $oy))
            (if (and $int-t (< $int-t $closest-t))
              (do (setq $closest-t $int-t)
                  (setq $result-x $int-x)
                  (setq $result-y $int-y))))))
    ;; Return closest intersection coordinates
    (values $result-x $result-y)))


(define-query connectable? (?area ?terminus)
  (or 
    ;; Case 1: Fixtures (transmitters/receivers) - check both architectural and geometric LOS
    (and (fixture ?terminus)
         ;; Static gate-based line-of-sight check
         (los? ?area ?terminus)
         ;; Dynamic occlusion/interference check
         (mvsetq ($source-x $source-y) (get-coordinates? ?area))
         (mvsetq ($target-x $target-y) (get-coordinates? ?terminus))
         (mvsetq ($end-x $end-y)
           (find-first-intersection? $source-x $source-y $target-x $target-y))
         ;; Verify beam reaches fixture
         (= $end-x $target-x)
         (= $end-y $target-y))
    ;; Case 2: Connectors require geometric beam path validation
    ;; Must verify actual beam reaches target through dynamic obstacles
    (and (connector ?terminus)
         (exists (?target-area area)
           (and (loc ?terminus ?target-area)
                ;; Verify areas are gate-visible
                (visible? ?area ?target-area)
                ;; Get source coordinates
                (mvsetq ($source-x $source-y) (get-coordinates? ?area))
                ;; Get target connector coordinates
                (mvsetq ($target-x $target-y) (get-coordinates? ?terminus))
                ;; Calculate where beam actually terminates
                (mvsetq ($end-x $end-y) 
                  (find-first-intersection? $source-x $source-y $target-x $target-y))
                ;; Verify beam reaches target
                (= $end-x $target-x)
                (= $end-y $target-y))))))


(define-query passable? (?area1 ?area2)
  (or (adjacent ?area1 ?area2)
      (exists (?g gate)
        (and (gate-separates ?g ?area1 ?area2)
             (gate-status ?g open)))))


(define-query vacant? (?area)
  (not (exists (?cargo cargo)
         (loc ?cargo ?area))))


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


(define-update create-beam-segment-p! (?source ?target)
  ; Create a beam segment from source, and return the beam endpoint.
  (do
    ;; Generate new beam entity with next available index
    (bind (current-beams $current-beams))
    (setq $next-index (1+ (length $current-beams)))
    (setq $new-beam (intern (format nil "BEAM~D" $next-index)))
    ;; Calculate beam path and intersection
    (mvsetq ($source-x $source-y) (get-coordinates? ?source))
    (mvsetq ($target-x $target-y) (get-coordinates? ?target))
    (mvsetq ($end-x $end-y) (find-first-intersection? $source-x $source-y $target-x $target-y))
    ;; Create beam relations
    (beam-segment $new-beam ?source ?target $end-x $end-y)
    (current-beams (cons $new-beam $current-beams))
    ;; Return success indicator (beam reaches target unobstructed)
    (and (= $end-x $target-x) (= $end-y $target-y))))


(define-update recalculate-all-beams! ()
  (doall (?b beam)
    (do (bind (beam-segment ?b $source $target $old-end-x $old-end-y))
        ;; Get source coordinates
        (mvsetq ($source-x $source-y) (get-coordinates? $source))
        ;; Get target coordinates  
        (mvsetq ($target-x $target-y) (get-coordinates? $target))
        ;; Recalculate endpoint using current gate/wall/beam states
        (mvsetq ($new-end-x $new-end-y) (find-first-intersection? $source-x $source-y $target-x $target-y))
        ;; Update beam segment if endpoint changed
        (if (or (/= $new-end-x $old-end-x) (/= $new-end-y $old-end-y))
          (do (not (beam-segment ?b $source $target $old-end-x $old-end-y))
              (beam-segment ?b $source $target $new-end-x $new-end-y))))))


(define-update chain-activate! (?terminus ?hue)
  (do
    ;; Activate the terminus based on its type
    (if (connector ?terminus)
      (activate-connector! ?terminus ?hue)
      (if (receiver ?terminus)
        (do (receiver-status ?terminus active)
            (setq $gates-opened nil)
            (doall (?g gate)
              (if (controls ?terminus ?g)
                (do (gate-status ?g open)
                    (setq $gates-opened t))))
            (if $gates-opened (recalculate-all-beams!)))))
    ;; Handle cascading effects based on terminus type
    (if (connector ?terminus)
      (do
        ;; Connector activation: activate connected receivers of matching color
        (doall (?r receiver)
          (if (and (paired ?terminus ?r)
                   (receiver-status ?r inactive)
                   (bind (chroma ?r $rhue))
                   (eql $rhue ?hue)
                   (or
                     ;; Check if existing beam reaches target
                     (exists (?b (get-current-beams?))
                       (and (bind (beam-segment ?b $source $target $end-x $end-y))
                            (eql $source ?terminus)
                            (eql $target ?r)
                            (mvsetq ($target-x $target-y) (get-coordinates? ?r))
                            (= $end-x $target-x)
                            (= $end-y $target-y)))
                     ;; No existing beam, but can create one that reaches target
                     (and (not (exists (?b (get-current-beams?))
                          (and (bind (beam-segment ?b $source $target $end-x $end-y))
                               (eql $source ?terminus)
                               (eql $target ?r))))
                          (create-beam-segment-p! ?terminus ?r))))
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
                         (create-beam-segment-p! ?t ?c))
                  (chain-activate! ?c $t-hue)))))))))


(define-update chain-deactivate! (?connector ?hue)
  (do 
    ;; Deactivate this connector
    (deactivate-connector! ?connector ?hue)
    ;; Deactivate receivers that lost power
    (doall (?r receiver)
      (if (and (paired ?connector ?r)
               (not (exists (?c connector)
                      (and (different ?c ?connector)
                           (paired ?c ?r)
                           (bind (color ?c $c-hue))
                           (eql $c-hue ?hue)))))
        (deactivate-receiver! ?r)))
    ;; Connector revalidation
    (doall (?c connector)
      (if (and (bind (color ?c $c-hue))
               (eql $c-hue ?hue))
        ;; Check if this connector still has valid line-of-sight to power sources
        (if (not (connector-has-valid-line-of-sight? ?c ?hue))
          (chain-deactivate! ?c ?hue))))))


(define-update update-beams-if-occluded! (?area)
  ;; Check if placing an object at ?area would occlude existing beam segments
  (do 
    ;; Get coordinates of area where object would be placed
    (mvsetq ($area-x $area-y) (get-coordinates? ?area))
    ;; Process each existing beam directly
    (doall (?b beam)
      (do (bind (beam-segment ?b $source $target $end-x $end-y))
          ;; Get beam start coordinates (source coordinates are stable)
          (mvsetq ($start-x $start-y) (get-coordinates? $source))
          ;; Check if object at ?area would occlude this beam segment
          (mvsetq ($int-t $int-x $int-y)
                  (beam-segment-occlusion? $start-x $start-y $end-x $end-y $area-x $area-y))
          ;; Update beam endpoint if occluded
          (if $int-t
            (do (not (beam-segment ?b $source $target $end-x $end-y))
                (beam-segment ?b $source $target $area-x $area-y)))))))  


;;;; ACTIONS ;;;;


(define-action connect-to-1-terminus
    2
  (?terminus terminus)
  (and (bind (holds me $cargo))
       (connector $cargo)
       (bind (loc me $area))
       (vacant? $area)
       (connectable? $area ?terminus))
  ($cargo ?terminus $area $hue)
  (assert (not (holds me $cargo))
          (loc $cargo $area)
          (paired ?terminus $cargo)
          (setq $hue (get-hue-if-source? ?terminus))
          (if $hue  ; if ?terminus is a source
            (do (setq $unobstructed-p (create-beam-segment-p! ?terminus $cargo))
                (if $unobstructed-p
                  (activate-connector! $cargo $hue))))
          ; Cargo may now be occluding beams
          (update-beams-if-occluded! $area)))  ;does an object at $area now occlude any other beams


(define-action connect-to-2-terminus
    3
  (combination (?terminus1 ?terminus2) terminus)
  (and (bind (holds me $cargo))
       (connector $cargo)
       (bind (loc me $area))
       (vacant? $area)
       (connectable? $area ?terminus1)
       (connectable? $area ?terminus2))
  ($cargo ?terminus1 ?terminus2 $area $hue)
  (assert (not (holds me $cargo))
          (loc $cargo $area)
          (paired $cargo ?terminus1)
          (paired $cargo ?terminus2)
          (setq $hue1 (get-hue-if-source? ?terminus1))
          (setq $hue2 (get-hue-if-source? ?terminus2))
          (setq $hue (resolve-consensus-hue? (list $hue1 $hue2)))
          (if $hue
            (do ; Create directional beam segments based on terminus type
                (if $hue1 
                  (create-beam-segment-p! ?terminus1 $cargo)
                  (create-beam-segment-p! $cargo ?terminus1))
                (if $hue2 
                  (create-beam-segment-p! ?terminus2 $cargo)
                  (create-beam-segment-p! $cargo ?terminus2))
                (chain-activate! $cargo $hue)
                ; Cargo may now be blocking beams
                (update-beams-if-occluded! $area)))))


(define-action connect-to-3-terminus
    4
  (combination (?terminus1 ?terminus2 ?terminus3) terminus)
  (and (bind (holds me $cargo))
       (connector $cargo)
       (bind (loc me $area))
       (vacant? $area)
       (connectable? $area ?terminus1)
       (connectable? $area ?terminus2)
       (connectable? $area ?terminus3))
  ($cargo ?terminus1 ?terminus2 ?terminus3 $area $hue)
  (assert (not (holds me $cargo))
          (loc $cargo $area)
          (paired $cargo ?terminus1)
          (paired $cargo ?terminus2)
          (paired $cargo ?terminus3)
          (setq $hue1 (get-hue-if-source? ?terminus1))
          (setq $hue2 (get-hue-if-source? ?terminus2))
          (setq $hue3 (get-hue-if-source? ?terminus3))
          (setq $hue (resolve-consensus-hue? (list $hue1 $hue2 $hue3)))
          (if $hue
            (do ; Create directional beam segments based on terminus type
                (if $hue1 
                  (create-beam-segment-p! ?terminus1 $cargo)
                  (create-beam-segment-p! $cargo ?terminus1))
                (if $hue2 
                  (create-beam-segment-p! ?terminus2 $cargo)
                  (create-beam-segment-p! $cargo ?terminus2))
                (if $hue3 
                  (create-beam-segment-p! ?terminus3 $cargo)
                  (create-beam-segment-p! $cargo ?terminus3))
                (chain-activate! $cargo $hue)
                ; Cargo may now be blocking beams
                (update-beams-if-occluded! $area)))))


(define-action pickup-connector
    1
  (?connector connector)
  (and (not (bind (holds me $cargo)))
       (bind (loc me $area))
       (loc ?connector $area))
  (?connector $area)
  (assert (holds me ?connector)
          (not (loc ?connector $area))
          ;; Step 1: Extract connector coordinates from area
          (mvsetq ($conn-x $conn-y) (get-coordinates? $area))
          ;; Step 2: Process beams where connector is occluder (not participant)
          ;; These beams terminate at connector coordinates but connector is neither source nor target
          (doall (?b beam)
            (do (bind (beam-segment ?b $source $target $end-x $end-y))
                (if (and (= $end-x $conn-x) (= $end-y $conn-y)
                         (different $source ?connector)
                         (different $target ?connector))
                  ;; Connector was occluding this beam - recalculate beam endpoint
                  (do (mvsetq ($source-x $source-y) (get-coordinates? $source))
                      (mvsetq ($target-x $target-y) (get-coordinates? $target))
                      (mvsetq ($new-end-x $new-end-y) 
                              (find-first-intersection? $source-x $source-y $target-x $target-y))
                      ;; Update beam segment with recalculated endpoint
                      (not (beam-segment ?b $source $target $end-x $end-y))
                      (beam-segment ?b $source $target $new-end-x $new-end-y)))))
          ;; Step 3: Remove beam segments where connector is participant (source or target)
          (doall (?b beam)
            (do (bind (beam-segment ?b $source $target $end-x $end-y))
                (if (eql $source ?connector)
                  ;; Connector is source - remove this beam segment
                  (do (not (beam-segment ?b ?connector $target $end-x $end-y))
                      (bind (current-beams $beams1))
                      (current-beams (remove ?b $beams1)))
                  (if (eql $target ?connector)
                    ;; Connector is target - remove this beam segment  
                    (do (not (beam-segment ?b $source ?connector $end-x $end-y))
                        (bind (current-beams $beams2))
                        (current-beams (remove ?b $beams2)))))))
          ;; Step 4: Deactivate connector
          (if (bind (color ?connector $hue))
            (chain-deactivate! ?connector $hue))
          ;; Step 5: Remove pairings (must be AFTER chain-deactivate!)
          (doall (?t terminus)
            (if (paired ?connector ?t)
              (not (paired ?connector ?t))))))


(define-action drop
    1
  ()
  (and (bind (holds me $cargo))  ;if not holding anything, then bind statement returns nil
       (bind (loc me $area))  ;me is always located somewhere
       (vacant? $area))
  ($cargo $area)
  (assert (not (holds me $cargo))
          (loc $cargo $area)
          (update-beams-if-occluded! $area)))


(define-action move
    1
  (?area2 area)
  (and (bind (loc me $area1))
       (different $area1 ?area2)
       (passable? $area1 ?area2))
  ($area1 ?area2)
  (assert (loc me ?area2)
          (recalculate-all-beams!)))


;;;; INITIALIZATION ;;;;


(define-init
  ;dynamic
  (loc me area1)
  (loc connector1 area4)
  (loc connector2 area5)
  (loc connector3 area2)
  (gate-status gate1 closed)
  (paired transmitter1 receiver3)
  (paired transmitter2 receiver1)
  (current-beams (beam1 beam2))
  ;static
  (vantage area1 25 18)
  (vantage area2 27 14)
  (vantage area3 25 10)
  (vantage area4 19 15)
  (vantage area5 25 14)
  (vantage area6 34 13)
  (adjacent area1 area2)
  (adjacent area1 area3)
  (adjacent area1 area4)
  (adjacent area1 area5)
  (adjacent area2 area3)
  (adjacent area2 area4)
  (adjacent area2 area5)
  (adjacent area3 area4)
  (adjacent area3 area5)
  (adjacent area4 area5)
  (fixpoint transmitter1 32 13)
  (fixpoint transmitter2 25  0)
  (fixpoint receiver1 25 26)
  (fixpoint receiver2  0  7)
  (fixpoint receiver3  0 18)
  (gate-segment gate1 31 15 31 11)
  (wall-segment wall1 33 13 33 13)
  (chroma transmitter1 blue)  ;chroma indicates a static color, color is dynamic
  (chroma transmitter2 red)
  (chroma receiver1 red)
  (chroma receiver2 red)
  (chroma receiver3 blue)
  (controls receiver1 gate1)
  (receiver-status receiver1 inactive)
  (receiver-status receiver2 inactive)
  (receiver-status receiver3 inactive)
  (los1 area1 gate1 transmitter1)  ;line of sight from area1 thru gate1 to transmitter1
  (los0 area1 transmitter2)  ;direct line of sight from area1 to transmitter2
  (los0 area1 receiver1)
  (los0 area1 receiver2)
  (los0 area1 receiver3)
  (los1 area2 gate1 transmitter1)
  (los0 area2 transmitter2)
  (los0 area2 receiver1)
  (los0 area2 receiver2)
  (los0 area2 receiver3)
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
  (los1 area6 gate1 receiver2)
  (visible0 area1 area2)
  (visible0 area1 area3)  ;direct visibility from area1 to area3
  (visible0 area1 area4)
  (visible0 area1 area5)
  (visible1 area1 gate1 area6)  ;visibility from area1 thru gate1 to area2
  (visible0 area2 area3)
  (visible0 area2 area4)
  (visible0 area2 area5)
  (visible0 area3 area4)
  (visible0 area3 area5)
  (visible1 area3 gate1 area6)
  (visible0 area4 area5)
)


(define-init-action create-initial-beams
  0
  ()
  (always-true)
  ()
  (assert (mvsetq ($source1-x $source1-y) (get-coordinates? transmitter1))
          (mvsetq ($target1-x $target1-y) (get-coordinates? receiver3))
          (mvsetq ($end1-x $end1-y) (find-first-intersection? $source1-x $source1-y $target1-x $target1-y))
          (beam-segment beam1 transmitter1 receiver3 $end1-x $end1-y)
          (mvsetq ($source2-x $source2-y) (get-coordinates? transmitter2))
          (mvsetq ($target2-x $target2-y) (get-coordinates? receiver1))
          (mvsetq ($end2-x $end2-y) (find-first-intersection? $source2-x $source2-y $target2-x $target2-y))
          (beam-segment beam2 transmitter2 receiver1 $end2-x $end2-y)))

;;;; GOAL ;;;;

(define-goal  ;always put this last
  (and (receiver-status receiver2 active)
       (receiver-status receiver3 active)))

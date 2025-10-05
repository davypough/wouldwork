;;; Filename: problem-tightspace.lisp


;;; Problem specification in the Talos Principle game
;;; for the tight space problem in The Lost Prisoner.


(in-package :ww)  ;required

(ww-set *problem-name* tightspace)

(ww-set *problem-type* planning)

(ww-set *solution-type* first)  ;min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 10)


(define-types
  agent       (agent1)  ;the name of the agent performing actions
  gate        (gate1)
  wall        (wall1)
  connector   (connector1 connector2 connector3)
  transmitter (transmitter1 transmitter2)
  receiver    (receiver1 receiver2 receiver3)
  hue         (blue red)  ;the color of a transmitter, receiver, or active connector
  beam        (beam1 beam2)
  area        (area1 area2 area3 area4 area5 area6)  ;vantage points
  gate-state  (open closed)
  receiver-state (active inactive)
  cargo       (either connector)  ;what an agent can pickup & carry
  terminus    (either transmitter receiver connector)  ;what a connector can connect to
  source      (either transmitter connector)  ;beam source
  target      (either connector receiver)  ;beam target
  fixture     (either transmitter receiver)
)


(define-dynamic-relations  ;relations with fluents can be bound in rules--eg (bind (holds agent1 $any-cargo))
  (holds agent $cargo)  ;fluent because we need to sometimes lookup what is currently being held
  (loc (either agent cargo) $area)  ;a location in an area
  (paired terminus terminus)  ;potential beam between two terminus
  (gate-status gate $gate-state)
  (receiver-status receiver $receiver-state)
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


(define-query get-current-beams ()
  (do (bind (current-beams $beams))
      $beams))


(define-query get-hue-if-source (?terminus)
  ;Returns color or chroma if source, otherwise nil
  (do (or (and (transmitter ?terminus)
               (bind (chroma ?terminus $hue)))
          (and (connector ?terminus)
               (bind (color ?terminus $hue))))
      $hue))


(define-query get-coordinates (?object)
  (if (and (bind (loc ?object $area))
           (bind (vantage $area $x $y)))
    (values $x $y)
    (if (bind (fixpoint ?object $x $y))
      (values $x $y)
      (if (bind (vantage ?object $x $y))
        (values $x $y)
        (values nil nil)))))


(define-query los-thru-1-gate (?area ?fixture)
  (exists (?g gate)
    (and (los1 ?area ?g ?fixture)
         (gate-status ?g open))))


(define-query los (?area ?fixture)
  (or (los0 ?area ?fixture)
      (los-thru-1-gate ?area ?fixture)))


(define-query visible-thru-1-gate (?area1 ?area2)
  (exists (?g gate)
    (and (visible1 ?area1 ?g ?area2)
         (gate-status ?g open))))


(define-query visible (?area1 ?area2)
  (or (visible0 ?area1 ?area2)
      (visible-thru-1-gate ?area1 ?area2)))


(define-query connector-has-valid-line-of-sight (?connector ?hue)
  (or 
    ;; Check direct transmitter connection with current line-of-sight
    (exists (?t transmitter)
       (and (paired ?connector ?t)
            (bind (chroma ?t $t-hue))
            (eql $t-hue ?hue)
            (bind (loc ?connector $c-area))
            (los $c-area ?t)))
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
                   (los $other-area ?t)))))))


(define-query resolve-consensus-hue (?hue-list)
  ;; Returns consensus hue from list of available hues, nil if no consensus
  (do (setq $unique-hues (remove-duplicates (remove nil ?hue-list)))
      (if (= (length $unique-hues) 1)
        (first $unique-hues)  ; Single unique hue (consensus achieved)
        nil)))               ; Multiple different hues or no hues (no consensus)


(define-query beam-segment-interference (?source-x ?source-y ?end-x ?end-y ?cross-x1 ?cross-y1 ?cross-x2 ?cross-y2)
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


(define-query beam-segment-occlusion (?source-x ?source-y ?end-x ?end-y ?px ?py)
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


(define-query find-first-obstacle-intersection (?source-x ?source-y ?target-x ?target-y)
  ;; Establishes each beam's intended full path by finding intersections with static obstacles only
  ;; Returns the endpoint coordinates where the beam terminates due to gates, walls, or cargo
  (do
    ;; Initialize closest intersection tracking
    (setq $closest-t 1.0)  ; Default to target if no intersections
    (setq $result-x ?target-x)   
    (setq $result-y ?target-y)
    ;; Check static obstacles only - exclude beams
    (doall (?obj (either gate wall cargo))
      (if (and ;; Type-specific coordinate binding and conditions
               (or (and (gate ?obj) 
                        (not (gate-status ?obj open))  ; Block UNLESS explicitly open
                        (bind (gate-segment ?obj $x1 $y1 $x2 $y2)))
                   (and (wall ?obj) 
                        (bind (wall-segment ?obj $x1 $y1 $x2 $y2)))
                   (and (cargo ?obj) 
                        (bind (loc ?obj $area))
                        (bind (vantage $area $x1 $y1))
                        (setq $x2 $x1) (setq $y2 $y1)))
               ;; Endpoint exclusion logic for all obstacle types
               (not (or (and (= $x1 ?source-x) (= $y1 ?source-y))
                        (and (= $x2 ?source-x) (= $y2 ?source-y))
                        (and (= $x1 ?target-x) (= $y1 ?target-y))
                        (and (= $x2 ?target-x) (= $y2 ?target-y)))))
        ;; Calculate intersection with obstacle
        (do (mvsetq ($int-t $int-x $int-y)
              (if (and (= $x1 $x2) (= $y1 $y2))
                ;; Point occlusion (cargo)
                (beam-segment-occlusion ?source-x ?source-y ?target-x ?target-y $x1 $y1)
                ;; Line segment interference (gates, walls)
                (beam-segment-interference ?source-x ?source-y ?target-x ?target-y $x1 $y1 $x2 $y2)))
            (if (and $int-t (< $int-t $closest-t))
              (do (setq $closest-t $int-t)
                  (setq $result-x $int-x)
                  (setq $result-y $int-y))))))
    ;; Return closest intersection coordinates
    (values $result-x $result-y)))


(define-query collect-all-beam-intersections ()
  ;; Returns list of intersection records: ((beam1 beam2 intersection-x intersection-y t1 t2) ...)
  ;; Uses intended target paths to detect all beam-beam intersections
  (do (doall (?b1 (get-current-beams))
        (doall (?b2 (get-current-beams))
          (if (and (different ?b1 ?b2)
                   (string< (symbol-name ?b1) (symbol-name ?b2))) ; Avoid duplicate pairs
            (do (bind (beam-segment ?b1 $src1 $tgt1 $end1-x $end1-y))
                (bind (beam-segment ?b2 $src2 $tgt2 $end2-x $end2-y))
                ;; Use intended target coordinates, not current endpoints
                (mvsetq ($src1-x $src1-y) (get-coordinates $src1))
                (mvsetq ($tgt1-x $tgt1-y) (get-coordinates $tgt1))
                (mvsetq ($src2-x $src2-y) (get-coordinates $src2))
                (mvsetq ($tgt2-x $tgt2-y) (get-coordinates $tgt2))
                ;; Check intersection between intended paths
                (mvsetq ($t1 $int-x $int-y) 
                  (beam-segment-interference $src1-x $src1-y $tgt1-x $tgt1-y 
                                              $src2-x $src2-y $tgt2-x $tgt2-y))
                (if $t1
                  (do
                    ;; Get t2 parameter by checking beam2 vs beam1
                    (mvsetq ($t2 $int-x2 $int-y2) 
                      (beam-segment-interference $src2-x $src2-y $tgt2-x $tgt2-y 
                                                  $src1-x $src1-y $tgt1-x $tgt1-y))
                    ;; Only push if both intersections valid
                    (if $t2
                      (push (list ?b1 ?b2 $int-x $int-y $t1 $t2) $intersections))))))))
      $intersections))


(define-query connectable (?area ?terminus)
  (or 
    ;; Case 1: Fixtures (transmitters/receivers) - check both architectural and geometric LOS
    (and (fixture ?terminus)
         ;; Static gate-based line-of-sight check
         (los ?area ?terminus)
         ;; Dynamic occlusion/interference check
         (mvsetq ($source-x $source-y) (get-coordinates ?area))
         (mvsetq ($target-x $target-y) (get-coordinates ?terminus))
         (mvsetq ($end-x $end-y)
           (find-first-obstacle-intersection $source-x $source-y $target-x $target-y))
         ;; Verify beam reaches fixture
         (= $end-x $target-x)
         (= $end-y $target-y))
    ;; Case 2: Connectors require geometric beam path validation
    ;; Must verify actual beam reaches target through dynamic obstacles
    (and (connector ?terminus)
         (exists (?target-area area)
           (and (loc ?terminus ?target-area)
                ;; Verify areas are gate-visible
                (visible ?area ?target-area)
                ;; Get source coordinates
                (mvsetq ($source-x $source-y) (get-coordinates ?area))
                ;; Get target connector coordinates
                (mvsetq ($target-x $target-y) (get-coordinates ?terminus))
                ;; Calculate where beam actually terminates
                (mvsetq ($end-x $end-y) 
                  (find-first-obstacle-intersection $source-x $source-y $target-x $target-y))
                ;; Verify beam reaches target
                (= $end-x $target-x)
                (= $end-y $target-y))))))


(define-query passable (?area1 ?area2)
  (or (adjacent ?area1 ?area2)
      (exists (?g gate)
        (and (gate-separates ?g ?area1 ?area2)
             (gate-status ?g open)))))


(define-query vacant (?area)
  (not (exists (?cargo cargo)
         (loc ?cargo ?area))))


(define-query receiver-beam-reaches (?receiver)
  ;; Returns t if a color-matching beam reaches the receiver
  (do
    (mvsetq ($r-x $r-y) (get-coordinates ?receiver))
    (bind (chroma ?receiver $required-hue))
    (exists (?b (get-current-beams))
      (and (bind (beam-segment ?b $source $target $end-x $end-y))
           (= $end-x $r-x)
           (= $end-y $r-y)
           (bind (chroma $source $source-hue))
           (eql $source-hue $required-hue)))))


(define-query receiver-activation-pending ()
  ;; Returns t if any inactive receiver has a matching beam reaching it
  (exists (?r receiver)
    (and (receiver-status ?r inactive)
         (receiver-beam-reaches ?r))))


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


(define-update activate-powered-receivers! ()
  ;; Activates all inactive receivers receiving matching-color beams
  ;; Returns t if any changes made
  (do
    (setq $changed nil)
    (doall (?r receiver)
      (if (and (receiver-status ?r inactive)
               (receiver-beam-reaches ?r))
        (do (receiver-status ?r active)
            (setq $changed t))))
    $changed))


(define-update open-controlled-gates! ()
  ;; Opens all gates controlled by currently active receivers
  (doall (?r receiver)
    (if (receiver-status ?r active)
      (doall (?g gate)
        (if (controls ?r ?g)
          (gate-status ?g open))))))


(define-update create-beam-segment-p! (?source ?target)
  ; Create a beam segment from source, and return the new beam's name.
  (do
    ;; Generate new beam entity with next available index
    (bind (current-beams $current-beams))
    (setq $next-index (1+ (length $current-beams)))
    (setq $new-beam (intern (format nil "BEAM~D" $next-index)))
    (register-dynamic-object $new-beam 'beam)
    ;; Calculate beam path and intersection
    (mvsetq ($source-x $source-y) (get-coordinates ?source))
    (mvsetq ($target-x $target-y) (get-coordinates ?target))
    (mvsetq ($end-x $end-y) (find-first-obstacle-intersection $source-x $source-y $target-x $target-y))
    ;; Create beam relations
    (beam-segment $new-beam ?source ?target $end-x $end-y)
    (current-beams (cons $new-beam $current-beams))
    $new-beam))


(define-update remove-beam-segment-p! (?beam)
  ;; bind and remove beam-segment; if it doesn't exist, return nil
  (if (bind (beam-segment ?beam $source $target $end-x $end-y))
    (do (not (beam-segment ?beam $source $target $end-x $end-y))
        (bind (current-beams $beams))
        (current-beams (remove ?beam $beams))
        t)
    nil))


(define-update recalculate-all-beams! ()
  (doall (?b (get-current-beams))
    (do (bind (beam-segment ?b $source $target $old-end-x $old-end-y))
        ;; Get source coordinates
        (mvsetq ($source-x $source-y) (get-coordinates $source))
        ;; Get target coordinates  
        (mvsetq ($target-x $target-y) (get-coordinates $target))
        ;; Recalculate endpoint using current gate/wall/beam states
        (mvsetq ($new-end-x $new-end-y) (find-first-obstacle-intersection $source-x $source-y $target-x $target-y))
        ;; Update beam segment if endpoint changed
        (if (or (/= $new-end-x $old-end-x) (/= $new-end-y $old-end-y))
          (do (not (beam-segment ?b $source $target $old-end-x $old-end-y))
              (beam-segment ?b $source $target $new-end-x $new-end-y))))))


(define-update update-beams-if-interference! ()
  ;; Simultaneously resolves all beam-beam intersections by truncating interfering beams
  ;; at their intersection points, eliminating sequential processing dependencies
  (do
    ;; Phase 1: For each beam, determine its closest intersection point
    (doall (?b (get-current-beams))
      (do (bind (beam-segment ?b $src $tgt $old-end-x $old-end-y))
          (mvsetq ($src-x $src-y) (get-coordinates $src))
          (mvsetq ($tgt-x $tgt-y) (get-coordinates $tgt))
          ;; CHANGED: Calculate t-parameter for current endpoint position
          (setq $dx (- $tgt-x $src-x))
          (setq $dy (- $tgt-y $src-y))
          (setq $length-sq (+ (* $dx $dx) (* $dy $dy)))
          (setq $curr-dx (- $old-end-x $src-x))
          (setq $curr-dy (- $old-end-y $src-y))
          (setq $dot (+ (* $curr-dx $dx) (* $curr-dy $dy)))
          (setq $closest-t (/ $dot $length-sq))
          (setq $new-end-x $old-end-x)
          (setq $new-end-y $old-end-y)
          ;; Check all intersections involving this beam
          (ww-loop for $intersection in (collect-all-beam-intersections) do
                (setq $beam1 (first $intersection))
                (setq $beam2 (second $intersection))
                (setq $int-x (third $intersection))
                (setq $int-y (fourth $intersection))
                (setq $t1 (fifth $intersection))
                (setq $t2 (sixth $intersection))
                ;; Determine which t-parameter applies to this beam
                (if (eql ?b $beam1)
                  (setq $t-param $t1)
                  (if (eql ?b $beam2)
                    (setq $t-param $t2)
                    (setq $t-param nil)))
                ;; Update closest intersection if this one is closer
                (if (and $t-param (< $t-param $closest-t))
                  (do (setq $closest-t $t-param)
                      (setq $new-end-x $int-x)
                      (setq $new-end-y $int-y))))
          ;; Phase 2: Atomically update beam segment if endpoint changed
          (if (or (/= $new-end-x $old-end-x) (/= $new-end-y $old-end-y))
            (do (not (beam-segment ?b $src $tgt $old-end-x $old-end-y))
                (beam-segment ?b $src $tgt $new-end-x $new-end-y)))))))


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
                     (exists (?b (get-current-beams))
                       (and (bind (beam-segment ?b $source $target $end-x $end-y))
                            (eql $source ?terminus)
                            (eql $target ?r)
                            (mvsetq ($target-x $target-y) (get-coordinates ?r))
                            (= $end-x $target-x)
                            (= $end-y $target-y)))
                     ;; No existing beam, but can create one that reaches target
                     (and (not (exists (?b (get-current-beams))
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
        (if (not (connector-has-valid-line-of-sight ?c ?hue))
          (chain-deactivate! ?c ?hue))))))


(define-update update-beams-if-occluded! (?area)
  ;; Check if placing an object at ?area would occlude existing beam segments
  (do 
    ;; Get coordinates of area where object would be placed
    (mvsetq ($area-x $area-y) (get-coordinates ?area))
    ;; Process each existing beam directly
    (doall (?b beam)
      (do (bind (beam-segment ?b $source $target $end-x $end-y))
          ;; CHANGED: Check if target's coordinates match ?area - if so, this is relay not occlusion
          (mvsetq ($target-x $target-y) (get-coordinates $target))
          (if (or (/= $target-x $area-x) (/= $target-y $area-y))
            ;; Target is elsewhere - check for occlusion
            (do (mvsetq ($start-x $start-y) (get-coordinates $source))
                (mvsetq ($int-t $int-x $int-y)
                        (beam-segment-occlusion $start-x $start-y $end-x $end-y $area-x $area-y))
                ;; Update beam endpoint if occluded
                (if $int-t
                  (do (not (beam-segment ?b $source $target $end-x $end-y))
                      (beam-segment ?b $source $target $area-x $area-y)))))
            ;; else: Target is at ?area coordinates - this is a relay, skip occlusion check
            ))))


;;;; ACTIONS ;;;;


(define-action connect-to-1-terminus
    2
  (?terminus terminus)
  (and (bind (holds agent1 $cargo))
       (connector $cargo)
       (bind (loc agent1 $area))
       (vacant $area)
       (connectable $area ?terminus))
  ($cargo ?terminus $area $hue1)
  (assert (not (holds agent1 $cargo))
        (loc $cargo $area)
        (paired ?terminus $cargo)
        (setq $hue1 (get-hue-if-source ?terminus))
        (if $hue1
          (setq $new-beam1 (create-beam-segment-p! ?terminus $cargo)))  ;truncates beam if occluded
        ; Handle all interference & occlusion before activation decision
        (update-beams-if-interference!)  ;placement of $cargo may alter beam interference
        (update-beams-if-occluded! $area)  ;does ?terminus now occlude any other beam (may update interference)
        ; Activate based on final beam state
        (if $new-beam1
          (do (bind (beam-segment $new-beam1 $src $tgt $end-x1 $end-y1))
              (mvsetq ($cargo-x $cargo-y) (get-coordinates $cargo))
              (if (and (= $end-x1 $cargo-x) (= $end-y1 $cargo-y))
                (activate-connector! $cargo $hue1))))))


(define-action connect-to-2-terminus
    3
  (combination (?terminus1 ?terminus2) terminus)
  (and (bind (holds agent1 $cargo))
       (connector $cargo)
       (bind (loc agent1 $area))
       (vacant $area)
       (connectable $area ?terminus1)
       (connectable $area ?terminus2))
  ($cargo ?terminus1 ?terminus2 $area $hue)
  (assert (not (holds agent1 $cargo))
          (loc $cargo $area)
          (paired $cargo ?terminus1)
          (paired $cargo ?terminus2)
          (setq $hue1 (get-hue-if-source ?terminus1))
          (setq $hue2 (get-hue-if-source ?terminus2))
          (if $hue1 
            (setq $new-beam1 (create-beam-segment-p! ?terminus1 $cargo))
            (setq $new-beam1 (create-beam-segment-p! $cargo ?terminus1)))
          (if $hue2 
            (setq $new-beam2 (create-beam-segment-p! ?terminus2 $cargo))
            (setq $new-beam2 (create-beam-segment-p! $cargo ?terminus2)))
          ; Handle all interference & occlusion before activation decision
          (update-beams-if-interference!)
          (update-beams-if-occluded! $area)
          ; Activate only if $cargo actually receiving power
          (setq $hue (resolve-consensus-hue (list $hue1 $hue2)))
          (if $hue
            (do (mvsetq ($cargo-x $cargo-y) (get-coordinates $cargo))
                (if (or (and $new-beam1
                             (bind (beam-segment $new-beam1 $src1 $tgt1 $end-x1 $end-y1))
                             (= $end-x1 $cargo-x)
                             (= $end-y1 $cargo-y))
                        (and $new-beam2
                             (bind (beam-segment $new-beam2 $src2 $tgt2 $end-x2 $end-y2))
                             (= $end-x2 $cargo-x)
                             (= $end-y2 $cargo-y)))
                  (chain-activate! $cargo $hue))))))


(define-action connect-to-3-terminus
    4
  (combination (?terminus1 ?terminus2 ?terminus3) terminus)
  (and (bind (holds agent1 $cargo))
       (connector $cargo)
       (bind (loc agent1 $area))
       (vacant $area)
       (connectable $area ?terminus1)
       (connectable $area ?terminus2)
       (connectable $area ?terminus3))
  ($cargo ?terminus1 ?terminus2 ?terminus3 $area $hue)
  (assert (not (holds agent1 $cargo))
          (loc $cargo $area)
          (paired $cargo ?terminus1)
          (paired $cargo ?terminus2)
          (paired $cargo ?terminus3)
          (setq $hue1 (get-hue-if-source ?terminus1))
          (setq $hue2 (get-hue-if-source ?terminus2))
          (setq $hue3 (get-hue-if-source ?terminus3))
          (if $hue1 
            (setq $new-beam1 (create-beam-segment-p! ?terminus1 $cargo))
            (setq $new-beam1 (create-beam-segment-p! $cargo ?terminus1)))
          (if $hue2 
            (setq $new-beam2 (create-beam-segment-p! ?terminus2 $cargo))
            (setq $new-beam2 (create-beam-segment-p! $cargo ?terminus2)))
          (if $hue3 
            (setq $new-beam3 (create-beam-segment-p! ?terminus3 $cargo))
            (setq $new-beam3 (create-beam-segment-p! $cargo ?terminus3)))
          ; Handle all interference & occlusion before activation decision
          (update-beams-if-interference!)
          (update-beams-if-occluded! $area)
          ; Activate only if $cargo actually receiving power
          (setq $hue (resolve-consensus-hue (list $hue1 $hue2 $hue3)))
          (if $hue
            (do (mvsetq ($cargo-x $cargo-y) (get-coordinates $cargo))
                (if (or (and $new-beam1
                             (bind (beam-segment $new-beam1 $src1 $tgt1 $end-x1 $end-y1))
                             (= $end-x1 $cargo-x)
                             (= $end-y1 $cargo-y))
                        (and $new-beam2
                             (bind (beam-segment $new-beam2 $src2 $tgt2 $end-x2 $end-y2))
                             (= $end-x2 $cargo-x)
                             (= $end-y2 $cargo-y))
                        (and $new-beam3
                             (bind (beam-segment $new-beam3 $src3 $tgt3 $end-x3 $end-y3))
                             (= $end-x3 $cargo-x)
                             (= $end-y3 $cargo-y)))
                  (chain-activate! $cargo $hue))))))


(define-action pickup-connector
    1
  (?connector connector)
  (and (not (bind (holds agent1 $cargo)))
       (bind (loc agent1 $area))
       (loc ?connector $area))
  (?connector $area)
  (assert (holds agent1 ?connector)
          ;; Step 1: Extract connector coordinates from area
          (mvsetq ($conn-x $conn-y) (get-coordinates $area))
          ;; Step 2: Process beams where connector is occluder (not participant)
          ;; These beams terminate at connector coordinates but connector is neither source nor target
          (doall (?b (get-current-beams))
            (do (bind (beam-segment ?b $source $target $end-x $end-y))
                (if (and (= $end-x $conn-x) (= $end-y $conn-y)
                         (different $source ?connector)
                         (different $target ?connector))
                  ;; Connector was occluding this beam - recalculate beam endpoint
                  (do (mvsetq ($source-x $source-y) (get-coordinates $source))
                      (mvsetq ($target-x $target-y) (get-coordinates $target))
                      (mvsetq ($new-end-x $new-end-y) 
                              (find-first-obstacle-intersection $source-x $source-y $target-x $target-y))
                      ;; Update beam segment with recalculated endpoint
                      (not (beam-segment ?b $source $target $end-x $end-y))
                      (beam-segment ?b $source $target $new-end-x $new-end-y)))))
          ;; Step 3: Remove beam segments where connector is participant (source or target)
          (doall (?b (get-current-beams))
            (do (bind (beam-segment ?b $source $target $end-x $end-y))
                (if (or (eql $source ?connector) (eql $target ?connector))
                  (remove-beam-segment-p! ?b))))
          (update-beams-if-interference!)  ;before chain-deactivate!
          (not (loc ?connector $area))
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
  (and (bind (holds agent1 $cargo))  ;if not holding anything, then bind statement returns nil
       (bind (loc agent1 $area))  ;agent1 is always located somewhere
       (vacant $area))
  ($cargo $area)
  (assert (not (holds agent1 $cargo))
          (loc $cargo $area)
          (update-beams-if-occluded! $area)))


(define-action move
    1
  (?area2 area)
  (and (bind (loc agent1 $area1))
       (different $area1 ?area2)
       (passable $area1 ?area2))
  ($area1 ?area2)
  (assert (loc agent1 ?area2)
          (recalculate-all-beams!)))  ;handle both extension and shortening


;;;; INITIALIZATION ;;;;


(define-init
  ;; Dynamic state (agent-manipulable or derived)
  (loc agent1 area1)
  (loc connector1 area4)
  (loc connector2 area5)
  (loc connector3 area2)
  (current-beams ())  ; Empty - populated by init-action
  (receiver-status receiver1 inactive)  ;default
  (receiver-status receiver2 inactive)
  (receiver-status receiver3 inactive)
  (gate-status gate1 closed)
  
  ;; Static spatial configuration
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
  
  ;; Static object configuration
  (fixpoint transmitter1 32 13)
  (fixpoint transmitter2 25  0)
  (fixpoint receiver1 25 26)
  (fixpoint receiver2  0  7)
  (fixpoint receiver3  0 18)
  (gate-segment gate1 31 15 31 11)
  (wall-segment wall1 33 13 33 13)
  
  ;; Static color assignments
  (chroma transmitter1 blue)
  (chroma transmitter2 red)
  (chroma receiver1 red)
  (chroma receiver2 red)
  (chroma receiver3 blue)
  
  ;; Control relationships
  (controls receiver1 gate1)
  
  ;; Line-of-sight relationships
  (los1 area1 gate1 transmitter1)
  (los0 area1 transmitter2)
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
  
  ;; Visibility relationships
  (visible0 area1 area2)
  (visible0 area1 area3)
  (visible0 area1 area4)
  (visible0 area1 area5)
  (visible1 area1 gate1 area6)
  (visible0 area2 area3)
  (visible0 area2 area4)
  (visible0 area2 area5)
  (visible0 area3 area4)
  (visible0 area3 area5)
  (visible1 area3 gate1 area6)
  (visible0 area4 area5)
)


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert
    ;; Phase 1: Create initial beams and resolve initial interference
    (create-beam-segment-p! transmitter1 receiver3)
    (create-beam-segment-p! transmitter2 receiver1)
    (update-beams-if-interference!)
    
    ;; Phase 2: Initial activation pass (capture result)
    (activate-powered-receivers!)

    ;; Phase 3: Iterate until convergence
    (setq $iteration 0)
    (setq $max-iterations 5)
    
    (ww-loop while (and (< $iteration $max-iterations) 
                        (receiver-activation-pending)) do
      (incf $iteration)
      (open-controlled-gates!)
      (recalculate-all-beams!)
      (update-beams-if-interference!)
      (setq $changed (activate-powered-receivers!)))
    
    ;; Validation
    (if (and (= $iteration $max-iterations) $changed)
      (format t "~&Warning: Initialization did not converge within ~A iterations~%" 
              $max-iterations))))


;;;; GOAL ;;;;

(define-goal  ;always put this last
  (and (receiver-status receiver2 active)
       (receiver-status receiver3 active)))

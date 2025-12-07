;;;; Filename: problem-windtunnel.lisp

;;; Talos Principle problem in Purgatory workshop


(in-package :ww)


(ww-set *problem-name* windtunnel)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 15)

(ww-set *progress-reporting-interval* 10000000)


(define-types
  real-agent      (agent1)  ;the name of the main agent performing actions
  ghost-agent     (agent1*)  ;ghost objects are starred
  agent           (either real-agent ghost-agent)
  recorder        (recorder1)
  gate            (gate1 gate2)
  wall            (wall1 wall2 wall3 wall4 wall5 wall6 wall7 wall8 wall9 wall10)
  real-connector  (connector1)
  ghost-connector (connector1*)
  connector       (either real-connector ghost-connector)
  transmitter     (transmitter1)
  receiver        (receiver1)
  beam            ()  ;initial beams
  repeater        (repeater1)
  plate           (plate1)
  blower          (blower1)
  hue             (blue)  ;the color of a transmitter, receiver, repeater, or active connector
  area            (area1 area2 area3 area4 area5)  ;position points
  real-cargo      (either real-connector)
  ghost-cargo     (either ghost-connector)
  cargo           (either connector)  ;what the player can pickup & carry
  terminus        (either transmitter receiver connector repeater)  ;what a connector can connect to
  source          (either transmitter connector repeater)  ;beam source
  occluder        (either cargo agent gate wall)  ;objects that can occlude a beam
  target          (either connector receiver repeater)  ;beam target
  focus           (either transmitter receiver repeater)  ;los object of interest
  fixture         (either transmitter receiver recorder repeater plate blower)
)


(define-dynamic-relations  ;relations with fluents can be bound in rules--eg (bind (holds agent1 $any-cargo))
  (holds agent $cargo)  ;fluent because we may need to lookup what is currently being held
  (loc (either agent cargo) $area)  ;a location in an area
  (open gate)  ;a gate can be open or not open, default is not open
  (active (either plate blower receiver))
  (paired terminus terminus)  ;potential beam between two terminus
  (color (either connector repeater) $hue)  ;having a color means it is active
  (beam-segment beam $source $target $rational $rational)  ;endpoint-x endpoint-y
  (current-beams $list)
  (ghost-toggled-active plate)  ;used to track ghost plate activations during playback to catch illegal moves
  (real-blower-moves $list)
)


(define-static-relations
  (coords (either area fixture) $fixnum $fixnum)  ;the (x,y) position
  (controls (either receiver plate) (either gate blower))
  (blows> blower $area $area)
  (chroma (either transmitter receiver) $hue)
  (gate-segment gate $fixnum $fixnum $fixnum $fixnum)
  (wall-segment wall $fixnum $fixnum $fixnum $fixnum)
  (toggles plate (either gate blower))
  (chroma (either transmitter receiver) $hue)  ;fixed color
  (gate-segment gate $fixnum $fixnum $fixnum $fixnum)
  ;potential clear los from an area to a focus
  (los0 area focus)  
  (los1 area (either $gate $area) focus)  ;los can be blocked either by a closed $gate or object in $area
  ;potential visibility from an area to another area
  (visible0 area area)  
  (visible1 area (either $gate $area) area)
  ;potential accesibility to move from an area to another area
  (accessible0 area area)
  (accessible1 area (either $gate $blower) area)
)


;;;; QUERY FUNCTIONS ;;;;


(define-query heuristic? ()
  ;; Estimates minimum actions to reach goal (agent1 in area5)
  (do (bind (loc agent1 $area))
      ;; Base distance considering topology
      (setq $base-distance
        (cond ((eql $area 'area5) 0)    ; At goal
              ((eql $area 'area4) 1)    ; One move (through gate2)
              ((eql $area 'area3) 2)    ; area3 -> area4 -> area5
              ;; From area1/area2: can reach area4 directly if blower off
              ((or (eql $area 'area1) (eql $area 'area2)) 2)
              (t 10)))                   ; Should not occur
      ;; Penalties for obstacles on path to goal
      (setq $penalty 0)
      ;; blower blocks area1/area2 -> area3/area4 passage
      (if (and (or (eql $area 'area1) (eql $area 'area2))
               (active blower1))
        (setq $penalty (+ $penalty 1)))  ; Need to toggle plate1
      ;; Gate2 blocks area4 -> area5 passage
      (if (and (not (eql $area 'area5))
               (not (open gate2)))
        (setq $penalty (+ $penalty 1)))  ; Need to activate receiver1
      ;; Return total heuristic value
      (+ $base-distance $penalty)))


(define-query get-current-beams ()
  (do (bind (current-beams $beams))
      (remove nil $beams)))  ; Filter out placeholders


(define-query get-hue-if-source (?terminus)
  ;gets the hue of ?terminus if it is a source, else nil
  (or (and (transmitter ?terminus)
           (bind (chroma ?terminus $hue))
           $hue)
      (and (connector ?terminus)
           (bind (color ?terminus $hue))
           $hue)
      (and (repeater ?terminus)
           (bind (color ?terminus $hue))
           $hue)))


(define-query get-fixed-coordinates (?area/fixture)
  ;; Efficiently get coordinates for fixed objects
  (do (bind (coords ?area/fixture $x $y))
      (values $x $y)))


(define-query get-coordinates (?object)
  ;; Finds coordinates for any arbitrary object
  (cond 
    ((or (area ?object) (fixture ?object))
     (bind (coords ?object $x $y))
     (values $x $y))
    ((or (agent ?object) (and (cargo ?object) (not (holds agent1 ?object))))
     (bind (loc ?object $area))
     (bind (coords $area $x $y))
     (values $x $y))
    (t (error "~%No coordinates found for ~A~%" ?object))))


(define-query beam-exists-p (?source ?target)
  ; Returns t if a beam already exists from ?source to ?target
  (exists (?b (get-current-beams))
    (and (bind (beam-segment ?b $src $tgt $end-x $end-y))
         (eql $src ?source)
         (eql $tgt ?target))))


(define-query beam-reaches-target (?source ?target)
  ; Returns t if a beam from ?source to ?target reaches ?target's coordinates
  (do (mvsetq ($target-x $target-y) (get-coordinates ?target))
      (exists (?b (get-current-beams))
        (and (bind (beam-segment ?b $src $tgt $end-x $end-y))
             (eql $src ?source)
             (eql $tgt ?target)
             (= $end-x $target-x)
             (= $end-y $target-y)))))


(define-query los (?agent ?area ?fixture)
  ;; For player: respects gate open states and area occupancy
  ;; For ghost: ignores environmental states (recorded when different)
  (or (los0 ?area ?fixture)
      (and (bind (los1 ?area $zone ?fixture))
           (or (ghost-agent ?agent)
               (and (gate $zone)
                    (open $zone))
               (and (area $zone)
                    (not (exists (?obj (either agent cargo))
                           (loc ?obj $zone))))))))


(define-query visible (?agent ?area1 ?area2)
  ;; For player: respects gate open states and area occupancy to connect connectors
  ;; For ghost: ignores environmental states (recorded when different)
  (or (visible0 ?area1 ?area2)
      (and (bind (visible1 ?area1 $zone ?area2))
           (or (ghost-agent ?agent)
               (and (gate $zone)
                    (open $zone))
               (and (area $zone)
                    (not (exists (?obj (either agent cargo))
                           (loc ?obj $zone))))))))


(define-query observable (?area ?terminus)
  ;; Static observability check - ignores gate state and area occupancy
  ;; Returns t if terminus could potentially be seen from ?area
  (or
    ;; Case 1: Fixture terminus - check los relationships exist
    (and (fixture ?terminus)
         (or (los0 ?area ?terminus)
             (bind (los1 ?area $zone ?terminus))))
    ;; Case 2: Connector terminus - check visible relationships to connector's area
    (and (connector ?terminus)
         (bind (loc ?terminus $target-area))
         (or (visible0 ?area $target-area)
             (bind (visible1 ?area $zone $target-area))))))


(define-query selectable (?agent ?area ?terminus)
  ;; Agent can select terminus if observable from ?area or accessible adjacent area
  (or (observable ?area ?terminus)
      (exists (?adj-area area)
        (and (accessible ?agent ?area ?adj-area)
             (observable ?adj-area ?terminus)))))


(define-query accessible (?agent ?area1 ?area2)
  ;; For player: respects gate and blower environmental states
  ;; For ghost: ignores environmental states (replaying recorded script)
  (or (accessible0 ?area1 ?area2)
      (exists (?g gate)
        (and (accessible1 ?area1 ?g ?area2)
             (or (ghost-agent ?agent)
                 (open ?g))))
      (exists (?b blower)
        (and (accessible1 ?area1 ?b ?area2)
             (or (ghost-agent ?agent)
                 (not (active ?b)))))))


(define-query placeable (?cargo ?area)
  ;Cargo cannot colocate with another cargo of the same type
  (not (or (and (real-cargo ?cargo)
                (exists (?rc real-cargo)
                  (loc ?rc ?area)))
           (and (ghost-cargo ?cargo)
                (exists (?gc ghost-cargo)
                  (loc ?gc ?area))))))


(define-query resolve-consensus-hue (?hue-list)
  ; Returns consensus hue from list of available hues, nil if no consensus
  (do (setq $unique-hues (remove-duplicates (remove nil ?hue-list)))
      (if (= (length $unique-hues) 1)
        (first $unique-hues)  ; Single unique hue (consensus achieved)
        nil)))               ; Multiple different hues or no hues (no consensus)


(define-query beam-segment-interference
    (?source-x ?source-y ?end-x ?end-y ?cross-x1 ?cross-y1 ?cross-x2 ?cross-y2)
  ; Determines if a cross segment (beam, gate, etc) interferes with the main beam-segment
  ; Returns the interference endpoint, or nil
  ; Step 1: Calculate direction vectors and displacement
  (do (setq $dx1 (- ?end-x ?source-x))    ; Beam direction x
      (setq $dy1 (- ?end-y ?source-y))    ; Beam direction y
      (setq $dx2 (- ?cross-x2 ?cross-x1)) ; Obstacle direction x
      (setq $dy2 (- ?cross-y2 ?cross-y1)) ; Obstacle direction y
      (setq $dx3 (- ?cross-x1 ?source-x)) ; Displacement x
      (setq $dy3 (- ?cross-y1 ?source-y)) ; Displacement y
      ;; Step 2: Calculate determinant for parallel line detection
      (setq $det (- (* $dy1 $dx2) (* $dx1 $dy2)))
      ;; Step 3: Handle parallel lines case
      (if (< (abs $det) 1e-10)
        (values nil nil nil)
        ;; Step 4: Solve for intersection parameters using Cramer's rule
        (do (setq $t (/ (- (* $dy3 $dx2) (* $dx3 $dy2)) $det))
            (setq $s (/ (- (* $dx1 $dy3) (* $dy1 $dx3)) $det))
            ;; Treat hits within 2% of either endpoint as "at the endpoint" => no interference
            (setq $eps 2e-2)  ; parameter-space epsilon (2%)
            ;; Step 5: Validate intersection lies well inside both segments
            (if (and (> $t $eps) (< $t (- 1.0 $eps))      ; main beam interior only
                     (> $s $eps) (< $s (- 1.0 $eps)))     ; cross segment interior only
              ;; Step 6: Calculate and return intersection coordinates
              (do (setq $int-x (+ ?source-x (* $t $dx1)))
                  (setq $int-y (+ ?source-y (* $t $dy1)))
                  (values $t $int-x $int-y))
              ;; No valid intersection within (epsilon-trimmed) interiors
              (values nil nil nil))))))


(define-query beam-segment-occlusion (?source-x ?source-y ?end-x ?end-y ?px ?py)
  ; Determines if an object in an area at (?px,?py) occludes a beam-segment with tolerance < 1.0
  ; Step 1: Calculate beam direction and length
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
                  (if (< $distance (+ 0.25d0 1d-6))  ; Tolerance for blocking the beam with epsilon
                    (values $t $closest-x $closest-y)  ; Point blocks beam
                    (values nil nil nil)))             ; Point too far from beam
              ;; Projection falls outside segment bounds
              (values nil nil nil))))))


(define-query find-first-obstacle-intersection (?source-x ?source-y ?target-x ?target-y)
  ; Establishes each beam's intended full path by finding intersections with static obstacles only
  ; Returns the endpoint coordinates where the beam terminates and what blocks it
  (do
    ;; Initialize closest intersection tracking
    (setq $closest-t 1.0)  ; Default to target if no intersections
    (setq $result-x ?target-x)   
    (setq $result-y ?target-y)
    ;; Check static obstacles only - exclude beams
    (doall (?obj occluder)
      (if (and ;; Type-specific coordinate binding and conditions
               (or (and (gate ?obj) 
                        (not (open ?obj))  ; Block UNLESS explicitly open
                        (bind (gate-segment ?obj $x1 $y1 $x2 $y2)))
                   (and (wall ?obj)
                        (bind (wall-segment ?obj $x1 $y1 $x2 $y2)))
                   (and (or (cargo ?obj) (agent ?obj))
                        (bind (loc ?obj $area))
                        (bind (coords $area $x1 $y1))
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
                ;; Line segment interference (gates)
                (beam-segment-interference ?source-x ?source-y ?target-x ?target-y $x1 $y1 $x2 $y2)))
            (if (and $int-t (< $int-t $closest-t))
              (do (setq $closest-t $int-t)
                  (setq $result-x $int-x)
                  (setq $result-y $int-y))))))
    ;; Return closest intersection coordinates
    (values $result-x $result-y)))


(define-query collect-all-beam-intersections ()
  ; Use current endpoints (actual segments), not intended targets
  ; Returns list of intersection records: ((beam1 beam2 intersection-x intersection-y t1 t2) ...)
  (do 
      ;; Build coordinate cache for all beam sources
      (setq $coord-cache (make-hash-table :test 'eq))
      (doall (?b (get-current-beams))
        (do (bind (beam-segment ?b $src $tgt $end-x $end-y))
            ;; Cache source coordinates if not already cached
            (if (not (gethash $src $coord-cache))
              (do (mvsetq ($x $y) (get-coordinates $src))
                  (setf (gethash $src $coord-cache) (list $x $y))))))
      ;; Detect intersections using cached coordinates
      (doall (?b1 (get-current-beams))
        (doall (?b2 (get-current-beams))
          (if (and (different ?b1 ?b2)
                   (string< (symbol-name ?b1) (symbol-name ?b2))) ; Avoid duplicate pairs
            (do (bind (beam-segment ?b1 $src1 $tgt1 $end1-x $end1-y))
                (bind (beam-segment ?b2 $src2 $tgt2 $end2-x $end2-y))
                (setq $src1-coords (gethash $src1 $coord-cache))
                (setq $src1-x (first $src1-coords))
                (setq $src1-y (second $src1-coords))
                (setq $tgt1-x $end1-x)
                (setq $tgt1-y $end1-y)
                (setq $src2-coords (gethash $src2 $coord-cache))
                (setq $src2-x (first $src2-coords))
                (setq $src2-y (second $src2-coords))
                (setq $tgt2-x $end2-x)
                (setq $tgt2-y $end2-y)
                ;; Check intersection between current segments (not intended paths)
                (mvsetq ($t1 $int-x $int-y)
                  (beam-segment-interference
                    $src1-x $src1-y $tgt1-x $tgt1-y
                    $src2-x $src2-y $tgt2-x $tgt2-y))
                (if $t1
                  (do
                    ;; Get t2 parameter by checking beam2 vs beam1
                    (mvsetq ($t2 $int-x2 $int-y2)
                      (beam-segment-interference
                        $src2-x $src2-y $tgt2-x $tgt2-y
                        $src1-x $src1-y $tgt1-x $tgt1-y))
                    ;; Only push if both intersections valid
                    (if $t2
                      (push (list ?b1 ?b2 $int-x $int-y $t1 $t2) $intersections))))))))
      $intersections))


(define-query receiver-beam-reaches (?receiver)
  ; Returns t if a color-matching beam reaches the receiver
  (do
    (mvsetq ($r-x $r-y) (get-fixed-coordinates ?receiver))
    (bind (chroma ?receiver $required-hue))
    (exists (?b (get-current-beams))
      (and (bind (beam-segment ?b $source $target $end-x $end-y))
           (= $end-x $r-x)
           (= $end-y $r-y)
           ;; Get hue from source
           (or (bind (chroma $source $source-hue))
               (bind (color $source $source-hue)))
           (eql $source-hue $required-hue)))))


(define-query connector-has-beam-power (?connector ?hue)
  ; Returns t if a beam with matching hue reaches the connector at its current coordinates
  (do
    (mvsetq ($c-x $c-y) (get-coordinates ?connector))
    (exists (?b (get-current-beams))
      (and (bind (beam-segment ?b $source $target $end-x $end-y))
           (eql $target ?connector)  ; Beam must target this connector
           (= $end-x $c-x)            ; Beam must reach connector coordinates
           (= $end-y $c-y)
           ;; Verify source has matching hue
           (or (bind (chroma $source $source-hue))
               (bind (color $source $source-hue)))
           (eql $source-hue ?hue)))))


(define-query terminus-is-powered-by (?connector ?potential-source)
  ;; Returns t if ?connector is currently receiving power from ?potential-source
  ;; This prevents creating reciprocal beams back to the power source
  (and (or (connector ?connector) (repeater ?connector))
       (bind (color ?connector $conn-hue))  ; Connector/repeater must be active
       ;; Check if a beam exists from ?potential-source to ?connector
       (exists (?b (get-current-beams))
         (and (bind (beam-segment ?b $src $tgt $end-x $end-y))
              (eql $src ?potential-source)
              (eql $tgt ?connector)
              ;; Verify beam actually reaches connector's coordinates
              (mvsetq ($conn-x $conn-y) (get-coordinates ?connector))
              (= $end-x $conn-x)
              (= $end-y $conn-y)
              ;; Verify source has matching hue (confirming power flow)
              (or (bind (chroma $src $src-hue))     ; Transmitter
                  (bind (color $src $src-hue)))     ; Active connector/repeater
              (eql $src-hue $conn-hue)))))


(define-query collect-transmitter-powered-relays ()
  ;; BFS from all transmitters, returning list of relays reachable via beam chains
  (do
    ;; Initialize frontier with all transmitters
    (setq $frontier nil)
    (doall (?t transmitter)
      (push ?t $frontier))
    ;; Initialize result set
    (setq $powered-relays nil)
    ;; BFS loop - continue while frontier has sources to process
    (ww-loop while $frontier do
      (setq $next-frontier nil)
      ;; Process each source in current frontier
      (ww-loop for $source in $frontier do
        ;; Check all beams originating from this source
        (doall (?b (get-current-beams))
          (do (bind (beam-segment ?b $src $tgt $end-x $end-y))
              (if (eql $src $source)
                ;; Beam originates from current source - check if target is a relay
                (if (or (connector $tgt) (repeater $tgt))
                  ;; Verify beam actually reaches target coordinates
                  (do (mvsetq ($tgt-x $tgt-y) (get-coordinates $tgt))
                      (if (and (= $end-x $tgt-x)
                               (= $end-y $tgt-y)
                               (not (member $tgt $powered-relays)))
                        ;; Target is newly discovered powered relay
                        (do (push $tgt $powered-relays)
                            (push $tgt $next-frontier)))))))))
      ;; Advance to next frontier
      (setq $frontier $next-frontier))
    $powered-relays))


(define-query find-blower-on-path? (?area1 ?area2)
  ;; Returns the blower controlling path from ?area1 to ?area2, or nil if none.
  (ww-loop for ?b in (gethash 'blower *types*)
           do (if (accessible1 ?area1 ?b ?area2)
                (return ?b))))


(define-query playback-blower-active? (?blower)
  ;; Returns t if blower will be active when playback begins.
  ;; This occurs when any controlling plate has been ghost-toggled to active.
  (exists (?p plate)
    (and (controls ?p ?blower)
         (ghost-toggled-active ?p))))


(define-query recording-playback-valid? ()
  ;; Validates that all real agent moves through blower-controlled paths
  ;; were legal given the recording-final blower states.
  ;; Returns t if valid, nil if any move was illegal.
  (do (bind (real-blower-moves $moves))
      (not (ww-loop for $move in $moves
                    thereis (playback-blower-active? (third $move))))))


;;;; UPDATE FUNCTIONS ;;;;


(define-update propagate-changes! ()
  (ww-loop for $iteration from 1 to 10
           do (if (not (propagate-consequences!))
                (return))  ;convergence achieved
           finally (inconsistent-state)))  ;no convergence, mark state inconsistent


(define-update propagate-consequences! ()
  ;; All functions must execute in order; returns t if any change occurred
  (some #'identity
        (mapcar (lambda (fn) (funcall fn state))
                (list #'update-plate-controlled-devices!
                      #'blow-real-objects-if-active!
                      #'create-missing-beams!
                      #'remove-orphaned-beams!
                      #'recalculate-all-beams!          ;always returns nil
                      #'update-beams-if-interference!   ;always returns nil
                      #'deactivate-receivers-that-lost-power!
                      #'deactivate-unpowered-relays!
                      #'activate-receivers-that-gained-power!
                      #'activate-reachable-connectors!))))


(define-update update-plate-controlled-devices! ()
  ;; Propagates plate activation state to controlled gates and blowers
  ;; Returns t if any change occurred, nil otherwise
  (do
    (doall (?p plate)
      (if (active ?p)
        ;; Plate is active: open controlled gates, activate controlled blowers
        (do (doall (?g gate)
              (if (and (controls ?p ?g) (not (open ?g)))
                (do (open ?g)
                    (setq $changed t))))
            (doall (?b blower)
              (if (and (controls ?p ?b) (not (active ?b)))
                (do (active ?b)
                    (setq $changed t)))))
        ;; Plate is inactive: close controlled gates, deactivate controlled blowers
        (do (doall (?g gate)
              (if (and (controls ?p ?g) (open ?g))
                (do (not (open ?g))
                    (setq $changed t))))
            (doall (?b blower)
              (if (and (controls ?p ?b) (active ?b))
                (do (not (active ?b))
                    (setq $changed t)))))))
    $changed))


(define-update blow-real-objects-if-active! ()
  ;; Blows real objects according to blows> relation when blower is active.
  ;; Ghost agents are ALWAYS immune to blower physics during playback -
  ;; they follow predetermined scripts that were valid when recorded.
  ;; Only real agent and real cargo are ever affected.
  ;; Returns t if any object was moved, nil otherwise.
  (do
    (setq $moved-any nil)
    (doall (?b blower)
      (if (and (active ?b)
               (bind (blows> ?b $from $to)))
        (do (if (loc agent1 $from)
              (do (loc agent1 $to)
                  (setq $moved-any t)))
            (doall (?rc real-cargo)
              (if (loc ?rc $from)
                (do (loc ?rc $to)
                    (setq $moved-any t)))))))
    $moved-any))


(define-update create-missing-beams! ()
  ;; Creates beams for active sources paired with targets where no beam exists yet
  ;; Returns t if any beams were created, nil otherwise
  (do
    (setq $created-any nil)
    (doall (?src terminus)
      (doall (?tgt terminus)
        (if (and (different ?src ?tgt)
                 (or (paired ?src ?tgt) (paired ?tgt ?src)))  ; Pairing exists (bidirectional)
          ;; Have a pairing - check if source can emit and beam should exist
          (do (setq $source-hue (get-hue-if-source ?src))
              (if (and $source-hue                              ; Source is active
                       (target ?tgt)
                       (not (beam-exists-p ?src ?tgt))          ; Beam doesn't exist yet
                       (not (terminus-is-powered-by ?src ?tgt))) ; ?tgt is not powering ?src
                ;; Create beam: source is transmitter (always active) or powered connector
                (do (create-beam-segment-p! ?src ?tgt)
                    (setq $created-any t)))))))
    $created-any))


(define-update remove-orphaned-beams! ()
  ;; Removes beams whose pairing no longer exists or whose source lost power
  ;; Returns t if any beams were removed, nil otherwise
  (do
    (setq $removed-any nil)
    (doall (?b (get-current-beams))
      (do (bind (beam-segment ?b $src $tgt $end-x $end-y))
          ;; Determine if beam should be removed
          (setq $should-remove nil)
          ;; Reason 1: Pairing no longer exists (check bidirectional)
          (if (not (or (paired $src $tgt) (paired $tgt $src)))
            (setq $should-remove t))
          ;; Reason 2: Source is connector that lost power (no color binding)
          (if (and (or (connector $src) (repeater $src))
                   (not (bind (color $src $hue))))
            (setq $should-remove t))
          ;; Execute removal if needed
          (if $should-remove
            (do (remove-beam-segment-p! ?b)
                (setq $removed-any t)))))
    $removed-any))


(define-update recalculate-all-beams! ()
  (do (doall (?b (get-current-beams))
        (do (bind (beam-segment ?b $source $target $old-end-x $old-end-y))
            ;; Get source coordinates
            (mvsetq ($source-x $source-y) (get-coordinates $source))
            ;; Get target coordinates  
            (mvsetq ($target-x $target-y) (get-coordinates $target))
            ;; Recalculate endpoint using current gate/beam states
            (mvsetq ($new-end-x $new-end-y)
                    (find-first-obstacle-intersection $source-x $source-y $target-x $target-y))
            ;; Update beam segment if endpoint changed
            (if (or (/= $new-end-x $old-end-x)
                    (/= $new-end-y $old-end-y))
              (beam-segment ?b $source $target $new-end-x $new-end-y))))
      nil))  ;must return nil


(define-update update-beams-if-interference! ()
  ;; Simultaneously resolves all beam-beam intersections by truncating interfering beams
  ;; at their intersection points, eliminating sequential processing dependencies
  (do
    ;; Phase 1: For each beam, determine its closest intersection point
    (doall (?b (get-current-beams))
      (do (bind (beam-segment ?b $src $tgt $old-end-x $old-end-y))
          (mvsetq ($src-x $src-y) (get-coordinates $src))
          (mvsetq ($tgt-x $tgt-y) (get-coordinates $tgt))
          ;; Calculate t-parameter for current endpoint position
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
                ;; Determine which t-parameter applies to this beam and identify blocking beam
                (if (eql ?b $beam1)
                  (do (setq $t-param $t1)
                      (setq $blocking-beam $beam2))
                  (if (eql ?b $beam2)
                    (do (setq $t-param $t2)
                        (setq $blocking-beam $beam1))
                    (setq $t-param nil)))
                ;; Update closest intersection if this one is closer
                (if (and $t-param (< $t-param $closest-t))
                  (do (setq $closest-t $t-param)
                      (setq $new-end-x $int-x)
                      (setq $new-end-y $int-y))))
          ;; Phase 2: Atomically update beam segment if endpoint changed
          (if (or (/= $new-end-x $old-end-x) 
                  (/= $new-end-y $old-end-y))
            (beam-segment ?b $src $tgt $new-end-x $new-end-y))))
    nil))  ;must return nil


(define-update deactivate-receivers-that-lost-power! ()
  ;; Returns t if any receiver was deactivated, nil otherwise
  (do
    (doall (?r receiver)
      (if (and (active ?r)
               (not (receiver-beam-reaches ?r)))
        (do (deactivate-receiver! ?r)
            (setq $any-deactivated t))))
    $any-deactivated))


(define-update deactivate-unpowered-relays! ()
  ;; Returns t if any connector/repeater was deactivated, nil otherwise
  ;; Uses forward reachability from transmitters to detect true power
  (do
    (setq $deactivated-any nil)
    ;; Get set of relays with valid transmitter power
    (setq $powered-relays (collect-transmitter-powered-relays))
    (doall (?c (either connector repeater))
      (if (bind (color ?c $c-hue))
        ;; Relay is currently active - verify it has transmitter power
        (if (not (member ?c $powered-relays))
          ;; No path to transmitter - deactivate
          (do
            (not (color ?c $c-hue))
            (setq $deactivated-any t)))))
    $deactivated-any))


(define-update activate-receivers-that-gained-power! ()
  ;; Returns t if any receiver was activated, nil otherwise
  (do
    (doall (?r receiver)
      (if (and (not (active ?r))
               (receiver-beam-reaches ?r))
        (do (active ?r)
            (doall (?g gate)
              (if (controls ?r ?g)
                (open ?g)))
            (setq $any-activated t))))
    $any-activated))


(define-update activate-reachable-connectors! ()
  ;; Returns t if any connector was activated, nil otherwise
  ;; Now uses consensus logic: connector only activates if ALL reaching beams have same hue
  (do
    (setq $activated-any nil)
    (doall (?c (either connector repeater))
      (if (not (bind (color ?c $existing-hue)))
        (do
          ;; Collect all hues from beams that reach this connector
          (setq $reaching-hues nil)
          ;; Check all paired termini that could be sources
          (doall (?src terminus)
            (if (or (paired ?c ?src) (paired ?src ?c))  ; Pairing exists (bidirectional)
              (do
                ;; Get source hue if it's an active source (transmitter or powered connector)
                (setq $src-hue (get-hue-if-source ?src))
                ;; If source has hue AND beam reaches connector, collect hue
                (if (and $src-hue
                         (beam-reaches-target ?src ?c))
                  (push $src-hue $reaching-hues)))))
          ;; Check for consensus among all reaching hues
          (setq $consensus-hue (resolve-consensus-hue $reaching-hues))
          ;; Only activate if consensus achieved (all hues identical)
          (if $consensus-hue
            (do (color ?c $consensus-hue)
                (setq $activated-any t))))))
    $activated-any))


(define-update deactivate-receiver! (?receiver)
  (do (not (active ?receiver))
      (doall (?g gate)
        (if (controls ?receiver ?g)
          (not (open ?g))))))


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
  (if (bind (beam-segment ?beam $source $target $end-x $end-y))
    (do (not (beam-segment ?beam $source $target $end-x $end-y))
        (bind (current-beams $beams))
        (current-beams (substitute nil ?beam $beams))
        t)
    nil))


;;;; ACTIONS ;;;;


(define-action connect-to-1-terminus
    1
  (?agent agent ?terminus terminus)
  (and (bind (holds ?agent $cargo))
       (or (and (real-agent ?agent) (real-cargo $cargo))
           (and (ghost-agent ?agent) (ghost-cargo $cargo)))
       (different $cargo ?terminus)
       (bind (loc ?agent $area))
       (placeable $cargo $area)
       (selectable ?agent $area ?terminus))
  (?agent $cargo ?terminus $area)
  (assert (not (holds ?agent $cargo))
          (loc $cargo $area)
          (paired $cargo ?terminus)
          (propagate-changes!)))


(define-action connect-to-2-terminus
    1
  (?agent agent (combination (?t1 ?t2) terminus))
  (and (bind (holds ?agent $cargo))
       (or (and (real-agent ?agent) (real-cargo $cargo))
           (and (ghost-agent ?agent) (ghost-cargo $cargo)))
       (different $cargo ?t1)
       (different $cargo ?t2)
       (bind (loc ?agent $area))
       (placeable $cargo $area)
       (selectable ?agent $area ?t1)
       (selectable ?agent $area ?t2))
  (?agent $cargo ?t1 ?t2 $area)
  (assert (not (holds ?agent $cargo))
          (loc $cargo $area)
          (paired $cargo ?t1)
          (paired $cargo ?t2)
          (propagate-changes!)))


(define-action connect-to-3-terminus
    1
  (?agent agent (combination (?t1 ?t2 ?t3) terminus))
  (and (bind (holds ?agent $cargo))
       (or (and (real-agent ?agent) (real-cargo $cargo))
           (and (ghost-agent ?agent) (ghost-cargo $cargo)))
       (different $cargo ?t1)
       (different $cargo ?t2)
       (different $cargo ?t3)
       (bind (loc ?agent $area))
       (placeable $cargo $area)
       (selectable ?agent $area ?t1)
       (selectable ?agent $area ?t2)
       (selectable ?agent $area ?t3))
  (?agent $cargo ?t1 ?t2 ?t3 $area)
  (assert (not (holds ?agent $cargo))
          (loc $cargo $area)
          (paired $cargo ?t1)
          (paired $cargo ?t2)
          (paired $cargo ?t3)
          (propagate-changes!)))


(define-action pickup-connector
    1
  (?agent agent ?cargo cargo)
  (and (or (and (real-agent ?agent) (real-cargo ?cargo))
           (and (ghost-agent ?agent) (ghost-cargo ?cargo)))
       (not (bind (holds ?agent $held)))
       (bind (loc ?agent $area))
       (loc ?cargo $area))
  (?agent ?cargo $area)
  (assert (holds ?agent ?cargo)
          (not (loc ?cargo $area))
          ;; Remove pairings before convergence (Phase 0 needs this)
          (doall (?t terminus)
            (if (paired ?cargo ?t)
              (not (paired ?cargo ?t))))
          ;; Deactivate connector if active
          (if (bind (color ?cargo $hue))
            (not (color ?cargo $hue)))
          (propagate-changes!)))


(define-action drop
    1
  (?agent agent)
  (and (bind (holds ?agent $cargo))
       (bind (loc ?agent $area))
       (placeable $cargo $area))
  (?agent $cargo $area)
  (assert (not (holds ?agent $cargo))
          (loc $cargo $area)
          (propagate-changes!)))


(define-action toggle-plate
  1
  (?agent agent ?plate plate)
  (and (bind (loc ?agent $area))
       (bind (coords $area $agent-x $agent-y))
       (bind (coords ?plate $plate-x $plate-y))
       (= $agent-x $plate-x)
       (= $agent-y $plate-y))
  (?agent ?plate)
  (assert (if (active ?plate)
            (not (active ?plate))
            (active ?plate))
          ;; Track ghost toggle parity for playback validation
          (if (ghost-agent ?agent)
            (if (ghost-toggled-active ?plate)
              (not (ghost-toggled-active ?plate))
              (ghost-toggled-active ?plate)))
          (propagate-changes!)))


(define-action move
    1
  (?agent agent ?area2 area)
  (and (bind (loc ?agent $area1))
       (different $area1 ?area2)
       (accessible ?agent $area1 ?area2)
       ;; Capture blower if real agent uses blower-controlled path
       (setq $blower-used
             (if (real-agent ?agent)
               (find-blower-on-path? $area1 ?area2)
               nil)))
  (?agent $area1 ?area2)
  (assert (loc ?agent ?area2)
          ;; Record real agent blower-path moves for validation
          (if $blower-used
            (do (bind (real-blower-moves $moves))
                (real-blower-moves (cons (list $area1 ?area2 $blower-used)
                                         $moves))))
          (propagate-changes!)))


;;;; INITIALIZATION ;;;;


(define-init
  ;; Dynamic state (agent-manipulable or derived)
  (loc agent1 area1)
  (loc agent1* area1)
  (loc connector1 area1)
  (loc connector1* area1)
  (current-beams ())  ; Empty - can be populated by init-action, if exist initially
  (real-blower-moves ())  ;empty list of recorded blower-path moves

  ;; Static spatial configuration
  (coords area1  4 20)
  (coords area2 11 14)
  (coords area3 21 8)
  (coords area4 22 3)
  (coords area5 14 3)
  (accessible0 area1 area2)
  (accessible0 area3 area4)
  (accessible1 area4 gate2 area5)
  (accessible1 area1 blower1 area3)
  (accessible1 area2 blower1 area3)
  (accessible1 area2 blower1 area4)

  ;; Static fixture configuration
  (coords recorder1 4 20)
  (coords transmitter1 19 19)
  (coords plate1 11 14)
  (coords repeater1 3 8)
  (coords blower1 23 8)
  (coords receiver1 21 0)
  (controls plate1 gate1)
  (controls plate1 blower1)
  (controls receiver1 gate2)
  (blows> blower1 area3 area1)
  (gate-segment gate1 12 21 12 17)
  (gate-segment gate2 17 6 17 0)
  (wall-segment wall1 19 21 19 17)
  (wall-segment wall2 19 17 12 17)
  (wall-segment wall3 12 17 12 10)
  (wall-segment wall4 12 10 23 10)
  (wall-segment wall5 23 10 23 0)
  (wall-segment wall6 23 0 0 0)
  (wall-segment wall7 0 0 0 21)
  (wall-segment wall8 0 21 19 21)
  (wall-segment wall9 12 0 12 6)
  (wall-segment wall10 12 6 19 6)
  
  ;; Static color assignments
  (chroma transmitter1 blue)
  (chroma receiver1 blue)
  
  ;; Line-of-sight relationships (connector area to fixture)
  (los1 area1 gate1 transmitter1)
  (los0 area1 repeater1)
  (los0 area3 repeater1)
  (los0 area3 receiver1)
  
  ;; Visibility relationships (connector area to area)
  (visible0 area1 area2)
  (visible0 area3 area4)
  (visible1 area4 gate2 area5)
)


;;;; GOAL ;;;;


(define-goal
  (and (loc agent1 area5)
       (if (recording-playback-valid?)
         (ut::prt '----valid-state state t)
         nil)))  ;invalid state, return nil, continue searching
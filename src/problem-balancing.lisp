;;; Filename: problem-balancing.lisp


;;; Enhanced problem specification in the Talos Principle game
;;; for the Balancing Act problem in The Lost Prisoner.
;;; Deals with beams that can be occluded or intersecting.
;;; Requires explicit object coordinates and obstructions like walls.


(in-package :ww)  ;required

(ww-set *problem-name* balancing)

(ww-set *problem-type* planning)

(ww-set *solution-type* first)  ;min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 2)


(define-types
  agent       (agent1)  ;the name of the agent performing actions
  gate        (gate1 gate2 gate3)
  barrier     (barrier1)
  hatch       (hatch1)
  plate       (plate1 plate2 plate3 plate4)
  blower      (blower1)
;  wall        (wall1)
  connector   (connector1 connector2 connector3 connector4)
  box         (box1)
  transmitter (transmitter1 transmitter2)
  receiver    (receiver1 receiver2)
  hue         (blue red)  ;the color of a transmitter, receiver, or active connector
  beam        ()  ;the initial beams
  area        (area1 area2 area3 area4 area5 area6 area7 area8)
  cargo       (either connector)  ;what an agent can pickup & carry
  terminus    (either transmitter receiver connector)  ;what a connector can connect to
  source      (either transmitter connector)  ;beam source
  target      (either connector receiver)  ;beam target
  occluder    (either cargo agent gate wall)  ;objects that can occlude a beam
  fixture     (either transmitter receiver plate blower)
)


(define-dynamic-relations  ;relations with fluents can be bound in rules--eg (bind (holds agent1 $any-cargo))
  (holds agent $cargo)  ;fluent because we need to sometimes lookup what is currently being held
  (loc (either agent cargo) $area)  ;a location in an area
  (paired terminus terminus)  ;potential beam between two terminus
  (open gate)  ;a gate can be open or not open, default is not open
  (on (either agent connector) plate)
  (active (either receiver blower))
  (color connector $hue)  ;dynamic connector color
  (beam-segment beam $source $target $occluder $rational $rational)  ;endpoint-x endpoint-y
  (current-beams $list)
)


(define-static-relations
  (coords (either area fixture) $fixnum $fixnum $fixnum)
  (controls (either receiver plate) (either gate blower))
  (gate-plane gate $fixnum $fixnum $fixnum $fixnum $fixnum $fixnum $fixnum $fixnum $fixnum)  ;A, U, V for beam intersections
  ;(wall-segment wall $fixnum $fixnum $fixnum $fixnum)
  (chroma (either transmitter receiver) $hue)
  (agent-height $fixnum)
  (connector-height $fixnum)
  (box-height $fixnum)
  (los0 area fixture)  ;direct line of sight to a fixture
  (los1 area $gate fixture)  ;line of sight thru a gate to a fixture
  (visible0 area area)  ;visibility between areas (to a connector)
  (visible1 area $gate area)  ;visibility thru a gate
  (visible2 area $gate $gate area)
  (accessible0 area area)
  (accessible1 area gate area)
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
  ;; For real agent: respects gate and blower environmental states
  ;; For ghost: respects states it caused (recording phase),
  ;;            ignores states caused by real agent (playback phase)
  (or (accessible0 ?area1 ?area2)
      (exists (?g gate)
        (and (accessible1 ?area1 ?g ?area2)
             (or (ghost-agent ?agent)
                 (open ?g))))
      (exists (?b blower)
        (and (accessible1 ?area1 ?b ?area2)
             (or (not (active ?b))                                 ;; blower off: anyone can pass
                 (and (ghost-agent ?agent)                         ;; blower on + ghost:
                      (not (blower-activated-by-ghost ?b))))))))  ;;   only if ghost didn't cause it


(define-query placeable (?cargo ?area)
  ;; Area can hold multiple cargos; coordinate reference used for calculations;
  ;; can be extended if future domains require area capacity constraints.
  (and (cargo ?cargo) (area ?area)))


(define-query same-type (?agent ?cargo)
  ; Agent type and cargo type are the same (real or ghost).
  (if (real-agent ?agent)
    (real-cargo ?cargo)
    (ghost-cargo ?cargo)))


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


(define-query beam-reaches-receiver (?receiver)
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


(define-query relay-is-powered-by (?relay ?potential-source)
  ;; Returns t if ?relay is currently receiving power from ?potential-source
  ;; This prevents creating reciprocal beams back to the power source
  (and (bind (color ?relay $conn-hue))  ; Relay must be active
       ;; Check if a beam exists from ?potential-source to ?relay
       (exists (?b (get-current-beams))
         (and (bind (beam-segment ?b $src $tgt $end-x $end-y))
              (eql $src ?potential-source)
              (eql $tgt ?relay)
              ;; Verify beam actually reaches relay's coordinates
              (mvsetq ($conn-x $conn-y) (get-coordinates ?relay))
              (= $end-x $conn-x)
              (= $end-y $conn-y)
              ;; Verify source has matching hue (confirming power flow)
              (or (bind (chroma $src $src-hue))     ; Transmitter
                  (bind (color $src $src-hue)))     ; Active relay
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
                (if (relay $tgt)
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


(define-query find-blower-on-path (?area1 ?area2)
  ;; Returns the blower controlling path from ?area1 to ?area2, or nil if none.
  (ww-loop for ?b in (gethash 'blower *types*)
           do (if (accessible1 ?area1 ?b ?area2)
                (return ?b))))


(define-query playback-blower-active (?blower)
  ;; Returns t if blower will be active when playback begins.
  ;; This occurs when any controlling plate has been ghost-toggled to active.
  (exists (?p plate)
    (and (controls ?p ?blower)
         (ghost-toggled-active ?p))))


(define-query blower-activated-by-ghost (?blower)
  ;; Returns t if blower is currently active due to ghost plate toggle.
  ;; Used to enforce recording-phase physics on ghost agent.
  (and (active ?blower)
       (exists (?p plate)
         (and (controls ?p ?blower)
              (ghost-toggled-active ?p)))))


(define-query recording-playback-valid? ()
  ;; Validates that all real agent moves through blower-controlled paths
  ;; were legal given the recording-final blower states.
  ;; Returns t if valid, nil if any move was illegal.
  (do (bind (moves-pending-validation $moves))
      (not (ww-loop for $move in $moves
                    thereis (playback-blower-active (third $move))))))


;;;; UPDATE FUNCTIONS ;;;;


(define-update propagate-changes! ()
  (ww-loop for $iteration from 1 to 10
           do (if (not (propagate-consequences!))
                (return))  ;convergence achieved
           finally (inconsistent-state)))  ;no convergence, mark state inconsistent for pruning


(define-update propagate-consequences! ()
  ;; All functions must execute in order; returns t if any change occurred
  (some #'identity
        (mapcar (lambda (fn) (funcall fn state))
                (list #'update-plate-controlled-devices!
                      #'blow-objects-if-active!
                      #'create-missing-beams!
                      #'remove-orphaned-beams!
                      #'recalculate-all-beams!          ;always returns nil
                      #'update-beams-if-interference!   ;always returns nil
                      #'deactivate-receivers-that-lost-power!
                      #'deactivate-unpowered-relays!
                      #'activate-receivers-that-gained-power!
                      #'activate-reachable-relays!))))


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


(define-update blow-objects-if-active! ()
  ;; Blows objects according to blows> relation when blower is active.
  ;; Real agents/cargo: always subject to active blowers.
  ;; Ghost agents/cargo: only subject if ghost activated the blower
  ;;   (recording-phase physics); immune if real agent activated it
  ;;   (playback-phase, ghost follows pre-recorded trajectory).
  ;; Returns t if any object was moved, nil otherwise.
  (do
    (doall (?b blower)
      (if (and (active ?b)
               (bind (blows> ?b $from $to)))
        (do ;; Real objects always affected
            (doall (?a real-agent)
              (if (loc ?a $from)
                (do (loc ?a $to)
                    (setq $moved-any t))))
            (doall (?c real-cargo)
              (if (loc ?c $from)
                (do (loc ?c $to)
                    (setq $moved-any t))))
            ;; Ghost objects only affected if ghost activated blower
            (if (blower-activated-by-ghost ?b)
              (do (doall (?a ghost-agent)
                    (if (loc ?a $from)
                      (do (loc ?a $to)
                          (setq $moved-any t))))
                  (doall (?c ghost-cargo)
                    (if (loc ?c $from)
                      (do (loc ?c $to)
                          (setq $moved-any t)))))))))
    $moved-any))


(define-update create-missing-beams! ()
  ;; Creates beams for active sources paired with targets where no beam exists yet
  ;; Returns t if any beams were created, nil otherwise
  (do
    (doall (?src terminus)
      (doall (?tgt terminus)
        (if (and (different ?src ?tgt)
                 (or (paired ?src ?tgt) (paired ?tgt ?src)))  ; Pairing exists (bidirectional)
          ;; Have a pairing - check if source can emit and beam should exist
          (do (setq $source-hue (get-hue-if-source ?src))
              (if (and $source-hue                              ; Source is active
                       (target ?tgt)
                       (not (beam-exists-p ?src ?tgt))          ; Beam doesn't exist yet
                       (not (relay-is-powered-by ?src ?tgt)))  ; ?tgt is not powering ?src
                ;; Create beam: source is transmitter (always active) or powered connector
                (do (create-beam-segment-p! ?src ?tgt)
                    (setq $created-any t)))))))
    $created-any))


(define-update remove-orphaned-beams! ()
  ;; Removes beams whose pairing no longer exists or whose source lost power
  ;; Returns t if any beams were removed, nil otherwise
  (do
    (doall (?b (get-current-beams))
      (do (bind (beam-segment ?b $src $tgt $end-x $end-y))
          ;; Determine if beam should be removed
          (setq $should-remove nil)
          ;; Reason 1: Pairing no longer exists (check bidirectional)
          (if (not (or (paired $src $tgt) (paired $tgt $src)))
            (setq $should-remove t))
          ;; Reason 2: Source is a relay that lost power (no color binding)
          (if (and (relay $src)
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
               (not (beam-reaches-receiver ?r)))
        (do (deactivate-receiver! ?r)
            (setq $any-deactivated t))))
    $any-deactivated))


(define-update deactivate-unpowered-relays! ()
  ;; Returns t if any relay was deactivated, nil otherwise
  ;; Uses forward reachability from transmitters to detect true power
  (do
    ;; Get set of relays with valid transmitter power
    (setq $powered-relays (collect-transmitter-powered-relays))
    (doall (?r relay)
      (if (bind (color ?r $c-hue))
        ;; Relay is currently active - verify it has transmitter power
        (if (not (member ?r $powered-relays))
          ;; No path to transmitter - deactivate
          (do
            (not (color ?r $c-hue))
            (setq $deactivated-any t)))))
    $deactivated-any))


(define-update activate-receivers-that-gained-power! ()
  ;; Returns t if any receiver was activated, nil otherwise
  (do
    (doall (?r receiver)
      (if (and (not (active ?r))
               (beam-reaches-receiver ?r))
        (do (active ?r)
            (doall (?g gate)
              (if (controls ?r ?g)
                (open ?g)))
            (setq $any-activated t))))
    $any-activated))


(define-update activate-reachable-relays! ()
  ;; Returns t if any connector was activated, nil otherwise
  ;; Now uses consensus logic: connector only activates if ALL reaching beams have same hue
  (do
    (doall (?r relay)
      (if (not (bind (color ?r $existing-hue)))
        (do
          ;; Collect all hues from beams that reach this connector
          (setq $reaching-hues nil)
          ;; Check all paired termini that could be sources
          (doall (?src terminus)
            (if (or (paired ?r ?src) (paired ?src ?r))  ; Pairing exists (bidirectional)
              (do
                ;; Get source hue if it's an active source (transmitter or powered connector)
                (setq $src-hue (get-hue-if-source ?src))
                ;; If source has hue AND beam reaches connector, collect hue
                (if (and $src-hue
                         (beam-reaches-target ?src ?r))
                  (push $src-hue $reaching-hues)))))
          ;; Check for consensus among all reaching hues
          (setq $consensus-hue (resolve-consensus-hue $reaching-hues))
          ;; Only activate if consensus achieved (all hues identical)
          (if $consensus-hue
            (do (color ?r $consensus-hue)
                (setq $activated-any t))))))
    $activated-any))


(define-update deactivate-receiver! (?receiver)
  (do (not (active ?receiver))
      (doall (?g gate)
        (if (controls ?receiver ?g)
          (not (open ?g))))))


(define-update disconnect-connector! (?cargo)
  ;; Clears all pairings and deactivates connector when picked up
  (do (doall (?t terminus)
        (if (paired ?cargo ?t)
          (not (paired ?cargo ?t))))
      (if (bind (color ?cargo $hue))
        (not (color ?cargo $hue)))))


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


(define-update record-move-for-playback-validation! (?agent ?area1 ?area2)
  ;; Records blower-path moves by real agents for playback validation.
  ;; Only records if agent is real and path is blower-controlled.
  (if (real-agent ?agent)
    (do (setq $blower (find-blower-on-path ?area1 ?area2))
        (if $blower
          (do (bind (moves-pending-validation $moves))
              (moves-pending-validation (cons (list ?area1 ?area2 $blower) $moves)))))))


(define-update log-if-ghost-toggle! (?agent ?plate)
  ;; Track ghost toggle parity for playback validation.
  ;; Only affects state when agent is a ghost-agent.
  (if (ghost-agent ?agent)
    (if (ghost-toggled-active ?plate)
      (not (ghost-toggled-active ?plate))
      (ghost-toggled-active ?plate))))


;;;; ACTIONS ;;;;


(define-action connect-to-1-terminus
    1
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
          (do (bind (beam-segment $new-beam1 $src $tgt $occluder $end-x1 $end-y1))
              (mvsetq ($cargo-x $cargo-y) (get-coordinates $cargo))
              (if (and (= $end-x1 $cargo-x) (= $end-y1 $cargo-y))
                (activate-connector! $cargo $hue1))))
        (converge-receiver-states!)))  ;handle all cascading activations/deactivations


(define-action connect-to-2-terminus
    1
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
                             (bind (beam-segment $new-beam1 $src1 $tgt1 $occluder1 $end-x1 $end-y1))
                             (= $end-x1 $cargo-x)
                             (= $end-y1 $cargo-y))
                        (and $new-beam2
                             (bind (beam-segment $new-beam2 $src2 $tgt2 $occluder2 $end-x2 $end-y2))
                             (= $end-x2 $cargo-x)
                             (= $end-y2 $cargo-y)))
                  (chain-activate! $cargo $hue))))
          (converge-receiver-states!)))


(define-action connect-to-3-terminus
    1
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
                             (bind (beam-segment $new-beam1 $src1 $tgt1 $occluder1 $end-x1 $end-y1))
                             (= $end-x1 $cargo-x)
                             (= $end-y1 $cargo-y))
                        (and $new-beam2
                             (bind (beam-segment $new-beam2 $src2 $tgt2 $occluder2 $end-x2 $end-y2))
                             (= $end-x2 $cargo-x)
                             (= $end-y2 $cargo-y))
                        (and $new-beam3
                             (bind (beam-segment $new-beam3 $src3 $tgt3 $occluder3 $end-x3 $end-y3))
                             (= $end-x3 $cargo-x)
                             (= $end-y3 $cargo-y)))
                  (chain-activate! $cargo $hue))))
          (converge-receiver-states!)))


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
          (not (loc ?connector $area))
          ;; Step 2: Process beams where connector is occluder (not participant)
          ;; These beams terminate at connector coordinates but connector is neither source nor target
          (doall (?b (get-current-beams))
            (do (bind (beam-segment ?b $source $target $occluder $end-x $end-y))
                (if (and (= $end-x $conn-x) (= $end-y $conn-y)
                         (different $source ?connector)
                         (different $target ?connector))
                  ;; Connector was occluding this beam - recalculate beam endpoint
                  (do (mvsetq ($source-x $source-y) (get-coordinates $source))
                      (mvsetq ($target-x $target-y) (get-coordinates $target))
                      (mvsetq ($new-end-x $new-end-y $new-occluder)
                              (find-first-obstacle-intersection $source-x $source-y $target-x $target-y))
                      ;; Update beam segment with recalculated endpoint
                      (beam-segment ?b $source $target $new-occluder $new-end-x $new-end-y)))))
          ;; Step 3: Remove beam segments where connector is participant (source or target)
          (doall (?b (get-current-beams))
            (do (bind (beam-segment ?b $source $target $occluder $end-x $end-y))
                (if (or (eql $source ?connector) (eql $target ?connector))
                  (remove-beam-segment-p! ?b))))
          (update-beams-if-interference!)  ;before chain-deactivate!
          ;; Step 4: Deactivate connector
          (if (bind (color ?connector $hue))
            (chain-deactivate! ?connector $hue))
          ;; Step 5: Remove pairings (must be AFTER chain-deactivate!)
          (doall (?t terminus)
            (if (paired ?connector ?t)
              (not (paired ?connector ?t))))
          ;; Step 6: Converge receiver states after all modifications
          (converge-receiver-states!)))


(define-action drop
    1
  ()
  (and (bind (holds agent1 $cargo))  ;if not holding anything, then bind statement returns nil
       (bind (loc agent1 $area))  ;agent1 is always located somewhere
       (vacant $area))
  ($cargo $area)
  (assert (not (holds agent1 $cargo))
          (loc $cargo $area)
          (update-beams-if-occluded! $area)
          (converge-receiver-states!)))  ;Handle receiver deactivations after occlusion


(define-action move
    1
  (?area2 area)
  (and (bind (loc agent1 $area1))
       (different $area1 ?area2)
       (accessible $area1 ?area2))
  ($area1 ?area2)
  (assert (loc agent1 ?area2)
          (recalculate-all-beams!)  ;handle both extension and shortening
          (converge-receiver-states!)))


;;;; INITIALIZATION ;;;;


(define-init
  ;; Dynamic state (agent-manipulable or derived)
  (loc agent1 area1)
  (loc connector1 area1)
  (loc connector2 area3)
  (loc connector3 area4)
  (loc connector4 area5)
  (loc box1 area5)
  (on connector4 box1)
  (current-beams ())  ; Empty - populated by init-action
  
  ;; Static spatial configuration
  (coords area1 13 3 5)
  (coords area2 7 2 5)
  (coords area3 4 2 5)
  (coords area4 5 9 4)
  (coords area5 14 19 0)
  (coords area6 2 16 6)
  (coords area7 14 19 6)
  (coords area8 14 21 6)
  (accessible1 area1 gate1 area2)  ;chain accessibility
  (accessible1 area2 gate2 area3)
  (accessible1 area1 gate3 area9)
  (accessible0 area9 area4)
  (accessible1 area9 barrier1 area5)
  (accessible1 area5 blower1 area7)
  (accessible0 area7 area8)
  (accessible1 area7 hatch1 area6)
  
  ;; Static object configuration
  (coords transmitter1 21 11 2)
  (coords transmitter2 23 20 6)
  (coords receiver1 10 4 6)
  (coords receiver2 5 18 5)  ;note that plates and blowers have the coords of their area
  (gate-plane gate1 )  ;A,U,V coords
  (gate-plane gate2 )
  (gate-plane gate3 )
  (gate-plane gate4 )
  (agent-height 2)
  (connector-height 1)
  (box-height 1)
  ;(wall-segment wall1 33 13 33 13)
  
  ;; Static color assignments
  (chroma transmitter1 red)
  (chroma transmitter2 blue)
  (chroma receiver1 blue)
  (chroma receiver2 red)
  
  ;; Control relationships
  (controls plate1 gate3)
  (controls plate2 gate2)
  (controls plate3 blower1)
  (controls plate4 gate4)
  (controls receiver1 gate1)
  (controls receiver2 hatch1)
  
  ;; Line-of-sight relationships
  (los1 area1 gate3 transmitter1)
  (los0 area1 receiver1)
  (los0 area4 transmitter1)
  (los0 area9 transmitter1)
  (los0 area9 transmitter2)
  (los0 area9 receiver2)
  (los0 area7 transmitter2)
  (los0 area8 transmitter2)

  ;; Visibility relationships
  (visible1 area1 gate1 area2)
  (visible1 area2 gate2 area3)
  (visible2 area1 gate1 gate2 area3)
  (visible1 area1 gate3 area9)
  (visible1 area1 gate3 area4)
  (visible1 area1 gate3 area5)
  (visible1 area1 gate3 area7)
  (visible1 area1 gate3 area8)
  (visible0 area9 area4)
  (visible0 area9 area5)
  (visible0 area9 area7)
  (visible0 area9 area8)
)


;;;; GOAL ;;;;


(define-goal
  (loc agent1 area3)
)

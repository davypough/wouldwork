;;; Filename: problem-corner.lisp

;;; Talos Principle problem 'Around the Corner' in Purgatory workshop.
;;; Represents coords as 3D, but uses only 2D for beam calculations.


(in-package :ww)


(ww-set *problem-name* corner)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 11)

;(ww-set *progress-reporting-interval* 3000000)


(define-types
  real-agent      (agent1)  ;the name of the main agent performing actions
  ghost-agent     ()  ;ghost objects are starred--eg, agent1*
  agent           (either real-agent ghost-agent)
  gate            (gate1)
  wall            ()
  real-connector  (connector1 connector2 connector3)
  ghost-connector ()
  connector       (either real-connector ghost-connector)
  repeater        ()
  recorder        ()
  plate           ()
  buzzer          ()
  box             ()
  mine            ()
  blower          ()
  transmitter     (transmitter1 transmitter2)
  receiver        (receiver1 receiver2 receiver3)
  beam            ()  ;initial beams
  hue             (blue red)  ;the hue of a transmitter, receiver, repeater, or active connector
  area            (area1 area2 area3 area4 area5)  ;position points
  real-cargo      (either real-connector real-box)
  ghost-cargo     (either ghost-connector ghost-box)
  cargo           (either connector box)  ;what the player can pickup & carry
  support         (either box buzzer mine)  ;objects that can support certain other objects
  relay           (either connector repeater)
  terminus        (either transmitter receiver connector repeater)  ;what a connector can connect to
  source          (either transmitter connector repeater)  ;beam source
  occluder        (either cargo agent gate wall)  ;objects that can occlude a beam
  target          (either connector receiver repeater)  ;beam target
  focus           (either transmitter receiver repeater)  ;los object of interest
  fixture         (either transmitter receiver recorder repeater plate blower)
)


(define-dynamic-relations  ;relations with fluents can be bound in rules--eg (bind (holds agent1 $any-cargo))
  (holds agent $cargo)  ;fluent because we may need to lookup what is currently being held
  (loc (either agent cargo buzzer mine) $area)  ;a location in an area
  (open gate)  ;a gate can be open or not open, default is not open
  (active (either plate blower receiver))
  (on (either $agent $cargo) $box :bijective)
  (supports (either $buzzer $mine) $box :bijective)
  (elevation (either agent cargo) $fixnum)
  (paired terminus terminus)  ;potential beam between two terminus
  (color relay $hue)  ;having a color means it is active
  (beam-segment beam $source $target $rational $rational)  ;endpoint-x endpoint-y
  (current-beams $list)
  (ghost-toggled-active plate)  ;used to track ghost plate activations during playback to catch illegal moves
  ;(moves-pending-validation $list)
)


(define-static-relations
  (coords (either area fixture) $fixnum $fixnum $fixnum)  ;the (x,y,z) position
  (controls (either receiver plate) (either gate blower))
  ;(blows> blower $area $area)
  (chroma (either transmitter receiver) $hue)  ;fixed color
  (gate-segment gate $fixnum $fixnum $fixnum $fixnum)
  ;(wall-segment wall $fixnum $fixnum $fixnum $fixnum)
  ;(toggles plate (either gate blower))
  ;potential clear los from an area to a focus
  (los0 area focus)  
  (los1 area (either $gate $area) focus)  ;los can be blocked either by a closed $gate or object in $area
  ;(los2 area (either $gate $area) (either $gate $area) focus)
  ;potential visibility from an area to another area
  (visible0 area area)  
  (visible1 area (either $gate $area) area)  ;visibility from one area through a potential occluder to another area
  ;(visible2 area (either $gate $area) (either $gate $area) area)
  ;potential accesibility to move from an area to another area
  (accessible0 area area)
  (accessible1 area (either $gate $blower) area)
  ;(accessible2 area (either $gate $blower) (either $gate $blower) area)
)


#+ignore (define-patroller buzzer1
  path (area1 area2 area3 area4 area5)
  mode :reverse
  rebound (exists (?c cargo)  ;buzzer just pushes agent aside in an area and continues on (no rebound or kill)
            (and (bind (loc buzzer1 $area))
                 (loc ?c $area)))
  aftereffect (if (and (bind (supports buzzer1 $box))
                       (bind (loc buzzer1 $buzzer-area)))
                (assert (relocate-stacked-object! $box $buzzer-area)))
)


;;;; QUERY FUNCTIONS ;;;;


#+ignore (define-query heuristic? ()
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


(define-query cleartop (?support)
  ;; True if nothing is on top of a box, buzzer, or mine.
  (cond ((box ?support) (not (bind (on $anything ?support))))
        ((buzzer ?support) (not (bind (supports ?support $any-box))))
        ((mine ?support) (not (bind (supports ?support $any-box))))))


(define-query safe (?area)
  (not (exists (?mine mine)
         (loc ?mine ?area))))


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
  ;; Efficiently get x,y coordinates for fixed objects
  (do (bind (coords ?area/fixture $x $y $z))
      (values $x $y)))


(define-query get-coordinates (?object)
  ;; Finds x,y coordinates for any arbitrary object
  (cond 
    ;; Direct coords for areas and fixtures
    ((bind (coords ?object $x $y $z))
     (values $x $y))
    ;; Indirect via loc for movable objects (agents, cargo, buzzer, mine)
    ((and (bind (loc ?object $area))
          (bind (coords $area $x $y $z)))
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
  (do (mvsetq ($target-x $target-y $target-z) (get-coordinates ?target))
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
  ;; Area can hold multiple cargos, but only one connector;
  ;; can be extended if future domains require area capacity constraints.
  (if (connector ?cargo)
    (not (exists (?c connector)
           (loc ?c ?area)))
    t))


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
                   ;(and (wall ?obj)
                   ;     (bind (wall-segment ?obj $x1 $y1 $x2 $y2)))
                   (and (or (cargo ?obj) (agent ?obj))
                        (bind (loc ?obj $area))
                        (bind (coords $area $x1 $y1 $z1))
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
              (do (mvsetq ($x $y $z) (get-coordinates $src))
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
    (mvsetq ($c-x $c-y $c-z) (get-coordinates ?connector))
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
              (mvsetq ($conn-x $conn-y $conn-z) (get-coordinates ?relay))
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
                  (do (mvsetq ($tgt-x $tgt-y $tgt-z) (get-coordinates $tgt))
                      (if (and (= $end-x $tgt-x)
                               (= $end-y $tgt-y)
                               (not (member $tgt $powered-relays)))
                        ;; Target is newly discovered powered relay
                        (do (push $tgt $powered-relays)
                            (push $tgt $next-frontier)))))))))
      ;; Advance to next frontier
      (setq $frontier $next-frontier))
    $powered-relays))


#+ignore (define-query find-blower-on-path (?area1 ?area2)
  ;; Returns the blower controlling path from ?area1 to ?area2, or nil if none.
  (ww-loop for ?b in (gethash 'blower *types*)
           do (if (accessible1 ?area1 ?b ?area2)
                (return ?b))))


#+ignore (define-query playback-blower-active (?blower)
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


#+ignore (define-query recording-playback-valid? ()
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
                (list ;#'update-plate-controlled-devices!
                      ;#'blow-objects-if-active!
                      #'create-missing-beams!
                      #'remove-orphaned-beams!
                      #'recalculate-all-beams!          ;always returns nil
                      #'update-beams-if-interference!   ;always returns nil
                      #'deactivate-receivers-that-lost-power!
                      #'deactivate-unpowered-relays!
                      #'activate-receivers-that-gained-power!
                      #'activate-reachable-relays!))))


#+ignore (define-update update-plate-controlled-devices! ()
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


#+ignore (define-update blow-objects-if-active! ()
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
            (mvsetq ($source-x $source-y $source-z) (get-coordinates $source))
            ;; Get target coordinates  
            (mvsetq ($target-x $target-y $target-z) (get-coordinates $target))
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
    ;; Collect all intersections ONCE before processing any beams
    (setq $all-intersections (collect-all-beam-intersections))
    ;; Phase 1: For each beam, determine its closest intersection point
    (doall (?b (get-current-beams))
      (do (bind (beam-segment ?b $src $tgt $old-end-x $old-end-y))
          (mvsetq ($src-x $src-y $src-z) (get-coordinates $src))
          (mvsetq ($tgt-x $tgt-y $tgt-z) (get-coordinates $tgt))
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
          (ww-loop for $intersection in $all-intersections do
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
    (mvsetq ($source-x $source-y $source-z) (get-coordinates ?source))
    (mvsetq ($target-x $target-y $target-z) (get-coordinates ?target))
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


#+ignore (define-update record-move-for-playback-validation! (?agent ?area1 ?area2)
  ;; Records blower-path moves by real agents for playback validation.
  ;; Only records if agent is real and path is blower-controlled.
  (if (real-agent ?agent)
    (do (setq $blower (find-blower-on-path ?area1 ?area2))
        (if $blower
          (do (bind (moves-pending-validation $moves))
              (moves-pending-validation (cons (list ?area1 ?area2 $blower) $moves)))))))


#+ignore (define-update log-if-ghost-toggle! (?agent ?plate)
  ;; Track ghost toggle parity for playback validation.
  ;; Only affects state when agent is a ghost-agent.
  (if (ghost-agent ?agent)
    (if (ghost-toggled-active ?plate)
      (not (ghost-toggled-active ?plate))
      (ghost-toggled-active ?plate))))


(define-update relocate-stacked-object! (?object ?area)
  (do (loc ?object ?area)
      (if (bind (on $higher-object ?object))
        (relocate-stacked-object! $higher-object ?area))))
  

(define-update collapse-cargo-above-box! (?box)
  ;; Collapses all the cargo items above a box by one level
  (if (bind (on $cargo ?box))
    (do (bind (elevation $cargo $h-cargo))
        (elevation $cargo (1- $h-cargo))
        (if (box $cargo)
          (collapse-cargo-above-box! $cargo)))))


;;;; ACTIONS ;;;;


(define-action put-cargo-on-place
  ;; Agent can place inactive cargo on a box, buzzer/mine (boxes only), or the ground.
  1
  (?agent agent)
  (and (bind (holds ?agent $cargo))
       (bind (loc ?agent $area))
       (bind (elevation ?agent $h-agent)))
  (?agent $cargo $place $area)
  (do ;; Box-only: can put box on a buzzer or mine
      (if (box $cargo)
        (doall (?rover (either buzzer mine))
          (if (and (loc ?rover $area)
                   (cleartop ?rover)
                   (>= $h-agent 1))  ;must be above buzzer/mine
            (assert (not (holds ?agent $cargo))
                    (loc $cargo $area)
                    (supports ?rover $cargo)
                    (elevation $cargo 1)
                    (setq $place ?rover)))))
      ;; All cargo: can put on a box
      (doall (?box box)
        (if (and (loc ?box $area)
                 (cleartop ?box)
                 (bind (elevation ?box $h-box))
                 (setq $h-delta (- $h-box $h-agent))
                 (< $h-delta 1))  ;within reach +1 up or any level down
          (assert (not (holds ?agent $cargo))
                  (loc $cargo $area)
                  (on $cargo ?box)
                  (elevation $cargo (1+ $h-box))
                  (setq $place ?box))))
      ;; All cargo: can put on ground
      (assert (not (holds ?agent $cargo))
              (loc $cargo $area)
              (elevation $cargo 0)
              (setq $place 'ground))
      (finally (propagate-changes!))))


(define-action connect-to-1-terminus
    1
  (?agent agent ?terminus terminus)
  (and (bind (holds ?agent $cargo))
       (same-type ?agent $cargo)
       (different $cargo ?terminus)
       (bind (loc ?agent $area))
       (bind (elevation ?agent $h-agent))
       (placeable $cargo $area)
       (selectable ?agent $area ?terminus))
  (?agent $cargo ?terminus $area $place)
  (do ;; Can place on a box
      (doall (?box box)
        (if (and (loc ?box $area)
                 (cleartop ?box)
                 (bind (elevation ?box $h-box))
                 (< (- $h-box $h-agent) 1))               ; within reach
          (assert (not (holds ?agent $cargo))
                  (loc $cargo $area)
                  (on $cargo ?box)
                  (elevation $cargo (1+ $h-box))
                  (paired $cargo ?terminus)
                  (setq $place ?box))))
      ;; Can place on ground
      (assert (not (holds ?agent $cargo))
              (loc $cargo $area)
              (elevation $cargo 0)
              (paired $cargo ?terminus)
              (setq $place 'ground))
      (finally (propagate-changes!))))


(define-action connect-to-2-terminus
    1
  (?agent agent (combination (?t1 ?t2) terminus))
  (and (bind (holds ?agent $cargo))
       (same-type ?agent $cargo)
       (different $cargo ?t1)
       (different $cargo ?t2)
       (bind (loc ?agent $area))
       (bind (elevation ?agent $h-agent))
       (placeable $cargo $area)
       (selectable ?agent $area ?t1)
       (selectable ?agent $area ?t2))
  (?agent $cargo ?t1 ?t2 $area $place)                    ; ADDED $place
  (do ;; Can place on a box
      (doall (?box box)
        (if (and (loc ?box $area)
                 (cleartop ?box)
                 (bind (elevation ?box $h-box))
                 (< (- $h-box $h-agent) 1))               ; within reach
          (assert (not (holds ?agent $cargo))
                  (loc $cargo $area)
                  (on $cargo ?box)
                  (elevation $cargo (1+ $h-box))
                  (paired $cargo ?t1)
                  (paired $cargo ?t2)
                  (setq $place ?box))))
      ;; Can place on ground
      (assert (not (holds ?agent $cargo))
              (loc $cargo $area)
              (elevation $cargo 0)
              (paired $cargo ?t1)
              (paired $cargo ?t2)
              (setq $place 'ground))
      (finally (propagate-changes!))))


(define-action connect-to-3-terminus
    1
  (?agent agent (combination (?t1 ?t2 ?t3) terminus))
  (and (bind (holds ?agent $cargo))
       (same-type ?agent $cargo)
       (different $cargo ?t1)
       (different $cargo ?t2)
       (different $cargo ?t3)
       (bind (loc ?agent $area))
       (bind (elevation ?agent $h-agent))
       (placeable $cargo $area)
       (selectable ?agent $area ?t1)
       (selectable ?agent $area ?t2)
       (selectable ?agent $area ?t3))
  (?agent $cargo ?t1 ?t2 ?t3 $area $place)
  (do ;; Can place on a box
      (doall (?box box)
        (if (and (loc ?box $area)
                 (cleartop ?box)
                 (bind (elevation ?box $h-box))
                 (< (- $h-box $h-agent) 1))               ; within reach
          (assert (not (holds ?agent $cargo))
                  (loc $cargo $area)
                  (on $cargo ?box)
                  (elevation $cargo (1+ $h-box))
                  (paired $cargo ?t1)
                  (paired $cargo ?t2)
                  (paired $cargo ?t3)
                  (setq $place ?box))))
      ;; Can place on ground
      (assert (not (holds ?agent $cargo))
              (loc $cargo $area)
              (elevation $cargo 0)
              (paired $cargo ?t1)
              (paired $cargo ?t2)
              (paired $cargo ?t3)
              (setq $place 'ground))
      (finally (propagate-changes!))))


(define-action pickup-connector
    1
  (?agent agent ?connector connector)
  (and (same-type ?agent ?connector)
       (not (bind (holds ?agent $held)))
       (bind (loc ?agent $area))
       (bind (elevation ?agent $h-agent))
       (loc ?connector $area)
       (bind (elevation ?connector $h-conn))
       (<= (abs (- $h-conn $h-agent)) 1))                 ; within reach
  (?agent ?connector $area)
  (assert (holds ?agent ?connector)
          (not (loc ?connector $area))
          (not (elevation ?connector $h-conn))
          (if (bind (on ?connector $box))
            (not (on ?connector $box)))
          (disconnect-connector! ?connector)
          (propagate-changes!)))


(define-action pickup-box
    1
  (?agent agent)
  (and (not (bind (holds ?agent $any-cargo)))
       (bind (loc ?agent $area))
       (bind (elevation ?agent $h-agent)))
  (?agent $box $area)
  (do (doall (?box box)
        (if (and (not (on ?agent ?box))                   ; can't pick up box you're standing on
                 (loc ?box $area)
                 (bind (elevation ?box $h-box))
                 (<= (abs (- $h-box $h-agent)) 1))        ; within reach (+1,0,-1)
          (assert (holds ?agent ?box)
                  (not (loc ?box $area))
                  (not (elevation ?box $h-box))
                  ;; First update elevations while on relationships still exist
                  (collapse-cargo-above-box! ?box)
                  ;; Now handle removal of relationships and establishing new ones
                  (cond ((bind (supports $rover ?box))
                           ;; Box was on buzzer or mine - remove supports, transfer cargo to rover
                           (not (supports $rover ?box))
                           (if (bind (on $cargo ?box))
                             (do (not (on $cargo ?box))
                                 (if (box $cargo)
                                   (supports $rover $cargo)
                                   (elevation $cargo 0)))))  ; non-box cargo falls to ground
                        ((bind (on ?box $under-box))
                           ;; Box was on another box - remove on, transfer cargo to under-box
                           (not (on ?box $under-box))
                           (if (bind (on $cargo ?box))
                             (do (not (on $cargo ?box))
                                 (on $cargo $under-box))))
                        (t
                           ;; Box was on ground - just remove on relationship for cargo
                           (if (bind (on $cargo ?box))
                             (not (on $cargo ?box))))))))
      (finally (propagate-changes!))))


(define-action jump-to-place
  ;; Agent can jump up or down to any reachable box or the ground.
  1
  (?agent agent)
  (and (bind (loc ?agent $area))
       (bind (elevation ?agent $h-agent)))
  (?agent $place $area)
  (do ;; Can jump to a reachable box
      (doall (?box box)
        (if (and (cleartop ?box)
                 (loc ?box $area)                         ; agent and box must be in same area
                 (bind (elevation ?box $h-box))
                 (< (- $h-box $h-agent) 1))               ; can only reach box at same level or below
          (assert (if (bind (on ?agent $old-box))
                    (not (on ?agent $old-box)))
                  (on ?agent ?box)
                  (elevation ?agent (1+ $h-box))
                  (setq $place ?box))))
      ;; Can jump to the ground
      (if (> $h-agent 0)
        (assert (elevation ?agent 0)
                (if (bind (on ?agent $box))
                  (not (on ?agent $box)))
                (setq $place 'ground)))
      (finally (propagate-changes!))))


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


#+ignore (define-action toggle-plate
  1
  (?agent agent ?plate plate)
  (and (bind (loc ?agent $area))
       (bind (coords $area $agent-x $agent-y $agent-z))
       (bind (coords ?plate $plate-x $plate-y $plate-z))
       (= $agent-x $plate-x)
       (= $agent-y $plate-y))
  (?agent ?plate)
  (assert (if (active ?plate)
            (not (active ?plate))
            (active ?plate))
          (log-if-ghost-toggle! ?agent ?plate)  ;tracks ghost toggling for playback validation
          (propagate-changes!)))


(define-action move
    1
  (?agent agent ?area2 area)
  (and (bind (loc ?agent $area1))
       (different $area1 ?area2)
       (bind (elevation ?agent $h-agent))
       (= $h-agent 0)                                     ; must be on ground
       (accessible ?agent $area1 ?area2)
       (safe ?area2))
  (?agent $area1 ?area2)
  (assert (loc ?agent ?area2)
          (propagate-changes!)))


;;;; INITIALIZATION ;;;;


(define-init
  ;; Dynamic state (agent-manipulable or derived)
  (loc agent1 area1)
  (loc connector1 area1)
  (loc connector2 area2)
  (loc connector3 area3)
  (elevation agent1 0)
  (elevation connector1 0)
  (elevation connector2 0)
  (elevation connector3 0)
  (current-beams ())  ; Empty - can be populated by init-action, if exist initially
  ;moves-pending-validation ())  ;empty list of recorded blower-path moves

  ;; Static spatial configuration
  (coords area1 13 3 0)
  (coords area2 13 1 0)
  (coords area3 10 8 0)
  (coords area4 5 5 0)
  (coords area5 10 10 0)
  ;(coords recorder1 4 20 0)
  (coords transmitter1 11 0 0)
  (coords transmitter2 10 0 0)
  (coords receiver1 8 1 0)
  (coords receiver2 7 11 0)
  (coords receiver3 1 11 0)
  (controls receiver1 gate1)
  ;(blows> blower1 area3 area1)
  (gate-segment gate1 8 3 8 7)
  ;(wall-segment wall1 8 7 8 8)
  ;(wall-segment wall2 8 0 8 3)
  (chroma transmitter1 red)
  (chroma transmitter2 blue)
  (chroma receiver1 red)
  (chroma receiver2 red)
  (chroma receiver3 blue)
  
  ;; Line-of-sight relationships (connector area to fixture)
  (los0 area1 transmitter1)
  (los0 area1 transmitter2)
  (los0 area1 receiver1)
  (los0 area1 receiver2)
  (los1 area1 gate1 receiver3)
  (los0 area2 transmitter1)
  (los0 area2 transmitter2)
  (los0 area2 receiver1)
  (los0 area2 receiver2)
  (los1 area2 gate1 receiver3)
  (los0 area3 transmitter1)
  (los0 area3 transmitter2)
  (los0 area3 receiver1)
  (los0 area3 receiver2)
  (los0 area3 receiver3)
  (los1 area4 gate1 transmitter1)
  (los1 area4 gate1 transmitter2)
  (los0 area4 receiver2)
  (los0 area4 receiver3)
  (los0 area5 transmitter1)
  (los0 area5 transmitter2)
  (los0 area5 receiver1)
  (los0 area5 receiver2)
  (los0 area5 receiver3)
  
  ;; Visibility relationships (connector area to area)
  (visible0 area1 area2)
  (visible0 area1 area3)
  (visible1 area1 gate1 area4)
  (visible0 area1 area5)
  (visible0 area2 area3)
  (visible1 area2 gate1 area4)
  (visible0 area2 area5)
  (visible1 area3 gate1 area4)
  (visible0 area3 area5)
  (visible0 area4 area5)
  

  ;; Accessibility (move area to area); chain moves between areas to lower search branching
  (accessible0 area1 area2)  
  (accessible0 area1 area3)
  (accessible1 area1 gate1 area4)
  (accessible0 area1 area5)
  (accessible0 area2 area3)
  (accessible1 area2 gate1 area4)
  (accessible0 area2 area5)
  (accessible1 area3 gate1 area4)
  (accessible0 area3 area5)
  (accessible1 area4 gate1 area5)
)


;;;; GOAL ;;;;


(define-goal
  (and (active receiver2)
       (active receiver3)
       (loc agent1 area4)
       ;(recording-playback-valid?)  ;if invalid state, return nil, continue searching
  ))     
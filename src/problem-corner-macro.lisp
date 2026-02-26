;;; Filename: problem-corner-macro.lisp

;;; Talos Principle problem 'Around the Corner' in Purgatory workshop 3.
;;; Uses macro actions (move folded into pickup/connect/deliver).


(in-package :ww)


(ww-set *problem-name* corner-macro)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 10)

(ww-set *progress-reporting-interval* 1000000)


(define-types
  real-agent      (agent1)  ;the name of the main agent performing actions
  ghost-agent     ()  ;ghost objects are starred--eg, agent1*
  agent           (either real-agent ghost-agent)
  gate            (gate1 gate2)
  wall            (wall1 wall2)  ;internal possible occluding walls
  window          (window1)
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
  area            (area1 area2 area3 area4)  ;position points
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
  ;(elevation (either agent cargo) $fixnum)
  (paired connector terminus)  ;potential beam between a connector and a terminus
  (color relay $hue)  ;having a color means it is active
  (beam-segment beam $source $target $rational $rational)  ;endpoint-x endpoint-y
  (current-beams $list)
)


(define-static-relations
  (coords (either area fixture) $rational $rational $rational)  ;the (x,y,z) position
  (controls (either receiver plate) (either gate blower))
  (chroma (either transmitter receiver) $hue)  ;fixed color
  (wall-segments $list)    ; ((wall1 x1 y1 x2 y2) ...)
  (gate-segments $list)    ; ((gate1 x1 y1 x2 y2) ...)
  (window-segments $list)
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


;;;;;;;;;;;; ENUMERATOR SPECS ;;;;;;;;;;;;;;;;;;;

#|
(define-base-relations  ;specify for enumerator only
  ;dyanamic relations which are not derived
  (loc paired)
)


(define-base-relation loc  ;specify for enumerator only
  :allow-unassigned (:types cargo)  ;allows cargo to have an unassigned location (ie, held)
  :symmetric-batch t)


(define-base-relation paired  ;specify for enumerator only
  :max-per-key 4
  :requires-fluent loc  ;prevents generating beam pairings for connectors without a location (held)
  :partner-feasible (:query observable :args ((:key-fluent loc) :partner)))

;;; Observable enum CSP Pruning: checks 1–3 are all captured by the existing observable query.
;;; 1. A connector paired with a fixture (transmitter or receiver) must have potential line-of-sight from its area to that fixture,
;;;    considering all possible gate states.
;;; 2. A connector paired with another connector must have potential inter-area visibility between their respective areas,
;;;    considering all possible gate states.
;;; 3. Two connectors in the same area cannot usefully pair with each other,
;;;    since beams require nonzero distance and only one connector per area can be active.


(find-goal-states goal-fn  ;example find best enumeration (fewest pairings)
  :sort-key (lambda (st)
              (count 'paired (list-database (problem-state.idb st))
                    :key #'car)))


(define-base-filter connectors-in-areas2&3   ;provides no extra pruning
  (exists ((?c1 ?c2) connector)
    (and (loc ?c1 area2) (loc ?c2 area3))))
|#

;;;; HEURISTIC FUNCTIONS ;;;;

#|
;;; The heuristic? function for this problem estimates the remaining "cost"
;;; to reach the goal from the current state. It combines multiple component
;;; heuristics using weighted summation, where higher weights indicate more
;;; important factors. Note that heuristics are not recommended for this problem,
;;; because intermediate good states must be undone to find a solution. No steady progress.
;;;
;;; Component Design Principles:
;;;   - Each component returns a non-negative integer
;;;   - Return 0 when the condition is satisfied (no penalty)
;;;   - Return positive values proportional to "distance" from satisfaction
;;;   - Weights encode relative importance and help break ties
;;;
;;; For this problem, the goal is:
;;;   (and (active receiver2) (active receiver3) (loc agent1 area4))
;;; Note that using heuristic guidance for this problem is counterproductive
;;;   since progress must often be undone


(define-query heuristic? ()
   ;Combines weighted heuristic components to estimate cost to goal.
   ;Components ordered by priority:
   ;  h-color-path-deficit (200): Missing ACTIVE transmitter→receiver paths
   ;  h-unpowered-placed-connectors (100): Placed but inactive connectors
   ;  h-color-mismatch (80): Powered connectors with wrong color for targets
   ;  h-goal-receivers-inactive (50): Goal receivers still needing activation  
   ;  h-gate1-blocks-goal (30): Gate1 must open for agent to reach area4
   ;  h-useless-pairings (20): Receiver-only pairings (can never activate)
   ;  h-agent-goal-distance (10): Agent must end in area4
   ;Returns: Non-negative integer estimate of remaining cost"
  (do (combine-heuristics            ;combine-heuristics function in ww-support.lisp
        state
        '((200  . h-color-path-deficit)
          (100  . h-unpowered-placed-connectors)
          (80   . h-color-mismatch)
          (50   . h-goal-receivers-inactive)
          (30   . h-gate1-blocks-goal)
          (20   . h-useless-pairings)
          (10   . h-agent-goal-distance))
        :combiner :weighted-sum)))


(define-query h-color-path-deficit ()
  ;; Counts receivers that need power but have no viable ACTIVE path from transmitter.
  ;; A viable path requires actual beam connectivity, not just structural potential.
  ;; Returns: 0-3 (deficit count)
  (do (setq $deficit 0)
      ;; receiver1 (red) - needed to open gate1
      (if (and (not (active receiver1))
               (not (active-path-to-receiver-p transmitter1 receiver1 red)))
        (incf $deficit))
      ;; receiver2 (red) - goal
      (if (and (not (active receiver2))
               (not (active-path-to-receiver-p transmitter1 receiver2 red)))
        (incf $deficit))
      ;; receiver3 (blue) - goal  
      (if (and (not (active receiver3))
               (not (active-path-to-receiver-p transmitter2 receiver3 blue)))
        (incf $deficit))
      $deficit))


(define-query active-path-to-receiver-p (?transmitter ?receiver ?hue)
  ;; Returns t only if an ACTUALLY POWERED connector provides a path to receiver
  ;; AND that path originates from the specified transmitter.
  ;; Requires connector to have correct color (be actually receiving power).
  ;; This is stricter than checking structural potential.
  (or
    ;; 1-hop: connector MUST be powered with correct color, paired with BOTH transmitter and receiver
    (exists (?c connector)
      (and (bind (loc ?c $c-area))
           (bind (color ?c $c-hue))           ; MUST be powered
           (eql $c-hue ?hue)                   ; with correct color
           (paired ?c ?transmitter)            ; paired with the specific transmitter
           (paired ?c ?receiver)
           (los agent1 $c-area ?receiver)))
    ;; 2-hop: c1 paired with transmitter and powered, c2 powered and paired with receiver
    (exists (?c1 connector)
      (and (bind (loc ?c1 $c1-area))
           (bind (color ?c1 $c1-hue))          ; c1 MUST be powered
           (eql $c1-hue ?hue)                   ; with correct color
           (paired ?c1 ?transmitter)            ; c1 paired with the specific transmitter
           (exists (?c2 connector)
             (and (different ?c1 ?c2)
                  (bind (loc ?c2 $c2-area))
                  (bind (color ?c2 $c2-hue))    ; c2 MUST be powered
                  (eql $c2-hue ?hue)             ; with correct color
                  (or (paired ?c1 ?c2) (paired ?c2 ?c1))
                  (paired ?c2 ?receiver)
                  (visible agent1 $c1-area $c2-area)
                  (los agent1 $c2-area ?receiver)))))))


(define-query h-unpowered-placed-connectors ()
  ;; Counts placed connectors that are not powered (no color).
  ;; A placed connector without power represents wasted work - it's not
  ;; contributing to any beam network yet.
  ;; Returns: 0-3 (count of unpowered placed connectors)
  (do (setq $count 0)
      (doall (?c connector)
        (if (and (bind (loc ?c $area))           ; connector is placed
                 (not (bind (color ?c $hue))))   ; but not powered
          (incf $count)))
      $count))


(define-query h-color-mismatch ()
  ;; Counts powered connectors paired with receivers of WRONG color.
  ;; Example: connector has color=blue but paired with receiver2 (red).
  ;; This is actively harmful - the connector is working but can never help.
  ;; Returns: count of mismatched pairings
  (do (setq $count 0)
      (doall (?c connector)
        (if (bind (color ?c $c-hue))  ; connector is powered
          (doall (?r receiver)
            (if (and (paired ?c ?r)
                     (bind (chroma ?r $r-hue))
                     (not (eql $c-hue $r-hue)))  ; colors don't match
              (incf $count)))))
      $count))


(define-query h-goal-receivers-inactive ()
  ;Counts how many goal receivers still need activation.
  ;The goal requires both receiver2 and receiver3 to be active.
  ; Each inactive receiver contributes 1 to the heuristic.
  ; This is the most direct measure of goal distance since activating
  ; receivers is the primary objective.
  ; Returns: 0 (both active) to 2 (neither active)
  (do (setq $n 0)
      (if (not (active receiver2)) (incf $n))
      (if (not (active receiver3)) (incf $n))
      $n))


(define-query h-gate1-blocks-goal ()
  ;Penalty for gate1 being closed.
  ;Gate1 separates areas 1-3 from area4. Since the goal requires
  ;the agent to be in area4, gate1 must eventually open.
  ;Gate1 is controlled by receiver1, which requires a red beam.
  ;Returns: 0 (gate open) or 1 (gate closed)
  (if (open gate1) 0 1))


(define-query h-agent-goal-distance ()
  ;Penalty for agent not being in the goal area.
  ;The goal requires (loc agent1 area4). This component provides
  ;a small incentive to move toward area4 once other conditions
  ;allow it.
  ;Lower weight than gate1-closed because reaching area4 is only
  ;possible after gate1 opens.
  ;Returns: 0 (in area4) or 1 (elsewhere)
  (do (bind (loc agent1 $area))
      (if (eql $area 'area4) 0 1)))


(define-query h-useless-pairings ()  ;remove because pairings can always become useful
  ;; Penalizes placed connectors with pairings that cannot carry beams.
  ;; A pairing is useless if it connects two receivers (neither emits).
  ;; Returns: count of useless pairings
  (do (setq $useless 0)
      (doall (?c connector)
        (if (bind (loc ?c $area))  ; only placed connectors
          (doall (?t1 terminus)
            (if (paired ?c ?t1)
              (doall (?t2 terminus)
                (if (and (different ?t1 ?t2)
                         (paired ?c ?t2)
                         (receiver ?t1)
                         (receiver ?t2))
                  ;; Both paired targets are receivers - useless
                  (incf $useless)))))))
      ;; Divide by 2 since we count each pair twice
      (floor $useless 2)))
|#


;;;; LOWER BOUND FUNCTION ;;;;


(define-query min-steps-remaining? ()
  ;; Admissible lower bound on minimum remaining macro actions to reach any goal state.
  ;; Decomposes into two independent cost components that can be safely summed:
  ;;   1. Pairing establishment — acquire/connect macro actions for missing chains
  ;;   2. Reach goal area — macro actions to get agent to area4, possibly opening gate1
  ;;
  ;; Independence argument: cost1 counts acquire+connect actions for goal receiver chains,
  ;; while cost4 counts actions to open gate1 (via receiver1) and reach area4.
  ;; These operate on non-overlapping goals and use disjoint connector resources,
  ;; so they sum validly.
  ;;
  ;; Initial state estimate: 4 + 1 = 5 (no pairings, not holding, not in area4).
  ;; With cutoff 10, pruning begins at depth 5.
  (do (bind (loc agent1 $agent-area))
      (setq $holding (bind (holds agent1 $cargo)))
      (setq $chains-needed (count-receivers-needing-chains))
      (setq $cost1 (pairing-establishment-cost $chains-needed $holding))
      (setq $cost4 (reach-goal-area-cost $chains-needed $holding $agent-area))
      (+ $cost1 $cost4)))


(define-query count-receivers-needing-chains ()
  ;; How many goal receivers still lack a pairing chain from their transmitter?
  ;; A "chain" is a path of paired connectors linking transmitter to receiver.
  ;; Receivers already active are satisfied regardless of pairings.
  ;; Returns: 0 (both connected or active), 1, or 2 (neither).
  (do (setq $count 0)
      (if (and (not (active receiver2))
               (not (pairing-chain-exists? transmitter1 receiver2)))
        (incf $count))
      (if (and (not (active receiver3))
               (not (pairing-chain-exists? transmitter2 receiver3)))
        (incf $count))
      $count))


(define-query pairing-establishment-cost (?chains-needed ?holding)
  ;; Minimum acquire/connect macro actions to establish missing pairing chains.
  ;; Each new chain requires: acquire connector (1) + connect-with-N (1) = 2.
  ;; If agent already holds a connector, first chain needs only connect-with-N (1).
  ;; Returns: 0, 1, 2, 3, or 4.
  (if (> ?chains-needed 0)
    (if ?holding
      (1+ (* 2 (1- ?chains-needed)))       ;held connector serves one; rest need acquire+connect
      (* 2 ?chains-needed))                 ;each needs acquire+connect
    0))


(define-query reach-goal-area-cost (?chains-needed ?holding ?agent-area)
  ;; Minimum macro actions to get agent to area4, accounting for gate1 state.
  ;;
  ;; Already in area4: 0.
  ;;
  ;; Chains still needed OR gate1 already open:
  ;;   1 move — either subsumed by a future macro action's optional move,
  ;;   or gate is open and only a simple move remains.
  ;;
  ;; All chains done but gate1 still closed (must open gate1 via receiver1):
  ;;   Holding a connector:         connect-with-N (piggyback receiver1) + move = 2
  ;;   Connector available:         acquire (includes move) + connect-with-N + move = 3   ;; CHANGED: was 3/4
  ;;   (acquire macro subsumes move-to-connector, so local vs remote = same cost)
  (if (eql ?agent-area 'area4)
    0
    (if (or (> ?chains-needed 0) (open gate1))
      1
      (if ?holding
        2
        3))))


(define-query pairing-chain-exists? (?transmitter ?receiver)
  ;; BFS from ?transmitter through paired connectors to ?receiver.
  ;; Returns t if a chain of connector pairings links transmitter to receiver.
  ;; With only 3 connectors, BFS terminates in at most 3 iterations.
  (do (setq $frontier (list ?transmitter))
      (setq $visited nil)
      (ww-loop while $frontier do
        (setq $next-frontier nil)
        (ww-loop for $current in $frontier do
          (push $current $visited)
          (doall (?c connector)
            (if (and (not (member ?c $visited))
                     (or (paired ?c $current)
                         (and (connector $current) (paired $current ?c))))
              (if (paired ?c ?receiver)
                (return-from pairing-chain-exists? t)
                (if (not (member ?c $next-frontier))
                  (push ?c $next-frontier))))))
        (setq $frontier $next-frontier))
      nil))


;;;; QUERY FUNCTIONS ;;;;


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
  ;; Efficiently get x,y,z coordinates for fixed objects
  (do (bind (coords ?area/fixture $x $y $z))
      (values $x $y $z)))


(define-query get-coordinates (?object)
  ;; Finds x,y,z coordinates for any arbitrary object
  (cond 
    ;; Direct coords for areas and fixtures
    ((bind (coords ?object $x $y $z))
     (values $x $y $z))
    ;; Indirect via loc for movable objects (agents, cargo, buzzer, mine)
    ((and (bind (loc ?object $area))
          (bind (coords $area $x $y $z)))
     (values $x $y $z))
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
      ;(exists (?b blower)
      ;  (and (accessible1 ?area1 ?b ?area2)
      ;       (or (not (active ?b))                              ;blower off: anyone can pass
      ;           (and (ghost-agent ?agent)                      ;blower on + ghost:
      ;                (not (blower-activated-by-ghost ?b))))))  ;only if ghost didn't cause it
))


(define-query connectable (?connector ?area)
  ;; A connector can be connected (activated) in an area only if
  ;; there is no already-colored connector in that area.
  (not (exists (?c connector)
         (and (different ?c ?connector)
              (loc ?c ?area)
              (bind (color ?c $hue))))))


(define-query resolve-hue-by-distance (?hue-distance-pairs)
  ;; Resolves color conflict using path distance priority
  ;; Input: list of (hue . distance) pairs
  ;; Returns: winning hue, or nil if true conflict (equal minimum distances)
  (do
    ;; Handle empty input
    (if (null ?hue-distance-pairs)
      (return-from resolve-hue-by-distance nil))
    ;; Find minimum distance for each unique hue
    (setq $hue-min-dist (make-hash-table :test 'eq))
    (ww-loop for $pair in ?hue-distance-pairs do
      (setq $hue (car $pair))
      (setq $dist (cdr $pair))
      (setq $existing (gethash $hue $hue-min-dist))
      (if (or (null $existing)
              (< $dist $existing))
        (setf (gethash $hue $hue-min-dist) $dist)))
    ;; Find the globally minimum distance and which hue(s) achieve it
    (setq $min-dist most-positive-fixnum)
    (setq $winning-hue nil)
    (setq $tie nil)
    (maphash (lambda ($h $d)
               (cond ((< $d $min-dist)
                      (setq $min-dist $d)
                      (setq $winning-hue $h)
                      (setq $tie nil))
                     ((= $d $min-dist)
                      (setq $tie t))))
             $hue-min-dist)
    ;; Return winner only if no tie at minimum distance
    (if $tie
      nil
      $winning-hue)))


(define-query beam-obstacle-intersection
    (?source-x ?source-y ?end-x ?end-y ?obs-x1 ?obs-y1 ?obs-x2 ?obs-y2)
  ;; Determines if a static obstacle (gate, wall, window) intersects the beam segment.
  ;; Uses strict bounds - no epsilon tolerance since obstacles have no topological
  ;; relationship with beams. Explicit endpoint exclusion is handled by caller.
  ;; Returns (values $t $int-x $int-y) or (values nil nil nil).
  (do (setq $dx1 (- ?end-x ?source-x))      ; Beam direction x
      (setq $dy1 (- ?end-y ?source-y))      ; Beam direction y
      (setq $dx2 (- ?obs-x2 ?obs-x1))       ; Obstacle direction x
      (setq $dy2 (- ?obs-y2 ?obs-y1))       ; Obstacle direction y
      (setq $dx3 (- ?obs-x1 ?source-x))     ; Displacement x
      (setq $dy3 (- ?obs-y1 ?source-y))     ; Displacement y
      ;; Calculate determinant for parallel line detection
      (setq $det (- (* $dy1 $dx2) (* $dx1 $dy2)))
      ;; Handle parallel lines case
      (if (< (abs $det) 1e-10)
        (values nil nil nil)
        ;; Solve for intersection parameters using Cramer's rule
        (do (setq $t (/ (- (* $dy3 $dx2) (* $dx3 $dy2)) $det))
            (setq $s (/ (- (* $dx1 $dy3) (* $dy1 $dx3)) $det))
            ;; Strict bounds: intersection must be in interior of both segments
            (if (and (> $t 0.0) (< $t 1.0)      ; beam interior
                     (> $s 0.0) (< $s 1.0))     ; obstacle interior  ; <-- CHANGED: no epsilon
              ;; Calculate and return intersection coordinates
              (do (setq $int-x (+ ?source-x (* $t $dx1)))
                  (setq $int-y (+ ?source-y (* $t $dy1)))
                  (values $t $int-x $int-y))
              ;; No valid intersection
              (values nil nil nil))))))


(define-query beam-beam-intersection
    (?source-x ?source-y ?end-x ?end-y ?cross-x1 ?cross-y1 ?cross-x2 ?cross-y2)
  ;; Determines if two beam segments intersect, with epsilon tolerance to handle
  ;; shared endpoints (beams from same connector) and chained beams (relay points).
  ;; Also handles collinear beams traveling toward each other (head-on collision).
  ;; Returns (values $t $int-x $int-y) or (values nil nil nil).
  (do (setq $dx1 (- ?end-x ?source-x))      ; Beam 1 direction x
      (setq $dy1 (- ?end-y ?source-y))      ; Beam 1 direction y
      (setq $dx2 (- ?cross-x2 ?cross-x1))   ; Beam 2 direction x
      (setq $dy2 (- ?cross-y2 ?cross-y1))   ; Beam 2 direction y
      (setq $dx3 (- ?cross-x1 ?source-x))   ; Displacement x
      (setq $dy3 (- ?cross-y1 ?source-y))   ; Displacement y
      ;; Calculate determinant for parallel line detection
      (setq $det (- (* $dy1 $dx2) (* $dx1 $dy2)))
      (setq $eps 2e-2)
      ;; Handle parallel/collinear lines case
      (if (< (abs $det) 1e-10)
        ;; Lines are parallel - check if collinear
        (do (setq $cross-disp (- (* $dx1 $dy3) (* $dy1 $dx3)))
            (if (>= (abs $cross-disp) 1e-10)
              ;; Parallel but not collinear - no intersection
              (values nil nil nil)
              ;; Collinear - check for overlap
              (do (setq $len-sq (+ (* $dx1 $dx1) (* $dy1 $dy1)))
                  (if (< $len-sq 1e-10)
                    ;; Degenerate beam1
                    (values nil nil nil)
                    ;; Project beam2 endpoints onto beam1's parameterization
                    (do (setq $t-start (/ (+ (* $dx3 $dx1) (* $dy3 $dy1)) $len-sq))
                        (setq $vx4 (- ?cross-x2 ?source-x))
                        (setq $vy4 (- ?cross-y2 ?source-y))
                        (setq $t-end (/ (+ (* $vx4 $dx1) (* $vy4 $dy1)) $len-sq))
                        ;; Determine overlap with beam1's valid range [0,1]
                        (setq $proj-min (min $t-start $t-end))
                        (setq $proj-max (max $t-start $t-end))
                        (setq $overlap-min (max 0.0 $proj-min))
                        (setq $overlap-max (min 1.0 $proj-max))
                        ;; Check if there's meaningful overlap
                        (if (<= $overlap-max $overlap-min)
                          ;; No overlap
                          (values nil nil nil)
                          ;; Have overlap - check if it's in interior of both beams
                          (do ;; Interior of beam1: (eps, 1-eps)
                              (setq $int-min (max $overlap-min $eps))
                              (setq $int-max (min $overlap-max (- 1.0 $eps)))
                              (if (>= $int-min $int-max)
                                ;; Overlap doesn't include interior
                                (values nil nil nil)
                                ;; Also check interior of beam2
                                (do (setq $t-range (- $t-end $t-start))
                                    (if (< (abs $t-range) 1e-10)
                                      ;; Degenerate beam2
                                      (values nil nil nil)
                                      ;; Find t range where s ∈ (eps, 1-eps)
                                      (do (if (> $t-range 0)
                                            (do (setq $s-valid-min (+ $t-start (* $eps $t-range)))
                                                (setq $s-valid-max (+ $t-start (* (- 1.0 $eps) $t-range))))
                                            (do (setq $s-valid-min (+ $t-start (* (- 1.0 $eps) $t-range)))
                                                (setq $s-valid-max (+ $t-start (* $eps $t-range)))))
                                          ;; Intersect with beam1's interior range
                                          (setq $final-min (max $int-min $s-valid-min))
                                          (setq $final-max (min $int-max $s-valid-max))
                                          (if (>= $final-min $final-max)
                                            ;; No valid overlap in both interiors
                                            (values nil nil nil)
                                            ;; Return midpoint of overlap using RATIONAL arithmetic
                                            ;; Use original rational bounds, not epsilon-adjusted floats
                                            (do (setq $rational-min (max 0 $proj-min))  ; <-- CHANGED: 0 not 0.0
                                                (setq $rational-max (min 1 $proj-max))  ; <-- CHANGED: 1 not 1.0
                                                (setq $t (/ (+ $rational-min $rational-max) 2))  ; <-- CHANGED: 2 not 2.0
                                                (setq $int-x (+ ?source-x (* $t $dx1)))
                                                (setq $int-y (+ ?source-y (* $t $dy1)))
                                                (values $t $int-x $int-y))))))))))))))  ; <-- ADDED: collinear handling
        ;; Non-parallel case: Solve for intersection parameters using Cramer's rule
        (do (setq $t (/ (- (* $dy3 $dx2) (* $dx3 $dy2)) $det))
            (setq $s (/ (- (* $dx1 $dy3) (* $dy1 $dx3)) $det))
            ;; Epsilon bounds: avoid false intersections at shared endpoints
            (if (and (> $t $eps) (< $t (- 1.0 $eps))      ; beam 1 interior
                     (> $s $eps) (< $s (- 1.0 $eps)))     ; beam 2 interior
              ;; Calculate and return intersection coordinates
              (do (setq $int-x (+ ?source-x (* $t $dx1)))
                  (setq $int-y (+ ?source-y (* $t $dy1)))
                  (values $t $int-x $int-y))
              ;; No valid intersection
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
  ;; Establishes each beam's intended full path by finding intersections with static obstacles only
  ;; Returns the endpoint coordinates where the beam terminates
  (do
    ;; Initialize closest intersection tracking
    (setq $closest-t 1.0)  ; Default to target if no intersections
    (setq $result-x ?target-x)   
    (setq $result-y ?target-y)
    
    ;; Bind cached geometry lists once
    (bind (wall-segments $walls))
    (bind (gate-segments $gates))
    
    ;; Loop 1: Check walls (always block)
    (ww-loop for $entry in $walls do
      (setq $x1 (second $entry))
      (setq $y1 (third $entry))
      (setq $x2 (fourth $entry))
      (setq $y2 (fifth $entry))
      ;; Endpoint exclusion
      (if (not (or (and (= $x1 ?source-x) (= $y1 ?source-y))
                   (and (= $x2 ?source-x) (= $y2 ?source-y))
                   (and (= $x1 ?target-x) (= $y1 ?target-y))
                   (and (= $x2 ?target-x) (= $y2 ?target-y))))
        (do (mvsetq ($int-t $int-x $int-y)
              (beam-obstacle-intersection ?source-x ?source-y ?target-x ?target-y $x1 $y1 $x2 $y2))
            (if (and $int-t (< $int-t $closest-t))
              (do (setq $closest-t $int-t)
                  (setq $result-x $int-x)
                  (setq $result-y $int-y))))))
    
    ;; Loop 2: Check gates (only if closed)
    (ww-loop for $entry in $gates do
      (setq $gate (first $entry))
      (if (not (open $gate))
        (do (setq $x1 (second $entry))
            (setq $y1 (third $entry))
            (setq $x2 (fourth $entry))
            (setq $y2 (fifth $entry))
            ;; Endpoint exclusion
            (if (not (or (and (= $x1 ?source-x) (= $y1 ?source-y))
                         (and (= $x2 ?source-x) (= $y2 ?source-y))
                         (and (= $x1 ?target-x) (= $y1 ?target-y))
                         (and (= $x2 ?target-x) (= $y2 ?target-y))))
              (do (mvsetq ($int-t $int-x $int-y)
                    (beam-obstacle-intersection ?source-x ?source-y ?target-x ?target-y $x1 $y1 $x2 $y2))
                  (if (and $int-t (< $int-t $closest-t))
                    (do (setq $closest-t $int-t)
                        (setq $result-x $int-x)
                        (setq $result-y $int-y))))))))
    
    ;; Loop 3: Check cargo and agents (dynamic positions)
    (doall (?obj (either cargo agent))
      (if (and (bind (loc ?obj $area))
               (bind (coords $area $x1 $y1 $z1))
               ;; Endpoint exclusion
               (not (or (and (= $x1 ?source-x) (= $y1 ?source-y))
                        (and (= $x1 ?target-x) (= $y1 ?target-y)))))
        (do (mvsetq ($int-t $int-x $int-y)
              (beam-segment-occlusion ?source-x ?source-y ?target-x ?target-y $x1 $y1))
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
                  (beam-beam-intersection
                    $src1-x $src1-y $tgt1-x $tgt1-y
                    $src2-x $src2-y $tgt2-x $tgt2-y))
                (if $t1
                  (do
                    ;; Get t2 parameter by checking beam2 vs beam1
                    (mvsetq ($t2 $int-x2 $int-y2)
                      (beam-beam-intersection
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


(define-query compute-relay-distances ()
  ;; BFS from all transmitters, returning hash table of relay -> min distance
  ;; Distance represents hop count from nearest transmitter
  (do
    ;; Initialize distance table - transmitters are at distance 0
    (setq $distances (make-hash-table :test 'eq))
    (setq $frontier nil)
    (doall (?t transmitter)
      (do (setf (gethash ?t $distances) 0)
          (push ?t $frontier)))
    ;; BFS loop
    (ww-loop while $frontier do
      (setq $next-frontier nil)
      (ww-loop for $source in $frontier do
        (setq $source-dist (gethash $source $distances))
        ;; Check all beams originating from this source
        (doall (?b (get-current-beams))
          (do (bind (beam-segment ?b $src $tgt $end-x $end-y))
              (if (eql $src $source)
                ;; Beam originates from current source - check if target is a relay
                (if (relay $tgt)
                  ;; Verify beam actually reaches target coordinates
                  (do (mvsetq ($tgt-x $tgt-y $tgt-z) (get-coordinates $tgt))
                      (if (and (= $end-x $tgt-x)
                               (= $end-y $tgt-y))
                        ;; Beam reaches target - record distance if not yet visited
                        (if (not (gethash $tgt $distances))
                          (do (setf (gethash $tgt $distances) (1+ $source-dist))
                              (push $tgt $next-frontier))))))))))
      (setq $frontier $next-frontier))
    $distances))


(define-query occlusion-area (?area)
  ;; True if placing cargo at ?area would occlude at least one current beam
  ;; that is reaching a meaningful target (receiver or relay).
  (do (bind (coords ?area $px $py $pz))
      (exists (?b (get-current-beams))
        (and (bind (beam-segment ?b $src $tgt $end-x $end-y))
             (or (receiver $tgt)
                 (relay $tgt))
             (beam-reaches-target $src $tgt)
             (mvsetq ($sx $sy $sz) (get-coordinates $src))
             (mvsetq ($t $ix $iy)
               (beam-segment-occlusion $sx $sy $end-x $end-y $px $py))
             $t))))


;;;; UPDATE FUNCTIONS ;;;;


(define-update propagate-changes! ()
  (ww-loop for $iteration from 1 to 5
           do (if (not (propagate-consequences!))
                (return))  ;convergence achieved
           finally (inconsistent-state)))  ;no convergence, mark state inconsistent for pruning


(define-update propagate-consequences! ()
  ;; All functions must execute in order; returns t if any change occurred
  (some #'identity
        (mapcar (lambda (fn) (funcall fn state))
                (list #'derive-holds!                   ;always returns nil
                      ;#'update-plate-controlled-devices!
                      ;#'blow-objects-if-active!
                      #'create-missing-beams!
                      #'remove-orphaned-beams!
                      #'recalculate-all-beams!          ;always returns nil
                      #'update-beams-if-interference!   ;always returns nil
                      #'deactivate-conflicted-relays!
                      #'deactivate-receivers-that-lost-power!
                      #'deactivate-unpowered-relays!
                      #'activate-receivers-that-gained-power!
                      #'activate-reachable-relays!))))


(define-update derive-holds! ()
  ;; Derives holds relation from loc state.
  ;; Cargo without loc is held; otherwise nothing is held.
  ;; Returns nil (no downstream propagation effects).
  (do
    (doall (?agent agent)
      (if (exists (?cargo cargo)
            (and (not (bind (loc ?cargo $any-loc)))
                 (setq $held ?cargo)))
        (holds ?agent $held)
        (do (bind (holds ?agent $any-cargo))
            (not (holds ?agent $any-cargo)))))
    nil))


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
  ;; Resolves all beam-beam intersections using iterative fixed-point computation.
  ;; Each iteration validates intersections against current effective endpoints,
  ;; converging when no beam's effective endpoint changes.  This avoids the
  ;; cascading phantom problem where a single-pass approach uses an invalid
  ;; (phantom) intersection to deflate a beam's effective-t.
  ;; Detects period-2 oscillation (mutual blocking cycles) and resolves by
  ;; taking element-wise min of the two alternating states.
  (do
    ;; Phase 1: Collect all geometric intersections
    (setq $all-intersections (collect-all-beam-intersections))
    ;; Phase 2: Build per-beam geometry cache and obstacle-only t parameters
    (setq $obstacle-t (make-hash-table :test 'eq))
    (setq $effective-t (make-hash-table :test 'eq))
    (setq $beam-geometry (make-hash-table :test 'eq))  ;beam -> (src-x src-y dx dy)
    (doall (?b (get-current-beams))
      (do (bind (beam-segment ?b $src $tgt $old-end-x $old-end-y))
          (mvsetq ($src-x $src-y $src-z) (get-coordinates $src))
          (mvsetq ($tgt-x $tgt-y $tgt-z) (get-coordinates $tgt))
          (setq $dx (- $tgt-x $src-x))
          (setq $dy (- $tgt-y $src-y))
          (setf (gethash ?b $beam-geometry) (list $src-x $src-y $dx $dy))
          (setq $length-sq (+ (* $dx $dx) (* $dy $dy)))
          (if (< $length-sq 1e-20)
            (setq $t0 0)
            (do (setq $curr-dx (- $old-end-x $src-x))
                (setq $curr-dy (- $old-end-y $src-y))
                (setq $dot (+ (* $curr-dx $dx) (* $curr-dy $dy)))
                (setq $t0 (/ $dot $length-sq))))
          (setf (gethash ?b $obstacle-t) $t0)
          (setf (gethash ?b $effective-t) $t0)))
    ;; Phase 3: Iterative fixed-point resolution of beam-beam interference
    (setq $prev1-t (make-hash-table :test 'eq))  ;values from 1 iteration ago
    (setq $prev2-t (make-hash-table :test 'eq))  ;values from 2 iterations ago
    (ww-loop for $iteration from 1 to 10 do  ;bounded; converges in <= num-beams iterations
      (setq $changed nil)
      (doall (?b (get-current-beams))
        (do (setq $new-t (gethash ?b $obstacle-t))  ;start from obstacle upper bound
            (ww-loop for $intersection in $all-intersections do
              (setq $beam1 (first $intersection))
              (setq $beam2 (second $intersection))
              (setq $t1 (fifth $intersection))
              (setq $t2 (sixth $intersection))
              (if (eql ?b $beam1)
                (do (setq $t-param $t1)
                    (setq $blocker $beam2)
                    (setq $blocker-t $t2))
                (if (eql ?b $beam2)
                  (do (setq $t-param $t2)
                      (setq $blocker $beam1)
                      (setq $blocker-t $t1))
                  (setq $t-param nil)))
              ;; Valid only if blocker's effective endpoint reaches the crossing
              (if (and $t-param
                       (>= (gethash $blocker $effective-t) $blocker-t)
                       (< $t-param $new-t))
                (setq $new-t $t-param)))
            (if (/= $new-t (gethash ?b $effective-t))
              (do (setf (gethash ?b $effective-t) $new-t)
                  (setq $changed t)))))
      (if (not $changed)
        (return))
      ;; Detect period-2 oscillation: current state matches state from 2 iterations ago
      (if (and (> $iteration 2)
               (not (exists (?b (get-current-beams))
                      (/= (gethash ?b $effective-t)
                          (gethash ?b $prev2-t -1)))))
        ;; Oscillation detected: resolve by element-wise min of the two alternating
        ;; states (current = state A, prev1-t = state B).  Crossing beams that
        ;; appear truncated in either state are truly intersecting, so min is correct.
        (do (doall (?b (get-current-beams))
              (do (setq $alt-val (gethash ?b $prev1-t))
                  (if (< $alt-val (gethash ?b $effective-t))
                    (setf (gethash ?b $effective-t) $alt-val))))
            (return)))
      ;; Rotate saved states: prev2 <- prev1, prev1 <- current
      (doall (?b (get-current-beams))
        (do (setf (gethash ?b $prev2-t) (gethash ?b $prev1-t))
            (setf (gethash ?b $prev1-t) (gethash ?b $effective-t)))))
    ;; Phase 4: Apply converged effective-t values to update beam endpoints
    (doall (?b (get-current-beams))
      (do (bind (beam-segment ?b $src $tgt $old-end-x $old-end-y))
          (setq $geom (gethash ?b $beam-geometry))
          (setq $src-x (first $geom))
          (setq $src-y (second $geom))
          (setq $dx (third $geom))
          (setq $dy (fourth $geom))
          (setq $new-end-x (+ $src-x (* (gethash ?b $effective-t) $dx)))
          (setq $new-end-y (+ $src-y (* (gethash ?b $effective-t) $dy)))
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


(define-update deactivate-conflicted-relays! ()
  ;; Deactivates relays receiving beams where a different color wins by distance,
  ;; or where there's a true conflict (equal minimum distances for different colors)
  ;; Returns t if any relay was deactivated, nil otherwise
  (do
    ;; Compute distance table once for all relays
    (setq $distances (compute-relay-distances))
    (doall (?r relay)
      (if (bind (color ?r $existing-hue))
        ;; Relay is active - check for conflicts using distance priority
        (do
          ;; Collect all (hue . distance) pairs from beams that reach this relay
          (setq $reaching-pairs nil)
          (doall (?src terminus)
            (if (or (paired ?r ?src) (paired ?src ?r))
              (do
                (setq $src-hue (get-hue-if-source ?src))
                (if (and $src-hue
                         (beam-reaches-target ?src ?r))
                  ;; Compute distance and collect pair
                  (do (setq $src-dist (gethash ?src $distances))
                      (if $src-dist
                        (push (cons $src-hue (1+ $src-dist)) $reaching-pairs)))))))
          ;; Resolve using distance priority
          (setq $winning-hue (resolve-hue-by-distance $reaching-pairs))
          ;; Deactivate if true conflict (nil) or winner differs from current
          (if (and $reaching-pairs
                   (or (null $winning-hue)
                       (not (eql $winning-hue $existing-hue))))
            (do (not (color ?r $existing-hue))
                (setq $deactivated-any t))))))
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
  ;; Returns t if any relay was activated, nil otherwise
  ;; Uses distance-aware resolution: shorter path to transmitter wins
  ;; For connectors: enforces single-active-connector-per-area constraint
  (do
    ;; Compute distance table once for all relays
    (setq $distances (compute-relay-distances))
    (doall (?r relay)
      (if (not (bind (color ?r $existing-hue)))
        (do
          ;; Collect all (hue . distance) pairs from beams that reach this relay
          (setq $reaching-pairs nil)
          ;; Check all paired termini that could be sources
          (doall (?src terminus)
            (if (or (paired ?r ?src) (paired ?src ?r))  ; Pairing exists (bidirectional)
              (do
                ;; Get source hue if it's an active source (transmitter or powered relay)
                (setq $src-hue (get-hue-if-source ?src))
                ;; If source has hue AND beam reaches relay, collect pair with distance
                (if (and $src-hue
                         (beam-reaches-target ?src ?r))
                  ;; Compute distance and collect pair
                  (do (setq $src-dist (gethash ?src $distances))
                      (if $src-dist
                        (push (cons $src-hue (1+ $src-dist)) $reaching-pairs)))))))
          ;; Resolve using distance priority instead of consensus
          (setq $winning-hue (resolve-hue-by-distance $reaching-pairs))
          ;; Only activate if there's a clear winner (not a tie)
          ;; For connectors: also verify no other active connector in same area
          (if (and $winning-hue
                   (or (repeater ?r)
                       (and (connector ?r)
                            (bind (loc ?r $relay-area))
                            (connectable ?r $relay-area))))
            (do (color ?r $winning-hue)
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
      ;; With (paired connector terminus), also remove facts where ?cargo appears
      ;; as the terminus: (paired other-connector ?cargo).
      (doall (?c connector)
        (if (paired ?c ?cargo)
          (not (paired ?c ?cargo))))
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


;;;; ACTIONS ;;;;


(define-action acquire-unpaired-connector
  ;; Move to unpaired connector (optional) + pickup.
  ;; Listed before acquire-paired to prefer non-disruptive pickups first.
    1
  (?agent agent ?connector connector)
  (and (not (exists (?t terminus)
              (paired ?connector ?t)))
       (not (exists (?c connector)
              (paired ?c ?connector)))
       (not (bind (holds ?agent $any-cargo)))
       (bind (loc ?agent $agent-area))
       (bind (loc ?connector $connector-area))
       (or (eql $agent-area $connector-area)
           (accessible ?agent $agent-area $connector-area)))
  (?agent ?connector $connector-area)
  (assert (loc ?agent $connector-area)
          (not (loc ?connector $connector-area))
          (finally (propagate-changes!))))


(define-action acquire-paired-connector
  ;; Move to paired connector (optional) + pickup + disconnect.
  ;; Listed after acquire-unpaired since this disrupts existing beam networks.
    1
  (?agent agent ?connector connector)
  (and (or (exists (?t terminus)
             (paired ?connector ?t))
           (exists (?c connector)
             (paired ?c ?connector)))
       (not (bind (holds ?agent $any-cargo)))
       (bind (loc ?agent $agent-area))
       (bind (loc ?connector $connector-area))
       (or (eql $agent-area $connector-area)
           (accessible ?agent $agent-area $connector-area)))
  (?agent ?connector $connector-area)
  (assert (loc ?agent $connector-area)
          (not (loc ?connector $connector-area))
          (disconnect-connector! ?connector)
          (finally (propagate-changes!))))


#+ignore (define-action connect-with-1-terminus
  ;; Move to destination (optional) + place connector paired with 1 terminus.
    1
  (?agent agent ?t1 terminus ?area2 area)
  (and (bind (holds ?agent $connector))
       (connector $connector)
       (different $connector ?t1)
       (not (and (connector ?t1) (loc ?t1 ?area2)))
       (selectable ?agent ?area2 ?t1)
       (bind (loc ?agent $area1))
       (or (eql $area1 ?area2)
           (accessible ?agent $area1 ?area2)))
  (?agent $connector ?t1 ?area2)
  (assert (loc ?agent ?area2)
          (loc $connector ?area2)
          (paired $connector ?t1)
          (finally (propagate-changes!))))


(define-action connect-with-2-terminus
  ;; Move to destination (optional) + place connector paired with 2 termini.
    1
  (?agent agent (combination (?t1 ?t2) terminus) ?area2 area)
  (and (bind (holds ?agent $connector))
       (connector $connector)
       (different $connector ?t1)
       (different $connector ?t2)
       (not (and (connector ?t1) (loc ?t1 ?area2)))
       (not (and (connector ?t2) (loc ?t2 ?area2)))
       (selectable ?agent ?area2 ?t1)
       (selectable ?agent ?area2 ?t2)
       (bind (loc ?agent $area1))
       (or (eql $area1 ?area2)
           (accessible ?agent $area1 ?area2)))
  (?agent $connector ?t1 ?t2 ?area2)
  (assert (loc ?agent ?area2)
          (loc $connector ?area2)
          (paired $connector ?t1)
          (paired $connector ?t2)
          (finally (propagate-changes!))))


(define-action connect-with-3-terminus
  ;; Move to destination (optional) + place connector paired with 3 termini.
    1
  (?agent agent (combination (?t1 ?t2 ?t3) terminus) ?area2 area)
  (and (bind (holds ?agent $connector))
       (connector $connector)
       (different $connector ?t1)
       (different $connector ?t2)
       (different $connector ?t3)
       (not (and (connector ?t1) (loc ?t1 ?area2)))
       (not (and (connector ?t2) (loc ?t2 ?area2)))
       (not (and (connector ?t3) (loc ?t3 ?area2)))
       (selectable ?agent ?area2 ?t1)
       (selectable ?agent ?area2 ?t2)
       (selectable ?agent ?area2 ?t3)
       (bind (loc ?agent $area1))
       (or (eql $area1 ?area2)
           (accessible ?agent $area1 ?area2)))
  (?agent $connector ?t1 ?t2 ?t3 ?area2)
  (assert (loc ?agent ?area2)
          (loc $connector ?area2)
          (paired $connector ?t1)
          (paired $connector ?t2)
          (paired $connector ?t3)
          (finally (propagate-changes!))))


(define-action deliver-to-place
  ;; Move to different area + place cargo. Constrained: placement must
  ;; occlude a current beam to be potentially useful.
    1
  (?agent agent ?area2 area)
  (and (bind (holds ?agent $cargo))
       (bind (loc ?agent $area1))
       (different $area1 ?area2)
       (accessible ?agent $area1 ?area2)
       (occlusion-area ?area2))
  (?agent $cargo ?area2)
  (assert (loc ?agent ?area2)
          (loc $cargo ?area2)
          (finally (propagate-changes!))))


(define-action move
  ;; Standalone move (no cargo manipulation). Needed for final move to goal area
  ;; or repositioning when no macro action applies.
    1
  (?agent agent ?area2 area)
  (and (bind (loc ?agent $area1))
       (different $area1 ?area2)
       (accessible ?agent $area1 ?area2))
  (?agent $area1 ?area2)
  (assert (loc ?agent ?area2)
          (finally (propagate-changes!))))


;;;; INITIALIZATION ;;;;


(define-init
  ;; Dynamic state (agent-manipulable or derived)
  (loc agent1 area1)
  (loc connector1 area1)
  (loc connector2 area2)
  (loc connector3 area3)
  ;(elevation agent1 0)
  ;(elevation connector1 0)
  ;(elevation connector2 0)
  ;(elevation connector3 0)
  (current-beams ())  ; Empty - can be populated by init-action, if exist initially

  ;; Static spatial configuration
  (coords area1 9 1 0)
  (coords area2 9 8 0)
  (coords area3 10 9 0)
  (coords area4 7 8 0)
  (coords transmitter1 11 1/10 1)
  (coords transmitter2 10 1/10 1)
  (coords receiver1 81/10 1 1)
  (coords receiver2 7 109/10 1)
  (coords receiver3 1 109/10 1)
  (controls receiver1 gate1)
  (wall-segments ((wall1 8 7 8 8) (wall2 8 0 8 3)))  ;internal walls only needed
  (gate-segments ((gate1 8 3 8 7) (gate2 2 11 6 11)))
  (window-segments ((window1 8 8 8 11)))
  (chroma transmitter1 red)
  (chroma transmitter2 blue)
  (chroma receiver1 red)
  (chroma receiver2 red)
  (chroma receiver3 blue)
  
  ;; Line-of-sight relationships (connector area to fixture)
  (los0 area1 transmitter1)
  (los0 area1 transmitter2)
  (los0 area1 receiver1)
  (los0 area2 transmitter1)
  (los0 area2 transmitter2)
  (los0 area2 receiver1)
  (los0 area2 receiver2)
  (los0 area2 receiver3)
  (los0 area3 transmitter1)
  (los0 area3 transmitter2)
  (los0 area3 receiver1)
  (los0 area3 receiver2)
  (los0 area3 receiver3)
  (los1 area4 gate1 transmitter1)
  (los1 area4 gate1 transmitter2)
  (los0 area4 receiver2)
  (los0 area4 receiver3)
  
  ;; Visibility relationships (connector area to area)
  (visible0 area1 area2)
  (visible0 area1 area3)
  (visible1 area1 gate1 area4)
  (visible0 area2 area3)
  (visible0 area2 area4)
  (visible0 area3 area4)
  

  ;; Accessibility (move area to area) ; chain moves between areas to lower search branching
  (accessible0 area1 area2)  
  (accessible0 area1 area3)
  (accessible1 area1 gate1 area4)
  (accessible0 area2 area3)
  (accessible1 area2 gate1 area4)
  (accessible1 area3 gate1 area4)
)


;;;; GOAL ;;;;


(define-goal
  (and (loc agent1 area4)
       (active receiver2)
       (active receiver3)))


#+ignore (define-goal
  (and (loc agent1 area4)
       (loc connector1 area2)
       (not (bind (loc connector2 $anywhere)))
       (loc connector3 area3)
       (paired connector1 transmitter2)
       (paired connector1 receiver3)
       (paired connector3 transmitter1)
       (paired connector3 receiver2)
))


#+ignore (define-goal
  (and (active receiver2)
       (active receiver3)
       (loc agent1 area4)
       (loc connector1 area2)
       (color connector1 blue)
       (paired connector1 transmitter2)
       (paired connector1 receiver3)
       (loc connector3 area3)
       (color connector3 red)
       (paired connector3 transmitter1)
       (paired connector3 receiver2)
       (holds agent1 connector2)
       (not (open gate1))
       (not (active receiver1))
))

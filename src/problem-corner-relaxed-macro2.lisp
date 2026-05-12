;;; Filename: problem-corner-relaxed-macro2.lisp

;;; Talos Principle problem 'Around the Corner' in Purgatory workshop 3.
;;; Can represent coords as 3D, but needs only 2D for beam calculations.
;;; Note that macros are best applied for problems that need to search for
;;; a sequence of subgoals to reach a solution.
;;;
;;; RELAXED + PAIR-MACRO SEARCH VARIANT.
;;;
;;; Relaxed search discipline (from problem-corner-relaxed.lisp):
;;;   Search uses base relations only; derived relations (open, active, color,
;;;   holds, beams) are NOT propagated during the search.  Candidate plans
;;;   are post-validated at goal by invoking propagate-changes! and checking
;;;   derived relations.  (open gate1) is approximated during search by
;;;   gate1-open-relaxed, a structural pairing-chain check over loc/paired
;;;   + static LOS; the `move` action retains an exact accessible check that
;;;   propagates a state copy.
;;;
;;; Pair-macro methodology:
;;;   The primitive action set (pickup-connector, connect-to-N-terminus,
;;;   move) is retained for completeness.  Macros are introduced one
;;;   producer/consumer pair at a time.  Each macro folds exactly two
;;;   primitives and is validated sound against its regressed precondition.
;;;   Primitives remain in the action set so no optimal plan expressible
;;;   in the primitive set becomes unreachable.
;;;
;;; This file is the working baseline for pair-macro development.  It
;;; starts with primitives only; pair macros will be added incrementally.


(in-package :ww)


(ww-set *problem-name* corner-relaxed-macro2)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 9)

(ww-set *symmetry-pruning* t)

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


;;;; LOWER BOUND FUNCTION ;;;;


(define-query min-steps-remaining? ()
  ;; Admissible lower bound on minimum remaining PRIMITIVE actions to reach goal.
  ;; Decomposes into four independent cost components that can be safely summed:
  ;;   1. Pairing establishment — pickup/connect actions for missing chains
  ;;   2. Connector acquisition move — move to reach a connector when none is local
  ;;   3. Placement viability move — move to an area with LOS to both transmitter and receiver
  ;;   4. Reach goal area — move agent to area4, possibly opening gate1 first
  ;;
  ;; Independence argument: pickup and connect are manipulation actions (not moves),
  ;; so pairing costs and move costs use disjoint action types and sum validly.
  ;; The placement viability move is independent of cost2 (only charged when cost2=0)
  ;; and of cost4 (only charged when gate1 is closed, blocking the area4 move).
  ;;
  ;; RELAXED: (active ?r) checks removed; gate state approximated by gate1-open-relaxed.
  ;;
  ;; Initial state estimate: 2 + 2 + 0 + 1 + 1 = 6 (no pairings, not holding,
  ;; no local connector, not in area4).
  ;;
  ;; Admissibility under min-length with pair macros: each macro step folds
  ;; at most 2 primitives, so true-remaining-steps >= ceiling(true-remaining-
  ;; primitives / 2).  We therefore wrap the primitive-step sum in ceiling/2
  ;; to obtain an admissible lower bound on remaining STEPS.
  (do (bind (loc agent1 $agent-area))
      (assign $holding (exists (?c cargo) (not (bind (loc ?c $any)))))  ;base-relation equiv of holds
      (assign $chains-needed (count-receivers-needing-chains))
      (assign $cost1 (pairing-establishment-cost $chains-needed $holding))
      (assign $cost2 (connector-acquisition-move-cost $chains-needed $holding $agent-area))
      (assign $cost3 (placement-viability-move-cost $chains-needed $holding $agent-area))
      (assign $cost4 (reach-goal-area-cost $chains-needed $holding $agent-area))
      (ceiling (+ $cost1 $cost2 $cost3 $cost4) 2)))


(define-query count-receivers-needing-chains ()
  ;; How many goal receivers still lack a pairing chain from their transmitter?
  ;; A "chain" is a path of paired connectors linking transmitter to receiver.
  ;; Returns: 0, 1, or 2.
  ;; RELAXED: (active receiverN) is never set, so only check pairing chains.
  (do (assign $count 0)
      (if (not (pairing-chain-exists? transmitter1 receiver2))
        (incf $count))
      (if (not (pairing-chain-exists? transmitter2 receiver3))
        (incf $count))
      $count))


(define-query pairing-establishment-cost (?chains-needed ?holding)
  ;; Minimum pickup/connect actions to establish missing pairing chains.
  ;; Each new chain requires: pickup a connector (1) + connect with pairings (1) = 2.
  ;; If agent already holds a connector, one pickup is saved.
  ;; Returns: 0, 1, 2, 3, or 4.
  (if (> ?chains-needed 0)
    (if ?holding
      (1+ (* 2 (1- ?chains-needed)))       ;held connector serves one; rest need pickup+connect
      (* 2 ?chains-needed))                 ;each needs pickup+connect
    0))


(define-query connector-acquisition-move-cost (?chains-needed ?holding ?agent-area)
  ;; When agent needs a connector but none is in the current area, one extra move
  ;; is required to reach one. This move is independent of the move-to-area4 when
  ;; either: (a) gate1 is closed (can't reach area4 yet), or
  ;;         (b) no connector is in area4 (going to area4 wouldn't help acquire one).
  ;; Returns: 0 or 1.
  (if (and (> ?chains-needed 0)
           (not ?holding)
           (not (exists (?c connector) (loc ?c ?agent-area)))
           (or (not (gate1-open-relaxed))
               (not (exists (?c connector) (loc ?c area4)))))
    1
    0))


(define-query placement-viability-move-cost (?chains-needed ?holding ?agent-area)
  ;; When chains are needed but the agent's current area cannot support a 1-hop
  ;; chain for any needed receiver (no observable LOS to both transmitter and
  ;; receiver), one extra move is required to reach a viable placement area.
  ;; Only charged when: (a) no connector-acquisition move to piggyback on
  ;; (holding or connector locally available), and (b) gate1 is closed so the
  ;; move to area4 cannot serve as the placement move.
  ;; Returns: 0 or 1.
  (if (and (> ?chains-needed 0)
           (not (gate1-open-relaxed))
           (or ?holding
               (exists (?c connector) (loc ?c ?agent-area)))
           (not (area-viable-for-any-needed-chain? ?agent-area)))
    1
    0))


(define-query area-viable-for-any-needed-chain? (?area)
  ;; Returns t if ?area has observable (static potential) LOS to both the
  ;; transmitter and receiver for at least one goal receiver still needing
  ;; a pairing chain.  Uses observable (ignores gate state and occupancy)
  ;; to check whether a 1-hop chain is geometrically possible from ?area.
  ;; RELAXED: (active receiverN) is never set, so only check pairing chains.
  (or (and (not (pairing-chain-exists? transmitter1 receiver2))
           (observable ?area transmitter1)
           (observable ?area receiver2))
      (and (not (pairing-chain-exists? transmitter2 receiver3))
           (observable ?area transmitter2)
           (observable ?area receiver3))))


(define-query reach-goal-area-cost (?chains-needed ?holding ?agent-area)
  ;; Minimum moves to get agent to area4, accounting for gate1 state.
  ;;
  ;; Already in area4: 0.
  ;;
  ;; Chains still needed OR gate1 already (relaxed) open:
  ;;   1 move — either the move is subsumed by connector-acquisition movement,
  ;;   or the gate is open and only a simple move remains.
  ;;
  ;; All chains done but gate1 still closed (must open gate1 via receiver1):
  ;;   Holding a connector:              connect (piggyback receiver1 pairing) + move = 2
  ;;   Connector in current area:        pickup + connect + move = 3
  ;;   No connector locally available:   move-to-connector + pickup + connect + move = 4
  (if (eql ?agent-area 'area4)
    0
    (if (or (> ?chains-needed 0) (gate1-open-relaxed))
      1
      (if ?holding
        2
        (if (exists (?c connector) (loc ?c ?agent-area))
          3
          4)))))


(define-query pairing-chain-exists? (?transmitter ?receiver)
  ;; BFS from ?transmitter through paired connectors to ?receiver.
  ;; Returns t if a chain of connector pairings links transmitter to receiver.
  ;; With only 3 connectors, BFS terminates in at most 3 iterations.
  (do (assign $frontier (list ?transmitter))
      (assign $visited nil)
      (ww-loop while $frontier do
        (assign $next-frontier nil)
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
        (assign $frontier $next-frontier))
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
  (do (mv-assign ($target-x $target-y $target-z) (get-coordinates ?target))
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
  ;; Agent can select terminus if observable from ?area or passable to adjacent area
  (or (observable ?area ?terminus)
      (exists (?adj-area area)
        (and (passable ?agent ?area ?adj-area)
             (observable ?adj-area ?terminus)))))


(define-query gate1-open-relaxed ()
  ;; Relaxed base-relation approximation of (open gate1).
  ;; Gate1 opens when receiver1 is activated by a red beam from transmitter1.
  ;; Checks for a pairing chain from transmitter1 to receiver1 using only
  ;; loc/paired + static LOS, ignoring beam occlusion, interference, and
  ;; gate occlusion of beams.
  ;; 1-hop: connector in any area paired with both transmitter1 and receiver1
  (or (exists (?c connector)
        (and (bind (loc ?c $area))
             (paired ?c transmitter1)
             (paired ?c receiver1)))
      ;; 2-hop: connector in area3 paired with transmitter1, linked to
      ;; connector in area2 paired with receiver1 (only viable direction;
      ;; reverse direction area2->area3 crosses beams causing interference)
      (exists ((?c1 ?c2) connector)
        (and (loc ?c1 area3)
             (loc ?c2 area2)
             (paired ?c1 transmitter1)
             (or (paired ?c1 ?c2) (paired ?c2 ?c1))
             (paired ?c2 receiver1)))))


(define-query accessible (?agent ?area1 ?area2)
  ;; BASELINE (non-relaxed): checks (open ?g) against a propagated copy
  ;; of the current state.  Because we are working only with base relations,
  ;; we must propagate a fresh copy per call to obtain the
  ;; true gate-open status. Expensive but semantically exact.
  (or (accessible0 ?area1 ?area2)
      (exists (?g gate)
        (and (accessible1 ?area1 ?g ?area2)
             (or (ghost-agent ?agent)
                 (do (assign $copy (copy-problem-state state))  ;don't mutate the state in a query
                     (funcall #'propagate-changes! $copy)  ;mutate the copy
                     (let ((state $copy))  ;use the copy for the following test
                       (open ?g))))))))  ;accessible only if the gate is open


(define-query passable (?agent ?area1 ?area2)  ;relaxed version of accessible
  ;; RELAXED: uses gate1-open-relaxed instead of a propagated (open ?g).
  ;; Structural pairing-chain approximation; admits a superset of the
  ;; true reachable states. Candidate solutions must be post-validated
  ;; with full propagation at goal check.
  (or (accessible0 ?area1 ?area2)
      (exists (?g gate)
        (and (accessible1 ?area1 ?g ?area2)
             (or (ghost-agent ?agent)
                 (gate1-open-relaxed))))))


(define-query connectable (?connector ?area)
  ;; A connector can be placed in an area only if no other connector
  ;; already there is engaged (has beams impinging on it).
  (not (exists (?c connector)
         (and (different ?c ?connector)
              (loc ?c ?area)
              (exists (?b (get-current-beams))
                (and (bind (beam-segment ?b $src $tgt $ex $ey))
                     (eql $tgt ?c)))))))


(define-query resolve-hue-by-distance (?hue-distance-pairs)
  ;; Resolves color conflict using path distance priority
  ;; Input: list of (hue . distance) pairs
  ;; Returns: winning hue, or nil if true conflict (equal minimum distances)
  (do
    ;; Handle empty input
    (if (null ?hue-distance-pairs)
      (return-from resolve-hue-by-distance nil))
    ;; Find minimum distance for each unique hue
    (assign $hue-min-dist (make-hash-table :test 'eq))
    (ww-loop for $pair in ?hue-distance-pairs do
      (assign $hue (car $pair))
      (assign $dist (cdr $pair))
      (assign $existing (gethash $hue $hue-min-dist))
      (if (or (null $existing)
              (< $dist $existing))
        (setf (gethash $hue $hue-min-dist) $dist)))
    ;; Find the globally minimum distance and which hue(s) achieve it
    (assign $min-dist most-positive-fixnum)
    (assign $winning-hue nil)
    (assign $tie nil)
    (maphash (lambda ($h $d)
               (cond ((< $d $min-dist)
                      (assign $min-dist $d)
                      (assign $winning-hue $h)
                      (assign $tie nil))
                     ((= $d $min-dist)
                      (assign $tie t))))
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
  (do (assign $dx1 (- ?end-x ?source-x))      ; Beam direction x
      (assign $dy1 (- ?end-y ?source-y))      ; Beam direction y
      (assign $dx2 (- ?obs-x2 ?obs-x1))       ; Obstacle direction x
      (assign $dy2 (- ?obs-y2 ?obs-y1))       ; Obstacle direction y
      (assign $dx3 (- ?obs-x1 ?source-x))     ; Displacement x
      (assign $dy3 (- ?obs-y1 ?source-y))     ; Displacement y
      ;; Calculate determinant for parallel line detection
      (assign $det (- (* $dy1 $dx2) (* $dx1 $dy2)))
      ;; Handle parallel lines case
      (if (< (abs $det) 1e-10)
        (values nil nil nil)
        ;; Solve for intersection parameters using Cramer's rule
        (do (assign $t (/ (- (* $dy3 $dx2) (* $dx3 $dy2)) $det))
            (assign $s (/ (- (* $dx1 $dy3) (* $dy1 $dx3)) $det))
            ;; Strict bounds: intersection must be in interior of both segments
            (if (and (> $t 0.0) (< $t 1.0)      ; beam interior
                     (> $s 0.0) (< $s 1.0))     ; obstacle interior  ; <-- CHANGED: no epsilon
              ;; Calculate and return intersection coordinates
              (do (assign $int-x (+ ?source-x (* $t $dx1)))
                  (assign $int-y (+ ?source-y (* $t $dy1)))
                  (values $t $int-x $int-y))
              ;; No valid intersection
              (values nil nil nil))))))


(define-query beam-beam-intersection
    (?source-x ?source-y ?end-x ?end-y ?cross-x1 ?cross-y1 ?cross-x2 ?cross-y2)
  ;; Determines if two beam segments intersect, with epsilon tolerance to handle
  ;; shared endpoints (beams from same connector) and chained beams (relay points).
  ;; Also handles collinear beams traveling toward each other (head-on collision).
  ;; Returns (values $t $int-x $int-y) or (values nil nil nil).
  (do (assign $dx1 (- ?end-x ?source-x))      ; Beam 1 direction x
      (assign $dy1 (- ?end-y ?source-y))      ; Beam 1 direction y
      (assign $dx2 (- ?cross-x2 ?cross-x1))   ; Beam 2 direction x
      (assign $dy2 (- ?cross-y2 ?cross-y1))   ; Beam 2 direction y
      (assign $dx3 (- ?cross-x1 ?source-x))   ; Displacement x
      (assign $dy3 (- ?cross-y1 ?source-y))   ; Displacement y
      ;; Calculate determinant for parallel line detection
      (assign $det (- (* $dy1 $dx2) (* $dx1 $dy2)))
      (assign $eps 2e-2)
      ;; Handle parallel/collinear lines case
      (if (< (abs $det) 1e-10)
        ;; Lines are parallel - check if collinear
        (do (assign $cross-disp (- (* $dx1 $dy3) (* $dy1 $dx3)))
            (if (>= (abs $cross-disp) 1e-10)
              ;; Parallel but not collinear - no intersection
              (values nil nil nil)
              ;; Collinear - check for overlap
              (do (assign $len-sq (+ (* $dx1 $dx1) (* $dy1 $dy1)))
                  (if (< $len-sq 1e-10)
                    ;; Degenerate beam1
                    (values nil nil nil)
                    ;; Project beam2 endpoints onto beam1's parameterization
                    (do (assign $t-start (/ (+ (* $dx3 $dx1) (* $dy3 $dy1)) $len-sq))
                        (assign $vx4 (- ?cross-x2 ?source-x))
                        (assign $vy4 (- ?cross-y2 ?source-y))
                        (assign $t-end (/ (+ (* $vx4 $dx1) (* $vy4 $dy1)) $len-sq))
                        ;; Determine overlap with beam1's valid range [0,1]
                        (assign $proj-min (min $t-start $t-end))
                        (assign $proj-max (max $t-start $t-end))
                        (assign $overlap-min (max 0.0 $proj-min))
                        (assign $overlap-max (min 1.0 $proj-max))
                        ;; Check if there's meaningful overlap
                        (if (<= $overlap-max $overlap-min)
                          ;; No overlap
                          (values nil nil nil)
                          ;; Have overlap - check if it's in interior of both beams
                          (do ;; Interior of beam1: (eps, 1-eps)
                              (assign $int-min (max $overlap-min $eps))
                              (assign $int-max (min $overlap-max (- 1.0 $eps)))
                              (if (>= $int-min $int-max)
                                ;; Overlap doesn't include interior
                                (values nil nil nil)
                                ;; Also check interior of beam2
                                (do (assign $t-range (- $t-end $t-start))
                                    (if (< (abs $t-range) 1e-10)
                                      ;; Degenerate beam2
                                      (values nil nil nil)
                                      ;; Find t range where s ∈ (eps, 1-eps)
                                      (do (if (> $t-range 0)
                                            (do (assign $s-valid-min (+ $t-start (* $eps $t-range)))
                                                (assign $s-valid-max (+ $t-start (* (- 1.0 $eps) $t-range))))
                                            (do (assign $s-valid-min (+ $t-start (* (- 1.0 $eps) $t-range)))
                                                (assign $s-valid-max (+ $t-start (* $eps $t-range)))))
                                          ;; Intersect with beam1's interior range
                                          (assign $final-min (max $int-min $s-valid-min))
                                          (assign $final-max (min $int-max $s-valid-max))
                                          (if (>= $final-min $final-max)
                                            ;; No valid overlap in both interiors
                                            (values nil nil nil)
                                            ;; Return midpoint of overlap using RATIONAL arithmetic
                                            ;; Use original rational bounds, not epsilon-adjusted floats
                                            (do (assign $rational-min (max 0 $proj-min))  ; <-- CHANGED: 0 not 0.0
                                                (assign $rational-max (min 1 $proj-max))  ; <-- CHANGED: 1 not 1.0
                                                (assign $t (/ (+ $rational-min $rational-max) 2))  ; <-- CHANGED: 2 not 2.0
                                                (assign $int-x (+ ?source-x (* $t $dx1)))
                                                (assign $int-y (+ ?source-y (* $t $dy1)))
                                                (values $t $int-x $int-y))))))))))))))  ; <-- ADDED: collinear handling
        ;; Non-parallel case: Solve for intersection parameters using Cramer's rule
        (do (assign $t (/ (- (* $dy3 $dx2) (* $dx3 $dy2)) $det))
            (assign $s (/ (- (* $dx1 $dy3) (* $dy1 $dx3)) $det))
            ;; Epsilon bounds: avoid false intersections at shared endpoints
            (if (and (> $t $eps) (< $t (- 1.0 $eps))      ; beam 1 interior
                     (> $s $eps) (< $s (- 1.0 $eps)))     ; beam 2 interior
              ;; Calculate and return intersection coordinates
              (do (assign $int-x (+ ?source-x (* $t $dx1)))
                  (assign $int-y (+ ?source-y (* $t $dy1)))
                  (values $t $int-x $int-y))
              ;; No valid intersection
              (values nil nil nil))))))


(define-query beam-segment-occlusion (?source-x ?source-y ?end-x ?end-y ?px ?py)
  ; Determines if an object in an area at (?px,?py) occludes a beam-segment with tolerance < 1.0
  ; Step 1: Calculate beam direction and length
  (do (assign $dx (- ?end-x ?source-x))
      (assign $dy (- ?end-y ?source-y))
      (assign $length-squared (+ (* $dx $dx) (* $dy $dy)))
      ;; Step 2: Handle degenerate case (zero-length beam)
      (if (< $length-squared 1e-10)
        ;; Check if point coincides with source/target
        (if (and (< (abs (- ?px ?source-x)) 1e-6)
                 (< (abs (- ?py ?source-y)) 1e-6))
          (values 0.0 ?source-x ?source-y)      ; Point at source
          (values nil nil nil))     ; Point not at degenerate beam
        ;; Step 3: Calculate projection parameter
        (do (assign $vx (- ?px ?source-x))   ; Vector from source to point
            (assign $vy (- ?py ?source-y))
            (assign $dot-product (+ (* $vx $dx) (* $vy $dy)))
            (assign $t (/ $dot-product $length-squared))
            ;; Step 4: Validate parameter within segment bounds
            (if (and (> $t 0.0) (< $t 1.0))
              ;; Step 5: Calculate closest point on beam to given point
              (do (assign $closest-x (+ ?source-x (* $t $dx)))
                  (assign $closest-y (+ ?source-y (* $t $dy)))
                  ;; Step 6: Calculate distance from point to beam
                  (assign $dist-x (- ?px $closest-x))
                  (assign $dist-y (- ?py $closest-y))
                  (assign $distance (sqrt (+ (* $dist-x $dist-x) 
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
    (assign $closest-t 1.0)  ; Default to target if no intersections
    (assign $result-x ?target-x)   
    (assign $result-y ?target-y)
    
    ;; Bind cached geometry lists once
    (bind (wall-segments $walls))
    (bind (gate-segments $gates))
    
    ;; Loop 1: Check walls (always block)
    (ww-loop for $entry in $walls do
      (assign $x1 (second $entry))
      (assign $y1 (third $entry))
      (assign $x2 (fourth $entry))
      (assign $y2 (fifth $entry))
      ;; Endpoint exclusion
      (if (not (or (and (= $x1 ?source-x) (= $y1 ?source-y))
                   (and (= $x2 ?source-x) (= $y2 ?source-y))
                   (and (= $x1 ?target-x) (= $y1 ?target-y))
                   (and (= $x2 ?target-x) (= $y2 ?target-y))))
        (do (mv-assign ($int-t $int-x $int-y)
              (beam-obstacle-intersection ?source-x ?source-y ?target-x ?target-y $x1 $y1 $x2 $y2))
            (if (and $int-t (< $int-t $closest-t))
              (do (assign $closest-t $int-t)
                  (assign $result-x $int-x)
                  (assign $result-y $int-y))))))
    
    ;; Loop 2: Check gates (only if closed)
    (ww-loop for $entry in $gates do
      (assign $gate (first $entry))
      (if (not (open $gate))
        (do (assign $x1 (second $entry))
            (assign $y1 (third $entry))
            (assign $x2 (fourth $entry))
            (assign $y2 (fifth $entry))
            ;; Endpoint exclusion
            (if (not (or (and (= $x1 ?source-x) (= $y1 ?source-y))
                         (and (= $x2 ?source-x) (= $y2 ?source-y))
                         (and (= $x1 ?target-x) (= $y1 ?target-y))
                         (and (= $x2 ?target-x) (= $y2 ?target-y))))
              (do (mv-assign ($int-t $int-x $int-y)
                    (beam-obstacle-intersection ?source-x ?source-y ?target-x ?target-y $x1 $y1 $x2 $y2))
                  (if (and $int-t (< $int-t $closest-t))
                    (do (assign $closest-t $int-t)
                        (assign $result-x $int-x)
                        (assign $result-y $int-y))))))))
    
    ;; Loop 3: Check cargo and agents (dynamic positions)
    (doall (?obj (either cargo agent))
      (if (and (bind (loc ?obj $area))
               (bind (coords $area $x1 $y1 $z1))
               ;; Endpoint exclusion
               (not (or (and (= $x1 ?source-x) (= $y1 ?source-y))
                        (and (= $x1 ?target-x) (= $y1 ?target-y)))))
        (do (mv-assign ($int-t $int-x $int-y)
              (beam-segment-occlusion ?source-x ?source-y ?target-x ?target-y $x1 $y1))
            (if (and $int-t (< $int-t $closest-t))
              (do (assign $closest-t $int-t)
                  (assign $result-x $int-x)
                  (assign $result-y $int-y))))))
    
    ;; Return closest intersection coordinates
    (values $result-x $result-y)))


(define-query collect-all-beam-intersections ()
  ; Use current endpoints (actual segments), not intended targets
  ; Returns list of intersection records: ((beam1 beam2 intersection-x intersection-y) ...)
  (do 
      ;; Build coordinate cache for all beam sources
      (assign $coord-cache (make-hash-table :test 'eq))
      (doall (?b (get-current-beams))
        (do (bind (beam-segment ?b $src $tgt $end-x $end-y))
            ;; Cache source coordinates if not already cached
            (if (not (gethash $src $coord-cache))
              (do (mv-assign ($x $y $z) (get-coordinates $src))
                  (setf (gethash $src $coord-cache) (list $x $y))))))
      ;; Detect intersections using cached coordinates.
      ;; Note: nested doalls are more efficient than (doall (combination (?b1 ?b2) ...)
      ;; for dynamic query types like (get-current-beams), because combination/standard
      ;; headers perform runtime product/filter/dedup on every call, whereas nested
      ;; doalls just iterate with a simple guard.
      (doall (?b1 (get-current-beams))
        (doall (?b2 (get-current-beams))
          (if (and (different ?b1 ?b2)
                   (string< (symbol-name ?b1) (symbol-name ?b2))) ; Avoid duplicate pairs
            (do (bind (beam-segment ?b1 $src1 $tgt1 $end1-x $end1-y))
                (bind (beam-segment ?b2 $src2 $tgt2 $end2-x $end2-y))
                ;; Skip counter-propagating pairs when:
                ;; 1. Both beams carry the same hue (no conflict), OR
                ;; 2. Either endpoint is a transmitter (transmitter beams are
                ;;    primary and must not be blocked by a relay's back-beam,
                ;;    which may carry a stale color during propagation).
                (assign $reverse-pair (and (eql $src1 $tgt2)
                                         (eql $tgt1 $src2)))
                (assign $src1-hue (get-hue-if-source $src1))
                (assign $src2-hue (get-hue-if-source $src2))
                (assign $skip-pair
                      (and $reverse-pair
                           (or (and $src1-hue $src2-hue (eql $src1-hue $src2-hue))
                               (transmitter $src1)
                               (transmitter $src2))))
                (if (not $skip-pair)
                  (do
                    (assign $src1-coords (gethash $src1 $coord-cache))
                    (assign $src1-x (first $src1-coords))
                    (assign $src1-y (second $src1-coords))
                    (assign $tgt1-x $end1-x)
                    (assign $tgt1-y $end1-y)
                    (assign $src2-coords (gethash $src2 $coord-cache))
                    (assign $src2-x (first $src2-coords))
                    (assign $src2-y (second $src2-coords))
                    (assign $tgt2-x $end2-x)
                    (assign $tgt2-y $end2-y)
                    ;; Check intersection between current segments (not intended paths)
                    (mv-assign ($t1 $int-x $int-y)
                      (beam-beam-intersection
                        $src1-x $src1-y $tgt1-x $tgt1-y
                        $src2-x $src2-y $tgt2-x $tgt2-y))
                    (if $t1
                      (push (list ?b1 ?b2 $int-x $int-y) $intersections))))))))
      $intersections))


(define-query beam-reaches-receiver (?receiver)
  ; Returns t if a color-matching beam reaches the receiver
  (do
    (mv-assign ($r-x $r-y) (get-fixed-coordinates ?receiver))
    (bind (chroma ?receiver $required-hue))
    (exists (?b (get-current-beams))
      (and (bind (beam-segment ?b $source $target $end-x $end-y))
           (eql $target ?receiver)
           (= $end-x $r-x)
           (= $end-y $r-y)
           ;; Get hue from source
           (or (bind (chroma $source $source-hue))
               (bind (color $source $source-hue)))
           (eql $source-hue $required-hue)))))


(define-query connector-has-beam-power (?connector ?hue)
  ; Returns t if a beam with matching hue reaches the connector at its current coordinates
  (do
    (mv-assign ($c-x $c-y $c-z) (get-coordinates ?connector))
    (exists (?b (get-current-beams))
      (and (bind (beam-segment ?b $source $target $end-x $end-y))
           (eql $target ?connector)  ; Beam must target this connector
           (= $end-x $c-x)            ; Beam must reach connector coordinates
           (= $end-y $c-y)
           ;; Verify source has matching hue
           (or (bind (chroma $source $source-hue))
               (bind (color $source $source-hue)))
           (eql $source-hue ?hue)))))


(define-query collect-transmitter-powered-relays ()
  ;; BFS from all transmitters, returning list of relays reachable via beam chains
  (do
    ;; Initialize frontier with all transmitters
    (assign $frontier nil)
    (doall (?t transmitter)
      (push ?t $frontier))
    ;; Initialize result set
    (assign $powered-relays nil)
    ;; BFS loop - continue while frontier has sources to process
    (ww-loop while $frontier do
      (assign $next-frontier nil)
      ;; Process each source in current frontier
      (ww-loop for $source in $frontier do
        ;; Check all beams originating from this source
        (doall (?b (get-current-beams))
          (do (bind (beam-segment ?b $src $tgt $end-x $end-y))
              (if (eql $src $source)
                ;; Beam originates from current source - check if target is a relay
                (if (relay $tgt)
                  ;; Verify beam actually reaches target coordinates
                  (do (mv-assign ($tgt-x $tgt-y $tgt-z) (get-coordinates $tgt))
                      (if (and (= $end-x $tgt-x)
                               (= $end-y $tgt-y)
                               (not (member $tgt $powered-relays)))
                        ;; Target is newly discovered powered relay
                        (do (push $tgt $powered-relays)
                            (push $tgt $next-frontier)))))))))
      ;; Advance to next frontier
      (assign $frontier $next-frontier))
    $powered-relays))


(define-query compute-relay-distances ()
  ;; BFS from all transmitters, returning hash table of relay -> min distance
  ;; Distance represents hop count from nearest transmitter
  (do
    ;; Initialize distance table - transmitters are at distance 0
    (assign $distances (make-hash-table :test 'eq))
    (assign $frontier nil)
    (doall (?t transmitter)
      (do (setf (gethash ?t $distances) 0)
          (push ?t $frontier)))
    ;; BFS loop
    (ww-loop while $frontier do
      (assign $next-frontier nil)
      (ww-loop for $source in $frontier do
        (assign $source-dist (gethash $source $distances))
        ;; Check all beams originating from this source
        (doall (?b (get-current-beams))
          (do (bind (beam-segment ?b $src $tgt $end-x $end-y))
              (if (eql $src $source)
                ;; Beam originates from current source - check if target is a relay
                (if (relay $tgt)
                  ;; Verify beam actually reaches target coordinates
                  (do (mv-assign ($tgt-x $tgt-y $tgt-z) (get-coordinates $tgt))
                      (if (and (= $end-x $tgt-x)
                               (= $end-y $tgt-y))
                        ;; Beam reaches target - record distance if not yet visited
                        (if (not (gethash $tgt $distances))
                          (do (setf (gethash $tgt $distances) (1+ $source-dist))
                              (push $tgt $next-frontier))))))))))
      (assign $frontier $next-frontier))
    $distances))


;;;; UPDATE FUNCTIONS ;;;;


(define-update propagate-changes! ()
  (ww-loop for $iteration from 1 to 5
           do (if (not (propagate-consequences!))
                (return t))
           finally (inconsistent-state) (return nil)))


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
                 (assign $held ?cargo)))
        (holds ?agent $held)
        (do (bind (holds ?agent $any-cargo))
            (not (holds ?agent $any-cargo)))))
    nil))


(define-update create-missing-beams! ()
  ;; Creates beams for all active sources paired with any terminus where no beam exists yet.
  ;; A connector emits toward every paired terminus including transmitters and its power source.
  ;; Returns t if any beams were created, nil otherwise.
  (do
    (if (not (bind (current-beams $any)))  ; initialize if not set in define-init
      (current-beams ()))
    (doall ((?src ?tgt) terminus)
      (if (or (paired ?src ?tgt) (paired ?tgt ?src))  ; Pairing exists (bidirectional)
        ;; Have a pairing - check if source can emit and beam should exist
        (do (assign $source-hue (get-hue-if-source ?src))
            (if (and $source-hue                              ; Source is active
                     (not (beam-exists-p ?src ?tgt)))         ; Beam doesn't exist yet
              ;; Create beam: source is transmitter (always active) or powered connector
              (do (create-beam-segment-p! ?src ?tgt)
                  (assign $created-any t))))))
    $created-any))


(define-update remove-orphaned-beams! ()
  ;; Removes beams whose pairing no longer exists or whose source lost power
  ;; Returns t if any beams were removed, nil otherwise
  (do
    (doall (?b (get-current-beams))
      (do (bind (beam-segment ?b $src $tgt $end-x $end-y))
          ;; Determine if beam should be removed
          (assign $should-remove nil)
          ;; Reason 1: Pairing no longer exists (check bidirectional)
          (if (not (or (paired $src $tgt) (paired $tgt $src)))
            (assign $should-remove t))
          ;; Reason 2: Source is a relay that lost power (no color binding)
          (if (and (relay $src)
                   (not (bind (color $src $hue))))
            (assign $should-remove t))
          ;; Execute removal if needed
          (if $should-remove
            (do (remove-beam-segment-p! ?b)
                (assign $removed-any t)))))
    $removed-any))


(define-update recalculate-all-beams! ()
  (do (doall (?b (get-current-beams))
        (do (bind (beam-segment ?b $source $target $old-end-x $old-end-y))
            ;; Get source coordinates
            (mv-assign ($source-x $source-y $source-z) (get-coordinates $source))
            ;; Get target coordinates  
            (mv-assign ($target-x $target-y $target-z) (get-coordinates $target))
            ;; Recalculate endpoint using current gate/beam states
            (mv-assign ($new-end-x $new-end-y)
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
    (assign $all-intersections (collect-all-beam-intersections))
    ;; Phase 2: Build per-beam geometry cache and obstacle-only t parameters
    (assign $obstacle-t (make-hash-table :test 'eq))
    (assign $effective-t (make-hash-table :test 'eq))
    (assign $beam-geometry (make-hash-table :test 'eq))  ;beam -> (src-x src-y dx dy)
    (doall (?b (get-current-beams))
      (do (bind (beam-segment ?b $src $tgt $old-end-x $old-end-y))
          (mv-assign ($src-x $src-y $src-z) (get-coordinates $src))
          (mv-assign ($tgt-x $tgt-y $tgt-z) (get-coordinates $tgt))
          (assign $dx (- $tgt-x $src-x))
          (assign $dy (- $tgt-y $src-y))
          (setf (gethash ?b $beam-geometry) (list $src-x $src-y $dx $dy))
          (assign $length-sq (+ (* $dx $dx) (* $dy $dy)))
          (if (< $length-sq 1e-20)
            (assign $t0 0)
            (do (assign $curr-dx (- $old-end-x $src-x))
                (assign $curr-dy (- $old-end-y $src-y))
                (assign $dot (+ (* $curr-dx $dx) (* $curr-dy $dy)))
                (assign $t0 (/ $dot $length-sq))))
          (setf (gethash ?b $obstacle-t) $t0)
          (setf (gethash ?b $effective-t) $t0)))
    ;; Phase 3: Iterative fixed-point resolution of beam-beam interference
    (assign $prev1-t (make-hash-table :test 'eq))  ;values from 1 iteration ago
    (assign $prev2-t (make-hash-table :test 'eq))  ;values from 2 iterations ago
    (ww-loop for $iteration from 1 to 10 do  ;bounded; converges in <= num-beams iterations
      (assign $changed nil)
      (doall (?b (get-current-beams))
        (do (assign $new-t (gethash ?b $obstacle-t))  ;start from obstacle upper bound
            (ww-loop for $intersection in $all-intersections do
              (assign $beam1 (first $intersection))
              (assign $beam2 (second $intersection))
              (assign $int-x (third $intersection))
              (assign $int-y (fourth $intersection))
              ;; Recompute full-path t parameters from intersection point so all
              ;; comparisons are in the same parameter space even when beams were
              ;; obstacle-truncated before interference resolution.
              (assign $geom1 (gethash $beam1 $beam-geometry))
              (assign $geom2 (gethash $beam2 $beam-geometry))
              (assign $src1-x (first $geom1))
              (assign $src1-y (second $geom1))
              (assign $dx1 (third $geom1))
              (assign $dy1 (fourth $geom1))
              (assign $src2-x (first $geom2))
              (assign $src2-y (second $geom2))
              (assign $dx2 (third $geom2))
              (assign $dy2 (fourth $geom2))
              (assign $len1-sq (+ (* $dx1 $dx1) (* $dy1 $dy1)))
              (assign $len2-sq (+ (* $dx2 $dx2) (* $dy2 $dy2)))
              (if (< $len1-sq 1e-20)
                (assign $t1-full nil)
                (do (assign $vx1 (- $int-x $src1-x))
                    (assign $vy1 (- $int-y $src1-y))
                    (assign $t1-full (/ (+ (* $vx1 $dx1) (* $vy1 $dy1)) $len1-sq))))
              (if (< $len2-sq 1e-20)
                (assign $t2-full nil)
                (do (assign $vx2 (- $int-x $src2-x))
                    (assign $vy2 (- $int-y $src2-y))
                    (assign $t2-full (/ (+ (* $vx2 $dx2) (* $vy2 $dy2)) $len2-sq))))
              (if (eql ?b $beam1)
                (do (assign $t-param $t1-full)
                    (assign $blocker $beam2)
                    (assign $blocker-t $t2-full))
                (if (eql ?b $beam2)
                  (do (assign $t-param $t2-full)
                      (assign $blocker $beam1)
                      (assign $blocker-t $t1-full))
                  (assign $t-param nil)))
              ;; Valid only if blocker's effective endpoint reaches the crossing
              (if (and $t-param
                       (>= (gethash $blocker $effective-t) $blocker-t)
                       (< $t-param $new-t))
                (assign $new-t $t-param)))
            (if (/= $new-t (gethash ?b $effective-t))
              (do (setf (gethash ?b $effective-t) $new-t)
                  (assign $changed t)))))
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
              (do (assign $alt-val (gethash ?b $prev1-t))
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
          (assign $geom (gethash ?b $beam-geometry))
          (assign $src-x (first $geom))
          (assign $src-y (second $geom))
          (assign $dx (third $geom))
          (assign $dy (fourth $geom))
          (assign $new-end-x (+ $src-x (* (gethash ?b $effective-t) $dx)))
          (assign $new-end-y (+ $src-y (* (gethash ?b $effective-t) $dy)))
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
            (assign $any-deactivated t))))
    $any-deactivated))


(define-update deactivate-unpowered-relays! ()
  ;; Returns t if any relay was deactivated, nil otherwise
  ;; Uses forward reachability from transmitters to detect true power
  (do
    ;; Get set of relays with valid transmitter power
    (assign $powered-relays (collect-transmitter-powered-relays))
    (doall (?r relay)
      (if (bind (color ?r $c-hue))
        ;; Relay is currently active - verify it has transmitter power
        (if (not (member ?r $powered-relays))
          ;; No path to transmitter - deactivate
          (do
            (not (color ?r $c-hue))
            (assign $deactivated-any t)))))
    $deactivated-any))


(define-update deactivate-conflicted-relays! ()
  ;; Deactivates relays receiving beams where a different color wins by distance,
  ;; or where there's a true conflict (equal minimum distances for different colors)
  ;; Returns t if any relay was deactivated, nil otherwise
  (do
    ;; Compute distance table once for all relays
    (assign $distances (compute-relay-distances))
    (doall (?r relay)
      (if (bind (color ?r $existing-hue))
        ;; Relay is active - check for conflicts using distance priority
        (do
          ;; Collect all (hue . distance) pairs from beams that reach this relay
          (assign $reaching-pairs nil)
          (doall (?src terminus)
            (if (or (paired ?r ?src) (paired ?src ?r))
              (do
                (assign $src-hue (get-hue-if-source ?src))
                (if (and $src-hue
                         (beam-reaches-target ?src ?r))
                  ;; Compute distance and collect pair
                  (do (assign $src-dist (gethash ?src $distances))
                      (if $src-dist
                        (push (cons $src-hue (1+ $src-dist)) $reaching-pairs)))))))
          ;; Resolve using distance priority
          (assign $winning-hue (resolve-hue-by-distance $reaching-pairs))
          ;; Deactivate if true conflict (nil) or winner differs from current
          (if (and $reaching-pairs
                   (or (null $winning-hue)
                       (not (eql $winning-hue $existing-hue))))
            (do (not (color ?r $existing-hue))
                (assign $deactivated-any t))))))
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
            (assign $any-activated t))))
    $any-activated))


(define-update activate-reachable-relays! ()
  ;; Returns t if any relay was activated, nil otherwise
  ;; Uses distance-aware resolution: shorter path to transmitter wins
  ;; For connectors: enforces single-active-connector-per-area constraint
  (do
    ;; Compute distance table once for all relays
    (assign $distances (compute-relay-distances))
    (doall (?r relay)
      (if (not (bind (color ?r $existing-hue)))
        (do
          ;; Collect all (hue . distance) pairs from beams that reach this relay
          (assign $reaching-pairs nil)
          ;; Check all paired termini that could be sources
          (doall (?src terminus)
            (if (or (paired ?r ?src) (paired ?src ?r))  ; Pairing exists (bidirectional)
              (do
                ;; Get source hue if it's an active source (transmitter or powered relay)
                (assign $src-hue (get-hue-if-source ?src))
                ;; If source has hue AND beam reaches relay, collect pair with distance
                (if (and $src-hue
                         (beam-reaches-target ?src ?r))
                  ;; Compute distance and collect pair
                  (do (assign $src-dist (gethash ?src $distances))
                      (if $src-dist
                        (push (cons $src-hue (1+ $src-dist)) $reaching-pairs)))))))
          ;; Resolve using distance priority instead of consensus
          (assign $winning-hue (resolve-hue-by-distance $reaching-pairs))
          ;; Only activate if there's a clear winner (not a tie)
          ;; For connectors: also verify no other active connector in same area
          (if (and $winning-hue
                   (or (repeater ?r)
                       (and (connector ?r)
                            (bind (loc ?r $relay-area))
                            (connectable ?r $relay-area))))
            (do (color ?r $winning-hue)
                (assign $activated-any t))))))
    $activated-any))


(define-update deactivate-receiver! (?receiver)
  (do (not (active ?receiver))
      (doall (?g gate)
        (if (controls ?receiver ?g)
          (not (open ?g))))))


(define-update disconnect-connector! (?cargo)
  ;; Clears all pairings when connector is picked up.
  (do (doall (?t terminus)
        (if (paired ?cargo ?t)
          (not (paired ?cargo ?t))))
      ;; With (paired connector terminus), also remove facts where ?cargo appears
      ;; as the terminus: (paired other-connector ?cargo).
      (doall (?c connector)
        (if (paired ?c ?cargo)
          (not (paired ?c ?cargo))))))


(define-update create-beam-segment-p! (?source ?target)
  ; Create a beam segment from source, and return the new beam's name.
  (do
    ;; Generate new beam entity with next available index
    (bind (current-beams $current-beams))
    (assign $next-index (1+ (length $current-beams)))
    (assign $new-beam (intern (format nil "BEAM~D" $next-index)))
    (register-dynamic-object $new-beam 'beam)
    ;; Calculate beam path and intersection
    (mv-assign ($source-x $source-y $source-z) (get-coordinates ?source))
    (mv-assign ($target-x $target-y $target-z) (get-coordinates ?target))
    (mv-assign ($end-x $end-y) (find-first-obstacle-intersection $source-x $source-y $target-x $target-y))
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


;;;; PAIR MACROS ;;;;

;;; Macros are listed before the primitives they fold, so the search
;;; tries the folded form first when both are applicable.


(define-action move-then-pickup
    2
  (?agent agent ?area2 area ?connector connector)
  (and (not (exists (?c cargo) (not (bind (loc ?c $any)))))  ;no cargo held (regressed through M; M preserves)
       (bind (loc ?agent $area1))
       (different $area1 ?area2)
       (accessible ?agent $area1 ?area2)
       (loc ?connector ?area2))  ;regressed P's colocation: connector already at post-M area
  (?agent ?connector $area1 ?area2)
  (assert (loc ?agent ?area2)
          (not (loc ?connector ?area2))
          (disconnect-connector! ?connector)))


(define-action move-then-connect-to-1-terminus
    2
  (?agent agent ?area2 area ?connector connector ?t1 terminus)
  (and (not (bind (loc ?connector $any)))  ;connector is held (M preserves)
       (different ?connector ?t1)
       (bind (loc ?agent $area1))
       (different $area1 ?area2)
       (accessible ?agent $area1 ?area2)
       (not (and (connector ?t1) (loc ?t1 ?area2)))  ;destination colocation (post-M)
       (selectable ?agent ?area2 ?t1))  ;selectable from destination (post-M)
  (?agent ?connector ?t1 $area1 ?area2)
  (assert (loc ?agent ?area2)
          (loc ?connector ?area2)
          (paired ?connector ?t1)))


#+ignore (define-action move-then-connect-to-2-terminus
    2
  (?agent agent ?area2 area ?connector connector (combination (?t1 ?t2) terminus))
  (and (not (bind (loc ?connector $any)))  ;connector is held (M preserves)
       (different ?connector ?t1)
       (different ?connector ?t2)
       (bind (loc ?agent $area1))
       (different $area1 ?area2)
       (accessible ?agent $area1 ?area2)
       (not (and (connector ?t1) (loc ?t1 ?area2)))  ;destination colocation (post-M)
       (not (and (connector ?t2) (loc ?t2 ?area2)))
       (selectable ?agent ?area2 ?t1)  ;selectable from destination (post-M)
       (selectable ?agent ?area2 ?t2))
  (?agent ?connector ?t1 ?t2 $area1 ?area2)
  (assert (loc ?agent ?area2)
          (loc ?connector ?area2)
          (paired ?connector ?t1)
          (paired ?connector ?t2)))


(define-action move-then-connect-to-3-terminus
    2
  (?agent agent ?area2 area ?connector connector (combination (?t1 ?t2 ?t3) terminus))
  (and (not (bind (loc ?connector $any)))  ;connector is held (M preserves)
       (different ?connector ?t1)
       (different ?connector ?t2)
       (different ?connector ?t3)
       (bind (loc ?agent $area1))
       (different $area1 ?area2)
       (accessible ?agent $area1 ?area2)
       (not (and (connector ?t1) (loc ?t1 ?area2)))  ;destination colocation (post-M)
       (not (and (connector ?t2) (loc ?t2 ?area2)))
       (not (and (connector ?t3) (loc ?t3 ?area2)))
       (selectable ?agent ?area2 ?t1)  ;selectable from destination (post-M)
       (selectable ?agent ?area2 ?t2)
       (selectable ?agent ?area2 ?t3))
  (?agent ?connector ?t1 ?t2 ?t3 $area1 ?area2)
  (assert (loc ?agent ?area2)
          (loc ?connector ?area2)
          (paired ?connector ?t1)
          (paired ?connector ?t2)
          (paired ?connector ?t3)))


;;;; PRIMITIVE ACTIONS ;;;;

;;; Primitive actions in relaxed discipline:
;;;   * "no cargo held" expressed as a base-relation test:
;;;       (not (exists (?c cargo) (not (bind (loc ?c $any)))))
;;;   * held connector identified by a connector parameter with no loc,
;;;     not by (bind (holds ?agent $connector))
;;;   * no (finally (propagate-changes!)) — propagation is deferred to goal
;;;   * `move` uses exact `accessible` (propagate-on-copy); all other actions
;;;     operate on base relations only.
;;;
;;; Pair macros will be inserted ABOVE the primitives, one pair at a time.
;;; Listing a macro before its constituent primitives lets the search prefer
;;; the folded form when both are applicable.


(define-action pickup-connector
    1
  (?agent agent ?connector connector)
  (and (not (exists (?c cargo) (not (bind (loc ?c $any)))))  ;no cargo held (base-relation equiv of holds)
       (bind (loc ?agent $area))
       (loc ?connector $area))
  (?agent ?connector $area)
  (assert (not (loc ?connector $area))
          (disconnect-connector! ?connector)))


(define-action connect-to-1-terminus
    1
  (?agent agent ?connector connector ?t1 terminus)
  (and (not (bind (loc ?connector $any)))
       (different ?connector ?t1)
       (bind (loc ?agent $area))
       (not (and (connector ?t1) (loc ?t1 $area)))
       (selectable ?agent $area ?t1))
  (?agent ?connector ?t1 $area $place)
  (do (assert (loc ?connector $area)
              (paired ?connector ?t1)
              (assign $place 'ground))))


(define-action connect-to-3-terminus
    1
  (?agent agent ?connector connector (combination (?t1 ?t2 ?t3) terminus))
  (and (not (bind (loc ?connector $any)))  ;base-relation equiv of (holds ?agent ?connector)
       (different ?connector ?t1)
       (different ?connector ?t2)
       (different ?connector ?t3)
       (bind (loc ?agent $area))
       (not (and (connector ?t1) (loc ?t1 $area)))
       (not (and (connector ?t2) (loc ?t2 $area)))
       (not (and (connector ?t3) (loc ?t3 $area)))
       (selectable ?agent $area ?t1)
       (selectable ?agent $area ?t2)
       (selectable ?agent $area ?t3))
  (?agent ?connector ?t1 ?t2 ?t3 $area $place)
  (do (assert (loc ?connector $area)
              (paired ?connector ?t1)
              (paired ?connector ?t2)
              (paired ?connector ?t3)
              (assign $place 'ground))))


#+ignore (define-action connect-to-2-terminus
    1
  (?agent agent ?connector connector (combination (?t1 ?t2) terminus))
  (and (not (bind (loc ?connector $any)))  ;base-relation equiv of (holds ?agent ?connector)
       (different ?connector ?t1)
       (different ?connector ?t2)
       (bind (loc ?agent $area))
       (not (and (connector ?t1) (loc ?t1 $area)))
       (not (and (connector ?t2) (loc ?t2 $area)))
       (selectable ?agent $area ?t1)
       (selectable ?agent $area ?t2))
  (?agent ?connector ?t1 ?t2 $area $place)
  (do (assert (loc ?connector $area)
              (paired ?connector ?t1)
              (paired ?connector ?t2)
              (assign $place 'ground))))


(define-action move
    1
  (?agent agent ?area2 area)
  (and (bind (loc ?agent $area1))
       (different $area1 ?area2)
       (accessible ?agent $area1 ?area2))
  (?agent $area1 ?area2)
  (assert (loc ?agent ?area2)))


;;;; INITIALIZATION ;;;;


(define-init
  ;; Dynamic state (base relations only)
  (loc agent1 area1)
  (loc connector1 area1)
  (loc connector2 area2)
  (loc connector3 area3)
  ;(elevation agent1 0)
  ;(elevation connector1 0)
  ;(elevation connector2 0)
  ;(elevation connector3 0)
  ;(current-beams ())  ; not a base relation, initialized in propagate-changes!

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
  (and ;; First check base relations
       (loc agent1 area4)
       (exists ((?c-blue ?c-red ?c-other) connector)
         (and (loc ?c-blue area2)
              (loc ?c-red area3)
              (not (bind (loc ?c-other $anywhere)))
              (paired ?c-blue transmitter2)
              (paired ?c-blue receiver3)
              (paired ?c-red transmitter1)
              (paired ?c-red receiver2)))
       ;; If base satisfied then propagate in place and check derived relations
       (propagate-changes!)
       (active receiver2)
       (active receiver3)))


(defparameter *known-goal-state*
  ;Verify this state is in (find-goal-states) with (known-goal-states-enumerated-p)
  '((ACTIVE RECEIVER2)
    (ACTIVE RECEIVER3)
    (BEAM-SEGMENT BEAM4 TRANSMITTER2 CONNECTOR1 9 8)
    (BEAM-SEGMENT BEAM5 TRANSMITTER1 CONNECTOR3 10 9)
    (BEAM-SEGMENT BEAM6 CONNECTOR3 RECEIVER2 7 109/10)
    (BEAM-SEGMENT BEAM7 CONNECTOR3 CONNECTOR1 19/2 17/2)
    (BEAM-SEGMENT BEAM8 CONNECTOR1 RECEIVER1 81/10 1)
    (BEAM-SEGMENT BEAM9 CONNECTOR1 RECEIVER3 1 109/10)
    (BEAM-SEGMENT BEAM10 CONNECTOR1 CONNECTOR3 19/2 17/2)
    (COLOR CONNECTOR3 RED)
    (COLOR CONNECTOR1 BLUE)
    (CURRENT-BEAMS (BEAM10 BEAM9 BEAM8 BEAM7 BEAM6 BEAM5 BEAM4 NIL NIL NIL))
    (HOLDS AGENT1 CONNECTOR2)
    (LOC AGENT1 AREA4)
    (LOC CONNECTOR1 AREA2)
    (LOC CONNECTOR3 AREA3)
    (PAIRED CONNECTOR1 RECEIVER3)
    (PAIRED CONNECTOR1 RECEIVER1)
    (PAIRED CONNECTOR1 TRANSMITTER2)
    (PAIRED CONNECTOR3 CONNECTOR1)
    (PAIRED CONNECTOR3 RECEIVER2)
    (PAIRED CONNECTOR3 TRANSMITTER1)))

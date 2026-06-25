;;; Filename: problem-claustro3.lisp

;;; Talos Principle problem 'Claustrophobia' in Escape from the Pit Reawakened.
;;; Full topological representation baseline; no coordinate calculations.
;;; Floating-point fluents disallowed (use $rational instead).
;;; List fluents are OK as long as they remain ordered for comparison with #'equal.
;;; Propagate-changes! handles derived relation updates; action rules handle base relation updates.
;;; Refactors claustro2 to use type-specific pickup and placement actions.
;;; Newest update: add (crossings-before-gate> beam-endpoint $list gate beam-endpoint) to static relations
;;;  where $list is any pre-gate crossings in all (crossings-along-beam> beam-endpoint $list beam-endpoint).

(in-package :ww)


(ww-set *problem-name* claustro3)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 33)

(defparameter *max-pairings* 2)  ;max termini a connector may pair in one connect; caps connect's branching to a domain's port count


(define-types
  agent (agent1)
  gate  (gate1 gate2 gate3 gate4 gate5 gate6 gate7 gate8 gate9)
  screen (screen1)
  location (location1 location2 location3 location4 location5 location6 location7 location8
            location9 location10 location11)
  plate (plate1 plate2 plate3)
  box (box1)
  jammer (jammer1 jammer2)
  connector ()  ;(connector1 connector2)
  transmitter (transmitter1)
  receiver (receiver1)
  ladder (ladder1)
  hue (blue)
  mode (normal inverted toggle)  ;controller mode
  cargo (either box jammer connector)
  relay (either connector)
  terminus (either transmitter receiver connector)  ;what a connector can pair/connect to
  support (either plate box)
  crossing ()  ;authored beam-crossing points; empty in the baseline
  beam-endpoint (either transmitter receiver location)  ;a fixture or a connector's location
)


(define-dynamic-relations
  (holding agent $cargo)
  (location (either agent box jammer connector) $location)
  (on (either agent box jammer connector) $support)  ;support an agent or movable object rests on (absent if ground)
  (depressed plate)
  (open gate)
  (active (either crossing receiver))
  (jamming jammer $gate)
  (paired connector terminus)  ;agent-established connection: a connector to a terminus
  (color connector $hue)  ;a connector's lit hue when a chroma-matching beam reaches it; absent if unlit
)


(define-static-relations
  (position (either plate ladder) $location)  ;static location
  (coupled transmitter receiver)  ;static pairing between a transmitter and receiver
  (controls $list gate $mode)  ;$list = DNF OR-list of AND-lists of controllers (receiver/plate); mode: normal | inverted
  (chroma (either transmitter receiver) $hue)
  (los-to-fixture location $list (either gate transmitter receiver))  ;per-location occluders on a sightline to a fixture
  (los-to-location location $list location)  ;symmetric per-pair sightline occluders; for connector-to-connector visibility
  (jam-disallowed> location location gate)  ;agent location, jammer placement, target gate; directional
  (walk-via location $list location)  ;symmetric walking edge; $list = guarding obstacles
  (traversable> location $list location)  ;one-way (> suppresses symmetry); eg ladders
  (reachable-via location $list location)  ;allows placing objects in one location from another location (eg, through a hole in a wall)
  (beam-via transmitter $list receiver)      ;beam corridor: gates to be open, locations to be unoccupied
  (beam-crossing> crossing $beam-endpoint $beam-endpoint $beam-endpoint $beam-endpoint)  ;a crossing (key) -> its two directed beams
  (crossings-along-beam> beam-endpoint $list beam-endpoint)  ;directed beam (from to) -> crossing ids nearest-source first
  (crossings-before-gate> beam-endpoint $list gate beam-endpoint)  ;directed beam (from to) + occluding gate -> crossings strictly source-side of that gate
)


;;;; QUERY FUNCTIONS ;;;;


(define-query accessible (?agent ?from)
  ;; Returns the set (list) of locations ?agent can reach from ?from via currently-passable
  ;; hops (see one-step-accessible), including ?from itself.  One of the three capability
  ;; queries (accessible / visible / reachable); a boolean "is X reachable" is recovered by
  ;; membership in the returned set, which is how move consumes it -- derived once per node,
  ;; then a successor branched per destination.  One-way edges (eg ladders) are not part of
  ;; accessibility; they are explicit fixture actions.  Breadth-first relaxation: each pass
  ;; expands only the frontier (nodes first reached on the prior pass), so every node is
  ;; expanded once; empties the frontier at the reachable-set boundary.
  (do (assign $visited (list ?from))
      (assign $frontier (list ?from))
      (ww-loop for $pass from 1 to 99
               do (assign $next-frontier nil)
                  (ww-loop for $loc in $frontier
                           do (doall (?next location)
                                (if (and (not (member ?next $visited))
                                         (one-step-accessible ?agent $loc ?next))
                                  (do (assign $visited (cons ?next $visited))
                                      (assign $next-frontier (cons ?next $next-frontier))))))
                  (assign $frontier $next-frontier)
                  (if (not $frontier)
                    (return t)))
      $visited))


(define-query one-step-accessible (?agent ?from ?to)
  ;; True iff a walking edge joins ?from and ?to and every guarding obstacle is
  ;; passable for ?agent.  Free-access regions are represented by sparse connected
  ;; edges; accessible computes their transitive closure.  One-way edges are explicit
  ;; fixture actions and are not routed here.
  (and (bind (walk-via ?from $obstacles ?to))
       (ww-loop for $o in $obstacles
                always (accessible-clear ?agent $o))))


(define-query one-way-clear (?agent ?means)
  ;; Every implement (means) enabling a one-way edge must be usable by ?agent.  Aggregates
  ;; the per-implement accessible-clear test over the edge's means list (eg (ladder1)), so
  ;; use-ladder can guard the hop without re-deriving passability inline.
  (ww-loop for $m in ?means
           always (accessible-clear ?agent $m)))


(define-query accessible-clear (?agent ?obstacle)
  ;; Per-kind passability for one obstacle on an edge. A problem includes only
  ;; the branches for obstacle kinds it declares; no null types required.
  (or (and (gate ?obstacle)
           (open ?obstacle))
      (and (screen ?obstacle)
           (not (bind (holding ?agent $any-held-object))))
      (and (ladder ?obstacle)
           (not (bind (holding ?agent $any-held-object))))))


(define-query visible (?location ?target)
  ;; Sightline must exist; clear only if every occluder is transparent. Empty occluder
  ;; list means a direct, always-clear sightline. Agent-independent: transparency depends
  ;; only on world state. ?target is a fixture (los-to-fixture) or another location
  ;; (los-to-location, symmetric); at most one matches, so try fixture then location.
  (and (or (bind (los-to-fixture ?location $occluders ?target))
           (bind (los-to-location ?location $occluders ?target)))
       (ww-loop for $o in $occluders
                always (visible-clear $o))))


(define-query visible-clear (?occluder)
  ;; Per-kind transparency for one occluder on a sightline. Claustro's sightlines
  ;; pass only through gates; the intervening-occupied-location branch is the
  ;; documented extension.
  (and (gate ?occluder)
       (open ?occluder)))


(define-query reachable (?location1 ?location2)
  ;; Two locations are within reach for placing/picking iff they are the same
  ;; location, or a reach edge joins them with every barrier gate open.
  ;; Agent-independent; reachable-via is symmetric because both endpoints are locations.
  (or (eql ?location1 ?location2)
      (and (bind (reachable-via ?location1 $barriers ?location2))
           (ww-loop for $b in $barriers
                    always (reachable-clear $b)))))


(define-query reachable-clear (?barrier)
  ;; A reach barrier is clear only when it is an open gate. Closed gates block, and
  ;; so does any non-gate barrier (eg a screen) -- no reaching through either.
  ;; Separate from visible-clear so sight and reach can diverge later.
  (and (gate ?barrier)
       (open ?barrier)))


(define-query energized (?controller)
  ;; Is this controller currently driving its output? Per-problem disjunction
  ;; over controller kinds: a receiver drives when active, a plate when depressed.
  (or (and (receiver ?controller)
           (active ?controller))
      (and (plate ?controller)
           (depressed ?controller))))


(define-query beam-reaches-receiver (?receiver)
  ;; A receiver lights when a chroma-matching beam clears its way in, by either route:
  ;;   direct -- the statically-coupled transmitter's beam-via corridor is clear (every
  ;;             gate open, every corridor location unoccupied); or
  ;;   relay  -- a connector paired to the receiver is lit (color) in the receiver's hue,
  ;;             with a clear sightline from the connector's location to the receiver.
  ;; The inbound hop that lights the connector is validated in update-connector-status!,
  ;; so the relay branch only checks the connector's outbound hop to the receiver here.
  (do (assign $reaches nil)
      (doall (?tr transmitter)
        (if (and (coupled ?tr ?receiver)
                 (bind (chroma ?tr $source-hue))
                 (bind (chroma ?receiver $required-hue))
                 (eql $source-hue $required-hue)
                 (bind (beam-via ?tr $obstacles ?receiver))
                 (ww-loop for $o in $obstacles
                          always (beam-clear $o))
                 (not (beam-cut ?tr ?receiver)))
          (assign $reaches t)))
      (doall (?c connector)
        (if (and (paired ?c ?receiver)
                 (bind (color ?c $c-hue))
                 (bind (chroma ?receiver $required-hue))
                 (eql $c-hue $required-hue)
                 (bind (location ?c $c-loc))
                 (visible $c-loc ?receiver)
                 (not (beam-cut $c-loc ?receiver)))
          (assign $reaches t)))
      $reaches))


(define-query beam-reaches-crossing (?from ?to ?xing ?active ?lighting)
  ;; The beam on this segment carries live light and is not already cut by a nearer crossing.
  ;; A connector-connector segment can carry the beam in either direction, so resolve the live
  ;; orientation first (source -> sink) and walk that orientation's crossings-along-beam> list, whose
  ;; ordering is nearest-source.  Fixed beams (T->L, L->R) are live only in their natural
  ;; orientation, so the reverse test fails harmlessly for them.  ?active is the frozen candidate
  ;; set for one resolution step.  A beam with no crossings reaches ?xing whenever live.
  ;; Option B: a closed occluding gate on the live orientation truncates the cut -- a crossing not
  ;; in that gate's crossings-before-gate> source-side list is behind the closed gate and is not
  ;; reached, matching the coordinate model and staying sound under inverted-mode gates.
  (do (assign $reaches nil)
      (assign $src nil)
      (assign $dst nil)
      (if (beam-live-for-cutting ?from ?to ?lighting)
        (do (assign $src ?from)
            (assign $dst ?to))
        (if (beam-live-for-cutting ?to ?from ?lighting)
          (do (assign $src ?to)
              (assign $dst ?from))))
      (if $src
        (do (assign $blocked nil)
            (assign $reached nil)
            (doall (?gate gate)
              (if (and (bind (crossings-before-gate> $src $before ?gate $dst))
                       (not (open ?gate))
                       (not (member ?xing $before)))
                (assign $blocked t)))
            (if (bind (crossings-along-beam> $src $ids $dst))
              (ww-loop for $e in $ids
                       do (if (eql $e ?xing)
                            (assign $reached t)
                            (if (and (not $reached)
                                     (member $e ?active))
                              (assign $blocked t)))))
            (if (not $blocked)
              (assign $reaches t))))
      $reaches))


(define-query compute-active-crossings (?active)
  ;; Compute one simultaneous crossing-resolution step from the frozen active set.  Lighting
  ;; is derived once from ?active (so relay-cutter liveness is consistent with the candidate
  ;; crossings); crossing-reaches applies the per-crossing both-beams test against it.  No
  ;; stored facts are read or written here.
  (do (assign $lighting (compute-connector-lighting ?active))
      (assign $next nil)
      (doall (?x crossing)
        (if (crossing-reaches ?x ?active $lighting)
          (assign $next (cons ?x $next))))
      $next))


(define-query arbitrate-crossings (?candidate)
  ;; Resolve a cascade-coupled candidate set to a single consistent crossing set by distance
  ;; priority.  Process the candidate crossings nearest-first (crossing-priority ascending,
  ;; lowest id on ties) and greedily keep each that still reaches given those already kept
  ;; and the lighting they induce.  A kept crossing's cut starves farther relays, so a later,
  ;; farther crossing whose feed is now cut drops out -- yielding the deterministic single-cut
  ;; fixpoint (eg {X1} for the symmetric bistable, lowest id winning the tie).  The caller
  ;; validates the result is a fixed point before committing.
  (do (assign $kept nil)
      (assign $remaining ?candidate)
      (ww-loop for $round from 1 to (length ?candidate)
               do (assign $lighting (compute-connector-lighting $kept))
                  (assign $best nil)
                  (assign $best-priority most-positive-fixnum)
                  (doall (?x crossing)
                    (if (and (member ?x $remaining)
                             (crossing-reaches ?x $kept $lighting))
                      (do (assign $priority (crossing-priority ?x $lighting))
                          (if (or (< $priority $best-priority)
                                  (and (= $priority $best-priority)
                                       (or (not $best)
                                           (string< (symbol-name ?x) (symbol-name $best)))))
                            (do (assign $best ?x)
                                (assign $best-priority $priority))))))
                  (if (not $best)
                    (return t)
                    (do (assign $kept (cons $best $kept))
                        (assign $remaining (remove $best $remaining)))))
      $kept))


(define-query crossing-reaches (?xing ?active ?lighting)
  ;; True iff both of ?xing's beams reach it under candidate set ?active and its induced
  ;; ?lighting -- the per-crossing firing test, factored so compute-active-crossings and the
  ;; arbitration sweep share one definition.
  (and (bind (beam-crossing> ?xing $f1 $t1 $f2 $t2))
       (beam-reaches-crossing $f1 $t1 ?xing ?active ?lighting)
       (beam-reaches-crossing $f2 $t2 ?xing ?active ?lighting)))


(define-query crossing-priority (?xing ?lighting)
  ;; A crossing's distance priority for arbitration: the larger of its two beams' source
  ;; distances (beam-source-distance).  For a relay-vs-feed crossing the feed contributes 0,
  ;; so this is the cutting relay's transmitter-hop count -- the quantity that breaks
  ;; asymmetric coupling, nearer relay winning.
  (do (bind (beam-crossing> ?xing $f1 $t1 $f2 $t2))
      (assign $d1 (beam-source-distance $f1 ?lighting))
      (assign $d2 (beam-source-distance $f2 ?lighting))
      (if (< $d1 $d2)
        $d2
        $d1)))


(define-query beam-source-distance (?from ?lighting)
  ;; Transmitter-hop distance of the beam emitted from ?from: 0 for a transmitter feed, else
  ;; the distance of the lit connector standing at location ?from (from the lighting records),
  ;; or most-positive-fixnum when no lit connector sources the beam.
  (do (assign $distance most-positive-fixnum)
      (if (transmitter ?from)
        (assign $distance 0)
        (doall (?c connector)
          (if (location ?c ?from)
            (do (assign $record (assoc ?c ?lighting))
                (if $record
                  (assign $distance (third $record)))))))
      $distance))


(define-query same-crossing-set (?left ?right)
  (and (= (length ?left) (length ?right))
       (ww-loop for $crossing in ?left
                always (member $crossing ?right))))


(define-query beam-live-for-cutting (?from ?to ?lighting)
  ;; True iff a beam currently traverses the directed segment (?from ?to), independent of color
  ;; -- a beam cuts a crosser regardless of hue.  Cutting turns on emission, not arrival: a beam
  ;; severs the crossings on its source side whether or not it reaches its target, so the
  ;; connector forms (B, C) do NOT test the outbound sightline (visible) here; arrival gates
  ;; lighting in compute-connector-lighting, not cutting.  Forms:
  ;;   A) transmitter -> receiver: the always-emitting transmitter's beam-via corridor is clear
  ;;      (gates open, corridor locations unoccupied);
  ;;   B) transmitter -> connector: a connector at ?to paired to the transmitter;
  ;;   C) connector -> receiver | connector: a lit connector at ?from paired toward whatever
  ;;      occupies ?to.
  ;; Dropping the B/C sightline test over-approximates only past a closed gate, which can merely
  ;; un-light a relay -- never open a normal-mode gate -- so it cannot admit an invalid solution
  ;; in a normal-mode problem.  Inverted/toggle gates are not yet covered here (future Option B).
  (or (and (transmitter ?from)
           (receiver ?to)
           (bind (beam-via ?from $obstacles ?to))
           (ww-loop for $o in $obstacles
                    always (beam-clear $o)))
      (and (transmitter ?from)
           (exists (?c connector)
             (and (location ?c ?to)
                  (paired ?c ?from))))
      (exists (?c connector)
        (and (location ?c ?from)
             (assoc ?c ?lighting)  ;lit in the candidate-set lighting snapshot, not committed color
             (or (and (receiver ?to)
                      (paired ?c ?to))
                 (exists (?c2 connector)
                   (and (location ?c2 ?to)
                        (different ?c2 ?c)
                        (or (paired ?c2 ?c) (paired ?c ?c2)))))))))  ;beam exists for either pairing direction


(define-query beam-cut (?from ?to)
  ;; True iff some crossing on this directed beam (?from ?to) is currently active, severing
  ;; it before its target.  Every authored crossing lies between source and target, so any
  ;; active one cuts the beam -- a flat membership scan (the producer already settled which
  ;; crossings fire, so no ordering is needed here).  No crossings-along-beam> entry means the beam
  ;; has no crossings and is never cut.  Reads the committed active crossing set, which
  ;; update-crossing-status! writes earlier in the same pass.
  (do (assign $cut nil)
      (if (bind (crossings-along-beam> ?from $ids ?to))
        (ww-loop for $e in $ids
                 do (if (active $e)
                      (assign $cut t))))
      $cut))


(define-query compute-connector-lighting (?active)
  ;; BFS from every transmitter over the pairing graph, returning the lit connectors as
  ;; ($connector $hue $distance) records: $hue the source hue carried to the connector,
  ;; $distance its transmitter-hop count (a transmitter-fed connector is 1).  A pairing
  ;; carries light only when the connector sees its source through a beam not cut by a
  ;; crossing in the frozen ?active candidate set (beam-cut-in?), so lighting is judged
  ;; against a candidate crossing set rather than the committed (active ...) facts.  Each
  ;; frontier record is ($source-obj $source-anchor $hue $distance): $source-obj is the
  ;; pairing terminus (a transmitter or a connector), while $source-anchor is its beam
  ;; endpoint (the transmitter itself, or a connector's location) used for the sightline and
  ;; cut tests.  The connect/pickup discipline keeps the pairing graph acyclic, so each
  ;; connector is resolved once, at its nearest transmitter-hop distance, since passes advance
  ;; by distance.  Every frontier source in a pass shares that distance, so the sources reaching
  ;; a connector in its pass are equidistant: gather their distinct hues and light it with the
  ;; sole hue, or -- on a genuine equidistant conflict of two or more hues -- leave it dark
  ;; (omitted from $lit), order-independently.  A conflicted connector is still marked visited
  ;; so a farther single hue cannot later claim it (nearest-distance-wins); a dark connector
  ;; never enters the next frontier, so it relays nothing.  At most one connector per location
  ;; enters $lit; this enforces the active-connector location limit without reading committed
  ;; color facts, so candidate crossing calculations remain self-contained.
  (do (assign $lit nil)
      (assign $lit-locations nil)
      (assign $visited nil)
      (assign $frontier nil)
      (doall (?tr transmitter)
        (if (bind (chroma ?tr $tr-hue))
          (assign $frontier (cons (list ?tr ?tr $tr-hue 0) $frontier))))
      (ww-loop for $pass from 1 to 99
               do (assign $next-frontier nil)
                  (doall (?c connector)
                    (if (and (not (member ?c $visited))
                             (bind (location ?c $c-loc)))
                      (do (assign $hues nil)
                          (assign $reach-hue nil)
                          (assign $reach-dist nil)
                          (ww-loop for $source-rec in $frontier
                                   do (assign $src-obj (first $source-rec))
                                      (assign $src-anchor (second $source-rec))
                                      (assign $src-hue (third $source-rec))
                                      (assign $src-dist (fourth $source-rec))
                                      (if (and (or (paired ?c $src-obj) (paired $src-obj ?c))
                                               (visible $c-loc $src-anchor)
                                               (not (beam-cut-in $src-anchor $c-loc ?active)))
                                        (do (if (not (member $src-hue $hues))
                                              (assign $hues (cons $src-hue $hues)))
                                            (assign $reach-hue $src-hue)
                                            (assign $reach-dist (1+ $src-dist)))))
                          (if $hues
                            (do (assign $visited (cons ?c $visited))
                                 (if (and (not (cdr $hues))
                                          (not (member $c-loc $lit-locations)))
                                   (do (assign $lit (cons (list ?c $reach-hue $reach-dist) $lit))
                                       (assign $lit-locations (cons $c-loc $lit-locations))
                                       (assign $next-frontier
                                               (cons (list ?c $c-loc $reach-hue $reach-dist) $next-frontier)))))))))
                  (assign $frontier $next-frontier)
                  (if (not $frontier)
                    (return t)))
      $lit))


(define-query beam-cut-in (?from ?to ?active)
  ;; Candidate-set analog of beam-cut?: true iff some crossing on the directed beam
  ;; (?from ?to) lies in the supplied ?active set, rather than in the committed (active ...)
  ;; facts.  The crossing producer uses this to judge connector lighting against a frozen
  ;; candidate crossing set during resolution; beam-cut remains the committed-state consumer.
  (do (assign $cut nil)
      (if (bind (crossings-along-beam> ?from $ids ?to))
        (ww-loop for $e in $ids
                 do (if (member $e ?active)
                      (assign $cut t))))
      $cut))


(define-query beam-clear (?obstacle)
  ;; A corridor obstacle is clear iff it is an open gate, or -- being a corridor
  ;; location -- has no occluding object standing on it.
  (if (gate ?obstacle)
    (open ?obstacle)
    (not (exists (?obj (either agent box jammer connector))
           (location ?obj ?obstacle)))))


(define-query clear-support-top (?support)
  ;; Nothing can step onto or remain supported by an occupied top.
  (not (exists (?x (either agent box jammer connector))
         (on ?x ?support))))
         

(define-query connectable-location (?connector ?location)
  ;; A connector can be connected at a location only if no other connector there is active.
  (not (exists (?other connector)
         (and (different ?other ?connector)
              (location ?other ?location)
              (bind (color ?other $hue))))))


(define-query connectable-terminus (?location ?connector ?terminus)
  ;; True iff a connector being placed at ?location may pair to ?terminus.  A transmitter
  ;; or receiver qualifies when visible from ?location (location-to-fixture sightline).
  ;; Another connector qualifies when it is placed (has a location), is not ?connector
  ;; itself, sits elsewhere, and its location is visible from ?location (location-to-
  ;; location sightline) -- visibility to a connector must route through its location,
  ;; since a connector is neither a fixture nor a location target.
  (or (and (or (transmitter ?terminus)
               (receiver ?terminus))
           (visible ?location ?terminus))
      (and (connector ?terminus)
           (different ?terminus ?connector)
           (bind (location ?terminus $t-loc))
           (different ?location $t-loc)
           (visible ?location $t-loc))))


;;;; UPDATE FUNCTIONS ;;;;


(define-update propagate-changes! ()
  ;; Binds the change-detection gate so add-prop/del-prop flag *propagated-state-changed*
  ;; on real derived-fact mutations during the fixpoint; the gate is off everywhere else,
  ;; leaving the search hot path unaffected.  Each pass runs to convergence (no change) or,
  ;; failing that, the cap declares the state inconsistent.
  (let ((*detect-propagated-changes* t))
    (ww-loop for $iteration from 1 to 5
             do (if (not (propagate-consequences!))
                  (return t))
             finally (inconsistent-state)
                     (return nil))))


(define-update propagate-consequences! ()
  ;; One propagation pass.  Binds the per-pass dirty flag to nil, runs the topological
  ;; cascade -- lit connectors -> active receivers / depressed plates -> open gates -- for
  ;; effect, then returns the flag: t iff some derivation actually changed stored state,
  ;; which tells propagate-changes! to run another pass.  Active crossings are computed
  ;; first, so both connector lighting and receiver activation can see which beams are cut
  ;; before deciding what is powered.  Connectors are lit next so a freshly-lit connector
  ;; can power its receiver within the same pass.  add-prop/del-prop
  ;; set the flag automatically, so the cascade functions no longer track changes themselves.
  (let ((*propagated-state-changed* nil))
    (update-crossing-status!)
    (update-connector-status!)
    (update-receiver-status!)
    (update-plate-status!)
    (update-gate-status!)
    *propagated-state-changed*))


(define-update update-crossing-status! ()
  ;; Recompute this pass's active crossing set.  A crossing fires -- cutting both its beams
  ;; at that point -- iff both beams reach it: each carries a live beam with no nearer
  ;; crossing already firing.  Each iteration computes every crossing from the same frozen
  ;; active set, so the result is independent of crossing visit order.
  ;;
  ;; A period-two cycle is first reconciled as the union of its alternating active sets (the
  ;; geometric minimum-endpoint reconciliation); if that union is itself a fixed point it
  ;; stands.  If it is not -- the cascade-coupled case, where firing a crossing un-lights a
  ;; connector and so un-cuts another -- arbitrate-crossings resolves it by distance priority
  ;; (nearer cutting relay wins, lowest id on ties), and that result must itself be a fixed
  ;; point.  Any remaining nonconvergence (higher-period, or a frustrated cycle with no
  ;; consistent fixed point) is inconsistent.  Stored crossing facts are changed only after a
  ;; fixed point has been established.
  (do (assign $active nil)
      (assign $previous nil)
      (assign $have-previous nil)
      (assign $resolved nil)
      (ww-loop for $iteration from 1 to 10
               do (assign $next (compute-active-crossings $active))
                  (if (same-crossing-set $next $active)
                    (do (assign $active $next)
                        (assign $resolved t)
                        (return t))
                    (if (and $have-previous
                             (same-crossing-set $next $previous))
                      (do (assign $candidate (union $active $next))
                          (assign $validated (compute-active-crossings $candidate))
                          (if (same-crossing-set $validated $candidate)
                            (do (assign $active $candidate)
                                (assign $resolved t))
                            (do (assign $arbitrated (arbitrate-crossings $candidate))
                                (assign $arb-validated (compute-active-crossings $arbitrated))
                                (if (same-crossing-set $arb-validated $arbitrated)
                                  (do (assign $active $arbitrated)
                                      (assign $resolved t))
                                  (inconsistent-state))))
                          (return nil))
                      (do (assign $previous $active)
                          (assign $have-previous t)
                          (assign $active $next))))
               finally (inconsistent-state))
      (if $resolved
        (doall (?x crossing)
          (if (member ?x $active)
            (active ?x)
            (not (active ?x)))))))


(define-update update-connector-status! ()
  ;; Commits each connector's lit hue (color) from the shared lighting model,
  ;; compute-connector-lighting, evaluated against the committed active-crossing set
  ;; (gathered here into $active, so beam-cut-in reproduces beam-cut exactly).  Routing
  ;; lighting through the one BFS the crossing producer also uses keeps the two from
  ;; drifting: the committer judges connectors against the committed crossing set, the
  ;; producer against a candidate set, by the same function.  The BFS resolves a whole relay
  ;; chain in one call, so a chain now lights fully in one pass rather than one hop per pass;
  ;; the connect/pickup discipline keeps the pairing graph acyclic, so no phantom
  ;; mutual-support fixpoint can form.  Hue is the source hue the BFS carries -- multi-source
  ;; hue arbitration is deferred (the live puzzle is monochromatic).  Sets or clears
  ;; (color ?c $hue); receiver activation is update-receiver-status!'s job.  Asserts
  ;; unconditionally -- change detection is automatic, so an unchanged re-assert is silent
  ;; and does not extend the fixpoint.
  (do (assign $active nil)
      (doall (?x crossing)
        (if (active ?x)
          (assign $active (cons ?x $active))))
      (assign $lighting (compute-connector-lighting $active))
      (doall (?c connector)
        (do (assign $record (assoc ?c $lighting))
            (if $record
              (color ?c (second $record))
              (if (bind (color ?c $old-hue))
                (not (color ?c $old-hue))))))))


(define-update update-receiver-status! ()
  ;; A receiver is active iff a chroma-matching beam reaches it (corridor clear).
  ;; Sets or clears (active ?r) to match; gate state is left to update-gate-status!.
  ;; Asserts the derived truth unconditionally -- change detection is automatic, so an
  ;; unchanged re-assert is silent and does not extend the fixpoint.
  (doall (?r receiver)
    (if (beam-reaches-receiver ?r)
      (active ?r)
      (not (active ?r)))))


(define-update update-plate-status! ()
  ;; A plate is depressed iff some agent or movable object is currently resting on it.
  ;; Sets or clears (depressed ?p) to match; gate state is left to
  ;; update-gate-status!.  Asserts the derived truth unconditionally -- change detection
  ;; is automatic, so an unchanged re-assert is silent and does not extend the fixpoint.
  (doall (?p plate)
    (if (exists (?x (either agent box jammer connector))
          (on ?x ?p))
      (depressed ?p)
      (not (depressed ?p)))))


(define-update update-gate-status! ()
  ;; Per-gate combine rule:  open  <=>  jammed  OR  control-on
  ;; Controllers are stored as $clauses in disjunctive normal form -- an OR-list of
  ;; AND-lists -- so control-on (normal mode) iff some clause has all its members
  ;; energized (a receiver is energized when active, a plate when depressed).  Inverted
  ;; mode negates that aggregate; jamming overrides the inverted force-close (jammed is
  ;; the leading disjunct).  Uncontrolled gates (no controls tuple) reduce to
  ;; open <=> jammed?, so jam-driven opening (eg gate1, gate5) is realized here.  Asserts
  ;; the derived open-state unconditionally -- change detection is automatic, so an
  ;; unchanged re-assert is silent.  Only normal and inverted modes are recognized.
  (doall (?gate gate)
    (do (assign $control-on nil)
        (if (bind (controls $clauses ?gate $mode))
          (do (assign $any-clause-on
                (ww-loop for $clause in $clauses
                         thereis (ww-loop for $c in $clause
                                          always (energized $c))))
              (if (eql $mode 'normal)
                (assign $control-on $any-clause-on)
                (if (eql $mode 'inverted)
                  (assign $control-on (not $any-clause-on))))))
        (if (or (exists (?j jammer)
                  (jamming ?j ?gate))
                $control-on)
          (open ?gate)
          (not (open ?gate))))))


;;;; ACTIONS ;;;;


(define-action pickup-box
  1
  (?agent agent ?box box)
  (and (not (bind (holding ?agent $any-held-object)))
       (bind (location ?agent $a-location))
       (bind (location ?box $box-location))
       (clear-support-top ?box)
       (reachable $box-location $a-location))
  (":" ?agent "picks up" ?box "at" $a-location)
  (assert (holding ?agent ?box)
          (not (location ?box $box-location))
          (if (bind (on ?box $support))
            (not (on ?box $support)))
          (finally (propagate-changes!))))


(define-action pickup-jammer
  1
  (?agent agent ?jammer jammer)
  (and (not (bind (holding ?agent $any-held-object)))
       (bind (location ?agent $a-location))
       (bind (location ?jammer $jammer-location))
       (reachable $jammer-location $a-location))
  (":" ?agent "picks up" ?jammer "at" $a-location)
  (assert (holding ?agent ?jammer)
          (not (location ?jammer $jammer-location))
          (if (bind (jamming ?jammer $any-target))
            (not (jamming ?jammer $any-target)))
          (if (bind (on ?jammer $support))
            (not (on ?jammer $support)))
          (finally (propagate-changes!))))


(define-action pickup-connector
  1
  (?agent agent ?connector connector)
  (and (not (bind (holding ?agent $any-held-object)))
       (bind (location ?agent $a-location))
       (bind (location ?connector $connector-location))
       (reachable $connector-location $a-location))
  (":" ?agent "picks up" ?connector "at" $a-location)
  (assert (holding ?agent ?connector)
          (not (location ?connector $connector-location))
          (do (doall (?t terminus)  ;outgoing: this connector's pairings to its termini
                (if (paired ?connector ?t)
                  (not (paired ?connector ?t))))
              (doall (?c connector)  ;incoming: other connectors paired to this one as terminus
                (if (paired ?c ?connector)
                  (not (paired ?c ?connector)))))
          (if (bind (on ?connector $support))
            (not (on ?connector $support)))
          (finally (propagate-changes!))))


(define-action jam-gate
  1
  (?agent agent ?gate gate ?location location)
  (and (bind (holding ?agent $any-jammer))
       (jammer $any-jammer)
       (bind (location ?agent $a-location))
       (reachable ?location $a-location)
       (visible ?location ?gate)
       (not (jam-disallowed> $a-location ?location ?gate)))
  (":" ?agent "jams" ?gate "with" $any-jammer "at" ?location "on" $place)
  (do ;; If a plate is at the drop location, one outcome rests the jammer on it (depressing it)
      (doall (?plate plate)
        (if (and (position ?plate ?location)
                 (clear-support-top ?plate))
          (assert (not (holding ?agent $any-jammer))
                  (jamming $any-jammer ?gate)
                  (location $any-jammer ?location)
                  (on $any-jammer ?plate)
                  (assign $place ?plate)
                  (finally (propagate-changes!)))))
      ;; If a box is at the drop location, one outcome rests the jammer on its clear top
      (doall (?box box)
        (if (and (location ?box ?location)
                 (clear-support-top ?box))
          (assert (not (holding ?agent $any-jammer))
                  (jamming $any-jammer ?gate)
                  (location $any-jammer ?location)
                  (on $any-jammer ?box)
                  (assign $place ?box)
                  (finally (propagate-changes!)))))
      ;; And always an outcome that rests the jammer on the ground
      (assert (not (holding ?agent $any-jammer))
              (jamming $any-jammer ?gate)
              (location $any-jammer ?location)
              (assign $place 'ground)
              (finally (propagate-changes!)))))


(define-action connect-connector
  ;; Place a held connector at a reachable location and, in one action, pair it to a chosen
  ;; non-empty subset of the termini connectable from that location -- the place-and-pair
  ;; analogue of jam-gate.  Each subset is a distinct successor; propagation then lights the
  ;; connector (and any relayed receiver).  Inert connector placement is not modeled, so only
  ;; non-empty subsets are emitted.  Subsets are generated in ascending cardinality up to
  ;; *max-pairings*, so a problem caps branching to its connectors' real port count.  Termini
  ;; range over the full terminus type.  Connectable-terminus admits a fixture by
  ;; location-to-fixture sightline and another placed
  ;; connector by location-to-location sightline, so a chain forms by pairing the placed
  ;; connector back to its already-placed upstream source.  Ground-only placement for now.
  1
  (?agent agent ?location location)
  (and (bind (holding ?agent $connector))
       (connector $connector)
       (bind (location ?agent $a-location))
       (reachable ?location $a-location)
       (connectable-location $connector ?location)
       (exists (?t terminus)
         (connectable-terminus ?location $connector ?t)))
  (":" ?agent "connects" $connector "at" ?location "to" $targets)
  (do (assign $connectable nil)
      (doall (?target terminus)
        (if (connectable-terminus ?location $connector ?target)
          (assign $connectable (cons ?target $connectable))))
      (ww-loop for $targets in (rest (subsets-up-to $connectable *max-pairings*))
               do (assert (not (holding ?agent $connector))
                          (location $connector ?location)
                          (ww-loop for $target in $targets
                                   do (paired $connector $target))
                          (finally (propagate-changes!))))))


(define-action put-box
  ;; Place a held box on the ground or on a clear support at a reachable location
  ;; (including the agent's own).
  1
  (?agent agent ?box box ?location location)
  (and (holding ?agent ?box)
       (bind (location ?agent $a-location))
       (reachable ?location $a-location))
  (":" ?agent "puts" ?box "at" ?location "on" $place)
  (do ;; If a plate is at the drop location, one outcome rests the box on it (depressing it)
      (doall (?plate plate)
        (if (and (position ?plate ?location)
                 (clear-support-top ?plate))
          (assert (not (holding ?agent ?box))
                  (location ?box ?location)
                  (on ?box ?plate)
                  (assign $place ?plate)
                  (finally (propagate-changes!)))))
      ;; If another box is at the drop location, one outcome rests the held box on its clear top
      (doall (?support-box box)
        (if (and (different ?support-box ?box)
                 (location ?support-box ?location)
                 (clear-support-top ?support-box))
          (assert (not (holding ?agent ?box))
                  (location ?box ?location)
                  (on ?box ?support-box)
                  (assign $place ?support-box)
                  (finally (propagate-changes!)))))
      ;; And always an outcome that rests the box on the ground
      (assert (not (holding ?agent ?box))
              (location ?box ?location)
              (assign $place 'ground)
              (finally (propagate-changes!)))))
         

(define-action move
  ;; Walk from ground at the current location to ground at any other accessible location.
  ;; The reachable set from the agent's current location is derived once in the precondition
  ;; (accessible), then the effect branches one successor per reachable destination, rather
  ;; than re-deriving accessibility separately for each candidate.  Support changes are
  ;; explicit separate actions: the agent must step off any plate before moving, so
  ;; accessibility is evaluated after the plate's effects are gone.
  1
  (?agent agent)
  (and (bind (location ?agent $a-location))
       (not (bind (on ?agent $anyplace)))
       (assign $reachable (accessible ?agent $a-location)))
  (":" ?agent "moves from" $a-location "to" $dest)
  (doall (?to-location location)
    (if (and (member ?to-location $reachable)
             (different $a-location ?to-location))
      (assert (location ?agent ?to-location)
              (assign $dest ?to-location)
              (finally (propagate-changes!))))))


#+ignore (define-action step-onto-plate
  ;; Step from ground onto a free reachable plate.  This is a support change; it may also
  ;; move the agent to the plate's location when the plate is reachable from nearby.
  1
  (?agent agent ?plate plate)
  (and (bind (location ?agent $a-location))
       (not (bind (on ?agent $anyplace)))
       (bind (position ?plate $plate-location))
       (reachable $a-location $plate-location)
       (clear-support-top ?plate))
  (":" ?agent "at" $a-location "steps on" ?plate "at" $plate-location)
  (assert (location ?agent $plate-location)
          (on ?agent ?plate)
          (finally (propagate-changes!))))


#+ignore (define-action step-off-plate
  ;; Step from a plate back onto the ground at the same location.
  1
  (?agent agent)
  (and (bind (on ?agent $plate))
       (plate $plate))
  (":" ?agent "steps off" $plate)
  (assert (not (on ?agent $plate))
          (finally (propagate-changes!))))


#+ignore (define-action jump-onto-box
  ;; Jump from ground onto a clear reachable box.  Elevation constraints belong here
  ;; when the problem gains explicit elevation state.
  1
  (?agent agent ?box box)
  (and (bind (location ?agent $a-location))
       (not (bind (on ?agent $anyplace)))
       (bind (location ?box $box-location))
       (reachable $a-location $box-location)
       (clear-support-top ?box))
  (":" ?agent "at" $a-location "jumps onto" ?box "at" $box-location)
  (assert (location ?agent $box-location)
          (on ?agent ?box)
          (finally (propagate-changes!))))


#+ignore (define-action jump-off-box
  ;; Jump from a box onto the ground at the same location.
  1
  (?agent agent ?box box)
  (on ?agent ?box)
  (":" ?agent "jumps off" ?box)
  (assert (not (on ?agent ?box))
          (finally (propagate-changes!))))


#+ignore (define-action use-ladder
  ;; Use a one-way ladder-like fixture from ground.  The agent lands on ground at the
  ;; traversal destination.  The one-way edge starts at the agent's current location;
  ;; the ladder fixture must be reachable from there.
  1
  (?agent agent ?ladder ladder)
  (and (bind (location ?agent $a-location))
       (not (bind (on ?agent $anyplace)))
       (bind (position ?ladder $ladder-location))
       (reachable $a-location $ladder-location)
       (bind (traversable> $a-location $means $dest))
       (member ?ladder $means)
       (one-way-clear ?agent $means))
  (":" ?agent "at" $a-location "uses" ?ladder "at" $ladder-location "to go to" $dest)
  (assert (location ?agent $dest)
          (finally (propagate-changes!))))


;;;; INITIALIZATION ;;;;


(define-init
  ;; Dynamic state (agent-manipulable or derived)
  (location agent1 location1)
  (location jammer1 location1)
  (location jammer2 location9)
  (location box1 location4)
  ;(location connector1 location1)
  ;(location connector2 location2)

  ;; Plates (fixed positions); box1 starts on plate1, so plate1 begins depressed
  (position plate1 location4)
  (position plate2 location5)
  (position plate3 location6)
  (on box1 plate1)

  ;; Ladder (fixed fixture, positioned like a plate); boards at location7, descends to location1
  ;(position ladder1 location7)

  ;; Static environment follows  
  ;; Gate controllers in DNF: ((c11 c12...) (c21...)) means (c11 AND c12...) OR (c21...)
  (controls ((receiver1)) gate2 normal)
  (controls ((receiver1)) gate3 normal)
  (controls ((receiver1)) gate4 inverted)
  (controls ((receiver1)) gate6 normal)
  (controls ((receiver1)) gate7 normal)
  (controls ((plate1 plate2 plate3)) gate8 normal)  ;all three plates depressed -> open
  (controls ((plate1 plate2 plate3)) gate9 normal)
  (chroma transmitter1 blue)
  (chroma receiver1 blue)
  (coupled transmitter1 receiver1)  ;fixed beam source -> target

  ;; Beam geometry
  (beam-via transmitter1 (gate1 location2) receiver1)  ;corridor: gate1 open, location2 unoccupied
  
  ;; Per-location line-of-sight to a fixture; $list = occluder gates that must be open
  (los-to-fixture location1 () gate1)
  (los-to-fixture location1 () gate2)
  (los-to-fixture location1 (gate2) gate3)
  (los-to-fixture location1 () gate4)
  (los-to-fixture location1 (gate2 gate3) gate5)
  (los-to-fixture location1 (gate1) transmitter1)
  (los-to-fixture location1 () receiver1)

  (los-to-fixture location2 () gate1)
  (los-to-fixture location2 () gate2)
  (los-to-fixture location2 (gate2) gate3)
  (los-to-fixture location2 (gate2 gate3) gate5)
  (los-to-fixture location2 () receiver1)
  (los-to-fixture location2 (gate1) transmitter1)

  (los-to-fixture location3 (gate3 gate2) gate1)
  (los-to-fixture location3 (gate3) gate2)
  (los-to-fixture location3 () gate3)
  (los-to-fixture location3 () gate4)
  (los-to-fixture location3 () gate5)
  (los-to-fixture location3 (gate5) gate6)
  (los-to-fixture location3 (gate5 gate6) gate7)
  (los-to-fixture location3 (gate5 gate6 gate7) gate8)
  (los-to-fixture location3 (gate5 gate6 gate7 gate8) gate9)
  (los-to-fixture location3 (gate2 gate3) receiver1)

  (los-to-fixture location4 (gate3 gate2) gate1)
  (los-to-fixture location4 (gate3) gate2)
  (los-to-fixture location4 () gate3)
  (los-to-fixture location4 () gate4)
  (los-to-fixture location4 () gate5)
  (los-to-fixture location4 (gate5) gate6)
  (los-to-fixture location4 (gate5 gate6) gate7)
  (los-to-fixture location4 (gate5 gate6 gate7) gate8)
  (los-to-fixture location4 (gate5 gate6 gate7 gate8) gate9)
  (los-to-fixture location4 (gate2 gate3) receiver1)

  (los-to-fixture location5 (gate3 gate2) gate1)
  (los-to-fixture location5 (gate3) gate2)
  (los-to-fixture location5 () gate3)
  (los-to-fixture location5 () gate4)
  (los-to-fixture location5 () gate5)
  (los-to-fixture location5 (gate5) gate6)
  (los-to-fixture location5 (gate5 gate6) gate7)
  (los-to-fixture location5 (gate5 gate6 gate7) gate8)
  (los-to-fixture location5 (gate5 gate6 gate7 gate8) gate9)

  (los-to-fixture location6 (gate3 gate2) gate1)
  (los-to-fixture location6 (gate3) gate2)
  (los-to-fixture location6 () gate3)
  (los-to-fixture location6 () gate4)
  (los-to-fixture location6 () gate5)
  (los-to-fixture location6 (gate5) gate6)
  (los-to-fixture location6 (gate5 gate6) gate7)
  (los-to-fixture location6 (gate5 gate6 gate7) gate8)
  (los-to-fixture location6 (gate5 gate6 gate7 gate8) gate9)

  (los-to-fixture location7 () gate1)
  (los-to-fixture location7 () gate2)
  (los-to-fixture location7 (gate2) gate3)
  (los-to-fixture location7 () gate4)
  (los-to-fixture location7 (gate2 gate3) gate5)
  (los-to-fixture location7 (gate1) transmitter1)

  (los-to-fixture location8 () gate4)
  (los-to-fixture location8 (gate4) gate5)

  (los-to-fixture location9 (gate5 gate3) gate2)
  (los-to-fixture location9 (gate5) gate3)
  (los-to-fixture location9 (gate5) gate4)
  (los-to-fixture location9 () gate5)
  (los-to-fixture location9 () gate6)
  (los-to-fixture location9 (gate6) gate7)
  (los-to-fixture location9 (gate6 gate7) gate8)
  (los-to-fixture location9 (gate6 gate7 gate8) gate9)

  (los-to-fixture location10 (gate7 gate6 gate5 gate3) gate2)
  (los-to-fixture location10 (gate7 gate6 gate5) gate3)
  (los-to-fixture location10 (gate7 gate6 gate5) gate4)
  (los-to-fixture location10 (gate7 gate6) gate5)
  (los-to-fixture location10 (gate7) gate6)
  (los-to-fixture location10 () gate7)
  (los-to-fixture location10 () gate8)
  (los-to-fixture location10 (gate8) gate9)

  (los-to-fixture location11 (gate9 gate8 gate7 gate6 gate5 gate3) gate2)
  (los-to-fixture location11 (gate9 gate8 gate7 gate6 gate5) gate3)
  (los-to-fixture location11 (gate9 gate8 gate7 gate6 gate5) gate4)
  (los-to-fixture location11 (gate9 gate8 gate7 gate6) gate5)
  (los-to-fixture location11 (gate9 gate8 gate7) gate6)
  (los-to-fixture location11 (gate9 gate8) gate7)
  (los-to-fixture location11 (gate9) gate8)
  (los-to-fixture location11 () gate9)

  ;; Per-location line-of-sight to a different location; $list = occluder gates that must be open
  (los-to-location location1 () location2)
  (los-to-location location1 (gate2 gate3) location3)
  (los-to-location location1 (gate2 gate3) location4)
  (los-to-location location1 (gate2 gate3) location5)
  (los-to-location location1 () location7)
  (los-to-location location1 () location8)

  (los-to-location location2 (gate2 gate3) location3)
  (los-to-location location2 (gate2 gate3) location4)
  (los-to-location location2 (gate2 gate3) location5)
  (los-to-location location2 (gate2 gate3) location6)
  (los-to-location location2 (gate2 gate3 gate5) location9)

  (los-to-location location3 () location4)
  (los-to-location location3 () location5)
  (los-to-location location3 () location6)
  (los-to-location location3 (gate5) location9)

  (los-to-location location4 () location5)
  (los-to-location location4 () location6)
  (los-to-location location4 (gate4) location8)
  (los-to-location location4 (gate5) location9)
  (los-to-location location4 (gate5 gate6 gate7) location10)
  (los-to-location location4 (gate5 gate6 gate7 gate8 gate9) location11)

  (los-to-location location5 () location6)
  (los-to-location location5 (gate4) location8)
  (los-to-location location5 (gate5) location9)
  (los-to-location location5 (gate5 gate6 gate7) location10)
  (los-to-location location5 (gate5 gate6 gate7 gate8 gate9) location11)

  (los-to-location location6 (gate4) location7)
  (los-to-location location6 (gate4) location8)
  (los-to-location location6 (gate5) location9)
  (los-to-location location6 (gate5 gate6 gate7) location10)
  (los-to-location location6 (gate5 gate6 gate7 gate8 gate9) location11)

  (los-to-location location7 () location8)

  (los-to-location location8 (gate4 gate5) location9)

  (los-to-location location9 (gate6 gate7) location10)
  (los-to-location location9 (gate6 gate7 gate8 gate9) location11)

  (los-to-location location10 (gate8 gate9) location11)

  ;; Directional jamming exclusions: agent location, jammer placement, target gate
  (jam-disallowed> location1 location7 gate1)
  (jam-disallowed> location7 location1 gate4)
  
  ;; Accessibility (move location to location).  Unobstructed edges form sparse
  ;; connected areas; guarded edges join those areas.  accessible computes the
  ;; complete reachable set, so these edges do not add walking actions to a plan.
  (walk-via location1 () location2)  ;area1
  (walk-via location3 () location4)  ;area2
  (walk-via location3 () location5)  ;area2
  (walk-via location3 () location6)  ;area2
  (walk-via location7 () location8)  ;area3

  (walk-via location2 (gate2 gate3) location3)  ;area1 -> area2

  (walk-via location3 (gate4 screen1) location7)  ;area2 -> area3
  (walk-via location3 (gate5) location9)  ;area2 -> area4

  (walk-via location9 (gate6 gate7) location10)  ;area4 -> area5

  (walk-via location10 (gate8 gate9) location11)  ;area5 -> area6

  ;; Alternate accessibility
  (traversable> location7 (ladder1) location1)   ;one-way via ladder

  ;; Reachability (put/pickup at a nearby location within reach); symmetric like accessible.
  ;; $list = barrier that must be open to reach across.
  (reachable-via location1 () location7)  ;wall opening between L1 and L7; reach only, not movement
  (reachable-via location2 (gate2 gate3) location3)
  (reachable-via location4 () location5)
  (reachable-via location5 () location6)
)


(define-init-action initialize-derived-state
  ;; Runs once during initialization (via do-init-action-updates), after define-init has
  ;; established the base and static facts.  Derives the entire derived layer (open, active)
  ;; via propagate-changes!, so the start state is consistent by construction and no derived
  ;; facts need hand-seeding (eg, (open gate4)).  propagate-changes! is called directly, not
  ;; as a (finally ...) followup: init-action application commits assert changes but does not
  ;; run deferred followups, so a direct call is required for the derivation to take effect.
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!)))


(define-goal
  ;; Claustrophobia planning goal: agent1 reaches location3.
  (location agent1 location11))

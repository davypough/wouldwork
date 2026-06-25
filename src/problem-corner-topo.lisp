;;; Filename: problem-corner-topo.lisp

;;; Talos Principle problem 'Around the Corner' (Purgatory workshop 3),
;;; re-expressed in fully topological relations (no coordinates).
;;; Topological vocabulary and machinery follow problem-claustro2.lisp;
;;; the puzzle's objects, hues, controls, and connectivity follow problem-corner.lisp.
;;; Beam reaching is decided by sightline relations and the crossing layer,
;;; not by coordinate geometry.  Only normal-mode gate control is used.


(in-package :ww)


(ww-set *problem-name* corner-topo)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 15)

(ww-set *symmetry-pruning* t)

(ww-set *progress-reporting-interval* 1000000)


(defparameter *max-pairings* 3)  ;max termini a connector may pair in one connect (corner's connect-to-3-terminus)


(define-types
  agent         (agent1)
  gate          (gate1)  ;only the dynamic gate (controlled by receiver1); corner's gate2/walls/window are static occlusion baked into the los facts
  location      (location1 location2 location3 location4)  ;corner area1..area4
  connector     (connector1 connector2 connector3)
  transmitter   (transmitter1 transmitter2)
  receiver      (receiver1 receiver2 receiver3)
  hue           (blue red)
  mode          (normal inverted toggle)  ;controller mode; corner uses only normal
  crossing      (crossing1 crossing2 crossing3 crossing4 crossing5 crossing6 crossing7 crossing8 crossing9 crossing10 crossing11 crossing12 crossing13 crossing14 crossing15 crossing16 crossing17 crossing18 crossing19 crossing20 crossing21 crossing22 crossing23 crossing24 crossing25 crossing26)  ;26 beam crossings (corner geometry); see define-init
  cargo         (either connector)  ;only connectors are carried
  relay         (either connector)
  terminus      (either transmitter receiver connector)  ;what a connector can pair/connect to
  beam-endpoint (either transmitter receiver location)  ;a fixture, or a connector's location
)


(define-dynamic-relations
  (holding agent $cargo)
  (location (either agent cargo) $location)
  (open gate)
  (active (either crossing receiver))
  (paired connector terminus)  ;agent-established connection: a connector to a terminus
  (color connector $hue)  ;a connector's lit hue when a chroma-matching beam reaches it; absent if unlit
)


(define-static-relations
  (controls $list gate $mode)  ;$list = DNF OR-list of AND-lists of controllers; mode: normal | inverted
  (chroma (either transmitter receiver) $hue)  ;fixed hue
  (los-to-fixture location $list (either transmitter receiver))  ;per-location occluder gates on a sightline to a fixture
  (los-to-location location $list location)  ;symmetric per-pair sightline occluder gates; for connector-to-connector visibility
  (walk-via location $list location)  ;directed walking edge; $list = guarding gates
  (beam-crossing> crossing $beam-endpoint $beam-endpoint $beam-endpoint $beam-endpoint)  ;a crossing -> its two directed beams (from1 to1 from2 to2)
  (crossings-along-beam> beam-endpoint $list beam-endpoint)  ;directed beam (from to) -> crossing ids nearest-source first
)


;;;; QUERY FUNCTIONS ;;;;


(define-query accessible (?from)
  ;; Breadth-first transitive reachable set from ?from over currently-passable walking edges
  ;; (one-step-accessible), including ?from.  move branches one successor per member.
  (do (assign $visited (list ?from))
      (assign $frontier (list ?from))
      (ww-loop for $pass from 1 to 99
               do (assign $next-frontier nil)
                  (ww-loop for $loc in $frontier
                           do (doall (?next location)
                                (if (and (not (member ?next $visited))
                                         (one-step-accessible $loc ?next))
                                  (do (assign $visited (cons ?next $visited))
                                      (assign $next-frontier (cons ?next $next-frontier))))))
                  (assign $frontier $next-frontier)
                  (if (not $frontier)
                    (return t)))
      $visited))


(define-query one-step-accessible (?from ?to)
  ;; A walking edge joins ?from and ?to with every guarding gate open.
  (and (bind (walk-via ?from $obstacles ?to))
       (ww-loop for $o in $obstacles
                always (open $o))))


(define-query visible (?location ?target)
  ;; Sightline must exist; clear only if every occluder is transparent. Empty occluder
  ;; list means a direct, always-clear sightline. Agent-independent: transparency depends
  ;; only on world state. ?target is a fixture (los-to-fixture) or another location
  ;; (los-to-location, symmetric); at most one matches, so try fixture then location.
  (and (or (bind (los-to-fixture ?location $occluders ?target))
           (bind (los-to-location ?location $occluders ?target)))
       (ww-loop for $o in $occluders
                always (visible-clear $o))))


(define-query beam-reaches-receiver (?receiver)
  ;; A receiver lights when a connector paired to it is lit in the receiver's hue, with a clear
  ;; sightline from the connector's location and no crossing cutting it.  (No direct transmitter
  ;; corridor in corner: every beam relays through a connector.)
  (do (assign $reaches nil)
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
  ;; One simultaneous crossing-resolution step from the frozen active set.  Lighting is derived
  ;; once from ?active; crossing-reaches applies the per-crossing both-beams test against it.
  (do (assign $lighting (compute-connector-lighting ?active))
      (assign $next nil)
      (doall (?x crossing)
        (if (crossing-reaches ?x ?active $lighting)
          (assign $next (cons ?x $next))))
      $next))


(define-query arbitrate-crossings (?candidate)
  ;; Resolve a cascade-coupled candidate set to a single consistent crossing set by distance
  ;; priority: process nearest-first (crossing-priority ascending, lowest id on ties) and greedily
  ;; keep each that still reaches given those already kept and the lighting they induce.  A kept
  ;; crossing's cut starves farther relays, so a later farther crossing whose feed is now cut
  ;; drops out -- the deterministic single-cut fixpoint.  The caller validates it is a fixed point.
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
  ;; Both of ?xing's beams reach it under candidate set ?active and induced ?lighting -- the
  ;; per-crossing firing test, shared by compute-active-crossings and the arbitration sweep.
  (and (bind (beam-crossing> ?xing $f1 $t1 $f2 $t2))
       (beam-reaches-crossing $f1 $t1 ?xing ?active ?lighting)
       (beam-reaches-crossing $f2 $t2 ?xing ?active ?lighting)))


(define-query crossing-priority (?xing ?lighting)
  ;; A crossing's distance priority for arbitration: the larger of its two beams' source
  ;; distances.  For a relay-vs-feed crossing the feed contributes 0, so this is the cutting
  ;; relay's transmitter-hop count -- nearer relay winning.
  (do (bind (beam-crossing> ?xing $f1 $t1 $f2 $t2))
      (assign $d1 (beam-source-distance $f1 ?lighting))
      (assign $d2 (beam-source-distance $f2 ?lighting))
      (if (< $d1 $d2)
        $d2
        $d1)))


(define-query beam-source-distance (?from ?lighting)
  ;; Transmitter-hop distance of the beam emitted from ?from: 0 for a transmitter, else the
  ;; distance of the lit connector at location ?from, or most-positive-fixnum when none.
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
  ;; A beam currently traverses the directed segment (?from ?to), regardless of color.  Cutting
  ;; turns on emission, not arrival: a beam severs the crossings on its source side whether or
  ;; not it ultimately reaches its target, so the outbound sightline is NOT tested here.  Arrival
  ;; (visible) gates lighting in compute-connector-lighting, not cutting.  Forms:
  ;;   B) transmitter -> connector: a connector at ?to paired to the transmitter;
  ;;   C) connector -> receiver | connector: a lit connector at ?from paired toward whatever
  ;;      occupies ?to.
  ;; (corner has no transmitter -> receiver direct beam, so that form is omitted.)  Dropping the
  ;; sightline test over-approximates only past a closed gate, which can merely un-light a relay
  ;; -- never open a gate or fire a goal receiver in a normal-mode problem -- so it cannot admit
  ;; an invalid solution.
  (or (and (transmitter ?from)
           (exists (?c connector)
             (and (location ?c ?to)
                  (paired ?c ?from))))
      (exists (?c connector)
        (and (location ?c ?from)
             (assoc ?c ?lighting)
             (or (and (receiver ?to)
                      (paired ?c ?to))
                 (exists (?c2 connector)
                   (and (location ?c2 ?to)
                        (different ?c2 ?c)
                        (or (paired ?c2 ?c) (paired ?c ?c2)))))))))  ;beam exists for either pairing direction


(define-query beam-cut (?from ?to)
  ;; Some crossing on this directed beam is currently active, severing it before its target.
  ;; Reads the committed active crossing set written earlier this pass.
  (do (assign $cut nil)
      (if (bind (crossings-along-beam> ?from $ids ?to))
        (ww-loop for $e in $ids
                 do (if (active $e)
                      (assign $cut t))))
      $cut))


(define-query compute-connector-lighting (?active)
  ;; BFS from every transmitter over the pairing graph, returning lit connectors as
  ;; ($connector $hue $distance): $hue the source hue carried to the connector, $distance its
  ;; transmitter-hop count (a transmitter-fed connector is 1).  A pairing carries light only
  ;; when the connector sees its source through a beam not cut by a crossing in the frozen
  ;; ?active candidate set (beam-cut-in?).  Passes advance by distance and the acyclic pairing
  ;; graph resolves each connector once at its nearest hop; equidistant sources of one hue light
  ;; it with that hue, a genuine equidistant multi-hue conflict leaves it dark (still marked
  ;; visited, so a farther single hue cannot later claim it).  At most one connector per location
  ;; enters $lit ($lit-locations guard) -- the active-connector-per-location limit corner relies
  ;; on, enforced here without reading committed color facts.
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
  ;; Candidate-set analog of beam-cut?: some crossing on the directed beam (?from ?to) lies in
  ;; the supplied ?active set.  Used to judge connector lighting against a candidate crossing set.
  (do (assign $cut nil)
      (if (bind (crossings-along-beam> ?from $ids ?to))
        (ww-loop for $e in $ids
                 do (if (member $e ?active)
                      (assign $cut t))))
      $cut))


(define-query connectable-location (?connector ?location)
  ;; A connector may be connected at a location only if no other connector there is lit.
  (not (exists (?other connector)
         (and (different ?other ?connector)
              (location ?other ?location)
              (bind (color ?other $hue))))))


(define-query connectable-terminus (?location ?connector ?terminus)
  ;; A connector placed at ?location may pair to ?terminus: a transmitter/receiver visible from
  ;; ?location (location-to-fixture sightline), or another placed connector elsewhere whose
  ;; location is visible from ?location (location-to-location sightline).
  (or (and (or (transmitter ?terminus)
               (receiver ?terminus))
           (visible ?location ?terminus))
      (and (connector ?terminus)
           (different ?terminus ?connector)
           (bind (location ?terminus $t-loc))
           (different ?location $t-loc)
           (visible ?location $t-loc))))


(define-query energized (?controller)
  ;; A controller drives its gate when it is an active receiver.  This MUST be a query call, not
  ;; an inline (active $c) in update-gate-status!: inside a define-update a bare relation form in
  ;; a ww-loop test clause is treated as an assertion, not a test, which would re-activate the
  ;; receiver every pass and oscillate against update-receiver-status!.
  (and (receiver ?controller)
       (active ?controller)))


;;;; UPDATE FUNCTIONS ;;;;


(define-update propagate-changes! ()
  ;; Bind the change-detection gate so add-prop/del-prop flag *propagated-state-changed* on real
  ;; derived-fact mutations during the fixpoint; the gate is off elsewhere, leaving the search
  ;; hot path unaffected.  Each pass runs to convergence (no change) or, failing that, the cap
  ;; declares the state inconsistent.
  (let ((*detect-propagated-changes* t))
    (ww-loop for $iteration from 1 to 5
             do (if (not (propagate-consequences!))
                  (return t))
             finally (inconsistent-state)
                     (return nil))))


(define-update propagate-consequences! ()
  ;; One propagation pass.  Bind the per-pass dirty flag to nil, run the topological cascade --
  ;; lit connectors -> active receivers -> open gates -- for effect, then return the flag: t iff
  ;; some derivation actually changed stored state, telling propagate-changes! to run again.
  ;; Active crossings are computed first so connector lighting and receiver activation both see
  ;; which beams are cut; connectors are lit next so a freshly-lit connector can power its
  ;; receiver within the same pass.  add-prop/del-prop set the flag automatically.
  (let ((*propagated-state-changed* nil))
    (update-crossing-status!)
    (update-connector-status!)
    (update-receiver-status!)
    (update-gate-status!)
    *propagated-state-changed*))


(define-update update-crossing-status! ()
  ;; Recompute this pass's active crossing set.  A crossing fires -- cutting both its beams at
  ;; that point -- iff both beams reach it: each carries a live beam with no nearer crossing
  ;; already firing.  Each iteration computes every crossing from the same frozen active set, so
  ;; the result is independent of crossing visit order.
  ;;
  ;; A period-two cycle is first reconciled as the union of its alternating active sets; if that
  ;; union is itself a fixed point it stands.  If not -- the cascade-coupled case, where firing a
  ;; crossing un-lights a connector and so un-cuts another -- arbitrate-crossings resolves it by
  ;; distance priority (nearer cutting relay wins, lowest id on ties), and that result must
  ;; itself be a fixed point.  Any remaining nonconvergence is inconsistent.  Stored crossing
  ;; facts change only after a fixed point is established.
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
  ;; Commit each connector's lit hue (color) from the shared lighting model,
  ;; compute-connector-lighting, evaluated against the committed active-crossing set (gathered
  ;; here into $active so beam-cut-in reproduces beam-cut exactly).  Routing lighting through
  ;; the one BFS the crossing producer also uses keeps committer and producer from drifting.  The
  ;; BFS resolves a whole relay chain in one call, and the connect/pickup discipline keeps the
  ;; pairing graph acyclic, so no phantom mutual-support fixpoint forms.  Sets or clears
  ;; (color ?c $hue); receiver activation is update-receiver-status!'s job.  Asserts
  ;; unconditionally -- change detection is automatic, so an unchanged re-assert is silent.
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
  ;; A receiver is active iff a chroma-matching beam reaches it.  Sets or clears (active ?r) to
  ;; match; gate state is left to update-gate-status!.  Asserts unconditionally -- change
  ;; detection is automatic, so an unchanged re-assert is silent.
  (doall (?r receiver)
    (if (beam-reaches-receiver ?r)
      (active ?r)
      (not (active ?r)))))


(define-update update-gate-status! ()
  ;; Per-gate combine rule:  open  <=>  control-on?
  ;; Controllers are stored as $clauses in disjunctive normal form -- an OR-list of AND-lists --
  ;; so control-on (normal mode) iff some clause has all its members active.  Inverted mode
  ;; negates that aggregate.  Uncontrolled gates (no controls tuple) stay closed.  Asserts the
  ;; derived open-state unconditionally.  Only normal and inverted modes are recognized; corner
  ;; uses only normal.
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
        (if $control-on
          (open ?gate)
          (not (open ?gate))))))


;;;; ACTIONS ;;;;


(define-action pickup-cargo
  ;; Pick up a connector from the agent's own location, clearing its pairings (outgoing to its
  ;; termini and incoming from connectors paired to it), then propagate the delighting.
  1
  (?agent agent ?cargo cargo)
  (and (not (bind (holding ?agent $any-cargo)))
       (bind (location ?agent $a-location))
       (bind (location ?cargo $c-location))
       (eql $c-location $a-location))
  (":" ?agent "picks up" ?cargo "at" $a-location)
  (assert (holding ?agent ?cargo)
          (not (location ?cargo $c-location))
          (doall (?t terminus)  ;outgoing: this connector's pairings to its termini
            (if (paired ?cargo ?t)
              (not (paired ?cargo ?t))))
          (doall (?c connector)  ;incoming: other connectors paired to this one as terminus
            (if (paired ?c ?cargo)
              (not (paired ?c ?cargo))))
          (finally (propagate-changes!))))


(define-action connect-connector
  ;; Place the held connector at the agent's location and, in one action, pair it to a chosen
  ;; non-empty subset of the termini connectable from there -- each subset a distinct successor.
  ;; Propagation then lights the connector and any relayed receiver.  connectable-location keeps
  ;; at most one lit connector per location; subsets are generated in ascending cardinality up to
  ;; *max-pairings* (3 for corner), capping branching to a connector's real port count.  A
  ;; terminus is connectable when visible from the location: a fixture directly, another placed
  ;; connector via its location, so a relay chain forms by pairing back to an upstream source.
  1
  (?agent agent)
  (and (bind (holding ?agent $connector))
       (bind (location ?agent $a-location))
       (connectable-location $connector $a-location)
       (exists (?t terminus)
         (connectable-terminus $a-location $connector ?t)))
  (":" ?agent "connects" $connector "at" $a-location "to" $targets)
  (do (assign $connectable nil)
      (doall (?target terminus)
        (if (connectable-terminus $a-location $connector ?target)
          (assign $connectable (cons ?target $connectable))))
      (ww-loop for $targets in (rest (subsets-up-to $connectable *max-pairings*))
               do (assert (not (holding ?agent $connector))
                          (location $connector $a-location)
                          (ww-loop for $target in $targets
                                   do (paired $connector $target))
                          (finally (propagate-changes!))))))


(define-action move
  ;; Walk from the current location to any other location in the transitive reachable set, derived
  ;; once (accessible) and branched one successor per destination.
  1
  (?agent agent)
  (and (bind (location ?agent $a-location))
       (assign $reachable (accessible $a-location)))
  (":" ?agent "moves from" $a-location "to" $dest)
  (doall (?to-location location)
    (if (and (member ?to-location $reachable)
             (different $a-location ?to-location))
      (assert (location ?agent ?to-location)
              (assign $dest ?to-location)
              (finally (propagate-changes!))))))


;;;; INITIALIZATION ;;;;


(define-init
  ;; Dynamic state
  (location agent1 location1)
  (location connector1 location1)
  (location connector2 location2)
  (location connector3 location3)

  ;; Gate control (DNF): receiver1 active -> gate1 open
  (controls ((receiver1)) gate1 normal)

  ;; Fixed hues
  (chroma transmitter1 red)
  (chroma transmitter2 blue)
  (chroma receiver1 red)
  (chroma receiver2 red)
  (chroma receiver3 blue)

  ;; Location -> fixture sightlines (occluder gates that must be open); from corner los0/los1
  (los-to-fixture location1 () transmitter1)
  (los-to-fixture location1 () transmitter2)
  (los-to-fixture location1 () receiver1)
  (los-to-fixture location2 () transmitter1)
  (los-to-fixture location2 () transmitter2)
  (los-to-fixture location2 () receiver1)
  (los-to-fixture location2 () receiver2)
  (los-to-fixture location2 () receiver3)
  (los-to-fixture location3 () transmitter1)
  (los-to-fixture location3 () transmitter2)
  (los-to-fixture location3 () receiver1)
  (los-to-fixture location3 () receiver2)
  (los-to-fixture location3 () receiver3)
  (los-to-fixture location4 (gate1) transmitter1)
  (los-to-fixture location4 (gate1) transmitter2)
  (los-to-fixture location4 () receiver2)
  (los-to-fixture location4 () receiver3)

  ;; Location -> location sightlines (symmetric matching); from corner visible0/visible1
  (los-to-location location1 () location2)
  (los-to-location location1 () location3)
  (los-to-location location1 (gate1) location4)
  (los-to-location location2 () location3)
  (los-to-location location2 () location4)
  (los-to-location location3 () location4)

  ;; Walking edges (symmetric matching); from corner accessible0/accessible1
  (walk-via location1 () location2)
  (walk-via location1 () location3)
  (walk-via location1 (gate1) location4)
  (walk-via location2 () location3)
  (walk-via location2 (gate1) location4)
  (walk-via location3 (gate1) location4)

  ;; Beam crossings: 26 crossings, one object per geometric point (corner geometry).  Connector-
  ;; connector (L->L) segments are bidirectional, so beam-crossing> names an L->L beam in a canonical
  ;; direction while crossings-along-beam> is authored for both directions; beam-reaches-crossing resolves
  ;; the live orientation.  beam-crossing> names the two beams meeting at a point; crossings-along-beam>
  ;; lists a directed beam's crossings nearest-source first.
  (beam-crossing> crossing1 transmitter1 location1 transmitter2 location2)
  (beam-crossing> crossing2 transmitter1 location1 transmitter2 location3)
  (beam-crossing> crossing3 transmitter1 location1 transmitter2 location4)
  (beam-crossing> crossing4 transmitter1 location2 transmitter2 location3)
  (beam-crossing> crossing5 transmitter1 location2 location3 receiver1)
  (beam-crossing> crossing6 transmitter1 location2 location1 location3)
  (beam-crossing> crossing7 transmitter2 location2 transmitter1 location4)
  (beam-crossing> crossing8 transmitter2 location2 location3 receiver1)
  (beam-crossing> crossing9 transmitter2 location2 location1 location3)
  (beam-crossing> crossing10 transmitter2 location3 transmitter1 location4)
  (beam-crossing> crossing11 transmitter1 location4 location2 receiver1)
  (beam-crossing> crossing12 transmitter1 location4 location3 receiver1)
  (beam-crossing> crossing13 transmitter1 location4 location1 location2)
  (beam-crossing> crossing14 transmitter1 location4 location1 location3)
  (beam-crossing> crossing15 transmitter2 location4 location2 receiver1)
  (beam-crossing> crossing16 transmitter2 location4 location3 receiver1)
  (beam-crossing> crossing17 transmitter2 location4 location1 location2)
  (beam-crossing> crossing18 transmitter2 location4 location1 location3)
  (beam-crossing> crossing19 location2 receiver1 location1 location4)
  (beam-crossing> crossing20 location2 receiver2 location3 receiver3)
  (beam-crossing> crossing21 location2 receiver2 location3 location4)
  (beam-crossing> crossing22 location2 receiver3 location4 receiver2)
  (beam-crossing> crossing23 location2 receiver3 location3 location4)
  (beam-crossing> crossing24 location3 receiver1 location1 location2)
  (beam-crossing> crossing25 location3 receiver1 location1 location4)
  (beam-crossing> crossing26 location3 receiver3 location4 receiver2)

  (crossings-along-beam> location1 (crossing17 crossing13 crossing24) location2)
  (crossings-along-beam> location1 (crossing18 crossing14 crossing9 crossing6) location3)
  (crossings-along-beam> location1 (crossing25 crossing19) location4)
  (crossings-along-beam> location2 (crossing24 crossing13 crossing17) location1)
  (crossings-along-beam> location2 (crossing11 crossing15 crossing19) receiver1)
  (crossings-along-beam> location2 (crossing21 crossing20) receiver2)
  (crossings-along-beam> location2 (crossing23 crossing22) receiver3)
  (crossings-along-beam> location3 (crossing6 crossing9 crossing14 crossing18) location1)
  (crossings-along-beam> location3 (crossing21 crossing23) location4)
  (crossings-along-beam> location3 (crossing5 crossing8 crossing24 crossing12 crossing16 crossing25) receiver1)
  (crossings-along-beam> location3 (crossing20 crossing26) receiver3)
  (crossings-along-beam> location4 (crossing19 crossing25) location1)
  (crossings-along-beam> location4 (crossing23 crossing21) location3)
  (crossings-along-beam> location4 (crossing22 crossing26) receiver2)
  (crossings-along-beam> transmitter1 (crossing2 crossing1 crossing3) location1)
  (crossings-along-beam> transmitter1 (crossing4 crossing6 crossing5) location2)
  (crossings-along-beam> transmitter1 (crossing10 crossing7 crossing14 crossing13 crossing12 crossing11) location4)
  (crossings-along-beam> transmitter2 (crossing1 crossing7 crossing9 crossing8) location2)
  (crossings-along-beam> transmitter2 (crossing2 crossing10 crossing4) location3)
  (crossings-along-beam> transmitter2 (crossing3 crossing18 crossing17 crossing16 crossing15) location4)
)


(define-init-action initialize-derived-state
  ;; Derive the full derived layer (open, active, color) once after define-init, so the start
  ;; state is consistent by construction.  Called directly (not as a finally followup).
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!)))


;;;; GOAL ;;;;


(define-goal
  (and (location agent1 location4)
       (active receiver2)
       (active receiver3)))

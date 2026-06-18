;;; Filename: problem-claustro1.lisp

;;; Talos Principle problem 'Claustrophobia' in Escape from the Pit Reawakened
;;; Full topological representation baseline; no coordinate calculations
;;; Floating-point fluents disallowed (use $rational instead)
;;; List fluents are OK as long as they remain ordered for comparison with #'equal
;;; Propagate-changes! handles derived relation updates; action rules handle base relation updates

(in-package :ww)


(ww-set *problem-name* claustro1)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 33)


(define-types
  agent (agent1)
  gate  (gate1 gate2 gate3 gate4 gate5 gate6 gate7 gate8 gate9)
  screen (screen1)
  location (location1 location2 location3 location4 location5 location6 location7 location8
            location9 location10 location11)
  area (area1 area2 area3 area4 area5 area6)
  los-group (los-group1 los-group2 los-group3 los-group4 los-group5 los-group6 los-group7)  ;each group has los to the same objects
  plate (plate1 plate2 plate3)
  box (box1)
  jammer (jammer1 jammer2)
  transmitter (transmitter1)
  receiver (receiver1)
  ladder (ladder1)
  hue (blue)
  mode (normal inverted toggle)  ;controller mode
  cargo (either box jammer)
)


(define-dynamic-relations
  (holding agent $cargo)
  (location (either agent cargo) $location)
  (on (either agent cargo) $plate)  ;the plate an agent or cargo rests on (absent if none)
  (depressed plate)
  (open gate)
  (active receiver)
  (jamming jammer $gate)
)


(define-static-relations
  (position (either plate ladder) $location)  ;static locations
  (paired transmitter receiver)  ;potential beam between a transmitter and receiver, static for this problem
  (controls $list gate $mode)  ;$list = DNF OR-list of AND-lists of controllers (receiver/plate); mode: normal | inverted
  (chroma (either transmitter receiver) $hue)
  (in-los-group los-group $list)  ;each sightline-equivalence group's clique of locations
  (los-via los-group $list gate)  ;occluders on an otherwise-clear sightline: () (gate2) (gate2 gate3) ...
  (in-area area $list)   ;each area's free-access clique of locations
  (area-of location $area)   ;reverse index of in-area: a location's containing area, built at init
  (interface area $list area)   ;inter-area interface; symmetric (same-type area args); $list = guarding obstacles
  (traversable> location $list location)  ;one-way (> suppresses symmetry); eg ladders
  (reachable-via location $list location)    ;reach edges (place/pick); $list = barrier gates that must be open
  (beam-via transmitter $list receiver)      ;beam corridor: gates to be open, locations to be unoccupied
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
  ;; True iff ?agent can move ?from -> ?to in a single intra-area or inter-area hop.
  ;; Resolves each endpoint's area via the area-of reverse index, then determines the
  ;; hop's guarding obstacle list -- none for an intra-area move, or the interface's
  ;; obstacles for an inter-area move -- and requires every obstacle passable for ?agent.
  ;; One-way edges are not routed here; they are explicit fixture actions.  No matching
  ;; hop means not directly accessible.
  (do (assign $hop nil)
      (assign $obstacles nil)
      (if (and (bind (area-of ?from $area-from))
               (bind (area-of ?to $area-to)))
        (if (eql $area-from $area-to)
          (assign $hop t)
          (if (bind (interface $area-from $interface-obstacles $area-to))
            (do (assign $hop t)
                (assign $obstacles $interface-obstacles)))))
      (assign $passable nil)
      (if $hop
        (assign $passable (ww-loop for $o in $obstacles
                                   always (accessible-clear ?agent $o))))
      $passable))


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
           (not (bind (holding ?agent $any-cargo))))
      (and (ladder ?obstacle)
           (not (bind (holding ?agent $any-cargo))))))


(define-query visible (?location ?target)
  ;; Sightline must exist; clear only if every occluder is transparent.
  ;; Empty occluder list means a direct, always-clear sightline.
  ;; Agent-independent: transparency here depends only on world state.
  ;; Sightline profiles are shared per los-group, so dereference the
  ;; location's group first, then look up the occluders for ?target.
  (do (assign $all-clear nil)
      (assign $group nil)
      (doall (?grp los-group)
        (if (bind (in-los-group ?grp $members))
          (if (member ?location $members)
            (assign $group ?grp))))
      (if (and $group
               (bind (los-via $group $occluders ?target)))
        (assign $all-clear (ww-loop for $o in $occluders
                                    always (visible-clear $o))))
      $all-clear))


(define-query visible-clear (?occluder)
  ;; Per-kind transparency for one occluder on a sightline. Claustro's sightlines
  ;; pass only through gates; the intervening-occupied-location branch is the
  ;; documented extension.
  (and (gate ?occluder)
       (open ?occluder)))


(define-query reachable (?location1 ?location2)
  ;; Two locations are within reach for placing/picking iff they are the same
  ;; location, or a reach edge joins them with every barrier gate open.
  ;; Agent-independent; reachable-via is symmetric (auto-symmetrized like interface).
  (do (assign $within-reach nil)
      (if (eql ?location1 ?location2)
        (assign $within-reach t)
        (if (bind (reachable-via ?location1 $barriers ?location2))
          (assign $within-reach (ww-loop for $b in $barriers
                                         always (reachable-clear $b)))))
      $within-reach))


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
  ;; A chroma-matching beam from the paired transmitter clears its corridor to
  ;; ?receiver: every corridor gate open, every corridor location unoccupied.
  (do (assign $reaches nil)
      (doall (?tr transmitter)
        (if (and (paired ?tr ?receiver)
                 (bind (chroma ?tr $source-hue))
                 (bind (chroma ?receiver $required-hue))
                 (eql $source-hue $required-hue)
                 (bind (beam-via ?tr $obstacles ?receiver)))
          (assign $reaches (ww-loop for $o in $obstacles
                                    always (beam-clear $o)))))
      $reaches))


(define-query beam-clear (?obstacle)
  ;; A corridor obstacle is clear iff it is an open gate, or -- being a corridor
  ;; location -- has no occluding object (agent or cargo) standing on it.
  (if (gate ?obstacle)
    (open ?obstacle)
    (not (exists (?obj (either agent cargo))
           (location ?obj ?obstacle)))))
         

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
  ;; cascade -- active receivers / depressed plates -> open gates -- for effect, then
  ;; returns the flag: t iff some derivation actually changed stored state, which tells
  ;; propagate-changes! to run another pass.  add-prop/del-prop set the flag automatically,
  ;; so the cascade functions no longer track changes themselves.
  (let ((*propagated-state-changed* nil))
    (update-receiver-status!)
    (update-plate-status!)
    (update-gate-status!)
    *propagated-state-changed*))


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
  ;; A plate is depressed iff some agent or cargo is currently resting on it.
  ;; Sets or clears (depressed ?p) to match; gate state is left to
  ;; update-gate-status!.  Asserts the derived truth unconditionally -- change detection
  ;; is automatic, so an unchanged re-assert is silent and does not extend the fixpoint.
  (doall (?p plate)
    (if (exists (?x (either agent cargo))
          (on ?x ?p))
      (depressed ?p)
      (not (depressed ?p)))))


(define-update update-gate-status! ()
  ;; Per-gate combine rule:  open  <=>  jammed?  OR  control-on?
  ;; Controllers are stored as $clauses in disjunctive normal form -- an OR-list of
  ;; AND-lists -- so control-on? (normal mode) iff some clause has all its members
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


(define-action pickup-cargo
  1
  (?agent agent ?cargo cargo)
  (and (not (bind (holding ?agent $any-cargo)))
       (bind (location ?agent $a-location))
       (bind (location ?cargo $c-location))
       (reachable $c-location $a-location))
  (":" ?agent "picks up" ?cargo "at" $a-location)
  (assert (holding ?agent ?cargo)
          (not (location ?cargo $c-location))
          (if (bind (jamming ?cargo $any-target))
            (not (jamming ?cargo $any-target)))
          (if (bind (on ?cargo $plate))
            (not (on ?cargo $plate)))
          (finally (propagate-changes!))))


(define-action jam-gate
  1
  (?agent agent ?gate gate ?location location)
  (and (bind (holding ?agent $any-jammer))
       (jammer $any-jammer)
       (bind (location ?agent $a-location))
       (reachable ?location $a-location)
       (visible ?location ?gate))
  (":" ?agent "jams" ?gate "with" $any-jammer "at" ?location "on" $place)
  (do ;; If a plate is at the drop location, one outcome rests the jammer on it (depressing it)
      (doall (?plate plate)
        (if (and (position ?plate ?location)
                 (not (exists (?c cargo)
                        (on ?c ?plate))))
          (assert (not (holding ?agent $any-jammer))
                  (jamming $any-jammer ?gate)
                  (location $any-jammer ?location)
                  (on $any-jammer ?plate)
                  (assign $place ?plate)
                  (finally (propagate-changes!)))))
      ;; And always an outcome that rests the jammer on the ground (no plate depressed)
      (assert (not (holding ?agent $any-jammer))
              (jamming $any-jammer ?gate)
              (location $any-jammer ?location)
              (assign $place 'ground)
              (finally (propagate-changes!)))))


(define-action put-cargo
  ;; Place held cargo on the ground, or on a plate, at a reachable location (including the agent's own).
  1
  (?agent agent ?location location)
  (and (bind (holding ?agent $cargo))
       (bind (location ?agent $a-location))
       (reachable ?location $a-location))
  (":" ?agent "puts" $cargo "at" ?location "on" $place)
  (do ;; If a plate is at the drop location, one outcome rests the cargo on it (depressing it)
      (doall (?plate plate)
        (if (and (position ?plate ?location)
                 (not (exists (?c cargo)
                        (on ?c ?plate))))
          (assert (not (holding ?agent $cargo))
                  (location $cargo ?location)
                  (on $cargo ?plate)
                  (assign $place ?plate)
                  (finally (propagate-changes!)))))
      ;; And always an outcome that rests the cargo on the ground (no plate depressed)
      (assert (not (holding ?agent $cargo))
              (location $cargo ?location)
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


(define-action step-onto-plate
  ;; Step from ground onto a free reachable plate.  This is a support change; it may also
  ;; move the agent to the plate's location when the plate is reachable from nearby.
  1
  (?agent agent ?plate plate)
  (and (bind (location ?agent $a-location))
       (not (bind (on ?agent $anyplace)))
       (bind (position ?plate $plate-location))
       (reachable $a-location $plate-location)
       (not (exists (?x (either agent cargo))
              (on ?x ?plate))))
  (":" ?agent "at" $a-location "steps on" ?plate "at" $plate-location)
  (assert (location ?agent $plate-location)
          (on ?agent ?plate)
          (finally (propagate-changes!))))


(define-action step-off-plate
  ;; Step from a plate back onto the ground at the same location.
  1
  (?agent agent)
  (bind (on ?agent $plate))
  (":" ?agent "steps off" $plate)
  (assert (not (on ?agent $plate))
          (finally (propagate-changes!))))


(define-action use-ladder
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

  ;; Plates (fixed positions); box1 starts on plate1, so plate1 begins depressed
  (position plate1 location4)
  (position plate2 location5)
  (position plate3 location6)
  (on box1 plate1)

  ;; Ladder (fixed fixture, positioned like a plate); boards at location7, descends to location1
  (position ladder1 location7)

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
  (paired transmitter1 receiver1)  ;fixed beam source -> target

  ;; Beam geometry
  (beam-via transmitter1 (gate1 location2) receiver1)  ;corridor: gate1 open, location2 unoccupied
  
  ;; Visibility. Each location's sightline-equivalence named group
  (in-los-group los-group1 (location1 location7))  ;locations can see the same targets
  (in-los-group los-group2 (location3 location4 location5 location6))
  (in-los-group los-group3 (location2))
  (in-los-group los-group4 (location8))
  (in-los-group los-group5 (location9))   ;area4
  (in-los-group los-group6 (location10))  ;area5
  (in-los-group los-group7 (location11))  ;area6

  ;; Line-of-sight per group (group to target gate); list = occluder gates that must be open
  (los-via los-group1 () gate1)
  (los-via los-group1 () gate2)
  (los-via los-group1 () gate4)
  (los-via los-group1 (gate2) gate3)
  (los-via los-group1 (gate2 gate3) gate5)

  (los-via los-group2 () gate3)
  (los-via los-group2 () gate4)
  (los-via los-group2 () gate5)
  (los-via los-group2 (gate3) gate2)
  (los-via los-group2 (gate3 gate2) gate1)
  (los-via los-group2 (gate5) gate6)             ;west down the hall, beyond gate5
  (los-via los-group2 (gate5 gate6) gate7)
  (los-via los-group2 (gate5 gate6 gate7) gate8)
  (los-via los-group2 (gate5 gate6 gate7 gate8) gate9)

  (los-via los-group3 () gate1)
  (los-via los-group3 () gate2)
  (los-via los-group3 (gate2) gate3)
  (los-via los-group3 (gate2 gate3) gate5)

  (los-via los-group4 () gate4)
  (los-via los-group4 (gate4) gate5)

  ;; Hall sightlines: straight west-east corridor  area2 -gate5- area4 -gate6/gate7- area5 -gate8- area6.
  ;; gate6 is the area4-side leaf of the gate6/gate7 pair, gate7 the area5-side leaf (cf gate2/gate3).
  (los-via los-group5 (gate5 gate3) gate2)
  (los-via los-group5 (gate5) gate3)
  (los-via los-group5 (gate5) gate4)
  (los-via los-group5 () gate5)            ;L9 (area4): gate5 adjacent east
  (los-via los-group5 () gate6)            ;gate6/gate7 adjacent west
  (los-via los-group5 (gate6) gate7)
  (los-via los-group5 (gate6 gate7) gate8) ;beyond the pair
  (los-via los-group5 (gate6 gate7 gate8) gate9)

  (los-via los-group6 (gate7 gate6 gate5 gate3) gate2)
  (los-via los-group6 (gate7 gate6 gate5) gate3)
  (los-via los-group6 (gate7 gate6 gate5) gate4)
  (los-via los-group6 (gate7 gate6) gate5) ;beyond the pair
  (los-via los-group6 (gate7) gate6)
  (los-via los-group6 () gate7)            ;gate6/gate7 adjacent east
  (los-via los-group6 () gate8)            ;L10 (area5): gate8 adjacent west
  (los-via los-group6 (gate8) gate9)

  (los-via los-group7 (gate9 gate8 gate7 gate6 gate5 gate3) gate2)
  (los-via los-group7 (gate9 gate8 gate7 gate6 gate5) gate3)
  (los-via los-group7 (gate9 gate8 gate7 gate6 gate5) gate4)
  (los-via los-group7 (gate9 gate8 gate7 gate6) gate5)
  (los-via los-group7 (gate9 gate8 gate7) gate6)
  (los-via los-group7 (gate9 gate8) gate7)       ;deeper gates seen through gate8
  (los-via los-group7 (gate9) gate8)            ;L11 (area6): gate8 adjacent east
  (los-via los-group7 () gate9)
  
  ;; Accessibility (move location to location).  Each location's area (areas are
  ;; free-access cliques); inter-area interfaces carry the guarding obstacles, which
  ;; the accessible query relaxes into full reachability.
  (in-area area1 (location1 location2))
  (in-area area2 (location3 location4 location5 location6))
  (in-area area3 (location7 location8))
  (in-area area4 (location9))
  (in-area area5 (location10))
  (in-area area6 (location11))

  (interface area1 (gate2 gate3) area2)
  (interface area2 (gate4 screen1) area3)
  (interface area2 (gate5) area4)
  (interface area4 (gate6 gate7) area5)
  (interface area5 (gate8 gate9) area6)
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


(define-init-action initialize-area-index
  ;; Builds the area-of reverse index (each location -> its containing area) once at
  ;; init from the in-area cliques, so one-step-accessible can resolve an endpoint's area
  ;; with a single bind instead of scanning every area.  area-of is static, so the asserted
  ;; tuples are written into the static database and persist for the whole run.  Depends only
  ;; on the in-area facts define-init has already established; order vs initialize-derived-state
  ;; is irrelevant since propagation never consults accessibility.
  0
  ()
  (always-true)
  ()
  (assert (doall (?area area)
            (if (bind (in-area ?area $members))
              (ww-loop for $location in $members
                       do (area-of $location ?area))))))


(define-goal
  (location agent1 location11))

;;; Filename: problem-claustro.lisp

;;; Talos Principle problem 'Claustrophobia' in Escape from the Pit Reawakened.


(in-package :ww)


(ww-set *problem-name* claustro)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 10)


(define-types
  agent (agent1)
  gate  (gate1 gate2 gate3 gate4 gate5)
  screen (screen1)
  location (location1 location2 location3 location4 location5
            location6 location7 location8)
  box (box1)
  jammer (jammer1)
  transmitter (transmitter1)
  receiver (receiver1)
  ladder (ladder1)
  hue (blue)
  mode (normal inverted toggle)
  cargo (either box jammer)
)


(define-dynamic-relations
  (holding agent $cargo)
  (location (either agent cargo) $location)
  (open gate)
  (active receiver)
  (jamming jammer $gate)
)


(define-static-relations
  (paired transmitter receiver)  ;potential beam between a transmitter and receiver, static for this problem
  (controls receiver gate $mode)  ;mode value: normal | inverted | toggle
  (chroma (either transmitter receiver) $hue)
  (los-via location gate $list)  ;occluders on an otherwise-clear sightline: () (gate2) (gate2 gate3) ...
  (accessible-via location location $list)   ;symmetric: same-type location args auto-symmetrized
  (accessible-via> location location $list)  ;one-way (> suppresses symmetry); eg ladders
  (reachable-via location location $list)    ;reach edges (place/pick); $list = barrier gates that must be open
  (beam-via transmitter receiver $list)      ;beam corridor: gates to be open, locations to be unoccupied
)


;;;; QUERY FUNCTIONS ;;;;


(define-query accessible (?agent ?location1 ?location2)
  ;; Edge must exist (symmetric accessible-via or one-way accessible-via>);
  ;; traversable only if every obstacle guarding the edge is passable for ?agent.
  ;; Empty obstacle list means directly accessible.
  (do (assign $all-passable nil)
      (if (or (bind (accessible-via ?location1 ?location2 $obstacles))
              (bind (accessible-via> ?location1 ?location2 $obstacles)))
        (do (assign $all-passable t)
            (ww-loop for $o in $obstacles
                     do (if (not (accessible-clear ?agent $o))
                          (assign $all-passable nil)))))
      $all-passable))


(define-query accessible-clear (?agent ?obstacle)
  ;; Per-kind passability for one obstacle on an edge. A problem includes only
  ;; the branches for obstacle kinds it declares; no null types required.
  (or (and (gate ?obstacle)
           (open ?obstacle))
      (and (screen ?obstacle)
           (not (bind (holding ?agent $any-cargo))))
      (and (ladder ?obstacle)
           (not (bind (holding ?agent $any-cargo))))))


(define-query los (?location ?target)
  ;; Sightline must exist; clear only if every occluder is transparent.
  ;; Empty occluder list means a direct, always-clear sightline.
  ;; Agent-independent: transparency here depends only on world state.
  (do (assign $all-clear nil)
      (if (bind (los-via ?location ?target $occluders))
        (do (assign $all-clear t)
            (ww-loop for $o in $occluders
                     do (if (not (los-clear $o))
                          (assign $all-clear nil)))))
      $all-clear))


(define-query los-clear (?occluder)
  ;; Per-kind transparency for one occluder on a sightline. Claustro's sightlines
  ;; pass only through gates; the intervening-occupied-location branch is the
  ;; documented extension.
  (and (gate ?occluder)
       (open ?occluder)))


(define-query reachable (?location1 ?location2)
  ;; Two locations are within reach for placing/picking iff they are the same
  ;; location, or a reach edge joins them with every barrier gate open.
  ;; Agent-independent; reachable-via is symmetric (auto-symmetrized like accessible-via).
  (do (assign $within-reach nil)
      (if (eql ?location1 ?location2)
        (assign $within-reach t)
        (if (bind (reachable-via ?location1 ?location2 $barriers))
          (do (assign $within-reach t)
              (ww-loop for $b in $barriers
                       do (if (not (reachable-clear $b))
                            (assign $within-reach nil))))))
      $within-reach))


(define-query reachable-clear (?barrier)
  ;; A reach barrier is clear only when it is an open gate. Closed gates block, and
  ;; so does any non-gate barrier (eg a screen) -- no reaching through either.
  ;; Separate from los-clear so sight and reach can diverge later.
  (and (gate ?barrier)
       (open ?barrier)))


(define-query energized (?controller)
  ;; Is this controller currently driving its output? Per-problem disjunction
  ;; over controller kinds; claustro has only receivers.
  (and (receiver ?controller)
       (active ?controller)))


(define-query beam-reaches-receiver (?receiver)
  ;; A chroma-matching beam from the paired transmitter clears its corridor to
  ;; ?receiver: every corridor gate open, every corridor location unoccupied.
  (do (assign $reaches nil)
      (doall (?tr transmitter)
        (if (and (paired ?tr ?receiver)
                 (bind (chroma ?tr $source-hue))
                 (bind (chroma ?receiver $required-hue))
                 (eql $source-hue $required-hue)
                 (bind (beam-via ?tr ?receiver $obstacles)))
          (do (assign $reaches t)
              (ww-loop for $o in $obstacles
                       do (if (not (beam-clear $o))
                            (assign $reaches nil))))))
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
  (ww-loop for $iteration from 1 to 5
           do (if (not (propagate-consequences!))
                (return t))
           finally (inconsistent-state) (return nil)))


(define-update propagate-consequences! ()
  ;; One propagation pass; returns t if any update changed state. Topological
  ;; cascade: receiver activation -> following control, iterated to fixpoint.
  (some #'identity
        (mapcar (lambda (fn) (funcall fn state))
                (list #'update-receiver-activation!
                      #'apply-following-control!))))


(define-update update-receiver-activation! ()
  ;; A receiver is active iff a chroma-matching beam reaches it (corridor clear).
  ;; Sets and clears (active ?rc) accordingly; gate state is left to
  ;; apply-following-control!.  Returns t iff any receiver's activation changed.
  (do (doall (?rc receiver)
        (if (beam-reaches-receiver ?rc)
          (if (not (active ?rc))
            (do (active ?rc)
                (assign $changed t)))
          (if (active ?rc)
            (do (not (active ?rc))
                (assign $changed t)))))
      $changed))


(define-update apply-following-control! ()
  ;; Per-gate combine rule:  open  <=>  jammed?  OR  control-on?
  ;; control-on? by mode: normal = energized controller; inverted = not energized.
  ;; Jamming overrides the inverted force-close (jammed is the leading disjunct).
  ;; Uncontrolled gates reduce to: open <=> jammed?.  Iterates every gate so that
  ;; jam-driven opening of an uncontrolled gate (eg gate1) is realized here.
  ;; Returns t iff any gate's open state changed.
  (do (doall (?gate gate)
        (do (assign $should-be-open nil)
            (if (exists (?j jammer) (jamming ?j ?gate))
              (assign $should-be-open t))
            (doall (?rc receiver)
              (if (bind (controls ?rc ?gate $mode))
                (cond ((eql $mode 'normal)
                       (if (energized ?rc)
                         (assign $should-be-open t)))
                      ((eql $mode 'inverted)
                       (if (not (energized ?rc))
                         (assign $should-be-open t)))
                      (t (error "Unsupported control mode ~A on gate ~A~%" $mode ?gate)))))
            (if $should-be-open
              (if (not (open ?gate))
                (do (open ?gate)
                    (assign $changed t)))
              (if (open ?gate)
                (do (not (open ?gate))
                    (assign $changed t))))))
      $changed))


;;;; ACTIONS ;;;;


(define-action pickup-cargo
  1
  (?agent agent ?cargo cargo)
  (and (not (bind (holding ?agent $any-cargo)))
       (bind (location ?agent $a-location))
       (bind (location ?cargo $c-location))
       (reachable $c-location $a-location))
  (?agent ?cargo $a-location)
  (assert (holding ?agent ?cargo)
          (not (location ?cargo $c-location))
          (if (bind (jamming ?cargo $any-target))
            (not (jamming ?cargo $any-target)))
          (finally (propagate-changes!))))


(define-action jam-gate
  1
  (?agent agent ?gate gate ?location location)
  (and (bind (holding ?agent $any-jammer))
       (jammer $any-jammer)
       (bind (location ?agent $a-location))
       (reachable ?location $a-location)
       (los ?location ?gate))
  (?agent $any-jammer ?gate ?location)
  (assert (jamming $any-jammer ?gate)
          (not (holding ?agent $any-jammer))
          (location $any-jammer ?location)
          (finally (propagate-changes!))))


(define-action put-cargo
  ;; Place held cargo on the ground at a reachable location (including the agent's own).
  1
  (?agent agent ?location location)
  (and (bind (holding ?agent $cargo))
       (bind (location ?agent $a-location))
       (reachable ?location $a-location))
  (?agent $cargo ?location)
  (assert (not (holding ?agent $cargo))
          (location $cargo ?location)
          (finally (propagate-changes!))))
         

(define-action move
  1
  (?agent agent ?location location)
  (and (bind (location ?agent $a-location))
       (different $a-location ?location)
       (accessible ?agent $a-location ?location))
  (?agent $a-location ?location)
  (assert (location ?agent ?location)
          (finally (propagate-changes!))))


;;;; INITIALIZATION ;;;;


(define-init
  ;; Dynamic state (agent-manipulable or derived)
  (location agent1 location1)
  (location jammer1 location1)
  (location box1 location4)
  (open gate4)

  ;; Static environment follows  
  (controls receiver1 gate2 normal)
  (controls receiver1 gate3 normal)
  (controls receiver1 gate4 inverted)
  (chroma transmitter1 blue)
  (chroma receiver1 blue)
  (paired transmitter1 receiver1)  ;fixed beam source -> target

  ;; Beam geometry
  (beam-via transmitter1 receiver1 (gate1 location2))  ;corridor: gate1 open, location2 unoccupied
  
  ;; Line-of-sight (source location to target gate); list = occluder gates that must be open
  (los-via location1 gate1 ())
  (los-via location1 gate2 ())
  (los-via location1 gate4 ())
  (los-via location1 gate3 (gate2))
  (los-via location1 gate5 (gate2 gate3))

  (los-via location2 gate1 ())
  (los-via location2 gate2 ())
  (los-via location2 gate3 (gate2))
  (los-via location2 gate5 (gate2 gate3))

  (los-via location3 gate3 ())
  (los-via location3 gate4 ())
  (los-via location3 gate5 ())
  (los-via location3 gate2 (gate3))
  (los-via location3 gate1 (gate3 gate2))

  (los-via location4 gate3 ())
  (los-via location4 gate4 ())
  (los-via location4 gate5 ())
  (los-via location4 gate2 (gate3))
  (los-via location4 gate1 (gate3 gate2))

  (los-via location5 gate3 ())
  (los-via location5 gate4 ())
  (los-via location5 gate5 ())
  (los-via location5 gate2 (gate3))
  (los-via location5 gate1 (gate3 gate2))

  (los-via location6 gate3 ())
  (los-via location6 gate4 ())
  (los-via location6 gate5 ())
  (los-via location6 gate2 (gate3))
  (los-via location6 gate1 (gate3 gate2))

  (los-via location7 gate1 ())
  (los-via location7 gate2 ())
  (los-via location7 gate4 ())
  (los-via location7 gate3 (gate2))
  (los-via location7 gate5 (gate2 gate3))

  (los-via location8 gate4 ())
  (los-via location8 gate5 (gate4))
  
  ;; Accessibility (move location to location); chain moves to lower search branching
  ;; symmetric edges asserted once (auto-symmetrized); ladder one-way via accessible-via>
  (accessible-via location1 location2 ())
  (accessible-via location1 location3 (gate2 gate3))
  (accessible-via location1 location4 (gate2 gate3))
  (accessible-via location1 location5 (gate2 gate3))
  (accessible-via location1 location6 (gate2 gate3))

  (accessible-via location2 location3 (gate2 gate3))
  (accessible-via location2 location4 (gate2 gate3))
  (accessible-via location2 location5 (gate2 gate3))
  (accessible-via location2 location6 (gate2 gate3))

  (accessible-via location3 location7 (gate4 screen1))
  (accessible-via location4 location7 (gate4 screen1))
  (accessible-via location5 location7 (gate4 screen1))
  (accessible-via location6 location7 (gate4 screen1))

  (accessible-via> location7 location1 (ladder1))   ;one-way via ladder
  (accessible-via location7 location8 ())

  ;; Reachability (place/pick at a nearby location); symmetric like accessible-via.
  ;; $list = barrier that must be open to reach across.
  (reachable-via location1 location7 ())  ;wall opening between L1 and L7; reach only, not movement
  (reachable-via location2 location3 (gate2 gate3))
  (reachable-via location4 location5 ())
  (reachable-via location5 location6 ())
)


(define-goal
  (open gate5))

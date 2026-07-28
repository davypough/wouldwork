;;; Filename: accessibility.lisp

;;; Accessibility background capability: walking locomotion.  accessible computes the full set
;;; of locations an agent can currently reach from a starting location over passable walking
;;; edges; move branches one successor per reachable destination.  One-way edges (ladders) are
;;; not part of accessibility -- they are explicit ladder actions (ladder).
;;;
;;; REQUIRES:
;;;   types     : agent, location  --  gate and screen come from nested -passability's own
;;;               nested -gate/-holding; ladder is declared optional by -passability directly
;;;   nested    : -support-occupancy (support-occupant, support, (on ...), cleartop);
;;;               -location (mobile-object, (has-location ...)); -passability
;;;               (open, obstacle-clear, all-clear, plus nested -gate's (open gate) relation
;;;               -- shared with gate, reachability, visibility, beam-direct, and
;;;               beam-crossing, which all nest -gate instead of hand-declaring it);
;;;               -elevation ((has-elevation ...), location-elevation)
;;;               -accessibility (identity-default accessible interface);
;;;               -accessibility-coordinates (optional coordinate-based WALK-VIA/WALK-VIA>
;;;               derivation from WALL/GATE/WINDOW/SCREEN-SEGMENTS, BOUNDARY-WALL, and
;;;               derived air-stream bands);
;;;               -threat (safe -- true unless an armed gun or other threat endangers the
;;;               location; gated inside one-step-accessible, not move, so an unsafe
;;;               location is excluded as a BFS through-node as well as a destination; a
;;;               problem with no threats pays nothing)
;;;               --  all shared via nested include-tech rather than local declaration
;;; PROVIDES:
;;;   relations : (walk-via location $list location), (walk-via> location $list location)
;;;               --  $list is a DNF clause list: () direct, else OR over clauses, AND within
;;;   queries   : accessible, one-step-accessible
;;;   action    : move

(include-tech -support-occupancy)
(include-tech -location)
(include-tech -passability)
(include-tech -elevation)
(include-tech -accessibility)
(include-tech -accessibility-coordinates)
(include-tech -threat)

(in-package :ww)


(define-static-relations
  (walk-via location $list location)  ;symmetric walking edge; $list = DNF door clauses: () direct, else OR over clauses, AND within, e.g. ((gate1) (gate2 gate3))
  (walk-via> location $list location))  ;directional walking edge, same $list convention; emitted by -accessibility-coordinates for rides into a stream's destination (inbound widened by side-curtain rides, outbound ordinary)


(define-action move
  ;; Walk from ground at the current location to ground at any other accessible location.  The
  ;; reachable set is derived once in the precondition (accessible), then the effect branches
  ;; one successor per destination.  The agent must not be on a support (step off first), so
  ;; accessibility is evaluated with that support's effects gone.  has-location is functional, so
  ;; asserting the new location replaces the old.
  1
  (?agent agent)
  (and (bind (has-location ?agent $a-location))
       (not (bind (on ?agent $anyplace)))
       (assign $reachable (accessible ?agent $a-location)))
  (">" ?agent "moves from" $a-location "to" $dest)
  (doall (?to-location location)
    (if (and (member ?to-location $reachable)
             (different $a-location ?to-location))
      (assert (has-location ?agent ?to-location)
              (assign $dest ?to-location)
              (finally (propagate-changes!))))))


(define-query accessible (?agent agent ?from location)
  ;; The set of locations navigable from ?from over currently-passable walking hops, including
  ;; ?from.  A boolean "is X accessible" is recovered by membership, which is how move consumes
  ;; it.  Breadth-first relaxation: each pass expands only the new frontier, so every node is
  ;; expanded once and the frontier empties at the accessible-set boundary.
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


(define-query one-step-accessible (?agent agent ?from location ?to location)
  ;; True iff a walking edge joins ?from and ?to -- symmetric WALK-VIA or directional
  ;; WALK-VIA> in this direction -- the two locations share the same elevation (walking
  ;; never changes elevation; jump does), the edge's DNF door clauses are satisfied:
  ;; () is direct, otherwise some clause must have every door passable for ?agent -- and
  ;; ?to is safe.  Gating safety here, not just in move's destination check, matters
  ;; because move is not a single hop: accessible's BFS uses this same predicate to expand
  ;; through every intermediate node of the closure, and move then offers any node in that
  ;; closure as a one-shot destination.  Checking safety only at move's final destination
  ;; would let an agent skip straight past an armed threat to a safe location beyond it,
  ;; never having to detour or disarm it; gating it here excludes an unsafe location as a
  ;; through-node as well as an endpoint, so it can neither be walked into nor walked past.
  (and (or (bind (walk-via ?from $clauses ?to))
           (bind (walk-via> ?from $clauses ?to)))
       (= (location-elevation ?from) (location-elevation ?to))
       (or (not $clauses)
           (ww-loop for $clause in $clauses
                    thereis (all-clear ?agent $clause)))
       (safe ?to)))

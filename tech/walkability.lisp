;;; Filename: walkability.lisp

;;; Walkability background capability: walking locomotion.  WALKABLE-LOCATIONS computes the
;;; full set of locations an agent can currently reach from a starting location over passable
;;; walking edges; WALK branches one successor per walkable destination.  WALKABLE is the
;;; Boolean membership predicate.  One-way climb edges are not part of walkability -- they are
;;; explicit ladder actions (ladder).
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
;;;               -walkability (WALK-VIA/WALK-VIA> topology relations and the
;;;               identity-default walkable-locations/walkable interface);
;;;               -walkability-coordinates (optional coordinate-based WALK-VIA/WALK-VIA>
;;;               derivation from WALL/GATE/WINDOW/SCREEN-SEGMENTS, BOUNDARY-WALL, and
;;;               derived air-stream bands);
;;;               -threat (safe -- true unless an armed gun or other threat endangers the
;;;               location; gated inside one-step-walkable, not walk, so an unsafe
;;;               location is excluded as a BFS through-node as well as a destination; a
;;;               problem with no threats pays nothing)
;;;               --  all shared via nested include-tech rather than local declaration
;;; PROVIDES:
;;;   relations : (walk-via location $list location), (walk-via> location $list location)
;;;               -- from nested -walkability; $list is a DNF clause list: () direct,
;;;               else OR over clauses, AND within
;;;   queries   : walkable-locations (overrides -walkability), walkable (from -walkability),
;;;               one-step-walkable
;;;   action    : walk

(include-tech -support-occupancy)
(include-tech -location)
(include-tech -passability)
(include-tech -elevation)
(include-tech -walkability)
(include-tech -walkability-coordinates)
(include-tech -threat)

(in-package :ww)


(define-action walk
  ;; Walk from ground at the current location to ground at any other walkable location.  The
  ;; walking closure is derived once in the precondition, then the effect branches one successor
  ;; per destination.  The agent must not be on a support (step off first), so walkability is
  ;; evaluated with that support's effects gone.  has-location is functional, so asserting the
  ;; new location replaces the old.
  1
  (?agent agent)
  (and (bind (has-location ?agent $a-location))
       (not (bind (on ?agent $anyplace)))
       (assign $walkable-locations (walkable-locations ?agent $a-location)))
  (">" ?agent "walks from" $a-location "to" $dest)
  (doall (?to-location location)
    (if (and (member ?to-location $walkable-locations)
             (different $a-location ?to-location))
      (assert (has-location ?agent ?to-location)
              (assign $dest ?to-location)
              (finally (propagate-changes!))))))


(define-query walkable-locations (?agent agent ?from location)
  ;; The set of locations walkable from ?from over currently-passable walking hops, including
  ;; ?from.  WALKABLE tests Boolean membership in this closure.  Breadth-first relaxation:
  ;; each pass expands only the new frontier, so every node is expanded once and the frontier
  ;; empties at the walkable-set boundary.
  (do (assign $visited (list ?from))
      (assign $frontier (list ?from))
      (ww-loop for $pass from 1 to 99
               do (assign $next-frontier nil)
                  (ww-loop for $loc in $frontier
                           do (doall (?next location)
                                (if (and (not (member ?next $visited))
                                         (one-step-walkable ?agent $loc ?next))
                                  (do (assign $visited (cons ?next $visited))
                                      (assign $next-frontier (cons ?next $next-frontier))))))
                  (assign $frontier $next-frontier)
                  (if (not $frontier)
                    (return t)))
      $visited))


(define-query one-step-walkable (?agent agent ?from location ?to location)
  ;; True iff a walking edge joins ?from and ?to -- symmetric WALK-VIA or directional
  ;; WALK-VIA> in this direction -- the two locations share the same elevation (walking
  ;; never changes elevation; jump does), the edge's DNF door clauses are satisfied:
  ;; () is direct, otherwise some clause must have every door passable for ?agent -- and
  ;; ?to is safe.  Gating safety here, not just in walk's destination check, matters
  ;; because walk is not a single hop: walkable-locations uses this same predicate to expand
  ;; through every intermediate node of the closure, and walk then offers any node in that
  ;; closure as a one-shot destination.  Checking safety only at walk's final destination
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

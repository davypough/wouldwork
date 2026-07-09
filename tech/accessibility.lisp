;;; Filename: accessibility.lisp

;;; Accessibility background capability: walking locomotion.  accessible computes the full set
;;; of locations an agent can currently reach from a starting location over passable walking
;;; edges; move branches one successor per reachable destination.  One-way edges (ladders) are
;;; not part of accessibility -- they are explicit ladder actions (ladder).
;;;
;;; REQUIRES:
;;;   types     : agent, location  --  gate, screen, and ladder are declared optional here
;;;               through nested -passability, coordinated with gate, visibility, reachability,
;;;               beam-direct, and beam-crossing, which all convert gate together since
;;;               they share the (open gate) relation verbatim
;;;   nested    : -support-occupancy (support-occupant, support, (on ...), cleartop);
;;;               -location (mobile-object, (has-location ...)); -passability
;;;               (open, obstacle-clear, all-clear); -elevation
;;;               ((has-elevation ...), location-elevation)
;;;               --  all shared via nested include-tech rather than local declaration
;;; PROVIDES:
;;;   relations : (walk-via location $list location)
;;;   queries   : accessible, one-step-accessible
;;;   action    : move

(include-tech -support-occupancy)
(include-tech -location)
(include-tech -passability)
(include-tech -elevation)

(in-package :ww)


(define-static-relations
  (walk-via location $list location))  ;symmetric walking edge; $list = guarding obstacles (gates/screens)


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
  (":" ?agent "moves from" $a-location "to" $dest)
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
  ;; True iff a walking edge joins ?from and ?to, the two locations share the same elevation
  ;; (walking never changes elevation; jump does), and every guarding obstacle is passable
  ;; for ?agent.  Free-access regions are sparse connected edges; accessible takes their closure.
  (and (bind (walk-via ?from $obstacles ?to))
       (= (location-elevation ?from) (location-elevation ?to))
       (all-clear ?agent $obstacles)))

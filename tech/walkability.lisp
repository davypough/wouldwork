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
;;;               -walk-recording-policy (neutral optional route-analysis hooks);
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
;;;               one-step-walkable, recording-walk-route-families
;;;   action    : walk

(include-tech -support-occupancy)
(include-tech -location)
(include-tech -passability)
(include-tech -elevation)
(include-tech -walkability)
(include-tech -walkability-coordinates)
(include-tech -threat)
(include-tech -walk-recording-policy)

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
       (if (walk-playback-validation-required ?agent)
         (do (assign $recording-route-families
               (recording-walk-route-families ?agent $a-location))
             (assign $walkable-locations
               (mapcar #'first $recording-route-families)))
         (do (assign $recording-route-families nil)
             (assign $walkable-locations
               (walkable-locations ?agent $a-location)))))
  (">" ?agent "walks from" $a-location "to" $dest)
  (doall (?to-location location)
    (if (and (member ?to-location $walkable-locations)
             (different $a-location ?to-location))
      (assert (has-location ?agent ?to-location)
              (if (walk-playback-validation-required ?agent)
                (record-walk-for-playback-validation!
                  ?agent $a-location ?to-location
                  (second (assoc ?to-location $recording-route-families))))
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


;;;; RECORDER-DEFERRED WALK ROUTES ;;;;


(define-query recording-walk-route-families (?agent agent ?from location)
  ;; Every location reachable when recorder-dependent obstacles are left unresolved.
  ;; Each alist value is the canonical family of minimal deferred-obstacle sets for
  ;; reaching that location.  Nondeferred doors, elevation, and safety remain immediate.
  (do (assign $edges nil)
      (doall (?edge-from location)
        (doall (?edge-to location)
          (if (different ?edge-from ?edge-to)
            (do (assign $edge-family
                  (recording-walk-edge-family ?agent ?edge-from ?edge-to))
                (if $edge-family
                  (push (list ?edge-from ?edge-to $edge-family) $edges))))))
      (walkability-route-family-alist ?from $edges)))


(define-query recording-walk-edge-family
    (?agent agent ?from location ?to location)
  ;; The currently viable non-fan alternatives across one directed topology edge.
  ;; Deferred obstacles are retained as requirements without consulting their current
  ;; playback state; all other members of a DNF clause must be clear now.
  (do (assign $family nil)
      (if (bind (walk-via ?from $symmetric-clauses ?to))
        (assign $family
          (walkability-family-union
            $family
            (recording-walk-clause-family ?agent $symmetric-clauses))))
      (if (bind (walk-via> ?from $directional-clauses ?to))
        (assign $family
          (walkability-family-union
            $family
            (recording-walk-clause-family ?agent $directional-clauses))))
      (if (and $family
               (= (location-elevation ?from) (location-elevation ?to))
               (safe ?to))
        $family
        nil)))


(define-query recording-walk-clause-family (?agent agent ?clauses)
  ;; WALK-VIA's NIL value is one unconditional alternative.  Otherwise retain every
  ;; DNF clause whose nondeferred members currently pass, projected down to the deferred
  ;; members that must be checked against the completed recording.
  (if (not ?clauses)
    (list nil)
    (do (assign $family nil)
        (ww-loop for $clause in ?clauses
                 do (assign $requirements nil)
                    (assign $nondeferred-clear t)
                    (ww-loop for $obstacle in $clause
                             do (if (deferred-walk-obstacle $obstacle)
                                  (push $obstacle $requirements)
                                  (if (not (obstacle-clear ?agent $obstacle))
                                    (assign $nondeferred-clear nil))))
                    (if $nondeferred-clear
                      (push $requirements $family)))
        (walkability-minimize-family $family))))


(defun walkability-route-family-alist (source edges)
  ;; Directed graph relaxation over antichains of deferred-obstacle sets.  One WALK
  ;; action may span several topology edges, so alternatives are combined to a fixpoint.
  (let ((families (make-hash-table :test #'eq)))
    (setf (gethash source families) (list nil))
    (loop for changed = nil
          do (dolist (edge edges)
               (destructuring-bind (from to edge-family) edge
                 (let ((from-family (gethash from families)))
                   (when from-family
                     (let* ((candidate
                              (walkability-family-product from-family edge-family))
                            (merged
                              (walkability-family-union
                                (gethash to families) candidate)))
                       (when (> (length merged) 32)
                         (error "The minimal recording-walk route family exceeds 32 ~
                                 alternatives; the walk topology is pathological."))
                       (unless (equal merged (gethash to families))
                         (setf (gethash to families) merged)
                         (setf changed t)))))))
          while changed)
    (sort
      (loop for location being the hash-keys of families
              using (hash-value family)
            collect (list location family))
      #'string< :key (lambda (entry) (symbol-name (first entry))))))

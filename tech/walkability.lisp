;;; Filename: walkability.lisp

;;; Walking mobility mode.  Registers the one predicate that makes a traversal edge a
;;; walk: the two endpoints must sit at the same level, and the clause's doors must be
;;; passable.  Everything else -- the relation, the iteration over modes and destinations,
;;; the choice among a family's clauses -- belongs to -traversal, which every mode shares.
;;;
;;; Walking is the only mode with an elevation *equality* test, and that is what makes the
;;; coordinate derivation safe: -walkability-coordinates is elevation-blind and happily
;;; emits an edge between two locations at different levels, which ONE-STEP-WALKABLE then
;;; refuses.  The nested -terrain-consistency validation automatically checks the universal
;;; geometric invariant: an edge's vertical span must match the determinate level step it
;;; separates.  Stronger connectivity assumptions belong to topology-spec review and are
;;; applied by TEST-TOPO, not by ordinary walking models.
;;;
;;; REQUIRES:
;;;   types     : agent, location
;;;   nested    : -support-occupancy; -location; -passability; -vertical; -elevation;
;;;               -traversal; -walkability-coordinates; -terrain-consistency; -threat;
;;;               -mobility-action
;;; PROVIDES:
;;;   mode      : walking, registered with -traversal
;;;   queries   : one-step-walkable
;;;   init      : automatic terrain edge-span validation during walking derivation
;;;   action    : move (from -mobility-action)

(include-tech -support-occupancy)
(include-tech -location)
(include-tech -passability)
(include-tech -vertical)
(include-tech -elevation)
(include-tech -traversal)
(include-tech -walkability-coordinates)
(include-tech -terrain-consistency)
(include-tech -threat)
(include-tech -mobility-action)

(in-package :ww)


(define-problem-helper walking-segment-for-clause
    (state agent source destination clause)
  "Return a normalized WALK segment when CLAUSE's doors are all passable and the endpoints
   share a level.  An empty clause is the direct, unguarded case: ALL-CLEAR reads it as
   clear, so the level test alone decides."
  (when (and (= (funcall (symbol-function 'location-elevation) state source)
                (funcall (symbol-function 'location-elevation) state destination))
             (funcall (symbol-function 'all-clear) state agent clause)
             (funcall (symbol-function 'safe) state destination))
    (list 'walk source clause destination)))


(register-traversal-mode 'walking 'walking-segment-for-clause
                         '(gate screen ladder gears))


(define-query one-step-walkable (?agent agent ?from location ?to location)
  ;; Restricted to WALK segments on purpose.  The shared provider now returns every mode's
  ;; segments, and a caller asking whether two locations are one *walk* apart -- the
  ;; elevation-equality question -- must not be answered by a stairs or ladder edge.
  (ww-loop for $segment in (traversal-segments ?agent ?from)
           thereis (and (eql (first $segment) 'walk)
                        (eql (fourth $segment) ?to))))

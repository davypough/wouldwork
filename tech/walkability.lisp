;;; Filename: walkability.lisp

;;; Walking traversal kind.  A clause naming no kind's marker -- only gates, screens and
;;; gears, or nothing -- is a walk, and this file registers the one predicate that decides
;;; it: the two endpoints must sit at the same level, and the clause's doors must be
;;; passable.  Everything else -- the relation, kind inference, the iteration over
;;; destinations, the choice among a family's clauses -- belongs to -traversal, which every
;;; kind shares.  Walking registers no markers and no static separators.
;;;
;;; Walking is the only kind with an elevation *equality* test.  -traversal's init check
;;; rejects an authored walk clause across a level difference when both endpoints carry
;;; coordinates, and reads one as a jump in a bare-level problem, so the test here is a
;;; last guard rather than the rule itself.  The nested -terrain-consistency validation automatically checks the universal
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
;;;   kind      : walk, registered with -traversal
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


(register-traversal-kind 'walk 'walking-segment-for-clause
                         nil nil '(gate screen gears))


(define-query one-step-walkable (?agent agent ?from location ?to location)
  ;; Restricted to WALK segments on purpose.  The shared provider now returns every kind's
  ;; segments, and a caller asking whether two locations are one *walk* apart -- the
  ;; elevation-equality question -- must not be answered by a stairs or ladder edge.
  (ww-loop for $segment in (traversal-segments ?agent ?from)
           thereis (and (eql (first $segment) 'walk)
                        (eql (fourth $segment) ?to))))

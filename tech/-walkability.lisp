;;; Filename: -walkability.lisp

;;; Walkability substrate: the shared interface for technologies that need the set of
;;; locations an agent can currently reach by walking.  The default exposes only the starting
;;; location; the public walkability technology overrides it with the walking closure over
;;; passable edges.  WALKABLE provides the Boolean predicate parallel to VISIBLE and REACHABLE.
;;;
;;; REQUIRES:
;;;   types    : agent, location
;;; PROVIDES:
;;;   queries  : walkable-locations  --  identity default, overridden by walkability
;;;              walkable            --  Boolean membership in that closure

(in-package :ww)


(define-query walkable-locations (?agent agent ?from location)
  (do ?agent (list ?from)))


(define-query walkable (?agent agent ?from location ?to location)
  (member ?to (walkable-locations ?agent ?from)))

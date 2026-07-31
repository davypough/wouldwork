;;; Filename: -walkability.lisp

;;; Walkability substrate: the shared topology and query interface for technologies that
;;; produce or consume walking edges.  WALK-VIA/WALK-VIA> hold authored or coordinate-derived
;;; topology.  The default query exposes only the starting location; the public walkability
;;; technology overrides it with the walking closure over passable edges.  WALKABLE provides
;;; the Boolean predicate parallel to VISIBLE and REACHABLE.
;;;
;;; REQUIRES:
;;;   types    : agent, location
;;; PROVIDES:
;;;   relations: (walk-via location $list location), (walk-via> location $list location)
;;;              -- $list is a DNF clause list: () direct, else OR over clauses, AND within
;;;   queries  : walkable-locations  --  identity default, overridden by walkability
;;;              walkable            --  Boolean membership in that closure

(in-package :ww)


(define-static-relations
  (walk-via location $list location)  ;symmetric walking edge; $list = DNF door clauses: () direct, else OR over clauses, AND within, e.g. ((gate1) (gate2 gate3))
  (walk-via> location $list location))  ;directional walking edge, same $list convention; emitted by -walkability-coordinates for rides into a stream's destination (inbound widened by side-curtain rides, outbound ordinary)


(define-query walkable-locations (?agent agent ?from location)
  (do ?agent (list ?from)))


(define-query walkable (?agent agent ?from location ?to location)
  (member ?to (walkable-locations ?agent ?from)))

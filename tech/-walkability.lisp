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
;;;   functions: canonical minimal-family algebra used by the coordinate zone-graph
;;;              derivation in -walkability-coordinates

(in-package :ww)


(define-static-relations
  (walk-via location $list location)  ;symmetric walking edge; $list = DNF door clauses: () direct, else OR over clauses, AND within, e.g. ((gate1) (gate2 gate3))
  (walk-via> location $list location))  ;directional walking edge, same $list convention; emitted by -walkability-coordinates for rides into a stream's destination (inbound widened by side-curtain rides, outbound ordinary)


(define-init-check walkability-init-check (literals)
  (:consumes gate screen ladder gears)
  (dolist (relation '(walk-via walk-via>))
    (dolist (literal (init-literals-with-relation relation literals))
      (init-check-dnf-list-items-have-types
        literal
        (third (init-literal-proposition literal))
        '(gate screen ladder gears)))))


(define-query walkable-locations (?agent agent ?from location)
  (do ?agent (list ?from)))


(define-query walkable (?agent agent ?from location ?to location)
  (member ?to (walkable-locations ?agent ?from)))


;;;; MINIMAL WALK-OBSTACLE FAMILIES ;;;;
;;;; Shared by coordinate topology derivation and optional runtime routes.  A
;;;; family is an antichain of obstacle sets: OR over clauses, AND within each clause.


(defun walkability-family-union (family1 family2)
  ;; Alternative routes: all clauses of both, minimized and canonicalized.
  (walkability-minimize-family (append family1 family2)))


(defun walkability-family-add-obstacle (family obstacle)
  ;; Path extension by one obstacle, retained for the coordinate zone graph.
  (walkability-minimize-family
    (mapcar (lambda (clause) (cons obstacle clause)) family)))


(defun walkability-minimize-family (family)
  ;; Canonical clauses, duplicates removed, and every nonminimal superset discarded.
  (let* ((clauses (remove-duplicates
                    (mapcar #'walkability-canonical-clause family)
                    :test #'equal))
         (minimal (remove-if (lambda (clause)
                               (some (lambda (other)
                                       (and (not (equal other clause))
                                            (subsetp other clause)))
                                     clauses))
                             clauses)))
    (sort (copy-list minimal) #'walkability-clause-precedes-p)))


(defun walkability-canonical-clause (clause)
  (sort (copy-list (remove-duplicates clause)) #'string< :key #'symbol-name))


(defun walkability-clause-precedes-p (clause1 clause2)
  (cond ((/= (length clause1) (length clause2))
         (< (length clause1) (length clause2)))
        (t (loop for obstacle1 in clause1
                 for obstacle2 in clause2
                 unless (eq obstacle1 obstacle2)
                   return (string< (symbol-name obstacle1)
                                   (symbol-name obstacle2))
                 finally (return nil)))))


(defun walkability-normalize-family (family)
  ;; WALK-VIA represents the family containing one empty clause as NIL.
  (if (equal family '(nil))
    nil
    family))

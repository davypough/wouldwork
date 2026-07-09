;;; Filename: -reachability.lisp

;;; Reachability substrate: the baseline meaning of physical reach for manipulation
;;; actions.  Objects at the same location are always mutually reachable.  The public
;;; reachability technology overrides this query to add authored reachable-via edges.
;;;
;;; Nested-only; included by technologies that call reachable.
;;;
;;; REQUIRES:
;;;   type  : location
;;; PROVIDES:
;;;   query : reachable  --  identity default, overridden by reachability

(in-package :ww)


(define-query reachable (?location1 location ?location2 location)
  (eql ?location1 ?location2))

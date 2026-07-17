;;; Filename: -accessibility.lisp

;;; Accessibility substrate: the shared interface for technologies that need the set of
;;; locations an agent can currently reach.  The default exposes only the starting location;
;;; the public accessibility technology overrides it with walking closure over passable edges.
;;;
;;; REQUIRES:
;;;   types    : agent, location
;;; PROVIDES:
;;;   query    : accessible  --  identity default, overridden by accessibility

(in-package :ww)


(define-query accessible (?agent agent ?from location)
  (do ?agent (list ?from)))

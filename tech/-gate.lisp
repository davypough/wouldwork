;;; Filename: -gate.lisp

;;; Gate substrate: the shared (open gate) relation and its gate optional type, owned in
;;; one place so every capability that reads gate openness -- accessibility (via nested
;;; -passability), reachability, visibility, beam-direct, beam-crossing, and -passability
;;; itself -- nests this file instead of hand-copying the declaration.  Only gate.lisp's
;;; update-gate-status! ever asserts (open gate); an unincluded gate leaves the relation
;;; declared but never true, which is the correct default when a problem has no gates.
;;;
;;; REQUIRES:
;;;   types     : (none bare)
;;; PROVIDES:
;;;   types     : gate  --  declared optional here
;;;   relations : (open gate)  --  asserted only by gate.lisp's update-gate-status!

(in-package :ww)


(define-optional-types gate)


(define-dynamic-relations
  (open gate))

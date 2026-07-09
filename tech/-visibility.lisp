;;; Filename: -visibility.lisp

;;; Visibility substrate: the shared interface for technologies that require a
;;; sightline.  The default exposes no visibility; the public visibility technology
;;; overrides it with authored line-of-sight relations and gate checks.
;;;
;;; REQUIRES:
;;;   type     : location
;;; PROVIDES:
;;;   types    : fixture (either gate transmitter receiver); gate, transmitter, and
;;;              receiver are declared optional
;;;   query    : visible  --  null default, overridden by visibility

(in-package :ww)


(define-optional-types gate transmitter receiver)


(define-types
  fixture (either gate transmitter receiver))


(define-query visible (?location location ?object (either fixture location))
  (do ?location ?object nil))

;;; Filename: -visibility.lisp

;;; Visibility substrate: the shared interface for technologies that require a
;;; sightline.  The default exposes no visibility; the public visibility technology
;;; overrides it with authored line-of-sight relations and gate checks.
;;;
;;; REQUIRES:
;;;   type     : location
;;; PROVIDES:
;;;   types    : fixture (either gate transmitter receiver gun), apparatus (either
;;;              transmitter receiver gun); gate, transmitter, receiver, and gun are
;;;              declared optional.  gun joins both unions as a point fixture exactly like
;;;              transmitter/receiver -- jam-target's LOS check reads it through visible,
;;;              not through has-position, so nothing can ever occupy a gun's position.
;;;   queries  : visible, potentially-visible  --  null defaults, overridden by visibility

(in-package :ww)


(define-optional-types gate transmitter receiver gun)


(define-types
  fixture (either gate transmitter receiver gun)
  apparatus (either transmitter receiver gun))  ;a transmitter, receiver, or gun; los-to-apparatus's target type


(define-query visible (?location location ?object (either fixture location))
  (do ?location ?object nil))


(define-query potentially-visible (?location location ?object (either fixture location))
  (do ?location ?object nil))

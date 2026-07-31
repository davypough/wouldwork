;;; Filename: -visibility.lisp

;;; Visibility substrate: the shared interface for technologies that require a
;;; sightline.  The default exposes no visibility; the public visibility technology
;;; overrides it with authored line-of-sight relations and gate checks.
;;;
;;; REQUIRES:
;;;   type     : location
;;; PROVIDES:
;;;   types    : fixture (either gate transmitter receiver repeater gun), apparatus (either
;;;              transmitter receiver repeater gun); gate, transmitter, receiver, repeater,
;;;              and gun are optional.  APPARATUS-COORDS> names each apparatus's functional
;;;              point: beam emission/reception/relay for beam apparatus, and the
;;;              firing/targeting point for a gun.
;;;   queries  : visible, potentially-visible, beam-visible  --  null defaults, overridden
;;;              by visibility. Their typed object parameters remain valid when an optional
;;;              type has no objects: the query is still installed and its null body returns
;;;              NIL; only iteration over that empty type produces no calls.

(in-package :ww)


(define-optional-types
  gate transmitter receiver gun floor-repeater wall-repeater)


(define-types
  repeater (either floor-repeater wall-repeater))


(define-types
  fixture (either gate transmitter receiver repeater gun)
  apparatus (either transmitter receiver repeater gun))


(define-query visible (?location location ?object (either fixture location))
  (do ?location ?object nil))


(define-query potentially-visible (?location location ?object (either fixture location))
  (do ?location ?object nil))


(define-query beam-visible
    (?location location
     ?near-elevation
     ?object (either apparatus location)
     ?far-elevation)
  ;; Locations/apparatus are Wouldwork objects. Elevations are computed Lisp values and
  ;; therefore deliberately have no Wouldwork object type.
  (do ?location ?near-elevation ?object ?far-elevation nil))

;;; Filename: -visibility.lisp

;;; Visibility substrate: the shared interface for technologies that require a
;;; sightline.  The default exposes no visibility; the public visibility technology
;;; overrides it with authored line-of-sight relations and gate checks.
;;;
;;; REQUIRES:
;;;   type     : location
;;; PROVIDES:
;;;   types    : apparatus (either transmitter receiver floor-repeater wall-repeater gun);
;;;              gate, transmitter, receiver, both repeater leaf types, and gun are optional.
;;;              APPARATUS-COORDS> names each apparatus's functional point: beam
;;;              emission/reception/relay for beam apparatus, and the firing/targeting point
;;;              for a gun.
;;;   queries  : visible, visible-for-object, potentially-visible, beam-visible,
;;;              beam-visible-for-object, elevation-visible-for-object -- null defaults,
;;;              overridden by visibility.
;;;              The FOR-OBJECT forms select actor/view-specific gate state.  Their typed
;;;              object parameters remain valid when an optional type has no objects: the
;;;              query is still installed and its null body returns NIL; only iteration
;;;              over that empty type produces no calls.

(in-package :ww)


(define-optional-types
  gate transmitter receiver gun floor-repeater wall-repeater)


(define-types
  repeater (either floor-repeater wall-repeater))


(define-types
  apparatus
    (either transmitter receiver floor-repeater wall-repeater gun))


(define-query visible
    (?location location
     ?object (either gate transmitter receiver floor-repeater wall-repeater gun location))
  (do ?location ?object nil))


(define-query visible-for-object
    (?view
     ?location location
     ?object (either gate transmitter receiver floor-repeater wall-repeater gun location))
  (do ?view ?location ?object nil))


(define-query potentially-visible
    (?location location
     ?object (either gate transmitter receiver floor-repeater wall-repeater gun location))
  (do ?location ?object nil))


(define-query beam-visible
    (?location location
     ?near-elevation
     ?object (either transmitter receiver floor-repeater wall-repeater gun location)
     ?far-elevation)
  ;; Locations/apparatus are Wouldwork objects. Elevations are computed Lisp values and
  ;; therefore deliberately have no Wouldwork object type.
  (do ?location ?near-elevation ?object ?far-elevation nil))


(define-query beam-visible-for-object
    (?view
     ?location location
     ?near-elevation
     ?object (either transmitter receiver floor-repeater wall-repeater gun location)
     ?far-elevation)
  (do ?view ?location ?near-elevation ?object ?far-elevation nil))


(define-query elevation-visible-for-object
    (?view
     ?location location
     ?near-elevation
     ?object (either gate transmitter receiver floor-repeater wall-repeater gun location)
     ?far-elevation)
  (do ?view ?location ?near-elevation ?object ?far-elevation nil))

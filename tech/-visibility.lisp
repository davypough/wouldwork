;;; Filename: -visibility.lisp

;;; Visibility substrate: the shared interface for technologies that require a
;;; sightline.  The default exposes no visibility; the public visibility technology
;;; overrides it with authored line-of-sight relations and gate checks.
;;;
;;; REQUIRES:
;;;   type     : location
;;; PROVIDES:
;;;   types    : repeater (either floor-repeater wall-repeater); gate, transmitter,
;;;              receiver, both repeater leaf types, and gun are optional.
;;;   queries  : visible, visible-for-object, potentially-visible, beam-visible,
;;;              beam-visible-for-object, elevation-visible-for-object -- null defaults,
;;;              overridden by visibility.  Their target domains list the point-apparatus
;;;              leaf types directly rather than introducing an unused composite alias.
;;;              The FOR-OBJECT forms select actor/view-specific gate state.  Their typed
;;;              object parameters remain valid when an optional type has no objects: the
;;;              query is still installed and its null body returns NIL; only iteration
;;;              over that empty type produces no calls.
;;;   init      : helpers recognizing authored or pending coordinate-derived LOS topology

(in-package :ww)


(define-optional-types
  gate transmitter receiver gun floor-repeater wall-repeater)


(define-types
  repeater (either floor-repeater wall-repeater))


(define-init-check-helper init-coordinate-derived-sightlines-p (literals)
  "Whether the installed visibility technology will derive LOS-VIA from this geometry."
  (and (find 'derive-los-from-segments *init-actions* :key #'action.name)
       (or (positive-init-literals-with-relation 'wall-segment> literals)
           (positive-init-literals-with-relation 'edge-segment> literals)
           (positive-init-literals-with-relation 'boundary-wall literals))))


(define-init-check-helper init-apparatus-has-potential-sightline-p (apparatus literals)
  (or (init-coordinate-derived-sightlines-p literals)
      (some (lambda (literal)
              (destructuring-bind (los-location occluders los-apparatus)
                  (rest (init-literal-proposition literal))
                (declare (ignore los-location occluders))
                (eql apparatus los-apparatus)))
            (positive-init-literals-with-relation 'los-via literals))))


(define-init-check-helper init-location-has-potential-sightline-p (location literals)
  (or (init-coordinate-derived-sightlines-p literals)
      (some (lambda (literal)
              (destructuring-bind (los-location1 occluders los-location2)
                  (rest (init-literal-proposition literal))
                (declare (ignore occluders))
                (or (eql location los-location1)
                    (eql location los-location2))))
            (positive-init-literals-with-relation 'los-via literals))))


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
  ;; Locations and point apparatus are Wouldwork objects. Elevations are computed Lisp
  ;; values and therefore deliberately have no Wouldwork object type.
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

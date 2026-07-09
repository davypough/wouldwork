;;; Filename: visibility.lisp

;;; Visibility background capability: whether a fixture or another location is in sight from a
;;; location.  In this file, a fixture is a fixed LOS object: a gate, transmitter, or
;;; receiver.  A sightline must exist in the los tables and is clear iff every occluder gate
;;; is open.
;;;
;;; REQUIRES:
;;;   types     : location  --  gate, transmitter, and receiver are declared optional here
;;;               through nested -visibility, coordinated with gate, accessibility,
;;;               reachability, beam-direct, and beam-crossing, which all convert gate
;;;               together since they share the (open gate) relation verbatim
;;;   nested    : -visibility (fixture and the null-default visible interface)
;;; PROVIDES:
;;;   relations : (open gate)  --  also declared identically by gate, accessibility,
;;;               reachability, and beam-direct; only gate's update-gate-status!
;;;               ever asserts it
;;;               (los-to-fixture location $list fixture),
;;;               (los-to-location location $list location)
;;;   queries   : visible (overrides -visibility's null default), visible-clear

(include-tech -visibility)

(in-package :ww)

(define-dynamic-relations
  (open gate))  ;also declared by gate/accessibility/reachability/beam-direct; only gate writes it


(define-static-relations
  (los-to-fixture location $list fixture)  ;per-location occluders on a sightline to a fixture
  (los-to-location location $list location))  ;symmetric per-pair occluders for location-to-location sightlines


(define-query visible (?location location ?object (either fixture location))
  ;; A sightline must exist (an empty occluder list is a direct, always-clear line); it is
  ;; clear iff every occluder is transparent.  Agent-independent.  ?object is a fixture
  ;; (los-to-fixture) or another location (los-to-location); at most one matches, so try
  ;; fixture then location.
  (and (or (bind (los-to-fixture ?location $occluders ?object))
           (bind (los-to-location ?location $occluders ?object)))
       (ww-loop for $o in $occluders
                always (visible-clear $o))))


(define-query visible-clear (?occluder gate)
  ;; Per-kind transparency for one occluder.  Claustro sightlines pass only through gates; the
  ;; intervening-occupied-location branch is the documented extension.
  (and (gate ?occluder)
       (open ?occluder)))

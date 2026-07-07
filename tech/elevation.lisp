;;; Filename: elevation.lisp

;;; Elevation background capability: public wrapper for the shared elevation substrate.
;;; Include this from a problem when its authored initial facts use fixed elevation levels,
;;; without exposing the dash-prefixed substrate file as part of the problem-facing API.
;;;
;;; PROVIDES:
;;;   via -elevation: elevated-object, has-elevation, object-elevation,
;;;                   location-elevation, fixture-elevation

(include-tech -elevation)

(in-package :ww)

;;; Filename: elevation.lisp

;;; Elevation background capability: public wrapper for the shared elevation substrate.
;;; Include this from a problem when its authored initial facts use fixed elevation levels,
;;; without exposing the dash-prefixed substrate file as part of the problem-facing API.
;;;
;;; PROVIDES:
;;;   via -elevation: elevated-object, has-elevation, object-elevation,
;;;                   location-elevation  --  the authored base level of an object with no
;;;                   coordinate relation of its own, default 0.  A problem needing the
;;;                   full vertical model (base, top, per-type heights and axes) includes
;;;                   -vertical instead, which nests this substrate.

(include-tech -elevation)

(in-package :ww)

;;; Filename: elevation.lisp

;;; Elevation background capability: public wrapper for the shared elevation substrate.
;;; Include this from a problem when its authored initial facts use fixed elevation levels,
;;; without exposing the dash-prefixed substrate file as part of the problem-facing API.
;;;
;;; PROVIDES:
;;;   via -elevation: elevated-object, has-elevation  --  the authored base level of an
;;;                   object with no coordinate relation of its own.  The substrate owns
;;;                   no query: a problem that needs to read a level includes -vertical,
;;;                   which nests this file and provides BASE, TOP, and LOCATION-ELEVATION.
;;;                   Include this wrapper alone only to author the facts.

(include-tech -elevation)

(in-package :ww)

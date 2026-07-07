;;; Filename: location-elevation.lisp

;;; Elevation background capability: compatibility wrapper for the shared elevation
;;; substrate.  Self-contained; spliced by (include-tech location-elevation).
;;;
;;; PROVIDES:
;;;   via -elevation: elevated-object, has-elevation, object-elevation, location-elevation,
;;;                   fixture-elevation

(include-tech -elevation)

(in-package :ww)

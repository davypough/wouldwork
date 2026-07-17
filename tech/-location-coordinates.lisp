;;; Filename: -location-coordinates.lisp

;;; Location-coordinates substrate: the fixed 2D placement of a location, as its own
;;; capability independent of walking, beams, or anything else.  Owns LOCATION-POSITION>
;;; so that accessibility-tech's own coordinate-driven WALK-VIA derivation and beam-
;;; crossing-tech's -beam-coordinates substrate share one source of truth for where each
;;; location is, without either technology depending on the other.  A problem that only
;;; needs walking between locations, with no transmitters/receivers/connectors, never
;;; needs to know beam-crossing exists; a problem using both capabilities enters each
;;; location's coordinates exactly once.
;;;
;;; Self-contained; spliced by (include-tech -location-coordinates).
;;;
;;; REQUIRES:
;;;   types     : location  --  declared by the problem
;;; PROVIDES:
;;;   relation  : (location-position> location $rational $rational)

(in-package :ww)


(define-static-relations
  (location-position> location $rational $rational))

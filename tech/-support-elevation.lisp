;;; Filename: -support-elevation.lisp

;;; Support-elevation substrate: the vertical level of a support's top and of an
;;; occupant standing either on a support or directly at a location.  These queries
;;; are shared by cargo manipulation and barrier vaulting.
;;;
;;; REQUIRES:
;;;   nested  : -support-occupancy, -location, -position, -height, -elevation
;;; PROVIDES:
;;;   queries : support-top-elevation, occupant-elevation,
;;;             within-agent-vertical-reach

(include-tech -support-occupancy)
(include-tech -location)
(include-tech -position)
(include-tech -height)
(include-tech -elevation)

(in-package :ww)


(define-optional-types plate box)


(define-query support-top-elevation (?support support)
  ;; A box top is its resting level plus its declared-or-default height.  A plate
  ;; top is the floor elevation of the location where the plate is positioned.
  (if (box ?support)
    (+ (occupant-elevation ?support) (declared-height ?support))
    (do (bind (has-position ?support $location))
        (location-elevation $location))))


(define-query occupant-elevation (?occupant support-occupant)
  ;; An occupant on a box stands at that box's top.  An occupant on the ground or
  ;; on a plate stands at the floor elevation of its own location.
  (if (and (bind (on ?occupant $support))
           (box $support))
    (+ (occupant-elevation $support) (declared-height $support))
    (do (bind (has-location ?occupant $location))
        (location-elevation $location))))


(define-query within-agent-vertical-reach (?agent agent ?target-elevation)
  ;; Cargo pickup and placement use one vertical-reach convention: measure from
  ;; the agent's standing elevation, with the agent's declared height as the
  ;; maximum absolute distance above or below that level.
  (<= (abs (- ?target-elevation (occupant-elevation ?agent)))
      (declared-height ?agent)))

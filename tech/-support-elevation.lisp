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


(define-optional-types box fan)


(define-query support-top-elevation (?support support)
  ;; A box top is its resting level plus its declared-or-default height.  A fan is a
  ;; movable, zero-thickness support: its top is its own resting level, so a fan mounted
  ;; on gears stays flush with the floor.  A plate top is the floor elevation of the
  ;; location where the plate is positioned.
  (if (box ?support)
    (+ (occupant-elevation ?support) (declared-height ?support))
    (if (fan ?support)
      (occupant-elevation ?support)
      (do (bind (has-position ?support $location))
          (location-elevation $location)))))


(define-query occupant-elevation (?occupant support-occupant)
  ;; An occupant on a support stands at that support's top (for a plate this is the floor
  ;; elevation, so flush fixtures cost nothing; a box or fan support chains recursively).
  ;; An occupant on the ground -- including a fan mounted on gears, whose attachment is
  ;; not an (on ...) fact -- stands at the floor elevation of its own location.
  (if (bind (on ?occupant $support))
    (support-top-elevation $support)
    (do (bind (has-location ?occupant $location))
        (location-elevation $location))))


(define-query within-agent-vertical-reach (?agent agent ?target-elevation)
  ;; Cargo pickup and placement use one vertical-reach convention: measure from
  ;; the agent's standing elevation, with the agent's declared height as the
  ;; maximum absolute distance above or below that level.
  (<= (abs (- ?target-elevation (occupant-elevation ?agent)))
      (declared-height ?agent)))

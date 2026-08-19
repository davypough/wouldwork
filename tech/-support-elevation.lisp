;;; Filename: -support-elevation.lisp

;;; Support-elevation substrate: the vertical level of an occupant standing either on a
;;; support or directly at a location, and the reach policies that consume it.
;;;
;;; A support's top is no longer computed here.  SUPPORT-TOP-ELEVATION and its held-tray
;;; special case TRAY-TOP-ELEVATION were exactly -vertical's TOP over the SUPPORT domain:
;;; a box's top is its base plus its height; a fan, tray, plate, or fixed blower is
;;; zero-thickness, so its top is its base; and a held tray's base is already its holder's
;;; top, which is what the special case computed by hand.  Callers use TOP directly.
;;;
;;; REQUIRES:
;;;   nested  : -vertical (base, top), -support-occupancy, -location, -position, -height,
;;;             -elevation, -holding
;;; PROVIDES:
;;;   parameter : *vertical-reach-limit*, default 1 -- the maximum elevation gap an agent
;;;               can act across vertically: lifting cargo above or below its own elevation,
;;;               raising cargo onto a higher resting place, or jumping up onto a higher
;;;               support or clearing a barrier (jump.lisp reuses this parameter rather than
;;;               defining its own).  Independent of the agent's own declared height.  A
;;;               problem overrides it with its own DEFPARAMETER.
;;;   queries   : occupant-elevation,
;;;               within-agent-vertical-reach (symmetric, for lifting),
;;;               within-agent-placement-reach (one-sided, for setting down)

(include-tech -vertical)
(include-tech -support-occupancy)
(include-tech -location)
(include-tech -position)
(include-tech -height)
(include-tech -elevation)
(include-tech -holding)

(in-package :ww)


(define-optional-types box fan tray)


(defvar *vertical-reach-limit* 1
  "Maximum elevation gap an agent can act across vertically -- lifting cargo above or below
   its own elevation, raising cargo onto a higher resting place, or jumping up onto a higher
   support or clearing a barrier -- independent of the agent's own declared height.  Lifting
   is symmetric: an object resting more than this far below the agent's elevation is out of
   reach exactly as one more than this far above it is.  Setting an object down is not: only
   the upward direction is bounded, since a drop needs no reach at all.  Problem files can
   override this.")


(define-query occupant-elevation (?occupant support-occupant)
  ;; An occupant on a support stands at that support's top (for a plate this is the floor
  ;; elevation, so flush fixtures cost nothing; a box or fan support chains recursively).
  ;; An occupant on the ground -- including a fan mounted on gears, whose attachment is
  ;; not an (on ...) fact -- stands at the floor elevation of its own location.
  (if (bind (on ?occupant $support))
    (top $support)
    (do (bind (has-location ?occupant $location))
        (location-elevation $location))))


(define-query within-agent-vertical-reach (?agent agent ?target-elevation)
  ;; The lifting convention, used by cargo pickup and by any fixture an agent must reach to
  ;; manipulate: measure from the agent's standing elevation, capped by
  ;; *vertical-reach-limit* in either direction, independent of the agent's own declared
  ;; height.  Setting cargo down uses WITHIN-AGENT-PLACEMENT-REACH instead.
  (<= (abs (- ?target-elevation (occupant-elevation ?agent)))
      *vertical-reach-limit*))


(define-query within-agent-placement-reach (?agent agent ?target-elevation)
  ;; The setting-down convention, one-sided where lifting is symmetric: an agent can lower or
  ;; drop cargo any distance below its own standing elevation, since gravity does the work,
  ;; but can only raise a resting place *vertical-reach-limit* above itself.  Recovering what
  ;; it dropped is the symmetric WITHIN-AGENT-VERTICAL-REACH test, so a drop down a ledge is
  ;; deliberately not reversible from where the agent stands.
  (<= (- ?target-elevation (occupant-elevation ?agent))
      *vertical-reach-limit*))

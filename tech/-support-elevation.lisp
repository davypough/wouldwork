;;; Filename: -support-elevation.lisp

;;; Support-elevation policy: how far an agent can reach vertically.  This file no longer
;;; computes any geometry -- -vertical owns all of it.
;;;
;;; SUPPORT-TOP-ELEVATION and its held-tray special case TRAY-TOP-ELEVATION were exactly
;;; -vertical's TOP over the SUPPORT domain: a box's top is its base plus its height; a
;;; fan, tray, plate, or fixed blower is zero-thickness, so its top is its base; and a
;;; held tray's base is already its holder's top, which is what the special case computed
;;; by hand.  OCCUPANT-ELEVATION was BASE restricted to SUPPORT-OCCUPANT, minus the
;;; HOLDING branch: it resolved ON, then fell through to the occupant's own location.
;;; That branch is unreachable for every occupant it was called on, since held cargo
;;; loses its HAS-LOCATION -- and the one exception, a tray, is excluded from pickup while
;;; held and is zero-thickness anyway.  Callers use BASE and TOP directly.
;;;
;;; What remains is policy rather than geometry, and stays: the reach limit is a rule
;;; about what an agent may do, not a fact about where anything is.
;;;
;;; REQUIRES:
;;;   nested  : -vertical (base)
;;; PROVIDES:
;;;   parameter : *vertical-reach-limit*, default 1 -- the maximum elevation gap an agent
;;;               can act across vertically: lifting cargo above or below its own elevation,
;;;               raising cargo onto a higher resting place, or jumping up onto a higher
;;;               support or clearing a barrier (jump.lisp reuses this parameter rather than
;;;               defining its own).  Independent of the agent's own declared height.  A
;;;               problem overrides it with its own DEFPARAMETER.
;;;   queries   : within-agent-vertical-reach (symmetric, for lifting),
;;;               within-agent-placement-reach (one-sided, for setting down)

(include-tech -vertical)

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


(define-query within-agent-vertical-reach (?agent agent ?target-elevation)
  ;; The lifting convention, used by cargo pickup and by any fixture an agent must reach to
  ;; manipulate: measure from the agent's standing elevation, capped by
  ;; *vertical-reach-limit* in either direction, independent of the agent's own declared
  ;; height.  Setting cargo down uses WITHIN-AGENT-PLACEMENT-REACH instead.
  (<= (abs (- ?target-elevation (base ?agent)))
      *vertical-reach-limit*))


(define-query within-agent-placement-reach (?agent agent ?target-elevation)
  ;; The setting-down convention, one-sided where lifting is symmetric: an agent can lower or
  ;; drop cargo any distance below its own standing elevation, since gravity does the work,
  ;; but can only raise a resting place *vertical-reach-limit* above itself.  Recovering what
  ;; it dropped is the symmetric WITHIN-AGENT-VERTICAL-REACH test, so a drop down a ledge is
  ;; deliberately not reversible from where the agent stands.
  (<= (- ?target-elevation (base ?agent))
      *vertical-reach-limit*))

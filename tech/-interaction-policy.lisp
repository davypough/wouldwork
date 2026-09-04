;;; Filename: -interaction-policy.lisp

;;; Interaction-policy hooks.  Shared actions call these neutral interfaces without knowing
;;; whether a recorder is present.  -recorder-core overrides them after nesting this file,
;;; so include-tech deduplication preserves the overrides regardless of public include order.
;;;
;;; PROVIDES:
;;;   queries : object-manipulation-allowed  -- actor may pick up, carry, place, or mount object
;;;             support-use-allowed          -- occupant may rest or stand on support
;;;             connector-pairing-allowed    -- actor may pair connector to terminus
;;;             connector-location-conflict-p -- another lit connector blocks this placement

(in-package :ww)


(define-query object-manipulation-allowed (?actor ?object)
  (do ?actor ?object t))


(define-query support-use-allowed (?occupant ?support)
  (do ?occupant ?support t))


(define-query connector-pairing-allowed (?actor ?connector ?terminus)
  (do ?actor ?connector ?terminus t))


(define-query connector-location-conflict-p (?connector ?other)
  (do ?connector ?other t))

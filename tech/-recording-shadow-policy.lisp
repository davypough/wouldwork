;;; Filename: -recording-shadow-policy.lisp

;;; Neutral hooks for selecting an object's environmental state view.  Ordinary problems
;;; use only shared playback state.  -recorder-core selects the recording objects and their
;;; presence, while the gate and wall-gears shadow components override their capability
;;; hooks.  Beam peers consume those selections for recording-side beam state.
;;;
;;; PROVIDES:
;;;   queries : recording-shadow-object          -- object uses recording-side physics
;;;             recording-shadow-object-present  -- object exists in recording view
;;;             recording-shadow-turning         -- wall gears turn in recording-side state
;;;             recording-shadow-gate-open        -- gate is open in recording-side state

(in-package :ww)


(define-query recording-shadow-object (?object)
  (do ?object nil))


(define-query recording-shadow-object-present (?object)
  (do ?object t))


(define-query recording-shadow-turning (?gears)
  (do ?gears nil))


(define-query recording-shadow-gate-open (?gate)
  (do ?gate nil))

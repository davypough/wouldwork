;;; Filename: -recording-shadow-policy.lisp

;;; Neutral hooks for selecting an object's environmental state view.  Ordinary problems
;;; use only shared playback state.  Recorder overrides these hooks so ghost objects read
;;; the narrowly scoped recording-side gears, gate, and beam state maintained by
;;; recorder.lisp and the beam peers.
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

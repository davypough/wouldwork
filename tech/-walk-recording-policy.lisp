;;; Filename: -walk-recording-policy.lisp

;;; Neutral hooks for optional deferred walking analysis.  Ordinary walking has no deferred
;;; obstacles and records nothing; a stream technology may classify its obstacles for a
;;; future consumer.  Snapshot-reset recorder validation replays exact ordinary actions and
;;; does not override these hooks.
;;;
;;; PROVIDES:
;;;   queries : walk-playback-validation-required  -- use deferred walk generation
;;;             deferred-walk-obstacle             -- obstacle is checked at goal instead
;;;             recording-walk-obstacle-present    -- obstacle has its physical actuator
;;;   update  : record-walk-for-playback-validation! -- neutral history hook

(in-package :ww)


(define-query walk-playback-validation-required (?agent)
  (do ?agent nil))


(define-query deferred-walk-obstacle (?obstacle)
  (do ?obstacle nil))


(define-query recording-walk-obstacle-present (?obstacle)
  (do ?obstacle nil))


(define-update record-walk-for-playback-validation!
    (?agent ?from ?to ?route-family)
  (do ?agent ?from ?to ?route-family nil))

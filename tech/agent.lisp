;;; Filename: agent.lisp

;;; Agent technology: declarable physical characteristics of the agent itself, starting with
;;; height.  Self-contained; spliced by (include-tech agent).  A natural home for further
;;; per-agent physical or behavioral attributes as they come up (eg, a footprint/occlusion
;;; radius, reach, speed), rather than retrofitting whichever consuming tech file needed one
;;; first.
;;;
;;; REQUIRES:
;;;   type   : agent  --  always required directly by the problem; never wrapped in an
;;;            X-alias, unlike gate/box/fence/etc, since every problem instantiates at least
;;;            one agent
;;;   nested : -height (heighted-object, (has-height ...))  --  shared via nested include-tech
;;;            rather than local declaration
;;; PROVIDES:
;;;   query    : agent-height

(include-tech -height)

(in-package :ww)


(define-query agent-height (?agent agent)
  ;; Declared physical height of an agent, or 1 (the historical assumed reach/climb bound)
  ;; when undeclared, so every existing problem's climb/vault/reach checks keep their current
  ;; numeric behavior unchanged.  Mirrors box-height's declared-or-default shape.
  (if (bind (has-height ?agent $h))
    $h
    1))

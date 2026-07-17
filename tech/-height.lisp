;;; Filename: -height.lisp

;;; Height substrate: the physical height of a heighted object, used for vertical reach and
;;; vaulting-clearance checks.  This file owns the heighted-object type composition, the
;;; (has-height ...) relation, and the shared declared-or-default query, declared identically
;;; by every tech file that reads or writes it -- box, jump, jammer, and beam-direct --
;;; so consumers nest-include this file instead of each re-declaring the same union, relation,
;;; or default-fallback query.
;;;
;;; declared-height's default of 1 covers box, agent, jammer, and beam-direct's beam-blocker
;;; (agent/box/jammer/connector); it is NOT used for barrier clearance -- fences, gates,
;;; screens, and walls use capability-specific defaults instead.
;;;
;;; PROVIDES:
;;;   type     : heighted-object (either box fence gate agent screen wall jammer connector)  --  what
;;;              can have a declared height; subtypes absent from the problem's own
;;;              define-types resolve to nil, a no-op
;;;   relation : (has-height heighted-object $fixnum)
;;;   query    : declared-height  --  declared value or a fixed default of 1; not for
;;;              fence/gate/screen/wall barrier-clearance height

(in-package :ww)


(define-optional-types wall)


(define-types
  heighted-object (either box fence gate agent screen wall jammer connector))  ;what can have a declared height


(define-static-relations
  (has-height heighted-object $fixnum))


(define-query declared-height (?object heighted-object)
  ;; Declared physical height of a box/agent/jammer/connector, or 1 (the historical
  ;; assumed unit height) when undeclared.  Barrier clearance uses its own kind-specific
  ;; defaults rather than this query.
  (if (bind (has-height ?object $h))
    $h
    1))

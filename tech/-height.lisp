;;; Filename: -height.lisp

;;; Height substrate: the physical height of a heighted object, used for vertical reach and
;;; vaulting-clearance checks.  This file owns the heighted-object type composition, the
;;; (has-height ...) relation, and the shared declared-or-default query, declared identically
;;; by every tech file that reads or writes it -- box, jump, jammer, and beam-direct --
;;; so consumers nest-include this file instead of each re-declaring the same union, relation,
;;; or default-fallback query.
;;;
;;; declared-height's default of 1 covers box, agent, jammer, connector, and repeater.
;;; A repeater's height follows its mounting axis: vertical for a floor-repeater and
;;; horizontal for a wall-repeater.  It is NOT used for barrier clearance -- gates,
;;; screens, and walls use capability-specific defaults instead.
;;;
;;; PROVIDES:
;;;   types    : repeater (either floor-repeater wall-repeater);
;;;              heighted-object (either box gate agent screen wall jammer connector
;;;              floor-repeater wall-repeater) -- what can have a declared height;
;;;              optional subtypes absent from the problem resolve to nil, a no-op
;;;   relation : (has-height heighted-object $fixnum)
;;;   query    : declared-height  --  declared value or a fixed default of 1; not for
;;;              gate/screen/wall barrier-clearance height

(in-package :ww)


(define-optional-types wall floor-repeater wall-repeater)


(define-types
  repeater (either floor-repeater wall-repeater)
  heighted-object
    (either box gate agent screen wall jammer connector floor-repeater wall-repeater))


(define-static-relations
  (has-height heighted-object $fixnum))


(define-query declared-height (?object heighted-object)
  ;; Declared physical height, or 1 when undeclared.  Barrier clearance uses its own
  ;; kind-specific defaults rather than this query.
  (if (bind (has-height ?object $h))
    $h
    1))

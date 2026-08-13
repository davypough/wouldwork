;;; Filename: -height.lisp

;;; Height substrate: the physical height of a heighted object, used for vertical reach,
;;; vaulting, and line-of-sight clearance checks.  This file owns the heighted-object type
;;; composition, the
;;; (has-height ...) relation, and the shared declared-or-default query, declared identically
;;; by every tech file that reads or writes it -- box, jump, jammer, and beam-direct --
;;; so consumers nest-include this file instead of each re-declaring the same union, relation,
;;; or default-fallback query.
;;;
;;; declared-height defaults gate, screen, and wall to 4; edge to 3/2; agent to 3/2; and
;;; box, jammer, connector, and repeater to 1.
;;; A repeater's height follows its mounting axis: vertical for a floor-repeater and
;;; horizontal for a wall-repeater.
;;;
;;; PROVIDES:
;;;   types    : repeater (either floor-repeater wall-repeater);
;;;              heighted-object (either box gate agent screen wall edge jammer connector
;;;              floor-repeater wall-repeater) -- what can have a declared height;
;;;              optional subtypes absent from the problem resolve to nil, a no-op
;;;   relation : (has-height heighted-object $rational)
;;;   query    : declared-height  --  declared value, or role default (gate/screen/wall 4,
;;;              edge 3/2, agent 3/2, box/jammer/connector/repeater 1)

(in-package :ww)


(define-optional-types wall edge floor-repeater wall-repeater)


(define-types
  repeater (either floor-repeater wall-repeater)
  heighted-object
    (either box gate agent screen wall edge jammer connector floor-repeater wall-repeater))


(define-static-relations
  (has-height heighted-object $rational))


(define-query declared-height (?object heighted-object)
  ;; Declared physical height, or the shared role default when undeclared.  Gate, screen,
  ;; and wall use 4; edge uses 3/2; agent uses 3/2; box, jammer, connector, and repeaters
  ;; use 1.
  (if (bind (has-height ?object $h))
    $h
    (if (or (gate ?object) (screen ?object) (wall ?object))
      4
      (if (edge ?object)
        3/2
        (if (agent ?object)
          3/2
          1)))))

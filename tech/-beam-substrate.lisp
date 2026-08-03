;;; Filename: -beam-substrate.lisp

;;; Beam substrate: the shared interface every beam capability programs against.  It owns
;;; the receiver-status driver, the direct/relay arrival and cut-liveness orchestrators,
;;; transmitter/receiver has-chroma and receiver-active relations, fixed coupling relations,
;;; and a null-object default for every pluggable hook.  beam-direct, beam-relay, and
;;; beam-crossing each include it and override only their own slots, so they compose in any
;;; combination.  Absent capabilities contribute no arrival, cuts, or relay lighting.
;;;
;;; Self-contained; spliced by (include-tech -beam-substrate).
;;;
;;; REQUIRES:
;;;   driver    : propagate-consequences! must call update-receiver-status!  (hue, transmitter,
;;;               and receiver are declared optional here via define-optional-types)
;;; PROVIDES:
;;;   types     : repeater, fixed-beam-source, fixed-beam-target, beam-node; hue,
;;;               transmitter, receiver, location, floor-repeater, and wall-repeater are
;;;               declared optional here; other techs
;;;               (beam-relay, beam-direct, gate, visibility, etc.) independently declare
;;;               their own hue-alias/transmitter-alias/receiver-alias for their own
;;;               pre-params; the bare and aliased forms resolve compatibly
;;;   relations : (active receiver), has-chroma, coupled, beam-via
;;;   queries   : beam-reaches-receiver, beam-live-for-cutting,
;;;               recording-shadow-beam-reaches-receiver,
;;;               plus null-object defaults for direct-beam-reaches-receiver,
;;;               relay-beam-reaches-receiver, direct-beam-live-for-cutting,
;;;               relay-beam-live-for-cutting, beam-cut, beam-cut-in,
;;;               recording-shadow-direct-beam-reaches-receiver,
;;;               recording-shadow-relay-beam-reaches-receiver,
;;;               fixed-beam-corridor-clear, fixed-beam-corridor-clear-for-object,
;;;               current-crossing-set,
;;;               compute-relay-lighting, beam-relay-source-distance
;;;   update    : update-receiver-status!

(include-tech -propagation)
(include-tech -beam-substrate-init-checks)

(in-package :ww)


(define-optional-types
  hue transmitter receiver location floor-repeater wall-repeater)


(define-types
  repeater (either floor-repeater wall-repeater)
  fixed-beam-source (either transmitter repeater)
  fixed-beam-target (either repeater receiver)
  beam-node (either transmitter receiver repeater location))


(define-dynamic-relations
  (active receiver))


(define-derived-relations
  active)


(define-static-relations
  (has-chroma (either transmitter receiver) $hue)
  (coupled fixed-beam-source fixed-beam-target)
  (beam-via fixed-beam-source $list fixed-beam-target))


(define-update update-receiver-status! ()
  ;; A receiver is active iff a chroma-matching direct or relay beam reaches it.
  (doall (?r receiver)
    (if (beam-reaches-receiver ?r)
      (active ?r)
      (not (active ?r)))))


(define-query beam-reaches-receiver (?receiver receiver)
  (or (direct-beam-reaches-receiver ?receiver)
      (relay-beam-reaches-receiver ?receiver)))


(define-query recording-shadow-beam-reaches-receiver (?receiver receiver)
  (or (recording-shadow-direct-beam-reaches-receiver ?receiver)
      (recording-shadow-relay-beam-reaches-receiver ?receiver)))


(define-query beam-live-for-cutting
    (?from beam-node
     ?to beam-node
     ?lighting)
  (or (direct-beam-live-for-cutting ?from ?to)
      (relay-beam-live-for-cutting ?from ?to ?lighting)))


;;;; NULL-OBJECT DEFAULT HOOKS ;;;;
;;;; Each is overridden by the capability that owns it; absent capabilities keep the default.
;;;; An optional type with no objects does not remove or skip one of these definitions. The
;;;; hook remains callable and returns its neutral result. Only an action or quantifier that
;;;; enumerates the empty type produces no instantiations/calls.


(define-query direct-beam-reaches-receiver (?receiver receiver)
  (do ?receiver nil))


(define-query relay-beam-reaches-receiver (?receiver receiver)
  (do ?receiver nil))


(define-query recording-shadow-direct-beam-reaches-receiver (?receiver receiver)
  (do ?receiver nil))


(define-query recording-shadow-relay-beam-reaches-receiver (?receiver receiver)
  (do ?receiver nil))


(define-query direct-beam-live-for-cutting (?from beam-node ?to beam-node)
  (do ?from ?to nil))


(define-query relay-beam-live-for-cutting
    (?from beam-node
     ?to beam-node
     ?lighting)
  (do ?from ?to ?lighting nil))


(define-query beam-cut
    (?from beam-node
     ?to beam-node)
  (do ?from ?to nil))


(define-query beam-cut-in
    (?from beam-node
     ?to beam-node
     ?active)
  (do ?from ?to ?active nil))


(define-query current-crossing-set ()
  nil)


(define-query fixed-beam-corridor-clear (?from beam-node ?to beam-node)
  (do ?from ?to nil))


(define-query fixed-beam-corridor-clear-for-object
    (?view ?from beam-node ?to beam-node)
  (do ?view ?from ?to nil))


(define-query compute-relay-lighting (?active)
  (do ?active nil))


(define-query beam-relay-source-distance (?from beam-node ?lighting)
  (do ?from ?lighting most-positive-fixnum))

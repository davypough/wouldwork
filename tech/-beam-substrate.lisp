;;; Filename: -beam-substrate.lisp

;;; Beam substrate: the shared interface every beam capability programs against.  It owns
;;; the receiver-status driver, the direct/relay arrival and cut-liveness orchestrators,
;;; the transmitter/receiver has-chroma and receiver-active relations, and a null-object default for every
;;; pluggable hook.  beam-direct, beam-relay, and beam-crossing each (include-tech -beam-substrate)
;;; and override only their own slots, so they compose in any combination -- and any single
;;; capability remains independent of the others.  The null defaults make absent capabilities
;;; resolve to "contributes nothing": no direct arrival, no relay arrival, no cuts, an empty
;;; crossing set, no connector lighting.
;;;
;;; Self-contained; spliced by (include-tech -beam-substrate).
;;;
;;; REQUIRES:
;;;   driver    : propagate-consequences! must call update-receiver-status!  (hue, transmitter,
;;;               and receiver are declared optional here via define-optional-types)
;;; PROVIDES:
;;;   types     : hue, transmitter, receiver  --  declared optional here; other techs
;;;               (beam-relay, beam-direct, gate, visibility, etc.) independently declare
;;;               their own hue-alias/transmitter-alias/receiver-alias for their own
;;;               pre-params; the bare and aliased forms resolve compatibly
;;;   relations : (active receiver), has-chroma
;;;   queries   : beam-reaches-receiver, beam-live-for-cutting,
;;;               plus null-object defaults for direct-beam-reaches-receiver,
;;;               relay-beam-reaches-receiver, direct-beam-live-for-cutting,
;;;               relay-beam-live-for-cutting, beam-cut, beam-cut-in,
;;;               current-crossing-set, compute-connector-lighting,
;;;               beam-relay-source-distance
;;;   update    : update-receiver-status!

(in-package :ww)


(define-optional-types hue transmitter receiver)


(define-dynamic-relations
  (active receiver))


(define-static-relations
  (has-chroma (either transmitter receiver) $hue))  ;a transmitter/receiver's fixed hue, read by direct and relay


(define-update update-receiver-status! ()
  ;; A receiver is active iff a chroma-matching direct or relay beam reaches it.
  (doall (?r receiver)
    (if (beam-reaches-receiver ?r)
      (active ?r)
      (not (active ?r)))))


(define-query beam-reaches-receiver (?receiver receiver)
  (or (direct-beam-reaches-receiver ?receiver)
      (relay-beam-reaches-receiver ?receiver)))


(define-query beam-live-for-cutting (?from ?to ?lighting)
  (or (direct-beam-live-for-cutting ?from ?to)
      (relay-beam-live-for-cutting ?from ?to ?lighting)))


;;;; NULL-OBJECT DEFAULT HOOKS ;;;;
;;;; Each is overridden by the capability that owns it; absent capabilities keep the default.


(define-query direct-beam-reaches-receiver (?receiver receiver)
  (do ?receiver nil))


(define-query relay-beam-reaches-receiver (?receiver receiver)
  (do ?receiver nil))


(define-query direct-beam-live-for-cutting (?from transmitter ?to receiver)
  (do ?from ?to nil))


(define-query relay-beam-live-for-cutting (?from ?to ?lighting)
  (do ?from ?to ?lighting nil))


(define-query beam-cut (?from ?to)
  (do ?from ?to nil))


(define-query beam-cut-in (?from ?to ?active)
  (do ?from ?to ?active nil))


(define-query current-crossing-set ()
  nil)


(define-query compute-connector-lighting (?active)
  (do ?active nil))


(define-query beam-relay-source-distance (?from ?lighting)
  (do ?from ?lighting most-positive-fixnum))

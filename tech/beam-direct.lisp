;;; Filename: beam-direct.lisp

;;; Fixed coupled beam technology.  It activates direct transmitter -> receiver links and
;;; supplies shared corridor clearance for every directional fixed link whose endpoints are
;;; transmitter/repeater -> repeater/receiver.  Location occluders are elevation-aware,
;;; interpolating between unequal endpoint anchors when visibility supplies coordinates.
;;; A problem with crossing fixed beams includes beam-crossing alongside this technology.
;;;
;;; Self-contained; spliced by (include-tech beam-direct).
;;;
;;; REQUIRES:
;;;   types  : location, hue, agent -- transmitter, receiver, repeaters, box, jammer,
;;;            connector, and plate are optional through the nested roles
;;;   nested : -beam-substrate (coupled, beam-via, receiver and beam hooks);
;;;            -beam-occlusion (beam-blocker-occludes-location);
;;;            -elevation (apparatus-anchor-elevation);
;;;            -beam-interpolation (horizontal default, overridden by visibility for
;;;            sloped beams); -gate (open)
;;; PROVIDES:
;;;   queries : direct-beam-reaches-receiver, fixed-beam-elevation-at, beam-clear,
;;;             fixed-beam-corridor-clear, direct-beam-live-for-cutting

(include-tech -beam-substrate)
(include-tech -beam-occlusion)
(include-tech -elevation)
(include-tech -beam-interpolation)
(include-tech -gate)

(in-package :ww)


(define-optional-types transmitter receiver box jammer connector plate)


(define-query direct-beam-reaches-receiver (?receiver receiver)
  (do (assign $reaches nil)
      (doall (?t transmitter)
        (if (and (coupled ?t ?receiver)
                 (bind (has-chroma ?t $source-hue))
                 (bind (has-chroma ?receiver $required-hue))
                 (eql $source-hue $required-hue)
                 (fixed-beam-corridor-clear ?t ?receiver)
                 (not (beam-cut ?t ?receiver)))
          (assign $reaches t)))
      $reaches))


(define-query fixed-beam-elevation-at
    (?from fixed-beam-source ?obstacle location ?to fixed-beam-target)
  (do (assign $from-elevation (apparatus-anchor-elevation ?from))
      (assign $to-elevation (apparatus-anchor-elevation ?to))
      (beam-elevation-at-location
        ?obstacle ?from $from-elevation ?to $to-elevation)))


(define-query beam-clear
    (?from fixed-beam-source
     ?obstacle (either gate location)
     ?to fixed-beam-target)
  ;; Closed gates block outright.  A location blocks only when one of its beam-blocking
  ;; occupants spans the fixed beam's interpolated elevation there.
  (if (gate ?obstacle)
    (open ?obstacle)
    (not (beam-blocker-occludes-location
           ?obstacle (fixed-beam-elevation-at ?from ?obstacle ?to)))))


(define-query fixed-beam-corridor-clear (?from beam-node ?to beam-node)
  (and (or (transmitter ?from) (repeater ?from))
       (or (repeater ?to) (receiver ?to))
       (coupled ?from ?to)
       (bind (beam-via ?from $obstacles ?to))
       (ww-loop for $o in $obstacles
                always (beam-clear ?from $o ?to))))


(define-query direct-beam-live-for-cutting (?from beam-node ?to beam-node)
  ;; A transmitter emits every coupled fixed beam whose authored corridor is clear.
  ;; Repeater-origin beams are gated by relay lighting in beam-relay's liveness hook.
  (and (transmitter ?from)
       (fixed-beam-corridor-clear ?from ?to)))

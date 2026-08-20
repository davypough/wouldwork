;;; Filename: beam-direct.lisp

;;; Fixed coupled beam technology.  It activates direct transmitter -> receiver links and
;;; supplies shared corridor clearance for every directional fixed link whose endpoints are
;;; transmitter/repeater -> repeater/receiver.  Coordinate-derived wall, edge, gate, and
;;; boundary crossings are recorded once during initialization and checked against the
;;; beam's live endpoint elevations at runtime.  Authored location occluders remain
;;; elevation-aware and interpolate between unequal endpoint anchors.
;;; A problem with crossing fixed beams includes beam-crossing alongside this technology.
;;;
;;; Self-contained; spliced by (include-tech beam-direct).
;;;
;;; REQUIRES:
;;;   types  : location, hue, agent -- transmitter, receiver, repeaters, box, jammer,
;;;            connector, and plate are optional through the nested roles
;;;   nested : -beam-substrate (coupled, beam-via, receiver and beam hooks);
;;;            visibility (segment-crossing records, coordinate interpolation, gate state,
;;;            apparatus elevation, and movable beam occlusion)
;;; PROVIDES:
;;;   init    : derive-fixed-beam-barrier-crossings
;;;   queries : direct-beam-reaches-receiver, recording-shadow-direct-beam-reaches-receiver,
;;;             fixed-beam-elevation-at, fixed-beam-recorded-barriers-clear-for-object,
;;;             beam-clear, beam-clear-for-object,
;;;             fixed-beam-corridor-clear, fixed-beam-corridor-clear-for-object,
;;;             direct-beam-live-for-cutting

(include-tech -beam-substrate)
(include-tech visibility)

(in-package :ww)


(define-optional-types transmitter receiver box jammer connector)


(define-init-action derive-fixed-beam-barrier-crossings
  ;; A fixed coupling has no location endpoint, so DERIVE-LOS-FROM-SEGMENTS's ordinary LOS
  ;; families do not encounter it.  Record its static segment crossings explicitly here.
  ;; Gate crossings are retained too: a coordinate-known closed gate blocks only through its
  ;; finite vertical span, while a hand-authored BEAM-VIA gate with no crossing geometry
  ;; keeps the legacy open-only rule in FIXED-BEAM-CORRIDOR-CLEAR-FOR-OBJECT.
  0
  ()
  (and (exists (?source fixed-beam-source)
         (exists (?sink fixed-beam-sink)
           (coupled ?source ?sink)))
       (or (exists (?wall wall)
             (bind (wall-segment> ?wall $x1 $y1 $x2 $y2)))
           (exists (?edge edge)
             (bind (edge-segment> ?edge $x1 $y1 $x2 $y2)))
           (exists (?gate gate)
             (bind (gate-segment> ?gate $x1 $y1 $x2 $y2)))
           (bind (boundary-wall $some-boundary-points))))
  ()
  (assert
    (do (assign $positions (beam-coordinates-endpoint-positions))
        (assign $walls (wall-segment-records))
        (assign $edges (edge-segment-records))
        (assign $gates (gate-segment-records))
        (assign $boundary-segments
                (if (bind (boundary-wall $boundary-points))
                  (beam-coordinates-boundary-segments $boundary-points)))
        (ww-loop for $beam in (beam-coordinates-coupled-beams)
                 do (assign $source (first $beam))
                    (assign $sink (second $beam))
                    (assign $crossings
                            (beam-coordinates-barrier-crossings
                              $beam $positions $walls $edges $boundary-segments $gates))
                    (los-barrier-crossings> $source $crossings $sink))
        (convert-databases-to-integers))))


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


(define-query recording-shadow-direct-beam-reaches-receiver
    (?view ?receiver receiver)
  (do (assign $reaches nil)
      (doall (?transmitter transmitter)
        (if (and (coupled ?transmitter ?receiver)
                 (bind (has-chroma ?transmitter $source-hue))
                 (bind (has-chroma ?receiver $required-hue))
                 (eql $source-hue $required-hue)
                 (fixed-beam-corridor-clear-for-object
                   ?view ?transmitter ?receiver)
                 (not (beam-cut ?transmitter ?receiver)))
          (assign $reaches t)))
      $reaches))


(define-query fixed-beam-elevation-at
    (?from fixed-beam-source ?obstacle location ?to fixed-beam-sink)
  (do (assign $from-elevation (top ?from))
      (assign $to-elevation (top ?to))
      (beam-elevation-at-location
        ?obstacle ?from $from-elevation ?to $to-elevation)))


(define-query beam-clear
    (?from fixed-beam-source
     ?obstacle (either gate location)
     ?to fixed-beam-sink)
  ;; Legacy authored-obstacle behavior: a gate without matching crossing geometry clears
  ;; only when open.  FIXED-BEAM-CORRIDOR-CLEAR-FOR-OBJECT handles coordinate-recorded gates
  ;; separately as finite-height barriers.  A location blocks only when one of its beam-
  ;; blocking occupants spans the fixed beam's interpolated elevation there.
  (beam-clear-for-object nil ?from ?obstacle ?to))


(define-query beam-clear-for-object
    (?view
     ?from fixed-beam-source
     ?obstacle (either gate location)
     ?to fixed-beam-sink)
  (if (gate ?obstacle)
    (gate-open-for-object ?view ?obstacle)
    (not (if (recording-shadow-object ?view)
           (beam-blocker-occludes-location-for-object
             ?view ?obstacle (fixed-beam-elevation-at ?from ?obstacle ?to))
           (beam-blocker-occludes-location
             ?obstacle (fixed-beam-elevation-at ?from ?obstacle ?to))))))


(define-query fixed-beam-corridor-clear (?from beam-node ?to beam-node)
  (fixed-beam-corridor-clear-for-object nil ?from ?to))


(define-query fixed-beam-recorded-barriers-clear-for-object
    (?view
     ?from fixed-beam-source
     ?crossings
     ?to fixed-beam-sink)
  (do (assign $from-elevation (top ?from))
      (assign $to-elevation (top ?to))
      (recorded-barriers-clear-for-object
        ?view ?crossings $from-elevation $to-elevation)))


(define-query fixed-beam-corridor-clear-for-object
    (?view ?from beam-node ?to beam-node)
  (and (or (transmitter ?from) (repeater ?from))
       (or (repeater ?to) (receiver ?to))
       (coupled ?from ?to)
       (bind (beam-via ?from $obstacles ?to))
       (assign $crossings (los-barrier-crossings ?from ?to))
       (or (eql $crossings :unrecorded)
           (fixed-beam-recorded-barriers-clear-for-object
             ?view ?from $crossings ?to))
       (ww-loop for $o in $obstacles
                always
                  (if (gate $o)
                    ;; A coordinate-recorded gate was evaluated at its exact crossing above.
                    ;; An authored gate without matching geometry keeps the legacy rule.
                    (if (and (not (eql $crossings :unrecorded))
                             (find $o $crossings :key #'second :test #'eql))
                      t
                      (gate-open-for-object ?view $o))
                    (beam-clear-for-object ?view ?from $o ?to)))))


(define-query direct-beam-live-for-cutting (?from beam-node ?to beam-node)
  ;; A transmitter emits every coupled fixed beam whose authored corridor is clear.
  ;; Repeater-origin beams are gated by relay lighting in beam-relay's liveness hook.
  (and (transmitter ?from)
       (fixed-beam-corridor-clear ?from ?to)))

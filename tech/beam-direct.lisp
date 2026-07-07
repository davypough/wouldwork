;;; Filename: beam-direct.lisp

;;; Direct beam technology: fixed transmitter -> receiver beams with authored corridor
;;; occluders.  Location occluders are elevation-aware: an object blocks only if its vertical
;;; span intersects the fixed transmitter/receiver beam elevation.  A peer capability over
;;; -beam-substrate -- it includes -beam-substrate and
;;; overrides the direct arrival and cut-liveness hooks, adding the direct-only wiring
;;; (coupled, beam-via).  The shared interface (has-chroma/active relations, the receiver-status
;;; driver, the arrival/cut orchestrators, and the null-object hook defaults) lives in
;;; -beam-substrate.
;;;
;;; Self-contained; spliced by (include-tech beam-direct).
;;;
;;; REQUIRES:
;;;   types : location, hue, agent  --  gate, transmitter, receiver, box, jammer, connector,
;;;           and plate are declared optional here (define-optional-types).  Gate is
;;;           coordinated with gate, accessibility, visibility, reachability, and
;;;           beam-crossing, which all convert gate together since they share the
;;;           (open gate) relation verbatim.  Box/jammer/connector are beam blockers;
;;;           plate may appear as a non-raising support.
;;;   nested : -location (mobile-object, (has-location ...)); -support-occupancy
;;;            (support-occupant, support, (on ...)); -height (heighted-object, has-height,
;;;            declared-height); -elevation (elevated-object, has-elevation,
;;;            fixture-elevation, location-elevation) -- shared via nested include-tech
;;;            rather than local declaration
;;; PROVIDES:
;;;   types     : beam-blocker (either agent box jammer connector)  --  sole consumer; not
;;;               declared elsewhere
;;;               gate, transmitter, receiver  --  declared optional here; other techs
;;;               (-beam-substrate, beam-relay, beam-crossing, visibility, gate, etc.)
;;;               independently declare their own gate-alias/transmitter-alias/receiver-alias
;;;               for their own pre-params; the bare and aliased forms resolve compatibly
;;;   relations : (open gate)  --  also declared identically by gate, accessibility,
;;;               visibility, and reachability; only gate's update-gate-status!
;;;               ever asserts it
;;;               coupled, beam-via
;;;   queries   : direct-beam-reaches-receiver, direct-beam-elevation, beam-clear,
;;;               beam-blocker-base-elevation, beam-blocker-top-elevation
;;;               (reads -height.lisp's declared-height for a blocker's default unit height),
;;;               beam-blocker-intersects-beam, direct-beam-live-for-cutting
;;;               (overriding -beam-substrate null-object defaults)

(include-tech -beam-substrate)
(include-tech -location)
(include-tech -support-occupancy)
(include-tech -height)
(include-tech -elevation)

(in-package :ww)


(define-types
  beam-blocker (either agent box jammer connector))  ;what can block/occlude a beam path; sole consumer of this type


(define-optional-types gate transmitter receiver box jammer connector plate)


(define-dynamic-relations
  (open gate))  ;also declared by gate/accessibility/visibility/reachability; only gate writes it


(define-static-relations
  (coupled transmitter receiver)  ;static beam source -> target pairing
  (beam-via transmitter $list receiver))  ;direct beam corridor: open gates and clear locations


(define-query direct-beam-reaches-receiver (?receiver receiver)
  (do (assign $reaches nil)
      (doall (?t transmitter)
        (if (and (coupled ?t ?receiver)
                 (bind (has-chroma ?t $source-hue))
                 (bind (has-chroma ?receiver $required-hue))
                 (eql $source-hue $required-hue)
                 (bind (beam-via ?t $obstacles ?receiver))
                 (ww-loop for $o in $obstacles
                          always (beam-clear ?t $o ?receiver))
                 (not (beam-cut ?t ?receiver)))
          (assign $reaches t)))
      $reaches))


(define-query direct-beam-elevation (?from transmitter ?to receiver)
  ;; Direct beams are currently horizontal.  Init validation rejects mismatched endpoint
  ;; elevations, so either endpoint gives the same beam level here.
  (do ?to
      (fixture-elevation ?from)))


(define-query beam-clear (?from transmitter ?obstacle (either gate location) ?to receiver)
  ;; A corridor obstacle is clear iff it is an open gate, or -- being a corridor
  ;; location -- has no beam-blocking object whose vertical span intersects this beam.
  (if (gate ?obstacle)
    (open ?obstacle)
    (not (exists (?obj beam-blocker)
           (and (has-location ?obj ?obstacle)
                (beam-blocker-intersects-beam ?obj ?from ?to))))))


(define-query beam-blocker-intersects-beam
    (?blocker beam-blocker ?from transmitter ?to receiver)
  (do (assign $beam-level (direct-beam-elevation ?from ?to))
      (assign $base-level (beam-blocker-base-elevation ?blocker))
      (assign $top-level (beam-blocker-top-elevation ?blocker))
      (and (<= $base-level $beam-level)
           (<= $beam-level $top-level))))


(define-query beam-blocker-base-elevation (?blocker beam-blocker)
  ;; A blocker resting on a box starts at that box's top; otherwise it starts at its
  ;; location floor.  Plate support does not raise the blocker above the location floor.
  (if (and (bind (on ?blocker $support))
           (box $support))
    (beam-blocker-top-elevation $support)
    (do (bind (has-location ?blocker $location))
        (location-elevation $location))))


(define-query beam-blocker-top-elevation (?blocker beam-blocker)
  ;; Blocker's own default unit height comes from -height.lisp's shared declared-height,
  ;; mirroring box/agent/jammer's default of 1 unless declared otherwise.
  (+ (beam-blocker-base-elevation ?blocker)
     (declared-height ?blocker)))


(define-query direct-beam-live-for-cutting (?from transmitter ?to receiver)
  ;; Cutting depends on emitted light, not arrival at the final receiver.  For a direct
  ;; transmitter -> receiver beam, emission is live whenever the authored corridor clears.
  (and (transmitter ?from)
       (receiver ?to)
       (bind (beam-via ?from $obstacles ?to))
       (ww-loop for $o in $obstacles
                always (beam-clear ?from $o ?to))))

;;; Filename: beam-direct.lisp

;;; Direct beam technology: fixed transmitter -> receiver beams with authored corridor
;;; occluders.  Location occluders are elevation-aware: an object blocks only if its vertical
;;; span intersects the fixed transmitter/receiver beam elevation.  A peer capability over
;;; -beam-substrate -- it includes -beam-substrate and
;;; overrides the direct arrival and cut-liveness hooks, adding the direct-only wiring
;;; (coupled, beam-via).  The shared interface (has-chroma/active relations, the receiver-status
;;; driver, the arrival/cut orchestrators, and the null-object hook defaults) lives in
;;; -beam-substrate.  The elevation-aware occlusion test itself -- the beam-blocker type and
;;; the vertical-span check -- lives in -beam-occlusion, so the same primitive is available to
;;; any other beam or sightline capability that needs it; visibility is a second consumer once
;;; its own occluder lists gain location entries alongside gates.  A problem that would rather
;;; author 2D positions than hand-list sightline occluders gets that derivation from
;;; visibility-tech, which nests -beam-los-coordinates (the owner of the los relations owns
;;; their coordinate derivation); this file no longer nests it itself.  A problem with two
;;; direct beams that can cross also includes beam-crossing alongside beam-direct.
;;;
;;; Self-contained; spliced by (include-tech beam-direct).
;;;
;;; REQUIRES:
;;;   types : location, hue, agent  --  transmitter, receiver, box, jammer, connector, and
;;;           plate are declared optional here (define-optional-types); gate comes from
;;;           nested -gate; beam-blocker (either agent box jammer connector) comes from
;;;           nested -beam-occlusion.  Plate may appear as a non-raising support.
;;;   nested : -beam-occlusion (beam-blocker type, beam-blocker-occludes-location);
;;;            -elevation (elevated-object, has-elevation, fixture-elevation,
;;;            location-elevation); -gate (gate optional type, (open gate) relation) --
;;;            shared with gate, accessibility (via -passability), reachability,
;;;            visibility, and beam-crossing, which all nest -gate instead of
;;;            hand-declaring it
;;; PROVIDES:
;;;   types     : transmitter, receiver  --  declared optional here; other techs
;;;               (-beam-substrate, beam-relay, beam-crossing, visibility, gate, etc.)
;;;               independently declare their own transmitter-alias/receiver-alias
;;;               for their own pre-params; the bare and aliased forms resolve compatibly
;;;   relations : coupled, beam-via
;;;   queries   : direct-beam-reaches-receiver, direct-beam-elevation, beam-clear,
;;;               direct-beam-live-for-cutting (overriding -beam-substrate null-object
;;;               defaults)

(include-tech -beam-substrate)
(include-tech -beam-occlusion)
(include-tech -elevation)
(include-tech -gate)

(in-package :ww)


(define-optional-types transmitter receiver box jammer connector plate)


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
  ;; location -- carries no beam-blocking object whose vertical span intersects this
  ;; beam's elevation.  The occlusion test itself is -beam-occlusion's shared primitive,
  ;; also used by visibility for sightline occluders.
  (if (gate ?obstacle)
    (open ?obstacle)
    (not (beam-blocker-occludes-location ?obstacle (direct-beam-elevation ?from ?to)))))


(define-query direct-beam-live-for-cutting (?from transmitter ?to receiver)
  ;; Cutting depends on emitted light, not arrival at the final receiver.  For a direct
  ;; transmitter -> receiver beam, emission is live whenever the authored corridor clears.
  (and (transmitter ?from)
       (receiver ?to)
       (bind (beam-via ?from $obstacles ?to))
       (ww-loop for $o in $obstacles
                always (beam-clear ?from $o ?to))))

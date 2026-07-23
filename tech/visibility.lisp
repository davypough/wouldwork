;;; Filename: visibility.lisp

;;; Visibility background capability: whether a fixture or another location is in sight from a
;;; location.  In this file, a fixture is a fixed LOS object: a gate, transmitter, or
;;; receiver.  Sightlines are split by consuming role, not just by object kind: LOS-TO-
;;; TRANSCEIVER covers a transceiver (transmitter or receiver), the beam endpoints beam-relay/
;;; beam-direct pair or relay to; LOS-TO-TARGET covers gate, what jammer's jam-target aims at.
;;; A connector-to-connector pairing never consults either -- it resolves through a connector's
;;; own location via LOS-TO-LOCATION instead.  A sightline must exist in the los tables and is
;;; clear iff every occluder gate is open.
;;;
;;; The los tables may be hand-authored, or -- when the problem asserts WALL-SEGMENTS --
;;; derived from raw 2D segment geometry by nested -beam-los-coordinates (entirely inert
;;; otherwise), mirroring accessibility's own nested -accessibility-coordinates deriving
;;; WALK-VIA.  This file owns the los relations, so it owns their coordinate derivation too;
;;; beam-direct, beam-relay, and beam-crossing all consume sightlines through the visible
;;; interface without nesting the derivation themselves (beam-crossing's own
;;; -beam-crossing-coordinates re-nests it only to guarantee LOS derivation splices before
;;; its crossing derivation; splicing is deduplicated, so that is never a second copy).
;;;
;;; REQUIRES:
;;;   types     : location  --  gate, transmitter, receiver, and transceiver are declared
;;;               optional/composite here through nested -visibility
;;;   nested    : -visibility (fixture, transceiver, and the null-default visible interface);
;;;               -gate (gate optional type, (open gate) relation) -- shared with gate,
;;;               accessibility (via -passability), reachability, beam-direct, and
;;;               beam-crossing, which all nest -gate instead of hand-declaring it;
;;;               -beam-los-coordinates (BEAM-ENDPOINT type; TRANSCEIVER-POSITION>,
;;;               WALL-SEGMENTS, GATE-SEGMENTS, BOUNDARY-WALL; DERIVE-LOS-FROM-SEGMENTS)
;;; PROVIDES:
;;;   relations : (los-to-transceiver location $list transceiver),
;;;               (los-to-target location $list gate),
;;;               (los-to-location location $list location)
;;;   queries   : visible and potentially-visible (override -visibility's null defaults),
;;;               visible-clear

(include-tech -visibility)
(include-tech -gate)
(include-tech -beam-los-coordinates)

(in-package :ww)


(define-static-relations
  (los-to-transceiver location $list transceiver)  ;per-location occluders on a sightline to a connector/beam-relay transceiver
  (los-to-target location $list gate)  ;per-location occluders on a sightline to a jammer target
  (los-to-location location $list location))  ;symmetric per-pair occluders for location-to-location sightlines


(define-query visible (?location location ?object (either fixture location))
  ;; A sightline must exist (an empty occluder list is a direct, always-clear line); it is
  ;; clear iff every occluder is transparent.  Agent-independent.  ?object is a transceiver
  ;; (los-to-transceiver), a jammer target (los-to-target), or another location (los-to-location);
  ;; at most one matches, so try all three in turn.
  (and (or (bind (los-to-transceiver ?location $occluders ?object))
           (bind (los-to-target ?location $occluders ?object))
           (bind (los-to-location ?location $occluders ?object)))
       (ww-loop for $o in $occluders
                always (visible-clear $o))))


(define-query potentially-visible (?location location ?object (either fixture location))
  ;; Structural LOS ignores whether its authored gate occluders are currently open.  Relay
  ;; pairing selection uses this query; live beams and all other operational sight checks use
  ;; visible instead.
  (or (bind (los-to-transceiver ?location $occluders ?object))
      (bind (los-to-target ?location $occluders ?object))
      (bind (los-to-location ?location $occluders ?object))))


(define-query visible-clear (?occluder gate)
  ;; Per-kind transparency for one occluder.  Claustro sightlines pass only through gates; the
  ;; intervening-occupied-location branch is the documented extension.
  (and (gate ?occluder)
       (open ?occluder)))

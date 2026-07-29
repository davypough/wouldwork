;;; Filename: visibility.lisp

;;; Visibility background capability: whether a fixture or another location is in sight from a
;;; location.  In this file, a fixture is a fixed LOS object: a gate, transmitter, or
;;; receiver.  Sightlines are split by consuming role, not just by object kind: LOS-TO-
;;; APPARATUS covers an apparatus (transmitter or receiver), the beam endpoints beam-relay/
;;; beam-direct pair or relay to; LOS-TO-TARGET covers gate, what jammer's jam-target aims at
;;; (a gears jam target instead resolves through its HAS-POSITION location's ordinary
;;; LOS-TO-LOCATION entry -- gears hang at a location, not along a segment -- so
;;; LOS-TO-TARGET remains gate-only).
;;; A connector-to-connector pairing never consults either -- it resolves through a connector's
;;; own location via LOS-TO-LOCATION instead.  A sightline must exist in the los tables and is
;;; clear iff every occluder gate is open.  A LOS-TO-APPARATUS/LOS-TO-LOCATION occluder list may
;;; also carry location entries (-beam-los-coordinates' own location-occlusion test, within
;;; *beam-occlusion-tolerance*); VISIBLE itself always treats a location occluder as clear --
;;; it stays the agent-independent, endpoint-elevation-blind structural check jam-target and
;;; potentially-visible both rely on.  BEAM-VISIBLE is the elevation-aware sibling beam-relay's
;;; own hops use instead: given both endpoints' live elevations, it additionally blocks on a
;;; location occluder whose beam-blocker spans the beam's own interpolated elevation there
;;; (-beam-occlusion's beam-blocker-occludes-location, -beam-los-coordinates' live
;;; beam-coordinates-elevation-at).  Never consulted for a jammer target, matching how a
;;; location occluder never appears in LOS-TO-TARGET's own entries.
;;;
;;; The los tables may be hand-authored, or -- when the problem asserts WALL-SEGMENTS --
;;; derived from raw 2D segment geometry by nested -beam-los-coordinates (entirely inert
;;; otherwise), mirroring accessibility's own nested -accessibility-coordinates deriving
;;; WALK-VIA.  This file owns the los relations, so it owns their coordinate derivation too;
;;; beam-direct, beam-relay, and beam-crossing all consume sightlines through the visible
;;; interface without nesting the derivation themselves (beam-crossing's own
;;; -beam-crossing-coordinates re-nests it only to guarantee LOS derivation splices before
;;; its crossing derivation; splicing is deduplicated, so that is never a second copy).  A
;;; hand-authored problem may list a location as an occluder exactly as it would a gate; that
;;; location, and both of the beam's own endpoints, still need LOCATION-COORDS>/APPARATUS-
;;; COORDS> asserted -- beam-visible's elevation interpolation reads them live regardless of
;;; whether the LOS fact itself was hand-authored or WALL-SEGMENTS-derived.
;;;
;;; REQUIRES:
;;;   types     : location  --  gate, transmitter, receiver, and apparatus are declared
;;;               optional/composite here through nested -visibility
;;;   nested    : -visibility (fixture, apparatus, and the null-default visible/beam-visible
;;;               interface);
;;;               -gate (gate optional type, (open gate) relation) -- shared with gate,
;;;               accessibility (via -passability), reachability, beam-direct, and
;;;               beam-crossing, which all nest -gate instead of hand-declaring it;
;;;               -beam-los-coordinates (BEAM-ENDPOINT type; APPARATUS-COORDS>,
;;;               WALL-SEGMENTS, GATE-SEGMENTS, BOUNDARY-WALL; DERIVE-LOS-FROM-SEGMENTS;
;;;               live BEAM-COORDINATES-ELEVATION-AT);
;;;               -beam-occlusion (BEAM-BLOCKER-OCCLUDES-LOCATION)
;;; PROVIDES:
;;;   relations : (los-to-apparatus location $list apparatus)  -- $list items are gate or
;;;               location names,
;;;               (los-to-target location $list gate)  -- $list items are gate names only,
;;;               (los-to-location location $list location)  -- $list items are gate or
;;;               location names
;;;   queries   : visible and potentially-visible (override -visibility's null defaults),
;;;               beam-visible (overrides -visibility's null default), visible-clear

(include-tech -visibility)
(include-tech -gate)
(include-tech -beam-los-coordinates)
(include-tech -beam-occlusion)

(in-package :ww)


(define-static-relations
  (los-to-apparatus location $list apparatus)  ;per-location occluders on a sightline to a connector/beam-relay apparatus
  (los-to-target location $list gate)  ;per-location occluders on a sightline to a jammer target
  (los-to-location location $list location))  ;symmetric per-pair occluders for location-to-location sightlines


(define-query visible (?location location ?object (either fixture location))
  ;; A sightline must exist (an empty occluder list is a direct, always-clear line); it is
  ;; clear iff every occluder gate is open.  Agent-independent and endpoint-elevation-blind:
  ;; a location occluder is always clear here.  ?object is an apparatus (los-to-apparatus), a
  ;; jammer target (los-to-target), or another location (los-to-location); at most one
  ;; matches, so try all three in turn.  beam-visible is the elevation-aware sibling that
  ;; additionally tests a location occluder, for the one consuming role (beam-relay's hops)
  ;; that has both endpoints' live elevations to test it with.
  (and (or (bind (los-to-apparatus ?location $occluders ?object))
           (bind (los-to-target ?location $occluders ?object))
           (bind (los-to-location ?location $occluders ?object)))
       (ww-loop for $o in $occluders
                always (if (gate $o) (visible-clear $o) t))))


(define-query beam-visible (?location ?near-elevation ?object ?far-elevation)
  ;; Bare parameter list, not the fully-typed pre-param style: ?location/?object mix
  ;; location and fixture objects with plain rational elevation values, and
  ;; check-precondition-parameters requires every parameter typed or none of them --
  ;; mirroring beam-relay.lisp's own relay-beam-live-for-cutting/beam-relay-source-distance.
  ;;
  ;; Elevation-aware sibling of visible, for a relay hop whose two live endpoint elevations
  ;; the caller already knows -- occupant-elevation of the specific connector at each end, or
  ;; fixture-elevation for a transmitter/receiver.  No los-to-target branch here: a jammer
  ;; target never carries a location occluder to test in the first place, so this query is
  ;; simply never the right one for jam-target to call.  A gate occluder is covered by the
  ;; visible call; a location occluder additionally blocks iff some beam-blocker there spans
  ;; the beam's own interpolated elevation at that point.
  (and (visible ?location ?object)
       (or (bind (los-to-apparatus ?location $occluders ?object))
           (bind (los-to-location ?location $occluders ?object)))
       (ww-loop for $o in $occluders
                always (or (gate $o)
                           (not (beam-blocker-occludes-location
                                  $o
                                  (beam-coordinates-elevation-at
                                    $o ?location ?near-elevation ?object ?far-elevation)))))))


(define-query potentially-visible (?location location ?object (either fixture location))
  ;; Structural LOS ignores whether its authored gate occluders are currently open.  Relay
  ;; pairing selection uses this query; live beams and all other operational sight checks use
  ;; visible instead.
  (or (bind (los-to-apparatus ?location $occluders ?object))
      (bind (los-to-target ?location $occluders ?object))
      (bind (los-to-location ?location $occluders ?object))))


(define-query visible-clear (?occluder gate)
  ;; Gate transparency for one occluder.  The intervening-occupied-location branch visible
  ;; and beam-visible each dispatch around directly, rather than through this query, since a
  ;; location occluder's elevation-aware test needs the beam's own interpolated elevation at
  ;; that point -- visible-clear's single-occluder signature has nowhere to carry that.
  (and (gate ?occluder)
       (open ?occluder)))

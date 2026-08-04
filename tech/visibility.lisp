;;; Filename: visibility.lisp

;;; Visibility background capability: whether a fixture or another location is in sight from
;;; a location.  LOS-TO-APPARATUS covers point apparatus (transmitter, receiver, repeater,
;;; or gun); LOS-TO-TARGET covers gate, what jammer's jam-target aims at
;;; (a gears jam target instead resolves through its HAS-POSITION location's ordinary
;;; LOS-TO-LOCATION entry -- gears hang at a location, not along a segment -- so
;;; LOS-TO-TARGET remains gate-only).
;;; A connector-to-connector pairing never consults either -- it resolves through a connector's
;;; own location via LOS-TO-LOCATION instead.  A sightline must exist in the los tables and is
;;; clear iff every occluder gate is open.  A LOS-TO-APPARATUS/LOS-TO-LOCATION occluder list may
;;; also carry location entries (-beam-los-coordinates' own location-occlusion test, within
;;; *beam-occlusion-tolerance*); VISIBLE itself always treats a location occluder as clear.
;;; VISIBLE is the ordinary playback view; VISIBLE-FOR-OBJECT selects gate openness for an
;;; actor's playback or recording view.  POTENTIALLY-VISIBLE remains actor-independent
;;; because it ignores current gate state.  BEAM-VISIBLE is the elevation-aware sibling
;;; beam-relay's own hops use instead; BEAM-VISIBLE-FOR-OBJECT is its actor/view-aware form.
;;; Given both endpoints' live elevations, it additionally blocks on a
;;; location occluder whose beam-blocker spans the beam's own interpolated elevation there
;;; (-beam-occlusion's beam-blocker-occludes-location, -beam-interpolation's live
;;; beam-elevation-at-location).  Never consulted for a jammer target, matching how a
;;; location occluder never appears in LOS-TO-TARGET's own entries.
;;;
;;; The los tables may be hand-authored, or -- when the problem asserts WALL-SEGMENTS --
;;; derived from raw 2D segment geometry by nested -beam-los-coordinates (entirely inert
;;; otherwise), mirroring walkability's own nested -walkability-coordinates deriving
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
;;;               walkability (via -passability), reachability, beam-direct, and
;;;               beam-crossing, which all nest -gate instead of hand-declaring it;
;;;               -beam-los-coordinates (LOS-ENDPOINT type; APPARATUS-COORDS>,
;;;               WALL-SEGMENTS, GATE-SEGMENTS, BOUNDARY-WALL; DERIVE-LOS-FROM-SEGMENTS;
;;;               live BEAM-COORDINATES-ELEVATION-AT);
;;;               -beam-interpolation (the sloped-beam elevation hook);
;;;               -beam-occlusion (BEAM-BLOCKER-OCCLUDES-LOCATION)
;;; PROVIDES:
;;;   relations : (los-to-apparatus location $list apparatus)  -- $list items are gate or
;;;               location names,
;;;               (los-to-target location $list gate)  -- $list items are gate names only,
;;;               (los-to-location location $list location)  -- $list items are gate or
;;;               location names
;;;   queries   : visible, visible-for-object, and potentially-visible (override
;;;               -visibility's null defaults), beam-visible and beam-visible-for-object
;;;               (override -visibility's null defaults), visible-clear

(include-tech -visibility)
(include-tech -gate)
(include-tech -beam-los-coordinates)
(include-tech -beam-interpolation)
(include-tech -beam-occlusion)

(in-package :ww)


(define-static-relations
  (los-to-apparatus location $list apparatus)  ;per-location occluders on a sightline to a connector/beam-relay apparatus
  (los-to-target location $list gate)  ;per-location occluders on a sightline to a jammer target
  (los-to-location location $list location))  ;symmetric per-pair occluders for location-to-location sightlines


(define-init-check visibility-init-check (literals)
  (check-init-list-relation-items-have-types
    literals 'los-to-apparatus '(gate location))
  (check-init-list-relation-items-have-types
    literals 'los-to-target '(gate))
  (check-init-list-relation-items-have-types
    literals 'los-to-location '(gate location)))


(define-query visible
    (?location location
     ?object (either gate transmitter receiver floor-repeater wall-repeater gun location))
  (visible-for-object nil ?location ?object))


(define-query visible-for-object
    (?view
     ?location location
     ?object (either gate transmitter receiver floor-repeater wall-repeater gun location))
  ;; A sightline must exist (an empty occluder list is a direct, always-clear line); it is
  ;; clear iff every occluder gate is open in ?view's environmental layer.  The NIL view
  ;; used by VISIBLE reads ordinary playback state.  Endpoint-elevation-blind: a location
  ;; occluder is always clear here.  ?object is an apparatus (los-to-apparatus), a jammer
  ;; target (los-to-target), or another location (los-to-location); at most one matches,
  ;; so try all three in turn.  BEAM-VISIBLE-FOR-OBJECT is the elevation-aware sibling that
  ;; additionally tests a location occluder, for consuming roles that carry both endpoints'
  ;; live elevations.
  (and (or (bind (los-to-apparatus ?location $occluders ?object))
           (bind (los-to-target ?location $occluders ?object))
           (bind (los-to-location ?location $occluders ?object)))
       (ww-loop for $o in $occluders
                always (if (gate $o)
                         (gate-open-for-object ?view $o)
                         t))))


(define-query beam-visible
    (?location location
     ?near-elevation
     ?object (either transmitter receiver floor-repeater wall-repeater gun location)
     ?far-elevation)
  ;; Locations/apparatus are Wouldwork objects. Elevations are computed Lisp values and
  ;; therefore deliberately have no Wouldwork object type.
  ;;
  ;; Elevation-aware sibling of visible, for a relay hop whose two live endpoint elevations
  ;; the caller already knows.  No los-to-target branch here: a jammer
  ;; target never carries a location occluder to test in the first place, so this query is
  ;; simply never the right one for jam-target to call.  A gate occluder is covered by the
  ;; visible call; a location occluder additionally blocks iff some beam-blocker there spans
  ;; the beam's own interpolated elevation at that point.
  (beam-visible-for-object
    nil ?location ?near-elevation ?object ?far-elevation))


(define-query beam-visible-for-object
    (?view
     ?location location
     ?near-elevation
     ?object (either transmitter receiver floor-repeater wall-repeater gun location)
     ?far-elevation)
  ;; A beam's recording view uses recording-side gate transparency and excludes mapped
  ;; live blockers.  The ordinary NIL view retains the shared playback environment.
  (and
       (or (bind (los-to-apparatus ?location $occluders ?object))
           (bind (los-to-location ?location $occluders ?object)))
       (ww-loop for $o in $occluders
                always
                  (if (gate $o)
                    (gate-open-for-object ?view $o)
                    (not (if (recording-shadow-object ?view)
                           (beam-blocker-occludes-location-for-object
                             ?view $o
                             (beam-elevation-at-location
                               $o ?location ?near-elevation ?object ?far-elevation))
                           (beam-blocker-occludes-location
                             $o
                             (beam-elevation-at-location
                               $o ?location ?near-elevation ?object ?far-elevation))))))))


(define-query beam-elevation-at-location
    (?location location
     ?from beam-node
     ?near-elevation
     ?to beam-node
     ?far-elevation)
  (if (= ?near-elevation ?far-elevation)
    ?near-elevation
    (beam-coordinates-elevation-at
      ?location ?from ?near-elevation ?to ?far-elevation)))


(define-query potentially-visible
    (?location location
     ?object (either gate transmitter receiver floor-repeater wall-repeater gun location))
  ;; Structural LOS ignores whether its authored gate occluders are currently open.  Relay
  ;; pairing selection uses this query; live beams and all other operational sight checks use
  ;; visible instead.
  (or (bind (los-to-apparatus ?location $occluders ?object))
      (bind (los-to-target ?location $occluders ?object))
      (bind (los-to-location ?location $occluders ?object))))


(define-query visible-clear (?occluder gate)
  ;; Ordinary playback transparency for one gate occluder.  Actor-aware consumers use
  ;; GATE-OPEN-FOR-OBJECT directly because this query's single-occluder signature has no
  ;; view parameter.
  (and (gate ?occluder)
       (open ?occluder)))

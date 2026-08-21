;;; Filename: visibility.lisp

;;; Visibility background capability: whether a fixture or another location is in sight from
;;; a location.  One relation carries every sightline, and one query answers every sightline
;;; question, under a policy:
;;;
;;;   (los-via <endpoint> <occluders> <endpoint>)
;;;
;;; The three relations this replaced -- LOS-TO-APPARATUS, LOS-TO-TARGET, LOS-TO-LOCATION --
;;; differed only in the type of the far endpoint, and every consumer bound all three in
;;; turn and took whichever matched, which is the clearest possible evidence that the
;;; distinction carried no information.  LOS-VIA's two endpoints share one type, so the
;;; engine mirrors it: a sightline is symmetric, which the old shape could only express for
;;; the location-to-location case, where both argument types happened to coincide.
;;; LOS-BARRIER-CROSSINGS> stays directed -- its records carry a parameter measured along
;;; the beam from the near endpoint, so orientation is part of their meaning.
;;;
;;; The occluder payload is a flat conjunction, deliberately unlike -traversal's DNF.  A
;;; traversal may take alternative routes, so its payload is a family of clauses; a
;;; sightline is a straight line and has exactly one path, so every occluder on it must be
;;; transparent.  Nothing here is a "clause".
;;;
;;; An authored sightline always starts at a location -- the spot an agent, connector or
;;; jammer occupies -- and the init check enforces that on the literal.  The far endpoint is
;;; a point apparatus (transmitter, receiver, repeater, gun), a gate that a jammer aims at,
;;; or another location.  A gears jam target instead resolves through its HAS-POSITION
;;; location's own sightline, since gears hang at a location rather than along a segment.  A
;;; connector-to-connector pairing likewise resolves through each connector's location.
;;;
;;; THE THREE POLICIES.  What the old VISIBLE, ELEVATION-VISIBLE-FOR-OBJECT and BEAM-VISIBLE
;;; really differed in was two independent rules, and only three of the four combinations
;;; are used:
;;;
;;;                     segment crossings                 location occluders
;;;   :sight            any non-gate crossing blocks      always clear
;;;   :elevation        tested at the crossing's own      always clear
;;;                     interpolated elevation
;;;   :beam             the same elevation test           blocks when a beam-blocker there
;;;                                                       spans the interpolated elevation
;;;
;;; :SIGHT is ordinary opaque sight, which never clears a wall, edge or boundary by height.
;;; :ELEVATION is height-aware visual sight for a viewer and target whose elevations are
;;; known, used by jammer, and it deliberately does not treat intervening movable occupants
;;; as blockers.  :BEAM is what a relay hop uses, and is the only policy that consults a
;;; location occluder at all.
;;;
;;; A hand-authored LOS fact has no crossing geometry, which LOS-BARRIER-CROSSINGS reports
;;; as :UNRECORDED, distinguishing it from a coordinate-derived sightline whose crossing
;;; list is genuinely empty.  Under every policy an :UNRECORDED sightline falls back to the
;;; legacy rule: its gate occluders must be open.
;;;
;;; The LOS table may be hand-authored, or -- when the problem asserts WALL-SEGMENT>,
;;; EDGE-SEGMENT>, or BOUNDARY-WALL -- derived from raw 2D segment geometry by nested
;;; -beam-los-coordinates (entirely inert otherwise), mirroring walkability's own nested
;;; -walkability-coordinates.  This file owns the relation, so it owns its coordinate
;;; derivation too; beam-direct also records every authored fixed coupling's segment
;;; crossings through this interface; beam-relay and beam-crossing consume location
;;; sightlines.  A hand-authored problem may list a location as an occluder exactly as it
;;; would a gate.  When the two live endpoint anchors differ in elevation, that location
;;; and both beam endpoints need LOCATION-COORDS>/APPARATUS-COORDS> so interpolation can
;;; project the occluder onto the sloped beam.  A horizontal beam returns its shared
;;; elevation directly and needs no coordinates.
;;;
;;; REQUIRES:
;;;   types     : location  --  gate, transmitter, receiver, and apparatus are declared
;;;               optional/composite here through nested -visibility
;;;   nested    : -visibility (apparatus and the null-default visible/beam-visible
;;;               interface); -gate (gate optional type, (open gate) relation);
;;;               -beam-los-coordinates (LOS-ENDPOINT; APPARATUS-COORDS>, the segment
;;;               relations, *BOUNDARY-WALL-HEIGHT*, DERIVE-LOS-FROM-SEGMENTS, live
;;;               BEAM-COORDINATES-ELEVATION-AT); -beam-interpolation (the sloped-beam
;;;               elevation hook); -vertical (base, top);
;;;               -beam-occlusion (BEAM-BLOCKER-OCCLUDES-LOCATION)
;;; PROVIDES:
;;;   relations : (los-via visibility-object $list visibility-object)  --  symmetric;
;;;               $list items are gate or location names,
;;;               (los-barrier-crossings> los-endpoint $list visibility-object) -- oriented
;;;               static wall/edge/gate/boundary crossing records, including fixed couplings
;;;   queries   : los-clear-for-object (the one sightline test, under a policy); visible,
;;;               visible-for-object, potentially-visible, beam-visible,
;;;               beam-visible-for-object, elevation-visible-for-object (all overriding
;;;               -visibility's null defaults); visible-clear

(include-tech -visibility)
(include-tech -gate)
(include-tech -beam-los-coordinates)
(include-tech -beam-interpolation)
(include-tech -vertical)
(include-tech -beam-occlusion)

(in-package :ww)


(define-types
  visibility-object
    (either gate transmitter receiver floor-repeater wall-repeater gun location))


(define-static-relations
  ;; Symmetric: both endpoints share one type, so the engine mirrors each fact.  $list is a
  ;; flat conjunction of gate and location occluders -- every one must be transparent.
  (los-via visibility-object $list visibility-object)
  ;; Directed, oriented records: (:kind identity parameter x1 y1 x2 y2).  An empty list
  ;; marks a coordinate-derived sightline with no segment crossings; absence means the LOS
  ;; fact was hand-authored and retains its legacy gate-only behavior.
  (los-barrier-crossings> los-endpoint $list visibility-object))


(define-init-check visibility-init-check (literals)
  (:consumes gate location)
  (check-init-list-relation-items-have-types
    (positive-init-literals-with-relation 'los-via literals)
    'los-via '(gate location))
  (check-init-los-starts-at-location literals)
  (check-init-los-structure literals))


(define-init-check-helper check-init-los-starts-at-location (literals)
  "An authored sightline runs from the spot an actor occupies to whatever it is looking at,
   so its first argument is a location.  The relation is symmetric and the engine stores the
   mirror image itself, so authoring the reverse form as well is redundant rather than
   wrong -- but authoring one whose *near* end is an apparatus or a gate says nothing anyone
   can stand at, and is refused here."
  (dolist (literal (positive-init-literals-with-relation 'los-via literals))
    (let ((near (second (init-literal-proposition literal))))
      (unless (init-type-member-p near 'location)
        (fail-init-check literal
          "~%A sightline's near endpoint must be a location, not ~S.~%~
           LOS-VIA is symmetric, so the engine already stores the reverse direction; write ~
           each sightline once, from the location an actor occupies."
          near)))))


(define-init-check-helper check-init-los-structure (literals)
  "Reject sightlines with no span and malformed intervening-occluder lists.  Repeating an
   occluder changes nothing, while naming either endpoint as intervening confuses an anchor
   with something between the anchors and can make a beam block itself."
  (dolist (literal (positive-init-literals-with-relation 'los-via literals))
    (destructuring-bind (near occluders far)
        (rest (init-literal-proposition literal))
      (when (eql near far)
        (fail-init-check literal
          "LOS-VIA has the same near and far endpoint: ~S.  A sightline must span two distinct endpoints."
          near))
      (when (/= (length occluders)
                (length (remove-duplicates occluders :test #'eql)))
        (fail-init-check literal
          "LOS-VIA repeats an occluder: ~S.  List each intervening gate or location once."
          occluders))
      (dolist (occluder occluders)
        (when (or (eql occluder near)
                  (eql occluder far))
          (fail-init-check literal
            "LOS-VIA lists endpoint ~S as its own intervening occluder.  Remove it from ~S."
            occluder occluders))))))


;;;; SIGHTLINE TEST ;;;;


(define-query los-clear-for-object
    (?view ?location location ?object visibility-object
     ?near-elevation ?far-elevation ?policy)
  ;; The one sightline test.  A sightline must exist -- an empty occluder list is a direct,
  ;; always-clear line -- and it is clear when its segment crossings and its occluders both
  ;; pass under ?POLICY.  The NIL view reads ordinary playback state; a recording view
  ;; selects recording-side gate transparency and excludes mapped live blockers.
  ;; ?NEAR-ELEVATION and ?FAR-ELEVATION are computed Lisp values with no Wouldwork object
  ;; type, and go unread under :SIGHT.
  (and (bind (los-via ?location $occluders ?object))
       (assign $crossings (los-barrier-crossings ?location ?object))
       (los-crossings-clear-for-object
         ?view $crossings ?near-elevation ?far-elevation ?policy)
       (los-occluders-clear-for-object
         ?view $occluders $crossings ?location ?near-elevation ?object ?far-elevation
         ?policy)))


(define-query los-crossings-clear-for-object
    (?view ?crossings ?near-elevation ?far-elevation ?policy)
  ;; A hand-authored sightline has no crossing geometry at all, so there is nothing to test
  ;; and its occluder list carries the whole rule.  Otherwise ordinary sight refuses any
  ;; crossing that is not a gate, while the two elevation-aware policies test each crossing
  ;; against its barrier's own vertical span.
  (if (eql ?crossings :unrecorded)
    t
    (if (eql ?policy :sight)
      (ww-loop for $crossing in ?crossings
               always (eql (first $crossing) :gate))
      (recorded-barriers-clear-for-object
        ?view ?crossings ?near-elevation ?far-elevation))))


(define-query los-occluders-clear-for-object
    (?view ?occluders ?crossings ?location ?near-elevation ?object ?far-elevation ?policy)
  ;; A gate occluder is checked whenever the sightline carries no crossing geometry, and
  ;; under :SIGHT always -- ordinary sight has no exact crossing parameter to have evaluated
  ;; it at.  A coordinate-derived gate crossing under an elevation policy was already
  ;; evaluated at its exact parameter above, so testing openness again here would double the
  ;; rule.  A location occluder matters only to a beam.
  (ww-loop for $o in ?occluders
           always (if (gate $o)
                    (if (or (eql ?crossings :unrecorded)
                            (eql ?policy :sight))
                      (gate-open-for-object ?view $o)
                      t)
                    (if (eql ?policy :beam)
                      (not (los-location-occluded
                             ?view $o ?location ?near-elevation ?object ?far-elevation))
                      t))))


(define-query los-location-occluded
    (?view ?occluder location ?location location ?near-elevation ?object ?far-elevation)
  ;; True when something standing at ?OCCLUDER spans the beam's own interpolated elevation
  ;; where it passes over that spot.  A recording view excludes mapped live blockers.
  (do (assign $elevation
              (beam-elevation-at-location
                ?occluder ?location ?near-elevation ?object ?far-elevation))
      (if (recording-shadow-object ?view)
        (beam-blocker-occludes-location-for-object ?view ?occluder $elevation)
        (beam-blocker-occludes-location ?occluder $elevation))))


(define-query los-barrier-crossings
    (?from los-endpoint ?to visibility-object)
  ;; :UNRECORDED distinguishes a hand-authored LOS fact from a coordinate-derived LOS
  ;; whose crossing list is genuinely empty.
  (if (bind (los-barrier-crossings> ?from $crossings ?to))
    $crossings
    :unrecorded))


(define-query recorded-barriers-clear-for-object
    (?view ?crossings ?near-elevation ?far-elevation)
  (ww-loop for $crossing in ?crossings
           always (barrier-crossing-clear-for-object
                    ?view $crossing ?near-elevation ?far-elevation)))


(define-query barrier-crossing-clear-for-object
    (?view ?crossing ?near-elevation ?far-elevation)
  (do (assign $kind (first ?crossing))
      (assign $barrier (second ?crossing))
      (assign $parameter (third ?crossing))
      (assign $crossing-elevation
              (+ ?near-elevation
                 (* $parameter (- ?far-elevation ?near-elevation))))
      ;; An open gate has no vertical span.  All other segment barriers use an inclusive
      ;; base-to-top blocking interval: equality with either boundary still blocks.  The
      ;; boundary polygon is the one barrier with no named object, so its base and height
      ;; come from the ground and from -segment-geometry's *BOUNDARY-WALL-HEIGHT* rather
      ;; than from -vertical's per-type table.
      (if (and (eql $kind :gate)
               (gate-open-for-object ?view $barrier))
        t
        (do (assign $base-elevation
                    (if (eql $kind :boundary)
                      0
                      (base $barrier)))
            (assign $top-elevation
                    (if (eql $kind :boundary)
                      *boundary-wall-height*
                      (top $barrier)))
            (or (< $crossing-elevation $base-elevation)
                (> $crossing-elevation $top-elevation))))))


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


;;;; THE THREE POLICIES ;;;;


(define-query visible
    (?location location ?object visibility-object)
  (visible-for-object nil ?location ?object))


(define-query visible-for-object
    (?view ?location location ?object visibility-object)
  ;; Ordinary opaque sight, elevation-blind at both ends: no crossing clears by height and
  ;; a location occluder is always transparent.  The elevations therefore go unread.
  (los-clear-for-object ?view ?location ?object nil nil :sight))


(define-query beam-visible
    (?location location
     ?near-elevation
     ?object (either transmitter receiver floor-repeater wall-repeater gun location)
     ?far-elevation)
  (beam-visible-for-object
    nil ?location ?near-elevation ?object ?far-elevation))


(define-query beam-visible-for-object
    (?view
     ?location location
     ?near-elevation
     ?object (either transmitter receiver floor-repeater wall-repeater gun location)
     ?far-elevation)
  ;; A relay hop, whose two live endpoint elevations the caller already knows.  The only
  ;; policy that consults a location occluder: one blocks when a beam-blocker standing there
  ;; spans the beam's interpolated elevation at that point.
  (los-clear-for-object
    ?view ?location ?object ?near-elevation ?far-elevation :beam))


(define-query elevation-visible-for-object
    (?view
     ?location location
     ?near-elevation
     ?object visibility-object
     ?far-elevation)
  ;; Height-aware visual sight for a viewer and target whose elevations are known -- jammer
  ;; sight.  Unlike a beam, intervening movable occupants do not block it.
  (los-clear-for-object
    ?view ?location ?object ?near-elevation ?far-elevation :elevation))


(define-query potentially-visible
    (?location location ?object visibility-object)
  ;; Structural LOS ignores whether its authored gate occluders are currently open.  Relay
  ;; pairing selection uses this; operational sight uses one of the policies above.
  (bind (los-via ?location $occluders ?object)))


(define-query visible-clear (?occluder gate)
  ;; Ordinary playback transparency for one gate occluder.  Actor-aware consumers use
  ;; GATE-OPEN-FOR-OBJECT directly because this query's single-occluder signature has no
  ;; view parameter.
  (and (gate ?occluder)
       (open ?occluder)))

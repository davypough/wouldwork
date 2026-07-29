;;; Filename: -beam-los-coordinates.lisp

;;; Beam LOS coordinates substrate: derives LOS-TO-APPARATUS/LOS-TO-TARGET/LOS-TO-LOCATION
;;; from raw WALL-SEGMENTS/GATE-SEGMENTS/BOUNDARY-WALL segment geometry, for a problem that
;;; would rather author 2D positions than hand-list sightlines.  Nested under visibility-tech
;;; (the owner of the los relations derived here) and beam-crossing-tech (via
;;; -beam-crossing-coordinates, which re-nests it only to guarantee splice order), so it is
;;; always present wherever either is included; entirely inert unless the problem actually
;;; asserts WALL-SEGMENTS, so a problem that hand-authors its own LOS-TO-APPARATUS/LOS-TO-
;;; TARGET/LOS-TO-LOCATION facts instead is unaffected.  No problem currently takes that
;;; hand-authored path -- corner-topo and claustro-topo both supply WALL-SEGMENTS and derive.
;;;
;;; Endpoint coordinates come from two relations, split by ownership: LOCATION-COORDS>
;;; (nested from -location-coordinates, shared with accessibility-tech's own coordinate
;;; substrate, so a location's position is entered once even when a problem uses both
;;; capabilities) for location endpoints, and APPARATUS-COORDS> (declared here) for
;;; transmitter/receiver/gun endpoints only -- a problem with pure location-to-location
;;; beams and no fixtures never needs APPARATUS-COORDS> at all.  A gun's LOS-TO-
;;; APPARATUS entries are derived the same way a receiver's are (gated on a jammer
;;; being present, like LOS-TO-TARGET, since nothing but jam-target ever reads them).
;;;
;;; DERIVE-LOS-FROM-SEGMENTS tests every location<->apparatus, location<->gate, and
;;; location<->location pair against that geometry, excluding any pair a wall blocks and
;;; recording any gate that properly does as an occluder.  A wall blocks at its own corner
;;; exactly like its interior -- so a sightline can't leak through the junction where it
;;; meets another wall, a gate, or the boundary wall -- while a gate stays strict at its own
;;; corner, since the neighboring wall already covers that point; a beam endpoint that lies
;;; exactly on a wall or gate, corner or interior, is an authoring error (see BEAM-
;;; COORDINATES-OBSTACLE-INTERSECTION-PARAMETER).  A gate has no APPARATUS-COORDS> of its
;;; own -- it is authored as an extended segment, not a point endpoint -- so its LOS-TO-TARGET
;;; entries use BEAM-COORDINATES-GATE-MIDPOINT as a single reference point instead.  When the
;;; problem also asserts BOUNDARY-WALL -- a closed polygon, its final point wrapping back to
;;; the first -- each polygon edge is folded into the wall list too, so a sightline that would
;;; have to leave the map's own silhouette is blocked the same as any other wall; unlike
;;; WALL-SEGMENTS/GATE-SEGMENTS, BOUNDARY-WALL is consulted only here, not by
;;; -accessibility-coordinates.lisp's own WALK-VIA derivation.
;;;
;;; The location<->apparatus and location<->location branches additionally test every other
;;; location as a candidate occluder: BEAM-COORDINATES-LOCATION-OCCLUDES-BEAM projects the
;;; candidate onto the beam's line and accepts it iff that projection falls strictly between
;;; the beam's own two endpoints (a location never occludes a beam it terminates) and its
;;; perpendicular distance from the line is within *BEAM-OCCLUSION-TOLERANCE* (declared below;
;;; default 1/2, a half-unit radius; a problem overrides it with its own DEFPARAMETER).  A
;;; qualifying location is appended to the occluder list as a bare location name, exactly like
;;; a qualifying gate; distances are compared squared throughout to stay in exact rational
;;; arithmetic.  The location<->gate (LOS-TO-TARGET) and location<->gun branches deliberately
;;; do not gain this test -- nothing but jam-target ever reads either, and a jammer's line to
;;; its target is not blocked by intervening objects, by design.
;;;
;;; Declares BEAM-ENDPOINT itself, as (either transmitter receiver gun location) -- the
;;; composite every consuming query/init-action here iterates over.  A problem may also declare it
;;; identically in its own DEFINE-TYPES (as problem-corner-topo does); CHECK-TYPE-SIGNATURE-
;;; CONSISTENCY requires every declaration to resolve to the same instance list, so the
;;; duplicate is harmless.
;;;
;;; Self-contained; spliced by (include-tech -beam-los-coordinates), nested from visibility
;;; and from -beam-crossing-coordinates (itself nested from beam-crossing).  Splicing is
;;; deduplicated per problem copy, so a problem including both visibility and beam-crossing
;;; still gets this file exactly once, and always before -beam-crossing-coordinates' own
;;; ESTABLISH-BEAM-COORDINATES/DERIVE-CROSSINGS-BEFORE-GATE, regardless of which of the two
;;; parent techs the problem lists first.
;;;
;;; REQUIRES:
;;;   types     : location  --  declared by the problem; transmitter, receiver, gun
;;;               declared optional by -visibility/-beam-substrate, sibling nested
;;;               includes of the parent techs
;;;   relations : los-to-apparatus, los-to-target, los-to-location  --  declared by
;;;               visibility-tech, this file's primary parent; a beam-crossing problem
;;;               reaching this file through -beam-crossing-coordinates must still include
;;;               visibility for these relations to exist
;;; PROVIDES:
;;;   nested    : -location-coordinates (LOCATION-COORDS>; shared with accessibility, so
;;;               a location's coordinates are entered once regardless of which
;;;               capabilities the problem uses)
;;;   parameter : *beam-occlusion-tolerance*, default 1/2 -- a Talos-problem default, not a
;;;               core wouldwork setting, so it lives here rather than in ww-settings.lisp;
;;;               a problem overrides it with its own DEFPARAMETER
;;;   types     : beam-endpoint (either transmitter receiver gun location); jammer and gun
;;;               declared optional here only to gate DERIVE-LOS-FROM-SEGMENTS's
;;;               LOS-TO-TARGET/gun LOS-TO-APPARATUS derivations below, so a problem with
;;;               no jammer never gets location<->gate or location<->gun sightlines
;;;               nothing can consume
;;;   relations : apparatus-coords> (transmitter/receiver/gun), wall-segments,
;;;               gate-segments, boundary-wall -- all default to no facts; a problem that
;;;               asserts wall-segments gets LOS-TO-APPARATUS/LOS-TO-TARGET/LOS-TO-LOCATION
;;;               derived automatically instead of hand-authoring them; boundary-wall
;;;               additionally folds its polygon edges into that derivation's wall list
;;;   queries   : beam-coordinates-endpoint-xy, beam-coordinates-elevation-at -- live
;;;               (query-time, not just init-time) coordinate lookup and interpolation,
;;;               read by visibility.lisp's beam-visible; consulted for a hand-authored
;;;               location occluder exactly as for a derived one, so a problem that hand-
;;;               authors LOS-TO-APPARATUS/LOS-TO-LOCATION directly (bypassing WALL-SEGMENTS
;;;               derivation) can still list a location as an occluder -- it still needs
;;;               LOCATION-COORDS>/APPARATUS-COORDS> asserted for that location and for both
;;;               of the beam's own endpoints, even though DERIVE-LOS-FROM-SEGMENTS itself
;;;               never runs; BEAM-COORDINATES-ENDPOINT-XY errors by name if one is missing
;;;   init      : derive-los-from-segments

(include-tech -location-coordinates)

(in-package :ww)


(define-types
  beam-endpoint (either transmitter receiver gun location))  ;a fixture, or a connector's location


(define-optional-types jammer gun)


(define-static-relations
  (apparatus-coords> (either transmitter receiver gun) $rational $rational)
  (wall-segments $list)
  (gate-segments $list)
  (boundary-wall $list))  ;closed polygon ((x1 y1) (x2 y2) ... (xn yn)); last point wraps to first


(defvar *beam-occlusion-tolerance* 1/2
  "Maximum perpendicular distance a location may sit off a beam's exact line and still
   count as a candidate occluder there (BEAM-COORDINATES-LOCATION-OCCLUDES-BEAM). Default is
   a half-unit radius. Problem files can override this.")


;;;; GEOMETRY HELPERS ;;;;
;;;; Plain Lisp functions operating on positions/beams passed as arguments -- no live
;;;; database access, so no WW query wrapper is needed for these.


(defun beam-coordinates-position (endpoint positions)
  (or (rest (assoc endpoint positions))
      (error "No position is defined for beam endpoint ~A." endpoint)))


(defun beam-coordinates-gate-midpoint (gate-record)
  ;; Returns GATE-RECORD's own (x y) midpoint -- a gate has no APPARATUS-COORDS> of its own,
  ;; since it is authored as an extended segment rather than a point endpoint, so its
  ;; LOS-TO-TARGET entries (DERIVE-LOS-FROM-SEGMENTS, below) use this single reference
  ;; point in its place.
  (list (/ (+ (second gate-record) (fourth gate-record)) 2)
        (/ (+ (third gate-record) (fifth gate-record)) 2)))


(defun beam-coordinates-gate-positions (gates)
  ;; Returns an alist of (gate-name x y), one per GATES record, keyed by name exactly like
  ;; BEAM-COORDINATES-ENDPOINT-POSITIONS's own entries, so it can extend that table for a
  ;; beam whose first endpoint is a gate.  Values come from BEAM-COORDINATES-GATE-MIDPOINT.
  (mapcar (lambda (gate-record)
            (cons (first gate-record) (beam-coordinates-gate-midpoint gate-record)))
          gates))


(defun beam-coordinates-boundary-segments (boundary-points)
  ;; Converts a BOUNDARY-WALL point list ((x1 y1) (x2 y2) ... (xn yn)) into wall-shaped
  ;; (name x1 y1 x2 y2) records, one per polygon edge, wrapping the final point back to
  ;; the first.  Fed into DERIVE-LOS-FROM-SEGMENTS below as unconditional LOS blockers
  ;; alongside WALL-SEGMENTS: a sightline that would have to leave the map's own
  ;; silhouette is never a real beam.  A wall's name is never read by BEAM-COORDINATES-
  ;; LOS-OCCLUDERS (only a gate's is), so a plain edge index suffices.
  (loop for (point1 point2) on (append boundary-points (list (first boundary-points)))
        while point2
        for edge-index from 1
        collect (list edge-index
                      (first point1) (second point1)
                      (first point2) (second point2))))


(defun beam-coordinates-obstacle-intersection-parameter (beam positions obstacle &optional endpoints-block)
  ;; Returns BEAM's own parameter (0 < t < 1) where OBSTACLE -- an (name x1 y1 x2 y2)
  ;; segment record, as found in a WALL-SEGMENTS/GATE-SEGMENTS list -- blocks BEAM's
  ;; interior, or nil if it doesn't.  Strict on BEAM's own side always: BEAM's own
  ;; endpoint touching OBSTACLE is never itself a crossing (see the error clause below
  ;; instead).  On OBSTACLE's side, ENDPOINTS-BLOCK controls whether OBSTACLE's own
  ;; endpoint also counts: nil (the default; gates use this) keeps the strict reading,
  ;; since a gate's own corner is already covered by its neighboring wall's inclusive
  ;; endpoint, so leaving gates strict avoids a redundant conditional occluder there;
  ;; true (walls, and BOUNDARY-WALL edges folded in as walls) also blocks at OBSTACLE's
  ;; own endpoint, so a beam threading exactly through the corner where two walls (or a
  ;; wall and the boundary wall) meet can't slip through for want of either wall's
  ;; interior, read strictly, containing that point.
  ;;
  ;; Errors if BEAM's own endpoint -- a location or fixture's authored position -- lies
  ;; exactly on OBSTACLE's own segment, corner or interior: a fixture must always be
  ;; offset off every wall/gate it sits beside, never placed on one, so this is an
  ;; authoring mistake to catch, not a case to silently resolve either way.
  (let* ((position1 (beam-coordinates-position (first beam) positions))
         (position2 (beam-coordinates-position (second beam) positions))
         (x1 (first position1))
         (y1 (second position1))
         (x2 (first position2))
         (y2 (second position2))
         (x3 (second obstacle))
         (y3 (third obstacle))
         (x4 (fourth obstacle))
         (y4 (fifth obstacle))
         (dx1 (- x2 x1))
         (dy1 (- y2 y1))
         (dx2 (- x4 x3))
         (dy2 (- y4 y3))
         (offset-x (- x3 x1))
         (offset-y (- y3 y1))
         (denominator (- (* dx1 dy2) (* dy1 dx2))))
    (unless (zerop denominator)
      (let ((parameter1 (/ (- (* offset-x dy2) (* offset-y dx2))
                           denominator))
            (parameter2 (/ (- (* offset-x dy1) (* offset-y dx1))
                           denominator)))
        (when (and (<= 0 parameter2 1) (or (zerop parameter1) (= parameter1 1)))
          (error "Beam endpoint ~A lies exactly on obstacle ~A; offset the fixture or ~
                  location off the wall/gate instead of leaving it on one."
                 (if (zerop parameter1) (first beam) (second beam))
                 (first obstacle)))
        (when (and (< 0 parameter1 1)
                   (if endpoints-block (<= 0 parameter2 1) (< 0 parameter2 1)))
          parameter1)))))


(defun beam-coordinates-los-occluders (beam positions walls gates)
  ;; Tests BEAM -- a (source destination) beam-endpoint pair -- against the problem's
  ;; WALL-SEGMENTS and GATE-SEGMENTS.  Returns the keyword :BLOCKED if any wall blocks
  ;; BEAM's interior -- including at the wall's own corner, since walls are tested with
  ;; ENDPOINTS-BLOCK true -- meaning no LOS fact should be asserted for it at all;
  ;; otherwise returns the (possibly empty) list of gate names whose segment properly
  ;; intersects BEAM's interior, strict at the gate's own corner (left to its
  ;; neighboring wall) -- BEAM's occluder list.  A LOS-TO-TARGET beam's own first
  ;; endpoint is that gate's own name, standing in for BEAM-COORDINATES-GATE-MIDPOINT
  ;; (a gate has no APPARATUS-COORDS> of its own) -- so a gate is always skipped against a
  ;; beam it is itself an endpoint of, or its own midpoint would trip BEAM-COORDINATES-
  ;; OBSTACLE-INTERSECTION-PARAMETER's lies-exactly-on-obstacle error every time, not
  ;; because of any authoring mistake.  A single return value, not multiple values:
  ;; CHECK-VARIABLE-NAMES, run on an init-action's effect (unlike a query/update body,
  ;; which it never checks), only recognizes ASSIGN/BIND/MVSETQ as $-variable-
  ;; establishing forms, not MV-ASSIGN.
  (if (some (lambda (wall)
              (beam-coordinates-obstacle-intersection-parameter beam positions wall t))
            walls)
    :blocked
    (loop for gate in gates
          when (and (not (member (first gate) beam :test #'eql))
                    (beam-coordinates-obstacle-intersection-parameter beam positions gate))
            collect (first gate))))


(defun beam-coordinates-location-occluders (beam positions locations tolerance)
  ;; Every other location whose position lies within TOLERANCE of BEAM's interior --
  ;; strictly between its two endpoints.  A location that is itself one of BEAM's own two
  ;; endpoints is skipped outright, the same way BEAM-COORDINATES-LOS-OCCLUDERS skips a
  ;; gate against a beam it is itself an endpoint of.
  (loop for location in locations
        unless (member location beam :test #'eql)
          append (when (beam-coordinates-location-occludes-beam
                         beam positions (beam-coordinates-position location positions)
                         tolerance)
                   (list location))))


(defun beam-coordinates-projection-parameter (x1 y1 x2 y2 x3 y3)
  ;; (x3,y3)'s own orthogonal projection parameter onto the line from (x1,y1) to (x2,y2) --
  ;; 0 at the first point, 1 at the second.  Shared by the init-time occlusion test
  ;; (BEAM-COORDINATES-LOCATION-OCCLUDES-BEAM, below) and the live elevation interpolation a
  ;; beam or sightline consumer performs at query time (BEAM-COORDINATES-ELEVATION-AT).
  (let ((dx (- x2 x1))
        (dy (- y2 y1)))
    (/ (+ (* (- x3 x1) dx) (* (- y3 y1) dy)) (+ (* dx dx) (* dy dy)))))


(defun beam-coordinates-location-occludes-beam (beam positions location-position tolerance)
  ;; True iff LOCATION-POSITION's own orthogonal projection onto BEAM's line falls strictly
  ;; between BEAM's two endpoints and its perpendicular distance from that line is within
  ;; TOLERANCE -- strict at BEAM's own endpoints exactly like BEAM-COORDINATES-OBSTACLE-
  ;; INTERSECTION-PARAMETER's own wall/gate test: a location standing at (or beyond) either
  ;; endpoint is never its own occluder.  Compares squared distances throughout to stay in
  ;; exact rational arithmetic.
  (let* ((position1 (beam-coordinates-position (first beam) positions))
         (position2 (beam-coordinates-position (second beam) positions))
         (x1 (first position1))
         (y1 (second position1))
         (x2 (first position2))
         (y2 (second position2))
         (x3 (first location-position))
         (y3 (second location-position))
         (parameter (beam-coordinates-projection-parameter x1 y1 x2 y2 x3 y3)))
    (and (< 0 parameter 1)
         (let* ((nearest-x (+ x1 (* parameter (- x2 x1))))
                (nearest-y (+ y1 (* parameter (- y2 y1))))
                (offset-x (- x3 nearest-x))
                (offset-y (- y3 nearest-y))
                (distance-squared (+ (* offset-x offset-x) (* offset-y offset-y))))
           (<= distance-squared (* tolerance tolerance))))))


;;;; QUERY FUNCTIONS ;;;;


(define-query beam-coordinates-endpoint-positions ()
  ;; Routes each beam-endpoint to its owning position relation: LOCATION-COORDS> for a
  ;; location, APPARATUS-COORDS> (transmitter/receiver only) for everything else.
  (do (assign $positions nil)
      (doall (?endpoint beam-endpoint)
        (if (location ?endpoint)
          (if (bind (location-coords> ?endpoint $x $y))
            (push (list ?endpoint $x $y) $positions)
            (error "No LOCATION-COORDS> is defined for location ~A." ?endpoint))
          (if (bind (apparatus-coords> ?endpoint $x $y))
            (push (list ?endpoint $x $y) $positions)
            (error "No APPARATUS-COORDS> is defined for beam endpoint ~A." ?endpoint))))
      $positions))


(define-query beam-coordinates-endpoint-xy (?endpoint beam-endpoint)
  ;; ?endpoint's own live coordinates, routed the same way BEAM-COORDINATES-ENDPOINT-
  ;; POSITIONS routes them at init time: LOCATION-COORDS> for a location, APPARATUS-COORDS>
  ;; for a non-location beam endpoint.  Errors by name if the fact is missing, exactly like
  ;; that init-time sibling -- a bare BIND left unguarded here would instead leave $x/$y nil
  ;; and only fail later, confusingly, inside BEAM-COORDINATES-PROJECTION-PARAMETER's
  ;; arithmetic.  A hand-authored location occluder needs coordinates for itself and both
  ;; of its beam's endpoints, not just for the LOS pair the fact itself names.
  (if (location ?endpoint)
    (do (or (bind (location-coords> ?endpoint $x $y))
            (error "No LOCATION-COORDS> is defined for location ~A." ?endpoint))
        (values $x $y))
    (do (or (bind (apparatus-coords> ?endpoint $x $y))
            (error "No APPARATUS-COORDS> is defined for beam endpoint ~A." ?endpoint))
        (values $x $y))))


(define-query beam-coordinates-elevation-at
    (?occluder location
     ?from beam-endpoint
     ?near-elevation
     ?to beam-endpoint
     ?far-elevation)
  ;; The occluder and endpoints are Wouldwork objects. Elevations are computed Lisp values
  ;; and therefore deliberately have no Wouldwork object type.
  ;;
  ;; The beam's own interpolated elevation at ?occluder's position along the line from ?from
  ;; to ?to -- linear between ?near-elevation (at ?from) and ?far-elevation (at ?to), weighted
  ;; by ?occluder's own live projection parameter on that line.  Read by visibility.lisp's
  ;; beam-visible for a location occluder it has already confirmed qualifies.
  (do (mv-assign ($x1 $y1) (beam-coordinates-endpoint-xy ?from))
      (mv-assign ($x2 $y2) (beam-coordinates-endpoint-xy ?to))
      (mv-assign ($x3 $y3) (beam-coordinates-endpoint-xy ?occluder))
      (assign $parameter (beam-coordinates-projection-parameter $x1 $y1 $x2 $y2 $x3 $y3))
      (+ ?near-elevation (* $parameter (- ?far-elevation ?near-elevation)))))


;;;; INITIALIZATION ;;;;


(define-init-action derive-los-from-segments
  ;; Derives LOS-TO-APPARATUS/LOS-TO-LOCATION, and LOS-TO-TARGET/gun's LOS-TO-APPARATUS
  ;; entries when a jammer is present, from WALL-SEGMENTS/GATE-SEGMENTS raw segment
  ;; geometry, when the problem supplies it, instead of requiring them hand-authored.
  ;; LOS-TO-TARGET and gun's LOS-TO-APPARATUS entries are both gated on (exists (?j
  ;; jammer) t): nothing but jam-target ever consumes a location<->gate or location<->gun
  ;; sightline, so a problem without a jammer (like corner-topo) skips both derivations
  ;; entirely rather than asserting facts no query ever reads.  A gate's own LOS-TO-TARGET
  ;; entries use BEAM-COORDINATES-GATE-MIDPOINT as their reference point, since a gate is
  ;; authored as an extended segment rather than
  ;; a point endpoint.  When the problem also asserts BOUNDARY-WALL, each polygon edge
  ;; (BEAM-COORDINATES-BOUNDARY-SEGMENTS) is folded into the wall list, so a sightline that
  ;; would have to cut outside the map's own silhouette is blocked exactly like a wall --
  ;; any consequence for a beam this blocks (eg, a connector losing its light) is resolved
  ;; the normal way, since this init-action runs before the problem's own INITIALIZE-
  ;; DERIVED-STATE calls PROPAGATE-CHANGES!.  Runs only when the problem has asserted
  ;; WALL-SEGMENTS -- inert otherwise, so a problem that hand-authors its own LOS facts
  ;; instead is unaffected.  Defined here, textually before
  ;; -beam-crossing-coordinates' own ESTABLISH-BEAM-COORDINATES when that file is also
  ;; spliced: init-actions run in file/load order (see that init-action's own commentary
  ;; there on DO-INIT-ACTION-UPDATES), not by the numeric-looking argument below, and
  ;; ESTABLISH-BEAM-COORDINATES reads LOS-TO-APPARATUS/LOS-TO-LOCATION to enumerate its own
  ;; beam set.  Ends with its own CONVERT-DATABASES-TO-INTEGERS for the same reason that
  ;; init-action does -- so the facts asserted here are visible to its own later BIND calls.
  ;;
  ;; The location<->apparatus and location<->location branches additionally append every
  ;; other location that occludes the beam within *BEAM-OCCLUSION-TOLERANCE* (default 0,
  ;; exact collinearity); the location<->gate and location<->gun branches deliberately do
  ;; not, since only jam-target ever reads those two, and a jammer's line to its target is
  ;; not blocked by intervening objects.
  0
  ()
  (bind (wall-segments $walls))
  ()
  (assert
    (do (bind (gate-segments $gates))
        (assign $boundary-walls
                (if (bind (boundary-wall $boundary-points))
                  (beam-coordinates-boundary-segments $boundary-points)))
        (assign $all-walls (append $walls $boundary-walls))
        (assign $positions (beam-coordinates-endpoint-positions))
        (assign $target-positions
                (append (beam-coordinates-gate-positions $gates) $positions))
        (assign $locations (gethash 'location *types*))
        (doall (?location location)
          (doall (?transmitter transmitter)
            (do (assign $occluders
                        (beam-coordinates-los-occluders
                          (list ?transmitter ?location) $positions $all-walls $gates))
                (if (not (eql $occluders :blocked))
                  (do (assign $location-occluders
                              (beam-coordinates-location-occluders
                                (list ?transmitter ?location) $positions $locations
                                *beam-occlusion-tolerance*))
                      (los-to-apparatus ?location
                                        (append $occluders $location-occluders)
                                        ?transmitter))))))
        (doall (?location location)
          (doall (?receiver receiver)
            (do (assign $occluders
                        (beam-coordinates-los-occluders
                          (list ?location ?receiver) $positions $all-walls $gates))
                (if (not (eql $occluders :blocked))
                  (do (assign $location-occluders
                              (beam-coordinates-location-occluders
                                (list ?location ?receiver) $positions $locations
                                *beam-occlusion-tolerance*))
                      (los-to-apparatus ?location
                                        (append $occluders $location-occluders)
                                        ?receiver))))))
        (if (exists (?j jammer) t)
          (do (doall (?location location)
                (doall (?gate gate)
                  (do (assign $occluders
                              (beam-coordinates-los-occluders
                                (list ?gate ?location) $target-positions $all-walls $gates))
                      (if (not (eql $occluders :blocked))
                        (los-to-target ?location $occluders ?gate)))))
              (doall (?location location)
                (doall (?gun gun)
                  (do (assign $occluders
                              (beam-coordinates-los-occluders
                                (list ?location ?gun) $positions $all-walls $gates))
                      (if (not (eql $occluders :blocked))
                        (los-to-apparatus ?location $occluders ?gun)))))))
        (doall (?source location)
          (doall (?destination location)
            (if (member ?destination
                        (rest (member ?source (gethash 'location *types*))))
              (do (assign $occluders
                          (beam-coordinates-los-occluders
                            (list ?source ?destination) $positions $all-walls $gates))
                  (if (not (eql $occluders :blocked))
                    (do (assign $location-occluders
                                (beam-coordinates-location-occluders
                                  (list ?source ?destination) $positions $locations
                                  *beam-occlusion-tolerance*))
                        (los-to-location ?source
                                          (append $occluders $location-occluders)
                                          ?destination)))))))
        (convert-databases-to-integers))))

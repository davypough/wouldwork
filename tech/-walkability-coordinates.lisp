;;; Filename: -walkability-coordinates.lisp

;;; Walkability coordinates substrate: derives WALK-VIA (and, for rides into an
;;; air stream's destination, WALK-VIA>) from raw segment geometry, for a problem that
;;; would rather author 2D positions than hand-list which locations can walk to which.
;;; Nested under public walkability and under -stream-passability; entirely inert unless
;;; the problem actually asserts WALL-SEGMENT> or BOUNDARY-WALL -- a problem that
;;; hand-authors WALK-VIA directly is unaffected.
;;;
;;; Walking connectivity is a region-adjacency question.  Every wall/gate/window/screen
;;; segment and boundary edge is axis-aligned (a diagonal one is an authoring mistake,
;;; caught during initialization validation; the derivation also checks its own
;;; invariant).  The segments' own coordinates induce a coordinate-compressed
;;; arrangement: a grid of cells whose edge-intervals are each classified from the
;;; segments covering them -- solid (wall/window/boundary), a single named door
;;; (gate/screen/stream curtain), or open.  Two doors covering one interval, or a
;;; gate/screen overlapping a solid, are authoring contradictions and error.  Cells
;;; united across open intervals form zones; door intervals between distinct zones form
;;; a labeled zone graph.  For every zone pair, a fixpoint over antichains of door-sets
;;; computes ALL subset-minimal door-sets -- each a set of doors sufficient for some
;;; physical route -- and a location pair's WALK-VIA value is that family in DNF: ()
;;; still means direct/unguarded, and a nonempty value is a list of clauses, OR over
;;; clauses, AND within (matching the CONTROLS convention).  Families are emitted in
;;; canonical order (doors within a clause by name; clauses by length then
;;; lexicographically) so derived facts are deterministic and diffable.
;;;
;;; A wall-mounted fan's air stream is DERIVED geometry, not authored: its center line
;;; runs from the gears' HAS-POSITION location (the swept location) to the AIMED-AT
;;; destination (error unless axis-aligned), extended backward behind the fan to the
;;; nearest solid (the backstop -- the wall the gears hang on), and widened to the
;;; stream's width -- 3 units by default, overridable per gears with a STREAM-WIDTH
;;; fact (see -stream-passability.lisp, which owns that relation and redefines
;;; WALKABILITY-COORDINATES-STREAM-SPECS to gather the facts; the default here
;;; returns none, so walkability alone never references blower relations).  The
;;; resulting band is a conditional barrier region: its perimeter -- the front edge at
;;; the destination and the two sides, never the backstop edge -- enters the
;;; arrangement as curtain intervals labeled with the GEARS name (passable only while
;;; no blowing fan is mounted -- see -stream-passability's OBSTACLE-CLEAR extension),
;;; clipped silently wherever a solid already covers them.  Crossing the band anywhere
;;; costs its gears once: two curtains on one route dedupe in the door-set algebra.
;;;
;;; A location on a stream curtain -- the AIMED-AT destination's normal situation,
;;; mid-interval on the front curtain -- belongs to the zone OUTSIDE the band only, and
;;; the swept location in the band's interior belongs to the band's own zone only, so
;;; every walking edge to or from either spot's band side carries the gears in each
;;; clause automatically: the flowing stream is a wall, and its interior is standable
;;; exactly while it is off.  Riding the stream is instead a property of the
;;; destination: from every zone flanking the band across a SIDE curtain, the
;;; destination's inbound direction additionally unions that zone's own family --
;;; while blowing, stepping laterally into the flow carries the walker to the
;;; destination; while off, the same trip is an ordinary walk across the dead band; so
;;; the unconditional edge is correct in both regimes, and such pairs are emitted as
;;; directional WALK-VIA> facts (rides widen inbound, never outbound).  The front
;;; curtain grants no ride -- walking in against the flow is barred like any other
;;; gears-gated crossing.  Any other location inside a
;;; band, on a covered solid interval, inside a gate/screen doorway, at a corner whose
;;; surrounding cells span several zones, or outside a declared boundary, errors.
;;; Location coordinates never induce grid lines -- they are points looked up in the
;;; arrangement afterward, so fractional authoring offsets need no grid alignment.
;;;
;;; The derivation is single-layer: segments are elevation-blind, blocking every walking
;;; pair that crosses them regardless of level.  Multi-level maps therefore author an
;;; elevated platform's ground-level footprint as wall segments, keep the platform's own
;;; locations inside that footprint, and connect the levels only with authored
;;; stairs-via, jump-via, or climb-via> edges (which this derivation never touches);
;;; ONE-STEP-WALKABLE's
;;; elevation-equality check rejects any derived edge between different levels.  See
;;; problem-claustro-topo's slab (wall4/wall5) for the pattern.
;;;
;;; Reuses LOCATION-COORDS> (nested from -location-coordinates, shared with
;;; visibility's -beam-los-coordinates substrate) for location coordinates.  Reuses the
;;; individually authored segment relations from nested -segment-geometry, shared with
;;; -beam-los-coordinates without requiring any beam tech.  SCREEN-SEGMENT> has no beam
;;; consumer, and streams none at
;;; all -- they affect walking only, never a sightline.
;;;
;;; Self-contained; spliced by (include-tech -walkability-coordinates), nested from
;;; walkability and from -stream-passability.
;;;
;;; REQUIRES:
;;;   types     : location  --  declared by the problem, as walkability itself already
;;;               requires; screen declared optional by nested -passability, spliced by
;;;               walkability.lisp before this file
;;;   nested    : -walkability (WALK-VIA/WALK-VIA> topology relations);
;;;               -location-coordinates (LOCATION-COORDS>)
;;; PROVIDES:
;;;   relations : wall-segment>, gate-segment>, window-segment>, screen-segment>,
;;;               boundary-wall  --  default to no facts; a problem that asserts
;;;               wall-segment> or boundary-wall gets WALK-VIA/WALK-VIA> derived
;;;               automatically instead of hand-authoring them
;;;   queries   : walkability-coordinates-stream-specs  --  default no streams;
;;;               redefined by -stream-passability where wall blowers exist
;;;   init      : derive-walk-via-from-segments

(include-tech -walkability)
(include-tech -location-coordinates)
(include-tech -segment-geometry)

(in-package :ww)


;;;; DERIVATION CORE ;;;;
;;;; Plain Lisp functions operating on segments/positions passed as arguments -- no live
;;;; database access, so no WW query wrapper is needed for these.  High-level first.


(defun walkability-coordinates-build-arrangement (positions walls gates windows screens stream-specs boundary-points)
  ;; Computes the full walking arrangement once: solids, derived stream bands with their
  ;; curtain segments, coordinate-compressed cells, per-interval coverage
  ;; classification, flood-filled zones, the door-labeled zone graph, every location's
  ;; zone membership (with its ride zones, if it is a stream's destination), and the
  ;; minimal door-set family between every membership zone and every zone.  Returned as
  ;; a plist consumed pairwise by WALKABILITY-COORDINATES-PAIR-SPEC.
  (let* ((solids (append (mapcar (lambda (segment) (list :wall segment)) walls)
                         (mapcar (lambda (segment) (list :window segment)) windows)
                         (mapcar (lambda (segment) (list :boundary segment))
                                 (walkability-coordinates-boundary-segments boundary-points))))
         (bands (mapcar (lambda (spec) (walkability-coordinates-stream-band spec solids))
                        stream-specs))
         (tagged (append solids
                         (mapcar (lambda (segment) (list :gate segment)) gates)
                         (mapcar (lambda (segment) (list :screen segment)) screens)
                         (loop for band in bands append (eighth band))))
         (xs (walkability-coordinates-axis-coordinates tagged :x))
         (ys (walkability-coordinates-axis-coordinates tagged :y))
         (coverage (walkability-coordinates-coverage-table tagged xs ys))
         (classified (walkability-coordinates-classify-coverage coverage))
         (zones (walkability-coordinates-flood-fill (length xs) (length ys) classified))
         (edges (walkability-coordinates-door-edges classified zones))
         (memberships (walkability-coordinates-memberships
                        positions xs ys classified zones boundary-points bands))
         (sources (remove-duplicates
                    (loop for (nil zone-list nil) in memberships append zone-list)))
         (families (walkability-coordinates-family-table edges sources)))
    (list :memberships memberships :families families)))


(defun walkability-coordinates-pair-spec (arrangement loc-a loc-b)
  ;; Resolves one location pair against the arrangement.  Returns NIL if no zone pair
  ;; across the two memberships is connected (blocked -- no fact), (:sym family) for an
  ;; ordinary symmetric WALK-VIA, or (:dir family-a->b family-b->a) when either endpoint
  ;; is a stream destination whose ride edges widen its inbound direction: the inbound
  ;; family additionally unions the source's families to the destination's ride zones
  ;; (riding the stream in from a side, or walking the same route while it is off), so
  ;; a ride can strictly widen inbound but never outbound.  A family of one empty
  ;; clause is normalized to NIL, the direct/unguarded value.
  (let* ((memberships (getf arrangement :memberships))
         (families (getf arrangement :families))
         (entry-a (assoc loc-a memberships))
         (entry-b (assoc loc-b memberships))
         (connected nil)
         (base nil))
    (dolist (za (second entry-a))
      (dolist (zb (second entry-b))
        (let ((fam (gethash (list za zb) families)))
          (when fam
            (setf connected t)
            (setf base (walkability-family-union base fam))))))
    (when connected
      (let ((into-b (walkability-coordinates-ride-augmented-family
                      base (second entry-a) (third entry-b) families))
            (into-a (walkability-coordinates-ride-augmented-family
                      base (second entry-b) (third entry-a) families)))
        (if (equal into-a into-b)
          (list :sym (walkability-normalize-family into-a))
          (list :dir
                (walkability-normalize-family into-b)
                (walkability-normalize-family into-a)))))))


(defun walkability-coordinates-ride-augmented-family (base source-zones ride-zones families)
  ;; The inbound family into a location: BASE plus, when the location is some stream's
  ;; destination, the families from every source zone to each of its ride zones --
  ;; reaching a zone flanking the band's side curtains suffices, because stepping
  ;; laterally into the blowing stream carries the walker to the destination, and
  ;; walking across the stopped band covers the same trip while it is off.
  (let ((family base))
    (dolist (source-zone source-zones)
      (dolist (ride-zone ride-zones)
        (let ((fam (gethash (list source-zone ride-zone) families)))
          (when fam
            (setf family (walkability-family-union family fam))))))
    family))


;;;; STREAM BANDS ;;;;
;;;; A band is (gears swept-location destination x-lo x-hi y-lo y-hi curtains): the
;;;; stream's barred rectangle and its perimeter curtain segments, each curtain a
;;;; (:stream (gears x1 y1 x2 y2)) tagged segment.  The front curtain (at the
;;;; destination) is always first in the curtain list; the two sides follow.


(defun walkability-coordinates-stream-band (spec solids)
  ;; SPEC is (gears swept-location destination sx sy dx dy width): the gears' own
  ;; position, the AIMED-AT destination and its position, and the stream's width.  The
  ;; center line must be axis-aligned; it extends backward from the swept location, away
  ;; from the destination, to the nearest solid (the backstop).  The band spans the
  ;; center line laterally by half the width each way; its curtains are the front edge
  ;; at the destination and the two sides -- the backstop edge is the solid itself, and
  ;; any gap beside the backstop is a real walkable slit, left open.
  (destructuring-bind (gears swept-location destination sx sy dx dy width) spec
    (when (and (= sx dx) (= sy dy))
      (error "The stream of ~A has coincident swept location and destination (~A ~A)."
             gears sx sy))
    (unless (or (= sy dy) (= sx dx))
      (error "The stream of ~A from (~A ~A) to (~A ~A) is not axis-aligned."
             gears sx sy dx dy))
    (if (= sy dy)
      (let* ((backstop (walkability-coordinates-stream-backstop
                         gears sx sy :vertical (> dx sx) solids))
             (x-lo (min backstop dx))
             (x-hi (max backstop dx))
             (y-lo (- sy (/ width 2)))
             (y-hi (+ sy (/ width 2))))
        (list gears swept-location destination x-lo x-hi y-lo y-hi
              (list (list :stream (list gears dx y-lo dx y-hi))
                    (list :stream (list gears x-lo y-lo x-hi y-lo))
                    (list :stream (list gears x-lo y-hi x-hi y-hi)))))
      (let* ((backstop (walkability-coordinates-stream-backstop
                         gears sx sy :horizontal (> dy sy) solids))
             (y-lo (min backstop dy))
             (y-hi (max backstop dy))
             (x-lo (- sx (/ width 2)))
             (x-hi (+ sx (/ width 2))))
        (list gears swept-location destination x-lo x-hi y-lo y-hi
              (list (list :stream (list gears x-lo dy x-hi dy))
                    (list :stream (list gears x-lo y-lo x-lo y-hi))
                    (list :stream (list gears x-hi y-lo x-hi y-hi))))))))


(defun walkability-coordinates-stream-backstop (gears sx sy transversal-orientation destination-above-p solids)
  ;; The coordinate of the nearest solid crossing the stream's center line behind the
  ;; fan: among solids of TRANSVERSAL-ORIENTATION whose own extent contains the center
  ;; line's fixed coordinate, the closest one on the opposite side of the swept
  ;; position from the destination.
  (let ((along (if (eql transversal-orientation :vertical) sx sy))
        (fixed (if (eql transversal-orientation :vertical) sy sx))
        (best nil))
    (dolist (tagged-solid solids)
      (let ((segment (second tagged-solid)))
        (when (eql (walkability-coordinates-orientation segment) transversal-orientation)
          (let ((coordinate (if (eql transversal-orientation :vertical)
                              (second segment)
                              (third segment)))
                (range (walkability-coordinates-along-range segment transversal-orientation)))
            (when (and (<= (car range) fixed (cdr range))
                       (if destination-above-p (< coordinate along) (> coordinate along))
                       (or (null best)
                           (if destination-above-p (> coordinate best) (< coordinate best))))
              (setf best coordinate))))))
    (unless best
      (error "The stream of ~A has no solid backstop behind its fan at (~A ~A)."
             gears sx sy))
    best))


(defun walkability-coordinates-along-range (segment orientation)
  ;; SEGMENT's own extent along its line's direction, as a sorted (low . high) pair: Y
  ;; for a :vertical segment, X for a :horizontal one.
  (let ((a (if (eql orientation :vertical) (third segment) (second segment)))
        (b (if (eql orientation :vertical) (fifth segment) (fourth segment))))
    (cons (min a b) (max a b))))


;;;; LOCATION MEMBERSHIP ;;;;


(defun walkability-coordinates-memberships (positions xs ys classified zones boundary-points bands)
  ;; One (location zone-list ride-zones) entry per location: the zones the location
  ;; belongs to, and -- when it is some stream's AIMED-AT destination -- the zones from
  ;; which that stream can be ridden to it (the zones across its band's side curtains).
  ;; With a declared boundary, the unbounded corner cell's zone is the outside; a
  ;; location landing there is an authoring mistake caught here.
  (let ((outside (when boundary-points (aref zones 0 0))))
    (loop for (location x y) in positions
          collect (let ((zone-list (walkability-coordinates-resolve-location
                                     location x y xs ys classified zones bands))
                        (ride-zones (loop for band in bands
                                          when (eql location (third band))
                                            append (walkability-coordinates-band-side-zones
                                                     band xs ys classified zones))))
                    (when (and outside (member outside zone-list))
                      (error "Location ~A (~A ~A) lies outside the boundary wall."
                             location x y))
                    (list location zone-list (remove-duplicates ride-zones))))))


(defun walkability-coordinates-resolve-location (location x y xs ys classified zones bands)
  ;; A location's zone membership list: its cell's zone when strictly inside a cell (an
  ;; error for any location inside a band other than the band's own swept location);
  ;; either flanking cell's zone (they are one zone through that very interval) when on
  ;; an uncovered grid-line interval; the zone outside the band when on a stream-curtain
  ;; interval; the surrounding cells' shared zone when at a grid vertex.  Inside a
  ;; solid, inside a gate/screen doorway, or at a corner whose surrounding cells
  ;; disagree, errors -- an authoring mistake, not a case to guess at.
  (let ((xi (position x xs :test #'=))
        (yi (position y ys :test #'=))
        (col (walkability-coordinates-interval-index x xs))
        (row (walkability-coordinates-interval-index y ys)))
    (cond ((and (null xi) (null yi))
           (walkability-coordinates-resolve-in-cell
             location x y (aref zones col row) bands))
          ((null yi)
           (walkability-coordinates-resolve-on-line
             location x y :v (gethash (list :v xi row) classified)
             (aref zones xi row) (aref zones (1+ xi) row) bands))
          ((null xi)
           (walkability-coordinates-resolve-on-line
             location x y :h (gethash (list :h yi col) classified)
             (aref zones col yi) (aref zones col (1+ yi)) bands))
          (t
           (let ((corner-zones (remove-duplicates
                                 (list (aref zones xi yi) (aref zones (1+ xi) yi)
                                       (aref zones xi (1+ yi)) (aref zones (1+ xi) (1+ yi))))))
             (when (rest corner-zones)
               (error "Location ~A (~A ~A) sits at a segment corner whose surrounding ~
                       cells span ~D zones; its zone is ambiguous."
                      location x y (length corner-zones)))
             corner-zones)))))


(defun walkability-coordinates-resolve-in-cell (location x y zone bands)
  ;; Membership for a location strictly inside a cell.  Inside a stream band, only the
  ;; band's own swept location may stand, and it belongs to the band's zone alone --
  ;; every walking edge to or from it then carries the band's gears in each clause
  ;; automatically, so it is standable and enterable exactly while the stream is off.
  (let ((containing (remove-if-not
                      (lambda (band)
                        (and (< (fourth band) x (fifth band))
                             (< (sixth band) y (seventh band))))
                      bands)))
    (cond ((null containing)
           (list zone))
          ((and (null (rest containing))
                (eql location (second (first containing))))
           (list zone))
          (t (error "Location ~A (~A ~A) lies inside the air stream band of ~A."
                    location x y (first (first containing)))))))


(defun walkability-coordinates-resolve-on-line (location x y axis cover near-zone far-zone bands)
  ;; Membership for a location on a single grid-line interval of AXIS (:v or :h), given
  ;; that interval's classification and the two flanking cells' zones.  On a stream
  ;; curtain the location belongs to the zone OUTSIDE the band only -- the AIMED-AT
  ;; destination's normal situation, standing at the stream's mouth: the band side is
  ;; a place it could occupy only while the stream is off, reached by an ordinary
  ;; gears-gated crossing.  Every stream-curtain line coincides with one of its band's
  ;; four bounds, so comparing the line coordinate against the band rectangle picks the
  ;; outside flank.
  (cond ((null cover)
         (list near-zone))
        ((eql cover :solid)
         (error "Location ~A (~A ~A) lies inside a wall, window, or boundary segment."
                location x y))
        ((member (third cover) '(:gate :screen))
         (error "Location ~A (~A ~A) lies inside doorway ~A."
                location x y (second cover)))
        (t  ;a stream curtain: the outside flank only
         (let ((band (find (second cover) bands :key #'first)))
           (if (eql axis :v)
             (list (if (= x (fourth band)) near-zone far-zone))
             (list (if (= y (sixth band)) near-zone far-zone)))))))


(defun walkability-coordinates-band-side-zones (band xs ys classified zones)
  ;; Every zone lying just across one of BAND's unclipped SIDE curtain intervals -- the
  ;; zones a walker can ride BAND's stream from: stepping laterally into the flow
  ;; carries them to the destination.  The front curtain (always first in the curtain
  ;; list) never grants a ride: entering against the flow is barred.
  (let ((neighbors nil))
    (dolist (curtain (rest (eighth band)))
      (dolist (key (walkability-coordinates-segment-interval-keys (second curtain) xs ys))
        (let ((class (gethash key classified)))
          (when (and (listp class)
                     (eql (first class) :door)
                     (eql (second class) (first band)))
            (destructuring-bind (axis line cross) key
              (if (eql axis :v)
                (progn (push (aref zones line cross) neighbors)
                       (push (aref zones (1+ line) cross) neighbors))
                (progn (push (aref zones cross line) neighbors)
                       (push (aref zones cross (1+ line)) neighbors))))))))
    (remove-duplicates neighbors)))


;;;; MINIMAL DOOR-SET FAMILIES ;;;;
;;;; A family is an antichain of door-sets: no clause a superset of another.  OR over
;;;; clauses, AND within -- the same DNF convention as CONTROLS.


(defun walkability-coordinates-family-table (edges sources)
  ;; For every source zone in SOURCES, relaxes families over the door-labeled zone graph
  ;; to a fixpoint: the source starts at the family of one empty clause; each edge
  ;; extends the near side's family by its door and merges into the far side.  Families
  ;; only ever gain shorter/incomparable clauses, so the fixpoint terminates.  Returns a
  ;; hash of (source zone) -> family; a zone never reached from a source has no entry
  ;; (blocked).  A family exceeding 32 clauses signals a pathological door layout.
  (let ((table (make-hash-table :test 'equal)))
    (dolist (source sources)
      (let ((fams (make-hash-table)))
        (setf (gethash source fams) (list nil))
        (loop for changed = nil
              do (dolist (edge edges)
                   (when (walkability-coordinates-relax-edge edge fams)
                     (setf changed t)))
              while changed)
        (loop for zone being the hash-keys of fams using (hash-value family)
              do (setf (gethash (list source zone) table) family))))
    table))


(defun walkability-coordinates-relax-edge (edge fams)
  ;; Relaxes one undirected door edge (zone-a zone-b door) in both directions.  Returns
  ;; true if either endpoint's family changed.
  (let ((changed nil))
    (destructuring-bind (zone-a zone-b door) edge
      (dolist (direction (list (list zone-a zone-b) (list zone-b zone-a)))
        (let ((from-family (gethash (first direction) fams)))
          (when from-family
            (let* ((to (second direction))
                   (candidate (walkability-family-add-obstacle from-family door))
                   (merged (walkability-family-union
                             (gethash to fams) candidate)))
              (when (> (length merged) 32)
                (error "The minimal door-set family between two zones exceeds 32 ~
                        alternatives; the door layout is pathological."))
              (unless (equal merged (gethash to fams))
                (setf (gethash to fams) merged)
                (setf changed t)))))))
    changed))


;;;; ARRANGEMENT GEOMETRY ;;;;


(defun walkability-coordinates-boundary-segments (points)
  ;; The explicitly closed polygon's consecutive edges as pseudo-segments
  ;; (:boundary x1 y1 x2 y2).  BOUNDARY-WALL validation requires axis alignment;
  ;; repeat the check here as an invariant of this rectangular-cell arrangement.
  (when points
    (loop for (p1 p2) on points
          while p2
          unless (or (= (first p1) (first p2)) (= (second p1) (second p2)))
            do (error "Boundary edge ~A -> ~A is not axis-aligned." p1 p2)
          collect (list :boundary (first p1) (second p1) (first p2) (second p2)))))


(defun walkability-coordinates-orientation (segment)
  ;; :horizontal if SEGMENT's two endpoints share Y, :vertical if they share X.  A
  ;; diagonal segment is an authoring mistake to catch here, not a case to generalize
  ;; the geometry for.
  (let ((x1 (second segment)) (y1 (third segment))
        (x2 (fourth segment)) (y2 (fifth segment)))
    (cond ((= y1 y2) :horizontal)
          ((= x1 x2) :vertical)
          (t (error "Segment ~A (~A ~A ~A ~A) is not axis-aligned."
                    (first segment) x1 y1 x2 y2)))))


(defun walkability-coordinates-axis-coordinates (tagged-segments axis)
  ;; The sorted, deduplicated coordinates every segment contributes along AXIS (:x or
  ;; :y), as a vector of grid-line coordinates.  Location coordinates never contribute.
  (coerce (sort (remove-duplicates
                  (loop for (nil segment) in tagged-segments
                        append (if (eql axis :x)
                                 (list (second segment) (fourth segment))
                                 (list (third segment) (fifth segment))))
                  :test #'=)
                #'<)
          'vector))


(defun walkability-coordinates-interval-index (value coordinates)
  ;; The index of the open interval VALUE falls in: interval i spans coordinate i-1 to
  ;; coordinate i, with unbounded intervals 0 and (length COORDINATES) outside.
  (count-if (lambda (coordinate) (< coordinate value)) coordinates))


(defun walkability-coordinates-segment-interval-keys (segment xs ys)
  ;; The coverage keys of every cell edge-interval SEGMENT covers: (:v k r) is the
  ;; interval of vertical grid line k flanking row r; (:h m i) is the interval of
  ;; horizontal grid line m flanking column i.  Segment endpoints are themselves grid
  ;; coordinates, so a segment covers each interval entirely or not at all.
  (let* ((orientation (walkability-coordinates-orientation segment))
         (range (walkability-coordinates-along-range
                  segment (if (eql orientation :vertical) :vertical :horizontal))))
    (if (eql orientation :vertical)
      (let ((k (position (second segment) xs :test #'=))
            (rlo (position (car range) ys :test #'=))
            (rhi (position (cdr range) ys :test #'=)))
        (loop for r from (1+ rlo) to rhi collect (list :v k r)))
      (let ((m (position (third segment) ys :test #'=))
            (ilo (position (car range) xs :test #'=))
            (ihi (position (cdr range) xs :test #'=)))
        (loop for i from (1+ ilo) to ihi collect (list :h m i))))))


(defun walkability-coordinates-coverage-table (tagged-segments xs ys)
  ;; Marks every cell edge-interval each segment covers with the segment's (kind name).
  (let ((coverage (make-hash-table :test 'equal)))
    (dolist (tagged-segment tagged-segments)
      (destructuring-bind (kind segment) tagged-segment
        (dolist (key (walkability-coordinates-segment-interval-keys segment xs ys))
          (push (list kind (first segment)) (gethash key coverage)))))
    coverage))


(defun walkability-coordinates-classify-coverage (coverage)
  ;; Classifies each covered interval: :solid when any wall/window/boundary covers it,
  ;; or (:door name kind) for a single gate/screen/stream curtain.  A stream curtain
  ;; overlapping a solid is silently clipped -- the solid wins; a gate or screen
  ;; overlapping a solid, or two differently-named doors covering one interval, are
  ;; authoring contradictions.
  (let ((classified (make-hash-table :test 'equal)))
    (loop for key being the hash-keys of coverage using (hash-value entries)
          for solids = (remove-if-not (lambda (entry)
                                        (member (first entry) '(:wall :window :boundary)))
                                      entries)
          for doors = (remove-duplicates
                        (remove-if (lambda (entry)
                                     (member (first entry) '(:wall :window :boundary)))
                                   entries)
                        :key #'second)
          do (cond (solids
                    (let ((hard-doors (remove :stream doors :key #'first)))
                      (when hard-doors
                        (error "Door segment(s) ~A overlap solid segment(s) ~A on one ~
                                interval; a doorway cannot coincide with a solid partition."
                               (mapcar #'second hard-doors) (mapcar #'second solids))))
                    (setf (gethash key classified) :solid))
                   ((rest doors)
                    (error "Doors/stream curtains ~A cover the same interval; a ~
                            crossing there has no single door."
                           (mapcar #'second doors)))
                   (doors (setf (gethash key classified)
                                (list :door (second (first doors)) (first (first doors)))))))
    classified))


(defun walkability-coordinates-flood-fill (nx ny classified)
  ;; Unions cells across open (unclassified) intervals into zones: a 2D array of zone
  ;; ids over the (1+ NX) x (1+ NY) cell grid, unbounded outer cells included -- a
  ;; closed solid boundary isolates them on its own.
  (let ((zones (make-array (list (1+ nx) (1+ ny)) :initial-element nil))
        (zone-count 0))
    (dotimes (i (1+ nx))
      (dotimes (j (1+ ny))
        (when (null (aref zones i j))
          (setf (aref zones i j) zone-count)
          (let ((stack (list (list i j))))
            (loop while stack
                  do (destructuring-bind (ci cj) (pop stack)
                       (dolist (neighbor (walkability-coordinates-open-neighbors
                                           ci cj nx ny classified))
                         (when (null (aref zones (first neighbor) (second neighbor)))
                           (setf (aref zones (first neighbor) (second neighbor)) zone-count)
                           (push neighbor stack))))))
          (incf zone-count))))
    zones))


(defun walkability-coordinates-open-neighbors (i j nx ny classified)
  ;; The cells adjacent to cell (I J) across open intervals.
  (let ((neighbors nil))
    (when (and (< i nx) (null (gethash (list :v i j) classified)))
      (push (list (1+ i) j) neighbors))
    (when (and (>= i 1) (null (gethash (list :v (1- i) j) classified)))
      (push (list (1- i) j) neighbors))
    (when (and (< j ny) (null (gethash (list :h j i) classified)))
      (push (list i (1+ j)) neighbors))
    (when (and (>= j 1) (null (gethash (list :h (1- j) i) classified)))
      (push (list i (1- j)) neighbors))
    neighbors))


(defun walkability-coordinates-door-edges (classified zones)
  ;; The labeled zone graph: one (zone-a zone-b door-name) edge per door that joins two
  ;; distinct zones somewhere, deduplicated; a door interval interior to one zone (a
  ;; walkaround exists) contributes nothing.
  (let ((edges nil))
    (loop for key being the hash-keys of classified using (hash-value class)
          when (and (listp class) (eql (first class) :door))
            do (destructuring-bind (axis line cross) key
                 (let ((zone-a (if (eql axis :v)
                                 (aref zones line cross)
                                 (aref zones cross line)))
                       (zone-b (if (eql axis :v)
                                 (aref zones (1+ line) cross)
                                 (aref zones cross (1+ line)))))
                   (unless (= zone-a zone-b)
                     (pushnew (list (min zone-a zone-b) (max zone-a zone-b) (second class))
                              edges :test #'equal)))))
    edges))


;;;; QUERY FUNCTIONS ;;;;


(define-query walkability-coordinates-location-coords ()
  (do (assign $positions nil)
      (doall (?location location)
        (if (bind (location-coords> ?location $x $y))
          (push (list ?location $x $y) $positions)
          (error "No LOCATION-COORDS> is defined for location ~A." ?location)))
      $positions))


(define-query walkability-coordinates-stream-specs ()
  ;; Default: no air streams.  -stream-passability, nested by wall-blower, redefines
  ;; this to gather one (gears swept-location destination sx sy dx dy width) spec per
  ;; wall-gears from HAS-POSITION, AIMED-AT, LOCATION-COORDS>, and STREAM-WIDTH
  ;; facts -- so this file never references blower relations itself.
  (do (assign $specs nil)
      $specs))


;;;; INITIALIZATION ;;;;


(define-init-action derive-walk-via-from-segments
  ;; Derives WALK-VIA (and WALK-VIA> for rides into stream destinations) from the
  ;; problem's raw segment geometry -- see the file header for the region-connectivity
  ;; derivation.  Runs only when the problem has asserted WALL-SEGMENT> or
  ;; BOUNDARY-WALL -- inert otherwise, so a problem that hand-authors its own WALK-VIA
  ;; facts is unaffected.  Only one direction per symmetric pair is asserted: WALK-VIA
  ;; has no ">" suffix, so WW mirrors it both ways itself; a pair whose ride edges
  ;; widen a destination's inbound direction gets its two explicit WALK-VIA>
  ;; directions instead, never both kinds.
  0
  ()
  (or (exists (?wall wall)
        (bind (wall-segment> ?wall $x1 $y1 $x2 $y2)))
      (bind (boundary-wall $trigger-boundary)))
  ()
  (assert
    (do (assign $walls (wall-segment-records))
        (assign $gates (gate-segment-records))
        (assign $windows (window-segment-records))
        (assign $screens (screen-segment-records))
        (assign $boundary (if (bind (boundary-wall $boundary-points)) $boundary-points))
        (assign $stream-specs (walkability-coordinates-stream-specs))
        (assign $positions (walkability-coordinates-location-coords))
        (assign $arrangement (walkability-coordinates-build-arrangement
                               $positions $walls $gates $windows $screens $stream-specs $boundary))
        (doall (?source location)
          (doall (?destination location)
            (if (member ?destination
                        (rest (member ?source (gethash 'location *types*))))
              (do (assign $spec (walkability-coordinates-pair-spec
                                  $arrangement ?source ?destination))
                  (if $spec
                    (if (eql (first $spec) :sym)
                      (do (assign $family (second $spec))
                          (walk-via ?source $family ?destination))
                      (do (assign $forward (second $spec))
                          (assign $backward (third $spec))
                          (walk-via> ?source $forward ?destination)
                          (walk-via> ?destination $backward ?source))))))))
        (convert-databases-to-integers))))

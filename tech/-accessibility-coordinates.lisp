;;; Filename: -accessibility-coordinates.lisp

;;; Accessibility coordinates substrate: derives WALK-VIA from raw wall/gate/window/screen
;;; segment geometry, for a problem that would rather author 2D positions than hand-list
;;; which locations can walk to which.  Nested under accessibility-tech, so it is always
;;; present wherever (include-tech accessibility) is used; entirely inert unless the
;;; problem actually asserts WALL-SEGMENTS -- a problem that hand-authors WALK-VIA
;;; directly (as corner-topo does) is unaffected.
;;;
;;; Walking connectivity is a zone-adjacency question, not a sightline question.  Every
;;; wall/gate/window/screen segment is axis-aligned (a diagonal one is an authoring
;;; mistake, caught by ACCESSIBILITY-COORDINATES-ORIENTATION); segments sharing an
;;; orientation and perpendicular coordinate lie on one infinite partition line (a
;;; "group").  Two parallel segments a fixed distance apart -- eg, a paired gate
;;; straddling its own center line -- land in different groups automatically, with no
;;; special handling: they are simply two independent lines.
;;;
;;; DERIVE-WALK-VIA-FROM-SEGMENTS classifies each location by which side of every group's
;;; line it falls on, then connects two locations directly (no occluder) if they agree on
;;; every group's side.  Where they disagree, a group only contributes an occluder if its
;;; own covered extent (the union of its members' own bounded ranges) actually reaches
;;; near enough to plausibly separate the two locations -- ACCESSIBILITY-COORDINATES-
;;; GROUP-RELEVANT-P; a wall far from where either location sits is not what's blocking a
;;; path between them, regardless of which side of its infinite line they nominally fall
;;; on.  A relevant group with at most one gate/screen resolves directly (that occluder,
;;; or :BLOCKED if it's solid wall/window).  A relevant group with more than one gate/
;;; screen -- eg, two independent doors sitting end-to-end on the same line -- is split
;;; into cells at every point where a WALL or WINDOW segment (never another gate or
;;; screen; only a solid, physical partition counts) transversal to the group has its own
;;; bounded extent reaching the group's line.  A location pair resolving to the same cell
;;; uses that cell's own occluder; different cells (no single door serves the crossing),
;;; or a cell that itself still has more than one gate/screen (two doors coincide with no
;;; wall or window to justify telling them apart), both error or block rather than guess
;;; -- see ACCESSIBILITY-COORDINATES-CELL-OCCLUDER and -SPLIT-INTO-CELLS.
;;;
;;; Reuses LOCATION-POSITION> (nested from -location-coordinates, shared with beam-
;;; crossing-tech's own -beam-los-coordinates substrate) for location coordinates, so a
;;; location's position is entered once regardless of which capabilities a problem uses.
;;; Declares its own WALL-SEGMENTS/GATE-SEGMENTS/WINDOW-SEGMENTS (identical signatures to
;;; -beam-los-coordinates.lisp's own wall-segments/gate-segments, for a problem that also
;;; includes beam-direct or beam-crossing), so accessibility's own coordinate derivation
;;; never requires either beam tech to be included at all.  SCREEN-SEGMENTS has no
;;; counterpart there -- a screen affects walking only, never a sightline.
;;;
;;; Self-contained; spliced by (include-tech -accessibility-coordinates), nested from
;;; accessibility.
;;;
;;; REQUIRES:
;;;   types     : location  --  declared by the problem, as accessibility itself already
;;;               requires; screen declared optional by nested -passability, spliced by
;;;               accessibility.lisp before this file
;;;   nested    : -location-coordinates (LOCATION-POSITION>)
;;; PROVIDES:
;;;   relations : wall-segments, gate-segments, window-segments, screen-segments  --
;;;               default to no facts; a problem that asserts wall-segments gets WALK-VIA
;;;               derived automatically instead of hand-authoring it
;;;   init      : derive-walk-via-from-segments

(include-tech -location-coordinates)

(in-package :ww)


(define-static-relations
  (wall-segments $list)
  (gate-segments $list)
  (window-segments $list)
  (screen-segments $list))


;;;; GEOMETRY HELPERS ;;;;
;;;; Plain Lisp functions operating on segments/positions passed as arguments -- no live
;;;; database access, so no WW query wrapper is needed for these.


(defun accessibility-coordinates-orientation (segment)
  ;; :horizontal if SEGMENT's two endpoints share Y, :vertical if they share X.  Every
  ;; wall/gate/window/screen segment in this codebase is axis-aligned; a diagonal one is
  ;; an authoring mistake to catch here, not a case to generalize the geometry for.
  (let ((x1 (second segment)) (y1 (third segment))
        (x2 (fourth segment)) (y2 (fifth segment)))
    (cond ((= y1 y2) :horizontal)
          ((= x1 x2) :vertical)
          (t (error "Segment ~A (~A ~A ~A ~A) is not axis-aligned."
                    (first segment) x1 y1 x2 y2)))))


(defun accessibility-coordinates-line-coordinate (segment orientation)
  ;; SEGMENT's fixed perpendicular coordinate -- Y for a :horizontal segment, X for a
  ;; :vertical one -- identifying which infinite line it lies on.
  (if (eql orientation :horizontal) (third segment) (second segment)))


(defun accessibility-coordinates-along-range (segment orientation)
  ;; SEGMENT's own extent along its line's direction, as a sorted (low . high) pair: X
  ;; for a :horizontal segment, Y for a :vertical one.
  (let ((a (if (eql orientation :horizontal) (second segment) (third segment)))
        (b (if (eql orientation :horizontal) (fourth segment) (fifth segment))))
    (cons (min a b) (max a b))))


(defun accessibility-coordinates-tagged-segments (walls gates windows screens)
  ;; Pools WALL-SEGMENTS/GATE-SEGMENTS/WINDOW-SEGMENTS/SCREEN-SEGMENTS into one list, each
  ;; element a (kind segment) pair, so later grouping can still tell a gate or screen from
  ;; a wall/window on the same partition line.
  (append (mapcar (lambda (segment) (list :wall segment)) walls)
          (mapcar (lambda (segment) (list :gate segment)) gates)
          (mapcar (lambda (segment) (list :window segment)) windows)
          (mapcar (lambda (segment) (list :screen segment)) screens)))


(defun accessibility-coordinates-group-into-lines (tagged-segments)
  ;; Groups TAGGED-SEGMENTS by shared orientation and perpendicular coordinate -- one
  ;; group per infinite line.
  (let ((groups (make-hash-table :test 'equal)))
    (dolist (tagged-segment tagged-segments)
      (let* ((orientation (accessibility-coordinates-orientation (second tagged-segment)))
             (coordinate (accessibility-coordinates-line-coordinate
                           (second tagged-segment) orientation)))
        (push tagged-segment (gethash (list orientation coordinate) groups))))
    (loop for group being the hash-values of groups collect group)))


(defun accessibility-coordinates-group-orientation (group)
  (accessibility-coordinates-orientation (second (first group))))


(defun accessibility-coordinates-group-coordinate (group)
  (accessibility-coordinates-line-coordinate
    (second (first group)) (accessibility-coordinates-group-orientation group)))


(defun accessibility-coordinates-group-extent (group)
  ;; GROUP's own total covered along-line range: the union of all its members' own
  ;; extents, as a (low . high) pair.
  (let ((orientation (accessibility-coordinates-group-orientation group)))
    (loop for (kind segment) in group
          for range = (accessibility-coordinates-along-range segment orientation)
          minimize (car range) into low
          maximize (cdr range) into high
          finally (return (cons low high)))))


(defun accessibility-coordinates-side (point orientation coordinate)
  ;; Returns the sign (-1, 0, or +1) of POINT -- an (x y) pair -- relative to the
  ;; infinite line at COORDINATE: comparing Y for a :horizontal line, X for a :vertical
  ;; one.
  (let ((value (if (eql orientation :horizontal) (second point) (first point))))
    (cond ((> value coordinate) 1)
          ((< value coordinate) -1)
          (t 0))))


(defun accessibility-coordinates-along-position (point orientation)
  ;; POINT's own coordinate along a line's direction: X for a :horizontal line, Y for a
  ;; :vertical one.
  (if (eql orientation :horizontal) (first point) (second point)))


(defun accessibility-coordinates-cell-bounds (split-points)
  ;; Returns one (low . high) pair per cell -- NIL meaning unbounded -- given
  ;; SPLIT-POINTS sorted increasingly: N split points make N+1 cells.
  (let ((edges (append (list nil) split-points (list nil))))
    (loop for low in edges
          for high in (rest edges)
          collect (cons low high))))


(defun accessibility-coordinates-in-bounds-p (range bounds)
  ;; True if RANGE (a low . high pair) falls entirely within BOUNDS (a low . high pair,
  ;; either end possibly NIL for unbounded).
  (and (or (null (car bounds)) (>= (car range) (car bounds)))
       (or (null (cdr bounds)) (<= (cdr range) (cdr bounds)))))


(defun accessibility-coordinates-split-points (group all-tagged-segments)
  ;; The sorted, deduplicated positions where a WALL or WINDOW segment -- never a gate or
  ;; screen; only a solid, physical partition counts -- transversal to GROUP's own
  ;; orientation has its own bounded extent reaching GROUP's line coordinate.  Each such
  ;; position splits GROUP's members into independent cells (see ACCESSIBILITY-
  ;; COORDINATES-SPLIT-INTO-CELLS); two doors merely touching each other, with no such
  ;; wall to justify treating them apart, are never split -- see ACCESSIBILITY-
  ;; COORDINATES-CELL-OCCLUDER's error for that case.
  (let* ((orientation (accessibility-coordinates-group-orientation group))
         (coordinate (accessibility-coordinates-group-coordinate group))
         (transversal (if (eql orientation :horizontal) :vertical :horizontal)))
    (sort
      (remove-duplicates
        (loop for (kind segment) in all-tagged-segments
              when (and (member kind '(:wall :window))
                        (eql (accessibility-coordinates-orientation segment) transversal)
                        (let ((range (accessibility-coordinates-along-range
                                       segment transversal)))
                          (<= (car range) coordinate (cdr range))))
                collect (accessibility-coordinates-line-coordinate segment transversal)))
      #'<)))


(defun accessibility-coordinates-split-into-cells (group split-points)
  ;; Partitions GROUP's own members into cells at SPLIT-POINTS -- each cell a list of
  ;; (kind segment) pairs whose own along-range falls entirely within that cell's bounds.
  ;; Two members that only touch at a shared split point (eg, a wall-justified pair of
  ;; adjacent doors) land in different cells.  Errors if some member's own range
  ;; straddles a split point's interior: a gate can't be crossed by a wall through its
  ;; own middle.
  (let* ((orientation (accessibility-coordinates-group-orientation group))
         (cells (mapcar (lambda (bounds)
                          (remove-if-not
                            (lambda (tagged-segment)
                              (accessibility-coordinates-in-bounds-p
                                (accessibility-coordinates-along-range
                                  (second tagged-segment) orientation)
                                bounds))
                            group))
                        (accessibility-coordinates-cell-bounds split-points))))
    (if (= (reduce #'+ cells :key #'length) (length group))
      cells
      (error "A member of partition line ~A straddles split point(s) ~A; its own extent ~
              must fall entirely on one side."
             group split-points))))


(defun accessibility-coordinates-cell-extent (cell orientation)
  ;; CELL's own total covered along-line range -- the union of its members' own extents.
  (loop for (kind segment) in cell
        for range = (accessibility-coordinates-along-range segment orientation)
        minimize (car range) into low
        maximize (cdr range) into high
        finally (return (cons low high))))


(defun accessibility-coordinates-cell-occluder (cell)
  ;; Returns CELL's single occluder name -- a gate or screen -- or the keyword :BLOCKED
  ;; if CELL has neither (solid wall/window, no door).  Errors if CELL still has more
  ;; than one gate/screen: two doors coincide with no wall or window to justify treating
  ;; them as separate crossings -- an authoring ambiguity, not a case to guess at.
  (let ((occluder-names (loop for (kind segment) in cell
                              when (member kind '(:gate :screen))
                                collect (first segment))))
    (cond ((null occluder-names) :blocked)
          ((= (length occluder-names) 1) (first occluder-names))
          (t (error "Gate/screen segments ~A coincide on the same partition line with no ~
                     wall or window endpoint to justify treating them as separate doors."
                    occluder-names)))))


(defun accessibility-coordinates-position-cell (position orientation cells)
  ;; The cell (among CELLS, in order) whose own covered extent contains POSITION --
  ;; half-open on the high end, except the last cell, so a position exactly at a shared
  ;; boundary resolves to the higher cell, and a position outside every cell's coverage
  ;; (the group doesn't physically reach it) returns NIL.
  (loop for remaining on cells
        for cell = (first remaining)
        for extent = (accessibility-coordinates-cell-extent cell orientation)
        when (and (>= position (car extent))
                  (if (rest remaining) (< position (cdr extent)) (<= position (cdr extent))))
          return cell))


(defun accessibility-coordinates-group-relevant-p (position1 position2 orientation extent)
  ;; True if the range between POSITION1 and POSITION2's own along-line coordinates
  ;; overlaps EXTENT (a group's own covered range) at all.  False otherwise: a wall that
  ;; doesn't reach anywhere near where either location sits can't be what's blocking a
  ;; path between them, regardless of which side of its infinite line they nominally
  ;; fall on.
  (let ((along1 (accessibility-coordinates-along-position position1 orientation))
        (along2 (accessibility-coordinates-along-position position2 orientation)))
    (and (<= (min along1 along2) (cdr extent))
         (>= (max along1 along2) (car extent)))))


(defun accessibility-coordinates-crossing-occluder (position1 position2 group all-tagged-segments)
  ;; Resolves GROUP's own occluder contribution for a location pair already known to
  ;; differ in side of GROUP's line.  Returns :IRRELEVANT if GROUP's own covered extent
  ;; doesn't reach near either location; otherwise resolves directly if GROUP has at most
  ;; one gate/screen, or via ACCESSIBILITY-COORDINATES-SPLIT-INTO-CELLS if it has more
  ;; than one -- :BLOCKED if the two positions land in different cells (no single door
  ;; serves the crossing) or neither lands in any cell, else that cell's own occluder.
  (let* ((orientation (accessibility-coordinates-group-orientation group))
         (extent (accessibility-coordinates-group-extent group)))
    (if (not (accessibility-coordinates-group-relevant-p position1 position2 orientation extent))
      :irrelevant
      (let ((occluder-count (count-if (lambda (tagged-segment)
                                        (member (first tagged-segment) '(:gate :screen)))
                                      group)))
        (if (<= occluder-count 1)
          (accessibility-coordinates-cell-occluder group)
          (let* ((split-points (accessibility-coordinates-split-points group all-tagged-segments))
                 (cells (accessibility-coordinates-split-into-cells group split-points))
                 (cell1 (accessibility-coordinates-position-cell
                          (accessibility-coordinates-along-position position1 orientation)
                          orientation cells))
                 (cell2 (accessibility-coordinates-position-cell
                          (accessibility-coordinates-along-position position2 orientation)
                          orientation cells)))
            (if (and cell1 (eq cell1 cell2))
              (accessibility-coordinates-cell-occluder cell1)
              :blocked)))))))


(defun accessibility-coordinates-walk-via-occluders (position1 position2 groups all-tagged-segments)
  ;; For each of GROUPS, compares POSITION1 and POSITION2's side.  Agreeing groups need
  ;; no occluder.  Differing groups resolve via ACCESSIBILITY-COORDINATES-CROSSING-
  ;; OCCLUDER, filtered for :IRRELEVANT (a group too far away to be a real partition
  ;; here).  Returns :BLOCKED overall if any relevant, differing group blocks, else the
  ;; (possibly empty) list of occluder names.
  (let ((crossings
          (loop for group in groups
                for orientation = (accessibility-coordinates-group-orientation group)
                for coordinate = (accessibility-coordinates-group-coordinate group)
                when (/= (accessibility-coordinates-side position1 orientation coordinate)
                         (accessibility-coordinates-side position2 orientation coordinate))
                  collect (accessibility-coordinates-crossing-occluder
                            position1 position2 group all-tagged-segments))))
    (let ((relevant (remove :irrelevant crossings)))
      (if (member :blocked relevant) :blocked relevant))))


;;;; QUERY FUNCTIONS ;;;;


(define-query accessibility-coordinates-location-positions ()
  (do (assign $positions nil)
      (doall (?location location)
        (if (bind (location-position> ?location $x $y))
          (push (list ?location $x $y) $positions)
          (error "No LOCATION-POSITION> is defined for location ~A." ?location)))
      $positions))


;;;; INITIALIZATION ;;;;


(define-init-action derive-walk-via-from-segments
  ;; Derives WALK-VIA from WALL-SEGMENTS/GATE-SEGMENTS/WINDOW-SEGMENTS/SCREEN-SEGMENTS raw
  ;; segment geometry, when the problem supplies it, instead of requiring it hand-authored.
  ;; A screen bridges a partition line exactly like a gate -- structurally never a solid
  ;; partition -- but has no (open ...) state of its own; -passability.lisp's OBSTACLE-CLEAR
  ;; gates it on the traversing agent being empty-handed instead.  Runs only when the
  ;; problem has asserted WALL-SEGMENTS -- inert otherwise, so a problem that hand-authors
  ;; its own WALK-VIA facts (as corner-topo does) is unaffected.  Only one direction per
  ;; location pair is asserted: WALK-VIA has no ">" suffix, so it is auto-symmetric (WW
  ;; mirrors it both ways itself), the same as LOS-TO-LOCATION.
  0
  ()
  (bind (wall-segments $walls))
  ()
  (assert
    (do (bind (gate-segments $gates))
        (bind (window-segments $windows))
        (bind (screen-segments $screens))
        (assign $all-tagged-segments
                (accessibility-coordinates-tagged-segments $walls $gates $windows $screens))
        (assign $groups (accessibility-coordinates-group-into-lines $all-tagged-segments))
        (assign $positions (accessibility-coordinates-location-positions))
        (doall (?source location)
          (doall (?destination location)
            (if (member ?destination
                        (rest (member ?source (gethash 'location *types*))))
              (do (assign $occluders
                          (accessibility-coordinates-walk-via-occluders
                            (rest (assoc ?source $positions))
                            (rest (assoc ?destination $positions))
                            $groups $all-tagged-segments))
                  (if (not (eql $occluders :blocked))
                    (walk-via ?source $occluders ?destination))))))
        (convert-databases-to-integers))))

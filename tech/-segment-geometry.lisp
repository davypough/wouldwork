;;; Filename: -segment-geometry.lisp

;;; Shared coordinate segment geometry: typed, individually authored wall/edge/gate/window/
;;; screen segments, the ordered boundary polygon, list-gathering queries for the geometry
;;; algorithms, and initialization validation.  A wall is a vertical linear partition; an
;;; edge is a vertical surface separating two regions of different elevation (eg, the
;;; ground-level footprint of a raised slab).  Both block walking identically and both have
;;; finite height for LOS.  Only wall participates in vaulting: an edge is not a physical
;;; feature whose top an agent can vault onto, so jump.lisp deliberately excludes it.


(in-package :ww)


(define-optional-types wall edge gate window screen)


(define-static-relations
  (wall-segment> wall $rational $rational $rational $rational)
  (edge-segment> edge $rational $rational $rational $rational)
  (gate-segment> gate $rational $rational $rational $rational)
  (window-segment> window $rational $rational $rational $rational)
  (screen-segment> screen $rational $rational $rational $rational)
  (boundary-wall $list))  ;closed polygon ((x1 y1) ... (x1 y1)); final point must repeat first


(defvar *boundary-wall-height* 6
  "The boundary polygon's height, for the sightline crossing test.  A boundary is the only
   barrier with no named object, so it can carry no HAS-HEIGHT and has no entry in
   -vertical's per-type table; this parameter is where its height lives instead, rather than
   as a bare literal inside the crossing test where it could silently drift from the
   documented default.  Its base is 0 by definition -- a boundary is the room's silhouette,
   standing on the ground.  A problem can override this with its own DEFPARAMETER.")


;;;; SEGMENT GATHERING ;;;;


(define-query wall-segment-records ()
  (do (assign $records nil)
      (doall (?wall wall)
        (if (bind (wall-segment> ?wall $x1 $y1 $x2 $y2))
          (push (list ?wall $x1 $y1 $x2 $y2) $records)))
      $records))


(define-query edge-segment-records ()
  (do (assign $records nil)
      (doall (?edge edge)
        (if (bind (edge-segment> ?edge $x1 $y1 $x2 $y2))
          (push (list ?edge $x1 $y1 $x2 $y2) $records)))
      $records))


(define-query gate-segment-records ()
  (do (assign $records nil)
      (doall (?gate gate)
        (if (bind (gate-segment> ?gate $x1 $y1 $x2 $y2))
          (push (list ?gate $x1 $y1 $x2 $y2) $records)))
      $records))


(define-query window-segment-records ()
  (do (assign $records nil)
      (doall (?window window)
        (if (bind (window-segment> ?window $x1 $y1 $x2 $y2))
          (push (list ?window $x1 $y1 $x2 $y2) $records)))
      $records))


(define-query screen-segment-records ()
  (do (assign $records nil)
      (doall (?screen screen)
        (if (bind (screen-segment> ?screen $x1 $y1 $x2 $y2))
          (push (list ?screen $x1 $y1 $x2 $y2) $records)))
      $records))


;;;; INITIALIZATION VALIDATION ;;;;


(define-init-check segment-geometry-init-check (literals)
  (check-init-boundary-walls literals)
  (check-init-segment-geometry literals)
  (check-init-segment-names-unique literals)
  (check-init-segment-types-covered literals))


(define-init-check-helper check-init-boundary-walls (literals)
  "Checks BOUNDARY-WALL point lists independently of any geometry consumer.
   A boundary has at least four explicitly authored edges and closes by repeating
   its first point as its final point.  Every edge is horizontal or vertical, matching
   the shared coordinate-geometry contract used by the segment relations."
  (dolist (literal (positive-init-literals-with-relation 'boundary-wall literals))
    (init-check-boundary-wall
      literal
      (second (init-literal-proposition literal)))))


(define-init-check-helper init-check-boundary-wall (literal points)
  (unless (and (listp points)
               (>= (length points) 5))
    (fail-init-check nil "~%BOUNDARY-WALL must contain at least four edges and an explicit closing point.~%~
            Literal: ~S~%~
            Points:  ~S"
           literal points))
  (dolist (point points)
    (unless (and (listp point)
                 (= (length point) 2)
                 (every #'rationalp point))
      (fail-init-check nil "~%Malformed point in BOUNDARY-WALL.~%~
              Literal: ~S~%~
              Point:   ~S~%~
              Expected shape: (x y), with rational coordinates."
             literal point)))
  (unless (equal (first points) (car (last points)))
    (fail-init-check nil "~%BOUNDARY-WALL must repeat its first point as its final point.~%~
            Literal:     ~S~%~
            First point: ~S~%~
            Final point: ~S"
           literal (first points) (car (last points))))
  (loop for (point1 point2) on points
        while point2
        when (equal point1 point2)
          do (fail-init-check nil "~%BOUNDARY-WALL contains a zero-length edge.~%~
                     Literal: ~S~%~
                     Point:   ~S"
                    literal point1)
        unless (or (= (first point1) (first point2))
                   (= (second point1) (second point2)))
          do (fail-init-check nil "~%BOUNDARY-WALL contains an edge that is not axis-aligned.~%~
                     Literal: ~S~%~
                     Edge:    ~S -> ~S~%~
                     Edges must be horizontal or vertical."
                    literal point1 point2)))


(define-init-check-helper init-segment-relation-types ()
  "The individually authored segment relations and their keyed object types."
  '((wall-segment> . wall)
    (edge-segment> . edge)
    (gate-segment> . gate)
    (window-segment> . window)
    (screen-segment> . screen)))


(define-init-check-helper init-segment-records (relation literals)
  (loop for literal in (positive-init-literals-with-relation relation literals)
        collect (rest (init-literal-proposition literal))))


(define-init-check-helper check-init-segment-geometry (literals)
  (dolist (entry (init-segment-relation-types))
    (dolist (record (init-segment-records (car entry) literals))
      (init-check-segment-record (car entry) record))))


(define-init-check-helper init-check-segment-record (kind record)
  (let ((x1 (second record))
        (y1 (third record))
        (x2 (fourth record))
        (y2 (fifth record)))
    (when (and (= x1 x2) (= y1 y2))
      (fail-init-check nil "~%Zero-length ~S record in DEFINE-INIT.~%~
              Record: ~S"
             kind record))
    (unless (or (= x1 x2) (= y1 y2))
      (fail-init-check nil "~%~S record in DEFINE-INIT is not axis-aligned.~%~
              Record: ~S~%~
              Segments must be horizontal (y1 = y2) or vertical (x1 = x2)."
             kind record))))


(define-init-check-helper check-init-segment-names-unique (literals)
  (let ((seen (make-hash-table :test #'eql)))
    (dolist (entry (init-segment-relation-types))
      (dolist (record (init-segment-records (car entry) literals))
        (ut::if-it (gethash (first record) seen)
          (fail-init-check nil "~%Duplicate segment name in DEFINE-INIT.~%~
                  Name:        ~S~%~
                  First kind:  ~S~%~
                  Second kind: ~S"
                 (first record) ut::it (car entry))
          (setf (gethash (first record) seen) (car entry)))))))


(define-init-check-helper check-init-segment-types-covered (literals)
  (when (or (positive-init-literals-with-relation 'wall-segment> literals)
            (positive-init-literals-with-relation 'edge-segment> literals))
    (dolist (entry (init-segment-relation-types))
      (init-check-segment-type-coverage (car entry) (cdr entry) literals))))


(define-init-check-helper init-check-segment-type-coverage (kind type literals)
  (let ((names (mapcar #'first (init-segment-records kind literals))))
    (dolist (instance (init-type-instances type))
      (unless (member instance names)
        (fail-init-check nil "~%Declared ~S instance has no ~S record in DEFINE-INIT.~%~
                Instance:       ~S~%~
                Record names:   ~S~%~
                Every ~S instance in a coordinate-driven problem must contribute a segment."
               type kind instance names type)))))

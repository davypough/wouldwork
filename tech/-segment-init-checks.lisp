;;; Filename: -segment-init-checks.lisp

;;; Initialization validation for shared coordinate segment geometry.


(in-package :ww)


(define-init-check segment-geometry-init-check (literals)
  (:consumes wall gate window screen)
  (check-init-boundary-walls literals)
  (check-init-segment-records-well-formed literals)
  (check-init-segment-names-unique literals)
  (check-init-segment-names-typed literals)
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
  "The named-segment relations and the object type each record name must instantiate."
  '((wall-segments . wall)
    (gate-segments . gate)
    (window-segments . window)
    (screen-segments . screen)))


(define-init-check-helper init-segment-records (relation literals)
  (loop for literal in (positive-init-literals-with-relation relation literals)
        append (second (init-literal-proposition literal))))


(define-init-check-helper check-init-segment-records-well-formed (literals)
  (dolist (entry (init-segment-relation-types))
    (dolist (record (init-segment-records (car entry) literals))
      (init-check-segment-record (car entry) record))))


(define-init-check-helper init-check-segment-record (kind record)
  (unless (and (listp record)
               (= (length record) 5)
               (symbolp (first record))
               (first record)
               (every #'rationalp (rest record)))
    (fail-init-check nil "~%Malformed ~S record in DEFINE-INIT.~%~
            Record: ~S~%~
            Expected shape: (name x1 y1 x2 y2), a symbol name and rational coordinates."
           kind record))
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


(define-init-check-helper check-init-segment-names-typed (literals)
  (dolist (entry (init-segment-relation-types))
    (dolist (record (init-segment-records (car entry) literals))
      (unless (init-type-member-p (first record) (cdr entry))
        (fail-init-check nil "~%Segment name in DEFINE-INIT is not a declared instance of its type.~%~
                Record kind:   ~S~%~
                Record:        ~S~%~
                Expected type: ~S~%~
                Declare ~S as an instance of type ~S in DEFINE-TYPES."
               (car entry) record (cdr entry) (first record) (cdr entry))))))


(define-init-check-helper check-init-segment-types-covered (literals)
  (when (positive-init-literals-with-relation 'wall-segments literals)
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



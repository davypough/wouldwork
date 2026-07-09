;;; Filename: -beam-coordinates.lisp

;;; Beam coordinates substrate: an alternate, coordinate-based way to populate
;;; CROSSINGS-ALONG-BEAM>, for a problem that would rather author 2D positions than
;;; hand-list which crossings lie on which beam and in what order.  Nested under
;;; beam-crossing-tech, so it is always present wherever (include-tech beam-crossing) is
;;; used; entirely inert unless the problem actually asserts BEAM-POSITION> facts -- a
;;; purely topological problem that hand-authors CROSSINGS-ALONG-BEAM> directly (as
;;; corner-topo2 does) is unaffected.  Reuses the problem's own LOS-TO-FIXTURE and
;;; LOS-TO-LOCATION sightline facts (already authored for visibility) to enumerate which
;;; beams exist; the problem still declares its own CROSSING pool (crossing1, crossing2,
;;; ...) by name and count -- WW's compile-time DOALL domain can't discover dynamically-
;;; created objects, so the pool size can't itself be derived -- but no longer needs to
;;; know or order which crossings occur on which beam; that is fully computed here.  Does
;;; not assert BEAM-CROSSING> -- beam-crossing-tech derives that itself, lazily, from
;;; whichever facts populate CROSSINGS-ALONG-BEAM>, regardless of how they got there.
;;;
;;; Self-contained; spliced by (include-tech -beam-coordinates), nested from beam-crossing.
;;;
;;; REQUIRES:
;;;   types     : location, beam-endpoint  --  declared by the problem, as beam-crossing
;;;               itself already requires; transmitter, receiver declared optional by
;;;               -beam-substrate, a sibling nested include
;;;   relations : los-to-fixture, los-to-location  --  from visibility-tech; only consulted
;;;               if the problem also includes visibility and authors BEAM-POSITION> facts
;;; PROVIDES:
;;;   types     : crossing  --  declared optional here (empty default); a problem with any
;;;               crossings still declares its own crossing1, crossing2, ... pool, matching
;;;               the count of geometric intersections its coordinates produce
;;;   relations : beam-position>
;;;   init      : establish-beam-coordinates

(in-package :ww)


(define-optional-types crossing)


(define-static-relations
  (beam-position> beam-endpoint $rational $rational))


;;;; GEOMETRY HELPERS ;;;;
;;;; Plain Lisp functions operating on positions/beams passed as arguments -- no live
;;;; database access, so no WW query wrapper is needed for these.


(defun beam-coordinates-position (endpoint positions)
  (or (rest (assoc endpoint positions))
      (error "No BEAM-POSITION> is defined for beam endpoint ~A." endpoint)))


(defun beam-coordinates-proper-intersection-parameters (beam1 beam2 positions)
  ;; Return each segment's parameter at a proper interior intersection.  Shared
  ;; endpoints and parallel or collinear segments are not beam crossings.
  (when (intersection beam1 beam2 :test #'eql)
    (return-from beam-coordinates-proper-intersection-parameters nil))
  (let* ((position1 (beam-coordinates-position (first beam1) positions))
         (position2 (beam-coordinates-position (second beam1) positions))
         (position3 (beam-coordinates-position (first beam2) positions))
         (position4 (beam-coordinates-position (second beam2) positions))
         (x1 (first position1))
         (y1 (second position1))
         (x2 (first position2))
         (y2 (second position2))
         (x3 (first position3))
         (y3 (second position3))
         (x4 (first position4))
         (y4 (second position4))
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
        (when (and (< 0 parameter1 1)
                   (< 0 parameter2 1))
          (values parameter1 parameter2))))))


(defun beam-coordinates-geometric-crossings (beams positions)
  ;; Each result is (beam1 parameter1 beam2 parameter2), in deterministic nested
  ;; beam order -- which becomes the crossing1, crossing2, ... assignment order.
  (loop for remaining on beams
        for beam1 = (first remaining)
        append
        (loop for beam2 in (rest remaining)
              for parameters = (multiple-value-list
                                  (beam-coordinates-proper-intersection-parameters
                                    beam1 beam2 positions))
              when (first parameters)
                collect (list beam1 (first parameters)
                              beam2 (second parameters)))))


(defun beam-coordinates-crossing-records (beams positions crossing-objects)
  ;; Attaches the problem's declared crossing pool to computed geometry, in order.
  ;; Record shape: (crossing beam1 parameter1 beam2 parameter2).
  (let ((geometry (beam-coordinates-geometric-crossings beams positions)))
    (unless (= (length geometry) (length crossing-objects))
      (error "Computed ~D beam crossings, but type CROSSING declares ~D objects."
             (length geometry) (length crossing-objects)))
    (mapcar (lambda (crossing intersection)
              (cons crossing intersection))
            crossing-objects geometry)))


(defun beam-coordinates-crossings-on-beam (beam records)
  (let ((parameter&crossing
          (loop for record in records
                when (equal beam (second record))
                  collect (cons (third record) (first record))
                when (equal beam (fourth record))
                  collect (cons (fifth record) (first record)))))
    (setf parameter&crossing (sort parameter&crossing #'< :key #'car))
    (loop for (left right) on parameter&crossing
          while right
          when (= (car left) (car right))
            do (error "Beam ~A has multiple crossings at parameter ~A."
                      beam (car left)))
    (mapcar #'cdr parameter&crossing)))


;;;; QUERY FUNCTIONS ;;;;


(define-query beam-coordinates-potential-beams ()
  ;; Canonical deterministic order: transmitter -> location, then location -> receiver,
  ;; then location -> location in declared type order (los-to-location is symmetric at
  ;; runtime, so the type-order test avoids counting each L-L pair twice).
  (do (assign $beams nil)
      (doall (?location location)
        (doall (?transmitter transmitter)
          (if (bind (los-to-fixture ?location $gates ?transmitter))
            (push (list ?transmitter ?location) $beams))))
      (doall (?location location)
        (doall (?receiver receiver)
          (if (bind (los-to-fixture ?location $gates ?receiver))
            (push (list ?location ?receiver) $beams))))
      (doall (?source location)
        (doall (?destination location)
          (if (and (member ?destination
                           (rest (member ?source (gethash 'location *types*))))
                   (bind (los-to-location ?source $gates ?destination)))
            (push (list ?source ?destination) $beams))))
      (reverse $beams)))


(define-query beam-coordinates-endpoint-positions ()
  (do (assign $positions nil)
      (doall (?endpoint beam-endpoint)
        (if (bind (beam-position> ?endpoint $x $y))
          (push (list ?endpoint $x $y) $positions)
          (error "No BEAM-POSITION> is defined for beam endpoint ~A." ?endpoint)))
      $positions))


;;;; INITIALIZATION ;;;;


(define-init-action establish-beam-coordinates
  ;; Runs only when the problem has authored BEAM-POSITION> facts -- inert otherwise, so
  ;; a purely topological problem (hand-authoring CROSSINGS-ALONG-BEAM> directly) is
  ;; entirely unaffected.  Computes every proper beam intersection from the authored
  ;; sightlines (LOS-TO-FIXTURE, LOS-TO-LOCATION) and coordinates, and asserts
  ;; CROSSINGS-ALONG-BEAM> accordingly.  Does not assert BEAM-CROSSING> -- beam-crossing
  ;; derives that itself, lazily, from whichever facts populate CROSSINGS-ALONG-BEAM>.
  ;; Ends with an explicit CONVERT-DATABASES-TO-INTEGERS: this init-action's own effect
  ;; is compiled without the usual int-code substitution (do-init-action-updates compiles
  ;; every init-action's effect with a plain COMPILE), so a newly-asserted static fact
  ;; only lands in *STATIC-DB* (symbolic).  Every DEFINE-QUERY/DEFINE-UPDATE, including
  ;; this problem's own INITIALIZE-DERIVED-STATE that runs right after this one in the
  ;; same DO-INIT-ACTION-UPDATES pass, was already compiled against *STATIC-IDB*
  ;; (integer-keyed) earlier, during the system's one DO-INTEGER-CONVERSION pass, which
  ;; runs before any init-action fires.  Without this explicit re-sync, initialize-
  ;; derived-state's propagate-changes! would read a not-yet-updated *STATIC-IDB* and
  ;; find no crossings at all.
  0
  ()
  (exists (?e beam-endpoint) (bind (beam-position> ?e $x $y)))
  ()
  (assert
    (do (assign $beams (beam-coordinates-potential-beams))
        (assign $positions (beam-coordinates-endpoint-positions))
        (assign $records
                (beam-coordinates-crossing-records
                  $beams $positions (gethash 'crossing *types*)))
        (ww-loop for $beam in $beams
                 do (assign $crossings
                            (beam-coordinates-crossings-on-beam $beam $records))
                    (if $crossings
                      (do (crossings-along-beam>
                            (first $beam) $crossings (second $beam))
                          (if (and (member (first $beam)
                                           (gethash 'location *types*))
                                   (member (second $beam)
                                           (gethash 'location *types*)))
                            (crossings-along-beam>
                              (second $beam) (reverse $crossings)
                              (first $beam))))))
        (convert-databases-to-integers))))

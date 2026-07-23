;;; Filename: -beam-crossing-coordinates.lisp

;;; Beam crossing coordinates substrate: computes CROSSINGS-ALONG-BEAM>/CROSSINGS-BEFORE-GATE>
;;; from the beam geometry -beam-los-coordinates.lisp already derived (or the problem hand-
;;; authored) as LOS-TO-TRANSCEIVER/LOS-TO-LOCATION, for a problem that would rather author 2D
;;; positions than hand-list which crossings lie on which beam and in what order.  Nested under
;;; beam-crossing-tech only -- unlike -beam-los-coordinates, this file's derivations are never
;;; useful to a problem that includes beam-direct alone, since CROSSINGS-ALONG-BEAM>/CROSSINGS-
;;; BEFORE-GATE> are consumed only by beam-crossing's own BEAM-REACHES-CROSSING.  A problem
;;; wanting two crossing direct beams includes both beam-direct and beam-crossing together;
;;; splicing is deduplicated per problem copy and this file always nests -beam-los-coordinates
;;; as its own first form, so LOS derivation runs before crossing derivation regardless of
;;; whether visibility-tech (which also nests -beam-los-coordinates, as the owner of the los
;;; relations it derives) was listed before or after beam-crossing.
;;;
;;; The problem still declares its own CROSSING pool (crossing1, crossing2, ...) by name and
;;; count -- WW's compile-time DOALL domain can't discover dynamically-created objects, so the
;;; pool size can't itself be derived -- but no longer needs to know or order which crossings
;;; occur on which beam; that is fully computed here.  Does not assert BEAM-CROSSING> --
;;; beam-crossing-tech derives that itself, lazily, from whichever facts populate CROSSINGS-
;;; ALONG-BEAM>, regardless of how they got there.
;;;
;;; Also derives CROSSINGS-BEFORE-GATE> (declared by beam-crossing-tech, alongside
;;; CROSSINGS-ALONG-BEAM>) when the problem asserts GATE-SEGMENTS: DERIVE-CROSSINGS-BEFORE-GATE
;;; splits each gate-conditioned beam's crossing set at that gate's own crossing parameter on
;;; the beam -- independently per gate, for a beam conditioned on more than one.  Walls have no
;;; counterpart here: a wall-blocked beam is excluded from LOS-TO-TRANSCEIVER/LOS-TO-LOCATION
;;; entirely by -beam-los-coordinates, so it never becomes a beam to split in the first place.
;;; Without a populated CROSSINGS-BEFORE-GATE>, BEAM-REACHES-CROSSING's (beam-crossing.lisp) own
;;; gate check is vacuously satisfied, so a beam paired through a gate stays live for cutting
;;; along its full geometric length even after the gate closes.
;;;
;;; Self-contained; spliced by (include-tech -beam-crossing-coordinates), nested from
;;; beam-crossing.
;;;
;;; REQUIRES:
;;;   nested    : -beam-los-coordinates (BEAM-ENDPOINT type; TRANSCEIVER-POSITION>, LOCATION-
;;;               POSITION>; LOS-TO-TRANSCEIVER/LOS-TO-LOCATION, hand-authored or derived)
;;;   relations : crossings-along-beam>, crossings-before-gate>  --  declared by
;;;               beam-crossing.lisp itself, the parent tech this file is always nested under
;;; PROVIDES:
;;;   nested    : -beam-los-coordinates
;;;   types     : crossing  --  declared optional here (empty default); a problem with any
;;;               crossings still declares its own crossing1, crossing2, ... pool, matching
;;;               the count of geometric intersections its coordinates produce
;;;   init      : establish-beam-coordinates, derive-crossings-before-gate  --  in this order,
;;;               and both after -beam-los-coordinates' own derive-los-from-segments

(include-tech -beam-los-coordinates)

(in-package :ww)


(define-optional-types crossing)


;;;; GEOMETRY HELPERS ;;;;
;;;; Plain Lisp functions operating on positions/beams passed as arguments -- no live
;;;; database access, so no WW query wrapper is needed for these.


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


(defun beam-coordinates-split-crossings-at-parameter (beam records split-parameter)
  ;; Splits BEAM's crossings (drawn from RECORDS, the same beam-crossing intersection
  ;; records BEAM-COORDINATES-CROSSING-RECORDS computes) at SPLIT-PARAMETER -- the point
  ;; along BEAM where one of its occluding gates' own segment properly intersects it.
  ;; Returns a cons of (BEFORE . AFTER): BEFORE is BEAM's crossings with a smaller
  ;; parameter, in increasing order (nearest BEAM's first endpoint first) -- CROSSINGS-
  ;; BEFORE-GATE>'s value for that endpoint; AFTER is the crossings with a larger
  ;; parameter, in decreasing order (nearest BEAM's second endpoint first) -- CROSSINGS-
  ;; BEFORE-GATE>'s value for the opposite endpoint, when BEAM's endpoints are both
  ;; locations and the beam can be traversed in either direction.  A single return value,
  ;; not multiple values, for the same CHECK-VARIABLE-NAMES reason noted on BEAM-
  ;; COORDINATES-LOS-OCCLUDERS above.
  (let ((parameter&crossing
          (loop for record in records
                when (equal beam (second record))
                  collect (cons (third record) (first record))
                when (equal beam (fourth record))
                  collect (cons (fifth record) (first record)))))
    (setf parameter&crossing (sort parameter&crossing #'< :key #'car))
    (cons (mapcar #'cdr (remove-if-not (lambda (pc) (< (car pc) split-parameter)) parameter&crossing))
          (mapcar #'cdr (reverse (remove-if-not (lambda (pc) (> (car pc) split-parameter)) parameter&crossing))))))


;;;; QUERY FUNCTIONS ;;;;


(define-query beam-coordinates-potential-beams ()
  ;; Canonical deterministic order: transmitter -> location, then location -> receiver,
  ;; then location -> location in declared type order (los-to-location is symmetric at
  ;; runtime, so the type-order test avoids counting each L-L pair twice).
  (do (assign $beams nil)
      (doall (?location location)
        (doall (?transmitter transmitter)
          (if (bind (los-to-transceiver ?location $gates ?transmitter))
            (push (list ?transmitter ?location) $beams))))
      (doall (?location location)
        (doall (?receiver receiver)
          (if (bind (los-to-transceiver ?location $gates ?receiver))
            (push (list ?location ?receiver) $beams))))
      (doall (?source location)
        (doall (?destination location)
          (if (and (member ?destination
                           (rest (member ?source (gethash 'location *types*))))
                   (bind (los-to-location ?source $gates ?destination)))
            (push (list ?source ?destination) $beams))))
      (reverse $beams)))


;;;; INITIALIZATION ;;;;


(define-init-action establish-beam-coordinates
  ;; Runs only when the problem has authored TRANSCEIVER-POSITION> or LOCATION-POSITION> facts --
  ;; inert otherwise, so a purely topological problem (hand-authoring CROSSINGS-ALONG-
  ;; BEAM> directly) is entirely unaffected.  Computes every proper beam intersection from
  ;; the authored sightlines (LOS-TO-TRANSCEIVER, LOS-TO-LOCATION) and coordinates, and
  ;; asserts CROSSINGS-ALONG-BEAM> accordingly.  Does not assert BEAM-CROSSING> --
  ;; beam-crossing derives that itself, lazily, from whichever facts populate CROSSINGS-
  ;; ALONG-BEAM>.  Ends with an explicit CONVERT-DATABASES-TO-INTEGERS: this init-action's
  ;; own effect is compiled without the usual int-code substitution (do-init-action-
  ;; updates compiles every init-action's effect with a plain COMPILE), so a newly-
  ;; asserted static fact only lands in *STATIC-DB* (symbolic).  Every DEFINE-QUERY/
  ;; DEFINE-UPDATE, including this problem's own INITIALIZE-DERIVED-STATE that runs right
  ;; after this one in the same DO-INIT-ACTION-UPDATES pass, was already compiled against
  ;; *STATIC-IDB* (integer-keyed) earlier, during the system's one DO-INTEGER-CONVERSION
  ;; pass, which runs before any init-action fires.  Without this explicit re-sync,
  ;; initialize-derived-state's propagate-changes! would read a not-yet-updated
  ;; *STATIC-IDB* and find no crossings at all.
  0
  ()
  (or (exists (?e beam-endpoint) (bind (transceiver-position> ?e $x $y)))
      (exists (?e location) (bind (location-position> ?e $x $y))))
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


(define-init-action derive-crossings-before-gate
  ;; Splits each gate-conditioned beam's crossing set into a per-gate CROSSINGS-BEFORE-
  ;; GATE> list, using each gate's own crossing parameter on that beam -- fixing a real
  ;; gap BEAM-REACHES-CROSSING (beam-crossing.lisp) already checks for: without this, a
  ;; beam paired through a gate stays live for cutting along its full geometric length
  ;; even after the gate closes, since RELAY-BEAM-LIVE-FOR-CUTTING never itself checks
  ;; gate state.  A beam conditioned on more than one gate gets one independent
  ;; CROSSINGS-BEFORE-GATE> fact per gate, each with that gate's own cutoff -- correct
  ;; because BEAM-REACHES-CROSSING already ORs the blocking test across every gate.
  ;; Runs only when the problem has asserted GATE-SEGMENTS -- inert otherwise.  Needs
  ;; GATE-SEGMENTS for the actual gate segment coordinates (the LOS-TO-TRANSCEIVER/LOS-TO-
  ;; LOCATION occluder list only has gate names), so, unlike ESTABLISH-BEAM-COORDINATES,
  ;; can't run for a problem that hand-authors its own gate-conditioned LOS facts without
  ;; also supplying the geometry -- such a problem simply leaves CROSSINGS-BEFORE-GATE>
  ;; unpopulated, exactly as problem-corner-topo.lisp does today.  Defined here, after
  ;; -beam-los-coordinates' own DERIVE-LOS-FROM-SEGMENTS (needs LOS-TO-TRANSCEIVER/LOS-TO-
  ;; LOCATION populated) -- file/load order, same as that init-action's own commentary
  ;; explains.  Ends with its own CONVERT-DATABASES-TO-INTEGERS, for the same reason the
  ;; other init-actions in this file do.
  0
  ()
  (bind (gate-segments $gates))
  ()
  (assert
    (do (assign $positions (beam-coordinates-endpoint-positions))
        (assign $beams (beam-coordinates-potential-beams))
        (assign $records
                (beam-coordinates-crossing-records
                  $beams $positions (gethash 'crossing *types*)))
        (doall (?location location)
          (doall (?transmitter transmitter)
            (if (bind (los-to-transceiver ?location $beam-gates ?transmitter))
              (ww-loop for $gate-name in $beam-gates
                       do (assign $gate-record (assoc $gate-name $gates))
                          (assign $gate-parameter
                                  (beam-coordinates-obstacle-intersection-parameter
                                    (list ?transmitter ?location) $positions $gate-record))
                          (assign $split
                                  (beam-coordinates-split-crossings-at-parameter
                                    (list ?transmitter ?location) $records $gate-parameter))
                          (crossings-before-gate>
                            ?transmitter (car $split) $gate-name ?location)))))
        (doall (?location location)
          (doall (?receiver receiver)
            (if (bind (los-to-transceiver ?location $beam-gates ?receiver))
              (ww-loop for $gate-name in $beam-gates
                       do (assign $gate-record (assoc $gate-name $gates))
                          (assign $gate-parameter
                                  (beam-coordinates-obstacle-intersection-parameter
                                    (list ?location ?receiver) $positions $gate-record))
                          (assign $split
                                  (beam-coordinates-split-crossings-at-parameter
                                    (list ?location ?receiver) $records $gate-parameter))
                          (crossings-before-gate>
                            ?location (car $split) $gate-name ?receiver)))))
        (doall (?source location)
          (doall (?destination location)
            (if (and (member ?destination
                             (rest (member ?source (gethash 'location *types*))))
                     (bind (los-to-location ?source $beam-gates ?destination)))
              (ww-loop for $gate-name in $beam-gates
                       do (assign $gate-record (assoc $gate-name $gates))
                          (assign $gate-parameter
                                  (beam-coordinates-obstacle-intersection-parameter
                                    (list ?source ?destination) $positions $gate-record))
                          (assign $split
                                  (beam-coordinates-split-crossings-at-parameter
                                    (list ?source ?destination) $records $gate-parameter))
                          (crossings-before-gate>
                            ?source (car $split) $gate-name ?destination)
                          (crossings-before-gate>
                            ?destination (cdr $split) $gate-name ?source)))))
        (convert-databases-to-integers))))

;;; Filename: -beam-crossing-coordinates.lisp

;;; Beam crossing coordinates substrate: computes CROSSINGS-ALONG-BEAM>/BEAM-CROSSINGS-BEFORE-GATE>
;;; from the beam geometry -beam-los-coordinates.lisp already derived (or the problem hand-
;;; authored) as LOS-TO-APPARATUS/LOS-TO-LOCATION, for a problem that would rather author 2D
;;; positions than hand-list which crossings lie on which beam and in what order.  Nested under
;;; beam-crossing-tech only -- unlike -beam-los-coordinates, this file's derivations are never
;;; useful to a problem that includes beam-direct alone, since CROSSINGS-ALONG-BEAM>/BEAM-CROSSINGS-
;;; BEFORE-GATE> are consumed only by beam-crossing's own BEAM-REACHES-CROSSING.  A problem
;;; wanting two crossing direct beams includes both beam-direct and beam-crossing together;
;;; splicing is deduplicated per problem copy and this file always nests -beam-los-coordinates
;;; as its own first form, so LOS derivation runs before crossing derivation regardless of
;;; whether visibility-tech (which also nests -beam-los-coordinates, as the owner of the los
;;; relations it derives) was listed before or after beam-crossing.
;;;
;;; The problem declares no CROSSING pool at all.  ESTABLISH-BEAM-COORDINATES mints exactly one
;;; crossing per computed intersection (BEAM-COORDINATES-DERIVE-CROSSING-RECORDS) and publishes
;;; the result as CURRENT-BEAM-CROSSINGS, so both the pool's size and its per-beam ordering are
;;; outputs of the geometry.  The minted objects never enter (gethash 'crossing *types*) -- that
;;; extension is frozen into every compiled DOALL literal at load time, well before any
;;; init-action runs -- so beam-crossing.lisp iterates them through its GET-CURRENT-BEAM-CROSSINGS
;;; query instead of over the bare CROSSING type.  Does not assert BEAM-CROSSING> --
;;; beam-crossing-tech derives that itself, lazily, from whichever facts populate CROSSINGS-
;;; ALONG-BEAM>, regardless of how they got there.
;;;
;;; Also derives BEAM-CROSSINGS-BEFORE-GATE> (declared by beam-crossing-tech, alongside
;;; CROSSINGS-ALONG-BEAM>) when the problem asserts GATE-SEGMENT>: DERIVE-BEAM-CROSSINGS-BEFORE-GATE
;;; splits each gate-conditioned beam's crossing set at that gate's own crossing parameter on
;;; the beam -- independently per gate, for a beam conditioned on more than one.  Wall,
;;; edge, and boundary crossings do not split the crossing sequence: visibility and
;;; beam-direct retain them separately and decide vertical clearance at runtime.
;;; Without a populated BEAM-CROSSINGS-BEFORE-GATE>, BEAM-REACHES-CROSSING's (beam-crossing.lisp) own
;;; gate check is vacuously satisfied, so a beam paired through a gate stays live for cutting
;;; along its full geometric length even after the gate closes.  A LOS-TO-APPARATUS/LOS-TO-
;;; LOCATION occluder list may also carry location entries (-beam-los-coordinates' own
;;; location-occlusion test); DERIVE-BEAM-CROSSINGS-BEFORE-GATE's three loops below skip any
;;; occluder that is not a gate, since a location entry has no GATE-SEGMENT> fact to split
;;; a beam's crossing set at.
;;;
;;; Self-contained; spliced by (include-tech -beam-crossing-coordinates), nested from
;;; beam-crossing.
;;;
;;; REQUIRES:
;;;   nested    : -beam-los-coordinates (LOS-ENDPOINT type; APPARATUS-COORDS>, LOCATION-
;;;               POSITION>; LOS-TO-APPARATUS/LOS-TO-LOCATION, hand-authored or derived)
;;;   relations : crossings-along-beam>, beam-crossings-before-gate>  --  declared by
;;;               beam-crossing.lisp itself, the parent tech this file is always nested under
;;; PROVIDES:
;;;   nested    : -beam-los-coordinates
;;;   types     : crossing  --  declared optional here and left permanently empty; the pool is
;;;               created at init time by ESTABLISH-BEAM-COORDINATES and published as
;;;               CURRENT-BEAM-CROSSINGS, not declared by the problem.  The declaration survives
;;;               only to keep the type name registered so beam-crossing.lisp's relation
;;;               signatures over CROSSING resolve
;;;   init      : establish-beam-coordinates, derive-beam-crossings-before-gate  --  in this order,
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
  ;; Attaches an already-existing crossing pool to computed geometry, in order.  Record
  ;; shape: (crossing beam1 parameter1 beam2 parameter2).  Used by the second and later
  ;; consumers of the topology, which recompute the same geometry and must land on the
  ;; same crossing objects ESTABLISH-BEAM-COORDINATES already created -- they pass the
  ;; pool read back from CURRENT-BEAM-CROSSINGS.  The length check is therefore a real
  ;; cross-check between init-actions: a mismatch means two passes over the same
  ;; coordinates disagreed, which should be impossible.  BEAM-COORDINATES-DERIVE-CROSSING-
  ;; RECORDS below is the creation path and does not go through here.
  (let ((geometry (beam-coordinates-geometric-crossings beams positions)))
    (unless (= (length geometry) (length crossing-objects))
      (error "Recomputed ~D beam crossings, but the established pool holds ~D objects."
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
  ;; parameter, in increasing order (nearest BEAM's first endpoint first) -- BEAM-CROSSINGS-
  ;; BEFORE-GATE>'s value for that endpoint; AFTER is the crossings with a larger
  ;; parameter, in decreasing order (nearest BEAM's second endpoint first) -- BEAM-CROSSINGS-
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
  ;; Canonical deterministic order: transmitter -> location, location -> receiver,
  ;; location -> repeater, then location -> location in declared type order
  ;; (los-to-location is symmetric at runtime, so the type-order test avoids counting each
  ;; L-L pair twice), and finally the fixed COUPLED beams.  The location families come from
  ;; structural LOS relations that retain finite barrier crossings; direct beams have
  ;; no LOS fact to enumerate from -- their existence is authored as COUPLED -- so they are
  ;; appended from BEAM-COORDINATES-COUPLED-BEAMS instead.  Appended last so that adding
  ;; beam-direct to an existing problem cannot renumber the crossings its relay beams
  ;; already had.
  (do (assign $beams nil)
      (doall (?location location)
        (doall (?transmitter transmitter)
          (if (bind (los-to-apparatus ?location $gates ?transmitter))
            (push (list ?transmitter ?location) $beams))))
      (doall (?location location)
        (doall (?receiver receiver)
          (if (bind (los-to-apparatus ?location $gates ?receiver))
            (push (list ?location ?receiver) $beams))))
      (doall (?location location)
        (doall (?repeater repeater)
          (if (bind (los-to-apparatus ?location $gates ?repeater))
            (push (list ?location ?repeater) $beams))))
      (doall (?source location)
        (doall (?destination location)
          (if (and (member ?destination
                           (rest (member ?source (gethash 'location *types*))))
                   (bind (los-to-location ?source $gates ?destination)))
            (push (list ?source ?destination) $beams))))
      (append (reverse $beams) (beam-coordinates-coupled-beams))))


;;;; CROSSING POOL ;;;;
;;;; Kept out of GEOMETRY HELPERS above: REGISTER-DYNAMIC-OBJECT writes to
;;;; *CONSTANT-INTEGERS* and *STATIC-IDB*, so this is not a pure function of its
;;;; arguments the way the helpers up there are.


(defun beam-coordinates-derive-crossing-records (beams positions)
  ;; The creation path.  Computes every proper beam intersection and mints exactly one
  ;; crossing per intersection, so the pool size is an output of the geometry rather than
  ;; something the problem has to hand-count and declare.  Record shape matches
  ;; BEAM-COORDINATES-CROSSING-RECORDS: (crossing beam1 parameter1 beam2 parameter2).
  ;;
  ;; The crossings are ordinary interned symbols registered as planning objects of type
  ;; CROSSING, never entries in (gethash 'crossing *types*) -- that extension was already
  ;; frozen into every compiled DOALL literal at load time, long before this runs.  They
  ;; are reachable instead through CURRENT-BEAM-CROSSINGS, which beam-crossing.lisp's
  ;; GET-CURRENT-BEAM-CROSSINGS query wraps for the DOALL sites.  REGISTER-DYNAMIC-OBJECT must
  ;; run before any proposition mentions the object, which is why minting happens here
  ;; rather than at the point of first assertion.
  (let* ((geometry (beam-coordinates-geometric-crossings beams positions))
         (beam-crossings (loop for index from 1 to (length geometry)
                               collect (register-dynamic-object
                                         (intern (format nil "CROSSING~D" index) :ww)
                                         'crossing))))
    (mapcar #'cons beam-crossings geometry)))


;;;; INITIALIZATION ;;;;


(define-init-action establish-beam-coordinates
  ;; Runs only when the problem has authored APPARATUS-COORDS> or LOCATION-COORDS> facts --
  ;; inert otherwise, so a purely topological problem (hand-authoring CROSSINGS-ALONG-
  ;; BEAM> directly) is entirely unaffected.  Computes every proper beam intersection from
  ;; the authored sightlines (LOS-TO-APPARATUS, LOS-TO-LOCATION) and coordinates, mints
  ;; one crossing per intersection, publishes the pool as CURRENT-BEAM-CROSSINGS, and asserts
  ;; CROSSINGS-ALONG-BEAM> accordingly.  CURRENT-BEAM-CROSSINGS is asserted before the
  ;; per-beam loop below purely for readability; nothing in this effect reads it back, but
  ;; DERIVE-BEAM-CROSSINGS-BEFORE-GATE does, and beam-crossing.lisp's GET-CURRENT-BEAM-CROSSINGS
  ;; reads it for every DOALL over crossings from here on.  Does not assert BEAM-CROSSING> --
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
  (or (exists (?e los-endpoint) (bind (apparatus-coords> ?e $x $y)))
      (exists (?e location) (bind (location-coords> ?e $x $y))))
  ()
  (assert
    (do (assign $beams (beam-coordinates-potential-beams))
        (assign $positions (beam-coordinates-endpoint-positions))
        (assign $records
                (beam-coordinates-derive-crossing-records $beams $positions))
        (assign $beam-crossings (mapcar #'first $records))
        (current-beam-crossings $beam-crossings)
        (ww-loop for $beam in $beams
                 do (assign $beam-crossings
                            (beam-coordinates-crossings-on-beam $beam $records))
                    (if $beam-crossings
                      (do (crossings-along-beam>
                            (first $beam) $beam-crossings (second $beam))
                          (if (and (member (first $beam)
                                           (gethash 'location *types*))
                                   (member (second $beam)
                                           (gethash 'location *types*)))
                            (crossings-along-beam>
                              (second $beam) (reverse $beam-crossings)
                              (first $beam))))))
        (convert-databases-to-integers))))


(define-init-action derive-beam-crossings-before-gate
  ;; Splits each gate-conditioned beam's crossing set into a per-gate BEAM-CROSSINGS-BEFORE-
  ;; GATE> list, using each gate's own crossing parameter on that beam -- fixing a real
  ;; gap BEAM-REACHES-CROSSING (beam-crossing.lisp) already checks for: without this, a
  ;; beam paired through a gate stays live for cutting along its full geometric length
  ;; even after the gate closes, since RELAY-BEAM-LIVE-FOR-CUTTING never itself checks
  ;; gate state.  A beam conditioned on more than one gate gets one independent
  ;; BEAM-CROSSINGS-BEFORE-GATE> fact per gate, each with that gate's own cutoff -- correct
  ;; because BEAM-REACHES-CROSSING already ORs the blocking test across every gate.
  ;; Runs only when the problem has asserted GATE-SEGMENT> -- inert otherwise.  Needs
  ;; GATE-SEGMENT> for the actual gate segment coordinates (the LOS-TO-APPARATUS/LOS-TO-
  ;; LOCATION occluder list only has gate names), so, unlike ESTABLISH-BEAM-COORDINATES,
  ;; can't run for a problem that hand-authors its own gate-conditioned LOS facts without
  ;; also supplying the geometry -- such a problem simply leaves BEAM-CROSSINGS-BEFORE-GATE>
  ;; unpopulated.  Defined here, after
  ;; -beam-los-coordinates' own DERIVE-LOS-FROM-SEGMENTS (needs LOS-TO-APPARATUS/LOS-TO-
  ;; LOCATION populated) -- file/load order, same as that init-action's own commentary
  ;; explains.  Reads the crossing pool back from CURRENT-BEAM-CROSSINGS rather than minting
  ;; its own: the crossings this splits must be the very objects CROSSINGS-ALONG-BEAM>
  ;; already names, and a second call to BEAM-COORDINATES-DERIVE-CROSSING-RECORDS would
  ;; produce a parallel set of same-named-but-distinct symbols that no other fact refers
  ;; to.  The geometry itself is recomputed here, and BEAM-COORDINATES-CROSSING-RECORDS'
  ;; length check confirms the two passes agree.  Ends with its own
  ;; CONVERT-DATABASES-TO-INTEGERS, for the same reason the other init-actions in this
  ;; file do.
  0
  ()
  (exists (?gate gate)
    (bind (gate-segment> ?gate $x1 $y1 $x2 $y2)))
  ()
  (assert
    (do (assign $gates (gate-segment-records))
        (assign $positions (beam-coordinates-endpoint-positions))
        (assign $beams (beam-coordinates-potential-beams))
        (bind (current-beam-crossings $beam-crossings))
        (assign $records
                (beam-coordinates-crossing-records
                  $beams $positions $beam-crossings))
        (doall (?location location)
          (doall (?transmitter transmitter)
            (if (bind (los-to-apparatus ?location $beam-gates ?transmitter))
              (ww-loop for $gate-name in $beam-gates
                       do (if (gate $gate-name)
                            (do (assign $gate-record (assoc $gate-name $gates))
                                (assign $gate-parameter
                                        (beam-coordinates-obstacle-intersection-parameter
                                          (list ?transmitter ?location) $positions $gate-record))
                                (assign $split
                                        (beam-coordinates-split-crossings-at-parameter
                                          (list ?transmitter ?location) $records $gate-parameter))
                                (beam-crossings-before-gate>
                                  ?transmitter (car $split) $gate-name ?location)))))))
        (doall (?location location)
          (doall (?receiver receiver)
            (if (bind (los-to-apparatus ?location $beam-gates ?receiver))
              (ww-loop for $gate-name in $beam-gates
                       do (if (gate $gate-name)
                            (do (assign $gate-record (assoc $gate-name $gates))
                                (assign $gate-parameter
                                        (beam-coordinates-obstacle-intersection-parameter
                                          (list ?location ?receiver) $positions $gate-record))
                                (assign $split
                                        (beam-coordinates-split-crossings-at-parameter
                                          (list ?location ?receiver) $records $gate-parameter))
                                (beam-crossings-before-gate>
                                  ?location (car $split) $gate-name ?receiver)))))))
        (doall (?source location)
          (doall (?destination location)
            (if (and (member ?destination
                             (rest (member ?source (gethash 'location *types*))))
                     (bind (los-to-location ?source $beam-gates ?destination)))
              (ww-loop for $gate-name in $beam-gates
                       do (if (gate $gate-name)
                            (do (assign $gate-record (assoc $gate-name $gates))
                                (assign $gate-parameter
                                        (beam-coordinates-obstacle-intersection-parameter
                                          (list ?source ?destination) $positions $gate-record))
                                (assign $split
                                        (beam-coordinates-split-crossings-at-parameter
                                          (list ?source ?destination) $records $gate-parameter))
                                (beam-crossings-before-gate>
                                  ?source (car $split) $gate-name ?destination)
                                (beam-crossings-before-gate>
                                  ?destination (cdr $split) $gate-name ?source)))))))
        (convert-databases-to-integers))))

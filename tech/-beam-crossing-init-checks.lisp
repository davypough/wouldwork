;;; Filename: -beam-crossing-init-checks.lisp

;;; Initialization validation for authored beam crossing topology.


(in-package :ww)


(define-init-check beam-crossing-init-check (literals)
  (check-init-beam-crossing-endpoints literals)
  (check-init-crossing-lists-have-unique-items literals)
  (check-init-crossing-list-items-are-defined literals)
  (check-init-beam-crossings-are-indexed-by-declared-beams literals)
  (check-init-crossing-lists-match-declared-beams literals)
  (check-init-location-beam-reverses literals)
  (check-init-crossing-beams-have-sightlines literals)
  (check-init-beam-crossings-before-gate-prefixes literals)
  (check-init-beam-crossings-before-gate-gates-occlude-beams literals))


(define-init-check-helper init-beam-crossings-along-beam-map (literals)
  (let ((beams (make-hash-table :test #'equal)))
    (dolist (literal (init-literals-with-relation 'crossings-along-beam> literals))
      (destructuring-bind (source beam-crossings destination)
          (rest (init-literal-proposition literal))
        (setf (gethash (list source destination) beams) beam-crossings)))
    beams))


(define-init-check-helper init-list-prefix-p (prefix list)
  (and (<= (length prefix) (length list))
       (every #'eql prefix list)))


(define-init-check-helper init-valid-directed-beam-p (source destination)
  (and (or (init-type-member-p source 'transmitter)
           (init-type-member-p source 'repeater)
           (init-type-member-p source 'location))
       (or (init-type-member-p destination 'repeater)
           (init-type-member-p destination 'receiver)
           (init-type-member-p destination 'location))))


(define-init-check-helper init-first-matching-list-value (relation literals test)
  (dolist (literal (init-literals-with-relation relation literals) (values nil nil))
    (let ((proposition (init-literal-proposition literal)))
      (when (funcall test proposition)
        (return (values (third proposition) t))))))


(define-init-check-helper init-occluders-for-directed-beam (source destination literals)
  "The occluder list guarding a beam between SOURCE and DESTINATION.  A hop with a location
   at either end is guarded by its sightline, and LOS-VIA records that once for both
   directions, so the lookup is order-agnostic -- which is why the location-to-location and
   location-to-apparatus cases, separate relations before Phase 5, are now one branch.  A
   fixed apparatus-to-apparatus coupling has no sightline fact and is guarded by its own
   BEAM-VIA corridor instead."
  (if (or (init-type-member-p source 'location)
          (init-type-member-p destination 'location))
    (init-first-matching-list-value
      'los-via literals
      (lambda (prop)
        (or (and (eql (second prop) source)
                 (eql (fourth prop) destination))
            (and (eql (second prop) destination)
                 (eql (fourth prop) source)))))
    (init-first-matching-list-value
      'beam-via literals
      (lambda (prop)
        (and (eql (second prop) source)
             (eql (fourth prop) destination))))))


(define-init-check-helper init-defined-beam-crossings (literals)
  (let ((beam-crossings nil))
    (dolist (literal (init-literals-with-relation 'beam-crossing> literals))
      (destructuring-bind (crossing from1 to1 from2 to2)
          (rest (init-literal-proposition literal))
        (declare (ignore from1 to1 from2 to2))
        (push crossing beam-crossings)))
    beam-crossings))


(define-init-check-helper init-beams-for-crossing-map (literals)
  (let ((crossing-beams (make-hash-table :test #'equal)))
    (dolist (literal (init-literals-with-relation 'beam-crossing> literals))
      (destructuring-bind (crossing from1 to1 from2 to2)
          (rest (init-literal-proposition literal))
        (setf (gethash crossing crossing-beams)
              (list (list from1 to1)
                    (list from2 to2)))))
    crossing-beams))


(define-init-check-helper check-init-beam-crossing-endpoints (literals)
  "Checks that BEAM-CROSSING> beams use a supported directed endpoint shape."
  (dolist (literal (init-literals-with-relation 'beam-crossing> literals))
    (destructuring-bind (crossing from1 to1 from2 to2)
        (rest (init-literal-proposition literal))
      (unless (init-valid-directed-beam-p from1 to1)
        (fail-init-check nil "~%BEAM-CROSSING> uses an unsupported directed beam.~%~
                Literal:  ~S~%~
                Crossing: ~S~%~
                Beam:     ~S -> ~S"
               literal crossing from1 to1))
      (unless (init-valid-directed-beam-p from2 to2)
        (fail-init-check nil "~%BEAM-CROSSING> uses an unsupported directed beam.~%~
                Literal:  ~S~%~
                Crossing: ~S~%~
                Beam:     ~S -> ~S"
               literal crossing from2 to2))
      (when (eql from1 to1)
        (fail-init-check nil "~%BEAM-CROSSING> declares a self-beam.~%~
                Literal:  ~S~%~
                Crossing: ~S~%~
                Beam:     ~S -> ~S"
               literal crossing from1 to1))
      (when (eql from2 to2)
        (fail-init-check nil "~%BEAM-CROSSING> declares a self-beam.~%~
                Literal:  ~S~%~
                Crossing: ~S~%~
                Beam:     ~S -> ~S"
               literal crossing from2 to2))
      (when (equal (list from1 to1) (list from2 to2))
        (fail-init-check nil "~%BEAM-CROSSING> declares the same beam twice.~%~
                Literal:  ~S~%~
                Crossing: ~S~%~
                Beam:     ~S -> ~S"
               literal crossing from1 to1)))))


(define-init-check-helper init-check-list-has-unique-items (literal items)
  (let ((seen nil))
    (dolist (item items)
      (when (member item seen)
        (fail-init-check nil "~%Duplicate item in DEFINE-INIT list.~%~
                Literal: ~S~%~
                Duplicate item: ~S"
               literal item))
      (push item seen))))


(define-init-check-helper check-init-crossing-lists-have-unique-items (literals)
  "Checks that crossing-order lists do not repeat a crossing."
  (dolist (literal (init-literals-with-relation 'crossings-along-beam> literals))
    (destructuring-bind (source beam-crossings destination)
        (rest (init-literal-proposition literal))
      (declare (ignore source destination))
      (init-check-list-has-unique-items literal beam-crossings)))
  (dolist (literal (init-literals-with-relation 'beam-crossings-before-gate> literals))
    (destructuring-bind (source before gate destination)
        (rest (init-literal-proposition literal))
      (declare (ignore source gate destination))
      (init-check-list-has-unique-items literal before))))


(define-init-check-helper init-check-crossing-list-items-are-defined (literal items defined)
  (dolist (item items)
    (unless (member item defined)
      (fail-init-check nil "~%Crossing-order list references a crossing with no BEAM-CROSSING> definition.~%~
              Literal:  ~S~%~
              Crossing: ~S"
             literal item))))


(define-init-check-helper check-init-crossing-list-items-are-defined (literals)
  "Checks that crossing-order lists reference authored beam crossings."
  (let ((defined (init-defined-beam-crossings literals)))
    (dolist (literal (init-literals-with-relation 'crossings-along-beam> literals))
      (destructuring-bind (source beam-crossings destination)
          (rest (init-literal-proposition literal))
        (declare (ignore source destination))
        (init-check-crossing-list-items-are-defined literal beam-crossings defined)))
    (dolist (literal (init-literals-with-relation 'beam-crossings-before-gate> literals))
      (destructuring-bind (source before gate destination)
          (rest (init-literal-proposition literal))
        (declare (ignore source gate destination))
        (init-check-crossing-list-items-are-defined literal before defined)))))


(define-init-check-helper init-check-beam-crossing-is-indexed
    (literal crossing source destination beams)
  (let ((beam-crossings (gethash (list source destination) beams)))
    (unless beam-crossings
      (fail-init-check nil "~%BEAM-CROSSING> declares a beam with no CROSSINGS-ALONG-BEAM> entry.~%~
              Literal:  ~S~%~
              Crossing: ~S~%~
              Beam:     ~S -> ~S"
             literal crossing source destination))
    (unless (member crossing beam-crossings)
      (fail-init-check nil "~%BEAM-CROSSING> is missing from its declared beam's crossing list.~%~
              Literal:              ~S~%~
              Crossing:             ~S~%~
              Beam:                 ~S -> ~S~%~
              Crossings along beam: ~S"
             literal crossing source destination beam-crossings))))


(define-init-check-helper check-init-beam-crossings-are-indexed-by-declared-beams (literals)
  "Checks that each BEAM-CROSSING> appears on both of its declared beam lists."
  (let ((beams (init-beam-crossings-along-beam-map literals)))
    (dolist (literal (init-literals-with-relation 'beam-crossing> literals))
      (destructuring-bind (crossing from1 to1 from2 to2)
          (rest (init-literal-proposition literal))
        (init-check-beam-crossing-is-indexed literal crossing from1 to1 beams)
        (init-check-beam-crossing-is-indexed literal crossing from2 to2 beams)))))


(define-init-check-helper check-init-crossing-lists-match-declared-beams (literals)
  "Checks that each crossing listed on a beam belongs to that declared beam.

Location-to-location beams are bidirectional (BEAM-CROSSING> names them in one
canonical direction while CROSSINGS-ALONG-BEAM> is authored for both directions),
so the reverse pairing is also accepted when both endpoints are locations."
  (let ((crossing-beams (init-beams-for-crossing-map literals)))
    (dolist (literal (init-literals-with-relation 'crossings-along-beam> literals))
      (destructuring-bind (source beam-crossings destination)
          (rest (init-literal-proposition literal))
        (let ((beam (list source destination))
              (reverse-beam (list destination source))
              (bidirectional-p (and (init-type-member-p source 'location)
                                    (init-type-member-p destination 'location))))
          (dolist (crossing beam-crossings)
            (unless (or (member beam (gethash crossing crossing-beams)
                                 :test #'equal)
                        (and bidirectional-p
                             (member reverse-beam (gethash crossing crossing-beams)
                                     :test #'equal)))
              (fail-init-check nil "~%CROSSINGS-ALONG-BEAM> lists a crossing on an undeclared beam.~%~
                      Literal:       ~S~%~
                      Crossing:      ~S~%~
                      Listed beam:   ~S -> ~S~%~
                      Declared beams: ~S"
                     literal crossing source destination
                     (gethash crossing crossing-beams)))))))))


(define-init-check-helper check-init-location-beam-reverses (literals)
  "Checks that location-location crossing lists exist in both directions and reverse exactly."
  (let ((beams (init-beam-crossings-along-beam-map literals)))
    (dolist (literal (init-literals-with-relation 'crossings-along-beam> literals))
      (destructuring-bind (source beam-crossings destination)
          (rest (init-literal-proposition literal))
        (when (and (init-type-member-p source 'location)
                   (init-type-member-p destination 'location))
          (let ((reverse-beam-crossings (gethash (list destination source) beams)))
            (unless reverse-beam-crossings
              (fail-init-check nil "~%Location-to-location CROSSINGS-ALONG-BEAM> has no reverse entry.~%~
                      Literal: ~S~%~
                      Expected reverse beam: (CROSSINGS-ALONG-BEAM> ~S ... ~S)"
                     literal destination source))
            (unless (equal reverse-beam-crossings (reverse beam-crossings))
              (fail-init-check nil "~%Location-to-location CROSSINGS-ALONG-BEAM> reverse ordering is inconsistent.~%~
                      Literal:           ~S~%~
                      Crossings:         ~S~%~
                      Reverse crossings: ~S~%~
                      Expected reverse:  ~S"
                     literal beam-crossings reverse-beam-crossings
                     (reverse beam-crossings)))))))))


(define-init-check-helper check-init-crossing-beams-have-sightlines (literals)
  "Checks that every CROSSINGS-ALONG-BEAM> beam has a matching sightline or corridor fact."
  (dolist (literal (init-literals-with-relation 'crossings-along-beam> literals))
    (destructuring-bind (source beam-crossings destination)
        (rest (init-literal-proposition literal))
      (declare (ignore beam-crossings))
      (multiple-value-bind (occluders found-p)
          (init-occluders-for-directed-beam source destination literals)
        (declare (ignore occluders))
        (unless found-p
          (fail-init-check nil "~%CROSSINGS-ALONG-BEAM> beam has no matching sightline/corridor fact.~%~
                  Literal:  ~S~%~
                  Beam:     ~S -> ~S"
                 literal source destination))))))


(define-init-check-helper check-init-beam-crossings-before-gate-prefixes (literals)
  "Checks that each BEAM-CROSSINGS-BEFORE-GATE> list is an initial prefix of
the matching CROSSINGS-ALONG-BEAM> list."
  (let ((beams (init-beam-crossings-along-beam-map literals)))
    (dolist (literal (init-literals-with-relation 'beam-crossings-before-gate> literals))
      (destructuring-bind (source before gate destination)
          (rest (init-literal-proposition literal))
        (let* ((beam-key (list source destination))
               (along (gethash beam-key beams)))
          (unless along
            (fail-init-check nil "~%BEAM-CROSSINGS-BEFORE-GATE> has no matching CROSSINGS-ALONG-BEAM>.~%~
                    Literal: ~S~%~
                    Expected matching beam: (CROSSINGS-ALONG-BEAM> ~S ... ~S)"
                   literal source destination))
          (unless (init-list-prefix-p before along)
            (fail-init-check nil "~%BEAM-CROSSINGS-BEFORE-GATE> list is not a prefix of CROSSINGS-ALONG-BEAM>.~%~
                    Literal:              ~S~%~
                    Crossings before gate: ~S~%~
                    Crossings along beam:  ~S~%~
                    Gate:                  ~S"
                   literal before along gate)))))))


(define-init-check-helper check-init-beam-crossings-before-gate-gates-occlude-beams (literals)
  "Checks that each BEAM-CROSSINGS-BEFORE-GATE> gate is an occluder for its beam."
  (dolist (literal (init-literals-with-relation 'beam-crossings-before-gate> literals))
    (destructuring-bind (source before gate destination)
        (rest (init-literal-proposition literal))
      (declare (ignore before))
      (multiple-value-bind (occluders found-p)
          (init-occluders-for-directed-beam source destination literals)
        (unless found-p
          (fail-init-check nil "~%BEAM-CROSSINGS-BEFORE-GATE> has no sightline/corridor facts for its beam.~%~
                  Literal: ~S~%~
                  Beam:    ~S -> ~S"
                 literal source destination))
        (unless (member gate occluders)
          (fail-init-check nil "~%BEAM-CROSSINGS-BEFORE-GATE> gate does not occlude its beam.~%~
                  Literal:  ~S~%~
                  Gate:     ~S~%~
                  Beam:     ~S -> ~S~%~
                  Occluders: ~S"
                 literal gate source destination occluders))))))

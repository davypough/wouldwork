;;; Filename: ww-init-validator.lisp

;;; Cross-fact validation for define-init entries.


(in-package :ww)


(defun validate-init-literals (literals)
  "Checks semantic consistency across raw DEFINE-INIT literals."
  (check-init-general-consistency literals)
  (check-init-physical-consistency literals)
  (check-init-connector-consistency literals)
  (check-init-control-and-beam-consistency literals)
  (check-init-crossing-consistency literals))


(defun check-init-general-consistency (literals)
  (check-init-duplicate-fluent-keys literals)
  (check-init-no-derived-facts literals))


(defun check-init-physical-consistency (literals)
  (check-init-object-placement-consistency literals)
  (check-init-list-contents literals))


(defun check-init-connector-consistency (literals)
  (check-init-paired-consistency literals)
  (check-init-paired-connector-graph-acyclic literals)
  (check-init-paired-sightlines literals))


(defun check-init-control-and-beam-consistency (literals)
  (check-init-controls-modes literals)
  (check-init-coupled-beam-consistency literals))


(defun check-init-crossing-consistency (literals)
  (check-init-beam-crossing-endpoints literals)
  (check-init-crossing-lists-have-unique-items literals)
  (check-init-crossing-list-items-are-defined literals)
  (check-init-beam-crossings-are-indexed-by-declared-beams literals)
  (check-init-crossing-lists-match-declared-beams literals)
  (check-init-location-beam-reverses literals)
  (check-init-crossings-before-gate-prefixes literals)
  (check-init-crossings-before-gate-gates-occlude-beams literals))


(defun init-literal-proposition (literal)
  (if (and (consp literal)
           (eql (car literal) 'not))
    (second literal)
    literal))


(defun init-literals-with-relation (relation literals)
  (remove-if-not (lambda (literal)
                   (let ((proposition (init-literal-proposition literal)))
                     (and (consp proposition)
                          (eql (car proposition) relation))))
                 literals))


(defun positive-init-literal-p (literal)
  (not (and (consp literal)
            (eql (car literal) 'not))))


(defun positive-init-literals-with-relation (relation literals)
  (remove-if-not #'positive-init-literal-p
                 (init-literals-with-relation relation literals)))


(defun check-init-duplicate-fluent-keys (literals)
  "Signals an error when DEFINE-INIT repeats the same fluent storage key.

A fluent relation is stored by its non-fluent arguments. Repeating that key
would overwrite the previous value during install-init."
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (literal literals)
      (let* ((proposition (init-literal-proposition literal))
             (fluent-indices (get-prop-fluent-indices proposition)))
        (when fluent-indices
          (let ((key (get-fluentless-prop proposition fluent-indices)))
            (ut::if-it (gethash key seen)
              (error "~%Duplicate DEFINE-INIT fluent key.~%~
                      First literal:  ~S~%~
                      Second literal: ~S~%~
                      Storage key:    ~S~%~
                      Only one fluent value can be stored for this key."
                     ut::it literal key)
              (setf (gethash key seen) literal))))))))


(defun check-init-no-derived-facts (literals)
  "Rejects derived facts that should be produced by initialization actions."
  (when (init-dnf-controls-relation-p)
    (dolist (literal literals)
      (let ((proposition (init-literal-proposition literal)))
        (when (member (car proposition) '(open active depressed color))
          (error "~%DEFINE-INIT contains a derived fact.~%~
                  Literal: ~S~%~
                  Relation ~S is derived during initialization; remove it from DEFINE-INIT."
                 literal (car proposition)))))))


(defun init-literal-map (relation literals key-index value-index)
  (let ((map (make-hash-table :test #'equal)))
    (dolist (literal (init-literals-with-relation relation literals))
      (let ((proposition (init-literal-proposition literal)))
        (setf (gethash (nth key-index proposition) map)
              (nth value-index proposition))))
    map))


(defun init-object-location (object locations positions)
  (or (gethash object locations)
      (gethash object positions)))


(defun init-relation-signature (relation)
  (or (gethash relation *relations*)
      (gethash relation *static-relations*)))


(defun init-relation-argument-type (relation index)
  (let ((signature (init-relation-signature relation)))
    (when (and (listp signature)
               (<= 1 index (length signature)))
      (nth (1- index) signature))))


(defun init-type-spec-member-p (object type-spec)
  (cond
    ((symbolp type-spec)
     (init-type-member-p object type-spec))
    ((and (consp type-spec)
          (eql (car type-spec) 'either))
     (init-member-of-any-type-p object (cdr type-spec)))))


(defun init-type-spec-includes-type-p (type-spec type)
  (cond
    ((eql type-spec type) t)
    ((and (consp type-spec)
          (eql (car type-spec) 'either))
     (member type (cdr type-spec)))))


(defun init-location-valued-relation-p (relation)
  (init-type-spec-includes-type-p
    (init-relation-argument-type relation 2)
    'location))


(defun init-relation-can-locate-object-p (relation object)
  (and (init-location-valued-relation-p relation)
       (init-type-spec-member-p
         object
         (init-relation-argument-type relation 1))))


(defun init-on-location-consistency-required-p (object support)
  (and (init-relation-can-locate-object-p 'location object)
       (or (init-relation-can-locate-object-p 'location support)
           (init-relation-can-locate-object-p 'position support))))


(defun init-binary-on-literals (literals)
  (remove-if-not (lambda (literal)
                   (= (length (rest (init-literal-proposition literal))) 2))
                 (positive-init-literals-with-relation 'on literals)))


(defun init-check-object-not-held-and-located (literals locations)
  (dolist (literal (init-literals-with-relation 'holding literals))
    (destructuring-bind (agent object)
        (rest (init-literal-proposition literal))
      (declare (ignore agent))
      (when (gethash object locations)
        (error "~%DEFINE-INIT object is both held and located.~%~
                Literal: ~S~%~
                Object:  ~S"
               literal object)))))


(defun init-check-support-has-one-object (literal object support support-occupants)
  (let ((occupant (gethash support support-occupants)))
    (when occupant
      (error "~%DEFINE-INIT places multiple objects on the same support.~%~
              Literal:          ~S~%~
              Existing object:  ~S~%~
              New object:       ~S~%~
              Support:          ~S"
             literal occupant object support))
    (setf (gethash support support-occupants) object)))


(defun init-check-on-location-consistency
    (literal object support locations positions)
  (let ((object-location (gethash object locations))
        (support-location (init-object-location support locations positions)))
    (unless object-location
      (error "~%DEFINE-INIT places an object on a support, but the object has no LOCATION.~%~
              Literal: ~S~%~
              Object:  ~S"
             literal object))
    (unless support-location
      (error "~%DEFINE-INIT places an object on a support with no LOCATION or POSITION.~%~
              Literal: ~S~%~
              Support: ~S"
             literal support))
    (unless (eql object-location support-location)
      (error "~%DEFINE-INIT object location does not match support location.~%~
              Literal:          ~S~%~
              Object location:  ~S~%~
              Support location: ~S"
             literal object-location support-location))))


(defun init-check-on-cycle (literal object on-map)
  (let ((seen nil)
        (current object))
    (loop
      (when (member current seen)
        (error "~%DEFINE-INIT contains an ON cycle.~%~
                Literal: ~S~%~
                Cycle includes: ~S"
               literal current))
      (push current seen)
      (let ((support (gethash current on-map)))
        (unless support
          (return))
        (when (eql support current)
          (error "~%DEFINE-INIT places an object on itself.~%~
                  Literal: ~S~%~
                  Object:  ~S"
                 literal current))
        (setf current support)))))


(defun check-init-object-placement-consistency (literals)
  "Checks physical consistency of LOCATION, HOLDING, ON, and POSITION facts."
  (let ((locations (init-literal-map 'location literals 1 2))
        (positions (init-literal-map 'position literals 1 2))
        (on-map (init-literal-map 'on literals 1 2))
        (support-occupants (make-hash-table :test #'equal)))
    (init-check-object-not-held-and-located literals locations)
    (dolist (literal (init-binary-on-literals literals))
      (destructuring-bind (object support)
          (rest (init-literal-proposition literal))
        (let ((location-consistency-required-p
                (init-on-location-consistency-required-p object support)))
          (when (eql object support)
            (error "~%DEFINE-INIT places an object on itself.~%~
                    Literal: ~S~%~
                    Object:  ~S"
                   literal object))
          (when location-consistency-required-p
            (init-check-support-has-one-object
              literal object support support-occupants)
            (init-check-on-location-consistency
              literal object support locations positions)))
        (init-check-on-cycle literal object on-map)))))


(defun init-connector-paired-relation-p ()
  (init-type-spec-includes-type-p
    (init-relation-argument-type 'paired 1)
    'connector))


(defun check-init-paired-consistency (literals)
  "Checks basic consistency of initial connector pairings."
  (when (init-connector-paired-relation-p)
    (let ((locations (init-literal-map 'location literals 1 2))
          (pair-counts (make-hash-table :test #'equal)))
      (dolist (literal (init-literals-with-relation 'paired literals))
        (destructuring-bind (connector terminus)
            (rest (init-literal-proposition literal))
          (unless (gethash connector locations)
            (error "~%PAIRED connector has no LOCATION.~%~
                    Literal:  ~S~%~
                    Connector: ~S"
                   literal connector))
          (when (eql connector terminus)
            (error "~%PAIRED connector is paired to itself.~%~
                    Literal:  ~S~%~
                    Connector: ~S"
                   literal connector))
          (when (and (init-type-member-p terminus 'connector)
                     (not (gethash terminus locations)))
            (error "~%PAIRED connector target has no LOCATION.~%~
                    Literal: ~S~%~
                    Target:  ~S"
                   literal terminus))
          (incf (gethash connector pair-counts 0))))
      (maphash (lambda (connector count)
                 (when (> count *max-pairings*)
                   (error "~%PAIRED connector exceeds *MAX-PAIRINGS*.~%~
                           Connector: ~S~%~
                           Pairings:  ~D~%~
                           Limit:     ~D"
                          connector count *max-pairings*)))
               pair-counts))))


(defun init-paired-connector-edges (literals)
  (let ((edges (make-hash-table :test #'equal)))
    (dolist (literal (init-literals-with-relation 'paired literals))
      (destructuring-bind (connector terminus)
          (rest (init-literal-proposition literal))
        (when (init-type-member-p terminus 'connector)
          (push terminus (gethash connector edges)))))
    edges))


(defun init-connector-pairing-path-exists-p (start target edges)
  (let ((frontier (copy-list (gethash start edges)))
        (visited nil))
    (loop while frontier
          do (let ((current (pop frontier)))
               (when (eql current target)
                 (return t))
               (unless (member current visited)
                 (push current visited)
                 (setf frontier
                       (append (copy-list (gethash current edges))
                               frontier)))))))


(defun check-init-paired-connector-graph-acyclic (literals)
  "Checks that initial connector-to-connector pairings do not form cycles."
  (when (init-connector-paired-relation-p)
    (let ((edges (init-paired-connector-edges literals)))
      (maphash (lambda (connector targets)
                 (dolist (target targets)
                   (when (init-connector-pairing-path-exists-p
                           target connector edges)
                     (error "~%Initial connector pairings contain a cycle.~%~
                             Connector: ~S~%~
                             Target:    ~S"
                            connector target))))
               edges))))


(defun init-los-to-fixture-p (location fixture literals)
  (some (lambda (literal)
          (destructuring-bind (los-location occluders los-fixture)
              (rest (init-literal-proposition literal))
            (declare (ignore occluders))
            (and (eql location los-location)
                 (eql fixture los-fixture))))
        (init-literals-with-relation 'los-to-fixture literals)))


(defun init-los-to-location-p (location1 location2 literals)
  (some (lambda (literal)
          (destructuring-bind (los-location1 occluders los-location2)
              (rest (init-literal-proposition literal))
            (declare (ignore occluders))
            (or (and (eql location1 los-location1)
                     (eql location2 los-location2))
                (and (eql location1 los-location2)
                     (eql location2 los-location1)))))
        (init-literals-with-relation 'los-to-location literals)))


(defun init-check-paired-fixture-sightline
    (literal connector connector-location terminus literals)
  (unless (init-los-to-fixture-p connector-location terminus literals)
    (error "~%PAIRED fixture target has no LOS-TO-FIXTURE from connector location.~%~
            Literal:            ~S~%~
            Connector:          ~S~%~
            Connector location: ~S~%~
            Target:             ~S"
           literal connector connector-location terminus)))


(defun init-check-paired-connector-sightline
    (literal connector connector-location terminus terminus-location literals)
  (when (eql connector-location terminus-location)
    (error "~%PAIRED connector target is at the same location.~%~
            Literal:  ~S~%~
            Connector: ~S~%~
            Target:    ~S~%~
            Location:  ~S"
           literal connector terminus connector-location))
  (unless (init-los-to-location-p
            connector-location terminus-location literals)
    (error "~%PAIRED connector target has no LOS-TO-LOCATION between connector locations.~%~
            Literal:            ~S~%~
            Connector:          ~S~%~
            Connector location: ~S~%~
            Target:             ~S~%~
            Target location:    ~S"
           literal connector connector-location terminus terminus-location)))


(defun check-init-paired-sightlines (literals)
  "Checks that initial pairings have authored sightline topology."
  (when (init-connector-paired-relation-p)
    (let ((locations (init-literal-map 'location literals 1 2)))
      (dolist (literal (init-literals-with-relation 'paired literals))
        (destructuring-bind (connector terminus)
            (rest (init-literal-proposition literal))
          (let ((connector-location (gethash connector locations)))
            (when connector-location
              (cond
                ((or (init-type-member-p terminus 'transmitter)
                     (init-type-member-p terminus 'receiver))
                 (init-check-paired-fixture-sightline
                   literal connector connector-location terminus literals))
                ((init-type-member-p terminus 'connector)
                 (let ((terminus-location (gethash terminus locations)))
                   (when terminus-location
                     (init-check-paired-connector-sightline
                       literal connector connector-location
                       terminus terminus-location literals))))))))))))


(defun init-crossings-along-beam-map (literals)
  (let ((beams (make-hash-table :test #'equal)))
    (dolist (literal (init-literals-with-relation 'crossings-along-beam> literals))
      (destructuring-bind (source crossings destination)
          (rest (init-literal-proposition literal))
        (setf (gethash (list source destination) beams) crossings)))
    beams))


(defun init-list-prefix-p (prefix list)
  (and (<= (length prefix) (length list))
       (every #'eql prefix list)))


(defun init-type-member-p (object type)
  (member object (gethash type *types*)))


(defun init-member-of-any-type-p (object types)
  (some (lambda (type)
          (init-type-member-p object type))
        types))


(defun init-check-list-items-have-types (literal items types)
  (dolist (item items)
    (unless (init-member-of-any-type-p item types)
      (error "~%Invalid item in DEFINE-INIT list.~%~
              Literal:       ~S~%~
              Invalid item:  ~S~%~
              Expected type: ~S"
             literal item types))))


(defun check-init-list-relation-items-have-types
    (literals relation types)
  (dolist (literal (init-literals-with-relation relation literals))
    (init-check-list-items-have-types
      literal
      (third (init-literal-proposition literal))
      types)))


(defun init-check-controls-clauses (literal clauses)
  (unless (listp clauses)
    (error "~%CONTROLS relation must use a DNF list of controller clauses.~%~
            Literal: ~S~%~
            Clauses: ~S"
           literal clauses))
  (dolist (clause clauses)
    (unless (listp clause)
      (error "~%CONTROLS relation must use a DNF list of controller clauses.~%~
              Literal: ~S~%~
              Clause:  ~S"
             literal clause))
    (init-check-list-items-have-types literal clause '(receiver plate))))


(defun check-init-controls-list-contents (literals)
  (dolist (literal (init-literals-with-relation 'controls literals))
    (init-check-controls-clauses
      literal
      (second (init-literal-proposition literal)))))


(defun init-dnf-controls-relation-p ()
  (equal (gethash 'controls *fluent-relation-indices*) '(1 3)))


(defun check-init-list-contents (literals)
  "Checks domain-specific element types inside DEFINE-INIT list values."
  (check-init-list-relation-items-have-types
    literals 'los-to-fixture '(gate))
  (check-init-list-relation-items-have-types
    literals 'los-to-location '(gate))
  (check-init-list-relation-items-have-types
    literals 'reachable-via '(gate))
  (check-init-list-relation-items-have-types
    literals 'beam-via '(gate location))
  (check-init-list-relation-items-have-types
    literals 'walk-via '(gate screen ladder))
  (check-init-list-relation-items-have-types
    literals 'traversable> '(ladder))
  (when (init-dnf-controls-relation-p)
    (check-init-controls-list-contents literals)))


(defun check-init-controls-modes (literals)
  "Checks that CONTROLS uses only modes implemented by update-gate-status!."
  (when (init-dnf-controls-relation-p)
    (dolist (literal (init-literals-with-relation 'controls literals))
      (let ((mode (fourth (init-literal-proposition literal))))
        (unless (member mode '(normal inverted))
          (error "~%CONTROLS uses an unsupported mode.~%~
                  Literal:          ~S~%~
                  Unsupported mode: ~S~%~
                  Supported modes:  (NORMAL INVERTED)"
                 literal mode))))))


(defun init-chroma-map (literals)
  (let ((chromas (make-hash-table :test #'equal)))
    (dolist (literal (init-literals-with-relation 'chroma literals))
      (destructuring-bind (endpoint hue)
          (rest (init-literal-proposition literal))
        (setf (gethash endpoint chromas) hue)))
    chromas))


(defun init-coupled-p (transmitter receiver literals)
  (some (lambda (literal)
          (destructuring-bind (coupled-transmitter coupled-receiver)
              (rest (init-literal-proposition literal))
            (and (eql transmitter coupled-transmitter)
                 (eql receiver coupled-receiver))))
        (init-literals-with-relation 'coupled literals)))


(defun init-beam-via-p (transmitter receiver literals)
  (some (lambda (literal)
          (destructuring-bind (beam-transmitter obstacles beam-receiver)
              (rest (init-literal-proposition literal))
            (declare (ignore obstacles))
            (and (eql transmitter beam-transmitter)
                 (eql receiver beam-receiver))))
        (init-literals-with-relation 'beam-via literals)))


(defun check-init-coupled-beam-consistency (literals)
  "Checks that coupled transmitter/receiver beams have chroma and corridors."
  (let ((chromas (init-chroma-map literals)))
    (dolist (literal (init-literals-with-relation 'coupled literals))
      (destructuring-bind (transmitter receiver)
          (rest (init-literal-proposition literal))
        (let ((transmitter-hue (gethash transmitter chromas))
              (receiver-hue (gethash receiver chromas)))
          (unless transmitter-hue
            (error "~%COUPLED transmitter has no CHROMA entry.~%~
                    Literal:     ~S~%~
                    Transmitter: ~S"
                   literal transmitter))
          (unless receiver-hue
            (error "~%COUPLED receiver has no CHROMA entry.~%~
                    Literal: ~S~%~
                    Receiver: ~S"
                   literal receiver))
          (unless (eql transmitter-hue receiver-hue)
            (error "~%COUPLED endpoints have mismatched CHROMA values.~%~
                    Literal:         ~S~%~
                    Transmitter hue: ~S~%~
                    Receiver hue:    ~S"
                   literal transmitter-hue receiver-hue))
          (unless (init-beam-via-p transmitter receiver literals)
            (error "~%COUPLED pair has no matching BEAM-VIA corridor.~%~
                    Literal:       ~S~%~
                    Expected beam: (BEAM-VIA ~S ... ~S)"
                   literal transmitter receiver))))))
  (dolist (literal (init-literals-with-relation 'beam-via literals))
    (destructuring-bind (transmitter obstacles receiver)
        (rest (init-literal-proposition literal))
      (declare (ignore obstacles))
      (unless (init-coupled-p transmitter receiver literals)
        (error "~%BEAM-VIA corridor has no matching COUPLED pair.~%~
                Literal:        ~S~%~
                Expected pair:  (COUPLED ~S ~S)"
               literal transmitter receiver)))))


(defun init-valid-directed-beam-p (source destination)
  (or (and (init-type-member-p source 'transmitter)
           (or (init-type-member-p destination 'receiver)
               (init-type-member-p destination 'location)))
      (and (init-type-member-p source 'location)
           (or (init-type-member-p destination 'receiver)
               (init-type-member-p destination 'location)))))


(defun init-first-matching-list-value (relation literals test)
  (dolist (literal (init-literals-with-relation relation literals))
    (let ((proposition (init-literal-proposition literal)))
      (when (funcall test proposition)
        (return (values (third proposition) t)))))
  (values nil nil))


(defun init-occluders-for-directed-beam (source destination literals)
  (cond
    ((and (init-type-member-p source 'location)
          (init-type-member-p destination 'location))
     (init-first-matching-list-value
       'los-to-location literals
       (lambda (prop)
         (or (and (eql (second prop) source)
                  (eql (fourth prop) destination))
             (and (eql (second prop) destination)
                  (eql (fourth prop) source))))))
    ((init-type-member-p source 'location)
     (init-first-matching-list-value
       'los-to-fixture literals
       (lambda (prop)
         (and (eql (second prop) source)
              (eql (fourth prop) destination)))))
    ((and (init-type-member-p source 'transmitter)
          (init-type-member-p destination 'receiver))
     (init-first-matching-list-value
       'beam-via literals
       (lambda (prop)
         (and (eql (second prop) source)
              (eql (fourth prop) destination)))))
    ((and (init-type-member-p source 'transmitter)
          (init-type-member-p destination 'location))
     (init-first-matching-list-value
       'los-to-fixture literals
       (lambda (prop)
         (and (eql (second prop) destination)
              (eql (fourth prop) source)))))))


(defun init-defined-beam-crossings (literals)
  (let ((crossings nil))
    (dolist (literal (init-literals-with-relation 'beam-crossing> literals))
      (destructuring-bind (crossing from1 to1 from2 to2)
          (rest (init-literal-proposition literal))
        (declare (ignore from1 to1 from2 to2))
        (push crossing crossings)))
    crossings))


(defun init-beams-for-crossing-map (literals)
  (let ((crossing-beams (make-hash-table :test #'equal)))
    (dolist (literal (init-literals-with-relation 'beam-crossing> literals))
      (destructuring-bind (crossing from1 to1 from2 to2)
          (rest (init-literal-proposition literal))
        (setf (gethash crossing crossing-beams)
              (list (list from1 to1)
                    (list from2 to2)))))
    crossing-beams))


(defun check-init-beam-crossing-endpoints (literals)
  "Checks that BEAM-CROSSING> beams use a supported directed endpoint shape."
  (dolist (literal (init-literals-with-relation 'beam-crossing> literals))
    (destructuring-bind (crossing from1 to1 from2 to2)
        (rest (init-literal-proposition literal))
      (unless (init-valid-directed-beam-p from1 to1)
        (error "~%BEAM-CROSSING> uses an unsupported directed beam.~%~
                Literal:  ~S~%~
                Crossing: ~S~%~
                Beam:     ~S -> ~S"
               literal crossing from1 to1))
      (unless (init-valid-directed-beam-p from2 to2)
        (error "~%BEAM-CROSSING> uses an unsupported directed beam.~%~
                Literal:  ~S~%~
                Crossing: ~S~%~
                Beam:     ~S -> ~S"
               literal crossing from2 to2))
      (when (eql from1 to1)
        (error "~%BEAM-CROSSING> declares a self-beam.~%~
                Literal:  ~S~%~
                Crossing: ~S~%~
                Beam:     ~S -> ~S"
               literal crossing from1 to1))
      (when (eql from2 to2)
        (error "~%BEAM-CROSSING> declares a self-beam.~%~
                Literal:  ~S~%~
                Crossing: ~S~%~
                Beam:     ~S -> ~S"
               literal crossing from2 to2))
      (when (equal (list from1 to1) (list from2 to2))
        (error "~%BEAM-CROSSING> declares the same beam twice.~%~
                Literal:  ~S~%~
                Crossing: ~S~%~
                Beam:     ~S -> ~S"
               literal crossing from1 to1)))))


(defun init-check-list-has-unique-items (literal items)
  (let ((seen nil))
    (dolist (item items)
      (when (member item seen)
        (error "~%Duplicate item in DEFINE-INIT list.~%~
                Literal: ~S~%~
                Duplicate item: ~S"
               literal item))
      (push item seen))))


(defun check-init-crossing-lists-have-unique-items (literals)
  "Checks that crossing-order lists do not repeat a crossing."
  (dolist (literal (init-literals-with-relation 'crossings-along-beam> literals))
    (destructuring-bind (source crossings destination)
        (rest (init-literal-proposition literal))
      (declare (ignore source destination))
      (init-check-list-has-unique-items literal crossings)))
  (dolist (literal (init-literals-with-relation 'crossings-before-gate> literals))
    (destructuring-bind (source before gate destination)
        (rest (init-literal-proposition literal))
      (declare (ignore source gate destination))
      (init-check-list-has-unique-items literal before))))


(defun init-check-crossing-list-items-are-defined (literal items defined)
  (dolist (item items)
    (unless (member item defined)
      (error "~%Crossing-order list references a crossing with no BEAM-CROSSING> definition.~%~
              Literal:  ~S~%~
              Crossing: ~S"
             literal item))))


(defun check-init-crossing-list-items-are-defined (literals)
  "Checks that crossing-order lists reference authored beam crossings."
  (let ((defined (init-defined-beam-crossings literals)))
    (dolist (literal (init-literals-with-relation 'crossings-along-beam> literals))
      (destructuring-bind (source crossings destination)
          (rest (init-literal-proposition literal))
        (declare (ignore source destination))
        (init-check-crossing-list-items-are-defined literal crossings defined)))
    (dolist (literal (init-literals-with-relation 'crossings-before-gate> literals))
      (destructuring-bind (source before gate destination)
          (rest (init-literal-proposition literal))
        (declare (ignore source gate destination))
        (init-check-crossing-list-items-are-defined literal before defined)))))


(defun init-check-beam-crossing-is-indexed
    (literal crossing source destination beams)
  (let ((crossings (gethash (list source destination) beams)))
    (unless crossings
      (error "~%BEAM-CROSSING> declares a beam with no CROSSINGS-ALONG-BEAM> entry.~%~
              Literal:  ~S~%~
              Crossing: ~S~%~
              Beam:     ~S -> ~S"
             literal crossing source destination))
    (unless (member crossing crossings)
      (error "~%BEAM-CROSSING> is missing from its declared beam's crossing list.~%~
              Literal:              ~S~%~
              Crossing:             ~S~%~
              Beam:                 ~S -> ~S~%~
              Crossings along beam: ~S"
             literal crossing source destination crossings))))


(defun check-init-beam-crossings-are-indexed-by-declared-beams (literals)
  "Checks that each BEAM-CROSSING> appears on both of its declared beam lists."
  (let ((beams (init-crossings-along-beam-map literals)))
    (dolist (literal (init-literals-with-relation 'beam-crossing> literals))
      (destructuring-bind (crossing from1 to1 from2 to2)
          (rest (init-literal-proposition literal))
        (init-check-beam-crossing-is-indexed literal crossing from1 to1 beams)
        (init-check-beam-crossing-is-indexed literal crossing from2 to2 beams)))))


(defun check-init-crossing-lists-match-declared-beams (literals)
  "Checks that each crossing listed on a beam belongs to that declared beam."
  (let ((crossing-beams (init-beams-for-crossing-map literals)))
    (dolist (literal (init-literals-with-relation 'crossings-along-beam> literals))
      (destructuring-bind (source crossings destination)
          (rest (init-literal-proposition literal))
        (let ((beam (list source destination)))
          (dolist (crossing crossings)
            (unless (member beam (gethash crossing crossing-beams)
                            :test #'equal)
              (error "~%CROSSINGS-ALONG-BEAM> lists a crossing on an undeclared beam.~%~
                      Literal:       ~S~%~
                      Crossing:      ~S~%~
                      Listed beam:   ~S -> ~S~%~
                      Declared beams: ~S"
                     literal crossing source destination
                     (gethash crossing crossing-beams)))))))))


(defun check-init-location-beam-reverses (literals)
  "Checks that location-location crossing lists exist in both directions and reverse exactly."
  (let ((beams (init-crossings-along-beam-map literals)))
    (dolist (literal (init-literals-with-relation 'crossings-along-beam> literals))
      (destructuring-bind (source crossings destination)
          (rest (init-literal-proposition literal))
        (when (and (init-type-member-p source 'location)
                   (init-type-member-p destination 'location))
          (let ((reverse-crossings (gethash (list destination source) beams)))
            (unless reverse-crossings
              (error "~%Location-to-location CROSSINGS-ALONG-BEAM> has no reverse entry.~%~
                      Literal: ~S~%~
                      Expected reverse beam: (CROSSINGS-ALONG-BEAM> ~S ... ~S)"
                     literal destination source))
            (unless (equal reverse-crossings (reverse crossings))
              (error "~%Location-to-location CROSSINGS-ALONG-BEAM> reverse ordering is inconsistent.~%~
                      Literal:           ~S~%~
                      Crossings:         ~S~%~
                      Reverse crossings: ~S~%~
                      Expected reverse:  ~S"
                     literal crossings reverse-crossings (reverse crossings)))))))))


(defun check-init-crossings-before-gate-prefixes (literals)
  "Checks that each CROSSINGS-BEFORE-GATE> list is an initial prefix of
the matching CROSSINGS-ALONG-BEAM> list."
  (let ((beams (init-crossings-along-beam-map literals)))
    (dolist (literal (init-literals-with-relation 'crossings-before-gate> literals))
      (destructuring-bind (source before gate destination)
          (rest (init-literal-proposition literal))
        (let* ((beam-key (list source destination))
               (along (gethash beam-key beams)))
          (unless along
            (error "~%CROSSINGS-BEFORE-GATE> has no matching CROSSINGS-ALONG-BEAM>.~%~
                    Literal: ~S~%~
                    Expected matching beam: (CROSSINGS-ALONG-BEAM> ~S ... ~S)"
                   literal source destination))
          (unless (init-list-prefix-p before along)
            (error "~%CROSSINGS-BEFORE-GATE> list is not a prefix of CROSSINGS-ALONG-BEAM>.~%~
                    Literal:              ~S~%~
                    Crossings before gate: ~S~%~
                    Crossings along beam:  ~S~%~
                    Gate:                  ~S"
                   literal before along gate)))))))


(defun check-init-crossings-before-gate-gates-occlude-beams (literals)
  "Checks that each CROSSINGS-BEFORE-GATE> gate is an occluder for its beam."
  (dolist (literal (init-literals-with-relation 'crossings-before-gate> literals))
    (destructuring-bind (source before gate destination)
        (rest (init-literal-proposition literal))
      (declare (ignore before))
      (multiple-value-bind (occluders found-p)
          (init-occluders-for-directed-beam source destination literals)
        (unless found-p
          (error "~%CROSSINGS-BEFORE-GATE> has no sightline/corridor facts for its beam.~%~
                  Literal: ~S~%~
                  Beam:    ~S -> ~S"
                 literal source destination))
        (unless (member gate occluders)
          (error "~%CROSSINGS-BEFORE-GATE> gate does not occlude its beam.~%~
                  Literal:  ~S~%~
                  Gate:     ~S~%~
                  Beam:     ~S -> ~S~%~
                  Occluders: ~S"
                 literal gate source destination occluders))))))

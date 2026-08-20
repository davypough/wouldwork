;;; Filename: -beam-relay-init-checks.lisp

;;; Initialization validation for connector pairing and relay sightlines.


(in-package :ww)


(define-init-check beam-relay-init-check (literals)
  (check-init-paired-consistency literals)
  (check-init-paired-connector-graph-acyclic literals)
  (check-init-paired-sightlines literals))


(define-init-check-helper init-connector-paired-relation-p ()
  (init-type-spec-includes-type-p
    (init-relation-argument-type 'paired 1)
    'connector))


(define-init-check-helper check-init-paired-consistency (literals)
  "Checks basic consistency of initial connector pairings."
  (when (init-connector-paired-relation-p)
    (let ((locations (init-literal-map 'has-location literals 1 2))
          (pair-counts (make-hash-table :test #'equal)))
      (dolist (literal (init-literals-with-relation 'paired literals))
        (destructuring-bind (connector terminus)
            (rest (init-literal-proposition literal))
          (unless (gethash connector locations)
            (fail-init-check nil "~%PAIRED connector has no HAS-LOCATION.~%~
                    Literal:  ~S~%~
                    Connector: ~S"
                   literal connector))
          (when (eql connector terminus)
            (fail-init-check nil "~%PAIRED connector is paired to itself.~%~
                    Literal:  ~S~%~
                    Connector: ~S"
                   literal connector))
          (when (and (init-type-member-p terminus 'connector)
                     (not (gethash terminus locations)))
            (fail-init-check nil "~%PAIRED connector target has no HAS-LOCATION.~%~
                    Literal: ~S~%~
                    Target:  ~S"
                   literal terminus))
          (incf (gethash connector pair-counts 0))))
      (maphash (lambda (connector count)
                 (when (> count *max-pairings*)
                   (fail-init-check nil "~%PAIRED connector exceeds *MAX-PAIRINGS*.~%~
                           Connector: ~S~%~
                           Pairings:  ~D~%~
                           Limit:     ~D"
                          connector count *max-pairings*)))
               pair-counts))))


(define-init-check-helper init-paired-connector-edges (literals)
  (let ((edges (make-hash-table :test #'equal)))
    (dolist (literal (init-literals-with-relation 'paired literals))
      (destructuring-bind (connector terminus)
          (rest (init-literal-proposition literal))
        (when (init-type-member-p terminus 'connector)
          (push terminus (gethash connector edges)))))
    edges))


(define-init-check-helper init-connector-pairing-path-exists-p (start target edges)
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


(define-init-check-helper check-init-paired-connector-graph-acyclic (literals)
  "Checks that initial connector-to-connector pairings do not form cycles."
  (when (init-connector-paired-relation-p)
    (let ((edges (init-paired-connector-edges literals)))
      (maphash (lambda (connector targets)
                 (dolist (target targets)
                   (when (init-connector-pairing-path-exists-p
                           target connector edges)
                     (fail-init-check nil "~%Initial connector pairings contain a cycle.~%~
                             Connector: ~S~%~
                             Target:    ~S"
                            connector target))))
               edges))))


(define-init-check-helper init-apparatus-has-potential-sightline-p (apparatus literals)
  (some (lambda (literal)
          (destructuring-bind (los-location occluders los-apparatus)
              (rest (init-literal-proposition literal))
            (declare (ignore los-location occluders))
            (eql apparatus los-apparatus)))
        (positive-init-literals-with-relation 'los-via literals)))


(define-init-check-helper init-location-has-potential-sightline-p (location literals)
  (some (lambda (literal)
          (destructuring-bind (los-location1 occluders los-location2)
              (rest (init-literal-proposition literal))
            (declare (ignore occluders))
            (or (eql location los-location1)
                (eql location los-location2))))
        (positive-init-literals-with-relation 'los-via literals)))


(define-init-check-helper init-check-paired-apparatus-sightline
    (literal connector apparatus literals)
  (unless (init-apparatus-has-potential-sightline-p apparatus literals)
    (fail-init-check nil "~%PAIRED apparatus target has no potential LOS-VIA from any location.~%~
            Literal:   ~S~%~
            Connector: ~S~%~
            Target:    ~S"
           literal connector apparatus)))


(define-init-check-helper init-check-paired-connector-sightline
    (literal connector connector-location terminus terminus-location literals)
  (when (eql connector-location terminus-location)
    (fail-init-check nil "~%PAIRED connector target is at the same location.~%~
            Literal:  ~S~%~
            Connector: ~S~%~
            Target:    ~S~%~
            Location:  ~S"
           literal connector terminus connector-location))
  (unless (init-location-has-potential-sightline-p terminus-location literals)
    (fail-init-check nil "~%PAIRED connector target location has no potential LOS-VIA from any location.~%~
            Literal:            ~S~%~
            Connector:          ~S~%~
            Connector location: ~S~%~
            Target:             ~S~%~
            Target location:    ~S"
           literal connector connector-location terminus terminus-location)))


(define-init-check-helper check-init-paired-sightlines (literals)
  "Checks that each initial pairing target has potential sightline topology."
  (when (init-connector-paired-relation-p)
    (let ((locations (init-literal-map 'has-location literals 1 2)))
      (dolist (literal (init-literals-with-relation 'paired literals))
        (destructuring-bind (connector terminus)
            (rest (init-literal-proposition literal))
          (let ((connector-location (gethash connector locations)))
            (when connector-location
              (cond
                ((or (init-type-member-p terminus 'transmitter)
                     (init-type-member-p terminus 'receiver)
                     (init-type-member-p terminus 'repeater))
                 (init-check-paired-apparatus-sightline
                   literal connector terminus literals))
                ((init-type-member-p terminus 'connector)
                 (let ((terminus-location (gethash terminus locations)))
                   (when terminus-location
                     (init-check-paired-connector-sightline
                       literal connector connector-location
                       terminus terminus-location literals))))))))))))



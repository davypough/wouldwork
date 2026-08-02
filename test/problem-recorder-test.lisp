;;; Filename: problem-recorder-test.lisp

;;; Dedicated zero-action regression for recorder identity and placement.  The authored
;;; mapping pairs agents with agents and connectors with connectors, while an unmapped fan
;;; proves that MOBILE-OBJECT membership alone does not assign a recording side.  Direct
;;; validation probes characterize the one-to-one, disjoint, category-compatible contract.
;;; Expected minimum path length: zero.

(in-package :ww)


(ww-set *problem-name* recorder-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 0)


;;;; TYPES ;;;;


(define-types
  agent (live-agent ghost-agent other-live-agent other-ghost-agent third-agent)
  connector (live-connector ghost-connector)
  fan (unmapped-fan)
  recorder (recorder1 unpositioned-recorder)
  location (recorder-site alternate-site))


;;;; TECHNOLOGY INCLUDE ;;;;


(include-tech recorder)


;;;; INITIALIZATION ;;;;


(define-init
  (has-position recorder1 recorder-site)
  (recording-copy> live-agent ghost-agent)
  (recording-copy> live-connector ghost-connector))


;;;; SCHEMA AND VALIDATION CHARACTERIZATION ;;;;


(setf
  (symbol-function 'recorder-schema-valid-p)
  (lambda ()
    (multiple-value-bind (static-signature staticp)
        (gethash 'recording-copy> *static-relations*)
      (multiple-value-bind (dynamic-signature dynamicp)
          (gethash 'recording-copy> *relations*)
        (declare (ignore dynamic-signature))
        (and
          staticp
          (equal static-signature '(mobile-object mobile-object))
          (not dynamicp)
          (equal
            (gethash 'recording-copy> *fluent-relation-indices*)
            '(2))
          (equal
            (gethash 'fixed-position-object *type-components*)
            '(plate ladder floor-gears wall-gears angled-gears recorder))
          (member 'recorder1 (gethash 'fixed-position-object *types*)))))))


(setf
  (symbol-function 'recorder-error-contains-p)
  (lambda (operation expected-text)
    (let ((condition
            (handler-case
                (progn
                  (funcall operation)
                  nil)
              (error (error-condition)
                error-condition))))
      (and condition
           (not
             (null
               (search expected-text
                       (princ-to-string condition))))))))


(setf
  (symbol-function 'recorder-validation-valid-p)
  (lambda ()
    (and
      (funcall (symbol-function 'recorder-error-contains-p)
        (lambda ()
          (check-proposition
            '(recording-copy> recorder1 ghost-agent)))
        "not of specified type MOBILE-OBJECT")
      (funcall (symbol-function 'recorder-error-contains-p)
        (lambda ()
          (check-init-duplicate-fluent-keys
            '((recording-copy> live-agent ghost-agent)
              (recording-copy> live-agent other-ghost-agent))))
        "Duplicate DEFINE-INIT fluent key")
      (funcall (symbol-function 'recorder-error-contains-p)
        (lambda ()
          (check-init-recorder-consistency
            '((recording-copy> live-agent ghost-agent)
              (recording-copy> other-live-agent ghost-agent))))
        "repeats a ghost object")
      (funcall (symbol-function 'recorder-error-contains-p)
        (lambda ()
          (check-init-recorder-consistency
            '((recording-copy> live-agent live-agent))))
        "maps an object to itself")
      (funcall (symbol-function 'recorder-error-contains-p)
        (lambda ()
          (check-init-recorder-consistency
            '((recording-copy> live-agent ghost-agent)
              (recording-copy> ghost-agent third-agent))))
        "both live and ghost sides")
      (funcall (symbol-function 'recorder-error-contains-p)
        (lambda ()
          (check-init-recorder-consistency
            '((recording-copy> live-agent ghost-connector))))
        "incompatible object categories"))))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query recorder-scenarios-valid ()
  (and
    (recording-copy> live-agent ghost-agent)
    (recording-copy> live-connector ghost-connector)
    (not (recording-copy> ghost-agent live-agent))
    (live-recording-object live-agent)
    (live-recording-object live-connector)
    (not (ghost-recording-object live-agent))
    (ghost-recording-object ghost-agent)
    (ghost-recording-object ghost-connector)
    (not (live-recording-object ghost-agent))
    (not (live-recording-object unmapped-fan))
    (not (ghost-recording-object unmapped-fan))
    (has-position recorder1 recorder-site)
    (not (has-position recorder1 alternate-site))
    (not (bind
           (has-position
             unpositioned-recorder $unpositioned-recorder-location)))
    (recorder-schema-valid-p)
    (recorder-validation-valid-p)))


(define-goal
  (recorder-scenarios-valid))

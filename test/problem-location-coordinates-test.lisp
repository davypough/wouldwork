;;; Filename: problem-location-coordinates-test.lisp

;;; Dedicated zero-action regression for the shared -location-coordinates role.
;;; Independent fixtures characterize the complete LOCATION-COORDS> contract:
;;;
;;;   1. Zero, negative, and exact fractional coordinates bind without coercion.
;;;   2. Two distinct locations may legitimately occupy the same coordinate pair.
;;;   3. A location with no authored coordinates receives no default position.
;;;   4. The relation remains static, location-keyed, rational-valued, and functional:
;;;      non-location and floating-point arguments are rejected, and a second pair for
;;;      one location is reported as a duplicate fluent-key inconsistency.
;;;
;;; The goal directly characterizes the installed schema and unchanged static facts.
;;; Initial and final dynamic states are empty.  Expected minimum path length: zero.

(in-package :ww)


(ww-set *problem-name* location-coordinates-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 1)


;;;; TYPES ;;;;


(define-types
  location (origin fractional coincident-a coincident-b unpositioned))


;;;; TECHNOLOGY INCLUDE ;;;;


(include-tech -location-coordinates)


;;;; INITIALIZATION ;;;;


(define-init
  (location-coords> origin 0 0)
  (location-coords> fractional -7/3 11/5)
  (location-coords> coincident-a 5/2 -4)
  (location-coords> coincident-b 5/2 -4))


;;;; SCHEMA AND VALIDATION CHARACTERIZATION ;;;;


(setf
  (symbol-function 'location-coordinates-schema-valid-p)
  (lambda ()
    (multiple-value-bind (static-signature staticp)
        (gethash 'location-coords> *static-relations*)
      (multiple-value-bind (dynamic-signature dynamicp)
          (gethash 'location-coords> *relations*)
        (declare (ignore dynamic-signature))
        (and staticp
             (equal static-signature '(location rational rational))
             (not dynamicp)
             (equal
               (gethash 'location-coords> *fluent-relation-indices*)
               '(2 3)))))))


(setf
  (symbol-function 'location-coordinates-error-contains-p)
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
  (symbol-function 'invalid-location-coordinate-object-rejected-p)
  (lambda ()
    (location-coordinates-error-contains-p
      (lambda ()
        (check-proposition
          '(location-coords> 0 0 0)))
      "not of specified type LOCATION")))


(setf
  (symbol-function 'invalid-location-coordinate-number-rejected-p)
  (lambda ()
    (location-coordinates-error-contains-p
      (lambda ()
        (check-proposition
          '(location-coords> origin 1.0 0)))
      "not of specified type RATIONAL")))


(setf
  (symbol-function 'duplicate-location-coordinate-rejected-p)
  (lambda ()
    (location-coordinates-error-contains-p
      (lambda ()
        (check-init-duplicate-fluent-keys
          '((location-coords> origin 0 0)
            (location-coords> origin 1 1))))
      "Duplicate DEFINE-INIT fluent key")))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query location-coordinates-scenarios-valid ()
  (do
    (bind (location-coords> origin $origin-x $origin-y))
    (bind
      (location-coords>
        fractional $fractional-x $fractional-y))
    (bind
      (location-coords>
        coincident-a $coincident-a-x $coincident-a-y))
    (bind
      (location-coords>
        coincident-b $coincident-b-x $coincident-b-y))
    (and
      ;; Exact values, including zero, negative, and rational boundaries.
      (= $origin-x 0)
      (= $origin-y 0)
      (= $fractional-x -7/3)
      (= $fractional-y 11/5)
      (= $coincident-a-x 5/2)
      (= $coincident-a-y -4)
      (= $coincident-b-x 5/2)
      (= $coincident-b-y -4)

      ;; The substrate never invents a default or an alternate coordinate.
      (not
        (bind
          (location-coords>
            unpositioned $unpositioned-x $unpositioned-y)))
      (not (location-coords> origin 1 0))
      (not (location-coords> fractional -7/3 11/4))

      ;; The installed relation and its authoring failures are the behavior under test.
      (location-coordinates-schema-valid-p)
      (invalid-location-coordinate-object-rejected-p)
      (invalid-location-coordinate-number-rejected-p)
      (duplicate-location-coordinate-rejected-p))))


(define-goal
  (location-coordinates-scenarios-valid))

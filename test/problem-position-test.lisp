;;; Filename: problem-position-test.lisp

;;; Dedicated zero-action regression for the shared -position role.  A complete
;;; matrix gives every FIXED-POSITION-OBJECT leaf one positioned fixture and one
;;; fixture with no authored HAS-POSITION fact:
;;;
;;;   plate, ladder, floor-gears, wall-gears, and angled-gears.
;;;
;;; The positioned plate and ladder deliberately share one location, proving that
;;; fixed placement is functional by object rather than exclusive by location.
;;; The characterization also verifies the static relation schema, exact type
;;; composition, wrong-type rejection, and duplicate fluent-key inconsistency.
;;;
;;; Initial and final dynamic states are empty.  The five authored static positions
;;; remain unchanged.  Expected minimum path length: zero.

(in-package :ww)


(ww-set *problem-name* position-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 0)


;;;; TYPES ;;;;


(define-types
  location (shared-site floor-site wall-site angled-site alternate-site)
  plate (positioned-plate unpositioned-plate)
  ladder (positioned-ladder unpositioned-ladder)
  floor-gears (positioned-floor-gears unpositioned-floor-gears)
  wall-gears (positioned-wall-gears unpositioned-wall-gears)
  angled-gears (positioned-angled-gears unpositioned-angled-gears))


;;;; TECHNOLOGY INCLUDE ;;;;


(include-tech -position)


;;;; INITIALIZATION ;;;;


(define-init
  (has-position positioned-plate shared-site)
  (has-position positioned-ladder shared-site)
  (has-position positioned-floor-gears floor-site)
  (has-position positioned-wall-gears wall-site)
  (has-position positioned-angled-gears angled-site))


;;;; SCHEMA AND VALIDATION CHARACTERIZATION ;;;;


(setf
  (symbol-function 'position-schema-valid-p)
  (lambda ()
    (multiple-value-bind (static-signature staticp)
        (gethash 'has-position *static-relations*)
      (multiple-value-bind (dynamic-signature dynamicp)
          (gethash 'has-position *relations*)
        (declare (ignore dynamic-signature))
        (let ((expected-instances
                '(positioned-plate unpositioned-plate
                  positioned-ladder unpositioned-ladder
                  positioned-floor-gears unpositioned-floor-gears
                  positioned-wall-gears unpositioned-wall-gears
                  positioned-angled-gears unpositioned-angled-gears)))
          (and
            (equal
              (gethash 'fixed-position-object *type-components*)
              '(plate ladder floor-gears wall-gears angled-gears))
            (null
              (set-exclusive-or
                (gethash 'fixed-position-object *types*)
                expected-instances))
            staticp
            (equal
              static-signature
              '(fixed-position-object location))
            (not dynamicp)
            (equal
              (gethash 'has-position *fluent-relation-indices*)
              '(2))))))))


(setf
  (symbol-function 'position-error-contains-p)
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
  (symbol-function 'invalid-position-object-rejected-p)
  (lambda ()
    (position-error-contains-p
      (lambda ()
        (check-proposition
          '(has-position 0 shared-site)))
      "not of specified type FIXED-POSITION-OBJECT")))


(setf
  (symbol-function 'invalid-position-location-rejected-p)
  (lambda ()
    (position-error-contains-p
      (lambda ()
        (check-proposition
          '(has-position positioned-plate 0)))
      "not of specified type LOCATION")))


(setf
  (symbol-function 'duplicate-position-rejected-p)
  (lambda ()
    (position-error-contains-p
      (lambda ()
        (check-init-duplicate-fluent-keys
          '((has-position positioned-plate shared-site)
            (has-position positioned-plate alternate-site))))
      "Duplicate DEFINE-INIT fluent key")))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query positioned-exactly
    (?object fixed-position-object ?expected location)
  (and
    (has-position ?object ?expected)
    (not
      (exists (?other location)
        (and
          (different ?other ?expected)
          (has-position ?object ?other))))))


(define-query position-scenarios-valid ()
  (and
    ;; Every leaf binds to its exact authored location.  The first two share one
    ;; location without competing for ownership.
    (positioned-exactly positioned-plate shared-site)
    (positioned-exactly positioned-ladder shared-site)
    (positioned-exactly positioned-floor-gears floor-site)
    (positioned-exactly positioned-wall-gears wall-site)
    (positioned-exactly positioned-angled-gears angled-site)

    ;; No default or alternate position is invented for any omitted fixture.
    (not
      (bind
        (has-position unpositioned-plate $unpositioned-plate-location)))
    (not
      (bind
        (has-position unpositioned-ladder $unpositioned-ladder-location)))
    (not
      (bind
        (has-position
          unpositioned-floor-gears $unpositioned-floor-location)))
    (not
      (bind
        (has-position
          unpositioned-wall-gears $unpositioned-wall-location)))
    (not
      (bind
        (has-position
          unpositioned-angled-gears $unpositioned-angled-location)))
    (not (has-position positioned-plate alternate-site))
    (not (has-position positioned-ladder alternate-site))

    ;; Installed metadata verifies the exact ten-object union (excluding locations);
    ;; authoring failures complete the substrate contract.
    (position-schema-valid-p)
    (invalid-position-object-rejected-p)
    (invalid-position-location-rejected-p)
    (duplicate-position-rejected-p)))


(define-goal
  (position-scenarios-valid))

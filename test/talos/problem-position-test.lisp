;;; Filename: problem-position-test.lisp

;;; Dedicated zero-action regression for the shared -position role.  A complete
;;; matrix gives every FIXED-POSITION-OBJECT leaf one positioned fixture and one
;;; fixture with no authored HAS-POSITION fact:
;;;
;;;   plate, ladder, floor-gears, wall-gears, angled-gears, and recorder.
;;;
;;; The positioned plate and ladder deliberately share one location, proving that
;;; fixed placement is functional by object rather than exclusive by location.
;;; The characterization also verifies the static relation schema, exact type
;;; composition, wrong-type rejection, and duplicate fluent-key inconsistency.
;;;
;;; Initial and final dynamic states are empty.  The six authored static positions
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
  location (shared-site floor-site wall-site angled-site recorder-site alternate-site)
  pressure-plate (positioned-plate unpositioned-plate)
  ladder (positioned-ladder unpositioned-ladder)
  floor-gears (positioned-floor-gears unpositioned-floor-gears)
  wall-gears (positioned-wall-gears unpositioned-wall-gears)
  angled-gears (positioned-angled-gears unpositioned-angled-gears)
  floor-blower (positioned-floor-blower unpositioned-floor-blower)
  wall-blower (positioned-wall-blower unpositioned-wall-blower)
  angled-blower (positioned-angled-blower unpositioned-angled-blower)
  recorder (positioned-recorder unpositioned-recorder))


;;;; TECHNOLOGY INCLUDE ;;;;


(include-tech -position)


;;;; INITIALIZATION ;;;;


(define-init
  (has-position positioned-plate shared-site)
  (has-position positioned-ladder shared-site)
  (has-position positioned-floor-gears floor-site)
  (has-position positioned-wall-gears wall-site)
  (has-position positioned-angled-gears angled-site)
  (has-position positioned-floor-blower floor-site)
  (has-position positioned-wall-blower wall-site)
  (has-position positioned-angled-blower angled-site)
  (has-position positioned-recorder recorder-site))


;;;; SCHEMA AND VALIDATION CHARACTERIZATION ;;;;


(define-test-claim position-contract
  (expect-type-components
    'fixed-position-object
    '(pressure-plate toggle-plate ladder
      floor-gears wall-gears angled-gears
      floor-blower wall-blower angled-blower recorder))
  (expect-type-instances
    'fixed-position-object
    '(positioned-plate unpositioned-plate
      positioned-ladder unpositioned-ladder
      positioned-floor-gears unpositioned-floor-gears
      positioned-wall-gears unpositioned-wall-gears
      positioned-angled-gears unpositioned-angled-gears
      positioned-floor-blower unpositioned-floor-blower
      positioned-wall-blower unpositioned-wall-blower
      positioned-angled-blower unpositioned-angled-blower
      positioned-recorder unpositioned-recorder))
  (expect-relation-schema
    'has-position :static '(fixed-position-object location)
    :fluent-indices '(2))
  (expect-condition
    (lambda ()
      (check-proposition '(has-position 0 shared-site)))
    'error
    :containing "not of specified type FIXED-POSITION-OBJECT")
  (expect-condition
    (lambda ()
      (check-proposition '(has-position positioned-plate 0)))
    'error
    :containing "not of specified type LOCATION")
  (expect-condition
    (lambda ()
      (check-init-duplicate-fluent-keys
        '((has-position positioned-plate shared-site)
          (has-position positioned-plate alternate-site))))
    'error
    :containing "Duplicate DEFINE-INIT fluent key"))


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
    (positioned-exactly positioned-recorder recorder-site)

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
    (not
      (bind
        (has-position
          unpositioned-recorder $unpositioned-recorder-location)))
    (not (has-position positioned-plate alternate-site))
    (not (has-position positioned-ladder alternate-site))))


(define-goal
  (position-scenarios-valid))

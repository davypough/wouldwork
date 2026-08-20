;;; Filename: problem-location-coordinates-test.lisp

;;; Dedicated zero-action regression for the shared -location-coordinates role.
;;; Independent fixtures characterize the complete LOCATION-COORDS> contract:
;;;
;;;   1. Zero, negative, and exact fractional coordinates bind without coercion.
;;;   2. Two distinct locations may legitimately occupy the same coordinate pair.
;;;   3. A location with no authored coordinates receives no default position.
;;;   4. The third coordinate is the location's own level.  It may be omitted, defaulting
;;;      to 0 through the registered init-literal default, and an omitted one is stored
;;;      and typed exactly as a written one -- the padded literal has the same arity, so
;;;      the level is type-checked rather than slipping through as a short literal.
;;;   5. The relation remains static, location-keyed, rational-valued, and functional:
;;;      non-location and floating-point arguments are rejected, and a second triple for
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

(setf *expected-min-length* 0)


;;;; TYPES ;;;;


(define-types
  location (origin fractional coincident-a coincident-b unpositioned
            raised sunken))


;;;; TECHNOLOGY INCLUDE ;;;;


(include-tech -location-coordinates)


;;;; INITIALIZATION ;;;;


(define-init
  ;; The third coordinate is optional and defaults to 0.
  (location-coords> origin 0 0)
  (location-coords> fractional -7/3 11/5)
  (location-coords> coincident-a 5/2 -4)
  (location-coords> coincident-b 5/2 -4)
  (location-coords> raised 1 1 3/2)
  (location-coords> sunken 2 2 -1))


;;;; SCHEMA AND VALIDATION CHARACTERIZATION ;;;;


(define-test-claim location-coordinates-contract
  (expect-relation-schema
    'location-coords> :static '(location rational rational rational)
    :fluent-indices '(2 3 4))

  ;; The level defaults to 0, and padding runs before anything else sees the literal.
  (equal (gethash 'location-coords> *init-literal-defaults*) '(0))
  (equal (pad-init-literal '(location-coords> origin 0 0))
         '(location-coords> origin 0 0 0))
  (equal (pad-init-literal '(location-coords> raised 1 1 3/2))
         '(location-coords> raised 1 1 3/2))

  (expect-condition
    (lambda ()
      (check-proposition '(location-coords> 0 0 0 0)))
    'error
    :containing "not of specified type LOCATION")
  (expect-condition
    (lambda ()
      (check-proposition '(location-coords> origin 1.0 0 0)))
    'error
    :containing "not of specified type RATIONAL")

  ;; An omitted level is type-checked exactly as a written one, because the literal is
  ;; padded to full arity first.  Without padding the short form would be read as a
  ;; fluentless lookup key and none of its arguments would be checked at all.
  (expect-condition
    (lambda ()
      (check-proposition (pad-init-literal '(location-coords> origin 1.0 0))))
    'error
    :containing "not of specified type RATIONAL")
  (expect-condition
    (lambda ()
      (check-proposition (pad-init-literal '(location-coords> origin 0 0 1.0))))
    'error
    :containing "not of specified type RATIONAL")

  (expect-condition
    (lambda ()
      (check-init-duplicate-fluent-keys
        '((location-coords> origin 0 0 0)
          (location-coords> origin 1 1 1))))
    'error
    :containing "Duplicate DEFINE-INIT fluent key")

  ;; A location may name its level once, in the coordinates or through HAS-ELEVATION,
  ;; but a disagreement between the two is refused rather than silently resolved.
  (expect-condition
    (lambda ()
      (check-init-location-level-agreement
        '((location-coords> raised 1 1 3/2)
          (has-elevation raised 2))))
    'error
    :containing "is given two different levels"))


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

      ;; An omitted level reads back as the exact default of zero, never as NIL.
      (bind (location-coords> origin $origin-x2 $origin-y2 $origin-z))
      (= $origin-z 0)
      (bind (location-coords> raised $raised-x $raised-y $raised-z))
      (= $raised-x 1)
      (= $raised-y 1)
      (= $raised-z 3/2)
      (bind (location-coords> sunken $sunken-x $sunken-y $sunken-z))
      (= $sunken-z -1)

      ;; The substrate never invents a default or an alternate coordinate.
      (not
        (bind
          (location-coords>
            unpositioned $unpositioned-x $unpositioned-y))))))


(define-goal
  (location-coordinates-scenarios-valid))

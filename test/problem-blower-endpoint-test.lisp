;;; Filename: problem-blower-endpoint-test.lisp

;;; Shared blower-drive endpoint validation.  One instance of each removable-fan gears
;;; type and each fixed blower type proves that every floor, wall, and angled drive must
;;; name distinct HAS-POSITION source and AIMED-AT destination locations.  Multiple values
;;; remain the relation fluency layer's responsibility.  Coordinate-known wall streams are
;;; also horizontal and axis-aligned, while coordinate-known angled streams must have some
;;; horizontal displacement.

(in-package :ww)

(ww-set *problem-name* blower-endpoint-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 0)

(setf *expected-min-length* 0)


;;;; TYPES ;;;;


(define-types
  agent (agent1)
  location (floor-gears-source floor-gears-destination
            wall-gears-source wall-gears-destination
            angled-gears-source angled-gears-destination
            floor-blower-source floor-blower-destination
            wall-blower-source wall-blower-destination
            angled-blower-source angled-blower-destination)
  floor-gears (floor-gears1)
  wall-gears (wall-gears1)
  angled-gears (angled-gears1)
  floor-blower (floor-blower1)
  wall-blower (wall-blower1)
  angled-blower (angled-blower1))


;;;; TECHNOLOGY INCLUDES ;;;;


(include-tech floor-gears)
(include-tech floor-blower)
(include-tech wall-blower)
(include-tech angled-blower)


;;;; INITIALIZATION ;;;;


(define-init
  (has-location agent1 floor-gears-source)
  (has-position floor-gears1 floor-gears-source)
  (aimed-at floor-gears1 floor-gears-destination)
  (has-position wall-gears1 wall-gears-source)
  (aimed-at wall-gears1 wall-gears-destination)
  (has-position angled-gears1 angled-gears-source)
  (aimed-at angled-gears1 angled-gears-destination)
  (has-position floor-blower1 floor-blower-source)
  (aimed-at floor-blower1 floor-blower-destination)
  (has-position wall-blower1 wall-blower-source)
  (aimed-at wall-blower1 wall-blower-destination)
  (has-position angled-blower1 angled-blower-source)
  (aimed-at angled-blower1 angled-blower-destination))


;;;; INITIALIZATION VALIDATION ;;;;


(define-test-helper blower-drive-complete-endpoints ()
  '((has-position floor-gears1 floor-gears-source)
    (aimed-at floor-gears1 floor-gears-destination)
    (has-position wall-gears1 wall-gears-source)
    (aimed-at wall-gears1 wall-gears-destination)
    (has-position angled-gears1 angled-gears-source)
    (aimed-at angled-gears1 angled-gears-destination)
    (has-position floor-blower1 floor-blower-source)
    (aimed-at floor-blower1 floor-blower-destination)
    (has-position wall-blower1 wall-blower-source)
    (aimed-at wall-blower1 wall-blower-destination)
    (has-position angled-blower1 angled-blower-source)
    (aimed-at angled-blower1 angled-blower-destination)))


(define-test-helper blower-drive-missing-endpoints-rejected-p ()
  (dolist (literal (blower-drive-complete-endpoints))
    (let ((relation (first literal)))
      (expect-condition
        (lambda ()
          (validate-init-literals
            (remove literal (blower-drive-complete-endpoints) :test #'equal)
            :checks '(gears-fan-init-check)))
        'init-check-failure
        :containing (case relation
                      (has-position "has no HAS-POSITION source")
                      (aimed-at "has no AIMED-AT destination"))
        :check 'gears-fan-init-check)))
  t)


(define-test-helper blower-drive-self-aims-rejected-p ()
  (dolist (source-literal
            (remove-if-not
              (lambda (literal) (eql (first literal) 'has-position))
              (blower-drive-complete-endpoints)))
    (let* ((drive (second source-literal))
           (source (third source-literal))
           (destination-literal
             (find-if
               (lambda (literal)
                 (and (eql (first literal) 'aimed-at)
                      (eql (second literal) drive)))
               (blower-drive-complete-endpoints))))
      (expect-condition
        (lambda ()
          (validate-init-literals
            (substitute `(aimed-at ,drive ,source)
                        destination-literal
                        (blower-drive-complete-endpoints)
                        :test #'equal)
            :checks '(gears-fan-init-check)))
        'init-check-failure
        :containing "same source and destination location"
        :check 'gears-fan-init-check)))
  t)


(define-test-helper blower-drive-coordinate-geometry ()
  (append
    (blower-drive-complete-endpoints)
    '((has-elevation wall-gears1 3)
      (location-coords> wall-gears-source 0 0 2)
      (location-coords> wall-gears-destination 4 0 2)
      (location-coords> angled-gears-source 10 10 1)
      (location-coords> angled-gears-destination 13 12 4))))


(define-test-claim blower-drive-endpoint-validation
  (null
    (validate-init-literals
      (blower-drive-complete-endpoints)
      :checks '(gears-fan-init-check)))
  (blower-drive-missing-endpoints-rejected-p)
  (blower-drive-self-aims-rejected-p))


(define-test-claim wall-stream-geometry-validation
  (null
    (validate-init-literals
      (blower-drive-coordinate-geometry)
      :checks '(stream-passability-init-check)))
  (expect-condition
    (lambda ()
      (validate-init-literals
        (substitute '(location-coords> wall-gears-destination 4 0 3)
                    '(location-coords> wall-gears-destination 4 0 2)
                    (blower-drive-coordinate-geometry)
                    :test #'equal)
        :checks '(stream-passability-init-check)))
    'init-check-failure
    :containing "connects locations at different levels"
    :check 'stream-passability-init-check)
  (expect-condition
    (lambda ()
      (validate-init-literals
        (substitute '(location-coords> wall-gears-destination 4 1 2)
                    '(location-coords> wall-gears-destination 4 0 2)
                    (blower-drive-coordinate-geometry)
                    :test #'equal)
        :checks '(stream-passability-init-check)))
    'init-check-failure
    :containing "is not axis-aligned"
    :check 'stream-passability-init-check)
  (expect-condition
    (lambda ()
      (validate-init-literals
        (substitute '(location-coords> wall-gears-destination 0 0 2)
                    '(location-coords> wall-gears-destination 4 0 2)
                    (blower-drive-coordinate-geometry)
                    :test #'equal)
        :checks '(stream-passability-init-check)))
    'init-check-failure
    :containing "coincident swept location and destination"
    :check 'stream-passability-init-check)
  (expect-condition
    (lambda ()
      (validate-init-literals
        (remove '(has-elevation wall-gears1 3)
                (blower-drive-coordinate-geometry)
                :test #'equal)
        :checks '(stream-passability-init-check)))
    'init-check-failure
    :containing "is not above its source floor"
    :check 'stream-passability-init-check))


(define-test-claim angled-stream-geometry-validation
  (null
    (validate-init-literals
      (blower-drive-coordinate-geometry)
      :checks '(angled-blower-init-check)))
  (expect-condition
    (lambda ()
      (validate-init-literals
        (substitute '(location-coords> angled-gears-destination 10 10 4)
                    '(location-coords> angled-gears-destination 13 12 4)
                    (blower-drive-coordinate-geometry)
                    :test #'equal)
        :checks '(angled-blower-init-check)))
    'init-check-failure
    :containing "has no horizontal displacement"
    :check 'angled-blower-init-check))


;;;; GOAL ;;;;


(define-query blower-drive-endpoints-valid ()
  (and (has-position floor-gears1 floor-gears-source)
       (aimed-at floor-gears1 floor-gears-destination)
       (has-position wall-gears1 wall-gears-source)
       (aimed-at wall-gears1 wall-gears-destination)
       (has-position angled-gears1 angled-gears-source)
       (aimed-at angled-gears1 angled-gears-destination)
       (has-position floor-blower1 floor-blower-source)
       (aimed-at floor-blower1 floor-blower-destination)
       (has-position wall-blower1 wall-blower-source)
       (aimed-at wall-blower1 wall-blower-destination)
       (has-position angled-blower1 angled-blower-source)
       (aimed-at angled-blower1 angled-blower-destination)))


(define-goal
  (blower-drive-endpoints-valid))

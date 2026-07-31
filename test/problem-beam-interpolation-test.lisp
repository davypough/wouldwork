;;; Filename: problem-beam-interpolation-test.lisp

;;; Dedicated zero-action regression for the shared -beam-interpolation default
;;; hook.  Representative BEAM-NODE endpoint shapes verify that a horizontal
;;; beam returns its shared elevation exactly, without coordinates or corridor
;;; facts.  Values include zero, a negative level, and an exact rational.
;;;
;;; A test-local condition helper invokes the installed hook with unequal
;;; endpoint elevations and requires its explicit visibility-coordinate error.
;;; This distinguishes the private default from visibility's successful sloped
;;; override without installing that override and erasing the branch under test.
;;; Initial and final states are empty.  Expected minimum path length: zero.

(in-package :ww)


(ww-set *problem-name* beam-interpolation-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 1)


;;;; TYPES ;;;;


(define-types
  location (sample-location source-location destination-location)
  transmitter (sample-transmitter)
  receiver (sample-receiver)
  floor-repeater (sample-floor-repeater)
  wall-repeater (sample-wall-repeater))


;;;; TECHNOLOGY INCLUDE ;;;;


(include-tech -beam-interpolation)


;;;; INITIALIZATION ;;;;


(define-init
  ;; DEFINE-INIT requires a literal.  This explicit absence leaves the dynamic
  ;; start state empty and contributes no interpolation input.
  (not (active sample-receiver)))


;;;; CONDITION CHARACTERIZATION ;;;;


(setf
  (symbol-function 'default-sloped-beam-rejected-p)
  (lambda (state)
    (let ((condition
            (handler-case
                (progn
                  (funcall
                    (symbol-function 'beam-elevation-at-location)
                    state
                    'sample-location
                    'sample-transmitter
                    1
                    'sample-receiver
                    2)
                  nil)
              (error (error-condition)
                error-condition))))
      (and condition
           (not
             (null
               (search
                 "A sloped fixed beam requires visibility's coordinate interpolation."
                 (princ-to-string condition))))))))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query beam-interpolation-scenarios-valid ()
  (and
    ;; Fixed apparatus endpoints, including both repeater orientations.
    (= (beam-elevation-at-location
         sample-location sample-transmitter 0 sample-receiver 0)
       0)
    (= (beam-elevation-at-location
         sample-location
         sample-transmitter
         5/2
         sample-floor-repeater
         5/2)
       5/2)
    (= (beam-elevation-at-location
         sample-location
         sample-wall-repeater
         -3
         sample-receiver
         -3)
       -3)

    ;; Visibility's location-to-location endpoint shape uses the same default
    ;; whenever the two live endpoint elevations are equal.
    (= (beam-elevation-at-location
         sample-location source-location 7 destination-location 7)
       7)

    ;; No beam state supplies or accidentally substitutes these results.
    (not (active sample-receiver))
    (not (coupled sample-transmitter sample-receiver))
    (not (bind
           (beam-via
             sample-transmitter $unexpected-obstacles sample-receiver)))

    ;; Unequal elevations must never be silently treated as horizontal.
    (default-sloped-beam-rejected-p state)))


(define-goal
  (beam-interpolation-scenarios-valid))

;;; Filename: problem-apparatus-coordinates-test.lisp

;;; Dedicated zero-action regression for APPARATUS-COORDS> and its role in the canonical
;;; vertical model.  Independent fixtures characterize:
;;;
;;;   1. The optional third coordinate defaults to the wall-mounting level 1 and retains
;;;      exact rational values when written explicitly.
;;;   2. An apparatus with no coordinate fact receives no invented XY position, while
;;;      FIXED-BASE still uses its per-type default.
;;;   3. A non-floor fixture naming both APPARATUS-COORDS> and HAS-ELEVATION must give the
;;;      same level; disagreement is rejected rather than silently resolved.
;;;   4. A floor repeater is the intentional exception: APPARATUS-COORDS> supplies only
;;;      its XY functional point, while HAS-ELEVATION supplies its floor base.  Its padded
;;;      coordinate level is therefore ignored by FIXED-BASE.
;;;   5. Wall repeaters use the coordinate level as their base and retain their horizontal
;;;      axis rule, so their descriptive height does not raise TOP.
;;;
;;; Initial and final states are identical.  Expected minimum path length: zero.

(in-package :ww)


(ww-set *problem-name* apparatus-coordinates-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 0)


;;;; TYPES ;;;;


(define-types
  location (unused-site)
  transmitter (default-transmitter explicit-transmitter matching-transmitter
               unpositioned-transmitter)
  receiver (default-receiver)
  gun (explicit-gun)
  floor-repeater (floor-repeater-probe)
  wall-repeater (wall-repeater-probe))


;;;; TECHNOLOGY INCLUDE ;;;;


(include-tech -vertical)


;;;; INITIALIZATION ;;;;


(define-init
  (apparatus-coords> default-transmitter 0 0)
  (apparatus-coords> explicit-transmitter 2 -3 5/2)
  (apparatus-coords> matching-transmitter 4 4 7/2)
  (has-elevation matching-transmitter 7/2)
  (apparatus-coords> default-receiver 6 1)
  (apparatus-coords> explicit-gun -1 -2 -3/2)

  ;; The coordinate level pads to 1 but is deliberately not the floor repeater's base.
  (apparatus-coords> floor-repeater-probe 8 0)
  (has-elevation floor-repeater-probe 3)
  (has-height floor-repeater-probe 2)

  (apparatus-coords> wall-repeater-probe 10 0 4)
  (has-elevation wall-repeater-probe 4))


;;;; SCHEMA AND VALIDATION CHARACTERIZATION ;;;;


(define-test-claim apparatus-coordinates-contract
  (equal (gethash 'apparatus-coords> *init-literal-defaults*) '(1))
  (equal (pad-init-literal '(apparatus-coords> default-transmitter 0 0))
         '(apparatus-coords> default-transmitter 0 0 1))
  (equal (pad-init-literal '(apparatus-coords> explicit-transmitter 2 -3 5/2))
         '(apparatus-coords> explicit-transmitter 2 -3 5/2))

  (expect-condition
    (lambda ()
      (check-proposition
        (pad-init-literal '(apparatus-coords> default-transmitter 1.0 0))))
    'error
    :containing "not of specified type RATIONAL")

  (expect-condition
    (lambda ()
      (check-init-duplicate-fluent-keys
        '((apparatus-coords> default-transmitter 0 0 1)
          (apparatus-coords> default-transmitter 1 1 1))))
    'error
    :containing "Duplicate DEFINE-INIT fluent key")

  ;; Matching declarations are accepted; a disagreement on an ordinary fixture is not.
  (not (check-init-apparatus-level-agreement
         '((apparatus-coords> matching-transmitter 4 4 7/2)
           (has-elevation matching-transmitter 7/2))))
  (expect-condition
    (lambda ()
      (check-init-apparatus-level-agreement
        '((apparatus-coords> matching-transmitter 4 4 7/2)
          (has-elevation matching-transmitter 9/2))))
    'error
    :containing "is given two different levels")

  ;; A floor repeater's coordinate level and base are intentionally different quantities.
  (not (check-init-apparatus-level-agreement
         '((apparatus-coords> floor-repeater-probe 8 0 1)
           (has-elevation floor-repeater-probe 3)))))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query apparatus-coordinates-scenarios-valid ()
  (and
    ;; Written and defaulted coordinate values remain exact.
    (bind (apparatus-coords>
            default-transmitter $default-x $default-y $default-z))
    (= $default-x 0)
    (= $default-y 0)
    (= $default-z 1)
    (bind (apparatus-coords>
            explicit-transmitter $explicit-x $explicit-y $explicit-z))
    (= $explicit-x 2)
    (= $explicit-y -3)
    (= $explicit-z 5/2)
    (bind (apparatus-coords> explicit-gun $gun-x $gun-y $gun-z))
    (= $gun-z -3/2)

    ;; Ordinary apparatus takes its base from coordinates; an unpositioned fixture uses
    ;; the type table's mounting default but receives no invented XY fact.
    (= (base default-transmitter) 1)
    (= (top default-transmitter) 1)
    (= (base explicit-transmitter) 5/2)
    (= (top explicit-transmitter) 5/2)
    (= (base matching-transmitter) 7/2)
    (= (base default-receiver) 1)
    (= (base explicit-gun) -3/2)
    (= (base unpositioned-transmitter) 1)
    (not (bind (apparatus-coords>
                 unpositioned-transmitter $unpositioned-x $unpositioned-y)))

    ;; Floor repeaters ignore the coordinate level and stand at HAS-ELEVATION; wall
    ;; repeaters use that coordinate level but project their height horizontally.
    (= (base floor-repeater-probe) 3)
    (= (object-height floor-repeater-probe) 2)
    (= (top floor-repeater-probe) 5)
    (= (base wall-repeater-probe) 4)
    (= (object-height wall-repeater-probe) 1)
    (= (top wall-repeater-probe) 4)))


(define-goal
  (apparatus-coordinates-scenarios-valid))

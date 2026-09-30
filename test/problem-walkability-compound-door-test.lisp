;;; Filename: problem-walkability-compound-door-test.lisp
;;;
;;; Coordinate-derived walking through co-located gates and screens.  Two full-height
;;; partitions each contain one doorway occupied by both fixtures.  The first gate is
;;; open: an empty-handed agent crosses, while an agent holding cargo cannot pass its
;;; screen.  The second gate is closed: its screen alone cannot permit the empty-handed
;;; agent through.  Exact traversal families prove each interval contributes one
;;; conjunctive clause rather than two alternative doors.
;;;
;;; A direct classifier probe retains the separate rule that a directional stream
;;; curtain cannot share an interval with an ordinary hard door.
;;;
;;; Expected minimum path length: 0.

(in-package :ww)

(ww-set *problem-name* walkability-compound-door-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 0)


(define-types
  agent (empty-agent holding-agent)
  location (left-site middle-site right-site)
  gate (gate-open gate-closed)
  screen (screen-open screen-closed)
  connector (carried-connector)
  wall (left-lower left-upper right-lower right-upper))


(include-tech walkability)


(define-init
  (has-location empty-agent left-site)
  (has-location holding-agent left-site)
  (holding holding-agent carried-connector)
  (open gate-open)

  (boundary-wall
    ((0 0) (12 0) (12 6) (0 6) (0 0)))

  (wall-segment> left-lower 4 0 4 2)
  (wall-segment> left-upper 4 4 4 6)
  (wall-segment> right-lower 8 0 8 2)
  (wall-segment> right-upper 8 4 8 6)

  (gate-segment> gate-open 4 2 4 4)
  (screen-segment> screen-open 4 2 4 4)
  (gate-segment> gate-closed 8 2 8 4)
  (screen-segment> screen-closed 8 2 8 4)

  (location-coords> left-site 2 3)
  (location-coords> middle-site 6 3)
  (location-coords> right-site 10 3))


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!)))


(define-test-helper compound-door-stream-overlap-rejected-p ()
  "A stream curtain keeps its unique directional interval instead of becoming a clause."
  (let ((coverage (make-hash-table :test #'equal)))
    (setf (gethash '(:v 1 1) coverage)
          '((:gate gate-open) (:stream stream-drive)))
    (expect-condition
      (lambda ()
        (walkability-coordinates-classify-coverage coverage))
      'error
      :containing "share an interval with hard door")))


(define-test-claim compound-door-classification-contract
  (compound-door-stream-overlap-rejected-p))


(define-query compound-walking-family-is (?from location ?to location ?expected)
  (do (bind (traverse-via ?from $actual ?to))
      (equal $actual ?expected)))


(define-query compound-door-scenarios-valid ()
  (and
    (compound-walking-family-is
      left-site middle-site '((gate-open screen-open)))
    (compound-walking-family-is
      middle-site right-site '((gate-closed screen-closed)))
    (compound-walking-family-is
      left-site right-site
      '((gate-closed gate-open screen-closed screen-open)))

    ;; Both fixtures are required.  An open gate does not excuse held cargo from its
    ;; screen, and an empty-handed actor does not excuse a closed gate.
    (one-step-walkable empty-agent left-site middle-site)
    (not (one-step-walkable holding-agent left-site middle-site))
    (not (one-step-walkable empty-agent middle-site right-site))
    (traversable empty-agent left-site middle-site)
    (not (traversable holding-agent left-site middle-site))
    (not (traversable empty-agent left-site right-site))))


(define-goal
  (compound-door-scenarios-valid))

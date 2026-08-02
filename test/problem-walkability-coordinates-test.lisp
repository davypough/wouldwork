;;; Filename: problem-walkability-coordinates-test.lisp
;;;
;;; Dedicated regression coverage for coordinate-derived walking topology.
;;;
;;; A rectangular boundary contains three zones separated by full-height partitions.
;;; The first partition has two alternative gates; the second has a gate or screen.
;;; Exact WALK-VIA families characterize same-zone connectivity, each partition, and
;;; the four canonical two-obstacle routes across both partitions.  The partitions
;;; terminate exactly on the boundary, so any endpoint leak changes those families.
;;;
;;; A room sealed by three walls and one window must remain disconnected.  Two further
;;; locations exercise valid placement exactly on an uncovered induced grid line and
;;; at an unambiguous induced grid vertex.  With only GATE-A open, an empty-handed
;;; agent crosses the second partition through SCREEN-A, while a holding agent cannot.
;;; A direct geometry probe confirms that the rectangular-cell derivation preserves
;;; the shared BOUNDARY-WALL axis-alignment invariant internally.
;;;
;;; The goal directly characterizes initialization-derived state; no action is needed.
;;; Expected minimum path length: 0.

(in-package :ww)

(ww-set *problem-name* walkability-coordinates-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 0)


;;;; TYPES ;;;;


(define-types
  agent (main-agent holding-agent)
  location (left-start left-peer middle
            right-goal right-line right-vertex sealed-site)
  gate (gate-a gate-b gate-c)
  screen (screen-a)
  connector (carried-connector)
  wall (first-lower first-middle first-upper
        second-lower second-middle second-upper
        island-left island-right island-top)
  window (sealed-window))


;;;; TECHNOLOGY INCLUDE ;;;;


(include-tech walkability)


;;;; INITIALIZATION ;;;;


(define-init
  (has-location main-agent left-start)
  (has-location holding-agent left-start)
  (holding holding-agent carried-connector)

  ;; Only the first partition has a passable gate.  MAIN-AGENT crosses the
  ;; second partition through its screen; HOLDING-AGENT cannot.
  (open gate-a)

  ;; The final point closes the rectangle back to (0 0).
  (boundary-wall
    ((0 0) (12 0) (12 8) (0 8) (0 0)))

  ;; Each vertical partition touches both boundary edges exactly.  Door segments
  ;; fill every deliberate gap between its solid pieces.
  (wall-segments
    ((first-lower 4 0 4 1)
     (first-middle 4 2 4 5)
     (first-upper 4 6 4 8)
     (second-lower 8 0 8 1)
     (second-middle 8 2 8 5)
     (second-upper 8 6 8 8)
     (island-left 9 5 9 7)
     (island-right 11 5 11 7)
     (island-top 9 7 11 7)))

  (gate-segments
    ((gate-a 4 1 4 2)
     (gate-b 4 5 4 6)
     (gate-c 8 1 8 2)))

  (screen-segments
    ((screen-a 8 5 8 6)))

  ;; A window is a walking solid.  Together with the island walls, this closes
  ;; SEALED-SITE into a zone with no door edge.
  (window-segments
    ((sealed-window 9 5 11 5)))

  ;; RIGHT-LINE lies exactly on the uncovered y=2 grid line.  RIGHT-VERTEX lies
  ;; exactly at the uncovered (9,2) grid vertex induced by unrelated segments.
  (location-coords> left-start 2 3)
  (location-coords> left-peer 2 4)
  (location-coords> middle 6 3)
  (location-coords> right-goal 10 3)
  (location-coords> right-line 10 2)
  (location-coords> right-vertex 9 2)
  (location-coords> sealed-site 10 13/2))


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!)))


;;;; CONSUMER-SPECIFIC BOUNDARY VALIDATION ;;;;


(setf
  (symbol-function 'walkability-diagonal-boundary-rejected-p)
  (lambda ()
    (let ((condition
            (handler-case
              (progn
                (walkability-coordinates-boundary-segments
                  '((0 0) (2 0) (1 1) (0 0)))
                nil)
              (error (error-condition)
                error-condition))))
      (and condition
           (search "not axis-aligned"
                   (princ-to-string condition))))))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query coordinate-walk-via-family-is
    (?from location ?to location ?expected)
  (do (bind (walk-via ?from $actual ?to))
      (equal $actual ?expected)))


(define-query walkability-coordinate-scenarios-valid ()
  (and
    ;; This is a zero-action characterization of initialized state.
    (has-location main-agent left-start)
    (has-location holding-agent left-start)
    (holding holding-agent carried-connector)
    (not (has-location main-agent right-goal))
    (not (has-location holding-agent right-goal))
    (open gate-a)
    (not (open gate-b))
    (not (open gate-c))

    ;; Same-zone, uncovered-grid-line, and unambiguous-grid-vertex pairs are
    ;; direct and unguarded.
    (coordinate-walk-via-family-is left-start left-peer nil)
    (coordinate-walk-via-family-is right-goal right-line nil)
    (coordinate-walk-via-family-is right-goal right-vertex nil)

    ;; Each partition retains every minimal single-door alternative.
    (coordinate-walk-via-family-is
      left-start middle
      '((gate-a) (gate-b)))
    (coordinate-walk-via-family-is
      middle left-start
      '((gate-a) (gate-b)))
    (coordinate-walk-via-family-is
      middle right-goal
      '((gate-c) (screen-a)))
    (coordinate-walk-via-family-is
      right-goal middle
      '((gate-c) (screen-a)))

    ;; Crossing both partitions takes one alternative from each.  Clauses and
    ;; their members must be canonical and deterministic.
    (coordinate-walk-via-family-is
      left-start right-goal
      '((gate-a gate-c)
        (gate-a screen-a)
        (gate-b gate-c)
        (gate-b screen-a)))
    (coordinate-walk-via-family-is
      right-goal left-start
      '((gate-a gate-c)
        (gate-a screen-a)
        (gate-b gate-c)
        (gate-b screen-a)))

    ;; Ordinary segment geometry is symmetric and emits no directional facts.
    (not (exists (?from location ?to location)
           (bind (walk-via> ?from $directional-family ?to))))

    ;; The wall/window island has no derived edge to any other location.
    (not (exists (?other location)
           (bind (walk-via sealed-site $sealed-family ?other))))
    (not (exists (?other location)
           (bind (walk-via> sealed-site $sealed-directional-family ?other))))

    ;; MAIN-AGENT passes GATE-A and then the screen, reaching every non-sealed
    ;; location.  No extra location may leak into its exact six-location closure.
    (= (length (walkable-locations main-agent left-start)) 6)
    (member 'left-start
            (walkable-locations main-agent left-start))
    (member 'left-peer
            (walkable-locations main-agent left-start))
    (member 'middle
            (walkable-locations main-agent left-start))
    (member 'right-goal
            (walkable-locations main-agent left-start))
    (member 'right-line
            (walkable-locations main-agent left-start))
    (member 'right-vertex
            (walkable-locations main-agent left-start))
    (not (member 'sealed-site
                 (walkable-locations main-agent left-start)))
    (one-step-walkable main-agent left-start right-goal)
    (walkable main-agent left-start right-goal)
    (not (walkable main-agent left-start sealed-site))

    ;; HOLDING-AGENT passes open GATE-A but neither closed GATE-C nor SCREEN-A.
    ;; Its closure therefore stops after exactly the middle zone.
    (= (length (walkable-locations holding-agent left-start)) 3)
    (member 'left-start
            (walkable-locations holding-agent left-start))
    (member 'left-peer
            (walkable-locations holding-agent left-start))
    (member 'middle
            (walkable-locations holding-agent left-start))
    (not (member 'right-goal
                 (walkable-locations holding-agent left-start)))
    (one-step-walkable holding-agent left-start middle)
    (not (one-step-walkable holding-agent middle right-goal))
    (not (walkable holding-agent left-start right-goal))
    (not (walkable holding-agent left-start sealed-site))

    ;; The public declaration validator rejects this triangle first; this direct probe
    ;; confirms that the rectangular consumer also preserves its own invariant.
    (funcall (symbol-function 'walkability-diagonal-boundary-rejected-p))))


(define-goal
  (walkability-coordinate-scenarios-valid))

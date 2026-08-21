;;; Filename: problem-walkability-coordinates-test.lisp
;;;
;;; Dedicated regression coverage for coordinate-derived walking topology.
;;;
;;; A rectangular boundary contains three zones separated by full-height partitions.
;;; The first partition has two alternative gates; the second has a gate or screen.
;;; Exact walking TRAVERSAL-VIA families characterize same-zone connectivity, each
;;; partition, and the four canonical two-obstacle routes across both partitions.  The
;;; partitions terminate exactly on the boundary, so any endpoint leak changes those
;;; families.
;;;
;;; A room sealed by two walls, one edge, and one window must remain disconnected --
;;; proving EDGE-SEGMENT> seals a zone exactly like WALL-SEGMENT> does.  Two further
;;; locations exercise valid placement exactly on an uncovered induced grid line and
;;; at an unambiguous induced grid vertex.  A loft shares LEFT-START's x/y coordinates
;;; but remains a distinct, non-walkable location because its elevation is five.  With
;;; only GATE-A open, an empty-handed agent crosses the second partition through SCREEN-A,
;;; while a holding agent cannot.
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
  location (left-start left-loft left-peer middle
            right-goal right-line right-vertex sealed-site)
  gate (gate-a gate-b gate-c)
  screen (screen-a)
  connector (carried-connector)
  wall (first-lower first-middle first-upper
        second-lower second-middle second-upper
        island-left island-right)
  edge (island-top)
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
  (wall-segment> first-lower 4 0 4 1)
  (wall-segment> first-middle 4 2 4 5)
  (wall-segment> first-upper 4 6 4 8)
  (wall-segment> second-lower 8 0 8 1)
  (wall-segment> second-middle 8 2 8 5)
  (wall-segment> second-upper 8 6 8 8)
  (wall-segment> island-left 9 5 9 7)
  (wall-segment> island-right 11 5 11 7)
  (edge-segment> island-top 9 7 11 7)  ;an edge seals a zone exactly like a wall

  (gate-segment> gate-a 4 1 4 2)
  (gate-segment> gate-b 4 5 4 6)
  (gate-segment> gate-c 8 1 8 2)

  (screen-segment> screen-a 8 5 8 6)

  ;; A window is a walking solid.  Together with the island walls and edge, this closes
  ;; SEALED-SITE into a zone with no door edge.
  (window-segment> sealed-window 9 5 11 5)

  ;; RIGHT-LINE lies exactly on the uncovered y=2 grid line.  RIGHT-VERTEX lies
  ;; exactly at the uncovered (9,2) grid vertex induced by unrelated segments.  LEFT-LOFT
  ;; deliberately shares LEFT-START's horizontal point while remaining five units above it.
  (location-coords> left-start 2 3)
  (location-coords> left-loft 2 3 5)
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


(define-test-claim individual-segment-schema
  (expect-relation-schema
    'wall-segment> :static '(wall rational rational rational rational)
    :fluent-indices '(2 3 4 5))
  (expect-relation-schema
    'edge-segment> :static '(edge rational rational rational rational)
    :fluent-indices '(2 3 4 5))
  (expect-relation-schema
    'gate-segment> :static '(gate rational rational rational rational)
    :fluent-indices '(2 3 4 5))
  (expect-relation-schema
    'window-segment> :static '(window rational rational rational rational)
    :fluent-indices '(2 3 4 5))
  (expect-relation-schema
    'screen-segment> :static '(screen rational rational rational rational)
    :fluent-indices '(2 3 4 5))
  (expect-relation-absent 'wall-segments)
  (expect-relation-absent 'edge-segments)
  (expect-relation-absent 'gate-segments)
  (expect-relation-absent 'window-segments)
  (expect-relation-absent 'screen-segments))


(define-test-claim walkability-diagonal-boundary-rejected
  (expect-condition
    (lambda ()
      (walkability-coordinates-boundary-segments
        '((0 0) (2 0) (1 1) (0 0))))
    'error
    :containing "not axis-aligned"))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query coordinate-walking-family-is
    (?from location ?to location ?expected)
  (do (bind (traversal-via walking ?from $actual ?to))
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
    (coordinate-walking-family-is left-start left-peer nil)
    (coordinate-walking-family-is right-goal right-line nil)
    (coordinate-walking-family-is right-goal right-vertex nil)

    ;; Geometry deliberately ignores z, so coincident LEFT-START/LEFT-LOFT receives the
    ;; same raw symmetric edge as any same-zone pair.  Walking then compares the two
    ;; independently authored levels and refuses that candidate in both directions.
    (coordinate-walking-family-is left-start left-loft nil)
    (= (location-elevation left-start) 0)
    (= (location-elevation left-loft) 5)
    (not (one-step-walkable main-agent left-start left-loft))
    (not (one-step-walkable main-agent left-loft left-start))
    (not (traversable main-agent left-start left-loft))

    ;; Each partition retains every minimal single-door alternative.
    (coordinate-walking-family-is
      left-start middle
      '((gate-a) (gate-b)))
    (coordinate-walking-family-is
      middle left-start
      '((gate-a) (gate-b)))
    (coordinate-walking-family-is
      middle right-goal
      '((gate-c) (screen-a)))
    (coordinate-walking-family-is
      right-goal middle
      '((gate-c) (screen-a)))

    ;; Crossing both partitions takes one alternative from each.  Clauses and
    ;; their members must be canonical and deterministic.
    (coordinate-walking-family-is
      left-start right-goal
      '((gate-a gate-c)
        (gate-a screen-a)
        (gate-b gate-c)
        (gate-b screen-a)))
    (coordinate-walking-family-is
      right-goal left-start
      '((gate-a gate-c)
        (gate-a screen-a)
        (gate-b gate-c)
        (gate-b screen-a)))

    ;; Ordinary segment geometry is symmetric and emits no directional facts.
    (not (exists (?from location ?to location)
           (bind (traversal-via> walking ?from $directional-family ?to))))

    ;; The wall/edge/window island has no derived edge to any other location.
    (not (exists (?other location)
           (bind (traversal-via walking sealed-site $sealed-family ?other))))
    (not (exists (?other location)
           (bind (traversal-via> walking sealed-site $sealed-directional-family ?other))))

    ;; MAIN-AGENT passes GATE-A and then the screen, reaching every non-sealed
    ;; location.  No extra location may leak into its exact six-location closure.
    (= (length (mobility-locations main-agent left-start)) 6)
    (member 'left-start
            (mobility-locations main-agent left-start))
    (member 'left-peer
            (mobility-locations main-agent left-start))
    (member 'middle
            (mobility-locations main-agent left-start))
    (member 'right-goal
            (mobility-locations main-agent left-start))
    (member 'right-line
            (mobility-locations main-agent left-start))
    (member 'right-vertex
            (mobility-locations main-agent left-start))
    (not (member 'sealed-site
                 (mobility-locations main-agent left-start)))
    (not (member 'left-loft
                 (mobility-locations main-agent left-start)))
    (one-step-walkable main-agent left-start right-goal)
    (traversable main-agent left-start right-goal)
    (not (traversable main-agent left-start sealed-site))
    (equal
      (second
        (assoc 'right-goal
               (mobility-results main-agent left-start)))
      '((walk left-start (gate-a screen-a) right-goal)))

    ;; HOLDING-AGENT passes open GATE-A but neither closed GATE-C nor SCREEN-A.
    ;; Its closure therefore stops after exactly the middle zone.
    (= (length (mobility-locations holding-agent left-start)) 3)
    (member 'left-start
            (mobility-locations holding-agent left-start))
    (member 'left-peer
            (mobility-locations holding-agent left-start))
    (member 'middle
            (mobility-locations holding-agent left-start))
    (not (member 'right-goal
                 (mobility-locations holding-agent left-start)))
    (one-step-walkable holding-agent left-start middle)
    (not (one-step-walkable holding-agent middle right-goal))
    (not (traversable holding-agent left-start right-goal))
    (not (traversable holding-agent left-start sealed-site))))


(define-goal
  (walkability-coordinate-scenarios-valid))

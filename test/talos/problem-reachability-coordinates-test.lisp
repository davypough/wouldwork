;;; Filename: problem-reachability-coordinates-test.lisp
;;;
;;; Dedicated regression coverage for coordinate-derived manipulation reach.
;;;
;;; One rectangular boundary holds sixteen independent scenarios, each a pair of points
;;; two units apart and every scenario at least four units from its neighbours, so that
;;; *HORIZONTAL-REACH-LIMIT* can never join one scenario's points to another's and each
;;; assertion below characterizes exactly the geometry it names.
;;;
;;; The scenarios cover the whole rule: a clear in-range pair; the limit itself, met
;;; exactly and exceeded by a tenth; a wall that blocks and a window that does not; one
;;; gate crossed and two co-located gates crossed, the latter authored out of alphabetical
;;; order to pin the canonical barrier sort; a screen, which blocks outright because
;;; REACHABLE-CLEAR can never clear one; the ledge exception, reaching over an edge from
;;; the slab at its top while the same edge blocks a pair standing on the ground beside it;
;;; a gate whose vertical span floats entirely above the reach line and so is not a
;;; barrier; REACH-DISALLOWED> read in both directions; reach from a location to a
;;; wall-mounted switch; and two switches a single unit apart, between which no reach is
;;; ever derived because REACHABLE's second argument is always the actor's own location.
;;; STACK-LOWER and STACK-UPPER share a horizontal position and differ only in level: they
;;; are stacked rather than side by side, so no reach edge joins them at any limit, and the
;;; vertical model owns what passes between them.
;;;
;;; AUTHORED-A and AUTHORED-B are in range and geometrically clear, so the derivation
;;; would emit an empty barrier list for them; the problem authors LOCKED-DOOR instead, and
;;; that authored list must survive untouched.  The derivation adds to a problem's reach
;;; facts and never restates or refines one.
;;;
;;; WALKABILITY is deliberately not included.  Reach derivation must stand on segment
;;; geometry alone, without the walking arrangement, and this file fails if it ever starts
;;; depending on it.
;;;
;;; The goal directly characterizes initialization-derived state; no action is needed.
;;; Expected minimum path length: 0.

(in-package :ww)

(ww-set *problem-name* reachability-coordinates-test)
(ww-set *problem-type* planning)
(ww-set *solution-type* min-length)
(ww-set *tree-or-graph* graph)
(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 0)


;;;; TYPES ;;;;


(define-types
  location (near-left near-right
            stack-lower stack-upper
            limit-a limit-b over-a over-b
            wall-left wall-right
            window-left window-right
            one-gate-left one-gate-right
            two-gate-left two-gate-right
            screen-left screen-right
            ledge-ground ledge-top
            high-gate-left high-gate-right
            disallowed-a disallowed-b
            rev-a rev-b
            switch-site
            authored-a authored-b
            flat-left flat-right)
  switch (wall-switch other-switch)
  gate (door-a door-b door-c high-door locked-door)
  wall (blocking-wall)
  edge (low-ledge ground-ledge)
  window (clear-window)
  screen (barrier-screen))


;;;; TECHNOLOGY INCLUDE ;;;;


(include-tech reachability)


;;;; INITIALIZATION ;;;;


(define-init
  ;; DOOR-A and DOOR-B are open and DOOR-C is not.  OPEN is dynamic state supplied by the
  ;; gate substrate, seeded here so the reach query is characterized independently of the
  ;; gate-control lifecycle -- the same isolation problem-reachability-test uses.  Barrier
  ;; membership is a static geometric fact and takes no notice of it; REACHABLE reads the
  ;; gate state at query time instead, which is what the last scenario clauses check.
  (open door-a)
  (open door-b)

  ;; The final point closes the rectangle back to (0 0).
  (boundary-wall
    ((0 0) (40 0) (40 20) (0 20) (0 0)))

  ;; A wall blocks reach; a window does not, exactly as in the line-of-sight derivation.
  (wall-segment> blocking-wall 3 9 3 11)
  (window-segment> clear-window 9 9 9 11)

  ;; A screen blocks outright rather than becoming a barrier.
  (screen-segment> barrier-screen 27 9 27 11)

  ;; DOOR-C is authored before DOOR-B so that the derived pair's barrier list must be
  ;; sorted rather than merely collected in authoring order.
  (gate-segment> door-a 15 9 15 11)
  (gate-segment> door-c 209/10 9 209/10 11)
  (gate-segment> door-b 211/10 9 211/10 11)

  ;; HIGH-DOOR hangs five units up: its span never meets a ground-level reach line, so it
  ;; is not a barrier to one.
  (gate-segment> high-door 3 15 3 17 5)

  ;; LOCKED-DOOR stands well away from every scenario.  It exists only to be named in the
  ;; authored REACH-VIA below, which the derivation must leave alone.
  (gate-segment> locked-door 38 2 38 4)

  ;; Two identical edges, both 3/2 high.  LEDGE-TOP stands on the first at exactly its top
  ;; and reaches over it; FLAT-LEFT and FLAT-RIGHT stand on the ground on either side of
  ;; the second and cannot.
  (edge-segment> low-ledge 33 9 33 11)
  (edge-segment> ground-ledge 33 15 33 17)
  (has-height low-ledge 3/2)
  (has-height ground-ledge 3/2)

  ;; Scenario coordinates.  The optional third coordinate is the location's own level.
  (location-coords> near-left 2 2)
  (location-coords> near-right 4 2)
  (location-coords> stack-lower 8 2)
  (location-coords> stack-upper 8 2 3/2)
  (location-coords> limit-a 2 6)
  (location-coords> limit-b 9/2 6)
  (location-coords> over-a 8 6)
  (location-coords> over-b 53/5 6)
  (location-coords> wall-left 2 10)
  (location-coords> wall-right 4 10)
  (location-coords> window-left 8 10)
  (location-coords> window-right 10 10)
  (location-coords> one-gate-left 14 10)
  (location-coords> one-gate-right 16 10)
  (location-coords> two-gate-left 20 10)
  (location-coords> two-gate-right 22 10)
  (location-coords> screen-left 26 10)
  (location-coords> screen-right 28 10)
  (location-coords> ledge-ground 32 10)
  (location-coords> ledge-top 34 10 3/2)
  (location-coords> high-gate-left 2 16)
  (location-coords> high-gate-right 4 16)
  (location-coords> disallowed-a 8 16)
  (location-coords> disallowed-b 10 16)
  (location-coords> rev-a 14 16)
  (location-coords> rev-b 16 16)
  (location-coords> switch-site 20 16)
  (location-coords> authored-a 26 16)
  (location-coords> authored-b 28 16)
  (location-coords> flat-left 32 16)
  (location-coords> flat-right 34 16)

  ;; Two wall-mounted switches one unit apart, both within reach of SWITCH-SITE.
  (apparatus-coords> wall-switch 20 17)
  (apparatus-coords> other-switch 21 17)

  ;; REACH-DISALLOWED> is authored in the derivation's own order for the first pair and in
  ;; the opposite order for the second, so both readings are exercised.
  (reach-disallowed> disallowed-a disallowed-b)
  (reach-disallowed> rev-b rev-a)

  ;; An authored fact the derivation must not restate.  Geometry would give this pair an
  ;; empty barrier list.
  (reach-via authored-a (locked-door) authored-b))


;;;; CONSUMER-SPECIFIC BOUNDARY VALIDATION ;;;;


(define-test-claim reach-relation-schema
  (expect-relation-schema
    'reach-via :static '(reach-target list reach-target)
    :fluent-indices '(2))
  (expect-relation-schema
    'reach-via> :static '(location list reach-target)
    :fluent-indices '(2))
  (expect-relation-schema
    'reach-disallowed> :static '(reach-target reach-target))
  (expect-relation-absent 'reach-disallowed)
  (expect-relation-absent 'no-reach-via))


(define-test-claim horizontal-reach-limit-is-exact
  (= *horizontal-reach-limit* 5/2)
  (reachability-coordinates-within-limit-p 0 0 3/2 2 *horizontal-reach-limit*)
  (not (reachability-coordinates-within-limit-p 0 0 3/2 21/10 *horizontal-reach-limit*))
  (not (reachability-coordinates-within-limit-p 3 4 3 4 *horizontal-reach-limit*)))


(define-test-claim door-span-meets-reach-line
  (reachability-coordinates-span-meets-p '(door 0 0 0 1 0 4) 0 0)
  (not (reachability-coordinates-span-meets-p '(door 0 0 0 1 5 9) 0 0))
  (not (reachability-coordinates-span-meets-p '(door 0 0 0 1 0 1) 3/2 3/2)))


(define-test-claim unused-disallowal-is-reported
  (expect-condition
    (lambda ()
      (report-unused-reach-disallowals '((near-left near-right))))
    'error
    :containing "does not produce"))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query coordinate-reach-barriers-are
    (?a reach-target ?b reach-target ?expected)
  ;; True only when a reach edge joins the pair and its barrier list is exactly ?EXPECTED.
  ;; The BIND leads, so an absent edge fails the conjunction rather than comparing NIL with
  ;; an expected empty list and passing by accident.
  (and (bind (reach-via ?a $actual ?b))
       (equal $actual ?expected)))


(define-query coordinate-reach-absent (?a reach-target ?b reach-target)
  (not (bind (reach-via ?a $family ?b))))


(define-query reachability-coordinate-scenarios-valid ()
  (and
    ;; This is a zero-action characterization of initialized state.
    (open door-a)
    (open door-b)
    (not (open door-c))

    ;; A clear pair inside the limit, read in both directions: the relation carries no ">"
    ;; suffix, so the derivation asserts one direction and WW mirrors it.
    (coordinate-reach-barriers-are near-left near-right nil)
    (coordinate-reach-barriers-are near-right near-left nil)

    ;; Stacked points are not horizontally separated at all, so no reach edge joins them
    ;; however close they stand.  The vertical model owns that relationship.
    (= (location-elevation stack-lower) 0)
    (= (location-elevation stack-upper) 3/2)
    (coordinate-reach-absent stack-lower stack-upper)
    (coordinate-reach-absent stack-upper stack-lower)

    ;; The limit is inclusive, and a tenth beyond it is out.
    (coordinate-reach-barriers-are limit-a limit-b nil)
    (coordinate-reach-absent over-a over-b)
    (coordinate-reach-absent over-b over-a)

    ;; A wall blocks; a window is an opening an arm passes through, exactly as sight does.
    (coordinate-reach-absent wall-left wall-right)
    (coordinate-reach-absent wall-right wall-left)
    (coordinate-reach-barriers-are window-left window-right nil)
    (coordinate-reach-barriers-are window-right window-left nil)

    ;; A crossed gate becomes a barrier, and co-located gates are listed in canonical name
    ;; order rather than in the order the problem authored them.
    (coordinate-reach-barriers-are one-gate-left one-gate-right '(door-a))
    (coordinate-reach-barriers-are one-gate-right one-gate-left '(door-a))
    (coordinate-reach-barriers-are two-gate-left two-gate-right '(door-b door-c))
    (coordinate-reach-barriers-are two-gate-right two-gate-left '(door-b door-c))

    ;; A screen yields no edge at all: emitting it as a barrier would produce a row
    ;; REACHABLE-CLEAR can never clear.
    (coordinate-reach-absent screen-left screen-right)
    (coordinate-reach-absent screen-right screen-left)

    ;; The ledge exception.  Both pairs cross an identical 3/2-high edge; only the pair
    ;; whose higher endpoint stands at that edge's top reaches across it.
    (= (location-elevation ledge-top) 3/2)
    (= (top low-ledge) 3/2)
    (coordinate-reach-barriers-are ledge-ground ledge-top nil)
    (coordinate-reach-barriers-are ledge-top ledge-ground nil)
    (= (top ground-ledge) 3/2)
    (coordinate-reach-absent flat-left flat-right)
    (coordinate-reach-absent flat-right flat-left)

    ;; A gate hanging entirely above the reach line is not in the way of the arm.
    (= (base high-door) 5)
    (coordinate-reach-barriers-are high-gate-left high-gate-right nil)
    (coordinate-reach-barriers-are high-gate-right high-gate-left nil)

    ;; REACH-DISALLOWED> suppresses the pair whichever order it is authored in.
    (coordinate-reach-absent disallowed-a disallowed-b)
    (coordinate-reach-absent disallowed-b disallowed-a)
    (coordinate-reach-absent rev-a rev-b)
    (coordinate-reach-absent rev-b rev-a)

    ;; A location reaches a wall-mounted switch, whose position comes from
    ;; APPARATUS-COORDS> rather than LOCATION-COORDS>.
    (coordinate-reach-barriers-are switch-site wall-switch nil)
    (coordinate-reach-barriers-are wall-switch switch-site nil)
    (coordinate-reach-barriers-are switch-site other-switch nil)

    ;; No reach is ever derived between two fixtures, however close they stand.
    (coordinate-reach-absent wall-switch other-switch)
    (coordinate-reach-absent other-switch wall-switch)

    ;; The authored fact survives with its own barrier list; the derivation adds to a
    ;; problem's reach facts and never restates one.
    (coordinate-reach-barriers-are authored-a authored-b '(locked-door))
    (coordinate-reach-barriers-are authored-b authored-a '(locked-door))

    ;; Planar geometry is symmetric and derives no directional fact.
    (not (exists (?from location ?to reach-target)
           (bind (reach-via> ?from $directional-barriers ?to))))

    ;; REACHABLE then reads the live gate state over those static edges.  DOOR-A is open
    ;; and DOOR-C is not, so one derived barrier list clears and the other does not.
    (reachable near-right near-left)
    (reachable one-gate-right one-gate-left)
    (not (reachable two-gate-right two-gate-left))
    (not (reachable wall-right wall-left))
    (reachable wall-left wall-left)))


(define-goal
  (reachability-coordinate-scenarios-valid))


;;;; MUTATION CHARACTERIZATION ;;;;


(define-query-mutation reachability-coordinates-emittable-ignores-disallowals
    reachability-coordinates-emittable
  (?a reach-target ?b reach-target)
  (not (reachability-coordinates-authored ?a ?b))
  "Drops REACH-DISALLOWED> from the emission test, leaving only the authored-fact guard.
   The two suppressed scenarios must then appear as ordinary derived edges and make the
   DISALLOWED-A/DISALLOWED-B and REV-A/REV-B probes fail.")

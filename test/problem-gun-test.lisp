;;; Filename: problem-gun-test.lisp

;;; Minimal gun exercise.  start holds the agent and jammer1.  gun1 is a point fixture,
;;; positioned via LOS rather than HAS-POSITION -- nothing can occupy its mounting point --
;;; with no controller wired to it, so it defaults armed (lethal): uncontrolled guns are
;;; always on, the same default -gears-fan uses for turning.  gun1 threatens watched, the
;;; only walking route from start to goal.  walk is not a single hop --
;;; walkable-locations' BFS walks the whole closure, and walk offers any node in it as a
;;; one-shot destination -- so safety is gated inside one-step-walkable, excluding watched
;;; as a through-node as well as an endpoint: while gun1 is armed, goal never enters the
;;; walkable-locations set at all, not even via a direct walk that would otherwise skip past
;;; watched.
;;;
;;; Jamming a gun is a line-of-sight check only now (jam-target's gun branch reads
;;; visible/los-to-apparatus, exactly like a gate, with no has-position/eql shortcut), so
;;; visibility is included for the relation to exist, and the sightline from start to gun1
;;; is hand-authored directly -- (los-to-apparatus start () gun1), an empty occluder list
;;; -- rather than derived from wall-segments/coordinates, which visibility.lisp's own
;;; header explicitly supports for a problem that would rather author positions than
;;; hand-list sightlines, or vice versa.
;;;
;;; Expected minimum solution (3 steps): pickup-jammer jammer1, jam-target gun1 (at start,
;;; via the hand-authored sightline), walk start->goal.


(in-package :ww)


(ww-set *problem-name* gun-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 8)


;;;; TYPES ;;;;


(define-types
  agent (agent1)
  location (start watched goal)
  jammer (jammer1)
  gun (gun1)
)


;;;; TECHNOLOGY INCLUDES ;;;;


(include-tech gun)
(include-tech jammer)
(include-tech walkability)
(include-tech visibility)


;;;; INITIALIZATION ;;;;


(define-init
  ;; Movable objects
  (has-location agent1 start)
  (has-location jammer1 start)

  ;; gun1's sightline from start, hand-authored directly rather than derived from
  ;; coordinates: an empty occluder list is a direct, always-clear line.
  (los-to-apparatus start () gun1)

  ;; gun1's kill zone: the only location on the route from start to goal.  Uncontrolled,
  ;; so it is armed (lethal) from t=0.
  (threatens gun1 (watched))

  ;; Walking topology.
  (walk-via start () watched)
  (walk-via watched () goal)
)


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!))
)


;;;; GOAL ;;;;


(define-goal
  (has-location agent1 goal)
)

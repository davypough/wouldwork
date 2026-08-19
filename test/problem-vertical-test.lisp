;;; Filename: problem-vertical-test.lisp

;;; Dedicated zero-action regression for the -vertical substrate: the per-type constant
;;; table and the BASE / TOP queries that replace the scattered elevation role branches.
;;; Four groups of fixtures characterize the model:
;;;
;;;   1. The height table.  One fixture per type takes its default height; three more
;;;      author HAS-HEIGHT to show an authored value overriding the default.
;;;   2. The axis rule.  A floor repeater stands, so its top is base plus height; a wall
;;;      repeater's height is its horizontal projection from the wall and cannot lift its
;;;      top, however large the authored value.  Point apparatus has no extent at all.
;;;   3. The structural base.  Every route is exercised in turn -- resting ON a support,
;;;      being HELD, sitting at a HAS-LOCATION, being positioned by HAS-POSITION, and
;;;      falling back to an authored or defaulted HAS-ELEVATION -- including the two
;;;      precedence cases, where an occupant keeps its location fact while resting on a
;;;      support and a held tray keeps its own while being carried.
;;;   4. The acceptance check.  The three achievable connector anchor heights, all at
;;;      RAISED-SITE's floor of two: on the ground TOP is 3, on a box 4, and on a tray
;;;      held by a standing agent 9/2.  Under this model all three fall out of BASE and
;;;      TOP with no held-tray special case, which is the property the refactor exists to
;;;      establish.
;;;
;;; Two deliberate divergences from the queries -vertical will eventually replace.  A
;;; transmitter, receiver, or gun has base zero here, not the functional elevation of one
;;; that FIXTURE-ELEVATION supplies, and a wall repeater's base defaults to zero rather
;;; than one.  Both of those defaults belong to the apparatus's own coordinate relation,
;;; not to HAS-ELEVATION, and move to APPARATUS-COORDS>'s optional third coordinate when
;;; the coordinate relations absorb their z.  Until then the old queries keep supplying
;;; the old defaults and these fixtures pin the new meaning.
;;;
;;; Initial and final states are identical.  Expected minimum path length: zero.

(in-package :ww)


(ww-set *problem-name* vertical-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 0)


;;;; TYPES ;;;;


(define-types
  location (floor-site raised-site)
  agent (ground-agent tray-agent)
  box (anchor-box)
  tray (held-tray)
  fan (ground-fan)
  jammer (ground-jammer)
  connector (ground-connector box-connector tray-connector)
  pressure-plate (site-plate)
  gate (default-gate explicit-gate)
  screen (default-screen)
  wall (default-wall)
  edge (default-edge)
  floor-repeater (default-floor-repeater explicit-floor-repeater)
  wall-repeater (default-wall-repeater explicit-wall-repeater)
  transmitter (default-transmitter explicit-transmitter)
  receiver (default-receiver)
  gun (default-gun))


;;;; TECHNOLOGY INCLUDE ;;;;


(include-tech -vertical)


;;;; INITIALIZATION ;;;;


(define-init
  ;; Two floors: one authored, one taking the zero default.
  (has-elevation raised-site 2)

  ;; Fixtures fixed in space.  Distinct values make each authored branch independently
  ;; observable, and each explicit height is chosen to differ from its type default.
  (has-elevation explicit-gate 3)
  (has-height explicit-gate 5)
  (has-elevation explicit-floor-repeater 10)
  (has-height explicit-floor-repeater 2)
  (has-elevation explicit-wall-repeater 11)
  (has-height explicit-wall-repeater 7)
  (has-elevation explicit-transmitter 7)

  ;; The positioned route: a plate lies flush on the floor it is positioned at.
  (has-position site-plate raised-site)

  ;; The three achievable connector anchor heights, gathered at RAISED-SITE.  Every
  ;; movable occupant keeps its ordinary location fact while resting on a support, so
  ;; ON and HOLDING must both take precedence over HAS-LOCATION.
  (has-location tray-agent raised-site)
  (holding tray-agent held-tray)
  (has-location held-tray raised-site)
  (has-location anchor-box raised-site)
  (has-location ground-connector raised-site)
  (has-location box-connector raised-site)
  (on box-connector anchor-box)
  (has-location tray-connector raised-site)
  (on tray-connector held-tray)

  ;; Ground-level fallbacks at the defaulted floor.
  (has-location ground-agent floor-site)
  (has-location ground-fan floor-site)
  (has-location ground-jammer floor-site))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query vertical-scenarios-valid ()
  (and
    ;; 1. The height table, one fixture per type.
    (= (object-height ground-agent) 3/2)
    (= (object-height anchor-box) 1)
    (= (object-height ground-connector) 1)
    (= (object-height ground-jammer) 1)
    (= (object-height held-tray) 0)
    (= (object-height ground-fan) 0)
    (= (object-height site-plate) 0)
    (= (object-height default-gate) 4)
    (= (object-height default-screen) 4)
    (= (object-height default-wall) 4)
    (= (object-height default-edge) 3/2)
    (= (object-height default-floor-repeater) 1)
    (= (object-height default-wall-repeater) 1)
    (= (object-height default-transmitter) 0)
    (= (object-height default-receiver) 0)
    (= (object-height default-gun) 0)

    ;; An authored height overrides the type default.
    (= (object-height explicit-gate) 5)
    (= (object-height explicit-floor-repeater) 2)
    (= (object-height explicit-wall-repeater) 7)

    ;; 2. The axis rule.  A vertical extent raises the top; a horizontal one does not,
    ;; and a point apparatus has no extent to raise it with.
    (= (base explicit-floor-repeater) 10)
    (= (top explicit-floor-repeater) 12)
    (= (base explicit-wall-repeater) 11)
    (= (top explicit-wall-repeater) 11)
    (not (= (top explicit-wall-repeater) 18))
    (= (base default-wall-repeater) 0)
    (= (top default-wall-repeater) 0)
    (= (base explicit-transmitter) 7)
    (= (top explicit-transmitter) 7)
    (= (base default-transmitter) 0)
    (= (top default-transmitter) 0)

    ;; 3. The structural base, one route at a time.
    ;; A location is a point: its base is its own level and its top adds nothing.
    (= (base floor-site) 0)
    (= (base raised-site) 2)
    (= (top raised-site) 2)

    ;; A fixture fixed in space takes its authored level, or zero.
    (= (base default-gate) 0)
    (= (top default-gate) 4)
    (= (base explicit-gate) 3)
    (= (top explicit-gate) 8)

    ;; HAS-POSITION resolves through the location it names.
    (= (base site-plate) 2)
    (= (top site-plate) 2)

    ;; HAS-LOCATION resolves through the location's own level.
    (= (base ground-agent) 0)
    (= (top ground-agent) 3/2)
    (= (base ground-fan) 0)
    (= (top ground-fan) 0)
    (= (base ground-jammer) 0)
    (= (top ground-jammer) 1)

    ;; HOLDING takes precedence over the held object's own location fact.
    (has-location held-tray raised-site)
    (= (base tray-agent) 2)
    (= (top tray-agent) 7/2)
    (= (base held-tray) 7/2)
    (= (top held-tray) 7/2)

    ;; ON takes precedence over the occupant's own location fact.
    (has-location box-connector raised-site)
    (= (base anchor-box) 2)
    (= (top anchor-box) 3)

    ;; 4. The acceptance check: the three achievable connector anchor heights,
    ;; measured from RAISED-SITE's floor of two, with no held-tray special case.
    (= (base ground-connector) 2)
    (= (top ground-connector) 3)
    (= (base box-connector) 3)
    (= (top box-connector) 4)
    (= (base tray-connector) 7/2)
    (= (top tray-connector) 9/2)))


(define-goal
  (vertical-scenarios-valid))

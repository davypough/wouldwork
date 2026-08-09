;;; Filename: problem-support-elevation-test.lisp

;;; Dedicated zero-action regression for the shared -support-elevation role.
;;; Four independent scenarios characterize its complete elevation model:
;;;
;;;   1. A nonzero-elevation stack chains through plate, explicit-height box,
;;;      zero-thickness fan, default-height box, and default-height agent.
;;;      The agent's recursively derived standing elevation supplies inclusive
;;;      reach boundaries two units above and below, with rejection one unit
;;;      beyond either boundary.
;;;   2. Ground occupants exercise both location-elevation fallbacks: an ordinary
;;;      default-elevation agent stands at zero, while a loose fan and a
;;;      default-height box at elevation four have tops of four and five.
;;;   3. A grounded tray is inert and zero-thickness, like a resting fan,
;;;      contributing nothing beyond its own resting level.
;;;   4. A held tray's top rides its holder's own top level -- occupant-elevation
;;;      plus declared-height, zero added for the tray itself -- and an occupant
;;;      resting on the held tray chains through the ordinary recursion.
;;;
;;; The characterization goal verifies the authored support chain, the absence of
;;; competing support and height facts, all exact intermediate elevations, fan and
;;; tray zero-thickness behavior, held-tray elevation, default heights, and the
;;; positive and negative reach boundaries.  Initial and final states are identical.
;;; Expected minimum path length: zero.

(in-package :ww)


(ww-set *problem-name* support-elevation-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 0)


;;;; TYPES ;;;;


(define-types
  agent (stack-agent ground-agent tray-holding-agent)
  location (stack-site default-site raised-ground-site tray-holding-site)
  pressure-plate (base-plate)
  box (base-box upper-box ground-box tray-occupant-box)
  fan (middle-fan ground-fan)
  tray (held-tray ground-tray))


;;;; TECHNOLOGY INCLUDES ;;;;


(include-tech -support-elevation)


;;;; INITIALIZATION ;;;;


(define-init
  ;; Plate at elevation three, then +2 box, +0 fan, +1 default box.
  ;; Every movable occupant retains the ordinary location fact for its stack
  ;; site; ON must take precedence when deriving its standing elevation.
  (has-elevation stack-site 3)
  (has-position base-plate stack-site)

  (has-location base-box stack-site)
  (has-height base-box 2)
  (on base-box base-plate)

  (has-location middle-fan stack-site)
  (on middle-fan base-box)

  (has-location upper-box stack-site)
  (on upper-box middle-fan)

  (has-location stack-agent stack-site)
  (on stack-agent upper-box)

  ;; Independent ground fallbacks.  DEFAULT-SITE omits HAS-ELEVATION; the
  ;; raised fixtures omit ON and therefore inherit their location elevation.
  (has-location ground-agent default-site)
  (has-elevation raised-ground-site 4)
  (has-location ground-fan raised-ground-site)
  (has-location ground-box raised-ground-site)
  (has-location ground-tray raised-ground-site)

  ;; A held tray's top rides its holder's own top level; an occupant resting on it
  ;; chains through the same recursion as any other support.
  (has-location tray-holding-agent tray-holding-site)
  (has-elevation tray-holding-site 2)
  (holding tray-holding-agent held-tray)
  (has-location held-tray tray-holding-site)
  (has-location tray-occupant-box tray-holding-site)
  (on tray-occupant-box held-tray))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query support-elevation-only-on
    (?occupant support-occupant ?expected-support support)
  (and
    (on ?occupant ?expected-support)
    (not (exists (?other-support support)
           (and (different ?other-support ?expected-support)
                (on ?occupant ?other-support))))))


(define-query support-elevation-scenarios-valid ()
  (and
    ;; The authored chain is exact, so a coincident competing support cannot
    ;; accidentally supply the expected elevation.
    (support-elevation-only-on base-box base-plate)
    (support-elevation-only-on middle-fan base-box)
    (support-elevation-only-on upper-box middle-fan)
    (support-elevation-only-on stack-agent upper-box)

    ;; Explicit nonzero plate elevation and every recursive intermediate result.
    (= (location-elevation stack-site) 3)
    (= (support-top-elevation base-plate) 3)
    (= (occupant-elevation base-box) 3)
    (= (declared-height base-box) 2)
    (= (support-top-elevation base-box) 5)

    ;; A fan is a zero-thickness movable support even when it rests on a box.
    (= (occupant-elevation middle-fan) 5)
    (= (support-top-elevation middle-fan) 5)

    ;; The omitted box height uses the exact default of one.
    (not (bind (has-height upper-box $upper-box-height)))
    (= (declared-height upper-box) 1)
    (= (occupant-elevation upper-box) 5)
    (= (support-top-elevation upper-box) 6)

    ;; The omitted agent height uses the exact default of two.
    (not (bind (has-height stack-agent $stack-agent-height)))
    (= (declared-height stack-agent) 2)
    (= (occupant-elevation stack-agent) 6)

    ;; Reach is absolute and inclusive around the recursively derived level.
    (within-agent-vertical-reach stack-agent 4)
    (within-agent-vertical-reach stack-agent 8)
    (not (within-agent-vertical-reach stack-agent 3))
    (not (within-agent-vertical-reach stack-agent 9))

    ;; Ground occupants have no support and inherit explicit or default floor
    ;; elevation.  The fan still adds zero; the default-height box adds one.
    (not (exists (?support support)
           (on ground-agent ?support)))
    (= (location-elevation default-site) 0)
    (= (occupant-elevation ground-agent) 0)

    (not (exists (?support support)
           (on ground-fan ?support)))
    (= (occupant-elevation ground-fan) 4)
    (= (support-top-elevation ground-fan) 4)

    (not (exists (?support support)
           (on ground-box ?support)))
    (not (bind (has-height ground-box $ground-box-height)))
    (= (declared-height ground-box) 1)
    (= (occupant-elevation ground-box) 4)
    (= (support-top-elevation ground-box) 5)

    ;; A grounded tray is inert, like a resting fan: zero-thickness, contributing
    ;; nothing beyond its own resting level.
    (not (exists (?a agent) (holding ?a ground-tray)))
    (not (exists (?support support)
           (on ground-tray ?support)))
    (= (occupant-elevation ground-tray) 4)
    (= (support-top-elevation ground-tray) 4)

    ;; A held tray's top is its holder's own top level, zero added for the tray
    ;; itself; an occupant resting on it chains through the ordinary recursion.
    (holding tray-holding-agent held-tray)
    (not (exists (?support support)
           (on held-tray ?support)))
    (= (location-elevation tray-holding-site) 2)
    (= (occupant-elevation tray-holding-agent) 2)
    (= (declared-height tray-holding-agent) 2)
    (= (support-top-elevation held-tray) 4)
    (support-elevation-only-on tray-occupant-box held-tray)
    (= (occupant-elevation tray-occupant-box) 4)
    (= (declared-height tray-occupant-box) 1)
    (= (support-top-elevation tray-occupant-box) 5)))


(define-goal
  (support-elevation-scenarios-valid))

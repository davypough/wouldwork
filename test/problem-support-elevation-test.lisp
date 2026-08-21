;;; Filename: problem-support-elevation-test.lisp

;;; Dedicated zero-action regression for -vertical's geometry and -support-elevation's
;;; reach policy.  Five independent scenarios characterize them:
;;;
;;;   1. A nonzero-elevation stack chains through plate, explicit-height box,
;;;      zero-thickness fan, default-height box, and default-height agent.
;;;      The agent's recursively derived standing elevation supplies inclusive
;;;      reach boundaries one unit above and below, with rejection one unit
;;;      beyond either boundary.
;;;   2. Ground occupants exercise both location-elevation fallbacks: an ordinary
;;;      default-elevation agent stands at zero, while a loose fan and a
;;;      default-height box at elevation four have tops of four and five.
;;;   3. A grounded tray is inert and zero-thickness, like a resting fan,
;;;      contributing nothing beyond its own resting level.
;;;   4. A held tray's top rides its holder's own top level -- base
;;;      plus object-height, zero added for the tray itself -- and an occupant
;;;      resting on the held tray chains through the ordinary recursion.
;;;   5. The three achievable connector anchor heights, all at one site so they read
;;;      as offsets from a single floor: a connector on the ground anchors at the
;;;      floor plus one, on a box at plus two, and on a tray held by a standing agent
;;;      at plus five halves.  These are the empirically established values the
;;;      vertical model must keep reproducing; all three follow the ordinary support
;;;      recursion.
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
  agent (stack-agent ground-agent tray-holding-agent anchor-agent)
  location (stack-site default-site raised-ground-site tray-holding-site anchor-site)
  pressure-plate (base-plate)
  box (base-box upper-box ground-box tray-occupant-box anchor-box)
  fan (middle-fan ground-fan)
  tray (held-tray ground-tray anchor-tray)
  connector (ground-connector box-connector tray-connector))


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
  (on tray-occupant-box held-tray)

  ;; The three achievable connector anchor heights, gathered at one site so each
  ;; reads directly as an offset from that site's floor.  ANCHOR-BOX and every
  ;; connector take the default height of one; ANCHOR-AGENT takes the default 3/2.
  (has-elevation anchor-site 2)
  (has-location anchor-agent anchor-site)
  (holding anchor-agent anchor-tray)
  (has-location anchor-tray anchor-site)
  (has-location anchor-box anchor-site)
  (has-location ground-connector anchor-site)
  (has-location box-connector anchor-site)
  (on box-connector anchor-box)
  (has-location tray-connector anchor-site)
  (on tray-connector anchor-tray))


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
    (= (top base-plate) 3)
    (= (base base-box) 3)
    (= (object-height base-box) 2)
    (= (top base-box) 5)

    ;; A fan is a zero-thickness movable support even when it rests on a box.
    (= (base middle-fan) 5)
    (= (top middle-fan) 5)

    ;; The omitted box height uses the exact default of one.
    (not (bind (has-height upper-box $upper-box-height)))
    (= (object-height upper-box) 1)
    (= (base upper-box) 5)
    (= (top upper-box) 6)

    ;; The omitted agent height uses the exact default of 3/2.
    (not (bind (has-height stack-agent $stack-agent-height)))
    (= (object-height stack-agent) 3/2)
    (= (base stack-agent) 6)

    ;; Reach is absolute and inclusive around the recursively derived level.
    (within-agent-vertical-reach stack-agent 5)
    (within-agent-vertical-reach stack-agent 7)
    (not (within-agent-vertical-reach stack-agent 4))
    (not (within-agent-vertical-reach stack-agent 8))

    ;; Ground occupants have no support and inherit explicit or default floor
    ;; elevation.  The fan still adds zero; the default-height box adds one.
    (not (exists (?support support)
           (on ground-agent ?support)))
    (= (location-elevation default-site) 0)
    (= (base ground-agent) 0)

    (not (exists (?support support)
           (on ground-fan ?support)))
    (= (base ground-fan) 4)
    (= (top ground-fan) 4)

    (not (exists (?support support)
           (on ground-box ?support)))
    (not (bind (has-height ground-box $ground-box-height)))
    (= (object-height ground-box) 1)
    (= (base ground-box) 4)
    (= (top ground-box) 5)

    ;; A grounded tray is inert, like a resting fan: zero-thickness, contributing
    ;; nothing beyond its own resting level.
    (not (exists (?a agent) (holding ?a ground-tray)))
    (not (exists (?support support)
           (on ground-tray ?support)))
    (= (base ground-tray) 4)
    (= (top ground-tray) 4)

    ;; A held tray's top is its holder's own top level, zero added for the tray
    ;; itself; an occupant resting on it chains through the ordinary recursion.
    (holding tray-holding-agent held-tray)
    (not (exists (?support support)
           (on held-tray ?support)))
    (= (location-elevation tray-holding-site) 2)
    (= (base tray-holding-agent) 2)
    (= (object-height tray-holding-agent) 3/2)
    (= (top held-tray) 7/2)
    (support-elevation-only-on tray-occupant-box held-tray)
    (= (base tray-occupant-box) 7/2)
    (= (object-height tray-occupant-box) 1)
    (= (top tray-occupant-box) 9/2)

    ;; The three achievable connector anchor heights, as offsets from ANCHOR-SITE's
    ;; floor elevation of two.  A connector's anchor is its TOP, so each pair below
    ;; pins the structural base and the public anchor it yields: ground gives floor + 1,
    ;; a box floor + 2, and a tray held by a standing agent floor + 5/2.
    (= (location-elevation anchor-site) 2)
    (= (base anchor-agent) 2)

    (not (exists (?support support)
           (on ground-connector ?support)))
    (= (base ground-connector) 2)
    (= (top ground-connector) 3)

    (support-elevation-only-on box-connector anchor-box)
    (= (base box-connector) 3)
    (= (top box-connector) 4)

    (support-elevation-only-on tray-connector anchor-tray)
    (= (base tray-connector) 7/2)
    (= (top tray-connector) 9/2)))


(define-test-claim vertical-reach-parameter-relevant-to-raised-supports
  (vertical-reach-limit-relevant-p *start-state*)
  (vertical-reach-box-support-values
    *start-state* '(0) (gethash 'cargo *types*))
  (vertical-reach-held-tray-values
    *start-state* '(0) (gethash 'agent *types*) (gethash 'cargo *types*))
  (search "*VERTICAL-REACH-LIMIT*"
          (with-output-to-string (*standard-output*)
            (display-current-parameters))))


(define-goal
  (support-elevation-scenarios-valid))

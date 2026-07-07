;;; Filename: barrier.lisp

;;; Barrier-vaulting technology: jump over a fence, gate, or screen from a box on
;;; the departure side.
;;; Barrier edges may use symmetric traversable facts or directed traversable> facts.  The agent
;;; must already be standing on a box at the departure location, no more than one level below the
;;; tallest barrier top in the edge's means list, may carry cargo, and lands either on the ground
;;; or on a clear box at the target location as long as the landing elevation is not above that
;;; original standing elevation.
;;;
;;; REQUIRES:
;;;   types     : agent, location  --  box and fence are declared optional here
;;;               (define-optional-types); gate and screen need no declaration of their own
;;;               here either, since vaultable-barrier's (either fence gate screen) tolerates
;;;               their absence directly
;;;   nested    : -support-occupancy (support-occupant, support, (on ...), cleartop);
;;;               -height (heighted-object, (has-height ...)); -location (mobile-object,
;;;               (has-location ...)); -elevation ((has-elevation ...), location-elevation)  --
;;;               all four shared via nested include-tech rather than
;;;               local declaration.  (Previously this file declared support-occupant/support
;;;               and mobile-object/has-location by hand instead of nesting their owning substrate
;;;               files -- only correct when box/jammer/accessibility/ladder happened to
;;;               also be included.  The nested includes make this file genuinely
;;;               self-contained.)
;;;   queries   : occupant-elevation, support-top-elevation (box, for the vaulting-height
;;;               and landing-height checks)
;;;   driver    : propagate-changes! (master)
;;; PROVIDES:
;;;   types     : box, fence  --  declared optional here; other techs (box, jammer,
;;;               beam-relay, etc.) independently declare their own box-alias for their own
;;;               pre-params; the bare and aliased forms resolve compatibly
;;;               vaultable-barrier (either fence gate screen)  --  sole consumer; not
;;;               declared elsewhere; what vault-over can vault
;;;   relations : (traversable location $list location)
;;;               (traversable> location $list location)
;;;               barrier-height reads the shared has-height relation, with a per-kind default
;;;               when undeclared (fence 2, gate/screen 3)
;;;   queries   : barrier-height, vaultable-barrier-list, vault-clearance-height
;;;   action    : vault-over

(include-tech -support-occupancy)
(include-tech -height)
(include-tech -location)
(include-tech -elevation)

(in-package :ww)


(define-types
  vaultable-barrier (either fence gate screen))  ;sole consumer; what vault-over can vault


(define-optional-types box fence)


(define-static-relations
  (traversable location $list location)  ;symmetric barrier edge; $list = barriers
  (traversable> location $list location))  ;directed barrier edge; $list = barriers


(define-query barrier-height (?barrier vaultable-barrier)
  ;; Declared clearance height of a vaultable barrier, or a per-kind default when undeclared:
  ;; fence 2, gate/screen 3.  Any of the three -- fence, gate, or screen -- may override its
  ;; default by an explicit (has-height ...) fact, since all three (plus box/agent) share the
  ;; heighted-object type.  Mirrors location-elevation's declared-or-default shape, but the
  ;; default is barrier-kind-specific rather than a single universal value.
  (if (bind (has-height ?barrier $h))
    $h
    (if (fence ?barrier)
      2
      3)))


(define-query vaultable-barrier-list (?barriers)
  ;; The whole means list must be a non-empty set of vaultable barriers; a ladder-only
  ;; traversable> edge remains ladder-only and cannot be used by vault-over.
  (and ?barriers
       (ww-loop for $barrier in ?barriers
                always (vaultable-barrier $barrier))))


(define-query vault-clearance-height (?barriers)
  ;; Clearance is governed by the tallest barrier named on the edge.
  (ww-loop for $barrier in ?barriers
           maximize (barrier-height $barrier)))


(define-action vault-over
  ;; Vault a fence/gate/screen edge from the box the agent is already standing on, tall enough
  ;; to clear the tallest listed barrier: occupant-elevation must be no more than one level
  ;; below that top.  Each landing branch must also be no higher than the departure elevation.
  ;; The jump removes the departure support and branches over possible landings on clear
  ;; target-side boxes, plus the ground fallback.
  1
  (?agent agent ?box box)
  (and (bind (has-location ?agent $from))
       (on ?agent ?box)
       (bind (has-location ?box $from))
       (or (bind (traversable $from $means $to))
           (bind (traversable> $from $means $to)))
       (vaultable-barrier-list $means)
       (assign $departure-elevation (occupant-elevation ?agent))
       (<= (- (vault-clearance-height $means) $departure-elevation) 1))
  (":" ?agent "jumps over" $means "from" ?box "at" $from "to" $to "on" $place)
  (do (doall (?landing-box box)
        (if (and (has-location ?landing-box $to)
                 (cleartop ?landing-box)
                 (<= (support-top-elevation ?landing-box) $departure-elevation))
          (assert (has-location ?agent $to)
                  (not (on ?agent ?box))
                  (on ?agent ?landing-box)
                  (assign $place ?landing-box)
                  (finally (propagate-changes!)))))
      (if (<= (location-elevation $to) $departure-elevation)
        (assert (has-location ?agent $to)
                (not (on ?agent ?box))
                (assign $place 'ground)
                (finally (propagate-changes!))))))

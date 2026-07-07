;;; Filename: problem-corner-topo2.lisp

;;; Talos Principle problem 'Around the Corner' (Purgatory workshop 3), rebuilt from
;;; self-contained technology files, following the problem-claustro4a.lisp architecture.
;;; Same objects, hues, controls, connectivity, and topological (coordinate-free) beam
;;; geometry as problem-corner-topo.lisp.  Behavior is supplied entirely by
;;; (include-tech ...) directives that the stage-time splicer (exchange-problem-file)
;;; expands in place; this file holds only the glue: types, the master propagation
;;; driver, and the init/goal.  Beam relaying through movable connectors is supplied by
;;; beam-relay-tech; crossing-based beam cutting (no coordinate geometry, just authored
;;; crossing/sightline facts) is supplied by beam-crossing-tech.  Only normal-mode gate
;;; control is used, matching corner-topo.


(in-package :ww)


(ww-set *problem-name* corner-topo2)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 15)

(ww-set *symmetry-pruning* t)

(ww-set *progress-reporting-interval* 1000000)


(defparameter *max-pairings* 3)  ;max termini a connector may pair in one connect (beam-relay-tech's connect-connector)


;;;; TYPES ;;;;
;;;; Leaf object types the problem instantiates live here, ahead of the technology
;;;; includes.  Every composite type that a consuming tech file needs (mobile-object,
;;;; cargo, support-occupant, support, target, fixed-position-object) is declared identically
;;;; inside that tech file instead, so no tech file depends on a type declaration living
;;;; in the problem.  beam-endpoint is the remaining exception: it is a corner-topo-specific
;;;; composite that no tech file declares, so it must live here.  terminus is now owned by
;;;; beam-relay-tech, but the identical declaration is left here for local readability.


(define-types
  agent       (agent1)
  gate        (gate1)  ;only the dynamic gate (controlled by receiver1); corner's gate2/walls/window are static occlusion baked into the los facts
  location    (location1 location2 location3 location4)  ;corner area1..area4
  connector   (connector1 connector2 connector3)
  transmitter (transmitter1 transmitter2)
  receiver    (receiver1 receiver2 receiver3)
  hue         (blue red)
  mode        (normal inverted toggle)  ;controller mode; corner uses only normal
  crossing    (crossing1 crossing2 crossing3 crossing4 crossing5 crossing6 crossing7 crossing8 crossing9 crossing10
               crossing11 crossing12 crossing13 crossing14 crossing15 crossing16 crossing17 crossing18 crossing19 crossing20
               crossing21 crossing22 crossing23 crossing24 crossing25 crossing26)  ;26 beam crossings (corner geometry); see define-init
  terminus      (either transmitter receiver connector)  ;what a connector can pair/connect to; also supplied by beam-relay-tech
  beam-endpoint (either transmitter receiver location)  ;a fixture, or a connector's location; not supplied by any tech file
  ;; No inert leaf types remain.  plate, jammer, box, screen, and ladder were all originally
  ;; listed here as null-instance types required only because a consuming technology's own
  ;; composite type (mobile-object, cargo, support-occupant, support, fixed-position-object) referenced
  ;; it, or because a tech file called it as a bare implicit type-predicate.  Each consuming
  ;; tech file now references its base type only through its own -alias (either <type>)
  ;; declaration, which tolerates the base type's absence without requiring this problem to
  ;; declare it at all.
)


;;;; TECHNOLOGY INCLUDES ;;;;
;;;; corner-topo2 needs beam relaying through movable connectors (beam-relay) with
;;;; crossing-based beam cutting (beam-crossing), plus the walking/sightline background
;;;; (accessibility, visibility).  The remaining five are transitive requirements, not
;;;; corner-topo behavior: gate-tech's energized and update-gate-status! test both a
;;;; receiver and a plate, and check jamming, so gate-tech requires plate-tech and
;;;; jammer-tech; plate-tech requires support-occupancy-tech's cleartop; accessibility-
;;;; tech requires elevation-tech's location-elevation; beam-relay-tech requires
;;;; reachability-tech for pickup/connect range-checking.  With plate, box, and jammer
;;;; declared as empty types above, every fact, action, and derived-state check these
;;;; five contribute is a no-op: no plates or jammers exist, and reachability with no
;;;; reachable-via facts reduces to identity, so pickup/connect still require the
;;;; agent's own location exactly as in corner-topo.


(include-tech gate)                  ;controls; energized; update-gate-status!
(include-tech beam-relay)            ;paired; color; pickup-connector; connect-connector
(include-tech beam-crossing)         ;crossing-active; beam-crossing>; crossings-along-beam>
(include-tech accessibility)         ;walk-via; accessible; one-step-accessible; move
(include-tech visibility)            ;los-to-fixture; los-to-location; visible; visible-clear


;;;; MASTER PROPAGATION DRIVER ;;;;


(define-update propagate-changes! ()
  ;; Binds the change-detection gate so add-prop/del-prop flag *propagated-state-changed*
  ;; on real derived-fact mutations during the fixpoint.  Each pass runs to convergence (no
  ;; change) or, failing that, the cap declares the state inconsistent.
  (let ((*detect-propagated-changes* t))
    (ww-loop for $iteration from 1 to 5
             do (if (not (propagate-consequences!))
                  (return t))
             finally (inconsistent-state)
                     (return nil))))


(define-update propagate-consequences! ()
  ;; One propagation pass.  Assembled here from exactly the loaded technologies' update
  ;; functions, in dependency order: active crossings are computed first so connector
  ;; lighting and receiver activation both see which beams are cut; connectors are lit
  ;; next so a freshly-lit connector can power its receiver within the same pass; plates
  ;; and gates follow (plates are inert here, but the causal slot is still exercised).
  ;; Returns t iff some derivation changed stored state, telling propagate-changes! to
  ;; run another pass.
  (let ((*propagated-state-changed* nil))
    (update-crossing-status!)
    (update-connector-status!)
    (update-receiver-status!)
    (update-gate-status!)
    *propagated-state-changed*))


;;;; INITIALIZATION ;;;;


(define-init
  ;; Dynamic state
  (has-location agent1 location1)
  (has-location connector1 location1)
  (has-location connector2 location2)
  (has-location connector3 location3)

  ;; Gate control (DNF): receiver1 active -> gate1 open
  (controls ((receiver1)) gate1 normal)

  ;; Fixed hues
  (has-has-chroma transmitter1 red)
  (has-chroma transmitter2 blue)
  (has-chroma receiver1 red)
  (has-chroma receiver2 red)
  (has-chroma receiver3 blue)

  ;; Location -> fixture sightlines (occluder gates that must be open); from corner los0/los1
  (los-to-fixture location1 () transmitter1)
  (los-to-fixture location1 () transmitter2)
  (los-to-fixture location1 () receiver1)
  (los-to-fixture location2 () transmitter1)
  (los-to-fixture location2 () transmitter2)
  (los-to-fixture location2 () receiver1)
  (los-to-fixture location2 () receiver2)
  (los-to-fixture location2 () receiver3)
  (los-to-fixture location3 () transmitter1)
  (los-to-fixture location3 () transmitter2)
  (los-to-fixture location3 () receiver1)
  (los-to-fixture location3 () receiver2)
  (los-to-fixture location3 () receiver3)
  (los-to-fixture location4 (gate1) transmitter1)
  (los-to-fixture location4 (gate1) transmitter2)
  (los-to-fixture location4 () receiver2)
  (los-to-fixture location4 () receiver3)

  ;; Location -> location sightlines (symmetric matching); from corner visible0/visible1
  (los-to-location location1 () location2)
  (los-to-location location1 () location3)
  (los-to-location location1 (gate1) location4)
  (los-to-location location2 () location3)
  (los-to-location location2 () location4)
  (los-to-location location3 () location4)

  ;; Walking edges (symmetric matching); from corner accessible0/accessible1
  (walk-via location1 () location2)
  (walk-via location1 () location3)
  (walk-via location1 (gate1) location4)
  (walk-via location2 () location3)
  (walk-via location2 (gate1) location4)
  (walk-via location3 (gate1) location4)

  ;; Beam crossings: 26 crossings, one object per geometric point (corner geometry).  Connector-
  ;; connector (L->L) segments are bidirectional, so beam-crossing> names an L->L beam in a canonical
  ;; direction while crossings-along-beam> is authored for both directions; beam-reaches-crossing resolves
  ;; the live orientation.  beam-crossing> names the two beams meeting at a point; crossings-along-beam>
  ;; lists a directed beam's crossings nearest-source first.
  (beam-crossing> crossing1 transmitter1 location1 transmitter2 location2)
  (beam-crossing> crossing2 transmitter1 location1 transmitter2 location3)
  (beam-crossing> crossing3 transmitter1 location1 transmitter2 location4)
  (beam-crossing> crossing4 transmitter1 location2 transmitter2 location3)
  (beam-crossing> crossing5 transmitter1 location2 location3 receiver1)
  (beam-crossing> crossing6 transmitter1 location2 location1 location3)
  (beam-crossing> crossing7 transmitter2 location2 transmitter1 location4)
  (beam-crossing> crossing8 transmitter2 location2 location3 receiver1)
  (beam-crossing> crossing9 transmitter2 location2 location1 location3)
  (beam-crossing> crossing10 transmitter2 location3 transmitter1 location4)
  (beam-crossing> crossing11 transmitter1 location4 location2 receiver1)
  (beam-crossing> crossing12 transmitter1 location4 location3 receiver1)
  (beam-crossing> crossing13 transmitter1 location4 location1 location2)
  (beam-crossing> crossing14 transmitter1 location4 location1 location3)
  (beam-crossing> crossing15 transmitter2 location4 location2 receiver1)
  (beam-crossing> crossing16 transmitter2 location4 location3 receiver1)
  (beam-crossing> crossing17 transmitter2 location4 location1 location2)
  (beam-crossing> crossing18 transmitter2 location4 location1 location3)
  (beam-crossing> crossing19 location2 receiver1 location1 location4)
  (beam-crossing> crossing20 location2 receiver2 location3 receiver3)
  (beam-crossing> crossing21 location2 receiver2 location3 location4)
  (beam-crossing> crossing22 location2 receiver3 location4 receiver2)
  (beam-crossing> crossing23 location2 receiver3 location3 location4)
  (beam-crossing> crossing24 location3 receiver1 location1 location2)
  (beam-crossing> crossing25 location3 receiver1 location1 location4)
  (beam-crossing> crossing26 location3 receiver3 location4 receiver2)

  (crossings-along-beam> location1 (crossing17 crossing13 crossing24) location2)
  (crossings-along-beam> location1 (crossing18 crossing14 crossing9 crossing6) location3)
  (crossings-along-beam> location1 (crossing25 crossing19) location4)
  (crossings-along-beam> location2 (crossing24 crossing13 crossing17) location1)
  (crossings-along-beam> location2 (crossing11 crossing15 crossing19) receiver1)
  (crossings-along-beam> location2 (crossing21 crossing20) receiver2)
  (crossings-along-beam> location2 (crossing23 crossing22) receiver3)
  (crossings-along-beam> location3 (crossing6 crossing9 crossing14 crossing18) location1)
  (crossings-along-beam> location3 (crossing21 crossing23) location4)
  (crossings-along-beam> location3 (crossing5 crossing8 crossing24 crossing12 crossing16 crossing25) receiver1)
  (crossings-along-beam> location3 (crossing20 crossing26) receiver3)
  (crossings-along-beam> location4 (crossing19 crossing25) location1)
  (crossings-along-beam> location4 (crossing23 crossing21) location3)
  (crossings-along-beam> location4 (crossing22 crossing26) receiver2)
  (crossings-along-beam> transmitter1 (crossing2 crossing1 crossing3) location1)
  (crossings-along-beam> transmitter1 (crossing4 crossing6 crossing5) location2)
  (crossings-along-beam> transmitter1 (crossing10 crossing7 crossing14 crossing13 crossing12 crossing11) location4)
  (crossings-along-beam> transmitter2 (crossing1 crossing7 crossing9 crossing8) location2)
  (crossings-along-beam> transmitter2 (crossing2 crossing10 crossing4) location3)
  (crossings-along-beam> transmitter2 (crossing3 crossing18 crossing17 crossing16 crossing15) location4)
)


(define-init-action initialize-derived-state
  ;; Derive the full derived layer (open, crossing-active, color, active) once after
  ;; define-init, so the start state is consistent by construction.  Called directly
  ;; (not as a finally followup).
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!)))


;;;; GOAL ;;;;


(define-goal
  (and (has-location agent1 location4)
       (active receiver2)
       (active receiver3)))

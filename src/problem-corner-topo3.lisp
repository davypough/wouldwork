;;; Filename: problem-corner-topo3.lisp

;;; Talos Principle problem 'Around the Corner' (Purgatory workshop 3), rebuilt from
;;; self-contained technology files, following the problem-claustro4a.lisp architecture.
;;; This is a hybrid of problem-corner.lisp's coordinate geometry and
;;; problem-corner-topo2.lisp's topological planning model: endpoint coordinates are
;;; authored once (BEAM-POSITION>), from which the nested -beam-coordinates substrate (see
;;; tech/-beam-coordinates.lisp, pulled in automatically by beam-crossing) derives
;;; CROSSINGS-ALONG-BEAM> at init time; planning itself uses only that derived topology,
;;; exactly as problem-corner-topo2.lisp does with its hand-authored version of the same
;;; relation.  This file supplies only the coordinates and the CROSSING pool size; all of
;;; the geometry math and the BEAM-CROSSING> derivation now live in the technology files.


(in-package :ww)


(ww-set *problem-name* corner-topo3)

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
;;;; crossing must still be declared in full here (its instance count can't itself be
;;;; derived -- see -beam-coordinates.lisp's header), even though its content
;;;; (crossings-along-beam>) is now fully computed from the coordinates below.


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
               crossing21 crossing22 crossing23 crossing24 crossing25 crossing26)  ;pool assigned from computed geometry during initialization
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
;;;; corner-topo3 needs beam relaying through movable connectors (beam-relay) with
;;;; crossing-based beam cutting (beam-crossing), plus the walking/sightline background
;;;; (accessibility, visibility).  Beam-relay's nested -reachability substrate supplies
;;;; identity reach, so pickup/connect require the agent's own location exactly as in
;;;; corner-topo.  Including the full reachability technology would override that default
;;;; and add reachable-via edges, but this problem has none.  beam-crossing nests in
;;;; -beam-coordinates automatically, which is what turns the BEAM-POSITION> facts below
;;;; into CROSSINGS-ALONG-BEAM>.


(include-tech gate)                  ;controls; energized; update-gate-status!
(include-tech beam-relay)            ;paired; color; pickup-connector; connect-connector
(include-tech beam-crossing)         ;crossing-active; beam-crossing>; crossings-along-beam>; beam-position>
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
  (has-chroma transmitter1 red)
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

  ;; Exact 2D endpoint coordinates from problem-corner.lisp.  -beam-coordinates.lisp's
  ;; establish-beam-coordinates init-action derives crossings-along-beam> from these
  ;; (together with the los-to-fixture/los-to-location facts above) before search begins;
  ;; the coordinates themselves drive nothing thereafter.
  (beam-position> location1 9 1)
  (beam-position> location2 9 8)
  (beam-position> location3 10 9)
  (beam-position> location4 7 8)
  (beam-position> transmitter1 11 1/10)
  (beam-position> transmitter2 10 1/10)
  (beam-position> receiver1 81/10 1)
  (beam-position> receiver2 7 109/10)
  (beam-position> receiver3 1 109/10)
)


(define-init-action initialize-derived-state
  ;; Derive the full derived layer (open, crossing-active, color, active) after
  ;; -beam-coordinates.lisp's establish-beam-coordinates has established the static
  ;; crossing topology from the coordinates above.
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

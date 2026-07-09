;;; Filename: problem-corner-topo4.lisp

;;; Talos Principle problem 'Around the Corner' (Purgatory workshop 3).  Identical to
;;; problem-corner-topo2.lisp in every respect except one: BEAM-CROSSING> is no longer
;;; hand-authored.  topo2 requires the user to maintain two independent tables describing
;;; the same 26 crossing points: BEAM-CROSSING> (which two beams meet at each crossing)
;;; and CROSSINGS-ALONG-BEAM> (each directed beam's crossings, nearest-source first) --
;;; with nothing to catch them drifting apart.  Only the ORDERING in CROSSINGS-ALONG-BEAM>
;;; is irreducible geometric content; which two beams meet at a given crossing is fully
;;; recoverable from that same table.  This file keeps CROSSINGS-ALONG-BEAM> exactly as
;;; topo2 authored it, drops BEAM-CROSSING> from DEFINE-INIT entirely, and derives it once
;;; at init time via INITIALIZE-BEAM-CROSSING-TOPOLOGY -- a symbolic grouping over
;;; CROSSINGS-ALONG-BEAM> that needs no coordinates.  A crossing that doesn't resolve to
;;; exactly two beams signals a load-time error instead of a silent inconsistency.


(in-package :ww)


(ww-set *problem-name* corner-topo4)

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
;;;; corner-topo4 needs beam relaying through movable connectors (beam-relay) with
;;;; crossing-based beam cutting (beam-crossing), plus the walking/sightline background
;;;; (accessibility, visibility).  Beam-relay's nested -reachability substrate supplies
;;;; identity reach, so pickup/connect require the agent's own location exactly as in
;;;; corner-topo.  Including the full reachability technology would override that default
;;;; and add reachable-via edges, but this problem has none.


(include-tech gate)                  ;controls; energized; update-gate-status!
(include-tech beam-relay)            ;paired; color; pickup-connector; connect-connector
(include-tech beam-crossing)         ;crossing-active; beam-crossing>; crossings-along-beam>
(include-tech accessibility)         ;walk-via; accessible; one-step-accessible; move
(include-tech visibility)            ;los-to-fixture; los-to-location; visible; visible-clear


;;;; BEAM-CROSSING DERIVATION ;;;;
;;;; BEAM-CROSSING> is not authored in this file's DEFINE-INIT.  It is derived once,
;;;; below, purely from the already-authored CROSSINGS-ALONG-BEAM> facts -- no
;;;; coordinates involved.  A crossing point necessarily lies on exactly two beams, and
;;;; CROSSINGS-ALONG-BEAM> already records, for every directed beam, which crossings lie
;;;; on it; grouping those entries by crossing id recovers the same pairing BEAM-CROSSING>
;;;; would otherwise hand-author.  Connector-connector (location-location) beams are
;;;; authored in both directions (crossings-along-beam> is consulted in whichever
;;;; direction is live at a given moment), so only one canonical direction per L-L pair is
;;;; taken here to avoid counting the same crossing from both mirrored copies -- the same
;;;; ascending-type-order convention problem-corner-topo3.lisp's CORNER-TOPO3-POTENTIAL-BEAMS
;;;; already uses for the identical purpose.  Fixed-direction beams (transmitter->location,
;;;; location->receiver) are never mirrored, so no disambiguation is needed for them.


(define-query corner-topo4-canonical-beams ()
  ;; Every authored crossings-along-beam> fact, as a (from to) pair, keeping only the
  ;; canonical direction for location-location pairs (ascending declared-type order).
  ;; Fixed-direction beams (transmitter->location, location->receiver) are never
  ;; mirrored, so they need no disambiguation.
  (do (assign $beams nil)
      (doall (?from beam-endpoint)
        (doall (?to beam-endpoint)
          (if (bind (crossings-along-beam> ?from $ids ?to))
            (if (and (location ?from) (location ?to))
              (if (member ?to (rest (member ?from (gethash 'location *types*))))
                (push (list ?from ?to) $beams))
              (push (list ?from ?to) $beams)))))
      $beams))


(define-query corner-topo4-beams-for-crossing (?crossing ?beams)
  ;; The canonical beams (drawn from ?beams) whose crossings-along-beam> list contains
  ;; ?crossing.  Correctly-authored data yields exactly two.
  (do (assign $containing nil)
      (ww-loop for $beam in ?beams
               do (assign $from (first $beam))
                  (assign $to (second $beam))
                  (bind (crossings-along-beam> $from $ids $to))
                  (if (member ?crossing $ids)
                    (push $beam $containing)))
      $containing))


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

  ;; Beam crossings: 26 crossings, one object per geometric point (corner geometry).
  ;; BEAM-CROSSING> is NOT authored here -- it is derived at init time by
  ;; INITIALIZE-BEAM-CROSSING-TOPOLOGY from the CROSSINGS-ALONG-BEAM> facts below.
  ;; Connector-connector (L->L) segments are bidirectional, so crossings-along-beam> is
  ;; authored for both directions; beam-reaches-crossing resolves the live orientation.
  ;; crossings-along-beam> lists a directed beam's crossings nearest-source first.
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


(define-init-action initialize-beam-crossing-topology
  ;; Derives BEAM-CROSSING> from the CROSSINGS-ALONG-BEAM> facts just asserted, purely
  ;; symbolically -- no coordinates.  For each declared crossing, finds the (canonical)
  ;; beams whose crossings-along-beam> list mentions it; errors if that count isn't
  ;; exactly 2, which would indicate crossings-along-beam> itself is inconsistent.
  0
  ()
  (always-true)
  ()
  (assert
    (do (assign $beams (corner-topo4-canonical-beams))
        (doall (?crossing crossing)
          (do (assign $containing (corner-topo4-beams-for-crossing ?crossing $beams))
              (if (/= (length $containing) 2)
                (error "Crossing ~A appears on ~A canonical beam(s); expected exactly 2."
                       ?crossing (length $containing)))
              (assign $beam1 (first $containing))
              (assign $beam2 (second $containing))
              (beam-crossing> ?crossing
                              (first $beam1) (second $beam1)
                              (first $beam2) (second $beam2)))))))


(define-init-action initialize-derived-state
  ;; Derive the full derived layer (open, crossing-active, color, active) after
  ;; initialize-beam-crossing-topology has established the static crossing topology.
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

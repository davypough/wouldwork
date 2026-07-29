;;; Filename: problem-reaction-order-test.lisp

;;; Two reactions in one driver, chained, in a problem small enough to solve.  Built to
;;; settle open decision 1 of Phase 3 in artifacts/propagation-order-plan.md: whether the
;;; relative order of two REACTIONS is load-bearing, or merely a matter of how many passes
;;; the fixpoint takes.
;;;
;;; Why a new problem was needed.  PROPAGATION-REPAIR-ORDER partitions the driver into
;;; derivations followed by reactions, preserving each group's authored relative order.
;;; With the driver derived rather than authored there is no relative order to preserve,
;;; so the generator needs to know whether any choice is wrong.  The propagation-order
;;; file header already argues that a reaction reading another reaction's base-fact write
;;; is mutual rather than misordered, and REPORT-PROPAGATION-VIOLATIONS therefore stays
;;; silent about it -- but that justifies not reporting the case, which is weaker than
;;; knowing how to choose.  PHOBIA is the only other problem with two reactions and does
;;; not solve, so it cannot answer the question.  Its final leg is also the floor blower's
;;; launch, which means that reaction has never run alongside a wall blower's sweep in a
;;; completed search; this problem is the first to do so.
;;;
;;; The driver is derived, not authored.  This problem carried a hand-written
;;; PROPAGATE-CONSEQUENCES! while the experiment below was run; Phase 3 stage 5 deleted it,
;;; and tech/-propagation.lisp plus INSTALL-DERIVED-PROPAGATION-DRIVER now supply it.  The
;;; derived order appends the reactions after the derivations in splice order, which for
;;; this problem reproduces the sequence the experiment measured -- floor blower, then wall
;;; blower.  Nothing here is now adjustable by editing this file; to re-run the experiment,
;;; consult the derivation rather than a driver body.
;;;
;;; The experiment, and its result.  Swapping UPDATE-FLOOR-BLOWER-STATUS! and
;;; UPDATE-WALL-BLOWER-STATUS! in the driver this problem then authored, and re-solving,
;;; produces an identical solution: the same three steps, the same final state, the same 23
;;; states processed.
;;; Both orders also report clean, as expected -- the reaction rule is narrowed to a
;;; derivation producer, and with the reactions in the graph every update collapses into
;;; one component, so no note fires either.
;;;
;;; That settles Phase 3 open decision 1: reaction order is not load-bearing, and the
;;; generated driver needs no tie-break among reactions.  It also converts the propagation-
;;; order file header's claim -- that a reaction reading another reaction's base-fact write
;;; is mutual rather than misordered -- from an argument into a measured result.
;;;
;;; What the identical statistics do NOT show is the convergence cost.  Program cycles
;;; counts search expansions, and both legs of the chain run inside a single STEP-ON's
;;; (finally (propagate-changes!)), so a difference in fixpoint passes cannot reach the
;;; search statistics.  By hand it is three passes against two: with the wall blower first,
;;; it sweeps pad1 before anything has been launched there, so box1 reaches pad1 on pass one
;;; and far on pass two, with a third pass confirming quiescence.  Measuring that would mean
;;; instrumenting PROPAGATE-CHANGES!'s iteration counter.
;;;
;;; Scope of the result: two reactions, one acyclic destination chain.  WALL-BLOWER's header
;;; already forbids cyclic chains outright -- they trip the iteration cap and land in
;;; INCONSISTENT-STATE -- so confluence is established exactly within the regime the
;;; technologies already require, and no further.
;;;
;;; How the two reactions chain.  UPDATE-FLOOR-BLOWER-STATUS! launches only what rests ON
;;; a blowing floor-mounted fan, and UPDATE-WALL-BLOWER-STATUS! sweeps whatever stands in
;;; its gears' faced location, so one does not feed the other by default: a launched
;;; object lands on the ground at the destination, and floor-blower's LOCATION-ELEVATION
;;; override floats that destination at 10, far above any wall stream.  Declaring
;;; (has-elevation pad1 0) is what couples them.  A declared elevation always wins, so
;;; pad1 becomes ordinary ground; wgears1 faces pad1 and hangs at the default stream
;;; elevation 1; and an object standing there at elevation 0 with unit height satisfies
;;; wall-blower's strike test, 0 < 1 <= 0 + 1.  So box1 travels
;;; ffan1 -> pad1 -> far, both legs inside a single call to PROPAGATE-CHANGES!.
;;;
;;; With floor before wall, both legs complete in one pass.  With wall before floor, the
;;; sweep of pad1 runs before the launch that puts anything there, so the box reaches pad1
;;; in the first pass and far in the second.  DROP-OCCUPANTS! does not interfere in the
;;; interval: ffan1 is still blowing and still aims at pad1, so nothing is dropped back.
;;;
;;; The puzzle.  Everything happens at pad0, so no walking is involved and no WALK-VIA is
;;; authored -- WALKABILITY is included only because both existing blower tests include
;;; it and the nested substrates expect it.  Expected minimum solution (3 steps): pickup
;;; box1, put it on ffan1, and step onto plate1, whose depression turns both sets of gears
;;; at once.  Stepping on the plate first works equally well and is the same length.  Both
;;; fans are welded to their gears, so PICKUP-FAN can never separate either pair and both
;;; reactions stay in the propagation graph for the whole search.


(in-package :ww)


(ww-set *problem-name* reaction-order-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 5)


;;;; TYPES ;;;;


(define-types
  agent (agent1)
  location (pad0 pad1 far)
  plate (plate1)
  box (box1)
  floor-gears (fgears1)
  wall-gears (wgears1)
  fan (ffan1 wfan1)
  mode (normal inverted)
)


;;;; TECHNOLOGY INCLUDES ;;;;


(include-tech plate)         ;depressed; update-plate-status!
(include-tech floor-blower)  ;update-floor-blower-status!; blow-occupants-away!; drop-occupants!
(include-tech wall-blower)   ;update-wall-blower-status!; sweep-occupants-away!
(include-tech box)           ;pickup-box; put-box
(include-tech step)          ;step-on; step-off
(include-tech walkability)  ;walk-via; walkable-locations; walkable; walk


;;;; INITIALIZATION ;;;;


(define-init
  ;; Movable objects.  ffan1 is floor-mounted, so it is a floor object with its own
  ;; has-location; wfan1 is wall-mounted and hangs with none.
  (has-location agent1 pad0)
  (has-location box1 pad0)
  (has-location ffan1 pad0)

  ;; Fixed-position objects.  fgears1 is flush with pad0's floor; wgears1 hangs on pad1's
  ;; wall, facing (sweeping) pad1.
  (has-position plate1 pad0)
  (has-position fgears1 pad0)
  (has-position wgears1 pad1)

  ;; Both fans start mounted and welded, an attachment rather than an (on ...) fact.
  ;; Welding keeps pickup-fan from separating either pair, so neither reaction can leave
  ;; the propagation graph mid-search.
  (mounted-on ffan1 fgears1)
  (mounted-on wfan1 wgears1)
  (welded ffan1 fgears1)
  (welded wfan1 wgears1)

  ;; The coupling between the two reactions.  pad1 is fgears1's aimed-at> destination, so
  ;; floor-blower's location-elevation override would float it at 10 -- above wgears1's
  ;; stream elevation of 1, where nothing landing there could ever be swept.  A declared
  ;; has-elevation always wins, so this one fact puts the landing pad back on the ground
  ;; and lets the wall stream strike what the floor blower delivers.
  (has-elevation pad1 0)

  ;; One plate turns both sets of gears, so a single step-on fires both reactions in the
  ;; same propagation.
  (controls ((plate1)) fgears1 normal)
  (controls ((plate1)) wgears1 normal)

  ;; Air-stream destinations, forming the chain pad0 -> pad1 -> far.
  (aimed-at> fgears1 pad1)
  (aimed-at> wgears1 far)
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
  (has-location box1 far)
)

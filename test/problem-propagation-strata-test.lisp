;;; Filename: problem-propagation-strata-test.lisp

;;; Targeted exercise for src/ww-propagation-order.lisp: the smallest problem whose
;;; MASTER PROPAGATION DRIVER carries five derivations and one reaction, assembled from
;;; a plate, a relayed beam, a controlled gate, and a wall blower.
;;;
;;; Why this problem exists.  PHOBIA is the only full problem with a reaction, and its
;;; derivations condense to {connector receiver gate} then gears -- two strata, the first
;;; of which has nothing ahead of it.  The two blower tests are reaction-bearing but
;;; single-technology, so their graphs have almost nothing to condense.  Here
;;; UPDATE-PLATE-STATUS! reads only (on ...), which no derivation writes, so once the
;;; reaction is set aside it becomes a genuine leading stratum and
;;; REPORT-DERIVATION-STRATA has three strata to report:
;;;
;;;   {update-plate-status!}
;;;     then {update-connector-status! update-receiver-status! update-gate-status!}
;;;       then {update-gears-status!}
;;;
;;; That is the second data point Phase 2 -- moving reactions out of PROPAGATE-CHANGES!'s
;;; fixpoint -- needs before it can be judged.  Run (report-derived-driver) by hand after
;;; staging and read its "derivation strata:" line; INIT never calls it.  It replaced
;;; REPORT-DERIVATION-STRATA, which read the authored driver this problem no longer has.
;;;
;;; With the reaction left in the graph, every update collapses into one component, the
;;; same way PHOBIA's do and for the same reason: SWEEP-OCCUPANTS-AWAY! writes
;;; HAS-LOCATION and (on ...), which UPDATE-CONNECTOR-STATUS! and UPDATE-PLATE-STATUS!
;;; both read, closing the cycle
;;; plate -> gate -> connector -> receiver -> gears -> blower -> plate.  So no
;;; component-boundary note can fire here, and the reaction rule is the only check with
;;; anything to say -- which is precisely the argument the file header of
;;; ww-propagation-order.lisp makes for why a component test alone would be useless.
;;;
;;; Two deliberate perturbations, for confirming the checks still fire.  Phase 3 stage 5
;;; deleted this problem's authored driver, so both now require restoring one first: paste
;;; the body below back into this file, below the technology includes.  A definition here is
;;; spliced after tech/-propagation.lisp's sentinel and overrides it, and
;;; INSTALL-DERIVED-PROPAGATION-DRIVER then leaves it alone -- which is what makes the
;;; perturbations still performable at all.
;;;
;;;   (define-update propagate-consequences! ()
;;;     (let ((*propagated-state-changed* nil))
;;;       (update-connector-status!)
;;;       (update-receiver-status!)
;;;       (update-plate-status!)
;;;       (update-gate-status!)
;;;       (update-gears-status!)
;;;       (update-wall-blower-status!)
;;;       *propagated-state-changed*))
;;;
;;;   - move UPDATE-GEARS-STATUS! below UPDATE-WALL-BLOWER-STATUS!.  The blower is a
;;;     reaction reading BLOWING from a derivation the driver now calls later, so INIT
;;;     halts with a propagation order error and prints a repaired body that restores
;;;     the authored order.
;;;   - delete UPDATE-PLATE-STATUS! from the driver.  DEPRESSED is then never derived,
;;;     the gate never opens, and the problem becomes unsolvable rather than
;;;     misdiagnosed -- the coverage gap the order analysis does NOT detect, recorded
;;;     here so the distinction stays visible.  Note this one cannot be reproduced by
;;;     deleting a line from the derived driver: the derivation assembles the candidate set
;;;     from the spliced technologies, so dropping an update means dropping its technology.
;;;
;;; Two translator-pruning paths are exercised together, which no single existing problem
;;; does.  JAMMER is empty, so UPDATE-GATE-STATUS!'s and UPDATE-GEARS-STATUS!'s
;;; (exists (?j jammer) (jamming ?j ...)) is dropped by TRANSLATE-EMPTY-STATIC-QUANTIFIER
;;; and must not appear as a read.  PLATE is nonempty, so ENERGIZED's
;;; (and (plate ?c) (depressed ?c)) disjunct survives and DEPRESSED must appear as a read.
;;;
;;; The puzzle.  Three locations.  WEST holds the agent, the box, the connector, and
;;; plate1; MID is the location wgears1 faces and sweeps; EAST is the air stream's
;;; destination and has no walking edge at all -- the stream is the only way in.  The
;;; beam is live from the first pass (the pairings are authored rather than built by
;;; CONNECT-CONNECTOR), but gate1 occludes the sightline from WEST to receiver1, so the
;;; connector is lit and the receiver is dark until the plate opens the gate.
;;; Any support occupant depresses the plate, so there are two 3-step routes and both end
;;; with move WEST to MID, where the sweep carries the agent on to EAST.  The one the
;;; search reports is pickup-connector, then connect-connector -- CONNECT-CONNECTOR places
;;; the held connector through PLACEMENT-OPTIONS, and plate1 is a legal placement, so the
;;; connector re-pairs and settles onto the plate in the same step, depressing it.  The
;;; box route (pickup-box, put-box box1 on plate1) is the same length and leaves the
;;; authored pairings untouched; box1 exists to keep it available, since it is the route
;;; that isolates the plate from the beam.  Stepping on the plate is one step shorter and
;;; a dead end either way: MOVE refuses to walk off a support, and stepping off clears the
;;; plate and shuts the gate again.


(in-package :ww)


(ww-set *problem-name* propagation-strata-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 6)


(defparameter *max-pairings* 2)  ;max termini a connector may pair in one connect (beam-relay's connect-connector)


;;;; TYPES ;;;;


(define-types
  agent (agent1)
  location (west mid east)
  plate (plate1)
  box (box1)
  connector (connector1)
  transmitter (transmitter1)
  receiver (receiver1)
  gate (gate1)
  wall-gears (wgears1)
  fan (fan1)
  hue (blue)
  mode (normal inverted)
)


;;;; TECHNOLOGY INCLUDES ;;;;


(include-tech plate)         ;depressed; update-plate-status!
(include-tech gate)          ;controls; energized; update-gate-status!
(include-tech beam-relay)    ;paired; color; update-connector-status!; update-receiver-status!
(include-tech wall-blower)   ;turning; blowing; update-gears-status!; update-wall-blower-status!
(include-tech box)           ;pickup-box; put-box
(include-tech step)          ;step-on; step-off
(include-tech visibility)    ;los-to-apparatus; visible; visible-clear
(include-tech accessibility) ;walk-via; accessible; move


;;;; INITIALIZATION ;;;;


(define-init
  ;; Movable objects; fan1 is wall-mounted, so it has no has-location.
  (has-location agent1 west)
  (has-location box1 west)
  (has-location connector1 west)

  ;; Fixed-position objects; wgears1 hangs on mid's wall, facing (sweeping) mid.
  (has-position plate1 west)
  (has-position wgears1 mid)

  ;; The fan starts mounted on the wall gears and is welded to them, so pickup-fan can
  ;; never separate the pair and the blower stays in the propagation graph throughout.
  (mounted-on fan1 wgears1)
  (welded fan1 wgears1)

  ;; Walking topology.  All three locations are ordinary ground (elevation 0), and wgears1
  ;; declares no has-elevation, so its stream works at the default elevation 1 and strikes
  ;; anything standing on mid's floor.  east has no walking edge: the stream is the only
  ;; way in, and nothing walks back out.
  (walk-via west ((gate1)) mid)

  ;; Beam wiring.  The pairings are authored rather than built by connect-connector, so
  ;; the relay chain transmitter1 -> connector1 -> receiver1 is live from the first pass
  ;; and the puzzle turns on the sightline rather than on assembling the beam.
  (has-chroma transmitter1 blue)
  (has-chroma receiver1 blue)
  (paired connector1 transmitter1)
  (paired connector1 receiver1)

  ;; Sightlines.  Hand-authored: with no wall-segments asserted, visibility's nested
  ;; -beam-los-coordinates derivation is inert and these facts stand as written.  west
  ;; sees transmitter1 directly; the sightline from west to receiver1 runs through gate1,
  ;; which is what makes the plate load-bearing.
  (los-to-apparatus west () transmitter1)
  (los-to-apparatus west (gate1) receiver1)

  ;; Control wiring: the plate opens the gate, the receiver turns the gears.
  (controls ((plate1)) gate1 normal)
  (controls ((receiver1)) wgears1 normal)

  ;; Air-stream destination
  (aimed-at> wgears1 east)
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
  (has-location agent1 east)
)

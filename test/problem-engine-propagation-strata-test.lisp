;;; Filename: problem-engine-propagation-strata-test.lisp

;;; Targeted exercise for src/ww-propagation-order.lisp: the smallest problem whose
;;; MASTER PROPAGATION DRIVER carries six derivations and one reaction, assembled from
;;; a plate, a relayed beam, a controlled gate, a wall blower, and walkability's threat
;;; safety check.
;;;
;;; Why this problem exists.  PHOBIA is the only full problem with a reaction, and its
;;; derivations condense to {gate relay receiver} then gears -- two strata, the first
;;; of which has nothing ahead of it.  The two blower tests are reaction-bearing but
;;; single-technology, so their graphs have almost nothing to condense.  Here
;;; UPDATE-PLATE-STATUS! reads only (on ...), which no derivation writes, so once the
;;; reaction is set aside it becomes a genuine leading stratum and
;;; REPORT-DERIVATION-STRATA has four strata to report:
;;;
;;;   {update-plate-status!}
;;;     then {update-gate-status! update-relay-status! update-receiver-status!}
;;;       then {update-gears-status!}
;;;         then {enforce-threat-safety!}
;;;
;;; The characterization query checks that structure automatically, including the exact
;;; candidate set, derived order, strata, and installed driver body.  REPORT-DERIVED-DRIVER
;;; remains useful for inspection, but passing this problem no longer depends on reading
;;; its output by hand.
;;;
;;; With the reaction left in the graph, the five core derivations and reaction collapse
;;; into one component, the same way PHOBIA's do and for the same reason:
;;; SWEEP-OCCUPANTS-AWAY! writes HAS-LOCATION and (on ...), which
;;; UPDATE-RELAY-STATUS! and UPDATE-PLATE-STATUS! both read, closing the cycle
;;; plate -> gate -> connector -> receiver -> gears -> blower -> plate.  So no
;;; component-boundary note can fire for that core chain, and the reaction rule is the
;;; only check with anything to say there.  ENFORCE-THREAT-SAFETY! remains an independent
;;; derivation, contributed by walkability.
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
;;;       (update-plate-status!)
;;;       (update-gate-status!)
;;;       (update-relay-status!)
;;;       (update-receiver-status!)
;;;       (update-gears-status!)
;;;       (enforce-threat-safety!)
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
;;; with walk WEST to MID, where the sweep carries the agent on to EAST.  The one the
;;; search reports is pickup-connector, then connect-connector -- CONNECT-CONNECTOR places
;;; the held connector through PLACEMENT-OPTIONS, and plate1 is a legal placement, so the
;;; connector re-pairs and settles onto the plate in the same step, depressing it.  The
;;; box route (pickup-box, put-box box1 on plate1) is the same length and leaves the
;;; authored pairings untouched; box1 exists to keep it available, since it is the route
;;; that isolates the plate from the beam.  Stepping on the plate is one step shorter and
;;; a dead end either way: WALK refuses to leave a support, and stepping off clears the
;;; plate and shuts the gate again.


(in-package :ww)


(ww-set *problem-name* engine-propagation-strata-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 3)

(setf *expected-min-length* 3)


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
(include-tech beam-relay)    ;paired; color; update-relay-status!; update-receiver-status!
(include-tech wall-blower)   ;turning; blowing; update-gears-status!; update-wall-blower-status!
(include-tech box)           ;pickup-box; put-box
(include-tech step)          ;step-on; step-off
(include-tech visibility)    ;los-to-apparatus; visible; visible-clear
(include-tech walkability)  ;walk-via; walkable-locations; walkable; walk


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


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(defun propagation-strata-structure-valid-p ()
  (let* ((expected-order
           '(update-plate-status!
             update-gate-status!
             update-relay-status!
             update-receiver-status!
             update-gears-status!
             enforce-threat-safety!
             update-wall-blower-status!))
         (expected-strata
           '((update-plate-status!)
             (update-gate-status!
              update-relay-status!
              update-receiver-status!)
             (update-gears-status!)
             (enforce-threat-safety!)))
         (candidates
           (remove-if #'update-quantifies-only-over-empty-types-p
                      (driver-candidate-updates)))
         (gate-sets
           (multiple-value-list
             (propagation-relation-sets 'update-gate-status!)))
         (gears-sets
           (multiple-value-list
             (propagation-relation-sets 'update-gears-status!)))
         (gate-reads (first gate-sets))
         (gears-reads (first gears-sets)))
    (multiple-value-bind (derived component-alist strata)
        (derived-propagation-order candidates)
      (declare (ignore component-alist))
      (and (subsetp candidates expected-order :test #'eq)
           (subsetp expected-order candidates :test #'eq)
           (equal derived expected-order)
           (equal strata expected-strata)
           (equal (get 'propagate-consequences! :raw-body)
                  (derived-propagation-driver-body expected-order))
           (gethash 'depressed gate-reads)
           (gethash 'depressed gears-reads)
           (not (gethash 'jamming gate-reads))
           (not (gethash 'jamming gears-reads))))))


(define-query propagation-strata-scenario-valid ()
  (and (propagation-strata-structure-valid-p)
       (has-location agent1 east)
       (not (on agent1 plate1))
       (depressed plate1)
       (or (on connector1 plate1)
           (on box1 plate1))
       (color connector1 blue)
       (active receiver1)
       (open gate1)
       (turning wgears1)
       (blowing fan1)
       (mounted-on fan1 wgears1)
       (welded fan1 wgears1)))


(define-goal
  (propagation-strata-scenario-valid))

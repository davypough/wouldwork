;;; Filename: problem-beam-relay-test.lisp

;;; Dedicated connector-action regression for beam-relay.lisp.  The required lifecycle
;;; and independent characterization fixtures exercise:
;;;
;;;   1. PICKUP-CONNECTOR removing its support occupancy, its outgoing pairing, and a
;;;      reverse-oriented pairing where another connector names it as terminus.
;;;   2. CONNECT-CONNECTOR selecting exactly two useful termini from three structurally
;;;      eligible choices at *MAX-PAIRINGS* = 2, then propagating color to a receiver.
;;;   3. PUT-CONNECTOR placing an independently held connector on a plate without pairings.
;;;   4. CONNECTABLE-LOCATION rejecting a site occupied by a lit connector while accepting
;;;      sites occupied only by unlit connectors.
;;;   5. CONNECTABLE-TERMINUS using structural visibility: a transmitter behind a closed
;;;      gate is eligible for pairing but not live visibility.  A no-LOS transmitter, the
;;;      connector itself, and a connector already at the placement location are rejected.
;;;
;;; Initial state: lifecycle-connector rests on old-plate at origin, paired outward to
;;; old-source and named inward by incoming-connector; placement-connector is held by
;;; put-agent; lit-blocker-connector is green; the lifecycle receiver is inactive.
;;;
;;; Final state: lifecycle-connector is on bare ground at target, paired only to new-source
;;; and new-receiver, red, and activating new-receiver.  old-plate is clear.  The old,
;;; incoming, decoy, self, and other unintended pairings remain absent.
;;; placement-connector rests unpaired and unlit on put-plate, depressing it.
;;;
;;; Expected minimum solution (3 steps): PICKUP-CONNECTOR and CONNECT-CONNECTOR for the
;;; lifecycle fixture, plus PUT-CONNECTOR for the independent fixture.  The put may occur
;;; anywhere relative to the two ordered lifecycle actions.


(in-package :ww)


(ww-set *problem-name* beam-relay-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 3)

(setf *expected-min-length* 3)


(defparameter *max-pairings* 2)


;;;; TYPES ;;;;


(define-types
  agent (relay-agent put-agent)
  location (origin target old-view incoming-site
            put-origin put-site lit-site dark-site)
  plate (old-plate put-plate)
  connector (lifecycle-connector incoming-connector placement-connector
             lit-blocker-connector dark-connector target-unlit-connector)
  transmitter (old-source new-source decoy-source invisible-source blocker-source)
  receiver (new-receiver)
  gate (closed-gate)
  hue (red blue green amber))


;;;; TECHNOLOGY INCLUDES ;;;;


(include-tech plate)
(include-tech beam-relay)
(include-tech visibility)
(include-tech reachability)


;;;; INITIALIZATION ;;;;


(define-init
  ;; Pickup lifecycle.  The two differently oriented pairings force pickup to clear both
  ;; of its pairing-removal loops.  OLD-VIEW supplies validator-visible topology without
  ;; making OLD-SOURCE connectable from the relay agent's actual vantage at ORIGIN.
  (has-location relay-agent origin)
  (has-position old-plate origin)
  (has-location lifecycle-connector origin)
  (on lifecycle-connector old-plate)
  (has-location incoming-connector incoming-site)
  (paired lifecycle-connector old-source)
  (paired incoming-connector lifecycle-connector)
  (has-chroma old-source amber)
  (los-to-apparatus old-view () old-source)
  (los-to-location old-view () origin)

  ;; The reconnect destination is within manipulation reach.  From ORIGIN exactly three
  ;; apparatus termini are structurally visible: the useful red source/receiver pair and
  ;; a blue decoy behind CLOSED-GATE.  Live sightlines from TARGET exist for the useful
  ;; pair; the decoy remains gate-blocked there too.
  (reach-via origin () target)
  (has-chroma new-source red)
  (has-chroma new-receiver red)
  (has-chroma decoy-source blue)
  (has-chroma invisible-source green)
  (los-to-apparatus origin () new-source)
  (los-to-apparatus origin () new-receiver)
  (los-to-apparatus origin (closed-gate) decoy-source)
  (los-to-apparatus target () new-source)
  (los-to-apparatus target () new-receiver)
  (los-to-apparatus target (closed-gate) decoy-source)

  ;; An unlit connector already at TARGET does not block placement, but is not itself a
  ;; connectable terminus there because connector-to-connector links require distinct
  ;; locations.  DARK-CONNECTOR supplies the same unlit-location positive branch elsewhere.
  (has-location target-unlit-connector target)
  (has-location dark-connector dark-site)

  ;; A separately lit connector makes LIT-SITE unavailable to another connector.
  (has-location lit-blocker-connector lit-site)
  (paired lit-blocker-connector blocker-source)
  (has-chroma blocker-source green)
  (los-to-apparatus lit-site () blocker-source)

  ;; Independent PUT-CONNECTOR fixture.  It begins held, has a reachable destination plate,
  ;; and has no pairing sightlines, so only the ordinary unpaired placement is relevant.
  (has-location put-agent put-origin)
  (holding put-agent placement-connector)
  (has-position put-plate put-site)
  (reach-via put-origin () put-site))


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!)))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query beam-relay-scenarios-valid ()
  (and
    ;; Pickup followed by reconnect: location/support/hold lifecycle and exact pairing set.
    (has-location relay-agent origin)
    (not (holding relay-agent lifecycle-connector))
    (has-location lifecycle-connector target)
    (not (exists (?support support)
           (on lifecycle-connector ?support)))
    (paired lifecycle-connector new-source)
    (paired lifecycle-connector new-receiver)
    (not (paired lifecycle-connector old-source))
    (not (paired incoming-connector lifecycle-connector))
    (not (paired lifecycle-connector decoy-source))
    (not (paired lifecycle-connector invisible-source))
    (not (paired lifecycle-connector incoming-connector))
    (not (paired lifecycle-connector placement-connector))
    (not (paired lifecycle-connector lit-blocker-connector))
    (not (paired lifecycle-connector dark-connector))
    (not (paired lifecycle-connector target-unlit-connector))

    ;; Reconnection propagates the unique incoming hue to the matching receiver.
    (color lifecycle-connector red)
    (active new-receiver)
    (cleartop old-plate)
    (not (depressed old-plate))

    ;; PUT-CONNECTOR releases the independent hold, places on the chosen support, and
    ;; creates no outgoing or incoming pairings and no derived color.
    (has-location put-agent put-origin)
    (not (holding put-agent placement-connector))
    (has-location placement-connector put-site)
    (on placement-connector put-plate)
    (depressed put-plate)
    (not (exists (?terminus terminus)
           (paired placement-connector ?terminus)))
    (not (exists (?connector connector)
           (paired ?connector placement-connector)))
    (not (exists (?h hue)
           (color placement-connector ?h)))

    ;; A different lit connector blocks its site.  Unlit occupants do not block either
    ;; DARK-SITE or TARGET, even though TARGET already contains another connector.
    (color lit-blocker-connector green)
    (not (connectable-location lifecycle-connector lit-site))
    (not (exists (?h hue)
           (color dark-connector ?h)))
    (connectable-location lifecycle-connector dark-site)
    (not (exists (?h hue)
           (color target-unlit-connector ?h)))
    (connectable-location lifecycle-connector target)

    ;; Structural pairing eligibility versus live beam visibility, plus negative branches.
    (potentially-visible origin decoy-source)
    (not (visible origin decoy-source))
    (not (open closed-gate))
    (connectable-terminus
      '(origin) target lifecycle-connector new-source)
    (connectable-terminus
      '(origin) target lifecycle-connector new-receiver)
    (connectable-terminus
      '(origin) target lifecycle-connector decoy-source)
    (not (connectable-terminus
           '(origin) target lifecycle-connector invisible-source))
    (not (connectable-terminus
           '(origin) target lifecycle-connector lifecycle-connector))
    (not (connectable-terminus
           '(origin) target lifecycle-connector target-unlit-connector))))


(define-goal
  (beam-relay-scenarios-valid))

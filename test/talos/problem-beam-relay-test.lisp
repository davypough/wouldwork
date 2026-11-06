;;; Filename: problem-beam-relay-test.lisp

;;; Dedicated connector-action regression for beam-relay.lisp.  The required lifecycle
;;; and independent characterization fixtures exercise:
;;;
;;;   1. PICKUP-CONNECTOR removing its support occupancy, its outgoing pairing, and a
;;;      reverse-oriented pairing where another connector names it as terminus.
;;;   2. SELECT-TERMINI choosing exactly two useful termini from three structurally
;;;      eligible choices at *MAX-CONNECTOR-PAIRINGS* = 2, and PUT-CONNECTOR pairing them at
;;;      put-down, then propagating color to a receiver.
;;;   3. PUT-CONNECTOR placing an independently held connector on a plate without pairings.
;;;   4. CONNECTABLE-LOCATION rejecting a site occupied by a lit connector while accepting
;;;      sites occupied only by unlit connectors.
;;;   5. CONNECTABLE-TERMINUS using structural visibility: a transmitter behind a closed
;;;      gate is eligible for selection but not live visibility.  A no-LOS transmitter, the
;;;      connector itself, and a terminus the connector is already linked to are rejected.
;;;   6. A selection surviving a one-way drop: drop-agent selects ledge-source from LEDGE,
;;;      jumps down a directed edge to FLOOR, from which ledge-source is not even
;;;      structurally visible, and puts down there.  The pairing forms but carries no color.
;;;   7. PUT-CONNECTOR refusing a put-down at the location of a selected connector terminus:
;;;      a stationary probe whose held connector already selects SITE-CONNECTOR, checked on
;;;      the generated put-down children of the start state.  Put-down at PROBE-SITE, where
;;;      SITE-CONNECTOR stands, is absent; put-down at PROBE-ALT pairs and clears the
;;;      selection.
;;;
;;; Initial state: lifecycle-connector rests on old-plate at origin, paired outward to
;;; old-source and named inward by incoming-connector; placement-connector is held by
;;; put-agent; lit-blocker-connector is green; the lifecycle receiver is inactive.
;;;
;;; Final state: lifecycle-connector is on bare ground at target, paired only to new-source
;;; and new-receiver, red, and activating new-receiver.  old-plate is clear.  The old,
;;; incoming, decoy, self, and other unintended pairings remain absent.
;;; placement-connector rests unpaired and unlit on put-plate, depressing it.
;;; drop-connector rests at floor, paired to ledge-source and unlit.  Neither connector
;;; retains a SELECTED fact.  The stationary probe is unconstrained by the goal.
;;;
;;; Expected minimum solution (7 steps): PICKUP-CONNECTOR, SELECT-TERMINI and
;;; PUT-CONNECTOR for the lifecycle fixture; PUT-CONNECTOR for the independent fixture;
;;; SELECT-TERMINI, MOVE and PUT-CONNECTOR for the drop fixture.


(in-package :ww)


(ww-set *problem-name* beam-relay-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 7)

(setf *expected-min-length* 7)


(ww-set *max-connector-pairings* 2)


;;;; TYPES ;;;;


(define-types
  agent (relay-agent put-agent drop-agent probe-agent)
  location (origin target old-view incoming-site
            put-origin put-site lit-site dark-site ledge floor
            probe-origin probe-site probe-alt)
  pressure-plate (old-plate put-plate)
  connector (lifecycle-connector incoming-connector placement-connector
             lit-blocker-connector dark-connector target-unlit-connector
             drop-connector probe-connector site-connector)
  transmitter (old-source new-source decoy-source invisible-source blocker-source
               ledge-source)
  receiver (new-receiver)
  gate (closed-gate)
  edge (drop-edge)
  hue (red blue green amber))


;;;; TECHNOLOGY INCLUDES ;;;;


(include-tech plate)
(include-tech beam-relay)
(include-tech visibility)
(include-tech reachability)
(include-tech jump)


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
  (los-via old-view () old-source)
  (los-via old-view () origin)

  ;; The reconnect destination is within manipulation reach.  From ORIGIN exactly three
  ;; apparatus termini are structurally visible: the useful red source/receiver pair and
  ;; a blue decoy behind CLOSED-GATE.  Live sightlines from TARGET exist for the useful
  ;; pair; the decoy remains gate-blocked there too.
  (reach-via origin () target)
  (has-chroma new-source red)
  (has-chroma new-receiver red)
  (has-chroma decoy-source blue)
  (has-chroma invisible-source green)
  (los-via origin () new-source)
  (los-via origin () new-receiver)
  (los-via origin (closed-gate) decoy-source)
  (los-via target () new-source)
  (los-via target () new-receiver)
  (los-via target (closed-gate) decoy-source)

  ;; An unlit connector already at TARGET does not block placement, but is not itself a
  ;; connectable terminus there because connector-to-connector links require distinct
  ;; locations.  DARK-CONNECTOR supplies the same unlit-location positive branch elsewhere.
  (has-location target-unlit-connector target)
  (has-location dark-connector dark-site)

  ;; A separately lit connector makes LIT-SITE unavailable to another connector.
  (has-location lit-blocker-connector lit-site)
  (paired lit-blocker-connector blocker-source)
  (has-chroma blocker-source green)
  (los-via lit-site () blocker-source)

  ;; Independent PUT-CONNECTOR fixture.  It begins held, has a reachable destination plate,
  ;; and has no pairing sightlines, so only the ordinary unpaired placement is relevant.
  (has-location put-agent put-origin)
  (holding put-agent placement-connector)
  (has-position put-plate put-site)
  (reach-via put-origin () put-site)
  ;; Drop fixture.  Only LEDGE sees ledge-source; the directed edge allows no return.
  (has-location drop-agent ledge)
  (holding drop-agent drop-connector)
  (traverse-via> ledge ((drop-edge)) floor)
  (has-chroma ledge-source blue)
  (los-via ledge () ledge-source)
  ;; Same-site put-down probe.  It remains stationary; a claim inspects the start state's
  ;; generated PUT-CONNECTOR children.  The selection is authored directly because no
  ;; sightline is needed to test the put-down restriction.
  (has-location probe-agent probe-origin)
  (holding probe-agent probe-connector)
  (selected probe-connector site-connector)
  (has-location site-connector probe-site)
  (reach-via probe-origin () probe-site)
  (reach-via probe-origin () probe-alt))


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
    (not (support-occupied old-plate))
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
    ;; NEW-SOURCE and NEW-RECEIVER are already linked here, so they are not offered again.
    (not (connectable-terminus
           relay-agent origin lifecycle-connector new-source))
    (not (connectable-terminus
           relay-agent origin lifecycle-connector new-receiver))
    (connectable-terminus
      relay-agent origin lifecycle-connector decoy-source)
    (not (connectable-terminus
           relay-agent origin lifecycle-connector invisible-source))
    (not (connectable-terminus
           relay-agent origin lifecycle-connector lifecycle-connector))

    ;; A selection made on LEDGE survives the drop and pairs at put-down on FLOOR, where
    ;; ledge-source is not even structurally visible, so the link carries no color.
    (has-location drop-agent floor)
    (not (holding drop-agent drop-connector))
    (has-location drop-connector floor)
    (paired drop-connector ledge-source)
    (not (potentially-visible floor ledge-source))
    (not (exists (?h hue)
           (color drop-connector ?h)))
    (not (exists (?t terminus)
           (or (selected lifecycle-connector ?t)
               (selected drop-connector ?t))))))


;;;; GENERATED-CHILD CHARACTERIZATION ;;;;


(define-test-claim beam-relay-selected-connector-site-put-down
  ;; No put-down lands the probe connector beside its selected connector terminus.
  (notany (lambda (child)
            (beam-relay-child-matches-p
              child
              '((has-location probe-connector probe-site))
              nil))
          (beam-relay-put-connector-children *start-state*))
  ;; Elsewhere the put-down is offered, converts the selection into a pairing, and
  ;; clears it.
  (some (lambda (child)
          (beam-relay-child-matches-p
            child
            '((has-location probe-connector probe-alt)
              (paired probe-connector site-connector))
            '((selected probe-connector site-connector))))
        (beam-relay-put-connector-children *start-state*)))


(define-test-helper beam-relay-put-connector-children (state)
  "Return STATE's generated PUT-CONNECTOR children."
  (let ((put-action (find 'put-connector *actions* :key #'action.name))
        (saved-dropped-count *inconsistent-states-dropped*))
    (unwind-protect
      (let ((*actions* (list put-action)))
        (generate-children
          (make-node :state state :depth 0)))
      (setf *inconsistent-states-dropped* saved-dropped-count))))


(define-test-helper beam-relay-child-matches-p (child required-facts absent-facts)
  (let ((facts (database child)))
    (and (every (lambda (fact)
                  (member fact facts :test #'equal))
                required-facts)
         (notany (lambda (fact)
                   (member fact facts :test #'equal))
                 absent-facts))))


(define-test-claim beam-relay-pairing-limit-override
  (= *max-connector-pairings* 2))


(define-test-claim beam-relay-positive-topology-validation
  (null
    (validate-init-literals
      '((has-location lifecycle-connector origin)
        (paired lifecycle-connector old-source)
        (los-via old-view () old-source))
      :checks '(beam-relay-init-check)))
  ;; An explicit absence is not an authored pairing and imposes no pairing invariants.
  (null
    (validate-init-literals
      '((not (paired lifecycle-connector old-source)))
      :checks '(beam-relay-init-check)))
  ;; Likewise, a negative location cannot satisfy a positive pairing's location requirement.
  (expect-condition
    (lambda ()
      (validate-init-literals
        '((not (has-location lifecycle-connector origin))
          (paired lifecycle-connector old-source)
          (los-via old-view () old-source))
        :checks '(beam-relay-init-check)))
    'init-check-failure
    :containing "PAIRED connector has no HAS-LOCATION"
    :check 'beam-relay-init-check)
  ;; Nor can an explicitly absent sightline satisfy a positive apparatus pairing.
  (expect-condition
    (lambda ()
      (validate-init-literals
        '((has-location lifecycle-connector origin)
          (paired lifecycle-connector old-source)
          (not (los-via old-view () old-source)))
        :checks '(beam-relay-init-check)))
    'init-check-failure
    :containing "target has no potential LOS-VIA"
    :check 'beam-relay-init-check))


(define-goal
  (beam-relay-scenarios-valid))

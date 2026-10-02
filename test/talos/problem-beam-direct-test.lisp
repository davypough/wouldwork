;;; Filename: problem-beam-direct-test.lisp

;;; Combined stageable regression for beam-direct.lisp.  Independent fixed-beam lanes
;;; characterize:
;;;
;;;   1. Matching chroma through an empty corridor.
;;;   2. An all-clear multi-obstacle corridor containing an open gate and a location.
;;;   3. A mixed open/closed-gate corridor, where one closed gate blocks the whole beam.
;;;   4. Sloped direct beams whose midpoint elevation is exactly 2: a height-1 blocker
;;;      clears, while a height-2 blocker blocks at its inclusive upper boundary.
;;;   5. Directional endpoint roles: transmitter -> repeater is direct cutting liveness,
;;;      while a structurally clear repeater -> receiver corridor is not.
;;;   6. Receiver lifecycle: a final test-local action moves a unit-height blocker into a
;;;      horizontal elevation-1 corridor and normal propagation deactivates the receiver.
;;;
;;; COUPLED and BEAM-VIA must be authored as matching pairs, and a coupled
;;; transmitter -> receiver pair must have present, matching HAS-CHROMA values.  Those are
;;; initialization invariants checked before a problem can stage, so this file uses only
;;; valid fixed-beam declarations and characterizes their runtime behavior.
;;;
;;; The stable lanes are checked by one characterization query used directly by the goal.
;;; OBSTRUCT-LIFECYCLE-BEAM is applicable only while its initially clear receiver is active,
;;; so broken positive arrival cannot solve the problem; the final goal requires that same
;;; receiver inactive after exact-boundary occlusion, so broken removal cannot solve it
;;; either.  The expected minimum solution is exactly 1 action.

(in-package :ww)


(ww-set *problem-name* beam-direct-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 1)

(setf *expected-min-length* 1)


;;;; TYPES ;;;;


(define-types
  agent (test-agent)
  location (idle
            mixed-clear-mid
            sloped-clear-mid
            sloped-blocked-mid
            lifecycle-staging
            lifecycle-mid)
  transmitter (clear-source
               mixed-clear-source
               closed-gate-source
               sloped-clear-source
               sloped-blocked-source
               repeater-target-source
               lifecycle-source)
  receiver (clear-receiver
            mixed-clear-receiver
            closed-gate-receiver
            sloped-clear-receiver
            sloped-blocked-receiver
            repeater-origin-receiver
            lifecycle-receiver)
  wall-repeater (repeater-target repeater-origin)
  gate (open-gate closed-gate)
  box (mixed-clear-blocker
       sloped-clear-blocker
       sloped-blocked-blocker
       lifecycle-blocker)
  hue (red blue))


;;;; TECHNOLOGY INCLUDES ;;;;


(include-tech beam-direct)
(include-tech visibility)


;;;; INITIALIZATION ;;;;


(define-init
  (has-location test-agent idle)

  ;; The shared open gate is a positive gate-branch fixture; closed-gate remains absent
  ;; from OPEN.
  (open open-gate)

  ;; Matching chroma and a vacuously clear empty corridor.
  (has-chroma clear-source red)
  (has-chroma clear-receiver red)
  (coupled clear-source clear-receiver)
  (beam-via clear-source () clear-receiver)

  ;; Every obstacle in this corridor clears: the gate is open, and the horizontal beam
  ;; runs at elevation 2 above a unit-height blocker.
  (has-chroma mixed-clear-source red)
  (has-chroma mixed-clear-receiver red)
  (has-elevation mixed-clear-source 2)
  (has-elevation mixed-clear-receiver 2)
  (has-location mixed-clear-blocker mixed-clear-mid)
  (has-height mixed-clear-blocker 1)
  (coupled mixed-clear-source mixed-clear-receiver)
  (beam-via
    mixed-clear-source
    (open-gate mixed-clear-mid)
    mixed-clear-receiver)

  ;; One closed gate blocks an otherwise open-gated corridor.
  (has-chroma closed-gate-source red)
  (has-chroma closed-gate-receiver red)
  (coupled closed-gate-source closed-gate-receiver)
  (beam-via
    closed-gate-source
    (open-gate closed-gate)
    closed-gate-receiver)

  ;; Both sloped beams rise from elevation 1 to 3.  Their midpoint elevation is exactly 2.
  ;; The first blocker ends at elevation 1; the second ends exactly at elevation 2.
  (has-chroma sloped-clear-source red)
  (has-chroma sloped-clear-receiver red)
  (has-location sloped-clear-blocker sloped-clear-mid)
  (has-height sloped-clear-blocker 1)
  (coupled sloped-clear-source sloped-clear-receiver)
  (beam-via sloped-clear-source (sloped-clear-mid) sloped-clear-receiver)
  (apparatus-coords> sloped-clear-source 0 0 1)
  (location-coords> sloped-clear-mid 5 0)
  (apparatus-coords> sloped-clear-receiver 10 0 3)

  (has-chroma sloped-blocked-source red)
  (has-chroma sloped-blocked-receiver red)
  (has-location sloped-blocked-blocker sloped-blocked-mid)
  (has-height sloped-blocked-blocker 2)
  (coupled sloped-blocked-source sloped-blocked-receiver)
  (beam-via
    sloped-blocked-source
    (sloped-blocked-mid)
    sloped-blocked-receiver)
  (apparatus-coords> sloped-blocked-source 0 10 1)
  (location-coords> sloped-blocked-mid 5 10)
  (apparatus-coords> sloped-blocked-receiver 10 10 3)

  ;; A transmitter -> repeater corridor is direct cutting liveness.  In the reverse role,
  ;; a repeater remains a valid fixed-beam source for corridor structure, but beam-direct's
  ;; cutting-liveness hook deliberately admits transmitter origins only.
  (has-chroma repeater-target-source red)
  (apparatus-coords> repeater-target 10 20)
  (coupled repeater-target-source repeater-target)
  (beam-via repeater-target-source () repeater-target)

  (has-chroma repeater-origin-receiver red)
  (apparatus-coords> repeater-origin 0 20)
  (coupled repeater-origin repeater-origin-receiver)
  (beam-via repeater-origin () repeater-origin-receiver)

  ;; Initially the unit blocker is outside this default-elevation-1 beam.  The one test
  ;; action moves it to lifecycle-mid and triggers the normal receiver-status propagation.
  (has-chroma lifecycle-source red)
  (has-chroma lifecycle-receiver red)
  (has-location lifecycle-blocker lifecycle-staging)
  (has-height lifecycle-blocker 1)
  (coupled lifecycle-source lifecycle-receiver)
  (beam-via lifecycle-source (lifecycle-mid) lifecycle-receiver))


;;;; LIFECYCLE TRIGGER ;;;;


(define-action obstruct-lifecycle-beam
  1
  ()
  (and (has-location lifecycle-blocker lifecycle-staging)
       (fixed-beam-corridor-clear lifecycle-source lifecycle-receiver)
       (direct-beam-reaches-receiver lifecycle-receiver)
       (active lifecycle-receiver))
  ("> test blocker moves into the lifecycle beam")
  (assert
    (has-location lifecycle-blocker lifecycle-mid)
    (finally (propagate-changes!))))


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!)))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query beam-direct-scenarios-valid ()
  (and
    ;; Empty corridor: full positive direct arrival and propagated receiver state.
    (fixed-beam-corridor-clear clear-source clear-receiver)
    (direct-beam-live-for-cutting clear-source clear-receiver)
    (direct-beam-reaches-receiver clear-receiver)
    (beam-reaches-receiver clear-receiver)
    (not (beam-cut clear-source clear-receiver))
    (active clear-receiver)

    ;; Multi-obstacle conjunction: open gate and elevated clear location both pass.
    (open open-gate)
    (not (open closed-gate))
    (= (fixed-beam-elevation-at
         mixed-clear-source mixed-clear-mid mixed-clear-receiver)
       2)
    (beam-clear mixed-clear-source open-gate mixed-clear-receiver)
    (not (beam-blocker-occludes-location mixed-clear-mid 2))
    (beam-clear mixed-clear-source mixed-clear-mid mixed-clear-receiver)
    (fixed-beam-corridor-clear mixed-clear-source mixed-clear-receiver)
    (direct-beam-live-for-cutting mixed-clear-source mixed-clear-receiver)
    (direct-beam-reaches-receiver mixed-clear-receiver)
    (active mixed-clear-receiver)

    ;; One closed gate blocks the complete corridor and every downstream direct result.
    (beam-clear closed-gate-source open-gate closed-gate-receiver)
    (not (beam-clear closed-gate-source closed-gate closed-gate-receiver))
    (not (fixed-beam-corridor-clear
           closed-gate-source
           closed-gate-receiver))
    (not (direct-beam-live-for-cutting
           closed-gate-source
           closed-gate-receiver))
    (not (direct-beam-reaches-receiver closed-gate-receiver))
    (not (active closed-gate-receiver))

    ;; Sloped midpoint interpolation and the exact inclusive blocker boundary.
    (= (fixed-beam-elevation-at
         sloped-clear-source sloped-clear-mid sloped-clear-receiver)
       2)
    (not (beam-blocker-spans-elevation sloped-clear-blocker 2))
    (beam-clear
      sloped-clear-source sloped-clear-mid sloped-clear-receiver)
    (fixed-beam-corridor-clear
      sloped-clear-source sloped-clear-receiver)
    (direct-beam-reaches-receiver sloped-clear-receiver)
    (active sloped-clear-receiver)

    (= (fixed-beam-elevation-at
         sloped-blocked-source sloped-blocked-mid sloped-blocked-receiver)
       2)
    (beam-blocker-spans-elevation sloped-blocked-blocker 2)
    (not (beam-clear
           sloped-blocked-source
           sloped-blocked-mid
           sloped-blocked-receiver))
    (not (fixed-beam-corridor-clear
           sloped-blocked-source
           sloped-blocked-receiver))
    (not (direct-beam-reaches-receiver sloped-blocked-receiver))
    (not (active sloped-blocked-receiver))

    ;; Endpoint direction: transmitter -> repeater is direct cutting liveness.
    (= (top repeater-target) 1)
    (fixed-beam-corridor-clear repeater-target-source repeater-target)
    (direct-beam-live-for-cutting repeater-target-source repeater-target)
    (not (beam-cut repeater-target-source repeater-target))

    ;; Repeater -> receiver remains structurally valid, but direct cutting liveness is
    ;; transmitter-only and the direct receiver query enumerates transmitters only.
    (= (top repeater-origin) 1)
    (fixed-beam-corridor-clear
      repeater-origin repeater-origin-receiver)
    (not (direct-beam-live-for-cutting
           repeater-origin repeater-origin-receiver))
    (not (direct-beam-reaches-receiver repeater-origin-receiver))
    (not (active repeater-origin-receiver))

    ;; Lifecycle final state: the action removed the old location, exact-height occlusion
    ;; broke the corridor, and receiver propagation removed ACTIVE.
    (has-location lifecycle-blocker lifecycle-mid)
    (not (has-location lifecycle-blocker lifecycle-staging))
    (= (fixed-beam-elevation-at
         lifecycle-source lifecycle-mid lifecycle-receiver)
       1)
    (beam-blocker-spans-elevation lifecycle-blocker 1)
    (beam-blocker-occludes-location lifecycle-mid 1)
    (not (beam-clear
           lifecycle-source lifecycle-mid lifecycle-receiver))
    (not (fixed-beam-corridor-clear
           lifecycle-source lifecycle-receiver))
    (not (direct-beam-live-for-cutting
           lifecycle-source lifecycle-receiver))
    (not (direct-beam-reaches-receiver lifecycle-receiver))
    (not (beam-reaches-receiver lifecycle-receiver))
    (not (beam-cut lifecycle-source lifecycle-receiver))
    (not (active lifecycle-receiver))))


(define-goal
  (beam-direct-scenarios-valid))

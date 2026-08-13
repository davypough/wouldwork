;;; Filename: problem-tray-test.lisp

;;; Dedicated regression for the tray capability, integrating pickup-tray, put-tray, and
;;; the held-tray relocation cascade in apply-agent-configuration!.  Four isolated
;;; scenarios exercise:
;;;
;;;   1. PICKUP-TRAY removing a grounded tray from a plate, within reach: has-location is
;;;      set to the picking-up agent's own location (not cleared, unlike other cargo), and
;;;      the prior ON fact is retracted.
;;;   2. PUT-TRAY placing a held, unloaded tray on a clear plate -- the ordinary case,
;;;      structurally identical to PUT-BOX.
;;;   3. The full cascade: a second agent loads a box onto BEARER-AGENT's held tray, then
;;;      BEARER-AGENT carries the loaded tray to a new, higher location (via a minimal
;;;      problem-local move action isolating apply-agent-configuration! from any general
;;;      mobility technology), then puts the loaded tray down.  The box's has-location and
;;;      elevation follow the tray throughout, and putting the tray down while still loaded
;;;      succeeds -- confirmed by the box remaining ON the tray with the tray's elevation
;;;      dropping from the holder's top level to its own grounded, zero-thickness level.
;;;   4. PICKUP-TRAY's not-already-held guard: unlike other cargo, a held tray keeps its
;;;      has-location, so a tray already held by one agent cannot be independently picked
;;;      up by another merely because it still resolves a location.
;;;
;;; Only the first three scenarios change state.  Expected minimum path length: five
;;; actions -- one PICKUP-TRAY, one PUT-TRAY, and the cascade's PUT-BOX/CARRY-MOVE/PUT-TRAY
;;; sequence, which is order-forced: the box can only be loaded before BEARER-AGENT departs
;;; CASCADE-ORIGIN, since nothing else can follow the tray there.


(in-package :ww)


(ww-set *problem-name* tray-test)

(ww-set *problem-type* planning)

(ww-set *solution-type* min-length)

(ww-set *tree-or-graph* graph)

(ww-set *depth-cutoff* 5)

(setf *expected-min-length* 5)


;;;; TYPES ;;;;


(define-types
  agent (pickup-agent put-agent bearer-agent loader-agent
         owner-agent thief-agent)
  location (pickup-agent-site pickup-tray-site put-origin
            cascade-origin cascade-destination theft-site)
  pressure-plate (pickup-source-plate put-plate)
  box (cascade-box)
  tray (ground-pickup-tray held-only-tray cascade-tray already-held-tray))


;;;; TECHNOLOGY INCLUDES ;;;;


(include-tech plate)
(include-tech box)
(include-tech tray)
(include-tech -configuration-transition)
(include-tech reachability)


;;;; A MINIMAL, PROBLEM-LOCAL MOVE ACTION ;;;;


(define-action carry-move
  ;; Isolate apply-agent-configuration!'s held-tray relocation cascade from any
  ;; general-purpose mobility technology: carry BEARER-AGENT, and its held tray, from
  ;; CASCADE-ORIGIN to CASCADE-DESTINATION on the ground.
  1
  (?agent agent)
  (and (eql ?agent 'bearer-agent)
       (has-location ?agent cascade-origin))
  (">" ?agent "carries its held tray to" "cascade-destination")
  (assert (assign $destination-configuration (list 'cascade-destination 'ground))
          (apply-agent-configuration! ?agent $destination-configuration)
          (finally (propagate-changes!))))


;;;; INITIALIZATION ;;;;


(define-init
  ;; PICKUP-TRAY lane: the tray rests on a plate at a reachable remote site.
  (has-location pickup-agent pickup-agent-site)
  (has-position pickup-source-plate pickup-tray-site)
  (has-location ground-pickup-tray pickup-tray-site)
  (on ground-pickup-tray pickup-source-plate)
  (reach-via pickup-agent-site () pickup-tray-site)

  ;; PUT-TRAY lane: an unloaded held tray goes onto a clear, co-located plate.
  (has-location put-agent put-origin)
  (holding put-agent held-only-tray)
  (has-location held-only-tray put-origin)
  (has-position put-plate put-origin)

  ;; Cascade lane: BEARER-AGENT already holds CASCADE-TRAY at CASCADE-ORIGIN (elevation
  ;; 0); LOADER-AGENT, co-located, holds CASCADE-BOX and will place it on the held tray.
  ;; CASCADE-DESTINATION sits three elevation units up, exercising a nontrivial cascade.
  (has-location bearer-agent cascade-origin)
  ;; Keep the held tray at the fixed unit reach boundary for LOADER-AGENT.
  (has-height bearer-agent 1)
  (holding bearer-agent cascade-tray)
  (has-location cascade-tray cascade-origin)
  (has-location loader-agent cascade-origin)
  (holding loader-agent cascade-box)
  (has-elevation cascade-destination 3)

  ;; Negative fixture: ALREADY-HELD-TRAY is held by OWNER-AGENT but, being a tray,
  ;; still resolves a has-location at THEFT-SITE, co-located with THIEF-AGENT.
  (has-location owner-agent theft-site)
  (has-location thief-agent theft-site)
  (holding owner-agent already-held-tray)
  (has-location already-held-tray theft-site))


(define-init-action initialize-derived-state
  0
  ()
  (always-true)
  ()
  (assert (propagate-changes!)))


;;;; ACTION-PRECONDITION CHARACTERIZATION ;;;;


(define-test-helper tray-action-applicable-p (state action-name args)
  "Whether the installed tray action accepts ARGS in STATE."
  (let ((action (find action-name *actions* :key #'action.name)))
    (and (member args (get-precondition-args action state) :test #'equal)
         (apply (action.pre-defun-name action) state args))))


;;;; CHARACTERIZATION QUERY AND GOAL ;;;;


(define-query tray-scenarios-valid ()
  (and
    ;; PICKUP-TRAY: has-location becomes the picking-up agent's own location -- not
    ;; cleared -- and the prior ON fact is gone.
    (has-location pickup-agent pickup-agent-site)
    (holding pickup-agent ground-pickup-tray)
    (has-location ground-pickup-tray pickup-agent-site)
    (not (exists (?support support)
           (on ground-pickup-tray ?support)))

    ;; PUT-TRAY, unloaded: ordinary release onto the target plate.
    (has-location put-agent put-origin)
    (not (holding put-agent held-only-tray))
    (has-location held-only-tray put-origin)
    (on held-only-tray put-plate)
    (not (exists (?support support)
           (and (not (eql ?support 'put-plate))
                (on held-only-tray ?support))))

    ;; Cascade, post-sequence: BEARER-AGENT and its (still loaded) tray have both
    ;; moved; CASCADE-BOX followed throughout and now sits at the tray's grounded,
    ;; zero-thickness level rather than the holder's former top level.
    (has-location bearer-agent cascade-destination)
    (not (holding bearer-agent cascade-tray))
    (has-location cascade-tray cascade-destination)
    (not (exists (?support support)
           (on cascade-tray ?support)))
    (not (holding loader-agent cascade-box))
    (on cascade-box cascade-tray)
    (not (cleartop cascade-tray))
    (has-location cascade-box cascade-destination)
    (= (occupant-elevation bearer-agent) 3)
    (= (support-top-elevation cascade-tray) 3)
    (= (occupant-elevation cascade-box) 3)

    ;; Negative: ALREADY-HELD-TRAY resolves a location but is not pickable by
    ;; THIEF-AGENT, since it is already held by OWNER-AGENT.
    (holding owner-agent already-held-tray)
    (has-location already-held-tray theft-site)
    (has-location thief-agent theft-site)
    (not (tray-action-applicable-p
           state 'pickup-tray '(thief-agent already-held-tray)))))


(define-goal
  (tray-scenarios-valid))


;;;; MUTATION CHARACTERIZATION ;;;;


(define-action-precondition-mutation pickup-tray-allows-already-held pickup-tray
  (and (bind (has-location ?agent $a-location))
       (bind (has-location ?tray $tray-location))
       (pickup-clear ?agent $a-location ?tray $tray-location))
  "Drops PICKUP-TRAY's not-already-held guard.  The already-held-tray probe must
   then make this characterization fail.")
